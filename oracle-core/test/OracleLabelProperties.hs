{-# LANGUAGE OverloadedStrings #-}

module OracleLabelProperties (tests) where

import Control.Monad (forM_)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteStringInternal
import Data.Either (isLeft, isRight)
import Data.List (sortOn)
import Data.Serialize.Put qualified as Put
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity
import Eclips.Domain.Label
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd, HeraldRetired))
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Value
import Eclips.Oracle.Canonical qualified as Canonical
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity
import Eclips.Oracle.Label hiding (initialOracleState)
import Eclips.Oracle.State qualified as Checkpoint
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (ptrToIntPtr)
import Numeric (showHex)
import OracleFixtures
import Step15ReferenceProperties (retireLiveTarget, step15Genesis, targetH4, targetH5)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Positive (..), Property, conjoin, counterexample, testProperty, (===))
import VoterFailureFixtures qualified as VF

initialOracleState :: CheckedOracleGenesis -> OracleState
initialOracleState = initialOracleStateWithDigestMode OracleStateDigestEnabled

tests :: TestTree
tests =
  testGroup
    "atomic Oracle label semantics"
    [ testGroup
        "pure transition and authority algebra"
        [ testCase "decision identity is exact and mutation-sensitive" caseDecisionIdentity,
          testCase "complete target transition table" caseTransitionTable,
          testCase "comparison precedes transition rules" caseExpectedMismatch,
          testCase "caller must be live" caseCallerMustBeLive,
          testCase "target must be live" caseTargetMustBeLive,
          testCase "stored zombies cannot bypass normalization" caseStoredZombieRejected,
          testCase "deleted prior is terminal" caseDeletedPrior,
          testCase "same owner retains authority and increments generation" caseSameLabel,
          testProperty "generated generations advance once" propGenerationTransitions,
          testCase "process to void retains historical authority" caseProcessToVoid,
          testCase "void to void retains historical authority" caseVoidToVoid,
          testCase "void to process derives new authority" caseVoidToProcess,
          testCase "zombie to process derives new authority" caseZombieToProcess,
          testCase "handoff derives authority" caseProcessHandoff,
          testCase "delete retains historical authority" caseDelete,
          testCase "ordinary labels never gain authority" caseOrdinaryAuthority,
          testCase "zero revision is rejected" caseReleaseIndexZero,
          testProperty "revision is the exact canonical index" propExactRevision,
          testCase "End normalizes without rewriting the overlay" caseEndProjection,
          testCase "End admission" caseEndAdmission,
          testCase "consequence provenance tags and views" caseConsequenceCauses,
          testCase "consequence index zero is rejected" caseConsequenceIndexZero,
          testProperty "positive consequence index round trips" propConsequenceRoundTrip,
          testCase "complete Start and End records" caseCompleteProcessRecords
        ],
      atomicTests
    ]

caseDecisionIdentity :: Assertion
caseDecisionIdentity = do
  let request = oracleClientRequestId fixtureHeraldEpoch 17
      same = deriveLabelDecisionId fixtureSystemId request
      changedSystem = deriveLabelDecisionId (systemId 2) request
      changedHome =
        deriveLabelDecisionId
          fixtureSystemId
          (oracleClientRequestId fixtureRemoteHeraldEpoch 17)
      changedSequence =
        deriveLabelDecisionId
          fixtureSystemId
          (oracleClientRequestId fixtureHeraldEpoch 18)
  assertEqual
    "exact retry"
    (labelDecisionIdBytes same)
    (labelDecisionIdBytes (deriveLabelDecisionId fixtureSystemId request))
  assertBool "system participates" (same /= changedSystem)
  assertBool "home epoch participates" (same /= changedHome)
  assertBool "request sequence participates" (same /= changedSequence)
  assertEqual
    "decision identity golden"
    "fcfaef17c2957bd3f745b77498d7fd86ef18ec8df8330a044e71de8aad46b406"
    (hexBytes (labelDecisionIdBytes same))

caseTransitionTable :: Assertion
caseTransitionTable = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      live = processTable [process, other, fixtureStartProcessEpoch]
      endedProcess = endProcess 3 process live
      processTargets =
        [ ("self", targetProcess process),
          ("other", targetProcess other),
          ("void", targetVoid),
          ("delete", targetDelete)
        ]
  forM_ processTargets $ \(targetName, target) ->
    assertAccepted
      ("owned process target " <> targetName)
      live
      process
      (ProcessLabel process, 0)
      (releasedLabel (ProcessLabel process, 0))
      target
  forM_ processTargets $ \(targetName, target) ->
    assertRejected
      ("foreign process target " <> targetName)
      live
      other
      (ProcessLabel process, 0)
      (releasedLabel (ProcessLabel process, 0))
      target
  forM_
    [("caller", targetProcess process), ("void", targetVoid), ("delete", targetDelete)]
    $ \(targetName, target) ->
      assertAccepted
        ("void target " <> targetName)
        live
        process
        (VoidLabel, 0)
        (releasedLabel (VoidLabel, 0))
        target
  assertRejected
    "void cannot name a different process"
    live
    process
    (VoidLabel, 0)
    (releasedLabel (VoidLabel, 0))
    (targetProcess other)

  forM_
    [("caller", targetProcess other), ("void", targetVoid), ("delete", targetDelete)]
    $ \(targetName, target) ->
      assertAccepted
        ("observed zombie target " <> targetName)
        endedProcess
        other
        (ZombieLabel process, 0)
        (releasedLabel (ProcessLabel process, 0))
        target
  assertRejected
    "zombie cannot target a different process"
    endedProcess
    other
    (ZombieLabel process, 0)
    (releasedLabel (ProcessLabel process, 0))
    (targetProcess fixtureStartProcessEpoch)

caseExpectedMismatch :: Assertion
caseExpectedMismatch = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      processes = processTable [process, other]
      prior = ordinaryPrior (releasedLabel (VoidLabel, 0))
  assertEqual
    "ordinary mismatch"
    (Left (LiveExpectedLabelMismatch (ProcessLabel process, 0) (VoidLabel, 0)))
    (prepareLiveLabel processes process (ProcessLabel process, 0) prior targetVoid)
  let ended = endProcess 3 process processes
      endedPrior = ordinaryPrior (releasedLabel (ProcessLabel process, 0))
  assertEqual
    "stale pre-End value does not address the zombie overlay"
    ( Left
        ( LiveExpectedLabelMismatch
            (ProcessLabel process, 0)
            (ZombieLabel process, 0)
        )
    )
    (prepareLiveLabel ended other (ProcessLabel process, 0) endedPrior targetVoid)

caseTargetMustBeLive :: Assertion
caseTargetMustBeLive = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      processes = endProcess 2 other (processTable [process, other])
      prior = ordinaryPrior (releasedLabel (ProcessLabel process, 0))
  assertEqual
    "ended target"
    (Left (LiveTargetProcessNotLive other))
    ( prepareLiveLabel
        processes
        process
        (ProcessLabel process, 0)
        prior
        (targetProcess other)
    )
  assertEqual
    "unknown target"
    (Left (LiveTargetProcessNotLive fixtureStartProcessEpoch))
    ( prepareLiveLabel
        processes
        process
        (ProcessLabel process, 0)
        prior
        (targetProcess fixtureStartProcessEpoch)
    )

caseStoredZombieRejected :: Assertion
caseStoredZombieRejected = do
  let zombie = releasedLabel (ZombieLabel fixtureProcessEpoch, 0)
      expected = Left (LivePriorStoredZombieNotPermitted fixtureProcessEpoch)
  assertEqual "ordinary prior" expected (liveOrdinaryPrior zombie)
  assertEqual
    "authority prior"
    expected
    ( liveAuthorityPrior
        zombie
        (liveExistingAuthority genesisAuthorityEpoch)
    )

caseCallerMustBeLive :: Assertion
caseCallerMustBeLive = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      live = processTable [process, other]
      ended = endProcess 2 process live
      prior = ordinaryPrior (releasedLabel (VoidLabel, 0))
  assertEqual
    "ended caller"
    (Left (LiveCallerProcessNotLive process))
    (prepareLiveLabel ended process (VoidLabel, 0) prior targetVoid)
  assertEqual
    "unknown caller"
    (Left (LiveCallerProcessNotLive fixtureStartProcessEpoch))
    ( prepareLiveLabel
        live
        fixtureStartProcessEpoch
        (VoidLabel, 0)
        prior
        targetVoid
    )

caseDeletedPrior :: Assertion
caseDeletedPrior = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
  forM_ [targetProcess process, targetVoid, targetDelete] $ \target ->
    assertRejected "deleted prior" processes process (VoidLabel, 0) (releasedDeleted 0) target

caseSameLabel :: Assertion
caseSameLabel = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      authority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          processes
          process
          (ProcessLabel process, 0)
          (authorityPrior (releasedLabel (ProcessLabel process, 0)) authority)
          (targetProcess process)
      record = releaseRight 7 prepared
  assertBool "same owner changes the complete effective pair" (not (livePreparedSameEffective prepared))
  assertEqual
    "effective prior"
    (releasedLabel (ProcessLabel process, 0))
    (livePreparedPriorEffectiveState prepared)
  assertEqual
    "prepared outcome"
    (releasedLabel (ProcessLabel process, 1))
    (livePreparedOutcome prepared)
  assertEqual
    "retained disposition"
    (LiveRetainPriorAuthorityView authority)
    (livePreparedAuthorityDisposition prepared)
  assertEqual "exact authority" (Just authority) (liveRecordRetainedAuthority record)
  assertEqual "still applicable" (Just authority) (liveRecordApplicableAuthority processes record)
  assertRevision 7 record

propGenerationTransitions :: Word8 -> Property
propGenerationTransitions input =
  let generation = fromIntegral input
      process = fixtureProcessEpoch
      processes = processTable [process]
      next expected target =
        livePreparedOutcome
          (prepareRight processes process expected (ordinaryPrior (releasedLabel expected)) target)
   in counterexample "generation advances once independently of the release control index"
        $ [ next (ProcessLabel process, generation) (targetProcess process),
            next (ProcessLabel process, generation) targetVoid,
            next (VoidLabel, generation) targetVoid,
            next (VoidLabel, generation) (targetProcess process),
            next (ProcessLabel process, generation) targetDelete
          ]
          === [ releasedLabel (ProcessLabel process, generation + 1),
                releasedLabel (VoidLabel, generation + 1),
                releasedLabel (VoidLabel, generation + 1),
                releasedLabel (ProcessLabel process, generation + 1),
                releasedDeleted (generation + 1)
              ]

caseProcessToVoid :: Assertion
caseProcessToVoid = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      authority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          processes
          process
          (ProcessLabel process, 0)
          (authorityPrior (releasedLabel (ProcessLabel process, 0)) authority)
          targetVoid
      record = releaseRight 7 prepared
  assertBool "operator assignment changed" (not (livePreparedSameEffective prepared))
  assertEqual "void outcome" (releasedLabel (VoidLabel, 1)) (livePreparedOutcome prepared)
  assertEqual
    "no new tenure for void"
    (LiveRetainPriorAuthorityView authority)
    (livePreparedAuthorityDisposition prepared)
  assertEqual "history retained" (Just authority) (liveRecordRetainedAuthority record)
  assertEqual "void has no applicable tenure" Nothing (liveRecordApplicableAuthority processes record)

caseVoidToVoid :: Assertion
caseVoidToVoid = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      authority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          processes
          process
          (VoidLabel, 0)
          (authorityPrior (releasedLabel (VoidLabel, 0)) authority)
          targetVoid
      record = releaseRight 8 prepared
  assertBool "void-to-void advances the complete pair" (not (livePreparedSameEffective prepared))
  assertEqual "void successor" (releasedLabel (VoidLabel, 1)) (liveRecordStoredState record)
  assertEqual
    "no new tenure for void"
    (LiveRetainPriorAuthorityView authority)
    (livePreparedAuthorityDisposition prepared)
  assertEqual "history retained" (Just authority) (liveRecordRetainedAuthority record)
  assertEqual "void authority remains inapplicable" Nothing (liveRecordApplicableAuthority processes record)

caseVoidToProcess :: Assertion
caseVoidToProcess = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      oldAuthority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          processes
          process
          (VoidLabel, 0)
          (authorityPrior (releasedLabel (VoidLabel, 0)) oldAuthority)
          (targetProcess process)
      firstRecord = releaseRight 7 prepared
      derived = liveRecordRetainedAuthority firstRecord
  assertEqual
    "live assignment derives"
    LiveDeriveAuthorityAtReleaseView
    (livePreparedAuthorityDisposition prepared)
  case derived of
    Nothing -> assertFailure "derived authority missing"
    Just authority -> do
      assertEqual
        "release index authority"
        (LiveLabelAuthorityView (controlIndex 7))
        (liveAuthorityView authority)
      assertEqual
        "derived authority applicable"
        (Just authority)
        (liveRecordApplicableAuthority processes firstRecord)
      let samePrepared =
            prepareRight
              processes
              process
              (ProcessLabel process, 1)
              (livePriorFromRecord firstRecord)
              (targetProcess process)
          secondRecord = releaseRight 8 samePrepared
      assertEqual
        "later same-label retains tag-2 live tenure"
        (LiveRetainPriorAuthorityView authority)
        (livePreparedAuthorityDisposition samePrepared)
      assertEqual
        "retained tag-2 authority"
        (Just authority)
        (liveRecordRetainedAuthority secondRecord)
      assertRevision 8 secondRecord

caseZombieToProcess :: Assertion
caseZombieToProcess = do
  let endedOperator = fixtureProcessEpoch
      caller = fixtureRemoteProcessEpoch
      live = processTable [endedOperator, caller]
      afterEnd = endProcess 6 endedOperator live
      priorAuthority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          afterEnd
          caller
          (ZombieLabel endedOperator, 0)
          ( authorityPrior
              (releasedLabel (ProcessLabel endedOperator, 0))
              priorAuthority
          )
          (targetProcess caller)
      record = releaseRight 9 prepared
  assertEqual
    "stored process projects to zombie before preparation"
    (releasedLabel (ZombieLabel endedOperator, 0))
    (livePreparedPriorEffectiveState prepared)
  assertEqual
    "new live assignment derives"
    LiveDeriveAuthorityAtReleaseView
    (livePreparedAuthorityDisposition prepared)
  assertEqual
    "release index authority"
    (Just (LiveLabelAuthorityView (controlIndex 9)))
    (liveAuthorityView <$> liveRecordRetainedAuthority record)

caseProcessHandoff :: Assertion
caseProcessHandoff = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      processes = processTable [process, other]
      prepared =
        prepareRight
          processes
          process
          (ProcessLabel process, 0)
          ( authorityPrior
              (releasedLabel (ProcessLabel process, 0))
              (liveExistingAuthority genesisAuthorityEpoch)
          )
          (targetProcess other)
      record = releaseRight 11 prepared
  assertEqual
    "different live operator derives"
    LiveDeriveAuthorityAtReleaseView
    (livePreparedAuthorityDisposition prepared)
  assertEqual
    "derived tenure"
    (Just (LiveLabelAuthorityView (controlIndex 11)))
    (liveAuthorityView <$> liveRecordRetainedAuthority record)

caseDelete :: Assertion
caseDelete = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      authority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          processes
          process
          (ProcessLabel process, 0)
          (authorityPrior (releasedLabel (ProcessLabel process, 0)) authority)
          targetDelete
      record = releaseRight 12 prepared
  assertEqual
    "delete retires prior"
    (LiveRetirePriorAuthorityView authority)
    (livePreparedAuthorityDisposition prepared)
  assertEqual "historical authority" (Just authority) (liveRecordRetainedAuthority record)
  assertEqual "deleted authority inapplicable" Nothing (liveRecordApplicableAuthority processes record)
  assertEqual "terminal successor generation" (releasedDeleted 1) (liveRecordStoredState record)
  assertRevision 12 record
  assertBool
    "no later label"
    ( isLeft
        ( prepareLiveLabel
            processes
            process
            (VoidLabel, 0)
            (livePriorFromRecord record)
            targetVoid
        )
    )

caseOrdinaryAuthority :: Assertion
caseOrdinaryAuthority = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      processes = processTable [process, other]
      prepared =
        prepareRight
          processes
          process
          (ProcessLabel process, 0)
          (ordinaryPrior (releasedLabel (ProcessLabel process, 0)))
          (targetProcess other)
      record = releaseRight 5 prepared
  assertEqual
    "ordinary disposition"
    LiveNoTargetAuthorityView
    (livePreparedAuthorityDisposition prepared)
  assertEqual "ordinary retained authority" Nothing (liveRecordRetainedAuthority record)
  assertEqual "ordinary applicable authority" Nothing (liveRecordApplicableAuthority processes record)

caseReleaseIndexZero :: Assertion
caseReleaseIndexZero = do
  let process = fixtureProcessEpoch
      processes = processTable [process]
      prepared =
        prepareRight
          processes
          process
          (VoidLabel, 0)
          (ordinaryPrior (releasedLabel (VoidLabel, 0)))
          targetVoid
  assertEqual
    "zero release"
    (Left LiveReleaseIndexMustBePositive)
    (releaseLiveLabel (controlIndex 0) prepared)

propExactRevision :: Positive Word64 -> Property
propExactRevision (Positive indexWord)
  | indexWord == 0 = propExactRevision (Positive 1)
  | otherwise =
      let process = fixtureProcessEpoch
          processes = processTable [process]
          prepared =
            prepareRight
              processes
              process
              (VoidLabel, 0)
              (ordinaryPrior (releasedLabel (VoidLabel, 0)))
              targetVoid
       in case releaseLiveLabel (controlIndex indexWord) prepared of
            Left problem -> counterexample (show problem) False
            Right record ->
              labelRevisionControlIndex (liveRecordRevision record)
                === controlIndex indexWord

caseEndProjection :: Assertion
caseEndProjection = do
  let process = fixtureProcessEpoch
      other = fixtureRemoteProcessEpoch
      live = processTable [process, other]
      authority = liveExistingAuthority genesisAuthorityEpoch
      prepared =
        prepareRight
          live
          process
          (ProcessLabel process, 0)
          (authorityPrior (releasedLabel (ProcessLabel process, 0)) authority)
          (targetProcess process)
      record = releaseRight 4 prepared
      ended = endProcess 9 process live
      afterEnd =
        prepareRight
          ended
          other
          (ZombieLabel process, 1)
          (livePriorFromRecord record)
          targetVoid
  assertEqual
    "stored state unchanged"
    (releasedLabel (ProcessLabel process, 1))
    (liveRecordStoredState record)
  assertEqual
    "revision unchanged"
    (controlIndex 4)
    (labelRevisionControlIndex (liveRecordRevision record))
  assertEqual
    "retained authority unchanged"
    (Just authority)
    (liveRecordRetainedAuthority record)
  assertEqual "applicable before End" (Just authority) (liveRecordApplicableAuthority live record)
  assertEqual "inapplicable after End" Nothing (liveRecordApplicableAuthority ended record)
  assertEqual
    "effective zombie"
    (releasedLabel (ZombieLabel process, 1))
    (liveEffectiveReleasedState ended (liveRecordStoredState record))
  assertEqual
    "the next preparation observes the zombie projection"
    (releasedLabel (ZombieLabel process, 1))
    (livePreparedPriorEffectiveState afterEnd)
  assertEqual
    "zombie CAS to void retains the historical tenure"
    (LiveRetainPriorAuthorityView authority)
    (livePreparedAuthorityDisposition afterEnd)
  assertEqual
    "normalization idempotent"
    (ZombieLabel process, 1)
    (liveEffectiveLabel ended (liveEffectiveLabel ended (ProcessLabel process, 1)))
  assertRevision 4 record

caseEndAdmission :: Assertion
caseEndAdmission = do
  let process = fixtureProcessEpoch
      unknown = fixtureRemoteProcessEpoch
      live = processTable [process]
  assertEqual
    "zero index"
    (Left LiveEndIndexMustBePositive)
    (liveEndProcess (controlIndex 0) ExplicitAdministrativeEnd process live)
  assertEqual
    "unknown process"
    (Left (LiveEndUnknownProcess unknown))
    (liveEndProcess (controlIndex 1) ExplicitAdministrativeEnd unknown live)
  let ended = endProcess 1 process live
  assertEqual
    "exact retained status"
    (Just (LiveProcessEndedView (controlIndex 1) ExplicitAdministrativeEnd))
    (liveProcessStatus process ended)
  assertEqual
    "fresh duplicate End"
    (Left (LiveEndAlreadyEnded process))
    (liveEndProcess (controlIndex 2) ExplicitAdministrativeEnd process ended)
  assertEqual
    "duplicate live-process presentation"
    (Left (LiveDuplicateProcess process))
    (liveLiveProcesses [process, process])

caseConsequenceCauses :: Assertion
caseConsequenceCauses = do
  let sequenceNumber = checked "structural sequence" (mkStructuralSequence 3)
      occurrence = structuralOccurrenceId fixtureHeraldEpoch sequenceNumber
      decision =
        deriveLabelDecisionId
          fixtureSystemId
          (oracleClientRequestId fixtureHeraldEpoch 21)
      structuralCause = liveStructuralOccurrenceCause occurrence
      labelCause = checked "label consequence" (liveLabelReleaseCause decision (controlIndex 4))
      endCause = checked "End consequence" (liveProcessEndCause fixtureProcessEpoch (controlIndex 5))
  assertEqual
    "structural view"
    (LiveStructuralOccurrenceCauseView occurrence)
    (liveConsequenceCauseView structuralCause)
  assertEqual
    "label view"
    (LiveLabelReleaseCauseView decision (controlIndex 4))
    (liveConsequenceCauseView labelCause)
  assertEqual
    "End view"
    (LiveProcessEndCauseView fixtureProcessEpoch (controlIndex 5))
    (liveConsequenceCauseView endCause)
  assertBool "structural tag before label" (structuralCause < labelCause)
  assertBool "label tag before End" (labelCause < endCause)
  assertEqual "structural canonical tag" 0 (liveConsequenceCauseTag structuralCause)
  assertEqual "label canonical tag" 1 (liveConsequenceCauseTag labelCause)
  assertEqual "End canonical tag" 2 (liveConsequenceCauseTag endCause)
  assertEqual "structural round-trip" structuralCause (causeFromView (liveConsequenceCauseView structuralCause))
  assertEqual "label round-trip" labelCause (causeFromView (liveConsequenceCauseView labelCause))
  assertEqual "End round-trip" endCause (causeFromView (liveConsequenceCauseView endCause))

caseConsequenceIndexZero :: Assertion
caseConsequenceIndexZero = do
  let decision =
        deriveLabelDecisionId
          fixtureSystemId
          (oracleClientRequestId fixtureHeraldEpoch 22)
      expected = Left LiveConsequenceControlIndexMustBePositive
  assertEqual "label cause" expected (liveLabelReleaseCause decision (controlIndex 0))
  assertEqual
    "End cause"
    expected
    (liveProcessEndCause fixtureProcessEpoch (controlIndex 0))

propConsequenceRoundTrip :: Positive Word64 -> Property
propConsequenceRoundTrip (Positive indexWord)
  | indexWord == 0 = propConsequenceRoundTrip (Positive 1)
  | otherwise =
      let decision =
            deriveLabelDecisionId
              fixtureSystemId
              (oracleClientRequestId fixtureHeraldEpoch indexWord)
          cause =
            checked
              "positive label consequence"
              (liveLabelReleaseCause decision (controlIndex indexWord))
       in causeFromView (liveConsequenceCauseView cause) === cause

caseCompleteProcessRecords :: Assertion
caseCompleteProcessRecords = do
  let initial = initialOracleState fixtureCheckedGenesis
      genesisRecord =
        maybe
          (error "missing genesis process record")
          id
          (oracleProcessRecord fixtureProcessEpoch initial)
      (started, startIndex, startEvents) =
        commitAccepted
          90
          fixtureHeraldEpoch
          (startProcessEpochCommand fixtureStart)
          initial
      startedRecord =
        maybe
          (error "missing configured Start process record")
          id
          (oracleProcessRecord fixtureStartProcessEpoch started)
      (ended, endIndex, endEvents) =
        commitAccepted
          91
          fixtureHeraldEpoch
          ( checkedEndCommand
              fixtureStartProcessEpoch
              ExplicitAdministrativeEnd
          )
          started
      endedRecord =
        maybe
          (error "missing ended configured process record")
          id
          (oracleProcessRecord fixtureStartProcessEpoch ended)
  assertEqual "genesis origin" GenesisProcessView (processRecordOrigin genesisRecord)
  assertEqual "genesis lifecycle" ProcessRecordLiveView (processRecordLifecycle genesisRecord)
  assertEqual "Start index" (controlIndex 1) startIndex
  assertEqual "Start event" [0] (fmap oracleProjectionEventTag startEvents)
  assertEqual "retained process epoch" fixtureStartProcessEpoch (processRecordProcessEpoch startedRecord)
  assertEqual "retained residence" fixtureHeraldEpoch (processRecordResidence startedRecord)
  assertEqual
    "configured origin retains index and manifest"
    ( DynamicStartView
        startIndex
        (oracleClientRequestId fixtureHeraldEpoch 90)
    )
    (processRecordOrigin startedRecord)
  assertEqual "configured lifecycle" ProcessRecordLiveView (processRecordLifecycle startedRecord)
  assertEqual "End index" (controlIndex 2) endIndex
  assertEqual "End event" [1] (fmap oracleProjectionEventTag endEvents)
  assertEqual
    "End retains the record and exact reason"
    (ProcessRecordEndedView endIndex ExplicitAdministrativeEnd)
    (processRecordLifecycle endedRecord)

processTable :: [ProcessEpochId] -> LiveProcessTable
processTable = checked "live process table" . liveLiveProcesses

endProcess :: Word64 -> ProcessEpochId -> LiveProcessTable -> LiveProcessTable
endProcess index process =
  checked "live process End"
    . liveEndProcess
      (controlIndex index)
      ExplicitAdministrativeEnd
      process

ordinaryPrior :: ReleasedLabelState -> LivePriorLabel
ordinaryPrior = checked "live ordinary prior" . liveOrdinaryPrior

authorityPrior ::
  ReleasedLabelState ->
  LiveAuthority ->
  LivePriorLabel
authorityPrior state =
  checked "live authority prior" . liveAuthorityPrior state

prepareRight ::
  LiveProcessTable ->
  ProcessEpochId ->
  Label ->
  LivePriorLabel ->
  LabelTarget ->
  LivePreparedLabel
prepareRight processes caller expected prior target =
  checked
    "live label preparation"
    (prepareLiveLabel processes caller expected prior target)

releaseRight :: Word64 -> LivePreparedLabel -> LiveLabelRecord
releaseRight index =
  checked "live label release"
    . releaseLiveLabel (controlIndex index)

assertAccepted ::
  String ->
  LiveProcessTable ->
  ProcessEpochId ->
  Label ->
  ReleasedLabelState ->
  LabelTarget ->
  Assertion
assertAccepted context processes caller expected prior target =
  assertBool
    context
    ( isRight
        ( prepareLiveLabel
            processes
            caller
            expected
            (ordinaryPrior prior)
            target
        )
    )

assertRejected ::
  String ->
  LiveProcessTable ->
  ProcessEpochId ->
  Label ->
  ReleasedLabelState ->
  LabelTarget ->
  Assertion
assertRejected context processes caller expected prior target =
  assertBool
    context
    ( isLeft
        ( prepareLiveLabel
            processes
            caller
            expected
            (ordinaryPrior prior)
            target
        )
    )

assertRevision :: Word64 -> LiveLabelRecord -> Assertion
assertRevision expected record =
  assertEqual
    "release revision"
    (controlIndex expected)
    (labelRevisionControlIndex (liveRecordRevision record))

causeFromView :: LiveConsequenceCauseView -> LiveConsequenceCause
causeFromView view = case view of
  LiveStructuralOccurrenceCauseView occurrence ->
    liveStructuralOccurrenceCause occurrence
  LiveLabelReleaseCauseView decision index ->
    checked "label cause view" (liveLabelReleaseCause decision index)
  LiveProcessEndCauseView process index ->
    checked "End cause view" (liveProcessEndCause process index)

hexBytes :: ByteString.ByteString -> String
hexBytes = concatMap hexByte . ByteString.unpack
  where
    hexByte byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

atomicTests :: TestTree
atomicTests =
  testGroup
    "canonical atomic decision"
    [ testCase "one decision installs and one completion closes" caseAtomicSuccess,
      testCase "failed CAS is accepted NotApplied and needs no completion" caseAtomicNotApplied,
      testCase "a committed busy proposal retains no receipt and exact retry succeeds" caseAtomicBusy,
      testCase "initial evidence is independent, required, and object-bound" caseAtomicInitialEvidence,
      testCase "bootstrap and publication provenance bind exact request identity" caseAtomicInitialProvenance,
      testCase "unrelated decisions at one home progress and complete independently" caseAtomicConcurrent,
      testCase "pending indexes survive different collectors and retirement" caseAtomicConcurrentRetirement,
      testProperty "concurrent completion orders preserve indexed checkpoints" propAtomicConcurrentCheckpoints,
      testCase "bootstrap authority is justified and transferred at the decision index" caseAtomicBootstrapAuthority,
      testCase "canonical End normalizes first publication and dominates caller claims" caseAtomicInitialEnd,
      testCase "the existing overlay dominates repeated initial evidence and authority claims" caseAtomicOverlay,
      testCase "same owner and owner ABA preserve full generation CAS" caseAtomicGeneration,
      testCase "two Heralds cannot both claim one void generation" caseAtomicCrossHerald,
      testCase "End before decision fixes NotApplied; End after decision never rewrites it" caseAtomicEndOrdering,
      testCase "deletion cannot be resurrected" caseAtomicDeletion,
      testCase "completion validates collector, digest and current membership" caseAtomicCompletion,
      testCase "compact completion witnesses preserve retired-receipt retries" caseAtomicCompletedWitness,
      testCase "compact completion checkpoint rejects unknown captured membership" caseAtomicCompletedMembership,
      testCase "collector retirement reassigns immutable decision evidence" caseAtomicCollectorRetirement,
      testCase "repeated retirement contracts participants without changing Applied" caseAtomicRepeatedRetirement,
      testCase "native voter exclusion and semantic retirement preserve the child transfer" caseAtomicVoterFailure,
      testCase "command and completion have independently constructed canonical bytes" caseAtomicCanonicalCommands,
      testCase "removed phase commands and events are unknown" caseAtomicRemovedTags,
      testCase "surviving rejection and NotApplied transcripts keep exact tags" caseCanonicalRejectionVectors,
      testCase "receipts, events and entries round trip with rejected corruption" caseAtomicCanonicalRoundTrips,
      testCase "optional decision fields and all target arms round trip" caseAtomicCanonicalOptionals,
      testCase "malformed atomic events and incoherent entries are rejected" caseAtomicCanonicalCoherence,
      testProperty "portable label facts preserve generated committed histories" propPortableLabelFacts,
      testCase "portable process and terminal facts cover every lifecycle and outcome" casePortableFactShapes,
      testCase "portable facts reject malformed bytes and inconsistent nominal fields" casePortableFactCoherence,
      testCase "portable nominal facts own their input buffers" casePortableFactBuffers,
      testCase "terminal and state digests use exact canonical preimages" caseAtomicDigestPreimages,
      testCase "all current state shapes restore exactly in both digest modes" caseAtomicCheckpoints,
      testProperty "successful workflows need two commands regardless of captured membership" propAtomicCommandCounts,
      testProperty "completed histories resume deterministically from every checkpoint" propAtomicHistory,
      testProperty "generation participates in exact request identity" propAtomicGenerationIdentity
    ]

caseAtomicSuccess :: Assertion
caseAtomicSuccess = do
  let (decided, _, entry, decision, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
      (completed, receipt, completion) = completeAt 2 decided
      record = recordAt objectA decided
  assertEqual "one decision control index" (controlIndex 1) (appliedEntryControlIndex entry)
  assertEqual "one decision projection event" [2] (tags entry)
  assertEqual "canonical overlay is installed immediately" (releasedLabel (VoidLabel, 1)) (labelRecordReleasedState record)
  assertEqual "decision is the label revision" (controlIndex 1) (labelRevisionControlIndex (labelRecordRevision record))
  assertEqual "installation collection retains the decision" (Just decision) (liveDecisionId <$> onlyOpenDecision decided)
  assertEqual "collection retains the terminal" (Just terminal) (onlyTerminalOutcome decided)
  assertEqual "one completion control index" (controlIndex 2) (oracleReceiptControlIndex receipt)
  assertEqual "one completion projection event" [10] (tags completion)
  assertEqual "completion releases the slot" Nothing (onlyOpenDecision completed)
  assertEqual "one historical decision" 1 (oracleCompletedWorkflowCount completed)
  assertEqual "completion never rewrites the overlay" (Just record) (oracleLabelRecord objectA completed)

caseAtomicNotApplied :: Assertion
caseAtomicNotApplied = do
  let request = requestId fixtureHeraldEpoch 1
      supplied = decisionEnvelope request fixtureProcessEpoch objectA (VoidLabel, 0) (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (failed, receipt, entry) = committedSubmission supplied initialState
      (_, terminal, _) = decisionEvent entry
      (next, _, _, _, _) = decideAt 2 fixtureHeraldEpoch fixtureProcessEpoch objectB initialPair targetVoid failed
  assertEqual "NotApplied is a canonical accepted decision" OracleAccepted (oracleReceiptResult receipt)
  assertNotApplied LivePriorLabelChanged terminal
  assertEqual "NotApplied leaves the overlay absent" Nothing (oracleLabelRecord objectA failed)
  assertEqual "NotApplied has no installation collection" Nothing (onlyOpenDecision failed)
  assertEqual "NotApplied is immediately historical" 1 (oracleCompletedWorkflowCount failed)
  assertEqual "no extra completion event" [2] (tags entry)
  assertEqual "the next request can decide immediately" (controlIndex 2) (oracleGreatestControlIndex next)

caseAtomicBusy :: Assertion
caseAtomicBusy = do
  let secondObject = objectA
  let first = decisionEnvelope (requestId fixtureHeraldEpoch 1) fixtureProcessEpoch objectA initialPair (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      second = decisionEnvelope (requestId fixtureHeraldEpoch 2) fixtureProcessEpoch secondObject initialPair (initialEvidence secondObject initialPair) Nothing Nothing targetVoid
      (occupied, receipt, _) = committedSubmission first initialState
      blocker = deriveLabelDecisionId fixtureSystemId (requestId fixtureHeraldEpoch 1)
      (deferredState, deferred) = submitOracleState second occupied
      (duplicateState, duplicate) = submitOracleState first occupied
      (completed, _, _) = completeAt 3 occupied
      (accepted, acceptedReceipt, acceptedEntry) = committedSubmission second completed
      (_, acceptedTerminal, _) = decisionEvent acceptedEntry
      (stillDeferred, repeatedDeferral) = submitOracleState second deferredState
      (retried, repeated) = submitOracleState second accepted
  assertEqual "both preflights can observe the original free state" [Nothing, Nothing] (fmap (`oracleSubmissionPreflightBlocker` initialState) [first, second])
  assertEqual "fresh busy preflight names the blocker" (Just blocker) (oracleSubmissionPreflightBlocker second occupied)
  assertEqual "serialized apply independently defers" (OracleSubmissionDeferredView (requestId fixtureHeraldEpoch 2) blocker (controlIndex 1)) (oracleSubmissionOutcomeView deferred)
  assertEqual "deferral leaves all canonical state unchanged" occupied deferredState
  assertEqual "a lost busy reply reoffers the exact request without state growth" deferredState stillDeferred
  assertEqual "repeated busy reply names the same blocker" (oracleSubmissionOutcomeView deferred) (oracleSubmissionOutcomeView repeatedDeferral)
  if secondObject == objectA then assertNotApplied LivePriorLabelChanged acceptedTerminal else assertApplied acceptedTerminal
  assertEqual "deferral creates no receipt" Nothing (oracleRequestReceipt (requestId fixtureHeraldEpoch 2) deferredState)
  assertEqual "retained exact retry precedes busy handling" (OracleSubmissionDuplicateView receipt) (oracleSubmissionOutcomeView duplicate)
  assertEqual "retained retry leaves state unchanged" occupied duplicateState
  assertEqual "same exact second identity is admitted later" (requestId fixtureHeraldEpoch 2) (oracleReceiptRequestId acceptedReceipt)
  assertEqual "deferred attempt consumed no Oracle index" (controlIndex 3) (oracleReceiptControlIndex acceptedReceipt)
  assertEqual "eventual receipt is stable" (OracleSubmissionDuplicateView acceptedReceipt) (oracleSubmissionOutcomeView repeated)
  assertEqual "duplicate changes no state" accepted retried

caseAtomicConcurrent :: Assertion
caseAtomicConcurrent = do
  let (first, _, _, decisionA, terminalA) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
      requestB = decisionEnvelope (requestId fixtureHeraldEpoch 2) fixtureProcessEpoch objectB initialPair (initialEvidence objectB initialPair) Nothing Nothing targetVoid
      (both, _, entryB) = committedSubmission requestB first
      (decisionBValue, terminalB, _) = decisionEvent entryB
      decisionB = liveDecisionId decisionBValue
      (onlyA, _, _) = completeDecisionAt 3 decisionB both
      (none, _, _) = completeDecisionAt 4 decisionA onlyA
  assertEqual "different object has no preflight blocker" Nothing (oracleSubmissionPreflightBlocker requestB first)
  assertEqual "both object slots remain pending" [Just decisionA, Just decisionB] (fmap (fmap liveDecisionId . (`oracleObjectDecision` both)) [objectA, objectB])
  assertEqual "first outcome unaffected by the second decision" (Just terminalA) (oracleTerminalOutcome decisionA both)
  assertEqual "second outcome has its own lookup" (Just terminalB) (oracleTerminalOutcome decisionB both)
  assertEqual "out-of-order completion clears only its object" [Just decisionA, Nothing] (fmap (fmap liveDecisionId . (`oracleObjectDecision` onlyA)) [objectA, objectB])
  assertEqual "all slots released" [] (oracleOpenDecisions none)
  assertEqual "both immutable completions retained" 2 (oracleCompletedWorkflowCount none)
  forM_ [first, both, onlyA, none] $ \state ->
    assertEqual "checkpoint rebuilds exact secondary indexes" (Right state) (Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (Checkpoint.oracleCheckpointBytes state))

caseAtomicConcurrentRetirement :: Assertion
caseAtomicConcurrentRetirement = do
  let (started, _, _) = commitAccepted 1 targetH4 (startProcessEpochCommand (processStart (processId 161) fixtureStartProcessEpoch targetH4)) (initialOracleState step15Genesis)
      (first, _, _, decisionA, terminalA) = decideAt 2 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid started
      supplied = decisionEnvelope (requestId targetH4 2) fixtureStartProcessEpoch objectB (VoidLabel, 0) (initialEvidence objectB (VoidLabel, 0)) Nothing Nothing (targetProcess fixtureStartProcessEpoch)
      (both, _, entryB) = committedSubmission supplied first
      (decisionBValue, terminalB, _) = decisionEvent entryB
      decisionB = liveDecisionId decisionBValue
      retired = retireLiveTarget targetH4 both
      (onlyA, _, _) = completeDecisionAt 10 decisionB retired
      (done, _, _) = completeDecisionAt 11 decisionA onlyA
  assertEqual "separate homes own their own collectors" [Just fixtureHeraldEpoch, Just targetH4] (fmap (`oracleLabelCollector` both) [decisionA, decisionB])
  assertEqual "retirement changes only the lost collector" [Just fixtureHeraldEpoch, Just fixtureHeraldEpoch] (fmap (`oracleLabelCollector` retired) [decisionA, decisionB])
  assertEqual "all outcomes remain immutable" [Just terminalA, Just terminalB] (fmap (`oracleTerminalOutcome` retired) [decisionA, decisionB])
  assertEqual "successor completion preserves unrelated pending work" [decisionA] (fmap liveDecisionId (oracleOpenDecisions onlyA))
  assertEqual "surviving collectors release both objects" [] (oracleOpenDecisions done)
  forM_ [both, retired, onlyA, done] $ \state ->
    assertEqual "retired checkpoint reconstructs participant and collector indexes" (Right state) (Checkpoint.decodeOracleCheckpoint step15Genesis (Checkpoint.oracleCheckpointBytes state))

propAtomicConcurrentCheckpoints :: [Bool] -> Property
propAtomicConcurrentCheckpoints choices =
  let count = 1 + length (take 7 choices)
      objects = map (globalObjectId . fromIntegral) [211 .. 210 + count]
      advance state (number, object) = let (next, _, _, _, _) = decideAt number fixtureHeraldEpoch fixtureProcessEpoch object initialPair targetVoid state in next
      opened = scanl advance initialState (zip [1 ..] objects)
      pending = last opened
      identities = map liveDecisionId (oracleOpenDecisions pending)
      completionOrder = order choices identities
      complete state (number, ident) = let (next, _, _) = completeDecisionAt number ident state in next
      completed = scanl complete pending (zip [fromIntegral count + 1 ..] completionOrder)
      states = opened <> drop 1 completed
      expectedRemaining = scanl (flip filterOut) identities completionOrder
      filterOut ident = filter (/= ident)
   in conjoin
        [ counterexample "each completion removes only its identity" (map (map liveDecisionId . oracleOpenDecisions) completed === expectedRemaining),
          counterexample "all pending and completed prefixes restore exact indexes" (all (\state -> Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (Checkpoint.oracleCheckpointBytes state) == Right state) states),
          counterexample "no global admission serialization" (length (oracleOpenDecisions pending) === count),
          counterexample "all decisions completed once" (oracleCompletedWorkflowCount (last completed) === count)
        ]
  where
    order [] values = values
    order _ [] = []
    order (reverseNext : rest) values = case if reverseNext then reverse values else values of
      first : remaining -> first : order rest remaining
      [] -> []

caseAtomicInitialEvidence :: Assertion
caseAtomicInitialEvidence = do
  let request = requestId fixtureHeraldEpoch 1
      make evidence = decisionEnvelope request fixtureProcessEpoch objectA initialPair evidence Nothing Nothing targetVoid
      (_, missing, _) = committedSubmission (make Nothing) initialState
      (_, different, _) = committedSubmission (make (initialEvidence objectB initialPair)) initialState
      wrongExpected = decisionEnvelope request fixtureProcessEpoch objectA (ProcessLabel fixtureProcessEpoch, 7) (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (failed, _, entry) = committedSubmission wrongExpected initialState
      (_, terminal, _) = decisionEvent entry
  assertEqual "first overlay needs independent evidence" (OracleRejected (OpenInitialLabelEvidenceMissing objectA)) (oracleReceiptResult missing)
  assertEqual "initial evidence is tied to the object" (OracleRejected (OpenInitialLabelEvidenceObjectMismatch objectA objectB)) (oracleReceiptResult different)
  assertEqual "nonzero raw generation cannot seed first evidence" Nothing (initialEvidence objectA (ProcessLabel fixtureProcessEpoch, 7))
  assertNotApplied LivePriorLabelChanged terminal
  assertEqual "expected generation never initializes the object" Nothing (oracleLabelRecord objectA failed)

caseAtomicInitialProvenance :: Assertion
caseAtomicInitialProvenance = do
  let request = requestId fixtureHeraldEpoch 1
      bootstrap = Just (initialBootstrapLabelEvidence objectA fixtureProcessEpoch)
      publication = initialEvidence objectA initialPair
      laterPublication = initialPublicationLabelEvidence objectA initialPair (publicationId (nablaId 181) genesisAuthorityEpoch fixtureHeraldEpoch (nablaSequence 2))
      make evidence = decisionEnvelope request fixtureProcessEpoch objectA initialPair evidence Nothing Nothing targetVoid
      (decided, receipt, _) = committedSubmission (make publication) initialState
      (done, _, _) = completeAt 2 decided
  forM_ [bootstrap, publication, laterPublication] $ \evidence -> do
    let supplied = make evidence
        (fresh, _, _) = committedSubmission supplied initialState
    assertEqual "each source initializes the same raw label" (Just (releasedLabel (VoidLabel, 1))) (labelRecordReleasedState <$> oracleLabelRecord objectA fresh)
    assertCodec "initial provenance" decodeOracleEnvelopeCanonicalBytes decodedOracleEnvelopeCanonicalBytes (oracleEnvelopeCanonicalBytes supplied)
  forM_ [decided, done] $ \state -> do
    let (same, duplicate) = submitOracleState (make publication) state
    assertEqual "unchanged provenance rediscovers the exact receipt" (OracleSubmissionDuplicateView receipt) (oracleSubmissionOutcomeView duplicate)
    assertEqual "duplicate preserves state" state same
    forM_ [bootstrap, laterPublication] $ \evidence -> do
      let (unchanged, conflict) = submitOracleState (make evidence) state
      assertBool "source alone changes the command digest" (oracleEnvelopeDigest (make evidence) /= oracleEnvelopeDigest (make publication))
      assertEqual "identity conflict preserves state" state unchanged
      case oracleSubmissionOutcomeView conflict of
        OracleSubmissionProtocolRejectedView {} -> pure ()
        other -> assertFailure ("changed provenance reused a request: " <> show other)

caseAtomicBootstrapAuthority :: Assertion
caseAtomicBootstrapAuthority = do
  let request = requestId fixtureHeraldEpoch 1
      authority = Just (liveExistingAuthority genesisAuthorityEpoch)
      justified = Just (checkedGenesisAuthority (checkedOracleInitialProjectionDigest fixtureCheckedGenesis))
      make suppliedAuthority justification target = decisionEnvelope request fixtureProcessEpoch objectA initialPair (Just (initialBootstrapLabelEvidence objectA fixtureProcessEpoch)) suppliedAuthority justification target
      (sameOwner, _, _) = committedSubmission (make authority justified (targetProcess fixtureProcessEpoch)) initialState
      (transferred, _, _) = committedSubmission (make authority justified (targetProcess fixtureRemoteProcessEpoch)) initialState
  assertEqual "same owner preserves bootstrap authority" authority (labelRecordRetainedAuthority (recordAt objectA sameOwner))
  assertEqual "handover derives authority at the decision index" (Just (labelAuthorityEpoch (controlIndex 1))) (labelRecordRetainedAuthority (recordAt objectA transferred))
  forM_ [targetProcess fixtureProcessEpoch, targetProcess fixtureRemoteProcessEpoch, targetVoid, targetDelete] $ \target -> do
    let (_, _, entry) = committedSubmission (make authority justified target) initialState
        (_, terminal, _) = decisionEvent entry
    case liveTerminalOutcomeView terminal of
      LiveReleasedOutcomeView facts _ _ _ -> assertEqual "portable facts reproduce retained, derived and retired authority" terminal (liveReleasedTerminalOutcomeFromFacts facts)
      other -> assertFailure ("expected authority-bearing release: " <> show other)
  forM_ [(authority, Nothing), (Nothing, justified)] $ \(suppliedAuthority, justification) -> do
    let (rejected, receipt, entry) = committedSubmission (make suppliedAuthority justification targetVoid) initialState
    assertEqual "authority and justification must agree" (OracleRejected OpenPriorAuthorityJustificationInvalid) (oracleReceiptResult receipt)
    assertEqual "invalid bootstrap authority creates no overlay" Nothing (oracleLabelRecord objectA rejected)
    assertEqual "rejection emits no label decision" [] (tags entry)

caseAtomicInitialEnd :: Assertion
caseAtomicInitialEnd = do
  let (ended, _, _) = commitAccepted 1 fixtureHeraldEpoch (checkedEndCommand fixtureProcessEpoch ExplicitAdministrativeEnd) initialState
      request = requestId fixtureRemoteHeraldEpoch 1
      make expected = decisionEnvelope request fixtureRemoteProcessEpoch objectA expected (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (failed, _, failureEntry) = committedSubmission (make initialPair) ended
      (_, failure, _) = decisionEvent failureEntry
      (applied, _, successEntry) = committedSubmission (make (ZombieLabel fixtureProcessEpoch, 0)) ended
      (_, success, _) = decisionEvent successEntry
  assertNotApplied LivePriorLabelChanged failure
  assertEqual "stale expected cannot override canonical End" Nothing (oracleLabelRecord objectA failed)
  assertApplied success
  assertEqual "zombie CAS succeeds using raw Process initial evidence" (Just (releasedLabel (VoidLabel, 1))) (labelRecordReleasedState <$> oracleLabelRecord objectA applied)

caseAtomicOverlay :: Assertion
caseAtomicOverlay = do
  let (first, _, _, _, _) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair (targetProcess fixtureProcessEpoch) initialState
      (completed, _, _) = completeAt 2 first
      retained = recordAt objectA completed
      make evidence authority justification = decisionEnvelope (requestId fixtureHeraldEpoch 3) fixtureProcessEpoch objectA (ProcessLabel fixtureProcessEpoch, 1) evidence authority justification targetVoid
      (plain, _, _) = committedSubmission (make Nothing Nothing Nothing) completed
      (staleHints, _, _) = committedSubmission (make (initialEvidence objectA (VoidLabel, 0)) (Just (liveExistingAuthority genesisAuthorityEpoch)) (Just (existingReleasedAuthority (labelRecordRevision retained)))) completed
  assertEqual "canonical object record determines result despite stale initial/authority hints" (oracleLabelRecord objectA plain) (oracleLabelRecord objectA staleHints)
  assertEqual "generation advances from canonical overlay" (Just (releasedLabel (VoidLabel, 2))) (labelRecordReleasedState <$> oracleLabelRecord objectA plain)

caseAtomicGeneration :: Assertion
caseAtomicGeneration = do
  let (first, _, _, _, _) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair (targetProcess fixtureProcessEpoch) initialState
      (firstDone, _, _) = completeAt 2 first
      (toVoid, _, _, _, _) = decideAt 3 fixtureHeraldEpoch fixtureProcessEpoch objectA (ProcessLabel fixtureProcessEpoch, 1) targetVoid firstDone
      (voidDone, _, _) = completeAt 4 toVoid
      (back, _, _, _, _) = decideAt 5 fixtureHeraldEpoch fixtureProcessEpoch objectA (VoidLabel, 2) (targetProcess fixtureProcessEpoch) voidDone
      (backDone, _, _) = completeAt 6 back
      (_, _, _, _, stale) = decideAt 7 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid backDone
  assertEqual "owner ABA retains generation three" (releasedLabel (ProcessLabel fixtureProcessEpoch, 3)) (labelRecordReleasedState (recordAt objectA backDone))
  assertNotApplied LivePriorLabelChanged stale

caseAtomicCrossHerald :: Assertion
caseAtomicCrossHerald = do
  let initialVoid = initialEvidence objectA (VoidLabel, 0)
      make home caller target = decisionEnvelope (requestId home 1) caller objectA (VoidLabel, 0) initialVoid Nothing Nothing target
      firstEnvelope = make fixtureHeraldEpoch fixtureProcessEpoch (targetProcess fixtureProcessEpoch)
      secondEnvelope = make fixtureRemoteHeraldEpoch fixtureRemoteProcessEpoch (targetProcess fixtureRemoteProcessEpoch)
      (first, _, _) = committedSubmission firstEnvelope initialState
      (_, blocked) = submitOracleState secondEnvelope first
      (done, _, _) = completeAt 2 first
      (second, _, entry) = committedSubmission secondEnvelope done
      (_, terminal, _) = decisionEvent entry
  case oracleSubmissionOutcomeView blocked of
    OracleSubmissionDeferredView {} -> pure ()
    other -> assertFailure ("second claim did not defer: " <> show other)
  assertNotApplied LivePriorLabelChanged terminal
  assertEqual "only the first caller owns generation one" (releasedLabel (ProcessLabel fixtureProcessEpoch, 1)) (labelRecordReleasedState (recordAt objectA second))

caseAtomicEndOrdering :: Assertion
caseAtomicEndOrdering = do
  let (targetEnded, _, _) = commitAccepted 1 fixtureRemoteHeraldEpoch (checkedEndCommand fixtureRemoteProcessEpoch ExplicitAdministrativeEnd) initialState
      (_, _, _, _, notApplied) = decideAt 2 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair (targetProcess fixtureRemoteProcessEpoch) targetEnded
      (callerEnded, _, _) = commitAccepted 1 fixtureHeraldEpoch (checkedEndCommand fixtureProcessEpoch ExplicitAdministrativeEnd) initialState
      (_, _, _, _, callerFailure) = decideAt 2 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid callerEnded
      (decided, _, _, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair (targetProcess fixtureRemoteProcessEpoch) initialState
      endEnvelope = envelope (requestId fixtureRemoteHeraldEpoch 2) (checkedEndCommand fixtureRemoteProcessEpoch ExplicitAdministrativeEnd)
      (endedAfter, _, _) = committedSubmission endEnvelope decided
      (completed, _, _) = completeAt 3 endedAfter
  assertNotApplied (LiveTargetProcessEnded fixtureRemoteProcessEpoch) notApplied
  assertNotApplied (LiveCallerProcessEnded fixtureProcessEpoch) callerFailure
  assertEqual "End never waits for installation collection" Nothing (oracleSubmissionPreflightBlocker endEnvelope decided)
  assertEqual "post-decision End preserves historical outcome" (Just terminal) (onlyTerminalOutcome endedAfter)
  assertEqual "stored owner remains historical" (releasedLabel (ProcessLabel fixtureRemoteProcessEpoch, 1)) (labelRecordReleasedState (recordAt objectA endedAfter))
  assertEqual "completion remains possible after End" Nothing (onlyOpenDecision completed)

caseAtomicDeletion :: Assertion
caseAtomicDeletion = do
  let (deleted, _, _, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetDelete initialState
      (done, _, _) = completeAt 2 deleted
      (after, _, _, _, rejected) = decideAt 3 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid done
  assertApplied terminal
  assertNotApplied LiveTransitionNoLongerPermitted rejected
  assertEqual "deleted overlay is terminal" (releasedDeleted 1) (labelRecordReleasedState (recordAt objectA after))

caseAtomicCompletion :: Assertion
caseAtomicCompletion = do
  let (decided, _, _, decision, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid (initialOracleState step15Genesis)
      digest = deriveLabelOutcomeDigest terminal
      generation = heraldMembershipGenerationId (oracleCurrentMembership decided)
      command collector gen value = completeLabelDecisionCommand (labelCompletionAttestation decision (terminalIndex terminal) value collector gen)
      (_, wrong, _) = committedSubmission (envelope (requestId fixtureRemoteHeraldEpoch 1) (command fixtureRemoteHeraldEpoch generation digest)) decided
      (unrelated, _, _) = commitAccepted 2 fixtureHeraldEpoch (startProcessEpochCommand fixtureStart) decided
      (completed, _, _) = committedSubmission (envelope (requestId fixtureHeraldEpoch 3) (command fixtureHeraldEpoch generation digest)) unrelated
      retired = retireLiveTarget targetH4 decided
      current = heraldMembershipGenerationId (oracleCurrentMembership retired)
      staleEnvelope = envelope (requestId fixtureHeraldEpoch 4) (command fixtureHeraldEpoch generation digest)
      (staleState, stale, staleEntry) = committedSubmission staleEnvelope retired
      (replayed, retry) = submitOracleState staleEnvelope staleState
      (done, _, _) = committedSubmission (envelope (requestId fixtureHeraldEpoch 5) (command fixtureHeraldEpoch current digest)) replayed
      restored = checked "completed membership checkpoint" (Checkpoint.decodeOracleCheckpoint step15Genesis (Checkpoint.oracleCheckpointBytes done))
      (newDecision, _, _, newIdentity, _) = decideAt 6 fixtureHeraldEpoch fixtureProcessEpoch objectB initialPair targetVoid restored
      (rediscovered, _, oldEntry) = committedSubmission (envelope (requestId fixtureRemoteHeraldEpoch 6) (command fixtureRemoteHeraldEpoch generation digest)) newDecision
      bad = checked "bad outcome digest" (mkLabelOutcomeDigest (ByteString.replicate 32 0xfe))
      (_, wrongDigest, _) = committedSubmission (envelope (requestId fixtureRemoteHeraldEpoch 7) (command fixtureRemoteHeraldEpoch generation bad)) rediscovered
  assertEqual "wrong collector rejected" (OracleRejected (LabelCompletionCollectorMismatch fixtureHeraldEpoch fixtureRemoteHeraldEpoch)) (oracleReceiptResult wrong)
  assertEqual "unrelated canonical progress does not stale completion" Nothing (onlyOpenDecision completed)
  assertEqual "membership changes do stale completion" (OracleRejected (LabelCompletionMembershipGenerationMismatch current generation)) (oracleReceiptResult stale)
  assertEqual "stale completion publishes no semantic event" [] (tags staleEntry)
  assertEqual "exact stale retry retains its result" (OracleSubmissionDuplicateView stale) (oracleSubmissionOutcomeView retry)
  assertEqual "exact retry preserves state" staleState replayed
  assertEqual "historical completion can be rediscovered" [10] (tags oldEntry)
  assertEqual "historical rediscovery cannot close a newer decision" (Just newIdentity) (liveDecisionId <$> onlyOpenDecision rediscovered)
  assertEqual "historical retry still binds digest" (OracleRejected (LabelCompletionDigestMismatch digest bad)) (oracleReceiptResult wrongDigest)

-- The request cache can be entirely gone while a successor collector still
-- needs semantic rediscovery by decision/outcome. Both terminal branches keep
-- that witness, including when no install-completion command was necessary.
caseAtomicCompletedWitness :: Assertion
caseAtomicCompletedWitness = forM_ [OracleStateDigestDisabled, OracleStateDigestEnabled] $ \mode ->
  forM_ [False, True] $ \applied -> do
    let initial = initialOracleStateWithDigestMode mode fixtureCheckedGenesis
        expected = if applied then initialPair else (ProcessLabel fixtureProcessEpoch, 99)
        (decided, _, _, decision, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA expected targetVoid initial
        completed = if applied then let (next, _, _) = completeAt 2 decided in next else decided
        retire = envelope (requestId fixtureHeraldEpoch 2) (retireOracleReceiptsCommand 2)
        (retired, retirement) = submitOracleState retire completed
        restored = checked "compact completion checkpoint" (Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (Checkpoint.oracleCheckpointBytes retired))
        digest = deriveLabelOutcomeDigest terminal
        generation = heraldMembershipGenerationId (oracleCurrentMembership decided)
        command reporter value = completeLabelDecisionCommand (labelCompletionAttestation decision (terminalIndex terminal) value reporter generation)
        supplied = envelope (requestId fixtureRemoteHeraldEpoch 10) (command fixtureRemoteHeraldEpoch digest)
        (rediscovered, receipt, entry) = committedSubmission supplied restored
        (same, duplicate) = submitOracleState supplied rediscovered
        bad = checked "wrong completed digest" (mkLabelOutcomeDigest (ByteString.replicate 32 0xfd))
        (_, wrongDigest, _) = committedSubmission (envelope (requestId fixtureRemoteHeraldEpoch 11) (command fixtureRemoteHeraldEpoch bad)) rediscovered
        (_, wrongHome, _) = committedSubmission (envelope (requestId fixtureHeraldEpoch 12) (command fixtureRemoteHeraldEpoch digest)) rediscovered
    case oracleSubmissionOutcomeView retirement of
      OracleSubmissionProgressRetiredView {} -> pure ()
      other -> assertFailure ("expected receipt retirement: " <> show other)
    assertEqual "all former command receipts are gone" 0 (oracleRequestCount restored)
    assertEqual "checkpoint preserves the witness exactly" retired restored
    assertEqual "completed witness uses a fixed 104-byte entry" (Just 112) (lookup "completed_workflows" (Checkpoint.oracleStateDiagnosticSections restored))
    assertEqual "no full transcript encoding is retained" (Just 0) (lookup "completed_workflow_cached_bytes" (Checkpoint.oracleStateDiagnosticCardinalities restored))
    assertEqual "captured successor can rediscover either outcome" OracleAccepted (oracleReceiptResult receipt)
    assertEqual "rediscovery repeats the same decision/digest event" [LabelWorkflowCompletedView decision digest] (map oracleProjectionEventView (appliedEntryProjectionEvents entry))
    assertEqual "rediscovery changes no label" (oracleLabelRecords restored) (oracleLabelRecords rediscovered)
    assertEqual "exact retry still retains its result" (OracleSubmissionDuplicateView receipt) (oracleSubmissionOutcomeView duplicate)
    assertEqual "exact retry preserves state" rediscovered same
    assertEqual "wrong outcome cannot rediscover completion" (OracleRejected (LabelCompletionDigestMismatch digest bad)) (oracleReceiptResult wrongDigest)
    assertEqual "reporter must still match submitting home" (OracleRejected (ReporterHomeMismatch fixtureRemoteHeraldEpoch fixtureHeraldEpoch)) (oracleReceiptResult wrongHome)

caseAtomicCompletedMembership :: Assertion
caseAtomicCompletedMembership = do
  let initial = initialOracleStateWithDigestMode OracleStateDigestDisabled fixtureCheckedGenesis
      (decided, _, _, decision, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initial
      (completed, _, _) = completeAt 2 decided
      bytes = Checkpoint.oracleCheckpointBytes completed
      generation = heraldMembershipGenerationIdBytes (heraldMembershipGenerationId (oracleCurrentMembership decided))
      entry = labelDecisionIdBytes decision <> Put.runPut (Put.putWord64be (controlIndexWord64 (terminalIndex terminal))) <> generation <> labelOutcomeDigestBytes (deriveLabelOutcomeDigest terminal)
      (prefix, suffix) = ByteString.breakSubstring entry bytes
      corrupted = prefix <> ByteString.take 40 suffix <> ByteString.replicate 32 0xff <> ByteString.drop 72 suffix
  assertBool "compact witness is present in the checkpoint" (not (ByteString.null suffix))
  assertBool "unknown captured generation cannot enter through a checkpoint" (isLeft (Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis corrupted))

caseAtomicCollectorRetirement :: Assertion
caseAtomicCollectorRetirement = do
  let (started, _, _) = commitAccepted 1 targetH4 (startProcessEpochCommand (processStart (processId 161) fixtureStartProcessEpoch targetH4)) (initialOracleState step15Genesis)
      supplied = decisionEnvelope (requestId targetH4 2) fixtureStartProcessEpoch objectA (VoidLabel, 0) (initialEvidence objectA (VoidLabel, 0)) Nothing Nothing (targetProcess fixtureStartProcessEpoch)
      (decided, _, _) = committedSubmission supplied started
      retired = retireLiveTarget targetH4 decided
      (done, _, entry) = completeAt 10 retired
  assertEqual "active home is collector" (Just targetH4) (onlyLabelCollector decided)
  assertEqual "minimum captured survivor takes over" (Just fixtureHeraldEpoch) (onlyLabelCollector retired)
  assertEqual "retirement never rewrites decision outcome" (onlyTerminalOutcome decided) (onlyTerminalOutcome retired)
  assertEqual "successor collector closes without old caller" Nothing (onlyOpenDecision done)
  assertEqual "one completion" [10] (tags entry)

caseAtomicRepeatedRetirement :: Assertion
caseAtomicRepeatedRetirement = do
  let (decided, _, _, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid (initialOracleState step15Genesis)
      first = retireLiveTarget targetH4 decided
      second = retireLiveTarget targetH5 first
      (done, _, _) = completeAt 100 second
  forM_ [first, second] $ \state -> assertEqual "contractions preserve the terminal bytes" (Just terminal) (onlyTerminalOutcome state)
  assertEqual "survivors complete one immutable decision" 1 (oracleCompletedWorkflowCount done)
  assertEqual "closed collection" Nothing (onlyOpenDecision done)

caseAtomicVoterFailure :: Assertion
caseAtomicVoterFailure = do
  let (started, _, _) = commitAccepted 1 fixtureThirdHeraldEpoch (startProcessEpochCommand (processStart (processId 161) fixtureStartProcessEpoch fixtureThirdHeraldEpoch)) (initialOracleState step15Genesis)
      (decided, _, _, _, terminal) = decideAt 2 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair (targetProcess fixtureStartProcessEpoch) started
      (excluded, certificate, _) = VF.excludeVoterHost fixtureThirdHeraldEpoch decided
      (retired, retirementEntry) = VF.retireExcludedHost certificate excluded
      contracted = retireLiveTarget targetH5 (retireLiveTarget targetH4 retired)
      (done, _, _) = completeAt 100 contracted
  assertEqual "native exclusion does not End the child" (Just ProcessRecordLiveView) (processRecordLifecycle <$> oracleProcessRecord fixtureStartProcessEpoch excluded)
  assertEqual "semantic retirement Ends the child at its canonical index" (Just (ProcessRecordEndedView (appliedEntryControlIndex retirementEntry) HeraldRetired)) (processRecordLifecycle <$> oracleProcessRecord fixtureStartProcessEpoch retired)
  forM_ [excluded, retired, contracted] $ \state -> do
    assertEqual "membership changes preserve accepted decision" (Just terminal) (onlyTerminalOutcome state)
    assertEqual "historical transferred owner stays stored" (releasedLabel (ProcessLabel fixtureStartProcessEpoch, 1)) (labelRecordReleasedState (recordAt objectA state))
  assertEqual "surviving collector completes" Nothing (onlyOpenDecision done)

caseAtomicCanonicalCommands :: Assertion
caseAtomicCanonicalCommands = do
  let request = requestId fixtureHeraldEpoch 1
      decision = deriveLabelDecisionId fixtureSystemId request
      cut = homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix
      command = decideLabelCommand decision fixtureProcessEpoch objectA initialPair Nothing Nothing Nothing targetVoid cut
      expected =
        ByteString.singleton 2
          <> framed
            ( Put.runPut $ do
                Put.putByteString (labelDecisionIdBytes decision)
                Put.putByteString (processEpochIdBytes fixtureProcessEpoch)
                Put.putByteString (globalObjectIdBytes objectA)
                putFramed (canonicalValueByteString (canonicalValueBytes (labelValue initialPair)))
                Put.putWord8 0
                Put.putWord8 0
                Put.putWord8 0
                Put.putWord8 1
                putFramed (homeLabelAcceptanceCutCanonicalBytes cut)
            )
      (decided, _, _, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
      generation = heraldMembershipGenerationId (oracleCurrentMembership decided)
      completion = completeLabelDecisionCommand (labelCompletionAttestation decision (terminalIndex terminal) (deriveLabelOutcomeDigest terminal) fixtureHeraldEpoch generation)
      completionBytes = ByteString.singleton 8 <> framed (labelDecisionIdBytes decision <> Put.runPut (Put.putWord64be (controlIndexWord64 (terminalIndex terminal))) <> labelOutcomeDigestBytes (deriveLabelOutcomeDigest terminal) <> heraldEpochBytes fixtureHeraldEpoch <> heraldMembershipGenerationIdBytes generation)
  assertEqual "DecideLabel encodes no caller-selected prior revision" expected (oracleCommandCanonicalBytes command)
  assertEqual "compact completion binds original index and four identities" completionBytes (oracleCommandCanonicalBytes completion)
  assertEqual "fixed completion byte count" 145 (ByteString.length completionBytes)

caseAtomicRemovedTags :: Assertion
caseAtomicRemovedTags = do
  let supplied = decisionEnvelope (requestId fixtureHeraldEpoch 1) fixtureProcessEpoch objectA initialPair (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      bytes = oracleEnvelopeCanonicalBytes supplied
      offset = 8 + ByteString.length "ECLIPS-ORACLE-ENVELOPE" + 40 + 1 + 32
  forM_ [3 .. 7] $ \tag ->
    assertBool ("removed command tag " <> show tag <> " cannot decode") (isLeft (decodeOracleEnvelopeCanonicalBytes (replaceByte offset tag bytes)))
  forM_ [3 .. 9] $ \tag -> do
    let bytes' = Put.runPut $ do
          putFramed "ECLIPS-ORACLE-PROJECTION-EVENT-VECTOR"
          Put.putWord64be 1
          Put.putWord8 tag
          putFramed ByteString.empty
    assertBool ("removed projection tag " <> show tag <> " cannot decode") (isLeft (decodeOracleProjectionEventVectorCanonicalBytes bytes'))

caseAtomicCanonicalRoundTrips :: Assertion
caseAtomicCanonicalRoundTrips = do
  let first = decisionEnvelope (requestId fixtureHeraldEpoch 1) fixtureProcessEpoch objectA initialPair (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (decided, firstReceipt, firstEntry) = committedSubmission first initialState
      completion = envelope (requestId fixtureHeraldEpoch 2) (completionCommand decided)
      (_, completedReceipt, completedEntry) = committedSubmission completion decided
      bad = envelope (requestId fixtureHeraldEpoch 3) (completeLabelDecisionCommand (labelCompletionAttestation (deriveLabelDecisionId fixtureSystemId (requestId fixtureHeraldEpoch 99)) (controlIndex 1) (deriveLabelOutcomeDigest (terminalAt decided)) fixtureHeraldEpoch (heraldMembershipGenerationId (oracleCurrentMembership decided))))
      (_, rejectedReceipt, rejectedEntry) = committedSubmission bad initialState
      (_, _, failureEntry, _, _) = decideAt 4 fixtureHeraldEpoch fixtureProcessEpoch objectA (VoidLabel, 9) targetVoid initialState
  forM_ [first, completion, bad] $ \value ->
    assertCodec "envelope" decodeOracleEnvelopeCanonicalBytes decodedOracleEnvelopeCanonicalBytes (oracleEnvelopeCanonicalBytes value)
  forM_ [firstReceipt, completedReceipt, rejectedReceipt] $ \value ->
    assertCodec "receipt" decodeOracleReceiptCanonicalBytes decodedOracleReceiptCanonicalBytes (oracleReceiptCanonicalBytes value)
  forM_ [firstEntry, completedEntry, rejectedEntry, failureEntry] $ \value -> do
    assertCodec "entry" decodeAppliedOracleEntryCanonicalBytes decodedAppliedOracleEntryCanonicalBytes (appliedOracleEntryCanonicalBytes value)
    assertCodec "events" decodeOracleProjectionEventVectorCanonicalBytes decodedOracleProjectionEventVectorCanonicalBytes (oracleProjectionEventVectorCanonicalBytes (appliedEntryProjectionEvents value))

caseAtomicCanonicalOptionals :: Assertion
caseAtomicCanonicalOptionals = do
  let evidence = [Nothing, Just (initialBootstrapLabelEvidence objectA fixtureProcessEpoch), initialEvidence objectA initialPair]
      authorities = [(Nothing, Nothing), (Just (liveExistingAuthority genesisAuthorityEpoch), Just (checkedGenesisAuthority (checkedOracleInitialProjectionDigest fixtureCheckedGenesis)))]
  forM_ evidence $ \initial ->
    forM_ authorities $ \(authority, justification) ->
      forM_ [targetVoid, targetDelete, targetProcess fixtureRemoteProcessEpoch] $ \target -> do
        let supplied = decisionEnvelope (requestId fixtureHeraldEpoch 1) fixtureProcessEpoch objectA initialPair initial authority justification target
        assertCodec "optional decision fields" decodeOracleEnvelopeCanonicalBytes decodedOracleEnvelopeCanonicalBytes (oracleEnvelopeCanonicalBytes supplied)

caseAtomicCanonicalCoherence :: Assertion
caseAtomicCanonicalCoherence = do
  let supplied = decisionEnvelope (requestId fixtureHeraldEpoch 1) fixtureProcessEpoch objectA initialPair (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (decided, receipt, entry) = committedSubmission supplied initialState
      unknown = deriveLabelDecisionId fixtureSystemId (requestId fixtureHeraldEpoch 99)
      wrong = completeLabelDecisionCommand (labelCompletionAttestation unknown (controlIndex 1) (deriveLabelOutcomeDigest (terminalAt decided)) fixtureHeraldEpoch (heraldMembershipGenerationId (oracleCurrentMembership decided)))
      (_, rejectedReceipt, rejectedEntry) = committedSubmission (envelope (requestId fixtureHeraldEpoch 2) wrong) initialState
      envelopeBytes = oracleEnvelopeCanonicalBytes supplied
      receiptBytes = oracleReceiptCanonicalBytes receipt
      rejectionBytes = oracleReceiptCanonicalBytes rejectedReceipt
      events = oracleProjectionEventVectorCanonicalBytes (appliedEntryProjectionEvents entry)
      commandOffset = 8 + ByteString.length "ECLIPS-ORACLE-ENVELOPE" + 40 + 1 + 32
      resultOffset = 8 + ByteString.length "ECLIPS-ORACLE-RECEIPT" + 80
      eventOffset = 8 + ByteString.length "ECLIPS-ORACLE-PROJECTION-EVENT-VECTOR" + 8
      indexOffset = 8 + ByteString.length "ECLIPS-APPLIED-ORACLE-ENTRY" + 7
      emptyCommand = ByteString.take (commandOffset + 1) envelopeBytes <> ByteString.replicate 8 0
      emptyEvent = ByteString.take (eventOffset + 1) events <> ByteString.replicate 8 0
      duplicateEvents = framed "ECLIPS-ORACLE-PROJECTION-EVENT-VECTOR" <> Put.runPut (Put.putWord64be 2) <> ByteString.drop eventOffset events <> ByteString.drop eventOffset events
      corruptedDigest = replaceByte (ByteString.length events - 1) (ByteString.last events + 1) events
      entryWithEvents original vector = Put.runPut $ do
        let command = maybe (error "fixture must be a command entry") id (appliedEntryCommand original)
            request = appliedEntryRequestId command
        putFramed "ECLIPS-APPLIED-ORACLE-ENTRY"
        Put.putWord64be (controlIndexWord64 (appliedEntryControlIndex original))
        Put.putByteString (heraldEpochBytes (oracleClientRequestHome request))
        Put.putWord64be (oracleClientRequestSequence request)
        Put.putByteString (oracleCommandDigestBytes (appliedEntryCommandDigest command))
        putFramed (oracleReceiptCanonicalBytes (appliedEntryReceipt command))
        putFramed vector
        case appliedEntryPostStateDigest original of
          Nothing -> Put.putWord8 0
          Just digest -> Put.putWord8 1 >> Put.putByteString (oracleStateDigestBytes digest)
  assertEqual "unknown command tag" (Left (UnknownOracleCommandTag 0xff)) (decodeOracleEnvelopeCanonicalBytes (replaceByte commandOffset 0xff envelopeBytes))
  assertEqual "unknown rejection tag" (Left (UnknownOracleRejectionTag 0xff)) (decodeOracleReceiptCanonicalBytes (replaceByte (resultOffset + 1) 0xff rejectionBytes))
  assertEqual "unknown event tag" (Left (UnknownProjectionEventTag 0xff)) (decodeOracleProjectionEventVectorCanonicalBytes (replaceByte eventOffset 0xff events))
  assertEqual "duplicate atomic event" (Left (DuplicateProjectionEventTag 2)) (decodeOracleProjectionEventVectorCanonicalBytes duplicateEvents)
  assertBool "empty known command is malformed" (isLeft (decodeOracleEnvelopeCanonicalBytes emptyCommand))
  assertBool "truncated known rejection is malformed" (isLeft (decodeOracleReceiptCanonicalBytes (ByteString.init rejectionBytes)))
  assertBool "empty known event is malformed" (isLeft (decodeOracleProjectionEventVectorCanonicalBytes emptyEvent))
  assertBool "invalid receipt result is malformed" (isLeft (decodeOracleReceiptCanonicalBytes (replaceByte resultOffset 0xff receiptBytes)))
  assertBool "outcome digest is checked against the immutable terminal" (isLeft (decodeOracleProjectionEventVectorCanonicalBytes corruptedDigest))
  assertBool "malformed event cannot enter an applied entry" (isLeft (decodeAppliedOracleEntryCanonicalBytes (entryWithEvents entry emptyEvent)))
  assertEqual "independent entry transcript" (appliedOracleEntryCanonicalBytes entry) (entryWithEvents entry events)
  assertEqual "entry and receipt must share the decision index" (Left InconsistentCanonicalOracleEntry) (decodeAppliedOracleEntryCanonicalBytes (replaceByte indexOffset 2 (appliedOracleEntryCanonicalBytes entry)))
  assertEqual "a rejected entry cannot carry a decision" (Left InconsistentCanonicalOracleEntry) (decodeAppliedOracleEntryCanonicalBytes (entryWithEvents rejectedEntry events))

-- Each value comes from normal canonical admission. The generated chain covers
-- absent and present prior revisions without retaining or replaying a checkpoint.
propPortableLabelFacts :: Positive Word64 -> Property
propPortableLabelFacts (Positive seed) = conjoin (go 0 initialState)
  where
    count = 1 + seed `mod` 8
    go generation state
      | generation == count = []
      | otherwise =
          let request = 2 * generation + 1
              (decided, _, entry, _, terminal) = decideAt request fixtureHeraldEpoch fixtureProcessEpoch objectA (ProcessLabel fixtureProcessEpoch, generation) (targetProcess fixtureProcessEpoch) state
              (decision, _, _) = decisionEvent entry
              (completed, _, _) = completeAt (request + 1) decided
              decodedTerminal = Canonical.decodeLabelTerminalOutcome (Canonical.encodeLabelTerminalOutcome terminal)
           in counterexample
                ("portable facts at generation " <> show generation)
                ( conjoin
                    [ Canonical.decodeLabelDecision (Canonical.encodeLabelDecision decision) === Right decision,
                      decodedTerminal === Right terminal,
                      fmap deriveLabelOutcomeDigest decodedTerminal === Right (deriveLabelOutcomeDigest terminal)
                    ]
                )
                : go (generation + 1) completed

casePortableFactShapes :: Assertion
casePortableFactShapes = do
  forM_ [targetProcess fixtureProcessEpoch, targetProcess fixtureRemoteProcessEpoch, targetVoid, targetDelete] $ \target -> do
    let (_, _, entry, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair target initialState
        (decision, _, _) = decisionEvent entry
    assertEqual "portable decision" (Right decision) (Canonical.decodeLabelDecision (Canonical.encodeLabelDecision decision))
    assertEqual "portable released overlay and facts" (Right terminal) (Canonical.decodeLabelTerminalOutcome (Canonical.encodeLabelTerminalOutcome terminal))
    case liveTerminalOutcomeView terminal of
      LiveReleasedOutcomeView facts _ _ _ -> assertEqual "checked facts derive the exact canonical released terminal" terminal (liveReleasedTerminalOutcomeFromFacts facts)
      other -> assertFailure ("expected released derivation fixture: " <> show other)
  let (_, _, _, _, rejected) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA (VoidLabel, 9) targetVoid initialState
  case liveTerminalOutcomeView rejected of
    LiveNotAppliedOutcomeView decision index object _ catalogue members ->
      forM_ [LiveCallerProcessEnded fixtureProcessEpoch, LiveTargetProcessEnded fixtureRemoteProcessEpoch, LivePriorLabelChanged, LiveTransitionNoLongerPermitted] $ \reason -> do
        let terminal = checked "portable NotApplied reason" (liveNotAppliedTerminalOutcome decision index object reason catalogue members)
        assertEqual "portable NotApplied reason" (Right terminal) (Canonical.decodeLabelTerminalOutcome (Canonical.encodeLabelTerminalOutcome terminal))
    other -> assertFailure ("expected NotApplied portable fixture: " <> show other)
  forM_ portableProcessRecords $ \record ->
    assertEqual "portable process lifecycle" (Right record) (Canonical.decodeProcessEpochRecord (Canonical.encodeProcessEpochRecord record))

portableProcessRecords :: [ProcessEpochRecord]
portableProcessRecords =
  let (started, _, _) = commitAccepted 90 fixtureHeraldEpoch (startProcessEpochCommand fixtureStart) initialState
      (ended, _, _) = commitAccepted 91 fixtureHeraldEpoch (checkedEndCommand fixtureStartProcessEpoch ExplicitAdministrativeEnd) started
      (genesisEnded, _, _) = commitAccepted 92 fixtureHeraldEpoch (checkedEndCommand fixtureProcessEpoch ExplicitAdministrativeEnd) ended
      record process state = maybe (error "missing portable process record") id (oracleProcessRecord process state)
   in [record fixtureProcessEpoch initialState, record fixtureStartProcessEpoch started, record fixtureStartProcessEpoch ended, record fixtureProcessEpoch genesisEnded]

casePortableFactCoherence :: Assertion
casePortableFactCoherence = do
  let (_, _, entry, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
      (decision, _, _) = decisionEvent entry
      decisionBytes = Canonical.encodeLabelDecision decision
      terminalBytes = Canonical.encodeLabelTerminalOutcome terminal
      decisionHeader = 8 + ByteString.length "ECLIPS-ORACLE-LABEL-DECISION"
      terminalHeader = 8 + ByteString.length "ECLIPS-ORACLE-LABEL-TERMINAL"
      processHeader = 8 + ByteString.length "ECLIPS-ORACLE-PROCESS-RECORD"
      replace offset replacement bytes = ByteString.take offset bytes <> replacement <> ByteString.drop (offset + ByteString.length replacement) bytes
      word value = Put.runPut (Put.putWord64be value)
      assertMalformed name decode encode value = do
        let bytes = encode value
        assertCodec name decode encode bytes
        forM_ [ByteString.empty, ByteString.init bytes] $ \malformed ->
          assertBool (name <> " rejects empty/truncated bytes") (isLeft (decode malformed))
  assertMalformed "portable decision" Canonical.decodeLabelDecision Canonical.encodeLabelDecision decision
  assertMalformed "portable terminal" Canonical.decodeLabelTerminalOutcome Canonical.encodeLabelTerminalOutcome terminal
  forM_ portableProcessRecords $ assertMalformed "portable process" Canonical.decodeProcessEpochRecord Canonical.encodeProcessEpochRecord
  assertBool "decision request and home agree" (isLeft (Canonical.decodeLabelDecision (replace (decisionHeader + 32 + 40) (heraldEpochBytes fixtureRemoteHeraldEpoch) decisionBytes)))
  assertBool "decision acceptance cut belongs to caller" (isLeft (Canonical.decodeLabelDecision (replace (decisionHeader + 32 + 40 + 32) (processEpochIdBytes fixtureRemoteProcessEpoch) decisionBytes)))
  assertBool "portable fact domains are distinct" (isLeft (Canonical.decodeLabelDecision terminalBytes) && isLeft (Canonical.decodeLabelTerminalOutcome decisionBytes))
  case liveTerminalOutcomeView terminal of
    LiveReleasedOutcomeView facts _ _ _ -> do
      let digestOffset = terminalHeader + 1 + 8 + ByteString.length (livePreparedLabelFactsCanonicalBytes facts)
      assertBool "prepared digest is checked independently" (isLeft (Canonical.decodeLabelTerminalOutcome (replaceByte digestOffset (ByteString.index terminalBytes digestOffset + 1) terminalBytes)))
      assertBool "release index agrees with prepared facts and overlay" (isLeft (Canonical.decodeLabelTerminalOutcome (replace (digestOffset + 32) (word 2) terminalBytes)))
    other -> assertFailure ("expected portable release: " <> show other)
  forM_ portableProcessRecords $ \record -> case (processRecordOrigin record, processRecordLifecycle record) of
    (DynamicStartView start _, ProcessRecordEndedView _ _) -> do
      let bytes = Canonical.encodeProcessEpochRecord record
      assertBool "dynamic Start request belongs to process residence" (isLeft (Canonical.decodeProcessEpochRecord (replace (processHeader + 96 + 1 + 8) (heraldEpochBytes fixtureRemoteHeraldEpoch) bytes)))
      assertBool "End strictly follows dynamic Start" (isLeft (Canonical.decodeProcessEpochRecord (replace (processHeader + 96 + 1 + 8 + 40 + 1) (word (controlIndexWord64 start)) bytes)))
    _ -> pure ()

casePortableFactBuffers :: Assertion
casePortableFactBuffers = do
  let (_, _, entry, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
      (decision, _, _) = decisionEvent entry
  assertOwnedFact "decision" Canonical.encodeLabelDecision Canonical.decodeLabelDecision decision $ \value ->
    [labelDecisionIdBytes (liveDecisionId value), heraldEpochBytes (liveDecisionHome value), processEpochIdBytes (liveDecisionCaller value), globalObjectIdBytes (liveDecisionObject value)]
  assertOwnedFact "terminal" Canonical.encodeLabelTerminalOutcome Canonical.decodeLabelTerminalOutcome terminal $ \value -> case liveTerminalOutcomeView value of
    LiveReleasedOutcomeView facts digest _ _ -> [labelDecisionIdBytes (livePreparedFactsDecisionId facts), globalObjectIdBytes (livePreparedFactsObjectId facts), preparedLabelDigestBytes digest]
    LiveNotAppliedOutcomeView ident _ object _ _ members -> [labelDecisionIdBytes ident, globalObjectIdBytes object, memberSetDigestBytes members]
  forM_ portableProcessRecords $ \record ->
    assertOwnedFact "process" Canonical.encodeProcessEpochRecord Canonical.decodeProcessEpochRecord record $ \value ->
      [processIdBytes (processRecordProcessId value), processEpochIdBytes (processRecordProcessEpoch value), heraldEpochBytes (processRecordResidence value)]

assertOwnedFact :: (Eq value, Show value, Show problem) => String -> (value -> ByteString.ByteString) -> (ByteString.ByteString -> Either problem value) -> value -> (value -> [ByteString.ByteString]) -> Assertion
assertOwnedFact name encode decode value retainedBuffers = do
  let encoded = encode value
      padding = ByteString.replicate 4096 0
      outer = ByteString.copy (padding <> encoded <> padding)
      bytes = ByteString.take (ByteString.length encoded) (ByteString.drop (ByteString.length padding) outer)
      decoded = checked ("owned portable " <> name) (decode bytes)
      (inputBuffer, inputOffset, inputLength) = ByteStringInternal.toForeignPtr outer
  assertEqual (name <> " value survives owned admission") value decoded
  withForeignPtr inputBuffer $ \inputPointer -> do
    let inputStart = ptrToIntPtr inputPointer + fromIntegral inputOffset
        inputEnd = inputStart + fromIntegral inputLength
    forM_ (retainedBuffers decoded) $ \retained -> do
      let (buffer, offset, count) = ByteStringInternal.toForeignPtr retained
      withForeignPtr buffer $ \pointer -> do
        let start = ptrToIntPtr pointer + fromIntegral offset
            end = start + fromIntegral count
        assertBool (name <> " retained identity shares enclosing allocation") (end <= inputStart || start >= inputEnd)

caseAtomicDigestPreimages :: Assertion
caseAtomicDigestPreimages = do
  let (state, _, _, _, terminal) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initialState
  assertEqual "terminal digest hashes exactly its canonical bytes" (SHA256.hash (liveTerminalOutcomeCanonicalBytes terminal)) (labelOutcomeDigestBytes (deriveLabelOutcomeDigest terminal))
  assertEqual "state digest hashes exactly its canonical bytes" (Just (SHA256.hash (oracleStateCanonicalBytes state))) (oracleStateDigestBytes <$> oracleStateDigest state)
  case liveTerminalOutcomeView terminal of
    LiveReleasedOutcomeView facts digest index record -> do
      assertEqual "decision facts bind the same canonical control point" index (livePreparedFactsResolveIndex facts)
      assertEqual "installed revision uses the decision point" index (labelRevisionControlIndex (liveRecordRevision record))
      assertEqual "prepared facts digest is exact" (SHA256.hash (livePreparedLabelFactsCanonicalBytes facts)) (preparedLabelDigestBytes digest)
    other -> assertFailure ("expected Applied: " <> show other)

caseAtomicCheckpoints :: Assertion
caseAtomicCheckpoints = forM_ [OracleStateDigestDisabled, OracleStateDigestEnabled] $ \mode -> do
  let states = atomicStates mode
  forM_ states $ \(name, state) -> do
    let bytes = Checkpoint.oracleCheckpointBytes state
    assertEqual (name <> " restores exact state in " <> show mode) (Right state) (Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis bytes)
    assertBool (name <> " rejects a trailing checkpoint byte") (isLeft (Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (bytes <> "x")))
  assertEqual
    (show mode <> " checkpoint fingerprints")
    (checkpointVectors mode)
    [(name, ByteString.length bytes, hexBytes (SHA256.hash bytes)) | (name, state) <- states, let bytes = Checkpoint.oracleCheckpointBytes state]

-- Named fingerprints cover every retained atomic state shape in both modes.
-- L and R add sixteen bytes; three active-home promises add 3 * (32 + 9 + 8)
-- bytes to the existing count framing. Each compact witness adds its original
-- eight-byte decision index. Enabled checkpoints retain another 32-byte digest.
checkpointVectors :: OracleStateDigestMode -> [(String, Int, String)]
checkpointVectors OracleStateDigestDisabled = [("initial", 2434, "d014fef16af5f66e26d82c45077f3cd5ab1d1c5327859ccf02ef26f45c0029b1"), ("pending-installation", 3742, "664c70d84ba67c621aa778a1eb9e9e288f95ce7caa084f3341390eead5d1fb21"), ("completed-applied", 2986, "9a41063bebb5a06c494efd223af6ee30eb4807b4e147d570bf9399ee2f0595ca"), ("completed-not-applied", 3280, "18e40ae4db06c3ce86a33d4fa3605b1e836c420029a0ae2faebd580768557bc2"), ("ended", 3520, "caef30ce9e95405351ac3d32c73250c8609fe4941c7cdb2dfa6a5ca4774829bd")]
checkpointVectors OracleStateDigestEnabled = [("initial", 2466, "440516b7e86e2d33d313ebb012dc9a57fccc44cc56a4ae76ef7136b32fd94a7d"), ("pending-installation", 3774, "2a3f9c675b35fce09076f3f8b7eea6d40f22bb05a017b4c1687745627f6c5df6"), ("completed-applied", 3018, "502f3a8de7a4284f70a9e6bf4c9d476e2a26d22744066fa74948ee50b1ec8ef7"), ("completed-not-applied", 3312, "3944b084a756e6fdec2085e1806e7849ce260ed0dbba951b5be815296d0bb0af"), ("ended", 3552, "34992f3b6e554684695eefb7d213c27a736387443665f98145ed37a75aef2af5")]

atomicStates :: OracleStateDigestMode -> [(String, OracleState)]
atomicStates mode =
  let initial = initialOracleStateWithDigestMode mode fixtureCheckedGenesis
      (decided, _, _, _, _) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initial
      (completed, _, _) = completeAt 2 decided
      (failed, _, _, _, _) = decideAt 3 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid completed
      (ended, _, _) = commitAccepted 4 fixtureHeraldEpoch (checkedEndCommand fixtureProcessEpoch ExplicitAdministrativeEnd) failed
   in [("initial", initial), ("pending-installation", decided), ("completed-applied", completed), ("completed-not-applied", failed), ("ended", ended)]

propAtomicCommandCounts :: Positive Word64 -> Property
propAtomicCommandCounts (Positive seed) =
  let initial = initialOracleState (if even seed then fixtureCheckedGenesis else step15Genesis)
      (decided, _, entry, _, _) = decideAt 1 fixtureHeraldEpoch fixtureProcessEpoch objectA initialPair targetVoid initial
      (done, _, completion) = completeAt 2 decided
   in counterexample
        "two commands regardless of three or five captured Heralds"
        ((oracleGreatestControlIndex done, oracleRequestCount done, tags entry <> tags completion) === (controlIndex 2, 2, [2, 10]))

propAtomicGenerationIdentity :: Word8 -> Property
propAtomicGenerationIdentity value =
  let request = requestId fixtureHeraldEpoch 1
      generation = fromIntegral value
      command expected = decisionEnvelope request fixtureProcessEpoch objectA (ProcessLabel fixtureProcessEpoch, expected) (initialEvidence objectA initialPair) Nothing Nothing targetVoid
      (state, receipt, _) = committedSubmission (command generation) initialState
      (same, duplicate) = submitOracleState (command generation) state
      (_, conflicting) = submitOracleState (command (generation + 1)) state
      conflict = case oracleSubmissionOutcomeView conflicting of OracleSubmissionProtocolRejectedView {} -> True; _ -> False
   in conjoin
        [ counterexample "generation binds command identity" (oracleEnvelopeDigest (command generation) /= oracleEnvelopeDigest (command (generation + 1))),
          counterexample "exact retry retains receipt" (oracleSubmissionOutcomeView duplicate == OracleSubmissionDuplicateView receipt),
          counterexample "exact retry preserves state" (same == state),
          counterexample "changed generation with same request ID conflicts" conflict
        ]

propAtomicHistory :: Positive Word64 -> Property
propAtomicHistory (Positive seed) =
  let count = 1 + seed `mod` 6
      commands = concat [pair generation | generation <- [0 .. count - 1]]
      pair generation =
        let number = 2 * generation + 1
            prior = (ProcessLabel fixtureProcessEpoch, generation)
         in [Left (decisionEnvelope (requestId fixtureHeraldEpoch number) fixtureProcessEpoch objectA prior (initialEvidence objectA initialPair) Nothing Nothing (targetProcess fixtureProcessEpoch)), Right (number + 1)]
      advance state = \case
        Left supplied -> let (next, _, _) = committedSubmission supplied state in next
        Right number -> let (next, _, _) = completeAt number state in next
      run mode = scanl advance (initialOracleStateWithDigestMode mode fixtureCheckedGenesis) commands
      checkpoints mode = and [Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (Checkpoint.oracleCheckpointBytes state) == Right state | state <- run mode]
      continuation mode =
        and
          [ case Checkpoint.decodeOracleCheckpoint fixtureCheckedGenesis (Checkpoint.oracleCheckpointBytes state) of
              Left _ -> False
              Right restored -> foldl advance restored suffix == last (run mode)
          | (state, suffix) <- zip (run mode) (tailsOf commands)
          ]
      canonicalCompletedOrder mode =
        let states = run mode
            final = last states
            entries =
              [ (labelDecisionIdBytes (liveDecisionId decision), Put.runPut (Put.putWord64be (controlIndexWord64 (liveDecisionRequestControlIndex decision))) <> heraldMembershipGenerationIdBytes (liveDecisionMembershipGeneration decision) <> labelOutcomeDigestBytes (deriveLabelOutcomeDigest terminal))
              | state <- states,
                decision <- oracleOpenDecisions state,
                Just terminal <- [oracleTerminalOutcome (liveDecisionId decision) state]
              ]
            expected = Put.runPut $ do
              Put.putWord64be count
              forM_ (sortOn fst entries) $ \(key, witness) -> Put.putByteString key >> Put.putByteString witness
            offset = sum (map snd (takeWhile ((/= "completed_workflows") . fst) (Checkpoint.oracleStateDiagnosticSections final)))
         in ByteString.take (ByteString.length expected) (ByteString.drop offset (oracleStateCanonicalBytes final)) == expected
      enabled = last (run OracleStateDigestEnabled)
      disabled = last (run OracleStateDigestDisabled)
   in conjoin
        [ counterexample "every prefix checkpoints in both modes" (all checkpoints [OracleStateDigestDisabled, OracleStateDigestEnabled]),
          counterexample "every checkpoint resumes to the same final state" (all continuation [OracleStateDigestDisabled, OracleStateDigestEnabled]),
          counterexample "digest mode preserves label evidence" (oracleLabelRecord objectA enabled == oracleLabelRecord objectA disabled),
          counterexample "one completed decision per pair" (oracleCompletedWorkflowCount enabled == fromIntegral count),
          counterexample "private completion keys preserve public identity ordering and exact bytes" (all canonicalCompletedOrder [OracleStateDigestDisabled, OracleStateDigestEnabled]),
          counterexample "completed witnesses retain 104 bytes per decision" (all ((== Just (8 + 104 * fromIntegral count)) . lookup "completed_workflows" . Checkpoint.oracleStateDiagnosticSections) [enabled, disabled]),
          counterexample "completed witnesses retain no transcript encodings" (all ((== Just 0) . lookup "completed_workflow_cached_bytes" . Checkpoint.oracleStateDiagnosticCardinalities) [enabled, disabled]),
          counterexample "two commands per successful label" (oracleGreatestControlIndex enabled == controlIndex (2 * count))
        ]
  where
    tailsOf [] = [[]]
    tailsOf values@(_ : rest) = values : tailsOf rest

initialState :: OracleState
initialState = initialOracleState fixtureCheckedGenesis

objectA, objectB :: GlobalObjectId
objectA = globalObjectId 211
objectB = globalObjectId 212

initialPair :: Label
initialPair = (ProcessLabel fixtureProcessEpoch, 0)

initialEvidence :: GlobalObjectId -> Label -> Maybe InitialLabelEvidence
initialEvidence object prior = initialPublicationLabelEvidence object prior (publicationId (nablaId 181) genesisAuthorityEpoch fixtureHeraldEpoch (nablaSequence 1))

decisionEnvelope :: OracleClientRequestId -> ProcessEpochId -> GlobalObjectId -> Label -> Maybe InitialLabelEvidence -> Maybe LiveAuthority -> Maybe PriorAuthorityJustification -> LabelTarget -> OracleEnvelope
decisionEnvelope request caller object expected evidence authority justification target =
  envelope request (decideLabelCommand (deriveLabelDecisionId fixtureSystemId request) caller object expected evidence authority justification target (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition caller) EmptyHeraldPublicationPrefix))

decideAt :: Word64 -> HeraldEpoch -> ProcessEpochId -> GlobalObjectId -> Label -> LabelTarget -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry, LabelDecisionId, LiveTerminalOutcome)
decideAt number home caller object expected target state =
  let supplied = decisionEnvelope (requestId home number) caller object expected (initialEvidence object initialPair) Nothing Nothing target
      (next, receipt, entry) = committedSubmission supplied state
      (decision, terminal, _) = decisionEvent entry
   in (next, receipt, entry, liveDecisionId decision, terminal)

onlyOpenDecision :: OracleState -> Maybe LiveLabelDecision
onlyOpenDecision state = case oracleOpenDecisions state of
  [] -> Nothing
  [decision] -> Just decision
  _ -> error "single-decision fixture has concurrent pending labels"

onlyLabelCollector :: OracleState -> Maybe HeraldEpoch
onlyLabelCollector state = onlyOpenDecision state >>= (\decision -> oracleLabelCollector (liveDecisionId decision) state)

onlyTerminalOutcome :: OracleState -> Maybe LiveTerminalOutcome
onlyTerminalOutcome state = onlyOpenDecision state >>= (\decision -> oracleTerminalOutcome (liveDecisionId decision) state)

completeDecisionAt :: Word64 -> LabelDecisionId -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry)
completeDecisionAt number ident state =
  let collector = maybe (error "pending decision lacks collector") id (oracleLabelCollector ident state)
      terminal = maybe (error "pending decision lacks terminal") id (oracleTerminalOutcome ident state)
      command = completeLabelDecisionCommand (labelCompletionAttestation ident (terminalIndex terminal) (deriveLabelOutcomeDigest terminal) collector (heraldMembershipGenerationId (oracleCurrentMembership state)))
   in committedSubmission (envelope (requestId collector number) command) state

completeAt :: Word64 -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry)
completeAt number state =
  let collector = maybe (error "pending decision lacks collector") id (onlyLabelCollector state)
   in committedSubmission (envelope (requestId collector number) (completionCommand state)) state

completionCommand :: OracleState -> OracleCommand
completionCommand state =
  let decision = maybe (error "completion lacks decision") liveDecisionId (onlyOpenDecision state)
      collector = maybe (error "completion lacks collector") id (onlyLabelCollector state)
   in completeLabelDecisionCommand (labelCompletionAttestation decision (terminalIndex (terminalAt state)) (deriveLabelOutcomeDigest (terminalAt state)) collector (heraldMembershipGenerationId (oracleCurrentMembership state)))

terminalIndex :: LiveTerminalOutcome -> ControlIndex
terminalIndex terminal = case liveTerminalOutcomeView terminal of
  LiveNotAppliedOutcomeView _ index _ _ _ _ -> index
  LiveReleasedOutcomeView _ _ index _ -> index

terminalAt :: OracleState -> LiveTerminalOutcome
terminalAt state = maybe (error "pending collection has no terminal outcome") id (onlyTerminalOutcome state)

recordAt :: GlobalObjectId -> OracleState -> LabelRecord
recordAt object state = maybe (error "canonical label overlay missing") id (oracleLabelRecord object state)

decisionEvent :: AppliedOracleEntry -> (LiveLabelDecision, LiveTerminalOutcome, LabelOutcomeDigest)
decisionEvent entry = case [(decision, terminal, digest) | event <- appliedEntryProjectionEvents entry, LabelDecidedView decision terminal digest <- [oracleProjectionEventView event]] of
  [result] -> result
  other -> error ("expected one atomic decision event: " <> show other)

assertNotApplied :: LiveNotAppliedReason -> LiveTerminalOutcome -> Assertion
assertNotApplied expected terminal = case liveTerminalOutcomeView terminal of
  LiveNotAppliedOutcomeView _ _ _ actual _ _ -> assertEqual "NotApplied reason" expected actual
  other -> assertFailure ("expected NotApplied: " <> show other)

assertApplied :: LiveTerminalOutcome -> Assertion
assertApplied terminal = case liveTerminalOutcomeView terminal of
  LiveReleasedOutcomeView {} -> pure ()
  other -> assertFailure ("expected Applied: " <> show other)

committedSubmission :: OracleEnvelope -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry)
committedSubmission supplied state = case oracleSubmissionOutcomeView outcome of
  OracleSubmissionCommittedView receipt entry -> (successor, receipt, entry)
  view -> error ("expected committed command: " <> show view)
  where
    (successor, outcome) = submitOracleState supplied state

commitAccepted :: Word64 -> HeraldEpoch -> OracleCommand -> OracleState -> (OracleState, ControlIndex, [OracleProjectionEvent])
commitAccepted number home command state =
  let (successor, receipt, entry) = committedSubmission (envelope (requestId home number) command) state
   in case oracleReceiptResult receipt of
        OracleAccepted -> (successor, oracleReceiptControlIndex receipt, appliedEntryProjectionEvents entry)
        OracleRejected problem -> error ("expected accepted command: " <> show problem)

envelope :: OracleClientRequestId -> OracleCommand -> OracleEnvelope
envelope request command = oracleEnvelope request Nothing (oracleClientRequestHome request) command

requestId :: HeraldEpoch -> Word64 -> OracleClientRequestId
requestId = oracleClientRequestId

checkedEndCommand :: ProcessEpochId -> ProcessEndReason -> OracleCommand
checkedEndCommand process reason = checked "End command" (endProcessEpochCommand process reason)

tags :: AppliedOracleEntry -> [Word8]
tags = fmap oracleProjectionEventTag . appliedEntryProjectionEvents

framed :: ByteString.ByteString -> ByteString.ByteString
framed = Put.runPut . putFramed

putFramed :: ByteString.ByteString -> Put.Put
putFramed bytes = Put.putWord64be (fromIntegral (ByteString.length bytes)) >> Put.putByteString bytes

replaceByte :: Int -> Word8 -> ByteString.ByteString -> ByteString.ByteString
replaceByte offset byte bytes = ByteString.take offset bytes <> ByteString.singleton byte <> ByteString.drop (offset + 1) bytes

assertCodec :: (Show problem) => String -> (ByteString.ByteString -> Either problem decoded) -> (decoded -> ByteString.ByteString) -> ByteString.ByteString -> Assertion
assertCodec name decode encode bytes = do
  case decode bytes of
    Left problem -> assertFailure (name <> " decode failed: " <> show problem)
    Right decoded -> assertEqual (name <> " exact round trip") bytes (encode decoded)
  assertBool (name <> " rejects trailing bytes") (isLeft (decode (bytes <> "x")))
  assertBool (name <> " rejects malformed domain") (isLeft (decode (replaceByte 8 0xff bytes)))

caseCanonicalRejectionVectors :: Assertion
caseCanonicalRejectionVectors = do
  let h1 = fixtureHeraldEpoch
      h2 = fixtureRemoteHeraldEpoch
      p1 = fixtureProcessEpoch
      p2 = fixtureRemoteProcessEpoch
      d1 = deriveLabelDecisionId fixtureSystemId (requestId h1 2300)
      d2 = deriveLabelDecisionId fixtureSystemId (requestId h2 2301)
      digest1 = checked "digest 1" (mkLabelOutcomeDigest (ByteString.replicate 32 0xb1))
      digest2 = checked "digest 2" (mkLabelOutcomeDigest (ByteString.replicate 32 0xb2))
      generation1 = heraldMembershipGenerationId (oracleCurrentMembership initialState)
      generation2 = heraldMembershipGenerationId (oracleCurrentMembership (initialOracleState step15Genesis))
      heralds = heraldEpochBytes h1 <> heraldEpochBytes h2
      processes = processEpochIdBytes p1 <> processEpochIdBytes p2
      decisions = labelDecisionIdBytes d1 <> labelDecisionIdBytes d2
      index value = Put.runPut (Put.putWord64be value)
      fixtures =
        [ (0, RequestHomeEpochMismatch h1 h2, heralds),
          (1, InactiveHomeHerald h1, heraldEpochBytes h1),
          (2, StaleExpectedControlIndex (controlIndex 1) (controlIndex 2), index 1 <> index 2),
          (3, StartProcessResidenceMismatch p1 h1 h2, processEpochIdBytes p1 <> heralds),
          (4, ProcessEpochAlreadyStarted p1, processEpochIdBytes p1),
          (5, ProcessAlreadyStarted (processId 202), processIdBytes (processId 202)),
          (6, EndProcessUnknown p1, processEpochIdBytes p1),
          (7, EndProcessResidenceMismatch p1 h1 h2, processEpochIdBytes p1 <> heralds),
          (8, EndProcessAlreadyEnded p1, processEpochIdBytes p1),
          (9, OpenDecisionIdMismatch d1 d2, decisions),
          (13, OpenAcceptanceCallerMismatch p1 p2, processes),
          (14, OpenExpectedPriorLabelMismatch (releasedLabel (VoidLabel, 0)) (releasedDeleted 0), ByteString.singleton 0 <> framed (canonicalValueByteString (canonicalValueBytes (labelValue (VoidLabel, 0)))) <> ByteString.singleton 1 <> index 0),
          (17, OpenPriorAuthorityJustificationInvalid, ByteString.empty),
          (19, UnknownLabelDecision d1, labelDecisionIdBytes d1),
          (21, ReporterHomeMismatch h1 h2, heralds),
          (22, ReporterNotCaptured h1, heraldEpochBytes h1),
          (32, LabelCompletionDecisionMismatch d1 d2, decisions),
          (33, LabelCompletionCollectorMismatch h1 h2, heralds),
          (34, LabelCompletionDigestMismatch digest1 digest2, labelOutcomeDigestBytes digest1 <> labelOutcomeDigestBytes digest2),
          (35, LabelCompletionMembershipGenerationMismatch generation1 generation2, heraldMembershipGenerationIdBytes generation1 <> heraldMembershipGenerationIdBytes generation2),
          (44, OpenInitialLabelEvidenceMissing objectA, globalObjectIdBytes objectA),
          (45, OpenInitialLabelEvidenceObjectMismatch objectA objectB, globalObjectIdBytes objectA <> globalObjectIdBytes objectB)
        ]
  forM_ fixtures $ \(tag, rejection, payload) -> do
    assertEqual "surviving rejection tag" tag (oracleRejectionTag rejection)
    assertEqual ("exact rejection transcript " <> show tag) (ByteString.singleton tag <> payload) (oracleRejectionCanonicalBytes rejection)
  let reasons = [(0, LiveCallerProcessEnded p1, processEpochIdBytes p1), (1, LiveTargetProcessEnded p1, processEpochIdBytes p1), (2, LivePriorLabelChanged, ByteString.empty), (5, LiveTransitionNoLongerPermitted, ByteString.empty)]
  forM_ reasons $ \(tag, reason, payload) -> do
    assertEqual "NotApplied reason tag" tag (liveNotAppliedReasonTag reason)
    assertEqual "NotApplied reason transcript" (ByteString.singleton tag <> framed payload) (liveNotAppliedReasonCanonicalBytes reason)
