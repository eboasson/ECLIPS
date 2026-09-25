module TransitionProperties
  ( tests,
  )
where

import Eclips.Domain.Identity (ControlIndex, controlIndex)
import Eclips.Domain.ProcessStart (ProcessStart, processStart, processStartProcessId)
import Eclips.Domain.Startup
  (
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    OracleEnvelope,
    oracleEnvelope,
    oracleEnvelopeCommand,
    oracleEnvelopeExpectedControlIndex,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleEffectBatch,
    OracleProtocolDisposition (ConflictingOracleRequestId),
    OracleStepOutcome (..),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Input (OracleInput (ApplyOracleEnvelope))
import Eclips.Oracle.Projection
  ( AppliedOracleCommandEntry,
    AppliedOracleEntry,
    OracleProjectionEventView (ProcessStartedView),
    ProcessEpochRecord,
    ProcessOriginView (DynamicStartView),
    appliedEntryCommand,
    appliedEntryCommandDigest,
    appliedEntryControlIndex,
    appliedEntryPostStateDigest,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    appliedEntryRequestId,
    oracleProjectionEventView,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordResidence,
  )
import Eclips.Oracle.Receipt
  ( OracleReceipt,
    OracleReceiptResult (..),
    OracleRejection (..),
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptResult,
  )
import Eclips.Oracle.State
  ( OracleState,
    oracleGreatestControlIndex,
    oracleLiveProcessResidence,
    oracleProcessRecord,
    oracleRequestCount,
    oracleStateDigest,
  )
import Eclips.Oracle.Transition (stepOracle, stepOracleInput)
import OracleFixtures
  ( alternateCommand,
    checked,
    envelopeFor,
    fixtureHeraldEpoch,
    fixtureInitialState,
    fixtureRemoteHeraldEpoch,
    fixtureRemoteStart,
    fixtureStart,
    fixtureStartProcessEpoch,
    fixtureThirdStart,
    heraldEpoch,
    processEpochId,
    processId,
    validCommand,
    validEnvelope,
  )
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
    "transition"
    [ testCase "first-seen Start records only its manifest, residence, and index" caseAccepted,
      testCase "fresh expected control zero is accepted" caseExpectedZeroAccepted,
      testCase "first-seen semantic rejection advances control and emits a delta-free entry" caseRejected,
      testCase "exact duplicate returns the retained receipt without changing state" caseDuplicate,
      testCase "conflicting request reuse produces no receipt or effect" caseConflict,
      testCase "request-ID classification precedes later semantic validation" caseConflictPrecedence,
      testCase "stepOracleInput is exactly the envelope transition" caseInputEquivalence,
      testGroup "Start rejection matrix" rejectionCases
    ]

caseAccepted :: Assertion
caseAccepted = do
  let (successor, outcome, effects) = runStep validEnvelope fixtureInitialState
      receipt = committedReceipt outcome
      entry = onlyEntry effects
      started = requireStartedEvent entry
  assertEqual "control index" (controlIndex 1) (oracleGreatestControlIndex successor)
  assertEqual "one retained request" 1 (oracleRequestCount successor)
  assertEqual "accepted result" OracleAccepted (oracleReceiptResult receipt)
  assertEqual "receipt index" (controlIndex 1) (oracleReceiptControlIndex receipt)
  assertEqual "dynamic process residence" (Just fixtureHeraldEpoch) (oracleLiveProcessResidence fixtureStartProcessEpoch successor)
  assertEqual "started-process state" (Just started) (oracleProcessRecord fixtureStartProcessEpoch successor)
  assertEqual "process epoch" fixtureStartProcessEpoch (processRecordProcessEpoch started)
  assertEqual "residence" fixtureHeraldEpoch (processRecordResidence started)
  assertEqual
    "checked manifest and Start prerequisite"
    ( DynamicStartView
        (controlIndex 1)
        (oracleEnvelopeRequestId validEnvelope)
    )
    (processRecordOrigin started)
  assertEqual "entry index" (controlIndex 1) (appliedEntryControlIndex entry)
  assertEqual "entry request" (oracleEnvelopeRequestId validEnvelope) (appliedEntryRequestId (commandEntry entry))
  assertEqual "entry receipt" receipt (appliedEntryReceipt (commandEntry entry))
  assertEqual "entry and receipt digest" (oracleReceiptCommandDigest receipt) (appliedEntryCommandDigest (commandEntry entry))
  assertEqual "entry commits post-state digest" (oracleStateDigest successor) (appliedEntryPostStateDigest entry)

caseExpectedZeroAccepted :: Assertion
caseExpectedZeroAccepted = do
  let envelope = withExpectedControlIndex (Just (controlIndex 0)) validEnvelope
      (successor, outcome, effects) = runStep envelope fixtureInitialState
  assertEqual "accepted" OracleAccepted (oracleReceiptResult (committedReceipt outcome))
  assertEqual "one index" (controlIndex 1) (oracleGreatestControlIndex successor)
  assertEqual "one Start event" 1 (length (appliedEntryProjectionEvents (onlyEntry effects)))

caseRejected :: Assertion
caseRejected = do
  let envelope = envelopeFor 2 (startProcessEpochCommand fixtureRemoteStart)
      (successor, outcome, effects) = runStep envelope fixtureInitialState
      receipt = committedReceipt outcome
      entry = onlyEntry effects
  assertEqual
    "wrong residence"
    (OracleRejected (StartProcessResidenceMismatch (processEpochId 173) fixtureHeraldEpoch fixtureRemoteHeraldEpoch))
    (oracleReceiptResult receipt)
  assertEqual "rejection consumes one index" (controlIndex 1) (oracleGreatestControlIndex successor)
  assertEqual "rejection is retained" 1 (oracleRequestCount successor)
  assertEqual "no process projection" [] (appliedEntryProjectionEvents entry)
  assertEqual "no live process" Nothing (oracleLiveProcessResidence (processEpochId 173) successor)
  assertEqual "entry commits post-state digest" (oracleStateDigest successor) (appliedEntryPostStateDigest entry)

caseDuplicate :: Assertion
caseDuplicate = do
  let (firstState, firstOutcome, _) = runStep validEnvelope fixtureInitialState
      receipt = committedReceipt firstOutcome
      (duplicateState, duplicateOutcome, duplicateEffects) = runStep validEnvelope firstState
  assertEqual "state unchanged" firstState duplicateState
  assertEqual "retained receipt" (OracleDuplicate receipt) duplicateOutcome
  assertEqual "no duplicate effect" [] (oracleEffects duplicateEffects)

caseConflict :: Assertion
caseConflict = do
  let (firstState, firstOutcome, _) = runStep validEnvelope fixtureInitialState
      receipt = committedReceipt firstOutcome
      conflictEnvelope = withCommand alternateCommand validEnvelope
      (conflictState, conflictOutcome, conflictEffects) = runStep conflictEnvelope firstState
  assertEqual "state unchanged" firstState conflictState
  assertEqual "no conflict effect" [] (oracleEffects conflictEffects)
  case conflictOutcome of
    OracleProtocolRejected (ConflictingOracleRequestId requestId retained supplied) -> do
      assertEqual "request" (oracleEnvelopeRequestId validEnvelope) requestId
      assertEqual "retained digest" (oracleReceiptCommandDigest receipt) retained
      assertBool "different digest" (retained /= supplied)
    other -> assertFailure ("expected conflict, got " <> show other)

caseConflictPrecedence :: Assertion
caseConflictPrecedence = do
  let (firstState, _, _) = runStep validEnvelope fixtureInitialState
      inactive = heraldEpoch 250
      conflicting =
        oracleEnvelope
          (oracleEnvelopeRequestId validEnvelope)
          (oracleEnvelopeExpectedControlIndex validEnvelope)
          inactive
          alternateCommand
      (successor, outcome, effects) = runStep conflicting firstState
  assertEqual "state unchanged" firstState successor
  assertEqual "no effect" [] (oracleEffects effects)
  case outcome of
    OracleProtocolRejected _ -> pure ()
    other -> assertFailure ("expected protocol conflict, got " <> show other)

caseInputEquivalence :: Assertion
caseInputEquivalence =
  assertEqual
    "input wrapper"
    (stepOracle validEnvelope fixtureInitialState)
    (stepOracleInput (ApplyOracleEnvelope validEnvelope) fixtureInitialState)

rejectionCases :: [TestTree]
rejectionCases =
  [ rejectionCase
      "request home must equal claimed home"
      (RequestHomeEpochMismatch fixtureRemoteHeraldEpoch fixtureHeraldEpoch)
      ( oracleEnvelope
          (oracleClientRequestId fixtureRemoteHeraldEpoch 10)
          Nothing
          fixtureHeraldEpoch
          validCommand
      ),
    let inactive = heraldEpoch 250
        bootstrap = processStart (processId 165) (processEpochId 173) inactive
     in rejectionCase
          "home must be active"
          (InactiveHomeHerald inactive)
          ( oracleEnvelope
              (oracleClientRequestId inactive 11)
              Nothing
              inactive
              (startProcessEpochCommand bootstrap)
          ),
    rejectionCase
      "expected index must be current"
      (StaleExpectedControlIndex (controlIndex 9) (controlIndex 0))
      (withExpectedControlIndex (Just (controlIndex 9)) validEnvelope),
    afterAcceptedRejection
      "process epoch identity is single-use"
      (ProcessEpochAlreadyStarted fixtureStartProcessEpoch)
      fixtureStart,
    afterAcceptedRejection
      "a stable process identity cannot have two live epochs"
      (ProcessAlreadyStarted (processStartProcessId fixtureStart))
      fixtureThirdStart
  ]

rejectionCase :: String -> OracleRejection -> OracleEnvelope -> TestTree
rejectionCase label expected envelope =
  testCase label (assertEqual "semantic rejection" (OracleRejected expected) (freshResult envelope))

afterAcceptedRejection :: String -> OracleRejection -> ProcessStart -> TestTree
afterAcceptedRejection label expected bootstrap =
  testCase label $ do
    let (acceptedState, _, _) = runStep validEnvelope fixtureInitialState
        envelope = envelopeFor 20 (startProcessEpochCommand bootstrap)
        (_, outcome, effects) = runStep envelope acceptedState
    assertEqual "semantic rejection" (OracleRejected expected) (oracleReceiptResult (committedReceipt outcome))
    assertEqual "no projection event" [] (appliedEntryProjectionEvents (onlyEntry effects))

freshResult :: OracleEnvelope -> OracleReceiptResult
freshResult envelope =
  let (_, outcome, _) = runStep envelope fixtureInitialState
   in oracleReceiptResult (committedReceipt outcome)

runStep :: OracleEnvelope -> OracleState -> (OracleState, OracleStepOutcome, OracleEffectBatch)
runStep envelope state = checked "Oracle transition" (stepOracle envelope state)

committedReceipt :: OracleStepOutcome -> OracleReceipt
committedReceipt outcome = case outcome of
  OracleCommitted receipt -> receipt
  other -> error ("expected committed receipt, got " <> show other)

onlyEntry :: OracleEffectBatch -> AppliedOracleEntry
onlyEntry effects = case oracleEffects effects of
  [EmitAppliedOracleEntry entry] -> entry
  other -> error ("expected one applied entry, got " <> show other)

requireStartedEvent :: AppliedOracleEntry -> ProcessEpochRecord
requireStartedEvent entry = case appliedEntryProjectionEvents entry of
  [event] -> case oracleProjectionEventView event of
    ProcessStartedView record -> record
    other -> error ("expected a ProcessStarted event, got " <> show other)
  events -> error ("expected one ProcessStarted event, got " <> show events)

withExpectedControlIndex :: Maybe ControlIndex -> OracleEnvelope -> OracleEnvelope
withExpectedControlIndex expected envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    expected
    (oracleEnvelopeHomeHeraldEpoch envelope)
    (oracleEnvelopeCommand envelope)

withCommand :: OracleCommand -> OracleEnvelope -> OracleEnvelope
withCommand command envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    (oracleEnvelopeExpectedControlIndex envelope)
    (oracleEnvelopeHomeHeraldEpoch envelope)
    command

commandEntry :: AppliedOracleEntry -> AppliedOracleCommandEntry
commandEntry entry = case appliedEntryCommand entry of
  Just command -> command
  Nothing -> error "command-only property fixture emitted a native configuration"
