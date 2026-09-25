{-# LANGUAGE OverloadedStrings #-}

module OracleModelProperties
  ( tests,
  )
where

import Control.Monad (foldM, replicateM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize.Get qualified as SerializeGet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
    ProcessId,
    controlIndex,
    heraldEpochBytes,
  )
import Eclips.Domain.ProcessStart (ProcessStart, processStart, processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Domain.Sort.Profile (profileCatalogueDigest)
import Eclips.Domain.Startup
  ( ConfigurationDigest,
    HeraldMember (heraldMemberEpoch),
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
    deriveInitialProjectionDigest,
    mkConfigurationDigest,
  )
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalizeAppliedOracleEntry,
    decodeCanonicalAppliedOracleEntry,
    oracleEnvelopeDigest,
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    OracleEnvelope,
    oracleCommandStart,
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
import Eclips.Oracle.Genesis
  ( checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    OracleCommandDigest,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
    oracleCommandDigestBytes,
    oracleStateDigestBytes,
  )
import Eclips.Oracle.Label (oracleReceiptCanonicalBytes, oracleStateCanonicalBytes)
import Eclips.Oracle.Projection
  ( AppliedOracleCommandEntry,
    AppliedOracleEntry,
    OracleProjectionEventView (ProcessStartedView),
    ProcessEpochRecord,
    ProcessOriginView (DynamicStartView),
    appliedEntryCommand,
    appliedEntryCommandDigest,
    appliedEntryConfiguration,
    appliedEntryControlIndex,
    appliedEntryPostStateDigest,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    appliedEntryReceiptRetirementProgress,
    appliedEntryRequestId,
    oracleProjectionEventView,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordProcessId,
    processRecordResidence,
  )
import Eclips.Oracle.Receipt
  ( OracleReceipt,
    OracleReceiptResult (..),
    OracleRejection (..),
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptRequestId,
    oracleReceiptResult,
  )
import Eclips.Oracle.State
  ( OracleState,
    OracleStateDigestMode (..),
    decodeOracleCheckpoint,
    oracleCheckpointBytes,
    oracleGreatestControlIndex,
    oracleLiveProcessResidence,
    oracleProcessRecord,
    oracleProcessRecords,
    oracleRequestCount,
    oracleRequestReceipt,
    oracleStateDigest,
    oracleStateDigestMode,
  )
import Eclips.Oracle.Transition (initialOracle, initialOracleWithStateDigestMode, stepOracle)
import OracleFixtures
  ( checked,
    envelopeFor,
    fixtureAlternateStart,
    fixtureBindings,
    fixtureBootstraps,
    fixtureCheckedGenesis,
    fixtureConfigurationDigest,
    fixtureDescriptors,
    fixtureHeraldEpoch,
    fixtureInitialState,
    fixtureMembers,
    fixtureRaftConfiguration,
    fixtureRemoteHeraldEpoch,
    fixtureRemoteStart,
    fixtureStart,
    fixtureStartProcessEpoch,
    fixtureSystemId,
    fixtureThirdStart,
    fixtureTopology,
    heraldEpoch,
    processEpochId,
    processId,
    validEnvelope,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Property,
    chooseInt,
    counterexample,
    cover,
    elements,
    frequency,
    property,
    shrinkList,
    testProperty,
    vectorOf,
  )

tests :: TestTree
tests =
  testGroup
    "normalized Oracle model"
    [ testCase "initial state contains genesis residences but no dynamic Starts" caseInitialProjection,
      testCase "accepted Start extends only the dynamic process projection" caseAcceptedProjection,
      testCase "rejected Start retains its result without extending process state" caseRejectedProjection,
      testCase "duplicate and conflicting request reuse leave the complete state unchanged" caseRetryStateIdentity,
      testCase "state digest commits the deployment ConfigurationDigest" caseDeploymentConfigurationDigest,
      testCase "genesis presentation order does not affect state identity" caseGenesisNormalization,
      testCase "accepted and rejected histories have distinct state identities" caseHistoryDigestSensitivity,
      testProperty "diagnostic modes preserve every semantic transition and checkpoint across mixed histories" propDigestModes,
      testCase "diagnostic digest is disabled by default and checkpoint option admission is exact" caseDigestModeAdmission,
      testProperty "generated mixed histories preserve canonical receipts and agree with an independent reference model at every prefix" propReferenceHistory
    ]

caseInitialProjection :: Assertion
caseInitialProjection = do
  assertEqual "control zero" (controlIndex 0) (oracleGreatestControlIndex fixtureInitialState)
  assertEqual "no retained requests" 0 (oracleRequestCount fixtureInitialState)
  assertEqual "no dynamic Starts" Map.empty (actualStarts fixtureInitialState)
  mapM_
    ( \bootstrap ->
        assertEqual
          "genesis residence"
          (Just (appliedProcessResidence bootstrap))
          (oracleLiveProcessResidence (appliedProcessEpochId bootstrap) fixtureInitialState)
    )
    fixtureBootstraps

caseAcceptedProjection :: Assertion
caseAcceptedProjection = do
  let (state, receipt) = committedState validEnvelope fixtureInitialState
      started = requireStarted fixtureStartProcessEpoch state
  assertEqual "accepted" OracleAccepted (oracleReceiptResult receipt)
  assertEqual "one request" 1 (oracleRequestCount state)
  assertEqual
    "one dynamic Start"
    (Map.singleton fixtureStartProcessEpoch (fixtureStart, controlIndex 1))
    (actualStarts state)
  assertEqual
    "manifest and Start index retained exactly"
    ( DynamicStartView
        (controlIndex 1)
        (oracleEnvelopeRequestId validEnvelope)
    )
    (processRecordOrigin started)
  assertEqual "residence" (Just fixtureHeraldEpoch) (oracleLiveProcessResidence fixtureStartProcessEpoch state)
  assertEqual "request result retained" (Just receipt) (oracleRequestReceipt (oracleEnvelopeRequestId validEnvelope) state)

caseRejectedProjection :: Assertion
caseRejectedProjection = do
  let envelope = envelopeFor 2 (startProcessEpochCommand fixtureRemoteStart)
      (state, receipt) = committedState envelope fixtureInitialState
  assertEqual
    "wrong residence"
    (OracleRejected (StartProcessResidenceMismatch (processStartProcessEpochId fixtureRemoteStart) fixtureHeraldEpoch fixtureRemoteHeraldEpoch))
    (oracleReceiptResult receipt)
  assertEqual "request retained" 1 (oracleRequestCount state)
  assertEqual "no dynamic Start" Map.empty (actualStarts state)
  assertEqual
    "remote epoch remains absent"
    Nothing
    (oracleLiveProcessResidence (processStartProcessEpochId fixtureRemoteStart) state)

caseRetryStateIdentity :: Assertion
caseRetryStateIdentity = do
  let (acceptedState, receipt) = committedState validEnvelope fixtureInitialState
      (duplicateState, duplicateOutcome, _) = checked "duplicate" (stepOracle validEnvelope acceptedState)
      conflicting = withCommand (startProcessEpochCommand fixtureAlternateStart) validEnvelope
      (conflictState, conflictOutcome, _) = checked "conflict" (stepOracle conflicting acceptedState)
  assertEqual "duplicate state" acceptedState duplicateState
  assertEqual "duplicate receipt" (OracleDuplicate receipt) duplicateOutcome
  assertEqual "conflict state" acceptedState conflictState
  case conflictOutcome of
    OracleProtocolRejected _ -> pure ()
    other -> error ("expected protocol rejection, got " <> show other)

caseDeploymentConfigurationDigest :: Assertion
caseDeploymentConfigurationDigest = do
  let alternateDigest = checked "alternate configuration digest" (mkConfigurationDigest (ByteString.replicate 32 90))
      alternateState = initialStateWith alternateDigest
  assertBool
    "deployment configuration digest participates in state identity"
    (oracleStateDigest fixtureInitialState /= oracleStateDigest alternateState)

caseGenesisNormalization :: Assertion
caseGenesisNormalization = do
  let reorderedGenesis =
        oracleGenesis
          fixtureSystemId
          (reverse fixtureMembers)
          profileCatalogueDigest
          (reverse fixtureDescriptors)
          (reverse fixtureBootstraps)
          fixtureConfigurationDigest
          fixtureTopology
          (deriveInitialProjectionDigest (reverse fixtureBootstraps) fixtureTopology)
          (reverse fixtureBindings)
          fixtureRaftConfiguration
          (deriveRaftConfigurationDigest fixtureSystemId (reverse fixtureBindings) fixtureRaftConfiguration)
      reorderedChecked = checked "reordered genesis" (checkOracleGenesis reorderedGenesis)
      reorderedState = checked "reordered state" (initialOracleWithStateDigestMode OracleStateDigestEnabled reorderedChecked)
  assertEqual "checked normalization" fixtureCheckedGenesis reorderedChecked
  assertEqual "state identity" (oracleStateDigest fixtureInitialState) (oracleStateDigest reorderedState)

caseHistoryDigestSensitivity :: Assertion
caseHistoryDigestSensitivity = do
  let (acceptedState, _) = committedState validEnvelope fixtureInitialState
      rejectedEnvelope = envelopeFor 1 (startProcessEpochCommand fixtureRemoteStart)
      (rejectedState, _) = committedState rejectedEnvelope fixtureInitialState
  assertBool "receipt and projection history are committed" (oracleStateDigest acceptedState /= oracleStateDigest rejectedState)

data Candidate
  = StartA
  | StartB
  | StartWithLiveProcessId
  | WrongResidence
  | StartC
  | ReusedEpoch
  | InactiveHome
  | StaleExpectedIndex
  | RequestHomeMismatch
  deriving stock (Bounded, Enum, Eq, Show)

data ModelAction
  = Submit Word8 Candidate
  | Retry Word8
  | Conflict Word8 Candidate
  deriving stock (Eq, Show)

newtype ActionTrace = ActionTrace [ModelAction]
  deriving stock (Show)

instance Arbitrary Candidate where
  arbitrary = elements [minBound .. maxBound]
  shrink _ = []

instance Arbitrary ModelAction where
  arbitrary = do
    slot <- fromIntegral <$> chooseInt (0, 15)
    frequency
      [ (5, Submit slot <$> arbitrary),
        (2, pure (Retry slot)),
        (2, Conflict slot <$> arbitrary)
      ]
  shrink _ = []

instance Arbitrary ActionTrace where
  arbitrary = do
    count <- chooseInt (0, 30)
    ActionTrace <$> vectorOf count arbitrary
  shrink (ActionTrace actions) = ActionTrace <$> shrinkList (const []) actions

data ReferenceRequest = ReferenceRequest
  { referenceRequestId :: OracleClientRequestId,
    referenceRequestDigest :: OracleCommandDigest,
    referenceRequestIndex :: ControlIndex,
    referenceRequestResult :: OracleReceiptResult
  }
  deriving stock (Eq, Show)

data ReferenceModel = ReferenceModel
  { referenceGreatestIndex :: Word64,
    referenceRequests :: Map OracleClientRequestId ReferenceRequest,
    referenceSlots :: Map Word8 OracleEnvelope,
    referenceStarts :: Map ProcessEpochId (ProcessStart, ControlIndex),
    referenceEpochIds :: Set ProcessEpochId,
    referenceLiveProcessIds :: Set ProcessId
  }
  deriving stock (Eq, Show)

data ExpectedStep
  = ExpectedFresh ReferenceRequest (Maybe ProcessStart)
  | ExpectedDuplicate ReferenceRequest
  | ExpectedConflict ReferenceRequest OracleCommandDigest
  deriving stock (Eq, Show)

data ObservedDisposition
  = ObservedFreshAccepted
  | ObservedFreshRejected OracleRejection
  | ObservedDuplicate
  | ObservedConflict
  deriving stock (Eq, Show)

propReferenceHistory :: ActionTrace -> Property
propReferenceHistory (ActionTrace generatedActions) =
  let actions = coveragePrefix <> generatedActions
   in case runTrace actions of
        Left problem -> counterexample (problem <> "\nactions=" <> show actions) False
        Right dispositions ->
          let rejections = [rejection | ObservedFreshRejected rejection <- dispositions]
           in counterexample ("actions=" <> show actions <> "\ndispositions=" <> show dispositions)
                $ cover 1 (ObservedFreshAccepted `elem` dispositions) "fresh acceptance"
                $ cover 1 (ObservedDuplicate `elem` dispositions) "exact duplicate"
                $ cover 1 (ObservedConflict `elem` dispositions) "conflicting request reuse"
                $ cover 1 (all (`elem` rejections) expectedRejections) "all reachable rejection classes"
                $ property True

-- Compare the whole semantic state transcript and every effect field except the
-- optional diagnostic witness. Neither state nor applied-entry Eq treats absence
-- as a wildcard; the enabled and disabled values must remain distinct.
propDigestModes :: ActionTrace -> Property
propDigestModes (ActionTrace generatedActions) =
  case foldM check (disabled, fixtureInitialState, initialReferenceModel) (coveragePrefix <> generatedActions) of
    Left problem -> counterexample problem False
    Right _ -> property True
  where
    disabled = checked "default Oracle" (initialOracle fixtureCheckedGenesis)
    check (withoutDigest, withDigest, model) action = do
      let (slot, envelope) = actionEnvelope action model
          (nextModel, _) = referenceStep slot envelope model
      (nextWithout, outcomeWithout, effectsWithout) <- either (Left . show) Right (stepOracle envelope withoutDigest)
      (nextWith, outcomeWith, effectsWith) <- either (Left . show) Right (stepOracle envelope withDigest)
      checkFacts
        [ ("disabled mode remains fixed", oracleStateDigestMode nextWithout == OracleStateDigestDisabled),
          ("enabled mode remains fixed", oracleStateDigestMode nextWith == OracleStateDigestEnabled),
          ("disabled state carries no witness", oracleStateDigest nextWithout == Nothing),
          ("enabled witness matches the complete semantic state", fmap oracleStateDigestBytes (oracleStateDigest nextWith) == Just (SHA256.hash (oracleStateCanonicalBytes nextWith))),
          ("all semantic state is identical", oracleStateCanonicalBytes nextWithout == oracleStateCanonicalBytes nextWith),
          ("outcomes are identical", outcomeWithout == outcomeWith),
          ("state Eq distinguishes absent evidence", nextWithout /= nextWith),
          ("effect semantics are identical", fmap semanticEffect (oracleEffects effectsWithout) == fmap semanticEffect (oracleEffects effectsWith)),
          ("disabled entries carry no witness", all (maybeAbsent . entryDigest) (oracleEffects effectsWithout)),
          ("entries in both modes round trip exactly", all entryRoundTrips (oracleEffects effectsWithout <> oracleEffects effectsWith)),
          ("enabled entries carry the successor witness", all ((== oracleStateDigest nextWith) . entryDigest) (oracleEffects effectsWith)),
          ("entry Eq distinguishes absent evidence", and (zipWith (/=) (oracleEffects effectsWithout) (oracleEffects effectsWith))),
          ("disabled checkpoint restores all state", decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes nextWithout) == Right nextWithout),
          ("enabled checkpoint restores all state", decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes nextWith) == Right nextWith)
        ]
      pure (nextWithout, nextWith, nextModel)
    maybeAbsent = (== Nothing)
    entryDigest (EmitAppliedOracleEntry entry) = appliedEntryPostStateDigest entry
    entryRoundTrips (EmitAppliedOracleEntry entry) =
      (canonicalAppliedOracleEntryValue <$> decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry))) == Right entry
    semanticEffect (EmitAppliedOracleEntry entry) =
      ( appliedEntryControlIndex entry,
        appliedEntryCommand entry,
        appliedEntryConfiguration entry,
        appliedEntryReceiptRetirementProgress entry,
        appliedEntryProjectionEvents entry
      )

caseDigestModeAdmission :: Assertion
caseDigestModeAdmission = do
  let disabled = checked "default Oracle" (initialOracle fixtureCheckedGenesis)
      disabledBytes = oracleCheckpointBytes disabled
      enabledBytes = oracleCheckpointBytes fixtureInitialState
      invalidDisabledOption = ByteString.init disabledBytes <> ByteString.singleton 2
      corruptedEnabled = ByteString.init enabledBytes <> ByteString.singleton (ByteString.last enabledBytes + 1)
      invalidEnabledOption = ByteString.take (ByteString.length enabledBytes - 33) enabledBytes <> ByteString.singleton 2 <> ByteString.takeEnd 32 enabledBytes
      rejected bytes = case decodeOracleCheckpoint fixtureCheckedGenesis bytes of
        Left _ -> True
        Right _ -> False
  assertEqual "default mode" OracleStateDigestDisabled (oracleStateDigestMode disabled)
  assertEqual "default has explicit absence" Nothing (oracleStateDigest disabled)
  assertEqual "disabled encoding has the absent option" 0 (ByteString.last disabledBytes)
  assertEqual "enabled encoding has the present option" 1 (ByteString.index enabledBytes (ByteString.length enabledBytes - 33))
  assertEqual "semantic checkpoint encoding is otherwise identical" (ByteString.init disabledBytes) (ByteString.take (ByteString.length enabledBytes - 33) enabledBytes)
  assertBool "unknown absent option is rejected" (rejected invalidDisabledOption)
  assertBool "unknown present option is rejected" (rejected invalidEnabledOption)
  assertBool "enabled checkpoint rejects a corrupted full-state digest" (rejected corruptedEnabled)
  assertBool "disabled checkpoint still rejects trailing bytes" (rejected (disabledBytes <> ByteString.singleton 0))

coveragePrefix :: [ModelAction]
coveragePrefix =
  [ Submit 0 StartA,
    Submit 1 StartB,
    Submit 2 WrongResidence,
    Submit 3 StartC,
    Submit 4 ReusedEpoch,
    Submit 5 InactiveHome,
    Submit 6 StaleExpectedIndex,
    Submit 7 RequestHomeMismatch,
    Submit 8 StartWithLiveProcessId,
    Submit 9 StartA,
    Retry 0,
    Conflict 0 StartB,
    Retry 2,
    Conflict 2 StartA
  ]

expectedRejections :: [OracleRejection]
expectedRejections =
  [ StartProcessResidenceMismatch
      (processStartProcessEpochId fixtureRemoteStart)
      fixtureHeraldEpoch
      fixtureRemoteHeraldEpoch,
    ProcessEpochAlreadyStarted (processStartProcessEpochId reusedEpochStart),
    InactiveHomeHerald inactiveHeraldEpoch,
    StaleExpectedControlIndex (controlIndex 999) (controlIndex 6),
    RequestHomeEpochMismatch fixtureHeraldEpoch fixtureRemoteHeraldEpoch,
    ProcessAlreadyStarted (processStartProcessId fixtureThirdStart),
    ProcessEpochAlreadyStarted (processStartProcessEpochId fixtureStart)
  ]

runTrace :: [ModelAction] -> Either String [ObservedDisposition]
runTrace actions = do
  checkReferenceState fixtureInitialState initialReferenceModel
  go 0 fixtureInitialState initialReferenceModel [] actions
  where
    go _ _ _ observed [] = Right (reverse observed)
    go prefix state model observed (action : remaining) = do
      let (slot, envelope) = actionEnvelope action model
          (successorModel, expected) = referenceStep slot envelope model
      (successor, outcome, effects) <-
        case stepOracle envelope state of
          Left fault -> Left ("Oracle invariant fault: " <> show fault)
          Right transition -> Right transition
      disposition <-
        withContext prefix action (checkTransition state successor envelope outcome effects expected)
      withContext prefix action (checkReferenceState successor successorModel)
      go (prefix + 1) successor successorModel (disposition : observed) remaining

withContext :: Int -> ModelAction -> Either String value -> Either String value
withContext prefix action =
  either
    (Left . (("prefix " <> show prefix <> " (" <> show action <> "): ") <>))
    Right

initialReferenceModel :: ReferenceModel
initialReferenceModel =
  ReferenceModel
    { referenceGreatestIndex = 0,
      referenceRequests = Map.empty,
      referenceSlots = Map.empty,
      referenceStarts = Map.empty,
      referenceEpochIds = Set.fromList (fmap appliedProcessEpochId fixtureBootstraps),
      referenceLiveProcessIds = Set.fromList (fmap appliedProcessId fixtureBootstraps)
    }

actionEnvelope :: ModelAction -> ReferenceModel -> (Word8, OracleEnvelope)
actionEnvelope action model = case action of
  Submit slot candidate -> (slot, candidateEnvelope slot candidate)
  Retry slot ->
    ( slot,
      Map.findWithDefault (candidateEnvelope slot StartA) slot (referenceSlots model)
    )
  Conflict slot candidate ->
    case Map.lookup slot (referenceSlots model) of
      Nothing -> (slot, candidateEnvelope slot candidate)
      Just retained -> (slot, conflictingEnvelope slot candidate retained)

conflictingEnvelope :: Word8 -> Candidate -> OracleEnvelope -> OracleEnvelope
conflictingEnvelope slot preferred retained =
  case find ((/= oracleEnvelopeDigest retained) . oracleEnvelopeDigest) candidates of
    Just envelope -> envelope
    Nothing -> error "candidate catalogue cannot produce a conflicting Oracle envelope"
  where
    candidates =
      fmap
        ( \candidate ->
            withRequestId
              (oracleEnvelopeRequestId retained)
              (candidateEnvelope slot candidate)
        )
        (preferred : [minBound .. maxBound])

candidateEnvelope :: Word8 -> Candidate -> OracleEnvelope
candidateEnvelope slot candidate =
  case candidate of
    InactiveHome ->
      oracleEnvelope
        (oracleClientRequestId inactiveHeraldEpoch sequenceNumber)
        Nothing
        inactiveHeraldEpoch
        (oracleEnvelopeCommand base)
    StaleExpectedIndex -> withExpectedControlIndex (Just (controlIndex 999)) base
    RequestHomeMismatch -> withHome fixtureRemoteHeraldEpoch base
    _ -> base
  where
    sequenceNumber = 1000 + fromIntegral slot
    base = envelopeFor sequenceNumber (startProcessEpochCommand (candidateBootstrap candidate))

candidateBootstrap :: Candidate -> ProcessStart
candidateBootstrap candidate = case candidate of
  StartA -> fixtureStart
  StartB -> fixtureAlternateStart
  StartWithLiveProcessId -> fixtureThirdStart
  WrongResidence -> fixtureRemoteStart
  StartC -> thirdFreshStart
  ReusedEpoch -> reusedEpochStart
  InactiveHome -> fixtureStart
  StaleExpectedIndex -> fixtureStart
  RequestHomeMismatch -> fixtureStart

thirdFreshStart :: ProcessStart
thirdFreshStart = processStart (processId 169) (processEpochId 175) fixtureHeraldEpoch

reusedEpochStart :: ProcessStart
reusedEpochStart = processStart (processId 170) (processEpochId 175) fixtureHeraldEpoch

inactiveHeraldEpoch :: HeraldEpoch
inactiveHeraldEpoch = heraldEpoch 250

referenceStep :: Word8 -> OracleEnvelope -> ReferenceModel -> (ReferenceModel, ExpectedStep)
referenceStep slot envelope model =
  case Map.lookup requestId (referenceRequests model) of
    Just retained
      | referenceRequestDigest retained == digest ->
          (model, ExpectedDuplicate retained)
      | otherwise ->
          (model, ExpectedConflict retained digest)
    Nothing ->
      let indexWord = referenceGreatestIndex model + 1
          index = controlIndex indexWord
          result = referenceFreshResult envelope model
          acceptedBootstrap = case result of
            OracleAccepted -> Just (envelopeBootstrap envelope)
            OracleRejected _ -> Nothing
          request =
            ReferenceRequest
              { referenceRequestId = requestId,
                referenceRequestDigest = digest,
                referenceRequestIndex = index,
                referenceRequestResult = result
              }
          retainedModel =
            model
              { referenceGreatestIndex = indexWord,
                referenceRequests = Map.insert requestId request (referenceRequests model),
                referenceSlots = Map.insert slot envelope (referenceSlots model)
              }
          successor = maybe retainedModel (insertAccepted index retainedModel) acceptedBootstrap
       in (successor, ExpectedFresh request acceptedBootstrap)
  where
    requestId = oracleEnvelopeRequestId envelope
    digest = oracleEnvelopeDigest envelope

insertAccepted :: ControlIndex -> ReferenceModel -> ProcessStart -> ReferenceModel
insertAccepted index model bootstrap =
  model
    { referenceStarts =
        Map.insert
          (processStartProcessEpochId bootstrap)
          (bootstrap, index)
          (referenceStarts model),
      referenceEpochIds =
        Set.insert
          (processStartProcessEpochId bootstrap)
          (referenceEpochIds model),
      referenceLiveProcessIds =
        Set.insert
          (processStartProcessId bootstrap)
          (referenceLiveProcessIds model)
    }

referenceFreshResult :: OracleEnvelope -> ReferenceModel -> OracleReceiptResult
referenceFreshResult envelope model
  | requestHome /= claimedHome = OracleRejected (RequestHomeEpochMismatch requestHome claimedHome)
  | claimedHome `notElem` fmap heraldMemberEpoch fixtureMembers = OracleRejected (InactiveHomeHerald claimedHome)
  | Just expected <- oracleEnvelopeExpectedControlIndex envelope,
    expected /= controlIndex (referenceGreatestIndex model) =
      OracleRejected (StaleExpectedControlIndex expected (controlIndex (referenceGreatestIndex model)))
  | processStartResidence supplied /= claimedHome =
      OracleRejected
        ( StartProcessResidenceMismatch
            (processStartProcessEpochId supplied)
            claimedHome
            (processStartResidence supplied)
        )
  | Set.member epoch (referenceEpochIds model) = OracleRejected (ProcessEpochAlreadyStarted epoch)
  | Set.member process (referenceLiveProcessIds model) = OracleRejected (ProcessAlreadyStarted process)
  | otherwise = OracleAccepted
  where
    requestHome = oracleClientRequestHome (oracleEnvelopeRequestId envelope)
    claimedHome = oracleEnvelopeHomeHeraldEpoch envelope
    supplied = envelopeBootstrap envelope
    epoch = processStartProcessEpochId supplied
    process = processStartProcessId supplied

envelopeBootstrap :: OracleEnvelope -> ProcessStart
envelopeBootstrap envelope =
  case oracleCommandStart (oracleEnvelopeCommand envelope) of
    Just bootstrap -> bootstrap
    Nothing -> error "generated Start model produced a non-Start command"

checkTransition ::
  OracleState ->
  OracleState ->
  OracleEnvelope ->
  OracleStepOutcome ->
  OracleEffectBatch ->
  ExpectedStep ->
  Either String ObservedDisposition
checkTransition predecessor successor envelope outcome effects expected =
  case expected of
    ExpectedFresh request acceptedBootstrap -> do
      receipt <- case outcome of
        OracleCommitted value -> Right value
        other -> Left ("expected a committed receipt, got " <> show other)
      checkFacts
        [ ("fresh receipt", receiptMatches request receipt),
          ("fresh transition changes state", predecessor /= successor),
          ("fresh transition changes state digest", oracleStateDigest predecessor /= oracleStateDigest successor)
        ]
      entry <- case oracleEffects effects of
        [EmitAppliedOracleEntry value] -> Right value
        other -> Left ("expected exactly one applied entry, got " <> show other)
      checkFreshEntry successor envelope request acceptedBootstrap receipt entry
      case referenceRequestResult request of
        OracleAccepted -> Right ObservedFreshAccepted
        OracleRejected rejection -> Right (ObservedFreshRejected rejection)
    ExpectedDuplicate request -> do
      receipt <- case outcome of
        OracleDuplicate value -> Right value
        other -> Left ("expected duplicate disposition, got " <> show other)
      checkFacts
        [ ("duplicate returns retained receipt", receiptMatches request receipt),
          ("duplicate preserves complete state", predecessor == successor),
          ("duplicate preserves state digest", oracleStateDigest predecessor == oracleStateDigest successor),
          ("duplicate emits no effects", null (oracleEffects effects))
        ]
      Right ObservedDuplicate
    ExpectedConflict retained suppliedDigest -> do
      checkFacts
        [ ( "exact conflict disposition",
            outcome
              == OracleProtocolRejected
                ( ConflictingOracleRequestId
                    (referenceRequestId retained)
                    (referenceRequestDigest retained)
                    suppliedDigest
                )
          ),
          ("conflict envelope has retained request ID", oracleEnvelopeRequestId envelope == referenceRequestId retained),
          ("conflict preserves complete state", predecessor == successor),
          ("conflict preserves state digest", oracleStateDigest predecessor == oracleStateDigest successor),
          ("conflict emits no effects", null (oracleEffects effects))
        ]
      Right ObservedConflict

checkFreshEntry ::
  OracleState ->
  OracleEnvelope ->
  ReferenceRequest ->
  Maybe ProcessStart ->
  OracleReceipt ->
  AppliedOracleEntry ->
  Either String ()
checkFreshEntry successor envelope request acceptedBootstrap receipt entry = do
  checkFacts
    [ ("entry index", appliedEntryControlIndex entry == referenceRequestIndex request),
      ("entry request ID", appliedEntryRequestId (commandEntry entry) == oracleEnvelopeRequestId envelope),
      ("entry command digest", appliedEntryCommandDigest (commandEntry entry) == referenceRequestDigest request),
      ("entry receipt", appliedEntryReceipt (commandEntry entry) == receipt),
      ("entry post-state digest", appliedEntryPostStateDigest entry == oracleStateDigest successor)
    ]
  case (acceptedBootstrap, appliedEntryProjectionEvents entry) of
    (Just bootstrap, [event]) ->
      case oracleProjectionEventView event of
        ProcessStartedView started ->
          checkFacts
            [ ("accepted event process", processRecordProcessId started == processStartProcessId bootstrap),
              ("accepted event epoch", processRecordProcessEpoch started == processStartProcessEpochId bootstrap),
              ("accepted event residence", processRecordResidence started == processStartResidence bootstrap),
              ( "accepted event manifest and index",
                processRecordOrigin started
                  == DynamicStartView
                    (referenceRequestIndex request)
                    (referenceRequestId request)
              )
            ]
        other -> Left ("accepted fresh Start emitted the wrong projection event: " <> show other)
    (Nothing, []) -> Right ()
    (Just _, []) -> Left "accepted fresh Start omitted its projection event"
    (Nothing, _ : _) -> Left "rejected fresh Start emitted a projection event"
    (Just _, _ : _) -> Left "accepted fresh Start emitted a non-singleton projection vector"

checkReferenceState :: OracleState -> ReferenceModel -> Either String ()
checkReferenceState state model = do
  checkFacts
    [ ("greatest control index", oracleGreatestControlIndex state == controlIndex (referenceGreatestIndex model)),
      ("retained request count", oracleRequestCount state == Map.size (referenceRequests model)),
      ("dynamic Start projection", actualStarts state == referenceStarts model)
    ]
  mapM_ (checkRetainedReceipt state) (Map.elems (referenceRequests model))
  checkCanonicalRequestTranscripts state model
  mapM_
    ( \(processEpoch, (bootstrap, _)) ->
        checkFacts
          [ ( "dynamic process residence",
              oracleLiveProcessResidence processEpoch state
                == Just (processStartResidence bootstrap)
            )
          ]
    )
    (Map.toList (referenceStarts model))

-- Read the retained bytes from the state transcript, independently of the
-- ordinary receipt encoder. This catches a stale or misassociated cached
-- receipt even when the state digest consistently hashes those wrong bytes.
checkCanonicalRequestTranscripts :: OracleState -> ReferenceModel -> Either String ()
checkCanonicalRequestTranscripts state model = do
  expected <- traverse expectedTranscript (Map.elems (referenceRequests model))
  (index, actual) <-
    SerializeGet.runGet getCanonicalRequestTranscripts (oracleStateCanonicalBytes state)
  checkFacts
    [ ("canonical request table control index", index == referenceGreatestIndex model),
      ("retained canonical request transcripts", actual == expected)
    ]
  where
    expectedTranscript request = do
      let identifier = referenceRequestId request
      receipt <-
        maybe
          (Left ("missing canonical receipt " <> show identifier))
          Right
          (oracleRequestReceipt identifier state)
      Right
        ( heraldEpochBytes (oracleClientRequestHome identifier),
          oracleClientRequestSequence identifier,
          oracleCommandDigestBytes (referenceRequestDigest request),
          oracleReceiptCanonicalBytes receipt
        )

type CanonicalRequestTranscript =
  (ByteString.ByteString, Word64, ByteString.ByteString, ByteString.ByteString)

-- Only the fixed canonical prefix is interpreted here: domain, genesis,
-- control index, then the ordered request table. Existing whole-state golden
-- vectors cover the remaining owner sections. Identities and digests have the
-- canonical 32-byte widths; all counts and frames use unsigned 64-bit lengths.
getCanonicalRequestTranscripts :: SerializeGet.Get (Word64, [CanonicalRequestTranscript])
getCanonicalRequestTranscripts = do
  domain <- getFramedTranscript
  if domain == "ECLIPS-ORACLE-STATE"
    then pure ()
    else fail "unexpected Oracle state transcript domain"
  SerializeGet.skip (3 * 32)
  memberCount <- SerializeGet.getWord64be
  SerializeGet.skip (fromIntegral memberCount * 64 + 2 * 32)
  index <- SerializeGet.getWord64be
  requestCount <- SerializeGet.getWord64be
  requests <-
    replicateM (fromIntegral requestCount)
      $ (,,,)
        <$> SerializeGet.getByteString 32
        <*> SerializeGet.getWord64be
        <*> SerializeGet.getByteString 32
        <*> getFramedTranscript
  SerializeGet.remaining >>= SerializeGet.skip
  pure (index, requests)

getFramedTranscript :: SerializeGet.Get ByteString.ByteString
getFramedTranscript = do
  byteCount <- SerializeGet.getWord64be
  SerializeGet.getByteString (fromIntegral byteCount)

actualStarts :: OracleState -> Map ProcessEpochId (ProcessStart, ControlIndex)
actualStarts state =
  Map.fromList
    [ ( processRecordProcessEpoch started,
        (processStart (processRecordProcessId started) (processRecordProcessEpoch started) (processRecordResidence started), index)
      )
    | started <- oracleProcessRecords state,
      DynamicStartView index _ <- [processRecordOrigin started]
    ]

checkRetainedReceipt :: OracleState -> ReferenceRequest -> Either String ()
checkRetainedReceipt state request =
  case oracleRequestReceipt (referenceRequestId request) state of
    Nothing -> Left ("missing retained request " <> show (referenceRequestId request))
    Just receipt -> checkFacts [("retained receipt fields", receiptMatches request receipt)]

receiptMatches :: ReferenceRequest -> OracleReceipt -> Bool
receiptMatches request receipt =
  oracleReceiptRequestId receipt == referenceRequestId request
    && oracleReceiptCommandDigest receipt == referenceRequestDigest request
    && oracleReceiptControlIndex receipt == referenceRequestIndex request
    && oracleReceiptResult receipt == referenceRequestResult request

checkFacts :: [(String, Bool)] -> Either String ()
checkFacts facts =
  case find (not . snd) facts of
    Nothing -> Right ()
    Just (label, _) -> Left (label <> " did not match the reference model")

committedState :: OracleEnvelope -> OracleState -> (OracleState, OracleReceipt)
committedState envelope state =
  case checked "committed model transition" (stepOracle envelope state) of
    (successor, OracleCommitted receipt, _) -> (successor, receipt)
    (_, other, _) -> error ("expected committed result, got " <> show other)

requireStarted :: ProcessEpochId -> OracleState -> ProcessEpochRecord
requireStarted process state =
  maybe (error "expected started-process projection") id (oracleProcessRecord process state)

withRequestId :: OracleClientRequestId -> OracleEnvelope -> OracleEnvelope
withRequestId request envelope =
  oracleEnvelope
    request
    (oracleEnvelopeExpectedControlIndex envelope)
    (oracleEnvelopeHomeHeraldEpoch envelope)
    (oracleEnvelopeCommand envelope)

withExpectedControlIndex :: Maybe ControlIndex -> OracleEnvelope -> OracleEnvelope
withExpectedControlIndex expected envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    expected
    (oracleEnvelopeHomeHeraldEpoch envelope)
    (oracleEnvelopeCommand envelope)

withHome :: HeraldEpoch -> OracleEnvelope -> OracleEnvelope
withHome home envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    (oracleEnvelopeExpectedControlIndex envelope)
    home
    (oracleEnvelopeCommand envelope)

withCommand :: OracleCommand -> OracleEnvelope -> OracleEnvelope
withCommand command envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    (oracleEnvelopeExpectedControlIndex envelope)
    (oracleEnvelopeHomeHeraldEpoch envelope)
    command

initialStateWith :: ConfigurationDigest -> OracleState
initialStateWith configurationDigest =
  checked "catalogue-specific initial state" $ do
    genesis <-
      checkOracleGenesis
        ( oracleGenesis
            fixtureSystemId
            fixtureMembers
            profileCatalogueDigest
            fixtureDescriptors
            fixtureBootstraps
            configurationDigest
            fixtureTopology
            (deriveInitialProjectionDigest fixtureBootstraps fixtureTopology)
            fixtureBindings
            fixtureRaftConfiguration
            (deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureRaftConfiguration)
        )
    initialOracleWithStateDigestMode OracleStateDigestEnabled genesis

commandEntry :: AppliedOracleEntry -> AppliedOracleCommandEntry
commandEntry entry = case appliedEntryCommand entry of
  Just command -> command
  Nothing -> error "command-only property fixture emitted a native configuration"
