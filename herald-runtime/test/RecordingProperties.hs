module RecordingProperties
  ( tests,
  )
where

import Data.List (isInfixOf, mapAccumL)
import Data.Unique (newUnique)
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation (ApplicationOperation (NewIdApplication))
import Eclips.Herald.Administration (checkedInitialAdministrationBinding)
import Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (..),
    ApplicationOutbound (AcceptApplicationCandidate),
    admitApplicationClientDto,
    projectApplicationEffect,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
    sessionAcceptanceBinding,
    sessionBindingSessionId,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Initialization
  ( HeraldState,
    initialHerald,
  )
import Eclips.Herald.Input
  ( HeraldInput,
    HeraldInputBody (..),
    RuntimeObservation (..),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (..),
    SourceId (..),
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchOrdinal (..),
    EffectMemberOrdinal (..),
    StepOrdinal (..),
  )
import Eclips.Herald.Runtime.Internal.Recording
  ( NormalizedHeraldEffectClass (..),
    NormalizedPhysicalName (..),
    NormalizedRuntimeEventOrdinal (..),
    NormalizedRuntimeTrace (..),
    NormalizedRuntimeTraceEvent (..),
    NormalizedShellTraceEvent (..),
    NormalizedSourceName (..),
    RuntimeTrace,
    RuntimeTraceFinalEvidence (..),
    RuntimeTraceTerminal (..),
    TypedReplayMismatch (..),
    TypedReplayResult (..),
    normalizeRuntimeTrace,
    replayRuntimeTrace,
    runtimeTraceApplicationRecoveryConfiguration,
    runtimeTraceEvents,
    runtimeTraceFinalEvidence,
    runtimeTraceInitialBatch,
    runtimeTraceInitialGeneratorSeed,
    runtimeTraceInitialObservation,
    runtimeTraceInitialOracleContacts,
    runtimeTracePeerRecoveryConfiguration,
    runtimeTraceSnapshot,
    runtimeTraceTerminal,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    PhysicalConnectionName (..),
    PhysicalConnectionRef (..),
    PhysicalLaneClass (..),
    RuntimeEventOrdinal (..),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
  )
import Eclips.Herald.Runtime.Internal.Types
  ( ConnectionRef (..),
    HeraldRuntimeFailure (..),
    HeraldRuntimeFailureClass (..),
    RuntimeDiagnosticClass (..),
    RuntimeLaneOffer (..),
    RuntimeToken (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
  )
import Eclips.Herald.Transition (stepHerald)
import Eclips.Protocol.Application.Types qualified as Protocol
import RuntimeFixtures
  ( fixtureAlternateApplicationRecoveryConfiguration,
    fixtureAlternateGeneratorSeed,
    fixtureAlternateOracleContacts,
    fixtureAlternatePeerRecoveryConfiguration,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureOpenDto,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
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
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "exact recording normalization and typed replay"
    [ testCase "typed replay reproduces every batch and the exact final state" caseReplayFinalState,
      testCase "typed replay reproduces a recorded invariant fault" caseReplayInvariantFault,
      testCase "reordering selected inputs is detected independently" caseReorderedInput,
      testCase "a stale physical event remains shell-only during replay" caseStaleShellEvent,
      testCase "normalization drops bookkeeping and freshly renames retained events" caseNormalization,
      testCase "normalization drops successful lane scheduling but retains lane loss" caseLaneOfferNormalization,
      testCase "stale physical names cannot collide with runtime-completion source names" casePhysicalSourceDomains,
      testCase "typed replay checks the retained final-state evidence" caseFinalStateEvidence,
      testCase "typed replay retains and checks the exact generator seed" caseGeneratorSeedEvidence,
      testCase "typed replay retains and checks the exact Oracle contacts" caseOracleContactEvidence,
      testCase "typed replay retains and checks the exact application recovery configuration" caseApplicationRecoveryConfigurationEvidence,
      testCase "typed replay retains and checks the exact peer recovery configuration" casePeerRecoveryConfigurationEvidence,
      testProperty "generated mixed monotone logical traces replay exactly" propGeneratedReplay,
      testProperty "normalization is deterministic" propNormalizationDeterministic
    ]

caseReplayFinalState :: Assertion
caseReplayFinalState = do
  let trace = fixtureTrace (fixtureInputs [101, 102, 108])
  case runtimeTraceFinalEvidence trace of
    RuntimeTraceFinalState finalState ->
      assertEqual
        "replay returns the exact retained final state"
        (Right (TypedReplayFinalState finalState))
        (fixtureReplay trace)
    RuntimeTraceFinalInvariantFault fault ->
      assertFailure ("a monotone fixture faulted: " <> show fault)

caseReplayInvariantFault :: Assertion
caseReplayInvariantFault = do
  let trace = fixtureTrace (fixtureInputs [99])
  case runtimeTraceFinalEvidence trace of
    RuntimeTraceFinalState _ -> assertFailure "a regressing observation was accepted"
    RuntimeTraceFinalInvariantFault fault ->
      assertEqual
        "the independently observed fault is exact"
        (Right (TypedReplayInvariantFault fault))
        (fixtureReplay trace)

caseReorderedInput :: Assertion
caseReorderedInput = do
  let original = fixtureTrace (fixtureInputs [101, 102])
      reordered = replaceStepInputs (reverse (fixtureInputs [101, 102])) original
  case fixtureReplay reordered of
    Left TypedReplayStepResultMismatch {} -> pure ()
    other -> assertFailure ("reordered inputs were not detected: " <> show other)

caseStaleShellEvent :: Assertion
caseStaleShellEvent = do
  let original = fixtureTrace (fixtureInputs [101, 102])
      stale = ShellEvent (RuntimeEventOrdinal 0) (ShellStaleSuppressed Nothing (Just peerSource))
      withStale = replaceEvents (renumberEvents (insertAfterInitialization stale (runtimeTraceEvents original))) original
  assertEqual
    "shell-only suppression cannot become a kernel input"
    (fixtureReplay original)
    (fixtureReplay withStale)

caseNormalization :: Assertion
caseNormalization = do
  token <- RuntimeToken <$> newUnique
  let original = fixtureTrace [fixtureOpenInput]
      peerReference = PhysicalPeerRef (ConnectionRef token 31)
      applicationReference = PhysicalApplicationRef (ConnectionRef token 72)
      initialEvent = case runtimeTraceEvents original of
        event : _ -> event
        [] -> error "fixture trace omitted initialization"
      effect = case runtimeTraceEvents original of
        _ : KernelEvent _ (KernelTraceStepped _ _ _ (Right batch)) : _ -> case effectBatchMembers batch of
          member : _ -> member
          [] -> error "application-open fixture unexpectedly emitted no effect"
        _ -> error "fixture trace omitted its successful step"
      terminalEffect = case effect of
        SetApplicationConnectionDisposition _ acceptance ->
          DisposeApplicationSession
            (sessionBindingSessionId (sessionAcceptanceBinding acceptance))
            ApplicationSessionNoLongerLive
        _ -> error "application-open fixture did not route its acceptance first"
      rawEvents =
        [ reordinal (RuntimeEventOrdinal 40) initialEvent,
          ShellEvent (RuntimeEventOrdinal 41) (ShellConnectionRegistered peerReference peerSource),
          ShellEvent (RuntimeEventOrdinal 92) (ShellDiagnosticEmitted RuntimeSuppressionDiagnostic),
          ShellEvent (RuntimeEventOrdinal 150) (ShellConnectionRegistered applicationReference applicationSource),
          ShellEvent (RuntimeEventOrdinal 151) (ShellBatchHanded (BatchOrdinal 9)),
          ShellEvent (RuntimeEventOrdinal 212) (ShellEffectRouted (BatchOrdinal 9) (EffectMemberOrdinal 0) effect),
          ShellEvent (RuntimeEventOrdinal 213) (ShellEffectRouted (BatchOrdinal 9) (EffectMemberOrdinal 1) terminalEffect),
          ShellEvent
            (RuntimeEventOrdinal 300)
            (ShellStaleSuppressed (Just (PhysicalConnectionName token 31)) (Just peerSource)),
          ShellEvent (RuntimeEventOrdinal 340) (ShellRuntimeExited HeraldRuntimeScopeClosed)
        ]
      raw = replaceEvents rawEvents original
      NormalizedRuntimeTrace _ _ _ _ _ normalized _ _ = normalizeRuntimeTrace raw
      peerName = NormalizedSourceName PeerSources 0
      applicationName = NormalizedSourceName ApplicationSources 1
  assertEqual
    "retained events get fresh contiguous ordinals and first-appearance source names"
    [ NormalizedShellEvent (normalizedOrdinal 0) (NormalizedConnectionRegistered peerName),
      NormalizedShellEvent (normalizedOrdinal 1) (NormalizedConnectionRegistered applicationName),
      NormalizedShellEvent (normalizedOrdinal 2) NormalizedBatchHanded,
      NormalizedShellEvent (normalizedOrdinal 3) (NormalizedEffectRouted NormalizedApplicationDisposition),
      NormalizedShellEvent (normalizedOrdinal 4) (NormalizedEffectRouted NormalizedApplicationSessionDisposal),
      NormalizedShellEvent (normalizedOrdinal 5) (NormalizedStalePhysicalSuppressed (NormalizedPhysicalName 0))
    ]
    normalized

casePhysicalSourceDomains :: Assertion
casePhysicalSourceDomains = do
  token <- RuntimeToken <$> newUnique
  let original = fixtureTrace []
      physical = PhysicalConnectionName token 9
      completion = SourceId RuntimeCompletionSources 9
      decorated =
        replaceEvents
          ( runtimeTraceEvents original
              <> [ ShellEvent (RuntimeEventOrdinal 40) (ShellStaleSuppressed (Just physical) Nothing),
                   ShellEvent (RuntimeEventOrdinal 41) (ShellStaleSuppressed Nothing (Just completion))
                 ]
          )
          original
      NormalizedRuntimeTrace _ _ _ _ _ normalized _ _ = normalizeRuntimeTrace decorated
  assertEqual
    "equal raw ordinals inhabit distinct physical and semantic-source domains"
    [ NormalizedShellEvent
        (normalizedOrdinal 0)
        (NormalizedStalePhysicalSuppressed (NormalizedPhysicalName 0)),
      NormalizedShellEvent
        (normalizedOrdinal 1)
        (NormalizedStaleSourceSuppressed (NormalizedSourceName RuntimeCompletionSources 0))
    ]
    normalized

caseLaneOfferNormalization :: Assertion
caseLaneOfferNormalization = do
  token <- RuntimeToken <$> newUnique
  let original = fixtureTrace []
      reference = PhysicalApplicationRef (ConnectionRef token 12)
      source = SourceId ApplicationSources 12
      decorated =
        replaceEvents
          ( runtimeTraceEvents original
              <> [ ShellEvent (RuntimeEventOrdinal 40) (ShellConnectionRegistered reference source),
                   ShellEvent (RuntimeEventOrdinal 41) (ShellLaneOffered reference ApplicationLane LaneOffered),
                   ShellEvent (RuntimeEventOrdinal 42) (ShellLaneOffered reference ApplicationLane LaneLost)
                 ]
          )
          original
      NormalizedRuntimeTrace _ _ _ _ _ normalized _ _ = normalizeRuntimeTrace decorated
      normalizedSource = NormalizedSourceName ApplicationSources 0
  assertEqual
    "only the route-losing callback result is causally retained"
    [ NormalizedShellEvent
        (normalizedOrdinal 0)
        (NormalizedConnectionRegistered normalizedSource),
      NormalizedShellEvent
        (normalizedOrdinal 1)
        (NormalizedLaneOffered normalizedSource ApplicationLane LaneLost)
    ]
    normalized

caseFinalStateEvidence :: Assertion
caseFinalStateEvidence = do
  let unchanged = fixtureTrace []
      changed = fixtureTrace (fixtureInputs [101])
  initialState <- requireFinalState unchanged
  let contradicted =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation changed)
          (runtimeTraceInitialOracleContacts changed)
          (runtimeTraceInitialGeneratorSeed changed)
          (runtimeTraceApplicationRecoveryConfiguration changed)
          (runtimeTracePeerRecoveryConfiguration changed)
          (runtimeTraceInitialBatch changed)
          (runtimeTraceEvents changed)
          (runtimeTraceTerminal changed)
          (RuntimeTraceFinalState initialState)
  assertEqual
    "matching batches alone cannot hide a different final state"
    (Left TypedReplayFinalStateMismatch)
    (fixtureReplay contradicted)

caseGeneratorSeedEvidence :: Assertion
caseGeneratorSeedEvidence = do
  let original = fixtureTrace [fixtureOpenInput, fixtureBareNewIdInput]
      wrongSeed =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation original)
          (runtimeTraceInitialOracleContacts original)
          fixtureAlternateGeneratorSeed
          (runtimeTraceApplicationRecoveryConfiguration original)
          (runtimeTracePeerRecoveryConfiguration original)
          (runtimeTraceInitialBatch original)
          (runtimeTraceEvents original)
          (runtimeTraceTerminal original)
          (runtimeTraceFinalEvidence original)
  assertEqual
    "the exact configured seed is retained in the trace header"
    fixtureGeneratorSeed
    (runtimeTraceInitialGeneratorSeed original)
  assertBool
    "ordinary trace rendering retains only the seed's opaque marker"
    ("<generator-seed>" `isInfixOf` show original)
  assertEqual
    "changing only the seed is detected by exact final-state replay"
    (Left TypedReplayFinalStateMismatch)
    (fixtureReplay wrongSeed)
  assertEqual
    "the opaque seed remains part of parity normalization"
    False
    (normalizeRuntimeTrace original == normalizeRuntimeTrace wrongSeed)

caseOracleContactEvidence :: Assertion
caseOracleContactEvidence = do
  let original = fixtureTrace []
      wrongContacts =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation original)
          fixtureAlternateOracleContacts
          (runtimeTraceInitialGeneratorSeed original)
          (runtimeTraceApplicationRecoveryConfiguration original)
          (runtimeTracePeerRecoveryConfiguration original)
          (runtimeTraceInitialBatch original)
          (runtimeTraceEvents original)
          (runtimeTraceTerminal original)
          (runtimeTraceFinalEvidence original)
  assertEqual
    "the exact checked contacts are retained in the trace header"
    fixtureOracleContacts
    (runtimeTraceInitialOracleContacts original)
  case fixtureReplay wrongContacts of
    Left TypedReplayInitializationBatchMismatch {} -> pure ()
    other -> assertFailure ("changed Oracle contacts were not detected: " <> show other)
  assertEqual
    "the contact set remains part of parity normalization"
    False
    (normalizeRuntimeTrace original == normalizeRuntimeTrace wrongContacts)

caseApplicationRecoveryConfigurationEvidence :: Assertion
caseApplicationRecoveryConfigurationEvidence = do
  let original = fixtureTrace []
      wrongRecovery =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation original)
          (runtimeTraceInitialOracleContacts original)
          (runtimeTraceInitialGeneratorSeed original)
          fixtureAlternateApplicationRecoveryConfiguration
          (runtimeTracePeerRecoveryConfiguration original)
          (runtimeTraceInitialBatch original)
          (runtimeTraceEvents original)
          (runtimeTraceTerminal original)
          (runtimeTraceFinalEvidence original)
  assertEqual
    "the exact checked application recovery configuration is retained in the trace header"
    fixtureApplicationRecoveryConfiguration
    (runtimeTraceApplicationRecoveryConfiguration original)
  assertEqual
    "changing only application recovery configuration is detected by exact final-state replay"
    (Left TypedReplayFinalStateMismatch)
    (fixtureReplay wrongRecovery)
  assertEqual
    "application recovery configuration remains part of parity normalization"
    False
    (normalizeRuntimeTrace original == normalizeRuntimeTrace wrongRecovery)

casePeerRecoveryConfigurationEvidence :: Assertion
casePeerRecoveryConfigurationEvidence = do
  let original = fixtureTrace []
      wrongRecovery =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation original)
          (runtimeTraceInitialOracleContacts original)
          (runtimeTraceInitialGeneratorSeed original)
          (runtimeTraceApplicationRecoveryConfiguration original)
          fixtureAlternatePeerRecoveryConfiguration
          (runtimeTraceInitialBatch original)
          (runtimeTraceEvents original)
          (runtimeTraceTerminal original)
          (runtimeTraceFinalEvidence original)
  assertEqual
    "the exact checked recovery configuration is retained in the trace header"
    fixturePeerRecoveryConfiguration
    (runtimeTracePeerRecoveryConfiguration original)
  assertEqual
    "changing only peer recovery configuration is detected by exact final-state replay"
    (Left TypedReplayFinalStateMismatch)
    (fixtureReplay wrongRecovery)
  assertEqual
    "peer recovery configuration remains part of parity normalization"
    False
    (normalizeRuntimeTrace original == normalizeRuntimeTrace wrongRecovery)

propGeneratedReplay :: [Positive Int] -> Property
propGeneratedReplay positiveDeltas =
  let deltas = fmap (\(Positive value) -> fromIntegral (1 + value `mod` 100)) (take 200 positiveDeltas)
      observations = drop 1 (scanl (+) 101 deltas)
      inputs = fixtureOpenInput : fixtureInputs observations
      trace = fixtureTrace inputs
   in counterexample ("trace length: " <> show (length inputs))
        $ fixtureReplay trace === replayExpected trace

propNormalizationDeterministic :: [Positive Int] -> Property
propNormalizationDeterministic positiveDeltas =
  let deltas = fmap (\(Positive value) -> fromIntegral (1 + value `mod` 100)) (take 200 positiveDeltas)
      observations = drop 1 (scanl (+) 101 deltas)
      trace = fixtureTrace (fixtureOpenInput : fixtureInputs observations)
      decorated = decorateWithBookkeeping trace
   in normalizeRuntimeTrace trace === normalizeRuntimeTrace decorated

fixtureReplay :: RuntimeTrace -> Either TypedReplayMismatch TypedReplayResult
fixtureReplay = replayRuntimeTrace fixtureCheckedGenesis fixtureCheckedBootstraps

replayExpected :: RuntimeTrace -> Either TypedReplayMismatch TypedReplayResult
replayExpected trace = case runtimeTraceFinalEvidence trace of
  RuntimeTraceFinalState state -> Right (TypedReplayFinalState state)
  RuntimeTraceFinalInvariantFault fault -> Right (TypedReplayInvariantFault fault)

fixtureInputs :: [Integer] -> [HeraldInput]
fixtureInputs = fmap $ \observed ->
  heraldInput
    (monotonicInstant (fromIntegral observed))
    ( RuntimeObserved
        (AdministrationBindingLost (checkedInitialAdministrationBinding fixtureCheckedGenesis))
    )

fixtureOpenInput :: HeraldInput
fixtureOpenInput =
  heraldInput (monotonicInstant 101) $ case admitApplicationClientDto
    (CandidateApplicationIngress (candidateApplicationLane 17))
    fixtureOpenDto of
    Right body -> body
    Left problem -> error ("fixture open DTO was not admitted: " <> show problem)

fixtureBareNewIdInput :: HeraldInput
fixtureBareNewIdInput =
  case initialHerald fixtureInitialObservation fixtureCheckedGenesis fixtureCheckedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration of
    Left fault -> error ("fixture initialization failed before newid: " <> show fault)
    Right (initialState, _) -> case stepHerald fixtureOpenInput initialState of
      Left fault -> error ("fixture open failed before newid: " <> show fault)
      Right (_, openingBatch) ->
        case [ (binding, session)
             | effect <- effectBatchMembers openingBatch,
               Right
                 ( Just
                     ( AcceptApplicationCandidate
                         _
                         binding
                         (Protocol.SessionOpened _ session _ _)
                       )
                   ) <-
                 [projectApplicationEffect effect]
             ] of
          [(binding, session)] ->
            heraldInput (monotonicInstant 102) $ case admitApplicationClientDto
              (EstablishedApplicationIngress binding)
              (Protocol.Call session (Protocol.applicationRequestIdClaim 0) (NewIdApplication BareNewId)) of
              Right body -> body
              Left problem -> error ("fixture newid DTO was not admitted: " <> show problem)
          unexpected -> error ("fixture open did not produce one accepted session: " <> show unexpected)

fixtureTrace :: [HeraldInput] -> RuntimeTrace
fixtureTrace inputs = case initialHerald fixtureInitialObservation fixtureCheckedGenesis fixtureCheckedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration of
  Left fault -> error ("fixture initialization failed: " <> show fault)
  Right (initialState, initialBatch) ->
    let initialized = KernelEvent (RuntimeEventOrdinal 0) (KernelTraceInitialized (BatchOrdinal 0) initialBatch)
     in finishFixture initialBatch initialized initialState inputs

finishFixture :: EffectBatch -> RuntimeTraceEvent -> HeraldState -> [HeraldInput] -> RuntimeTrace
finishFixture initialBatch initialized initialState inputs =
  case recordSteps 1 (StepOrdinal 0) initialState inputs of
    (events, RuntimeTraceFinalState finalState) ->
      runtimeTraceSnapshot
        fixtureInitialObservation
        fixtureOracleContacts
        fixtureGeneratorSeed
        fixtureApplicationRecoveryConfiguration
        fixturePeerRecoveryConfiguration
        initialBatch
        (initialized : events)
        (RuntimeTraceExited HeraldRuntimeScopeClosed)
        (RuntimeTraceFinalState finalState)
    (events, RuntimeTraceFinalInvariantFault fault) ->
      runtimeTraceSnapshot
        fixtureInitialObservation
        fixtureOracleContacts
        fixtureGeneratorSeed
        fixtureApplicationRecoveryConfiguration
        fixturePeerRecoveryConfiguration
        initialBatch
        (initialized : events)
        (RuntimeTraceFailed (HeraldRuntimeFailure HeraldRuntimeKernelInvariantFailure))
        (RuntimeTraceFinalInvariantFault fault)

recordSteps ::
  Integer ->
  StepOrdinal ->
  HeraldState ->
  [HeraldInput] ->
  ([RuntimeTraceEvent], RuntimeTraceFinalEvidence)
recordSteps _ _ state [] = ([], RuntimeTraceFinalState state)
recordSteps eventOrdinal stepOrdinal state (input : inputs) =
  case stepHerald input state of
    Left fault ->
      ( [KernelEvent (RuntimeEventOrdinal (fromIntegral eventOrdinal)) (KernelTraceStepped stepOrdinal completionSource input (Left fault))],
        RuntimeTraceFinalInvariantFault fault
      )
    Right (successor, batch) ->
      let event = KernelEvent (RuntimeEventOrdinal (fromIntegral eventOrdinal)) (KernelTraceStepped stepOrdinal completionSource input (Right batch))
          (events, finalEvidence) = recordSteps (eventOrdinal + 1) (nextStep stepOrdinal) successor inputs
       in (event : events, finalEvidence)

replaceStepInputs :: [HeraldInput] -> RuntimeTrace -> RuntimeTrace
replaceStepInputs inputs trace = replaceEvents successor trace
  where
    (_, successor) = mapAccumL replace inputs (runtimeTraceEvents trace)
    replace retained event = case event of
      KernelEvent ordinal (KernelTraceStepped step source _ result) -> case retained of
        input : rest -> (rest, KernelEvent ordinal (KernelTraceStepped step source input result))
        [] -> ([], event)
      _ -> (retained, event)

replaceEvents :: [RuntimeTraceEvent] -> RuntimeTrace -> RuntimeTrace
replaceEvents events trace =
  runtimeTraceSnapshot
    (runtimeTraceInitialObservation trace)
    (runtimeTraceInitialOracleContacts trace)
    (runtimeTraceInitialGeneratorSeed trace)
    (runtimeTraceApplicationRecoveryConfiguration trace)
    (runtimeTracePeerRecoveryConfiguration trace)
    (runtimeTraceInitialBatch trace)
    events
    (runtimeTraceTerminal trace)
    (runtimeTraceFinalEvidence trace)

decorateWithBookkeeping :: RuntimeTrace -> RuntimeTrace
decorateWithBookkeeping trace =
  replaceEvents
    ( zipWith
        (reordinal . RuntimeEventOrdinal . (\index -> 10 + 7 * index))
        [0 ..]
        ( concatMap
            (\event -> [event, ShellEvent (RuntimeEventOrdinal 0) (ShellDiagnosticEmitted RuntimeSuppressionDiagnostic)])
            (runtimeTraceEvents trace)
        )
    )
    trace

insertAfterInitialization :: RuntimeTraceEvent -> [RuntimeTraceEvent] -> [RuntimeTraceEvent]
insertAfterInitialization inserted = \case
  [] -> [inserted]
  first : rest -> first : inserted : rest

renumberEvents :: [RuntimeTraceEvent] -> [RuntimeTraceEvent]
renumberEvents = zipWith (reordinal . RuntimeEventOrdinal) [0 ..]

reordinal :: RuntimeEventOrdinal -> RuntimeTraceEvent -> RuntimeTraceEvent
reordinal ordinal = \case
  KernelEvent _ event -> KernelEvent ordinal event
  ShellEvent _ event -> ShellEvent ordinal event

requireFinalState :: RuntimeTrace -> IO HeraldState
requireFinalState trace = case runtimeTraceFinalEvidence trace of
  RuntimeTraceFinalState state -> pure state
  RuntimeTraceFinalInvariantFault fault -> assertFailure ("expected final state, got " <> show fault)

nextStep :: StepOrdinal -> StepOrdinal
nextStep (StepOrdinal step) = StepOrdinal (step + 1)

normalizedOrdinal :: Integer -> NormalizedRuntimeEventOrdinal
normalizedOrdinal = NormalizedRuntimeEventOrdinal . fromIntegral

fixtureInitialObservation :: MonotonicInstant
fixtureInitialObservation = monotonicInstant 100

completionSource :: SourceId
completionSource = SourceId RuntimeCompletionSources 9

peerSource :: SourceId
peerSource = SourceId PeerSources 31

applicationSource :: SourceId
applicationSource = SourceId ApplicationSources 72
