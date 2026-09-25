{-# LANGUAGE ImportQualifiedPost #-}

module ApplicationOwnerProperties (tests) where

import ApplicationLabelProperties (heldFixtureStateForOwnerProperties)
import Data.ByteString qualified as ByteString
import Data.List (tails)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16, Word8)
import Eclips.Application.Types.Label (LabelResult (LabelNotApplied))
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewIdApplication),
  )
import Eclips.Application.Types.Result (RegularCallResult (LabelCompleted))
import Eclips.Domain.Identity
  ( GlobalObjectId,
    NablaId,
    ProcessEpochId,
    globalObjectIdFromNablaId,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    nablaIdFromGlobalObjectId,
  )
import Eclips.Domain.Label (labelProcessAcceptancePositionProcess)
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Completed),
    requestId,
  )
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Publication.Groups qualified as Groups
import Eclips.Herald.Startup.State (startupApplicationState)
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
    "live application owner laws"
    [ testCase
        "the four request classes are disjoint and only ordinary results are exposed"
        caseFourRequestClasses,
      testCase
        "sequencing selection identifies one caller and target publication group"
        caseSequencingSelection,
      testCase
        "fresh and retained label admission reject another caller's selection"
        caseSelectionCallerAdmission,
      testCase
        "label completion updates the ordinary body without changing request identity"
        caseOrdinaryLabelCompletionIdentity,
      testProperty
        "endpoint ingress remains unaccepted across membership gates until its label is installed"
        propEndpointIngressRetainsIdentity,
      testProperty
        "arbitrary duplicate calls preserve the four-class partition"
        propDuplicateCallsPreservePartition
    ]

caseFourRequestClasses :: Assertion
caseFourRequestClasses = do
  fixture <- fourClassFixture
  assertPartition fixture
  active <- requiredActive (fixtureState fixture)
  let state = fixtureState fixture
      session = fixtureSession fixture
      binding = fixtureBinding fixture
      activeProcess =
        labelProcessAcceptancePositionProcess
          (Application.activeLabelApplicationPosition active)
      heldProcesses =
        labelProcessAcceptancePositionProcess
          . Application.applicationFenceHeldPosition
          <$> Application.applicationFenceHeldEntries state
  assertBool
    "every retained fence hold is process-scoped to the active label"
    (not (null heldProcesses) && all (== activeProcess) heldProcesses)
  assertEqual
    "provider reconsideration sees candidates in canonical request-key order"
    (Set.toAscList (fixtureCandidates fixture))
    ( Application.applicationLabelCandidateKey
        <$> Application.applicationLabelCandidateEntries state
    )
  assertBool
    "ordinary reply bodies are the only resume-visible class"
    ( Set.fromList
        (fst <$> Application.applicationRequestReplyBodyEntries session state)
        == Set.map Application.applicationRequestKeyRequest (fixtureOrdinary fixture)
    )
  mapM_
    (assertResultDisposition "candidate" state session binding Nothing)
    (Set.toList (fixtureCandidates fixture))
  mapM_
    (assertResultDisposition "gated" state session binding Nothing)
    (Set.toList (fixtureGated fixture))
  mapM_
    (assertResultDisposition "held" state session binding Nothing)
    (Set.toList (fixtureHeld fixture))
  mapM_
    (assertOrdinaryDisposition state session binding)
    (Set.toList (fixtureOrdinary fixture))
  assertClassifiesAs
    state
    session
    binding
    (fixtureCandidateKey fixture)
    (fixtureLabelOperation fixture)
    Application.LabelCandidateApplicationRequestClass
  assertClassifiesAs
    state
    session
    binding
    (fixtureGatedKey fixture)
    (fixtureGatedOperation fixture)
    Application.GatedIngressApplicationRequestClass
  case fixtureHeldEntry fixture of
    (key, operation) ->
      assertClassifiesAs
        state
        session
        binding
        key
        operation
        Application.FenceHeldApplicationRequestClass
  assertConflict
    state
    session
    binding
    (fixtureCandidateKey fixture)
    (fixtureGatedOperation fixture)
  let closedCandidate =
        firstCandidate
          session
          binding
          (requestId 22)
          (fixtureLabelOperation fixture)
          state
  assertBool
    "the closed gate rejects a new pre-acceptance candidate"
    (isLeft (Application.prepareApplicationLabelCandidate closedCandidate))
  case Application.applicationFenceHeldEntries state of
    heldView : _ ->
      let heldCandidate =
            firstCandidate
              session
              binding
              (requestId 23)
              (Application.applicationFenceHeldOperation heldView)
              state
       in assertBool
            "the closed gate rejects a new positioned fence hold"
            ( isLeft
                ( Application.prepareApplicationFenceHeld
                    heldCandidate
                    (Application.applicationFenceHeldSemanticWork heldView)
                )
            )
    [] -> assertFailure "live four-class fixture lost held work"

caseSequencingSelection :: Assertion
caseSequencingSelection = do
  let target = globalObject 0x31
      caller = process 0x41
      other = process 0x42
      selection = Application.sequencingSelection caller target
  assertEqual "selection retains its explicit target" target (Application.sequencingSelectionTarget selection)
  assertEqual
    "selection is exactly the caller's target group"
    (Groups.groupKey caller target)
    (Application.sequencingSelectionGroup selection)
  assertBool
    "another caller's otherwise identical target is a different selection"
    (selection /= Application.sequencingSelection other target)

caseSelectionCallerAdmission :: Assertion
caseSelectionCallerAdmission = do
  fixture <- fourClassFixture
  active <- requiredActive (fixtureState fixture)
  let previousDecision = Application.activeLabelApplicationDecision active
      terminal = Application.commitApplicationLabelTerminal (checked "complete prior label" (Application.prepareApplicationLabelTerminal previousDecision LabelNotApplied (fixtureState fixture)))
      reopened = Application.setApplicationMembershipGate False terminal
      cleared = fst (Application.commitApplicationLabelWorkflowCompletion (checked "retire prior active label" (Application.prepareApplicationLabelWorkflowCompletion previousDecision reopened)))
      request = requestId 31
      candidate = firstCandidate (fixtureSession fixture) (fixtureBinding fixture) request (fixtureLabelOperation fixture) cleared
      caller = Application.applicationRequestCandidateProcess candidate
      target = Application.sequencingSelectionTarget (Application.activeLabelApplicationSelection active)
      selection = Application.sequencingSelection caller target
      wrongCaller = Application.sequencingSelection (process 0xf1) target
      wrongTarget = Application.sequencingSelection caller (globalObject 0xf2)
      decision = checked "next decision identity" (mkLabelDecisionId (ByteString.replicate 32 0xf3))
      expected = Left (Application.ApplicationSessionInvariantFault (Application.ApplicationLabelSelectionContradiction caller (Application.labelApplicationObject (Application.activeLabelApplicationCall active))))
      retained = Application.commitApplicationDetachedRequest (checked "retain a label candidate" (Application.prepareApplicationLabelCandidate candidate))
      retainedKey = Application.applicationRequestKey (fixtureSession fixture) request
  assertBool "the wrong caller fixture is distinct" (caller /= process 0xf1)
  assertBool "the wrong object fixture is distinct" (target /= globalObject 0xf2)
  assertEqual
    "fresh admission rejects the other caller despite the correct target"
    expected
    (fmap (const ()) (Application.prepareApplicationLabelAcceptance decision wrongCaller candidate))
  assertEqual
    "retained admission performs the same caller check"
    expected
    (fmap (const ()) (Application.prepareRetainedApplicationLabelAcceptance decision wrongCaller retainedKey retained))
  assertEqual
    "fresh admission still checks the target's private mapping"
    expected
    (fmap (const ()) (Application.prepareApplicationLabelAcceptance decision wrongTarget candidate))
  assertEqual
    "retained admission still checks the target's private mapping"
    expected
    (fmap (const ()) (Application.prepareRetainedApplicationLabelAcceptance decision wrongTarget retainedKey retained))
  assertEqual "fresh admission accepts the checked caller and target" (Right ()) (fmap (const ()) (Application.prepareApplicationLabelAcceptance decision selection candidate))
  assertEqual "retained admission accepts the checked caller and target" (Right ()) (fmap (const ()) (Application.prepareRetainedApplicationLabelAcceptance decision selection retainedKey retained))

caseOrdinaryLabelCompletionIdentity :: Assertion
caseOrdinaryLabelCompletionIdentity = do
  fixture <- fourClassFixture
  active <- requiredActive (fixtureState fixture)
  let decision = Application.activeLabelApplicationDecision active
      key = Application.activeLabelApplicationKey active
      beforeCall = Application.activeLabelApplicationCall active
      beforePosition = Application.activeLabelApplicationPosition active
      withTerminal =
        Application.commitApplicationLabelTerminal
          ( checked
              "retain label terminal"
              ( Application.prepareApplicationLabelTerminal
                  decision
                  LabelNotApplied
                  (fixtureState fixture)
              )
          )
      reopened =
        Application.setApplicationMembershipGate False withTerminal
      (completed, _) =
        Application.commitApplicationLabelWorkflowCompletion
          ( checked
              "complete label workflow"
              (Application.prepareApplicationLabelWorkflowCompletion decision reopened)
          )
      session = Application.applicationRequestKeySession key
      request = Application.applicationRequestKeyRequest key
      retained =
        checked
          "read completed label request"
          ( Application.applicationRequestResult
              session
              (fixtureBinding fixture)
              request
              completed
          )
  assertEqual
    "completion retains the exact request key"
    (Just beforeCall)
    ( Application.applicationOperationLabel
        =<< lookupRequestCall session request completed
    )
  assertEqual
    "completion retains the original process position"
    (Just beforePosition)
    (lookupRequestPosition session request completed)
  case retained of
    RetainedRequestReply _ (Completed observed (LabelCompleted LabelNotApplied)) ->
      assertEqual "completed request id" request observed
    other -> assertFailure ("unexpected completed label reply: " <> show other)
  assertEqual "workflow completion clears only the active owner" Nothing (Application.applicationActiveLabel completed)

propDuplicateCallsPreservePartition :: [(Word16, Word8)] -> Property
propDuplicateCallsPreservePartition choices = ioProperty $ do
  fixture <- fourClassFixture
  let observations = fmap (observeDuplicate fixture) choices
      correct = all id observations
  pure
    ( counterexample
        ("duplicate observations=" <> show observations)
        (correct && partitionValid fixture)
    )

propEndpointIngressRetainsIdentity :: [Bool] -> Property
propEndpointIngressRetainsIdentity membershipChanges = ioProperty $ do
  fixture <- fourClassFixture
  active <- requiredActive (fixtureState fixture)
  let oldDecision = Application.activeLabelApplicationDecision active
      terminal = Application.commitApplicationLabelTerminal (checked "finish previous owner" (Application.prepareApplicationLabelTerminal oldDecision LabelNotApplied (fixtureState fixture)))
      reopened = Application.setApplicationMembershipGate False terminal
      cleared = fst (Application.commitApplicationLabelWorkflowCompletion (checked "retire previous owner" (Application.prepareApplicationLabelWorkflowCompletion oldDecision reopened)))
      target = Application.sequencingSelectionTarget (Application.activeLabelApplicationSelection active)
      endpoint = nablaIdFromGlobalObjectId target
      decision = checked "endpoint decision" (mkLabelDecisionId (ByteString.replicate 32 0xe7))
      session = fixtureSession fixture
      binding = fixtureBinding fixture
      labelCandidate = firstCandidate session binding (requestId 31) (fixtureLabelOperation fixture) cleared
      selection = Application.sequencingSelection (Application.applicationRequestCandidateProcess labelCandidate) target
      prepared = checked "accept next label" (Application.prepareApplicationLabelAcceptance decision selection labelCandidate)
      preparedHeld = checked "hold exact target endpoint" (Application.retainApplicationLabelEndpointHold endpoint prepared)
      withLabel = fst (Application.commitApplicationLabelAcceptance preparedHeld)
      ingressRequest = requestId 32
      ingressKey = Application.applicationRequestKey session ingressRequest
      operation = NewIdApplication BareNewId
      ingress = firstCandidate session binding ingressRequest operation withLabel
      held = Application.commitApplicationDetachedRequest (checked "retain endpoint ingress" (Application.prepareApplicationEndpointHeldIngress decision ingress))
      states = scanl (flip Application.setApplicationMembershipGate) held membershipChanges
      final = last states
      installed = Application.commitApplicationLabelTerminal (checked "install terminal decision" (Application.prepareApplicationLabelTerminal decision LabelNotApplied final))
      released = Application.setApplicationMembershipGate False installed
      matchingEntry current = case [item | item <- Application.applicationGatedIngressEntries current, Application.applicationGatedIngressKey item == ingressKey] of
        [item] -> item
        _ -> error "expected exactly one retained endpoint ingress"
      entry = matchingEntry held
      ready = matchingEntry released
      redriven = checked "redrive original ingress" (Application.applicationGatedIngressForRedrive ingressKey released)
      originalCounter = Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness withLabel)
      valid state =
        Application.applicationEndpointAdmissionHoldFor endpoint state == Just decision
          && not (Application.applicationGatedIngressReady entry state)
          && Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness state) == originalCounter
          && case Application.classifyApplicationRequest session binding ingressRequest operation state of
            Right (Application.AttachedApplicationRequest Application.GatedIngressApplicationRequestClass) -> True
            _ -> False
  pure
    ( counterexample
        ("membership gate schedule=" <> show membershipChanges)
        ( all valid states
            && Application.applicationEndpointAdmissionHoldFor endpoint released == Nothing
            && Application.applicationGatedIngressReady ready released
            && Application.applicationRequestCandidateId redriven == ingressRequest
            && Application.applicationRequestCandidateCall redriven == operation
            && isLeft (Application.retainApplicationLabelEndpointHold (nabla 0xf4) prepared)
        )
    )

data FourClassFixture = FourClassFixture
  { fixtureState :: Application.State,
    fixtureSession :: ApplicationSessionId,
    fixtureBinding :: ApplicationSessionBinding,
    fixtureLabelOperation :: ApplicationOperation,
    fixtureGatedOperation :: ApplicationOperation,
    fixtureCandidateKey :: Application.ApplicationRequestKey,
    fixtureGatedKey :: Application.ApplicationRequestKey,
    fixtureHeldEntry :: (Application.ApplicationRequestKey, ApplicationOperation),
    fixtureOrdinary :: Set Application.ApplicationRequestKey,
    fixtureCandidates :: Set Application.ApplicationRequestKey,
    fixtureGated :: Set Application.ApplicationRequestKey,
    fixtureHeld :: Set Application.ApplicationRequestKey
  }

fourClassFixture :: IO FourClassFixture
fourClassFixture = do
  herald <- heldFixtureStateForOwnerProperties
  let initial = startupApplicationState herald
  sessionWitness <- case Application.applicationWitnessSessions (Application.applicationStateWitness initial) of
    [witness] -> pure witness
    witnesses -> assertFailure ("expected one live session, got " <> show (length witnesses))
  active <- requiredActive initial
  let session = Application.applicationSessionWitnessId sessionWitness
      binding = Application.applicationSessionWitnessBinding sessionWitness
      labelOperation =
        Application.labelApplicationOperation
          (Application.activeLabelApplicationCall active)
      candidateKey = Application.applicationRequestKey session (requestId 20)
      gatedKey = Application.applicationRequestKey session (requestId 21)
      withCandidate20 =
        Application.commitApplicationDetachedRequest
          ( checked
              "retain live candidate"
              ( Application.prepareApplicationLabelCandidate
                  (firstCandidate session binding (requestId 20) labelOperation initial)
              )
          )
      withCandidates =
        Application.commitApplicationDetachedRequest
          ( checked
              "retain earlier canonical live candidate"
              ( Application.prepareApplicationLabelCandidate
                  (firstCandidate session binding (requestId 19) labelOperation withCandidate20)
              )
          )
      closed =
        Application.setApplicationMembershipGate True withCandidates
      gatedOperation = NewIdApplication BareNewId
      final =
        Application.commitApplicationDetachedRequest
          ( checked
              "retain live gated ingress"
              ( Application.prepareApplicationGatedIngress
                  (firstCandidate session binding (requestId 21) gatedOperation closed)
              )
          )
      ordinary = ordinaryKeys final
      candidates =
        Set.fromList
          (Application.applicationLabelCandidateKey <$> Application.applicationLabelCandidateEntries final)
      gated =
        Set.fromList
          (Application.applicationGatedIngressKey <$> Application.applicationGatedIngressEntries final)
      heldEntries =
        [ (key, Application.applicationFenceHeldOperation view)
        | view <- Application.applicationFenceHeldEntries final,
          Just key <- [Application.applicationFenceHeldReplyKey view]
        ]
  heldEntry <- case heldEntries of
    first : _ -> pure first
    [] -> assertFailure "live held fixture exposed no reply-owned work"
  pure
    FourClassFixture
      { fixtureState = final,
        fixtureSession = session,
        fixtureBinding = binding,
        fixtureLabelOperation = labelOperation,
        fixtureGatedOperation = gatedOperation,
        fixtureCandidateKey = candidateKey,
        fixtureGatedKey = gatedKey,
        fixtureHeldEntry = heldEntry,
        fixtureOrdinary = ordinary,
        fixtureCandidates = candidates,
        fixtureGated = gated,
        fixtureHeld = Set.fromList (fst <$> heldEntries)
      }

firstCandidate ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  ApplicationOperation ->
  Application.State ->
  Application.ApplicationRequestCandidate
firstCandidate session binding request operation state =
  case Application.classifyApplicationRequest session binding request operation state of
    Right (Application.FirstApplicationRequest candidate) -> candidate
    other -> error ("expected first request, got " <> classificationName other)

ordinaryKeys :: Application.State -> Set Application.ApplicationRequestKey
ordinaryKeys state =
  Set.fromList
    [ Application.applicationRequestKey session request
    | (session, _) <- Application.applicationRequestOwnerEntries state,
      (request, _) <- Application.applicationRequestEntries session state
    ]

assertPartition :: FourClassFixture -> Assertion
assertPartition fixture =
  assertBool "all four request-key classes are pairwise disjoint" (partitionValid fixture)

partitionValid :: FourClassFixture -> Bool
partitionValid fixture =
  all
    (\(left, right) -> Set.null (Set.intersection left right))
    (unorderedPairs classes)
  where
    classes =
      [ fixtureOrdinary fixture,
        fixtureCandidates fixture,
        fixtureGated fixture,
        fixtureHeld fixture
      ]

unorderedPairs :: [value] -> [(value, value)]
unorderedPairs values =
  [ (first, later)
  | first : rest <- tails values,
    later <- rest
  ]

assertResultDisposition ::
  String ->
  Application.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Maybe ApplicationRequestReply ->
  Application.ApplicationRequestKey ->
  Assertion
assertResultDisposition context state session binding expected key =
  assertEqual
    (context <> " result disposition")
    expected
    ( checked
        (context <> " result lookup")
        ( Application.applicationRequestResultDisposition
            session
            binding
            (Application.applicationRequestKeyRequest key)
            state
        )
    )

assertOrdinaryDisposition ::
  Application.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Application.ApplicationRequestKey ->
  Assertion
assertOrdinaryDisposition state session binding key =
  assertBool
    "ordinary result is exposed"
    ( case checked
        "ordinary result lookup"
        ( Application.applicationRequestResultDisposition
            session
            binding
            (Application.applicationRequestKeyRequest key)
            state
        ) of
        Just _ -> True
        Nothing -> False
    )

assertClassifiesAs ::
  Application.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Application.ApplicationRequestKey ->
  ApplicationOperation ->
  Application.ApplicationRequestClass ->
  Assertion
assertClassifiesAs state session binding key operation expected =
  case checked
    "exact detached retry"
    ( Application.classifyApplicationRequest
        session
        binding
        (Application.applicationRequestKeyRequest key)
        operation
        state
    ) of
    Application.AttachedApplicationRequest observed -> assertEqual "detached request class" expected observed
    other -> assertFailure ("expected attached detached request, got " <> classificationName (Right other))

assertConflict ::
  Application.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Application.ApplicationRequestKey ->
  ApplicationOperation ->
  Assertion
assertConflict state session binding key operation =
  case checked
    "conflicting retry"
    ( Application.classifyApplicationRequest
        session
        binding
        (Application.applicationRequestKeyRequest key)
        operation
        state
    ) of
    Application.ConflictingApplicationRequest {} -> pure ()
    other -> assertFailure ("expected conflict, got " <> classificationName (Right other))

observeDuplicate :: FourClassFixture -> (Word16, Word8) -> Bool
observeDuplicate fixture (selector, variant) =
  case selected of
    (key, operation, expectedClass, ordinary) ->
      case Application.classifyApplicationRequest
        (fixtureSession fixture)
        (fixtureBinding fixture)
        (Application.applicationRequestKeyRequest key)
        supplied
        (fixtureState fixture) of
        Right (Application.RetainedApplicationRequest {}) -> ordinary && exact
        Right (Application.AttachedApplicationRequest actual) ->
          not ordinary && exact && actual == expectedClass
        Right (Application.ConflictingApplicationRequest {}) -> not exact
        _ -> False
      where
        exact = even variant
        supplied
          | exact = operation
          | operation == fixtureGatedOperation fixture = fixtureLabelOperation fixture
          | otherwise = fixtureGatedOperation fixture
  where
    selected = duplicateCases fixture !! (fromIntegral selector `mod` length (duplicateCases fixture))

duplicateCases ::
  FourClassFixture ->
  [ ( Application.ApplicationRequestKey,
      ApplicationOperation,
      Application.ApplicationRequestClass,
      Bool
    )
  ]
duplicateCases fixture =
  [ ( Application.activeLabelApplicationKey active,
      fixtureLabelOperation fixture,
      Application.OrdinaryApplicationRequestClass,
      True
    ),
    ( fixtureCandidateKey fixture,
      fixtureLabelOperation fixture,
      Application.LabelCandidateApplicationRequestClass,
      False
    ),
    ( fixtureGatedKey fixture,
      fixtureGatedOperation fixture,
      Application.GatedIngressApplicationRequestClass,
      False
    ),
    ( heldKey,
      heldOperation,
      Application.FenceHeldApplicationRequestClass,
      False
    )
  ]
  where
    active = maybe (error "active label missing") id (Application.applicationActiveLabel (fixtureState fixture))
    (heldKey, heldOperation) = fixtureHeldEntry fixture

requiredActive :: Application.State -> IO Application.ActiveLabelApplicationView
requiredActive state =
  case Application.applicationActiveLabel state of
    Just active -> pure active
    Nothing -> assertFailure "live held fixture has no active label owner"

lookupRequestCall ::
  ApplicationSessionId ->
  RequestId ->
  Application.State ->
  Maybe ApplicationOperation
lookupRequestCall session request state =
  Application.applicationRequestWitnessCall
    <$> lookup request (Application.applicationRequestEntries session state)

lookupRequestPosition ::
  ApplicationSessionId ->
  RequestId ->
  Application.State ->
  Maybe Request.ProcessAcceptancePosition
lookupRequestPosition session request state =
  Application.applicationRequestWitnessPosition
    =<< lookup request (Application.applicationRequestEntries session state)

classificationName ::
  Either problem Application.ApplicationRequestClassification -> String
classificationName classification = case classification of
  Left _ -> "classification failure"
  Right Application.FirstApplicationRequest {} -> "first request"
  Right Application.RetainedApplicationRequest {} -> "retained request"
  Right Application.AttachedApplicationRequest {} -> "attached request"
  Right Application.ConflictingApplicationRequest {} -> "conflicting request"

globalObject :: Word8 -> GlobalObjectId
globalObject = globalObjectIdFromNablaId . nabla

nabla :: Word8 -> NablaId
nabla byte = checked "Nabla" (mkNablaId (ByteString.replicate 32 byte))

process :: Word8 -> ProcessEpochId
process byte = checked "process" (mkProcessEpochId (ByteString.replicate 32 byte))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

isLeft :: Either problem value -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
