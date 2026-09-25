{-# LANGUAGE LambdaCase #-}

-- | Real four-Herald application lifetime, loss, and local-read evidence.
module Step15ApplicationLossProperties (tests) where

import Control.Concurrent
  ( MVar,
    forkFinally,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    fromException,
    throwIO,
  )
import Control.Monad (forM_, void, when)
import Data.List (nub, partition, (\\))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (NeutralVertexRole),
    predefinedReader,
  )
import Eclips.Application.Types.Operation
  ( ApplicationOperation (ReadApplication),
  )
import Eclips.Application.Types.Query (ApplicationQuery (..), ApplicationQueryPredicate (QueryAlways))
import Eclips.Domain.Identity (ControlIndex)
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ApplicationPermanentlyLost),
  )
import Eclips.Herald.Application.Request (ApplicationRequestReply (ApplicationReceiptsRetired))
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    sessionAcceptanceBinding,
    sessionBindingSessionId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( ApplicationReceiptRetirementIngress (RetireApplicationReceipts),
    ApplicationRequestIngress (CallApplicationRequest),
    ApplicationRetirementWork (RetiringApplicationRequest),
    ApplicationSessionIngress (ResumeApplicationSession),
    HeraldInputBody (ApplicationReceiptRetirementInput, ApplicationRequestInput, ApplicationSessionInput, OracleInput, PeerInput, RuntimeObserved),
    RuntimeObservation (ApplicationBindingLost, OracleHealthRoundObserved),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ReleaseOracleProgress, ScheduleOracleProgress, SubmitOracleProgress),
    OracleClientIngress (OracleProgressConfirmed, OracleProgressFlush),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (ApplicationConnectionPromoted),
    KernelTraceStep (KernelTraceStepped),
    PhysicalConnectionRef (PhysicalApplicationRef),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent
      ( ShellConnectionClosed,
        ShellConnectionPromoted,
        ShellConnectionWriterFailed,
        ShellTimerArmed,
        ShellTimerCancelled
      ),
  )
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
  )
import Eclips.Oracle.Runtime
  ( oracleRuntimeAppliedControlIndex,
    oracleRuntimeMembershipGeneration,
  )
import Eclips.Oracle.Runtime.TCP (oracleTcpNodeStatuses)
import PeerEvidence (data SemanticPeerAlignmentControl, data SemanticPeerControlReceived, data SemanticSendPeerControl)
import Step15Applications
  ( awaitStep15ApplicationCallWithin,
    readStep15PredefinedStore,
    step15PredefinedAccess,
    withStep15Application,
  )
import Step15Configuration
  ( Step15HeraldName (Step15H1),
  )
import Step15Fixtures
  ( Step15Deployment,
    Step15Herald,
    awaitStep15PeerMesh,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldWorkLedger,
    withStep15ApplicationLossDeployment,
    withStep15Deployment,
    withStep15DeploymentWithHooks,
  )
import Step15Genesis (step15H1ProcessEpochId)
import Step15WorkCounts
  ( Step15AttributableWork (..),
    Step15WorkCursor,
    Step15WorkEvidence,
    Step15WorkLedger,
    snapshotStep15WorkBaselines,
    snapshotStep15WorkCursors,
    snapshotStep15WorkEvidenceSince,
    step15ConfiguredSeedEvidence,
    step15HeartbeatEvidence,
    step15HeartbeatLifecycleEvents,
    step15KernelFaultEvidence,
    step15OracleActionEvidence,
    step15OracleLifecycleEvents,
    step15OracleManagerEvidence,
    step15OracleProjectionViews,
    step15PeerDialEvidence,
    step15PeerDialLifecycleEvents,
    step15RuntimeEvidence,
    step15TerminalControlEvents,
    step15TimerLifecycleEvents,
    summarizeStep15WorkEvidence,
  )
import System.Timeout (timeout)
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
    "Step-15 application loss and local reads"
    [ testCase
        "an ordinary real application read creates no distributed work"
        caseReadCreatesNoDistributedWork,
      testCase
        "a lost application session ends while its live sibling preserves the process"
        caseApplicationLossPreservesSibling,
      testCase
        "permanent final-session loss ends its exact process and disposes its lane"
        casePermanentApplicationLossEndsProcess
    ]

caseReadCreatesNoDistributedWork :: Assertion
caseReadCreatesNoDistributedWork = do
  observed <- timeout scenarioTimeoutMicroseconds $ withStep15Deployment $ \deployment -> do
    awaitStep15PeerMesh deployment
    withStep15Application Step15H1 (step15H1 deployment) 15_511
      $ \application startup -> do
        access <- step15PredefinedAccess "H1 local-read" NeutralVertexRole startup
        baseline <- readStep15PredefinedStore "H1 warm local read" application access
        let ledgers = deploymentWorkLedgers deployment
        (baselines, cursors) <- snapshotStep15WorkBaselines ledgers
        oracleBefore <- oracleAppliedIndices deployment

        result <- readStep15PredefinedStore "H1 measured local read" application access
        evidence <- evidenceSince cursors ledgers
        oracleAfter <- oracleAppliedIndices deployment

        assertEqual "the measured local read observes the same stable Store cut" baseline result
        assertEqual
          "the local read advances no Oracle/Raft applied index"
          oracleBefore
          oracleAfter
        assertLocalReadEvidence baselines evidence

  maybe
    (assertFailure "Step-15 local-read schedule exceeded its failure bound")
    pure
    observed

caseApplicationLossPreservesSibling :: Assertion
caseApplicationLossPreservesSibling = do
  armed <- newEmptyMVar
  failed <- newEmptyMVar
  let hooksFor = \case
        Step15H1 -> failOneApplicationWrite armed failed
        _ -> defaultRuntimeHooks
  observed <- timeout scenarioTimeoutMicroseconds
    $ withStep15DeploymentWithHooks hooksFor
    $ \deployment -> do
      awaitStep15PeerMesh deployment
      predecessor <- currentMembership deployment
      let ledgers = deploymentWorkLedgers deployment
      withStep15Application Step15H1 (step15H1 deployment) 15_514
        $ \sibling siblingStartup -> do
          siblingAccess <- step15PredefinedAccess "H1 surviving sibling" NeutralVertexRole siblingStartup
          before <- readStep15PredefinedStore "H1 sibling before loss" sibling siblingAccess
          cursors <- snapshotStep15WorkCursors ledgers
          withStep15Application Step15H1 (step15H1 deployment) 15_512
            $ \application startup -> do
              access <- step15PredefinedAccess "H1 lost session" NeutralVertexRole startup
              openingEvidence <- evidenceSince cursors ledgers
              physical <- case applicationPromotions (concatMap step15RuntimeEvidence openingEvidence) of
                [(openedPhysical, _)] -> pure openedPhysical
                other -> assertFailure ("expected the new victim application lane: " <> show other)
              putMVar armed physical
              let query = ApplicationQuery (Set.singleton (predefinedReader access)) QueryAlways
              call <- EAPP.read application query
              first <- awaitStep15ApplicationCallWithin "H1 lost read result" call
              failedPhysical <- awaitMVarWithin "application writer did not fail" failed
              assertEqual "lost read result is unknown to its caller" (EAPP.ApplicationCallUnknown EAPP.SessionNoLongerLive) first
              assertEqual "the failed session is terminal" EAPP.SessionNoLongerLive =<< EAPP.awaitApplicationUnavailable application
              later <- EAPP.read application query >>= awaitStep15ApplicationCallWithin "H1 closed application read"
              assertEqual "the lost handle cannot submit another call" EAPP.ApplicationCallClosed later
              after <- readStep15PredefinedStore "H1 live sibling after loss" sibling siblingAccess
              assertEqual "the live sibling keeps its unchanged local Store" before after
              readyEvidence <- awaitApplicationDisposal cursors ledgers
              assertSiblingLossEvidence failedPhysical readyEvidence
              assertEqual
                "application loss cannot change Herald membership"
                predecessor
                =<< currentMembership deployment
              evidence <- evidenceSince cursors ledgers
              assertApplicationWorkBounds "application loss with live sibling" evidence

  maybe
    (assertFailure "Step-15 sibling application-loss schedule exceeded its failure bound")
    pure
    observed

casePermanentApplicationLossEndsProcess :: Assertion
casePermanentApplicationLossEndsProcess = do
  observed <- timeout scenarioTimeoutMicroseconds
    $ withStep15ApplicationLossDeployment
    $ \deployment -> do
      awaitStep15PeerMesh deployment
      let ledgers = deploymentWorkLedgers deployment
      cursors <- snapshotStep15WorkCursors ledgers
      ready <- newEmptyMVar
      hold <- newEmptyMVar
      finished <- newEmptyMVar
      applicationThread <-
        forkFinally
          ( withStep15Application Step15H1 (step15H1 deployment) 15_513
              $ \_ startup -> do
                putMVar ready startup
                readMVar hold
          )
          (putMVar finished)
      void (awaitMVarWithin "H1 application did not attach" ready)

      killThread applicationThread
      outcome <- awaitMVarWithin "abrupt H1 application did not join" finished
      case outcome of
        Left exception
          | fromException exception == Just ThreadKilled -> pure ()
          | otherwise -> throwIO exception
        Right () -> assertFailure "abrupt application loss returned normally"

      readyEvidence <- awaitPermanentEnd cursors ledgers
      assertPermanentLossEvidence readyEvidence
      evidence <- evidenceSince cursors ledgers
      assertApplicationWorkBounds "permanent application loss" evidence

  maybe
    (assertFailure "Step-15 permanent application-loss schedule exceeded its failure bound")
    pure
    observed

failOneApplicationWrite :: MVar PhysicalConnectionRef -> MVar PhysicalConnectionRef -> RuntimeHooks
failOneApplicationWrite armed failed =
  defaultRuntimeHooks
    { hookAfterWriterCommandDequeued = \case
        physical@PhysicalApplicationRef {} -> do
          victim <- tryReadMVar armed
          when (victim == Just physical) $ do
            first <- tryPutMVar failed physical
            when first (throwIO (userError "intentional application writer loss"))
        _ -> pure ()
    }

assertLocalReadEvidence :: [Step15WorkEvidence] -> [Step15WorkEvidence] -> Assertion
assertLocalReadEvidence baselines evidence = do
  let steps = concatMap (successfulSteps . step15RuntimeEvidence) evidence
      (healthSteps, otherSteps) = partition isNativeHealthStep steps
      (retirementSteps, readSteps) = partition isReceiptMaintenanceStep otherSteps
  assertEqual "the measured read window contains no kernel fault" [] (concatMap step15KernelFaultEvidence evidence)
  case readSteps of
    [(body, effects)]
      | isReadInput body ->
          assertBool
            ("the local read emitted non-application effects: " <> show effects)
            (all isApplicationReply effects)
    other ->
      assertFailure
        ("the measured read delta was not one local read transition: " <> show other)
  forM_ healthSteps assertNativeHealthStep
  forM_ retirementSteps assertReceiptMaintenanceStep
  -- Native health runs independently of application reads. Retain the exact
  -- prefix as well: a health transition before the cursor can have its timer
  -- effect routed during the measured read. Consume prefix timer events so an
  -- already-routed effect cannot excuse a second timer event in this window.
  assertEqual "the read evidence retains one baseline per Herald" (length baselines) (length evidence)
  forM_ (zip baselines evidence) $ \(baseline, delta) -> do
    let prefixEvents = step15RuntimeEvidence baseline
        deltaEvents = step15RuntimeEvidence delta
        allHealthSteps = filter isNativeHealthStep (successfulSteps (prefixEvents <> deltaEvents))
        healthTimers = [effect | (_, effects) <- allHealthSteps, effect <- effects, isTimerEffect effect]
        outstandingHealthTimers = healthTimers \\ shellTimerEffects prefixEvents
        unattributedTimers = shellTimerEffects deltaEvents \\ outstandingHealthTimers
        receiptActions = [action | RunOracleClientAction action <- kernelEffects (prefixEvents <> deltaEvents), isReceiptMaintenanceAction action]
        outstandingReceiptActions = receiptActions \\ step15OracleActionEvidence baseline
        unattributedOracleActions = step15OracleActionEvidence delta \\ outstandingReceiptActions
    forM_ allHealthSteps assertNativeHealthStep
    assertEqual
      "every timer effect in the read window belongs to an exact independent native-health transition"
      []
      unattributedTimers
    assertEqual
      "every Oracle action in the read window belongs to exact independent receipt maintenance"
      []
      unattributedOracleActions
  assertEqual "the local read starts no configured-seed work" [] (concatMap step15ConfiguredSeedEvidence evidence)
  assertEqual "the local read starts no learned-peer dial work" [] (concatMap step15PeerDialEvidence evidence)
  assertEqual "the local read starts no heartbeat/probe work" [] (concatMap step15HeartbeatEvidence evidence)
  assertEqual "the local read starts no semantic Oracle action" [] (filter (not . isReceiptMaintenanceAction) (concatMap step15OracleActionEvidence evidence))
  assertEqual "the local read starts no Oracle-manager work" [] (concatMap step15OracleManagerEvidence evidence)
  let work = fmap summarizeStep15WorkEvidence evidence
  assertEqual "the local read queues no timer outcome" 0 (sumField step15TimerOutcomesQueued work)
  assertEqual "the local read suppresses no timer outcome" 0 (sumField step15TimerOutcomesSuppressed work)
  assertEqual "the local read emits no distributed peer control" 0 (sumField terminalAndProbeControls work)
  where
    isApplicationReply SendApplicationReply {} = True
    isApplicationReply _ = False
    isReadInput (ApplicationRequestInput (CallApplicationRequest _ _ _ ReadApplication {})) = True
    isReadInput (ApplicationReceiptRetirementInput (RetireApplicationReceipts _ _ _ (Just (RetiringApplicationRequest _ ReadApplication {})))) = True
    isReadInput _ = False
    isReceiptMaintenanceStep (ApplicationReceiptRetirementInput (RetireApplicationReceipts _ _ _ Nothing), _) = True
    isReceiptMaintenanceStep (OracleInput OracleProgressConfirmed {}, _) = True
    isReceiptMaintenanceStep (OracleInput OracleProgressFlush {}, _) = True
    isReceiptMaintenanceStep _ = False
    assertReceiptMaintenanceStep (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding _ _ Nothing), effects) =
      case effects of
        [SendApplicationReply replyBinding ApplicationReceiptsRetired {}] -> assertEqual "standalone receipt confirmation stays on its application binding" binding replyBinding
        other -> assertFailure ("application receipt maintenance emitted non-receipt work: " <> show other)
    assertReceiptMaintenanceStep (_, effects) =
      assertBool
        ("Oracle receipt maintenance emitted semantic work: " <> show effects)
        (all (\case RunOracleClientAction action -> isReceiptMaintenanceAction action; _ -> False) effects)
    isReceiptMaintenanceAction ReleaseOracleProgress {} = True
    isReceiptMaintenanceAction ScheduleOracleProgress {} = True
    isReceiptMaintenanceAction SubmitOracleProgress {} = True
    isReceiptMaintenanceAction _ = False
    successfulSteps events =
      [ (inputBody input, effectBatchMembers effects)
      | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events
      ]
    isNativeHealthStep (RuntimeObserved OracleHealthRoundObserved {}, _) = True
    isNativeHealthStep _ = False
    assertNativeHealthStep (RuntimeObserved (OracleHealthRoundObserved roundNumber _), effects) =
      assertBool
        ("native-health input emitted work beyond its next round and optional grace timer: " <> show effects)
        ( case effects of
            [] -> True
            [RunOracleHealthRound next _ _ _] -> next == roundNumber + 1
            [ArmTimer {}, RunOracleHealthRound next _ _ _] -> next == roundNumber + 1
            [CancelTimer {}, RunOracleHealthRound next _ _ _] -> next == roundNumber + 1
            _ -> False
        )
    assertNativeHealthStep other = assertFailure ("unexpected native-health attribution: " <> show other)
    isTimerEffect ArmTimer {} = True
    isTimerEffect CancelTimer {} = True
    isTimerEffect _ = False
    shellTimerEffects events =
      [ effect
      | ShellEvent _ event <- events,
        effect <- case event of
          ShellTimerArmed attempt specification -> [ArmTimer attempt specification]
          ShellTimerCancelled attempt -> [CancelTimer attempt]
          _ -> []
      ]
    terminalAndProbeControls item =
      step15FailureProbeRequests item
        + step15FailureProbeResponses item
        + step15TerminalSourceInventories item
        + step15TerminalPayloadRequests item
        + step15TerminalOccurrenceReplays item
        + step15TerminalUnionMessages item

applicationPromotions :: [RuntimeTraceEvent] -> [(PhysicalConnectionRef, ApplicationSessionBinding)]
applicationPromotions events =
  [ (physical, binding)
  | ShellEvent _ (ShellConnectionPromoted physical@PhysicalApplicationRef {} _ (ApplicationConnectionPromoted binding)) <- events
  ]

awaitApplicationDisposal :: [Step15WorkCursor] -> [Step15WorkLedger] -> IO [Step15WorkEvidence]
awaitApplicationDisposal cursors ledgers = do
  observed <- timeout progressTimeoutMicroseconds loop
  maybe (assertFailure "the application binding loss did not immediately dispose its session") pure observed
  where
    loop = do
      evidence <- evidenceSince cursors ledgers
      if null [() | DisposeApplicationSession {} <- kernelEffects (concatMap step15RuntimeEvidence evidence)]
        then threadDelay 10_000 >> loop
        else pure evidence

assertSiblingLossEvidence ::
  PhysicalConnectionRef ->
  [Step15WorkEvidence] ->
  Assertion
assertSiblingLossEvidence failedPhysical evidence = do
  let events = concatMap step15RuntimeEvidence evidence
      failures =
        [ physical
        | ShellEvent _ (ShellConnectionWriterFailed physical@PhysicalApplicationRef {}) <- events
        ]
      closes =
        [ physical
        | ShellEvent _ (ShellConnectionClosed physical@PhysicalApplicationRef {} _) <- events
        ]
      effects = kernelEffects events
      views = nub (concatMap step15OracleProjectionViews evidence)
  binding <- case applicationPromotions events of
    [(physical, binding)] -> assertEqual "the failed client used exactly its original lane" failedPhysical physical >> pure binding
    other -> assertFailure ("the failed client opened unexpected replacement lanes: " <> show other)
  assertEqual "the injected physical lane records one writer failure" [failedPhysical] failures
  assertEqual "the injected physical lane closes once" [failedPhysical] closes
  assertEqual
    "an established application loss never requests Resume"
    []
    [() | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events, ApplicationSessionInput ResumeApplicationSession {} <- [inputBody input]]
  assertEqual
    "application loss disposes only the failed session"
    [sessionBindingSessionId binding]
    [session | DisposeApplicationSession session _ <- effects]
  assertImmediateDisposal events
  assertEqual
    "a live sibling session prevents automatic process End"
    []
    [ process
    | ProcessEpochEndedEventView process _ ApplicationPermanentlyLost <- views
    ]

awaitPermanentEnd ::
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  IO [Step15WorkEvidence]
awaitPermanentEnd cursors ledgers = do
  observed <- timeout progressTimeoutMicroseconds loop
  case observed of
    Just evidence -> pure evidence
    Nothing -> do
      evidence <- evidenceSince cursors ledgers
      assertFailure
        ( "the final application loss did not project its immediate End; views="
            <> show (fmap step15OracleProjectionViews evidence)
            <> "; work="
            <> show (fmap summarizeStep15WorkEvidence evidence)
        )
  where
    loop = do
      evidence <- evidenceSince cursors ledgers
      let projectedEverywhere =
            all
              (any isExpectedEnd . step15OracleProjectionViews)
              evidence
          disposed =
            [ session
            | event <- concatMap step15RuntimeEvidence evidence,
              DisposeApplicationSession session _ <- kernelEffects [event]
            ]
      if projectedEverywhere && not (null disposed)
        then pure evidence
        else threadDelay 10_000 >> loop
    isExpectedEnd = \case
      ProcessEpochEndedEventView process _ ApplicationPermanentlyLost ->
        process == step15H1ProcessEpochId
      _ -> False

assertPermanentLossEvidence :: [Step15WorkEvidence] -> Assertion
assertPermanentLossEvidence evidence = do
  let effects = kernelEffects (concatMap step15RuntimeEvidence evidence)
      opened =
        [ sessionBindingSessionId (sessionAcceptanceBinding acceptance)
        | SetApplicationConnectionDisposition _ acceptance <- effects
        ]
      disposed =
        [ session
        | DisposeApplicationSession session _ <- effects
        ]
      views = nub (concatMap step15OracleProjectionViews evidence)
      ended =
        [ process
        | ProcessEpochEndedEventView process _ ApplicationPermanentlyLost <- views
        ]
  case nub opened of
    [session] ->
      assertEqual
        "the permanent-loss transition disposes the exact opened application session"
        [session]
        disposed
    sessions -> assertFailure ("expected one opened application session, got " <> show sessions)
  assertEqual
    "the automatic End names only H1's exact resident process"
    [step15H1ProcessEpochId]
    ended
  assertImmediateDisposal (concatMap step15RuntimeEvidence evidence)

assertImmediateDisposal :: [RuntimeTraceEvent] -> Assertion
assertImmediateDisposal events = do
  let lossEffects =
        [ (sessionBindingSessionId binding, effectBatchMembers effects)
        | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events,
          RuntimeObserved (ApplicationBindingLost binding) <- [inputBody input]
        ]
      immediate = [session | (lostSession, effects) <- lossEffects, DisposeApplicationSession session _ <- effects, session == lostSession]
      allDisposals = [session | DisposeApplicationSession session _ <- kernelEffects events]
  assertEqual "the physical loss transition itself disposes the session" allDisposals immediate
  assertEqual "application loss arms no recovery grace" [] [effect | (_, effects) <- lossEffects, effect@ArmTimer {} <- effects]

kernelEffects :: [RuntimeTraceEvent] -> [HeraldEffect]
kernelEffects events =
  concat
    [ effectBatchMembers effects
    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events
    ]

assertApplicationWorkBounds :: String -> [Step15WorkEvidence] -> Assertion
assertApplicationWorkBounds scenario evidence = do
  let work = fmap summarizeStep15WorkEvidence evidence
      physicalDials =
        sumField
          (\item -> step15ConfiguredSeedDialAttempts item + step15PeerDialAttempts item)
          work
  bound "kernel inputs" 64 (sumField step15KernelInputs work) work
  bound "physical peer dials" 16 physicalDials work
  bound "complete timer lifecycle" 128 (sumField step15TimerLifecycleEvents work) work
  bound "complete peer-dial lifecycle" 64 (sumField step15PeerDialLifecycleEvents work) work
  bound "complete heartbeat lifecycle" 64 (sumField step15HeartbeatLifecycleEvents work) work
  bound "complete Oracle lifecycle" 32 (sumField step15OracleLifecycleEvents work) work
  bound "Oracle submissions" 4 (sumField step15OracleSubmissions work) work
  bound "heartbeat/probe messages" 8 (sumField heartbeatAndProbe work) work
  bound "terminal-source controls" 8 (sumField step15TerminalControlEvents work) work
  bound "structural-applied reports" 64 (sumField step15StructuralAppliedReports work) work
  where
    heartbeatAndProbe item =
      step15HeartbeatWrites item
        + step15HeartbeatTimeouts item
        + step15FailureProbeRequests item
        + step15FailureProbeResponses item
    bound description upper actual work =
      assertBool
        ( "Step-15 "
            <> scenario
            <> " exceeded the attributable "
            <> description
            <> " bound "
            <> show upper
            <> "; observed="
            <> show actual
            <> "; per-Herald="
            <> show work
            <> "; input families="
            <> show inputFamilies
            <> "; emitted peer controls (total, distinct payloads)="
            <> show peerControlFamilies
        )
        (actual <= upper)
    inputFamilies = Map.toAscList (Map.fromListWith (+) [(inputFamily (inputBody input), 1 :: Int) | KernelEvent _ (KernelTraceStepped _ _ input _) <- events])
    peerControlFamilies =
      Map.toAscList
        ( fmap
            (\controls -> (length controls, length (nub controls)))
            (Map.fromListWith (<>) [(peerFamily control, [show control]) | SemanticSendPeerControl _ control <- kernelEffects events])
        )
    events = concatMap step15RuntimeEvidence evidence
    inputFamily = \case
      PeerInput (SemanticPeerControlReceived _ control) -> "Peer:" <> peerFamily control
      OracleInput ingress -> "Oracle:" <> constructorName ingress
      RuntimeObserved observation -> "Runtime:" <> constructorName observation
      other -> constructorName other
    peerFamily = \case
      SemanticPeerAlignmentControl control -> "Alignment:" <> constructorName control
      other -> constructorName other
    constructorName :: (Show value) => value -> String
    constructorName = takeWhile (\character -> character /= ' ' && character /= '{') . show

deploymentHeralds :: Step15Deployment -> [Step15Herald]
deploymentHeralds deployment =
  [step15H1 deployment, step15H2 deployment, step15H3 deployment, step15H4 deployment]

deploymentWorkLedgers :: Step15Deployment -> [Step15WorkLedger]
deploymentWorkLedgers = fmap step15ManagedHeraldWorkLedger . deploymentHeralds

evidenceSince ::
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  IO [Step15WorkEvidence]
evidenceSince cursors ledgers =
  fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)

currentMembership :: Step15Deployment -> IO HeraldMembershipGeneration
currentMembership deployment = do
  statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
  case nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses) of
    [generation] -> pure generation
    generations ->
      assertFailure ("Oracle replicas disagree on Step-15 membership: " <> show generations)

oracleAppliedIndices :: Step15Deployment -> IO [ControlIndex]
oracleAppliedIndices deployment =
  fmap (oracleRuntimeAppliedControlIndex . snd)
    <$> oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)

awaitMVarWithin :: String -> MVar value -> IO value
awaitMVarWithin context variable = do
  observed <- timeout progressTimeoutMicroseconds (readMVar variable)
  maybe (assertFailure context) pure observed

sumField :: (value -> Int) -> [value] -> Int
sumField project = sum . fmap project

scenarioTimeoutMicroseconds, progressTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 15_000_000
progressTimeoutMicroseconds = 5_000_000
