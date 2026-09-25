{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Step15FoundationProperties (tests) where

import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (NeutralVertexRole),
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity (PrivateProcessId)
import Eclips.Application.Types.Rejection
  ( ApplicationRejection (ApplicationOperateNotPermitted),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (WriteCompleted),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel, ZombieLabel),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( WriteResult (WriteAccepted),
  )
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Herald.Application.Session
  ( ApplicationSessionUnavailableReason (ApplicationHeraldIsolated),
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (DisposeApplicationSession),
    effectBatchMembers,
  )
import Eclips.Herald.Input (HeraldInputBody (RuntimeObserved), RuntimeObservation (OracleHealthRoundObserved), inputBody)
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeScopeClosed),
  )
import Eclips.Oracle.Runtime
  ( oracleRuntimeMembershipGeneration,
    oracleRuntimeNode,
  )
import Eclips.Oracle.Runtime.TCP (oracleTcpNodeStatuses)
import Eclips.Raft.Identity (RaftNodeId)
import PeerEvidence (data SemanticSendPeerControl, data SemanticSendPeerItem)
import Step15Applications
  ( awaitStep15ApplicationCallWithin,
    publishStep15NeutralCarrier,
    readStep15PredefinedStore,
    step15PredefinedAccess,
    withStep15Application,
  )
import Step15Configuration
  ( Step15HeraldName (..),
    Step15HeraldRole (..),
    step15ActiveHeraldEpochs,
    step15HeraldEpoch,
    step15HeraldNames,
    step15HeraldRole,
    step15OracleVoterBindings,
    step15OracleVoterHostEpochs,
    step15OracleVoterNodes,
  )
import Step15Fixtures
  ( Step15Deployment,
    Step15Herald,
    awaitStep15PeerMesh,
    awaitStep15SurvivorMesh,
    closeStep15H4PeerCut,
    closeStep15H4Scope,
    openStep15H4PeerTransport,
    reopenStep15H4PeerCut,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldIsRunning,
    step15ManagedHeraldWorkLedger,
    withStep15Deployment,
    withStep15H4OracleHealthCutDeployment,
  )
import Step15WorkCounts
  ( Step15AttributableWork (..),
    Step15WorkEvidence,
    awaitStep15WorkEvidenceSince,
    snapshotStep15WorkCursors,
    snapshotStep15WorkEvidenceSince,
    step15ConfiguredSeedEvidence,
    step15HeartbeatEvidence,
    step15HeartbeatLifecycleEvents,
    step15OracleActionEvidence,
    step15OracleLifecycleEvents,
    step15OracleManagerEvidence,
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
    "Step-15 acceptance foundation"
    [ testCase
        "H4 is a non-voter and its scope closes independently"
        caseIndependentH4ScopeClose,
      testCase
        "H4's real EPRP cut rejoins without stopping any Herald"
        caseReversibleH4PeerCut,
      testCase
        "voter-quorum-isolated H4 self-fences and cannot revive"
        caseIsolatedH4SelfFence
    ]

caseIndependentH4ScopeClose :: Assertion
caseIndependentH4ScopeClose = do
  observed <- timeout 30_000_000 $ withStep15Deployment $ \deployment -> do
    awaitStep15PeerMesh deployment

    assertEqual
      "the checked Oracle genesis has the four configured active Heralds"
      (fmap step15HeraldEpoch step15HeraldNames)
      step15ActiveHeraldEpochs
    assertEqual
      "only H1-H3 have checked Oracle/Raft voter bindings"
      (fmap step15HeraldEpoch [Step15H1, Step15H2, Step15H3])
      step15OracleVoterHostEpochs
    assertEqual
      "the checked voter bindings cohost the running nodes at H1-H3"
      [Step15H1, Step15H2, Step15H3]
      (fmap fst step15OracleVoterBindings)
    assertEqual
      "the named acceptance roles retain H4 as the sole non-voter"
      [ Step15OracleVoterHost,
        Step15OracleVoterHost,
        Step15OracleVoterHost,
        Step15NonVoter
      ]
      (fmap step15HeraldRole step15HeraldNames)
    assertBool
      "H4 is active but absent from the voter-host binding set"
      ( step15HeraldEpoch Step15H4 `elem` step15ActiveHeraldEpochs
          && step15HeraldEpoch Step15H4 `notElem` step15OracleVoterHostEpochs
      )

    oracleNodesBefore <- currentOracleNodes deployment
    assertEqual
      "the running Oracle cluster contains exactly the configured three voters"
      step15OracleVoterNodes
      oracleNodesBefore

    let heralds =
          [ step15H1 deployment,
            step15H2 deployment,
            step15H3 deployment,
            step15H4 deployment
          ]
        workLedgers = fmap step15ManagedHeraldWorkLedger heralds
    workCursors <- snapshotStep15WorkCursors workLedgers

    h4Exit <- closeStep15H4Scope deployment
    assertEqual
      "returning H4's managed callback closes its runtime/TCP scope normally"
      HeraldRuntimeScopeClosed
      h4Exit
    h4TerminalCursor <-
      snapshotStep15WorkCursors [step15ManagedHeraldWorkLedger (step15H4 deployment)]
    awaitStep15SurvivorMesh deployment
    survivorsRunning <-
      traverse
        step15ManagedHeraldIsRunning
        [step15H1 deployment, step15H2 deployment, step15H3 deployment]
    assertEqual
      "H1-H3 remain independently supervised after H4 stops"
      [True, True, True]
      survivorsRunning
    workEvidence <-
      fmap
        (fmap snd)
        (snapshotStep15WorkEvidenceSince workCursors workLedgers)
    assertEqual
      "the independently supervised close introduces no kernel invariant fault"
      [0, 0, 0, 0]
      (fmap (step15KernelFaults . summarizeStep15WorkEvidence) workEvidence)
    assertFoundationWorkBounds
      "independent H4 scope close"
      (fmap summarizeStep15WorkEvidence workEvidence)
    h4TerminalEvidence <-
      fmap
        (fmap snd)
        ( snapshotStep15WorkEvidenceSince
            h4TerminalCursor
            [step15ManagedHeraldWorkLedger (step15H4 deployment)]
        )
    assertBool
      "the joined H4 scope performs no runtime or physical work after its terminal cut"
      (all evidenceIsEmpty h4TerminalEvidence)
    assertEqual
      "closing non-voter H4 does not change the Oracle/Raft voter topology"
      oracleNodesBefore
      =<< currentOracleNodes deployment

  case observed of
    Nothing -> assertFailure "Step-15 foundation scenario exceeded its failure bound"
    Just () -> pure ()

caseReversibleH4PeerCut :: Assertion
caseReversibleH4PeerCut = do
  observed <- timeout 30_000_000 $ withStep15Deployment $ \deployment -> do
    awaitStep15PeerMesh deployment
    let heralds = deploymentHeralds deployment
        workLedgers = fmap step15ManagedHeraldWorkLedger heralds
    oracleNodesBefore <- currentOracleNodes deployment
    membershipBefore <- currentOracleMembership deployment
    workCursors <- snapshotStep15WorkCursors workLedgers

    closeStep15H4PeerCut deployment
    assertEqual
      "the EPRP-only cut leaves all four managed Herald scopes live"
      [True, True, True, True]
      =<< traverse step15ManagedHeraldIsRunning heralds
    assertEqual
      "the EPRP-only cut does not alter the independently supervised voter cluster"
      oracleNodesBefore
      =<< currentOracleNodes deployment
    assertEqual
      "the EPRP-only cut does not alter Herald membership"
      membershipBefore
      =<< currentOracleMembership deployment

    reopenStep15H4PeerCut deployment
    assertEqual
      "all managed Herald scopes remain live after the exact peer mesh rejoins"
      [True, True, True, True]
      =<< traverse step15ManagedHeraldIsRunning heralds
    assertEqual
      "the rejoined mesh retains the exact predecessor Herald membership"
      membershipBefore
      =<< currentOracleMembership deployment
    workEvidence <-
      fmap
        (fmap snd)
        (snapshotStep15WorkEvidenceSince workCursors workLedgers)
    let work = fmap summarizeStep15WorkEvidence workEvidence
    assertEqual
      "the physical peer cut and rejoin introduce no kernel invariant fault"
      [0, 0, 0, 0]
      (fmap step15KernelFaults work)
    assertBool
      "rejoining three H4 peer links requires at least three attributable physical dials"
      ( sum
          [ step15ConfiguredSeedDialAttempts counts
              + step15PeerDialAttempts counts
          | counts <- work
          ]
          >= 3
      )
    assertFoundationWorkBounds "reversible H4 peer cut" work

  case observed of
    Nothing -> assertFailure "Step-15 EPRP cut/rejoin exceeded its failure bound"
    Just () -> pure ()

caseIsolatedH4SelfFence :: Assertion
caseIsolatedH4SelfFence = do
  observed <- timeout 30_000_000 $ withStep15H4OracleHealthCutDeployment $ \deployment cutNativeHealth healNativeHealth -> do
    awaitStep15PeerMesh deployment
    let heralds = deploymentHeralds deployment
        workLedgers = fmap step15ManagedHeraldWorkLedger heralds
    oracleNodesBefore <- currentOracleNodes deployment
    membershipBefore <- currentOracleMembership deployment
    workCursors <- snapshotStep15WorkCursors workLedgers

    withStep15Application Step15H4 (step15H4 deployment) 15_505
      $ \application startup -> do
        access <-
          step15PredefinedAccess
            "H4 isolation drain"
            NeutralVertexRole
            startup
        (_, publication) <-
          publishStep15NeutralCarrier
            "H4 isolation drain"
            application
            startup
            access
        assertEqual
          "H4 establishes a genuine local value before its peer cut"
          (EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted))
          =<< awaitStep15ApplicationCallWithin
            "H4 pre-isolation publication"
            publication
        baseline <-
          readStep15PredefinedStore
            "H4 pre-isolation local read"
            application
            access
        assertBool
          "the isolation predicate/value check uses a non-empty H4 Store view"
          (not (null baseline))

        _ <-
          awaitWithin "H4 did not receive its initial native-voter health replies"
            $ awaitStep15WorkEvidenceSince workCursors workLedgers
            $ \evidence -> 3 `elem` nativeHealthReplyCounts (evidence !! 3)
        cutNativeHealth
        cutCursors <- snapshotStep15WorkCursors workLedgers
        closeStep15H4PeerCut deployment
        _ <-
          awaitWithin "H4 native-health cut did not complete an empty TCP round"
            $ awaitStep15WorkEvidenceSince cutCursors workLedgers
            $ \evidence -> 0 `elem` nativeHealthReplyCounts (evidence !! 3)
        overlay <-
          awaitWithin
            "H4 application did not observe its isolation overlay"
            (EAPP.awaitApplicationIsolation application)
        assertEqual
          "the losing resident process is presented through the closed Zombie overlay"
          EAPP.ResidentProcessesZombie
          overlay

        duringDrain <-
          readStep15PredefinedStore
            "H4 read-only isolation drain"
            application
            access
        assertEqual
          "the isolation drain preserves the Store cut and projects H4 labels to Zombie"
          (fmap (projectZombieLabel (startupAccessProcess startup)) baseline)
          duringDrain

        mutation <- EAPP.wait application []
        assertEqual
          "the application client rejects mutation after the isolation notice"
          (EAPP.ApplicationCallRejected ApplicationOperateNotPermitted)
          =<< awaitStep15ApplicationCallWithin
            "H4 post-isolation mutation"
            mutation

        assertEqual
          "the terminal application disposition reports the exact isolation cause"
          EAPP.HeraldIsolated
          =<< awaitWithin
            "H4 application did not observe its terminal isolation disposition"
            (EAPP.awaitApplicationUnavailable application)
        _ <-
          awaitWithin "H4 did not finish its application isolation drain"
            $ awaitStep15WorkEvidenceSince
              workCursors
              workLedgers
              (\evidence -> fmap (step15IsolationFinishes . summarizeStep15WorkEvidence) evidence == [0, 0, 0, 1])
        pure ()
    awaitStep15SurvivorMesh deployment
    assertEqual
      "H1-H3 remain independently supervised after H4 self-fences"
      [True, True, True]
      =<< traverse
        step15ManagedHeraldIsRunning
        [step15H1 deployment, step15H2 deployment, step15H3 deployment]
    assertEqual
      "the isolated H4 retains its live control shell"
      True
      =<< step15ManagedHeraldIsRunning (step15H4 deployment)

    h4TerminalCursor <-
      snapshotStep15WorkCursors [step15ManagedHeraldWorkLedger (step15H4 deployment)]
    healNativeHealth
    openStep15H4PeerTransport deployment
    awaitStep15SurvivorMesh deployment
    assertEqual
      "the fenced H4 control shell remains alive after native-health and peer transports heal"
      True
      =<< step15ManagedHeraldIsRunning (step15H4 deployment)
    h4TerminalEvidence <-
      fmap
        (fmap snd)
        ( snapshotStep15WorkEvidenceSince
            h4TerminalCursor
            [step15ManagedHeraldWorkLedger (step15H4 deployment)]
        )
    assertBool
      "the fenced H4 emits no semantic peer work after physical connectivity returns"
      (all evidenceHasNoSemanticPeerWork h4TerminalEvidence)

    workEvidence <-
      fmap
        (fmap snd)
        (snapshotStep15WorkEvidenceSince workCursors workLedgers)
    let work = fmap summarizeStep15WorkEvidence workEvidence
    assertEqual
      "the isolation schedule introduces no kernel invariant fault"
      [0, 0, 0, 0]
      (fmap step15KernelFaults work)
    assertEqual
      "only H4 emits the one terminal isolation effect"
      [0, 0, 0, 1]
      (fmap step15IsolationFinishes work)
    assertEqual
      "only H4 disposes its resident session, with the exact isolation reason"
      [[], [], [], [ApplicationHeraldIsolated]]
      (fmap isolationDisposalReasons workEvidence)
    assertFoundationWorkBounds "isolated-H4 self-fence" work
    assertEqual
      "local H4 isolation does not change the Oracle/Raft voter topology"
      oracleNodesBefore
      =<< currentOracleNodes deployment
    assertEqual
      "self-fencing the isolated non-voter cannot invent an Oracle membership change"
      membershipBefore
      =<< currentOracleMembership deployment

  case observed of
    Nothing -> assertFailure "Step-15 H4 isolation schedule exceeded its failure bound"
    Just () -> pure ()

-- Only successfully admitted, immutable completions from the ordinary health
-- interpreter count; the test fault injector never submits observations.
nativeHealthReplyCounts :: Step15WorkEvidence -> [Int]
nativeHealthReplyCounts evidence =
  [ length observations
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- step15RuntimeEvidence evidence,
    RuntimeObserved (OracleHealthRoundObserved _ observations) <- [inputBody input]
  ]

deploymentHeralds :: Step15Deployment -> [Step15Herald]
deploymentHeralds deployment =
  [ step15H1 deployment,
    step15H2 deployment,
    step15H3 deployment,
    step15H4 deployment
  ]

evidenceIsEmpty :: Step15WorkEvidence -> Bool
evidenceIsEmpty evidence =
  null (step15RuntimeEvidence evidence)
    && null (step15ConfiguredSeedEvidence evidence)
    && null (step15PeerDialEvidence evidence)
    && null (step15HeartbeatEvidence evidence)
    && null (step15OracleActionEvidence evidence)
    && null (step15OracleManagerEvidence evidence)

-- | These are amplification guards, not latency targets.  The kernel and
-- Oracle ceilings retain the established Step-14 four-Herald envelope.  The
-- physical dial ceiling is derived from the three survivor-to-H4 links, the
-- two-second isolation grace, and the configured 10 ms retry pace, with room
-- for boundary races.  A retry path which stops honouring its pace therefore
-- fails by attributable work even if the wall-clock guard has not expired.
assertFoundationWorkBounds ::
  String ->
  [Step15AttributableWork] ->
  Assertion
assertFoundationWorkBounds scenario work = do
  assertWorkBound "kernel inputs" 2_000 (sumField step15KernelInputs)
  assertWorkBound "physical peer-dial attempts" 768 physicalPeerDialAttempts
  assertWorkBound "complete timer lifecycle" 2_048 (sumField step15TimerLifecycleEvents)
  assertWorkBound "complete peer-dial lifecycle" 1_536 (sumField step15PeerDialLifecycleEvents)
  assertWorkBound "complete heartbeat lifecycle" 256 (sumField step15HeartbeatLifecycleEvents)
  assertWorkBound "complete Oracle lifecycle" 1_024 (sumField step15OracleLifecycleEvents)
  assertWorkBound "heartbeat/probe messages" 64 heartbeatAndProbeMessages
  assertWorkBound "Oracle submissions" 320 (sumField step15OracleSubmissions)
  assertWorkBound "terminal-source controls" 256 (sumField step15TerminalControlEvents)
  assertWorkBound "structural-applied reports" 512 (sumField step15StructuralAppliedReports)
  where
    assertWorkBound description upperBound observed =
      assertBool
        ( "Step-15 "
            <> scenario
            <> " exceeded the attributable "
            <> description
            <> " bound "
            <> show upperBound
            <> "; observed="
            <> show observed
            <> "; per-Herald work="
            <> show work
        )
        (observed <= upperBound)

    sumField project = sum (fmap project work)
    physicalPeerDialAttempts =
      sumField
        ( \counts ->
            step15ConfiguredSeedDialAttempts counts
              + step15PeerDialAttempts counts
        )
    heartbeatAndProbeMessages =
      sumField
        ( \counts ->
            step15HeartbeatWrites counts
              + step15FailureProbeRequests counts
              + step15FailureProbeResponses counts
        )

currentOracleNodes :: Step15Deployment -> IO [RaftNodeId]
currentOracleNodes deployment =
  sort
    . fmap (oracleRuntimeNode . snd)
    <$> oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)

currentOracleMembership :: Step15Deployment -> IO HeraldMembershipGeneration
currentOracleMembership deployment = do
  statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
  case nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses) of
    [generation] -> pure generation
    generations ->
      assertFailure
        ("Oracle replicas disagree on Step-15 Herald membership: " <> show generations)

isolationDisposalReasons :: Step15WorkEvidence -> [ApplicationSessionUnavailableReason]
isolationDisposalReasons evidence =
  [ reason
  | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- step15RuntimeEvidence evidence,
    DisposeApplicationSession _ reason <- effectBatchMembers effects
  ]

evidenceHasNoSemanticPeerWork :: Step15WorkEvidence -> Bool
evidenceHasNoSemanticPeerWork evidence =
  null
    [ effect
    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- step15RuntimeEvidence evidence,
      effect <- effectBatchMembers effects,
      case effect of
        SemanticSendPeerItem {} -> True
        SemanticSendPeerControl {} -> True
        _ -> False
    ]

awaitWithin :: String -> IO value -> IO value
awaitWithin context action = do
  observed <- timeout 5_000_000 action
  maybe (assertFailure context) pure observed

projectZombieLabel :: PrivateProcessId -> ApplicationValue -> ApplicationValue
projectZombieLabel process = \case
  BoolValue value -> BoolValue value
  Int64Value value -> Int64Value value
  BytesValue value -> BytesValue value
  TextValue value -> TextValue value
  UniqueIdValue value -> UniqueIdValue value
  LabelValue (ProcessLabel labelled, generation)
    | labelled == process -> LabelValue (ZombieLabel labelled, generation)
  LabelValue label -> LabelValue label
  RecordValue fields -> RecordValue (Map.map (projectZombieLabel process) fields)
  SortDefinitionValue value -> SortDefinitionValue value
  EnumValue value -> EnumValue value
  OptionalUniqueIdValue value -> OptionalUniqueIdValue value
