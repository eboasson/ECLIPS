{-# LANGUAGE LambdaCase #-}

module Step15CrashProperties (tests, currentMembership, awaitH4Retirement) where

import Control.Concurrent (threadDelay)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (NeutralVertexRole),
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationRetiredHeraldEpoch,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (HeraldRetired))
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (WatchOracle),
    OracleClientIngress (OracleBindingLost, OracleEntriesReceived),
    oracleBindingContact,
    oracleContactNode,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( OracleManagerProbeEvent,
    PeerDialProbeEvent (PeerDialProbeEvent),
  )
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Command (ProbeResult (ProbeUnreachable))
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
    appliedEntryControlIndex,
  )
import Eclips.Oracle.Runtime
  ( oracleRuntimeAppliedControlIndex,
    oracleRuntimeMembershipGeneration,
    oracleRuntimeNode,
  )
import Eclips.Oracle.Runtime.TCP (oracleTcpNodeStatuses)
import Eclips.Raft.Identity (RaftNodeId)
import Step15Applications
  ( readStep15PredefinedStore,
    step15PredefinedAccess,
    withStep15Applications,
  )
import Step15Configuration
  ( Step15HeraldName (..),
    step15HeraldEpoch,
    step15OracleVoterHostEpochs,
    step15OracleVoterNodes,
  )
import Step15Fixtures
  ( Step15Deployment,
    Step15Herald,
    awaitStep15PeerMesh,
    awaitStep15SurvivorMesh,
    crashStep15H4,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldIsRunning,
    step15ManagedHeraldWorkLedger,
    withStep15CrashDeployment,
  )
import Step15Genesis (step15H4ProcessEpochId)
import Step15WorkCounts
  ( Step15AttributableWork (..),
    Step15WorkCursor,
    Step15WorkEvidence,
    Step15WorkLedger,
    snapshotStep15WorkCursors,
    snapshotStep15WorkEvidenceSince,
    step15HeartbeatLifecycleEvents,
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
    "Step-15 crash retirement"
    [ testCase
        "the voter majority retires crashed non-voter H4 and survivors keep working"
        caseVoterMajorityRetiresH4
    ]

caseVoterMajorityRetiresH4 :: Assertion
caseVoterMajorityRetiresH4 = do
  observed <- timeout scenarioTimeoutMicroseconds $ withStep15CrashDeployment $ \deployment -> do
    awaitStep15PeerMesh deployment
    predecessor <- currentMembership deployment
    assertEqual
      "the crash starts in the four-Herald genesis generation"
      Nothing
      (heraldMembershipGenerationRetiredHeraldEpoch predecessor)

    withStep15Applications deployment
      $ \h1Application h1Startup _ _ _h4Application _h4Startup -> do
        h1Neutral <- step15PredefinedAccess "H1" NeutralVertexRole h1Startup
        h1Before <- readStep15PredefinedStore "H1 pre-crash neutral read" h1Application h1Neutral
        let heralds = deploymentHeralds deployment
            ledgers = fmap step15ManagedHeraldWorkLedger heralds
        cursors <- snapshotStep15WorkCursors ledgers

        crashStep15H4 deployment
        awaitStep15SurvivorMesh deployment
        assertEqual
          "an abrupt H4 crash joins only H4's independently managed scope"
          [True, True, True, False]
          =<< traverse step15ManagedHeraldIsRunning heralds

        successor <- awaitH4Retirement deployment predecessor
        assertEqual
          "H4 retirement preserves the three Oracle/Raft voters"
          step15OracleVoterNodes
          =<< currentOracleNodes deployment

        readyEvidence <- awaitCrashEvidence deployment cursors ledgers
        assertCrashProjection predecessor successor (concatMap step15OracleProjectionViews (take 3 readyEvidence))
        h1After <-
          readStep15PredefinedStore
            "H1 post-retirement local read"
            h1Application
            h1Neutral
        assertEqual
          "H1 continues serving a real local application read after the successor cut"
          h1Before
          h1After
        evidence <-
          fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
        let work = fmap summarizeStep15WorkEvidence evidence
        assertEqual
          "the real crash/retirement tour introduces no kernel invariant fault"
          [0, 0, 0, 0]
          (fmap step15KernelFaults work)
        assertCrashProjection predecessor successor (concatMap step15OracleProjectionViews (take 3 evidence))
        assertCrashWorkBounds work

  case observed of
    Nothing -> assertFailure "Step-15 H4 crash/retirement tour exceeded its failure bound"
    Just () -> pure ()

deploymentHeralds :: Step15Deployment -> [Step15Herald]
deploymentHeralds deployment =
  [step15H1 deployment, step15H2 deployment, step15H3 deployment, step15H4 deployment]

currentMembership :: Step15Deployment -> IO HeraldMembershipGeneration
currentMembership deployment = do
  statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
  case nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses) of
    [generation] -> pure generation
    generations ->
      assertFailure ("Oracle replicas disagree on Step-15 membership: " <> show generations)

awaitH4Retirement ::
  Step15Deployment ->
  HeraldMembershipGeneration ->
  IO HeraldMembershipGeneration
awaitH4Retirement deployment predecessor = do
  observed <- timeout progressTimeoutMicroseconds loop
  maybe (assertFailure "Oracle did not retire H4 through the live voter-majority path") pure observed
  where
    loop = do
      statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
      let generations = nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses)
      case generations of
        [successor]
          | heraldMembershipGenerationRetiredHeraldEpoch successor
              == Just (step15HeraldEpoch Step15H4),
            sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))
              == sort (fmap step15HeraldEpoch [Step15H1, Step15H2, Step15H3]),
            heraldMembershipGenerationId successor /= heraldMembershipGenerationId predecessor ->
              pure successor
        _ -> threadDelay 10_000 >> loop

awaitCrashEvidence ::
  Step15Deployment ->
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  IO [Step15WorkEvidence]
awaitCrashEvidence deployment cursors ledgers = do
  observed <- timeout progressTimeoutMicroseconds loop
  case observed of
    Just evidence -> pure evidence
    Nothing -> do
      evidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      oracleStatuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
      assertFailure
        ( "survivors did not project the complete H4 retirement evidence; views="
            <> show (fmap (nub . step15OracleProjectionViews) (take 3 evidence))
            <> "; dial intents="
            <> show (fmap uniqueDialIntents (take 3 evidence))
            <> "; Oracle watch diagnostics="
            <> show (fmap oracleWatchDiagnostic (take 3 evidence))
            <> "; Oracle replica statuses="
            <> show
              [ ( oracleRuntimeNode status,
                  oracleRuntimeAppliedControlIndex status,
                  oracleRuntimeMembershipGeneration status
                )
              | (_, status) <- oracleStatuses
              ]
            <> "; per-Herald work="
            <> show (fmap summarizeStep15WorkEvidence evidence)
        )
  where
    loop = do
      evidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      let survivorViews = fmap (nub . step15OracleProjectionViews) (take 3 evidence)
          views = nub (concat survivorViews)
          reports =
            nub
              [ reporter
              | OracleFailureProbeReportRecordedView _ reporter ProbeUnreachable <- views
              ]
          projectedEverywhere =
            all
              (\installed -> any isH4End installed && any isMembershipAdvance installed)
              survivorViews
          complete =
            length reports >= 2
              && projectedEverywhere
              && any isRetirement views
      if complete then pure evidence else threadDelay 10_000 >> loop

    isH4End = \case
      ProcessEpochEndedEventView process _ HeraldRetired -> process == step15H4ProcessEpochId
      _ -> False
    isMembershipAdvance = \case
      HeraldMembershipAdvancedView {} -> True
      _ -> False
    isRetirement = \case
      OracleFailureProbeRetiredView {} -> True
      _ -> False

    uniqueDialIntents evidence =
      nub [intent | PeerDialProbeEvent intent _ <- step15PeerDialEvidence evidence]

data OracleWatchDiagnostic = OracleWatchDiagnostic
  { watchRequests :: [(String, String)],
    receivedBatches :: [(String, [String])],
    lostBindings :: [String],
    managerEvents :: [OracleManagerProbeEvent]
  }
  deriving stock (Show)

oracleWatchDiagnostic :: Step15WorkEvidence -> OracleWatchDiagnostic
oracleWatchDiagnostic evidence =
  OracleWatchDiagnostic
    { watchRequests =
        [ (show (oracleContactNode (oracleBindingContact binding)), show index)
        | WatchOracle binding index <- step15OracleActionEvidence evidence
        ],
      receivedBatches =
        [ ( show (oracleContactNode (oracleBindingContact binding)),
            fmap
              (show . appliedEntryControlIndex . canonicalAppliedOracleEntryValue)
              (NonEmpty.toList entries)
          )
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- step15RuntimeEvidence evidence,
          OracleInput (OracleEntriesReceived binding entries) <- [inputBody input]
        ],
      lostBindings =
        [ show binding
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- step15RuntimeEvidence evidence,
          OracleInput (OracleBindingLost binding) <- [inputBody input]
        ],
      managerEvents = step15OracleManagerEvidence evidence
    }

assertCrashProjection ::
  HeraldMembershipGeneration ->
  HeraldMembershipGeneration ->
  [OracleProjectionEventView] ->
  Assertion
assertCrashProjection predecessor successor observedViews = do
  let views = nub observedViews
      opens =
        [ (probe, target, generation)
        | OracleFailureProbeOpenedView probe target generation _ <- views
        ]
      unreachableReports =
        [ (probe, reporter)
        | OracleFailureProbeReportRecordedView probe reporter ProbeUnreachable <- views
        ]
      advances = [generation | HeraldMembershipAdvancedView generation <- views]
      retirements =
        [ (probe, resolution, successorId)
        | OracleFailureProbeRetiredView probe resolution successorId <- views
        ]
      residentEnds =
        [ process
        | ProcessEpochEndedEventView process _ HeraldRetired <- views
        ]
  case opens of
    [(probe, target, generation)] -> do
      assertEqual "the canonical probe targets H4" (step15HeraldEpoch Step15H4) target
      assertEqual
        "the canonical probe is opened against the predecessor generation"
        (heraldMembershipGenerationId predecessor)
        generation
      let reporters = nub [reporter | (reportedProbe, reporter) <- unreachableReports, reportedProbe == probe]
      assertBool
        "two or three distinct voter hosts report H4 unreachable"
        ( length reporters >= 2
            && length reporters <= 3
            && all (`elem` step15OracleVoterHostEpochs) reporters
        )
      assertEqual
        "one canonical successor membership is projected"
        [successor]
        advances
      case retirements of
        [(retiredProbe, _, successorId)] -> do
          assertEqual "the retirement consumes the opened probe" probe retiredProbe
          assertEqual
            "the retirement names the exact successor generation"
            (heraldMembershipGenerationId successor)
            successorId
        other -> assertFailure ("expected one canonical H4 retirement, got " <> show other)
    other -> assertFailure ("expected one canonical H4 failure probe, got " <> show other)
  assertEqual
    "the resident H4 process receives one canonical HeraldRetired End"
    [step15H4ProcessEpochId]
    residentEnds

assertCrashWorkBounds :: [Step15AttributableWork] -> Assertion
assertCrashWorkBounds work = do
  bound "kernel inputs" 512 (sumField step15KernelInputs)
  bound "physical peer dials" 384 physicalDials
  bound "complete timer lifecycle" 1_024 (sumField step15TimerLifecycleEvents)
  bound "complete peer-dial lifecycle" 768 (sumField step15PeerDialLifecycleEvents)
  bound "complete heartbeat lifecycle" 256 (sumField step15HeartbeatLifecycleEvents)
  bound "complete Oracle lifecycle" 512 (sumField step15OracleLifecycleEvents)
  bound "Oracle submissions" 64 (sumField step15OracleSubmissions)
  bound "failure-probe messages" 64 failureProbeMessages
  bound "terminal-source controls" 64 (sumField step15TerminalControlEvents)
  bound "structural-applied reports" 256 (sumField step15StructuralAppliedReports)
  where
    sumField project = sum (fmap project work)
    physicalDials =
      sumField (\item -> step15ConfiguredSeedDialAttempts item + step15PeerDialAttempts item)
    failureProbeMessages =
      sumField (\item -> step15FailureProbeRequests item + step15FailureProbeResponses item)
    bound description upper observed =
      assertBool
        ( "Step-15 crash tour exceeded the attributable "
            <> description
            <> " bound "
            <> show upper
            <> "; observed="
            <> show observed
            <> "; per-Herald="
            <> show work
        )
        (observed <= upper)

currentOracleNodes :: Step15Deployment -> IO [RaftNodeId]
currentOracleNodes deployment =
  sort
    . fmap (oracleRuntimeNode . snd)
    <$> oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)

scenarioTimeoutMicroseconds, progressTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 20_000_000
progressTimeoutMicroseconds = 10_000_000
