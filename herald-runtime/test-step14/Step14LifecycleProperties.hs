{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Public real-TCP witnesses for the label lifecycle cases whose terminal
-- application effects are directly observable. Authority-tenure tags and
-- control-qualified topology debt remain covered by deterministic owner tests:
-- neither is exported through EAPP merely to make this scenario inspect it.
module Step14LifecycleProperties (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToDelete, LabelToProcess),
    LabelResult (LabelApplied, LabelNotApplied),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel),
  )
import Eclips.Domain.Identity (controlIndexWord64)
import Eclips.Herald.Input (HeraldInputBody (OracleInput), inputBody)
import Eclips.Herald.OracleClient (OracleClientIngress (OracleEntriesReceived))
import Eclips.Herald.Runtime.Internal.Trace (KernelTraceStep (KernelTraceStepped), RuntimeTraceEvent (KernelEvent))
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Projection (OracleProjectionEventView (LabelWorkflowCompletedView), appliedEntryControlIndex, appliedEntryProjectionEvents, oracleProjectionEventView)
import Eclips.Protocol.Application.Types (applicationClientNonce)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats qualified as RTS
import Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    establishStep14ApplicationTopology,
    expectStep14ApplicationLabel,
    expectStep14LabelResult,
    expectStep14WriteAccepted,
    publishStep14Message,
    readStep14EdgeCarrier,
    readStep14Messages,
    readStep14NablaCarrier,
    step14MessageValue,
  )
import Step14Fixtures
  ( Step14Deployment,
    awaitStep14OraclePrefix,
    awaitStep14PeerMesh,
    snapshotStep14HeraldTrace,
    step14ApplicationLivenessConfiguration,
    step14H1,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14Deployment,
  )
import System.CPUTime (getCPUTime)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Step-14 public label lifecycle"
    [ testCase
        "same-label and process handoff cross real TCP"
        caseAuthorityLifecycle,
      testCase
        "Edge deletion removes the route without retracting retained state"
        caseEdgeDeletion
    ]

caseAuthorityLifecycle :: IO ()
caseAuthorityLifecycle = do
  measureLabels <- (== Just "1") <$> lookupEnv "ECLIPS_LABEL_MEASURE"
  stage <- newIORef "deployment startup"
  completed <-
    timeout 60_000_000
      $ withStep14Topology
      $ \deployment pApplication pStartup qApplication _ topology -> do
        baseline <-
          atLifecycleStage stage "submit baseline publication"
            $ publishStep14Message pApplication topology routedMessage
        atLifecycleStage stage "await baseline publication result"
          $ expectStep14WriteAccepted "routed value before lifecycle changes" baseline
        atLifecycleStage stage "await baseline at Q"
          $ awaitMessagePresence
            "the preserving Edge carries the baseline value"
            qApplication
            topology
            routedMessage

        let pProcess = startupAccessProcess pStartup
        measureLabelCall measureLabels "same_owner" $ do
          sameLabel <-
            atLifecycleStage stage "submit same-label Nabla release"
              $ EAPP.label
                pApplication
                (asPrivateObjectId topology.pNablaObject)
                ((ProcessLabel pProcess, 0))
                (LabelToProcess pProcess)
          atLifecycleStage stage "await same-label Nabla release result"
            $ expectStep14LabelResult "same-label Nabla release" LabelApplied sameLabel
        atLifecycleStage stage "await same-label Oracle prefix"
          $ awaitCompletedLabelOraclePrefix deployment
        atLifecycleStage
          stage
          "read same-label Nabla carrier"
          (readStep14NablaCarrier pApplication topology)
          >>= expectStep14ApplicationLabel
            "same-owner release retains P and advances generation"
            (ProcessLabel pProcess, 1)
        atLifecycleStage stage "confirm same-label baseline at Q"
          $ awaitMessagePresence
            "same-label release preserves the ordinary route"
            qApplication
            topology
            routedMessage

        measureLabelCall measureLabels "stale_generation" $ do
          stale <-
            atLifecycleStage stage "submit stale generation after same-owner release"
              $ EAPP.label
                pApplication
                (asPrivateObjectId topology.pNablaObject)
                (ProcessLabel pProcess, 0)
                (LabelToProcess topology.localizedQProcess)
          atLifecycleStage stage "stale generation is not applied"
            $ expectStep14LabelResult "same-owner generation invalidates the old expected pair" LabelNotApplied stale
        atLifecycleStage
          stage
          "read after stale generation"
          (readStep14NablaCarrier pApplication topology)
          >>= expectStep14ApplicationLabel "failed compare preserves generation" (ProcessLabel pProcess, 1)

        measureLabelCall measureLabels "handoff" $ do
          handoff <-
            atLifecycleStage stage "submit P-to-Q Nabla handoff"
              $ EAPP.label
                pApplication
                (asPrivateObjectId topology.pNablaObject)
                (ProcessLabel pProcess, 1)
                (LabelToProcess topology.localizedQProcess)
          atLifecycleStage stage "await P-to-Q Nabla handoff result"
            $ expectStep14LabelResult "P-to-Q Nabla handoff" LabelApplied handoff
        atLifecycleStage stage "await handoff Oracle prefix"
          $ awaitCompletedLabelOraclePrefix deployment
        atLifecycleStage
          stage
          "read handed-off Nabla carrier"
          (readStep14NablaCarrier pApplication topology)
          >>= expectStep14ApplicationLabel
            "P observes Q as the new Nabla controller at generation two"
            (ProcessLabel topology.localizedQProcess, 2)
        atLifecycleStage stage "confirm handoff baseline at Q"
          $ awaitMessagePresence
            "handoff preserves already-routed ordinary state"
            qApplication
            topology
            routedMessage
        writeIORef stage "scenario complete"
  case completed of
    Just () -> pure ()
    Nothing -> do
      blocked <- readIORef stage
      assertFailure
        ( "the public authority lifecycle exceeded its failure bound at: "
            <> blocked
        )

caseEdgeDeletion :: IO ()
caseEdgeDeletion = do
  measureLabels <- (== Just "1") <$> lookupEnv "ECLIPS_LABEL_MEASURE"
  stage <- newIORef "deployment startup"
  completed <-
    timeout 60_000_000
      $ withStep14Topology
      $ \deployment pApplication pStartup qApplication _ topology -> do
        baseline <-
          atLifecycleStage stage "submit baseline publication"
            $ publishStep14Message pApplication topology routedMessage
        atLifecycleStage stage "await baseline publication result"
          $ expectStep14WriteAccepted "routed value before Edge deletion" baseline
        atLifecycleStage stage "await baseline at Q"
          $ awaitMessagePresence
            "the preserving Edge carries the baseline value"
            qApplication
            topology
            routedMessage

        let pProcess = startupAccessProcess pStartup
        edgeBeforeDelete <-
          atLifecycleStage stage "read Edge before deletion"
            $ readStep14EdgeCarrier pApplication topology
        edge <-
          maybe
            (assertFailure "the preserving Edge carrier is absent before deletion")
            pure
            edgeBeforeDelete
        expectStep14ApplicationLabel
          "the preserving Edge remains controlled by P"
          ((ProcessLabel pProcess, 0))
          edge

        measureLabelCall measureLabels "edge_delete" $ do
          deleted <-
            atLifecycleStage stage "submit Edge deletion"
              $ EAPP.label
                pApplication
                (asPrivateObjectId topology.pEdgeObject)
                ((ProcessLabel pProcess, 0))
                LabelToDelete
          atLifecycleStage stage "await Edge deletion result"
            $ expectStep14LabelResult "preserving Edge deletion" LabelApplied deleted
        atLifecycleStage stage "await Edge deletion Oracle prefix"
          $ awaitCompletedLabelOraclePrefix deployment
        atLifecycleStage stage "await Edge carrier absence"
          $ awaitEdgeDeletion pApplication topology
        atLifecycleStage stage "confirm retained baseline at Q"
          $ awaitMessagePresence
            "Edge deletion does not retract already-routed ordinary state"
            qApplication
            topology
            routedMessage

        afterDelete <-
          atLifecycleStage stage "submit post-deletion publication"
            $ publishStep14Message pApplication topology postDeleteMessage
        atLifecycleStage stage "await post-deletion publication result"
          $ expectStep14WriteAccepted "ordinary value after Edge deletion" afterDelete

        measureLabelCall measureLabels "sequencing_fence" $ do
          settled <-
            atLifecycleStage stage "submit post-deletion sequencing fence"
              $ EAPP.label
                pApplication
                (asPrivateObjectId topology.pSequencingObject)
                ((ProcessLabel pProcess, 0))
                (LabelToProcess pProcess)
          atLifecycleStage stage "await post-deletion sequencing fence result"
            $ expectStep14LabelResult
              "post-deletion sequencing-target fence"
              LabelApplied
              settled
        atLifecycleStage stage "await post-deletion fence Oracle prefix"
          $ awaitCompletedLabelOraclePrefix deployment
        values <-
          atLifecycleStage stage "read Q after post-deletion fence"
            $ readStep14Messages qApplication topology
        assertBool
          "new ordinary state crossed the deleted Edge after its sequencing fence"
          (step14MessageValue postDeleteMessage `notElem` values)
        writeIORef stage "scenario complete"
  case completed of
    Just () -> pure ()
    Nothing -> do
      blocked <- readIORef stage
      assertFailure
        ( "the public Edge-deletion lifecycle exceeded its failure bound at: "
            <> blocked
        )

-- The application success already follows compact completion. Use its observed
-- canonical coordinate when waiting for all voters, independently of how many
-- unrelated Oracle entries preceded the operation.
awaitCompletedLabelOraclePrefix :: Step14Deployment -> IO ()
awaitCompletedLabelOraclePrefix deployment = do
  events <- snapshotStep14HeraldTrace (step14H1 deployment)
  let completions =
        [ appliedEntryControlIndex entry
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
          canonical <- NonEmpty.toList entries,
          let entry = canonicalAppliedOracleEntryValue canonical,
          event <- appliedEntryProjectionEvents entry,
          LabelWorkflowCompletedView {} <- [oracleProjectionEventView event]
        ]
  case completions of
    [] -> assertFailure "successful label has no observed canonical completion"
    _ -> awaitStep14OraclePrefix (controlIndexWord64 (maximum completions)) deployment

atLifecycleStage :: IORef String -> String -> IO value -> IO value
atLifecycleStage stage name action = writeIORef stage name >> action

-- | Opt-in measurements include submission, waiting, and terminal-result
-- validation. All four Heralds, the Oracle voters and the application clients
-- run in this test process, so CPU and allocation include their concurrent work
-- and the fixture's exact-history recording. Select exactly one Tasty case when
-- using ECLIPS_LABEL_MEASURE=1, with +RTS -T to enable the RTS counters.
-- Allocation is observed only through the latest GC; it is an approximate
-- interval delta, not retained memory. No GC is forced for these observations.
measureLabelCall :: Bool -> String -> IO () -> IO ()
measureLabelCall False _ action = action
measureLabelCall True operation action = do
  statsEnabled <- RTS.getRTSStatsEnabled
  unless statsEnabled
    $ assertFailure "ECLIPS_LABEL_MEASURE=1 requires RTS statistics (+RTS -T)"
  beforeStats <- RTS.getRTSStats
  beforeCpu <- getCPUTime
  beforeWall <- getMonotonicTimeNSec
  action
  afterWall <- getMonotonicTimeNSec
  afterCpu <- getCPUTime
  afterStats <- RTS.getRTSStats
  putStrLn
    ( "ECLIPS_LABEL_MEASURE {\"operation\":"
        <> show operation
        <> ",\"process_cpu_picoseconds\":"
        <> show (afterCpu - beforeCpu)
        <> ",\"elapsed_nanoseconds\":"
        <> show (afterWall - beforeWall)
        <> ",\"gc_observed_allocation_bytes\":"
        <> show (RTS.allocated_bytes afterStats - RTS.allocated_bytes beforeStats)
        <> ",\"gc_cpu_nanoseconds\":"
        <> show (RTS.gc_cpu_ns afterStats - RTS.gc_cpu_ns beforeStats)
        <> "}"
    )

withStep14Topology ::
  ( Step14Deployment ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    Step14ApplicationTopology ->
    IO result
  ) ->
  IO result
withStep14Topology use =
  withStep14Deployment $ \deployment -> do
    awaitStep14PeerMesh deployment
    withStep14Applications deployment $ \pApplication pStartup qApplication qStartup -> do
      topology <-
        establishStep14ApplicationTopology
          pApplication
          pStartup
          qApplication
          qStartup
      use deployment pApplication pStartup qApplication qStartup topology

withStep14Applications ::
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep14Applications deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_101)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_102)
                  step14ApplicationLivenessConfiguration
              )
              (use pApplication pStartup)
          requireApplicationResult "Q" qResult
      )
  requireApplicationResult "P" pResult

requireApplicationResult ::
  String ->
  Either EAPP.ApplicationRuntimeFailure result ->
  IO result
requireApplicationResult name = \case
  Left failure ->
    assertFailure (name <> " application runtime failed: " <> show failure)
  Right result -> pure result

awaitMessagePresence ::
  String ->
  EAPP.Application ->
  Step14ApplicationTopology ->
  Text ->
  IO ()
awaitMessagePresence context application topology message = do
  observed <- timeout 10_000_000 loop
  unless (observed == Just ()) (assertFailure (context <> " did not settle"))
  where
    expected = step14MessageValue message
    loop = do
      values <- readStep14Messages application topology
      if expected `elem` values
        then pure ()
        else threadDelay 10_000 >> loop

awaitEdgeDeletion ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO ()
awaitEdgeDeletion application topology = do
  observed <- timeout 10_000_000 loop
  unless (observed == Just ())
    $ assertFailure "the deleted Edge remained visible through P's predefined reader"
  where
    loop = do
      edge <- readStep14EdgeCarrier application topology
      case edge of
        Nothing -> pure ()
        Just _ -> threadDelay 10_000 >> loop

routedMessage :: Text
routedMessage = "lifecycle route witness"

postDeleteMessage :: Text
postDeleteMessage = "post-deletion route witness"
