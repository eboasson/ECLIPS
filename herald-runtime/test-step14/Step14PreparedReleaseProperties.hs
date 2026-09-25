{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Real transport witnesses selected settlement before the atomic decision,
-- independent Q-to-P publication while the decision is paused, and direct
-- installation completion before the successful label returns.
module Step14PreparedReleaseProperties (tests) where

import Control.Concurrent
  ( forkFinally,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception (SomeException, finally, onException)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (DeltaRole),
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateObjectId,
    sortIdBytes,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToVoid),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (LabelApplication, WriteApplication),
  )
import Eclips.Application.Types.Query
  ( ApplicationProjection (ApplicationProjection),
    ApplicationQuery (..),
    ApplicationQueryLiteral (QueryBytes),
    ApplicationQueryPredicate (QueryCompare),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult
      ( LabelCompleted,
        NewIdCompleted,
        WriteCompleted
      ),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationScalarComparison (ScalarEqual),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel, VoidLabel),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (WriteAccepted),
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    LabelDecisionId,
    controlIndexWord64,
  )
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Cancelled, Completed, Rejected),
    retainedReplyBodyRequestId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendApplicationReply),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (ApplicationRequestInput, OracleInput),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientIngress (OracleEntriesReceived),
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (ShellEffectRouted),
  )
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)

import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Label (LiveTerminalOutcomeView (LiveReleasedOutcomeView), liveDecisionId, liveTerminalOutcomeView, oracleCommandTag)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    appliedEntryReceiptRetirement,
    appliedEntryRequestId,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Runtime
  ( configureOracleRuntimeSubmissionCheckpoint,
  )
import Eclips.Protocol.Application.Types (applicationClientNonce)
import Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    awaitStep14Message,
    establishStep14ApplicationTopology,
    expectStep14ApplicationLabel,
    expectStep14Read,
    expectStep14WriteAccepted,
    publishStep14Message,
    readStep14SequencingCarrier,
    step14MessageValue,
  )
import Step14Fixtures
  ( Step14Deployment,
    awaitStep14LatchWithin,
    awaitStep14OraclePrefix,
    awaitStep14PeerMesh,
    newStep14Latch,
    semanticStep14InputBody,
    signalStep14Latch,
    snapshotStep14HeraldTrace,
    step14ApplicationLivenessConfiguration,
    step14H1,
    step14H2,
    step14H3,
    step14H4,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14DeploymentWithOracleConfigurations,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Step-14 atomic decision cut"
    [ testCase
        "unrelated Q-to-P traffic completes during a paused label decision"
        casePreparedReleasePause
    ]

casePreparedReleasePause :: IO ()
casePreparedReleasePause = do
  stage <- newIORef "construct deployment"
  retainedTraces <- newEmptyMVar :: IO (MVar (IO [(String, [RuntimeTraceEvent])]))
  checkpointRequest <- newEmptyMVar
  checkpointReached <- newStep14Latch
  releaseCheckpoint <- newStep14Latch
  let checkpoint request command observedIndex =
        when (oracleCommandTag command == 2) $ do
          claimed <- tryPutMVar checkpointRequest (request, observedIndex)
          when claimed $ do
            signalStep14Latch checkpointReached
            _ <- readMVar releaseCheckpoint
            pure ()
      configure =
        fmap (configureOracleRuntimeSubmissionCheckpoint checkpoint)
  completed <-
    ( timeout 150_000_000
        $ withStep14DeploymentWithOracleConfigurations configure
        $ \deployment -> do
          putMVar retainedTraces
            $ traverse
              (\(name, herald) -> do events <- snapshotStep14HeraldTrace herald; pure (name, events))
              [("H1", step14H1 deployment), ("H2", step14H2 deployment), ("H3", step14H3 deployment), ("H4", step14H4 deployment)]
          atStage stage "await peer mesh" (awaitStep14PeerMesh deployment)
          atStage stage "attach P and Q applications" $ withStep14ApplicationsReleasing releaseCheckpoint deployment $ \pApplication pStartup qApplication qStartup -> do
            topology <-
              atStage stage "establish application topology"
                $ establishStep14ApplicationTopology
                  pApplication
                  pStartup
                  qApplication
                  qStartup

            writeIORef stage "publish pre-fence message"
            preFenceCall <- publishStep14Message pApplication topology preFenceMessage
            expectStep14WriteAccepted "pre-fence publication" preFenceCall
            atStage stage "await pre-fence message at Q"
              $ awaitMessageWithin
                "pre-fence publication at Q"
                qApplication
                topology
                preFenceMessage

            unrelated <-
              atStage stage "reserve and observe unrelated Q Delta"
                $ prepareUnrelatedDelta
                  pApplication
                  pStartup
                  qApplication
                  qStartup
                  topology

            let expectedLabel = (ProcessLabel (startupAccessProcess pStartup), 0)
            writeIORef stage "submit label and pause its canonical decision"
            labelCall <-
              EAPP.label
                pApplication
                (asPrivateObjectId topology.pSequencingObject)
                expectedLabel
                LabelToVoid
            labelCompletion <- observeApplicationCall labelCall
            labelRequest <-
              awaitExactApplicationInput
                "the retained label request"
                deployment
                ( LabelApplication
                    (asPrivateObjectId topology.pSequencingObject)
                    expectedLabel
                    LabelToVoid
                )

            writeIORef stage "submit and observe post-fence write"
            postFenceCall <- publishStep14Message pApplication topology postFenceMessage
            postFenceCompletion <- observeApplicationCall postFenceCall
            (postRequest, postInputOrdinal) <-
              awaitWriteInput deployment topology.pNablaWriter postFenceMessage

            reached <- atStage stage "await atomic decision checkpoint" (awaitStep14LatchWithin 15_000_000 checkpointReached)
            assertBool "Release reached the atomic decision checkpoint" reached
            (blockedRequest, preparedPrefix) <- readMVar checkpointRequest
            atStage stage "verify captured Oracle prefix" (awaitStep14OraclePrefix (controlIndexWord64 preparedPrefix) deployment)
            preparedCheckpoint <- snapshotStep14HeraldTrace (step14H1 deployment)
            assertEqual
              "the captured prefix contains no decision before the paused submission"
              []
              (map (.phase) (labelControlTrace preparedCheckpoint))
            assertNoRetainedTerminalEffect
              "label remains pending while decision preflight is paused"
              labelRequest
              preparedCheckpoint
            assertNoRetainedTerminalEffect
              "post-fence write remains pending while decision preflight is paused"
              postRequest
              preparedCheckpoint

            writeIORef stage "publish unrelated Q-to-P write while decision is paused"
            unrelatedCall <-
              EAPP.write
                qApplication
                unrelated.writer
                (PublishValue unrelated.sourceValue)
            unrelatedCompletion <- observeApplicationCall unrelatedCall
            unrelatedRequest <- awaitUnrelatedWriteInput deployment unrelated
            unrelatedResult <- awaitObservedCall "unrelated Q-to-P Delta while decision is paused" unrelatedCompletion
            assertEqual
              "unrelated publication progresses without a global label gate"
              (EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted))
              unrelatedResult

            writeIORef stage "release checkpoint and await retained application results"
            signalStep14Latch releaseCheckpoint
            labelResult <- awaitObservedCall "released label result" labelCompletion
            assertEqual
              "the paused decision releases successfully"
              (EAPP.ApplicationCallSucceeded (LabelCompleted LabelApplied))
              labelResult
            postResult <- awaitObservedCall "released post-fence result" postFenceCompletion
            assertEqual
              "the positioned post-fence publication completes after Release"
              (EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted))
              postResult
            openedDecision <- awaitOpenedDecision deployment
            atStage stage "await released post-fence message at Q"
              $ awaitMessageWithin
                "post-fence publication at Q"
                qApplication
                topology
                postFenceMessage
            carrier <- atStage stage "read released sequencing carrier" (readStep14SequencingCarrier pApplication topology)
            expectStep14ApplicationLabel
              "the released label is visible through EAPP"
              (VoidLabel, 1)
              carrier
            visibleUnrelated <-
              atStage stage "await unrelated Delta visibility at P" (awaitUnrelatedDeltaVisible pApplication unrelated)
            afterVisibility <- readUnrelatedDelta pApplication unrelated
            assertEqual
              "unrelated Q-to-P traffic remains visible after Release"
              visibleUnrelated
              afterVisibility
            atStage stage "await canonical installation completion" (awaitWorkflowCompletion deployment openedDecision)

            writeIORef stage "check retained trace assertions"
            h2Events <- snapshotStep14HeraldTrace (step14H2 deployment)
            assertUnrelatedWriteCompletedOnce
              unrelated
              unrelatedRequest
              h2Events

            events <- snapshotStep14HeraldTrace (step14H1 deployment)
            (preRequest, _) <-
              requireWriteInput events topology.pNablaWriter preFenceMessage
            preCompletionOrdinal <- requireWriteCompletion events preRequest
            (tracedPostRequest, tracedPostInputOrdinal) <-
              requireWriteInput events topology.pNablaWriter postFenceMessage
            assertEqual
              "the retained post-fence input keeps its exact request identity"
              postRequest
              tracedPostRequest
            assertEqual
              "the retained post-fence input has one trace ordinal"
              postInputOrdinal
              tracedPostInputOrdinal
            postCompletionOrdinal <- requireWriteCompletion events postRequest
            release <- requireLabelRelease events
            assertEqual
              "the blocked preflight request is the applied Release request"
              blockedRequest
              release.request
            assertBool
              "the decision follows its observed preflight prefix"
              (release.index > preparedPrefix)
            let intervening =
                  [ entry
                  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
                    canonical <- NonEmpty.toList entries,
                    let entry = canonicalAppliedOracleEntryValue canonical,
                    appliedEntryControlIndex entry > preparedPrefix,
                    appliedEntryControlIndex entry < release.index
                  ]
            assertBool
              "only receipt maintenance may occupy the gap between preflight observation and decision"
              (all (\entry -> appliedEntryCommand entry == Nothing && appliedEntryReceiptRetirement entry /= Nothing) intervening)
            assertLabelControlSchedule release.decision events
            assertBool
              "trace order is pre-fence completion < Release < routed post-fence completion"
              ( preCompletionOrdinal < release.runtimeOrdinal
                  && release.runtimeOrdinal < postCompletionOrdinal
              )
            assertBool
              "the post-fence input was positioned before Release"
              (postInputOrdinal < release.runtimeOrdinal)
            writeIORef stage "application scope teardown"
          writeIORef stage "Herald semantic drain and worker join"
    )
      `onException` (releaseFailureContext stage retainedTraces checkpointRequest >>= putStrLn)
  unless (completed == Just ())
    $ assertFailure . ("the atomic-decision real-transport schedule exceeded its failure bound; " <>)
      =<< releaseFailureContext stage retainedTraces checkpointRequest

releaseFailureContext :: IORef String -> MVar (IO [(String, [RuntimeTraceEvent])]) -> MVar (OracleClientRequestId, ControlIndex) -> IO String
releaseFailureContext stage retainedTraces checkpointRequest = do
  context <- readIORef stage
  snapshots <- tryReadMVar retainedTraces >>= maybe (pure []) id
  checkpoint <- tryReadMVar checkpointRequest
  pure ("stage=" <> context <> "; checkpoint request=" <> show checkpoint <> "; retained traces=" <> show (fmap releaseFailureTrace snapshots))

atStage :: IORef String -> String -> IO result -> IO result
atStage stage context action = writeIORef stage context >> action

releaseFailureTrace :: (String, [RuntimeTraceEvent]) -> (String, [(ControlIndex, LabelPhase, LabelDecisionId)], [String], [String])
releaseFailureTrace (name, events) =
  ( name,
    nub [(entry.index, entry.phase, entry.decision) | entry <- labelControlTrace events],
    [show (inputBody input, fault) | KernelEvent _ (KernelTraceStepped _ _ input (Left fault)) <- events],
    reverse (take 8 (reverse [show (inputBody input) | KernelEvent _ (KernelTraceStepped _ _ input _) <- events]))
  )

-- Canonical Oracle entries expose the immutable decision and completion.
data LabelReleaseTrace = LabelReleaseTrace
  { request :: OracleClientRequestId,
    decision :: LabelDecisionId,
    index :: ControlIndex,
    runtimeOrdinal :: RuntimeEventOrdinal
  }

data LabelPhase = LabelDecisionApplied | LabelDecisionCompleted
  deriving stock (Eq, Show)

data LabelControlTrace = LabelControlTrace
  { index :: ControlIndex,
    phase :: LabelPhase,
    decision :: LabelDecisionId
  }

awaitOpenedDecision :: Step14Deployment -> IO LabelDecisionId
awaitOpenedDecision deployment = do
  observed <- timeout 10_000_000 loop
  case observed of
    Just decision -> pure decision
    Nothing -> do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      assertFailure
        ( "H1 did not receive the public LabelDecided trace event after every "
            <> "Oracle voter applied it; observed count="
            <> show (length (openedDecisions events))
        )
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      case openedDecisions events of
        [] -> threadDelay 10_000 >> loop
        [decision] -> pure decision
        observed ->
          assertFailure
            ( "expected one public LabelDecided trace event, got "
                <> show (length observed)
            )

openedDecisions :: [RuntimeTraceEvent] -> [LabelDecisionId]
openedDecisions events =
  [ entry.decision
  | entry <- labelControlTrace events,
    entry.phase == LabelDecisionApplied
  ]

requireLabelRelease :: [RuntimeTraceEvent] -> IO LabelReleaseTrace
requireLabelRelease events =
  case [ LabelReleaseTrace
           (appliedEntryRequestId command)
           (liveDecisionId decision)
           embeddedIndex
           ordinal
       | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
         OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
         canonical <- NonEmpty.toList entries,
         let entry = canonicalAppliedOracleEntryValue canonical,
         Just command <- [appliedEntryCommand entry],
         event <- appliedEntryProjectionEvents entry,
         LabelDecidedView decision terminal _ <- [oracleProjectionEventView event],
         LiveReleasedOutcomeView _ _ embeddedIndex _ <- [liveTerminalOutcomeView terminal]
       ] of
    [release] -> pure release
    observed ->
      assertFailure
        ("expected one public Applied decision trace event, got " <> show (length observed))

assertLabelControlSchedule :: LabelDecisionId -> [RuntimeTraceEvent] -> IO ()
assertLabelControlSchedule releasedDecision events = do
  let observed = labelControlTrace events
  assertEqual
    "one atomic decision is followed by one compact completion"
    [LabelDecisionApplied, LabelDecisionCompleted]
    (map (.phase) observed)
  assertBool
    "semantic workflow coordinates increase across any intervening maintenance"
    (and (zipWith (<) (map (.index) observed) (drop 1 (map (.index) observed))))
  assertEqual
    "every public workflow event carries the same decision identity"
    (Set.singleton releasedDecision)
    (Set.fromList (fmap (.decision) observed))

awaitWorkflowCompletion :: Step14Deployment -> LabelDecisionId -> IO ()
awaitWorkflowCompletion deployment decision = do
  completed <- timeout 15_000_000 loop
  assertBool "the collector commits one canonical completion after all installations" (completed == Just ())
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      let completions =
            Set.fromList
              [ appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)
              | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
                canonical <- NonEmpty.toList entries,
                event <- appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical),
                LabelWorkflowCompletedView observed _ <- [oracleProjectionEventView event],
                observed == decision
              ]
      if Set.size completions == 1 then pure () else threadDelay 10_000 >> loop

labelControlTrace :: [RuntimeTraceEvent] -> [LabelControlTrace]
labelControlTrace events =
  [ LabelControlTrace (appliedEntryControlIndex entry) phase decision
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries,
    let entry = canonicalAppliedOracleEntryValue canonical,
    event <- appliedEntryProjectionEvents entry,
    Just (phase, decision) <- [labelPhase (oracleProjectionEventView event)]
  ]
  where
    labelPhase = \case
      LabelDecidedView decided terminal _
        | LiveReleasedOutcomeView {} <- liveTerminalOutcomeView terminal ->
            Just (LabelDecisionApplied, liveDecisionId decided)
      LabelWorkflowCompletedView decision _ -> Just (LabelDecisionCompleted, decision)
      _ -> Nothing

withStep14ApplicationsReleasing ::
  MVar () ->
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep14ApplicationsReleasing releaseCheckpoint deployment use =
  withStep14Applications deployment $ \pApplication pStartup qApplication qStartup ->
    use pApplication pStartup qApplication qStartup
      `finally` signalStep14Latch releaseCheckpoint

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
          (applicationClientNonce 14_201)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_202)
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

observeApplicationCall ::
  EAPP.ApplicationCall ->
  IO (MVar (Either SomeException EAPP.ApplicationCallCompletion))
observeApplicationCall call = do
  observed <- newEmptyMVar
  _ <- forkFinally (EAPP.awaitApplicationCall call) (void . tryPutMVar observed)
  pure observed

assertNoRetainedTerminalEffect ::
  String ->
  RequestId ->
  [RuntimeTraceEvent] ->
  IO ()
assertNoRetainedTerminalEffect context request events =
  case [ body
       | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
         SendApplicationReply _ (RetainedRequestReply _ body) <-
           effectBatchMembers effects,
         retainedReplyBodyRequestId body == request,
         isTerminal body
       ] of
    [] -> pure ()
    observed ->
      assertFailure (context <> ": observed terminal effects " <> show observed)
  where
    isTerminal = \case
      Completed _ _ -> True
      Rejected _ _ -> True
      Cancelled _ _ -> True
      _ -> False

awaitObservedCall ::
  String ->
  MVar (Either SomeException EAPP.ApplicationCallCompletion) ->
  IO EAPP.ApplicationCallCompletion
awaitObservedCall context observed = do
  settled <- timeout 20_000_000 (readMVar observed)
  case settled of
    Nothing -> assertFailure (context <> " did not settle")
    Just (Left failure) -> assertFailure (context <> " raised " <> show failure)
    Just (Right completion) -> pure completion

prepareUnrelatedDelta ::
  EAPP.Application ->
  ApplicationStartupAccess ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  Step14ApplicationTopology ->
  IO UnrelatedDelta
prepareUnrelatedDelta pApplication pStartup qApplication qStartup topology = do
  pDelta <- accessFor "P" DeltaRole pStartup
  qDelta <- accessFor "Q" DeltaRole qStartup
  object <-
    expectNewId "unrelated Q Delta reservation"
      =<< EAPP.newid qApplication (ControlledNewId (predefinedWriter qDelta))
  let value =
        RecordValue
          ( Map.fromList
              [ ("label", LabelValue ((ProcessLabel (startupAccessProcess qStartup), 0))),
                ("object_id", UniqueIdValue object),
                ("sort_id", BytesValue (sortIdBytes topology.messageSort))
              ]
          )
      query =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton (predefinedReader pDelta),
            applicationQueryPredicate =
              QueryCompare
                (ApplicationProjection ("sort_id" :| []))
                ScalarEqual
                (QueryBytes (sortIdBytes topology.messageSort))
          }
  baseline <-
    expectStep14Read "P unrelated Delta baseline"
      =<< EAPP.read pApplication query
  pure
    UnrelatedDelta
      { writer = predefinedWriter qDelta,
        query,
        baseline,
        sourceValue = value,
        localizedProcess = topology.localizedQProcess,
        priorLocalizedObject = topology.localizedQDelta,
        sort = topology.messageSort
      }

data UnrelatedDelta = UnrelatedDelta
  { writer :: PrivateNablaId,
    query :: ApplicationQuery,
    baseline :: [ApplicationValue],
    sourceValue :: ApplicationValue,
    localizedProcess :: PrivateProcessId,
    priorLocalizedObject :: PrivateUniqueId,
    sort :: SortId
  }

awaitUnrelatedDeltaVisible ::
  EAPP.Application ->
  UnrelatedDelta ->
  IO [ApplicationValue]
awaitUnrelatedDeltaVisible application witness = do
  observed <- timeout 15_000_000 loop
  case observed of
    Nothing ->
      assertFailure "unrelated Q-to-P Delta did not become visible after Release"
    Just values -> pure values
  where
    loop = do
      values <- readUnrelatedDelta application witness
      if localizedUnrelatedAddition witness values
        then pure values
        else threadDelay 10_000 >> loop

-- Private identifiers are scoped to one application session.  H2's source
-- record therefore cannot be compared with P's observation: localization must
-- replace both its Q-private label and object ID.  The retained P baseline,
-- stable global sort bytes, and P-localized Q process identify the exact new
-- Delta; the distinct object ID proves that it is a newly localized carrier.
localizedUnrelatedAddition ::
  UnrelatedDelta ->
  [ApplicationValue] ->
  Bool
localizedUnrelatedAddition witness values =
  null (witness.baseline \\ values)
    && case values \\ witness.baseline of
      [RecordValue fields]
        | Map.size fields == 3,
          Just (LabelValue ((ProcessLabel process, _))) <- Map.lookup "label" fields,
          process == witness.localizedProcess,
          Just (UniqueIdValue object) <- Map.lookup "object_id" fields,
          object /= witness.priorLocalizedObject,
          Just (BytesValue observedSort) <- Map.lookup "sort_id" fields,
          observedSort == sortIdBytes witness.sort ->
            True
      _ -> False

awaitUnrelatedWriteInput ::
  Step14Deployment ->
  UnrelatedDelta ->
  IO RequestId
awaitUnrelatedWriteInput deployment witness = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing ->
      assertFailure "H2 did not retain the exact unrelated write input"
    Just request -> pure request
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H2 deployment)
      case unrelatedWriteRequests witness events of
        [] -> threadDelay 10_000 >> loop
        [request] -> pure request
        requests ->
          assertFailure
            ("H2 retained duplicate unrelated write inputs: " <> show requests)

assertUnrelatedWriteCompletedOnce ::
  UnrelatedDelta ->
  RequestId ->
  [RuntimeTraceEvent] ->
  IO ()
assertUnrelatedWriteCompletedOnce witness request events = do
  assertEqual
    "H2 admits the exact unrelated write request once"
    [request]
    (unrelatedWriteRequests witness events)
  assertEqual
    "H2 routes one terminal WriteAccepted reply for the unrelated request"
    [request]
    [ actual
    | ShellEvent _ (ShellEffectRouted _ _ effect) <- events,
      SendApplicationReply
        _
        (RetainedRequestReply _ (Completed actual (WriteCompleted WriteAccepted))) <-
        [effect],
      actual == request
    ]

unrelatedWriteRequests ::
  UnrelatedDelta ->
  [RuntimeTraceEvent] ->
  [RequestId]
unrelatedWriteRequests witness events =
  [ request
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    ApplicationRequestInput
      ( CallApplicationRequest
          _
          _
          request
          (WriteApplication writer (PublishValue value))
        ) <-
      [semanticStep14InputBody input],
    writer == witness.writer,
    value == witness.sourceValue
  ]

readUnrelatedDelta ::
  EAPP.Application ->
  UnrelatedDelta ->
  IO [ApplicationValue]
readUnrelatedDelta application witness =
  expectStep14Read "P unrelated Delta observation"
    =<< EAPP.read application (unrelatedQuery witness)

unrelatedQuery :: UnrelatedDelta -> ApplicationQuery
unrelatedQuery = (.query)

accessFor ::
  String ->
  ApplicationPredefinedSortRole ->
  ApplicationStartupAccess ->
  IO PredefinedAccess
accessFor process role startup =
  case (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey role) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess role writer reader)
    _ ->
      assertFailure
        (process <> " startup access is missing " <> show role)

expectNewId :: String -> EAPP.ApplicationCall -> IO PrivateUniqueId
expectNewId context call = do
  completion <- awaitApplicationCallWithin context 15_000_000 call
  case completion of
    EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
    other -> assertFailure (context <> ": unexpected result " <> show other)

awaitApplicationCallWithin ::
  String ->
  Int ->
  EAPP.ApplicationCall ->
  IO EAPP.ApplicationCallCompletion
awaitApplicationCallWithin context microseconds call = do
  observed <- observeApplicationCall call
  settled <- timeout microseconds (readMVar observed)
  case settled of
    Nothing -> do
      EAPP.cancelApplicationCall call
      assertFailure (context <> " did not settle")
    Just (Left failure) -> assertFailure (context <> " raised " <> show failure)
    Just (Right completion) -> pure completion

awaitMessageWithin ::
  String ->
  EAPP.Application ->
  Step14ApplicationTopology ->
  Text ->
  IO ()
awaitMessageWithin context application topology message = do
  observed <- newEmptyMVar
  _ <- forkFinally (awaitStep14Message application topology message) (void . tryPutMVar observed)
  settled <- timeout 15_000_000 (readMVar observed)
  case settled of
    Nothing -> assertFailure (context <> " did not settle")
    Just (Left failure) -> assertFailure (context <> " raised " <> show failure)
    Just (Right ()) -> pure ()

awaitExactApplicationInput ::
  String ->
  Step14Deployment ->
  ApplicationOperation ->
  IO RequestId
awaitExactApplicationInput context deployment expected = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing -> assertFailure (context <> " did not enter H1")
    Just request -> pure request
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      case [ request
           | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
             ApplicationRequestInput
               (CallApplicationRequest _ _ request operation) <-
               [semanticStep14InputBody input],
             operation == expected
           ] of
        [] -> threadDelay 10_000 >> loop
        [request] -> pure request
        requests ->
          assertFailure
            (context <> " entered H1 more than once: " <> show requests)

awaitWriteInput ::
  Step14Deployment ->
  PrivateNablaId ->
  Text ->
  IO (RequestId, RuntimeEventOrdinal)
awaitWriteInput deployment writer message = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing -> assertFailure "H1 did not retain the expected application write input"
    Just witness -> pure witness
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      case writeInput events writer message of
        Just witness -> pure witness
        Nothing -> threadDelay 10_000 >> loop

requireWriteInput ::
  [RuntimeTraceEvent] ->
  PrivateNablaId ->
  Text ->
  IO (RequestId, RuntimeEventOrdinal)
requireWriteInput events writer message =
  maybe
    (assertFailure ("missing exact write input for " <> show message))
    pure
    (writeInput events writer message)

writeInput ::
  [RuntimeTraceEvent] ->
  PrivateNablaId ->
  Text ->
  Maybe (RequestId, RuntimeEventOrdinal)
writeInput events writer message =
  case [ (request, ordinal)
       | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
         ApplicationRequestInput
           (CallApplicationRequest _ _ request (WriteApplication actualWriter (PublishValue value))) <-
           [semanticStep14InputBody input],
         actualWriter == writer,
         value == step14MessageValue message
       ] of
    [witness] -> Just witness
    _ -> Nothing

requireWriteCompletion ::
  [RuntimeTraceEvent] ->
  RequestId ->
  IO RuntimeEventOrdinal
requireWriteCompletion events request =
  case [ ordinal
       | ShellEvent ordinal (ShellEffectRouted _ _ effect) <- events,
         SendApplicationReply
           _
           (RetainedRequestReply _ (Completed actual (WriteCompleted WriteAccepted))) <-
           [effect],
         actual == request
       ] of
    [completion] -> pure completion
    [] -> assertFailure ("missing routed WriteAccepted reply for " <> show request)
    completions ->
      assertFailure
        ( "expected exactly one routed WriteAccepted reply for "
            <> show request
            <> ", got "
            <> show completions
        )

preFenceMessage, postFenceMessage :: Text
preFenceMessage = "prepared 01 pre-fence"
postFenceMessage = "prepared 02 post-fence"
