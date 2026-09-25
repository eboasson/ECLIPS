module AdapterRetentionProperties (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_, void)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity (controlIndex, mkGlobalObjectId)
import Eclips.Domain.Label (firstLabelProcessAcceptancePosition, homeLabelAcceptanceCut, initialBootstrapLabelEvidence, targetVoid)
import Eclips.Domain.Membership (heraldMembershipGenerationId)
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Oracle.Canonical (CanonicalOracleEnvelope, canonicalOracleEnvelopeBytes, canonicalOracleEnvelopeValue, canonicalizeOracleEnvelope)
import Eclips.Oracle.Command (completeLabelDecisionCommand, decideLabelCommand, oracleEnvelope, oracleEnvelopeWithReceiptRetirementProgress)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label (deriveLabelDecisionId, deriveLabelOutcomeDigest, labelCompletionAttestation)
import Eclips.Oracle.Receipt (OracleReceiptResult (OracleAccepted), OracleRequestRetirement (OracleRequestPrefixRetired), oracleReceiptControlIndex, oracleReceiptRequestId, oracleReceiptResult)
import Eclips.Oracle.Runtime.Internal.Adapter (runCommittedEntryAdapter)
import Eclips.Oracle.Runtime.Internal.CheckpointCache (captureOracleCheckpoint, emptyOracleCheckpointCache)
import Eclips.Oracle.Runtime.Internal.Coordination (newRuntimeCoordination, observeProposalStatus, registerProposal)
import Eclips.Oracle.Runtime.Internal.Hello (activeHeraldIdentities)
import Eclips.Oracle.Runtime.Internal.Scope (newRuntimeScope)
import Eclips.Oracle.Runtime.Internal.Types
import Eclips.Oracle.State (OracleState, oracleCheckedMembershipHistory, oracleCurrentMembership, oracleGreatestControlIndex, oracleHeraldCatalogue, oracleReceiptRetirementProgresses, oracleRetiredHeralds, oracleTerminalOutcome)
import Eclips.Oracle.Transition (initialOracle, oracleSubmissionPreflightBlocker, stepOracle)
import Eclips.Oracle.Voter (oracleReplicaRegistrations, oracleVoterConfiguration)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Configuration (genesisRaftConfigurationRef, raftVoterSet, stableRaftConfiguration)
import Eclips.Raft.Effect (RaftCommittedEntry, RaftEffect (ExposeCommittedEntries), RaftProposalStatus (RaftProposalAppended), committedEntryIndex, raftEffectBatchEffects)
import Eclips.Raft.Genesis (checkRaftGenesis, raftGenesis)
import Eclips.Raft.Identity (mkRaftDurationMicros, mkRaftProposalId, raftLogIndex, raftTerm)
import Eclips.Raft.Input (acknowledgeCommittedEntries, fireElectionTimer, proposeRaftApplication, selectElectionTimeout)
import Eclips.Raft.State (RaftRole (RaftFollower), raftStateElectionGeneration)
import Eclips.Raft.Transition (initialRaft, stepRaft)
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import TestFixtures (checked, fixtureCheckedGenesis, fixtureHeraldEpoch, fixtureInitialLeaderNode, fixtureProcessEpoch, fixtureSystemId, identifierBytes, validOracleCommand)

tests :: TestTree
tests =
  testGroup
    "adapter published receipt retention"
    [ testCase "the running adapter releases obsolete retirement projections before a result query" caseColdPublishedReceipts,
      testCase "a committed busy decision returns deferral and the exact envelope succeeds after completion" caseCommittedDecisionDeferral
    ]

-- Both semantic preflights observe the free object slot. The committed adapter must
-- nevertheless defer the second proposal against the state installed by the
-- first; no preflight observation can authorize a durable busy rejection.
caseCommittedDecisionDeferral :: Assertion
caseCommittedDecisionDeferral = do
  completed <- timeout 10_000_000 $ do
    commands <- newTQueueIO
    oracleCommands <- newTQueueIO
    watchCommands <- newTQueueIO
    raftCommands <- newTQueueIO
    witnesses <- newTVarIO []
    status <- newTVarIO initialStatus
    coordination <- atomically newRuntimeCoordination
    scope <- atomically newRuntimeScope
    withWorkers
      [ oraclePeer oracleCommands witnesses initialOracleState,
        watchPeer watchCommands status,
        raftPeer raftCommands,
        runCommittedEntryAdapter (const (pure ())) commands oracleCommands watchCommands raftCommands status coordination scope
      ]
      $ do
        forM_ [firstDecisionEnvelope, secondDecisionEnvelope] $ \canonical -> do
          result <- newEmptyTMVarIO
          atomically (writeTQueue oracleCommands (PreflightOracleOwner canonical result))
          observed <- atomically (takeTMVar result)
          assertEqual "both preflights observed a free initial slot" (OracleSubmissionMayProceed (controlIndex 0)) observed
        let batches = committedEnvelopes [firstDecisionEnvelope, secondDecisionEnvelope, completionEnvelope, secondDecisionEnvelope, secondDecisionEnvelope]
            applyBatch entries = do
              done <- newEmptyTMVarIO
              atomically (writeTQueue commands (ApplyCommittedRaftEntries entries done))
              observed <- atomically (takeTMVar done)
              assertEqual "the sealed committed batch is acknowledged" (Right ()) observed
            applyProposal entries = do
              result <- atomically $ do
                (proposal, waiter) <- registerProposal coordination
                observeProposalStatus coordination proposal (RaftProposalAppended (committedEntryIndex (NonEmpty.last entries)))
                pure waiter
              applyBatch entries
              atomically (takeTMVar result)
        case batches of
          [noOp, firstBatch, busyBatch, completionBatch, retryBatch, duplicateBatch] -> do
            applyBatch noOp
            acceptedFirst <- applyProposal firstBatch
            assertAccepted "first decision" 1 1 acceptedFirst
            deferred <- applyProposal busyBatch
            assertEqual
              "the committed second proposal returns a transient disposition"
              (OracleSubmitDeferred secondRequest firstDecision (controlIndex 1))
              deferred
            queryResult <- newEmptyTMVarIO
            atomically (writeTQueue commands (QueryPublishedOracleResult secondRequest queryResult))
            (receipt, retirement, published, _) <- atomically (takeTMVar queryResult)
            assertEqual "a busy decision creates no published receipt" Nothing receipt
            assertEqual "a busy decision is not retired" Nothing retirement
            assertEqual "a busy decision creates no Oracle control entry" (controlIndex 1) published.statusAppliedControlIndex
            completion <- applyProposal completionBatch
            assertAccepted "first completion" 3 2 completion
            acceptedSecond <- applyProposal retryBatch
            assertAccepted "the unchanged second envelope" 2 3 acceptedSecond
            duplicate <- applyProposal duplicateBatch
            assertEqual "exact retry retains the eventually accepted receipt" acceptedSecond duplicate
          _ -> assertFailure "decision fixture did not produce its sealed native batches"
  case completed of
    Just () -> pure ()
    Nothing -> assertFailure "adapter decision deferral fixture did not complete"
  where
    firstRequest = oracleClientRequestId fixtureHeraldEpoch 1
    secondRequest = oracleClientRequestId fixtureHeraldEpoch 2
    firstDecision = deriveLabelDecisionId fixtureSystemId firstRequest
    decisionEnvelope request objectByte =
      let object = checked "adapter decision object" (mkGlobalObjectId (identifierBytes objectByte))
       in canonicalizeOracleEnvelope
            ( oracleEnvelope
                request
                Nothing
                fixtureHeraldEpoch
                ( decideLabelCommand
                    (deriveLabelDecisionId fixtureSystemId request)
                    fixtureProcessEpoch
                    object
                    (ProcessLabel fixtureProcessEpoch, 0)
                    (Just (initialBootstrapLabelEvidence object fixtureProcessEpoch))
                    Nothing
                    Nothing
                    targetVoid
                    (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix)
                )
            )
    firstDecisionEnvelope = decisionEnvelope firstRequest 240
    secondDecisionEnvelope = decisionEnvelope secondRequest 240
    firstState = let (state, _, _) = checked "first decision" (stepOracle (canonicalOracleEnvelopeValue firstDecisionEnvelope) initialOracleState) in state
    terminal = maybe (error "first decision has no terminal outcome") id (oracleTerminalOutcome firstDecision firstState)
    completionEnvelope =
      canonicalizeOracleEnvelope
        ( oracleEnvelope
            (oracleClientRequestId fixtureHeraldEpoch 3)
            Nothing
            fixtureHeraldEpoch
            (completeLabelDecisionCommand (labelCompletionAttestation firstDecision (controlIndex 1) (deriveLabelOutcomeDigest terminal) fixtureHeraldEpoch (heraldMembershipGenerationId (oracleCurrentMembership firstState))))
        )
    assertAccepted label sequenceNumber index = \case
      OracleSubmitReceipt receipt -> do
        assertEqual (label <> " preserves the request identity") (oracleClientRequestId fixtureHeraldEpoch sequenceNumber) (oracleReceiptRequestId receipt)
        assertEqual (label <> " has the expected control coordinate") (controlIndex index) (oracleReceiptControlIndex receipt)
        assertEqual (label <> " is accepted") OracleAccepted (oracleReceiptResult receipt)
      other -> assertFailure (label <> " returned " <> show other)

-- Exercise the real serialized adapter, not a second implementation of its
-- published map. Peer owners acknowledge its ordinary queue protocol. The
-- Oracle peer executes the real kernel and weakly witnesses each concrete
-- retention projection handed to the adapter. No result query or map audit is
-- allowed before collection: those would demand the cold map and hide a leak.
caseColdPublishedReceipts :: Assertion
caseColdPublishedReceipts = do
  completed <- timeout 10_000_000 $ do
    commands <- newTQueueIO
    oracleCommands <- newTQueueIO
    watchCommands <- newTQueueIO
    raftCommands <- newTQueueIO
    witnesses <- newTVarIO []
    status <- newTVarIO initialStatus
    coordination <- atomically newRuntimeCoordination
    scope <- atomically newRuntimeScope
    withWorkers
      [ oraclePeer oracleCommands witnesses initialOracleState,
        watchPeer watchCommands status,
        raftPeer raftCommands,
        runCommittedEntryAdapter (const (pure ())) commands oracleCommands watchCommands raftCommands status coordination scope
      ]
      $ do
        forM_ (committedHistory 64) $ \entries -> do
          result <- newEmptyTMVarIO
          atomically (writeTQueue commands (ApplyCommittedRaftEntries entries result))
          observed <- atomically (takeTMVar result)
          assertEqual "each committed batch completed" (Right ()) observed
        barrier <- newEmptyTMVarIO
        atomically (writeTQueue commands (BarrierAdapter barrier))
        atomically (takeTMVar barrier)
        newestFirst <- readTVarIO witnesses
        assertEqual "every application generated a retention witness" 64 (length newestFirst)
        performGC
        performGC
        obsolete <- catMaybes <$> traverse deRefWeak (drop 1 newestFirst)
        assertEqual "old projections are collectible before the first receipt lookup" 0 (length obsolete)
        -- Queries come after the liveness assertion and verify that forcing
        -- publication has preserved both the current receipt and retirement.
        current <- query commands 64
        assertEqual "current request remains published" (Just (oracleClientRequestId fixtureHeraldEpoch 64)) (fmap oracleReceiptRequestId (first current))
        old <- query commands 1
        assertEqual "the first receipt was reclaimed" Nothing (first old)
        assertEqual "the old request has its actual retirement classification" (Just (OracleRequestPrefixRetired 63)) (second old)
  case completed of
    Just () -> pure ()
    Nothing -> assertFailure "adapter retention fixture did not complete"
  where
    query commands sequenceNumber = do
      result <- newEmptyTMVarIO
      atomically (writeTQueue commands (QueryPublishedOracleResult (oracleClientRequestId fixtureHeraldEpoch sequenceNumber) result))
      atomically (takeTMVar result)
    first (value, _, _, _) = value
    second (_, value, _, _) = value

-- Keep allocation identity at the real queue handoff. Each distinct committed
-- retirement frontier yields a fresh record; the weak reference itself cannot
-- retain its projection or the Oracle state from which it was prepared.
{-# NOINLINE witnessedRetention #-}
witnessedRetention :: TVar [Weak OracleReceiptRetention] -> OracleState -> IO OracleReceiptRetention
witnessedRetention witnesses state = do
  retention <- evaluate (OracleReceiptRetention (Map.fromList (oracleReceiptRetirementProgresses state)) (Set.fromList (oracleRetiredHeralds state)))
  witness <- mkWeakPtr retention Nothing
  atomically (modifyTVar' witnesses (witness :))
  pure retention

oraclePeer :: TQueue OracleOwnerCommand -> TVar [Weak OracleReceiptRetention] -> OracleState -> IO ()
oraclePeer commands witnesses = loop
  where
    loop state = do
      command <- atomically (readTQueue commands)
      case command of
        ApplyOracleOwner canonical result -> do
          let (successor, outcome, effects) = checked "adapter peer Oracle step" (stepOracle (canonicalOracleEnvelopeValue canonical) state)
          retention <- witnessedRetention witnesses successor
          atomically (putTMVar result (Right (OracleOwnerResult outcome effects retention)))
          loop successor
        CaptureOracleCheckpointOwner result -> do
          let (_, captured, _) = captureOracleCheckpoint (oracleGreatestControlIndex state) ByteString.empty emptyOracleCheckpointCache
          atomically (putTMVar result captured)
          loop state
        PreflightOracleOwner canonical result -> do
          let prefix = oracleGreatestControlIndex state
              preflight = case oracleSubmissionPreflightBlocker (canonicalOracleEnvelopeValue canonical) state of
                Nothing -> OracleSubmissionMayProceed prefix
                Just decision -> OracleSubmissionMustDefer decision prefix
          atomically (putTMVar result preflight)
          loop state
        _ -> error "unexpected Oracle command in adapter retention fixture"

watchPeer :: TQueue WatchLedgerCommand -> TVar OracleRuntimeStatus -> IO ()
watchPeer commands status = loop
  where
    loop = do
      command <- atomically (readTQueue commands)
      case command of
        InsertWatchEntry _ result -> atomically (putTMVar result (Right ()))
        PublishWatchThrough index result -> atomically $ do
          modifyTVar' status (\current -> current {statusAppliedControlIndex = index})
          putTMVar result (Right ())
        _ -> error "unexpected watch command in adapter retention fixture"
      loop

raftPeer :: TQueue RaftOwnerCommand -> IO ()
raftPeer commands = loop
  where
    loop = do
      command <- atomically (readTQueue commands)
      case command of
        StepRaftOwner _ _ -> pure ()
        BarrierRaftOwner result -> atomically (putTMVar result ())
        _ -> error "unexpected Raft command in adapter retention fixture"
      loop

withWorkers :: [IO ()] -> IO value -> IO value
withWorkers actions action = bracket (traverse start actions) (mapM_ stop) (const action)
  where
    start work = do
      done <- newEmptyMVar
      thread <- forkFinally work (putMVar done)
      pure (thread, done)
    stop (thread, done) = killThread thread >> void (readMVar done)

-- Native sealed entries are produced through the public Raft kernel. The
-- first batch is its leader no-op; subsequent envelopes advance retirement
-- and keep only the newest request live at the adapter's public-result gate.
committedHistory :: Word64 -> [NonEmpty.NonEmpty (RaftCommittedEntry ByteString.ByteString)]
committedHistory count =
  committedEnvelopes
    [ canonicalizeOracleEnvelope
        ( oracleEnvelopeWithReceiptRetirementProgress
            (checked "adapter receipt retirement" (Lifetime.receiptRetirement (Just (number - 1)) Set.empty))
            (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch number) Nothing fixtureHeraldEpoch validOracleCommand)
        )
    | number <- [1 .. count]
    ]

committedEnvelopes :: [CanonicalOracleEnvelope] -> [NonEmpty.NonEmpty (RaftCommittedEntry ByteString.ByteString)]
committedEnvelopes envelopes =
  exposed electionEffects : append 1 envelopes applied
  where
    node = fixtureInitialLeaderNode
    duration amount = checked "adapter Raft duration" (mkRaftDurationMicros amount)
    genesis = checked "adapter singleton Raft" (checkRaftGenesis (raftGenesis node [node] (duration 20_000) (duration 60_000) (duration 120_000)))
    initial = initialRaft genesis
    generation = raftStateElectionGeneration initial
    selected = fst (checked "adapter election selection" (stepRaft (selectElectionTimeout generation (duration 60_000)) initial))
    (leader, electionEffects) = checked "adapter election" (stepRaft (fireElectionTimer generation) selected)
    applied = fst (checked "adapter no-op acknowledgement" (stepRaft (acknowledgeCommittedEntries (raftLogIndex 1)) leader))
    append _ [] _ = []
    append number (envelope : remaining) state =
      let bytes = canonicalOracleEnvelopeBytes envelope
          proposal = checked "adapter proposal" (mkRaftProposalId number)
          (appended, effects) = checked "adapter proposal commit" (stepRaft (proposeRaftApplication proposal bytes) state)
          successor = fst (checked "adapter application acknowledgement" (stepRaft (acknowledgeCommittedEntries (raftLogIndex (number + 1))) appended))
       in exposed effects : append (number + 1) remaining successor
    exposed effects = case [entries | ExposeCommittedEntries entries <- raftEffectBatchEffects effects] of
      [entries] -> entries
      _ -> error "adapter fixture expected one committed batch"

initialOracleState :: OracleState
initialOracleState = checked "adapter initial Oracle" (initialOracle fixtureCheckedGenesis)

initialStatus :: OracleRuntimeStatus
initialStatus =
  OracleRuntimeStatus
    { statusNode = fixtureInitialLeaderNode,
      statusRole = RaftFollower,
      statusTerm = raftTerm 0,
      statusLeaderHint = Nothing,
      statusServiceReady = False,
      statusAppliedControlIndex = controlIndex 0,
      statusLabelRetiredThrough = controlIndex 0,
      statusCompletedWorkflowCount = 0,
      statusAppliedRaftIndex = raftLogIndex 0,
      statusVoterConfiguration = oracleVoterConfiguration initialOracleState,
      statusReplicaRegistrations = oracleReplicaRegistrations initialOracleState,
      statusPendingVoterChange = Nothing,
      statusConfigurationFrontier = Nothing,
      statusEffectiveConfiguration = (genesisRaftConfigurationRef, stableRaftConfiguration (checked "adapter status voter set" (raftVoterSet [fixtureInitialLeaderNode]))),
      statusConfigurationGeneration = 0,
      statusMembershipHistory = oracleCheckedMembershipHistory initialOracleState,
      statusHeraldCatalogue = oracleHeraldCatalogue initialOracleState,
      statusActiveHeralds = activeHeraldIdentities (oracleHeraldCatalogue initialOracleState) (oracleCheckedMembershipHistory initialOracleState),
      statusAccepting = True
    }
