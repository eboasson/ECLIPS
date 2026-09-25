module TraceProperties
  ( tests,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( atomically,
    orElse,
    retry,
  )
import Control.Exception (bracket, evaluate, throwIO)
import Control.Monad (forM, forM_, void)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, mkWeakIORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Unique (newUnique)
import Data.Word (Word64)
import Eclips.Herald.Discovery (PeerHelloDisposition (..), peerCandidateConnectionNonce)
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (..),
    candidateApplicationLane,
    heraldInput,
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (..),
    OracleClientDiagnostic (..),
    OracleClientIngress (..),
  )
import Eclips.Herald.Peer.RPC (EstablishedPeerInbound (..), admitEstablishedPeerEnvelope)
import Eclips.Herald.Runtime
  ( configureHeraldRuntimePeerDialSink,
    configureHeraldRuntimeWorkCountsSink,
    heraldRuntimeConfiguration,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers, runtimeApplicationConnectionHandlers)
import Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (..),
    SourceId (..),
  )
import Eclips.Herald.Runtime.Internal.Connection
  ( ConnectionSlot,
    allocateApplication,
    newRegistry,
    removeCurrentSlot,
    slotSource,
  )
import Eclips.Herald.Runtime.Internal.Coordination (BatchOrdinal (..), EffectMemberOrdinal (..), StepOrdinal (..))
import Eclips.Herald.Runtime.Internal.Owner (deduplicateDiscardedWorkTickets)
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    PhysicalConnectionRef (..),
    PhysicalLaneClass (..),
    RuntimeEventOrdinal (..),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    TraceCaptureMode (..),
    TraceLedger,
    addTraceWorkCounts,
    appendKernelTrace,
    appendShellTrace,
    newTraceLedger,
    newTraceLedgerWithMode,
    newTraceLedgerWithWorkCounts,
    snapshotTraceLedger,
    snapshotTraceLedgerSince,
    snapshotTraceWorkCounts,
    traceLedgerCapturesHistory,
    traceLedgerCursor,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( ConnectionRef (..),
    RuntimeLaneOffer (..),
    RuntimeToken (..),
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (..),
    RuntimeDiagnosticClass (..),
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Protocol.Peer.Types qualified as Peer
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedSource,
    fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixtureOracleContacts,
    fixturePeerCandidate,
    fixturePeerHello,
    fixturePeerRecoveryConfiguration,
  )
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    ioProperty,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "exact typed trace ledger"
    [ testCase "kernel and shell writers share one contiguous causal ordinal" caseContiguousOrdinals,
      testCase "a rolled-back append consumes neither event nor ordinal" caseRollback,
      testCase "trace cursors return each chronological suffix once" caseTraceCursor,
      testCase "explicit capture retains the exact mixed event payloads" caseExplicitCapture,
      testCase "physical trace identity releases the closed connection owner before inspection" casePhysicalTraceOwnerRetention,
      testCase "ordinal-only appends do not evaluate discarded payloads" caseOrdinalOnlyPayload,
      testCase "both modes roll back the shared ordinal transactionally" caseModeRollback,
      testCase "both modes serialize concurrent kernel and shell ordinals" caseConcurrentModes,
      testCase "a drain cut suppresses the delayed, dormant, and queued ticket union once" caseDrainCutTicketUnion,
      testCase "work counts roll back with their trace events" caseWorkCountsRollback,
      testCase "owner diagnostic counts are opt-in and roll back atomically" caseOwnerWorkCounts,
      testCase "enabled work counts inspect constructors without forcing payloads" caseWorkCountsPayload,
      testCase "delivery headers preserve semantic work categories without counting progress twice" casePeerDeliveryWorkCounts,
      testCase "explicit work-count sink receives one joined snapshot before scope completion" caseWorkCountsSink,
      testProperty "generated event runs retain append order" propAppendOrder,
      testProperty "capture choice preserves generated ordinals and cursors" propModeOrdinalsAndCursors,
      testProperty "aggregate work counts reconcile with exact trace replay without retaining history" propWorkCountsReplay
    ]

caseContiguousOrdinals :: Assertion
caseContiguousOrdinals = do
  ledger <- atomically newTraceLedger
  token <- RuntimeToken <$> newUnique
  let reference = PhysicalApplicationRef (ConnectionRef token 7)
  first <- atomically (appendShellTrace ledger (ShellConnectionRegistered reference sourceA))
  second <- atomically (appendShellTrace ledger (ShellDiagnosticEmitted RuntimeSuppressionDiagnostic))
  third <- atomically (appendShellTrace ledger (ShellConnectionClosed reference sourceA))
  events <- atomically (snapshotTraceLedger ledger)
  assertEqual "returned ordinals" [ordinal 0, ordinal 1, ordinal 2] [first, second, third]
  assertEqual "snapshot is chronological" [ordinal 0, ordinal 1, ordinal 2] (fmap eventOrdinal events)

caseRollback :: Assertion
caseRollback = do
  ledger <- atomically newTraceLedger
  atomically
    ( (appendShellTrace ledger ShellDiagnosticDisabled >> retry)
        `orElse` pure ()
    )
  first <- atomically (appendShellTrace ledger (ShellRuntimeExited HeraldRuntimeScopeClosed))
  events <- atomically (snapshotTraceLedger ledger)
  assertEqual "the first committed event keeps ordinal zero" (ordinal 0) first
  assertEqual "the rolled-back event is absent" [ordinal 0] (fmap eventOrdinal events)

caseTraceCursor :: Assertion
caseTraceCursor = do
  ledger <- atomically newTraceLedger
  initial <- atomically (traceLedgerCursor ledger)
  _ <- atomically (appendShellTrace ledger ShellDiagnosticDisabled)
  _ <- atomically (appendShellTrace ledger (ShellRuntimeExited HeraldRuntimeScopeClosed))
  (afterFirst, firstSuffix) <- atomically (snapshotTraceLedgerSince initial ledger)
  _ <- atomically (appendShellTrace ledger (ShellDiagnosticEmitted RuntimeConnectionDiagnostic))
  (afterSecond, secondSuffix) <- atomically (snapshotTraceLedgerSince afterFirst ledger)
  (_, emptySuffix) <- atomically (snapshotTraceLedgerSince afterSecond ledger)
  assertEqual "first suffix is chronological" [ordinal 0, ordinal 1] (fmap eventOrdinal firstSuffix)
  assertEqual "second suffix contains only the next event" [ordinal 2] (fmap eventOrdinal secondSuffix)
  assertEqual "an unchanged cursor has an empty suffix" [] emptySuffix

caseExplicitCapture :: Assertion
caseExplicitCapture = do
  ledger <- atomically (newTraceLedgerWithMode CaptureExactHistory)
  let initialized = KernelTraceInitialized (BatchOrdinal 9) mempty
      diagnostic = ShellDiagnosticEmitted RuntimeConnectionDiagnostic
  _ <- atomically (appendKernelTrace ledger initialized)
  _ <- atomically (appendShellTrace ledger diagnostic)
  assertEqual "the explicit mode retains history" True (traceLedgerCapturesHistory ledger)
  assertEqual
    "both exact typed payloads remain available"
    [KernelEvent (ordinal 0) initialized, ShellEvent (ordinal 1) diagnostic]
    =<< atomically (snapshotTraceLedger ledger)

-- Exercise the same slotSource projection used by the runtime's connection
-- close trace. Its identity is required by exact replay; its old writer queue
-- and handler resource are not. Collect before reading the event, since an
-- inspection would force the identity and conceal a retained selector closure.
{-# NOINLINE captureClosedConnectionOwner #-}
captureClosedConnectionOwner :: TraceLedger -> IO (Weak ConnectionSlot, Weak (IORef Int))
captureClosedConnectionOwner ledger = do
  token <- RuntimeToken <$> newUnique
  registry <- atomically (newRegistry token (heraldRuntimeHandlers (const (pure ()))))
  resource <- newIORef (37 :: Int)
  resourceWitness <- mkWeakIORef resource (pure ())
  let handlers = runtimeApplicationConnectionHandlers (\_ -> readIORef resource >>= evaluate >> pure LaneOffered) (pure ())
  (reference, supplied) <- atomically (allocateApplication registry handlers)
  slot <- evaluate supplied
  slotWitness <- mkWeakPtr slot Nothing
  -- The physical reference is already used for removing the registry entry.
  -- The source projection exists solely for the retained diagnostic event.
  atomically $ do
    void (removeCurrentSlot registry reference)
    void (appendShellTrace ledger (ShellConnectionClosed (PhysicalApplicationRef reference) (slotSource slot)))
  pure (slotWitness, resourceWitness)

casePhysicalTraceOwnerRetention :: Assertion
casePhysicalTraceOwnerRetention = do
  ledger <- atomically newTraceLedger
  witnesses <- forM [1 .. 16 :: Int] (const (captureClosedConnectionOwner ledger))
  bracket (newStablePtr ledger) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses $ \(slot, resource) -> do
      slotAlive <- maybe False (const True) <$> deRefWeak slot
      resourceAlive <- maybe False (const True) <$> deRefWeak resource
      pure (slotAlive, resourceAlive)
    assertEqual "exact identities do not retain old slots or handler resources" (replicate 16 (False, False)) alive
    retained <- deRefStablePtr root
    events <- atomically (snapshotTraceLedger retained)
    let identities =
          [ (eventIndex, connectionOrdinal, source)
          | ShellEvent eventIndex (ShellConnectionClosed (PhysicalApplicationRef (ConnectionRef _ connectionOrdinal)) source) <- events
          ]
    assertEqual
      "all exact close identities remain available after the owners are collected"
      [(ordinal index, 0, SourceId ApplicationSources 0) | index <- [0 .. 15]]
      identities

caseOrdinalOnlyPayload :: Assertion
caseOrdinalOnlyPayload = do
  ledger <- atomically (newTraceLedgerWithMode OrdinalsOnly)
  initial <- atomically (traceLedgerCursor ledger)
  first <- atomically (appendKernelTrace ledger (error "discarded kernel payload was evaluated"))
  second <- atomically (appendShellTrace ledger (error "discarded shell payload was evaluated"))
  (after, suffix) <- atomically (snapshotTraceLedgerSince initial ledger)
  reference <- atomically newTraceLedger
  _ <- atomically (appendShellTrace reference ShellDiagnosticDisabled)
  _ <- atomically (appendShellTrace reference ShellDiagnosticDisabled)
  expectedCursor <- atomically (traceLedgerCursor reference)
  assertEqual "the ledger has no event storage" False (traceLedgerCapturesHistory ledger)
  assertEqual "discarded payloads still receive ordinals" [ordinal 0, ordinal 1] [first, second]
  assertEqual "the cursor advances past discarded events" expectedCursor after
  assertEqual "the suffix contains no discarded event" [] suffix
  assertEqual "the complete snapshot is empty" [] =<< atomically (snapshotTraceLedger ledger)
  assertEqual "disabled work counts allocate no retained map" Nothing =<< atomically (snapshotTraceWorkCounts ledger)

caseWorkCountsRollback :: Assertion
caseWorkCountsRollback = forM_ [CaptureExactHistory, OrdinalsOnly] $ \mode -> do
  ledger <- atomically (newTraceLedgerWithWorkCounts mode)
  atomically
    $ (appendKernelTrace ledger (KernelTraceInitialized (BatchOrdinal 0) mempty) >> retry)
      `orElse` pure ()
  assertEqual "an aborted initialization increments no counter" (Just Map.empty) =<< atomically (snapshotTraceWorkCounts ledger)
  _ <- atomically (appendShellTrace ledger ShellDiagnosticDisabled)
  assertEqual
    "only the committed constructor is counted"
    (Just (Map.singleton "shell.event.ShellDiagnosticDisabled" 1))
    =<< atomically (snapshotTraceWorkCounts ledger)

caseWorkCountsSink :: Assertion
caseWorkCountsSink = do
  snapshots <- newIORef []
  let configuration =
        configureHeraldRuntimePeerDialSink (const (pure ()))
          $ configureHeraldRuntimeWorkCountsSink (\complete counts -> modifyIORef' snapshots ((complete, counts) :))
          $ heraldRuntimeConfiguration
            fixtureCheckedGenesis
            fixtureCheckedBootstraps
            fixtureOracleContacts
            fixtureGeneratorSeedSource
            (runtimeMonotonicClock (pure (monotonicInstant 0)))
            (runtimePeerWorkDelayMicroseconds 0)
            (heraldRuntimeHandlers (const (pure ())))
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
      byPrefix prefix = Map.fromList . fmap (\(key, value) -> (drop (length prefix) key, value)) . filter (isPrefixOf prefix . fst)
  result <- withHeraldRuntime configuration (const (pure ()))
  assertEqual "measurement preserves ordinary scope completion" (Right ((), HeraldRuntimeScopeClosed)) result
  readIORef snapshots >>= \case
    [(complete, counts)] -> do
      assertEqual "the final snapshot follows all worker joins" True complete
      assertEqual "the sink enabled the finite census" (Just 1) (lookup "kernel.initialized" counts)
      assertEqual "the private owner publishes one final alignment inventory" (Just 1) (lookup "alignment.inventory.snapshots" counts)
      assertEqual "explicit plan tables use the same single final-owner census" (Just 1) (lookup "alignment.plans.snapshots" counts)
      assertEqual "operational retention uses the same single final-owner census" (Just 1) (lookup "alignment.state.snapshots" counts)
      assertEqual "every started tracked worker was joined before handoff" (byPrefix "worker.started." counts) (byPrefix "worker.joined." counts)
    observed -> assertFailure ("expected one final census, observed " <> show observed)

caseOwnerWorkCounts :: Assertion
caseOwnerWorkCounts = do
  disabled <- atomically (newTraceLedgerWithMode OrdinalsOnly)
  atomically (addTraceWorkCounts disabled (error "disabled owner diagnostics were evaluated"))
  assertEqual "disabled owner diagnostics allocate no map" Nothing =<< atomically (snapshotTraceWorkCounts disabled)
  enabled <- atomically (newTraceLedgerWithWorkCounts OrdinalsOnly)
  before <- atomically (traceLedgerCursor enabled)
  atomically $ (addTraceWorkCounts enabled [("diagnostic.test", 9)] >> retry) `orElse` pure ()
  assertEqual "aborted owner diagnostics leave no partial counts" (Just Map.empty) =<< atomically (snapshotTraceWorkCounts enabled)
  atomically (addTraceWorkCounts enabled [("diagnostic.test", 2), ("diagnostic.test", 3)])
  assertEqual "weighted owner observations add exactly" (Just (Map.singleton "diagnostic.test" 5)) =<< atomically (snapshotTraceWorkCounts enabled)
  assertEqual "diagnostics do not invent trace events" before =<< atomically (traceLedgerCursor enabled)
  assertEqual "no diagnostic event payload is retained" [] =<< atomically (snapshotTraceLedger enabled)

casePeerDeliveryWorkCounts :: Assertion
casePeerDeliveryWorkCounts = do
  ledger <- atomically (newTraceLedgerWithWorkCounts OrdinalsOnly)
  let bytes = ByteString.replicate 32 0x31
      ready =
        Peer.ClassMemberReadyDto
          (checked (Peer.positiveAlignmentEvidenceSequenceDto 1))
          (checked (Peer.contextClassGenerationClaim bytes))
          (checked (Peer.storeIncarnationClaim bytes))
          (checked (Peer.memberReadyEvidenceDigestClaim bytes))
          (Peer.storeRevisionDto 0)
      envelope = Peer.PeerControlEnvelope mempty (Peer.AlignmentEvidenceDeliveredDto (checked (Peer.positiveAlignmentDeliverySequenceDto 1)) (Peer.AlignmentMemberReadyEvidenceDto ready))
  evidence <- case admitEstablishedPeerEnvelope envelope of
    Right (EstablishedPeerControl _ control) -> pure control
    observed -> assertFailure ("test evidence failed public admission: " <> show observed)
  let (initial, _) = checked (initialHerald (monotonicInstant 0) fixtureCheckedGenesis fixtureCheckedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
      hello = PeerHelloReceived fixturePeerCandidate mempty (fixturePeerHello (peerCandidateConnectionNonce fixturePeerCandidate)) fixtureMembershipGenerationId fixtureMemberSetDigest Nothing
      (_, accepted) = checked (stepHerald (heraldInput (monotonicInstant 0) (PeerInput hello)) initial)
  binding <- case [value | SetPeerCandidateDisposition _ (PeerHelloAccepted _ value) <- effectBatchMembers accepted] of
    [value] -> pure value
    values -> assertFailure ("test Hello did not produce one binding: " <> show values)
  let input = heraldInput (monotonicInstant 0) (PeerInput (PeerControlReceivedWithProgress binding mempty evidence))
      batch = orderedEffectBatch [SendPeerControlWithProgress binding mempty evidence, SendPeerControlWithProgress binding mempty PeerAlignmentDeliveryProgress]
  _ <- atomically (appendKernelTrace ledger (KernelTraceStepped (StepOrdinal 0) sourceA input (Right batch)))
  counts <- maybe Map.empty id <$> atomically (snapshotTraceWorkCounts ledger)
  let observed key = Map.findWithDefault 0 key counts
  assertEqual "both physical control effects counted once" 2 (observed "effect.emitted.total")
  assertEqual "the sequenced member-ready keeps its semantic category" 1 (observed "effect.emitted.SendPeerControlWithProgress.PeerAlignmentControl.AlignmentMemberReadyAdvertised")
  assertEqual "incoming evidence keeps its semantic category" 1 (observed "kernel.input.PeerInput.PeerControlReceivedWithProgress.PeerAlignmentControl.AlignmentMemberReadyAdvertised")
  assertEqual "progress-only frame is counted once" 1 (observed "effect.emitted.SendPeerControlWithProgress.PeerAlignmentDeliveryProgress")
  assertEqual "both explicit progress headers are visible" 2 (observed "effect.emitted.SendPeerControlWithProgress.ReceiptProgress")
  where
    checked :: (Show problem) => Either problem value -> value
    checked = either (error . show) id

caseWorkCountsPayload :: Assertion
caseWorkCountsPayload = do
  ledger <- atomically (newTraceLedgerWithWorkCounts OrdinalsOnly)
  let input = heraldInput (monotonicInstant 0) (JoinInput 7 (error "counting forced join payload"))
      batch = orderedEffectBatch [SendJoinReply 7 (error "counting forced reply payload")]
  _ <- atomically (appendKernelTrace ledger (KernelTraceStepped (StepOrdinal 0) sourceA input (Right batch)))
  _ <- atomically (appendKernelTrace ledger (KernelTraceStepped (StepOrdinal 1) sourceA input (Left (error "counting forced invariant fault payload"))))
  assertEqual
    "constructor categories distinguish emitted work and failed steps"
    ( Just
        ( Map.fromList
            [ ("kernel.input.total", 2),
              ("kernel.source.ApplicationSources", 2),
              ("kernel.input.JoinInput", 2),
              ("kernel.result.success", 1),
              ("kernel.result.fault", 1),
              ("effect.emitted.total", 1),
              ("effect.emitted.SendJoinReply", 1)
            ]
        )
    )
    =<< atomically (snapshotTraceWorkCounts ledger)
  assertEqual "aggregate capture retains no event history" [] =<< atomically (snapshotTraceLedger ledger)

caseModeRollback :: Assertion
caseModeRollback = forM_ [CaptureExactHistory, OrdinalsOnly] $ \mode -> do
  ledger <- atomically (newTraceLedgerWithMode mode)
  initial <- atomically (traceLedgerCursor ledger)
  atomically
    $ (appendKernelTrace ledger (KernelTraceInitialized (BatchOrdinal 4) mempty) >> retry)
      `orElse` pure ()
  assertEqual "rollback preserves the cursor" initial =<< atomically (traceLedgerCursor ledger)
  first <- atomically (appendShellTrace ledger ShellDiagnosticDisabled)
  assertEqual "rollback preserves the next ordinal" (ordinal 0) first
  let expected = case mode of
        CaptureExactHistory -> [ShellEvent (ordinal 0) ShellDiagnosticDisabled]
        OrdinalsOnly -> []
  assertEqual "rollback retains no aborted event" expected =<< atomically (snapshotTraceLedger ledger)

caseConcurrentModes :: Assertion
caseConcurrentModes = forM_ [CaptureExactHistory, OrdinalsOnly] $ \mode -> do
  ledger <- atomically (newTraceLedgerWithMode mode)
  workers <- forM [0 .. 3 :: Int] $ \worker -> do
    completed <- newEmptyMVar
    _ <-
      forkFinally
        ( forM [0 .. 24 :: Int] $ \index ->
            atomically
              $ if even worker
                then appendKernelTrace ledger (KernelTraceInitialized (BatchOrdinal (fromIntegral index)) mempty)
                else appendShellTrace ledger (fixtureEvent index)
        )
        (putMVar completed)
    pure completed
  returned <- concat <$> mapM (\completed -> takeMVar completed >>= either throwIO pure) workers
  events <- atomically (snapshotTraceLedger ledger)
  assertEqual "all writers share one committed ordinal sequence" (expectedOrdinals 100) (sort returned)
  let retainedOrdinals = case mode of
        CaptureExactHistory -> expectedOrdinals 100
        OrdinalsOnly -> []
  assertEqual "capture preserves commit order" retainedOrdinals (fmap eventOrdinal events)

caseDrainCutTicketUnion :: Assertion
caseDrainCutTicketUnion = do
  let first = 1 :: Int
      second = 2
      third = 3
  assertEqual
    "one ticket retained in several physical queues has one cut witness"
    [first, second, third]
    (deduplicateDiscardedWorkTickets [[first, second], [first, third], [second, third]])

propAppendOrder :: NonNegative Int -> Property
propAppendOrder (NonNegative rawCount) =
  ioProperty $ do
    let count = rawCount `mod` 501
    ledger <- atomically newTraceLedger
    returned <-
      mapM
        (\index -> atomically (appendShellTrace ledger (fixtureEvent index)))
        [0 .. count - 1]
    events <- atomically (snapshotTraceLedger ledger)
    pure $ (returned, fmap eventOrdinal events) === (expectedOrdinals count, expectedOrdinals count)

propModeOrdinalsAndCursors :: NonNegative Int -> Property
propModeOrdinalsAndCursors (NonNegative rawCount) = ioProperty $ do
  let count = rawCount `mod` 201
      split = count `div` 2
      run mode = do
        ledger <- atomically (newTraceLedgerWithMode mode)
        initial <- atomically (traceLedgerCursor ledger)
        before <- mapM (atomically . appendShellTrace ledger . fixtureEvent) [0 .. split - 1]
        (middle, prefix) <- atomically (snapshotTraceLedgerSince initial ledger)
        after <- mapM (atomically . appendShellTrace ledger . fixtureEvent) [split .. count - 1]
        (final, suffix) <- atomically (snapshotTraceLedgerSince middle ledger)
        (unchanged, emptySuffix) <- atomically (snapshotTraceLedgerSince final ledger)
        pure ((before <> after, initial, middle, final, unchanged), prefix, suffix, emptySuffix)
  (captured, prefix, suffix, emptyCaptured) <- run CaptureExactHistory
  (discarded, emptyPrefix, emptySuffix, emptyDiscarded) <- run OrdinalsOnly
  let expected = zipWith (\index payload -> ShellEvent (ordinal (fromIntegral index)) payload) [0 :: Int ..] (fmap fixtureEvent [0 .. count - 1])
  pure
    $ (discarded, prefix, suffix, emptyPrefix, emptySuffix, emptyCaptured, emptyDiscarded)
      === (captured, take split expected, drop split expected, [], [], [], [])

propWorkCountsReplay :: NonNegative Int -> Property
propWorkCountsReplay (NonNegative rawCount) = ioProperty $ do
  let count = rawCount `mod` 101
      oracleAction = RunOracleClientAction (ReportOracleClientDiagnostic StaleOracleClientIngress)
      makeBatch index =
        orderedEffectBatch
          ( SendJoinReply (fromIntegral index) (ByteString.replicate (index + 1) 0x61)
              : [oracleAction | index `mod` 3 == 0]
          )
      makeInput index =
        heraldInput (monotonicInstant (fromIntegral index))
          $ if even index
            then RuntimeObserved (ApplicationCandidateLost (candidateApplicationLane (fromIntegral index)))
            else OracleInput (OracleContactsDiscovered fixtureOracleContacts)
  token <- RuntimeToken <$> newUnique
  let reference = PhysicalApplicationRef (ConnectionRef token (fromIntegral count))
      appendRun ledger = do
        _ <- atomically (appendKernelTrace ledger (KernelTraceInitialized (BatchOrdinal 0) mempty))
        forM_ [0 .. count - 1] $ \index -> do
          let batchOrdinal = BatchOrdinal (fromIntegral (index + 1))
              batch = makeBatch index
              input = makeInput index
          _ <- atomically (appendKernelTrace ledger (KernelTraceStepped (StepOrdinal (fromIntegral index)) sourceA input (Right batch)))
          forM_ (zip [0 ..] (effectBatchMembers batch)) $ \(member, effect) ->
            atomically (appendShellTrace ledger (ShellEffectRouted batchOrdinal (EffectMemberOrdinal member) effect))
          _ <- atomically (appendShellTrace ledger (ShellLaneOffered reference ApplicationLane (if even index then LaneOffered else LaneLost)))
          pure ()
  captured <- atomically (newTraceLedgerWithWorkCounts CaptureExactHistory)
  aggregate <- atomically (newTraceLedgerWithWorkCounts OrdinalsOnly)
  appendRun captured
  appendRun aggregate
  events <- atomically (snapshotTraceLedger captured)
  captureCounts <- atomically (snapshotTraceWorkCounts captured)
  aggregateCounts <- atomically (snapshotTraceWorkCounts aggregate)
  assertEqual "aggregate-only counters match the complete captured execution" captureCounts aggregateCounts
  assertEqual "aggregate-only mode retains no history" [] =<< atomically (snapshotTraceLedger aggregate)
  let counts = maybe Map.empty id aggregateCounts
      observed key = Map.findWithDefault 0 key counts
      replayed predicate = fromIntegral (length (filter predicate events))
      emitted =
        fromIntegral
          ( sum
              [ length (effectBatchMembers batch)
              | event <- events,
                batch <- case event of
                  KernelEvent _ (KernelTraceInitialized _ initial) -> [initial]
                  KernelEvent _ (KernelTraceStepped _ _ _ (Right successor)) -> [successor]
                  _ -> []
              ]
          )
  assertEqual "kernel input count reconciles with trace replay" (replayed isStep) (observed "kernel.input.total")
  assertEqual "successful kernel results reconcile with trace replay" (replayed isStep) (observed "kernel.result.success")
  assertEqual "emitted effects reconcile with kernel batches" emitted (observed "effect.emitted.total")
  assertEqual "routed effects reconcile separately with shell interpretation" (replayed isRouted) (observed "effect.routed.total")
  assertEqual "accepted physical offers are not inferred from emitted effects" (replayed isOffered) (observed "lane.offer.ApplicationLane.LaneOffered")
  assertEqual "lost physical offers remain separate" (replayed isLost) (observed "lane.offer.ApplicationLane.LaneLost")
  assertEqual "Oracle callbacks retain their ingress kind" (replayed isOracleInput) (observed "kernel.input.OracleInput.OracleContactsDiscovered")
  assertEqual "routed Oracle actions retain their action kind" (replayed isOracleAction) (observed "effect.routed.RunOracleClientAction.ReportOracleClientDiagnostic")
  pure True
  where
    isStep (KernelEvent _ KernelTraceStepped {}) = True
    isStep _ = False
    isRouted (ShellEvent _ ShellEffectRouted {}) = True
    isRouted _ = False
    isOffered (ShellEvent _ (ShellLaneOffered _ _ LaneOffered)) = True
    isOffered _ = False
    isLost (ShellEvent _ (ShellLaneOffered _ _ LaneLost)) = True
    isLost _ = False
    isOracleInput (KernelEvent _ (KernelTraceStepped _ _ input _)) = case inputBody input of
      OracleInput OracleContactsDiscovered {} -> True
      _ -> False
    isOracleInput _ = False
    isOracleAction (ShellEvent _ (ShellEffectRouted _ _ (RunOracleClientAction ReportOracleClientDiagnostic {}))) = True
    isOracleAction _ = False

fixtureEvent :: Int -> ShellTraceEvent
fixtureEvent index = case index `mod` 4 of
  0 -> ShellStaleSuppressed Nothing (Just (SourceId ApplicationSources (fromIntegral index)))
  1 -> ShellDiagnosticEmitted RuntimeConnectionDiagnostic
  2 -> ShellStaleSuppressed Nothing (Just (SourceId PeerSources (fromIntegral index)))
  _ -> ShellStaleSuppressed Nothing (Just (SourceId RuntimeCompletionSources (fromIntegral index)))

eventOrdinal :: RuntimeTraceEvent -> RuntimeEventOrdinal
eventOrdinal event = case event of
  KernelEvent eventOrdinal' _ -> eventOrdinal'
  ShellEvent eventOrdinal' _ -> eventOrdinal'

expectedOrdinals :: Int -> [RuntimeEventOrdinal]
expectedOrdinals count = fmap (ordinal . fromIntegral) [0 .. count - 1]

ordinal :: Word64 -> RuntimeEventOrdinal
ordinal = RuntimeEventOrdinal

sourceA :: SourceId
sourceA = SourceId ApplicationSources 7
