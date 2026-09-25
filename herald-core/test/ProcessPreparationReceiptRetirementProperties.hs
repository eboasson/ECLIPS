{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessPreparationReceiptRetirementProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement)
import Eclips.Domain.Identity (heraldEpochBytes)
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
import Eclips.Herald.Input
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.ProcessPreparation.Protocol (RemotePreparationStatus (RemotePreparationPending))
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.ProcessPreparation qualified as Lifecycle
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Public.Types.ReceiptRetirement
import GenesisFixtures qualified as Fixtures
import ProcessPreparationProperties qualified as Local
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Property, ioProperty, testProperty)

tests :: TestTree
tests =
  testGroup
    "application lifecycle receipt retirement"
    [ testProperty "inclusive frontiers retire only the named session, including request zero" propSessionPrefix,
      testCase "an unresolved exception preserves pending work while releasing completed suffixes" casePendingPrefix,
      testCase "local Begin receipt retirement preserves its accepted child and later Start settlement" caseLocalBeginProvenance,
      testCase "remote Begin provenance survives receipt retirement and ordinary handoff updates" caseOutgoingBeginProvenance,
      testCase "old live-session calls and result queries are retired before lookup or conflict classification" caseRetiredIngress,
      testCase "session death drops observation waits and terminal recovery while preserving pending mutations" caseDeadSessionCleanup,
      testCase "own-End emits its final reply before reclaiming the dead session receipt" caseOwnEndFinalReply
    ]

propSessionPrefix :: Word8 -> Word8 -> Property
propSessionPrefix countSeed frontierSeed = ioProperty $ do
  Local.Fixture _ _ binding session attachment parent <- Local.fixture
  let count = fromIntegral (countSeed `mod` 32)
      through = fromIntegral frontierSeed `mod` (count + 1)
      (otherSession, _, otherBinding) = Session.initialSessionAllocation (Session.applicationSessionIdHeraldEpoch session) (Session.applicationSessionIdOrdinal session + 1)
      reference = preparationFor session 0
      command = AwaitPreparedChild reference
      result = LifecycleRejected LifecycleUnknownPreparation
      insert state (currentSession, currentBinding, ordinal) =
        Preparation.retainRequest (Preparation.lifecycleKey parent (Local.request currentSession ordinal)) command result currentSession currentBinding attachment state
  original <- checkedIO (foldM insert Preparation.emptyState [(currentSession, currentBinding, ordinal) | (currentSession, currentBinding) <- [(session, binding), (otherSession, otherBinding)], ordinal <- [0 .. count]])
  retired <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just through)) original)
  assertEqual "no frontier means no retirement" (Right original) (Preparation.retireLifecycleReceipts session mempty original)
  assertEqual "repeated or lower frontiers cannot restore receipts" (Right retired) (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 0)) retired)
  assertEqual "only the acknowledged session prefix is removed" (fromIntegral (2 * (count + 1) - (through + 1))) (length (Preparation.requestEntries retired))
  forM_ [0 .. count] $ \ordinal -> do
    let currentKey = Preparation.lifecycleKey parent (Local.request session ordinal)
        otherKey = Preparation.lifecycleKey parent (Local.request otherSession ordinal)
    assertEqual "named session uses inclusive frontier" (if ordinal <= through then Nothing else Just result) (Preparation.requestStatus <$> Preparation.lookupRequest currentKey retired)
    assertEqual "other session retains every exact result" (Preparation.lookupRequest otherKey original) (Preparation.lookupRequest otherKey retired)
  assertEqual "retained owner is valid" (Right ()) (Preparation.validateState retired)
  pure True

casePendingPrefix :: Assertion
casePendingPrefix = do
  Local.Fixture _ _ binding session attachment parent <- Local.fixture
  let low = Preparation.lifecycleKey parent (Local.request session 0)
      high = Preparation.lifecycleKey parent (Local.request session 1)
      command = BeginChild locator Local.emptySelection
  pending <- checkedIO (Preparation.retainRequest low command (LifecyclePending []) session binding attachment Preparation.emptyState)
  deferred <- checkedIO (Preparation.retainDeferredBegin low locator pending)
  original <- checkedIO (Preparation.retainRequest high command (LifecycleRejected LifecycleTargetUnavailable) session binding attachment deferred)
  assertEqual "one unresolved low request pins the completed suffix" (Left Preparation.PreparationReceiptRetirementNotReady) (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 1)) original)
  assertEqual "the pending selection and return locator remain owned" [(low, locator)] (Preparation.deferredBeginEntries original)
  assertEqual "no retirement frontier is inert with pending work" (Right original) (Preparation.retireLifecycleReceipts session mempty original)
  exceptions <- checkedIO (receiptRetirement (Just 1) (Set.singleton 0))
  reclaimed <- checkedIO (Preparation.retireLifecycleReceipts session exceptions original)
  assertEqual "completed suffix is released around the pending exception" [low] (map fst (Preparation.requestEntries reclaimed))
  assertEqual "deferred semantic work survives retirement" [(low, locator)] (Preparation.deferredBeginEntries reclaimed)
  completed <- checkedIO (Preparation.settleRequest low (LifecycleRejected LifecycleTargetUnavailable) (Preparation.removeDeferredBegin low reclaimed))
  retired <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 1)) completed)
  assertEqual "settlement allows the whole prefix to retire" [] (Preparation.requestEntries retired)
  assertEqual "retired owner validates" (Right ()) (Preparation.validateState retired)

caseLocalBeginProvenance :: Assertion
caseLocalBeginProvenance = do
  fixtureValue@(Local.Fixture _ oracleBinding _ session _ _) <- Local.fixture
  (accepted, effects, reference) <- Local.begin fixtureValue Local.emptySelection
  before <- require (Preparation.lookupPreparation reference (startupProcessPreparationState accepted))
  owner <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 1)) (startupProcessPreparationState accepted))
  let retired = replaceStartupProcessPreparationState owner accepted
  after <- require (Preparation.lookupPreparation reference owner)
  assertEqual "accepted Begin no longer retains its lifecycle receipt" [] (Preparation.requestEntries owner)
  assertEqual "child identity and Oracle Start intention survive" (Preparation.preparationStart before, Preparation.preparationStartReference before) (Preparation.preparationStart after, Preparation.preparationStartReference after)
  assertEqual "opaque retained origin satisfies full Herald invariants" (Right ()) (validateHeraldState retired)
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (started, _, _) <- Local.apply oracleBinding oracle effects retired
  (queried, _) <- Local.call fixtureValue 2 (AwaitPreparedChild reference) started
  result <- Local.status fixtureValue 2 queried
  assertBool "the retained child still finishes preparation" (case result of LifecycleCompleted (ChildPrepared child) -> preparedChildPreparation child == reference; _ -> False)
  assertEqual "later child progress preserves the checked origin" (Right ()) (validateHeraldState queried)

caseOutgoingBeginProvenance :: Assertion
caseOutgoingBeginProvenance = do
  Local.Fixture _ _ binding session attachment parent <- Local.fixture
  let request = Local.request session 0
      reference = preparationFor session 0
      key = Preparation.lifecycleKey parent request
      outgoing = Preparation.OutgoingPreparation parent (Session.applicationSessionIdHeraldEpoch session) locator "retained transfer" (RemotePreparationPending []) Nothing Nothing False Nothing Nothing
  receipt <- checkedIO (Preparation.retainRequest key (BeginChild locator Local.emptySelection) (LifecycleCompleted (ChildPreparationAccepted reference)) session binding attachment Preparation.emptyState)
  original <- checkedIO (Preparation.retainOutgoing reference outgoing receipt)
  assertEqual "outgoing begin has its exact live origin" (Right ()) (Preparation.validateState original)
  retired <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 0)) original)
  assertEqual "outgoing semantic payload remains exact" (Just outgoing) (Preparation.lookupOutgoing reference retired)
  assertEqual "retired Begin does not break outgoing provenance" (Right ()) (Preparation.validateState retired)
  updated <- checkedIO (Preparation.replaceOutgoing reference outgoing {Preparation.outgoingCancelled = True} retired)
  assertEqual "ordinary updates preserve privately admitted origin" (Right ()) (Preparation.validateState updated)
  assertEqual "updating semantic state does not restore a receipt" [] (Preparation.requestEntries updated)

caseRetiredIngress :: Assertion
caseRetiredIngress = do
  fixtureValue@(Local.Fixture initial _ binding session _ _) <- Local.fixture
  let request = Local.request session 0
      command = AwaitPreparedChild (preparationFor session 0)
  (rejected, _) <- Local.call fixtureValue 0 command initial
  owner <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 0)) (startupProcessPreparationState rejected))
  prepared <- checkedIO (Application.prepareApplicationReceiptRetirement binding session (applicationReceiptRetirement Nothing (Just 0)) (startupApplicationState rejected))
  let retired = replaceStartupApplicationState (Application.commitApplicationReceiptRetirement prepared) (replaceStartupProcessPreparationState owner rejected)
      expected = SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleRetired request 0)
  forM_
    [ GetApplicationLifecycleResult binding session request,
      CallApplicationLifecycle binding session request locator command,
      CallApplicationLifecycle binding session request locator EndOwnProcess
    ]
    $ \ingress -> do
      (successor, effects) <- checkedIO (Lifecycle.applyProcessPreparationIngress ingress retired)
      assertEqual "old query or conflicting command has the same retired disposition" [expected] (effectBatchMembers effects)
      assertEqual "old command cannot recreate a lifecycle receipt" owner (startupProcessPreparationState successor)
      assertEqual "old command cannot mint a new Oracle intent" (Client.oracleClientStateWitness (startupOracleClientState retired)) (Client.oracleClientStateWitness (startupOracleClientState successor))
  assertEqual "retired live-session state validates" (Right ()) (validateHeraldState retired)

caseDeadSessionCleanup :: Assertion
caseDeadSessionCleanup = do
  Local.Fixture initial _ binding session attachment parent <- Local.fixture
  let reference = preparationFor session 0
      key ordinal = Preparation.lifecycleKey parent (Local.request session ordinal)
      terminal = LifecycleRejected LifecycleUnknownPreparation
      ownEndResult = LifecycleRejected LifecycleTargetUnavailable
  first <- checkedIO (Preparation.retainRequest (key 0) (AwaitPreparedChild reference) terminal session binding attachment Preparation.emptyState)
  second <- checkedIO (Preparation.retainRequest (key 1) (AwaitChildReady reference) (LifecyclePending []) session binding attachment first)
  third <- checkedIO (Preparation.retainRequest (key 2) EndOwnProcess ownEndResult session binding attachment second)
  fourth <- checkedIO (Preparation.retainRequest (key 3) (AwaitPreparedChild reference) (LifecyclePending []) session binding attachment third)
  fifth <- checkedIO (Preparation.retainRequest (key 4) (BeginChild locator Local.emptySelection) (LifecyclePending []) session binding attachment fourth)
  deferred <- checkedIO (Preparation.retainDeferredBegin (key 4) locator fifth)
  original <- checkedIO (Preparation.retainRequest (key 5) (CancelChild reference) (LifecyclePending []) session binding attachment deferred)
  let lane = candidateApplicationLane 791
      recover state = Lifecycle.applyProcessPreparationIngress (RecoverApplicationLifecycleResult lane attachment (Local.request session 2)) state
  (_, beforeEffects) <- checkedIO (recover (replaceStartupProcessPreparationState original initial))
  assertEqual "restricted recovery works while the own-End receipt exists" [SendApplicationLifecycleReply (CandidateApplicationDisposition lane) (LifecycleReply (Local.request session 2) ownEndResult)] (effectBatchMembers beforeEffects)
  retired <- checkedIO (Preparation.retireClosedSessionLifecycleReceipts session original)
  assertEqual "a dead session releases terminal non-End replies" Nothing (Preparation.lookupRequest (key 0) retired)
  assertEqual "pending readiness observation loses its dead consumer" Nothing (Preparation.lookupRequest (key 1) retired)
  assertEqual "pending preparation observation also loses its dead consumer" Nothing (Preparation.lookupRequest (key 3) retired)
  assertEqual "pending mutations survive disconnection" [key 4, key 5] (map fst (Preparation.requestEntries retired))
  assertEqual "deferred Begin keeps its selected return locator" [(key 4, locator)] (Preparation.deferredBeginEntries retired)
  assertEqual "terminal own-End no longer accumulates one record per process lifetime" Nothing (Preparation.lookupRequest (key 2) retired)
  (_, afterEffects) <- checkedIO (recover (replaceStartupProcessPreparationState retired initial))
  assertEqual "recovery after terminal cleanup is correlated absence" [SendApplicationLifecycleReply (CandidateApplicationDisposition lane) (LifecycleAbsent (Local.request session 2))] (effectBatchMembers afterEffects)
  begun <- checkedIO (Preparation.settleRequest (key 4) (LifecycleRejected LifecycleTargetUnavailable) (Preparation.removeDeferredBegin (key 4) retired))
  settled <- checkedIO (Preparation.settleRequest (key 5) (LifecycleCompleted ChildCancelled) begun)
  cleaned <- checkedIO (Preparation.retireClosedSessionLifecycleReceipts session settled)
  assertEqual "late settlement releases the final dead-session receipt" [] (Preparation.requestEntries cleaned)

caseOwnEndFinalReply :: Assertion
caseOwnEndFinalReply = do
  fixtureValue@(Local.Fixture initial oracleBinding binding session attachment _) <- Local.fixture
  (pending, effects) <- Local.call fixtureValue 0 EndOwnProcess initial
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (ended, _, terminalEffects) <- Local.apply oracleBinding oracle effects pending
  assertBool
    "the emitted batch owns the correlated final result"
    (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleReply (Local.request session 0) (LifecycleCompleted ProcessEnded)) `elem` effectBatchMembers terminalEffects)
  assertEqual "own-End has ended the session" Nothing (Application.applicationSessionProcess session (startupApplicationState ended))
  assertEqual "terminal result no longer pins request history" [] (Preparation.requestEntries (startupProcessPreparationState ended))
  assertEqual "reclamation preserves full Herald invariants" (Right ()) (validateHeraldState ended)
  let lane = candidateApplicationLane 792
  (_, recoveryEffects) <- checkedIO (Lifecycle.applyProcessPreparationIngress (RecoverApplicationLifecycleResult lane attachment (Local.request session 0)) ended)
  assertEqual "a subsequent recovery request observes completed retirement" [SendApplicationLifecycleReply (CandidateApplicationDisposition lane) (LifecycleAbsent (Local.request session 0))] (effectBatchMembers recoveryEffects)

locator :: HeraldLocator
locator = either (error . show) id (heraldLocator "localhost" 35001)

preparationFor :: Session.ApplicationSessionId -> Word64 -> ChildPreparation
preparationFor session ordinal =
  either (error . show) id (childPreparation (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session)) (Session.applicationSessionIdOrdinal session) ordinal)

checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure

require :: Maybe value -> IO value
require = maybe (assertFailure "missing retained preparation") pure
