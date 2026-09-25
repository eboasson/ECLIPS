{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessPreparationWorkProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Lifecycle
import Eclips.Domain.Identity
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Discovery.Internal (ConnectionNonce (..), PeerBinding (..), PeerCandidate (..), firstPeerBindingGeneration)
import Eclips.Herald.Input (candidateApplicationLane)
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.ProcessPreparation.Protocol (RemoteChild (..), RemotePreparationStatus (..))
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.State (startupOracleClientState, startupProcessPreparationState)
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)
import ProcessPreparationProperties qualified as Local
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "process preparation owned work"
    [ testCase "delivery stamps and offer replay wake only their delivery stage" caseDelivery,
      testCase "terminal outgoing work retires and retry cleanup reactivates exactly" caseOutgoingLifetime,
      testCase "readiness dependencies replace old watches and share one compact answer" caseReadiness,
      testCase "terminal preparation and retired Begin still observe both Oracle results" caseLateResults,
      testCase "own-End observation and session retirement preserve exact recovery lifetime" caseOwnEnd,
      testCase "claim replacement removes obsolete lane and dependency memberships" caseClaimReplacement,
      QC.testProperty "receipt mutations agree with independent session and work projections" propReceiptHistory
    ]

caseDelivery :: Assertion
caseDelivery = do
  Local.Fixture _ _ binding session attachment parent <- Local.fixture
  let source = Session.applicationSessionIdHeraldEpoch session
      outgoingReference = preparationFor session 0
      incomingReference = preparationFor session 1
      key = Preparation.lifecycleKey parent (Local.request session 0)
      peer = peerBinding source
      entry = Preparation.OutgoingPreparation parent source locator "transfer" (RemotePreparationPending []) Nothing Nothing False Nothing Nothing
  receipt <- checkedIO (Preparation.retainRequest key (BeginChild locator Local.emptySelection) (LifecycleCompleted (ChildPreparationAccepted outgoingReference)) session binding attachment Preparation.emptyState)
  outgoing <- checkedIO (Preparation.retainOutgoing outgoingReference entry receipt)
  incoming <- checkedIO (Preparation.retainIncomingOffer incomingReference source locator "offer" outgoing)
  let parked = parkAll incoming
  sent <- checkedIO (Preparation.recordOutgoingDelivery outgoingReference peer True parked)
  sentAgain <- checkedIO (Preparation.recordOutgoingDelivery outgoingReference peer False sent)
  replied <- checkedIO (Preparation.recordIncomingReply incomingReference peer (RemotePreparationPending []) sentAgain)
  assertEqual "offer receipt does not wake semantic work" Set.empty (Preparation.pendingWork Preparation.OutgoingStage replied)
  assertEqual "delivery stamps do not wake delivery" Set.empty (Preparation.pendingWork Preparation.OutgoingDeliveryStage replied)
  assertEqual "reply stamp does not wake incoming delivery" Set.empty (Preparation.pendingWork Preparation.IncomingDeliveryStage replied)
  assertEqual "an ordinary reoffer preserves a prior cancellation stamp" (Just (Just peer)) (Preparation.outgoingCancelBinding <$> Preparation.lookupOutgoing outgoingReference replied)
  replayed <- checkedIO (Preparation.retainIncomingOffer incomingReference source locator "offer" replied)
  assertEqual "exact offer replay requests one reply" (Set.singleton incomingReference) (Preparation.pendingWork Preparation.IncomingDeliveryStage replayed)
  assertEqual "offer replay does not restart local preparation" Set.empty (Preparation.pendingWork Preparation.PreparationStage replayed)
  let rebound = Preparation.notifyPreparationDependencies (Set.singleton (Preparation.PeerBindingChanged source)) (parkAll replayed)
  assertEqual "binding changes wake outgoing delivery" (Set.singleton outgoingReference) (Preparation.pendingWork Preparation.OutgoingDeliveryStage rebound)
  assertEqual "binding changes wake incoming delivery" (Set.singleton incomingReference) (Preparation.pendingWork Preparation.IncomingDeliveryStage rebound)
  assertEqual "binding changes leave outgoing semantics parked" Set.empty (Preparation.pendingWork Preparation.OutgoingStage rebound)
  cancelled <- checkedIO (Preparation.cancelOutgoing outgoingReference Preparation.OrdinaryCancellation (parkAll replied))
  assertEqual "later cancellation wakes semantics and reads the recorded offer" (Set.singleton outgoingReference) (Preparation.pendingWork Preparation.OutgoingStage cancelled)
  assertEqual "the earlier offer stamp remains available" (Just (Just peer)) (Preparation.outgoingOfferBinding <$> Preparation.lookupOutgoing outgoingReference cancelled)
  assertValid rebound
  assertValid cancelled

caseOutgoingLifetime :: Assertion
caseOutgoingLifetime = do
  fixtureValue@(Local.Fixture _ _ binding session attachment parent) <- Local.fixture
  (preparedState, _, _, preparedChildValue) <- Local.prepare fixtureValue Local.emptySelection
  process <- Local.childProcess (preparedChildPreparation preparedChildValue) preparedState
  let reference = preparationFor session 5
      key = Preparation.lifecycleKey parent (Local.request session 5)
      source = Session.applicationSessionIdHeraldEpoch session
      child = RemoteChild process (controlIndex 3) (preparedChildConnection preparedChildValue)
      entry = Preparation.OutgoingPreparation parent source locator "transfer" (RemotePreparationPending []) Nothing Nothing False Nothing Nothing
  receipt <- checkedIO (Preparation.retainRequest key (BeginChild locator Local.emptySelection) (LifecycleCompleted (ChildPreparationAccepted reference)) session binding attachment Preparation.emptyState)
  original <- checkedIO (Preparation.retainOutgoing reference entry receipt)
  available <- checkedIO (Preparation.receiveOutgoingStatus reference (RemotePreparationAvailable child Nothing) (parkAll original))
  failed <- checkedIO (Preparation.receiveOutgoingStatus reference (RemotePreparationFailed (StartupRequiredObjectUnavailable "endpoint")) available)
  assertEqual "terminal remote work is unregistered" Set.empty (Preparation.workIds Preparation.OutgoingStage failed)
  assertEqual "terminal remote delivery is unregistered" Set.empty (Preparation.workIds Preparation.OutgoingDeliveryStage failed)
  assertEqual "status without a child preserves the accepted child" (Just (Just child)) (Preparation.outgoingChild <$> Preparation.lookupOutgoing reference failed)
  retry <- checkedIO (Preparation.cancelOutgoing reference Preparation.RetryRemoteCleanup failed)
  assertEqual "cleanup retry reactivates semantic work" (Set.singleton reference) (Preparation.pendingWork Preparation.OutgoingStage retry)
  assertEqual "cleanup retry reactivates delivery" (Set.singleton reference) (Preparation.pendingWork Preparation.OutgoingDeliveryStage retry)
  assertEqual "retry restores pending cancellation status" (Just (RemotePreparationPending [])) (Preparation.outgoingStatus <$> Preparation.lookupOutgoing reference retry)
  done <- checkedIO (Preparation.receiveOutgoingStatus reference RemotePreparationCancelled retry)
  let later = Preparation.notifyPreparationDependencies (Set.fromList [Preparation.ActivityChanged, Preparation.PeerBindingChanged source]) done
  assertEqual "terminal entries cannot be resurrected by old dependency keys" Set.empty (Preparation.pendingWork Preparation.OutgoingStage later)
  forM_ [available, failed, retry, later] assertValid

caseReadiness :: Assertion
caseReadiness = do
  fixtureValue@(Local.Fixture _ _ binding session _ parent) <- Local.fixture
  (accepted, _, reference) <- Local.begin fixtureValue Local.emptySelection
  process <- Local.childProcess reference accepted
  let attachment = Session.applicationAttachmentForProcess process
      claim = checked (initialClaimId (Bytes.replicate 32 21))
      key = Preparation.lifecycleKey parent (Local.request session 9)
      object = globalObjectIdFromProcessEpochId process
      firstDependency = Preparation.ObjectChanged object
      secondDependency = Preparation.ProcessChanged process
      pending = Right (Just ["reader", "writer"])
  requested <- checkedIO (Preparation.retainRequest key (AwaitChildReady reference) (LifecyclePending []) session binding (Session.applicationAttachmentForProcess parent) (startupProcessPreparationState accepted))
  let claimed = Preparation.retainPendingClaim attachment claim (candidateApplicationLane 12) requested
  cached <- checkedIO (Preparation.retainReadiness reference (Set.singleton firstDependency) pending (parkAll claimed))
  assertEqual "one compact outcome serves every consumer" (Just pending) (Preparation.lookupReadiness reference cached)
  assertEqual "installing readiness does not self-wake requests" Set.empty (Preparation.pendingWork Preparation.RequestStage cached)
  let unrelated = Preparation.notifyPreparationDependencies (Set.singleton Preparation.OracleBindingChanged) cached
  assertEqual "unrelated notifications preserve the cache" (Just pending) (Preparation.lookupReadiness reference unrelated)
  replacement <- checkedIO (Preparation.retainReadiness reference (Set.singleton secondDependency) (Right Nothing) cached)
  let oldChanged = Preparation.notifyPreparationDependencies (Set.singleton firstDependency) replacement
  assertEqual "obsolete readiness dependencies are removed" (Just (Right Nothing)) (Preparation.lookupReadiness reference oldChanged)
  let changed = Preparation.notifyPreparationDependencies (Set.singleton secondDependency) oldChanged
  assertEqual "current readiness dependency invalidates the compact outcome" Nothing (Preparation.lookupReadiness reference changed)
  assertEqual "waiting request is woken" (Set.singleton key) (Preparation.pendingWork Preparation.RequestStage changed)
  assertEqual "waiting claim is woken" (Set.singleton (attachment, claim)) (Preparation.pendingWork Preparation.ClaimStage changed)
  recached <- checkedIO (Preparation.retainReadiness reference Set.empty (Left StartupCancelled) (parkAll changed))
  assertEqual "immutable outcomes need no endpoint watches" (Just (Left StartupCancelled)) (Preparation.lookupReadiness reference (Preparation.notifyPreparationDependencies (Set.singleton secondDependency) recached))
  forM_ [cached, replacement, changed, recached] assertValid

caseLateResults :: Assertion
caseLateResults = do
  fixtureValue@(Local.Fixture _ _ _ session _ _) <- Local.fixture
  (accepted, _, reference) <- Local.begin fixtureValue Local.emptySelection
  process <- Local.childProcess reference accepted
  entry <- require (Preparation.lookupPreparation reference (startupProcessPreparationState accepted))
  start <- require (Preparation.preparationStartReference entry)
  preparedEnd <- checkedIO (Client.prepareEndProcessEpochRequest "work-owner-end" process ExplicitAdministrativeEnd (startupOracleClientState accepted))
  let end = Client.preparedOracleRequestRef preparedEnd
      first = startupProcessPreparationState accepted
  repeated <- checkedIO (Preparation.markPreparationStartReference reference start (parkAll first))
  assertEqual "exact reference replay does not make work dirty" Set.empty (Preparation.pendingWork Preparation.PreparationStage repeated)
  cancelling <- checkedIO (Preparation.markPreparationCancelling reference repeated)
  ending <- checkedIO (Preparation.markPreparationEndReference reference end cancelling)
  terminal <- checkedIO (Preparation.markPreparationTerminal reference ending)
  retired <- checkedIO (Preparation.retireLifecycleReceipts session (receiptRetirementPrefix (Just 1)) terminal)
  assertEqual "terminal and Begin retirement preserve both required observations" [(Preparation.PreparationStartResult reference, start), (Preparation.PreparationEndResult reference, end)] (Preparation.unobservedPreparationResults reference retired)
  let notified = Preparation.notifyOracleResults (Set.fromList [start, end]) (parkAll retired)
  assertEqual "late projected results wake the original preparation stage" (Set.singleton reference) (Preparation.pendingWork Preparation.PreparationStage notified)
  observedStart <- checkedIO (Preparation.markResultObserved (Preparation.PreparationStartResult reference) notified)
  assertEqual "the remaining End keeps terminal work registered" (Set.singleton reference) (Preparation.workIds Preparation.PreparationStage observedStart)
  observedEnd <- checkedIO (Preparation.markResultObserved (Preparation.PreparationEndResult reference) observedStart)
  assertEqual "both observations release terminal work" Set.empty (Preparation.workIds Preparation.PreparationStage observedEnd)
  assertEqual "observed results have no residual wake memberships" observedEnd (Preparation.notifyOracleResults (Set.fromList [start, end]) observedEnd)
  assertEqual "observation flags survive comparison normalization" [] (Preparation.unobservedPreparationResults reference (Preparation.clearPreparationScheduling observedEnd))
  forM_ [ending, retired, observedStart, observedEnd] assertValid

caseOwnEnd :: Assertion
caseOwnEnd = do
  fixtureValue@(Local.Fixture initial _ _ session attachment parent) <- Local.fixture
  (pending, _) <- Local.call fixtureValue 0 EndOwnProcess initial
  let key = Preparation.lifecycleKey parent (Local.request session 0)
      owner = startupProcessPreparationState pending
  reference <- require (Preparation.unobservedOwnEndResult key owner)
  let parked = parkAll (snd (Preparation.takeLifecycleRetirementSessions owner))
      closed = Preparation.notifyPreparationDependencies (Set.singleton (Preparation.SessionChanged session)) parked
  assertEqual "session loss only queues the known receipt session" (Set.singleton session) (fst (Preparation.takeLifecycleRetirementSessions closed))
  surviving <- checkedIO (Preparation.retireClosedSessionLifecycleReceipts session closed)
  assertEqual "session death preserves an unresolved own-End" (Just reference) (Preparation.unobservedOwnEndResult key surviving)
  assertEqual "cleaning the closed session consumes its candidate" Set.empty (fst (Preparation.takeLifecycleRetirementSessions surviving))
  let notified = Preparation.notifyOracleResults (Set.singleton reference) (parkAll surviving)
  assertEqual "late own-End results wake requests" (Set.singleton key) (Preparation.pendingWork Preparation.RequestStage notified)
  completed <- checkedIO (Preparation.settleRequest key (LifecycleCompleted ProcessEnded) notified)
  observed <- checkedIO (Preparation.markResultObserved (Preparation.OwnEndResult key) completed)
  assertEqual "settlement re-enqueues the closed session for cleanup" (Set.singleton session) (fst (Preparation.takeLifecycleRetirementSessions observed))
  assertEqual "exact own-End result remains recoverable until receipt cleanup" (Just (LifecycleCompleted ProcessEnded)) (Preparation.ownEndRecovery attachment (Local.request session 0) observed)
  retired <- checkedIO (Preparation.retireClosedSessionLifecycleReceipts session observed)
  assertEqual "receipt retirement removes restricted recovery" Nothing (Preparation.ownEndRecovery attachment (Local.request session 0) retired)
  assertEqual "retired result cannot wake stale request work" Set.empty (Preparation.pendingWork Preparation.RequestStage (Preparation.notifyOracleResults (Set.singleton reference) retired))
  assertEqual "removed sessions no longer enter retirement candidates" Set.empty (fst (Preparation.takeLifecycleRetirementSessions (Preparation.notifyPreparationDependencies (Set.singleton (Preparation.SessionChanged session)) retired)))
  forM_ [surviving, observed, retired] assertValid

caseClaimReplacement :: Assertion
caseClaimReplacement = do
  Local.Fixture _ _ _ _ attachment _ <- Local.fixture
  let firstClaim = checked (initialClaimId (Bytes.replicate 32 30))
      secondClaim = checked (initialClaimId (Bytes.replicate 32 31))
      firstLane = candidateApplicationLane 20
      secondLane = candidateApplicationLane 21
      initial = Preparation.retainPendingClaim attachment firstClaim firstLane Preparation.emptyState
      parked = parkAll initial
      replayed = Preparation.retainPendingClaim attachment firstClaim firstLane parked
      rebound = Preparation.retainPendingClaim attachment firstClaim secondLane replayed
      replaced = Preparation.retainPendingClaim attachment secondClaim secondLane rebound
  assertEqual "pending retry cannot perpetually redirty itself" parked replayed
  assertEqual "a replacement lane owns only the latest claim" [((attachment, secondClaim), secondLane)] (Preparation.pendingClaimEntries replaced)
  assertEqual "old lane loss cannot remove the replacement" replaced (Preparation.observeCandidateLoss firstLane replaced)
  let removed = Preparation.observeCandidateLoss secondLane replaced
      notified = Preparation.notifyPreparationDependencies (Set.fromList [Preparation.AttachmentChanged attachment, Preparation.GateChanged]) removed
  assertEqual "current lane loss removes all claim work" Set.empty (Preparation.workIds Preparation.ClaimStage notified)
  assertEqual "removed claim dependencies cannot restore work" Set.empty (Preparation.pendingWork Preparation.ClaimStage notified)
  forM_ [rebound, replaced, removed, notified] assertValid

propReceiptHistory :: [Word8] -> QC.Property
propReceiptHistory actions = QC.ioProperty $ do
  Local.Fixture _ _ binding session attachment parent <- Local.fixture
  let reference = preparationFor session 0
      key ordinal = Preparation.lifecycleKey parent (Local.request session ordinal)
      insert state ordinal = Preparation.retainRequest (key ordinal) (AwaitChildReady reference) (LifecyclePending []) session binding attachment state
  initial <- checkedIO (foldM insert Preparation.emptyState [0 .. 7])
  _ <- foldM (step key session) initial (take 45 actions)
  pure True
  where
    step key session state action = do
      let ordinal = fromIntegral (action `mod` 8)
          selected = key ordinal
      next <- case action `mod` 3 of
        0 -> case Preparation.lookupRequest selected state of
          Just record | LifecyclePending {} <- Preparation.requestStatus record -> checkedIO (Preparation.settleRequest selected (LifecycleRejected LifecycleAlreadyTerminal) state)
          _ -> pure state
        1 -> pure (Preparation.notifyPreparationDependencies (Set.singleton (Preparation.SessionChanged session)) state)
        _ -> checkedIO (Preparation.retireClosedSessionLifecycleReceipts session state)
      assertValid next
      let entries = Preparation.requestEntries next
          expected = Set.fromList [entryKey | (entryKey, record) <- entries, LifecyclePending {} <- [Preparation.requestStatus record]]
      assertEqual "work inventory follows the independent request transcript" expected (Preparation.workIds Preparation.RequestStage next)
      assertEqual "session index equals transcript filtering" [(entryKey, record) | (entryKey, record) <- entries, Preparation.requestSession record == session] (Preparation.requestEntriesForSession session next)
      pure next

parkAll :: Preparation.State -> Preparation.State
parkAll = park Preparation.PreparationStage . park Preparation.OutgoingStage . park Preparation.RequestStage . park Preparation.ClaimStage . park Preparation.OutgoingDeliveryStage . park Preparation.IncomingDeliveryStage

park :: (Ord key) => Preparation.WorkStage key -> Preparation.State -> Preparation.State
park stage state = Set.foldl' (flip (Preparation.consumeWork stage)) state (Preparation.workIds stage state)

preparationFor :: Session.ApplicationSessionId -> Word64 -> ChildPreparation
preparationFor session ordinal = checked (childPreparation (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session)) (Session.applicationSessionIdOrdinal session) ordinal)

peerBinding :: HeraldEpoch -> PeerBinding
peerBinding epoch =
  let identifier = checked (mkHeraldId (Bytes.replicate 32 71))
   in PeerBinding identifier epoch firstPeerBindingGeneration (PeerCandidate identifier epoch (ConnectionNonce 1))

locator :: HeraldLocator
locator = checked (heraldLocator "localhost" 35001)

assertValid :: Preparation.State -> Assertion
assertValid = assertEqual "owner indexes and lifecycle facts remain valid" (Right ()) . Preparation.validateState

checked :: (Show error) => Either error value -> value
checked = either (error . show) id

checkedIO :: (Show error) => Either error value -> IO value
checkedIO = either (assertFailure . show) pure

require :: Maybe value -> IO value
require = maybe (assertFailure "missing owner test fixture") pure
