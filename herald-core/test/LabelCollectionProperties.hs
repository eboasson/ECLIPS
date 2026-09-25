module LabelCollectionProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List (inits)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Herald.Label.Collection qualified as Collection
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    listOf,
    shuffle,
    sublistOf,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "label installation collection"
    [ testProperty "duplicate and reordered reports agree with a reference installed set" propReportsMatchSet,
      testProperty "only captured surviving participants remain required after canonical membership changes" propMembershipReference,
      testProperty "early reports drain exactly through their terminal control prerequisite" propFutureReports,
      testCase "reports bind decision, terminal index, digest and captured reporter" caseReportAdmission,
      testCase "early duplicates are inert and conflicting immutable evidence rejects" caseEarlyConflict,
      testCase "no transport event or unchanged generation can retire a participant" caseNoLocalShrinking,
      testCase "retirement reassigns collection to the least surviving captured Herald" caseCollectorRetirement,
      testCase "reconnect replays once per binding and preserves local installation evidence" caseReconnect,
      testCase "stale completion requests can be retried in a new membership generation" caseCompletionRetry,
      testCase "historical installation evidence survives until matching canonical completion" caseRetention,
      testProperty "independent concurrent collections agree with per-decision installed sets" propConcurrentReports,
      testCase "retirement, reconnect and completion wake only indexed dependent decisions" caseIndexedWork,
      testCase "a stale completion offer wakes only when its exact decision is notified" caseExactCompletionWake
    ]

propReportsMatchSet :: Property
propReportsMatchSet =
  forAll (listOf (elements participants)) $ \reporters ->
    let initial = beginAt heraldA heraldA
        reports = fmap reportBy reporters
        observed supplied = foldM (flip Collection.observe) initial supplied
        expectedRemaining supplied = Set.difference captured (Set.insert heraldA (Set.fromList supplied))
     in conjoin
          ( [ counterexample
                ("report prefix " <> show prefix)
                (fmap (Collection.outstanding decisionA) (observed (fmap reportBy prefix)) === Right (expectedRemaining prefix))
            | prefix <- inits reporters
            ]
              <> [ observed reports === observed (reverse reports),
                   observed reports === observed (reports <> reports)
                 ]
          )

propMembershipReference :: Property
propMembershipReference =
  forAll (sublistOf [heraldB, heraldC]) $ \survivors ->
    forAll (listOf (elements participants)) $ \reporters ->
      let initial = beginAt heraldA heraldB
          installed = Set.insert heraldA (Set.fromList reporters)
          active = Set.fromList (heraldA : heraldD : survivors)
          required = Set.intersection captured active
          assigned = if Set.member heraldB required then Just heraldB else Set.lookupMin required
          remaining = Set.difference required installed
          ready = if assigned == Just heraldA && Set.null remaining then Just generationB else Nothing
          observed = checked (foldM (flip Collection.observe) initial (fmap reportBy reporters))
          refreshed = Collection.refreshMembership generationB active observed
       in conjoin
            [ (Collection.outstanding decisionA) refreshed === remaining,
              (Collection.collector decisionA) refreshed === assigned,
              (Collection.completionReady decisionA) refreshed === ready,
              (Collection.currentReport decisionA) refreshed === Just (reportBy heraldA),
              Collection.refreshMembership generationB active refreshed === refreshed
            ]

propFutureReports :: Property
propFutureReports =
  forAll (chooseInt (1, 20)) $ \count ->
    forAll (chooseInt (0, count + 1)) $ \through ->
      forAll (shuffle [1 .. count]) $ \order ->
        let reports = fmap reportAt order
            staged = checked (foldM (flip Collection.stage) Collection.emptyState (reports <> reports))
            started = checked (Collection.begin (reportBy heraldA) heraldA captured generationA captured staged)
            (drained, ready) = Collection.takeReady (Identity.controlIndex (fromIntegral through)) started
            expected = Set.fromList (fmap reportAt [1 .. min through count])
            (again, replayed) = Collection.takeReady (Identity.controlIndex (fromIntegral through)) drained
            (_, remaining) = Collection.takeReady (Identity.controlIndex (fromIntegral (count + 1))) drained
         in conjoin
              [ Set.fromList ready === expected,
                length ready === Set.size expected,
                replayed === [],
                again === drained,
                Set.fromList remaining === Set.fromList (fmap reportAt [through + 1 .. count]),
                (Collection.currentReport decisionA) drained === Just (reportBy heraldA)
              ]

caseReportAdmission :: Assertion
caseReportAdmission = do
  let initial = beginAt heraldA heraldA
      wrongDecision = makeReport decisionB heraldB terminalIndex digestA
      wrongIndex = makeReport decisionA heraldB (Identity.controlIndex 12) digestA
      wrongDigest = makeReport decisionA heraldB terminalIndex digestB
      uncaptured = reportBy heraldD
  mapM_
    (\report -> assertBool ("reject unrelated evidence " <> show report) (isLeft (Collection.observe report initial)))
    [wrongDecision, wrongIndex, wrongDigest, uncaptured]
  assertBool "reports cannot create their own terminal history" (isLeft (Collection.observe (reportBy heraldB) Collection.emptyState))
  assertBool "a second terminal cannot replace an active collection" (isLeft (Collection.begin (reportBy heraldA) heraldA captured generationA captured initial))
  assertBool "a local reporter must have been captured" (isLeft (Collection.begin (reportBy heraldD) heraldA captured generationA captured Collection.emptyState))

caseEarlyConflict :: Assertion
caseEarlyConflict = do
  let report = reportBy heraldB
      staged = checked (Collection.stage report Collection.emptyState)
      conflict = makeReport decisionA heraldB terminalIndex digestB
  assertEqual "exact replay remains one retained claim" (Right staged) (Collection.stage report staged)
  assertBool "changed terminal digest cannot overwrite queued evidence" (isLeft (Collection.stage conflict staged))
  assertEqual "nothing drains before the prerequisite" (staged, []) (Collection.takeReady (Identity.controlIndex 10) staged)
  let (drained, reports) = Collection.takeReady terminalIndex staged
  assertEqual "original immutable report remains intact" [report] reports
  assertEqual "drained claims leave no retained queue" Collection.emptyState drained

caseNoLocalShrinking :: Assertion
caseNoLocalShrinking = do
  let initial = beginAt heraldA heraldA
      unchangedGeneration = Collection.refreshMembership generationA (Set.singleton heraldA) initial
  assertEqual "the same canonical generation cannot shrink the captured requirements" initial unchangedGeneration
  assertEqual "missing peers continue to block completion" Nothing ((Collection.completionReady decisionA) unchangedGeneration)
  assertEqual "self-report replay does not stand in for other participants" (Right initial) (Collection.observe (reportBy heraldA) initial)
  let dispatched = (Collection.markReportDispatched decisionA) heraldB 5 initial
  assertEqual "transport dispatch does not discharge an installer" ((Collection.outstanding decisionA) initial) ((Collection.outstanding decisionA) dispatched)

caseCollectorRetirement :: Assertion
caseCollectorRetirement = do
  let initial = beginAt heraldB heraldA
      cInstalled = checked (Collection.observe (reportBy heraldC) initial)
      retired = Collection.refreshMembership generationB (Set.fromList [heraldB, heraldC, heraldD]) cInstalled
  assertEqual "the live request home remains collector before retirement" (Just heraldA) ((Collection.collector decisionA) cInstalled)
  assertEqual "a non-collector cannot offer completion" Nothing ((Collection.completionReady decisionA) cInstalled)
  assertEqual "the least surviving captured epoch replaces the retired home" (Just heraldB) ((Collection.collector decisionA) retired)
  assertEqual "canonical retirement excuses only the retired captured participant" Set.empty ((Collection.outstanding decisionA) retired)
  assertEqual "new members are not extra old-workflow installers" (Just generationB) ((Collection.completionReady decisionA) retired)
  assertEqual "the survivor keeps its historical installation evidence" (Just (reportBy heraldB)) ((Collection.currentReport decisionA) retired)

caseReconnect :: Assertion
caseReconnect = do
  let initial = beginAt heraldC heraldA
      sent = (Collection.markReportDispatched decisionA) heraldA 7 initial
      reconnected = (Collection.markReportDispatched decisionA) heraldA 8 sent
      reassigned = Collection.refreshMembership generationB (Set.fromList [heraldB, heraldC]) reconnected
  assertBool "a connected collector receives the retained report" ((Collection.reportNeedsDispatch decisionA) heraldA 7 initial)
  assertBool "ordinary driver passes do not resend to the same binding" (not ((Collection.reportNeedsDispatch decisionA) heraldA 7 sent))
  assertBool "a replacement binding replays the same evidence" ((Collection.reportNeedsDispatch decisionA) heraldA 8 sent)
  assertBool "replacement binding is marked once" (not ((Collection.reportNeedsDispatch decisionA) heraldA 8 reconnected))
  assertBool "unrelated peers never receive this report" (not ((Collection.reportNeedsDispatch decisionA) heraldB 8 reconnected))
  assertBool "collector retirement reoffers to the successor even with the same binding number" ((Collection.reportNeedsDispatch decisionA) heraldB 8 reassigned)
  assertEqual "dispatch and reassignment retain exact report bytes" (Just (reportBy heraldC)) ((Collection.currentReport decisionA) reassigned)

caseCompletionRetry :: Assertion
caseCompletionRetry = do
  let initial = beginAt heraldA heraldA
      installed = checked (foldM (flip Collection.observe) initial [reportBy heraldB, reportBy heraldC])
      offered = (Collection.markCompletionOffered decisionA) installed
      refreshed = Collection.refreshMembership generationB (Set.fromList [heraldA, heraldB, heraldC, heraldD]) offered
      retried = (Collection.markCompletionOffered decisionA) refreshed
  assertEqual "all installed participants allow one generation-qualified completion" (Just generationA) ((Collection.completionReady decisionA) installed)
  assertEqual "repeated driver passes do not allocate repeated completion requests" Nothing ((Collection.completionReady decisionA) offered)
  assertEqual "a stale-generation rejection has a new-generation retry" (Just generationB) ((Collection.completionReady decisionA) refreshed)
  assertEqual "the retry is also offered once" Nothing ((Collection.completionReady decisionA) retried)
  assertEqual "an offered attestation does not reclaim local evidence" (Just (reportBy heraldA)) ((Collection.currentReport decisionA) retried)
  assertEqual "the old request generation remains identifiable until retry is actually offered" (Just generationA) ((Collection.offeredGeneration decisionA) refreshed)
  assertEqual "offering the retry records its new generation" (Just generationB) ((Collection.offeredGeneration decisionA) retried)

caseRetention :: Assertion
caseRetention = do
  let initial = beginAt heraldA heraldA
      offered = (Collection.markCompletionOffered decisionA) initial
      staged = checked (Collection.stage (reportAt 12) offered)
      completed = Collection.complete decisionA staged
  assertEqual "unrelated completion cannot erase installation history" staged (Collection.complete decisionB staged)
  assertEqual "matching canonical completion releases local history" Nothing ((Collection.currentReport decisionA) completed)
  assertEqual "completion also releases outstanding requirements" Set.empty ((Collection.outstanding decisionA) completed)
  assertEqual "completed history cannot offer a new attestation" Nothing ((Collection.completionReady decisionA) completed)
  assertEqual "canonical completion replay is inert" completed (Collection.complete decisionA completed)
  assertEqual "completion of this decision preserves future queued reports" [reportAt 12] (snd (Collection.takeReady (Identity.controlIndex 12) completed))
  let successorReport = makeReport decisionB heraldA (Identity.controlIndex 12) digestB
      successor = checked (Collection.begin successorReport heraldA captured generationA captured completed)
  assertEqual "an old completion replay preserves a newer active collector" successor (Collection.complete decisionA successor)
  assertEqual "the newer workflow retains its own immutable evidence" (Just successorReport) ((Collection.currentReport decisionB) successor)

propConcurrentReports :: Property
propConcurrentReports =
  forAll (listOf ((,) <$> elements decisions <*> elements participants)) $ \deliveries ->
    let initial = foldl (\state decision -> checked (Collection.begin (report decision heraldA) heraldA captured generationA captured state)) Collection.emptyState decisions
        histories = inits deliveries
        snapshots = scanl (\state (decision, reporter) -> checked (Collection.observe (report decision reporter) state)) initial deliveries
        expected decision prefix = Set.difference captured (Set.insert heraldA (Set.fromList [reporter | (observed, reporter) <- prefix, observed == decision]))
     in conjoin
          [ counterexample
              (show prefix)
              ( conjoin
                  ( [Collection.valid state === True]
                      <> [ conjoin
                             [ Collection.outstanding decision state === expected decision prefix,
                               Collection.currentReport decision state === Just (report decision heraldA),
                               Collection.completionReady decision state === if Set.null (expected decision prefix) then Just generationA else Nothing
                             ]
                         | decision <- decisions
                         ]
                  )
              )
          | (prefix, state) <- zip histories snapshots
          ]
  where
    decisions = [decisionA, decisionB, identity Identity.mkLabelDecisionId 23]
    report decision reporter = makeReport decision reporter terminalIndex digestA

caseIndexedWork :: Assertion
caseIndexedWork = do
  let allActive = Set.fromList [heraldA, heraldB, heraldC, heraldD]
      reportA reporter = makeReport decisionA reporter terminalIndex digestA
      reportB reporter = makeReport decisionB reporter (Identity.controlIndex 12) digestB
      first = checked (Collection.begin (reportA heraldA) heraldB captured generationA allActive Collection.emptyState)
      both = checked (Collection.begin (reportB heraldA) heraldD (Set.fromList [heraldA, heraldD]) generationA allActive first)
      idle = consumeAll both
      reported = checked (Collection.observe (reportA heraldB) idle)
      retired = Collection.refreshMembership generationB (Set.delete heraldB allActive) (consumeAll reported)
      ready = checked (Collection.observe (reportA heraldC) (consumeAll retired))
      offered = Collection.markCompletionOffered decisionA (consumeAll ready)
      newerGeneration = identity Membership.mkHeraldMembershipGenerationId 33
      renewed = Collection.refreshMembership newerGeneration (Set.delete heraldB allActive) offered
      bound = Collection.refreshBindings (Map.singleton heraldD 7) (consumeAll renewed)
      sent = Collection.markReportDispatched decisionB heraldD 7 (consumeAll bound)
      reconnected = Collection.refreshBindings (Map.singleton heraldD 8) sent
      completed = Collection.complete decisionA reconnected
  assertEqual "begin schedules both independent installations" (Set.fromList [decisionA, decisionB]) (Collection.pendingWork both)
  assertEqual "report wakes only its decision" (Set.singleton decisionA) (Collection.pendingWork reported)
  assertEqual "retiring an already-installed collector still wakes its assigned decision" (Set.singleton decisionA) (Collection.pendingWork retired)
  assertEqual "retirement reassigns only that collector" (Just heraldA, Just heraldD) (Collection.collector decisionA retired, Collection.collector decisionB retired)
  assertEqual "other decision retains its independent missing reporter" (Set.singleton heraldD) (Collection.outstanding decisionB retired)
  assertEqual "remaining report makes only the exact local collection ready" (Just generationB, Nothing) (Collection.completionReady decisionA ready, Collection.completionReady decisionB ready)
  assertEqual "new generation wakes a ready collector even without a newly missing peer" (Set.singleton decisionA) (Collection.pendingWork renewed)
  assertEqual "completion uses the new canonical generation" (Just newerGeneration) (Collection.completionReady decisionA renewed)
  assertEqual "new binding touches only its assigned collections" (Set.singleton decisionB) (Collection.pendingWork bound)
  assertEqual "unchanged binding is quiescent" sent (Collection.refreshBindings (Map.singleton heraldD 7) sent)
  assertEqual "reconnect wakes only the destination's collection" (Set.singleton decisionB) (Collection.pendingWork reconnected)
  assertEqual "other completion retains pending report work" (Set.singleton decisionB) (Collection.pendingWork completed)
  assertEqual "other completion retains exact report" (Just (reportB heraldA)) (Collection.currentReport decisionB completed)
  assertEqual "completed decision is absent" Nothing (Collection.currentReport decisionA completed)
  assertEqual "completed decision cannot be woken by its old collector" (consumeAll completed) (Collection.wakeCollector heraldA (consumeAll completed))
  forM_ [first, both, idle, reported, retired, ready, offered, renewed, bound, sent, reconnected, completed] $ \state ->
    assertBool "all event and reverse indices reconstruct independently" (Collection.valid state)
  where
    consumeAll state = Set.foldl' (flip Collection.consumeWork) state (Collection.pendingWork state)

caseExactCompletionWake :: Assertion
caseExactCompletionWake = do
  let initial = beginAt heraldA heraldA
      ready = checked (foldM (flip Collection.observe) initial [reportBy heraldB, reportBy heraldC])
      offered = Collection.consumeWork decisionA (Collection.markCompletionOffered decisionA ready)
      refreshed = Collection.refreshMembership generationB captured offered
      waitingForOldReceipt = Collection.consumeWork decisionA refreshed
      woken = Collection.wakeDecision decisionA waitingForOldReceipt
  assertEqual "waiting for an earlier request need not spin the driver" Set.empty (Collection.pendingWork waitingForOldReceipt)
  assertEqual "exact receipt projection wakes its blocked newer attestation" (Set.singleton decisionA) (Collection.pendingWork woken)
  assertEqual "unrelated receipt projection wakes nothing" waitingForOldReceipt (Collection.wakeDecision decisionB waitingForOldReceipt)
  assertEqual "the eventual offer remains ready in the new generation" (Just generationB) (Collection.completionReady decisionA woken)
  assertEqual "completion removes the wake index" Set.empty (Collection.pendingWork (Collection.complete decisionA woken))

beginAt :: Identity.HeraldEpoch -> Identity.HeraldEpoch -> Collection.State
beginAt local home = checked (Collection.begin (reportBy local) home captured generationA captured Collection.emptyState)

reportBy :: Identity.HeraldEpoch -> Label.LabelInstallationReport
reportBy reporter = makeReport decisionA reporter terminalIndex digestA

reportAt :: Int -> Label.LabelInstallationReport
reportAt index = makeReport (identity Identity.mkLabelDecisionId (fromIntegral index)) heraldB (Identity.controlIndex (fromIntegral index)) digestA

makeReport :: Identity.LabelDecisionId -> Identity.HeraldEpoch -> Identity.ControlIndex -> Label.LabelOutcomeDigest -> Label.LabelInstallationReport
makeReport = Label.labelInstallationReport

participants :: [Identity.HeraldEpoch]
participants = [heraldA, heraldB, heraldC]

captured :: Set Identity.HeraldEpoch
captured = Set.fromList participants

heraldA, heraldB, heraldC, heraldD :: Identity.HeraldEpoch
heraldA = identity Identity.mkHeraldEpoch 1
heraldB = identity Identity.mkHeraldEpoch 2
heraldC = identity Identity.mkHeraldEpoch 3
heraldD = identity Identity.mkHeraldEpoch 4

decisionA, decisionB :: Identity.LabelDecisionId
decisionA = identity Identity.mkLabelDecisionId 21
decisionB = identity Identity.mkLabelDecisionId 22

generationA, generationB :: Membership.HeraldMembershipGenerationId
generationA = identity Membership.mkHeraldMembershipGenerationId 31
generationB = identity Membership.mkHeraldMembershipGenerationId 32

digestA, digestB :: Label.LabelOutcomeDigest
digestA = identity Label.mkLabelOutcomeDigest 41
digestB = identity Label.mkLabelOutcomeDigest 42

terminalIndex :: Identity.ControlIndex
terminalIndex = Identity.controlIndex 11

identity :: (Show error) => (ByteString.ByteString -> Either error value) -> Word8 -> value
identity constructor = checked . constructor . ByteString.replicate 32

checked :: (Show error) => Either error value -> value
checked = either (error . ("invalid label collection fixture: " <>) . show) id
