{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Named deterministic owner schedules. H4 stops after the old markers have
-- completed; only H1/H2/H3 consume its canonical retirement. All absence claims
-- are produced by live, independently serialized Heralds.
module DisappearanceMembershipProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.List (permutations)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import DisappearanceLiveProperties
  ( LiveNetwork,
    commitNetwork,
    commitNetworkWithEffects,
    drainNetwork,
    prepareQuietNetwork,
    step,
  )
import Eclips.Domain.Disappearance qualified as Domain
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.EffectBatch (HeraldEffect (RunOracleClientAction), effectBatchIsEmpty, effectBatchMembers)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.DisappearanceReadiness qualified as Readiness
import Eclips.Herald.Input (HeraldInputBody (OracleInput))
import Eclips.Herald.OracleClient (OracleClientAction (WatchOracle), OracleClientIngress (OracleEntriesReceived))
import Eclips.Herald.OracleClient.Request qualified as Request
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command qualified as Command
import Eclips.Oracle.Effect (OracleStepOutcome (OracleRequestRetired), oracleEffects)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Receipt qualified as Receipt
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures
  ( fixtureCheckedOracleGenesisFor,
    fixtureDeploymentManifest,
    fixtureIdentifierBytes,
    fixtureStep14CheckedGenesis,
  )
import Step16RegularRetirementAcceptanceProperties (liveRegularDisappearanceFixtureFor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

-- The seed names the exact scripted order, rather than replacing randomized
-- QuickCheck coverage. Each failure is directly replayable with Tasty -p.
tests :: TestTree
tests =
  testGroup
    "live disappearance membership corpus"
    [ testCase "seed-161001: Q=0; H4-stop; retire; successor-base" (membershipSchedule 0 False),
      testCase "seed-161002: Q=1; markers-before-stop; stale-report; successor-reopen" (membershipSchedule 1 False),
      testCase "seed-161003: Q=3; reverse-Open; markers-before-stop; stale-reports; successor-reopen" (membershipSchedule 3 True),
      testCase "repeated non-voter retirement preserves old terminals and renews absence evidence at each successor" repeatedMembershipSchedule
    ]

membershipSchedule :: Int -> Bool -> Assertion
membershipSchedule count reverseSchedule = do
  let (seed, subjects) = liveRegularDisappearanceFixtureFor fourHeraldGenesis count
      local = Genesis.checkedLocalHeraldEpoch fourHeraldGenesis
      order = if reverseSchedule then reverse else id
  assertEqual
    "real LocalTake creates exactly Q distinct subjects"
    (Set.fromList subjects)
    (Set.fromList [candidate.candidateSubject | candidate <- Disappearance.candidateWitnesses (startupDisappearanceState seed)])
  quiet <- prepareQuietNetwork seed
  oracle0 <- checked "initialize exact four-member Oracle" (initialOracle (fixtureCheckedOracleGenesisFor fourHeraldGenesis))
  let opens = order (requests isOpen (stateAt local quiet))
  assertEqual "Q retained live Open intentions" count (length opens)
  (oracle1, opened) <- commitRequests "predecessor Open" oracle0 quiet opens
  reported <- drainNetwork Set.empty opened []
  let oldReports = concatMap (order . requests isReport . fst) (Map.elems reported)
      oldProbes = probeIds (stateAt local reported)
  assertEqual "all four real owners reach an old-generation absence report" (4 * count) (length oldReports)
  -- Complete the ordinary failure-probe authority before stopping H4. These
  -- commands do not synthesize disappearance reports or alter their cut evidence.
  (oracle2, failureOpened, failureEntry) <-
    commitNetwork
      "failure Open"
      oracle1
      reported
      (manual local 10001 (Command.openHeraldFailureProbeCommand retiredHerald (Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership oracle1)) (Voter.voterConfigurationId (Voter.oracleVoterConfiguration oracle1))))
  failureProbe <-
    one
      "canonical failure probe"
      [probe | OracleFailureProbeOpenedView probe _ _ _ <- views failureEntry]
  let voters = take 2 (Map.keys reported)
  (oracle3, failureReported) <-
    foldM
      ( \(oracle, network) voter -> do
          (nextOracle, nextNetwork, _) <-
            commitNetwork
              "failure Unreachable"
              oracle
              network
              (manual voter 10002 (Command.reportHeraldFailureProbeCommand failureProbe (Voter.voterConfigurationId (Voter.oracleVoterConfiguration oracle2)) Command.ProbeUnreachable))
          pure (nextOracle, nextNetwork)
      )
      (oracle2, failureOpened)
      voters
  let liveBeforeRetire = Map.delete retiredHerald failureReported
      before = ledgerWork liveBeforeRetire
      candidatesBefore = ledgerField (.candidateCreations) liveBeforeRetire
      terminalBefore = ledgerField (.terminalInstallations) liveBeforeRetire
      resolution = Membership.deriveFailureProbeResolutionId failureProbe Membership.RetireFailureProbeTarget
  (oracle4, aborted, retireEntry, successorMessages) <-
    commitNetworkWithEffects
      "canonical H4 retirement"
      oracle3
      liveBeforeRetire
      (manual local 10003 (Command.retireHeraldEpochCommand resolution retiredHerald))
  assertEqual
    "one canonical Abort event for each collecting predecessor probe"
    count
    (length [() | DisappearanceProbeAbortedView {} <- views retireEntry])
  assertEqual
    "predecessor cleanup a0+aQ*Q, a0=0 and aQ=three delivered survivor terminals"
    (3 * fromIntegral count)
    (ledgerField (.terminalInstallations) aborted - terminalBefore)
  assertEqual
    "candidate activation belongs to successor work exactly once"
    (3 * fromIntegral count)
    (ledgerField (.candidateCreations) aborted - candidatesBefore)
  forM_ (Map.elems aborted) $ \(state, _) -> do
    assertEqual
      "all retained predecessor probes become terminal"
      count
      (length [() | witness <- Disappearance.probeWitnesses (startupDisappearanceState state), Disappearance.ProbeAbortedView {} <- [witness.witnessPhase]])
    assertEqual "no successor Open before its structural base" [] (requests isOpen state)
    assertEqual "membership retirement has not fabricated successor base readiness" Nothing (openContext state)
  assertReplay "exact retirement entry replay" retireEntry aborted
  (oracle5, staleRejected) <-
    foldM
      (\(oracle, network) envelope -> rejectStaleReport "stale old-generation Report" oracle network envelope)
      (oracle4, aborted)
      oldReports
  based <- drainNetwork Set.empty staleRejected successorMessages
  forM_ (Map.elems based) $ \(state, _) -> do
    assertBool "actual successor structural protocol establishes the Open context" (case openContext state of Just _ -> True; Nothing -> False)
    assertEqual "one successor Open per eligible retained subject" count (length (requests isOpen state))
  let successorOpens = concatMap (order . requests isOpen . fst) (Map.elems based)
  (oracle6, reopened) <- commitRequests "successor Open race" oracle5 based successorOpens
  let successorGeneration = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership oracle6)
  forM_ (Map.elems reopened) $ \(state, _) -> do
    let active = [witness | witness <- Disappearance.probeWitnesses (startupDisappearanceState state), witness.witnessPhase == Disappearance.ProbeCollectingView]
    assertEqual "each subject reopens exactly once" count (length active)
    forM_ active $ \witness -> do
      assertEqual
        "each successor source captures both surviving remote publication streams"
        2
        (length witness.witnessOutgoingPublicationMarkers)
      assertEqual
        "the ordinary successor fixture has no captured alignment subscriptions"
        []
        witness.witnessIncomingAlignmentCuts
      assertBool "successor gets a distinct probe identity" (Protocol.projectedProbeId witness.witnessProjectedProbe `notElem` oldProbes)
      assertEqual
        "reopened probe captures exact successor lineage"
        successorGeneration
        (Membership.heraldMembershipGenerationId (Protocol.projectedProbeMembership witness.witnessProjectedProbe))
  readyReports <- drainNetwork Set.empty reopened []
  let reports = concatMap (order . requests isReport . fst) (Map.elems readyReports)
  assertEqual "one fresh Report per surviving member and subject" (3 * count) (length reports)
  (oracle7, readyResolve) <- commitRequests "successor Report" oracle6 readyReports reports
  let dimensions = Disappearance.disappearanceWorkDimensions 3 6 0 0
      ordinary = Disappearance.disappearanceSuccessfulWorkBound dimensions
      cleanup = 3 * fromIntegral count
  assertEqual "ordinary successor coefficients remain (7,4,3,2)" (7 * 3 + 4 * 6) ordinary
  assertEqual
    "exact cleanup plus ordinary reopened work; predecessor evidence is never recounted"
    (cleanup + fromIntegral count * ordinary)
    (ledgerWork readyResolve - before)
  let resolves = concatMap (order . requests isResolve . fst) (Map.elems readyResolve)
  assertEqual "one stable Resolve intention per survivor and subject" (3 * count) (length resolves)
  (_, resolved) <- commitRequests "successor Resolve race" oracle7 readyResolve resolves
  forM_ (Map.elems resolved) $ \(state, _) -> do
    assertEqual "whole owner state remains valid" (Right ()) (validateHeraldState state)
    assertEqual
      "every reopened subject resolves"
      count
      (length [() | witness <- Disappearance.probeWitnesses (startupDisappearanceState state), Disappearance.ProbeResolvedView {} <- [witness.witnessPhase]])
    assertEqual "terminal workflows retain no live Oracle intentions" [] (requests isDisappearance state)
    (unchanged, effects) <- checked "unchanged successor scheduler" (Live.advanceDisappearanceWork state)
    assertBool "unchanged successor scheduler is state inert" (state == unchanged)
    assertBool "unchanged successor scheduler emits no effects" (effectBatchIsEmpty effects)

repeatedMembershipSchedule :: Assertion
repeatedMembershipSchedule = forM_ (permutations [retiredHerald, secondRetiredHerald]) $ \targets -> do
  let (seed, subjects) = liveRegularDisappearanceFixtureFor fiveHeraldGenesis 1
      local = Genesis.checkedLocalHeraldEpoch fiveHeraldGenesis
  assertEqual "one real LocalTake subject" 1 (length subjects)
  quiet <- prepareQuietNetwork seed
  oracle0 <- checked "initialize exact five-member Oracle" (initialOracle (fixtureCheckedOracleGenesisFor fiveHeraldGenesis))
  (oracle1, opened) <- commitRequests "original absence Open" oracle0 quiet (requests isOpen (stateAt local quiet))
  reported <- drainNetwork Set.empty opened []
  (oracleFinal, survivors) <- foldM retireAndReopen (oracle1, reported) targets
  assertEqual "complete Oracle generation history" 3 (length (Oracle.oracleMembershipHistory oracleFinal))
  assertEqual "only three immutable voter hosts survive" 3 (Map.size survivors)
  let reports = concatMap (requests isReport . fst) (Map.elems survivors)
  assertEqual "fresh evidence requires exactly the current three survivors" 3 (length reports)
  (oracleReady, ready) <- commitRequests "final survivor absence Reports" oracleFinal survivors reports
  (_, resolved) <- commitRequests "final survivor Resolve" oracleReady ready (concatMap (requests isResolve . fst) (Map.elems ready))
  forM_ (Map.elems resolved) $ \(state, _) -> do
    assertEqual "every whole owner remains valid after repeated contractions" (Right ()) (validateHeraldState state)
    let witnesses = Disappearance.probeWitnesses (startupDisappearanceState state)
    assertEqual "both predecessor probe tombstones remain" 2 (length [() | witness <- witnesses, Disappearance.ProbeAbortedView {} <- [witness.witnessPhase]])
    assertEqual "only the final current probe resolves" 1 (length [() | witness <- witnesses, Disappearance.ProbeResolvedView {} <- [witness.witnessPhase]])
  where
    submit description reporter command oracle network =
      commitNetwork description oracle network (manual reporter (20_000 + Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex oracle)) command)
    retireAndReopen (oracle, network) target = do
      let local = Genesis.checkedLocalHeraldEpoch fiveHeraldGenesis
          oldReports = concatMap (requests isReport . fst) (Map.elems network)
          oldIds = probeIds (stateAt local network)
          generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership oracle)
          terminalBefore = [(member, [(Protocol.projectedProbeId witness.witnessProjectedProbe, witness.witnessPhase) | witness <- Disappearance.probeWitnesses (startupDisappearanceState state), witness.witnessPhase /= Disappearance.ProbeCollectingView]) | (member, (state, _)) <- Map.toAscList network, member /= target]
      (openedOracle, openedNetwork, failureEntry) <- submit "next-generation failure Open" local (Command.openHeraldFailureProbeCommand target generation (Voter.voterConfigurationId (Voter.oracleVoterConfiguration oracle))) oracle network
      failureProbe <- one "new failure probe" [probe | OracleFailureProbeOpenedView probe _ _ _ <- views failureEntry]
      (reportedOracle, reportedNetwork) <-
        foldM
          ( \(currentOracle, currentNetwork) voter -> do
              (nextOracle, nextNetwork, _) <- submit "current-generation failure Report" voter (Command.reportHeraldFailureProbeCommand failureProbe (Voter.voterConfigurationId (Voter.oracleVoterConfiguration openedOracle)) Command.ProbeUnreachable) currentOracle currentNetwork
              pure (nextOracle, nextNetwork)
          )
          (openedOracle, openedNetwork)
          (take 2 (Map.keys network))
      let resolution = Membership.deriveFailureProbeResolutionId failureProbe Membership.RetireFailureProbeTarget
          envelope = manual local (20_000 + Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex reportedOracle)) (Command.retireHeraldEpochCommand resolution target)
      (retiredOracle, aborted, retirementEntry, messages) <- commitNetworkWithEffects "next canonical non-voter retirement" reportedOracle (Map.delete target reportedNetwork) envelope
      assertEqual "exactly the collecting probe is sealed" 1 (length [() | DisappearanceProbeAbortedView {} <- views retirementEntry])
      forM_ terminalBefore $ \(member, terminals) -> do
        let after = [(Protocol.projectedProbeId witness.witnessProjectedProbe, witness.witnessPhase) | witness <- Disappearance.probeWitnesses (startupDisappearanceState (stateAt member aborted))]
        assertBool "earlier terminal dispositions remain exact" (all (`elem` after) terminals)
      assertReplay "replayed intermediate retirement" retirementEntry aborted
      (staleOracle, staleNetwork) <-
        foldM
          (\(currentOracle, currentNetwork) envelopeBefore -> rejectStaleReport "late predecessor absence report" currentOracle currentNetwork envelopeBefore)
          (retiredOracle, aborted)
          oldReports
      based <- drainNetwork Set.empty staleNetwork messages
      forM_ (Map.elems based) $ \(state, _) -> assertBool "every survivor establishes the exact successor base" (case openContext state of Just _ -> True; Nothing -> False)
      (nextOracle, reopened) <- commitRequests "fresh successor absence Open race" staleOracle based (concatMap (requests isOpen . fst) (Map.elems based))
      forM_ (Map.elems reopened) $ \(state, _) -> do
        let active = [witness | witness <- Disappearance.probeWitnesses (startupDisappearanceState state), witness.witnessPhase == Disappearance.ProbeCollectingView]
        assertEqual "one fresh current probe" 1 (length active)
        forM_ active $ \witness -> do
          assertBool "fresh probe never reuses any predecessor identity" (Protocol.projectedProbeId witness.witnessProjectedProbe `notElem` oldIds)
          assertEqual "fresh probe captures exactly the surviving owners" (Map.keysSet reopened) (Set.fromList (foldr (:) [] (Protocol.projectedProbeMembers witness.witnessProjectedProbe)))
      ready <- drainNetwork Set.empty reopened []
      pure (nextOracle, ready)

-- A closed home is fenced before semantic admission, while stale reports
-- from surviving homes still retain their ordinary indexed rejection.
rejectStaleReport :: String -> Oracle.OracleState -> LiveNetwork -> Command.OracleEnvelope -> IO (Oracle.OracleState, LiveNetwork)
rejectStaleReport description oracle network envelope
  | Command.oracleEnvelopeHomeHeraldEpoch envelope `elem` Oracle.oracleRetiredHeralds oracle = do
      (nextOracle, outcome, effects) <- checked description (stepOracle envelope oracle)
      (replayed, retryOutcome, retryEffects) <- checked (description <> " replay") (stepOracle envelope nextOracle)
      assertEqual "closed-home report is explicitly obsolete" (OracleRequestRetired Receipt.OracleRequestHomeRetired) outcome
      assertEqual "closed-home report replay remains obsolete" outcome retryOutcome
      assertEqual "closed-home report cannot grow or change Oracle state" oracle replayed
      assertEqual "obsolete report emits no projection entry" [] (oracleEffects effects <> oracleEffects retryEffects)
      let request = Command.oracleEnvelopeRequestId envelope
      assertEqual "closed-home query is retired" (Just Receipt.OracleRequestHomeRetired) (Oracle.oracleRequestRetirement request nextOracle)
      assertEqual "obsolete report cannot recreate a receipt" Nothing (Oracle.oracleRequestReceipt request nextOracle)
      pure (nextOracle, network)
  | otherwise = do
      (nextOracle, nextNetwork, entry) <- commitNetwork description oracle network envelope
      command <- maybe (assertFailure "stale active-home Report entry must contain a command receipt") pure (appliedEntryCommand entry)
      assertBool "stale active-home report is rejected" (case Receipt.oracleReceiptResult (appliedEntryReceipt command) of Receipt.OracleRejected {} -> True; _ -> False)
      assertEqual "stale report emits no disappearance projection" [] (views entry)
      assertEqual "stale report performs no disappearance work" (ledgerWork network) (ledgerWork nextNetwork)
      pure (nextOracle, nextNetwork)

openContext :: HeraldState -> Maybe Protocol.DisappearanceOpenContext
openContext state = do
  base <- either (const Nothing) Just (Readiness.captureEstablishedStructuralBase (startupStructuralProgressState state))
  either (const Nothing) Just (Readiness.disappearanceOpenContextFromOwnerCaptures (Readiness.captureCurrentMembership (startupOracleProjectionState state)) base)

requests :: (Request.OracleSemanticIntent -> Bool) -> HeraldState -> [Command.OracleEnvelope]
requests predicate state =
  [ canonicalOracleEnvelopeValue (Request.oracleRequestWitnessEnvelope witness)
  | (_, witness) <- Client.oracleClientRequestEntries (startupOracleClientState state),
    Request.oracleRequestWitnessStatus witness == Request.OracleRequestAwaitingProjection,
    predicate (Request.oracleRequestWitnessIntent witness)
  ]

isOpen :: Request.OracleSemanticIntent -> Bool
isOpen Request.OpenDisappearanceProbeIntent {} = True
isOpen _ = False
isReport :: Request.OracleSemanticIntent -> Bool
isReport Request.ReportPredefinedAbsenceIntent {} = True
isReport _ = False
isResolve :: Request.OracleSemanticIntent -> Bool
isResolve Request.ResolveDisappearanceProbeIntent {} = True
isResolve _ = False
isDisappearance :: Request.OracleSemanticIntent -> Bool
isDisappearance intent = isOpen intent || isReport intent || isResolve intent

probeIds :: HeraldState -> [Domain.DisappearanceProbeId]
probeIds state = [Protocol.projectedProbeId witness.witnessProjectedProbe | witness <- Disappearance.probeWitnesses (startupDisappearanceState state)]

ledgerField :: (Disappearance.DisappearanceWorkLedger -> Word64) -> LiveNetwork -> Word64
ledgerField field = sum . fmap (field . Disappearance.disappearanceWorkLedger . startupDisappearanceState . fst) . Map.elems

ledgerWork :: LiveNetwork -> Word64
ledgerWork = ledgerField Disappearance.disappearanceLogicalWork

commitRequests :: String -> Oracle.OracleState -> LiveNetwork -> [Command.OracleEnvelope] -> IO (Oracle.OracleState, LiveNetwork)
commitRequests label oracle network envelopes =
  foldM
    ( \(currentOracle, currentNetwork) envelope -> do
        (nextOracle, nextNetwork, entry) <- commitNetwork label currentOracle currentNetwork envelope
        if any isResolvedEvent (views entry)
          then assertReplay "exact successor Resolve replay" entry nextNetwork
          else pure ()
        pure (nextOracle, nextNetwork)
    )
    (oracle, network)
    envelopes

assertReplay :: String -> AppliedOracleEntry -> LiveNetwork -> Assertion
assertReplay label entry network = forM_ (Map.elems network) $ \(state, binding) -> do
  (successor, effects) <- step label (OracleInput (OracleEntriesReceived binding (canonicalizeAppliedOracleEntry entry :| []))) state
  assertBool (label <> " leaves state unchanged") (state == successor)
  assertEqual
    (label <> " reoffers only the exact continuing watch and retained independent intentions")
    ( [RunOracleClientAction (WatchOracle binding (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState state))))]
        <> fmap RunOracleClientAction (Client.oracleClientRequestActions (startupOracleClientState state))
    )
    (effectBatchMembers effects)

isResolvedEvent :: OracleProjectionEventView -> Bool
isResolvedEvent DisappearanceProbeResolvedView {} = True
isResolvedEvent _ = False

manual :: Identity.HeraldEpoch -> Word64 -> Command.OracleCommand -> Command.OracleEnvelope
manual home ordinal = Command.oracleEnvelope (oracleClientRequestId home ordinal) Nothing home
views :: AppliedOracleEntry -> [OracleProjectionEventView]
views = fmap oracleProjectionEventView . appliedEntryProjectionEvents
stateAt :: Identity.HeraldEpoch -> LiveNetwork -> HeraldState
stateAt member network = maybe (error "missing live Herald") fst (Map.lookup member network)

fourHeraldGenesis :: Genesis.CheckedHeraldGenesis
fourHeraldGenesis = genesisWithNonVoters [(0x74, retiredHerald)]

fiveHeraldGenesis :: Genesis.CheckedHeraldGenesis
fiveHeraldGenesis = genesisWithNonVoters [(0x74, retiredHerald), (0x75, secondRetiredHerald)]

genesisWithNonVoters :: [(Word8, Identity.HeraldEpoch)] -> Genesis.CheckedHeraldGenesis
genesisWithNonVoters added = either (error . show) id (Genesis.checkHeraldGenesis manifest)
  where
    members =
      Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis
        <> [Genesis.HeraldMember (either (error . show) id (Identity.mkHeraldId (fixtureIdentifierBytes byte))) epoch | (byte, epoch) <- added]
    manifest =
      fixtureDeploymentManifest
        { Genesis.deploymentActiveHeralds = members,
          Genesis.deploymentOracleGenesis =
            (Genesis.deploymentOracleGenesis fixtureDeploymentManifest)
              { Genesis.oracleGenesisActiveHeralds = members
              }
        }

secondRetiredHerald :: Identity.HeraldEpoch
secondRetiredHerald = either (error . show) id (Identity.mkHeraldEpoch (fixtureIdentifierBytes 0x85))

retiredHerald :: Identity.HeraldEpoch
retiredHerald = either (error . show) id (Identity.mkHeraldEpoch (fixtureIdentifierBytes 0x84))

checked :: (Show problem) => String -> Either problem value -> IO value
checked label = either (assertFailure . ((label <> ": ") <>) . show) pure

one :: String -> [value] -> IO value
one _ [value] = pure value
one label values = assertFailure (label <> ": expected one, got " <> show (length values))
