{-# LANGUAGE OverloadedStrings #-}

module Step15ReferenceProperties
  ( tests,
    step15Genesis,
    targetH4,
    targetH5,
    retireLiveTarget,
  )
where

import Control.Monad (foldM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word16, Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
    controlIndex,
    controlIndexWord64,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (..),
    HeraldFailureProbeId,
    HeraldMembershipGenerationId,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    failureProbeResolutionDisposition,
    failureProbeResolutionProbeId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (HeraldRetired),
  )
import Eclips.Domain.Sort.Profile (profileCatalogueDigest)
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    CheckedInitialTopologyProjection,
    ConfiguredProcessBootstrap,
    HeraldMember (..),
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
    materializeConfiguredProcessBootstrap,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    RaftVoterBinding,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
  )
import Eclips.Oracle.Label qualified as Live
import Eclips.Oracle.Step15.Reference
  ( ReferenceAppliedOracleEntry,
    ReferenceCommandCanonicalProblem (..),
    ReferenceCommandResult (..),
    ReferenceFailureProbeView (..),
    ReferenceOracleCommand,
    ReferenceOracleEnvelope,
    ReferenceOracleReceipt,
    ReferenceOracleReceiptResult (..),
    ReferenceOracleRejection (..),
    ReferenceOracleState,
    ReferenceProbeResult (..),
    ReferenceProbeTerminalView (..),
    ReferenceProcessLifecycleView (..),
    ReferenceProjectionEventView (..),
    ReferenceStateCanonicalProblem (..),
    ReferenceSubmissionOutcome (..),
    ReferenceTerminalIntention,
    decodeReferenceOracleCommandCanonicalBytes,
    decodeReferenceOracleEnvelopeCanonicalBytes,
    decodeReferenceOracleStateCanonicalBytes,
    initialReferenceOracle,
    referenceAppliedEntryProjectionEvents,
    referenceDismissHeraldFailureProbeCommand,
    referenceOpenHeraldFailureProbeCommand,
    referenceOracleActiveProbeFor,
    referenceOracleCommandCanonicalBytes,
    referenceOracleCommandDigest,
    referenceOracleCommandTag,
    referenceOracleCurrentMembership,
    referenceOracleEnvelope,
    referenceOracleEnvelopeCanonicalBytes,
    referenceOracleEnvelopeDigest,
    referenceOracleFailureProbe,
    referenceOracleGreatestControlIndex,
    referenceOracleMembershipHistory,
    referenceOracleProcessRecord,
    referenceOracleReceiptControlIndex,
    referenceOracleReceiptResult,
    referenceOracleRequestCount,
    referenceOracleRetiredHeralds,
    referenceOracleStateCanonicalBytes,
    referenceOracleStateDigest,
    referenceOracleTerminalIntention,
    referenceProcessRecordLifecycle,
    referenceProjectionEventView,
    referenceReportHeraldFailureProbeCommand,
    referenceRetireHeraldEpochCommand,
    referenceTerminalIntentionCommand,
    referenceTerminalIntentionCommandDigest,
    referenceTerminalIntentionResolutionId,
    submitReferenceOracle,
  )
import Eclips.Oracle.Voter qualified as V
import Eclips.Raft.Genesis (RaftNativeConfiguration)
import Numeric (showHex)
import OracleFixtures
  ( checked,
    configuredProcess,
    fixtureBindings,
    fixtureBootstraps,
    fixtureCheckedGenesis,
    fixtureConfigurationDigest,
    fixtureDescriptors,
    fixtureHeraldEpoch,
    fixtureMembers,
    fixtureProcessEpoch,
    fixtureRaftConfiguration,
    fixtureRaftNodes,
    fixtureRemoteHeraldEpoch,
    fixtureSystemId,
    fixtureThirdHeraldEpoch,
    fixtureTopologyFor,
    heraldEpoch,
    heraldId,
    processEpochId,
    raftConfigurationForVoters,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
    arbitrary,
    elements,
    forAll,
    ioProperty,
    shuffle,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "Step-15 Oracle failure"
    [ testGroup
        "live cutover"
        [ testCase "the live command/watch/state path retires H4 canonically" caseLiveFailureCutover,
          testProperty "one-, two-, and three-voter failure reports use a strict majority at every prefix" propSmallVoterFailureMajorities,
          testProperty "repeated non-voter retirements match the independent model under stale traffic and replay" propRepeatedRetirementAgreement
        ],
      testGroup
        "canonical values"
        [ testCase "the failure command family occupies tags 9 through 12" caseCommandTags,
          testCase "all four commands and envelopes round-trip canonically" caseCommandRoundTrip,
          testCase "command codecs reject wrong domains and trailing bytes" caseCommandNegativeControls,
          testCase "command canonical bytes have fixed goldens" caseCommandGoldens,
          testCase "initial, dismissed, and retired states round-trip by checked replay" caseStateRoundTrip,
          testCase "state rejects another genesis, mutation, and trailing bytes" caseStateNegativeControls,
          testCase "state canonical streams have fixed digests" caseStateGoldens
        ],
      testGroup
        "request discipline"
        [ testCase "exact retry is immutable and conflicting reuse is protocol-rejected" caseRequestRetryAndConflict,
          testCase "fresh rejection is retained at the next control index" caseFreshRejectionRetained
        ],
      testGroup
        "probe admission and voting"
        [ testCase "equal-purpose Opens alias one first-index probe" caseOpenAliasing,
          testCase "an active non-voter may originate Open" caseNonVoterMayOpen,
          testProperty "Open alias is stable under caller arrival order" propOpenArrivalOrder,
          testCase "stale, foreign, and voter-host targets cannot open" caseOpenRejections,
          testCase "only voter hosts report and conflicting duplicate evidence rejects" caseReportAdmission,
          testCase "one reporter cannot authorize either terminal" caseMinorityCannotResolve,
          testCase "one reachable report cannot defeat two unreachable reports" caseMixedMajority,
          testProperty "every two-of-three reporter order derives one terminal intention" propReporterOrder,
          testCase "terminal command digest is semantic, not envelope-specific" caseSemanticTerminalDigest
        ],
      testGroup
        "terminal transitions"
        [ testCase "Dismiss closes one probe, deduplicates, and permits a later fresh probe" caseDismiss,
          testCase "terminal admission rejects wrong targets and ineligible proposers" caseTerminalAdmissionRejections,
          testCase "Retire contracts membership, supersedes peers, and ends resident processes" caseRetirement,
          testCase "stale work stays terminal while a fresh successor-generation probe may open" caseRetirementIsTerminal,
          testCase "physical stop and still-running partition have one canonical Oracle result" casePhysicalStatusIsNotOracleInput
        ]
    ]

propSmallVoterFailureMajorities :: Property
propSmallVoterFailureMajorities =
  forAll (elements [1, 2, 3]) $ \count ->
    forAll (shuffle (take count (fmap heraldMemberEpoch fixtureMembers))) $ \reporters ->
      forAll (vectorOf count arbitrary) $ \reachable ->
        let voters = take count fixtureRaftNodes
            bindings = take count fixtureBindings
            native = raftConfigurationForVoters voters 100000 300000 500000
            genesis = step15GenesisWithVoters bindings native
            initial = Live.initialOracleState genesis
            generation = heraldMembershipGenerationId (Live.oracleCurrentMembership initial)
            (opened, openedReceipt, _) = commitLive 1 Nothing fixtureHeraldEpoch (Live.openHeraldFailureProbeCommand targetH4 generation (V.voterConfigurationId (V.oracleVoterConfiguration initial))) initial
            probe = case Live.oracleReceiptFailureResult openedReceipt of
              Just (Live.FailureProbeOpened identifier) -> identifier
              other -> error ("small-voter Open failed: " <> show other)
            report (state, _) (reporter, isReachable) =
              let result = if isReachable then Live.ProbeReachable else Live.ProbeUnreachable
                  (successor, receipt, _) = commitLive 2 Nothing reporter (Live.reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration initial)) result) state
                  resolution = case Live.oracleReceiptFailureResult receipt of
                    Just (Live.FailureProbeReportRecorded _ ready) -> fmap (\(Live.FailureResolutionReady identifier _ _) -> failureProbeResolutionDisposition identifier) ready
                    other -> error ("small-voter Report failed: " <> show other)
               in (successor, resolution)
            observed = fmap snd (drop 1 (scanl report (opened, Nothing) (zip reporters reachable)))
            expected flags
              | 2 * length (filter id flags) > count = Just DismissFailureProbe
              | 2 * length (filter not flags) > count = Just RetireFailureProbeTarget
              | otherwise = Nothing
         in observed === fmap (expected . (`take` reachable)) [1 .. count]

propRepeatedRetirementAgreement :: Property
propRepeatedRetirementAgreement =
  forAll (shuffle [targetH4, targetH5]) $ \targets ->
    forAll (shuffle [fixtureHeraldEpoch, fixtureRemoteHeraldEpoch, fixtureThirdHeraldEpoch]) $ \reporters -> ioProperty $ do
      let initial = (initialReferenceOracle step15Genesis, Live.initialOracleState step15Genesis)
          original = currentGenerationId (fst initial)
          proposer = case reporters of
            reporter : _ -> reporter
            [] -> error "generated reporter permutation is empty"
          report probe state reporter =
            pairedFailureStep
              True
              reporter
              (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable)
              (Live.reportHeraldFailureProbeCommand probe capturedVoterId Live.ProbeUnreachable)
              state
          retire target probe =
            pairedFailureStep
              True
              proposer
              (referenceRetireHeraldEpochCommand probe target)
              (Live.retireHeraldEpochCommand (deriveFailureProbeResolutionId probe RetireFailureProbeTarget) target)
      case targets of
        [firstTarget, secondTarget] -> do
          (openedFirst, firstProbe) <- pairedOpen firstTarget initial
          (openedBoth, oldSecondProbe) <- pairedOpen secondTarget openedFirst
          reportedFirst <- foldM (report firstProbe) openedBoth (take 2 reporters)
          retiredFirst <- retire firstTarget firstProbe reportedFirst
          staleReport <-
            pairedFailureStep
              False
              proposer
              (referenceReportHeraldFailureProbeCommand oldSecondProbe ReferenceProbeUnreachable)
              (Live.reportHeraldFailureProbeCommand oldSecondProbe capturedVoterId Live.ProbeUnreachable)
              retiredFirst
          staleOpen <-
            pairedFailureStep
              False
              proposer
              (referenceOpenHeraldFailureProbeCommand secondTarget original)
              (Live.openHeraldFailureProbeCommand secondTarget original capturedVoterId)
              staleReport
          (reopened, secondProbe) <- pairedOpen secondTarget staleOpen
          assertBool "the new generation owns a fresh probe" (secondProbe /= oldSecondProbe)
          reportedSecond <- foldM (report secondProbe) reopened (take 2 (reverse reporters))
          retriedFirst <- retire firstTarget firstProbe reportedSecond
          retiredSecond <- retire secondTarget secondProbe retriedFirst
          staleHome <-
            pairedFailureStep
              False
              firstTarget
              (referenceOpenHeraldFailureProbeCommand secondTarget (currentGenerationId (fst retiredSecond)))
              (Live.openHeraldFailureProbeCommand secondTarget (currentGenerationId (fst retiredSecond)) capturedVoterId)
              retiredSecond
          staleTerminal <-
            pairedFailureStep
              False
              proposer
              (referenceRetireHeraldEpochCommand oldSecondProbe secondTarget)
              (Live.retireHeraldEpochCommand (deriveFailureProbeResolutionId oldSecondProbe RetireFailureProbeTarget) secondTarget)
              staleHome
          retriedOld <- retire firstTarget firstProbe staleTerminal
          final <- retire secondTarget secondProbe retriedOld
          assertEqual "all three exact generations remain retained" 3 (length (referenceOracleMembershipHistory (fst final)))
          assertEqual "tombstones retain retirement order" targets (referenceOracleRetiredHeralds (fst final))
          assertEqual "only immutable voter hosts remain active" (sort reporters) (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (referenceOracleCurrentMembership (fst final)))))
          assertEqual "reference canonical history replays the full finite trace" (Right (fst final)) (decodeReferenceOracleStateCanonicalBytes step15Genesis (referenceOracleStateCanonicalBytes (fst final)))
        other -> assertFailure ("generated retirement target shape: " <> show other)

pairedOpen :: HeraldEpoch -> (ReferenceOracleState, Live.OracleState) -> IO ((ReferenceOracleState, Live.OracleState), HeraldFailureProbeId)
pairedOpen target predecessor = do
  let generation = currentGenerationId (fst predecessor)
      probe = checkedProbe (controlIndexWord64 (referenceOracleGreatestControlIndex (fst predecessor)) + 1)
  successor <-
    pairedFailureStep
      True
      fixtureHeraldEpoch
      (referenceOpenHeraldFailureProbeCommand target generation)
      (Live.openHeraldFailureProbeCommand target generation capturedVoterId)
      predecessor
  pure (successor, probe)

pairedFailureStep :: Bool -> HeraldEpoch -> ReferenceOracleCommand -> Live.OracleCommand -> (ReferenceOracleState, Live.OracleState) -> IO (ReferenceOracleState, Live.OracleState)
pairedFailureStep accepted home referenceCommand liveCommand (reference, live)
  -- The append-only failure model predates explicit receipt lifetimes. Its
  -- independently derived terminal-home catalogue now fences obsolete input
  -- before semantic admission, retaining the historical records as reference
  -- evidence while exposing no result and consuming no new control index.
  | home `elem` referenceOracleRetiredHeralds reference = do
      let sequenceNumber = controlIndexWord64 (referenceOracleGreatestControlIndex reference) + 1
          request = oracleClientRequestId home sequenceNumber
          envelope = Live.oracleEnvelope request Nothing home liveCommand
          (nextLive, outcome) = Live.submitOracleState envelope live
          (replayed, replayOutcome) = Live.submitOracleState envelope nextLive
      assertEqual "closed-home reference never admits stale traffic" False accepted
      assertEqual
        "closed-home input has an explicit retired result"
        (Live.OracleSubmissionRetiredView Live.OracleRequestHomeRetired)
        (Live.oracleSubmissionOutcomeView outcome)
      assertEqual
        "closed-home replay has the same retired result"
        (Live.oracleSubmissionOutcomeView outcome)
        (Live.oracleSubmissionOutcomeView replayOutcome)
      assertEqual "closed-home traffic and replay preserve the complete live state" live replayed
      assertEqual "closed-home query is retired" (Just Live.OracleRequestHomeRetired) (Live.oracleRequestRetirement request nextLive)
      assertEqual "closed-home traffic cannot retain a fresh rejection" Nothing (Live.oracleRequestReceipt request nextLive)
      pure (reference, nextLive)
  | otherwise = do
      let sequenceNumber = controlIndexWord64 (referenceOracleGreatestControlIndex reference) + 1
          referenceEnvelope = mkEnvelope home sequenceNumber referenceCommand
          liveEnvelope = Live.oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home liveCommand
          (nextReference, referenceReceipt, referenceEntry) = committed referenceEnvelope reference
          (nextLive, liveReceipt, liveEntry) = commitLive sequenceNumber Nothing home liveCommand live
          referenceAccepted = case referenceOracleReceiptResult referenceReceipt of ReferenceOracleAccepted _ -> True; _ -> False
          liveAccepted = Live.oracleReceiptResult liveReceipt == Live.OracleAccepted
          referenceEvents = referenceProjectionEventView <$> referenceAppliedEntryProjectionEvents referenceEntry
          liveEvents = traverse normalizedFailureEvent (Live.oracleProjectionEventView <$> Live.appliedEntryProjectionEvents liveEntry)
          (referenceReplay, _) = submitReferenceOracle referenceEnvelope nextReference
          (liveReplay, _) = Live.submitOracleState liveEnvelope nextLive
      assertEqual "independent reference admission follows the scheduled authority" accepted referenceAccepted
      assertEqual "live admission agrees at every stale/active prefix" referenceAccepted liveAccepted
      assertEqual "the independent command constructor agrees with the live command" (referenceOracleCommandTag referenceCommand) (Live.oracleCommandTag liveCommand)
      assertEqual "canonical projection consequences agree at every prefix" (Just referenceEvents) liveEvents
      assertEqual "current authority agrees" (referenceOracleCurrentMembership nextReference) (Live.oracleCurrentMembership nextLive)
      assertEqual "the complete retained lineage agrees" (referenceOracleMembershipHistory nextReference) (Live.oracleMembershipHistory nextLive)
      assertEqual "terminal Herald identities agree" (referenceOracleRetiredHeralds nextReference) (Live.oracleRetiredHeralds nextLive)
      assertEqual "exact request replay cannot grow reference history" nextReference referenceReplay
      assertEqual "exact request replay cannot grow live history" (Live.oracleStateCanonicalBytes nextLive) (Live.oracleStateCanonicalBytes liveReplay)
      assertLiveEntryRoundTrip liveEntry
      pure (nextReference, nextLive)

normalizedFailureEvent :: Live.OracleProjectionEventView -> Maybe ReferenceProjectionEventView
normalizedFailureEvent = \case
  Live.OracleFailureProbeOpenedView probe target generation _ -> Just (ReferenceFailureProbeOpenedView probe target generation)
  Live.OracleFailureProbeReportRecordedView probe reporter result -> Just (ReferenceFailureProbeReportRecordedView probe reporter (case result of Live.ProbeReachable -> ReferenceProbeReachable; Live.ProbeUnreachable -> ReferenceProbeUnreachable))
  Live.OracleFailureProbeDismissedView probe resolution -> Just (ReferenceFailureProbeDismissedEventView probe resolution)
  Live.OracleFailureProbeRetiredView probe resolution successor -> Just (ReferenceFailureProbeRetiredEventView probe resolution successor)
  Live.OracleFailureProbeSupersededView probe successor -> Just (ReferenceFailureProbeSupersededEventView probe successor)
  Live.HeraldMembershipAdvancedView generation -> Just (ReferenceHeraldMembershipAdvancedView generation)
  Live.ProcessEpochEndedEventView process index reason -> Just (ReferenceProcessEpochEndedView process index reason)
  _ -> Nothing

caseLiveFailureCutover :: Assertion
caseLiveFailureCutover = do
  let initial = Live.initialOracleState step15Genesis
      generation =
        heraldMembershipGenerationId (Live.oracleCurrentMembership initial)
      (openedState, openedReceipt, openedEntry) =
        commitLive
          1
          (Just (controlIndex 0))
          fixtureHeraldEpoch
          (Live.openHeraldFailureProbeCommand targetH4 generation capturedVoterId)
          initial
  probe <- case Live.oracleReceiptFailureResult openedReceipt of
    Just (Live.FailureProbeOpened identifier) -> pure identifier
    result -> assertFailure ("live Open returned " <> show result)
  assertEqual
    "Open event"
    [Live.OracleFailureProbeOpenedView probe targetH4 generation (V.oracleVoterConfiguration initial)]
    (fmap Live.oracleProjectionEventView (Live.appliedEntryProjectionEvents openedEntry))
  assertLiveEntryRoundTrip openedEntry
  let (reportedOnce, _, _) =
        commitLive
          2
          (Just (controlIndex 1))
          fixtureHeraldEpoch
          (Live.reportHeraldFailureProbeCommand probe capturedVoterId Live.ProbeUnreachable)
          openedState
      (reportedTwice, secondReceipt, secondEntry) =
        commitLive
          1
          (Just (controlIndex 2))
          fixtureRemoteHeraldEpoch
          (Live.reportHeraldFailureProbeCommand probe capturedVoterId Live.ProbeUnreachable)
          reportedOnce
      resolution =
        deriveFailureProbeResolutionId probe RetireFailureProbeTarget
  case Live.oracleReceiptFailureResult secondReceipt of
    Just (Live.FailureProbeReportRecorded _ (Just (Live.FailureResolutionReady ready _ target))) -> do
      assertEqual "terminal resolution" resolution ready
      assertEqual "terminal target" targetH4 target
    result -> assertFailure ("second live report returned " <> show result)
  assertLiveEntryRoundTrip secondEntry
  let (liveRetiredState, retiredReceipt, retiredEntry) =
        commitLive
          3
          Nothing
          fixtureHeraldEpoch
          (Live.retireHeraldEpochCommand resolution targetH4)
          reportedTwice
      successor = Live.oracleCurrentMembership liveRetiredState
      eventViews =
        fmap Live.oracleProjectionEventView (Live.appliedEntryProjectionEvents retiredEntry)
      endedProcesses =
        [ process
        | Live.ProcessEpochEndedEventView process _ HeraldRetired <- eventViews
        ]
  case Live.oracleReceiptFailureResult retiredReceipt of
    Just (Live.HeraldRetiredResult retained successorId) -> do
      assertEqual "retained resolution" resolution retained
      assertEqual "receipt successor" (heraldMembershipGenerationId successor) successorId
    result -> assertFailure ("live Retire returned " <> show result)
  assertEqual
    "successor active membership"
    (filter (/= targetH4) initialEpochs)
    (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))
  assertEqual "retired catalogue" [targetH4] (Live.oracleRetiredHeralds liveRetiredState)
  assertEqual "resident process End set" (sort targetH4Processes) (sort endedProcesses)
  assertBool
    "retirement event carries the exact successor"
    (Live.HeraldMembershipAdvancedView successor `elem` eventViews)
  assertBool
    "authorizing probe is terminal"
    ( Live.OracleFailureProbeRetiredView
        probe
        resolution
        (heraldMembershipGenerationId successor)
        `elem` eventViews
    )
  mapM_
    ( \process ->
        case Live.oracleProcessRecord process liveRetiredState of
          Just record ->
            assertEqual
              "resident process lifecycle"
              (Live.ProcessRecordEndedView (controlIndex 4) HeraldRetired)
              (Live.processRecordLifecycle record)
          Nothing -> assertFailure "retired resident process disappeared"
    )
    targetH4Processes
  assertLiveEntryRoundTrip retiredEntry

commitLive ::
  Word64 ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  Live.OracleCommand ->
  Live.OracleState ->
  (Live.OracleState, Live.OracleReceipt, Live.AppliedOracleEntry)
commitLive sequenceNumber expected home command predecessor =
  case Live.oracleSubmissionOutcomeView
    outcome of
    Live.OracleSubmissionCommittedView receipt entry -> (successor, receipt, entry)
    other -> error ("expected committed live Oracle entry, got " <> show other)
  where
    (successor, outcome) =
      Live.submitOracleState
        ( Live.oracleEnvelope
            (oracleClientRequestId home sequenceNumber)
            expected
            home
            command
        )
        predecessor

retireLiveTarget :: HeraldEpoch -> Live.OracleState -> Live.OracleState
retireLiveTarget target initial = retired
  where
    generation = heraldMembershipGenerationId (Live.oracleCurrentMembership initial)
    commit home command state =
      let sequenceNumber = 10_000 + controlIndexWord64 (Live.oracleGreatestControlIndex state)
          (successor, committedReceipt, _) = commitLive sequenceNumber Nothing home command state
       in if Live.oracleReceiptResult committedReceipt == Live.OracleAccepted
            then (successor, committedReceipt)
            else error ("repeated retirement was rejected: " <> show (Live.oracleReceiptResult committedReceipt))
    (opened, receipt) = commit fixtureHeraldEpoch (Live.openHeraldFailureProbeCommand target generation (V.voterConfigurationId (V.oracleVoterConfiguration initial))) initial
    probe = case Live.oracleReceiptFailureResult receipt of
      Just (Live.FailureProbeOpened identifier) -> identifier
      other -> error ("failure Open result missing: " <> show other)
    (reportedOnce, _) = commit fixtureHeraldEpoch (Live.reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration initial)) Live.ProbeUnreachable) opened
    (reportedTwice, _) = commit fixtureRemoteHeraldEpoch (Live.reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration initial)) Live.ProbeUnreachable) reportedOnce
    (retired, _) = commit fixtureHeraldEpoch (Live.retireHeraldEpochCommand (deriveFailureProbeResolutionId probe RetireFailureProbeTarget) target) reportedTwice

assertLiveEntryRoundTrip :: Live.AppliedOracleEntry -> Assertion
assertLiveEntryRoundTrip entry =
  case Live.decodeAppliedOracleEntryCanonicalBytes bytes of
    Left problem -> assertFailure ("live applied entry at " <> show (Live.appliedEntryControlIndex entry) <> " failed decoding: " <> show problem)
    Right decoded -> do
      assertEqual "live decoded entry" entry (Live.decodedAppliedOracleEntryValue decoded)
      assertEqual "live canonical bytes" bytes (Live.decodedAppliedOracleEntryCanonicalBytes decoded)
  where
    bytes = Live.appliedOracleEntryCanonicalBytes entry

caseCommandTags :: Assertion
caseCommandTags =
  assertEqual
    "closed prospective tags"
    [9, 10, 11, 12]
    (fmap referenceOracleCommandTag canonicalCommands)

caseCommandRoundTrip :: Assertion
caseCommandRoundTrip = do
  mapM_
    ( \command ->
        assertEqual
          (show command)
          (Right command)
          ( decodeReferenceOracleCommandCanonicalBytes
              (referenceOracleCommandCanonicalBytes command)
          )
    )
    canonicalCommands
  mapM_
    ( \command ->
        let envelope = mkEnvelope fixtureHeraldEpoch 41 command
         in assertEqual
              (show envelope)
              (Right envelope)
              ( decodeReferenceOracleEnvelopeCanonicalBytes
                  (referenceOracleEnvelopeCanonicalBytes envelope)
              )
    )
    canonicalCommands

caseCommandNegativeControls :: Assertion
caseCommandNegativeControls = do
  let bytes = referenceOracleCommandCanonicalBytes canonicalOpen
      wrongFamily = replaceByte 8 0x58 bytes
  case decodeReferenceOracleCommandCanonicalBytes wrongFamily of
    Left ReferenceCommandCanonicalWrongDomain {} -> pure ()
    other -> assertFailure ("wrong command domain was not identified: " <> show other)
  assertBool
    "trailing command byte rejects"
    (isLeft (decodeReferenceOracleCommandCanonicalBytes (bytes <> "\NUL")))
  let envelopeBytes = referenceOracleEnvelopeCanonicalBytes (mkEnvelope fixtureHeraldEpoch 1 canonicalOpen)
  assertBool
    "trailing envelope byte rejects"
    (isLeft (decodeReferenceOracleEnvelopeCanonicalBytes (envelopeBytes <> "\NUL")))

caseCommandGoldens :: Assertion
caseCommandGoldens =
  assertEqual
    "fixed command canonical vectors"
    commandGoldens
    (fmap (renderHex . referenceOracleCommandCanonicalBytes) canonicalCommands)

caseStateRoundTrip :: Assertion
caseStateRoundTrip = do
  let initial = initialReferenceOracle step15Genesis
      dismissed = dismissedState
      retired = retiredState
  mapM_
    ( \state ->
        assertEqual
          "state round-trip"
          (Right state)
          ( decodeReferenceOracleStateCanonicalBytes
              step15Genesis
              (referenceOracleStateCanonicalBytes state)
          )
    )
    [initial, dismissed, retired]

caseStateNegativeControls :: Assertion
caseStateNegativeControls = do
  let state = retiredState
      bytes = referenceOracleStateCanonicalBytes state
      wrongFamily = replaceByte 8 0x58 bytes
      mutated = replaceByte (ByteString.length bytes - 1) 0xff bytes
  case decodeReferenceOracleStateCanonicalBytes step15Genesis wrongFamily of
    Left ReferenceStateCanonicalWrongDomain {} -> pure ()
    other -> assertFailure ("wrong state domain was not identified: " <> show other)
  assertEqual
    "another checked genesis rejects"
    (Left ReferenceStateCanonicalGenesisMismatch)
    (decodeReferenceOracleStateCanonicalBytes fixtureCheckedGenesis bytes)
  assertBool
    "semantic state mutation rejects"
    (isLeft (decodeReferenceOracleStateCanonicalBytes step15Genesis mutated))
  assertBool
    "trailing state byte rejects"
    (isLeft (decodeReferenceOracleStateCanonicalBytes step15Genesis (bytes <> "\NUL")))

caseStateGoldens :: Assertion
caseStateGoldens =
  assertEqual
    "fixed hashes of exact canonical state streams"
    stateGoldenDigests
    [ canonicalStreamDigest (referenceOracleStateCanonicalBytes (initialReferenceOracle step15Genesis)),
      canonicalStreamDigest (referenceOracleStateCanonicalBytes dismissedState),
      canonicalStreamDigest (referenceOracleStateCanonicalBytes retiredState)
    ]

caseRequestRetryAndConflict :: Assertion
caseRequestRetryAndConflict = do
  let initial = initialReferenceOracle step15Genesis
      envelope = mkEnvelope fixtureHeraldEpoch 1 canonicalOpen
      (opened, receipt, _) = committed envelope initial
      (retried, retryOutcome) = submitReferenceOracle envelope opened
      conflictEnvelope =
        referenceOracleEnvelope
          (requestId fixtureHeraldEpoch 1)
          Nothing
          fixtureHeraldEpoch
          (referenceReportHeraldFailureProbeCommand canonicalProbe ReferenceProbeReachable)
      (conflicted, conflictOutcome) = submitReferenceOracle conflictEnvelope opened
  assertEqual "retry leaves byte-identical state" opened retried
  assertEqual "retry returns retained receipt" (ReferenceSubmissionDuplicate receipt) retryOutcome
  assertEqual "conflict leaves byte-identical state" opened conflicted
  case conflictOutcome of
    ReferenceSubmissionProtocolRejected {} -> pure ()
    other -> assertFailure ("expected protocol rejection, got " <> show other)
  assertEqual "one request retained" 1 (referenceOracleRequestCount opened)
  assertEqual "one index consumed" (controlIndex 1) (referenceOracleGreatestControlIndex opened)

caseFreshRejectionRetained :: Assertion
caseFreshRejectionRetained = do
  let initial = initialReferenceOracle step15Genesis
      stale =
        referenceOracleEnvelope
          (requestId fixtureHeraldEpoch 2)
          (Just (controlIndex 99))
          fixtureHeraldEpoch
          canonicalOpen
      (successor, receipt, entry) = committed stale initial
  assertEqual "fresh rejection consumes index one" (controlIndex 1) (referenceOracleReceiptControlIndex receipt)
  assertEqual "fresh rejection is retained" 1 (referenceOracleRequestCount successor)
  assertEqual "fresh rejection has no semantic events" [] (referenceAppliedEntryProjectionEvents entry)
  assertRejected
    (ReferenceStaleExpectedControlIndex (controlIndex 99) (controlIndex 0))
    receipt

caseOpenAliasing :: Assertion
caseOpenAliasing = do
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      (afterFirst, firstReceipt, firstEntry) =
        committed
          (mkEnvelope fixtureHeraldEpoch 1 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          initial
      firstProbe = openedProbe firstReceipt
      (afterAlias, aliasReceipt, aliasEntry) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 1 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          afterFirst
  assertEqual "first index derives probe" (checkedProbe 1) firstProbe
  assertEqual "alias returns exact probe" firstProbe (openedProbe aliasReceipt)
  assertEqual "first Open projects" 1 (length (referenceAppliedEntryProjectionEvents firstEntry))
  assertEqual "alias has no semantic event" [] (referenceAppliedEntryProjectionEvents aliasEntry)
  assertEqual "one active exact-purpose probe" (Just firstProbe) (referenceOracleActiveProbeFor targetH4 generation afterAlias)

-- A non-voter cannot report or resolve, but §7.1/§7.2 deliberately permits
-- any active Herald to turn its exhausted local recovery episode into Open.
caseNonVoterMayOpen :: Assertion
caseNonVoterMayOpen = do
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      (_, receipt, _) =
        committed
          (mkEnvelope targetH5 1 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          initial
  assertEqual "non-voter Open is admitted" (checkedProbe 1) (openedProbe receipt)

propOpenArrivalOrder :: Positive Word16 -> Positive Word16 -> Property
propOpenArrivalOrder (Positive firstSequence) (Positive secondSequence) =
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      first home sequenceNumber =
        mkEnvelope home (fromIntegral sequenceNumber) (referenceOpenHeraldFailureProbeCommand targetH4 generation)
      run left right =
        let (state1, receipt1, _) = committed left initial
            (state2, receipt2, _) = committed right state1
         in (openedProbe receipt1, openedProbe receipt2, referenceOracleFailureProbe (openedProbe receipt1) state2)
      leftOrder = run (first fixtureHeraldEpoch firstSequence) (first fixtureRemoteHeraldEpoch secondSequence)
      rightOrder = run (first fixtureRemoteHeraldEpoch secondSequence) (first fixtureHeraldEpoch firstSequence)
   in leftOrder === rightOrder

caseOpenRejections :: Assertion
caseOpenRejections = do
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      wrongGeneration = checked "foreign membership ID" (mkHeraldMembershipGenerationId (ByteString.replicate 32 0xab))
  assertFreshRejected
    (ReferenceFailureProbeTargetHostsVoter fixtureHeraldEpoch)
    (mkEnvelope targetH4 1 (referenceOpenHeraldFailureProbeCommand fixtureHeraldEpoch generation))
    initial
  assertFreshRejected
    (ReferenceFailureProbeTargetNotActive foreignHerald)
    (mkEnvelope targetH4 2 (referenceOpenHeraldFailureProbeCommand foreignHerald generation))
    initial
  assertFreshRejected
    (ReferenceStaleMembershipGeneration wrongGeneration generation)
    (mkEnvelope targetH4 3 (referenceOpenHeraldFailureProbeCommand targetH4 wrongGeneration))
    initial

caseReportAdmission :: Assertion
caseReportAdmission = do
  let (opened, probe) = openTarget targetH4
  assertFreshRejected
    (ReferenceIneligibleFailureReporter targetH4)
    (mkEnvelope targetH4 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
    opened
  let (oneReport, firstReceipt, firstEntry) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          opened
      (equalRepeat, repeatReceipt, repeatEntry) =
        committed
          (mkEnvelope fixtureHeraldEpoch 3 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          oneReport
  assertEqual "one voter is below threshold" Nothing (reportedIntention firstReceipt)
  assertEqual "equal report is accepted" Nothing (reportedIntention repeatReceipt)
  assertEqual "equal report does not duplicate semantic event" [] (referenceAppliedEntryProjectionEvents repeatEntry)
  assertEqual "first report projects once" 1 (length (referenceAppliedEntryProjectionEvents firstEntry))
  assertFreshRejected
    ( ReferenceConflictingFailureReport
        probe
        fixtureHeraldEpoch
        ReferenceProbeReachable
        ReferenceProbeUnreachable
    )
    (mkEnvelope fixtureHeraldEpoch 4 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
    equalRepeat

caseMinorityCannotResolve :: Assertion
caseMinorityCannotResolve = do
  let (opened, probe) = openTarget targetH4
      (oneReport, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
          opened
  assertFreshRejected
    (ReferenceFailureProbeThresholdNotReached probe RetireFailureProbeTarget)
    (mkEnvelope fixtureHeraldEpoch 3 (referenceRetireHeraldEpochCommand probe targetH4))
    oneReport

caseMixedMajority :: Assertion
caseMixedMajority = do
  let (opened, probe) = openTarget targetH4
      (oneReachable, firstReceipt, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          opened
      (split, secondReceipt, _) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
          oneReachable
      (majorityState, thirdReceipt, _) =
        committed
          (mkEnvelope fixtureThirdHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
          split
  assertEqual "one reachable is below threshold" Nothing (reportedIntention firstReceipt)
  assertEqual "one vote each is below threshold" Nothing (reportedIntention secondReceipt)
  assertEqual
    "two unreachable reports derive only Retire"
    ( Just
        (deriveFailureProbeResolutionId probe RetireFailureProbeTarget)
    )
    (referenceTerminalIntentionResolutionId <$> reportedIntention thirdReceipt)
  assertBool "threshold is represented in state" (referenceOracleTerminalIntention probe majorityState /= Nothing)

propReporterOrder :: Property
propReporterOrder =
  forAll
    ( elements
        [ [fixtureHeraldEpoch, fixtureRemoteHeraldEpoch],
          [fixtureRemoteHeraldEpoch, fixtureHeraldEpoch],
          [fixtureHeraldEpoch, fixtureThirdHeraldEpoch],
          [fixtureThirdHeraldEpoch, fixtureHeraldEpoch],
          [fixtureRemoteHeraldEpoch, fixtureThirdHeraldEpoch],
          [fixtureThirdHeraldEpoch, fixtureRemoteHeraldEpoch]
        ]
    )
    $ \reporters ->
      let (opened, probe) = openTarget targetH4
          final =
            foldl
              ( \state (sequenceNumber, reporter) ->
                  let (successor, _, _) =
                        committed
                          ( mkEnvelope
                              reporter
                              sequenceNumber
                              (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable)
                          )
                          state
                   in successor
              )
              opened
              (zip [10 ..] reporters)
          actual = referenceTerminalIntentionResolutionId <$> referenceOracleTerminalIntention probe final
          expected = Just (deriveFailureProbeResolutionId probe RetireFailureProbeTarget)
       in actual === expected

caseSemanticTerminalDigest :: Assertion
caseSemanticTerminalDigest = do
  let (reported, probe, intention) = unreachableMajority targetH4
      command = referenceTerminalIntentionCommand intention
      thresholdIndex = referenceOracleGreatestControlIndex reported
      firstEnvelope = mkExpectedEnvelope fixtureHeraldEpoch 20 thresholdIndex command
      secondEnvelope = mkExpectedEnvelope fixtureRemoteHeraldEpoch 20 thresholdIndex command
  assertEqual
    "intention retains the semantic command digest"
    (referenceOracleCommandDigest command)
    (referenceTerminalIntentionCommandDigest intention)
  assertBool
    "per-envelope digests retain reporter/request identity"
    (referenceOracleEnvelopeDigest firstEnvelope /= referenceOracleEnvelopeDigest secondEnvelope)
  let (firstState, firstReceipt, _) = committed firstEnvelope reported
      (_, secondReceipt, secondEntry) = committed secondEnvelope firstState
  assertBool
    "the retained threshold index is stale after first terminal commit"
    (thresholdIndex /= referenceOracleGreatestControlIndex firstState)
  assertEqual "equal terminal commands return equal semantic result" (acceptedResult firstReceipt) (acceptedResult secondReceipt)
  assertEqual "second terminal command has no semantic event" [] (referenceAppliedEntryProjectionEvents secondEntry)
  assertEqual "probe remained exact" probe (resolutionProbeFromIntention intention)

caseDismiss :: Assertion
caseDismiss = do
  let (opened, probe) = openTarget targetH4
      (afterFirst, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          opened
      (reported, reportReceipt, _) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          afterFirst
      intention = checkedMaybe "Dismiss intention" (reportedIntention reportReceipt)
      command = referenceTerminalIntentionCommand intention
      thresholdIndex = referenceOracleGreatestControlIndex reported
      (dismissed, firstReceipt, firstEntry) = committed (mkExpectedEnvelope fixtureHeraldEpoch 3 thresholdIndex command) reported
      (deduplicated, secondReceipt, secondEntry) = committed (mkExpectedEnvelope fixtureRemoteHeraldEpoch 3 thresholdIndex command) dismissed
      generation = currentGenerationId dismissed
      (reopened, reopenReceipt, _) =
        committed
          (mkEnvelope fixtureThirdHeraldEpoch 3 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          deduplicated
  assertEqual "Dismiss result" (acceptedResult firstReceipt) (acceptedResult secondReceipt)
  assertEqual "Dismiss projects once" 1 (length (referenceAppliedEntryProjectionEvents firstEntry))
  assertEqual "equal Dismiss is semantic no-op" [] (referenceAppliedEntryProjectionEvents secondEntry)
  assertBool "new probe has a fresh index-derived identity" (openedProbe reopenReceipt /= probe)
  assertEqual "new probe is active" (Just (openedProbe reopenReceipt)) (referenceOracleActiveProbeFor targetH4 generation reopened)

caseTerminalAdmissionRejections :: Assertion
caseTerminalAdmissionRejections = do
  let (reported, probe, _) = unreachableMajority targetH4
  assertFreshRejected
    (ReferenceRetirementTargetMismatch targetH5 targetH4)
    (mkEnvelope fixtureHeraldEpoch 30 (referenceRetireHeraldEpochCommand probe targetH5))
    reported
  assertFreshRejected
    (ReferenceIneligibleFailureReporter targetH4)
    (mkEnvelope targetH4 30 (referenceRetireHeraldEpochCommand probe targetH4))
    reported

caseRetirement :: Assertion
caseRetirement = do
  let fixture = retirementFixture
      state = retirementState fixture
      successor = referenceOracleCurrentMembership state
      initialGenerationId = currentGenerationId (initialReferenceOracle step15Genesis)
      active = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)
      authorizer = retirementAuthorizer fixture
      superseded = retirementSuperseded fixture
      events = retirementEvents fixture
      ended =
        [ process
        | ReferenceProcessEpochEndedView process _ HeraldRetired <- events
        ]
  assertEqual "one generation successor" 2 (length (referenceOracleMembershipHistory state))
  assertEqual
    "membership history is exact lineage order"
    [initialGenerationId, heraldMembershipGenerationId successor]
    (fmap heraldMembershipGenerationId (referenceOracleMembershipHistory state))
  assertEqual "retired tombstone" [targetH4] (referenceOracleRetiredHeralds state)
  assertEqual "target removed exactly" (sort (filter (/= targetH4) initialEpochs)) (sort active)
  case referenceOracleFailureProbe authorizer state of
    Just (ReferenceFailureProbeView _ _ _ _ _ (Just ReferenceProbeRetiredView {})) -> pure ()
    other -> assertFailure ("authorizer was not retired: " <> show other)
  case referenceOracleFailureProbe superseded state of
    Just (ReferenceFailureProbeView _ _ _ _ _ (Just ReferenceProbeMembershipSupersededView {})) -> pure ()
    other -> assertFailure ("other genesis probe was not superseded: " <> show other)
  assertEqual "only exact target-resident processes end, sorted" (sort targetH4Processes) ended
  mapM_
    ( \process ->
        assertEqual
          ("target process ended: " <> show process)
          (Just (ReferenceProcessEndedView (retirementIndex fixture) HeraldRetired))
          (referenceProcessRecordLifecycle <$> referenceOracleProcessRecord process state)
    )
    targetH4Processes
  assertEqual
    "survivor process remains live"
    (Just ReferenceProcessLiveView)
    (referenceProcessRecordLifecycle <$> referenceOracleProcessRecord fixtureProcessEpoch state)
  case events of
    ReferenceHeraldMembershipAdvancedView {} : rest ->
      assertBool
        "all probe terminals precede sorted End events"
        (probeEventsPrecedeEnds rest)
    _ -> assertFailure ("membership event was not first: " <> show events)

caseRetirementIsTerminal :: Assertion
caseRetirementIsTerminal = do
  let fixture = retirementFixture
      state = retirementState fixture
      authorizer = retirementAuthorizer fixture
      superseded = retirementSuperseded fixture
      generation = currentGenerationId state
  assertFreshRejectedMatching
    isTerminalRejection
    (mkEnvelope fixtureHeraldEpoch 30 (referenceReportHeraldFailureProbeCommand superseded ReferenceProbeUnreachable))
    state
  assertFreshRejectedMatching
    isTerminalRejection
    (mkEnvelope fixtureRemoteHeraldEpoch 30 (referenceRetireHeraldEpochCommand superseded targetH5))
    state
  let (reopened, reopenedReceipt, _) =
        committed (mkEnvelope fixtureHeraldEpoch 31 (referenceOpenHeraldFailureProbeCommand targetH5 generation)) state
  case referenceOracleReceiptResult reopenedReceipt of
    ReferenceOracleAccepted (ReferenceProbeOpened fresh) ->
      assertBool "the new probe cannot reuse the superseded probe" (fresh /= superseded)
    result -> assertFailure ("fresh successor-generation Open failed: " <> show result)
  let command = referenceRetireHeraldEpochCommand authorizer targetH4
      (deduplicated, _, entry) = committed (mkEnvelope fixtureThirdHeraldEpoch 31 command) reopened
  assertEqual "equal retirement emits no duplicate consequence" [] (referenceAppliedEntryProjectionEvents entry)
  assertEqual "membership remains the sole successor" 2 (length (referenceOracleMembershipHistory deduplicated))

casePhysicalStatusIsNotOracleInput :: Assertion
casePhysicalStatusIsNotOracleInput = do
  let stoppedTrace = TargetPhysicallyStopped ["EOF", "dial timeout"]
      partitionTrace = TargetStillRunningPartition ["heartbeat timeout", "connection refused"]
      stoppedInputs = erasePhysicalTrace stoppedTrace
      partitionInputs = erasePhysicalTrace partitionTrace
      stopped = runRetirementSchedule stoppedTrace
      isolatedButRunning = runRetirementSchedule partitionTrace
  assertBool "outer traces are physically distinct" (stoppedTrace /= partitionTrace)
  assertEqual "physical observations erase to the same Oracle evidence" stoppedInputs partitionInputs
  assertEqual
    "physical execution status cannot affect canonical Oracle state"
    (referenceOracleStateCanonicalBytes stopped)
    (referenceOracleStateCanonicalBytes isolatedButRunning)
  assertEqual
    "state digest agrees"
    (referenceOracleStateDigest stopped)
    (referenceOracleStateDigest isolatedButRunning)

-- Deterministic fixture and schedules --------------------------------------

data RetirementFixture
  = RetirementFixture
      ReferenceOracleState
      HeraldFailureProbeId
      HeraldFailureProbeId
      ControlIndex
      [ReferenceProjectionEventView]

retirementFixture :: RetirementFixture
retirementFixture =
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      (withH4, h4Receipt, _) =
        committed
          (mkEnvelope targetH4 1 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          initial
      h4Probe = openedProbe h4Receipt
      (withH5, h5Receipt, _) =
        committed
          (mkEnvelope targetH5 1 (referenceOpenHeraldFailureProbeCommand targetH5 generation))
          withH4
      h5Probe = openedProbe h5Receipt
      (oneReport, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand h4Probe ReferenceProbeUnreachable))
          withH5
      (reported, reportReceipt, _) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand h4Probe ReferenceProbeUnreachable))
          oneReport
      intention = checkedMaybe "retirement intention" (reportedIntention reportReceipt)
      (retired, terminalReceipt, terminalEntry) =
        committed
          (mkEnvelope fixtureHeraldEpoch 3 (referenceTerminalIntentionCommand intention))
          reported
   in case acceptedResult terminalReceipt of
        ReferenceHeraldRetiredResult {} ->
          RetirementFixture
            retired
            h4Probe
            h5Probe
            (referenceOracleReceiptControlIndex terminalReceipt)
            (fmap referenceProjectionEventView (referenceAppliedEntryProjectionEvents terminalEntry))
        other -> error ("retirement fixture terminal result: " <> show other)

retirementState :: RetirementFixture -> ReferenceOracleState
retirementState (RetirementFixture state _ _ _ _) = state

retirementAuthorizer :: RetirementFixture -> HeraldFailureProbeId
retirementAuthorizer (RetirementFixture _ probe _ _ _) = probe

retirementSuperseded :: RetirementFixture -> HeraldFailureProbeId
retirementSuperseded (RetirementFixture _ _ probe _ _) = probe

retirementIndex :: RetirementFixture -> ControlIndex
retirementIndex (RetirementFixture _ _ _ index _) = index

retirementEvents :: RetirementFixture -> [ReferenceProjectionEventView]
retirementEvents (RetirementFixture _ _ _ _ events) = events

retiredState :: ReferenceOracleState
retiredState = retirementState retirementFixture

dismissedState :: ReferenceOracleState
dismissedState =
  let (opened, probe) = openTarget targetH4
      (afterFirst, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          opened
      (reported, reportReceipt, _) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable))
          afterFirst
      intention = checkedMaybe "Dismiss state intention" (reportedIntention reportReceipt)
      (dismissed, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 3 (referenceTerminalIntentionCommand intention))
          reported
   in dismissed

data TargetPhysicalTrace
  = TargetPhysicallyStopped [String]
  | TargetStillRunningPartition [String]
  deriving stock (Eq, Show)

erasePhysicalTrace :: TargetPhysicalTrace -> [(HeraldEpoch, ReferenceProbeResult)]
erasePhysicalTrace trace = case trace of
  TargetPhysicallyStopped _ -> unreachableEvidence
  TargetStillRunningPartition _ -> unreachableEvidence
  where
    unreachableEvidence =
      [ (fixtureHeraldEpoch, ReferenceProbeUnreachable),
        (fixtureRemoteHeraldEpoch, ReferenceProbeUnreachable)
      ]

runRetirementSchedule :: TargetPhysicalTrace -> ReferenceOracleState
runRetirementSchedule physicalTrace =
  let initial = initialReferenceOracle step15Genesis
      generation = currentGenerationId initial
      (opened, openReceipt, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 1 (referenceOpenHeraldFailureProbeCommand targetH4 generation))
          initial
      probe = openedProbe openReceipt
      reported =
        foldl
          ( \state (sequenceNumber, (reporter, result)) ->
              let (successor, _, _) =
                    committed
                      (mkEnvelope reporter sequenceNumber (referenceReportHeraldFailureProbeCommand probe result))
                      state
               in successor
          )
          opened
          (zip [2 ..] (erasePhysicalTrace physicalTrace))
      intention = checkedMaybe "erased physical trace intention" (referenceOracleTerminalIntention probe reported)
      (retired, _, _) =
        committed
          (mkEnvelope fixtureThirdHeraldEpoch 4 (referenceTerminalIntentionCommand intention))
          reported
   in retired

unreachableMajority ::
  HeraldEpoch ->
  (ReferenceOracleState, HeraldFailureProbeId, ReferenceTerminalIntention)
unreachableMajority target =
  let (opened, probe) = openTarget target
      (oneReport, _, _) =
        committed
          (mkEnvelope fixtureHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
          opened
      (reported, receipt, _) =
        committed
          (mkEnvelope fixtureRemoteHeraldEpoch 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable))
          oneReport
   in (reported, probe, checkedMaybe "unreachable majority intention" (reportedIntention receipt))

openTarget :: HeraldEpoch -> (ReferenceOracleState, HeraldFailureProbeId)
openTarget target =
  let initial = initialReferenceOracle step15Genesis
      command = referenceOpenHeraldFailureProbeCommand target (currentGenerationId initial)
      (opened, receipt, _) = committed (mkEnvelope target 1 command) initial
   in (opened, openedProbe receipt)

committed ::
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  (ReferenceOracleState, ReferenceOracleReceipt, ReferenceAppliedOracleEntry)
committed envelope state =
  case submitReferenceOracle envelope state of
    (successor, ReferenceSubmissionCommitted receipt entry) ->
      (successor, receipt, entry)
    (_, outcome) -> error ("fixture request did not commit: " <> show outcome)

mkEnvelope :: HeraldEpoch -> Word64 -> ReferenceOracleCommand -> ReferenceOracleEnvelope
mkEnvelope home sequenceNumber =
  referenceOracleEnvelope (requestId home sequenceNumber) Nothing home

mkExpectedEnvelope ::
  HeraldEpoch ->
  Word64 ->
  ControlIndex ->
  ReferenceOracleCommand ->
  ReferenceOracleEnvelope
mkExpectedEnvelope home sequenceNumber expected =
  referenceOracleEnvelope (requestId home sequenceNumber) (Just expected) home

requestId :: HeraldEpoch -> Word64 -> OracleClientRequestId
requestId = oracleClientRequestId

openedProbe :: ReferenceOracleReceipt -> HeraldFailureProbeId
openedProbe receipt = case acceptedResult receipt of
  ReferenceProbeOpened probe -> probe
  other -> error ("expected Open receipt, got " <> show other)

reportedIntention ::
  ReferenceOracleReceipt -> Maybe ReferenceTerminalIntention
reportedIntention receipt = case acceptedResult receipt of
  ReferenceProbeReportRecorded _ intention -> intention
  other -> error ("expected Report receipt, got " <> show other)

acceptedResult :: ReferenceOracleReceipt -> ReferenceCommandResult
acceptedResult receipt = case referenceOracleReceiptResult receipt of
  ReferenceOracleAccepted result -> result
  other -> error ("expected accepted receipt, got " <> show other)

assertRejected :: ReferenceOracleRejection -> ReferenceOracleReceipt -> Assertion
assertRejected expected receipt =
  assertEqual
    "retained rejection"
    (ReferenceOracleRejected expected)
    (referenceOracleReceiptResult receipt)

assertFreshRejected ::
  ReferenceOracleRejection ->
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  Assertion
assertFreshRejected expected envelope state = do
  let (_, receipt, _) = committed envelope state
  assertRejected expected receipt

assertFreshRejectedMatching ::
  (ReferenceOracleRejection -> Bool) ->
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  Assertion
assertFreshRejectedMatching predicate envelope state = do
  let (_, receipt, _) = committed envelope state
  case referenceOracleReceiptResult receipt of
    ReferenceOracleRejected rejection
      | predicate rejection -> pure ()
    other -> assertFailure ("unexpected rejection result: " <> show other)

isTerminalRejection :: ReferenceOracleRejection -> Bool
isTerminalRejection ReferenceFailureProbeTerminal {} = True
isTerminalRejection _ = False

currentGenerationId :: ReferenceOracleState -> HeraldMembershipGenerationId
currentGenerationId =
  heraldMembershipGenerationId . referenceOracleCurrentMembership

resolutionProbeFromIntention ::
  ReferenceTerminalIntention -> HeraldFailureProbeId
resolutionProbeFromIntention =
  failureProbeResolutionProbeId . referenceTerminalIntentionResolutionId

probeEventsPrecedeEnds :: [ReferenceProjectionEventView] -> Bool
probeEventsPrecedeEnds = go False
  where
    go _ [] = True
    go seenEnd (event : rest) = case event of
      ReferenceProcessEpochEndedView {} -> go True rest
      _ -> not seenEnd && go False rest

isLeft :: Either left right -> Bool
isLeft = either (const True) (const False)

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error (context <> ": missing")) id

checkedProbe :: Word64 -> HeraldFailureProbeId
checkedProbe index = checked "probe" (deriveHeraldFailureProbeId (controlIndex index))

canonicalProbe :: HeraldFailureProbeId
canonicalProbe = checkedProbe 7

canonicalGeneration :: HeraldMembershipGenerationId
canonicalGeneration = currentGenerationId (initialReferenceOracle step15Genesis)

canonicalOpen, canonicalReport, canonicalDismiss, canonicalRetire :: ReferenceOracleCommand
canonicalOpen = referenceOpenHeraldFailureProbeCommand targetH4 canonicalGeneration
canonicalReport = referenceReportHeraldFailureProbeCommand canonicalProbe ReferenceProbeUnreachable
canonicalDismiss = referenceDismissHeraldFailureProbeCommand canonicalProbe
canonicalRetire = referenceRetireHeraldEpochCommand canonicalProbe targetH4

canonicalCommands :: [ReferenceOracleCommand]
canonicalCommands = [canonicalOpen, canonicalReport, canonicalDismiss, canonicalRetire]

step15Genesis :: CheckedOracleGenesis
step15Genesis = step15GenesisWithVoters fixtureBindings fixtureRaftConfiguration

step15GenesisWithVoters :: [RaftVoterBinding] -> RaftNativeConfiguration -> CheckedOracleGenesis
step15GenesisWithVoters bindings native =
  checked
    "Step-15 Oracle genesis"
    ( checkOracleGenesis
        ( oracleGenesis
            fixtureSystemId
            step15Members
            profileCatalogueDigest
            fixtureDescriptors
            step15Bootstraps
            fixtureConfigurationDigest
            step15Topology
            (deriveInitialProjectionDigest step15Bootstraps step15Topology)
            bindings
            native
            (deriveRaftConfigurationDigest fixtureSystemId bindings native)
        )
    )

step15Members :: [HeraldMember]
step15Members =
  fixtureMembers
    <> [ HeraldMember (heraldId 16) targetH4,
         HeraldMember (heraldId 18) targetH5
       ]

initialEpochs :: [HeraldEpoch]
initialEpochs = fmap heraldMemberEpoch step15Members

targetH4, targetH5, foreignHerald :: HeraldEpoch
targetH4 = heraldEpoch 17
targetH5 = heraldEpoch 19
foreignHerald = heraldEpoch 250

targetH4Processes :: [ProcessEpochId]
targetH4Processes = [processEpochId 232, processEpochId 236]

targetConfiguredBootstraps :: [ConfiguredProcessBootstrap]
targetConfiguredBootstraps =
  [ configuredProcess 230 231 (processEpochId 232) targetH4 60,
    configuredProcess 234 235 (processEpochId 236) targetH4 90
  ]

targetAppliedBootstraps :: [AppliedProcessBootstrap]
targetAppliedBootstraps =
  fmap
    ( checked "Step-15 target bootstrap"
        . materializeConfiguredProcessBootstrap
          (controlIndex 0)
          (genesisPredefinedOccurrenceSet fixtureSystemId)
    )
    targetConfiguredBootstraps

step15Bootstraps :: [AppliedProcessBootstrap]
step15Bootstraps = fixtureBootstraps <> targetAppliedBootstraps

step15Topology :: CheckedInitialTopologyProjection
step15Topology = fixtureTopologyFor fixtureSystemId step15Members step15Bootstraps

replaceByte :: Int -> Word -> ByteString -> ByteString
replaceByte offset byte bytes =
  ByteString.take offset bytes
    <> ByteString.singleton (fromIntegral byte)
    <> ByteString.drop (offset + 1) bytes

canonicalStreamDigest :: ByteString -> String
canonicalStreamDigest = renderHex . SHA256.hash

renderHex :: ByteString -> String
renderHex = concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte = case showHex byte "" of
  [digit] -> ['0', digit]
  digits -> digits

-- Filled from the canonical implementation and intentionally reviewed as raw
-- byte vectors rather than generated at test time.
commandGoldens :: [String]
commandGoldens =
  [ "000000000000001c45434c4950532d5354455031352d4f5241434c452d434f4d4d414e4409000000000000005000000000000000201112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f3000000000000000201c3322afd565255f9764d4f7d4e31d00a2affc6a14bfc3536e7838cc86eb5d26",
    "000000000000001c45434c4950532d5354455031352d4f5241434c452d434f4d4d414e440a00000000000000620000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db272000000000000000701",
    "000000000000001c45434c4950532d5354455031352d4f5241434c452d434f4d4d414e440b00000000000000c600000000000000be000000000000002c45434c4950532d484552414c442d4641494c5552452d50524f42452d5245534f4c5554494f4e2d56414c554500000000000000208879b33bd8a5cbc9964e0982d0f5b146c060644ed90335f9c3954df20349ee530000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db272000000000000000700",
    "000000000000001c45434c4950532d5354455031352d4f5241434c452d434f4d4d414e440c00000000000000ee00000000000000be000000000000002c45434c4950532d484552414c442d4641494c5552452d50524f42452d5245534f4c5554494f4e2d56414c55450000000000000020fbb2e310dbfb26c4aa41af5e997c50e99cad6ba97211ddd1731b1ce83cbb14af0000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db27200000000000000070100000000000000201112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f30"
  ]

stateGoldenDigests :: [String]
stateGoldenDigests =
  -- Canonical live state includes L/R and a combined progress promise for
  -- every active Herald; canonical retirement removes its target's promise.
  [ "af0b63c700697c188e298858cbc4869be0dfaf40dfb28b9d1881023b43361fd3",
    "c8c61721b66cbe3727299677fde879a0f134ea70365c2626863d96d2599f9bad",
    "e2301551e2a15cc0937e87dbc729b77b561708514f74180982aaf70ae6605830"
  ]

capturedVoterId :: V.VoterConfigurationId
capturedVoterId = V.voterConfigurationId (V.oracleVoterConfiguration (Live.initialOracleState step15Genesis))
