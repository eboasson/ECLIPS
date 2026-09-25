{-# LANGUAGE OverloadedStrings #-}

module ReceiptRetirementClientProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Bytes
import Data.List (permutations)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleClient qualified as Public
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Canonical qualified as Canonical
import Eclips.Oracle.Command qualified as Command
import Eclips.Oracle.Effect qualified as Effect
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Identity qualified as OracleIdentity
import Eclips.Oracle.Label qualified as OracleLabel
import Eclips.Oracle.Progress qualified as Progress
import Eclips.Oracle.Projection qualified as OracleProjection
import Eclips.Oracle.Receipt qualified as Receipt
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition qualified as Transition
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import GenesisFixtures qualified as Fixtures
import NativeVoterFailureFixtures qualified as NativeFailure
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Oracle receipt retirement client lifecycle"
    [ testCase "ordinary dispatch refreshes the ready prefix without changing semantic identity" casePiggyback,
      testCase "a membership-changing command confirms its piggybacked ready prefix" caseMembershipPiggyback,
      testCase "maintenance NotReady retries its exact prefix without allocating a request" caseMaintenanceRetry,
      testCase "a lost confirmation is recovered by exact maintenance replay after reconnect" caseLostConfirmation,
      testCase "duplicate and regressed confirmations and retired replies preserve settlement" caseLateOutcomes,
      testCase "old-binding maintenance outcomes cannot alter a reconnected owner" caseStaleBinding,
      testCase "local result evidence begins after its request was prepared" caseRequestEvidenceLifetime,
      testCase "a closed home keeps its watch and suppresses submissions across reconnect" caseClosedHome,
      testCase "retirement cannot cross a deferred low-sequence hole" caseRetiredHole,
      testCase "all consumer release orders agree with the pending-decision model" caseLabelPinOrders,
      testCase "a blocked decision after nonlabel controls cannot create idle progress" caseLabelNonlabelGaps,
      testCase "all completion offers pin their decision after local collection consumption" caseCompletionOfferPins,
      QC.testProperty "settled label request reclamation follows the consumed prefix and preserves other requests" propSettledLabelReclamation,
      QC.testProperty "plain label evidence retains one exact anchor and the unretired suffix" propLabelEvidenceCompaction,
      testCase "a result settling after its label consumers releases only its old sparse evidence" caseLabelEvidenceLateSettlement,
      testCase "label-only progress flushes at unchanged receipt high water without an ACK loop" caseLabelOnlyProgress,
      testCase "joining adoption includes consumed NotApplied decisions after the earlier base" caseJoiningLabelAdoption,
      testCase "checked joining client base carries pending global decisions without donor requests" caseJoiningClientBase,
      QC.testProperty "checked joining client base matches completed-prefix replay" propJoiningClientBaseReplay,
      QC.testProperty "joining label summaries survive covered released decisions and nonlabel gaps" propJoiningLabelSemanticSummary,
      QC.testProperty "covered canonical prefixes preserve exact local results without retaining unrelated history" propCanonicalPrefixReclamation,
      testCase "covered prefixes retain the End settling an unsent source-draining label" caseCoveredUnsentLabelEnd
    ]

-- Detached control-prefix fixtures may admit canonical local-ID entries without
-- owning those request witnesses. A later preparation cannot retrospectively
-- turn such a historical entry into an observed result for its fresh witness.
caseRequestEvidenceLifetime :: Assertion
caseRequestEvidenceLifetime = do
  let oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      priorProcess = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 0xfa))
      preparedPrior = checked (Client.prepareEndProcessEpochRequest "prior-reference-prefix" priorProcess ExplicitAdministrativeEnd initialClient)
      (priorPending, _, priorReference) = Client.commitOracleRequest preparedPrior
      (_, priorSettled) = settleRequest oracle priorPending priorReference
      index = Client.oracleClientAppliedCursor priorSettled
      entry = maybe (error "missing historical reference entry") id (Client.oracleClientRetainedEntry index priorSettled)
      prefixed = audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry initialClient)))
      (pending, reference) = prepareRequest "fresh-after-reference-prefix" prefixed
  assertEqual "the detached prefix and later witness use the same raw identity" priorReference reference
  assertEqual "the old canonical entry remains available by its control coordinate" (Just entry) (Client.oracleClientRetainedEntry index pending)
  assertEqual "preparation cannot retroactively observe an older entry" Nothing (Client.oracleClientRequestEvidence reference pending)
  assertEqual "the fresh request still awaits its own result" (Just Client.OracleRequestAwaitingProjection) (Client.oracleRequestWitnessStatus <$> Client.lookupOracleRequest reference pending)
  assertEqual "explicit result admission still checks the original command digest" (Left Client.OracleRequestDigestMismatch) (Client.preparedOracleResultClassification <$> Client.prepareOracleResult reference entry pending)
  assertEqual "the lifetime-indexed pending owner remains valid" (Right ()) (Client.validateState pending)

casePiggyback :: Assertion
casePiggyback = do
  let oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (firstPending, first) = prepareRequest "piggyback-first" initialClient
      (afterFirstOracle, afterFirst) = settleRequest oracle firstPending first
      (secondPending, second) = prepareRequest "piggyback-second" afterFirst
      (bothPending, third) = prepareRequest "piggyback-third" secondPending
      before = ordinaryEnvelope third bothPending
      (_, afterSecond) = settleRequest afterFirstOracle bothPending second
      after = ordinaryEnvelope third afterSecond
      retained = Client.oracleRequestWitnessEnvelope (maybe (error "missing piggyback witness") id (Client.lookupOracleRequest third bothPending))
      binding = currentBinding afterSecond
      (flushed, flushActions) = ingress (Public.OracleProgressFlush binding) afterSecond
      scheduledAtOne = Client.oracleClientRequestActions bothPending
      readyProgress = checked (Lifetime.receiptRetirement (Just 3) (Set.singleton 3))
  assertEqual "ordinary traffic carries the first ready prefix" (Just 1) (Command.oracleEnvelopeReceiptRetirement (Canonical.canonicalOracleEnvelopeValue before))
  assertEqual "the same pending request carries full progress with its own exception" (Just readyProgress) (Command.oracleEnvelopeReceiptRetirementProgress (Canonical.canonicalOracleEnvelopeValue after))
  assertEqual "dispatch decorates the retained semantic envelope" (Command.oracleEnvelopeWithReceiptRetirementProgress readyProgress (Canonical.canonicalOracleEnvelopeValue retained)) (Canonical.canonicalOracleEnvelopeValue after)
  assertEqual "piggyback preserves the pending request ID" (Client.oracleRequestRefRequestId third) (Command.oracleEnvelopeRequestId (Canonical.canonicalOracleEnvelopeValue after))
  assertEqual "both metadata revisions preserve the semantic digest" [Canonical.canonicalOracleEnvelopeDigest retained, Canonical.canonicalOracleEnvelopeDigest retained] (map Canonical.canonicalOracleEnvelopeDigest [before, after])
  assertEqual "dispatch leaves the retained witness unchanged" (Client.lookupOracleRequest third bothPending) (Client.lookupOracleRequest third afterSecond)
  assertEqual "ready progress schedules a binding-scoped idle flush" [binding] [supplied | Public.ScheduleOracleProgress supplied <- scheduledAtOne]
  assertEqual "ready progress does not submit separate maintenance immediately" [] [envelope | Public.SubmitOracleProgress _ envelope <- scheduledAtOne]
  assertEqual "an earlier scheduled flush uses the latest full progress" [Command.retireOracleReceiptProgressCommand readyProgress] [Command.oracleEnvelopeCommand (Canonical.canonicalOracleEnvelopeValue envelope) | Public.SubmitOracleProgress _ envelope <- flushActions]
  assertEqual "flushing does not mutate the owner or allocate a request" afterSecond flushed

caseMembershipPiggyback :: Assertion
caseMembershipPiggyback = do
  let target = Genesis.heraldMemberEpoch Fixtures.fixtureRemoteMember
      transcript = NativeFailure.nativeVoterFailureTranscript Fixtures.fixtureCheckedOracleGenesis [] target
      excluded = NativeFailure.nativeFailureExcluded transcript
      prefix = NativeFailure.nativeFailureEvidence transcript <> [NativeFailure.nativeFailureJoint transcript, excluded]
      observe client (_, precedingEntry) = audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance precedingEntry client)))
      atExclusion = foldl observe initialClient prefix
      (firstPending, first) = prepareRequest "membership-retirement-first" atExclusion
      (afterFirstOracle, afterFirst) = settleRequest (fst excluded) firstPending first
      (secondPending, second) = prepareRequest "membership-retirement-second" afterFirst
      (afterSecondOracle, ready) = settleRequest afterFirstOracle secondPending second
      resolution = Failure.acceptedVoterHostFailureResolution (NativeFailure.nativeFailureCertificate transcript)
      prepared = checked (Client.prepareRetireHeraldEpochRequest resolution target ready)
      (pending, _, reference) = Client.commitOracleRequest prepared
      envelope = ordinaryEnvelope reference pending
      (retiredOracle, outcome, effects) = checked (Transition.stepOracle (Canonical.canonicalOracleEnvelopeValue envelope) afterSecondOracle)
      entry = one [Canonical.canonicalizeAppliedOracleEntry supplied | Effect.EmitAppliedOracleEntry supplied <- Effect.oracleEffects effects]
      generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership retiredOracle)
      advanced = audited (Client.commitMembershipCursorAdvance (checked (Client.prepareCanonicalMembershipCursorAdvance entry generation pending)))
      (settled, _, _) = Client.commitOracleResult (checked (Client.prepareOracleResult reference entry advanced))
  case outcome of
    Effect.OracleCommitted receipt -> assertEqual "real retirement command is accepted" Receipt.OracleAccepted (Receipt.oracleReceiptResult receipt)
    other -> assertFailure ("membership fixture retirement was not committed: " <> show other)
  assertBool "retirement changes the actual Herald membership" (generation /= Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership afterSecondOracle))
  assertEqual "ordinary membership entry carries its home ready prefix" (Just (Genesis.checkedLocalHeraldEpoch Fixtures.fixtureStep14CheckedGenesis, 2)) (OracleProjection.appliedEntryReceiptRetirement (Canonical.canonicalAppliedOracleEntryValue entry))
  assertEqual "retirement prunes the acknowledged ordinary receipts" Nothing (Oracle.oracleRequestReceipt (Client.oracleRequestRefRequestId first) retiredOracle)
  assertEqual "membership cursor advancement confirms the piggyback" 2 (Client.oracleClientReceiptRetirementConfirmed advanced)
  assertEqual "membership cursor advancement preserves ready progress" 2 (Client.oracleClientReceiptRetirementReady advanced)
  assertEqual "a pending idle flush does not resend the confirmed prefix" [] (snd (ingress (Public.OracleProgressFlush (currentBinding advanced)) advanced))
  assertEqual "the membership command's own result can still settle" 3 (Client.oracleClientReceiptRetirementReady settled)
  assertEqual "the complete settled owner remains valid" (Right ()) (Client.validateState settled)

caseMaintenanceRetry :: Assertion
caseMaintenanceRetry = do
  let (_, ready, _, _) = settledFixture
      binding = currentBinding ready
      maintenance = maintenanceEnvelope ready
      request = Command.oracleEnvelopeRequestId (Canonical.canonicalOracleEnvelopeValue maintenance)
      notReady = Public.OracleProgressNotReadyReceived binding request (Client.oracleClientProgressReady ready) (Public.oracleObservedTerm 1)
      (waiting, actions) = ingress notReady ready
      retry = one [token | Public.ScheduleOracleRetry Public.OracleSubmissionRetry token <- actions]
      (stillWaiting, repeatedActions) = ingress notReady waiting
      (retried, retryActions) = ingress (Public.OracleRetryElapsed retry) waiting
      (_, waitingFlushActions) = ingress (Public.OracleProgressFlush binding) waiting
      (_, retryFlushActions) = ingress (Public.OracleProgressFlush binding) retried
  assertEqual "NotReady releases only its maintenance offer" [Client.oracleClientProgressReady ready] [through | Public.ReleaseOracleProgress supplied through <- actions, supplied == binding]
  assertEqual "waiting does not reoffer maintenance" [] (Client.oracleClientRequestActions waiting)
  assertEqual "an already scheduled flush respects NotReady pacing" [] waitingFlushActions
  assertEqual "repeated NotReady preserves the exact pending retry" waiting stillWaiting
  assertEqual "repeated NotReady does not schedule another timer" [] [token | Public.ScheduleOracleRetry _ token <- repeatedActions]
  assertEqual "retry cancels its exact timer" [retry] [token | Public.CancelOracleRetry token <- retryActions]
  assertEqual "retry schedules a coalesced idle flush" [binding] [supplied | Public.ScheduleOracleProgress supplied <- retryActions]
  assertEqual "retry does not directly submit standalone maintenance" [] [envelope | Public.SubmitOracleProgress _ envelope <- retryActions]
  assertEqual "the idle flush reoffers the unchanged maintenance envelope" [maintenance] [envelope | Public.SubmitOracleProgress supplied envelope <- retryFlushActions, supplied == binding]
  assertEqual "maintenance retry does not allocate an ordinary request" (Client.oracleClientNextRequestSequence ready) (Client.oracleClientNextRequestSequence retried)
  assertEqual "ordinary witnesses remain unchanged" (Client.oracleClientRequestEntries ready) (Client.oracleClientRequestEntries retried)
  let (confirmed, _) = ingress (Public.OracleProgressConfirmed binding request (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2)))) retried
      (late, lateActions) = ingress notReady confirmed
  assertEqual "confirmation stops maintenance offers" [] (Client.oracleClientRequestActions confirmed)
  assertEqual "late NotReady cannot restart confirmed maintenance" confirmed late
  assertEqual "late NotReady is stale" [Public.ReportOracleClientDiagnostic Public.StaleOracleClientIngress] lateActions

caseLostConfirmation :: Assertion
caseLostConfirmation = do
  let (oracle, ready, _, _) = settledFixture
      maintenance = maintenanceEnvelope ready
      rawMaintenance = Canonical.canonicalOracleEnvelopeValue maintenance
      (compacted, _, effects) = checked (Transition.stepOracle rawMaintenance oracle)
      (reconnected, reconnectActions) = reconnect ready
      newBinding = currentBinding reconnected
      (_, flushActions) = ingress (Public.OracleProgressFlush newBinding) reconnected
      (replayed, _, replayEffects) = checked (Transition.stepOracle rawMaintenance compacted)
      request = Command.oracleEnvelopeRequestId rawMaintenance
      (confirmed, _) = ingress (Public.OracleProgressConfirmed newBinding request (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2)))) reconnected
  assertEqual "maintenance actually removed the Oracle receipts" 0 (Oracle.oracleRequestCount compacted)
  assertEqual "first maintenance emits one committed entry" 1 (length (Effect.oracleEffects effects))
  assertEqual "the dropped confirmation does not advance the home prefix" 0 (Client.oracleClientReceiptRetirementConfirmed ready)
  assertEqual "reconnect schedules recovery through the idle flush" [newBinding] [supplied | Public.ScheduleOracleProgress supplied <- reconnectActions]
  assertEqual "reconnect does not immediately submit separate maintenance" [] [envelope | Public.SubmitOracleProgress _ envelope <- reconnectActions]
  assertEqual "the idle flush reoffers exactly the lost maintenance" [maintenance] [envelope | Public.SubmitOracleProgress supplied envelope <- flushActions, supplied == newBinding]
  assertEqual "reconnect keeps the retained projection cursor" (Client.oracleClientAppliedCursor ready) (Client.oracleClientAppliedCursor reconnected)
  assertEqual "replayed maintenance leaves the Oracle unchanged" compacted replayed
  assertEqual "replayed maintenance emits no new entry" [] (Effect.oracleEffects replayEffects)
  assertEqual "duplicate confirmation catches the home up" 2 (Client.oracleClientReceiptRetirementConfirmed confirmed)
  assertEqual "confirmation preserves local receipt consumers" (Client.oracleClientRequestEntries ready) (Client.oracleClientRequestEntries confirmed)

caseLateOutcomes :: Assertion
caseLateOutcomes = do
  let (_, ready, first, second) = settledFixture
      binding = currentBinding ready
      request = Client.oracleRequestRefRequestId second
      (confirmed, _) = ingress (Public.OracleProgressConfirmed binding request (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2)))) ready
      observations =
        [ Public.OracleProgressConfirmed binding request (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2))),
          Public.OracleProgressConfirmed binding (Client.oracleRequestRefRequestId first) (receiptOnly (Lifetime.receiptRetirementPrefix (Just 1))),
          Public.OracleRequestRetiredReceived binding request (Receipt.OracleRequestPrefixRetired 2),
          Public.OracleRequestRetiredReceived binding (Client.oracleRequestRefRequestId first) (Receipt.OracleRequestPrefixRetired 2),
          Public.OracleProgressFlush binding
        ]
  mapM_
    ( \observation -> do
        let (successor, actions) = ingress observation confirmed
        assertEqual "late outcome preserves the entire owner" confirmed successor
        assertEqual "late outcome emits no retry or maintenance" [] actions
    )
    observations
  let exact = checked (Client.prepareEndProcessEpochRequest "retirement-first" unknownProcess ExplicitAdministrativeEnd confirmed)
  assertEqual "local semantic retry retains its original request" first (Client.preparedOracleRequestRef exact)
  assertEqual "local semantic retry remains exact after remote retirement" Client.OracleRequestExactRetry (Client.preparedOracleRequestClassification exact)

caseStaleBinding :: Assertion
caseStaleBinding = do
  let (_, ready, _, second) = settledFixture
      oldBinding = currentBinding ready
      request = Client.oracleRequestRefRequestId second
      (reconnected, _) = reconnect ready
      observations =
        [ Public.OracleProgressConfirmed oldBinding request (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2))),
          Public.OracleProgressNotReadyReceived oldBinding request (Client.oracleClientProgressReady ready) (Public.oracleObservedTerm 1),
          Public.OracleRequestRetiredReceived oldBinding request Receipt.OracleRequestHomeRetired,
          Public.OracleProgressFlush oldBinding
        ]
  assertBool "reconnect installs a different binding" (currentBinding reconnected /= oldBinding)
  mapM_
    ( \observation -> do
        let (successor, actions) = ingress observation reconnected
        assertEqual "old binding leaves the owner unchanged" reconnected successor
        assertEqual "old binding is diagnosed without cancelling current work" [Public.ReportOracleClientDiagnostic Public.StaleOracleClientIngress] actions
    )
    observations
  assertEqual "current binding still schedules its unconfirmed maintenance" [currentBinding reconnected] [binding | Public.ScheduleOracleProgress binding <- Client.oracleClientRequestActions reconnected]
  assertEqual "current binding does not submit maintenance before the flush" [] [envelope | Public.SubmitOracleProgress _ envelope <- Client.oracleClientRequestActions reconnected]

caseClosedHome :: Assertion
caseClosedHome = do
  let (_, ready, _, _) = settledFixture
      (pending, reference) = prepareRequest "retirement-pending" ready
      request = Client.oracleRequestRefRequestId reference
      (closed, _) = ingress (Public.OracleRequestRetiredReceived (currentBinding pending) request Receipt.OracleRequestHomeRetired) pending
      (reconnected, actions) = reconnect closed
  assertBool "fixture contains an unresolved ordinary request" (not (null (Client.oracleClientRequestDispatches pending)))
  mapM_
    ( \state -> do
        assertEqual "closed home has no ordinary or maintenance submission" [] (Client.oracleClientRequestActions state)
        assertEqual "closed home has no raw dispatch" [] (Client.oracleClientRequestDispatches state)
        assertEqual "closed home has no label dispatch" [] (Client.oracleClientLabelRequestActions state)
        assertEqual "closed home has no disappearance dispatch" [] (Client.oracleClientDisappearanceRequestActions state)
        assertEqual "watch remains active until semantic retirement arrives" [Client.oracleClientAppliedCursor state] [cursor | Public.WatchOracle _ cursor <- Client.oracleClientActions state]
        assertEqual "unresolved request evidence remains retained" (Client.oracleClientRequestEntries pending) (Client.oracleClientRequestEntries state)
        let (flushed, flushActions) = ingress (Public.OracleProgressFlush (currentBinding state)) state
        assertEqual "a pending idle flush preserves the closed owner" state flushed
        assertEqual "a closed home ignores its idle flush" [] flushActions
    )
    [closed, reconnected]
  assertEqual "reconnect renews the watch, not submissions" [Client.oracleClientAppliedCursor closed] [cursor | Public.WatchOracle _ cursor <- actions]
  assertEqual "closed reconnect emits no maintenance" [] [envelope | Public.SubmitOracleProgress _ envelope <- actions]
  assertEqual "closed reconnect schedules no idle flush" [] [binding | Public.ScheduleOracleProgress binding <- actions]
  assertEqual "closed reconnect emits no ordinary request" [] [dispatch | Public.SubmitOracleRequest _ dispatch <- actions]

caseRetiredHole :: Assertion
caseRetiredHole = do
  let oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      object = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 0xfc))
      firstPrepared = checked (Client.prepareLabelDecisionRequest "retirement-hole" unknownProcess object (ProcessLabel unknownProcess, 0) (Just (Label.initialBootstrapLabelEvidence object unknownProcess)) Nothing Nothing Label.targetVoid (Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition unknownProcess) EmptyHeraldPublicationPrefix) initialClient)
      (firstPending, _, _, first) = Client.commitLabelDecisionRequest firstPrepared
      deferred = Client.commitOracleSubmissionDeferral (checked (Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId first) decision (Client.oracleClientAppliedCursor firstPending) firstPending))
      (bothPending, second) = prepareRequest "retirement-after-hole" deferred
      (_, withLaterResult) = settleRequest oracle bothPending second
      binding = currentBinding withLaterResult
  let progress = checked (Lifetime.receiptRetirement (Just 2) (Set.singleton 1))
  assertEqual "later projection preserves the deferred first request as an exception" progress (Client.oracleClientReceiptRetirementProgressReady withLaterResult)
  assertEqual "a pending hole does not prevent an idle flush" [binding] [supplied | Public.ScheduleOracleProgress supplied <- Client.oracleClientRequestActions withLaterResult]
  assertEqual "the idle flush carries the exact sparse lifetime" (Just progress) (Command.oracleCommandReceiptRetirementProgress (Command.oracleEnvelopeCommand (Canonical.canonicalOracleEnvelopeValue (maintenanceEnvelope withLaterResult))))
  let pendingRequest = Client.oracleRequestRefRequestId first
  case Client.prepareClientIngress (Public.OracleRequestRetiredReceived binding pendingRequest (Receipt.OracleRequestPrefixRetired 2)) withLaterResult of
    Left problem -> assertEqual "premature retirement is an invariant fault" (Client.OracleRequestRetiredBeforeSettlement pendingRequest) problem
    Right _ -> assertFailure "retired reply crossed a deferred exception"
  let settledRequest = Client.oracleRequestRefRequestId second
      (obsolete, _) = ingress (Public.OracleRequestRetiredReceived binding settledRequest (Receipt.OracleRequestPrefixRetired 2)) withLaterResult
  assertEqual "late retirement of a consumed suffix is harmless" withLaterResult obsolete
  case Client.prepareClientIngress (Public.OracleProgressConfirmed binding settledRequest (receiptOnly (Lifetime.receiptRetirementPrefix (Just 2)))) withLaterResult of
    Left problem -> assertEqual "confirmation cannot remove a pending exception" Client.OracleClientStateContradiction problem
    Right _ -> assertFailure "confirmation released a pending exception"
  let (confirmed, _) = ingress (Public.OracleProgressConfirmed binding settledRequest (receiptOnly progress)) withLaterResult
  assertEqual "sparse confirmation retains the exception" progress (Client.oracleClientReceiptRetirementProgressConfirmed confirmed)
  assertEqual "rejected ingresses preserve a valid predecessor" (Right ()) (Client.validateState withLaterResult)
  let covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix (Client.oracleClientAppliedCursor withLaterResult) withLaterResult))
  assertEqual "canonical coverage cannot settle a deferred request" (Client.lookupOracleRequest first withLaterResult) (Client.lookupOracleRequest first covered)
  assertEqual "canonical coverage preserves the receipt hole" progress (Client.oracleClientReceiptRetirementProgressReady covered)
  assertEqual "covered control does not authorize early receipt retirement" (Left (Client.OracleRequestRetiredBeforeSettlement pendingRequest)) (Client.commitClientIngress <$> Client.prepareClientIngress (Public.OracleRequestRetiredReceived binding pendingRequest (Receipt.OracleRequestPrefixRetired 2)) covered)

caseLabelPinOrders :: Assertion
caseLabelPinOrders = do
  let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      add (oracle, client, accumulated) key =
        let (gapPending, gapReference) = prepareRequest (key <> "-gap") client
            (afterGapOracle, afterGap) = settleRequest oracle gapPending gapReference
            (nextOracle, nextClient, ident, index, _) = prepareDecision key afterGapOracle afterGap
         in (nextOracle, nextClient, accumulated <> [(ident, index)])
      (_, pinned, decisions) = foldl add (initialOracle, initialClient, []) ["label-a", "label-b", "label-c", "label-d"]
      highWater = maximum (map snd decisions)
      frontier = Progress.oracleProgressLabelsThrough . Client.oracleClientProgressReady
      release (client, remaining) ident = do
        let consumed = audited (checked (Client.consumeLabelDecision ident client))
            retained = Map.delete ident remaining
            expected = if Map.null retained then highWater else maximum (Identity.controlIndex 0 : [index | (_, index) <- decisions, index < minimum (Map.elems retained)])
        assertEqual "indexed readiness reaches the actual decision before the oldest consumer" expected (frontier consumed)
        assertEqual "checked duplicate completion cannot remove another pin" consumed (checked (Client.consumeLabelDecision ident consumed))
        let covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix (Client.oracleClientAppliedCursor consumed) consumed))
        assertEqual "coverage preserves every label pin's original predecessor" (Client.oracleClientProgressReady consumed) (Client.oracleClientProgressReady covered)
        pure (covered, retained)
  forM_ (permutations (map fst decisions)) $ \order -> do
    (released, _) <- foldl (\previous ident -> previous >>= (\state -> release state ident)) (pure (pinned, Map.fromList decisions)) order
    assertEqual "all release orders end at the label high water" highWater (frontier released)
    forM_ decisions $ \(ident, index) ->
      assertBool
        "retired decision cannot acquire a new consumer"
        ( case Client.observeLabelDecision ident index released of
            Left _ -> True
            Right _ -> False
        )

caseLabelNonlabelGaps :: Assertion
caseLabelNonlabelGaps = do
  let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (afterFirstOracle, firstDonor, first, firstIndex, _) = prepareDecision "gap-first" initialOracle initialClient
      addGap (oracle, client) key =
        let (pending, reference) = prepareRequest key client
         in settleRequest oracle pending reference
      (beforeLaterOracle, beforeLater) = foldl addGap (afterFirstOracle, firstDonor) ["gap-a", "gap-b"]
      (_, donor, later, laterIndex, _) = prepareDecision "gap-later" beforeLaterOracle beforeLater
      observe client index =
        let entry = maybe (error "missing nonlabel-gap fixture entry") id (Client.oracleClientRetainedEntry index donor)
         in audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry client)))
      firstObserved = audited (checked (Client.observeLabelDecision first firstIndex (observe initialClient firstIndex)))
      consumed = audited (checked (Client.consumeLabelDecision first firstObserved))
      promised = Client.oracleClientProgressReady consumed
      binding = currentBinding consumed
      request = Command.oracleEnvelopeRequestId (Canonical.canonicalOracleEnvelopeValue (maintenanceEnvelope consumed))
      (confirmed, _) = ingress (Public.OracleProgressConfirmed binding request promised) consumed
      advanced = foldl observe confirmed (map Identity.controlIndex [Identity.controlIndexWord64 firstIndex + 1 .. Identity.controlIndexWord64 laterIndex])
      blocked = audited (checked (Client.observeLabelDecision later laterIndex advanced))
      released = audited (checked (Client.consumeLabelDecision later blocked))
  assertBool "fixture separates actual label decisions by nonlabel controls" (Identity.controlIndexWord64 laterIndex > Identity.controlIndexWord64 firstIndex + 1)
  assertEqual "watch cursor movement alone produces no promise" promised (Client.oracleClientProgressReady advanced)
  assertEqual "a newly blocked label cannot retire the intervening nonlabel gap" promised (Client.oracleClientProgressReady blocked)
  assertEqual "an unchanged promise schedules no idle acknowledgement" [] (Client.oracleClientProgressActions blocked)
  assertEqual "an already queued flush sends no redundant acknowledgement" [] (snd (ingress (Public.OracleProgressFlush binding) blocked))
  assertEqual "consuming the later decision advances to its actual coordinate" laterIndex (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady released))
  assertEqual "the actual label advance schedules the normal idle flush" [Public.ScheduleOracleProgress binding] (Client.oracleClientProgressActions released)

caseCompletionOfferPins :: Assertion
caseCompletionOfferPins = do
  let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (oracle, observed, ident, index, digest) = prepareDecision "offered-label" initialOracle initialClient
      home = Genesis.checkedLocalHeraldEpoch Fixtures.fixtureStep14CheckedGenesis
      generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership oracle)
      laterGeneration = checked (Membership.mkHeraldMembershipGenerationId (Fixtures.fixtureIdentifierBytes 0xf8))
      badDigest = checked (Label.mkLabelOutcomeDigest (Fixtures.fixtureIdentifierBytes 0xf9))
      offer gen value state =
        let prepared = checked (Client.prepareLabelCompletionRequest (OracleLabel.labelCompletionAttestation ident index value home gen) state)
            (next, _, reference) = Client.commitOracleRequest prepared
         in (audited next, reference)
      (firstOffered, first) = offer generation digest observed
      (bothOffered, second) = offer laterGeneration badDigest firstOffered
      consumed = audited (checked (Client.consumeLabelDecision ident bothOffered))
      (reconnected, _) = reconnect consumed
      (afterFirstOracle, afterFirst, firstResult) = projectRequest oracle reconnected first
      (_, afterSecond, secondResult) = projectRequest afterFirstOracle afterFirst second
      frontier = Progress.oracleProgressLabelsThrough . Client.oracleClientProgressReady
  assertEqual "two exact request pins survive collection consumption" 2 (length (Client.oracleClientWitnessLabelCompletionRequests (Client.oracleClientStateWitness consumed)))
  assertEqual "former collector's offer survives reconnect" (Client.oracleClientWitnessLabelCompletionRequests (Client.oracleClientStateWitness consumed)) (Client.oracleClientWitnessLabelCompletionRequests (Client.oracleClientStateWitness reconnected))
  assertEqual "consumer removal cannot advance across unprojected offers" (Identity.controlIndex 0) (frontier consumed)
  assertEqual "first projected offer leaves the other request pin" (Identity.controlIndex 0) (frontier afterFirst)
  assertEqual "every projected result releases its exact pin, including rejection" index (frontier afterSecond)
  assertBool "first semantic rediscovery was accepted" (case firstResult of Client.OracleRequestLabelCompleted {} -> True; _ -> False)
  assertBool "second offer had an immutable rejected result" (case secondResult of Client.OracleRequestRejected {} -> True; _ -> False)
  assertEqual "settled offer evidence remains available to collectors" 3 (length (Client.oracleClientRequestEntries afterSecond))
  assertEqual "an unprojected completion offer keeps every settled label witness" afterFirst (checked (Client.retireSettledLabelRequests afterFirst))
  let retired = audited (checked (Client.retireSettledLabelRequests afterSecond))
      (rebound, actions) = reconnect retired
      (late, _) = ingress (Public.OracleRequestRetiredReceived (currentBinding rebound) (Client.oracleRequestRefRequestId first) (Receipt.OracleRequestPrefixRetired 3)) rebound
  assertEqual "the final release retires decision and both settled offers" [] (Client.oracleClientRequestEntries retired)
  assertEqual "reclaimed completion reference no longer discovers an Oracle result" Nothing (Client.oracleClientRequestEvidence first retired)
  assertEqual "request retirement leaves the issued sequence unchanged" (Client.oracleClientNextRequestSequence afterSecond) (Client.oracleClientNextRequestSequence retired)
  assertEqual "request retirement preserves progress" (Client.oracleClientProgressReady afterSecond) (Client.oracleClientProgressReady retired)
  assertEqual "reconnect cannot reoffer reclaimed label requests" [] [() | Public.SubmitOracleRequest {} <- actions]
  assertEqual "a late retired reply is inert without its former witness" rebound late

propSettledLabelReclamation :: QC.Positive Int -> QC.Property
propSettledLabelReclamation (QC.Positive seed) =
  QC.forAll (QC.shuffle [1 .. count]) $ \order -> QC.ioProperty $ do
    let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
        (unrelatedPending, unrelated) = prepareRequest "retained-unrelated-result" initialClient
        (afterUnrelatedOracle, afterUnrelated) = settleRequest initialOracle unrelatedPending unrelated
        (withPending, pending) = prepareRequest "retained-unrelated-pending" afterUnrelated
        add (oracle, client, accumulated) ordinal =
          let key = Bytes.pack ("reclaimed-label-" <> show ordinal)
              (nextOracle, nextClient, ident, index, _) = prepareDecision key oracle client
              witness = maybe (error "missing indexed label witness") id (Client.lookupOracleRequestBySemanticKey key nextClient)
           in (nextOracle, nextClient, accumulated <> [(ordinal, ident, index, key, Client.oracleRequestWitnessRef witness)])
        (_, pinned, decisions) = foldl add (afterUnrelatedOracle, withPending, []) [1 .. count]
        highWater = maximum [index | (_, _, index, _, _) <- decisions]
        release (client, remaining) ordinal = do
          let ident = one [selected | (number, selected, _, _, _) <- decisions, number == ordinal]
              consumed = checked (Client.consumeLabelDecision ident client)
              retired = audited (checked (Client.retireSettledLabelRequests consumed))
              stillPending = Map.delete ident remaining
              expectedFrontier = if Map.null stillPending then highWater else maximum (Identity.controlIndex 0 : [index | (_, _, index, _, _) <- decisions, index < minimum (Map.elems stillPending)])
              expectedRequests = Set.fromList (unrelated : pending : [reference | (_, _, index, _, reference) <- decisions, index > expectedFrontier])
          assertEqual "reclamation agrees with the independent consumed-prefix model" expectedRequests (Set.fromList (map fst (Client.oracleClientRequestEntries retired)))
          forM_ decisions $ \(_, _, index, key, _) ->
            assertEqual "semantic keys share their request's retirement" (index > expectedFrontier) (case Client.lookupOracleRequestBySemanticKey key retired of Just _ -> True; Nothing -> False)
          assertEqual "canonical result evidence has an independent lifetime" (Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness pinned)) (Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness retired))
          assertEqual "repeated reclamation is inert" retired (checked (Client.retireSettledLabelRequests retired))
          pure (retired, stillPending)
    assertEqual "settlement alone cannot release a label consumer" pinned (checked (Client.retireSettledLabelRequests pinned))
    (retired, _) <- foldM release (pinned, Map.fromList [(ident, index) | (_, ident, index, _, _) <- decisions]) order
    assertEqual "unrelated settled result is unchanged" (Client.lookupOracleRequest unrelated withPending) (Client.lookupOracleRequest unrelated retired)
    let resultBefore = Client.oracleClientRequestEvidence unrelated withPending
        (reconnected, _) = reconnect retired
    assertBool "the unrelated settled request has exact result evidence" (case resultBefore of Just _ -> True; Nothing -> False)
    assertEqual "unrelated result lookup survives other controls and reconnect" resultBefore (Client.oracleClientRequestEvidence unrelated reconnected)
    assertEqual "unrelated pending request has no result after reconnect" Nothing (Client.oracleClientRequestEvidence pending reconnected)
    forM_ decisions $ \(_, _, _, _, oldReference) ->
      assertEqual "reclaimed label result cannot reappear on reconnect" Nothing (Client.oracleClientRequestEvidence oldReference reconnected)
    assertEqual "unrelated pending request is unchanged" (Client.lookupOracleRequest pending withPending) (Client.lookupOracleRequest pending retired)
    let (fresh, reference) = prepareRequest "after-label-reclamation" retired
    assertEqual "reclaimed request identities are never reused" (Client.oracleClientNextRequestSequence pinned + 1) (Client.oracleClientNextRequestSequence fresh)
    assertBool "fresh reference is distinct from every reclaimed request" (reference `notElem` [old | (_, _, _, _, old) <- decisions])
    pure True
  where
    count = 2 + seed `mod` 9

propLabelEvidenceCompaction :: QC.Positive Int -> QC.Property
propLabelEvidenceCompaction (QC.Positive seed) =
  QC.forAll (QC.shuffle [1 .. count]) $ \order -> QC.ioProperty $ do
    let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
        add (oracle, client, accumulated) ordinal =
          let key = Bytes.pack ("label-evidence-" <> show ordinal)
              (gapPending, gapReference) = prepareRequest (key <> "-gap") client
              (afterGapOracle, afterGap) = settleRequest oracle gapPending gapReference
              (nextOracle, nextClient, ident, index, _) = prepareDecision key afterGapOracle afterGap
           in (nextOracle, nextClient, accumulated <> [(ordinal, ident, index)])
        (_, pinned, decisions) = foldl add (initialOracle, initialClient, []) [1 .. count]
        entries = Map.fromList [(index, maybe (error "missing original evidence") id (Client.oracleClientRetainedEntry index pinned)) | (index, _) <- Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness pinned)]
        projection = foldl (\state entry -> Projection.commitAppliedEntry (checked (Projection.prepareAppliedEntry entry state))) Fixtures.fixtureOracleProjectionState (Map.elems entries)
        highWater = maximum [index | (_, _, index) <- decisions]
        release (client, remaining) ordinal = do
          let ident = one [selected | (number, selected, _) <- decisions, number == ordinal]
              consumed = checked (Client.consumeLabelDecision ident client)
              requestsRetired = checked (Client.retireSettledLabelRequests consumed)
              compacted = audited (checked (Client.compactLabelDecisionEvidence requestsRetired))
              stillPending = Map.delete ident remaining
              expectedFrontier = if Map.null stillPending then highWater else maximum (Identity.controlIndex 0 : [index | (_, _, index) <- decisions, index < minimum (Map.elems stillPending)])
              removed = Set.fromList [index | (_, _, index) <- decisions, index < expectedFrontier]
          assertEqual "only old plain decisions disappear behind the exact anchor" (Map.keysSet entries Set.\\ removed) (Set.fromList (map fst (Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness compacted))))
          forM_ (Set.toList removed) $ \index -> do
            let original = entries Map.! index
            assertEqual "Projection independently retains every exact old entry" (Just original) (Projection.appliedEntryEvidence index projection)
            assertBool "Projection-proven old overlaps remain recognizable" (Client.oracleClientMatchesProjectedEntry original compacted)
            assertBool "the expected Client subset excludes the reclaimed entry" (not (Client.oracleClientRetainsEntry original compacted))
          assertEqual "compaction at unchanged progress is inert" compacted (checked (Client.compactLabelDecisionEvidence compacted))
          pure (compacted, stillPending)
    (compacted, _) <- foldM release (pinned, Map.fromList [(ident, index) | (_, ident, index) <- decisions]) order
    assertEqual "only the exact newest decision remains after every consumer finishes" [highWater] [index | (_, _, index) <- decisions, Client.oracleClientRetainedEntry index compacted /= Nothing]
    pure True
  where
    count = 2 + seed `mod` 9

caseLabelEvidenceLateSettlement :: Assertion
caseLabelEvidenceLateSettlement = do
  let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      process = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 30))
      object = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 41))
      prepared = checked (Client.prepareLabelDecisionRequest "late-label-result" process object (ProcessLabel process, 99) (Just (Label.initialBootstrapLabelEvidence object process)) Nothing Nothing Label.targetVoid (Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition process) EmptyHeraldPublicationPrefix) initialClient)
      (pending, _, ident, reference) = Client.commitLabelDecisionRequest prepared
      witness = maybe (error "missing unsettled decision") id (Client.lookupOracleRequest reference pending)
      (afterDecisionOracle, _, effects) = checked (Transition.stepOracle (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) initialOracle)
      entry = one [Canonical.canonicalizeAppliedOracleEntry supplied | Effect.EmitAppliedOracleEntry supplied <- Effect.oracleEffects effects]
      index = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue entry)
      advanced = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry pending))
      observed = checked (Client.observeLabelDecision ident index advanced)
      consumed = audited (checked (Client.consumeLabelDecision ident observed))
      (_, later, laterId, laterIndex, _) = prepareDecision "later-label-result" afterDecisionOracle consumed
      ready = checked (Client.consumeLabelDecision laterId later)
      compacted = audited (checked (Client.compactLabelDecisionEvidence (checked (Client.retireSettledLabelRequests ready))))
      (settled, _, _) = Client.commitOracleResult (checked (Client.prepareOracleResult reference entry compacted))
      (compactedAfterResult, released) = checked (Client.compactCompletedBatch settled)
      retired = audited compactedAfterResult
  assertEqual "the outstanding local result preserves its old exact decision" (Just entry) (Client.oracleClientRetainedEntry index compacted)
  assertEqual "late settlement removes sparse evidence without rewalking old intervals" Nothing (Client.oracleClientRetainedEntry index retired)
  assertBool "late settlement preserves the current exact anchor" (Client.oracleClientRetainedEntry laterIndex retired /= Nothing)
  assertEqual "the old request witness also retires" Nothing (Client.lookupOracleRequest reference retired)
  assertEqual "late settlement leaves the already-consumed label frontier unchanged" (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady compacted)) (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady retired))
  assertEqual "late settlement does not advance the canonical cursor" (Client.oracleClientAppliedCursor compacted) (Client.oracleClientAppliedCursor retired)
  assertBool "completed-batch compaction reports the same-cursor evidence release" released
  assertBool "outer composition can observe the released canonical pin" (Client.canonicalEvidenceReleased compacted retired)
  assertEqual "an unchanged batch reports no further release" (retired, False) (checked (Client.compactCompletedBatch retired))
  assertEqual "later compaction remains inert" retired (checked (Client.compactLabelDecisionEvidence retired))

caseLabelOnlyProgress :: Assertion
caseLabelOnlyProgress = do
  let initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (_, observed, ident, index, _) = prepareDecision "idle-label" initialOracle initialClient
      binding = currentBinding observed
      oldProgress = Client.oracleClientProgressReady observed
      request = Command.oracleEnvelopeRequestId (Canonical.canonicalOracleEnvelopeValue (maintenanceEnvelope observed))
      (receiptsConfirmed, _) = ingress (Public.OracleProgressConfirmed binding request oldProgress) observed
      ready = audited (checked (Client.consumeLabelDecision ident receiptsConfirmed))
      full = Client.oracleClientProgressReady ready
      (_, flushed) = ingress (Public.OracleProgressFlush binding) ready
      (confirmed, actions) = ingress (Public.OracleProgressConfirmed binding request full) ready
      (late, lateActions) = ingress (Public.OracleProgressNotReadyReceived binding request oldProgress (Public.oracleObservedTerm 1)) confirmed
  assertEqual "only the label component changes" (Progress.oracleProgressReceipts oldProgress) (Progress.oracleProgressReceipts full)
  assertEqual "label high water is the original decision index" index (Progress.oracleProgressLabelsThrough full)
  assertEqual "label-only readiness schedules an idle flush" [Public.ScheduleOracleProgress binding] (Client.oracleClientProgressActions ready)
  assertEqual "idle flush carries the full promise" [Just full] [Command.oracleCommandProgress (Command.oracleEnvelopeCommand (Canonical.canonicalOracleEnvelopeValue envelope)) | Public.SubmitOracleProgress _ envelope <- flushed]
  assertEqual "confirmation does not create another maintenance acknowledgement" [] actions
  assertEqual "late same-high-water failure cannot lower confirmed progress" confirmed late
  assertEqual "late failure is stale" [Public.ReportOracleClientDiagnostic Public.StaleOracleClientIngress] lateActions
  assertEqual "maintenance allocates no request" (Client.oracleClientNextRequestSequence observed) (Client.oracleClientNextRequestSequence confirmed)

caseJoiningLabelAdoption :: Assertion
caseJoiningLabelAdoption = do
  let (oracleAtBase, donorAtBase, _, _) = settledFixture
      (_, donor, ident, index, _) = prepareDecision "after-join-base" oracleAtBase donorAtBase
      joining = Client.initialJoiningState (Client.oracleClientHelloClaims initialClient) (Client.oracleClientContacts initialClient) (Identity.controlIndex 0)
      entryAt cursor = maybe (error "missing joining fixture entry") id (Client.oracleClientRetainedEntry cursor donor)
      observe client cursor = audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance (entryAt cursor) client)))
      atBase = foldl observe joining [Identity.controlIndex 1, Identity.controlIndex 2]
      withLater = observe atBase index
      pinned = audited (checked (Client.observeLabelDecision ident index withLater))
      consumed = audited (checked (Client.consumeLabelDecision ident pinned))
      connecting = audited (checked (Client.startJoiningParticipation consumed))
      attempt = one [value | Public.ConnectAndHelloOracle value _ <- Client.oracleClientActions connecting]
      (serving, actions) = ingress (hello attempt index) connecting
      binding = currentBinding serving
      frontier = Progress.oracleProgressLabelsThrough . Client.oracleClientProgressReady
  assertEqual "an earlier control base alone promises no label retirement" (Identity.controlIndex 0) (frontier atBase)
  assertBool "the later canonical decision follows the earlier base" (index > Client.oracleClientAppliedCursor atBase)
  assertBool "unconsumed historical label forbids joining participation" (case Client.startJoiningParticipation pinned of Left Client.OracleClientStateContradiction -> True; _ -> False)
  assertEqual "joining replay emits no historical completion or progress offer" [] (Client.oracleClientActions consumed)
  assertEqual "activation retains the full preceding label high water" index (frontier serving)
  assertEqual "serving starts an idle flush for the adopted promise" [binding] [value | Public.ScheduleOracleProgress value <- actions]
  assertEqual "observer replay creates no local request witnesses" [] (Client.oracleClientRequestEntries serving)
  assertEqual "joining adoption leaves no label consumer or request pins" ([], []) (Client.oracleClientWitnessLabelConsumers (Client.oracleClientStateWitness serving), Client.oracleClientWitnessLabelCompletionRequests (Client.oracleClientStateWitness serving))

caseJoiningClientBase :: Assertion
caseJoiningClientBase = do
  let oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (afterFirstOracle, afterFirst, _, firstIndex, _) = prepareDecision "base-completed" oracle initialClient
      (pendingOracle, pendingDonor, ident, index, digest) = prepareDecisionWithGeneration 0 "base-released" afterFirstOracle afterFirst
      projection = projectionForClient pendingDonor
      base = checked (Client.captureJoiningClientBase projection)
      imported = audited (checked (Client.installJoiningClientBase base freshJoiningClient))
      witness = Client.oracleClientStateWitness imported
      generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership pendingOracle)
      home = Public.oracleHelloHeraldEpoch (Client.oracleClientHelloClaims pendingDonor)
      offered = checked (Client.prepareLabelCompletionRequest (OracleLabel.labelCompletionAttestation ident index digest home generation) pendingDonor)
      (withOffer, _, reference) = Client.commitOracleRequest offered
      (_, completedDonor, _) = projectRequest pendingOracle withOffer reference
      completedBase = checked (Client.captureJoiningClientBase (projectionForClient completedDonor))
      completed = audited (checked (Client.installJoiningClientBase completedBase freshJoiningClient))
      claims = Client.oracleClientHelloClaims freshJoiningClient
      wrongSystem = checked (Identity.mkSystemId (Fixtures.fixtureIdentifierBytes 0xea))
      wrongClaims = Public.oracleHelloClaims wrongSystem (Public.oracleHelloCatalogueDigest claims) (Public.oracleHelloConfigurationDigest claims) (Public.oracleHelloInitialProjectionDigest claims) (Public.oracleHelloHeraldId claims) (Public.oracleHelloHeraldEpoch claims) (Public.oracleHelloMembershipGeneration claims)
      wrongReceiver = Client.initialJoiningState wrongClaims (Client.oracleClientContacts freshJoiningClient) (Identity.controlIndex 0)
      (usedReceiver, _) = prepareRequest "receiver-local-request" freshJoiningClient
  assertEqual "capture names the current complete prefix" index (Client.joiningClientBaseControlIndex base)
  assertEqual "only the globally released decision remains pending" [(ident, index)] (Client.oracleClientWitnessLabelConsumers witness)
  assertEqual "the pending decision preserves its preceding actual label frontier" firstIndex (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady imported))
  assertEqual "source local completion offers do not cross the base" [] (Client.oracleClientWitnessLabelCompletionRequests witness)
  assertEqual "source request identities and semantic keys do not cross the base" [] (Client.oracleClientRequestEntries imported)
  assertEqual "receiver request allocator is preserved" 1 (Client.oracleClientNextRequestSequence imported)
  assertEqual "receiver local identity is preserved" (Public.oracleHelloHeraldEpoch claims) (Public.oracleHelloHeraldEpoch (Client.oracleClientHelloClaims imported))
  assertEqual "observer import has no network work" [] (Client.oracleClientActions imported)
  assertEqual "full archive preserves the original genesis cursor boundary" (Client.oracleClientRetainedEntry firstIndex pendingDonor) (Client.oracleClientRetainedEntry firstIndex imported)
  assertEqual "global completion removes the imported consumer despite donor local pins" [] (Client.oracleClientWitnessLabelConsumers (Client.oracleClientStateWitness completed))
  assertEqual "global completion adopts the latest decision frontier" index (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady completed))
  assertEqual "import never copies source progress confirmations" (Identity.controlIndex 0) (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressConfirmed completed))
  assertEqual "a target with local request ownership is not pristine" (Left Client.OracleJoiningBaseReceiverNotFresh) (Client.installJoiningClientBase base usedReceiver)
  assertEqual "a serving client cannot adopt an observer base" (Left Client.OracleJoiningBaseReceiverNotFresh) (Client.installJoiningClientBase base initialClient)
  assertEqual "a different checked system cannot adopt this base" (Left Client.OracleJoiningBaseIdentityMismatch) (Client.installJoiningClientBase base wrongReceiver)
  let covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix index imported))
      sparseProjection = checked (Projection.reclaimAppliedPrefixThroughBase (Map.keysSet (Client.oracleClientProtectedPrefixEvidence covered)) Nothing (checked (Projection.advanceReplayBase projection)))
      sparseBase = checked (Client.captureJoiningClientBase sparseProjection)
      sparseImported = audited (checked (Client.installJoiningClientBase sparseBase freshJoiningClient))
  assertEqual "a covered base imports its original canonical floor" index (Client.oracleClientCanonicalCoveredThrough sparseImported)
  assertEqual "sparse base preserves the pending decision and its preceding label" (Client.oracleClientStateWitness covered) (Client.oracleClientStateWitness sparseImported)

propJoiningClientBaseReplay :: QC.Positive Int -> QC.Property
propJoiningClientBaseReplay (QC.Positive seed) = QC.ioProperty $ do
  let count = 1 + seed `mod` 12
      oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      add (previousOracle, client) ordinal =
        let (nextOracle, next, _, _, _) = prepareDecision (Bytes.pack ("base-prefix-" <> show ordinal)) previousOracle client
         in (nextOracle, next)
      (_, donor) = foldl add (oracle, initialClient) [1 .. count]
      entries = clientCanonicalEntries donor
      projection = projectionForClient donor
      captured = checked (Projection.captureProjectionBase projection)
      decoded = checked (Projection.decodeProjectionBase Fixtures.fixtureOracleProjectionState (Projection.encodeProjectionBase captured))
      base = checked (Client.captureJoiningClientBaseFromProjection captured)
      imported = audited (checked (Client.installJoiningClientBase base freshJoiningClient))
      replay client canonical =
        let advanced = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance canonical client))
            consume current event = case OracleProjection.oracleProjectionEventView event of
              OracleProjection.LabelDecidedView decisionValue _ _ ->
                checked (Client.consumeLabelDecision (OracleLabel.liveDecisionId decisionValue) (checked (Client.observeLabelDecision (OracleLabel.liveDecisionId decisionValue) (OracleLabel.liveDecisionRequestControlIndex decisionValue) current)))
              _ -> current
         in audited (foldl consume advanced (OracleProjection.appliedEntryProjectionEvents (Canonical.canonicalAppliedOracleEntryValue canonical)))
      reference = foldl replay freshJoiningClient entries
  assertEqual "shared Projection capture preserves the checked Client capture" (Client.captureJoiningClientBase projection) (Right base)
  assertEqual "admitted wire Projection derives the same Client facts" (Right base) (Client.captureJoiningClientBaseFromProjection decoded)
  assertEqual "base installation agrees with explicit completed-prefix replay" reference imported
  assertEqual "completed prefix can join without historical local request pins" (Right ()) (() <$ Client.startJoiningParticipation imported)
  assertEqual "base compaction keeps the same retained archive subset" (checked (Client.compactLabelDecisionEvidence reference)) (checked (Client.compactLabelDecisionEvidence imported))
  let prefix = Client.oracleClientAppliedCursor reference
      covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix prefix (checked (Client.compactLabelDecisionEvidence reference))))
      projectionAtBase = checked (Projection.advanceReplayBase (projectionForClient donor))
      sparseProjection = checked (Projection.reclaimAppliedPrefixThroughBase (Map.keysSet (Client.oracleClientProtectedPrefixEvidence covered)) Nothing projectionAtBase)
      sparseCapture = checked (Projection.captureProjectionBase sparseProjection)
      sparseBase = checked (Client.captureJoiningClientBaseFromProjection sparseCapture)
      sparseDecoded = checked (Projection.decodeProjectionBase Fixtures.fixtureOracleProjectionState (Projection.encodeProjectionBase sparseCapture))
      sparseImported = audited (checked (Client.installJoiningClientBase sparseBase freshJoiningClient))
      withoutAnchors = checked (Projection.reclaimAppliedPrefixThroughBase Set.empty Nothing projectionAtBase)
  assertEqual "shared sparse Projection preserves the checked Client capture" (Client.captureJoiningClientBase sparseProjection) (Right sparseBase)
  assertEqual "admitted sparse Projection derives the same Client pins" (Right sparseBase) (Client.captureJoiningClientBaseFromProjection sparseDecoded)
  assertEqual "a sparse joining base imports its coverage marker" prefix (Client.oracleClientCanonicalCoveredThrough sparseImported)
  assertEqual "sparse joining import preserves exact anchors and all client facts" (Client.oracleClientStateWitness covered) (Client.oracleClientStateWitness sparseImported)
  let summarized = audited (checked (Client.installJoiningClientBase (checked (Client.captureJoiningClientBase withoutAnchors)) freshJoiningClient))
      compactedSummary = audited (checked (Client.compactLabelDecisionEvidence summarized))
  assertEqual "checked semantic facts replace missing current decision bytes" prefix (Client.joiningClientBaseControlIndex (checked (Client.captureJoiningClientBase withoutAnchors)))
  assertEqual "a semantic summary does not reconstruct a canonical archive" [] (Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness summarized))
  assertEqual "semantic label anchors preserve completed progress" (Client.oracleClientProgressReady covered) (Client.oracleClientProgressReady compactedSummary)
  pure True

propJoiningLabelSemanticSummary :: QC.Positive Int -> Bool -> QC.Property
propJoiningLabelSemanticSummary (QC.Positive seed) reverseOrder = QC.ioProperty $ do
  let count = 1 + seed `mod` 5
      firstObject = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 41))
      middleObject = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 45))
      lastObject = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 43))
      initialOracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (firstOracle, firstDonor, first, firstIndex, _) = prepareDecisionForObject firstObject 0 "summary-old-released" initialOracle initialClient
      firstEntry = maybe (error "missing summary first decision") id (Client.oracleClientRetainedEntry firstIndex firstDonor)
      finish ident client = audited (checked (Client.compactLabelDecisionEvidence (checked (Client.retireSettledLabelRequests (checked (Client.consumeLabelDecision ident client))))))
      afterFirst = finish first firstDonor
      addGap key (oracle, client, priorEntries) =
        let (pending, gapReference) = prepareRequest key client
            (nextOracle, next) = settleRequest oracle pending gapReference
            entry = maybe (error "missing summary gap entry") id (Client.oracleClientRequestEvidence gapReference next)
         in (nextOracle, next, priorEntries <> [entry])
      addMiddle state ordinal =
        let key = Bytes.pack ("summary-middle-" <> show ordinal)
            (oracle, before, priorEntries) = addGap (key <> "-gap") state
            (nextOracle, next, ident, index, _) = prepareDecisionForObject middleObject 99 key oracle before
            entry = maybe (error "missing summary completed decision") id (Client.oracleClientRetainedEntry index next)
         in (nextOracle, finish ident next, priorEntries <> [entry])
      withMiddle = foldl addMiddle (firstOracle, afterFirst, [firstEntry]) [1 .. count]
      (beforeLastOracle, beforeLast, preceding) = addGap "summary-last-nonlabel-gap" withMiddle
      (oracleAtBase, lastDonor, lastDecision, lastIndex, _) = prepareDecisionForObject lastObject 0 "summary-later-released" beforeLastOracle beforeLast
      lastEntry = maybe (error "missing summary last decision") id (Client.oracleClientRetainedEntry lastIndex lastDonor)
      entries = preceding <> [lastEntry]
      donor = audited (checked (Client.reclaimOracleClientCanonicalPrefix lastIndex (finish lastDecision lastDonor)))
      originalProjection = foldl (\priorProjection entry -> Projection.commitAppliedEntry (checked (Projection.prepareAppliedEntry entry priorProjection))) Fixtures.fixtureOracleProjectionState entries
      projection = checked (Projection.reclaimAppliedPrefixThroughBase (Map.keysSet (Client.oracleClientProtectedPrefixEvidence donor)) Nothing (checked (Projection.advanceReplayBase originalProjection)))
      imported = audited (checked (Client.installJoiningClientBase (checked (Client.captureJoiningClientBase projection)) freshJoiningClient))
      replay client canonical =
        let advanced = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance canonical client))
            observe current event = case OracleProjection.oracleProjectionEventView event of
              OracleProjection.LabelDecidedView value outcome _ ->
                let ident = OracleLabel.liveDecisionId value
                    withConsumer = checked (Client.observeLabelDecision ident (OracleLabel.liveDecisionRequestControlIndex value) current)
                 in case OracleLabel.liveTerminalOutcomeView outcome of
                      OracleLabel.LiveNotAppliedOutcomeView {} -> checked (Client.consumeLabelDecision ident withConsumer)
                      OracleLabel.LiveReleasedOutcomeView {} -> withConsumer
              _ -> current
         in audited (foldl observe advanced (OracleProjection.appliedEntryProjectionEvents (Canonical.canonicalAppliedOracleEntryValue canonical)))
      reference = foldl replay freshJoiningClient entries
      withoutBytes state = (Client.oracleClientStateWitness state) {Client.oracleClientWitnessRetainedEntryBytes = []}
      decisionIndices = [OracleLabel.liveDecisionRequestControlIndex value | entry <- entries, event <- OracleProjection.appliedEntryProjectionEvents (Canonical.canonicalAppliedOracleEntryValue entry), OracleProjection.LabelDecidedView value _ _ <- [OracleProjection.oracleProjectionEventView event]]
      precedingDecision = maximum (filter (< lastIndex) decisionIndices)
      order = if reverseOrder then [lastDecision, first] else [first, lastDecision]
  assertEqual "local donor completion does not globally complete the old workflow" (Just Projection.LabelWorkflowReleased) (Projection.projectedLabelWorkflowPhase <$> Projection.oracleViewLabelWorkflow first (Projection.oracleView projection))
  assertEqual "the source actually discarded the old raw decision" Nothing (Client.oracleClientRetainedEntry firstIndex donor)
  assertEqual "Projection also discarded the old raw decision" Nothing (Projection.appliedEntryEvidence firstIndex projection)
  assertBool "nonlabel control separates the later decision from its actual label predecessor" (Identity.controlIndexWord64 precedingDecision + 1 < Identity.controlIndexWord64 lastIndex)
  assertEqual "imported consumers are the two globally Released workflows" (Map.fromList [(first, firstIndex), (lastDecision, lastIndex)]) (Map.fromList (Client.oracleClientWitnessLabelConsumers (Client.oracleClientStateWitness imported)))
  assertEqual "semantic summary agrees with chronological replay" (withoutBytes reference) (withoutBytes imported)
  assertEqual "summary supplies no donor request ownership" [] (Client.oracleClientRequestEntries imported)
  assertEqual "summary supplies no missing canonical decision" Nothing (Client.oracleClientRetainedEntry firstIndex imported)
  let (requestPending, newReference) = prepareRequest "summary-cannot-settle-a-request" imported
  assertEqual "a semantic decision marker cannot admit a local result" (Left (Client.OracleRequestEntryMissing firstIndex)) (Client.preparedOracleResultClassification <$> Client.prepareOracleResult newReference firstEntry requestPending)
  (released, releasedReference) <-
    foldM
      ( \(client, chronological) ident -> do
          let next = audited (checked (Client.compactLabelDecisionEvidence (checked (Client.consumeLabelDecision ident client))))
              expected = audited (checked (Client.compactLabelDecisionEvidence (checked (Client.consumeLabelDecision ident chronological))))
          assertEqual "consumer release preserves actual label predecessor readiness" (withoutBytes expected) (withoutBytes next)
          if ident == first && not reverseOrder
            then assertEqual "a completed intervening decision remains the next pin's predecessor" precedingDecision (Progress.oracleProgressLabelsThrough (Client.oracleClientProgressReady next))
            else pure ()
          pure (next, expected)
      )
      (imported, reference)
      order
  let (_, suffixDonor, _, suffixIndex, _) = prepareDecisionForObject middleObject 99 "summary-canonical-suffix" oracleAtBase donor
      suffix = maybe (error "missing summary canonical suffix") id (Client.oracleClientRetainedEntry suffixIndex suffixDonor)
      finishSuffix = audited . checked . Client.reclaimOracleClientCanonicalPrefix suffixIndex . checked . Client.compactLabelDecisionEvidence . (`replay` suffix)
      final = finishSuffix released
      finalReference = finishSuffix releasedReference
  assertEqual "canonical suffix restores identical client ownership after old summaries retire" finalReference final
  assertEqual "fresh observer can activate after summarized consumers finish" (Right ()) (() <$ Client.startJoiningParticipation final)
  pure True

propCanonicalPrefixReclamation :: QC.Positive Int -> QC.Property
propCanonicalPrefixReclamation (QC.Positive seed) = QC.ioProperty $ do
  let count = 2 + seed `mod` 12
      (initialOracle, settled, first, second) = settledFixture
      (pending, pendingReference) = prepareRequest "covered-pending-result" settled
      (withLateRequest, lateReference) = prepareRequest "covered-late-first-result" pending
      witness = maybe (error "missing covered result request") id (Client.lookupOracleRequest lateReference withLateRequest)
      (afterLateOracle, lateEntry) = canonicalStep (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) initialOracle
      beforeUnrelated = audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance lateEntry withLateRequest)))
      home = Public.oracleHelloHeraldEpoch (Client.oracleClientHelloClaims beforeUnrelated)
      add (oracle, client, entries) ordinal =
        let envelope = Command.oracleEnvelope (OracleIdentity.oracleClientRequestId home (1000 + fromIntegral ordinal)) Nothing home (checked (Command.endProcessEpochCommand unknownProcess ExplicitAdministrativeEnd))
            (nextOracle, entry) = canonicalStep envelope oracle
            advanced = audited (Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry client)))
         in (nextOracle, advanced, entries <> [entry])
      (_, complete, unrelated) = foldl add (afterLateOracle, beforeUnrelated, []) [1 .. count]
      prefix = Client.oracleClientAppliedCursor complete
      partialPrefix = Identity.controlIndex (Identity.controlIndexWord64 prefix - 1)
      partial = audited (checked (Client.reclaimOracleClientCanonicalPrefix partialPrefix complete))
      covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix prefix partial))
      firstEntry = maybe (error "missing first exact result") id (Client.oracleClientRequestEvidence first settled)
      firstIndex = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue firstEntry)
      lateIndex = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue lateEntry)
      expectedPins = Map.fromList [(index, entry) | reference <- [first, second, lateReference], Just entry <- [Client.oracleClientRequestEvidence reference complete], let index = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue entry)]
      withoutBytes state = (Client.oracleClientStateWitness state) {Client.oracleClientWitnessRetainedEntryBytes = []}
  assertEqual "covering the prefix preserves all operational and request facts" (withoutBytes complete) (withoutBytes covered)
  assertEqual "covering the prefix emits the same outstanding actions" (Client.oracleClientActions complete) (Client.oracleClientActions covered)
  assertEqual "partial reclamation retains the canonical suffix" (Just (last unrelated)) (Client.oracleClientRetainedEntry prefix partial)
  assertEqual "no pending local request stalls the covered prefix" prefix (Client.oracleClientCanonicalCoveredThrough covered)
  assertEqual "the protected prefix exposes precisely the three exact local results" expectedPins (Client.oracleClientProtectedPrefixEvidence covered)
  assertEqual "a not-yet-settled first result remains indexed" (Just lateEntry) (Client.oracleClientRequestEvidence lateReference covered)
  assertEqual "an unobserved pending request has no fabricated result" Nothing (Client.oracleClientRequestEvidence pendingReference covered)
  forM_ unrelated $ \entry -> do
    let index = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue entry)
    assertEqual "unowned old canonical bytes are released" Nothing (Client.oracleClientRetainedEntry index covered)
    assertBool "removed old evidence is covered without an equality claim" (Client.oracleClientMatchesCoveredEntry entry covered)
    assertBool "coverage is not claimed before the prefix was reclaimed" (not (Client.oracleClientMatchesCoveredEntry entry complete))
    assertEqual "a covered entry cannot become a pending local result" (Left (Client.OracleRequestEntryMissing index)) (Client.preparedOracleResultClassification <$> Client.prepareOracleResult pendingReference entry covered)
  let settledLate = checked (Client.prepareOracleResult lateReference lateEntry covered)
      (afterLate, classification, _) = Client.commitOracleResult settledLate
      duplicate = checked (Client.prepareOracleResult first firstEntry covered)
      alternateProcess = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 0xfd))
      alternateEnvelope = Command.oracleEnvelope (Client.oracleRequestRefRequestId first) Nothing home (checked (Command.endProcessEpochCommand alternateProcess ExplicitAdministrativeEnd))
      (_, conflict) = canonicalStep alternateEnvelope (checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis))
      (reconnected, _) = reconnect (audited afterLate)
      settledRequestId = Client.oracleRequestRefRequestId first
      (lateReply, _) = ingress (Public.OracleRequestRetiredReceived (currentBinding reconnected) settledRequestId (Receipt.OracleRequestPrefixRetired (Client.oracleClientNextRequestSequence reconnected - 1))) reconnected
  assertEqual "the retained old coordinate remains exact" firstIndex (OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue conflict))
  assertBool "a conflicting retained exception is rejected despite coverage" (not (Client.oracleClientMatchesCoveredEntry conflict covered))
  assertEqual "exact result admission also rejects retained conflicts" (Left (Client.OracleRequestEntryConflict firstIndex)) (Client.preparedOracleResultClassification <$> Client.prepareOracleResult first conflict covered)
  assertEqual "late first settlement still uses its exact result" Client.OracleResultFirstRecorded classification
  assertEqual "earlier settled result remains an exact retry" Client.OracleResultExactRetry (Client.preparedOracleResultClassification duplicate)
  assertEqual "late settlement does not move its result coordinate" (Just lateEntry) (Client.oracleClientRetainedEntry lateIndex afterLate)
  assertEqual "reconnect preserves exact late-result evidence" (Just lateEntry) (Client.oracleClientRequestEvidence lateReference reconnected)
  assertEqual "a late retired reply remains inert" reconnected lateReply
  assertEqual "repeated coverage is inert" covered (checked (Client.reclaimOracleClientCanonicalPrefix prefix covered))
  assertEqual "regressed coverage never rewinds the owner" covered (checked (Client.reclaimOracleClientCanonicalPrefix partialPrefix covered))
  let future = Identity.controlIndex (Identity.controlIndexWord64 prefix + 1)
  assertEqual "coverage cannot cross unapplied control" (Left (Client.OracleCanonicalPrefixAhead prefix future)) (Client.reclaimOracleClientCanonicalPrefix future covered)
  pure True

caseCoveredUnsentLabelEnd :: Assertion
caseCoveredUnsentLabelEnd = do
  let process = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 30))
      object = checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 41))
      call = checked (Client.prepareDrainingLabelDecisionRequest "covered-unsent-label" process object (ProcessLabel process, 0) (Just (Label.initialBootstrapLabelEvidence object process)) Nothing Nothing Label.targetVoid (Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition process) EmptyHeraldPublicationPrefix) initialClient)
      (draining, _, _, reference) = Client.commitLabelDecisionRequest call
      home = Public.oracleHelloHeraldEpoch (Client.oracleClientHelloClaims draining)
      envelope = Command.oracleEnvelope (OracleIdentity.oracleClientRequestId home 1000) Nothing home (checked (Command.endProcessEpochCommand process ExplicitAdministrativeEnd))
      (_, endEntry) = canonicalStep envelope (checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis))
      index = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue endEntry)
      observed = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance endEntry draining))
      (ended, _, _) = Client.commitOracleRequest (checked (Client.prepareUnsentLabelEnd reference process index observed))
      covered = audited (checked (Client.reclaimOracleClientCanonicalPrefix index ended))
      retried = checked (Client.prepareUnsentLabelEnd reference process index covered)
  assertEqual "the unsent request retains its exact terminal End" (Map.singleton index endEntry) (Client.oracleClientProtectedPrefixEvidence covered)
  assertEqual "covering the End preserves the original unsent outcome" (Client.lookupOracleRequest reference ended) (Client.lookupOracleRequest reference covered)
  assertEqual "source-drain End retries still validate their canonical terminal" Client.OracleRequestExactRetry (Client.preparedOracleRequestClassification retried)
  assertEqual "an unsent request never gains Oracle result evidence" Nothing (Client.oracleClientRequestEvidence reference covered)
  assertEqual "coverage does not add a source-drained submission" [] (Client.oracleClientRequestDispatches covered)

canonicalStep :: Command.OracleEnvelope -> Oracle.OracleState -> (Oracle.OracleState, Canonical.CanonicalAppliedOracleEntry)
canonicalStep envelope oracle =
  let (next, _, effects) = checked (Transition.stepOracle envelope oracle)
   in (next, one [Canonical.canonicalizeAppliedOracleEntry entry | Effect.EmitAppliedOracleEntry entry <- Effect.oracleEffects effects])

clientCanonicalEntries :: Client.State -> [Canonical.CanonicalAppliedOracleEntry]
clientCanonicalEntries client =
  [ maybe (error "base fixture is missing exact control history") id (Client.oracleClientRetainedEntry (Identity.controlIndex index) client)
  | index <- [1 .. Identity.controlIndexWord64 (Client.oracleClientAppliedCursor client)]
  ]

projectionForClient :: Client.State -> Projection.State
projectionForClient = foldl (\projection entry -> Projection.commitAppliedEntry (checked (Projection.prepareAppliedEntry entry projection))) Fixtures.fixtureOracleProjectionState . clientCanonicalEntries

freshJoiningClient :: Client.State
freshJoiningClient =
  let claims = Client.oracleClientHelloClaims initialClient
      receiverId = checked (Identity.mkHeraldId (Fixtures.fixtureIdentifierBytes 0xe8))
      receiverEpoch = checked (Identity.mkHeraldEpoch (Fixtures.fixtureIdentifierBytes 0xe9))
      receiverClaims = Public.oracleHelloClaims (Public.oracleHelloSystemId claims) (Public.oracleHelloCatalogueDigest claims) (Public.oracleHelloConfigurationDigest claims) (Public.oracleHelloInitialProjectionDigest claims) receiverId receiverEpoch (Public.oracleHelloMembershipGeneration claims)
   in Client.initialJoiningState receiverClaims (Client.oracleClientContacts initialClient) (Identity.controlIndex 0)

-- The client pin owner is tested against real canonical decisions and results.
-- A failed comparison keeps the Oracle free for independent release schedules;
-- the tests explicitly control the composed consumer lifetime.
prepareDecision :: ByteString -> Oracle.OracleState -> Client.State -> (Oracle.OracleState, Client.State, Identity.LabelDecisionId, Identity.ControlIndex, Label.LabelOutcomeDigest)
prepareDecision = prepareDecisionWithGeneration 99

prepareDecisionWithGeneration :: Word64 -> ByteString -> Oracle.OracleState -> Client.State -> (Oracle.OracleState, Client.State, Identity.LabelDecisionId, Identity.ControlIndex, Label.LabelOutcomeDigest)
prepareDecisionWithGeneration = prepareDecisionForObject (checked (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 41)))

prepareDecisionForObject :: Identity.GlobalObjectId -> Word64 -> ByteString -> Oracle.OracleState -> Client.State -> (Oracle.OracleState, Client.State, Identity.LabelDecisionId, Identity.ControlIndex, Label.LabelOutcomeDigest)
prepareDecisionForObject object generation key oracle client =
  let process = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 30))
      call = checked (Client.prepareLabelDecisionRequest key process object (ProcessLabel process, generation) (Just (Label.initialBootstrapLabelEvidence object process)) Nothing Nothing Label.targetVoid (Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition process) EmptyHeraldPublicationPrefix) client)
      (pending, _, ident, reference) = Client.commitLabelDecisionRequest call
      (nextOracle, projected, result) = projectRequest oracle pending reference
      index = case result of Client.OracleRequestLabelDecided _ value -> value; _ -> error "expected canonical label decision"
      entry = maybe (error "missing label entry") id (Client.oracleClientRetainedEntry index projected)
      digest = one [value | event <- OracleProjection.appliedEntryProjectionEvents (Canonical.canonicalAppliedOracleEntryValue entry), OracleProjection.LabelDecidedView _ _ value <- [OracleProjection.oracleProjectionEventView event]]
      observed = audited (checked (Client.observeLabelDecision ident index projected))
   in (nextOracle, observed, ident, index, digest)

projectRequest :: Oracle.OracleState -> Client.State -> Client.OracleRequestRef -> (Oracle.OracleState, Client.State, Client.OracleRequestResult)
projectRequest oracle client reference =
  let witness = maybe (error "missing label fixture request") id (Client.lookupOracleRequest reference client)
      (nextOracle, _, effects) = checked (Transition.stepOracle (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) oracle)
      entry = one [Canonical.canonicalizeAppliedOracleEntry supplied | Effect.EmitAppliedOracleEntry supplied <- Effect.oracleEffects effects]
      advanced = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry client))
      (settled, _, result) = Client.commitOracleResult (checked (Client.prepareOracleResult reference entry advanced))
   in (nextOracle, audited settled, result)

settledFixture :: (Oracle.OracleState, Client.State, Client.OracleRequestRef, Client.OracleRequestRef)
settledFixture =
  let oracle = checked (Transition.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (firstPending, first) = prepareRequest "retirement-first" initialClient
      (afterFirstOracle, afterFirst) = settleRequest oracle firstPending first
      (secondPending, second) = prepareRequest "retirement-second" afterFirst
      (afterSecondOracle, afterSecond) = settleRequest afterFirstOracle secondPending second
   in (afterSecondOracle, afterSecond, first, second)

prepareRequest :: ByteString -> Client.State -> (Client.State, Client.OracleRequestRef)
prepareRequest key state =
  let prepared = checked (Client.prepareEndProcessEpochRequest key unknownProcess ExplicitAdministrativeEnd state)
      (successor, _, reference) = Client.commitOracleRequest prepared
   in (audited successor, reference)

settleRequest :: Oracle.OracleState -> Client.State -> Client.OracleRequestRef -> (Oracle.OracleState, Client.State)
settleRequest oracle client reference =
  let witness = maybe (error "missing retirement fixture request") id (Client.lookupOracleRequest reference client)
      envelope = Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)
      (nextOracle, _, effects) = checked (Transition.stepOracle envelope oracle)
      entry = one [Canonical.canonicalizeAppliedOracleEntry supplied | Effect.EmitAppliedOracleEntry supplied <- Effect.oracleEffects effects]
      advanced = Client.commitCursorAdvance (checked (Client.prepareCursorAdvance entry client))
      prepared = checked (Client.prepareOracleResult reference entry advanced)
      (settled, _, result) = Client.commitOracleResult prepared
   in case result of
        Client.OracleRequestRejected {} -> (nextOracle, audited settled)
        _ -> error "retirement fixture expected a rejected unknown-process End"

maintenanceEnvelope :: Client.State -> Canonical.CanonicalOracleEnvelope
maintenanceEnvelope state = one [envelope | Public.SubmitOracleProgress _ envelope <- snd (ingress (Public.OracleProgressFlush (currentBinding state)) state)]

ordinaryEnvelope :: Client.OracleRequestRef -> Client.State -> Canonical.CanonicalOracleEnvelope
ordinaryEnvelope reference state =
  one
    [ Client.oracleRequestDispatchEnvelope dispatch
    | Public.SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions state,
      Client.oracleRequestDispatchRef dispatch == reference
    ]

ingress :: Public.OracleClientIngress -> Client.State -> (Client.State, [Public.OracleClientAction])
ingress observation state =
  let prepared = checked (Client.prepareClientIngress observation state)
   in (audited (Client.commitClientIngress prepared), Client.preparedClientIngressActions prepared)

reconnect :: Client.State -> (Client.State, [Public.OracleClientAction])
reconnect state =
  let (disconnected, lostActions) = ingress (Public.OracleBindingLost (currentBinding state)) state
      retry = one [token | Public.ScheduleOracleRetry Public.OracleConnectionRetry token <- lostActions]
      (connecting, connectActions) = ingress (Public.OracleRetryElapsed retry) disconnected
      attempt = one [supplied | Public.ConnectAndHelloOracle supplied _ <- connectActions]
   in ingress (hello attempt (Client.oracleClientAppliedCursor state)) connecting

currentBinding :: Client.State -> Public.OracleBinding
currentBinding = maybe (error "retirement fixture is not bound") id . Client.oracleClientCurrentBinding

hello :: Public.OracleConnectAttempt -> Identity.ControlIndex -> Public.OracleClientIngress
hello attempt cursor =
  Public.OracleHelloReceived
    attempt
    ( Public.oracleHelloAcceptance
        (Public.oracleContactNode (Public.oracleConnectAttemptContact attempt))
        (Public.oracleObservedTerm 1)
        cursor
        Nothing
        True
    )

initialClient :: Client.State
initialClient =
  let genesis = Fixtures.fixtureStep14CheckedGenesis
      membership = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView Fixtures.fixtureOracleProjectionState)
      initial =
        Client.initialState
          ( Public.oracleHelloClaims
              (Genesis.checkedSystemId genesis)
              (Genesis.checkedCatalogueDigest genesis)
              (Genesis.checkedConfigurationDigest genesis)
              (Genesis.checkedInitialProjectionDigest Fixtures.fixtureCheckedInitialBootstraps)
              (Genesis.checkedLocalHeraldId genesis)
              (Genesis.checkedLocalHeraldEpoch genesis)
              (Membership.heraldMembershipGenerationId membership)
          )
          Fixtures.fixtureOracleContacts
          (Identity.controlIndex 0)
      attempt = one [supplied | Public.ConnectAndHelloOracle supplied _ <- Client.oracleClientActions initial]
   in fst (ingress (hello attempt (Identity.controlIndex 0)) initial)

unknownProcess :: Identity.ProcessEpochId
unknownProcess = checked (Identity.mkProcessEpochId (Fixtures.fixtureIdentifierBytes 0xfb))

decision :: Identity.LabelDecisionId
decision = checked (Identity.mkLabelDecisionId (Fixtures.fixtureIdentifierBytes 0xb1))

audited :: Client.State -> Client.State
audited state = case Client.validateState state of
  Right () -> state
  Left problem -> error ("retirement client invariant: " <> show problem)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

one :: [value] -> value
one [value] = value
one _ = error "retirement fixture expected exactly one result"

receiptOnly :: Lifetime.ReceiptRetirement -> Progress.OracleProgress
receiptOnly progress = Progress.oracleProgress progress (Identity.controlIndex 0)
