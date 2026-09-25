{-# LANGUAGE OverloadedStrings #-}

module OracleClientProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString.Char8 qualified as Bytes
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    LabelDecisionId,
    controlIndex,
    mkLabelDecisionId,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label
  ( LabelOutcomeDigest,
    mkLabelOutcomeDigest,
  )
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.SortOccurrence (SortOccurrenceBase (Genesis))
import Eclips.Domain.Value (LabelOwner (VoidLabel))
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleClient qualified as PublicClient
import Eclips.Herald.OracleClient.State
  ( OracleClientProblem (OracleRequestResultMismatch),
    OracleRequestResult (OracleRequestLabelCompleted),
    interpretLabelCompletionEventView,
  )
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Canonical qualified as Canonical
import Eclips.Oracle.Command qualified as Command
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Effect qualified as Effect
import Eclips.Oracle.Identity qualified as OracleIdentity
import Eclips.Oracle.Projection
  ( OracleProjectionEventView
      ( LabelWorkflowCompletedView,
        ProcessEpochEndedEventView
      ),
  )
import Eclips.Oracle.Projection qualified as OracleProjection
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition qualified as Oracle
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures (fixtureIdentifierBytes)
import GenesisFixtures qualified as Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Oracle-client typed result vectors"
    [ testCase "completion admits its exact immutable outcome" caseCompoundOutcome,
      testCase "discovered Oracle routes preserve the established lane and semantic cursor" caseDiscoveredOracleContacts,
      testCase "committed voter preference selects a newly discovered route after reconnect" casePreferredOracleReconnect,
      testCase "voter cancellation bypasses an ordinary submission retry without losing either request" caseCancellationBypassesRetry,
      testCase "completion identity and outcome mismatches are rejected" caseCompoundOutcomeMismatch,
      testCase "rejected Open remains stable until a later contiguous projection permits reevaluation" caseRejectedOpenRearm,
      testCase "a deferral before the initial cursor is rejected without changing the retained request" caseDeferralBeforeInitialCursor,
      testCase "busy decision survives reconnect and reoffers the exact envelope after completion" caseBusyDecisionReconnect,
      testCase "canonical caller End settles only a provably unsent label decision and closes its receipt hole" caseUnsentLabelEnd,
      QC.testProperty "source drain reserves exact label decisions without blocking unrelated traffic or dispatching on reconnect" propDrainingLabelRequests,
      QC.testProperty "completed batches discard foreign rejections and maintenance while preserving exact local retries" propSparseClientEvidence,
      QC.testProperty "long request histories retain the full audit through deferral, retry and settlement" propRetainedHistoryAudit,
      QC.testProperty "receipt retirement advances only through projected holes and preserves local exact retries" propReceiptRetirement,
      QC.testProperty "flowing requests piggyback retirement without another control entry" propPiggybackFlow,
      QC.testProperty "aliased Opens and terminals settle against exact history for every report ordering" propDisappearanceOwnership
    ]

propDrainingLabelRequests :: QC.NonNegative Int -> QC.Property
propDrainingLabelRequests (QC.NonNegative seed) =
  case run of
    Left problem -> QC.counterexample problem False
    Right () -> QC.property True
  where
    count = 2 + seed `mod` 8
    key ordinal = Bytes.pack ("source-draining-label-" <> show ordinal)
    run = do
      reserved <- foldM reserve initialClient [1 .. count]
      ensure (null (Client.oracleClientRequestDispatches reserved)) "a reserved label decision became dispatchable before source drain"
      unrelated <- shown (Client.prepareEndProcessEpochRequest "unrelated-end-during-drain" unknownProcess ExplicitAdministrativeEnd reserved)
      let (withOtherRequest, _, otherRef) = Client.commitOracleRequest unrelated
          references = map fst (Client.oracleClientRequestEntries reserved)
          dispatchReferences state = map Client.oracleRequestDispatchRef (Client.oracleClientRequestDispatches state)
      ensure (dispatchReferences withOtherRequest == [otherRef]) "source drain prevented an unrelated request from being dispatched"
      (reconnected, actions) <- reconnectClient withOtherRequest
      ensure (dispatchReferences reconnected == [otherRef]) "reconnect released a source-draining label decision"
      ensure
        ([PublicClient.oracleRequestDispatchRef dispatch | PublicClient.SubmitOracleRequest _ dispatch <- actions] == [otherRef])
        "reconnect actions submitted a held label decision or omitted unrelated work"
      ensure
        (all (\reference -> Client.lookupOracleRequest reference reserved == Client.lookupOracleRequest reference reconnected) references)
        "reconnect changed a reserved request witness"
      final <- foldM release reconnected [1 .. count]
      ensure (length (dispatchReferences final) == count + 1) "drained label decisions did not join dispatchable traffic"
      shown (Client.validateState final)
    reserve state ordinal = do
      prepared <- shown (prepareTestDecision True (key ordinal) labelCaller state)
      let (reserved, classification, _, reference) = Client.commitLabelDecisionRequest prepared
      ensure (classification == Client.OracleRequestFirstPrepared) "fresh draining label decision was classified as a retry"
      witness <- maybe (Left "reserved label decision missing") Right (Client.lookupOracleRequest reference reserved)
      ensure (Client.oracleRequestWitnessStatus witness == Client.OracleRequestAwaitingSourceDrain) "new label decision was not source-draining"
      ensure (Client.lookupOracleRequestBySemanticKey (key ordinal) reserved == Just witness) "semantic-key lookup lost a reserved label decision"
      -- The legacy immediate entry point must not accidentally release an
      -- already accepted request when a caller repeats its semantic intent.
      repeated <- shown (prepareTestDecision False (key ordinal) labelCaller reserved)
      let (unchanged, repeatedClass, _, repeatedRef) = Client.commitLabelDecisionRequest repeated
      ensure (unchanged == reserved && repeatedClass == Client.OracleRequestExactRetry && repeatedRef == reference) "exact retry released or replaced a held label decision"
      shown (Client.validateState reserved)
      pure reserved
    release state ordinal = do
      witness <- maybe (Left "reserved semantic key disappeared") Right (Client.lookupOracleRequestBySemanticKey (key ordinal) state)
      let reference = Client.oracleRequestWitnessRef witness
      prepared <- shown (Client.prepareDrainedLabelSubmission reference state)
      let (released, classification, releasedRef) = Client.commitOracleRequest prepared
      submitted <- maybe (Left "released label decision disappeared") Right (Client.lookupOracleRequest reference released)
      ensure (classification == Client.OracleRequestFirstPrepared && releasedRef == reference) "source drain allocated another request identity"
      ensure
        ( Client.oracleRequestWitnessStatus submitted == Client.OracleRequestAwaitingProjection
            && Client.oracleRequestWitnessEnvelope submitted == Client.oracleRequestWitnessEnvelope witness
            && Client.oracleRequestWitnessIntent submitted == Client.oracleRequestWitnessIntent witness
        )
        "source drain changed accepted label decision bytes or failed to enable submission"
      repeated <- shown (Client.prepareDrainedLabelSubmission reference released)
      let (unchanged, repeatedClass, _) = Client.commitOracleRequest repeated
      ensure (unchanged == released && repeatedClass == Client.OracleRequestExactRetry) "drain replay changed submitted work"
      retried <- shown (prepareTestDecision True (key ordinal) labelCaller released)
      let (afterRetry, _, _, _) = Client.commitLabelDecisionRequest retried
      ensure (afterRetry == released) "application retry moved a submitted request back to source drain"
      shown (Client.validateState released)
      pure released

caseUnsentLabelEnd :: Assertion
caseUnsentLabelEnd = case run of
  Left problem -> error problem
  Right () -> pure ()
  where
    key = "label-ended-before-submission"
    run = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      prepared <- shown (prepareTestDecision True key labelCaller initialClient)
      let (held, _, _, reference) = Client.commitLabelDecisionRequest prepared
      witness <- maybe (Left "reserved label decision missing") Right (Client.lookupOracleRequest reference held)
      ensure
        (case Client.prepareUnsentLabelEnd reference labelCaller (controlIndex 1) held of Left Client.OracleRequestControlMismatch -> True; _ -> False)
        "unprojected caller End authorized cancellation"
      endPrepared <- shown (Client.prepareEndProcessEpochRequest "end-draining-caller" labelCaller ExplicitAdministrativeEnd held)
      let (ending, _, endRef) = Client.commitOracleRequest endPrepared
      endWitness <- maybe (Left "caller End request missing") Right (Client.lookupOracleRequest endRef ending)
      (_, advanced, _, endEntry) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope endWitness)) oracle ending Fixtures.fixtureOracleProjectionState
      result <- shown (Client.prepareOracleResult endRef endEntry advanced)
      let (endSettled, _, _) = Client.commitOracleResult result
          endIndex = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue endEntry)
      ensure (Client.oracleClientReceiptRetirementReady endSettled == 0) "unfinished source drain did not retain its receipt hole"
      ensure
        (case Client.prepareUnsentLabelEnd reference unknownProcess endIndex endSettled of Left Client.OracleRequestResultMismatch -> True; _ -> False)
        "another process End authorized cancellation"
      ensure
        (case Client.prepareDrainedLabelSubmission endRef endSettled of Left Client.OracleRequestResultMismatch -> True; _ -> False)
        "source drain transition accepted a non-label decision request"
      released <- shown (Client.prepareDrainedLabelSubmission reference endSettled)
      let (submitted, _, _) = Client.commitOracleRequest released
      ensure
        (case Client.prepareUnsentLabelEnd reference labelCaller endIndex submitted of Left Client.OracleRequestResultMismatch -> True; _ -> False)
        "caller End cancelled a request after submission was enabled"
      cancelled <- shown (Client.prepareUnsentLabelEnd reference labelCaller endIndex endSettled)
      let (settled, classification, settledRef) = Client.commitOracleRequest cancelled
      retained <- maybe (Left "locally settled label decision missing") Right (Client.lookupOracleRequestBySemanticKey key settled)
      ensure (classification == Client.OracleRequestFirstPrepared && settledRef == reference) "local caller End changed label decision identity"
      ensure
        ( Client.oracleRequestWitnessStatus retained == Client.OracleRequestEndedBeforeSubmission endIndex
            && Client.oracleRequestWitnessEnvelope retained == Client.oracleRequestWitnessEnvelope witness
            && Client.oracleRequestWitnessIntent retained == Client.oracleRequestWitnessIntent witness
        )
        "local caller End lost its proof or changed immutable label decision bytes"
      ensure (Client.oracleClientReceiptRetirementReady settled == 2) "local caller End failed to close the receipt hole"
      ensure (null (Client.oracleClientRequestDispatches settled)) "locally ended label decision remained dispatchable"
      ensure
        (case Client.prepareDrainedLabelSubmission reference settled of Left Client.OracleRequestResultMismatch -> True; _ -> False)
        "locally ended label decision was resurrected for submission"
      replay <- shown (Client.prepareUnsentLabelEnd reference labelCaller endIndex settled)
      let (replayed, replayClass, _) = Client.commitOracleRequest replay
      ensure (replayed == settled && replayClass == Client.OracleRequestExactRetry) "caller End replay changed local settlement"
      retried <- shown (prepareTestDecision True key labelCaller settled)
      let (unchanged, retryClass, _, retryRef) = Client.commitLabelDecisionRequest retried
      ensure (unchanged == settled && retryClass == Client.OracleRequestExactRetry && retryRef == reference) "application retry replaced locally ended label decision"
      (reconnected, actions) <- reconnectClient settled
      ensure (null [() | PublicClient.SubmitOracleRequest {} <- actions]) "reconnect submitted a locally ended label decision"
      ensure (Client.lookupOracleRequest reference reconnected == Just retained) "reconnect erased the local End witness"
      shown (Client.validateState reconnected)

prepareTestDecision ::
  Bool ->
  Bytes.ByteString ->
  Identity.ProcessEpochId ->
  Client.State ->
  Either Client.OracleClientProblem Client.PreparedLabelDecisionRequest
prepareTestDecision draining key caller =
  (if draining then Client.prepareDrainingLabelDecisionRequest else Client.prepareLabelDecisionRequest)
    key
    caller
    (checked (Identity.mkGlobalObjectId (fixtureIdentifierBytes 0xfc)))
    (VoidLabel, 0)
    ( Label.initialPublicationLabelEvidence
        (checked (Identity.mkGlobalObjectId (fixtureIdentifierBytes 0xfc)))
        (VoidLabel, 0)
        ( Identity.publicationId
            (checked (Identity.mkNablaId (fixtureIdentifierBytes 0xfd)))
            Identity.genesisAuthorityEpoch
            localEpoch
            (Identity.nablaSequence 1)
        )
    )
    Nothing
    Nothing
    Label.targetVoid
    (Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition caller) EmptyHeraldPublicationPrefix)

labelCaller :: Identity.ProcessEpochId
labelCaller = case [process | process <- Projection.projectedProcessEpochs Fixtures.fixtureOracleProjectionState, Projection.oracleViewProcessResidence process (Projection.oracleView Fixtures.fixtureOracleProjectionState) == Just localEpoch] of
  process : _ -> process
  [] -> error "fixture has no local caller"

reconnectClient :: Client.State -> Either String (Client.State, [PublicClient.OracleClientAction])
reconnectClient state = do
  binding <- maybe (Left "client has no established binding") Right (Client.oracleClientCurrentBinding state)
  lost <- shown (Client.prepareClientIngress (PublicClient.OracleBindingLost binding) state)
  retry <- case Client.preparedClientIngressActions lost of
    [PublicClient.ScheduleOracleRetry PublicClient.OracleConnectionRetry value] -> Right value
    other -> Left ("expected connection retry, got " <> show other)
  reconnect <- shown (Client.prepareClientIngress (PublicClient.OracleRetryElapsed retry) (Client.commitClientIngress lost))
  attempt <- case [value | PublicClient.ConnectAndHelloOracle value _ <- Client.preparedClientIngressActions reconnect] of
    [value] -> Right value
    other -> Left ("expected reconnect, got " <> show other)
  accepted <-
    shown
      ( Client.prepareClientIngress
          ( PublicClient.OracleHelloReceived
              attempt
              ( PublicClient.oracleHelloAcceptance
                  (PublicClient.oracleContactNode (PublicClient.oracleConnectAttemptContact attempt))
                  (PublicClient.oracleObservedTerm 1)
                  (Client.oracleClientAppliedCursor state)
                  Nothing
                  True
              )
          )
          (Client.commitClientIngress reconnect)
      )
  pure (Client.commitClientIngress accepted, Client.preparedClientIngressActions accepted)

propReceiptRetirement :: QC.Positive Int -> QC.Property
propReceiptRetirement (QC.Positive seed) =
  QC.forAll (QC.shuffle [1 .. count]) $ \order ->
    case run order of
      Left problem -> QC.counterexample problem False
      Right () -> QC.property True
  where
    count = 2 + seed `mod` 14
    run order = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      requested <- foldM prepare initialClient [1 .. count]
      (_, final, _) <- foldM settle (oracle, requested, Fixtures.fixtureOracleProjectionState) order
      ensure (Client.oracleClientReceiptRetirementReady final == fromIntegral count) "prefix did not close"
      ensure (Client.oracleClientReceiptRetirementConfirmed final == fromIntegral count) "retirement was not projected"
      ensure (length (Client.oracleClientRequestEntries final) == count) "retirement deleted local evidence"
      ensure (Client.oracleClientNextRequestSequence final == fromIntegral count + 1) "maintenance allocated a request sequence"
      shown (Client.validateState final)
    key ordinal = Bytes.pack ("retirement-label-" <> show ordinal)
    prepare client ordinal = do
      prepared <- shown (prepareTestDecision False (key ordinal) unknownProcess client)
      let (requested, _, _, reference) = Client.commitLabelDecisionRequest prepared
      -- Retain an actual deferred low request while later requests complete.
      if ordinal == 1
        then do
          deferred <- shown (Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId reference) decision (Client.oracleClientAppliedCursor requested) requested)
          pure (Client.commitOracleSubmissionDeferral deferred)
        else pure requested
    settle (oracle, client, projection) ordinal = do
      retry <- shown (prepareTestDecision False (key ordinal) unknownProcess client)
      let reference = Client.preparedLabelDecisionRequestRef retry
      released <- if ordinal == 1 then shown (Client.releaseCompletedOracleDeferrals (== decision) client) else pure client
      witness <- maybe (Left "missing witness") Right (Client.lookupOracleRequest reference released)
      let envelope = Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)
      (nextOracle, advanced, nextProjection, entry) <- submitAndProject envelope oracle released projection
      result <- shown (Client.prepareOracleResult reference entry advanced)
      let (settled, _, _) = Client.commitOracleResult result
          unresolved = sort [OracleIdentity.oracleClientRequestSequence (Client.oracleRequestRefRequestId ref) | (ref, retained) <- Client.oracleClientRequestEntries settled, case Client.oracleRequestWitnessStatus retained of Client.OracleRequestProjected _ -> False; _ -> True]
          expected = case unresolved of [] -> fromIntegral count; first : _ -> first - 1
      ensure (Client.oracleClientReceiptRetirementReady settled == expected) "retirement crossed an unresolved hole"
      (compacted, confirmed, projected) <- case [ack | PublicClient.SubmitOracleProgress _ ack <- retirementFlushActions settled] of
        [] -> pure (nextOracle, settled, nextProjection)
        [ack] -> do
          (updated, withAck, projectionWithAck, _) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue ack) nextOracle settled nextProjection
          (replayed, replayOutcome, replayEffects) <- shown (Oracle.stepOracle (Canonical.canonicalOracleEnvelopeValue ack) updated)
          ensure (replayed == updated && null (Effect.oracleEffects replayEffects)) ("maintenance replay changed state: " <> show replayOutcome)
          pure (updated, withAck, projectionWithAck)
        _ -> Left "multiple retirement intentions"
      if OracleIdentity.oracleClientRequestSequence (Client.oracleRequestRefRequestId reference) <= expected
        then do
          (_, outcome, effects) <- shown (Oracle.stepOracle envelope compacted)
          ensure (null (Effect.oracleEffects effects)) ("obsolete retry executed: " <> show outcome)
        else pure ()
      exact <- shown (prepareTestDecision False (key ordinal) unknownProcess confirmed)
      ensure (Client.preparedLabelDecisionRequestClassification exact == Client.OracleRequestExactRetry && Client.preparedLabelDecisionRequestRef exact == reference) "local retry allocated another request"
      _ <- shown (Client.prepareOracleResult reference entry confirmed)
      shown (Client.validateState confirmed)
      pure (compacted, confirmed, projected)

retirementFlushActions :: Client.State -> [PublicClient.OracleClientAction]
retirementFlushActions state = case Client.oracleClientCurrentBinding state of
  Nothing -> []
  Just binding -> Client.preparedClientIngressActions (checked (Client.prepareClientIngress (PublicClient.OracleProgressFlush binding) state))

propPiggybackFlow :: QC.Positive Int -> QC.Property
propPiggybackFlow (QC.Positive seed) = case run of
  Left problem -> QC.counterexample problem False
  Right () -> QC.property True
  where
    count = 2 + seed `mod` 25
    run = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (flowing, client, projection) <- foldM step (oracle, initialClient, Fixtures.fixtureOracleProjectionState) [1 .. count]
      ensure (Oracle.oracleGreatestControlIndex flowing == controlIndex (fromIntegral count)) "piggyback created a separate control entry"
      ensure (Oracle.oracleRequestCount flowing == 1) "flowing work retained more than its newest receipt"
      ack <- case [value | PublicClient.SubmitOracleProgress _ value <- retirementFlushActions client] of
        [value] -> pure value
        _ -> Left "idle flush missing"
      (drained, confirmed, _, _) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue ack) flowing client projection
      ensure (Oracle.oracleRequestCount drained == 0 && Client.oracleClientReceiptRetirementConfirmed confirmed == fromIntegral count) "idle tail was not reclaimed"
    step (oracle, client, projection) ordinal = do
      prepared <- shown (Client.prepareEndProcessEpochRequest (Bytes.pack ("piggyback-end-" <> show ordinal)) unknownProcess ExplicitAdministrativeEnd client)
      let (requested, _, reference) = Client.commitOracleRequest prepared
      witness <- maybe (Left "missing witness") Right (Client.lookupOracleRequest reference requested)
      envelope <- case [PublicClient.oracleRequestDispatchEnvelope dispatch | PublicClient.SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions requested, PublicClient.oracleRequestDispatchRef dispatch == reference] of
        [value] -> pure value
        _ -> Left "missing dispatch"
      ensure (Canonical.canonicalOracleEnvelopeDigest envelope == Canonical.canonicalOracleEnvelopeDigest (Client.oracleRequestWitnessEnvelope witness)) "metadata changed semantic request identity"
      ensure (Command.oracleEnvelopeReceiptRetirement (Canonical.canonicalOracleEnvelopeValue envelope) == if ordinal == 1 then Nothing else Just (fromIntegral ordinal - 1)) "dispatch did not carry latest ready frontier"
      (updated, advanced, projected, entry) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue envelope) oracle requested projection
      settled <- shown (Client.prepareOracleResult reference entry advanced)
      let (next, _, _) = Client.commitOracleResult settled
      ensure (Oracle.oracleRequestCount updated == 1) "piggyback failed to reclaim predecessor receipt"
      shown (Client.validateState next)
      pure (updated, next, projected)

caseDiscoveredOracleContacts :: Assertion
caseDiscoveredOracleContacts = do
  let hints = checked (PublicClient.oracleContactSet (NonEmpty.singleton addedOracleContact))
      prepared = checked (Client.prepareClientIngress (PublicClient.OracleContactsDiscovered hints) initialClient)
      learned = Client.commitClientIngress prepared
  assertEqual "contact hints create no runtime or semantic action" [] (Client.preparedClientIngressActions prepared)
  assertEqual "physical binding remains unchanged" (Client.oracleClientCurrentBinding initialClient) (Client.oracleClientCurrentBinding learned)
  assertEqual "watch cursor remains unchanged" (Client.oracleClientAppliedCursor initialClient) (Client.oracleClientAppliedCursor learned)
  assertEqual "Hello bootstrap authority remains unchanged" (Client.oracleClientHelloClaims initialClient) (Client.oracleClientHelloClaims learned)
  assertBool "new route is available" (addedOracleContact `elem` NonEmpty.toList (PublicClient.oracleContacts (Client.oracleClientContacts learned)))
  assertEqual "duplicate discovery is idempotent" learned (Client.observeOracleContactHints hints learned)
  assertEqual "owner audit accepts discovered routes" (Right ()) (Client.validateState learned)

casePreferredOracleReconnect :: Assertion
casePreferredOracleReconnect = do
  let hints = checked (PublicClient.oracleContactSet (NonEmpty.singleton addedOracleContact))
      preferred = Client.preferOracleVoters (Set.singleton (PublicClient.oracleContactNode addedOracleContact)) (Client.observeOracleContactHints hints initialClient)
      binding = maybe (error "fixture has no established Oracle") id (Client.oracleClientCurrentBinding preferred)
      lost = checked (Client.prepareClientIngress (PublicClient.OracleBindingLost binding) preferred)
      retrying = Client.commitClientIngress lost
      retry = case Client.preparedClientIngressActions lost of
        [PublicClient.ScheduleOracleRetry PublicClient.OracleConnectionRetry value] -> value
        other -> error ("expected one connection retry: " <> show other)
      reconnect = checked (Client.prepareClientIngress (PublicClient.OracleRetryElapsed retry) retrying)
      connectedRoute = [PublicClient.oracleConnectAttemptContact attempt | PublicClient.ConnectAndHelloOracle attempt _ <- Client.preparedClientIngressActions reconnect]
  assertEqual "new voter route is tried first" [addedOracleContact] connectedRoute
  assertEqual "historical routes remain available" True (all (`elem` NonEmpty.toList (PublicClient.oracleContacts (Client.oracleClientContacts preferred))) (NonEmpty.toList (PublicClient.oracleContacts (Client.oracleClientContacts initialClient))))
  assertEqual "owner audit accepts reconnect" (Right ()) (Client.validateState (Client.commitClientIngress reconnect))

addedOracleContact :: PublicClient.OracleContact
addedOracleContact = checked (PublicClient.oracleContact (checked (PublicClient.oracleNodeClaim (Bytes.replicate 32 'v'))) "oracle-new" 39118)

caseCancellationBypassesRetry :: Assertion
caseCancellationBypassesRetry = do
  let ordinary = checked (Client.prepareEndProcessEpochRequest "ordinary-during-preparation" unknownProcess ExplicitAdministrativeEnd initialClient)
      (requested, _, ordinaryRef) = Client.commitOracleRequest ordinary
      binding = maybe (error "fixture has no Oracle binding") id (Client.oracleClientCurrentBinding requested)
      postponed = checked (Client.prepareClientIngress (PublicClient.OracleSubmissionNotReadyReceived binding (Client.oracleRequestRefRequestId ordinaryRef) (PublicClient.oracleObservedTerm 1)) requested)
      waiting = Client.commitClientIngress postponed
      ident = checked (Voter.mkVoterChangeId (controlIndex 1))
      operation = Client.CancelOracleVoterChange ident
      cancellation = checked (Client.prepareVoterAdministrationRequest "cancel-preparation" operation waiting)
      (cancelling, _, cancelRef) = Client.commitOracleRequest cancellation
      submitted state = [PublicClient.oracleRequestDispatchRef dispatch | PublicClient.SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions state]
      duplicate = checked (Client.prepareVoterAdministrationRequest "cancel-preparation" operation cancelling)
  assertEqual "ordinary work is parked behind the retry" [] (submitted waiting)
  assertEqual "cancellation can terminate the preparation while the timer is pending" [cancelRef] (submitted cancelling)
  assertEqual "neither retained envelope was discarded" 2 (length (Client.oracleClientRequestEntries cancelling))
  assertEqual "cancellation retry retains its exact request identity" cancelRef (Client.preparedOracleRequestRef duplicate)
  assertEqual "cancellation retry is classified exactly" Client.OracleRequestExactRetry (Client.preparedOracleRequestClassification duplicate)
  assertEqual "client audit accepts the bypass" (Right ()) (Client.validateState cancelling)

caseDeferralBeforeInitialCursor :: Assertion
caseDeferralBeforeInitialCursor = do
  let initial = Client.initialJoiningState (Client.oracleClientHelloClaims initialClient) (Client.oracleClientContacts initialClient) (controlIndex 5)
      prepared = checked (prepareTestDecision False "before-initial" unknownProcess initial)
      (client, _, _, reference) = Client.commitLabelDecisionRequest prepared
  assertEqual "valid initial cursor and request" (Right ()) (Client.validateState client)
  case Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId reference) decision (controlIndex 4) client of
    Left Client.OracleClientStateContradiction -> pure ()
    other -> error ("expected the original deferral-bound contradiction, got " <> either show (const "admitted") other)

caseBusyDecisionReconnect :: Assertion
caseBusyDecisionReconnect = case run of
  Left problem -> error problem
  Right () -> pure ()
  where
    run = do
      prepared <- shown (prepareTestDecision False "busy-reconnect" labelCaller initialClient)
      let (requested, _, _, reference) = Client.commitLabelDecisionRequest prepared
      original <- maybe (Left "decision witness missing") Right (Client.lookupOracleRequest reference requested)
      deferred <- shown (Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId reference) decision (Client.oracleClientAppliedCursor requested) requested)
      let retained = Client.commitOracleSubmissionDeferral deferred
      ensure (null (Client.oracleClientRequestDispatches retained)) "busy request remained dispatchable"
      (reconnected, actions) <- reconnectClient retained
      ensure (null [() | PublicClient.SubmitOracleRequest {} <- actions]) "reconnect blindly resubmitted the busy decision"
      incomplete <- shown (Client.releaseCompletedOracleDeferrals (const False) reconnected)
      ensure (incomplete == reconnected) "unrelated progress released a busy request"
      ready <- shown (Client.releaseCompletedOracleDeferrals (== decision) incomplete)
      resumed <- maybe (Left "resumed decision missing") Right (Client.lookupOracleRequest reference ready)
      ensure (Client.oracleRequestWitnessEnvelope resumed == Client.oracleRequestWitnessEnvelope original) "completion changed the canonical request bytes"
      ensure (Client.oracleRequestWitnessIntent resumed == Client.oracleRequestWitnessIntent original) "completion changed the decision identity"
      ensure (map Client.oracleRequestDispatchRef (Client.oracleClientRequestDispatches ready) == [reference]) "completion did not reoffer the one retained request"
      end <- shown (Client.prepareEndProcessEpochRequest "end-never-defers" labelCaller ExplicitAdministrativeEnd ready)
      let (withEnd, _, endRef) = Client.commitOracleRequest end
      ensure
        (case Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId endRef) decision (Client.oracleClientAppliedCursor withEnd) withEnd of Left Client.OracleSubmissionDeferralUnsupportedIntent {} -> True; _ -> False)
        "the removed End deferral remained admissible"
      shown (Client.validateState withEnd)

unknownProcess :: Identity.ProcessEpochId
unknownProcess = checked (Identity.mkProcessEpochId (fixtureIdentifierBytes 0xfb))

-- A local request is an independent sparse pin, never a prefix-retirement
-- barrier. Every unrelated rejection and its progress entry is reclaimed at
-- the completed-batch boundary while Projection keeps the exact shared proof.
propSparseClientEvidence :: QC.Positive Int -> QC.Property
propSparseClientEvidence (QC.Positive seed) = case run of
  Left problem -> QC.counterexample problem False
  Right () -> QC.property True
  where
    count = 2 + seed `mod` 25
    run = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      prepared <- shown (Client.prepareEndProcessEpochRequest "sparse-local-rejection" unknownProcess ExplicitAdministrativeEnd initialClient)
      let (pending, _, reference) = Client.commitOracleRequest prepared
      witness <- maybe (Left "missing local request") Right (Client.lookupOracleRequest reference pending)
      (afterLocal, advanced, projection, localEntry) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) oracle pending Fixtures.fixtureOracleProjectionState
      result <- shown (Client.prepareOracleResult reference localEntry advanced)
      let (settled, _, _) = Client.commitOracleResult result
      (_, final, _) <- foldM cycleEntries (afterLocal, settled, projection) [1 .. count]
      ensure (Client.oracleClientRequestEntries final == Client.oracleClientRequestEntries settled) "compaction changed the exact local request witness"
      ensure (Client.oracleClientEvidenceCompactedThrough final == controlIndex (1 + 2 * fromIntegral count)) "the oldest sparse local pin held back the compaction coordinate"
      ensure (Client.oracleClientRetainedEntry (controlIndex 1) final == Just localEntry) "the local rejected result lost its exact evidence"
      let laterInitial = Client.initialState (Client.oracleClientHelloClaims initialClient) Fixtures.fixtureOracleContacts (controlIndex 1)
      ensure (not (Client.oracleClientRetainsEntry localEntry laterInitial) && not (Client.oracleClientMatchesProjectedEntry localEntry laterInitial)) "evidence at the initial cursor was attributed to an unobserving client"
      exact <- shown (Client.prepareOracleResult reference localEntry final)
      let (retried, classification, _) = Client.commitOracleResult exact
      ensure (retried == final && classification == Client.OracleResultExactRetry) "a local result retry changed after unrelated history reclamation"
      repeated <- shown (Client.compactOracleClientEvidence final)
      ensure (repeated == final) "repeated compaction changed the owner"
      shown (Client.validateState final)
    cycleEntries (oracle, client, projection) ordinal = do
      reject <- shown (Command.endProcessEpochCommand unknownProcess ExplicitAdministrativeEnd)
      (afterReject, rejected, rejectedProjection, rejectionEntry) <- submitAndProject (externalEnvelope fixtureRemoteHerald (fromIntegral ordinal) reject) oracle client projection
      (afterAck, acknowledged, ackProjection, ackEntry) <- submitAndProject (externalEnvelope fixtureRemoteHerald (fromIntegral ordinal) (Command.retireOracleReceiptsCommand (fromIntegral ordinal))) afterReject rejected rejectedProjection
      mapM_ (checkDropped acknowledged ackProjection) [rejectionEntry, ackEntry]
      ensure (length (Client.oracleClientWitnessRetainedEntryBytes (Client.oracleClientStateWitness acknowledged)) == 1) "unrelated control history accumulated in the sparse client archive"
      ensure (Client.oracleClientReceiptRetirementReady acknowledged == Client.oracleClientReceiptRetirementReady client && Client.oracleClientProgressReady acknowledged == Client.oracleClientProgressReady client) "foreign compaction changed local progress"
      pure (afterAck, acknowledged, ackProjection)
    checkDropped client projection entry = do
      let index = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue entry)
      ensure (Client.oracleClientRetainedEntry index client == Nothing) "discardable entry remains in the client archive"
      exact <- shown (Projection.prepareAppliedEntry entry projection)
      ensure (Projection.preparedAppliedEntryClassification exact == Projection.AppliedEntryExactDuplicate index) "Projection lost the canonical exact-duplicate proof"
      ensure (Client.oracleClientMatchesProjectedEntry entry client) "the compacted client rejected a Projection-proven old duplicate"

propRetainedHistoryAudit :: QC.NonNegative Int -> Bool -> QC.Property
propRetainedHistoryAudit (QC.NonNegative seed) defer =
  case run of
    Left problem -> QC.counterexample problem False
    Right () -> QC.property True
  where
    count = 12 + seed `mod` 25
    run = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      (_, final, _) <- foldM step (oracle, initialClient, Fixtures.fixtureOracleProjectionState) [1 .. count]
      ensure (length (Client.oracleClientRequestEntries final) == count) "settlement lost a retained request"
      shown (Client.validateState final)
    audit state = do
      shown (Client.validateState state)
      ensure
        (all (\(reference, witness) -> Client.lookupOracleRequestById (Client.oracleRequestRefRequestId reference) state == Just witness) (Client.oracleClientRequestEntries state))
        "exact request-id lookup disagreed with the independently enumerated retained witnesses"
      ensure
        (all (\(_, witness) -> Client.lookupOracleRequestBySemanticKey (Client.oracleRequestWitnessSemanticKey witness) state == Just witness) (Client.oracleClientRequestEntries state))
        "semantic-key lookup disagreed with retained request history"
      ensure
        (Client.lookupOracleRequestBySemanticKey "unallocated-semantic-key" state == Nothing)
        "an unallocated semantic key matched retained history"
      ensure
        (Client.lookupOracleRequestById (OracleIdentity.oracleClientRequestId localEpoch (Client.oracleClientNextRequestSequence state)) state == Nothing)
        "an unallocated request id matched retained history"
      ensure
        (Client.lookupOracleRequestById (OracleIdentity.oracleClientRequestId fixtureRemoteHerald 1) state == Nothing)
        "a foreign request home matched a local request sequence"
      pure state
    step (oracle, client, projection) ordinal = do
      let key = Bytes.pack ("retained-label-" <> show ordinal)
      prepared <- shown (prepareTestDecision False key unknownProcess client)
      let (requested, _, _, reference) = Client.commitLabelDecisionRequest prepared
      _ <- audit requested
      repeated <- shown (prepareTestDecision False key unknownProcess requested)
      ensure (Client.preparedLabelDecisionRequestClassification repeated == Client.OracleRequestExactRetry && Client.preparedLabelDecisionRequestRef repeated == reference) "exact intent retry allocated a new identity"
      released <-
        if defer
          then do
            pending <- shown (Client.prepareOracleSubmissionDeferral (Client.oracleRequestRefRequestId reference) decision (Client.oracleClientAppliedCursor requested) requested)
            deferred <- audit (Client.commitOracleSubmissionDeferral pending)
            unchanged <- shown (Client.releaseCompletedOracleDeferrals (const False) deferred)
            ensure (unchanged == deferred) "an incomplete decision released the retained request"
            shown (Client.releaseCompletedOracleDeferrals (== decision) deferred) >>= audit
          else pure requested
      witness <- maybe (Left "request witness disappeared") Right (Client.lookupOracleRequest reference released)
      (nextOracle, advanced, nextProjection, entry) <- submitAndProject (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) oracle released projection
      result <- shown (Client.prepareOracleResult reference entry advanced)
      let (settled, _, _) = Client.commitOracleResult result
      _ <- audit settled
      retry <- shown (Client.prepareOracleResult reference entry settled)
      let (retried, classification, _) = Client.commitOracleResult retry
      ensure (retried == settled && classification == Client.OracleResultExactRetry) "receipt retry changed history"
      ensure (all (\(oldRef, oldWitness) -> Client.lookupOracleRequest oldRef settled == Just oldWitness) (Client.oracleClientRequestEntries client)) "a new settlement changed an old witness"
      _ <- audit retried
      pure (nextOracle, retried, nextProjection)

caseCompoundOutcome :: Assertion
caseCompoundOutcome =
  assertEqual
    "completion is classified at the canonical entry index"
    (Right (OracleRequestLabelCompleted decision entryIndex))
    (interpretLabelCompletionEventView decision digest entryIndex (LabelWorkflowCompletedView decision digest))

caseCompoundOutcomeMismatch :: Assertion
caseCompoundOutcomeMismatch =
  assertEqual
    "decision and immutable outcome remain exact"
    (replicate 3 (Left OracleRequestResultMismatch))
    [ classify (LabelWorkflowCompletedView otherDecision digest),
      classify (LabelWorkflowCompletedView decision otherDigest),
      classify (ProcessEpochEndedEventView unknownProcess entryIndex ExplicitAdministrativeEnd)
    ]
  where
    classify = interpretLabelCompletionEventView decision digest entryIndex

decision :: LabelDecisionId
decision = checked (mkLabelDecisionId (fixtureIdentifierBytes 0xb1))

otherDecision :: LabelDecisionId
otherDecision = checked (mkLabelDecisionId (fixtureIdentifierBytes 0xb2))

digest :: LabelOutcomeDigest
digest = checked (mkLabelOutcomeDigest (fixtureIdentifierBytes 0xb5))

otherDigest :: LabelOutcomeDigest
otherDigest = checked (mkLabelOutcomeDigest (fixtureIdentifierBytes 0xb6))

entryIndex :: ControlIndex
entryIndex = controlIndex 79

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

-- The real Oracle supplies every entry here.  The generated reporter order
-- exercises projection, client alias settlement, and terminal suppression
-- independently of the composed Herald workflow scheduler.
propDisappearanceOwnership :: Bool -> Bool -> QC.Property
propDisappearanceOwnership regular resolved =
  QC.forAll (QC.shuffle capturedMembers) $ \reporters ->
    case runDisappearanceOwnership regular resolved reporters of
      Left problem -> QC.counterexample problem False
      Right () -> QC.property True

fixtureRemoteHerald :: HeraldEpoch
fixtureRemoteHerald = case filter (/= localEpoch) capturedMembers of
  remote : _ -> remote
  [] -> error "checked membership fixture has no remote Herald"

runDisappearanceOwnership :: Bool -> Bool -> [HeraldEpoch] -> Either String ()
runDisappearanceOwnership regular resolved reporters = do
  oracle0 <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
  let projection0 = Fixtures.fixtureOracleProjectionState
      subject = if regular then regularSubject else controlledSubject
      coordinate = Disappearance.disappearanceSubjectMembershipCoordinate subject capturedMembership
  preparedOpen <- shown (Client.prepareOpenDisappearanceProbeRequest subject coordinate initialClient)
  let (client0, _, reference) = Client.commitOracleRequest preparedOpen
      remote = fixtureRemoteHerald
  (oracle1, client1, projection1, _) <-
    submitAndProject
      (externalEnvelope remote 1 (Command.openDisappearanceProbeCommand subject coordinate))
      oracle0
      client0
      projection0
  witness <- maybe (Left "missing retained Open") Right (Client.lookupOracleRequest reference client1)
  (oracle2, client2, projection2, aliasEntry) <-
    submitAndProject
      (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness))
      oracle1
      client1
      projection1
  result <- shown (Client.prepareOracleResult reference aliasEntry client2)
  let (client3, _, classified) = Client.commitOracleResult result
      probe = checked (Disappearance.deriveDisappearanceProbeId (controlIndex 1))
  ensure
    (classified == Client.OracleRequestDisappearanceOpened (Disappearance.aliasedDisappearanceProbe probe))
    "racing Open did not settle against the original probe"
  retried <- shown (Client.prepareOpenDisappearanceProbeRequest subject coordinate client3)
  ensure
    ( Client.preparedOracleRequestRef retried == reference
        && Client.preparedOracleRequestClassification retried == Client.OracleRequestExactRetry
    )
    "the exact Open intention was replaced after alias projection"
  let claim member =
        checked
          ( Disappearance.admitDisappearanceEvidenceClaim
              subject
              capturedMembership
              probe
              member
              (Disappearance.deriveDisappearanceEvidenceDigest (Identity.heraldEpochBytes member))
          )
      claims = fmap claim capturedMembers
  pending <- shown (Client.preparePredefinedAbsenceReportRequest (claim localEpoch) client3)
  let (client4, _, reportRef) = Client.commitOracleRequest pending
  (oracle3, client5, projection3) <-
    foldM
      ( \(oracle, client, projection) member -> do
          (nextOracle, nextClient, nextProjection, _) <-
            submitAndProject
              (externalEnvelope member 20 (Command.reportPredefinedAbsenceCommand probe (claim member)))
              oracle
              client
              projection
          pure (nextOracle, nextClient, nextProjection)
      )
      (oracle2, client4, projection2)
      reporters
  projected <-
    maybe
      (Left "missing projected report history")
      Right
      (Projection.oracleViewDisappearanceProbe probe (Projection.oracleView projection3))
  ensure
    (Projection.projectedDisappearanceReports projected == zip capturedMembers claims)
    "projection lost a complete checked report or canonical reporter order"
  preparedTerminal <-
    shown
      ( if resolved
          then Client.prepareResolveDisappearanceProbeRequest probe (OracleDisappearance.completeDisappearanceEvidenceDigest claims) client5
          else Client.prepareAbortDisappearanceProbeRequest probe OracleDisappearance.authorizedDisappearanceAbortReason client5
      )
  let (clientWithTerminal, _, terminalRef) = Client.commitOracleRequest preparedTerminal
  (oracle4, client6, projection4, terminalEntry) <-
    submitAndProject
      ( externalEnvelope
          remote
          30
          ( if resolved
              then
                Command.resolveDisappearanceProbeCommand
                  probe
                  (OracleDisappearance.completeDisappearanceEvidenceDigest claims)
              else Command.abortDisappearanceProbeCommand probe OracleDisappearance.authorizedDisappearanceAbortReason
          )
      )
      oracle3
      clientWithTerminal
      projection3
  let terminalIndex = OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue terminalEntry)
      rejectsSuppression suppliedProbe suppliedIndex = case Client.suppressDisappearanceProbeRequests suppliedProbe subject coordinate suppliedIndex client6 of
        Left Client.OracleClientStateContradiction -> True
        _ -> False
  ensure (rejectsSuppression probe (controlIndex 2)) "non-terminal history authorized suppression"
  ensure (rejectsSuppression probe (controlIndex 9999)) "missing history authorized suppression"
  ensure (rejectsSuppression (checked (Disappearance.deriveDisappearanceProbeId (controlIndex 999))) terminalIndex) "another probe's terminal authorized suppression"
  client7 <- shown (Client.suppressDisappearanceProbeRequests probe subject coordinate terminalIndex client6)
  ensure
    (all ((/= reportRef) . Client.oracleRequestDispatchRef) (Client.oracleClientRequestDispatches client7))
    "terminal cleanup left the exact outstanding Report dispatchable"
  ensure
    (Client.lookupOracleRequest reportRef client7 == Client.lookupOracleRequest reportRef client6)
    "terminal cleanup erased the immutable request history"
  _ <- shown (Projection.validateState projection4)
  _ <- shown (Client.validateState client7)
  before <- shown (Projection.topologyControlCheckpointAt (controlIndex 2) projection2)
  historical <- shown (Projection.topologyControlCheckpointAt (controlIndex 2) projection4)
  terminal <- shown (Projection.topologyControlCheckpointAt terminalIndex projection4)
  ensure
    (historical == before && (terminal /= historical) == resolved)
    "terminal history did not preserve the exact lifecycle checkpoint"
  terminalWitness <- maybe (Left "missing racing terminal request") Right (Client.lookupOracleRequest terminalRef client7)
  let terminalEnvelope = Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope terminalWitness)
  -- The same canonical alias cannot settle before its local terminal authority
  -- has been recorded. No fabricated client state or Oracle result is used.
  (_, withoutProof, _, unprovenAlias) <- submitAndProject terminalEnvelope oracle4 client6 projection4
  ensure
    ( case Client.prepareOracleResult terminalRef unprovenAlias withoutProof of
        Left Client.OracleRequestResultMismatch -> True
        _ -> False
    )
    "empty-event terminal alias settled without its retained canonical terminal proof"
  (oracle5, aliasClient, projection5, terminalAlias) <- submitAndProject terminalEnvelope oracle4 client7 projection4
  ensure
    (null (OracleProjection.appliedEntryProjectionEvents (Canonical.canonicalAppliedOracleEntryValue terminalAlias)))
    "a racing terminal emitted a second logical terminal event"
  aliasResult <- shown (Client.prepareOracleResult terminalRef terminalAlias aliasClient)
  let (settledAlias, _, aliasOutcome) = Client.commitOracleResult aliasResult
      matchesTerminal = case (aliasOutcome, Projection.projectedDisappearanceTerminal =<< Projection.oracleViewDisappearanceProbe probe (Projection.oracleView projection4)) of
        (Client.OracleRequestDisappearanceResolved observed outcome, Just (Projection.ProjectedDisappearanceResolved retained _)) -> observed == probe && outcome == retained
        (Client.OracleRequestDisappearanceAborted observed, Just Projection.ProjectedDisappearanceAborted {}) -> observed == probe
        _ -> False
  ensure matchesTerminal "terminal alias did not bind the exact retained probe and outcome"
  ensure
    ( case Client.prepareOracleResult reportRef terminalAlias aliasClient of
        Left Client.OracleRequestIdMismatch -> True
        _ -> False
    )
    "a mismatched report intention accepted a terminal alias receipt"
  aliasRetry <- shown (Client.prepareOracleResult terminalRef terminalAlias settledAlias)
  ensure
    (Client.preparedOracleResultClassification aliasRetry == Client.OracleResultExactRetry)
    "terminal alias exact receipt replay changed its logical settlement"
  if resolved
    then pure ()
    else do
      rearmed <- shown (Client.prepareOpenDisappearanceProbeRequest subject coordinate settledAlias)
      let (client8, _, freshReference) = Client.commitOracleRequest rearmed
      ensure (freshReference /= reference) "a terminal Open key prevented same-coordinate rearming"
      replayedTerminal <- shown (Client.suppressDisappearanceProbeRequests probe subject coordinate terminalIndex client8)
      ensure (replayedTerminal == client8) "old terminal replay suppressed the fresh Open cycle"
      freshWitness <- maybe (Left "fresh Open witness missing") Right (Client.lookupOracleRequest freshReference client8)
      (_, client9, _, freshEntry) <-
        submitAndProject
          (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope freshWitness))
          oracle5
          client8
          projection5
      freshResult <- shown (Client.prepareOracleResult freshReference freshEntry client9)
      let (_, _, opened) = Client.commitOracleResult freshResult
          expectedProbe =
            checked
              ( Disappearance.deriveDisappearanceProbeId
                  (OracleProjection.appliedEntryControlIndex (Canonical.canonicalAppliedOracleEntryValue freshEntry))
              )
      ensure
        (opened == Client.OracleRequestDisappearanceOpened (Disappearance.openedDisappearanceProbe expectedProbe))
        "same-coordinate rearm did not create a fresh live Oracle probe"

-- A committed Open rejection is stable on unchanged scheduler ticks. Once
-- a later control fact arrives, the client permits a newly eligible candidate
-- to allocate a new request while retaining the rejected envelope forever.
caseRejectedOpenRearm :: Assertion
caseRejectedOpenRearm = case run of
  Left problem -> error problem
  Right () -> pure ()
  where
    run = do
      oracle <- shown (Oracle.initialOracle Fixtures.fixtureCheckedOracleGenesis)
      let coordinate = Disappearance.disappearanceSubjectMembershipCoordinate regularSubject capturedMembership
      prepared <- shown (Client.prepareOpenDisappearanceProbeRequest controlledSubject coordinate initialClient)
      let (client, _, reference) = Client.commitOracleRequest prepared
      witness <- maybe (Left "missing rejected Open witness") Right (Client.lookupOracleRequest reference client)
      (oracle1, advanced, projection1, rejected) <-
        submitAndProject
          (Canonical.canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness))
          oracle
          client
          Fixtures.fixtureOracleProjectionState
      let remote = fixtureRemoteHerald
          validCoordinate = Disappearance.disappearanceSubjectMembershipCoordinate regularSubject capturedMembership
          laterEnvelope = externalEnvelope remote 900 (Command.openDisappearanceProbeCommand regularSubject validCoordinate)
      (_, delayed, _, _) <- submitAndProject laterEnvelope oracle1 advanced projection1
      ensure
        (case Client.prepareOracleResult reference rejected delayed of Left Client.OracleClientStateContradiction -> True; _ -> False)
        "a delayed rejected Open result admitted an obsolete semantic key"
      shown (Client.validateState delayed)
      result <- shown (Client.prepareOracleResult reference rejected advanced)
      let (settled, _, disposition) = Client.commitOracleResult result
      ensure
        (case disposition of Client.OracleRequestRejected {} -> True; _ -> False)
        "coordinate mismatch did not commit an Open rejection"
      repeated <- shown (Client.prepareOpenDisappearanceProbeRequest controlledSubject coordinate settled)
      ensure
        (Client.preparedOracleRequestRef repeated == reference)
        "unchanged rejected Open spawned a retry request"
      maintenance <- case [ack | PublicClient.SubmitOracleProgress _ ack <- retirementFlushActions settled] of
        [ack] -> pure (Canonical.canonicalOracleEnvelopeValue ack)
        _ -> Left "projected rejection did not offer receipt retirement"
      (afterMaintenance, maintained, maintainedProjection, _) <- submitAndProject maintenance oracle1 settled projection1
      afterAckRetry <- shown (Client.prepareOpenDisappearanceProbeRequest controlledSubject coordinate maintained)
      ensure (Client.preparedOracleRequestRef afterAckRetry == reference) "receipt maintenance rearmed a rejected Open"
      (_, later, _, _) <-
        submitAndProject
          laterEnvelope
          afterMaintenance
          maintained
          maintainedProjection
      fresh <- shown (Client.prepareOpenDisappearanceProbeRequest controlledSubject coordinate later)
      let (rearmed, _, freshReference) = Client.commitOracleRequest fresh
      ensure (freshReference /= reference) "new contiguous projection could not rearm a rejected Open"
      ensure
        (Client.lookupOracleRequest reference rearmed == Client.lookupOracleRequest reference settled)
        "rearming erased the original immutable rejected request"
      shown (Client.validateState rearmed)

submitAndProject ::
  Command.OracleEnvelope ->
  Oracle.OracleState ->
  Client.State ->
  Projection.State ->
  Either String (Oracle.OracleState, Client.State, Projection.State, Canonical.CanonicalAppliedOracleEntry)
submitAndProject envelope oracle client projection = do
  _ <- shown (Client.validateState client)
  (nextOracle, _, effects) <- shown (Oracle.stepOracle envelope oracle)
  entry <- case Effect.oracleEffects effects of
    [Effect.EmitAppliedOracleEntry supplied] -> pure (Canonical.canonicalizeAppliedOracleEntry supplied)
    other -> Left ("expected one live Oracle entry, got " <> show other)
  preparedClient <- shown (Client.prepareCursorAdvance entry client)
  preparedProjection <- shown (Projection.prepareAppliedEntry entry projection)
  compacted <- shown (Client.compactOracleClientEvidence (Client.commitCursorAdvance preparedClient))
  _ <- shown (Client.validateState compacted)
  pure (nextOracle, compacted, Projection.commitAppliedEntry preparedProjection, entry)

externalEnvelope :: HeraldEpoch -> Word -> Command.OracleCommand -> Command.OracleEnvelope
externalEnvelope home sequenceNumber =
  Command.oracleEnvelope (OracleIdentity.oracleClientRequestId home (fromIntegral sequenceNumber)) Nothing home

initialClient :: Client.State
initialClient = Client.commitClientIngress (checked (Client.prepareClientIngress acceptance initial))
  where
    genesis = Fixtures.fixtureStep14CheckedGenesis
    initial =
      Client.initialState
        ( PublicClient.oracleHelloClaims
            (Genesis.checkedSystemId genesis)
            (Genesis.checkedCatalogueDigest genesis)
            (Genesis.checkedConfigurationDigest genesis)
            (Genesis.checkedInitialProjectionDigest Fixtures.fixtureCheckedInitialBootstraps)
            (Genesis.checkedLocalHeraldId genesis)
            localEpoch
            (Membership.heraldMembershipGenerationId capturedMembership)
        )
        Fixtures.fixtureOracleContacts
        (controlIndex 0)
    acceptance = case Client.oracleClientActions initial of
      [PublicClient.ConnectAndHelloOracle attempt _] ->
        PublicClient.OracleHelloReceived
          attempt
          ( PublicClient.oracleHelloAcceptance
              (PublicClient.oracleContactNode (PublicClient.oracleConnectAttemptContact attempt))
              (PublicClient.oracleObservedTerm 1)
              (controlIndex 0)
              Nothing
              True
          )
      _ -> error "initial Oracle client connection invariant"

capturedMembership :: Membership.HeraldMembershipGeneration
capturedMembership =
  Projection.oracleViewCurrentHeraldMembership
    (Projection.oracleView Fixtures.fixtureOracleProjectionState)

capturedMembers :: [HeraldEpoch]
capturedMembers = NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs capturedMembership)

localEpoch :: HeraldEpoch
localEpoch = Genesis.checkedLocalHeraldEpoch Fixtures.fixtureStep14CheckedGenesis

controlledSubject, regularSubject :: Disappearance.DisappearanceSubject
controlledSubject =
  checked
    ( Disappearance.controlledPredefinedDisappearanceSubject
        Profile.NeutralVertexRole
        (checked (Identity.mkGlobalObjectId (fixtureIdentifierBytes 0xef)))
        (Identity.structuralOccurrenceId localEpoch Identity.firstStructuralSequence)
        Nothing
    )
regularSubject =
  Disappearance.regularSortDefinitionDisappearanceSubject
    ( Disappearance.deriveRegularSortOccurrenceClaim
        (Genesis.checkedSystemId Fixtures.fixtureStep14CheckedGenesis)
        (Profile.predefinedCatalogueDescriptor (Profile.profileEntryFor Profile.NeutralVertexRole))
        Genesis
    )

shown :: (Show problem) => Either problem value -> Either String value
shown = either (Left . show) Right

ensure :: Bool -> String -> Either String ()
ensure condition problem = if condition then Right () else Left problem
