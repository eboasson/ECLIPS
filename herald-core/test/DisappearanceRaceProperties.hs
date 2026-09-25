{-# LANGUAGE OverloadedRecordDot #-}

module DisappearanceRaceProperties (tests) where

import ApplicationLabelProperties (commitCompleteLabelWorkflow, commitNextLabelRequest, locallyTakenNeutralDisappearanceFixtureWithRequests)
import Control.Monad (foldM, replicateM_)
import Data.List.NonEmpty (NonEmpty (..))
import DisappearanceLiveProperties (prepareLiveResolve, step, submitEnvelope)
import Eclips.Domain.Disappearance qualified as Domain
import Eclips.Domain.Identity (GlobalObjectId)
import Eclips.Herald.Application.Request.Internal (requestId)
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.EffectBatch (effectBatchIsEmpty)
import Eclips.Herald.Input (ApplicationRequestIngress (CallApplicationRequest), HeraldInputBody (..), heraldInput)
import Eclips.Herald.OracleClient qualified as Actions
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue, canonicalOracleEnvelopeValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command (OracleEnvelope)
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Label (LiveNotAppliedReason (LiveTransitionNoLongerPermitted), LiveTerminalOutcomeView (LiveNotAppliedOutcomeView), liveTerminalOutcomeView)
import Eclips.Oracle.Progress (oracleProgressLabelsThrough)
import Eclips.Oracle.Projection (AppliedOracleEntry, OracleProjectionEventView (LabelDecidedView), appliedEntryCommand, appliedEntryControlIndex, appliedEntryProjectionEvents, appliedEntryReceipt, oracleProjectionEventView)
import Eclips.Oracle.Receipt (OracleReceiptResult (..), OracleRejection (..), oracleReceiptResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import VerifiedHeraldTransition (verifiedStepHerald)

-- These seeds select the committed winner and the number of duplicate ingress
-- deliveries. They pin regression schedules without changing randomized suites.
tests :: TestTree
tests =
  testGroup
    "Step16 deterministic race corpus"
    [ testCase "seed-160100-relabel-before-resolve-completion-rearm" (caseRelabel 160100),
      testCase "seed-160101-resolve-before-relabel-deleted" (caseRelabel 160101),
      testCase "seed-160102-publication-before-resolve" (casePublication 160102),
      testCase "seed-160103-resolve-before-publication" (casePublication 160103)
    ]

caseRelabel :: Int -> Assertion
caseRelabel seedNumber = do
  (seed, object, _, relabel, _) <- locallyTakenNeutralDisappearanceFixtureWithRequests
  subject <- candidateSubject object seed
  (oracle, ready, binding) <- prepareLiveResolve seed subject
  resolve <- envelopeFor isResolve ready
  (accepted, _) <- step "accept ordinary relabel beside retained disappearance Resolve" (ApplicationRequestInput relabel) ready
  active <- required "accepted local label owner" (Application.applicationActiveLabel (startupApplicationState accepted))
  let decision = Application.activeLabelApplicationDecision active
  labelDecision <- envelopeFor isLabelDecision accepted
  repeated <- repeatInput seedNumber (ApplicationRequestInput relabel) accepted
  assertBool "duplicate relabel cannot allocate another label or Oracle request" (repeated == accepted)
  if odd seedNumber
    then do
      (resolvedOracle, resolved, terminal) <- submitEnvelope "Resolve commits before retained label Open" binding oracle accepted resolve
      assertBool "Resolve installs terminal controlled deletion" (Controlled.controlledObjectTerminallyDeleted object (startupControlledState resolved))
      (_, rejected, late) <- submitEnvelope "late relabel observes deleted authority" binding resolvedOracle resolved labelDecision
      assertBool
        "late relabel records an immutable NotApplied decision after deletion"
        ( case map oracleProjectionEventView (appliedEntryProjectionEvents late) of
            [LabelDecidedView _ outcome _] -> case liveTerminalOutcomeView outcome of
              LiveNotAppliedOutcomeView _ _ _ LiveTransitionNoLongerPermitted _ _ -> True
              _ -> False
            _ -> False
        )
      assertBool "rejected relabel cannot resurrect the controlled object" (Controlled.controlledObjectTerminallyDeleted object (startupControlledState rejected))
      replayTerminal binding terminal rejected
    else do
      (openedOracle, opened, decidedEntry) <- submitEnvelope "label decision atomically invalidates disappearance" binding oracle accepted labelDecision
      witness <- required "invalidated original probe" (firstProbe opened)
      assertBool
        "label decision and exact-probe invalidation are one canonical entry"
        (case witness.witnessPhase of Disappearance.ProbeInvalidatedView (Protocol.ProjectedLabelInvalidation actual target) _ -> actual == decision && target == object; _ -> False)
      assertBool "label decision preserves the controlled object" (not (Controlled.controlledObjectTerminallyDeleted object (startupControlledState opened)))
      assertEqual "candidate waits for that exact label terminal" [Just decision] [candidate.candidateAwaitingLabelDecision | candidate <- Disappearance.candidateWitnesses (startupDisappearanceState opened)]
      (lateOracle, afterLate, late) <- submitEnvelope "old disappearance Resolve loses the committed race" binding openedOracle opened resolve
      lateCommand <- required "late Resolve entry must contain a command receipt" (appliedEntryCommand late)
      assertBool
        "late Resolve is rejected against the terminal probe"
        (case oracleReceiptResult (appliedEntryReceipt lateCommand) of OracleRejected (DisappearanceCommandRejected OracleDisappearance.DisappearanceProbeAlreadyTerminal {}) -> True; _ -> False)
      (completedOracle, terminal, _) <- commitCompleteLabelWorkflow 200 lateOracle afterLate
      assertBool "completed label keeps controlled authority live" (not (Controlled.controlledObjectTerminallyDeleted object (startupControlledState terminal)))
      assertBool "completion consumes the label wait" (all ((== Nothing) . (.candidateAwaitingLabelDecision)) (Disappearance.candidateWitnesses (startupDisappearanceState terminal)))
      _ <- envelopeFor isOpen terminal
      replayTerminal binding decidedEntry terminal
      -- The first label and disappearance invalidation share one entry. A later
      -- genuine NotApplied decision advances the label retirement frontier;
      -- the other semantic event must keep that older compound entry pinned.
      let retryAsNew = case relabel of
            CallApplicationRequest applicationBinding session _ operation -> CallApplicationRequest applicationBinding session (requestId 5) operation
            _ -> error "relabel race fixture needs an ordinary application call"
      (nextAccepted, _) <- requiredEither (verifiedStepHerald (heraldInput (startupLastObservedTime terminal) (ApplicationRequestInput retryAsNew)) terminal)
      (_, advanced, _) <- commitNextLabelRequest 210 completedOracle nextAccepted
      let latestIndex = Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState advanced))
      plainCanonical <- required "the later canonical stale CAS decision" (Projection.appliedEntryEvidence latestIndex (startupOracleProjectionState advanced))
      let plainDecision = canonicalAppliedOracleEntryValue plainCanonical
      assertBool
        "the later request produces only an ordinary NotApplied decision"
        ( case map oracleProjectionEventView (appliedEntryProjectionEvents plainDecision) of
            [LabelDecidedView _ outcome _] -> case liveTerminalOutcomeView outcome of
              LiveNotAppliedOutcomeView {} -> True
              _ -> False
            _ -> False
        )
      let compoundIndex = appliedEntryControlIndex decidedEntry
          compoundCanonical = canonicalizeAppliedOracleEntry decidedEntry
      assertBool "the compound entry is strictly older than label retirement readiness" (compoundIndex < oracleProgressLabelsThrough (Client.oracleClientProgressReady (startupOracleClientState advanced)))
      assertEqual "the disappearance event retains its exact shared client entry" (Just compoundCanonical) (Client.oracleClientRetainedEntry compoundIndex (startupOracleClientState advanced))
      assertEqual "Projection retains the same exact compound entry" (Just compoundCanonical) (Projection.appliedEntryEvidence compoundIndex (startupOracleProjectionState advanced))
      replayTerminal binding decidedEntry advanced
  where
    isLabelDecision Client.DecideLabelIntent {} = True
    isLabelDecision _ = False

casePublication :: Int -> Assertion
casePublication seedNumber = do
  (seed, object, _, _, publication) <- locallyTakenNeutralDisappearanceFixtureWithRequests
  subject <- candidateSubject object seed
  (oracle, ready, binding) <- prepareLiveResolve seed subject
  resolve <- envelopeFor isResolve ready
  (held, _) <- step "retain a matching controlled publication after the cut" (ApplicationRequestInput publication) ready
  assertBool "matching controlled publication cannot mutate Store before the terminal" (startupStoreState held == startupStoreState ready)
  invalidate <- envelopeFor isInvalidate held
  repeated <- repeatInput seedNumber (ApplicationRequestInput publication) held
  assertBool "equal publication retries retain the same whole request and intention" (held == repeated)
  (_, terminal, entry) <- submitEnvelope "serialize controlled publication terminal winner" binding oracle held (if odd seedNumber then resolve else invalidate)
  assertEqual "terminal consumes exact gated application work" [] (Application.applicationFenceHeldEntries (startupApplicationState terminal))
  assertEqual "only Resolve may install controlled terminal deletion" (odd seedNumber) (Controlled.controlledObjectTerminallyDeleted object (startupControlledState terminal))
  assertEqual "controlled publication race preserves whole-state invariants" (Right ()) (validateHeraldState terminal)
  replayTerminal binding entry terminal

candidateSubject :: GlobalObjectId -> HeraldState -> IO Domain.DisappearanceSubject
candidateSubject object state = case [candidate.candidateSubject | candidate <- Disappearance.candidateWitnesses (startupDisappearanceState state), Domain.ControlledPredefinedSubjectView target _ _ <- [Domain.disappearanceSubjectView candidate.candidateSubject], target == object] of
  [subject] -> pure subject
  _ -> assertFailure "expected exact LocalTake candidate"

firstProbe :: HeraldState -> Maybe Disappearance.ProbeWitness
firstProbe state = case Disappearance.probeWitnesses (startupDisappearanceState state) of
  witness : _ -> Just witness
  [] -> Nothing

repeatInput :: Int -> HeraldInputBody -> HeraldState -> IO HeraldState
repeatInput seedNumber input state = foldM (\current _ -> fst <$> step "seeded exact duplicate ingress" input current) state [1 .. 1 + seedNumber `mod` 3]

replayTerminal :: Actions.OracleBinding -> AppliedOracleEntry -> HeraldState -> Assertion
replayTerminal binding entry state = do
  (replayed, _) <- requiredEither (verifiedStepHerald (heraldInput (startupLastObservedTime state) (OracleInput (Actions.OracleEntriesReceived binding (canonicalizeAppliedOracleEntry entry :| [])))) state)
  assertBool "exact terminal watch replay preserves the owner product" (replayed == state)
  replicateM_ 3 $ do
    (unchanged, effects) <- requiredEither (Live.advanceDisappearanceWork state)
    assertBool "unchanged scheduling retains all request identities" (unchanged == state)
    assertBool "unchanged scheduling emits no work" (effectBatchIsEmpty effects)

envelopeFor :: (Client.OracleSemanticIntent -> Bool) -> HeraldState -> IO OracleEnvelope
envelopeFor predicate state = case [canonicalOracleEnvelopeValue (Client.oracleRequestDispatchEnvelope dispatch) | Actions.SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions client, Just witness <- [Client.lookupOracleRequest (Client.oracleRequestDispatchRef dispatch) client], predicate (Client.oracleRequestWitnessIntent witness)] of
  [envelope] -> pure envelope
  found -> assertFailure ("expected one retained matching request, observed " <> show (length found))
  where
    client = startupOracleClientState state

isResolve, isInvalidate, isOpen :: Client.OracleSemanticIntent -> Bool
isResolve Client.ResolveDisappearanceProbeIntent {} = True
isResolve _ = False
isInvalidate Client.InvalidateDisappearanceProbeIntent {} = True
isInvalidate _ = False
isOpen Client.OpenDisappearanceProbeIntent {} = True
isOpen _ = False

required :: String -> Maybe value -> IO value
required description = maybe (assertFailure description) pure

requiredEither :: (Show problem) => Either problem value -> IO value
requiredEither = either (assertFailure . show) pure
