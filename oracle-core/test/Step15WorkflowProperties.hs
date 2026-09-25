{-# LANGUAGE OverloadedStrings #-}

module Step15WorkflowProperties
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Identity
  ( HeraldEpoch,
    LabelDecisionId,
    SystemId,
    controlIndex,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkSystemId,
  )
import Eclips.Domain.Label (LabelOutcomeDigest)
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    FailureProbeResolutionId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Step15.WorkflowReference
  ( ReferenceWorkflowCompletion,
    ReferenceWorkflowPhaseView (..),
    ReferenceWorkflowProblem (..),
    ReferenceWorkflowReportResult (..),
    ReferenceWorkflowReportSubmission (..),
    ReferenceWorkflowState,
    ReferenceWorkflowTerminalView (..),
    applyReferenceWorkflowRetirement,
    decideReferenceWorkflow,
    decideReferenceWorkflowAt,
    initialReferenceWorkflowState,
    referenceWorkflowApplicableGeneration,
    referenceWorkflowApplicableReporters,
    referenceWorkflowCapturedGeneration,
    referenceWorkflowCapturedMemberSetDigest,
    referenceWorkflowCapturedMembers,
    referenceWorkflowCollector,
    referenceWorkflowCompletion,
    referenceWorkflowCurrentMembership,
    referenceWorkflowPhase,
    referenceWorkflowStateCanonicalBytes,
    referenceWorkflowTerminal,
    submitReferenceWorkflowCompletion,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Step-15 detached membership-sensitive label workflow"
    [ testCase "the atomic decision captures one authoritative generation" caseAuthoritativeOpen,
      testCase "NotApplied completes immediately and retirement preserves it" caseAlreadyNotApplied,
      testCase "released value and terminal survive while reporter applicability contracts" caseReleasedRetirement,
      testCase "one assigned collector completes and exact retry is inert" caseSurvivorReportOrder,
      testCase "collector retirement deterministically reassigns without changing Released" caseCollectorRetirement,
      testCase "two contractions preserve terminal evidence and restrict the captured reporter set" caseRepeatedRetirement
    ]

caseAuthoritativeOpen :: Assertion
caseAuthoritativeOpen = do
  let initial = checked "initial workflow state" (initialReferenceWorkflowState genesis)
  assertEqual
    "a retained successor cannot masquerade as the genesis seed"
    (Left (ReferenceWorkflowInitialGenerationNotGenesis successorId))
    (initialReferenceWorkflowState successor)
  assertEqual
    "a successor identity is stale before retirement"
    (Left (ReferenceWorkflowStaleMembershipGeneration successorId genesisId))
    (decideReferenceWorkflow decision successorId (controlIndex 8) (Right "value") initial)
  let opened = checked "authoritative Open" (decideReferenceWorkflow decision genesisId (controlIndex 8) (Right "value") initial)
  assertEqual "captured generation" (Just genesisId) (referenceWorkflowCapturedGeneration opened)
  assertEqual "captured exact members" members (referenceWorkflowCapturedMembers opened)
  assertEqual
    "captured exact member digest"
    (Just (heraldMembershipGenerationActiveMemberSetDigest genesis))
    (referenceWorkflowCapturedMemberSetDigest opened)
  assertEqual "initial completion generation" (Just genesisId) (referenceWorkflowApplicableGeneration opened)

caseAlreadyNotApplied :: Assertion
caseAlreadyNotApplied = do
  let notApplied =
        checked
          "ordinary NotApplied"
          (decideReferenceWorkflow decision genesisId (controlIndex 7) (Left "prior changed") initialState)
      terminal = referenceWorkflowTerminal notApplied
      retired = checked "retire existing NotApplied" (applyReferenceWorkflowRetirement successor notApplied)
  assertEqual "existing terminal identity is immutable" terminal (referenceWorkflowTerminal retired)
  assertEqual "only reporter applicability changes" survivors (referenceWorkflowApplicableReporters retired)
  assertEqual "workflow remains terminal, not rewritten" (Just ReferenceWorkflowCompletedNotAppliedView) (referenceWorkflowPhase retired)

caseReleasedRetirement :: Assertion
caseReleasedRetirement = do
  let released = releasedState
      terminal = referenceWorkflowTerminal released
      digest = terminalDigest released
      retired = checked "retire released workflow" (applyReferenceWorkflowRetirement successor released)
      stale = report h1 40 genesisId digest
      (rejectedState, rejected) = submitReferenceWorkflowCompletion stale retired
      (retriedState, retry) = submitReferenceWorkflowCompletion stale rejectedState
      (completed, accepted) = submitReferenceWorkflowCompletion (report h1 41 successorId digest) retriedState
      (_, obsoleteDiscovery) = submitReferenceWorkflowCompletion (report h2 42 genesisId digest) completed
  assertEqual "released terminal is immutable" terminal (referenceWorkflowTerminal retired)
  assertEqual "released workflow now awaits only survivors" survivors (referenceWorkflowApplicableReporters retired)
  assertEqual
    "same collector uses the new membership generation"
    (ReferenceWorkflowReportCommitted (ReferenceWorkflowReportRejected (ReferenceWorkflowStaleMembershipGeneration genesisId successorId)))
    rejected
  assertEqual
    "exact rejected retry retains its result"
    (ReferenceWorkflowReportDuplicate (ReferenceWorkflowReportRejected (ReferenceWorkflowStaleMembershipGeneration genesisId successorId)))
    retry
  assertEqual "exact rejected retry leaves state unchanged" rejectedState retriedState
  assertEqual "refreshed completion is accepted" (ReferenceWorkflowReportCommitted ReferenceWorkflowReportAccepted) accepted
  assertEqual "another survivor rediscovers completion with its old generation" (ReferenceWorkflowReportCommitted ReferenceWorkflowReportAccepted) obsoleteDiscovery
  assertEqual "one collector attestation completes Released" (Just ReferenceWorkflowCompletedReleasedView) (referenceWorkflowPhase completed)

caseSurvivorReportOrder :: Assertion
caseSurvivorReportOrder = do
  let retired = checked "retire released workflow" (applyReferenceWorkflowRetirement successor releasedState)
      digest = terminalDigest retired
      (unchangedWorkflow, rejected) = submitReferenceWorkflowCompletion (report h2 51 successorId digest) retired
      completion = report h1 52 successorId digest
      (completed, accepted) = submitReferenceWorkflowCompletion completion unchangedWorkflow
      (duplicateState, duplicate) = submitReferenceWorkflowCompletion completion completed
  assertEqual
    "another surviving participant cannot claim the collector role"
    (ReferenceWorkflowReportCommitted (ReferenceWorkflowReportRejected (ReferenceWorkflowCollectorMismatch h1 h2)))
    rejected
  assertEqual "wrong collector does not complete" (Just ReferenceWorkflowReleasedView) (referenceWorkflowPhase unchangedWorkflow)
  assertEqual "home collector completes once" (ReferenceWorkflowReportCommitted ReferenceWorkflowReportAccepted) accepted
  assertEqual "exact completion is replayed" (ReferenceWorkflowReportDuplicate ReferenceWorkflowReportAccepted) duplicate
  assertEqual "exact completion replay changes no canonical state" (referenceWorkflowStateCanonicalBytes completed) (referenceWorkflowStateCanonicalBytes duplicateState)
  assertEqual "successor membership remains authoritative" successor (referenceWorkflowCurrentMembership completed)

caseRepeatedRetirement :: Assertion
caseRepeatedRetirement = forM_ [notAppliedState, releasedState] $ \initial -> do
  let first = checked "first contraction" (applyReferenceWorkflowRetirement successor initial)
      terminal = referenceWorkflowTerminal first
      second = checked "second contraction" (applyReferenceWorkflowRetirement secondSuccessor first)
      secondId = heraldMembershipGenerationId secondSuccessor
      replayed = checked "old exact retirement replay" (applyReferenceWorkflowRetirement successor second)
      (completed, accepted) = submitReferenceWorkflowCompletion (report h1 90 secondId (terminalDigest second)) second
  assertEqual "the first terminal outcome remains byte-identical" terminal (referenceWorkflowTerminal second)
  assertEqual "only the current captured survivors are applicable" [h1, h2] (referenceWorkflowApplicableReporters second)
  assertEqual "the surviving home remains collector" (Just h1) (referenceWorkflowCollector second)
  assertEqual "exact old-generation replay cannot roll current authority back" second replayed
  assertEqual "one refreshed completion suffices" (ReferenceWorkflowReportCommitted ReferenceWorkflowReportAccepted) accepted
  assertEqual "completion preserves the frozen terminal" terminal (referenceWorkflowTerminal completed)
  assertEqual "completion requires no retired reporter" (Just (if initial == releasedState then ReferenceWorkflowCompletedReleasedView else ReferenceWorkflowCompletedNotAppliedView)) (referenceWorkflowPhase completed)

caseCollectorRetirement :: Assertion
caseCollectorRetirement = do
  let initial = checked "initial" (initialReferenceWorkflowState genesis)
      released = checked "non-minimum home" (decideReferenceWorkflowAt h4 decision genesisId (controlIndex 8) (Right "released") initial)
      retired = checked "collector retirement" (applyReferenceWorkflowRetirement successor released)
      (completed, result) = submitReferenceWorkflowCompletion (report h1 91 successorId (terminalDigest retired)) retired
  assertEqual "active home is collector before retirement" (Just h4) (referenceWorkflowCollector released)
  assertEqual "retirement deterministically chooses minimum survivor" (Just h1) (referenceWorkflowCollector retired)
  assertEqual "collector retirement never rewrites Released" (referenceWorkflowTerminal released) (referenceWorkflowTerminal retired)
  assertEqual "successor can complete independently of the retired home" (ReferenceWorkflowReportCommitted ReferenceWorkflowReportAccepted) result
  assertEqual "successor completes" (Just ReferenceWorkflowCompletedReleasedView) (referenceWorkflowPhase completed)

initialState :: ReferenceWorkflowState
initialState = checked "initial workflow state" (initialReferenceWorkflowState genesis)

notAppliedState :: ReferenceWorkflowState
notAppliedState = checked "NotApplied" (decideReferenceWorkflow decision genesisId (controlIndex 7) (Left "prior changed") initialState)

releasedState :: ReferenceWorkflowState
releasedState = checked "Applied" (decideReferenceWorkflow decision genesisId (controlIndex 8) (Right "released-value") initialState)

report ::
  HeraldEpoch ->
  Word ->
  HeraldMembershipGenerationId ->
  LabelOutcomeDigest ->
  ReferenceWorkflowCompletion
report reporter sequenceNumber generation digest =
  referenceWorkflowCompletion
    (oracleClientRequestId reporter (fromIntegral sequenceNumber))
    reporter
    decision
    generation
    digest

terminalDigest :: ReferenceWorkflowState -> LabelOutcomeDigest
terminalDigest state = case referenceWorkflowTerminal state of
  Just (ReferenceWorkflowOtherNotAppliedView _ _ digest) -> digest
  Just (ReferenceWorkflowReleasedValueView _ _ digest) -> digest
  Nothing -> error "workflow has no terminal digest"

genesis :: HeraldMembershipGeneration
genesis = checked "genesis membership" (genesisHeraldMembershipGeneration system (h1 :| [h2, h3, h4]))

successor :: HeraldMembershipGeneration
successor = checked "successor membership" (retireHeraldMembershipGeneration (controlIndex 10) (retirementResolution 9) h4 genesis)

secondSuccessor :: HeraldMembershipGeneration
secondSuccessor = checked "second successor membership" (retireHeraldMembershipGeneration (controlIndex 12) (retirementResolution 11) h3 successor)

retirementResolution :: Word -> FailureProbeResolutionId
retirementResolution index = deriveFailureProbeResolutionId (checked "retirement probe" (deriveHeraldFailureProbeId (controlIndex (fromIntegral index)))) RetireFailureProbeTarget

genesisId, successorId :: HeraldMembershipGenerationId
genesisId = heraldMembershipGenerationId genesis
successorId = heraldMembershipGenerationId successor

members, survivors :: [HeraldEpoch]
members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs genesis)
survivors = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)

system :: SystemId
system = checked "system" (mkSystemId (ByteString.replicate 32 0x91))

decision :: LabelDecisionId
decision = checked "decision" (mkLabelDecisionId (ByteString.replicate 32 0x92))

h1, h2, h3, h4 :: HeraldEpoch
h1 = herald 0x11
h2 = herald 0x22
h3 = herald 0x33
h4 = herald 0x44

herald :: Word -> HeraldEpoch
herald byte = checked "Herald epoch" (mkHeraldEpoch (ByteString.replicate 32 (fromIntegral byte)))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
