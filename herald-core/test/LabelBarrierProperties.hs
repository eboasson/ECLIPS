module LabelBarrierProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (HeraldEpoch, LabelDecisionId, controlIndex, mkHeraldEpoch, mkLabelDecisionId)
import Eclips.Domain.Label (LabelOutcomeDigest, labelInstallationReport, mkLabelOutcomeDigest)
import Eclips.Domain.Membership (mkHeraldMembershipGenerationId)
import Eclips.Domain.Sort.Profile (profileCatalogueDigest)
import Eclips.Domain.Topology (deriveMemberSetDigest)
import Eclips.Herald.Label.Collection qualified as Collection
import Eclips.Herald.LabelBarrier.State qualified as Barrier
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (chooseInt, counterexample, forAll, testProperty)

tests :: TestTree
tests =
  testGroup
    "historical label installation owner"
    [ testCase "installation is retained until exact canonical completion" $ do
        let installed = install (evidence 1 7) initial
            completed = Barrier.commitWorkflowCompletion (checked (Barrier.prepareWorkflowCompletion (decision 1) installed))
        assertEqual "installed decision" (Set.singleton (decision 1)) (Barrier.barrierActiveDecisions installed)
        assertEqual "historical evidence" (Just (evidence 1 7)) (Barrier.barrierTerminalEvidence (decision 1) installed)
        assertEqual "completion removes evidence" Nothing (Barrier.barrierTerminalEvidence (decision 1) completed)
        assertEqual "completion removes reports" Nothing (Collection.currentReport (decision 1) (Barrier.barrierCollectionState completed))
        assertEqual "valid completed state" (Right ()) (Barrier.validateBarrierState completed),
      testCase "exact installation replay is idempotent and conflicting evidence is rejected" $ do
        let installed = install (evidence 1 7) initial
        assertEqual "exact replay" installed (install (evidence 1 7) installed)
        let concurrent = install (evidence 2 8) installed
        assertEqual "independent decisions retain both installations" (Set.fromList [decision 1, decision 2]) (Barrier.barrierActiveDecisions concurrent)
        assertEqual "other installation preserves the first evidence" (Just (evidence 1 7)) (Barrier.barrierTerminalEvidence (decision 1) concurrent)
        assertBool "same decision cannot change index" (isLeft (Barrier.prepareTerminalApplication (evidence 1 8) installed))
        assertBool "unrelated completion cannot retire evidence" (isLeft (Barrier.prepareWorkflowCompletion (decision 2) installed))
        assertBool "zero is not a decision entry" (isLeft (Barrier.prepareTerminalApplication (evidence 1 0) initial)),
      testCase "collection report must agree with local installation evidence" $ do
        let installed = install (evidence 1 7) initial
            exact = withReport 1 7 installed
            wrongDecision = withReport 2 7 installed
            wrongIndex = withReport 1 8 installed
            completed = Barrier.commitWorkflowCompletion (checked (Barrier.prepareWorkflowCompletion (decision 1) exact))
        assertEqual "matching report validates" (Right ()) (Barrier.validateBarrierState exact)
        assertBool "wrong decision fails invariant" (isLeft (Barrier.validateBarrierState wrongDecision))
        assertBool "wrong index fails invariant" (isLeft (Barrier.validateBarrierState wrongIndex))
        assertEqual "completion retires collector and evidence together" initial completed,
      testProperty "concurrent completion reclaims only its own installation"
        $ forAll (chooseInt (1, 100))
        $ \count ->
          let installed = foldl (\state n -> withReport (fromIntegral n) (fromIntegral n) (install (evidence (fromIntegral n) (fromIntegral n)) state)) initial [1 .. count]
              complete state n = Barrier.commitWorkflowCompletion (checked (Barrier.prepareWorkflowCompletion (decision (fromIntegral n)) state))
              states = scanl complete installed [count, count - 1 .. 1]
              expected = [Set.fromList [decision (fromIntegral n) | n <- [1 .. remaining]] | remaining <- [count, count - 1 .. 0]]
           in counterexample (show states) (map Barrier.barrierActiveDecisions states == expected && all ((== Right ()) . Barrier.validateBarrierState) states && last states == initial),
      testProperty "successive completed installations do not retain prior terminal evidence"
        $ forAll (chooseInt (1, 100))
        $ \count ->
          let advance state n = Barrier.commitWorkflowCompletion (checked (Barrier.prepareWorkflowCompletion (decision (fromIntegral n)) (withReport (fromIntegral n) (fromIntegral n) (install (evidence (fromIntegral n) (fromIntegral n)) state))))
              completed = foldl advance initial [1 .. count]
           in counterexample (show completed) (completed == initial && Barrier.validateBarrierState completed == Right ())
    ]

initial :: Barrier.State
initial = Barrier.initialState home

home :: HeraldEpoch
home = checked (mkHeraldEpoch (bytes 0x31))

decision :: Word8 -> LabelDecisionId
decision = checked . mkLabelDecisionId . bytes

evidence :: Word8 -> Word64 -> Barrier.TerminalOutcomeEvidence
evidence byte index = Barrier.terminalOutcomeEvidence (decision byte) (controlIndex index) digest profileCatalogueDigest (deriveMemberSetDigest (home :| []))

digest :: LabelOutcomeDigest
digest = checked (mkLabelOutcomeDigest (bytes 0x61))

install :: Barrier.TerminalOutcomeEvidence -> Barrier.State -> Barrier.State
install fact state = Barrier.commitTerminalApplication (checked (Barrier.prepareTerminalApplication fact state))

withReport :: Word8 -> Word64 -> Barrier.State -> Barrier.State
withReport byte index state = Barrier.replaceBarrierCollectionState collected state
  where
    report = labelInstallationReport (decision byte) home (controlIndex index) digest
    members = Set.singleton home
    generation = checked (mkHeraldMembershipGenerationId (bytes 0x71))
    collected = checked (Collection.begin report home members generation members (Barrier.barrierCollectionState state))

bytes :: Word8 -> ByteString.ByteString
bytes = ByteString.replicate 32

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
