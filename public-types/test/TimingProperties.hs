module TimingProperties (tests) where

import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import Data.Word (Word64)
import Eclips.Public.Types.Timing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, choose, conjoin, counterexample, forAll, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "takeover timing policy"
    [ testCase "five-second defaults leave 1.5 seconds for committed decisions" caseDefaultBudget,
      testProperty "representable targets preserve detection, election and retry ordering" propRelationships,
      testProperty "fixed waits use at most seventy percent of the target" propDecisionBudget,
      testProperty "whole-millisecond targets scale every derived duration proportionally" propScaling,
      testProperty "checked targets round trip through the current binary schema" propBinaryRoundTrip,
      testProperty "sub-resolution targets fail both construction and binary admission" propMalformed
    ]

caseDefaultBudget :: IO ()
caseDefaultBudget = do
  let policy = deriveTimingPolicy defaultTakeoverTarget
  takeoverTargetMicroseconds defaultTakeoverTarget @?= 5_000_000
  policyDurations policy
    @?= [ 250_000,
          500_000,
          2_000_000,
          500_000,
          25_000,
          250_000,
          500_000,
          250_000,
          100_000,
          3_000_000,
          2_000_000,
          10_000,
          100_000,
          250_000
        ]
  fixedDecisionWait policy @?= 3_500_000
  defaultApplicationReplyTimeoutMicroseconds @?= 10_000_000
  defaultReadOnlyDrainMicroseconds @?= 500_000

propRelationships :: Property
propRelationships = forAll (choose (500, 60_000_000)) $ \microseconds ->
  let policy = deriveTimingPolicy (checked (takeoverTarget microseconds))
   in conjoin
        [ counterexample "every duration is positive" (all (> 0) (policyDurations policy)),
          counterexample "ten heartbeats fit before election" (10 * timingRaftHeartbeatMicroseconds policy <= timingRaftElectionLowerMicroseconds policy),
          counterexample "election is randomized over a nonempty range" (timingRaftElectionLowerMicroseconds policy < timingRaftElectionUpperMicroseconds policy),
          counterexample "health query finishes before the following round" (timingHealthQueryMicroseconds policy < timingHealthRoundMicroseconds policy),
          counterexample "initial reconnect delay is within its cap" (timingReconnectInitialMicroseconds policy <= timingRetryCapMicroseconds policy),
          counterexample "initial submission delay is within its cap" (timingSubmissionInitialMicroseconds policy <= timingRetryCapMicroseconds policy),
          counterexample "several elections fit inside isolation grace" (3 * timingRaftElectionUpperMicroseconds policy <= timingIsolationGraceMicroseconds policy),
          timingApplicationRecoveryGraceMicroseconds policy === timingPeerRecoveryGraceMicroseconds policy
        ]

propDecisionBudget :: Property
propDecisionBudget = forAll (choose (500, 60_000_000)) $ \microseconds ->
  let wait = fixedDecisionWait (deriveTimingPolicy (checked (takeoverTarget microseconds)))
   in counterexample
        "fixed waits leave at least thirty percent for consensus and processing"
        (10 * wait <= 7 * microseconds)

propScaling :: Property
propScaling = forAll (choose (1, 60_000)) $ \milliseconds ->
  forAll (choose (1, 20)) $ \multiple ->
    let base = deriveTimingPolicy (checked (takeoverTarget (milliseconds * 1000)))
        scaled = deriveTimingPolicy (checked (takeoverTarget (multiple * milliseconds * 1000)))
     in policyDurations scaled === map (* multiple) (policyDurations base)

propBinaryRoundTrip :: Property
propBinaryRoundTrip = forAll (choose (500, 60_000_000)) $ \microseconds ->
  let target = checked (takeoverTarget microseconds)
   in decodeBinary (encode target) === Right target

propMalformed :: Property
propMalformed = forAll (choose (0, 499 :: Word64)) $ \microseconds ->
  conjoin
    [ takeoverTarget microseconds === Left TakeoverTargetBelowMicrosecondResolution,
      counterexample
        "Binary admission rejects an unrepresentable target"
        (isLeft (decodeBinary (encode microseconds) :: Either String TakeoverTarget))
    ]

fixedDecisionWait :: TimingPolicy -> Word64
fixedDecisionWait policy =
  2 * timingHeartbeatIdleMicroseconds policy
    + timingHeartbeatReplyMicroseconds policy
    + timingPeerRecoveryGraceMicroseconds policy
    + timingFailureProbeMicroseconds policy

policyDurations :: TimingPolicy -> [Word64]
policyDurations policy =
  map
    ($ policy)
    [ timingHeartbeatIdleMicroseconds,
      timingHeartbeatReplyMicroseconds,
      timingPeerRecoveryGraceMicroseconds,
      timingFailureProbeMicroseconds,
      timingRaftHeartbeatMicroseconds,
      timingRaftElectionLowerMicroseconds,
      timingRaftElectionUpperMicroseconds,
      timingHealthRoundMicroseconds,
      timingHealthQueryMicroseconds,
      timingIsolationGraceMicroseconds,
      timingApplicationRecoveryGraceMicroseconds,
      timingReconnectInitialMicroseconds,
      timingSubmissionInitialMicroseconds,
      timingRetryCapMicroseconds
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

decodeBinary :: (Binary value) => Lazy.ByteString -> Either String value
decodeBinary bytes = case decodeOrFail bytes of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, value)
    | Lazy.null remaining -> Right value
    | otherwise -> Left "unexpected trailing bytes"
