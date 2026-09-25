{-# LANGUAGE OverloadedStrings #-}

module TimingProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Deployment.Configuration
import Eclips.Deployment.Discovery
import Eclips.Deployment.Manifest
import Eclips.Deployment.Timing
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Membership (genesisHeraldMembershipGeneration, heraldMembershipGenerationId)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Genesis (checkedOracleRaftNativeConfiguration, checkedOracleRaftVoterBindings, checkedOracleSystemId, raftVoterBindingNode)
import Eclips.Public.Types.Timing qualified as Timing
import Eclips.Raft.Genesis (raftNativeElectionTimeoutLower, raftNativeElectionTimeoutUpper, raftNativeHeartbeatInterval)
import Eclips.Raft.Identity (raftDurationMicrosWord64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "takeover timing"
    [ testProperty "manifest and discovered bootstrap preserve selected target and native policy" propPolicy,
      testCase "operator durations are exact and default policy is inspectable" caseOperator
    ]

propPolicy :: Positive Int -> Property
propPolicy (Positive generated) =
  let target = checked (takeoverTarget (500 + fromIntegral (generated `mod` 10000000)))
      timing = Timing.deriveTimingPolicy target
      planes = checked (deploymentEndpoints "127.0.0.1" 43000 Nothing)
      plan = checked (checkDeploymentWithTiming target (Bytes.pack [0 .. 31]) (planes :| []))
      decoded = checked (decodeDeployment (encodeDeployment plan))
      oracle = deploymentCheckedOracleGenesis decoded
      native = checkedOracleRaftNativeConfiguration oracle
      member = residentMember (headResident decoded)
      generation = checked (genesisHeraldMembershipGeneration (checkedOracleSystemId oracle) (heraldMemberEpoch member :| []))
      node = case checkedOracleRaftVoterBindings oracle of
        [binding] -> raftVoterBindingNode binding
        _ -> error "timing test needs one founder"
      contact = checked (discoveryOracleContact node (oracleListen planes))
      snapshot = discoverySnapshot decoded (heraldMembershipGenerationId generation) (controlIndex 0) [] (contact :| [])
      response = checked (decodeDiscoveryResponse (encodeDiscoveryResponse (SystemDiscovered 1 snapshot)))
   in conjoin
        [ deploymentTakeoverTarget decoded === target,
          raftDurationMicrosWord64 (raftNativeHeartbeatInterval native) === Timing.timingRaftHeartbeatMicroseconds timing,
          raftDurationMicrosWord64 (raftNativeElectionTimeoutLower native) === Timing.timingRaftElectionLowerMicroseconds timing,
          raftDurationMicrosWord64 (raftNativeElectionTimeoutUpper native) === Timing.timingRaftElectionUpperMicroseconds timing,
          case response of
            SystemDiscovered _ discovered -> deploymentTakeoverTarget (discoveryBootstrap discovered) === target
            _ -> counterexample "discovery changed response family" False
        ]
  where
    headResident plan = case deploymentResidents plan of first :| _ -> first

caseOperator :: Assertion
caseOperator = do
  assertEqual "default duration spelling" (Right defaultTakeoverTarget) (parseTakeoverTarget "5s")
  assertEqual "millisecond spelling" (parseTakeoverTarget "5s") (parseTakeoverTarget "5000ms")
  assertEqual "fractional seconds are exact" (Right 2500000) (takeoverTargetMicroseconds <$> parseTakeoverTarget "2.5s")
  mapM_ (\input -> assertBool (show input) (case parseTakeoverTarget input of Left _ -> True; Right _ -> False)) ["0s", "5", "-1s", "NaNs", "0.0000001s", "0.000499s"]
  assertBool "resolved probe is visible" ("timing failure-probe 500000us" `elem` takeoverTimingLines defaultTakeoverTarget)
  assertBool "application responses have an independent allowance" ("timing application-reply 10000000us" `elem` takeoverTimingLines defaultTakeoverTarget)
  assertBool "cleanup duration is separate" ("timing read-only-drain 500000us" `elem` takeoverTimingLines defaultTakeoverTarget)

checked :: (Show failure) => Either failure value -> value
checked = either (error . show) id
