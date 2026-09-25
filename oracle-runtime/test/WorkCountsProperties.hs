module WorkCountsProperties (tests) where

import Data.List (nub, sort)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Oracle.Command (oracleEnvelope, retireOracleReceiptsCommand)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Runtime.Internal.WorkCounts
  ( countOracleWork,
    emptyOracleWorkCounts,
    oracleCommandWorkTag,
    oracleOutcomeWorkTag,
    oracleWorkRows,
    oracleWorkSnapshotComplete,
  )
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import TestFixtures (checked, fixtureCheckedGenesis, fixtureHeraldEpoch, validOracleCommand)

tests :: TestTree
tests =
  testGroup
    "Oracle work counts"
    [ testProperty "coalesced counters reconcile with an independent event census" propCensus,
      testCase "fresh rejection, exact retry and receipt maintenance remain distinct" caseOutcomeTags,
      testCase "interrupted publication cannot be reported as an installed complete snapshot" caseSnapshotBoundary
    ]

propCensus :: [(Bool, Bool)] -> Property
propCensus observations = oracleWorkRows counts === expected
  where
    events = [(if family then "apply" else "preflight", if result then "accepted" else "rejected") | (family, result) <- observations]
    counts = foldl' (\previous (family, result) -> countOracleWork family result previous) emptyOracleWorkCounts events
    expected = [(family, result, fromIntegral (length (filter (== (family, result)) events))) | (family, result) <- sort (nub events)]

caseSnapshotBoundary :: IO ()
caseSnapshotBoundary = do
  let interrupted = countOracleWork "owner_exit" "exception" emptyOracleWorkCounts
      returned = countOracleWork "owner_exit" "returned" emptyOracleWorkCounts
  assertEqual "an uninstalled successor is incomplete" False (oracleWorkSnapshotComplete (controlIndex 2) (controlIndex 1) interrupted)
  assertEqual "an installed snapshot survives cancellation" True (oracleWorkSnapshotComplete (controlIndex 2) (controlIndex 2) interrupted)
  assertEqual "an ordinarily returned installed snapshot is complete" True (oracleWorkSnapshotComplete (controlIndex 2) (controlIndex 2) returned)
  assertEqual "without terminal evidence even equal prefixes are not complete" False (oracleWorkSnapshotComplete (controlIndex 2) (controlIndex 2) emptyOracleWorkCounts)

caseOutcomeTags :: IO ()
caseOutcomeTags = do
  let initial = checked "initial" (initialOracle fixtureCheckedGenesis)
      envelope sequenceNumber command = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch sequenceNumber) Nothing fixtureHeraldEpoch command
      first = envelope 1 validOracleCommand
      next = envelope 2 validOracleCommand
      maintenance = envelope 2 (retireOracleReceiptsCommand 2)
      (started, accepted, _) = checked "start" (stepOracle first initial)
      (_, duplicate, _) = checked "duplicate" (stepOracle first started)
      (rejected, rejection, _) = checked "fresh rejection" (stepOracle next started)
      (_, repeatedRejection, _) = checked "duplicate rejection" (stepOracle next rejected)
      (retired, retirement, _) = checked "retirement" (stepOracle maintenance rejected)
      (_, oldRequest, _) = checked "retired request" (stepOracle first retired)
  assertEqual "command constructor" "StartProcessEpoch" (oracleCommandWorkTag validOracleCommand)
  assertEqual
    "outcome categories contain no request identity or rejection payload"
    [ "fresh_accepted",
      "duplicate_accepted",
      "fresh_rejected_ProcessEpochAlreadyStarted",
      "duplicate_rejected_ProcessEpochAlreadyStarted",
      "progress_retired",
      "request_retired_OracleRequestPrefixRetired"
    ]
    (fmap oracleOutcomeWorkTag [accepted, duplicate, rejection, repeatedRejection, retirement, oldRequest])
