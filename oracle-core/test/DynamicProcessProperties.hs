module DynamicProcessProperties (tests) where

import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart
  ( processStart,
    processStartProcessEpochId,
    processStartProcessId,
    processStartResidence,
  )
import Eclips.Oracle.Canonical
  ( canonicalOracleEnvelopeBytes,
    canonicalizeOracleEnvelope,
    decodeCanonicalOracleEnvelope,
  )
import Eclips.Oracle.Command
  ( endProcessEpochCommand,
    oracleEnvelope,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect (OracleStepOutcome (..), oracleEffects)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
  ( ProcessLifecycleView (..),
    ProcessOriginView (..),
    processRecordLifecycle,
    processRecordOrigin,
  )
import Eclips.Oracle.Receipt (OracleReceiptResult (..), OracleRejection (ProcessAlreadyStarted, ProcessEpochAlreadyStarted), oracleReceiptResult)
import Eclips.Oracle.State
  ( oracleLiveProcessResidence,
    oracleProcessRecord,
    oracleStateGenesis,
  )
import Eclips.Oracle.Transition (stepOracle)
import OracleFixtures
  ( checked,
    fixtureCheckedGenesis,
    fixtureHeraldEpoch,
    fixtureInitialState,
    fixtureRemoteHeraldEpoch,
    processEpochId,
    processId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Property,
    arbitrary,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    property,
    testProperty,
    vectorOf,
  )

tests :: TestTree
tests =
  testGroup
    "dynamic process identity"
    [ testProperty "fresh starts at generated local and remote residences need no genesis manifest; retry and End preserve identity" propDynamicStarts
    ]

propDynamicStarts :: Property
propDynamicStarts =
  forAll (chooseInt (2, 25)) $ \count ->
    forAll (vectorOf count arbitrary) $ \remoteChoices ->
      let starts =
            [ processStart
                (processId seed)
                (processEpochId (seed + 40))
                (if remote then fixtureRemoteHeraldEpoch else fixtureHeraldEpoch)
            | (seed, remote) <- zip [180 ..] (False : True : remoteChoices)
            ]
          (_, checks) = foldl' applyStart (fixtureInitialState, []) (zip [1 ..] starts)
       in counterexample "minimal Start history did not preserve exact admission, request retry, or terminal End" (conjoin checks)
  where
    applyStart (state, checks) (number, start) =
      let residence = processStartResidence start
          epoch = processStartProcessEpochId start
          request = oracleClientRequestId residence number
          envelope = oracleEnvelope request Nothing residence (startProcessEpochCommand start)
          (started, outcome, _) = checked "generated Start" (stepOracle envelope state)
          (retried, retryOutcome, retryEffects) = checked "generated retry" (stepOracle envelope started)
          endRequest = oracleClientRequestId residence (1000 + number)
          end =
            oracleEnvelope
              endRequest
              Nothing
              residence
              (checked "generated End command" (endProcessEpochCommand epoch ExplicitAdministrativeEnd))
          (ended, endOutcome, _) = checked "generated End" (stepOracle end retried)
          (afterRetry, finalRetry, finalEffects) = checked "Start retry after End" (stepOracle envelope ended)
          repeated command =
            let supplied = oracleEnvelope (oracleClientRequestId residence (2000 + number)) Nothing residence command
                (_, result, _) = checked "fresh reuse after End" (stepOracle supplied ended)
             in case result of
                  OracleCommitted receipt -> Just (oracleReceiptResult receipt)
                  _ -> Nothing
          reusedProcess = processStart (processStartProcessId start) (processEpochId (fromIntegral number + 50)) residence
          startIndex = controlIndex (2 * number - 1)
          endIndex = controlIndex (2 * number)
          accepted = case outcome of
            OracleCommitted receipt ->
              oracleReceiptResult receipt == OracleAccepted
                && retryOutcome == OracleDuplicate receipt
                && finalRetry == OracleDuplicate receipt
            _ -> False
          endAccepted = case endOutcome of
            OracleCommitted receipt -> oracleReceiptResult receipt == OracleAccepted
            _ -> False
          facts =
            [ property accepted,
              property endAccepted,
              property (repeated (startProcessEpochCommand start) == Just (OracleRejected (ProcessEpochAlreadyStarted epoch))),
              property (repeated (startProcessEpochCommand reusedProcess) == Just (OracleRejected (ProcessAlreadyStarted (processStartProcessId start)))),
              property (oracleLiveProcessResidence epoch started == Just residence),
              property (fmap processRecordOrigin (oracleProcessRecord epoch started) == Just (DynamicStartView startIndex request)),
              property (retried == started && null (oracleEffects retryEffects)),
              property (afterRetry == ended && null (oracleEffects finalEffects)),
              property (oracleLiveProcessResidence epoch ended == Nothing),
              property (fmap processRecordLifecycle (oracleProcessRecord epoch ended) == Just (ProcessRecordEndedView endIndex ExplicitAdministrativeEnd)),
              property (oracleStateGenesis ended == fixtureCheckedGenesis),
              property (decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope envelope)) == Right (canonicalizeOracleEnvelope envelope))
            ]
       in (ended, checks <> facts)
