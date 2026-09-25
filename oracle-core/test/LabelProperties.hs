{-# LANGUAGE OverloadedStrings #-}

module LabelProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity (GlobalObjectId, LabelDecisionId, controlIndex, genesisAuthorityEpoch, nablaSequence, publicationId)
import Eclips.Domain.Label (HomeLabelAcceptanceCut, InitialLabelEvidence, firstLabelProcessAcceptancePosition, homeLabelAcceptanceCut, initialPublicationLabelEvidence, mkLabelOutcomeDigest, targetVoid)
import Eclips.Domain.Membership (FailureProbeResolution (RetireFailureProbeTarget), FailureProbeResolutionId, HeraldFailureProbeId, HeraldMembershipGenerationId, deriveFailureProbeResolutionId, deriveHeraldFailureProbeId, genesisHeraldMembershipGeneration, heraldMembershipGenerationId)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ApplicationPermanentlyLost, ExplicitAdministrativeEnd, HeraldRetired))
import Eclips.Domain.Value (LabelOwner (VoidLabel))
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestId)
import Eclips.Oracle.Label
import Eclips.Oracle.Voter qualified as V
import OracleFixtures (checked, fixtureCheckedGenesis, fixtureHeraldEpoch, fixtureProcessEpoch, fixtureRemoteHeraldEpoch, fixtureStart, fixtureSystemId, fixtureThirdHeraldEpoch, globalObjectId, nablaId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Positive (..), Property, counterexample, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "atomic Oracle label vocabulary"
    [ testCase "process, atomic label, completion and failure envelopes decode exactly" caseTypedCommandRoundTrips,
      testCase "the two Herald-submitted End reasons survive decoding" caseLiveEndReasonRoundTrips,
      testCase "Herald retirement remains Oracle-derived" caseHeraldRetiredReasonRejected,
      testCase "completion digest is an untrusted claim until Oracle admission" caseUntrustedCompletionClaim,
      testCase "wrong completion digest is retained as a committed rejection" caseCompletionDigestCommittedRejection,
      testProperty "typed Start envelope decoding is stable across request sequences" propStartEnvelopeRoundTrip
    ]

caseTypedCommandRoundTrips :: Assertion
caseTypedCommandRoundTrips = do
  assertEqual "closed command tags" ([0, 1, 2, 8, 9, 10, 11, 12, 20]) (fmap oracleCommandTag commands)
  mapM_ (\(number, command) -> assertRoundTrip (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch number) Nothing fixtureHeraldEpoch command)) (zip [100 ..] commands)

assertRoundTrip :: OracleEnvelope -> Assertion
assertRoundTrip envelope = case decodeOracleEnvelopeCanonicalBytes (oracleEnvelopeCanonicalBytes envelope) of
  Left problem -> assertFailure ("envelope decode failed: " <> show problem)
  Right decoded -> do
    assertEqual "typed envelope" envelope (decodedOracleEnvelopeValue decoded)
    assertEqual "canonical bytes" (oracleEnvelopeCanonicalBytes envelope) (decodedOracleEnvelopeCanonicalBytes decoded)

caseUntrustedCompletionClaim :: Assertion
caseUntrustedCompletionClaim = assertRoundTrip (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 201) Nothing fixtureHeraldEpoch (completeLabelDecisionCommand wrongCompletion))

caseCompletionDigestCommittedRejection :: Assertion
caseCompletionDigestCommittedRejection = do
  let envelope = oracleEnvelope requestId Nothing fixtureHeraldEpoch decisionCommand
      (decided, _) = submitOracleState envelope (initialOracleState fixtureCheckedGenesis)
      expectedDigest = maybe (error "decision lacks outcome") deriveLabelOutcomeDigest (oracleTerminalOutcome decision decided)
      (successor, outcome) = submitOracleState (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 200) Nothing fixtureHeraldEpoch (completeLabelDecisionCommand wrongCompletion)) decided
  assertEqual "semantic rejection consumes one control index" (controlIndex 2) (oracleGreatestControlIndex successor)
  case oracleSubmissionOutcomeView outcome of
    OracleSubmissionCommittedView receipt entry -> do
      assertEqual "the wrong digest is rejected" (OracleRejected (LabelCompletionDigestMismatch expectedDigest (labelCompletionOutcomeDigest wrongCompletion))) (oracleReceiptResult receipt)
      assertEqual "rejection emits no projection events" [] (appliedEntryProjectionEvents entry)
    other -> assertFailure ("completion was not committed: " <> show other)

caseLiveEndReasonRoundTrips :: Assertion
caseLiveEndReasonRoundTrips =
  mapM_ assertReasonRoundTrip [ExplicitAdministrativeEnd, ApplicationPermanentlyLost]
  where
    assertReasonRoundTrip reason = do
      let command =
            checked
              "live End command"
              (endProcessEpochCommand fixtureProcessEpoch reason)
          envelope =
            oracleEnvelope
              (oracleClientRequestId fixtureHeraldEpoch (102 + fromIntegral (fromEnum reason)))
              (Just (controlIndex 0))
              fixtureHeraldEpoch
              command
          bytes = oracleEnvelopeCanonicalBytes envelope
      case decodeOracleEnvelopeCanonicalBytes bytes of
        Left problem ->
          assertFailure
            ("End reason failed EORC decoding: " <> show reason <> ": " <> show problem)
        Right decoded -> do
          assertEqual "typed End envelope" envelope (decodedOracleEnvelopeValue decoded)
          assertEqual "canonical End bytes" bytes (decodedOracleEnvelopeCanonicalBytes decoded)

caseHeraldRetiredReasonRejected :: Assertion
caseHeraldRetiredReasonRejected = do
  assertEqual
    "typed End construction rejects the Oracle-derived reason"
    (Left (EndReasonNotHeraldSubmittable HeraldRetired))
    (endProcessEpochCommand fixtureProcessEpoch HeraldRetired)
  let admittedCommand =
        checked
          "live End command"
          ( endProcessEpochCommand
              fixtureProcessEpoch
              ApplicationPermanentlyLost
          )
      envelope =
        oracleEnvelope
          (oracleClientRequestId fixtureHeraldEpoch 104)
          (Just (controlIndex 0))
          fixtureHeraldEpoch
          admittedCommand
      stagedReasonBytes =
        ByteString.init (oracleEnvelopeCanonicalBytes envelope)
          <> ByteString.singleton 2
  case decodeOracleEnvelopeCanonicalBytes stagedReasonBytes of
    Left _ -> pure ()
    Right decoded ->
      assertFailure
        ( "live EORC decoded the Oracle-derived-only Herald-retirement reason: "
            <> show (decodedOracleEnvelopeValue decoded)
        )

propStartEnvelopeRoundTrip :: Positive Word64 -> Property
propStartEnvelopeRoundTrip (Positive sequenceNumber) =
  let envelope =
        oracleEnvelope
          (oracleClientRequestId fixtureHeraldEpoch sequenceNumber)
          (Just (controlIndex 0))
          fixtureHeraldEpoch
          (startProcessEpochCommand fixtureStart)
      bytes = oracleEnvelopeCanonicalBytes envelope
   in case decodeOracleEnvelopeCanonicalBytes bytes of
        Left problem -> counterexample (show problem) False
        Right decoded -> decodedOracleEnvelopeValue decoded === envelope

initialEvidence :: Maybe InitialLabelEvidence
initialEvidence = initialPublicationLabelEvidence object (VoidLabel, 0) (publicationId (nablaId 181) genesisAuthorityEpoch fixtureHeraldEpoch (nablaSequence 1))

decisionCommand :: OracleCommand
decisionCommand = decideLabelCommand decision fixtureProcessEpoch object (VoidLabel, 0) initialEvidence Nothing Nothing targetVoid acceptanceCut

commands :: [OracleCommand]
commands =
  [ startProcessEpochCommand fixtureStart,
    checked "live End command" (endProcessEpochCommand fixtureProcessEpoch ExplicitAdministrativeEnd),
    decisionCommand,
    completeLabelDecisionCommand wrongCompletion,
    openHeraldFailureProbeCommand fixtureRemoteHeraldEpoch failureGeneration (V.voterConfigurationId (V.oracleVoterConfiguration (initialOracleState fixtureCheckedGenesis))),
    reportHeraldFailureProbeCommand failureProbe (V.voterConfigurationId (V.oracleVoterConfiguration (initialOracleState fixtureCheckedGenesis))) ProbeUnreachable,
    dismissHeraldFailureProbeCommand failureResolution,
    retireHeraldEpochCommand failureResolution fixtureRemoteHeraldEpoch,
    acceptVoterHostFailureCommand failureResolution
  ]

wrongCompletion :: LabelCompletionAttestation
wrongCompletion = labelCompletionAttestation decision (controlIndex 1) (checked "wrong digest" (mkLabelOutcomeDigest (ByteString.replicate 32 0xdd))) fixtureHeraldEpoch failureGeneration

failureGeneration :: HeraldMembershipGenerationId
failureGeneration =
  heraldMembershipGenerationId
    ( checked
        "failure generation"
        ( genesisHeraldMembershipGeneration
            fixtureSystemId
            (fixtureHeraldEpoch :| [fixtureRemoteHeraldEpoch, fixtureThirdHeraldEpoch])
        )
    )

failureProbe :: HeraldFailureProbeId
failureProbe = checked "failure probe" (deriveHeraldFailureProbeId (controlIndex 7))

failureResolution :: FailureProbeResolutionId
failureResolution =
  deriveFailureProbeResolutionId failureProbe RetireFailureProbeTarget

requestId :: OracleClientRequestId
requestId = oracleClientRequestId fixtureHeraldEpoch 99

decision :: LabelDecisionId
decision = deriveLabelDecisionId fixtureSystemId requestId

object :: GlobalObjectId
object = globalObjectId 211

acceptanceCut :: HomeLabelAcceptanceCut
acceptanceCut =
  homeLabelAcceptanceCut
    (firstLabelProcessAcceptancePosition fixtureProcessEpoch)
    EmptyHeraldPublicationPrefix
