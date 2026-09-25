{-# LANGUAGE OverloadedStrings #-}

module CanonicalProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
    DeltaId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (..),
    ProcessEpochId,
    ProcessId,
    StoreIncarnationId,
    controlIndex,
  )
import Eclips.Domain.ProcessStart (processStart, processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Domain.Startup
  ( ConfiguredProcessBootstrap,
    ConfiguredRootBootstrap,
    configuredProcessBootstrapManifestId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    configuredProcessBootstrapRoots,
    configuredRootBootstrap,
    configuredRootBootstrapCatalogueRole,
    configuredRootBootstrapReaderDelta,
    configuredRootBootstrapStoreIncarnation,
    configuredRootBootstrapWriterNabla,
    configuredRootBootstrapWriterSequencing,
    mkConfiguredProcessBootstrap,
  )
import Eclips.Oracle.Canonical
  ( CanonicalOracleEnvelope,
    OracleCanonicalError (MalformedCanonicalOracleBytes),
    canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeBytes,
    canonicalOracleEnvelopeDigest,
    canonicalOracleEnvelopeValue,
    canonicalOracleProjectionEventVectorBytes,
    canonicalOracleReceiptBytes,
    canonicalOracleReceiptValue,
    canonicalizeAppliedOracleEntry,
    canonicalizeOracleEnvelope,
    canonicalizeOracleProjectionEventVector,
    canonicalizeOracleReceipt,
    configuredProcessBootstrapTranscriptBytes,
    decodeCanonicalAppliedOracleEntry,
    decodeCanonicalOracleEnvelope,
    decodeCanonicalOracleProjectionEventVector,
    decodeCanonicalOracleReceipt,
    decodeConfiguredProcessBootstrapTranscriptBytes,
  )
import Eclips.Oracle.Command
  ( OracleEnvelope,
    oracleEnvelope,
    oracleEnvelopeCommand,
    oracleEnvelopeExpectedControlIndex,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    appliedEntryProjectionEvents,
  )
import Eclips.Oracle.Receipt (OracleReceipt)
import Eclips.Oracle.State (decodeOracleCheckpoint, oracleCheckpointBytes, oracleStateGenesis)
import Eclips.Oracle.Transition (stepOracle)
import OracleFixtures
  ( checked,
    envelopeFor,
    fixtureGenesisTemplate,
    fixtureInitialState,
    fixtureRemoteHeraldEpoch,
    fixtureRemoteStart,
    fixtureStart,
    globalObjectId,
    processEpochId,
    processId,
    validCommand,
    validEnvelope,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "canonical transcripts"
    [ testProperty "successful envelope decoding re-encodes exactly" propEnvelopeRoundTrip,
      testProperty "successful committed transcript decoding re-encodes exactly" propCommittedRoundTrip,
      testCase "accepted and rejected receipts round-trip" caseReceiptRoundTrip,
      testCase "accepted and rejected applied entries round-trip" caseEntryRoundTrip,
      testCase "empty and Start event vectors round-trip" caseEventVectorRoundTrip,
      testCase "malformed and trailing bytes are rejected" caseMalformedAndTrailing,
      testCase "checkpoints reject empty, truncated, trailing and altered digest bytes" caseMalformedCheckpoint,
      testCase "command digest commits every envelope and minimal Start field" caseCommandDigestSensitivity,
      testCase "a sequenced configured root round-trips without a default" caseSequencedRootRoundTrip,
      testCase "immutable genesis root presentation is canonical" caseRootPresentationNormalization
    ]

propEnvelopeRoundTrip :: Word64 -> Bool
propEnvelopeRoundTrip sequenceNumber =
  envelopeRoundTripsExactly
    (canonicalizeOracleEnvelope (envelopeFor sequenceNumber validCommand))

propCommittedRoundTrip :: Word64 -> Bool
propCommittedRoundTrip sequenceNumber =
  let accepted = transitionProducts (envelopeFor sequenceNumber validCommand)
      rejected = transitionProducts (wrongResidenceEnvelope sequenceNumber)
   in all receiptRoundTripsExactly [fst accepted, fst rejected]
        && all entryRoundTripsExactly [snd accepted, snd rejected]

caseReceiptRoundTrip :: Assertion
caseReceiptRoundTrip =
  mapM_ assertReceiptRoundTrip [acceptedReceipt, rejectedReceipt]

caseEntryRoundTrip :: Assertion
caseEntryRoundTrip =
  mapM_ assertEntryRoundTrip [acceptedEntry, rejectedEntry]

caseEventVectorRoundTrip :: Assertion
caseEventVectorRoundTrip =
  mapM_
    ( \events ->
        let canonical = canonicalizeOracleProjectionEventVector events
         in assertEqual
              "projection event vector"
              (Right canonical)
              (decodeCanonicalOracleProjectionEventVector (canonicalOracleProjectionEventVectorBytes canonical))
    )
    [appliedEntryProjectionEvents rejectedEntry, appliedEntryProjectionEvents acceptedEntry]

caseMalformedAndTrailing :: Assertion
caseMalformedAndTrailing = do
  assertMalformed (decodeCanonicalOracleEnvelope ByteString.empty)
  assertMalformed (decodeCanonicalOracleReceipt ByteString.empty)
  assertMalformed (decodeCanonicalAppliedOracleEntry ByteString.empty)
  assertMalformed (decodeCanonicalOracleProjectionEventVector ByteString.empty)
  assertMalformed (decodeCanonicalOracleEnvelope (ByteString.snoc envelopeBytes 0))
  assertMalformed (decodeCanonicalOracleReceipt (ByteString.snoc acceptedReceiptBytes 0))
  assertMalformed (decodeCanonicalAppliedOracleEntry (ByteString.snoc acceptedEntryBytes 0))
  assertMalformed
    ( decodeCanonicalOracleProjectionEventVector
        (ByteString.snoc acceptedEventVectorBytes 0)
    )

caseMalformedCheckpoint :: Assertion
caseMalformedCheckpoint = do
  let bytes = oracleCheckpointBytes fixtureInitialState
      decode = decodeOracleCheckpoint (oracleStateGenesis fixtureInitialState)
      altered = ByteString.snoc (ByteString.init bytes) (ByteString.last bytes + 1)
  assertEqual "initial checkpoint" (Right fixtureInitialState) (decode bytes)
  mapM_
    (assertMalformed . decode)
    [ByteString.empty, ByteString.init bytes, ByteString.snoc bytes 0, altered]

caseCommandDigestSensitivity :: Assertion
caseCommandDigestSensitivity =
  mapM_
    ( \(label, changed) ->
        assertBool
          (label <> " participates")
          ( canonicalOracleEnvelopeDigest validCanonicalEnvelope
              /= canonicalOracleEnvelopeDigest (canonicalizeOracleEnvelope changed)
          )
    )
    [ ("request sequence", envelopeFor 2 validCommand),
      ("expected index", withExpectedControlIndex (Just (controlIndex 0)) validEnvelope),
      ("claimed home", withHome fixtureRemoteHeraldEpoch validEnvelope),
      ("process identity", envelopeFor 1 (startProcessEpochCommand (processStart (processId 221) (processStartProcessEpochId fixtureStart) (processStartResidence fixtureStart)))),
      ("process epoch", envelopeFor 1 (startProcessEpochCommand (processStart (processStartProcessId fixtureStart) (processEpochId 222) (processStartResidence fixtureStart)))),
      ("residence", envelopeFor 1 (startProcessEpochCommand (processStart (processStartProcessId fixtureStart) (processStartProcessEpochId fixtureStart) fixtureRemoteHeraldEpoch)))
    ]

caseSequencedRootRoundTrip :: Assertion
caseSequencedRootRoundTrip = do
  let sequenced =
        toBootstrap
          ( (replaceFirstRoot fixtureGenesisTemplate)
              { rootSequencing = NablaSequencedBy (globalObjectId 225)
              }
          )
      transcript = configuredProcessBootstrapTranscriptBytes sequenced
  assertEqual
    "configured-bootstrap inverse retains the explicit sequencing arm"
    (Right sequenced)
    (decodeConfiguredProcessBootstrapTranscriptBytes transcript)

caseRootPresentationNormalization :: Assertion
caseRootPresentationNormalization = do
  let normalized =
        requireBootstrap
          (configuredProcessBootstrapManifestId fixtureGenesisTemplate)
          (configuredProcessBootstrapProcessId fixtureGenesisTemplate)
          (configuredProcessBootstrapProcessEpochId fixtureGenesisTemplate)
          (configuredProcessBootstrapResidence fixtureGenesisTemplate)
          (reverse (configuredProcessBootstrapRoots fixtureGenesisTemplate))
  assertEqual "checked bootstrap normalization" fixtureGenesisTemplate normalized
  assertEqual
    "canonical bytes"
    (configuredProcessBootstrapTranscriptBytes fixtureGenesisTemplate)
    (configuredProcessBootstrapTranscriptBytes normalized)

class Rebuildable value where
  toBootstrap :: value -> ConfiguredProcessBootstrap

data RootReplacement = RootReplacement
  { rootBootstrap :: ConfiguredProcessBootstrap,
    rootWriter :: NablaId,
    rootSequencing :: NablaSequencing,
    rootReader :: DeltaId,
    rootStore :: StoreIncarnationId
  }

replaceFirstRoot :: ConfiguredProcessBootstrap -> RootReplacement
replaceFirstRoot bootstrap = case configuredProcessBootstrapRoots bootstrap of
  [] -> error "configured bootstrap has no roots"
  root : _ ->
    RootReplacement
      bootstrap
      (configuredRootBootstrapWriterNabla root)
      (configuredRootBootstrapWriterSequencing root)
      (configuredRootBootstrapReaderDelta root)
      (configuredRootBootstrapStoreIncarnation root)

instance Rebuildable RootReplacement where
  toBootstrap replacement = case configuredProcessBootstrapRoots (rootBootstrap replacement) of
    [] -> error "configured bootstrap has no roots"
    root : remaining ->
      requireBootstrap
        (configuredProcessBootstrapManifestId (rootBootstrap replacement))
        (configuredProcessBootstrapProcessId (rootBootstrap replacement))
        (configuredProcessBootstrapProcessEpochId (rootBootstrap replacement))
        (configuredProcessBootstrapResidence (rootBootstrap replacement))
        ( configuredRootBootstrap
            (configuredRootBootstrapCatalogueRole root)
            (rootWriter replacement)
            (rootSequencing replacement)
            (rootReader replacement)
            (rootStore replacement)
            : remaining
        )

requireBootstrap ::
  BootstrapManifestId ->
  ProcessId ->
  ProcessEpochId ->
  HeraldEpoch ->
  [ConfiguredRootBootstrap] ->
  ConfiguredProcessBootstrap
requireBootstrap manifest process epoch residence roots =
  maybe
    (error "canonical mutation did not produce a structurally checked bootstrap")
    id
    (mkConfiguredProcessBootstrap manifest process epoch residence roots)

wrongResidenceEnvelope :: Word64 -> OracleEnvelope
wrongResidenceEnvelope sequenceNumber =
  envelopeFor sequenceNumber (startProcessEpochCommand fixtureRemoteStart)

transitionProducts :: OracleEnvelope -> (OracleReceipt, AppliedOracleEntry)
transitionProducts supplied =
  case checked "canonical transition" (stepOracle supplied fixtureInitialState) of
    (_, OracleCommitted receipt, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] -> (receipt, entry)
      other -> error ("expected one canonical entry, got " <> show other)
    (_, other, _) -> error ("expected committed canonical result, got " <> show other)

acceptedReceipt :: OracleReceipt
acceptedEntry :: AppliedOracleEntry
(acceptedReceipt, acceptedEntry) = transitionProducts validEnvelope

rejectedReceipt :: OracleReceipt
rejectedEntry :: AppliedOracleEntry
(rejectedReceipt, rejectedEntry) = transitionProducts (wrongResidenceEnvelope 2)

validCanonicalEnvelope :: CanonicalOracleEnvelope
validCanonicalEnvelope = canonicalizeOracleEnvelope validEnvelope

envelopeBytes :: ByteString
envelopeBytes = canonicalOracleEnvelopeBytes validCanonicalEnvelope

acceptedReceiptBytes :: ByteString
acceptedReceiptBytes =
  canonicalOracleReceiptBytes (canonicalizeOracleReceipt acceptedReceipt)

acceptedEntryBytes :: ByteString
acceptedEntryBytes =
  canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry acceptedEntry)

acceptedEventVectorBytes :: ByteString
acceptedEventVectorBytes =
  canonicalOracleProjectionEventVectorBytes
    (canonicalizeOracleProjectionEventVector (appliedEntryProjectionEvents acceptedEntry))

assertReceiptRoundTrip :: OracleReceipt -> Assertion
assertReceiptRoundTrip receipt =
  let canonical = canonicalizeOracleReceipt receipt
   in assertEqual
        "receipt"
        (Right canonical)
        (decodeCanonicalOracleReceipt (canonicalOracleReceiptBytes canonical))

assertEntryRoundTrip :: AppliedOracleEntry -> Assertion
assertEntryRoundTrip entry =
  let canonical = canonicalizeAppliedOracleEntry entry
   in assertEqual
        "entry"
        (Right canonical)
        (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes canonical))

envelopeRoundTripsExactly :: CanonicalOracleEnvelope -> Bool
envelopeRoundTripsExactly canonical =
  case decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes canonical) of
    Right decoded ->
      canonicalOracleEnvelopeValue decoded == canonicalOracleEnvelopeValue canonical
        && canonicalOracleEnvelopeBytes decoded == canonicalOracleEnvelopeBytes canonical
    Left _ -> False

receiptRoundTripsExactly :: OracleReceipt -> Bool
receiptRoundTripsExactly receipt =
  let canonical = canonicalizeOracleReceipt receipt
   in case decodeCanonicalOracleReceipt (canonicalOracleReceiptBytes canonical) of
        Right decoded ->
          canonicalOracleReceiptValue decoded == receipt
            && canonicalOracleReceiptBytes decoded == canonicalOracleReceiptBytes canonical
        Left _ -> False

entryRoundTripsExactly :: AppliedOracleEntry -> Bool
entryRoundTripsExactly entry =
  let canonical = canonicalizeAppliedOracleEntry entry
   in case decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes canonical) of
        Right decoded ->
          canonicalAppliedOracleEntryValue decoded == entry
            && canonicalAppliedOracleEntryBytes decoded == canonicalAppliedOracleEntryBytes canonical
        Left _ -> False

assertMalformed :: (Show value) => Either OracleCanonicalError value -> Assertion
assertMalformed result = case result of
  Left (MalformedCanonicalOracleBytes _) -> pure ()
  other -> error ("expected malformed canonical bytes, got " <> show other)

withExpectedControlIndex :: Maybe ControlIndex -> OracleEnvelope -> OracleEnvelope
withExpectedControlIndex expected envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    expected
    (oracleEnvelopeHomeHeraldEpoch envelope)
    (oracleEnvelopeCommand envelope)

withHome :: HeraldEpoch -> OracleEnvelope -> OracleEnvelope
withHome home envelope =
  oracleEnvelope
    (oracleEnvelopeRequestId envelope)
    (oracleEnvelopeExpectedControlIndex envelope)
    home
    (oracleEnvelopeCommand envelope)
