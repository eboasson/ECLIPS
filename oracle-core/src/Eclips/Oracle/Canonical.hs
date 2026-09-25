-- | Domain-separated checked canonical Oracle transcripts.
module Eclips.Oracle.Canonical
  ( OracleCanonicalError (..),
    CanonicalOracleEnvelope,
    canonicalizeOracleEnvelope,
    decodeCanonicalOracleEnvelope,
    canonicalOracleEnvelopeValue,
    canonicalOracleEnvelopeBytes,
    canonicalOracleEnvelopeDigest,
    oracleEnvelopeDigest,
    CanonicalOracleReceipt,
    canonicalizeOracleReceipt,
    decodeCanonicalOracleReceipt,
    canonicalOracleReceiptValue,
    canonicalOracleReceiptBytes,
    CanonicalOracleProjectionEventVector,
    canonicalizeOracleProjectionEventVector,
    decodeCanonicalOracleProjectionEventVector,
    canonicalOracleProjectionEventVectorValue,
    canonicalOracleProjectionEventVectorBytes,
    CanonicalAppliedOracleEntry,
    canonicalizeAppliedOracleEntry,
    decodeCanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryValue,
    canonicalAppliedOracleEntryBytes,
    configuredProcessBootstrapTranscriptBytes,
    decodeConfiguredProcessBootstrapTranscriptBytes,
    encodeLabelDecision,
    decodeLabelDecision,
    encodeLabelTerminalOutcome,
    decodeLabelTerminalOutcome,
    encodeProcessEpochRecord,
    decodeProcessEpochRecord,
  )
where

import Data.ByteString (ByteString)
import Eclips.Domain.Startup (ConfiguredProcessBootstrap)
import Eclips.Oracle.Identity (OracleCommandDigest)
import Eclips.Oracle.Internal.ConfiguredBootstrapCanonical qualified as Bootstrap
import Eclips.Oracle.Internal.Label
  ( AppliedOracleEntry,
    DecodedAppliedOracleEntry,
    DecodedOracleEnvelope,
    DecodedOracleProjectionEventVector,
    DecodedOracleReceipt,
    OracleCanonicalError (..),
    OracleEnvelope,
    OracleProjectionEvent,
    OracleReceipt,
    decodeLabelDecision,
    decodeLabelTerminalOutcome,
    decodeProcessEpochRecord,
    encodeLabelDecision,
    encodeLabelTerminalOutcome,
    encodeProcessEpochRecord,
  )
import Eclips.Oracle.Internal.Label qualified as Internal

data CanonicalOracleEnvelope
  = CanonicalOracleEnvelope OracleEnvelope ByteString OracleCommandDigest
  deriving stock (Eq, Show)

canonicalizeOracleEnvelope :: OracleEnvelope -> CanonicalOracleEnvelope
canonicalizeOracleEnvelope envelope =
  CanonicalOracleEnvelope
    envelope
    (Internal.oracleEnvelopeCanonicalBytes envelope)
    (Internal.oracleEnvelopeDigest envelope)

decodeCanonicalOracleEnvelope ::
  ByteString -> Either OracleCanonicalError CanonicalOracleEnvelope
decodeCanonicalOracleEnvelope bytes = do
  decoded <- Internal.decodeOracleEnvelopeCanonicalBytes bytes
  pure (canonicalEnvelopeFromDecoded decoded)

canonicalEnvelopeFromDecoded ::
  DecodedOracleEnvelope -> CanonicalOracleEnvelope
canonicalEnvelopeFromDecoded decoded =
  canonicalizeOracleEnvelope (Internal.decodedOracleEnvelopeValue decoded)

canonicalOracleEnvelopeValue :: CanonicalOracleEnvelope -> OracleEnvelope
canonicalOracleEnvelopeValue (CanonicalOracleEnvelope envelope _ _) = envelope

canonicalOracleEnvelopeBytes :: CanonicalOracleEnvelope -> ByteString
canonicalOracleEnvelopeBytes (CanonicalOracleEnvelope _ bytes _) = bytes

canonicalOracleEnvelopeDigest :: CanonicalOracleEnvelope -> OracleCommandDigest
canonicalOracleEnvelopeDigest (CanonicalOracleEnvelope _ _ digest) = digest

oracleEnvelopeDigest :: OracleEnvelope -> OracleCommandDigest
oracleEnvelopeDigest = Internal.oracleEnvelopeDigest

data CanonicalOracleReceipt
  = CanonicalOracleReceipt OracleReceipt ByteString
  deriving stock (Eq, Show)

canonicalizeOracleReceipt :: OracleReceipt -> CanonicalOracleReceipt
canonicalizeOracleReceipt receipt =
  CanonicalOracleReceipt receipt (Internal.oracleReceiptCanonicalBytes receipt)

decodeCanonicalOracleReceipt ::
  ByteString -> Either OracleCanonicalError CanonicalOracleReceipt
decodeCanonicalOracleReceipt bytes = do
  decoded <- Internal.decodeOracleReceiptCanonicalBytes bytes
  pure (canonicalReceiptFromDecoded decoded)

canonicalReceiptFromDecoded :: DecodedOracleReceipt -> CanonicalOracleReceipt
canonicalReceiptFromDecoded decoded =
  canonicalizeOracleReceipt (Internal.decodedOracleReceiptValue decoded)

canonicalOracleReceiptValue :: CanonicalOracleReceipt -> OracleReceipt
canonicalOracleReceiptValue (CanonicalOracleReceipt receipt _) = receipt

canonicalOracleReceiptBytes :: CanonicalOracleReceipt -> ByteString
canonicalOracleReceiptBytes (CanonicalOracleReceipt _ bytes) = bytes

data CanonicalOracleProjectionEventVector
  = CanonicalOracleProjectionEventVector [OracleProjectionEvent] ByteString
  deriving stock (Eq, Show)

canonicalizeOracleProjectionEventVector ::
  [OracleProjectionEvent] -> CanonicalOracleProjectionEventVector
canonicalizeOracleProjectionEventVector events =
  CanonicalOracleProjectionEventVector
    events
    (Internal.oracleProjectionEventVectorCanonicalBytes events)

decodeCanonicalOracleProjectionEventVector ::
  ByteString -> Either OracleCanonicalError CanonicalOracleProjectionEventVector
decodeCanonicalOracleProjectionEventVector bytes = do
  decoded <- Internal.decodeOracleProjectionEventVectorCanonicalBytes bytes
  pure (canonicalProjectionVectorFromDecoded decoded)

canonicalProjectionVectorFromDecoded ::
  DecodedOracleProjectionEventVector ->
  CanonicalOracleProjectionEventVector
canonicalProjectionVectorFromDecoded decoded =
  canonicalizeOracleProjectionEventVector
    (Internal.decodedOracleProjectionEventVectorValue decoded)

canonicalOracleProjectionEventVectorValue ::
  CanonicalOracleProjectionEventVector -> [OracleProjectionEvent]
canonicalOracleProjectionEventVectorValue
  (CanonicalOracleProjectionEventVector events _) = events

canonicalOracleProjectionEventVectorBytes ::
  CanonicalOracleProjectionEventVector -> ByteString
canonicalOracleProjectionEventVectorBytes
  (CanonicalOracleProjectionEventVector _ bytes) = bytes

data CanonicalAppliedOracleEntry
  = CanonicalAppliedOracleEntry AppliedOracleEntry ByteString
  deriving stock (Eq, Show)

canonicalizeAppliedOracleEntry ::
  AppliedOracleEntry -> CanonicalAppliedOracleEntry
canonicalizeAppliedOracleEntry entry =
  CanonicalAppliedOracleEntry
    entry
    (Internal.appliedOracleEntryCanonicalBytes entry)

decodeCanonicalAppliedOracleEntry ::
  ByteString -> Either OracleCanonicalError CanonicalAppliedOracleEntry
decodeCanonicalAppliedOracleEntry bytes = do
  decoded <- Internal.decodeAppliedOracleEntryCanonicalBytes bytes
  pure (canonicalEntryFromDecoded decoded)

canonicalEntryFromDecoded ::
  DecodedAppliedOracleEntry -> CanonicalAppliedOracleEntry
canonicalEntryFromDecoded decoded =
  canonicalizeAppliedOracleEntry (Internal.decodedAppliedOracleEntryValue decoded)

canonicalAppliedOracleEntryValue ::
  CanonicalAppliedOracleEntry -> AppliedOracleEntry
canonicalAppliedOracleEntryValue (CanonicalAppliedOracleEntry entry _) = entry

canonicalAppliedOracleEntryBytes ::
  CanonicalAppliedOracleEntry -> ByteString
canonicalAppliedOracleEntryBytes (CanonicalAppliedOracleEntry _ bytes) = bytes

configuredProcessBootstrapTranscriptBytes ::
  ConfiguredProcessBootstrap -> ByteString
configuredProcessBootstrapTranscriptBytes =
  Bootstrap.configuredProcessBootstrapTranscriptBytes

decodeConfiguredProcessBootstrapTranscriptBytes ::
  ByteString -> Either OracleCanonicalError ConfiguredProcessBootstrap
decodeConfiguredProcessBootstrapTranscriptBytes bytes =
  case Bootstrap.decodeConfiguredProcessBootstrapTranscriptBytes bytes of
    Left problem -> Left (MalformedCanonicalOracleBytes (show problem))
    Right bootstrap -> Right bootstrap
