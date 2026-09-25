{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Temporary pure reference model for the Step-15 Herald-failure workflow.
--
-- The live Oracle command, protocol, and runtime are deliberately unchanged.
-- This module gives the package-private Step-15 composition harness a checked
-- canonical model whose command tags already occupy their eventual positions.
-- Dismissal records only the clock-free Oracle terminal; the detached Herald
-- harness owns cooldown tokens, absolute deadlines, and recovery rearming.
module Eclips.Oracle.Step15.Reference
  ( -- * Commands and envelopes
    ReferenceProbeResult (..),
    ReferenceOracleCommand,
    referenceOpenHeraldFailureProbeCommand,
    referenceReportHeraldFailureProbeCommand,
    referenceDismissHeraldFailureProbeCommand,
    referenceRetireHeraldEpochCommand,
    referenceOracleCommandTag,
    referenceOracleCommandDigest,
    referenceOracleCommandCanonicalBytes,
    ReferenceCommandCanonicalProblem (..),
    decodeReferenceOracleCommandCanonicalBytes,
    ReferenceOracleEnvelope,
    referenceOracleEnvelope,
    referenceOracleEnvelopeRequestId,
    referenceOracleEnvelopeHome,
    referenceOracleEnvelopeCommand,
    referenceOracleEnvelopeDigest,
    referenceOracleEnvelopeCanonicalBytes,
    ReferenceEnvelopeCanonicalProblem (..),
    decodeReferenceOracleEnvelopeCanonicalBytes,

    -- * Probe and process views
    ReferenceProbeTerminalView (..),
    ReferenceFailureProbeView (..),
    ReferenceTerminalIntention,
    referenceTerminalIntentionResolutionId,
    referenceTerminalIntentionCommand,
    referenceTerminalIntentionCommandDigest,
    ReferenceProcessLifecycleView (..),
    ReferenceProcessEpochRecord,
    referenceProcessRecordProcessId,
    referenceProcessRecordProcessEpoch,
    referenceProcessRecordResidence,
    referenceProcessRecordLifecycle,

    -- * Receipts and projection
    ReferenceCommandResult (..),
    ReferenceOracleRejection (..),
    ReferenceOracleReceiptResult (..),
    ReferenceOracleReceipt,
    referenceOracleReceiptRequestId,
    referenceOracleReceiptCommandDigest,
    referenceOracleReceiptControlIndex,
    referenceOracleReceiptResult,
    referenceOracleReceiptCanonicalBytes,
    ReferenceProjectionEventView (..),
    ReferenceProjectionEvent,
    referenceProjectionEventView,
    referenceProjectionEventTag,
    ReferenceAppliedOracleEntry,
    referenceAppliedEntryControlIndex,
    referenceAppliedEntryReceipt,
    referenceAppliedEntryProjectionEvents,
    referenceAppliedEntryPostStateDigest,
    ReferenceProtocolDisposition (..),
    ReferenceSubmissionOutcome (..),

    -- * State and transition
    ReferenceOracleState,
    initialReferenceOracle,
    submitReferenceOracle,
    referenceOracleGreatestControlIndex,
    referenceOracleStateDigest,
    referenceOracleRequestCount,
    referenceOracleCurrentMembership,
    referenceOracleMembershipHistory,
    referenceOracleRetiredHeralds,
    referenceOracleFailureProbe,
    referenceOracleActiveProbeFor,
    referenceOracleTerminalIntention,
    referenceOracleProcessRecord,
    referenceOracleStateCanonicalBytes,
    ReferenceStateCanonicalProblem (..),
    decodeReferenceOracleStateCanonicalBytes,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Serialize.Put qualified as SerializePut
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    IdentityError,
    ProcessEpochId,
    ProcessId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    mkHeraldEpoch,
    processEpochIdBytes,
    processIdBytes,
  )
import Eclips.Domain.Membership
  ( FailureProbeIdCanonicalProblem,
    FailureProbeResolution (..),
    FailureProbeResolutionCanonicalProblem,
    FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    MembershipIdentityError,
    decodeFailureProbeResolutionIdCanonicalBytes,
    decodeHeraldFailureProbeIdCanonicalBytes,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    failureProbeResolutionDisposition,
    failureProbeResolutionIdCanonicalBytes,
    failureProbeResolutionProbeId,
    genesisHeraldMembershipGeneration,
    heraldFailureProbeIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    mkHeraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (..),
    processEndReasonCanonicalBytes,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    HeraldMember (heraldMemberEpoch),
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleActiveHeralds,
    checkedOracleAppliedBootstraps,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    raftVoterBindingHeraldEpoch,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    OracleCommandDigest,
    OracleStateDigest,
    mkOracleCommandDigest,
    mkOracleStateDigest,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
    oracleCommandDigestBytes,
  )
import Eclips.Oracle.Internal.Label
  ( initialOracleState,
    oracleStateCanonicalBytes,
  )
import GHC.Generics (Generic)

-- Commands -----------------------------------------------------------------

data ReferenceProbeResult
  = ReferenceProbeReachable
  | ReferenceProbeUnreachable
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data ReferenceOracleCommand
  = ReferenceOpenHeraldFailureProbe
      HeraldEpoch
      HeraldMembershipGenerationId
  | ReferenceReportHeraldFailureProbe
      HeraldFailureProbeId
      ReferenceProbeResult
  | ReferenceDismissHeraldFailureProbe
      FailureProbeResolutionId
  | ReferenceRetireHeraldEpoch
      FailureProbeResolutionId
      HeraldEpoch
  deriving stock (Eq, Show)

referenceOpenHeraldFailureProbeCommand ::
  HeraldEpoch -> HeraldMembershipGenerationId -> ReferenceOracleCommand
referenceOpenHeraldFailureProbeCommand = ReferenceOpenHeraldFailureProbe

referenceReportHeraldFailureProbeCommand ::
  HeraldFailureProbeId -> ReferenceProbeResult -> ReferenceOracleCommand
referenceReportHeraldFailureProbeCommand = ReferenceReportHeraldFailureProbe

referenceDismissHeraldFailureProbeCommand ::
  HeraldFailureProbeId -> ReferenceOracleCommand
referenceDismissHeraldFailureProbeCommand probe =
  ReferenceDismissHeraldFailureProbe
    (deriveFailureProbeResolutionId probe DismissFailureProbe)

referenceRetireHeraldEpochCommand ::
  HeraldFailureProbeId -> HeraldEpoch -> ReferenceOracleCommand
referenceRetireHeraldEpochCommand probe =
  ReferenceRetireHeraldEpoch
    (deriveFailureProbeResolutionId probe RetireFailureProbeTarget)

-- | Final prospective command tags.  The live tags 0 through 8 remain owned by
-- the current closed Oracle sum until the atomic cutover.
referenceOracleCommandTag :: ReferenceOracleCommand -> Word8
referenceOracleCommandTag command = case command of
  ReferenceOpenHeraldFailureProbe {} -> 9
  ReferenceReportHeraldFailureProbe {} -> 10
  ReferenceDismissHeraldFailureProbe {} -> 11
  ReferenceRetireHeraldEpoch {} -> 12

referenceOracleCommandDigest :: ReferenceOracleCommand -> OracleCommandDigest
referenceOracleCommandDigest =
  commandDigestInvariant . SHA256.hash . referenceOracleCommandCanonicalBytes

referenceOracleCommandCanonicalBytes :: ReferenceOracleCommand -> ByteString
referenceOracleCommandCanonicalBytes command =
  Serialize.encode
    ( ReferenceCommandTranscript
        referenceCommandDomain
        (referenceOracleCommandTag command)
        (referenceCommandPayloadBytes command)
    )

referenceCommandPayloadBytes :: ReferenceOracleCommand -> ByteString
referenceCommandPayloadBytes command = case command of
  ReferenceOpenHeraldFailureProbe target generation ->
    Serialize.encode
      ( ReferenceOpenPayload
          (heraldEpochBytes target)
          (heraldMembershipGenerationIdBytes generation)
      )
  ReferenceReportHeraldFailureProbe probe result ->
    Serialize.encode
      ( ReferenceReportPayload
          (heraldFailureProbeIdCanonicalBytes probe)
          (probeResultTag result)
      )
  ReferenceDismissHeraldFailureProbe resolution ->
    Serialize.encode
      (ReferenceDismissPayload (failureProbeResolutionIdCanonicalBytes resolution))
  ReferenceRetireHeraldEpoch resolution target ->
    Serialize.encode
      ( ReferenceRetirePayload
          (failureProbeResolutionIdCanonicalBytes resolution)
          (heraldEpochBytes target)
      )

data ReferenceCommandCanonicalProblem
  = ReferenceCommandCanonicalDecodeFailed String
  | ReferenceCommandCanonicalWrongDomain ByteString
  | ReferenceCommandCanonicalUnknownTag Word8
  | ReferenceCommandCanonicalPayloadDecodeFailed String
  | ReferenceCommandCanonicalInvalidHerald IdentityError
  | ReferenceCommandCanonicalInvalidMembershipIdentity MembershipIdentityError
  | ReferenceCommandCanonicalInvalidProbe FailureProbeIdCanonicalProblem
  | ReferenceCommandCanonicalInvalidResolution FailureProbeResolutionCanonicalProblem
  | ReferenceCommandCanonicalWrongResolutionDisposition
      FailureProbeResolution
      FailureProbeResolution
  | ReferenceCommandCanonicalUnknownProbeResult Word8
  | ReferenceCommandCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeReferenceOracleCommandCanonicalBytes ::
  ByteString -> Either ReferenceCommandCanonicalProblem ReferenceOracleCommand
decodeReferenceOracleCommandCanonicalBytes bytes = do
  ReferenceCommandTranscript domain tag payload <-
    decodeCanonical ReferenceCommandCanonicalDecodeFailed bytes
  if domain == referenceCommandDomain
    then Right ()
    else Left (ReferenceCommandCanonicalWrongDomain domain)
  command <- case tag of
    9 -> do
      ReferenceOpenPayload targetBytes generationBytes <- decodeReferenceCommandPayload payload
      target <- decodeHerald targetBytes
      generation <-
        either
          (Left . ReferenceCommandCanonicalInvalidMembershipIdentity)
          Right
          (mkHeraldMembershipGenerationId generationBytes)
      Right (ReferenceOpenHeraldFailureProbe target generation)
    10 -> do
      ReferenceReportPayload probeBytes resultTag <- decodeReferenceCommandPayload payload
      probe <- decodeProbe probeBytes
      result <- case resultTag of
        0 -> Right ReferenceProbeReachable
        1 -> Right ReferenceProbeUnreachable
        other -> Left (ReferenceCommandCanonicalUnknownProbeResult other)
      Right (ReferenceReportHeraldFailureProbe probe result)
    11 -> do
      ReferenceDismissPayload resolutionBytes <- decodeReferenceCommandPayload payload
      resolution <- decodeResolution resolutionBytes
      checkDisposition DismissFailureProbe resolution
      Right (ReferenceDismissHeraldFailureProbe resolution)
    12 -> do
      ReferenceRetirePayload resolutionBytes targetBytes <- decodeReferenceCommandPayload payload
      resolution <- decodeResolution resolutionBytes
      checkDisposition RetireFailureProbeTarget resolution
      target <- decodeHerald targetBytes
      Right (ReferenceRetireHeraldEpoch resolution target)
    other -> Left (ReferenceCommandCanonicalUnknownTag other)
  if referenceOracleCommandCanonicalBytes command == bytes
    then Right command
    else Left ReferenceCommandCanonicalNonCanonical
  where
    decodeHerald =
      either (Left . ReferenceCommandCanonicalInvalidHerald) Right . mkHeraldEpoch
    decodeProbe =
      either (Left . ReferenceCommandCanonicalInvalidProbe) Right
        . decodeHeraldFailureProbeIdCanonicalBytes
    decodeResolution =
      either (Left . ReferenceCommandCanonicalInvalidResolution) Right
        . decodeFailureProbeResolutionIdCanonicalBytes
    checkDisposition expected resolution
      | failureProbeResolutionDisposition resolution == expected = Right ()
      | otherwise =
          Left
            ( ReferenceCommandCanonicalWrongResolutionDisposition
                expected
                (failureProbeResolutionDisposition resolution)
            )

probeResultTag :: ReferenceProbeResult -> Word8
probeResultTag ReferenceProbeReachable = 0
probeResultTag ReferenceProbeUnreachable = 1

-- Envelopes ----------------------------------------------------------------

data ReferenceOracleEnvelope
  = ReferenceOracleEnvelope
      OracleClientRequestId
      (Maybe ControlIndex)
      HeraldEpoch
      ReferenceOracleCommand
  deriving stock (Eq, Show)

referenceOracleEnvelope ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  ReferenceOracleCommand ->
  ReferenceOracleEnvelope
referenceOracleEnvelope = ReferenceOracleEnvelope

referenceOracleEnvelopeRequestId ::
  ReferenceOracleEnvelope -> OracleClientRequestId
referenceOracleEnvelopeRequestId
  (ReferenceOracleEnvelope request _ _ _) = request

referenceOracleEnvelopeHome :: ReferenceOracleEnvelope -> HeraldEpoch
referenceOracleEnvelopeHome (ReferenceOracleEnvelope _ _ home _) = home

referenceOracleEnvelopeCommand ::
  ReferenceOracleEnvelope -> ReferenceOracleCommand
referenceOracleEnvelopeCommand (ReferenceOracleEnvelope _ _ _ command) = command

referenceOracleEnvelopeDigest :: ReferenceOracleEnvelope -> OracleCommandDigest
referenceOracleEnvelopeDigest =
  commandDigestInvariant . SHA256.hash . referenceOracleEnvelopeCanonicalBytes

referenceOracleEnvelopeCanonicalBytes :: ReferenceOracleEnvelope -> ByteString
referenceOracleEnvelopeCanonicalBytes
  (ReferenceOracleEnvelope request expected home command) =
    Serialize.encode
      ( ReferenceEnvelopeTranscript
          referenceEnvelopeDomain
          (heraldEpochBytes (oracleClientRequestHome request))
          (oracleClientRequestSequence request)
          (controlIndexWord64 <$> expected)
          (heraldEpochBytes home)
          (referenceOracleCommandCanonicalBytes command)
      )

data ReferenceEnvelopeCanonicalProblem
  = ReferenceEnvelopeCanonicalDecodeFailed String
  | ReferenceEnvelopeCanonicalWrongDomain ByteString
  | ReferenceEnvelopeCanonicalInvalidHerald IdentityError
  | ReferenceEnvelopeCanonicalInvalidCommand ReferenceCommandCanonicalProblem
  | ReferenceEnvelopeCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeReferenceOracleEnvelopeCanonicalBytes ::
  ByteString -> Either ReferenceEnvelopeCanonicalProblem ReferenceOracleEnvelope
decodeReferenceOracleEnvelopeCanonicalBytes bytes = do
  ReferenceEnvelopeTranscript domain requestHomeBytes sequenceNumber expectedRaw homeBytes commandBytes <-
    decodeCanonical ReferenceEnvelopeCanonicalDecodeFailed bytes
  if domain == referenceEnvelopeDomain
    then Right ()
    else Left (ReferenceEnvelopeCanonicalWrongDomain domain)
  requestHome <- decodeHerald requestHomeBytes
  home <- decodeHerald homeBytes
  command <-
    either
      (Left . ReferenceEnvelopeCanonicalInvalidCommand)
      Right
      (decodeReferenceOracleCommandCanonicalBytes commandBytes)
  let envelope =
        ReferenceOracleEnvelope
          (oracleClientRequestId requestHome sequenceNumber)
          (controlIndex <$> expectedRaw)
          home
          command
  if referenceOracleEnvelopeCanonicalBytes envelope == bytes
    then Right envelope
    else Left ReferenceEnvelopeCanonicalNonCanonical
  where
    decodeHerald =
      either (Left . ReferenceEnvelopeCanonicalInvalidHerald) Right . mkHeraldEpoch

-- Probe and process state ---------------------------------------------------

data ReferenceProbeTerminal
  = ReferenceProbeDismissed FailureProbeResolutionId ControlIndex
  | ReferenceProbeRetired
      FailureProbeResolutionId
      HeraldMembershipGenerationId
      ControlIndex
  | ReferenceProbeMembershipSuperseded
      HeraldMembershipGenerationId
      ControlIndex
  deriving stock (Eq, Show)

data ReferenceProbeTerminalView
  = ReferenceProbeDismissedView FailureProbeResolutionId ControlIndex
  | ReferenceProbeRetiredView
      FailureProbeResolutionId
      HeraldMembershipGenerationId
      ControlIndex
  | ReferenceProbeMembershipSupersededView
      HeraldMembershipGenerationId
      ControlIndex
  deriving stock (Eq, Show)

probeTerminalView :: ReferenceProbeTerminal -> ReferenceProbeTerminalView
probeTerminalView terminal = case terminal of
  ReferenceProbeDismissed resolution index ->
    ReferenceProbeDismissedView resolution index
  ReferenceProbeRetired resolution successor index ->
    ReferenceProbeRetiredView resolution successor index
  ReferenceProbeMembershipSuperseded successor index ->
    ReferenceProbeMembershipSupersededView successor index

data ReferenceFailureProbe
  = ReferenceFailureProbe
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      ControlIndex
      (Map HeraldEpoch ReferenceProbeResult)
      (Maybe ReferenceProbeTerminal)
  deriving stock (Eq, Show)

data ReferenceFailureProbeView
  = ReferenceFailureProbeView
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      ControlIndex
      [(HeraldEpoch, ReferenceProbeResult)]
      (Maybe ReferenceProbeTerminalView)
  deriving stock (Eq, Show)

failureProbeView :: ReferenceFailureProbe -> ReferenceFailureProbeView
failureProbeView
  (ReferenceFailureProbe probe target generation opened reports terminal) =
    ReferenceFailureProbeView
      probe
      target
      generation
      opened
      (Map.toAscList reports)
      (probeTerminalView <$> terminal)

data ReferenceTerminalIntention
  = ReferenceTerminalIntention
      FailureProbeResolutionId
      ReferenceOracleCommand
      OracleCommandDigest
  deriving stock (Eq, Show)

referenceTerminalIntentionResolutionId ::
  ReferenceTerminalIntention -> FailureProbeResolutionId
referenceTerminalIntentionResolutionId
  (ReferenceTerminalIntention resolution _ _) = resolution

referenceTerminalIntentionCommand ::
  ReferenceTerminalIntention -> ReferenceOracleCommand
referenceTerminalIntentionCommand
  (ReferenceTerminalIntention _ command _) = command

referenceTerminalIntentionCommandDigest ::
  ReferenceTerminalIntention -> OracleCommandDigest
referenceTerminalIntentionCommandDigest
  (ReferenceTerminalIntention _ _ digest) = digest

data ReferenceProcessLifecycle
  = ReferenceProcessLive
  | ReferenceProcessEnded ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

data ReferenceProcessLifecycleView
  = ReferenceProcessLiveView
  | ReferenceProcessEndedView ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

data ReferenceProcessEpochRecord
  = ReferenceProcessEpochRecord
      ProcessId
      ProcessEpochId
      HeraldEpoch
      ReferenceProcessLifecycle
  deriving stock (Eq, Show)

referenceProcessRecordProcessId :: ReferenceProcessEpochRecord -> ProcessId
referenceProcessRecordProcessId
  (ReferenceProcessEpochRecord processId _ _ _) = processId

referenceProcessRecordProcessEpoch ::
  ReferenceProcessEpochRecord -> ProcessEpochId
referenceProcessRecordProcessEpoch
  (ReferenceProcessEpochRecord _ process _ _) = process

referenceProcessRecordResidence ::
  ReferenceProcessEpochRecord -> HeraldEpoch
referenceProcessRecordResidence
  (ReferenceProcessEpochRecord _ _ residence _) = residence

referenceProcessRecordLifecycle ::
  ReferenceProcessEpochRecord -> ReferenceProcessLifecycleView
referenceProcessRecordLifecycle
  (ReferenceProcessEpochRecord _ _ _ lifecycle) = case lifecycle of
    ReferenceProcessLive -> ReferenceProcessLiveView
    ReferenceProcessEnded index reason ->
      ReferenceProcessEndedView index reason

-- Receipts and projections --------------------------------------------------

data ReferenceCommandResult
  = ReferenceProbeOpened HeraldFailureProbeId
  | ReferenceProbeReportRecorded
      HeraldFailureProbeId
      (Maybe ReferenceTerminalIntention)
  | ReferenceProbeDismissedResult FailureProbeResolutionId
  | ReferenceHeraldRetiredResult
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data ReferenceOracleRejection
  = ReferenceRequestHomeEpochMismatch HeraldEpoch HeraldEpoch
  | ReferenceInactiveHomeHerald HeraldEpoch
  | ReferenceStaleExpectedControlIndex ControlIndex ControlIndex
  | ReferenceStaleMembershipGeneration
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | ReferenceFailureProbeTargetNotActive HeraldEpoch
  | ReferenceFailureProbeTargetHostsVoter HeraldEpoch
  | ReferenceUnknownFailureProbe HeraldFailureProbeId
  | ReferenceFailureProbeTerminal
      HeraldFailureProbeId
      ReferenceProbeTerminalView
  | ReferenceIneligibleFailureReporter HeraldEpoch
  | ReferenceConflictingFailureReport
      HeraldFailureProbeId
      HeraldEpoch
      ReferenceProbeResult
      ReferenceProbeResult
  | ReferenceFailureProbeThresholdNotReached
      HeraldFailureProbeId
      FailureProbeResolution
  | ReferenceRetirementTargetMismatch HeraldEpoch HeraldEpoch
  deriving stock (Eq, Show)

data ReferenceOracleReceiptResult
  = ReferenceOracleAccepted ReferenceCommandResult
  | ReferenceOracleRejected ReferenceOracleRejection
  deriving stock (Eq, Show)

data ReferenceOracleReceipt
  = ReferenceOracleReceipt
      OracleClientRequestId
      OracleCommandDigest
      ControlIndex
      ReferenceOracleReceiptResult
  deriving stock (Eq, Show)

referenceOracleReceiptRequestId ::
  ReferenceOracleReceipt -> OracleClientRequestId
referenceOracleReceiptRequestId
  (ReferenceOracleReceipt request _ _ _) = request

referenceOracleReceiptCommandDigest ::
  ReferenceOracleReceipt -> OracleCommandDigest
referenceOracleReceiptCommandDigest
  (ReferenceOracleReceipt _ digest _ _) = digest

referenceOracleReceiptControlIndex :: ReferenceOracleReceipt -> ControlIndex
referenceOracleReceiptControlIndex
  (ReferenceOracleReceipt _ _ index _) = index

referenceOracleReceiptResult ::
  ReferenceOracleReceipt -> ReferenceOracleReceiptResult
referenceOracleReceiptResult
  (ReferenceOracleReceipt _ _ _ result) = result

referenceOracleReceiptCanonicalBytes :: ReferenceOracleReceipt -> ByteString
referenceOracleReceiptCanonicalBytes
  (ReferenceOracleReceipt request digest index result) =
    Serialize.encode
      ( ReferenceReceiptTranscript
          referenceReceiptDomain
          (heraldEpochBytes (oracleClientRequestHome request))
          (oracleClientRequestSequence request)
          (oracleCommandDigestBytes digest)
          (controlIndexWord64 index)
          (referenceReceiptResultBytes result)
      )

data ReferenceProjectionEvent
  = ReferenceFailureProbeOpened
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
  | ReferenceFailureProbeReportRecorded
      HeraldFailureProbeId
      HeraldEpoch
      ReferenceProbeResult
  | ReferenceFailureProbeDismissed
      HeraldFailureProbeId
      FailureProbeResolutionId
  | ReferenceHeraldMembershipAdvanced HeraldMembershipGeneration
  | ReferenceFailureProbeRetired
      HeraldFailureProbeId
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | ReferenceFailureProbeSuperseded
      HeraldFailureProbeId
      HeraldMembershipGenerationId
  | ReferenceProcessEpochEnded ProcessEpochId ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

data ReferenceProjectionEventView
  = ReferenceFailureProbeOpenedView
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
  | ReferenceFailureProbeReportRecordedView
      HeraldFailureProbeId
      HeraldEpoch
      ReferenceProbeResult
  | ReferenceFailureProbeDismissedEventView
      HeraldFailureProbeId
      FailureProbeResolutionId
  | ReferenceHeraldMembershipAdvancedView HeraldMembershipGeneration
  | ReferenceFailureProbeRetiredEventView
      HeraldFailureProbeId
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | ReferenceFailureProbeSupersededEventView
      HeraldFailureProbeId
      HeraldMembershipGenerationId
  | ReferenceProcessEpochEndedView ProcessEpochId ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

referenceProjectionEventView ::
  ReferenceProjectionEvent -> ReferenceProjectionEventView
referenceProjectionEventView event = case event of
  ReferenceFailureProbeOpened probe target generation ->
    ReferenceFailureProbeOpenedView probe target generation
  ReferenceFailureProbeReportRecorded probe reporter result ->
    ReferenceFailureProbeReportRecordedView probe reporter result
  ReferenceFailureProbeDismissed probe resolution ->
    ReferenceFailureProbeDismissedEventView probe resolution
  ReferenceHeraldMembershipAdvanced generation ->
    ReferenceHeraldMembershipAdvancedView generation
  ReferenceFailureProbeRetired probe resolution successor ->
    ReferenceFailureProbeRetiredEventView probe resolution successor
  ReferenceFailureProbeSuperseded probe successor ->
    ReferenceFailureProbeSupersededEventView probe successor
  ReferenceProcessEpochEnded process index reason ->
    ReferenceProcessEpochEndedView process index reason

referenceProjectionEventTag :: ReferenceProjectionEvent -> Word8
referenceProjectionEventTag event = case event of
  ReferenceProcessEpochEnded {} -> 1
  ReferenceFailureProbeOpened {} -> 11
  ReferenceFailureProbeReportRecorded {} -> 12
  ReferenceFailureProbeDismissed {} -> 13
  ReferenceHeraldMembershipAdvanced {} -> 14
  ReferenceFailureProbeRetired {} -> 15
  ReferenceFailureProbeSuperseded {} -> 16

data ReferenceAppliedOracleEntry
  = ReferenceAppliedOracleEntry
      ControlIndex
      ReferenceOracleReceipt
      [ReferenceProjectionEvent]
      OracleStateDigest
  deriving stock (Eq, Show)

referenceAppliedEntryControlIndex :: ReferenceAppliedOracleEntry -> ControlIndex
referenceAppliedEntryControlIndex
  (ReferenceAppliedOracleEntry index _ _ _) = index

referenceAppliedEntryReceipt ::
  ReferenceAppliedOracleEntry -> ReferenceOracleReceipt
referenceAppliedEntryReceipt
  (ReferenceAppliedOracleEntry _ receipt _ _) = receipt

referenceAppliedEntryProjectionEvents ::
  ReferenceAppliedOracleEntry -> [ReferenceProjectionEvent]
referenceAppliedEntryProjectionEvents
  (ReferenceAppliedOracleEntry _ _ events _) = events

referenceAppliedEntryPostStateDigest ::
  ReferenceAppliedOracleEntry -> OracleStateDigest
referenceAppliedEntryPostStateDigest
  (ReferenceAppliedOracleEntry _ _ _ digest) = digest

data ReferenceProtocolDisposition
  = ReferenceConflictingOracleRequestId
      OracleClientRequestId
      OracleCommandDigest
      OracleCommandDigest
  deriving stock (Eq, Show)

data ReferenceSubmissionOutcome
  = ReferenceSubmissionCommitted
      ReferenceOracleReceipt
      ReferenceAppliedOracleEntry
  | ReferenceSubmissionDuplicate ReferenceOracleReceipt
  | ReferenceSubmissionProtocolRejected ReferenceProtocolDisposition
  deriving stock (Eq, Show)

-- State --------------------------------------------------------------------

data ReferenceOracleRequestRecord
  = ReferenceOracleRequestRecord
      ReferenceOracleEnvelope
      ReferenceOracleReceipt
  deriving stock (Eq, Show)

data ReferenceOracleState
  = ReferenceOracleState
      CheckedOracleGenesis
      ControlIndex
      (Map OracleClientRequestId ReferenceOracleRequestRecord)
      (Map HeraldMembershipGenerationId HeraldMembershipGeneration)
      HeraldMembershipGeneration
      (Map HeraldFailureProbeId ReferenceFailureProbe)
      (Map ProcessEpochId ReferenceProcessEpochRecord)
      OracleStateDigest
  deriving stock (Eq, Show)

initialReferenceOracle :: CheckedOracleGenesis -> ReferenceOracleState
initialReferenceOracle genesis = refreshStateDigest provisional
  where
    generation = initialMembershipGeneration genesis
    provisional =
      ReferenceOracleState
        genesis
        (controlIndex 0)
        Map.empty
        (Map.singleton (heraldMembershipGenerationId generation) generation)
        generation
        Map.empty
        ( Map.fromList
            (fmap genesisProcessRecord (checkedOracleAppliedBootstraps genesis))
        )
        zeroStateDigest

initialMembershipGeneration ::
  CheckedOracleGenesis -> HeraldMembershipGeneration
initialMembershipGeneration genesis =
  either
    (error . ("checked Oracle genesis membership invariant: " <>) . show)
    id
    ( genesisHeraldMembershipGeneration
        (checkedOracleSystemId genesis)
        active
    )
  where
    active = case NonEmpty.nonEmpty (fmap heraldMemberEpoch (checkedOracleActiveHeralds genesis)) of
      Just members -> members
      Nothing -> error "checked Oracle genesis unexpectedly has no active Herald"

genesisProcessRecord ::
  AppliedProcessBootstrap -> (ProcessEpochId, ReferenceProcessEpochRecord)
genesisProcessRecord bootstrap =
  ( process,
    ReferenceProcessEpochRecord
      (appliedProcessId bootstrap)
      process
      (appliedProcessResidence bootstrap)
      ReferenceProcessLive
  )
  where
    process = appliedProcessEpochId bootstrap

zeroStateDigest :: OracleStateDigest
zeroStateDigest =
  either
    (error . ("zero Step-15 state digest invariant: " <>) . show)
    id
    (mkOracleStateDigest (ByteString.replicate 32 0))

referenceOracleGreatestControlIndex :: ReferenceOracleState -> ControlIndex
referenceOracleGreatestControlIndex
  (ReferenceOracleState _ index _ _ _ _ _ _) = index

referenceOracleStateDigest :: ReferenceOracleState -> OracleStateDigest
referenceOracleStateDigest
  (ReferenceOracleState _ _ _ _ _ _ _ digest) = digest

referenceOracleRequestCount :: ReferenceOracleState -> Int
referenceOracleRequestCount
  (ReferenceOracleState _ _ requests _ _ _ _ _) = Map.size requests

referenceOracleCurrentMembership ::
  ReferenceOracleState -> HeraldMembershipGeneration
referenceOracleCurrentMembership
  (ReferenceOracleState _ _ _ _ generation _ _ _) = generation

referenceOracleMembershipHistory ::
  ReferenceOracleState -> [HeraldMembershipGeneration]
referenceOracleMembershipHistory state =
  lineage (referenceOracleCurrentMembership state)
  where
    history = stateMembershipHistory state
    lineage generation = case heraldMembershipGenerationPredecessor generation of
      Nothing -> [generation]
      Just predecessor ->
        case Map.lookup predecessor history of
          Just previous -> lineage previous <> [generation]
          Nothing ->
            error
              ( "membership lineage invariant: missing predecessor "
                  <> show predecessor
              )

referenceOracleRetiredHeralds :: ReferenceOracleState -> [HeraldEpoch]
referenceOracleRetiredHeralds state =
  [ retired
  | generation <- referenceOracleMembershipHistory state,
    Just retired <- [heraldMembershipGenerationRetiredHeraldEpoch generation]
  ]

referenceOracleFailureProbe ::
  HeraldFailureProbeId ->
  ReferenceOracleState ->
  Maybe ReferenceFailureProbeView
referenceOracleFailureProbe probe state =
  failureProbeView <$> Map.lookup probe (stateProbes state)

referenceOracleActiveProbeFor ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  ReferenceOracleState ->
  Maybe HeraldFailureProbeId
referenceOracleActiveProbeFor target generation state =
  probeIdentifier <$> find isExactOpen (Map.elems (stateProbes state))
  where
    isExactOpen probe =
      probeTarget probe == target
        && probeGeneration probe == generation
        && probeTerminal probe == Nothing

referenceOracleTerminalIntention ::
  HeraldFailureProbeId ->
  ReferenceOracleState ->
  Maybe ReferenceTerminalIntention
referenceOracleTerminalIntention probe state =
  Map.lookup probe (stateProbes state) >>= terminalIntention state

referenceOracleProcessRecord ::
  ProcessEpochId ->
  ReferenceOracleState ->
  Maybe ReferenceProcessEpochRecord
referenceOracleProcessRecord process state =
  Map.lookup process (stateProcesses state)

-- Transition ----------------------------------------------------------------

submitReferenceOracle ::
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  (ReferenceOracleState, ReferenceSubmissionOutcome)
submitReferenceOracle envelope state =
  case Map.lookup requestId (stateRequests state) of
    Just (ReferenceOracleRequestRecord retainedEnvelope receipt)
      | referenceOracleEnvelopeDigest retainedEnvelope == commandDigest ->
          (state, ReferenceSubmissionDuplicate receipt)
      | otherwise ->
          ( state,
            ReferenceSubmissionProtocolRejected
              ( ReferenceConflictingOracleRequestId
                  requestId
                  (referenceOracleEnvelopeDigest retainedEnvelope)
                  commandDigest
              )
          )
    Nothing -> commitFreshEnvelope envelope state
  where
    requestId = referenceOracleEnvelopeRequestId envelope
    commandDigest = referenceOracleEnvelopeDigest envelope

commitFreshEnvelope ::
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  (ReferenceOracleState, ReferenceSubmissionOutcome)
commitFreshEnvelope envelope state =
  (successor, ReferenceSubmissionCommitted receipt entry)
  where
    nextIndex =
      controlIndex
        (controlIndexWord64 (referenceOracleGreatestControlIndex state) + 1)
    (result, semanticSuccessor, events) =
      case applyFreshCommand nextIndex envelope state of
        Left rejection -> (ReferenceOracleRejected rejection, state, [])
        Right (updated, acceptedResult, acceptedEvents) ->
          (ReferenceOracleAccepted acceptedResult, updated, acceptedEvents)
    receipt =
      ReferenceOracleReceipt
        (referenceOracleEnvelopeRequestId envelope)
        (referenceOracleEnvelopeDigest envelope)
        nextIndex
        result
    successor =
      refreshStateDigest
        ( setStateRequestAndIndex
            nextIndex
            (referenceOracleEnvelopeRequestId envelope)
            (ReferenceOracleRequestRecord envelope receipt)
            semanticSuccessor
        )
    entry =
      ReferenceAppliedOracleEntry
        nextIndex
        receipt
        events
        (referenceOracleStateDigest successor)

applyFreshCommand ::
  ControlIndex ->
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  Either
    ReferenceOracleRejection
    ( ReferenceOracleState,
      ReferenceCommandResult,
      [ReferenceProjectionEvent]
    )
applyFreshCommand nextIndex envelope state = do
  validateEnvelopeIdentity envelope state
  case referenceOracleEnvelopeCommand envelope of
    ReferenceOpenHeraldFailureProbe target generation -> do
      validateExpectedControlIndex (envelopeExpectedControlIndex envelope) state
      applyOpen nextIndex target generation state
    ReferenceReportHeraldFailureProbe probe result -> do
      validateExpectedControlIndex (envelopeExpectedControlIndex envelope) state
      applyReport
        probe
        (referenceOracleEnvelopeHome envelope)
        result
        state
    ReferenceDismissHeraldFailureProbe resolution ->
      applyDismiss
        nextIndex
        (referenceOracleEnvelopeHome envelope)
        (envelopeExpectedControlIndex envelope)
        resolution
        state
    ReferenceRetireHeraldEpoch resolution target ->
      applyRetire
        nextIndex
        (referenceOracleEnvelopeHome envelope)
        (envelopeExpectedControlIndex envelope)
        resolution
        target
        state

validateEnvelopeIdentity ::
  ReferenceOracleEnvelope ->
  ReferenceOracleState ->
  Either ReferenceOracleRejection ()
validateEnvelopeIdentity
  (ReferenceOracleEnvelope request _ home _)
  state = do
    let requestHome = oracleClientRequestHome request
    if requestHome == home
      then Right ()
      else Left (ReferenceRequestHomeEpochMismatch requestHome home)
    if Set.member home (currentActiveSet state)
      then Right ()
      else Left (ReferenceInactiveHomeHerald home)

validateExpectedControlIndex ::
  Maybe ControlIndex ->
  ReferenceOracleState ->
  Either ReferenceOracleRejection ()
validateExpectedControlIndex expected state = case expected of
  Nothing -> Right ()
  Just expectedIndex
    | expectedIndex == referenceOracleGreatestControlIndex state -> Right ()
    | otherwise ->
        Left
          ( ReferenceStaleExpectedControlIndex
              expectedIndex
              (referenceOracleGreatestControlIndex state)
          )

envelopeExpectedControlIndex :: ReferenceOracleEnvelope -> Maybe ControlIndex
envelopeExpectedControlIndex
  (ReferenceOracleEnvelope _ expected _ _) = expected

applyOpen ::
  ControlIndex ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  ReferenceOracleState ->
  Either
    ReferenceOracleRejection
    ( ReferenceOracleState,
      ReferenceCommandResult,
      [ReferenceProjectionEvent]
    )
applyOpen nextIndex target suppliedGeneration state = do
  if suppliedGeneration == currentGenerationId
    then Right ()
    else
      Left
        ( ReferenceStaleMembershipGeneration
            suppliedGeneration
            currentGenerationId
        )
  if Set.member target (currentActiveSet state)
    then Right ()
    else Left (ReferenceFailureProbeTargetNotActive target)
  if Set.member target (eligibleReporterSet state)
    then Left (ReferenceFailureProbeTargetHostsVoter target)
    else Right ()
  case referenceOracleActiveProbeFor target suppliedGeneration state of
    Just existing ->
      Right (state, ReferenceProbeOpened existing, [])
    Nothing -> do
      let probe =
            either
              (error . ("positive control index probe invariant: " <>) . show)
              id
              (deriveHeraldFailureProbeId nextIndex)
      let record =
            ReferenceFailureProbe
              probe
              target
              suppliedGeneration
              nextIndex
              Map.empty
              Nothing
          successor = setStateProbes (Map.insert probe record (stateProbes state)) state
      Right
        ( successor,
          ReferenceProbeOpened probe,
          [ReferenceFailureProbeOpened probe target suppliedGeneration]
        )
  where
    currentGeneration = referenceOracleCurrentMembership state
    currentGenerationId = heraldMembershipGenerationId currentGeneration

applyReport ::
  HeraldFailureProbeId ->
  HeraldEpoch ->
  ReferenceProbeResult ->
  ReferenceOracleState ->
  Either
    ReferenceOracleRejection
    ( ReferenceOracleState,
      ReferenceCommandResult,
      [ReferenceProjectionEvent]
    )
applyReport probeId reporter suppliedResult state = do
  record <- lookupProbe probeId state
  case probeTerminal record of
    Just terminal ->
      Left (ReferenceFailureProbeTerminal probeId (probeTerminalView terminal))
    Nothing -> Right ()
  requireEligibleReporter reporter state
  case Map.lookup reporter (probeReports record) of
    Just retained
      | retained == suppliedResult ->
          Right
            ( state,
              ReferenceProbeReportRecorded probeId (terminalIntention state record),
              []
            )
      | otherwise ->
          Left
            ( ReferenceConflictingFailureReport
                probeId
                reporter
                retained
                suppliedResult
            )
    Nothing ->
      let updated =
            setProbeReports
              (Map.insert reporter suppliedResult (probeReports record))
              record
          successor =
            setStateProbes (Map.insert probeId updated (stateProbes state)) state
       in Right
            ( successor,
              ReferenceProbeReportRecorded
                probeId
                (terminalIntention successor updated),
              [ ReferenceFailureProbeReportRecorded
                  probeId
                  reporter
                  suppliedResult
              ]
            )

applyDismiss ::
  ControlIndex ->
  HeraldEpoch ->
  Maybe ControlIndex ->
  FailureProbeResolutionId ->
  ReferenceOracleState ->
  Either
    ReferenceOracleRejection
    ( ReferenceOracleState,
      ReferenceCommandResult,
      [ReferenceProjectionEvent]
    )
applyDismiss nextIndex reporter expectedIndex resolution state = do
  requireEligibleReporter reporter state
  let probeId = resolutionProbeId resolution
  record <- lookupProbe probeId state
  case probeTerminal record of
    Just (ReferenceProbeDismissed retained _)
      | retained == resolution ->
          Right (state, ReferenceProbeDismissedResult retained, [])
    Just terminal ->
      validateExpectedControlIndex expectedIndex state
        >> Left (ReferenceFailureProbeTerminal probeId (probeTerminalView terminal))
    Nothing -> do
      validateExpectedControlIndex expectedIndex state
      requireThreshold probeId DismissFailureProbe record state
      let updated =
            setProbeTerminal
              (Just (ReferenceProbeDismissed resolution nextIndex))
              record
          successor =
            setStateProbes (Map.insert probeId updated (stateProbes state)) state
      Right
        ( successor,
          ReferenceProbeDismissedResult resolution,
          [ReferenceFailureProbeDismissed probeId resolution]
        )

applyRetire ::
  ControlIndex ->
  HeraldEpoch ->
  Maybe ControlIndex ->
  FailureProbeResolutionId ->
  HeraldEpoch ->
  ReferenceOracleState ->
  Either
    ReferenceOracleRejection
    ( ReferenceOracleState,
      ReferenceCommandResult,
      [ReferenceProjectionEvent]
    )
applyRetire nextIndex reporter expectedIndex resolution suppliedTarget state = do
  requireEligibleReporter reporter state
  let probeId = resolutionProbeId resolution
  record <- lookupProbe probeId state
  case probeTerminal record of
    Just (ReferenceProbeRetired retained successor _)
      | retained == resolution && probeTarget record == suppliedTarget ->
          Right
            ( state,
              ReferenceHeraldRetiredResult retained successor,
              []
            )
    Just terminal ->
      validateExpectedControlIndex expectedIndex state
        >> Left (ReferenceFailureProbeTerminal probeId (probeTerminalView terminal))
    Nothing -> do
      validateExpectedControlIndex expectedIndex state
      if probeTarget record == suppliedTarget
        then Right ()
        else
          Left
            ( ReferenceRetirementTargetMismatch
                suppliedTarget
                (probeTarget record)
            )
      requireThreshold probeId RetireFailureProbeTarget record state
      let currentGeneration = referenceOracleCurrentMembership state
          currentGenerationId = heraldMembershipGenerationId currentGeneration
      if probeGeneration record == currentGenerationId
        then Right ()
        else
          Left
            ( ReferenceStaleMembershipGeneration
                (probeGeneration record)
                currentGenerationId
            )
      let successorGeneration =
            either
              (error . ("admitted membership retirement invariant: " <>) . show)
              id
              (retireHeraldMembershipGeneration nextIndex resolution suppliedTarget currentGeneration)
      let successorId = heraldMembershipGenerationId successorGeneration
          (updatedProbes, terminalEvents) =
            terminalizeProbes
              nextIndex
              probeId
              resolution
              (probeGeneration record)
              successorId
              (stateProbes state)
          (updatedProcesses, endEvents) =
            retireResidentProcesses nextIndex suppliedTarget (stateProcesses state)
          successor =
            setStateProcesses updatedProcesses
              . setStateProbes updatedProbes
              . setStateMembership successorGeneration
              $ state
      Right
        ( successor,
          ReferenceHeraldRetiredResult resolution successorId,
          ReferenceHeraldMembershipAdvanced successorGeneration
            : terminalEvents
              <> endEvents
        )

terminalizeProbes ::
  ControlIndex ->
  HeraldFailureProbeId ->
  FailureProbeResolutionId ->
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  Map HeraldFailureProbeId ReferenceFailureProbe ->
  (Map HeraldFailureProbeId ReferenceFailureProbe, [ReferenceProjectionEvent])
terminalizeProbes index authorizer resolution predecessor successor probes =
  foldr terminalize (probes, []) (Map.toAscList probes)
  where
    terminalize (probeId, record) (updated, events)
      | probeTerminal record /= Nothing = (updated, events)
      | probeGeneration record /= predecessor = (updated, events)
      | probeId == authorizer =
          ( Map.insert
              probeId
              ( setProbeTerminal
                  (Just (ReferenceProbeRetired resolution successor index))
                  record
              )
              updated,
            ReferenceFailureProbeRetired probeId resolution successor : events
          )
      | otherwise =
          ( Map.insert
              probeId
              ( setProbeTerminal
                  (Just (ReferenceProbeMembershipSuperseded successor index))
                  record
              )
              updated,
            ReferenceFailureProbeSuperseded probeId successor : events
          )

retireResidentProcesses ::
  ControlIndex ->
  HeraldEpoch ->
  Map ProcessEpochId ReferenceProcessEpochRecord ->
  ( Map ProcessEpochId ReferenceProcessEpochRecord,
    [ReferenceProjectionEvent]
  )
retireResidentProcesses index target processes =
  foldr retireOne (processes, []) (Map.toAscList processes)
  where
    retireOne (process, record) (updated, events)
      | referenceProcessRecordResidence record /= target = (updated, events)
      | referenceProcessRecordLifecycle record /= ReferenceProcessLiveView =
          (updated, events)
      | otherwise =
          ( Map.insert process (endProcessRecord index record) updated,
            ReferenceProcessEpochEnded process index HeraldRetired : events
          )

endProcessRecord ::
  ControlIndex -> ReferenceProcessEpochRecord -> ReferenceProcessEpochRecord
endProcessRecord
  index
  (ReferenceProcessEpochRecord processId process residence _) =
    ReferenceProcessEpochRecord
      processId
      process
      residence
      (ReferenceProcessEnded index HeraldRetired)

lookupProbe ::
  HeraldFailureProbeId ->
  ReferenceOracleState ->
  Either ReferenceOracleRejection ReferenceFailureProbe
lookupProbe probe state =
  maybe
    (Left (ReferenceUnknownFailureProbe probe))
    Right
    (Map.lookup probe (stateProbes state))

requireEligibleReporter ::
  HeraldEpoch ->
  ReferenceOracleState ->
  Either ReferenceOracleRejection ()
requireEligibleReporter reporter state
  | Set.member reporter (eligibleReporterSet state) = Right ()
  | otherwise = Left (ReferenceIneligibleFailureReporter reporter)

requireThreshold ::
  HeraldFailureProbeId ->
  FailureProbeResolution ->
  ReferenceFailureProbe ->
  ReferenceOracleState ->
  Either ReferenceOracleRejection ()
requireThreshold probe disposition record state
  | hasStrictMajority disposition record state = Right ()
  | otherwise =
      Left (ReferenceFailureProbeThresholdNotReached probe disposition)

terminalIntention ::
  ReferenceOracleState ->
  ReferenceFailureProbe ->
  Maybe ReferenceTerminalIntention
terminalIntention state record
  | probeTerminal record /= Nothing = Nothing
  | hasStrictMajority DismissFailureProbe record state =
      Just (mkTerminalIntention record DismissFailureProbe)
  | hasStrictMajority RetireFailureProbeTarget record state =
      Just (mkTerminalIntention record RetireFailureProbeTarget)
  | otherwise = Nothing

mkTerminalIntention ::
  ReferenceFailureProbe ->
  FailureProbeResolution ->
  ReferenceTerminalIntention
mkTerminalIntention record disposition =
  ReferenceTerminalIntention resolution command (referenceOracleCommandDigest command)
  where
    resolution = deriveFailureProbeResolutionId (probeIdentifier record) disposition
    command = case disposition of
      DismissFailureProbe -> ReferenceDismissHeraldFailureProbe resolution
      RetireFailureProbeTarget ->
        ReferenceRetireHeraldEpoch resolution (probeTarget record)

hasStrictMajority ::
  FailureProbeResolution ->
  ReferenceFailureProbe ->
  ReferenceOracleState ->
  Bool
hasStrictMajority disposition record state =
  2 * matching > Set.size eligible
  where
    eligible = eligibleReporterSet state
    expected = case disposition of
      DismissFailureProbe -> ReferenceProbeReachable
      RetireFailureProbeTarget -> ReferenceProbeUnreachable
    matching =
      length
        [ ()
        | (reporter, result) <- Map.toList (probeReports record),
          Set.member reporter eligible,
          result == expected
        ]

resolutionProbeId :: FailureProbeResolutionId -> HeraldFailureProbeId
resolutionProbeId = failureProbeResolutionProbeId

currentActiveSet :: ReferenceOracleState -> Set HeraldEpoch
currentActiveSet =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . referenceOracleCurrentMembership

eligibleReporterSet :: ReferenceOracleState -> Set HeraldEpoch
eligibleReporterSet =
  Set.fromList
    . fmap raftVoterBindingHeraldEpoch
    . checkedOracleRaftVoterBindings
    . stateGenesis

probeIdentifier :: ReferenceFailureProbe -> HeraldFailureProbeId
probeIdentifier (ReferenceFailureProbe value _ _ _ _ _) = value

probeTarget :: ReferenceFailureProbe -> HeraldEpoch
probeTarget (ReferenceFailureProbe _ target _ _ _ _) = target

probeGeneration :: ReferenceFailureProbe -> HeraldMembershipGenerationId
probeGeneration (ReferenceFailureProbe _ _ generation _ _ _) = generation

probeReports ::
  ReferenceFailureProbe -> Map HeraldEpoch ReferenceProbeResult
probeReports (ReferenceFailureProbe _ _ _ _ reports _) = reports

probeTerminal :: ReferenceFailureProbe -> Maybe ReferenceProbeTerminal
probeTerminal (ReferenceFailureProbe _ _ _ _ _ terminal) = terminal

setProbeReports ::
  Map HeraldEpoch ReferenceProbeResult ->
  ReferenceFailureProbe ->
  ReferenceFailureProbe
setProbeReports
  reports
  (ReferenceFailureProbe probe target generation opened _ terminal) =
    ReferenceFailureProbe probe target generation opened reports terminal

setProbeTerminal ::
  Maybe ReferenceProbeTerminal ->
  ReferenceFailureProbe ->
  ReferenceFailureProbe
setProbeTerminal
  terminal
  (ReferenceFailureProbe probe target generation opened reports _) =
    ReferenceFailureProbe probe target generation opened reports terminal

stateGenesis :: ReferenceOracleState -> CheckedOracleGenesis
stateGenesis (ReferenceOracleState genesis _ _ _ _ _ _ _) = genesis

stateRequests ::
  ReferenceOracleState -> Map OracleClientRequestId ReferenceOracleRequestRecord
stateRequests (ReferenceOracleState _ _ requests _ _ _ _ _) = requests

stateProbes ::
  ReferenceOracleState -> Map HeraldFailureProbeId ReferenceFailureProbe
stateProbes (ReferenceOracleState _ _ _ _ _ probes _ _) = probes

stateMembershipHistory ::
  ReferenceOracleState ->
  Map HeraldMembershipGenerationId HeraldMembershipGeneration
stateMembershipHistory
  (ReferenceOracleState _ _ _ history _ _ _ _) = history

stateProcesses ::
  ReferenceOracleState -> Map ProcessEpochId ReferenceProcessEpochRecord
stateProcesses (ReferenceOracleState _ _ _ _ _ _ processes _) = processes

setStateRequestAndIndex ::
  ControlIndex ->
  OracleClientRequestId ->
  ReferenceOracleRequestRecord ->
  ReferenceOracleState ->
  ReferenceOracleState
setStateRequestAndIndex
  index
  request
  record
  (ReferenceOracleState genesis _ requests history current probes processes digest) =
    ReferenceOracleState
      genesis
      index
      (Map.insert request record requests)
      history
      current
      probes
      processes
      digest

setStateMembership ::
  HeraldMembershipGeneration ->
  ReferenceOracleState ->
  ReferenceOracleState
setStateMembership
  generation
  (ReferenceOracleState genesis index requests history _ probes processes digest) =
    ReferenceOracleState
      genesis
      index
      requests
      (Map.insert (heraldMembershipGenerationId generation) generation history)
      generation
      probes
      processes
      digest

setStateProbes ::
  Map HeraldFailureProbeId ReferenceFailureProbe ->
  ReferenceOracleState ->
  ReferenceOracleState
setStateProbes
  probes
  (ReferenceOracleState genesis index requests history current _ processes digest) =
    ReferenceOracleState genesis index requests history current probes processes digest

setStateProcesses ::
  Map ProcessEpochId ReferenceProcessEpochRecord ->
  ReferenceOracleState ->
  ReferenceOracleState
setStateProcesses
  processes
  (ReferenceOracleState genesis index requests history current probes _ digest) =
    ReferenceOracleState genesis index requests history current probes processes digest

setStateDigest :: OracleStateDigest -> ReferenceOracleState -> ReferenceOracleState
setStateDigest
  digest
  (ReferenceOracleState genesis index requests history current probes processes _) =
    ReferenceOracleState genesis index requests history current probes processes digest

-- Canonical state -----------------------------------------------------------

refreshStateDigest :: ReferenceOracleState -> ReferenceOracleState
refreshStateDigest state =
  setStateDigest
    ( stateDigestInvariant
        (SHA256.hash (referenceOracleStateCanonicalBytes state))
    )
    state

referenceOracleStateCanonicalBytes :: ReferenceOracleState -> ByteString
referenceOracleStateCanonicalBytes state =
  Serialize.encode
    ( ReferenceStateTranscript
        referenceStateDomain
        (referenceGenesisDigest (stateGenesis state))
        (controlIndexWord64 (referenceOracleGreatestControlIndex state))
        (fmap requestTranscript (requestsByIndex state))
        ( fmap
            heraldMembershipGenerationCanonicalBytes
            (referenceOracleMembershipHistory state)
        )
        ( heraldMembershipGenerationCanonicalBytes
            (referenceOracleCurrentMembership state)
        )
        (fmap failureProbeTranscript (Map.elems (stateProbes state)))
        (fmap processTranscript (Map.elems (stateProcesses state)))
    )

data ReferenceStateCanonicalProblem
  = ReferenceStateCanonicalDecodeFailed String
  | ReferenceStateCanonicalWrongDomain ByteString
  | ReferenceStateCanonicalGenesisMismatch
  | ReferenceStateCanonicalNonContiguousControlIndex Word64 Word64
  | ReferenceStateCanonicalInvalidEnvelope
      Word64
      ReferenceEnvelopeCanonicalProblem
  | ReferenceStateCanonicalReplayDidNotCommit Word64
  | ReferenceStateCanonicalReceiptMismatch Word64
  | ReferenceStateCanonicalGreatestIndexMismatch Word64 Word64
  | ReferenceStateCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeReferenceOracleStateCanonicalBytes ::
  CheckedOracleGenesis ->
  ByteString ->
  Either ReferenceStateCanonicalProblem ReferenceOracleState
decodeReferenceOracleStateCanonicalBytes genesis bytes = do
  ReferenceStateTranscript domain suppliedGenesis suppliedGreatest requests _ _ _ _ <-
    decodeCanonical ReferenceStateCanonicalDecodeFailed bytes
  if domain == referenceStateDomain
    then Right ()
    else Left (ReferenceStateCanonicalWrongDomain domain)
  if suppliedGenesis == referenceGenesisDigest genesis
    then Right ()
    else Left ReferenceStateCanonicalGenesisMismatch
  replayed <- replayRequests (initialReferenceOracle genesis) requests
  let observedGreatest =
        controlIndexWord64 (referenceOracleGreatestControlIndex replayed)
  if suppliedGreatest == observedGreatest
    then Right ()
    else
      Left
        ( ReferenceStateCanonicalGreatestIndexMismatch
            suppliedGreatest
            observedGreatest
        )
  if referenceOracleStateCanonicalBytes replayed == bytes
    then Right replayed
    else Left ReferenceStateCanonicalNonCanonical

replayRequests ::
  ReferenceOracleState ->
  [ReferenceStateRequestTranscript] ->
  Either ReferenceStateCanonicalProblem ReferenceOracleState
replayRequests = go 1
  where
    go _ state [] = Right state
    go expected state (ReferenceStateRequestTranscript supplied envelopeBytes receiptBytes : rest) = do
      if supplied == expected
        then Right ()
        else
          Left
            ( ReferenceStateCanonicalNonContiguousControlIndex
                expected
                supplied
            )
      envelope <-
        either
          (Left . ReferenceStateCanonicalInvalidEnvelope supplied)
          Right
          (decodeReferenceOracleEnvelopeCanonicalBytes envelopeBytes)
      let (successor, outcome) = submitReferenceOracle envelope state
      receipt <- case outcome of
        ReferenceSubmissionCommitted committed _ -> Right committed
        _ -> Left (ReferenceStateCanonicalReplayDidNotCommit supplied)
      if referenceOracleReceiptCanonicalBytes receipt == receiptBytes
        then go (expected + 1) successor rest
        else Left (ReferenceStateCanonicalReceiptMismatch supplied)

requestsByIndex ::
  ReferenceOracleState -> [ReferenceOracleRequestRecord]
requestsByIndex =
  sortOn (referenceOracleReceiptControlIndex . requestRecordReceipt)
    . Map.elems
    . stateRequests

requestRecordReceipt :: ReferenceOracleRequestRecord -> ReferenceOracleReceipt
requestRecordReceipt (ReferenceOracleRequestRecord _ receipt) = receipt

requestTranscript ::
  ReferenceOracleRequestRecord -> ReferenceStateRequestTranscript
requestTranscript (ReferenceOracleRequestRecord envelope receipt) =
  ReferenceStateRequestTranscript
    (controlIndexWord64 (referenceOracleReceiptControlIndex receipt))
    (referenceOracleEnvelopeCanonicalBytes envelope)
    (referenceOracleReceiptCanonicalBytes receipt)

failureProbeTranscript :: ReferenceFailureProbe -> ByteString
failureProbeTranscript
  (ReferenceFailureProbe probe target generation opened reports terminal) =
    Serialize.encode
      ( ReferenceFailureProbeTranscript
          (heraldFailureProbeIdCanonicalBytes probe)
          (heraldEpochBytes target)
          (heraldMembershipGenerationIdBytes generation)
          (controlIndexWord64 opened)
          [ (heraldEpochBytes reporter, probeResultTag result)
          | (reporter, result) <- Map.toAscList reports
          ]
          (probeTerminalTranscript <$> terminal)
      )

probeTerminalTranscript :: ReferenceProbeTerminal -> ReferenceProbeTerminalTranscript
probeTerminalTranscript terminal = case terminal of
  ReferenceProbeDismissed resolution index ->
    ReferenceProbeTerminalTranscript
      0
      (Just (failureProbeResolutionIdCanonicalBytes resolution))
      Nothing
      (controlIndexWord64 index)
  ReferenceProbeRetired resolution successor index ->
    ReferenceProbeTerminalTranscript
      1
      (Just (failureProbeResolutionIdCanonicalBytes resolution))
      (Just (heraldMembershipGenerationIdBytes successor))
      (controlIndexWord64 index)
  ReferenceProbeMembershipSuperseded successor index ->
    ReferenceProbeTerminalTranscript
      2
      Nothing
      (Just (heraldMembershipGenerationIdBytes successor))
      (controlIndexWord64 index)

processTranscript :: ReferenceProcessEpochRecord -> ByteString
processTranscript
  (ReferenceProcessEpochRecord processId process residence lifecycle) =
    Serialize.encode
      ( ReferenceProcessTranscript
          (processIdBytes processId)
          (processEpochIdBytes process)
          (heraldEpochBytes residence)
          (processLifecycleTranscript lifecycle)
      )

processLifecycleTranscript ::
  ReferenceProcessLifecycle -> ReferenceProcessLifecycleTranscript
processLifecycleTranscript lifecycle = case lifecycle of
  ReferenceProcessLive ->
    ReferenceProcessLifecycleTranscript 0 Nothing Nothing
  ReferenceProcessEnded index reason ->
    ReferenceProcessLifecycleTranscript
      1
      (Just (controlIndexWord64 index))
      (Just (processEndReasonCanonicalBytes reason))

referenceGenesisDigest :: CheckedOracleGenesis -> ByteString
referenceGenesisDigest =
  SHA256.hash . oracleStateCanonicalBytes . initialOracleState

-- Canonical receipt details -------------------------------------------------

referenceReceiptResultBytes :: ReferenceOracleReceiptResult -> ByteString
referenceReceiptResultBytes result = SerializePut.runPut $ case result of
  ReferenceOracleAccepted accepted -> do
    SerializePut.putWord8 0
    putCommandResult accepted
  ReferenceOracleRejected rejection -> do
    SerializePut.putWord8 1
    putRejection rejection

putCommandResult :: ReferenceCommandResult -> SerializePut.Put
putCommandResult result = case result of
  ReferenceProbeOpened probe -> do
    SerializePut.putWord8 0
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
  ReferenceProbeReportRecorded probe intention -> do
    SerializePut.putWord8 1
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    case intention of
      Nothing -> SerializePut.putWord8 0
      Just terminal -> do
        SerializePut.putWord8 1
        putFramedBytes
          ( failureProbeResolutionIdCanonicalBytes
              (referenceTerminalIntentionResolutionId terminal)
          )
        SerializePut.putByteString
          ( oracleCommandDigestBytes
              (referenceTerminalIntentionCommandDigest terminal)
          )
  ReferenceProbeDismissedResult resolution -> do
    SerializePut.putWord8 2
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
  ReferenceHeraldRetiredResult resolution successor -> do
    SerializePut.putWord8 3
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)

putRejection :: ReferenceOracleRejection -> SerializePut.Put
putRejection rejection = case rejection of
  ReferenceRequestHomeEpochMismatch expected supplied -> do
    tag 0
    putHerald expected
    putHerald supplied
  ReferenceInactiveHomeHerald home -> tag 1 >> putHerald home
  ReferenceStaleExpectedControlIndex expected observed -> do
    tag 2
    putIndex expected
    putIndex observed
  ReferenceStaleMembershipGeneration supplied expected -> do
    tag 42
    putGeneration supplied
    putGeneration expected
  ReferenceFailureProbeTargetNotActive target -> tag 43 >> putHerald target
  ReferenceFailureProbeTargetHostsVoter target -> tag 44 >> putHerald target
  ReferenceUnknownFailureProbe probe -> tag 45 >> putProbe probe
  ReferenceFailureProbeTerminal probe terminal -> do
    tag 46
    putProbe probe
    putTerminalView terminal
  ReferenceIneligibleFailureReporter reporter -> tag 47 >> putHerald reporter
  ReferenceConflictingFailureReport probe reporter retained supplied -> do
    tag 48
    putProbe probe
    putHerald reporter
    SerializePut.putWord8 (probeResultTag retained)
    SerializePut.putWord8 (probeResultTag supplied)
  ReferenceFailureProbeThresholdNotReached probe disposition -> do
    tag 49
    putProbe probe
    SerializePut.putWord8 (resolutionTag disposition)
  ReferenceRetirementTargetMismatch supplied expected -> do
    tag 50
    putHerald supplied
    putHerald expected
  where
    tag = SerializePut.putWord8
    putHerald = SerializePut.putByteString . heraldEpochBytes
    putProbe = putFramedBytes . heraldFailureProbeIdCanonicalBytes
    putGeneration = SerializePut.putByteString . heraldMembershipGenerationIdBytes
    putIndex = SerializePut.putWord64be . controlIndexWord64

putTerminalView :: ReferenceProbeTerminalView -> SerializePut.Put
putTerminalView terminal = case terminal of
  ReferenceProbeDismissedView resolution index -> do
    SerializePut.putWord8 0
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putWord64be (controlIndexWord64 index)
  ReferenceProbeRetiredView resolution successor index -> do
    SerializePut.putWord8 1
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
    SerializePut.putWord64be (controlIndexWord64 index)
  ReferenceProbeMembershipSupersededView successor index -> do
    SerializePut.putWord8 2
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
    SerializePut.putWord64be (controlIndexWord64 index)

resolutionTag :: FailureProbeResolution -> Word8
resolutionTag DismissFailureProbe = 0
resolutionTag RetireFailureProbeTarget = 1

putFramedBytes :: ByteString -> SerializePut.Put
putFramedBytes bytes = do
  SerializePut.putWord64be (fromIntegral (ByteString.length bytes))
  SerializePut.putByteString bytes

commandDigestInvariant :: ByteString -> OracleCommandDigest
commandDigestInvariant =
  either (error . ("Step-15 command digest invariant: " <>) . show) id
    . mkOracleCommandDigest

stateDigestInvariant :: ByteString -> OracleStateDigest
stateDigestInvariant =
  either (error . ("Step-15 state digest invariant: " <>) . show) id
    . mkOracleStateDigest

decodeCanonical ::
  (Serialize value) =>
  (String -> problem) ->
  ByteString ->
  Either problem value
decodeCanonical failure = either (Left . failure) Right . Serialize.decode

decodeReferenceCommandPayload ::
  (Serialize value) =>
  ByteString ->
  Either ReferenceCommandCanonicalProblem value
decodeReferenceCommandPayload =
  decodeCanonical ReferenceCommandCanonicalPayloadDecodeFailed

-- Raw canonical transcripts -------------------------------------------------

data ReferenceCommandTranscript
  = ReferenceCommandTranscript ByteString Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceOpenPayload
  = ReferenceOpenPayload ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceReportPayload
  = ReferenceReportPayload ByteString Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

newtype ReferenceDismissPayload
  = ReferenceDismissPayload ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceRetirePayload
  = ReferenceRetirePayload ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceEnvelopeTranscript
  = ReferenceEnvelopeTranscript
      ByteString
      ByteString
      Word64
      (Maybe Word64)
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceReceiptTranscript
  = ReferenceReceiptTranscript
      ByteString
      ByteString
      Word64
      ByteString
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceStateTranscript
  = ReferenceStateTranscript
      ByteString
      ByteString
      Word64
      [ReferenceStateRequestTranscript]
      [ByteString]
      ByteString
      [ByteString]
      [ByteString]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceStateRequestTranscript
  = ReferenceStateRequestTranscript Word64 ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceFailureProbeTranscript
  = ReferenceFailureProbeTranscript
      ByteString
      ByteString
      ByteString
      Word64
      [(ByteString, Word8)]
      (Maybe ReferenceProbeTerminalTranscript)
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceProbeTerminalTranscript
  = ReferenceProbeTerminalTranscript
      Word8
      (Maybe ByteString)
      (Maybe ByteString)
      Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceProcessTranscript
  = ReferenceProcessTranscript
      ByteString
      ByteString
      ByteString
      ReferenceProcessLifecycleTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceProcessLifecycleTranscript
  = ReferenceProcessLifecycleTranscript
      Word8
      (Maybe Word64)
      (Maybe ByteString)
  deriving stock (Generic)
  deriving anyclass (Serialize)

referenceCommandDomain,
  referenceEnvelopeDomain,
  referenceReceiptDomain,
  referenceStateDomain ::
    ByteString
referenceCommandDomain = "ECLIPS-STEP15-ORACLE-COMMAND"
referenceEnvelopeDomain = "ECLIPS-STEP15-ORACLE-ENVELOPE"
referenceReceiptDomain = "ECLIPS-STEP15-ORACLE-RECEIPT"
referenceStateDomain = "ECLIPS-STEP15-ORACLE-STATE"
