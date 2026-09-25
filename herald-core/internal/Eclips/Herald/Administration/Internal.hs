{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Package-private constructors for administration and drain correlations.
module Eclips.Herald.Administration.Internal
  ( AdministrationBinding (..),
    administrationBinding,
    administrationBindingHeraldEpoch,
    administrationBindingGeneration,
    AdminBindingGeneration (..),
    adminBindingGenerationWord64,
    initialAdministrationBinding,
    AdminCorrelationId (..),
    adminCorrelationId,
    adminCorrelationIdForBinding,
    adminCorrelationBelongsTo,
    adminCorrelationIdWord64,
    StartProcessRequest,
    startProcessRequest,
    startProcessCorrelation,
    ProcessStartRequestDigest,
    processStartRequestDigestBytes,
    startProcessRequestDigest,
    EndProcessEpochRequest,
    endProcessEpochRequest,
    endProcessEpochCorrelation,
    endProcessEpochProcess,
    endProcessEpochReason,
    EndProcessRequestDigest,
    endProcessRequestDigestBytes,
    endProcessEpochRequestDigest,
    AdministrationCommand (..),
    administrationCommandCorrelation,
    AdministrationCommandDigest,
    administrationCommandDigestBytes,
    administrationCommandDigest,
    ConfiguredStartAcceptancePosition,
    configuredStartAcceptancePosition,
    configuredStartAcceptancePositionWord64,
    AdminError (..),
    AdminResultStatus (..),
    AdminReply (..),
    AdminHeraldStatus (..),
    AdminServicePhase (..),
    AdminPreparationInspection (..),
    AdminPreparationPhase (..),
    DrainId (..),
    drainId,
    drainIdWord64,
    FinalAdminReply (..),
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (ChildPreparation, LifecycleStatus, StartupError, childPreparationOrdinal, childPreparationScopeBytes, childPreparationSessionOrdinal)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
    SystemId,
    processEpochIdBytes,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason,
    processEndReasonCanonicalBytes,
  )
import Eclips.Herald.Application.Session (ApplicationAttachment)
import Eclips.Herald.OracleClient (OracleNodeClaim)
import Eclips.Oracle.Canonical (CanonicalOracleReceipt)
import Eclips.Oracle.Receipt (OracleRejection)
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterChange, VoterConfiguration)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GHC.Generics (Generic)

newtype AdminBindingGeneration = AdminBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

adminBindingGenerationWord64 :: AdminBindingGeneration -> Word64
adminBindingGenerationWord64 (AdminBindingGeneration generation) = generation

data AdministrationBinding
  = AdministrationBinding HeraldEpoch AdminBindingGeneration
  deriving stock (Eq, Ord, Show)

administrationBinding ::
  HeraldEpoch ->
  AdminBindingGeneration ->
  AdministrationBinding
administrationBinding = AdministrationBinding

administrationBindingHeraldEpoch :: AdministrationBinding -> HeraldEpoch
administrationBindingHeraldEpoch (AdministrationBinding herald _) = herald

administrationBindingGeneration ::
  AdministrationBinding ->
  AdminBindingGeneration
administrationBindingGeneration (AdministrationBinding _ generation) = generation

initialAdministrationBinding :: HeraldEpoch -> AdministrationBinding
initialAdministrationBinding herald =
  AdministrationBinding herald (AdminBindingGeneration 0)

data AdminCorrelationId = AdminCorrelationId AdminBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

adminCorrelationId :: Word64 -> AdminCorrelationId
adminCorrelationId = AdminCorrelationId (AdminBindingGeneration 0)

-- | A caller's ordinal belongs only to this established TCP lifetime.
adminCorrelationIdForBinding :: AdministrationBinding -> Word64 -> AdminCorrelationId
adminCorrelationIdForBinding binding = AdminCorrelationId (administrationBindingGeneration binding)

adminCorrelationBelongsTo :: AdministrationBinding -> AdminCorrelationId -> Bool
adminCorrelationBelongsTo binding (AdminCorrelationId generation _) = administrationBindingGeneration binding == generation

adminCorrelationTranscript :: AdminCorrelationId -> (Word64, Word64)
adminCorrelationTranscript (AdminCorrelationId generation ordinal) = (adminBindingGenerationWord64 generation, ordinal)

adminCorrelationIdWord64 :: AdminCorrelationId -> Word64
adminCorrelationIdWord64 (AdminCorrelationId _ correlation) = correlation

-- | A stable target-local lifecycle request. The resident generator supplies
-- fresh identities exactly once after admission; callers supply no future manifest.
data StartProcessRequest
  = StartProcessRequest AdminCorrelationId
  deriving stock (Eq, Ord, Show)

startProcessRequest ::
  AdminCorrelationId ->
  StartProcessRequest
startProcessRequest = StartProcessRequest

startProcessCorrelation ::
  StartProcessRequest ->
  AdminCorrelationId
startProcessCorrelation (StartProcessRequest correlation) = correlation

newtype ProcessStartRequestDigest
  = ProcessStartRequestDigest ByteString
  deriving stock (Eq, Ord)

instance Show ProcessStartRequestDigest where
  show = renderGroupedHex . processStartRequestDigestBytes

processStartRequestDigestBytes ::
  ProcessStartRequestDigest ->
  ByteString
processStartRequestDigestBytes (ProcessStartRequestDigest bytes) = bytes

data ConfiguredProcessRequestTranscript
  = ConfiguredProcessRequestTranscript ByteString (Word64, Word64)
  deriving stock (Generic)
  deriving anyclass (Serialize)

startProcessRequestDigest ::
  StartProcessRequest ->
  ProcessStartRequestDigest
startProcessRequestDigest request =
  ProcessStartRequestDigest
    ( SHA256.hash
        ( encode
            ( ConfiguredProcessRequestTranscript
                "ECLIPS-ADMIN-START-PROCESS"
                (adminCorrelationTranscript (startProcessCorrelation request))
            )
        )
    )

-- | One explicit process-End administration call. The closed reason is part
-- of exact retry identity; connection state is deliberately absent.
data EndProcessEpochRequest
  = EndProcessEpochRequest AdminCorrelationId ProcessEpochId ProcessEndReason
  deriving stock (Eq, Ord, Show)

endProcessEpochRequest ::
  AdminCorrelationId ->
  ProcessEpochId ->
  ProcessEndReason ->
  EndProcessEpochRequest
endProcessEpochRequest = EndProcessEpochRequest

endProcessEpochCorrelation :: EndProcessEpochRequest -> AdminCorrelationId
endProcessEpochCorrelation (EndProcessEpochRequest correlation _ _) = correlation

endProcessEpochProcess :: EndProcessEpochRequest -> ProcessEpochId
endProcessEpochProcess (EndProcessEpochRequest _ process _) = process

endProcessEpochReason :: EndProcessEpochRequest -> ProcessEndReason
endProcessEpochReason (EndProcessEpochRequest _ _ reason) = reason

newtype EndProcessRequestDigest = EndProcessRequestDigest ByteString
  deriving stock (Eq, Ord)

instance Show EndProcessRequestDigest where
  show = renderGroupedHex . endProcessRequestDigestBytes

endProcessRequestDigestBytes :: EndProcessRequestDigest -> ByteString
endProcessRequestDigestBytes (EndProcessRequestDigest bytes) = bytes

data EndProcessRequestTranscript
  = EndProcessRequestTranscript ByteString (Word64, Word64) ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

endProcessEpochRequestDigest :: EndProcessEpochRequest -> EndProcessRequestDigest
endProcessEpochRequestDigest request =
  EndProcessRequestDigest
    ( SHA256.hash
        ( encode
            ( EndProcessRequestTranscript
                "ECLIPS-ADMIN-END-PROCESS-EPOCH"
                (adminCorrelationTranscript (endProcessEpochCorrelation request))
                (processEpochIdBytes (endProcessEpochProcess request))
                (processEndReasonCanonicalBytes (endProcessEpochReason request))
            )
        )
    )

data AdministrationCommand
  = StartAdministrationCommand StartProcessRequest
  | EndAdministrationCommand EndProcessEpochRequest
  | CancelPreparationAdministrationCommand AdminCorrelationId ChildPreparation
  | VoterAdministrationCommand AdminCorrelationId
  deriving stock (Eq, Ord, Show)

administrationCommandCorrelation :: AdministrationCommand -> AdminCorrelationId
administrationCommandCorrelation command = case command of
  StartAdministrationCommand request -> startProcessCorrelation request
  EndAdministrationCommand request -> endProcessEpochCorrelation request
  CancelPreparationAdministrationCommand correlation _ -> correlation
  VoterAdministrationCommand correlation -> correlation

newtype AdministrationCommandDigest = AdministrationCommandDigest ByteString
  deriving stock (Eq, Ord)

instance Show AdministrationCommandDigest where
  show = renderGroupedHex . administrationCommandDigestBytes

administrationCommandDigestBytes :: AdministrationCommandDigest -> ByteString
administrationCommandDigestBytes (AdministrationCommandDigest bytes) = bytes

administrationCommandDigest :: AdministrationCommand -> AdministrationCommandDigest
administrationCommandDigest command =
  AdministrationCommandDigest $ case command of
    StartAdministrationCommand request ->
      processStartRequestDigestBytes (startProcessRequestDigest request)
    EndAdministrationCommand request ->
      endProcessRequestDigestBytes (endProcessEpochRequestDigest request)
    VoterAdministrationCommand correlation ->
      SHA256.hash (encode (("ECLIPS-ADMIN-ORACLE-VOTER" :: ByteString), adminCorrelationTranscript correlation))
    CancelPreparationAdministrationCommand correlation preparation ->
      SHA256.hash (encode (("ECLIPS-ADMIN-CANCEL-PREPARATION" :: ByteString), adminCorrelationTranscript correlation, childPreparationScopeBytes preparation, childPreparationSessionOrdinal preparation, childPreparationOrdinal preparation))

newtype ConfiguredStartAcceptancePosition
  = ConfiguredStartAcceptancePosition Word64
  deriving stock (Eq, Ord, Show)

configuredStartAcceptancePosition :: Word64 -> ConfiguredStartAcceptancePosition
configuredStartAcceptancePosition = ConfiguredStartAcceptancePosition

configuredStartAcceptancePositionWord64 ::
  ConfiguredStartAcceptancePosition ->
  Word64
configuredStartAcceptancePositionWord64 (ConfiguredStartAcceptancePosition value) = value

data AdminError
  = AdminOracleReplicaEndpointsNotConfigured
  | AdminStartProcessNotApplied ControlIndex OracleRejection
  | AdminEndProcessNotApplied ControlIndex OracleRejection
  deriving stock (Eq, Show)

data AdminResultStatus
  = AdminRequestAccepted
  | AdminStartRequestCompleted ProcessEpochId ApplicationAttachment
  | AdminStartRequestEndedBeforeAttachment ProcessEpochId
  | AdminEndRequestCompleted ProcessEpochId
  | AdminRequestRejected AdminError
  | AdminPreparationCancellation LifecycleStatus
  | AdminOracleVoterResult CanonicalOracleReceipt
  deriving stock (Eq, Show)

data AdminServicePhase = AdminServing | AdminDraining | AdminStopped | AdminIsolationDrain | AdminIsolated
  deriving stock (Eq, Show)

data AdminHeraldStatus
  = AdminHeraldStatus
      SystemId
      HeraldEpoch
      [HeraldEpoch]
      [HeraldEpoch]
      HeraldMembershipGenerationId
      ControlIndex
      AdminServicePhase
      (Maybe OracleNodeClaim)
      (Maybe OracleNodeClaim)
  deriving stock (Eq, Show)

data AdminPreparationPhase = AdminPreparationPreparing | AdminPreparationPrepared | AdminPreparationAttached | AdminPreparationCancelling | AdminPreparationTerminal
  deriving stock (Eq, Show)

data AdminPreparationInspection = AdminPreparationInspection ChildPreparation (Maybe ProcessEpochId) AdminPreparationPhase (Maybe StartupError)
  deriving stock (Eq, Show)

data AdminReply
  = AdminHeraldStatusReply AdminCorrelationId AdminHeraldStatus
  | AdminOracleConfigurationReply AdminCorrelationId ControlIndex (Maybe VoterConfiguration) [OracleReplicaRegistration] (Maybe VoterChange)
  | AdminVoterChangeStatusReply AdminCorrelationId ControlIndex (Maybe VoterChange)
  | AdminPreparationsReply AdminCorrelationId [AdminPreparationInspection]
  | AdminDrainAccepted AdminCorrelationId
  | RetainedAdminResult AdminCorrelationId AdminResultStatus
  | AbsentAdminResult AdminCorrelationId
  | ConflictingAdminResult AdminCorrelationId
  | RetiredAdminResult AdminCorrelationId ReceiptRetirement
  | AdministrationReceiptsRetired ReceiptRetirement
  | AdministrationReceiptRetirementNotReady
  deriving stock (Eq, Show)

newtype DrainId = DrainId Word64
  deriving stock (Eq, Ord, Show)

drainId :: Word64 -> DrainId
drainId = DrainId

drainIdWord64 :: DrainId -> Word64
drainIdWord64 (DrainId value) = value

data FinalAdminReply
  = OrderlyHeraldShutdownCompleted AdminCorrelationId
  | ConfiguredHeraldShutdownCompleted AdministrationBinding AdminCorrelationId
  deriving stock (Eq, Show)
