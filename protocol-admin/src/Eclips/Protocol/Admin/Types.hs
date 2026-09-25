{-# LANGUAGE DeriveAnyClass #-}

-- | Structurally checked claims and the closed Start/End administration DTO
-- vocabulary.
--
-- Nominal claims establish representation shape only. Deployment authorization,
-- manifest residence, retained-correlation admission, and semantic result
-- production remain responsibilities of the Herald administration adapter.
module Eclips.Protocol.Admin.Types
  ( AdminClaimError (..),
    AdminDeploymentIdClaim,
    adminDeploymentIdClaim,
    adminDeploymentIdClaimBytes,
    AdminProcessEpochIdClaim,
    adminProcessEpochIdClaim,
    adminProcessEpochIdClaimBytes,
    AdminProcessIdClaim,
    adminProcessIdClaim,
    adminProcessIdClaimBytes,
    AdminHeraldEpochClaim,
    adminHeraldEpochClaim,
    adminHeraldEpochClaimBytes,
    AdminApplicationAttachmentClaim,
    adminApplicationAttachmentClaim,
    adminApplicationAttachmentClaimBytes,
    AdminCorrelationIdClaim,
    adminCorrelationIdClaim,
    adminCorrelationIdClaimWord64,
    AdminControlIndexDto,
    adminControlIndexDto,
    adminControlIndexDtoWord64,
    AdminProcessEndReason (..),
    AdminRoleClaim (..),
    AdminStartOracleRejectionDto (..),
    AdminEndOracleRejectionDto (..),
    AdminCompletedResultDto (..),
    AdminRejectedErrorDto (..),
    AdminResultStatusDto (..),
    AdminHeraldStatusDto (..),
    AdminServicePhaseDto (..),
    AdminPreparationDto (..),
    AdminPreparationPhaseDto (..),
    AdminMembershipGenerationClaim,
    adminMembershipGenerationClaim,
    adminMembershipGenerationClaimBytes,
    AdminOracleNodeClaim,
    adminOracleNodeClaim,
    adminOracleNodeClaimBytes,
    AdminVoterConfigurationClaim,
    adminVoterConfigurationClaim,
    adminVoterConfigurationClaimBytes,
    AdminVoterChangeIdClaim,
    adminVoterChangeIdClaim,
    adminVoterChangeIdClaimWord64,
    AdminVoterBindingDto (..),
    AdminVoterChangeReasonDto (..),
    AdminOracleConfigurationDto (..),
    AdminVoterChangeStatusDto (..),
    AdminClientDto (..),
    adminReceiptRetirementWork,
    AdminServerDto (..),
    AdminEnvelope (..),
  )
where

import Data.Binary (Binary (..))
import Data.Binary.Get qualified as BinaryGet
import Data.Binary.Put qualified as BinaryPut
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (ChildPreparation, LifecycleStatus, StartupError)
import Eclips.Domain.ProcessLifecycle qualified as Domain
import Eclips.Oracle.Canonical
  ( CanonicalOracleReceipt,
    canonicalOracleReceiptBytes,
    decodeCanonicalOracleReceipt,
  )
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GHC.Generics (Generic)

adminClaimByteCount :: Int
adminClaimByteCount = 32

-- | A malformed nominal administration claim.
data AdminClaimError
  = AdminClaimWrongByteCount Int Int
  | AdminVoterChangeIdZero
  deriving stock (Eq, Show)

newtype Claim domain = Claim ByteString
  deriving stock (Eq, Ord)

instance Show (Claim domain) where
  show = renderGroupedHex . claimBytes

type role Claim nominal

data DeploymentIdClaimDomain

type AdminDeploymentIdClaim = Claim DeploymentIdClaimDomain

data ProcessEpochIdClaimDomain

type AdminProcessEpochIdClaim = Claim ProcessEpochIdClaimDomain

data ProcessIdClaimDomain

type AdminProcessIdClaim = Claim ProcessIdClaimDomain

data HeraldEpochClaimDomain

type AdminHeraldEpochClaim = Claim HeraldEpochClaimDomain

data MembershipGenerationClaimDomain
type AdminMembershipGenerationClaim = Claim MembershipGenerationClaimDomain
data OracleNodeClaimDomain
type AdminOracleNodeClaim = Claim OracleNodeClaimDomain
data VoterConfigurationClaimDomain
type AdminVoterConfigurationClaim = Claim VoterConfigurationClaimDomain

adminVoterConfigurationClaim :: ByteString -> Either AdminClaimError AdminVoterConfigurationClaim
adminVoterConfigurationClaim = checkedClaim
adminVoterConfigurationClaimBytes :: AdminVoterConfigurationClaim -> ByteString
adminVoterConfigurationClaimBytes = claimBytes

newtype AdminVoterChangeIdClaim = AdminVoterChangeIdClaim Word64
  deriving stock (Eq, Ord, Show)

adminVoterChangeIdClaim :: Word64 -> Either AdminClaimError AdminVoterChangeIdClaim
adminVoterChangeIdClaim 0 = Left AdminVoterChangeIdZero
adminVoterChangeIdClaim value = Right (AdminVoterChangeIdClaim value)
adminVoterChangeIdClaimWord64 :: AdminVoterChangeIdClaim -> Word64
adminVoterChangeIdClaimWord64 (AdminVoterChangeIdClaim value) = value

instance Binary AdminVoterChangeIdClaim where
  put = put . adminVoterChangeIdClaimWord64
  get = get >>= either (fail . show) pure . adminVoterChangeIdClaim

data AdminVoterBindingDto = AdminVoterBindingDto AdminOracleNodeClaim AdminHeraldEpochClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AdminVoterChangeReasonDto = AdminCommissionVotersDto | AdminDecommissionVotersDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AdminOracleConfigurationDto
  = AdminOracleConfigurationDto
      AdminControlIndexDto
      (Maybe Voter.VoterConfiguration)
      [Voter.OracleReplicaRegistration]
      (Maybe Voter.VoterChange)
  deriving stock (Eq, Show)

instance Binary AdminOracleConfigurationDto where
  put (AdminOracleConfigurationDto prefix configuration replicas pending) = do
    put prefix
    put (Voter.encodeVoterConfiguration <$> configuration)
    put (map Voter.encodeReplicaRegistration replicas)
    put (Voter.encodeVoterChange <$> pending)
  get =
    AdminOracleConfigurationDto
      <$> get
      <*> (get >>= traverse (admitCanonical . Voter.decodeVoterConfiguration))
      <*> (get >>= traverse (admitCanonical . Voter.decodeReplicaRegistration))
      <*> (get >>= traverse (admitCanonical . Voter.decodeVoterChange))

data AdminVoterChangeStatusDto = AdminVoterChangeStatusDto AdminControlIndexDto (Maybe Voter.VoterChange)
  deriving stock (Eq, Show)

instance Binary AdminVoterChangeStatusDto where
  put (AdminVoterChangeStatusDto prefix change) = put prefix >> put (Voter.encodeVoterChange <$> change)
  get = AdminVoterChangeStatusDto <$> get <*> (get >>= traverse (admitCanonical . Voter.decodeVoterChange))

admitCanonical :: (Show problem) => Either problem value -> BinaryGet.Get value
admitCanonical = either (fail . show) pure

adminMembershipGenerationClaim :: ByteString -> Either AdminClaimError AdminMembershipGenerationClaim
adminMembershipGenerationClaim = checkedClaim
adminMembershipGenerationClaimBytes :: AdminMembershipGenerationClaim -> ByteString
adminMembershipGenerationClaimBytes = claimBytes
adminOracleNodeClaim :: ByteString -> Either AdminClaimError AdminOracleNodeClaim
adminOracleNodeClaim = checkedClaim
adminOracleNodeClaimBytes :: AdminOracleNodeClaim -> ByteString
adminOracleNodeClaimBytes = claimBytes

data ApplicationAttachmentClaimDomain

type AdminApplicationAttachmentClaim = Claim ApplicationAttachmentClaimDomain

checkedClaim :: ByteString -> Either AdminClaimError (Claim domain)
checkedClaim bytes
  | ByteString.length bytes == adminClaimByteCount = Right (Claim bytes)
  | otherwise =
      Left (AdminClaimWrongByteCount adminClaimByteCount (ByteString.length bytes))

claimBytes :: Claim domain -> ByteString
claimBytes (Claim bytes) = bytes

instance Binary (Claim domain) where
  put = put . claimBytes
  get = get >>= either (fail . show) pure . checkedClaim

adminDeploymentIdClaim :: ByteString -> Either AdminClaimError AdminDeploymentIdClaim
adminDeploymentIdClaim = checkedClaim

adminDeploymentIdClaimBytes :: AdminDeploymentIdClaim -> ByteString
adminDeploymentIdClaimBytes = claimBytes

adminProcessEpochIdClaim :: ByteString -> Either AdminClaimError AdminProcessEpochIdClaim
adminProcessEpochIdClaim = checkedClaim

adminProcessEpochIdClaimBytes :: AdminProcessEpochIdClaim -> ByteString
adminProcessEpochIdClaimBytes = claimBytes

adminProcessIdClaim :: ByteString -> Either AdminClaimError AdminProcessIdClaim
adminProcessIdClaim = checkedClaim

adminProcessIdClaimBytes :: AdminProcessIdClaim -> ByteString
adminProcessIdClaimBytes = claimBytes

adminHeraldEpochClaim :: ByteString -> Either AdminClaimError AdminHeraldEpochClaim
adminHeraldEpochClaim = checkedClaim

adminHeraldEpochClaimBytes :: AdminHeraldEpochClaim -> ByteString
adminHeraldEpochClaimBytes = claimBytes

adminApplicationAttachmentClaim :: ByteString -> Either AdminClaimError AdminApplicationAttachmentClaim
adminApplicationAttachmentClaim = checkedClaim

adminApplicationAttachmentClaimBytes :: AdminApplicationAttachmentClaim -> ByteString
adminApplicationAttachmentClaimBytes = claimBytes

-- | A caller-selected opaque correlation scoped to one Herald runtime.
newtype AdminCorrelationIdClaim = AdminCorrelationIdClaim Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

-- | Construct a correlation claim. Zero is valid.
adminCorrelationIdClaim :: Word64 -> AdminCorrelationIdClaim
adminCorrelationIdClaim = AdminCorrelationIdClaim

adminCorrelationIdClaimWord64 :: AdminCorrelationIdClaim -> Word64
adminCorrelationIdClaimWord64 (AdminCorrelationIdClaim value) = value

-- | A control-ledger position mirrored only inside a typed Oracle rejection.
newtype AdminControlIndexDto = AdminControlIndexDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

adminControlIndexDto :: Word64 -> AdminControlIndexDto
adminControlIndexDto = AdminControlIndexDto

adminControlIndexDtoWord64 :: AdminControlIndexDto -> Word64
adminControlIndexDtoWord64 (AdminControlIndexDto value) = value

-- | The sole role accepted by the current administration family.
data AdminRoleClaim
  = ProcessAdministrator
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Exact DTO mirror of the Oracle rejection vocabulary reachable by Start.
data AdminStartOracleRejectionDto
  = AdminStartRequestHomeEpochMismatchDto AdminHeraldEpochClaim AdminHeraldEpochClaim
  | AdminStartInactiveHomeHeraldDto AdminHeraldEpochClaim
  | AdminStaleExpectedControlIndexDto AdminControlIndexDto AdminControlIndexDto
  | AdminStartProcessResidenceMismatchDto
      AdminProcessEpochIdClaim
      AdminHeraldEpochClaim
      AdminHeraldEpochClaim
  | AdminProcessEpochAlreadyStartedDto AdminProcessEpochIdClaim
  | AdminProcessAlreadyStartedDto AdminProcessIdClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Exact DTO mirror of the Oracle rejection vocabulary reachable by End.
data AdminEndOracleRejectionDto
  = AdminEndRequestHomeEpochMismatchDto AdminHeraldEpochClaim AdminHeraldEpochClaim
  | AdminEndInactiveHomeHeraldDto AdminHeraldEpochClaim
  | AdminEndProcessUnknownDto AdminProcessEpochIdClaim
  | AdminEndProcessResidenceMismatchDto
      AdminProcessEpochIdClaim
      AdminHeraldEpochClaim
      AdminHeraldEpochClaim
  | AdminEndProcessAlreadyEndedDto AdminProcessEpochIdClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A terminal result for one accepted Start or End command.
data AdminCompletedResultDto
  = StartProcessReadyDto
      AdminProcessEpochIdClaim
      AdminApplicationAttachmentClaim
  | StartProcessEndedBeforeAttachmentDto AdminProcessEpochIdClaim
  | ProcessEpochEndedDto AdminProcessEpochIdClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The rejected result families for the Start/End administration build.
data AdminRejectedErrorDto
  = StartProcessNotApplied AdminStartOracleRejectionDto
  | EndProcessNotApplied AdminEndOracleRejectionDto
  | AdminOracleReplicaEndpointsNotConfiguredDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Monotone retained state of one admitted correlation.
data AdminResultStatusDto
  = AdminAccepted
  | AdminCompleted AdminCompletedResultDto
  | AdminRejected AdminRejectedErrorDto
  | AdminPreparationCancellation LifecycleStatus
  | AdminOracleVoterResult CanonicalOracleReceipt
  deriving stock (Eq, Show, Generic)

instance Binary AdminResultStatusDto where
  put = \case
    AdminAccepted -> BinaryPut.putWord8 0
    AdminCompleted result -> BinaryPut.putWord8 1 >> put result
    AdminRejected problem -> BinaryPut.putWord8 2 >> put problem
    AdminPreparationCancellation status -> BinaryPut.putWord8 3 >> put status
    AdminOracleVoterResult receipt -> BinaryPut.putWord8 4 >> put (canonicalOracleReceiptBytes receipt)
  get =
    BinaryGet.getWord8 >>= \case
      0 -> pure AdminAccepted
      1 -> AdminCompleted <$> get
      2 -> AdminRejected <$> get
      3 -> AdminPreparationCancellation <$> get
      4 -> AdminOracleVoterResult <$> (get >>= admitCanonical . decodeCanonicalOracleReceipt)
      tag -> fail ("unknown administration result tag: " <> show tag)

-- | The only process-End reason an external administrator may express.
--
-- Application-loss inference is owned by the resident Herald and deliberately
-- has no EADM constructor.  Keeping this as a distinct singleton type makes
-- that boundary hold before decoding reaches the Herald RPC adapter.
data AdminProcessEndReason
  = AdminExplicitAdministrativeEnd
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data AdminServicePhaseDto = AdminServingDto | AdminDrainingDto | AdminStoppedDto | AdminIsolationDrainDto | AdminIsolatedDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AdminHeraldStatusDto
  = AdminHeraldStatusDto
      AdminDeploymentIdClaim
      AdminHeraldEpochClaim
      [AdminHeraldEpochClaim]
      [AdminHeraldEpochClaim]
      AdminMembershipGenerationClaim
      AdminControlIndexDto
      AdminServicePhaseDto
      (Maybe AdminOracleNodeClaim)
      (Maybe AdminOracleNodeClaim)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AdminPreparationPhaseDto = AdminPreparationPreparingDto | AdminPreparationPreparedDto | AdminPreparationAttachedDto | AdminPreparationCancellingDto | AdminPreparationTerminalDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AdminPreparationDto = AdminPreparationDto ChildPreparation (Maybe AdminProcessEpochIdClaim) AdminPreparationPhaseDto (Maybe StartupError)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Complete client-to-Herald Start/End administration vocabulary.
data AdminClientDto
  = AdminHello AdminDeploymentIdClaim AdminRoleClaim
  | StartProcessEpoch AdminCorrelationIdClaim
  | EndProcessEpoch
      AdminCorrelationIdClaim
      AdminProcessEpochIdClaim
      AdminProcessEndReason
  | GetAdminResult AdminCorrelationIdClaim
  | CancelChildPreparation AdminCorrelationIdClaim ChildPreparation
  | GetHeraldStatus AdminCorrelationIdClaim
  | ListChildPreparations AdminCorrelationIdClaim
  | DrainHerald AdminCorrelationIdClaim
  | PrepareOracleReplica AdminCorrelationIdClaim
  | BeginVoterChange AdminCorrelationIdClaim AdminVoterConfigurationClaim AdminVoterChangeReasonDto [AdminVoterBindingDto]
  | CancelVoterChange AdminCorrelationIdClaim AdminVoterChangeIdClaim
  | GetOracleConfiguration AdminCorrelationIdClaim
  | GetVoterChangeStatus AdminCorrelationIdClaim AdminVoterChangeIdClaim
  | AdminWithRetirement ReceiptRetirement AdminClientDto
  | RetireAdminReceipts ReceiptRetirement
  deriving stock (Eq, Show, Generic)

-- | Only one established operation can carry progress. Handshake and
-- maintenance frames cannot be recursively wrapped.
adminReceiptRetirementWork :: AdminClientDto -> Bool
adminReceiptRetirementWork = \case
  AdminHello {} -> False
  AdminWithRetirement {} -> False
  RetireAdminReceipts {} -> False
  _ -> True

-- Keep the closed client tag assignment explicit because the Domain-owned End
-- reason deliberately has canonical bytes rather than a generic Binary
-- instance.  EADM then narrows the checked Domain result to its one externally
-- administrable value.
instance Binary AdminClientDto where
  put client = case client of
    AdminWithRetirement progress request -> BinaryPut.putWord8 13 >> put progress >> put request
    RetireAdminReceipts progress -> BinaryPut.putWord8 14 >> put progress
    GetHeraldStatus correlation -> BinaryPut.putWord8 5 >> put correlation
    ListChildPreparations correlation -> BinaryPut.putWord8 6 >> put correlation
    DrainHerald correlation -> BinaryPut.putWord8 7 >> put correlation
    PrepareOracleReplica correlation -> BinaryPut.putWord8 8 >> put correlation
    BeginVoterChange correlation expected reason bindings -> BinaryPut.putWord8 9 >> put correlation >> put expected >> put reason >> put bindings
    CancelVoterChange correlation change -> BinaryPut.putWord8 10 >> put correlation >> put change
    GetOracleConfiguration correlation -> BinaryPut.putWord8 11 >> put correlation
    GetVoterChangeStatus correlation change -> BinaryPut.putWord8 12 >> put correlation >> put change
    AdminHello deployment role -> do
      BinaryPut.putWord8 0
      put deployment
      put role
    StartProcessEpoch correlation -> do
      BinaryPut.putWord8 1
      put correlation
    EndProcessEpoch correlation process reason -> do
      BinaryPut.putWord8 2
      put correlation
      put process
      put (adminProcessEndReasonCanonicalBytes reason)
    CancelChildPreparation correlation preparation -> do
      BinaryPut.putWord8 4
      put correlation
      put preparation
    GetAdminResult correlation -> do
      BinaryPut.putWord8 3
      put correlation

  get = do
    tag <- BinaryGet.getWord8
    case tag of
      0 -> AdminHello <$> get <*> get
      1 -> StartProcessEpoch <$> get
      2 -> do
        correlation <- get
        process <- get
        reasonBytes <- get
        reason <-
          either
            (fail . show)
            pure
            (decodeAdminProcessEndReasonCanonicalBytes reasonBytes)
        pure (EndProcessEpoch correlation process reason)
      3 -> GetAdminResult <$> get
      4 -> CancelChildPreparation <$> get <*> get
      5 -> GetHeraldStatus <$> get
      6 -> ListChildPreparations <$> get
      7 -> DrainHerald <$> get
      8 -> PrepareOracleReplica <$> get
      9 -> BeginVoterChange <$> get <*> get <*> get <*> get
      10 -> CancelVoterChange <$> get <*> get
      11 -> GetOracleConfiguration <$> get
      12 -> GetVoterChangeStatus <$> get <*> get
      13 -> do
        progress <- get
        request <- get
        if adminReceiptRetirementWork request
          then pure (AdminWithRetirement progress request)
          else fail "retirement metadata requires one established administration operation"
      14 -> RetireAdminReceipts <$> get
      _ -> fail ("unknown administration client tag: " <> show tag)

adminProcessEndReasonCanonicalBytes :: AdminProcessEndReason -> ByteString
adminProcessEndReasonCanonicalBytes AdminExplicitAdministrativeEnd =
  Domain.processEndReasonCanonicalBytes Domain.ExplicitAdministrativeEnd

decodeAdminProcessEndReasonCanonicalBytes ::
  ByteString -> Either String AdminProcessEndReason
decodeAdminProcessEndReasonCanonicalBytes bytes =
  case Domain.decodeProcessEndReasonCanonicalBytes bytes of
    Right Domain.ExplicitAdministrativeEnd ->
      Right AdminExplicitAdministrativeEnd
    Right Domain.ApplicationPermanentlyLost ->
      Left "application-loss reason is not an EADM operation"
    Right Domain.HeraldRetired ->
      Left "Herald-retirement reason is not an EADM operation"
    Left problem -> Left (show problem)

-- | Complete Herald-to-client Start/End administration vocabulary.
data AdminServerDto
  = AdminResult AdminCorrelationIdClaim AdminResultStatusDto
  | AdminAbsent AdminCorrelationIdClaim
  | AdminConflict AdminCorrelationIdClaim
  | HeraldStatus AdminCorrelationIdClaim AdminHeraldStatusDto
  | ChildPreparations AdminCorrelationIdClaim [AdminPreparationDto]
  | HeraldDrainAccepted AdminCorrelationIdClaim
  | HeraldDrained AdminCorrelationIdClaim
  | OracleConfiguration AdminCorrelationIdClaim AdminOracleConfigurationDto
  | VoterChangeStatus AdminCorrelationIdClaim AdminVoterChangeStatusDto
  | AdminResultRetired AdminCorrelationIdClaim ReceiptRetirement
  | AdminReceiptsRetired ReceiptRetirement
  | AdminRetirementNotReady
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | One closed serialized administration payload.
data AdminEnvelope
  = AdminClientEnvelope AdminClientDto
  | AdminServerEnvelope AdminServerDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
