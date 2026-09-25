{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Structural claims and the complete current EORC DTO vocabulary.
--
-- These values establish wire shape and nominal separation only. Canonical
-- Oracle admission, active-Herald membership, leader readiness, authoritative
-- request absence, watch-cursor continuity, and contact routing remain explicit
-- core/runtime responsibilities.
module Eclips.Protocol.Oracle.Types
  ( OracleClaimError (..),
    SystemIdClaim,
    systemIdClaim,
    systemIdClaimBytes,
    CatalogueDigestClaim,
    catalogueDigestClaim,
    catalogueDigestClaimBytes,
    ConfigurationDigestClaim,
    configurationDigestClaim,
    configurationDigestClaimBytes,
    InitialProjectionDigestClaim,
    initialProjectionDigestClaim,
    initialProjectionDigestClaimBytes,
    HeraldIdClaim,
    heraldIdClaim,
    heraldIdClaimBytes,
    HeraldEpochClaim,
    heraldEpochClaim,
    heraldEpochClaimBytes,
    HeraldMembershipGenerationClaim,
    heraldMembershipGenerationClaim,
    heraldMembershipGenerationClaimBytes,
    RaftNodeIdClaim,
    raftNodeIdClaim,
    raftNodeIdClaimBytes,
    LabelDecisionIdClaim,
    labelDecisionIdClaim,
    labelDecisionIdClaimBytes,
    ControlIndexDto,
    controlIndexDto,
    controlIndexDtoWord64,
    OracleProgressDto,
    oracleProgressDto,
    oracleProgressDtoReceipts,
    oracleProgressDtoLabelsThrough,
    RaftTermDto,
    raftTermDto,
    raftTermDtoWord64,
    OracleRequestSequenceDto,
    oracleRequestSequenceDto,
    oracleRequestSequenceDtoWord64,
    OracleRequestRetirementDto (..),
    OracleClientRequestIdDto,
    oracleClientRequestIdDto,
    oracleRequestHeraldEpoch,
    oracleRequestSequence,
    CanonicalOracleEnvelopeDto,
    canonicalOracleEnvelopeDto,
    canonicalOracleEnvelopeDtoBytes,
    CanonicalOracleReceiptDto,
    canonicalOracleReceiptDto,
    canonicalOracleReceiptDtoBytes,
    CanonicalAppliedOracleEntryDto,
    canonicalAppliedOracleEntryDto,
    canonicalAppliedOracleEntryDtoBytes,
    OracleHelloDto,
    oracleHelloDto,
    oracleHelloSystemId,
    oracleHelloCatalogueDigest,
    oracleHelloConfigurationDigest,
    oracleHelloInitialProjectionDigest,
    oracleHelloHeraldId,
    oracleHelloHeraldEpoch,
    oracleHelloLastAppliedControlIndex,
    oracleHelloMembershipGeneration,
    OracleHelloContext,
    oracleHelloContext,
    validateOracleHelloContext,
    OracleHelloAcceptedDto,
    oracleHelloAcceptedDto,
    oracleHelloAcceptedRaftNodeId,
    oracleHelloAcceptedCurrentTerm,
    oracleHelloAcceptedLocalAppliedControlIndex,
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedServiceReady,
    OracleHealthReplyDto,
    oracleHealthReplyDto,
    oracleHealthReplyRound,
    oracleHealthReplyNode,
    oracleHealthReplyTerm,
    oracleHealthReplyGeneration,
    oracleHealthReplyConfiguration,
    OracleRedirectDto,
    oracleRedirectDto,
    oracleRedirectLeaderHint,
    oracleRedirectCurrentTerm,
    OracleShapeError (..),
    CommittedOracleEntriesDto,
    committedOracleEntriesAfter,
    committedOracleEntryDtos,
    OracleClientMessage (..),
    OracleServerMessage (..),
    OracleProtocolEnvelope (..),
    OracleLaneRole (..),
    OracleLanePhase (..),
    OracleRoleError (..),
    admitOracleEnvelopeRole,
    OracleServiceReadyLeaderContext,
    oracleServiceReadyLeaderContext,
    OracleRequestAbsenceAuthority,
    noOracleRequestAbsenceAuthority,
    OracleAuthorityError (..),
    admitOracleHelloAuthority,
    admitOracleEnvelopeAuthority,
  )
where

import Data.Binary (Binary (..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import Eclips.Domain.Identity (controlIndexWord64)
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryValue,
    decodeCanonicalAppliedOracleEntry,
  )
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Raft.Configuration
import Eclips.Raft.Identity
  ( mkRaftNodeId,
    raftLogIndex,
    raftLogIndexWord64,
    raftNodeIdBytes,
    raftTerm,
    raftTermWord64,
  )
import GHC.Generics (Generic)

claimByteCount :: Int
claimByteCount = 32

-- | A malformed nominal 256-bit EORC claim.
data OracleClaimError = OracleClaimWrongByteCount
  { expectedOracleClaimByteCount :: Int,
    actualOracleClaimByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype Claim domain = Claim ByteString
  deriving stock (Eq, Ord)

instance Show (Claim domain) where
  show = renderGroupedHex . claimBytes

type role Claim nominal

data SystemIdClaimDomain

type SystemIdClaim = Claim SystemIdClaimDomain

data CatalogueDigestClaimDomain

type CatalogueDigestClaim = Claim CatalogueDigestClaimDomain

data ConfigurationDigestClaimDomain

type ConfigurationDigestClaim = Claim ConfigurationDigestClaimDomain

data InitialProjectionDigestClaimDomain

type InitialProjectionDigestClaim = Claim InitialProjectionDigestClaimDomain

data HeraldIdClaimDomain

type HeraldIdClaim = Claim HeraldIdClaimDomain

data HeraldEpochClaimDomain

type HeraldEpochClaim = Claim HeraldEpochClaimDomain

data HeraldMembershipGenerationClaimDomain

type HeraldMembershipGenerationClaim = Claim HeraldMembershipGenerationClaimDomain

data RaftNodeIdClaimDomain

type RaftNodeIdClaim = Claim RaftNodeIdClaimDomain

data LabelDecisionIdClaimDomain

type LabelDecisionIdClaim = Claim LabelDecisionIdClaimDomain

checkedClaim :: ByteString -> Either OracleClaimError (Claim domain)
checkedClaim bytes
  | ByteString.length bytes == claimByteCount = Right (Claim bytes)
  | otherwise =
      Left
        OracleClaimWrongByteCount
          { expectedOracleClaimByteCount = claimByteCount,
            actualOracleClaimByteCount = ByteString.length bytes
          }

claimBytes :: Claim domain -> ByteString
claimBytes (Claim bytes) = bytes

instance Binary (Claim domain) where
  put = put . claimBytes
  get = get >>= either (fail . show) pure . checkedClaim

systemIdClaim :: ByteString -> Either OracleClaimError SystemIdClaim
systemIdClaim = checkedClaim

systemIdClaimBytes :: SystemIdClaim -> ByteString
systemIdClaimBytes = claimBytes

catalogueDigestClaim :: ByteString -> Either OracleClaimError CatalogueDigestClaim
catalogueDigestClaim = checkedClaim

catalogueDigestClaimBytes :: CatalogueDigestClaim -> ByteString
catalogueDigestClaimBytes = claimBytes

configurationDigestClaim :: ByteString -> Either OracleClaimError ConfigurationDigestClaim
configurationDigestClaim = checkedClaim

configurationDigestClaimBytes :: ConfigurationDigestClaim -> ByteString
configurationDigestClaimBytes = claimBytes

initialProjectionDigestClaim :: ByteString -> Either OracleClaimError InitialProjectionDigestClaim
initialProjectionDigestClaim = checkedClaim

initialProjectionDigestClaimBytes :: InitialProjectionDigestClaim -> ByteString
initialProjectionDigestClaimBytes = claimBytes

heraldIdClaim :: ByteString -> Either OracleClaimError HeraldIdClaim
heraldIdClaim = checkedClaim

heraldIdClaimBytes :: HeraldIdClaim -> ByteString
heraldIdClaimBytes = claimBytes

heraldEpochClaim :: ByteString -> Either OracleClaimError HeraldEpochClaim
heraldEpochClaim = checkedClaim

heraldEpochClaimBytes :: HeraldEpochClaim -> ByteString
heraldEpochClaimBytes = claimBytes

heraldMembershipGenerationClaim ::
  ByteString -> Either OracleClaimError HeraldMembershipGenerationClaim
heraldMembershipGenerationClaim = checkedClaim

heraldMembershipGenerationClaimBytes ::
  HeraldMembershipGenerationClaim -> ByteString
heraldMembershipGenerationClaimBytes = claimBytes

raftNodeIdClaim :: ByteString -> Either OracleClaimError RaftNodeIdClaim
raftNodeIdClaim = checkedClaim

raftNodeIdClaimBytes :: RaftNodeIdClaim -> ByteString
raftNodeIdClaimBytes = claimBytes

labelDecisionIdClaim :: ByteString -> Either OracleClaimError LabelDecisionIdClaim
labelDecisionIdClaim = checkedClaim

labelDecisionIdClaimBytes :: LabelDecisionIdClaim -> ByteString
labelDecisionIdClaimBytes = claimBytes

newtype ControlIndexDto = ControlIndexDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

controlIndexDto :: Word64 -> ControlIndexDto
controlIndexDto = ControlIndexDto

controlIndexDtoWord64 :: ControlIndexDto -> Word64
controlIndexDtoWord64 (ControlIndexDto value) = value

-- | Independent receipt and label-completion lifetime promises. The complete
-- snapshot also correlates maintenance replies with their physical offer.
data OracleProgressDto = OracleProgressDto !ReceiptRetirement !ControlIndexDto
  deriving stock (Eq, Show)

oracleProgressDto :: ReceiptRetirement -> ControlIndexDto -> OracleProgressDto
oracleProgressDto = OracleProgressDto

oracleProgressDtoReceipts :: OracleProgressDto -> ReceiptRetirement
oracleProgressDtoReceipts (OracleProgressDto receipts _) = receipts

oracleProgressDtoLabelsThrough :: OracleProgressDto -> ControlIndexDto
oracleProgressDtoLabelsThrough (OracleProgressDto _ labels) = labels

instance Binary OracleProgressDto where
  put (OracleProgressDto receipts labels) = put receipts >> put labels
  get = OracleProgressDto <$> get <*> get

newtype RaftTermDto = RaftTermDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

raftTermDto :: Word64 -> RaftTermDto
raftTermDto = RaftTermDto

raftTermDtoWord64 :: RaftTermDto -> Word64
raftTermDtoWord64 (RaftTermDto value) = value

newtype OracleRequestSequenceDto = OracleRequestSequenceDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

oracleRequestSequenceDto :: Word64 -> OracleRequestSequenceDto
oracleRequestSequenceDto = OracleRequestSequenceDto

oracleRequestSequenceDtoWord64 :: OracleRequestSequenceDto -> Word64
oracleRequestSequenceDtoWord64 (OracleRequestSequenceDto value) = value

-- | A committed lifetime boundary, distinct from authoritative absence.
data OracleRequestRetirementDto
  = OracleRequestPrefixRetiredDto OracleRequestSequenceDto
  | OracleRequestHomeRetiredDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data OracleClientRequestIdDto
  = OracleClientRequestIdDto HeraldEpochClaim OracleRequestSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

oracleClientRequestIdDto :: HeraldEpochClaim -> OracleRequestSequenceDto -> OracleClientRequestIdDto
oracleClientRequestIdDto = OracleClientRequestIdDto

oracleRequestHeraldEpoch :: OracleClientRequestIdDto -> HeraldEpochClaim
oracleRequestHeraldEpoch (OracleClientRequestIdDto epoch _) = epoch

oracleRequestSequence :: OracleClientRequestIdDto -> OracleRequestSequenceDto
oracleRequestSequence (OracleClientRequestIdDto _ sequenceNumber) = sequenceNumber

newtype CanonicalOracleEnvelopeDto = CanonicalOracleEnvelopeDto ByteString
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

canonicalOracleEnvelopeDto :: ByteString -> CanonicalOracleEnvelopeDto
canonicalOracleEnvelopeDto = CanonicalOracleEnvelopeDto

canonicalOracleEnvelopeDtoBytes :: CanonicalOracleEnvelopeDto -> ByteString
canonicalOracleEnvelopeDtoBytes (CanonicalOracleEnvelopeDto bytes) = bytes

newtype CanonicalOracleReceiptDto = CanonicalOracleReceiptDto ByteString
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

canonicalOracleReceiptDto :: ByteString -> CanonicalOracleReceiptDto
canonicalOracleReceiptDto = CanonicalOracleReceiptDto

canonicalOracleReceiptDtoBytes :: CanonicalOracleReceiptDto -> ByteString
canonicalOracleReceiptDtoBytes (CanonicalOracleReceiptDto bytes) = bytes

-- | One canonical command or committed native-configuration control entry.
-- The core decoder admits the closed constructor domain and its exact event
-- coherence; both kinds participate in the same contiguous ControlIndex watch.
newtype CanonicalAppliedOracleEntryDto = CanonicalAppliedOracleEntryDto ByteString
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

canonicalAppliedOracleEntryDto :: ByteString -> CanonicalAppliedOracleEntryDto
canonicalAppliedOracleEntryDto = CanonicalAppliedOracleEntryDto

canonicalAppliedOracleEntryDtoBytes :: CanonicalAppliedOracleEntryDto -> ByteString
canonicalAppliedOracleEntryDtoBytes (CanonicalAppliedOracleEntryDto bytes) = bytes

data OracleHelloDto
  = OracleHelloDto
      SystemIdClaim
      CatalogueDigestClaim
      ConfigurationDigestClaim
      InitialProjectionDigestClaim
      HeraldIdClaim
      HeraldEpochClaim
      ControlIndexDto
      HeraldMembershipGenerationClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

oracleHelloDto ::
  SystemIdClaim ->
  CatalogueDigestClaim ->
  ConfigurationDigestClaim ->
  InitialProjectionDigestClaim ->
  HeraldIdClaim ->
  HeraldEpochClaim ->
  ControlIndexDto ->
  HeraldMembershipGenerationClaim ->
  OracleHelloDto
oracleHelloDto = OracleHelloDto

oracleHelloSystemId :: OracleHelloDto -> SystemIdClaim
oracleHelloSystemId (OracleHelloDto value _ _ _ _ _ _ _) = value

oracleHelloCatalogueDigest :: OracleHelloDto -> CatalogueDigestClaim
oracleHelloCatalogueDigest (OracleHelloDto _ value _ _ _ _ _ _) = value

oracleHelloConfigurationDigest :: OracleHelloDto -> ConfigurationDigestClaim
oracleHelloConfigurationDigest (OracleHelloDto _ _ value _ _ _ _ _) = value

oracleHelloInitialProjectionDigest :: OracleHelloDto -> InitialProjectionDigestClaim
oracleHelloInitialProjectionDigest (OracleHelloDto _ _ _ value _ _ _ _) = value

oracleHelloHeraldId :: OracleHelloDto -> HeraldIdClaim
oracleHelloHeraldId (OracleHelloDto _ _ _ _ value _ _ _) = value

oracleHelloHeraldEpoch :: OracleHelloDto -> HeraldEpochClaim
oracleHelloHeraldEpoch (OracleHelloDto _ _ _ _ _ value _ _) = value

oracleHelloLastAppliedControlIndex :: OracleHelloDto -> ControlIndexDto
oracleHelloLastAppliedControlIndex (OracleHelloDto _ _ _ _ _ _ value _) = value

oracleHelloMembershipGeneration ::
  OracleHelloDto -> HeraldMembershipGenerationClaim
oracleHelloMembershipGeneration (OracleHelloDto _ _ _ _ _ _ _ value) = value

-- | Deployment claims fixed before an EORC connection is accepted. Active
-- Herald membership remains a stateful Oracle admission after this comparison.
data OracleHelloContext
  = OracleHelloContext
      SystemIdClaim
      CatalogueDigestClaim
      ConfigurationDigestClaim
      InitialProjectionDigestClaim
  deriving stock (Eq, Show)

oracleHelloContext :: SystemIdClaim -> CatalogueDigestClaim -> ConfigurationDigestClaim -> InitialProjectionDigestClaim -> OracleHelloContext
oracleHelloContext = OracleHelloContext

data OracleShapeError
  = OracleHelloSystemMismatch
  | OracleHelloCatalogueMismatch
  | OracleHelloConfigurationMismatch
  | OracleHelloInitialProjectionMismatch
  | CommittedOracleEntriesEmpty
  | CanonicalAppliedOracleEntryMalformed
  | CommittedOracleEntriesNotContiguous ControlIndexDto ControlIndexDto
  | CommittedOracleEntriesDoNotFollowCursor ControlIndexDto ControlIndexDto
  deriving stock (Eq, Show)

validateOracleHelloContext :: OracleHelloContext -> OracleHelloDto -> Either OracleShapeError ()
validateOracleHelloContext (OracleHelloContext system catalogue configuration projection) hello
  | oracleHelloSystemId hello /= system = Left OracleHelloSystemMismatch
  | oracleHelloCatalogueDigest hello /= catalogue = Left OracleHelloCatalogueMismatch
  | oracleHelloConfigurationDigest hello /= configuration = Left OracleHelloConfigurationMismatch
  | oracleHelloInitialProjectionDigest hello /= projection = Left OracleHelloInitialProjectionMismatch
  | otherwise = Right ()

data OracleHelloAcceptedDto
  = OracleHelloAcceptedDto
      RaftNodeIdClaim
      RaftTermDto
      ControlIndexDto
      (Maybe RaftNodeIdClaim)
      Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

oracleHelloAcceptedDto :: RaftNodeIdClaim -> RaftTermDto -> ControlIndexDto -> Maybe RaftNodeIdClaim -> Bool -> OracleHelloAcceptedDto
oracleHelloAcceptedDto = OracleHelloAcceptedDto

oracleHelloAcceptedRaftNodeId :: OracleHelloAcceptedDto -> RaftNodeIdClaim
oracleHelloAcceptedRaftNodeId (OracleHelloAcceptedDto value _ _ _ _) = value

oracleHelloAcceptedCurrentTerm :: OracleHelloAcceptedDto -> RaftTermDto
oracleHelloAcceptedCurrentTerm (OracleHelloAcceptedDto _ value _ _ _) = value

oracleHelloAcceptedLocalAppliedControlIndex :: OracleHelloAcceptedDto -> ControlIndexDto
oracleHelloAcceptedLocalAppliedControlIndex (OracleHelloAcceptedDto _ _ value _ _) = value

oracleHelloAcceptedLeaderHint :: OracleHelloAcceptedDto -> Maybe RaftNodeIdClaim
oracleHelloAcceptedLeaderHint (OracleHelloAcceptedDto _ _ _ value _) = value

oracleHelloAcceptedServiceReady :: OracleHelloAcceptedDto -> Bool
oracleHelloAcceptedServiceReady (OracleHelloAcceptedDto _ _ _ _ value) = value

data OracleRedirectDto
  = OracleRedirectDto (Maybe RaftNodeIdClaim) RaftTermDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

oracleRedirectDto :: Maybe RaftNodeIdClaim -> RaftTermDto -> OracleRedirectDto
oracleRedirectDto = OracleRedirectDto

oracleRedirectLeaderHint :: OracleRedirectDto -> Maybe RaftNodeIdClaim
oracleRedirectLeaderHint (OracleRedirectDto value _) = value

oracleRedirectCurrentTerm :: OracleRedirectDto -> RaftTermDto
oracleRedirectCurrentTerm (OracleRedirectDto _ value) = value

newtype CommittedOracleEntriesDto
  = CommittedOracleEntriesDto (NonEmpty CanonicalAppliedOracleEntryDto)
  deriving stock (Eq, Show)

committedOracleEntriesDto :: [CanonicalAppliedOracleEntryDto] -> Either OracleShapeError CommittedOracleEntriesDto
committedOracleEntriesDto [] = Left CommittedOracleEntriesEmpty
committedOracleEntriesDto entries@(entry : following) = do
  indexes <- traverse canonicalEntryControlIndex entries
  checkContiguous indexes
  Right (CommittedOracleEntriesDto (entry :| following))

-- | Construct the exact non-empty suffix after a server-ledger cursor. The
-- decoder independently repeats internal contiguity; a Herald still compares
-- the received first index with its privately owned cursor.
committedOracleEntriesAfter :: ControlIndexDto -> [CanonicalAppliedOracleEntryDto] -> Either OracleShapeError CommittedOracleEntriesDto
committedOracleEntriesAfter cursor entries = do
  batch <- committedOracleEntriesDto entries
  firstIndex <- canonicalEntryControlIndex (NonEmpty.head (committedOracleEntryDtos batch))
  let expected = controlIndexDto (controlIndexDtoWord64 cursor + 1)
  if firstIndex == expected
    then Right batch
    else Left (CommittedOracleEntriesDoNotFollowCursor expected firstIndex)

committedOracleEntryDtos :: CommittedOracleEntriesDto -> NonEmpty CanonicalAppliedOracleEntryDto
committedOracleEntryDtos (CommittedOracleEntriesDto entries) = entries

instance Binary CommittedOracleEntriesDto where
  put = put . NonEmpty.toList . committedOracleEntryDtos
  get = do
    entries <- get
    either (fail . show) pure (committedOracleEntriesDto entries)

canonicalEntryControlIndex :: CanonicalAppliedOracleEntryDto -> Either OracleShapeError ControlIndexDto
canonicalEntryControlIndex dto = do
  canonical <-
    either
      (const (Left CanonicalAppliedOracleEntryMalformed))
      Right
      (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryDtoBytes dto))
  Right
    ( controlIndexDto
        ( controlIndexWord64
            (appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical))
        )
    )

checkContiguous :: [ControlIndexDto] -> Either OracleShapeError ()
checkContiguous [] = Right ()
checkContiguous [_] = Right ()
checkContiguous (previous : current : remaining)
  | controlIndexDtoWord64 current == controlIndexDtoWord64 previous + 1 =
      checkContiguous (current : remaining)
  | otherwise =
      Left
        ( CommittedOracleEntriesNotContiguous
            (controlIndexDto (controlIndexDtoWord64 previous + 1))
            current
        )

-- | The complete current client-to-server EORC vocabulary.
-- A health exchange is a separate one-shot lane: its run claims are checked,
-- but it needs neither active semantic membership nor service-ready leadership.
-- Checked native values here establish shape, not committed Oracle authority.
data OracleHealthReplyDto
  = OracleHealthReplyDto
      Word64
      RaftNodeIdClaim
      RaftTermDto
      Word64
      RaftConfigurationRef
      RaftVotingConfiguration
  deriving stock (Eq, Show)

oracleHealthReplyDto :: Word64 -> RaftNodeIdClaim -> RaftTermDto -> Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> OracleHealthReplyDto
oracleHealthReplyDto = OracleHealthReplyDto

oracleHealthReplyRound :: OracleHealthReplyDto -> Word64
oracleHealthReplyRound (OracleHealthReplyDto value _ _ _ _ _) = value

oracleHealthReplyNode :: OracleHealthReplyDto -> RaftNodeIdClaim
oracleHealthReplyNode (OracleHealthReplyDto _ value _ _ _ _) = value

oracleHealthReplyTerm :: OracleHealthReplyDto -> RaftTermDto
oracleHealthReplyTerm (OracleHealthReplyDto _ _ value _ _ _) = value

oracleHealthReplyGeneration :: OracleHealthReplyDto -> Word64
oracleHealthReplyGeneration (OracleHealthReplyDto _ _ _ value _ _) = value

oracleHealthReplyConfiguration :: OracleHealthReplyDto -> (RaftConfigurationRef, RaftVotingConfiguration)
oracleHealthReplyConfiguration (OracleHealthReplyDto _ _ _ _ reference configuration) = (reference, configuration)

instance Binary OracleHealthReplyDto where
  put (OracleHealthReplyDto roundNumber node term generation reference configuration) = do
    put roundNumber
    put node
    put term
    put generation
    case raftConfigurationRefView reference of
      GenesisRaftConfigurationRefView -> putWord8 0
      RaftConfigurationEntryRefView index entryTerm ->
        putWord8 1 >> put (raftLogIndexWord64 index) >> put (raftTermWord64 entryTerm)
    case raftVotingConfigurationView configuration of
      StableRaftConfigurationView voters -> putWord8 0 >> put (map raftNodeIdBytes voters)
      JointRaftConfigurationView old new -> putWord8 1 >> put (map raftNodeIdBytes old) >> put (map raftNodeIdBytes new)
  get = do
    roundNumber <- get
    node <- get
    term <- get
    generation <- get
    reference <-
      getWord8 >>= \case
        0 -> pure genesisRaftConfigurationRef
        1 -> do
          index <- raftLogIndex <$> get
          entryTerm <- raftTerm <$> get
          either (fail . show) pure (raftConfigurationEntryRef index entryTerm)
        _ -> fail "invalid health configuration reference"
    let voters = do
          bytes <- get
          nodes <- traverse (either (fail . show) pure . mkRaftNodeId) bytes
          either (fail . show) pure (raftVoterSet nodes)
    configuration <-
      getWord8 >>= \case
        0 -> stableRaftConfiguration <$> voters
        1 -> jointRaftConfiguration <$> voters <*> voters
        _ -> fail "invalid health voting configuration"
    pure (OracleHealthReplyDto roundNumber node term generation reference configuration)

data OracleClientMessage
  = OracleHello OracleHelloDto
  | SubmitOracleCommand CanonicalOracleEnvelopeDto
  | WatchOracle ControlIndexDto
  | GetOracleRequestResult OracleClientRequestIdDto
  | OracleHealthQuery Word64 OracleHelloDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The complete current server-to-client EORC vocabulary.
data OracleServerMessage
  = OracleHelloAccepted OracleHelloAcceptedDto
  | OracleRedirect OracleRedirectDto
  | OracleReceipt CanonicalOracleReceiptDto
  | OracleRequestAbsent OracleClientRequestIdDto ControlIndexDto
  | OracleSubmissionDeferred OracleClientRequestIdDto LabelDecisionIdClaim ControlIndexDto
  | CommittedOracleEntries CommittedOracleEntriesDto
  | OracleSubmissionNotReady OracleClientRequestIdDto RaftTermDto
  | OracleHealthReply OracleHealthReplyDto
  | OracleProgressRetired OracleClientRequestIdDto OracleProgressDto
  | OracleProgressNotReady OracleClientRequestIdDto OracleProgressDto RaftTermDto
  | OracleRequestRetired OracleClientRequestIdDto OracleRequestRetirementDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data OracleProtocolEnvelope
  = OracleClientEnvelope OracleClientMessage
  | OracleServerEnvelope OracleServerMessage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The remote role accepted by one local EORC lane.
data OracleLaneRole
  = OracleServerLane
  | OracleClientLane
  deriving stock (Eq, Show, Enum, Bounded)

data OracleLanePhase
  = AwaitingOracleHello
  | OracleLaneEstablished
  | OracleHealthLaneComplete
  deriving stock (Eq, Show, Enum, Bounded)

data OracleRoleError
  = OracleWrongEnvelopeDirection
  | OracleHelloRequired
  | OracleHelloRepeated
  | OracleHealthLaneClosed
  deriving stock (Eq, Show)

-- | Admit direction and the mandatory first Hello exchange. A redirect is a
-- valid response to a client Hello but does not establish the redirected lane.
admitOracleEnvelopeRole :: OracleLaneRole -> OracleLanePhase -> OracleProtocolEnvelope -> Either OracleRoleError OracleLanePhase
admitOracleEnvelopeRole OracleServerLane AwaitingOracleHello (OracleClientEnvelope (OracleHello _)) =
  Right OracleLaneEstablished
admitOracleEnvelopeRole OracleServerLane AwaitingOracleHello (OracleClientEnvelope (OracleHealthQuery _ _)) =
  Right OracleHealthLaneComplete
admitOracleEnvelopeRole OracleServerLane AwaitingOracleHello (OracleClientEnvelope _) =
  Left OracleHelloRequired
admitOracleEnvelopeRole OracleServerLane OracleLaneEstablished (OracleClientEnvelope (OracleHello _)) =
  Left OracleHelloRepeated
admitOracleEnvelopeRole OracleServerLane OracleLaneEstablished (OracleClientEnvelope (OracleHealthQuery _ _)) =
  Left OracleHealthLaneClosed
admitOracleEnvelopeRole OracleServerLane OracleLaneEstablished (OracleClientEnvelope _) =
  Right OracleLaneEstablished
admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello (OracleServerEnvelope (OracleHelloAccepted _)) =
  Right OracleLaneEstablished
admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello (OracleServerEnvelope (OracleHealthReply _)) =
  Right OracleHealthLaneComplete
admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello (OracleServerEnvelope (OracleRedirect _)) =
  Right AwaitingOracleHello
admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello (OracleServerEnvelope _) =
  Left OracleHelloRequired
admitOracleEnvelopeRole OracleClientLane OracleLaneEstablished (OracleServerEnvelope (OracleHelloAccepted _)) =
  Left OracleHelloRepeated
admitOracleEnvelopeRole OracleClientLane OracleLaneEstablished (OracleServerEnvelope (OracleHealthReply _)) =
  Left OracleHealthLaneClosed
admitOracleEnvelopeRole OracleClientLane OracleLaneEstablished (OracleServerEnvelope _) =
  Right OracleLaneEstablished
admitOracleEnvelopeRole OracleServerLane OracleHealthLaneComplete (OracleClientEnvelope _) = Left OracleHealthLaneClosed
admitOracleEnvelopeRole OracleClientLane OracleHealthLaneComplete (OracleServerEnvelope _) = Left OracleHealthLaneClosed
admitOracleEnvelopeRole _ _ _ = Left OracleWrongEnvelopeDirection

-- | Out-of-band authority for one server lane. The positive case asserts that
-- the remote endpoint is the service-ready leader and that the supplied index
-- is its complete applied committed prefix. Neither fact is trusted from EORC
-- bytes, including 'oracleHelloAcceptedServiceReady'.
-- | Runtime-owned service-ready-leader facts awaiting attachment to an exact
-- accepted Hello. This is deliberately distinct from installed authority.
data OracleServiceReadyLeaderContext
  = OracleServiceReadyLeaderContext
      RaftNodeIdClaim
      RaftTermDto
      ControlIndexDto
  deriving stock (Eq, Show)

-- | Runtime-owned facts for one exact accepted Hello. The constructor asserts
-- service-ready leadership out of band; admission still requires its node,
-- term, and complete prefix to match the Hello retained by the ingress lane.
oracleServiceReadyLeaderContext ::
  RaftNodeIdClaim ->
  RaftTermDto ->
  ControlIndexDto ->
  OracleServiceReadyLeaderContext
oracleServiceReadyLeaderContext = OracleServiceReadyLeaderContext

data OracleRequestAbsenceAuthority
  = OracleRequestAbsenceNotAuthoritative
  | OracleServiceReadyLeaderCompletePrefix
      RaftNodeIdClaim
      RaftTermDto
      ControlIndexDto
  deriving stock (Eq, Show)

noOracleRequestAbsenceAuthority :: OracleRequestAbsenceAuthority
noOracleRequestAbsenceAuthority = OracleRequestAbsenceNotAuthoritative

data OracleAuthorityError
  = OracleAuthorityNodeMismatch RaftNodeIdClaim RaftNodeIdClaim
  | OracleAuthorityTermMismatch RaftTermDto RaftTermDto
  | OracleAuthorityHelloNotServiceReady
  | OracleAuthorityCompletePrefixMismatch ControlIndexDto ControlIndexDto
  | OracleRequestAbsentNotAuthoritative
  | OracleRequestAbsentPrefixMismatch ControlIndexDto ControlIndexDto
  deriving stock (Eq, Show)

-- | Bind out-of-band service-ready-leader facts to the exact Hello accepted on
-- this connection. A mismatch cannot be retained as absence authority.
admitOracleHelloAuthority ::
  OracleHelloAcceptedDto ->
  OracleServiceReadyLeaderContext ->
  Either OracleAuthorityError OracleRequestAbsenceAuthority
admitOracleHelloAuthority hello (OracleServiceReadyLeaderContext node term completePrefix)
  | node /= oracleHelloAcceptedRaftNodeId hello =
      Left
        ( OracleAuthorityNodeMismatch
            (oracleHelloAcceptedRaftNodeId hello)
            node
        )
  | term /= oracleHelloAcceptedCurrentTerm hello =
      Left
        ( OracleAuthorityTermMismatch
            (oracleHelloAcceptedCurrentTerm hello)
            term
        )
  | not (oracleHelloAcceptedServiceReady hello) =
      Left OracleAuthorityHelloNotServiceReady
  | completePrefix /= oracleHelloAcceptedLocalAppliedControlIndex hello =
      Left
        ( OracleAuthorityCompletePrefixMismatch
            (oracleHelloAcceptedLocalAppliedControlIndex hello)
            completePrefix
        )
  | otherwise =
      Right
        ( OracleServiceReadyLeaderCompletePrefix
            node
            term
            completePrefix
        )

-- | Admit terminal absence only against current runtime-owned leader and
-- complete-prefix facts. Other messages need no absence authority.
admitOracleEnvelopeAuthority :: OracleRequestAbsenceAuthority -> OracleProtocolEnvelope -> Either OracleAuthorityError ()
admitOracleEnvelopeAuthority authority (OracleServerEnvelope (OracleRequestAbsent _ reportedPrefix)) =
  case authority of
    OracleRequestAbsenceNotAuthoritative ->
      Left OracleRequestAbsentNotAuthoritative
    OracleServiceReadyLeaderCompletePrefix _ _ completePrefix
      | reportedPrefix == completePrefix -> Right ()
      | otherwise ->
          Left
            ( OracleRequestAbsentPrefixMismatch
                completePrefix
                reportedPrefix
            )
admitOracleEnvelopeAuthority _ _ = Right ()
