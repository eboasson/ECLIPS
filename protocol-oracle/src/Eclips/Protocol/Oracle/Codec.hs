-- | Current-build EORC serialization and explicit semantic adapters.
module Eclips.Protocol.Oracle.Codec
  ( OraclePayloadError (..),
    OracleAdapterError (..),
    encodeOracleEnvelope,
    decodeOracleEnvelope,
    systemIdClaimFromDomain,
    systemIdClaimToDomain,
    catalogueDigestClaimFromDomain,
    catalogueDigestClaimToDomain,
    configurationDigestClaimFromDomain,
    configurationDigestClaimToDomain,
    initialProjectionDigestClaimFromDomain,
    initialProjectionDigestClaimToDomain,
    heraldIdClaimFromDomain,
    heraldIdClaimToDomain,
    heraldEpochClaimFromDomain,
    heraldEpochClaimToDomain,
    heraldMembershipGenerationClaimFromDomain,
    heraldMembershipGenerationClaimToDomain,
    labelDecisionIdClaimFromDomain,
    labelDecisionIdClaimToDomain,
    controlIndexDtoFromDomain,
    controlIndexDtoToDomain,
    oracleProgressDtoFromCore,
    oracleProgressDtoToCore,
    raftNodeIdClaimFromCore,
    raftNodeIdClaimToCore,
    raftTermDtoFromCore,
    raftTermDtoToCore,
    oracleClientRequestIdDtoFromCore,
    oracleClientRequestIdDtoToCore,
    oracleRequestRetirementDtoFromCore,
    oracleRequestRetirementDtoToCore,
    canonicalOracleEnvelopeDtoFromCore,
    canonicalOracleEnvelopeDtoToCore,
    canonicalOracleEnvelopeDtoFromValue,
    canonicalOracleEnvelopeDtoValue,
    canonicalOracleReceiptDtoFromCore,
    canonicalOracleReceiptDtoToCore,
    canonicalOracleReceiptDtoFromValue,
    canonicalOracleReceiptDtoValue,
    canonicalAppliedOracleEntryDtoFromCore,
    canonicalAppliedOracleEntryDtoToCore,
    canonicalAppliedOracleEntryDtoFromValue,
    canonicalAppliedOracleEntryDtoValue,
    committedOracleEntriesDtoFromValues,
    committedOracleEntriesDtoValues,
  )
where

import Control.Monad (void)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    LabelDecisionId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    heraldIdBytes,
    labelDecisionIdBytes,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
    mkSystemId,
    systemIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.Startup
  ( CatalogueDigest,
    ConfigurationDigest,
    InitialProjectionDigest,
    catalogueDigestBytes,
    configurationDigestBytes,
    initialProjectionDigestBytes,
    mkCatalogueDigest,
    mkConfigurationDigest,
    mkInitialProjectionDigest,
  )
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    CanonicalOracleEnvelope,
    CanonicalOracleReceipt,
    canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeBytes,
    canonicalOracleEnvelopeValue,
    canonicalOracleReceiptBytes,
    canonicalOracleReceiptValue,
    canonicalizeAppliedOracleEntry,
    canonicalizeOracleEnvelope,
    canonicalizeOracleReceipt,
    decodeCanonicalAppliedOracleEntry,
    decodeCanonicalOracleEnvelope,
    decodeCanonicalOracleReceipt,
  )
import Eclips.Oracle.Command (OracleEnvelope)
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
  )
import Eclips.Oracle.Progress (OracleProgress, oracleProgress, oracleProgressLabelsThrough, oracleProgressReceipts)
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Oracle.Receipt (OracleReceipt, OracleRequestRetirement (..))
import Eclips.Protocol.Oracle.Types
import Eclips.Raft.Identity
  ( RaftNodeId,
    RaftTerm,
    mkRaftNodeId,
    raftNodeIdBytes,
    raftTerm,
    raftTermWord64,
  )

data OraclePayloadError
  = OraclePayloadMalformed
  deriving stock (Eq, Show)

-- | Coarse named-adapter failures. Detailed core decoder diagnostics remain
-- core-local and do not become EORC protocol surface.
data OracleAdapterError
  = OracleSystemIdClaimRejected
  | OracleCatalogueDigestClaimRejected
  | OracleConfigurationDigestClaimRejected
  | OracleInitialProjectionDigestClaimRejected
  | OracleHeraldIdClaimRejected
  | OracleHeraldEpochClaimRejected
  | OracleMembershipGenerationClaimRejected
  | OracleLabelDecisionIdClaimRejected
  | OracleRaftNodeIdClaimRejected
  | OracleCanonicalEnvelopeRejected
  | OracleCanonicalReceiptRejected
  | OracleCanonicalAppliedEntryRejected
  deriving stock (Eq, Show, Enum, Bounded)

encodeOracleEnvelope :: OracleProtocolEnvelope -> ByteString
encodeOracleEnvelope = LazyByteString.toStrict . Binary.encode

-- | Decode exactly one body and validate every embedded canonical semantic
-- transcript before it can reach a kernel adapter.
decodeOracleEnvelope :: ByteString -> Either OraclePayloadError OracleProtocolEnvelope
decodeOracleEnvelope body =
  case Binary.decodeOrFail (LazyByteString.fromStrict body) of
    Left _ -> Left OraclePayloadMalformed
    Right (residual, _, envelope)
      | not (LazyByteString.null residual) -> Left OraclePayloadMalformed
      | otherwise ->
          case validateCanonicalPayload envelope of
            Left _ -> Left OraclePayloadMalformed
            Right () -> Right envelope

systemIdClaimFromDomain :: SystemId -> SystemIdClaim
systemIdClaimFromDomain = checkedOutbound systemIdClaim . systemIdBytes

systemIdClaimToDomain :: SystemIdClaim -> Either OracleAdapterError SystemId
systemIdClaimToDomain = mapLeft (const OracleSystemIdClaimRejected) . mkSystemId . systemIdClaimBytes

catalogueDigestClaimFromDomain :: CatalogueDigest -> CatalogueDigestClaim
catalogueDigestClaimFromDomain = checkedOutbound catalogueDigestClaim . catalogueDigestBytes

catalogueDigestClaimToDomain :: CatalogueDigestClaim -> Either OracleAdapterError CatalogueDigest
catalogueDigestClaimToDomain = mapLeft (const OracleCatalogueDigestClaimRejected) . mkCatalogueDigest . catalogueDigestClaimBytes

configurationDigestClaimFromDomain :: ConfigurationDigest -> ConfigurationDigestClaim
configurationDigestClaimFromDomain = checkedOutbound configurationDigestClaim . configurationDigestBytes

configurationDigestClaimToDomain :: ConfigurationDigestClaim -> Either OracleAdapterError ConfigurationDigest
configurationDigestClaimToDomain =
  mapLeft (const OracleConfigurationDigestClaimRejected)
    . mkConfigurationDigest
    . configurationDigestClaimBytes

initialProjectionDigestClaimFromDomain :: InitialProjectionDigest -> InitialProjectionDigestClaim
initialProjectionDigestClaimFromDomain = checkedOutbound initialProjectionDigestClaim . initialProjectionDigestBytes

initialProjectionDigestClaimToDomain :: InitialProjectionDigestClaim -> Either OracleAdapterError InitialProjectionDigest
initialProjectionDigestClaimToDomain =
  mapLeft (const OracleInitialProjectionDigestClaimRejected)
    . mkInitialProjectionDigest
    . initialProjectionDigestClaimBytes

heraldIdClaimFromDomain :: HeraldId -> HeraldIdClaim
heraldIdClaimFromDomain = checkedOutbound heraldIdClaim . heraldIdBytes

heraldIdClaimToDomain :: HeraldIdClaim -> Either OracleAdapterError HeraldId
heraldIdClaimToDomain = mapLeft (const OracleHeraldIdClaimRejected) . mkHeraldId . heraldIdClaimBytes

heraldEpochClaimFromDomain :: HeraldEpoch -> HeraldEpochClaim
heraldEpochClaimFromDomain = checkedOutbound heraldEpochClaim . heraldEpochBytes

heraldEpochClaimToDomain :: HeraldEpochClaim -> Either OracleAdapterError HeraldEpoch
heraldEpochClaimToDomain = mapLeft (const OracleHeraldEpochClaimRejected) . mkHeraldEpoch . heraldEpochClaimBytes

heraldMembershipGenerationClaimFromDomain ::
  HeraldMembershipGenerationId -> HeraldMembershipGenerationClaim
heraldMembershipGenerationClaimFromDomain =
  checkedOutbound heraldMembershipGenerationClaim
    . heraldMembershipGenerationIdBytes

heraldMembershipGenerationClaimToDomain ::
  HeraldMembershipGenerationClaim ->
  Either OracleAdapterError HeraldMembershipGenerationId
heraldMembershipGenerationClaimToDomain =
  mapLeft (const OracleMembershipGenerationClaimRejected)
    . mkHeraldMembershipGenerationId
    . heraldMembershipGenerationClaimBytes

labelDecisionIdClaimFromDomain :: LabelDecisionId -> LabelDecisionIdClaim
labelDecisionIdClaimFromDomain = checkedOutbound labelDecisionIdClaim . labelDecisionIdBytes

labelDecisionIdClaimToDomain :: LabelDecisionIdClaim -> Either OracleAdapterError LabelDecisionId
labelDecisionIdClaimToDomain =
  mapLeft (const OracleLabelDecisionIdClaimRejected)
    . mkLabelDecisionId
    . labelDecisionIdClaimBytes

controlIndexDtoFromDomain :: ControlIndex -> ControlIndexDto
controlIndexDtoFromDomain = controlIndexDto . controlIndexWord64

controlIndexDtoToDomain :: ControlIndexDto -> ControlIndex
controlIndexDtoToDomain = controlIndex . controlIndexDtoWord64

oracleProgressDtoFromCore :: OracleProgress -> OracleProgressDto
oracleProgressDtoFromCore progress =
  oracleProgressDto (oracleProgressReceipts progress) (controlIndexDtoFromDomain (oracleProgressLabelsThrough progress))

oracleProgressDtoToCore :: OracleProgressDto -> OracleProgress
oracleProgressDtoToCore progress =
  oracleProgress (oracleProgressDtoReceipts progress) (controlIndexDtoToDomain (oracleProgressDtoLabelsThrough progress))

raftNodeIdClaimFromCore :: RaftNodeId -> RaftNodeIdClaim
raftNodeIdClaimFromCore = checkedOutbound raftNodeIdClaim . raftNodeIdBytes

raftNodeIdClaimToCore :: RaftNodeIdClaim -> Either OracleAdapterError RaftNodeId
raftNodeIdClaimToCore = mapLeft (const OracleRaftNodeIdClaimRejected) . mkRaftNodeId . raftNodeIdClaimBytes

raftTermDtoFromCore :: RaftTerm -> RaftTermDto
raftTermDtoFromCore = raftTermDto . raftTermWord64

raftTermDtoToCore :: RaftTermDto -> RaftTerm
raftTermDtoToCore = raftTerm . raftTermDtoWord64

oracleClientRequestIdDtoFromCore :: OracleClientRequestId -> OracleClientRequestIdDto
oracleClientRequestIdDtoFromCore requestId =
  oracleClientRequestIdDto
    (heraldEpochClaimFromDomain (oracleClientRequestHome requestId))
    (oracleRequestSequenceDto (oracleClientRequestSequence requestId))

oracleClientRequestIdDtoToCore :: OracleClientRequestIdDto -> Either OracleAdapterError OracleClientRequestId
oracleClientRequestIdDtoToCore dto = do
  home <- heraldEpochClaimToDomain (oracleRequestHeraldEpoch dto)
  Right
    ( oracleClientRequestId
        home
        (oracleRequestSequenceDtoWord64 (oracleRequestSequence dto))
    )

oracleRequestRetirementDtoFromCore :: OracleRequestRetirement -> OracleRequestRetirementDto
oracleRequestRetirementDtoFromCore = \case
  OracleRequestPrefixRetired frontier -> OracleRequestPrefixRetiredDto (oracleRequestSequenceDto frontier)
  OracleRequestHomeRetired -> OracleRequestHomeRetiredDto

oracleRequestRetirementDtoToCore :: OracleRequestRetirementDto -> OracleRequestRetirement
oracleRequestRetirementDtoToCore = \case
  OracleRequestPrefixRetiredDto frontier -> OracleRequestPrefixRetired (oracleRequestSequenceDtoWord64 frontier)
  OracleRequestHomeRetiredDto -> OracleRequestHomeRetired

canonicalOracleEnvelopeDtoFromCore :: CanonicalOracleEnvelope -> CanonicalOracleEnvelopeDto
canonicalOracleEnvelopeDtoFromCore = canonicalOracleEnvelopeDto . canonicalOracleEnvelopeBytes

canonicalOracleEnvelopeDtoToCore :: CanonicalOracleEnvelopeDto -> Either OracleAdapterError CanonicalOracleEnvelope
canonicalOracleEnvelopeDtoToCore =
  mapLeft (const OracleCanonicalEnvelopeRejected)
    . decodeCanonicalOracleEnvelope
    . canonicalOracleEnvelopeDtoBytes

canonicalOracleEnvelopeDtoFromValue :: OracleEnvelope -> CanonicalOracleEnvelopeDto
canonicalOracleEnvelopeDtoFromValue = canonicalOracleEnvelopeDtoFromCore . canonicalizeOracleEnvelope

canonicalOracleEnvelopeDtoValue :: CanonicalOracleEnvelopeDto -> Either OracleAdapterError OracleEnvelope
canonicalOracleEnvelopeDtoValue = fmap canonicalOracleEnvelopeValue . canonicalOracleEnvelopeDtoToCore

canonicalOracleReceiptDtoFromCore :: CanonicalOracleReceipt -> CanonicalOracleReceiptDto
canonicalOracleReceiptDtoFromCore = canonicalOracleReceiptDto . canonicalOracleReceiptBytes

canonicalOracleReceiptDtoToCore :: CanonicalOracleReceiptDto -> Either OracleAdapterError CanonicalOracleReceipt
canonicalOracleReceiptDtoToCore =
  mapLeft (const OracleCanonicalReceiptRejected)
    . decodeCanonicalOracleReceipt
    . canonicalOracleReceiptDtoBytes

canonicalOracleReceiptDtoFromValue :: OracleReceipt -> CanonicalOracleReceiptDto
canonicalOracleReceiptDtoFromValue = canonicalOracleReceiptDtoFromCore . canonicalizeOracleReceipt

canonicalOracleReceiptDtoValue :: CanonicalOracleReceiptDto -> Either OracleAdapterError OracleReceipt
canonicalOracleReceiptDtoValue = fmap canonicalOracleReceiptValue . canonicalOracleReceiptDtoToCore

canonicalAppliedOracleEntryDtoFromCore :: CanonicalAppliedOracleEntry -> CanonicalAppliedOracleEntryDto
canonicalAppliedOracleEntryDtoFromCore = canonicalAppliedOracleEntryDto . canonicalAppliedOracleEntryBytes

canonicalAppliedOracleEntryDtoToCore :: CanonicalAppliedOracleEntryDto -> Either OracleAdapterError CanonicalAppliedOracleEntry
canonicalAppliedOracleEntryDtoToCore =
  mapLeft (const OracleCanonicalAppliedEntryRejected)
    . decodeCanonicalAppliedOracleEntry
    . canonicalAppliedOracleEntryDtoBytes

canonicalAppliedOracleEntryDtoFromValue :: AppliedOracleEntry -> CanonicalAppliedOracleEntryDto
canonicalAppliedOracleEntryDtoFromValue = canonicalAppliedOracleEntryDtoFromCore . canonicalizeAppliedOracleEntry

canonicalAppliedOracleEntryDtoValue :: CanonicalAppliedOracleEntryDto -> Either OracleAdapterError AppliedOracleEntry
canonicalAppliedOracleEntryDtoValue = fmap canonicalAppliedOracleEntryValue . canonicalAppliedOracleEntryDtoToCore

committedOracleEntriesDtoFromValues :: ControlIndex -> [AppliedOracleEntry] -> Either OracleShapeError CommittedOracleEntriesDto
committedOracleEntriesDtoFromValues cursor =
  committedOracleEntriesAfter
    (controlIndexDtoFromDomain cursor)
    . fmap canonicalAppliedOracleEntryDtoFromValue

committedOracleEntriesDtoValues :: CommittedOracleEntriesDto -> Either OracleAdapterError (NonEmpty.NonEmpty AppliedOracleEntry)
committedOracleEntriesDtoValues =
  traverse canonicalAppliedOracleEntryDtoValue . committedOracleEntryDtos

validateCanonicalPayload :: OracleProtocolEnvelope -> Either OracleAdapterError ()
validateCanonicalPayload (OracleClientEnvelope (SubmitOracleCommand dto)) =
  void (canonicalOracleEnvelopeDtoToCore dto)
validateCanonicalPayload (OracleServerEnvelope (OracleReceipt dto)) =
  void (canonicalOracleReceiptDtoToCore dto)
validateCanonicalPayload (OracleServerEnvelope (CommittedOracleEntries batch)) =
  void (committedOracleEntriesDtoValues batch)
validateCanonicalPayload _ = Right ()

checkedOutbound :: (ByteString -> Either problem value) -> ByteString -> value
checkedOutbound constructor bytes =
  case constructor bytes of
    Left _ -> error "checked semantic value violated its exact-width wire invariant"
    Right value -> value

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft f = \case
  Left problem -> Left (f problem)
  Right value -> Right value
