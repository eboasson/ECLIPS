-- | Nominal identities shared by the pure semantic domain.
--
-- Every byte identity in profile 0.1 is exactly 256 bits. The common private
-- representation does not weaken nominal separation: the role annotation makes
-- coercion between identity domains illegal.
--
-- These types establish byte shape and nominal namespace only. In particular, a
-- checked byte parser or explicit same-bit conversion is not evidence that an
-- object exists or has the asserted semantic role; the owning pure transition
-- must still admit that claim against its state.
module Eclips.Domain.Identity
  ( IdentityError (..),
    GlobalUniqueId,
    mkGlobalUniqueId,
    globalUniqueIdBytes,
    GlobalObjectId,
    mkGlobalObjectId,
    globalObjectIdBytes,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    NablaSequencing (..),
    ProcessEpochId,
    mkProcessEpochId,
    processEpochIdBytes,
    processEpochIdFromGlobalObjectId,
    globalObjectIdFromProcessEpochId,
    SystemId,
    mkSystemId,
    systemIdBytes,
    HeraldId,
    mkHeraldId,
    heraldIdBytes,
    ProcessId,
    mkProcessId,
    processIdBytes,
    BootstrapManifestId,
    mkBootstrapManifestId,
    bootstrapManifestIdBytes,
    SortId,
    mkSortId,
    sortIdBytes,
    SortDefinitionOccurrenceId,
    mkSortDefinitionOccurrenceId,
    sortDefinitionOccurrenceIdBytes,
    NablaId,
    mkNablaId,
    nablaIdBytes,
    nablaIdFromGlobalObjectId,
    globalObjectIdFromNablaId,
    DeltaId,
    mkDeltaId,
    deltaIdBytes,
    deltaIdFromGlobalObjectId,
    globalObjectIdFromDeltaId,
    HeraldEpoch,
    mkHeraldEpoch,
    heraldEpochBytes,
    StructuralSequence,
    StructuralSequenceError (..),
    mkStructuralSequence,
    firstStructuralSequence,
    nextStructuralSequence,
    structuralSequenceWord64,
    structuralSequencePredecessor,
    StructuralOccurrenceId,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    TopologyCutId,
    mkTopologyCutId,
    topologyCutIdBytes,
    StoreIncarnationId,
    mkStoreIncarnationId,
    storeIncarnationIdBytes,
    LabelDecisionId,
    mkLabelDecisionId,
    labelDecisionIdBytes,
    AuthorityEpoch,
    AuthorityEpochView (..),
    genesisAuthorityEpoch,
    structuralAuthorityEpoch,
    labelAuthorityEpoch,
    authorityEpochView,
    AuthorityEpochCanonicalProblem (..),
    authorityEpochCanonicalBytes,
    decodeAuthorityEpochCanonicalBytes,
    NablaSequence,
    nablaSequence,
    nablaSequenceWord64,
    PublicationId,
    publicationId,
    publicationNabla,
    publicationAuthorityEpoch,
    publicationSourceHeraldEpoch,
    publicationNablaSequence,
    ControlIndex,
    controlIndex,
    controlIndexWord64,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize.Get qualified as SerializeGet
import Data.Serialize.Put qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.SortId (SortId)
import Eclips.Public.Types.SortId qualified as PublicSortId

identifierByteCount :: Int
identifierByteCount = 32

-- | A structurally invalid nominal identity.
data IdentityError = WrongIdentityByteCount
  { expectedIdentityByteCount :: Int,
    actualIdentityByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype Identifier domain = Identifier ByteString
  deriving stock (Eq, Ord)

instance Show (Identifier domain) where
  show = renderGroupedHex . identifierBytes

type role Identifier nominal

data GlobalObjectDomain

-- | A nominal identifier presented in the controlled-object namespace.
--
-- Values of this type are still subject to stateful controlled-object admission.
type GlobalObjectId = Identifier GlobalObjectDomain

data GlobalUniqueDomain

-- | A globally meaningful opaque identifier carried by semantic values.
--
-- Application processes do not observe this identity directly: their private
-- identifiers are translated at the Herald boundary. A bare global unique
-- identifier has no controlled-object meaning until a checked controlled-
-- instantiation transition binds the same 256 value bits as a distinct
-- 'GlobalObjectId'.
type GlobalUniqueId = Identifier GlobalUniqueDomain

-- | The explicit semantic sequencing relation of one nabla.  Absence is a
-- real catalogue fact rather than an omitted/defaulted object identity.
data NablaSequencing
  = UnsequencedNabla
  | NablaSequencedBy GlobalObjectId
  deriving stock (Eq, Ord, Show)

data ProcessEpochDomain

-- | A controlled-object identity refined as one application-process epoch.
type ProcessEpochId = Identifier ProcessEpochDomain

data SystemDomain

type SystemId = Identifier SystemDomain

data HeraldDomain

-- | Stable configured logical identity of a Herald.
--
-- A 'HeraldId' alone never authenticates traffic from a particular run; that
-- role belongs to the separately typed 'HeraldEpoch'.
type HeraldId = Identifier HeraldDomain

data ProcessDomain

-- | Stable configured logical identity of an application process.
--
-- Authority and liveness are always qualified by a 'ProcessEpochId'.
type ProcessId = Identifier ProcessDomain

data BootstrapManifestDomain

-- | Opaque key of one deployment-configured process bootstrap manifest.
type BootstrapManifestId = Identifier BootstrapManifestDomain

data SortDefinitionOccurrenceDomain

type SortDefinitionOccurrenceId = Identifier SortDefinitionOccurrenceDomain

data NablaDomain

-- | A controlled-object identity refined as a nabla.
type NablaId = Identifier NablaDomain

data DeltaDomain

-- | A controlled-object identity refined as a delta.
type DeltaId = Identifier DeltaDomain

data HeraldEpochDomain

type HeraldEpoch = Identifier HeraldEpochDomain

-- | A strictly positive source-local structural occurrence sequence.
--
-- This sequence is nominally distinct from both a nabla's publication sequence
-- and a peer stream's assignment sequence. Profile-0.1 counter exhaustion is
-- outside the finite-run model.
newtype StructuralSequence = StructuralSequence Word64
  deriving stock (Eq, Ord, Show)

data StructuralSequenceError
  = StructuralSequenceMustBePositive
  deriving stock (Eq, Show)

-- | The exact source-local identity of one topology-affecting occurrence.
data StructuralOccurrenceId
  = StructuralOccurrenceId HeraldEpoch StructuralSequence
  deriving stock (Eq, Ord, Show)

data TopologyCutDomain

-- | Canonical SHA-256 identity of an established topology cut.
--
-- The distinguished genesis root inhabits the same nominal domain as dynamic
-- cuts.  Construction checks only the exact digest width; the topology owner
-- separately recomputes and validates its semantic transcript.
type TopologyCutId = Identifier TopologyCutDomain

data StoreIncarnationDomain

type StoreIncarnationId = Identifier StoreIncarnationDomain

data LabelDecisionDomain

-- | Stable nominal identity of one global label-decision workflow.
--
-- Construction establishes only the exact 256-bit shape.  The owning workflow
-- separately derives and verifies the system/request-qualified transcript.
type LabelDecisionId = Identifier LabelDecisionDomain

mkIdentifier :: ByteString -> Either IdentityError (Identifier domain)
mkIdentifier bytes
  | ByteString.length bytes == identifierByteCount = Right (Identifier bytes)
  | otherwise =
      Left
        WrongIdentityByteCount
          { expectedIdentityByteCount = identifierByteCount,
            actualIdentityByteCount = ByteString.length bytes
          }

identifierBytes :: Identifier domain -> ByteString
identifierBytes (Identifier bytes) = bytes

reinterpretIdentifier :: Identifier source -> Identifier destination
reinterpretIdentifier (Identifier bytes) = Identifier bytes

-- | Check only the structural 32-byte representation of an untrusted nominal ID.
mkGlobalObjectId :: ByteString -> Either IdentityError GlobalObjectId
mkGlobalObjectId = mkIdentifier

globalObjectIdBytes :: GlobalObjectId -> ByteString
globalObjectIdBytes = identifierBytes

mkGlobalUniqueId :: ByteString -> Either IdentityError GlobalUniqueId
mkGlobalUniqueId = mkIdentifier

globalUniqueIdBytes :: GlobalUniqueId -> ByteString
globalUniqueIdBytes = identifierBytes

-- | Present a global unique identifier in the controlled-object namespace.
--
-- This conversion preserves the exact 256 bits but carries no proof of controlled
-- instantiation. It belongs at a trusted transition that checks or establishes
-- that fact; the nominal types prevent accidental interchange elsewhere.
globalObjectIdFromGlobalUniqueId :: GlobalUniqueId -> GlobalObjectId
globalObjectIdFromGlobalUniqueId = reinterpretIdentifier

-- | View a controlled object identity as the global unique identifier stored in
-- its canonical application value.
--
-- This conversion also preserves the exact 256 bits and belongs at the trusted
-- semantic boundary that constructs or inspects controlled instances.
globalUniqueIdFromGlobalObjectId :: GlobalObjectId -> GlobalUniqueId
globalUniqueIdFromGlobalObjectId = reinterpretIdentifier

-- | Check only the structural representation of a nominal process-epoch claim.
mkProcessEpochId :: ByteString -> Either IdentityError ProcessEpochId
mkProcessEpochId = mkIdentifier

processEpochIdBytes :: ProcessEpochId -> ByteString
processEpochIdBytes = identifierBytes

-- | Present a controlled object identity as a process-epoch claim.
--
-- The caller must admit that claim against checked controlled state.
processEpochIdFromGlobalObjectId :: GlobalObjectId -> ProcessEpochId
processEpochIdFromGlobalObjectId = reinterpretIdentifier

-- | Recover the controlled object identity denoted by a process epoch.
globalObjectIdFromProcessEpochId :: ProcessEpochId -> GlobalObjectId
globalObjectIdFromProcessEpochId = reinterpretIdentifier

mkSystemId :: ByteString -> Either IdentityError SystemId
mkSystemId = mkIdentifier

systemIdBytes :: SystemId -> ByteString
systemIdBytes = identifierBytes

mkHeraldId :: ByteString -> Either IdentityError HeraldId
mkHeraldId = mkIdentifier

heraldIdBytes :: HeraldId -> ByteString
heraldIdBytes = identifierBytes

mkProcessId :: ByteString -> Either IdentityError ProcessId
mkProcessId = mkIdentifier

processIdBytes :: ProcessId -> ByteString
processIdBytes = identifierBytes

mkBootstrapManifestId :: ByteString -> Either IdentityError BootstrapManifestId
mkBootstrapManifestId = mkIdentifier

bootstrapManifestIdBytes :: BootstrapManifestId -> ByteString
bootstrapManifestIdBytes = identifierBytes

-- | Check a public sort identity while preserving the domain identity error API.
mkSortId :: ByteString -> Either IdentityError SortId
mkSortId bytes =
  case PublicSortId.mkSortId bytes of
    Left problem ->
      Left
        WrongIdentityByteCount
          { expectedIdentityByteCount =
              PublicSortId.expectedSortIdByteCount problem,
            actualIdentityByteCount =
              PublicSortId.actualSortIdByteCount problem
          }
    Right identifier -> Right identifier

-- | Recover the exact public content-address bytes of a checked sort identity.
sortIdBytes :: SortId -> ByteString
sortIdBytes = PublicSortId.sortIdBytes

mkSortDefinitionOccurrenceId ::
  ByteString -> Either IdentityError SortDefinitionOccurrenceId
mkSortDefinitionOccurrenceId = mkIdentifier

sortDefinitionOccurrenceIdBytes :: SortDefinitionOccurrenceId -> ByteString
sortDefinitionOccurrenceIdBytes = identifierBytes

-- | Check only the structural representation of a nominal nabla claim.
mkNablaId :: ByteString -> Either IdentityError NablaId
mkNablaId = mkIdentifier

nablaIdBytes :: NablaId -> ByteString
nablaIdBytes = identifierBytes

-- | Present a controlled object identity as a nabla claim.
--
-- The caller must admit that claim against checked predefined state.
nablaIdFromGlobalObjectId :: GlobalObjectId -> NablaId
nablaIdFromGlobalObjectId = reinterpretIdentifier

-- | Recover the controlled object identity denoted by a nabla.
globalObjectIdFromNablaId :: NablaId -> GlobalObjectId
globalObjectIdFromNablaId = reinterpretIdentifier

-- | Check only the structural representation of a nominal delta claim.
mkDeltaId :: ByteString -> Either IdentityError DeltaId
mkDeltaId = mkIdentifier

deltaIdBytes :: DeltaId -> ByteString
deltaIdBytes = identifierBytes

-- | Present a controlled object identity as a delta claim.
--
-- The caller must admit that claim against checked predefined state.
deltaIdFromGlobalObjectId :: GlobalObjectId -> DeltaId
deltaIdFromGlobalObjectId = reinterpretIdentifier

-- | Recover the controlled object identity denoted by a delta.
globalObjectIdFromDeltaId :: DeltaId -> GlobalObjectId
globalObjectIdFromDeltaId = reinterpretIdentifier

mkHeraldEpoch :: ByteString -> Either IdentityError HeraldEpoch
mkHeraldEpoch = mkIdentifier

heraldEpochBytes :: HeraldEpoch -> ByteString
heraldEpochBytes = identifierBytes

mkStructuralSequence ::
  Word64 -> Either StructuralSequenceError StructuralSequence
mkStructuralSequence 0 = Left StructuralSequenceMustBePositive
mkStructuralSequence value = Right (StructuralSequence value)

firstStructuralSequence :: StructuralSequence
firstStructuralSequence = StructuralSequence 1

-- | Advance a positive structural sequence.
--
-- The profile-wide finite-run assumption excludes counter exhaustion and
-- wrap-around, so this operation deliberately has no capacity outcome.
nextStructuralSequence :: StructuralSequence -> StructuralSequence
nextStructuralSequence (StructuralSequence value) =
  StructuralSequence (value + 1)

structuralSequenceWord64 :: StructuralSequence -> Word64
structuralSequenceWord64 (StructuralSequence value) = value

structuralSequencePredecessor ::
  StructuralSequence -> Maybe StructuralSequence
structuralSequencePredecessor (StructuralSequence 1) = Nothing
structuralSequencePredecessor (StructuralSequence value) =
  Just (StructuralSequence (value - 1))

structuralOccurrenceId ::
  HeraldEpoch -> StructuralSequence -> StructuralOccurrenceId
structuralOccurrenceId = StructuralOccurrenceId

structuralOccurrenceSourceHeraldEpoch ::
  StructuralOccurrenceId -> HeraldEpoch
structuralOccurrenceSourceHeraldEpoch
  (StructuralOccurrenceId source _) = source

structuralOccurrenceSourceSequence ::
  StructuralOccurrenceId -> StructuralSequence
structuralOccurrenceSourceSequence
  (StructuralOccurrenceId _ sequenceNumber) = sequenceNumber

mkTopologyCutId :: ByteString -> Either IdentityError TopologyCutId
mkTopologyCutId = mkIdentifier

topologyCutIdBytes :: TopologyCutId -> ByteString
topologyCutIdBytes = identifierBytes

mkStoreIncarnationId ::
  ByteString -> Either IdentityError StoreIncarnationId
mkStoreIncarnationId = mkIdentifier

storeIncarnationIdBytes :: StoreIncarnationId -> ByteString
storeIncarnationIdBytes = identifierBytes

mkLabelDecisionId :: ByteString -> Either IdentityError LabelDecisionId
mkLabelDecisionId = mkIdentifier

labelDecisionIdBytes :: LabelDecisionId -> ByteString
labelDecisionIdBytes = identifierBytes

-- | Exact provenance of one nabla-operator tenure.
--
-- Only index-zero startup nablas use the genesis arm.  A later data-plane
-- nabla receives its initial authority from the occurrence and installed cut
-- which establish it.  Every subsequent live tenure is identified by the
-- control index of the label release which established it.
data AuthorityEpoch
  = GenesisAuthorityEpoch
  | StructuralAuthorityEpoch StructuralOccurrenceId TopologyCutId
  | LabelAuthorityEpoch ControlIndex
  deriving stock (Eq, Show)

-- | Read-only elimination vocabulary for the opaque authority sum.
data AuthorityEpochView
  = GenesisAuthorityEpochView
  | StructuralAuthorityEpochView StructuralOccurrenceId TopologyCutId
  | LabelAuthorityEpochView ControlIndex
  deriving stock (Eq, Ord, Show)

genesisAuthorityEpoch :: AuthorityEpoch
genesisAuthorityEpoch = GenesisAuthorityEpoch

structuralAuthorityEpoch ::
  StructuralOccurrenceId -> TopologyCutId -> AuthorityEpoch
structuralAuthorityEpoch = StructuralAuthorityEpoch

labelAuthorityEpoch :: ControlIndex -> AuthorityEpoch
labelAuthorityEpoch = LabelAuthorityEpoch

authorityEpochView :: AuthorityEpoch -> AuthorityEpochView
authorityEpochView GenesisAuthorityEpoch = GenesisAuthorityEpochView
authorityEpochView (StructuralAuthorityEpoch occurrence cut) =
  StructuralAuthorityEpochView occurrence cut
authorityEpochView (LabelAuthorityEpoch index) = LabelAuthorityEpochView index

-- | Canonical leaf encoding embedded in every semantic digest transcript.
--
-- Tags are fixed at genesis = 0, structural = 1, and label = 2.  Structural
-- payload order is occurrence source, positive sequence, then topology-cut
-- digest; the label payload is its release control index.
authorityEpochCanonicalBytes :: AuthorityEpoch -> ByteString
authorityEpochCanonicalBytes authority =
  Serialize.runPut $ case authority of
    GenesisAuthorityEpoch -> Serialize.putWord8 0
    StructuralAuthorityEpoch occurrence cut -> do
      Serialize.putWord8 1
      Serialize.putByteString
        (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
      Serialize.putWord64be
        (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence))
      Serialize.putByteString (topologyCutIdBytes cut)
    LabelAuthorityEpoch index -> do
      Serialize.putWord8 2
      Serialize.putWord64be (controlIndexWord64 index)

-- | A malformed or semantically inadmissible authority transcript.
data AuthorityEpochCanonicalProblem
  = AuthorityEpochCanonicalDecodeFailed String
  | AuthorityEpochCanonicalUnknownTag Word8
  | AuthorityEpochCanonicalInvalidIdentity IdentityError
  | AuthorityEpochCanonicalInvalidStructuralSequence StructuralSequenceError
  | AuthorityEpochCanonicalNonCanonical
  deriving stock (Eq, Show)

-- | Checked inverse of 'authorityEpochCanonicalBytes'.
--
-- The decoder owns only the closed authority grammar and nested nominal
-- admission.  Stateful evidence that an authority was actually established
-- remains with its kernel owner.
decodeAuthorityEpochCanonicalBytes ::
  ByteString -> Either AuthorityEpochCanonicalProblem AuthorityEpoch
decodeAuthorityEpochCanonicalBytes bytes = do
  raw <-
    case SerializeGet.runGetState getRawAuthorityEpoch bytes 0 of
      Left problem -> Left (AuthorityEpochCanonicalDecodeFailed problem)
      Right (decoded, trailing)
        | ByteString.null trailing -> Right decoded
        | otherwise ->
            Left
              (AuthorityEpochCanonicalDecodeFailed "trailing bytes")
  authority <- admitRawAuthorityEpoch raw
  if authorityEpochCanonicalBytes authority == bytes
    then Right authority
    else Left AuthorityEpochCanonicalNonCanonical

data RawAuthorityEpoch
  = RawGenesisAuthorityEpoch
  | RawStructuralAuthorityEpoch ByteString Word64 ByteString
  | RawLabelAuthorityEpoch Word64
  | RawUnknownAuthorityEpoch Word8

getRawAuthorityEpoch :: SerializeGet.Get RawAuthorityEpoch
getRawAuthorityEpoch = do
  tag <- SerializeGet.getWord8
  case tag of
    0 -> pure RawGenesisAuthorityEpoch
    1 ->
      RawStructuralAuthorityEpoch
        <$> SerializeGet.getByteString identifierByteCount
        <*> SerializeGet.getWord64be
        <*> SerializeGet.getByteString identifierByteCount
    2 -> RawLabelAuthorityEpoch <$> SerializeGet.getWord64be
    _ -> pure (RawUnknownAuthorityEpoch tag)

admitRawAuthorityEpoch ::
  RawAuthorityEpoch -> Either AuthorityEpochCanonicalProblem AuthorityEpoch
admitRawAuthorityEpoch raw = case raw of
  RawGenesisAuthorityEpoch -> Right genesisAuthorityEpoch
  RawStructuralAuthorityEpoch sourceBytes sequenceNumber cutBytes -> do
    source <-
      either
        (Left . AuthorityEpochCanonicalInvalidIdentity)
        Right
        (mkHeraldEpoch sourceBytes)
    sequenceValue <-
      either
        (Left . AuthorityEpochCanonicalInvalidStructuralSequence)
        Right
        (mkStructuralSequence sequenceNumber)
    cut <-
      either
        (Left . AuthorityEpochCanonicalInvalidIdentity)
        Right
        (mkTopologyCutId cutBytes)
    Right
      ( structuralAuthorityEpoch
          (structuralOccurrenceId source sequenceValue)
          cut
      )
  RawLabelAuthorityEpoch index ->
    Right (labelAuthorityEpoch (controlIndex index))
  RawUnknownAuthorityEpoch tag ->
    Left (AuthorityEpochCanonicalUnknownTag tag)

instance Ord AuthorityEpoch where
  compare left right = compare (authorityEpochView left) (authorityEpochView right)

-- | Monotone publication sequence within one nabla authority tenure.
newtype NablaSequence = NablaSequence Word64
  deriving stock (Eq, Ord, Show)

nablaSequence :: Word64 -> NablaSequence
nablaSequence = NablaSequence

nablaSequenceWord64 :: NablaSequence -> Word64
nablaSequenceWord64 (NablaSequence value) = value

-- | Stable publication provenance.
--
-- Ordering is normative and deliberately differs from field declaration order:
-- nabla, authority epoch, nabla sequence, then source Herald epoch.
data PublicationId = PublicationId
  { publicationNabla :: NablaId,
    publicationAuthorityEpoch :: AuthorityEpoch,
    publicationSourceHeraldEpoch :: HeraldEpoch,
    publicationNablaSequence :: NablaSequence
  }
  deriving stock (Eq, Show)

publicationId ::
  NablaId ->
  AuthorityEpoch ->
  HeraldEpoch ->
  NablaSequence ->
  PublicationId
publicationId = PublicationId

-- | Position in the first-seen applied Oracle transition stream.
--
-- Profile 0.1 runs are assumed to remain far below exhaustion; construction is
-- therefore total and deliberately has no capacity or rollover outcome.
newtype ControlIndex = ControlIndex Word64
  deriving stock (Eq, Ord, Show)

controlIndex :: Word64 -> ControlIndex
controlIndex = ControlIndex

controlIndexWord64 :: ControlIndex -> Word64
controlIndexWord64 (ControlIndex value) = value

instance Ord PublicationId where
  compare left right =
    compare
      ( publicationNabla left,
        publicationAuthorityEpoch left,
        publicationNablaSequence left,
        publicationSourceHeraldEpoch left
      )
      ( publicationNabla right,
        publicationAuthorityEpoch right,
        publicationNablaSequence right,
        publicationSourceHeraldEpoch right
      )
