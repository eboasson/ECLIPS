{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The closed ordinary/structural semantic peer-stream payload.
--
-- This is deliberately a typed kernel value rather than a wire DTO.  The
-- digest is computed over a private normalized transcript; stream direction
-- and sequence qualify an assignment outside that transcript.
module Eclips.Herald.PeerPublication
  ( PublicationDestination,
    publicationDestination,
    publicationDestinationDelta,
    publicationDestinationStoreIncarnation,
    publicationDestinationStrength,
    PublicationBatch,
    PublicationBatchProblem (..),
    mkPublicationBatch,
    publicationBatchId,
    publicationBatchSourceProcess,
    publicationBatchSortId,
    publicationBatchOccurrenceId,
    publicationBatchCanonicalValue,
    publicationBatchSourceStrength,
    publicationBatchSourceTopologyPrerequisite,
    publicationBatchControlPrerequisite,
    publicationBatchHasFenceDependency,
    publicationBatchDestinations,
    StructuralPublicationDigest,
    StructuralPublicationDigestProblem (..),
    mkStructuralPublicationDigest,
    structuralPublicationDigestBytes,
    StructuralOccurrenceStamp,
    StructuralOccurrenceStampProblem (..),
    mkStructuralOccurrenceStamp,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralOccurrenceStampCarrierRole,
    PeerPublication,
    PeerPublicationKind (..),
    PeerPublicationProblem (..),
    structuralPublicationDigestForSemantics,
    structuralPublicationCanonicalBytesForSemantics,
    StructuralPublicationSemantics,
    structuralPublicationSemanticsPublicationId,
    structuralPublicationSemanticsSourceProcess,
    structuralPublicationSemanticsSortId,
    structuralPublicationSemanticsOccurrenceId,
    structuralPublicationSemanticsCanonicalValue,
    structuralPublicationSemanticsSourceStrength,
    structuralPublicationSemanticsSourceTopologyPrerequisite,
    structuralPublicationSemanticsControlPrerequisite,
    structuralPublicationSemanticsCarrierRole,
    decodeStructuralPublicationCanonicalBytes,
    structuralPublicationDigestFor,
    mkOrdinaryPeerPublication,
    mkStructuralPeerPublication,
    presentedOrdinaryPeerPublication,
    presentedStructuralPeerPublication,
    authenticatePeerPublication,
    peerPublicationKind,
    peerPublicationBatch,
    peerPublicationStructuralStamp,
    peerPublicationStructuralCanonicalBytes,
    peerPublicationDigest,
    peerPublicationItem,
  )
where

import Control.Monad (unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( AuthorityEpochCanonicalProblem,
    ControlIndex,
    DeltaId,
    HeraldEpoch,
    IdentityError,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    TopologyCutId,
    authorityEpochCanonicalBytes,
    controlIndex,
    controlIndexWord64,
    decodeAuthorityEpochCanonicalBytes,
    deltaIdBytes,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkTopologyCutId,
    nablaIdBytes,
    nablaSequence,
    nablaSequenceWord64,
    processEpochIdBytes,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (..),
    descriptorStructuralCarrierRole,
    structuralCarrierRoleTag,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    profileEntryFor,
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVersionVector,
    structuralPredecessorPrefix,
    structuralPrefixSequence,
    structuralVersionVectorComponent,
    structuralVersionVectorEntries,
  )
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    ValueError,
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
  )
import Eclips.Herald.PeerStream
  ( PeerItem,
    PeerItemDigest,
    StreamShapeProblem,
    mkPeerItemDigest,
    peerItem,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

-- | One exact destination in the peer named by the surrounding stream.
--
-- Host identity is intentionally absent: a batch cannot name a third Herald.
data PublicationDestination = PublicationDestination
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Ord, Show)

publicationDestination ::
  DeltaId ->
  StoreIncarnationId ->
  ReplicaStrength ->
  PublicationDestination
publicationDestination = PublicationDestination

publicationDestinationDelta :: PublicationDestination -> DeltaId
publicationDestinationDelta destination = destination.delta

publicationDestinationStoreIncarnation ::
  PublicationDestination ->
  StoreIncarnationId
publicationDestinationStoreIncarnation destination = destination.storeIncarnation

publicationDestinationStrength :: PublicationDestination -> ReplicaStrength
publicationDestinationStrength destination = destination.strength

-- | An immutable, unsequenced publication cut for exactly one peer Herald.
--
-- Source strength is fixed at local acceptance and authenticated by both the
-- ordinary and structural transcripts.  The absent fence dependency remains a
-- closed current-profile fact.
data PublicationBatch = PublicationBatch
  { identifier :: PublicationId,
    sourceProcess :: ProcessEpochId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    canonicalValue :: CanonicalValueBytes,
    sourceStrength :: ReplicaStrength,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex,
    destinations :: NonEmpty PublicationDestination,
    ordinaryDigest :: PeerItemDigest
  }
  deriving stock (Eq, Show)

data PublicationBatchProblem
  = ConflictingPublicationDestination
      DeltaId
      PublicationDestination
      PublicationDestination
  | PublicationDestinationStrongerThanSource
      ReplicaStrength
      PublicationDestination
  | PublicationBatchDigestShapeFault StreamShapeProblem
  deriving stock (Eq, Show)

-- | Construct a batch and normalize its non-empty destination cut by Delta.
-- Exact duplicates collapse; unequal claims for one Delta are contradictory.
mkPublicationBatch ::
  PublicationId ->
  ProcessEpochId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  Either PublicationBatchProblem PublicationBatch
mkPublicationBatch
  identifier
  sourceProcess
  sortId
  occurrenceId
  canonicalValue
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  suppliedDestinations = do
    normalized <- normalizeDestinations suppliedDestinations
    case filter ((> sourceStrength) . publicationDestinationStrength) (toList normalized) of
      stronger : _ ->
        Left (PublicationDestinationStrongerThanSource sourceStrength stronger)
      [] -> Right ()
    let digestBytes =
          SHA256.hash
            ( Serialize.encode
                ( publicationBatchTranscript
                    identifier
                    sourceProcess
                    sortId
                    occurrenceId
                    canonicalValue
                    sourceStrength
                    sourceTopologyPrerequisite
                    controlPrerequisite
                    normalized
                )
            )
    checkedDigest <-
      either
        (Left . PublicationBatchDigestShapeFault)
        Right
        (mkPeerItemDigest digestBytes)
    Right
      PublicationBatch
        { identifier,
          sourceProcess,
          sortId,
          occurrenceId,
          canonicalValue,
          sourceStrength,
          sourceTopologyPrerequisite,
          controlPrerequisite,
          destinations = normalized,
          ordinaryDigest = checkedDigest
        }

publicationBatchId :: PublicationBatch -> PublicationId
publicationBatchId batch = batch.identifier

publicationBatchSourceProcess :: PublicationBatch -> ProcessEpochId
publicationBatchSourceProcess batch = batch.sourceProcess

publicationBatchSortId :: PublicationBatch -> SortId
publicationBatchSortId batch = batch.sortId

publicationBatchOccurrenceId ::
  PublicationBatch ->
  SortDefinitionOccurrenceId
publicationBatchOccurrenceId batch = batch.occurrenceId

publicationBatchCanonicalValue :: PublicationBatch -> CanonicalValueBytes
publicationBatchCanonicalValue batch = batch.canonicalValue

publicationBatchSourceStrength :: PublicationBatch -> ReplicaStrength
publicationBatchSourceStrength batch = batch.sourceStrength

publicationBatchSourceTopologyPrerequisite ::
  PublicationBatch -> TopologyCutId
publicationBatchSourceTopologyPrerequisite batch =
  batch.sourceTopologyPrerequisite

publicationBatchControlPrerequisite :: PublicationBatch -> ControlIndex
publicationBatchControlPrerequisite batch = batch.controlPrerequisite

publicationBatchHasFenceDependency :: PublicationBatch -> Bool
publicationBatchHasFenceDependency _ = False

publicationBatchDestinations ::
  PublicationBatch ->
  NonEmpty PublicationDestination
publicationBatchDestinations batch = batch.destinations

-- | Destination-independent semantic identity of one structural publication.
--
-- This is deliberately distinct from the receiver-qualified peer-item digest.
newtype StructuralPublicationDigest
  = StructuralPublicationDigest ByteString
  deriving stock (Eq, Ord)

instance Show StructuralPublicationDigest where
  show = renderGroupedHex . structuralPublicationDigestBytes

data StructuralPublicationDigestProblem
  = StructuralPublicationDigestWrongByteCount
  { expectedStructuralPublicationDigestByteCount :: Int,
    actualStructuralPublicationDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

mkStructuralPublicationDigest ::
  ByteString -> Either StructuralPublicationDigestProblem StructuralPublicationDigest
mkStructuralPublicationDigest bytes
  | byteCount == 32 = Right (StructuralPublicationDigest bytes)
  | otherwise =
      Left
        StructuralPublicationDigestWrongByteCount
          { expectedStructuralPublicationDigestByteCount = 32,
            actualStructuralPublicationDigestByteCount = byteCount
          }
  where
    byteCount = ByteString.length bytes

structuralPublicationDigestBytes ::
  StructuralPublicationDigest ->
  ByteString
structuralPublicationDigestBytes
  (StructuralPublicationDigest bytes) = bytes

-- | Checked causal/publication stamp for one source-local occurrence.
--
-- Construction proves the source Herald agrees with publication provenance and
-- that the source component of the complete predecessor vector is exactly the
-- prefix immediately before the occurrence.  The enclosing structural draft
-- additionally checks publication, carrier-role, and semantic-digest equality
-- against its exact batch and checked value.
data StructuralOccurrenceStamp = StructuralOccurrenceStamp
  { occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector,
    publication :: PublicationId,
    publicationDigest :: StructuralPublicationDigest,
    carrierRole :: StructuralCarrierRole
  }
  deriving stock (Eq, Show)

data StructuralOccurrenceStampProblem
  = StructuralStampSourceMismatch
      HeraldEpoch
      HeraldEpoch
  | StructuralStampPredecessorSourceMissing HeraldEpoch
  | StructuralStampPredecessorMismatch
      HeraldEpoch
      StructuralPrefix
      StructuralPrefix
  | StructuralStampProcessEpochUnsupported
  deriving stock (Eq, Show)

mkStructuralOccurrenceStamp ::
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  PublicationId ->
  StructuralPublicationDigest ->
  StructuralCarrierRole ->
  Either StructuralOccurrenceStampProblem StructuralOccurrenceStamp
mkStructuralOccurrenceStamp
  occurrence
  predecessor
  publication
  publicationDigest
  carrierRole = do
    let occurrenceSource = structuralOccurrenceSourceHeraldEpoch occurrence
        publicationSource = publicationSourceHeraldEpoch publication
        expectedPredecessor =
          structuralPredecessorPrefix
            (structuralOccurrenceSourceSequence occurrence)
    unless
      (occurrenceSource == publicationSource)
      (Left (StructuralStampSourceMismatch occurrenceSource publicationSource))
    actualPredecessor <-
      maybe
        (Left (StructuralStampPredecessorSourceMissing occurrenceSource))
        Right
        (structuralVersionVectorComponent occurrenceSource predecessor)
    unless
      (actualPredecessor == expectedPredecessor)
      ( Left
          ( StructuralStampPredecessorMismatch
              occurrenceSource
              expectedPredecessor
              actualPredecessor
          )
      )
    when
      (carrierRole == ProcessEpochCarrier)
      (Left StructuralStampProcessEpochUnsupported)
    Right
      StructuralOccurrenceStamp
        { occurrence,
          predecessor,
          publication,
          publicationDigest,
          carrierRole
        }

structuralOccurrenceStampOccurrence ::
  StructuralOccurrenceStamp ->
  StructuralOccurrenceId
structuralOccurrenceStampOccurrence stamp = stamp.occurrence

structuralOccurrenceStampPredecessor ::
  StructuralOccurrenceStamp ->
  StructuralVersionVector
structuralOccurrenceStampPredecessor stamp = stamp.predecessor

structuralOccurrenceStampPublication ::
  StructuralOccurrenceStamp ->
  PublicationId
structuralOccurrenceStampPublication stamp = stamp.publication

structuralOccurrenceStampPublicationDigest ::
  StructuralOccurrenceStamp ->
  StructuralPublicationDigest
structuralOccurrenceStampPublicationDigest stamp = stamp.publicationDigest

structuralOccurrenceStampCarrierRole ::
  StructuralOccurrenceStamp ->
  StructuralCarrierRole
structuralOccurrenceStampCarrierRole stamp = stamp.carrierRole

-- | The closed semantic peer-publication item. Constructors remain hidden so
-- local producers cannot bypass the ordinary/structural role checks.
data PeerPublication
  = OrdinaryPublication
      PublicationBatch
  | StructuralPublication
      StructuralOccurrenceStamp
      PublicationBatch
      PeerItemDigest
  deriving stock (Eq, Show)

data PeerPublicationKind
  = OrdinaryPublicationKind
  | StructuralPublicationKind
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PeerPublicationProblem
  = DescriptorPublicationSortMismatch SortId SortId
  | BatchPublicationSortMismatch SortId SortId
  | BatchDefinitionOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | BatchPublicationIdentityMismatch PublicationId PublicationId
  | BatchCanonicalValueMismatch
  | OrdinaryStructuralCarrier StructuralCarrierRole
  | StructuralCarrierMissing
  | StructuralDescriptorNotExactPredefined StructuralCarrierRole
  | StructuralProcessEpochUnsupported
  | StructuralStampPublicationMismatch PublicationId PublicationId
  | StructuralStampCarrierRoleMismatch
      StructuralCarrierRole
      StructuralCarrierRole
  | StructuralStampDigestMismatch
      StructuralPublicationDigest
      StructuralPublicationDigest
  | StructuralPeerDigestShapeFault StreamShapeProblem
  | StructuralPublicationTranscriptDecodeFailed String
  | StructuralPublicationTranscriptDomainMismatch ByteString
  | StructuralPublicationTranscriptIdentityProblem IdentityError
  | StructuralPublicationTranscriptAuthorityProblem AuthorityEpochCanonicalProblem
  | StructuralPublicationTranscriptValueProblem ValueError
  | StructuralPublicationTranscriptStrengthTagUnknown Word8
  | StructuralPublicationTranscriptFenceDependencyUnsupported Word8
  | StructuralPublicationTranscriptCarrierRoleTagUnknown Word8
  | StructuralPublicationTranscriptNonCanonical
  deriving stock (Eq, Show)

-- | Recompute the destination-independent semantic digest claimed by a stamp.
-- The descriptor and checked value must be the exact source of the batch facts.
structuralPublicationDigestFor ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  PublicationBatch ->
  Either PeerPublicationProblem StructuralPublicationDigest
structuralPublicationDigestFor descriptor expectedOccurrence publication batch = do
  validateBatch descriptor expectedOccurrence publication batch
  structuralPublicationDigestForSemantics
    descriptor
    expectedOccurrence
    publication
    batch.sourceProcess
    batch.sourceStrength
    batch.sourceTopologyPrerequisite
    batch.controlPrerequisite

-- | Derive the receiver-independent structural digest before constructing any
-- receiver-qualified batch. This is also the single-member source path, where
-- there is deliberately no remote batch from which to recover the semantic
-- transcript.
structuralPublicationDigestForSemantics ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  ProcessEpochId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  Either PeerPublicationProblem StructuralPublicationDigest
structuralPublicationDigestForSemantics
  descriptor
  expectedOccurrence
  publication
  sourceProcess
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite = do
    bytes <-
      structuralPublicationCanonicalBytesForSemantics
        descriptor
        expectedOccurrence
        publication
        sourceProcess
        sourceStrength
        sourceTopologyPrerequisite
        controlPrerequisite
    structuralDigestInvariant (SHA256.hash bytes)

-- | The destination-free semantic transcript authenticated by a structural
-- occurrence stamp. Terminal-source repair carries these exact bytes; remote
-- destination batches are deliberately not part of this transcript.
structuralPublicationCanonicalBytesForSemantics ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  ProcessEpochId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  Either PeerPublicationProblem ByteString
structuralPublicationCanonicalBytesForSemantics
  descriptor
  expectedOccurrence
  publication
  sourceProcess
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite = do
    let descriptorSort = descriptorSortId descriptor
        publicationSort = checkedPublicationSort publication
    unless
      (descriptorSort == publicationSort)
      (Left (DescriptorPublicationSortMismatch descriptorSort publicationSort))
    carrierRole <- structuralRoleFor descriptor
    when
      (carrierRole == ProcessEpochCarrier)
      (Left StructuralProcessEpochUnsupported)
    pure
      ( Serialize.encode
          ( structuralPublicationTranscript
              publication
              sourceProcess
              expectedOccurrence
              sourceStrength
              sourceTopologyPrerequisite
              controlPrerequisite
              carrierRole
          )
      )

-- | The complete receiver-independent meaning authenticated by a structural
-- occurrence stamp.  This checked carrier is the only inverse of the private
-- transcript used by terminal-source repair; callers still have to resolve
-- its sort and authority claims against their serialized owner state.
data StructuralPublicationSemantics = StructuralPublicationSemantics
  { publicationId :: PublicationId,
    sourceProcess :: ProcessEpochId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    canonicalValue :: CanonicalValueBytes,
    sourceStrength :: ReplicaStrength,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex,
    carrierRole :: StructuralCarrierRole
  }
  deriving stock (Eq, Show)

structuralPublicationSemanticsPublicationId ::
  StructuralPublicationSemantics -> PublicationId
structuralPublicationSemanticsPublicationId semantics = semantics.publicationId

structuralPublicationSemanticsSourceProcess ::
  StructuralPublicationSemantics -> ProcessEpochId
structuralPublicationSemanticsSourceProcess semantics = semantics.sourceProcess

structuralPublicationSemanticsSortId :: StructuralPublicationSemantics -> SortId
structuralPublicationSemanticsSortId semantics = semantics.sortId

structuralPublicationSemanticsOccurrenceId ::
  StructuralPublicationSemantics -> SortDefinitionOccurrenceId
structuralPublicationSemanticsOccurrenceId semantics = semantics.occurrenceId

structuralPublicationSemanticsCanonicalValue ::
  StructuralPublicationSemantics -> CanonicalValueBytes
structuralPublicationSemanticsCanonicalValue semantics = semantics.canonicalValue

structuralPublicationSemanticsSourceStrength ::
  StructuralPublicationSemantics -> ReplicaStrength
structuralPublicationSemanticsSourceStrength semantics = semantics.sourceStrength

structuralPublicationSemanticsSourceTopologyPrerequisite ::
  StructuralPublicationSemantics -> TopologyCutId
structuralPublicationSemanticsSourceTopologyPrerequisite semantics =
  semantics.sourceTopologyPrerequisite

structuralPublicationSemanticsControlPrerequisite ::
  StructuralPublicationSemantics -> ControlIndex
structuralPublicationSemanticsControlPrerequisite semantics = semantics.controlPrerequisite

structuralPublicationSemanticsCarrierRole ::
  StructuralPublicationSemantics -> StructuralCarrierRole
structuralPublicationSemanticsCarrierRole semantics = semantics.carrierRole

-- | Parse and re-admit the exact private structural transcript.  Generic
-- @cereal@ decoding is only the first step: every nominal field, the nested
-- authority/value encoding, closed tags, and byte-for-byte canonical form are
-- checked before the semantic carrier escapes.
decodeStructuralPublicationCanonicalBytes ::
  ByteString -> Either PeerPublicationProblem StructuralPublicationSemantics
decodeStructuralPublicationCanonicalBytes supplied = do
  transcript <-
    either
      (Left . StructuralPublicationTranscriptDecodeFailed)
      Right
      (Serialize.decode supplied)
  semantics <- admitStructuralPublicationTranscript transcript
  unless
    (structuralPublicationSemanticsCanonicalBytes semantics == supplied)
    (Left StructuralPublicationTranscriptNonCanonical)
  Right semantics

admitStructuralPublicationTranscript ::
  StructuralPublicationTranscript ->
  Either PeerPublicationProblem StructuralPublicationSemantics
admitStructuralPublicationTranscript
  ( StructuralPublicationTranscript
      domain
      (PublicationIdTranscript nablaBytes authorityBytes sequenceNumber sourceBytes)
      processBytes
      sortBytes
      occurrenceBytes
      valueBytes
      sourceStrengthTag
      topologyBytes
      rawControlPrerequisite
      fenceTag
      carrierTag
    ) = do
    unless
      (domain == "ECLIPS-STRUCTURAL-PUBLICATION")
      (Left (StructuralPublicationTranscriptDomainMismatch domain))
    nabla <- admittedIdentity (mkNablaId nablaBytes)
    authority <-
      either
        (Left . StructuralPublicationTranscriptAuthorityProblem)
        Right
        (decodeAuthorityEpochCanonicalBytes authorityBytes)
    source <- admittedIdentity (mkHeraldEpoch sourceBytes)
    sourceProcess <- admittedIdentity (mkProcessEpochId processBytes)
    sortId <- admittedIdentity (mkSortId sortBytes)
    occurrenceId <- admittedIdentity (mkSortDefinitionOccurrenceId occurrenceBytes)
    value <-
      either
        (Left . StructuralPublicationTranscriptValueProblem)
        Right
        (decodeCanonicalValue valueBytes)
    sourceStrength <- case sourceStrengthTag of
      0 -> Right Weak
      1 -> Right Normal
      unknown -> Left (StructuralPublicationTranscriptStrengthTagUnknown unknown)
    sourceTopologyPrerequisite <- admittedIdentity (mkTopologyCutId topologyBytes)
    unless
      (fenceTag == noFenceDependencyTag)
      (Left (StructuralPublicationTranscriptFenceDependencyUnsupported fenceTag))
    carrierRole <- case carrierTag of
      0 -> Right NeutralVertexCarrier
      1 -> Right EdgeCarrier
      2 -> Right NablaCarrier
      3 -> Right DeltaCarrier
      4 -> Left StructuralProcessEpochUnsupported
      unknown -> Left (StructuralPublicationTranscriptCarrierRoleTagUnknown unknown)
    Right
      StructuralPublicationSemantics
        { publicationId =
            publicationId
              nabla
              authority
              source
              (nablaSequence sequenceNumber),
          sourceProcess,
          sortId,
          occurrenceId,
          canonicalValue = canonicalValueBytes value,
          sourceStrength,
          sourceTopologyPrerequisite,
          controlPrerequisite = controlIndex rawControlPrerequisite,
          carrierRole
        }
    where
      admittedIdentity =
        either
          (Left . StructuralPublicationTranscriptIdentityProblem)
          Right

structuralPublicationSemanticsCanonicalBytes ::
  StructuralPublicationSemantics -> ByteString
structuralPublicationSemanticsCanonicalBytes semantics =
  Serialize.encode
    ( structuralPublicationTranscriptFromFacts
        semantics.publicationId
        semantics.sourceProcess
        semantics.sortId
        semantics.occurrenceId
        semantics.canonicalValue
        semantics.sourceStrength
        semantics.sourceTopologyPrerequisite
        semantics.controlPrerequisite
        semantics.carrierRole
    )

mkOrdinaryPeerPublication ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  PublicationBatch ->
  Either PeerPublicationProblem PeerPublication
mkOrdinaryPeerPublication descriptor expectedOccurrence publication batch = do
  validateBatch descriptor expectedOccurrence publication batch
  case descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor) of
    Nothing -> Right (OrdinaryPublication batch)
    Just ProcessEpochCarrier -> do
      _ <- structuralRoleFor descriptor
      Right (OrdinaryPublication batch)
    Just role -> Left (OrdinaryStructuralCarrier role)

mkStructuralPeerPublication ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  Either PeerPublicationProblem PeerPublication
mkStructuralPeerPublication descriptor expectedOccurrence publication stamp batch = do
  expectedDigest <-
    structuralPublicationDigestFor
      descriptor
      expectedOccurrence
      publication
      batch
  carrierRole <- structuralRoleFor descriptor
  unless
    (stamp.publication == batch.identifier)
    ( Left
        ( StructuralStampPublicationMismatch
            batch.identifier
            stamp.publication
        )
    )
  unless
    (stamp.carrierRole == carrierRole)
    ( Left
        ( StructuralStampCarrierRoleMismatch
            carrierRole
            stamp.carrierRole
        )
    )
  unless
    (stamp.publicationDigest == expectedDigest)
    ( Left
        ( StructuralStampDigestMismatch
            expectedDigest
            stamp.publicationDigest
        )
    )
  let digestBytes =
        SHA256.hash
          ( Serialize.encode
              (structuralPeerTranscript stamp batch)
          )
  checkedDigest <-
    either
      (Left . StructuralPeerDigestShapeFault)
      Right
      (mkPeerItemDigest digestBytes)
  Right (StructuralPublication stamp batch checkedDigest)

-- | Shape-only ordinary admission at the wire boundary. The peer-input owner
-- authenticates the effective sort/value classification before application.
presentedOrdinaryPeerPublication :: PublicationBatch -> PeerPublication
presentedOrdinaryPeerPublication = OrdinaryPublication

-- | Shape-only structural admission at the wire boundary. This validates all
-- facts available without the destination's effective sort registry.
presentedStructuralPeerPublication ::
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  Either PeerPublicationProblem PeerPublication
presentedStructuralPeerPublication stamp batch = do
  unless
    (stamp.publication == batch.identifier)
    ( Left
        ( StructuralStampPublicationMismatch
            batch.identifier
            stamp.publication
        )
    )
  when
    (stamp.carrierRole == ProcessEpochCarrier)
    (Left StructuralProcessEpochUnsupported)
  checkedDigest <- structuralPeerDigest stamp batch
  Right (StructuralPublication stamp batch checkedDigest)

authenticatePeerPublication ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  PeerPublication ->
  Either PeerPublicationProblem PeerPublication
authenticatePeerPublication descriptor expectedOccurrence publication item =
  case item of
    OrdinaryPublication batch ->
      mkOrdinaryPeerPublication descriptor expectedOccurrence publication batch
    StructuralPublication stamp batch _ ->
      mkStructuralPeerPublication
        descriptor
        expectedOccurrence
        publication
        stamp
        batch

peerPublicationKind :: PeerPublication -> PeerPublicationKind
peerPublicationKind item = case item of
  OrdinaryPublication _ -> OrdinaryPublicationKind
  StructuralPublication {} -> StructuralPublicationKind

peerPublicationBatch :: PeerPublication -> PublicationBatch
peerPublicationBatch item = case item of
  OrdinaryPublication batch -> batch
  StructuralPublication _ batch _ -> batch

peerPublicationStructuralStamp ::
  PeerPublication ->
  Maybe StructuralOccurrenceStamp
peerPublicationStructuralStamp item = case item of
  OrdinaryPublication _ -> Nothing
  StructuralPublication stamp _ _ -> Just stamp

-- | The exact destination-free semantic transcript retained by an
-- authenticated structural peer publication. Receiver-qualified destinations
-- and stream coordinates deliberately do not enter these bytes, so a survivor
-- can relay them after the source Herald has been retired.
peerPublicationStructuralCanonicalBytes ::
  PeerPublication -> Maybe ByteString
peerPublicationStructuralCanonicalBytes item = case item of
  OrdinaryPublication _ -> Nothing
  StructuralPublication stamp batch _ ->
    Just
      ( Serialize.encode
          ( structuralPublicationTranscriptFromBatch
              batch
              (structuralOccurrenceStampCarrierRole stamp)
          )
      )

peerPublicationDigest :: PeerPublication -> PeerItemDigest
peerPublicationDigest item = case item of
  OrdinaryPublication batch -> batch.ordinaryDigest
  StructuralPublication _ _ digest -> digest

peerPublicationItem :: PeerPublication -> PeerItem PeerPublication
peerPublicationItem item = peerItem (peerPublicationDigest item) item

structuralPeerDigest ::
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  Either PeerPublicationProblem PeerItemDigest
structuralPeerDigest stamp batch =
  either
    (Left . StructuralPeerDigestShapeFault)
    Right
    ( mkPeerItemDigest
        ( SHA256.hash
            (Serialize.encode (structuralPeerTranscript stamp batch))
        )
    )

structuralDigestInvariant ::
  ByteString -> Either PeerPublicationProblem StructuralPublicationDigest
structuralDigestInvariant bytes =
  case mkStructuralPublicationDigest bytes of
    Right digest -> Right digest
    Left problem ->
      error ("SHA-256 structural digest invariant: " <> show problem)

validateBatch ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  PublicationBatch ->
  Either PeerPublicationProblem ()
validateBatch descriptor expectedOccurrence publication batch = do
  let descriptorSort = descriptorSortId descriptor
      publicationSort = checkedPublicationSort publication
      publicationIdentifier = checkedPublicationId publication
  unless
    (descriptorSort == publicationSort)
    ( Left
        ( DescriptorPublicationSortMismatch
            descriptorSort
            publicationSort
        )
    )
  unless
    (publicationSort == batch.sortId)
    ( Left
        ( BatchPublicationSortMismatch
            publicationSort
            batch.sortId
        )
    )
  unless
    (expectedOccurrence == batch.occurrenceId)
    ( Left
        ( BatchDefinitionOccurrenceMismatch
            expectedOccurrence
            batch.occurrenceId
        )
    )
  unless
    (publicationIdentifier == batch.identifier)
    ( Left
        ( BatchPublicationIdentityMismatch
            publicationIdentifier
            batch.identifier
        )
    )
  unless
    (checkedPublicationCanonicalValue publication == batch.canonicalValue)
    (Left BatchCanonicalValueMismatch)

structuralRoleFor ::
  CanonicalDescriptor ->
  Either PeerPublicationProblem StructuralCarrierRole
structuralRoleFor descriptor = do
  role <-
    maybe
      (Left StructuralCarrierMissing)
      Right
      (descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor))
  unless
    ( descriptor
        == predefinedCatalogueDescriptor
          (profileEntryFor (predefinedRoleForCarrier role))
    )
    (Left (StructuralDescriptorNotExactPredefined role))
  Right role

predefinedRoleForCarrier :: StructuralCarrierRole -> PredefinedSortRole
predefinedRoleForCarrier role = case role of
  NeutralVertexCarrier -> NeutralVertexRole
  EdgeCarrier -> EdgeRole
  NablaCarrier -> NablaRole
  DeltaCarrier -> DeltaRole
  ProcessEpochCarrier -> ProcessEpochRole

data StructuralPublicationTranscript
  = StructuralPublicationTranscript
      ByteString
      PublicationIdTranscript
      ByteString
      ByteString
      ByteString
      ByteString
      Word8
      ByteString
      Word64
      Word8
      Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

data StructuralPeerTranscript
  = StructuralPeerTranscript
      ByteString
      StructuralStampTranscript
      PublicationBatchTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data StructuralStampTranscript
  = StructuralStampTranscript
      ByteString
      Word64
      [StructuralVectorEntryTranscript]
      PublicationIdTranscript
      ByteString
      Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

data StructuralVectorEntryTranscript
  = StructuralVectorEntryTranscript
      ByteString
      Word8
      Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

structuralPublicationTranscript ::
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  StructuralCarrierRole ->
  StructuralPublicationTranscript
structuralPublicationTranscript
  publication
  sourceProcess
  occurrence
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  carrierRole =
    structuralPublicationTranscriptFromFacts
      (checkedPublicationId publication)
      sourceProcess
      (checkedPublicationSort publication)
      occurrence
      (checkedPublicationCanonicalValue publication)
      sourceStrength
      sourceTopologyPrerequisite
      controlPrerequisite
      carrierRole

structuralPublicationTranscriptFromBatch ::
  PublicationBatch ->
  StructuralCarrierRole ->
  StructuralPublicationTranscript
structuralPublicationTranscriptFromBatch batch =
  structuralPublicationTranscriptFromFacts
    batch.identifier
    batch.sourceProcess
    batch.sortId
    batch.occurrenceId
    batch.canonicalValue
    batch.sourceStrength
    batch.sourceTopologyPrerequisite
    batch.controlPrerequisite

structuralPublicationTranscriptFromFacts ::
  PublicationId ->
  ProcessEpochId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  StructuralCarrierRole ->
  StructuralPublicationTranscript
structuralPublicationTranscriptFromFacts
  publication
  sourceProcess
  sortId
  occurrence
  canonicalValue
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  carrierRole =
    StructuralPublicationTranscript
      "ECLIPS-STRUCTURAL-PUBLICATION"
      (publicationIdTranscript publication)
      (processEpochIdBytes sourceProcess)
      (sortIdBytes sortId)
      (sortDefinitionOccurrenceIdBytes occurrence)
      (canonicalValueByteString canonicalValue)
      (strengthTag sourceStrength)
      (topologyCutIdBytes sourceTopologyPrerequisite)
      (controlIndexWord64 controlPrerequisite)
      noFenceDependencyTag
      (structuralCarrierRoleTag carrierRole)

structuralPeerTranscript ::
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  StructuralPeerTranscript
structuralPeerTranscript stamp batch =
  StructuralPeerTranscript
    "ECLIPS-PEER-STRUCTURAL-PUBLICATION-BATCH"
    (structuralStampTranscript stamp)
    ( publicationBatchTranscript
        batch.identifier
        batch.sourceProcess
        batch.sortId
        batch.occurrenceId
        batch.canonicalValue
        batch.sourceStrength
        batch.sourceTopologyPrerequisite
        batch.controlPrerequisite
        batch.destinations
    )

structuralStampTranscript ::
  StructuralOccurrenceStamp ->
  StructuralStampTranscript
structuralStampTranscript stamp =
  StructuralStampTranscript
    (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch stamp.occurrence))
    ( structuralSequenceWord64
        (structuralOccurrenceSourceSequence stamp.occurrence)
    )
    (fmap structuralVectorEntryTranscript (structuralVersionVectorEntries stamp.predecessor))
    (publicationIdTranscript stamp.publication)
    (structuralPublicationDigestBytes stamp.publicationDigest)
    (structuralCarrierRoleTag stamp.carrierRole)

structuralVectorEntryTranscript ::
  (HeraldEpoch, StructuralPrefix) ->
  StructuralVectorEntryTranscript
structuralVectorEntryTranscript (herald, prefix) =
  case structuralPrefixSequence prefix of
    Nothing ->
      StructuralVectorEntryTranscript
        (heraldEpochBytes herald)
        emptyStructuralPrefixTag
        0
    Just sequenceNumber ->
      StructuralVectorEntryTranscript
        (heraldEpochBytes herald)
        structuralPrefixThroughTag
        (structuralSequenceWord64 sequenceNumber)

emptyStructuralPrefixTag, structuralPrefixThroughTag :: Word8
emptyStructuralPrefixTag = 0
structuralPrefixThroughTag = 1

normalizeDestinations ::
  NonEmpty PublicationDestination ->
  Either PublicationBatchProblem (NonEmpty PublicationDestination)
normalizeDestinations (first :| remaining) =
  foldl insertDestination (Right (first :| [])) remaining
  where
    insertDestination ::
      Either PublicationBatchProblem (NonEmpty PublicationDestination) ->
      PublicationDestination ->
      Either PublicationBatchProblem (NonEmpty PublicationDestination)
    insertDestination accumulated candidate = do
      current <- accumulated
      insertOrdered candidate current

    insertOrdered ::
      PublicationDestination ->
      NonEmpty PublicationDestination ->
      Either PublicationBatchProblem (NonEmpty PublicationDestination)
    insertOrdered candidate (firstCurrent :| remainingCurrent)
      | candidate.delta < firstCurrent.delta =
          Right (candidate :| (firstCurrent : remainingCurrent))
      | candidate.delta == firstCurrent.delta =
          if candidate == firstCurrent
            then Right (firstCurrent :| remainingCurrent)
            else
              Left
                ( ConflictingPublicationDestination
                    candidate.delta
                    firstCurrent
                    candidate
                )
      | otherwise =
          (firstCurrent :|) <$> insertTail candidate remainingCurrent

    insertTail ::
      PublicationDestination ->
      [PublicationDestination] ->
      Either PublicationBatchProblem [PublicationDestination]
    insertTail candidate [] = Right [candidate]
    insertTail candidate (current : tailDestinations)
      | candidate.delta < current.delta =
          Right (candidate : current : tailDestinations)
      | candidate.delta == current.delta =
          if candidate == current
            then Right (current : tailDestinations)
            else
              Left
                ( ConflictingPublicationDestination
                    candidate.delta
                    current
                    candidate
                )
      | otherwise = (current :) <$> insertTail candidate tailDestinations

data PublicationBatchTranscript
  = PublicationBatchTranscript
      ByteString
      PublicationIdTranscript
      ByteString
      ByteString
      ByteString
      ByteString
      Word8
      ByteString
      Word64
      Word8
      [PublicationDestinationTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PublicationIdTranscript
  = PublicationIdTranscript
      ByteString
      ByteString
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PublicationDestinationTranscript
  = PublicationDestinationTranscript
      ByteString
      ByteString
      Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

publicationBatchTranscript ::
  PublicationId ->
  ProcessEpochId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  PublicationBatchTranscript
publicationBatchTranscript
  identifier
  sourceProcess
  sortId
  occurrenceId
  canonicalValue
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  destinations =
    PublicationBatchTranscript
      "ECLIPS-PEER-PUBLICATION-BATCH"
      (publicationIdTranscript identifier)
      (processEpochIdBytes sourceProcess)
      (sortIdBytes sortId)
      (sortDefinitionOccurrenceIdBytes occurrenceId)
      (canonicalValueByteString canonicalValue)
      (strengthTag sourceStrength)
      (topologyCutIdBytes sourceTopologyPrerequisite)
      (controlIndexWord64 controlPrerequisite)
      noFenceDependencyTag
      (fmap destinationTranscript (toList destinations))

publicationIdTranscript :: PublicationId -> PublicationIdTranscript
publicationIdTranscript identifier =
  PublicationIdTranscript
    (nablaIdBytes (publicationNabla identifier))
    (authorityEpochCanonicalBytes (publicationAuthorityEpoch identifier))
    (nablaSequenceWord64 (publicationNablaSequence identifier))
    (heraldEpochBytes (publicationSourceHeraldEpoch identifier))

destinationTranscript ::
  PublicationDestination ->
  PublicationDestinationTranscript
destinationTranscript destination =
  PublicationDestinationTranscript
    (deltaIdBytes destination.delta)
    (storeIncarnationIdBytes destination.storeIncarnation)
    (strengthTag destination.strength)

strengthTag :: ReplicaStrength -> Word8
strengthTag Weak = 0
strengthTag Normal = 1

noFenceDependencyTag :: Word8
noFenceDependencyTag = 0

toList :: NonEmpty value -> [value]
toList (first :| remaining) = first : remaining
