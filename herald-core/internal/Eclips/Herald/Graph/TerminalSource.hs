{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure canonical algebra for closing retired structural sources jointly.
--
-- Inventories advertise only occurrence identities and semantic digests.  The
-- separately retained payload archive contains complete destination-free
-- occurrences, so repair never turns an inventory digest into an instruction
-- to reconstruct receiver-local publication destinations.
module Eclips.Herald.Graph.TerminalSource
  ( TerminalStructuralOccurrence,
    terminalStructuralOccurrence,
    terminalStructuralOccurrenceStamp,
    terminalStructuralOccurrenceId,
    terminalStructuralOccurrencePublicationDigest,
    terminalStructuralOccurrencePayloadBytes,
    terminalStructuralOccurrenceCanonicalBytes,
    decodeTerminalStructuralOccurrenceCanonicalBytes,
    deriveTerminalStructuralPayloadDigest,
    TerminalSourceOccurrenceClaim,
    terminalSourceOccurrenceClaim,
    terminalSourceOccurrenceClaimId,
    terminalSourceOccurrenceClaimDigest,
    TerminalSourceInventoryDigest,
    mkTerminalSourceInventoryDigest,
    terminalSourceInventoryDigestBytes,
    TerminalSourceInventory,
    terminalSourceInventory,
    terminalSourceInventoryWithClaimedPrefix,
    sealTerminalSourceInventory,
    terminalSourceInventoryPredecessorGenerationId,
    terminalSourceInventorySuccessorGenerationId,
    terminalSourceInventoryRetiredSource,
    terminalSourceInventoryReporter,
    terminalSourceInventoryEstablishedChain,
    terminalSourceInventoryEstablishedBases,
    terminalSourceInventoryRetainedClaims,
    terminalSourceInventoryRetainedEvidence,
    retainTerminalSourceInventoryEvidence,
    deriveTerminalSourceEvidencePayloadRequests,
    terminalSourceEvidencePayloadRelay,
    acceptTerminalSourceEvidencePayloadRelay,
    terminalSourceInventoryContiguousPrefix,
    terminalSourceInventoryClaims,
    terminalSourceInventoryCanonicalBytes,
    terminalSourceInventoryDigest,
    decodeTerminalSourceInventoryCanonicalBytes,
    TerminalSourcePayloadArchive,
    emptyTerminalSourcePayloadArchive,
    terminalSourcePayloadArchive,
    retainTerminalSourcePayload,
    lookupTerminalSourcePayload,
    terminalSourcePayloadEntries,
    TerminalSourcePredecessorBase,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInstalledPredecessorBase,
    terminalSourcePredecessorBaseCutId,
    terminalSourcePredecessorBaseVector,
    TerminalSourcePayloadRequest,
    deriveTerminalSourcePayloadRequests,
    terminalSourcePayloadRequestOccurrence,
    terminalSourcePayloadRequestSupportingInventory,
    terminalSourcePayloadRequestPredecessorGenerationId,
    terminalSourcePayloadRequestSuccessorGenerationId,
    terminalSourcePayloadRequestRetiredSource,
    terminalSourcePayloadRequestFromClaimedCoordinates,
    terminalSourcePayloadRequestCanonicalBytes,
    TerminalSourcePayloadRelay,
    terminalSourcePayloadRelay,
    acceptTerminalSourcePayloadRelay,
    terminalSourcePayloadRelayHerald,
    terminalSourcePayloadRelayOccurrence,
    terminalSourcePayloadRelayPredecessorGenerationId,
    terminalSourcePayloadRelaySuccessorGenerationId,
    terminalSourcePayloadRelayRetiredSource,
    terminalSourcePayloadRelaySupportingInventory,
    terminalSourcePayloadRelayCanonicalBytes,
    decodeTerminalSourcePayloadRelayCanonicalBytes,
    TerminalSourceUnion,
    deriveTerminalSourceUnion,
    deriveTerminalSourceUnionChecked,
    terminalSourceUnionPredecessorGenerationId,
    terminalSourceUnionSuccessorGenerationId,
    terminalSourceUnionRetiredSources,
    terminalSourceUnionGenerationLineage,
    terminalSourceUnionPredecessorCutId,
    terminalSourceUnionInventoryDigests,
    terminalSourceUnionClosedPrefixes,
    terminalSourceUnionIncludedClaims,
    terminalSourceUnionAheadClaims,
    terminalSourceUnionTerminalPredecessorVector,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTerminalTopologyOccurrenceDigest,
    terminalSourceUnionCanonicalBytes,
    terminalSourceUnionDigest,
    decodeTerminalSourceUnionCanonicalBytes,
    terminalSourceUnionTopologyPredecessor,
    compareTerminalSourceUnions,
    TerminalSourceUnionAnnounce,
    terminalSourceUnionAnnouncer,
    terminalSourceUnionAnnounce,
    terminalSourceUnionAnnounceAnnouncer,
    terminalSourceUnionAnnounceUnion,
    terminalSourceUnionAnnounceCanonicalBytes,
    terminalSourceUnionAnnounceFromClaimedCoordinates,
    TerminalSourceUnionReady,
    terminalSourceUnionReady,
    terminalSourceUnionReadyUnion,
    TerminalSourceAcceptanceDigest,
    mkTerminalSourceAcceptanceDigest,
    terminalSourceAcceptanceDigestBytes,
    TerminalSourceUnionAcceptance,
    terminalSourceUnionAcceptance,
    validateTerminalSourceUnionAcceptance,
    terminalSourceUnionAcceptanceReporter,
    terminalSourceUnionAcceptancePredecessorGenerationId,
    terminalSourceUnionAcceptanceSuccessorGenerationId,
    terminalSourceUnionAcceptanceRetiredSources,
    terminalSourceUnionAcceptanceUnionDigest,
    terminalSourceUnionAcceptanceDigest,
    terminalSourceUnionAcceptanceCanonicalBytes,
    terminalSourceUnionAcceptanceFromClaimedCoordinates,
    TerminalSourceUnionEstablished,
    terminalSourceUnionEstablished,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionEstablishedAcceptances,
    terminalSourceUnionEstablishedCanonicalBytes,
    decodeTerminalSourceUnionEstablishedCanonicalBytes,
    terminalSourceUnionEstablishedFromClaimedCoordinates,
    SuccessorStructuralBase,
    establishedSuccessorStructuralBase,
    successorStructuralBaseGenerationId,
    successorStructuralBaseLineage,
    successorStructuralBaseCutId,
    successorStructuralBaseEstablishedUnion,
    successorStructuralBaseReleases,
    MembershipBaseClosure (..),
    beginTerminalStructuralCoordinator,
    structuralBaseCoordinatorLocalInventories,
    extendTerminalStructuralCoordinator,
    projectSurvivorLiveAheadVector,
    SuccessorStructuralAdvance,
    successorStructuralAdvanceOccurrence,
    successorStructuralAdvancePredecessor,
    successorStructuralAdvanceVector,
    allocateSuccessorStructuralOccurrence,
    CarriedStampedSurvivor,
    carriedStampedSurvivorStamp,
    carriedStampedSurvivorPredecessor,
    carriedStampedSurvivorVector,
    carryStampedSurvivorOccurrence,
    carriedStampedSurvivorFromAdvance,
    TerminalSourceProblem (..),
  )
where

import Control.Monad (foldM, replicateM, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize.Get qualified as SerializeGet
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    StructuralOccurrenceId,
    TopologyCutId,
    authorityEpochCanonicalBytes,
    controlIndex,
    controlIndexWord64,
    decodeAuthorityEpochCanonicalBytes,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkNablaId,
    mkStructuralSequence,
    mkTopologyCutId,
    nablaIdBytes,
    nablaSequence,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.MemberSet
  ( memberSetDigestBytes,
    mkMemberSetDigest,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipHistory,
    heraldMembershipLineage,
    heraldMembershipLineageFrom,
    heraldMembershipLineageGenerations,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageRetiredHeraldEpochs,
    heraldMembershipLineageTarget,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (..),
    structuralCarrierRoleTag,
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVectorProblem,
    StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    nextAfterStructuralPrefix,
    structuralPredecessorPrefix,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorCovers,
    structuralVersionVectorEntries,
    structuralVersionVectorFromClaimedCoordinates,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Topology
  ( TerminalSourceUnionDigest,
    TopologyCut,
    TopologyOccurrenceDigest,
    TopologyPredecessor,
    TopologyShapeProblem,
    deriveTopologyCutId,
    membershipSuccessorPredecessor,
    mkTerminalSourceUnionDigest,
    mkTopologyOccurrenceDigest,
    terminalSourceUnionDigestBytes,
    topologyCut,
    topologyCutFrontier,
    topologyCutPredecessor,
    topologyFrontier,
    topologyFrontierStructuralVersionVector,
    topologyOccurrenceDigestBytes,
  )
import Eclips.Herald.Graph.Protocol
  ( TopologyCutEstablished,
    decodeTopologyCutEstablishedCanonicalBytes,
    topologyCutEstablishedCanonicalBytes,
    topologyCutEstablishedCut,
  )
import Eclips.Herald.PeerPublication
  ( StructuralOccurrenceStamp,
    StructuralPublicationDigest,
    mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralPublicationDigestBytes,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

-- | One retained closure lineage. An overlapping retirement replaces only the
-- physical agreement attempt; source facts and payloads remain immutable.
data MembershipBaseClosure = MembershipBaseClosure
  { historyLineage :: HeraldMembershipLineage,
    lineage :: HeraldMembershipLineage,
    localHerald :: HeraldEpoch,
    predecessorBase :: TerminalSourcePredecessorBase,
    establishedChain :: [TopologyCutEstablished],
    establishedBases :: [TerminalSourceUnionEstablished],
    localInventories :: Map HeraldEpoch TerminalSourceInventory,
    inventories :: Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventory,
    retainedInventories :: Map TerminalSourceInventoryDigest TerminalSourceInventory,
    payloads :: TerminalSourcePayloadArchive,
    agreedUnion :: Maybe TerminalSourceUnion,
    acceptances :: Map HeraldEpoch TerminalSourceUnionAcceptance,
    established :: Maybe TerminalSourceUnionEstablished,
    installedBase :: Maybe SuccessorStructuralBase
  }
  deriving stock (Eq, Show)

beginTerminalStructuralCoordinator ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  HeraldEpoch ->
  TerminalSourcePredecessorBase ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalStructuralOccurrence] ->
  Either TerminalSourceProblem MembershipBaseClosure
beginTerminalStructuralCoordinator historyLineage lineage local predecessorBase establishedChain establishedBases retained = do
  validateHistoryLineage historyLineage lineage
  validateClosureLineage lineage
  validateSuccessorMember (heraldMembershipLineageTarget lineage) local
  validateVectorCoordinate (heraldMembershipLineageOrigin lineage) Nothing predecessorBase.vector
  payloads <- terminalSourcePayloadArchive retained
  localInventories <- Map.fromList <$> traverse seal (retiredSourcesFor lineage)
  let inventories = Map.fromList [((origin, local), inventory) | (origin, inventory) <- Map.toAscList localInventories]
  pure
    MembershipBaseClosure
      { historyLineage,
        lineage,
        localHerald = local,
        predecessorBase,
        establishedChain,
        establishedBases,
        localInventories,
        inventories,
        retainedInventories = Map.fromList [(inventory.digest, inventory) | inventory <- Map.elems inventories],
        payloads,
        agreedUnion = Nothing,
        acceptances = Map.empty,
        established = Nothing,
        installedBase = Nothing
      }
  where
    seal origin = do
      let occurrences = filter ((== origin) . structuralOccurrenceSourceHeraldEpoch . terminalStructuralOccurrenceId) retained
      (inventory, _) <- sealTerminalSourceInventory historyLineage lineage origin local establishedChain establishedBases occurrences
      witnessed <- retainTerminalSourceInventoryEvidence [] retained inventory
      pure (origin, witnessed)

-- | Re-seal the survivor's exact retained bytes under a longer retirement
-- lineage. An old digest cannot supply a payload; only the retained archive
-- contributes claims to the new current-survivor inventory.
extendTerminalStructuralCoordinator ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalStructuralOccurrence] ->
  MembershipBaseClosure ->
  Either TerminalSourceProblem MembershipBaseClosure
extendTerminalStructuralCoordinator historyLineage lineage chain bases newlyRetained incumbent = do
  unless
    ( heraldMembershipLineageOrigin lineage == heraldMembershipLineageOrigin incumbent.lineage
        && take (length oldGenerations) newGenerations == oldGenerations
    )
    (Left TerminalSourceClosureLineageMismatch)
  archive <- foldM (flip retainTerminalSourcePayload) incumbent.payloads newlyRetained
  replacement <- beginTerminalStructuralCoordinator historyLineage lineage incumbent.localHerald incumbent.predecessorBase chain bases (fmap snd (terminalSourcePayloadEntries archive))
  localInventories <- traverse (retainTerminalSourceInventoryEvidence (Map.elems incumbent.retainedInventories) (fmap snd (terminalSourcePayloadEntries archive))) replacement.localInventories
  let inventories = Map.fromList [((origin, replacement.localHerald), inventory) | (origin, inventory) <- Map.toAscList localInventories]
  pure
    replacement
      { localInventories,
        inventories,
        retainedInventories = Map.union incumbent.retainedInventories (Map.fromList [(inventory.digest, inventory) | inventory <- Map.elems inventories])
      }
  where
    oldGenerations = NonEmpty.toList (heraldMembershipLineageGenerations incumbent.lineage)
    newGenerations = NonEmpty.toList (heraldMembershipLineageGenerations lineage)

structuralBaseCoordinatorLocalInventories :: MembershipBaseClosure -> [TerminalSourceInventory]
structuralBaseCoordinatorLocalInventories state = Map.elems state.localInventories

retiredSourcesFor :: HeraldMembershipLineage -> [HeraldEpoch]
retiredSourcesFor = heraldMembershipLineageRetiredHeraldEpochs

validateHistoryLineage :: HeraldMembershipLineage -> HeraldMembershipLineage -> Either TerminalSourceProblem ()
validateHistoryLineage history lineage = do
  unless (heraldMembershipGenerationPredecessor (heraldMembershipLineageOrigin history) == Nothing) (Left TerminalSourceClosureLineageMismatch)
  suffix <- either (const (Left TerminalSourceClosureLineageMismatch)) Right (heraldMembershipLineageFrom (heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage)) history)
  unless (suffix == lineage) (Left TerminalSourceClosureLineageMismatch)

validateClosureLineage :: HeraldMembershipLineage -> Either TerminalSourceProblem ()
validateClosureLineage lineage = do
  let removed = Set.fromList (retiredSourcesFor lineage)
      origin = successorMembers (heraldMembershipLineageOrigin lineage)
      target = successorMembers (heraldMembershipLineageTarget lineage)
  unless
    (not (Set.null removed) && target `Set.isSubsetOf` origin && origin `Set.difference` target == removed)
    (Left TerminalSourceClosureLineageMismatch)

digestByteCount :: Int
digestByteCount = 32

-- | A complete, destination-free occurrence retained by the publication owner.
--
-- The opaque payload bytes are the complete canonical semantic occurrence body,
-- not merely its publication digest.  Keeping both the checked stamp and those
-- bytes makes a same-identity/same-digest but unequal-payload repeat observable.
data TerminalStructuralOccurrence = TerminalStructuralOccurrence
  { stamp :: StructuralOccurrenceStamp,
    payloadBytes :: ByteString
  }
  deriving stock (Eq, Show)

terminalStructuralOccurrence ::
  StructuralOccurrenceStamp ->
  ByteString ->
  Either TerminalSourceProblem TerminalStructuralOccurrence
terminalStructuralOccurrence stamp payloadBytes = do
  let identifier = structuralOccurrenceStampOccurrence stamp
      expected = structuralOccurrenceStampPublicationDigest stamp
      actual = deriveTerminalStructuralPayloadDigest payloadBytes
  unless
    (actual == expected)
    ( Left
        ( TerminalSourceOccurrencePayloadDigestMismatch
            identifier
            expected
            actual
        )
    )
  pure TerminalStructuralOccurrence {stamp, payloadBytes}

-- | Derive the stamp digest for the already-canonical destination-free
-- structural semantic transcript retained as a terminal payload.
deriveTerminalStructuralPayloadDigest :: ByteString -> StructuralPublicationDigest
deriveTerminalStructuralPayloadDigest = structuralDigestInvariant . SHA256.hash

terminalStructuralOccurrenceStamp ::
  TerminalStructuralOccurrence -> StructuralOccurrenceStamp
terminalStructuralOccurrenceStamp occurrence = occurrence.stamp

terminalStructuralOccurrenceId ::
  TerminalStructuralOccurrence -> StructuralOccurrenceId
terminalStructuralOccurrenceId =
  structuralOccurrenceStampOccurrence . terminalStructuralOccurrenceStamp

terminalStructuralOccurrencePublicationDigest ::
  TerminalStructuralOccurrence -> StructuralPublicationDigest
terminalStructuralOccurrencePublicationDigest =
  structuralOccurrenceStampPublicationDigest . terminalStructuralOccurrenceStamp

terminalStructuralOccurrencePayloadBytes ::
  TerminalStructuralOccurrence -> ByteString
terminalStructuralOccurrencePayloadBytes occurrence = occurrence.payloadBytes

terminalStructuralOccurrenceCanonicalBytes ::
  TerminalStructuralOccurrence -> ByteString
terminalStructuralOccurrenceCanonicalBytes occurrence =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-STRUCTURAL-OCCURRENCE"
    putOccurrenceId (terminalStructuralOccurrenceId occurrence)
    putStructuralVector
      (structuralOccurrenceStampPredecessor occurrence.stamp)
    let publication = structuralOccurrenceStampPublication occurrence.stamp
    putSizedBytes (nablaIdBytes (publicationNabla publication))
    putSizedBytes
      (authorityEpochCanonicalBytes (publicationAuthorityEpoch publication))
    putSizedBytes (heraldEpochBytes (publicationSourceHeraldEpoch publication))
    Serialize.putWord64be
      (nablaSequenceWord64 (publicationNablaSequence publication))
    putSizedBytes
      ( structuralPublicationDigestBytes
          (terminalStructuralOccurrencePublicationDigest occurrence)
      )
    Serialize.putWord8
      (structuralCarrierRoleTag (structuralOccurrenceStampCarrierRole occurrence.stamp))
    putSizedBytes occurrence.payloadBytes

-- | Parse the canonical destination-free occurrence transcript without
-- consulting mutable Graph or membership state.  Nominal identities, the
-- claimed vector shape, stamp coherence, and payload digest are all checked;
-- the owning transition still resolves the claimed membership generation.
decodeTerminalStructuralOccurrenceCanonicalBytes ::
  ByteString -> Either TerminalSourceProblem TerminalStructuralOccurrence
decodeTerminalStructuralOccurrenceCanonicalBytes =
  decodeTerminalCanonical
    getTerminalStructuralOccurrence
    terminalStructuralOccurrenceCanonicalBytes

data TerminalSourceOccurrenceClaim = TerminalSourceOccurrenceClaim
  { occurrence :: StructuralOccurrenceId,
    publicationDigest :: StructuralPublicationDigest
  }
  deriving stock (Eq, Ord, Show)

terminalSourceOccurrenceClaim ::
  StructuralOccurrenceId ->
  StructuralPublicationDigest ->
  TerminalSourceOccurrenceClaim
terminalSourceOccurrenceClaim = TerminalSourceOccurrenceClaim

terminalSourceOccurrenceClaimId ::
  TerminalSourceOccurrenceClaim -> StructuralOccurrenceId
terminalSourceOccurrenceClaimId claim = claim.occurrence

terminalSourceOccurrenceClaimDigest ::
  TerminalSourceOccurrenceClaim -> StructuralPublicationDigest
terminalSourceOccurrenceClaimDigest claim = claim.publicationDigest

newtype TerminalSourceInventoryDigest = TerminalSourceInventoryDigest ByteString
  deriving stock (Eq, Ord)

instance Show TerminalSourceInventoryDigest where
  show = renderGroupedHex . terminalSourceInventoryDigestBytes

mkTerminalSourceInventoryDigest ::
  ByteString -> Either TerminalSourceProblem TerminalSourceInventoryDigest
mkTerminalSourceInventoryDigest bytes
  | ByteString.length bytes == digestByteCount =
      Right (TerminalSourceInventoryDigest bytes)
  | otherwise =
      Left
        ( TerminalSourceInventoryDigestWrongByteCount
            digestByteCount
            (ByteString.length bytes)
        )

terminalSourceInventoryDigestBytes ::
  TerminalSourceInventoryDigest -> ByteString
terminalSourceInventoryDigestBytes (TerminalSourceInventoryDigest bytes) = bytes

data TerminalSourceInventory = TerminalSourceInventory
  { predecessorGeneration :: HeraldMembershipGenerationId,
    successorGeneration :: HeraldMembershipGenerationId,
    retiredSource :: HeraldEpoch,
    reporter :: HeraldEpoch,
    establishedChain :: [TopologyCutEstablished],
    establishedBases :: [TerminalSourceUnionEstablished],
    retainedEvidence :: [TerminalSourceInventory],
    retainedClaims :: Map StructuralOccurrenceId StructuralPublicationDigest,
    contiguousPrefix :: StructuralPrefix,
    claims :: Map StructuralOccurrenceId StructuralPublicationDigest,
    canonicalBytes :: ByteString,
    digest :: TerminalSourceInventoryDigest
  }
  deriving stock (Eq, Show)

-- | Every reporter supplies actual established certificates with its frozen
-- claims. Their lineage is checked by the Graph owner before the set is sealed.
terminalSourceInventory ::
  HeraldMembershipLineage ->
  HeraldEpoch ->
  HeraldEpoch ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalSourceOccurrenceClaim] ->
  Either TerminalSourceProblem TerminalSourceInventory
terminalSourceInventory lineage retired reporter establishedChain establishedBases suppliedClaims = do
  validateTransition lineage retired
  validateSuccessorMember (heraldMembershipLineageTarget lineage) reporter
  normalized <- normalizeClaims retired suppliedClaims
  pure
    ( canonicalInventoryFromCoordinates
        (heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage))
        (heraldMembershipGenerationId (heraldMembershipLineageTarget lineage))
        retired
        reporter
        establishedChain
        establishedBases
        []
        Map.empty
        (contiguousPrefixForClaims normalized)
        normalized
    )

terminalSourceInventoryWithClaimedPrefix ::
  HeraldMembershipLineage ->
  HeraldEpoch ->
  HeraldEpoch ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  StructuralPrefix ->
  [TerminalSourceOccurrenceClaim] ->
  Either TerminalSourceProblem TerminalSourceInventory
terminalSourceInventoryWithClaimedPrefix lineage retired reporter chain bases claimed claims = do
  inventory <- terminalSourceInventory lineage retired reporter chain bases claims
  unless
    (claimed == inventory.contiguousPrefix)
    (Left (TerminalSourceInventoryPrefixMismatch inventory.contiguousPrefix claimed))
  pure inventory

sealTerminalSourceInventory ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  HeraldEpoch ->
  HeraldEpoch ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalStructuralOccurrence] ->
  Either TerminalSourceProblem (TerminalSourceInventory, TerminalSourcePayloadArchive)
sealTerminalSourceInventory historyLineage lineage retired reporter chain bases occurrences = do
  validateTransition lineage retired
  validateHistoryLineage historyLineage lineage
  traverse_ (validateOccurrence historyLineage retired) occurrences
  archive <- terminalSourcePayloadArchive occurrences
  inventory <- terminalSourceInventory lineage retired reporter chain bases (fmap claimForOccurrence occurrences)
  pure (inventory, archive)

terminalSourceInventoryPredecessorGenerationId :: TerminalSourceInventory -> HeraldMembershipGenerationId
terminalSourceInventoryPredecessorGenerationId inventory = inventory.predecessorGeneration

terminalSourceInventorySuccessorGenerationId :: TerminalSourceInventory -> HeraldMembershipGenerationId
terminalSourceInventorySuccessorGenerationId inventory = inventory.successorGeneration

terminalSourceInventoryRetiredSource :: TerminalSourceInventory -> HeraldEpoch
terminalSourceInventoryRetiredSource inventory = inventory.retiredSource

terminalSourceInventoryReporter :: TerminalSourceInventory -> HeraldEpoch
terminalSourceInventoryReporter inventory = inventory.reporter

terminalSourceInventoryEstablishedChain :: TerminalSourceInventory -> [TopologyCutEstablished]
terminalSourceInventoryEstablishedChain inventory = inventory.establishedChain

terminalSourceInventoryEstablishedBases :: TerminalSourceInventory -> [TerminalSourceUnionEstablished]
terminalSourceInventoryEstablishedBases inventory = inventory.establishedBases

terminalSourceInventoryRetainedClaims :: TerminalSourceInventory -> [TerminalSourceOccurrenceClaim]
terminalSourceInventoryRetainedClaims inventory = claimsFromMap inventory.retainedClaims

terminalSourceInventoryRetainedEvidence :: TerminalSourceInventory -> [TerminalSourceInventory]
terminalSourceInventoryRetainedEvidence inventory = inventory.retainedEvidence

-- | The current survivor reattests only bytes present in its own archive.
-- Historical inventories preserve provenance but contribute no availability.
retainTerminalSourceInventoryEvidence :: [TerminalSourceInventory] -> [TerminalStructuralOccurrence] -> TerminalSourceInventory -> Either TerminalSourceProblem TerminalSourceInventory
retainTerminalSourceInventoryEvidence evidence occurrences inventory = do
  archive <- terminalSourcePayloadArchive occurrences
  let retained = Map.fromList [(identifier, terminalStructuralOccurrencePublicationDigest occurrence) | (identifier, occurrence) <- terminalSourcePayloadEntries archive]
      normalizedEvidence = Map.elems (Map.fromList [(entry.digest, entry) | entry <- evidence])
  pure (canonicalInventoryFromCoordinates inventory.predecessorGeneration inventory.successorGeneration inventory.retiredSource inventory.reporter inventory.establishedChain inventory.establishedBases normalizedEvidence retained inventory.contiguousPrefix inventory.claims)

terminalSourceInventoryContiguousPrefix :: TerminalSourceInventory -> StructuralPrefix
terminalSourceInventoryContiguousPrefix inventory = inventory.contiguousPrefix

terminalSourceInventoryClaims :: TerminalSourceInventory -> [TerminalSourceOccurrenceClaim]
terminalSourceInventoryClaims inventory = claimsFromMap inventory.claims

terminalSourceInventoryCanonicalBytes :: TerminalSourceInventory -> ByteString
terminalSourceInventoryCanonicalBytes inventory = inventory.canonicalBytes

terminalSourceInventoryDigest :: TerminalSourceInventory -> TerminalSourceInventoryDigest
terminalSourceInventoryDigest inventory = inventory.digest

decodeTerminalSourceInventoryCanonicalBytes :: ByteString -> Either TerminalSourceProblem TerminalSourceInventory
decodeTerminalSourceInventoryCanonicalBytes = decodeTerminalCanonical getTerminalSourceInventory terminalSourceInventoryCanonicalBytes

canonicalInventoryFromCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalSourceInventory] ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  StructuralPrefix ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  TerminalSourceInventory
canonicalInventoryFromCoordinates predecessorId successorId retired reporter chain bases evidence retained prefix claims =
  let bytes = Serialize.runPut $ do
        putSizedBytes "ECLIPS-TERMINAL-SOURCE-INVENTORY"
        putSizedBytes (heraldMembershipGenerationIdBytes predecessorId)
        putSizedBytes (heraldMembershipGenerationIdBytes successorId)
        putSizedBytes (heraldEpochBytes retired)
        putSizedBytes (heraldEpochBytes reporter)
        putList (putSizedBytes . topologyCutEstablishedCanonicalBytes) chain
        putList (putSizedBytes . terminalSourceUnionEstablishedCanonicalBytes) bases
        putList (putSizedBytes . terminalSourceInventoryCanonicalBytes) evidence
        putList putClaim (claimsFromMap retained)
        putStructuralPrefix prefix
        putList putClaim (claimsFromMap claims)
   in TerminalSourceInventory
        { predecessorGeneration = predecessorId,
          successorGeneration = successorId,
          retiredSource = retired,
          reporter,
          establishedChain = chain,
          establishedBases = bases,
          retainedEvidence = evidence,
          retainedClaims = retained,
          contiguousPrefix = prefix,
          claims,
          canonicalBytes = bytes,
          digest = inventoryDigestInvariant (SHA256.hash bytes)
        }

newtype TerminalSourcePayloadArchive
  = TerminalSourcePayloadArchive
      (Map StructuralOccurrenceId TerminalStructuralOccurrence)
  deriving stock (Eq, Show)

emptyTerminalSourcePayloadArchive :: TerminalSourcePayloadArchive
emptyTerminalSourcePayloadArchive = TerminalSourcePayloadArchive Map.empty

terminalSourcePayloadArchive ::
  [TerminalStructuralOccurrence] ->
  Either TerminalSourceProblem TerminalSourcePayloadArchive
terminalSourcePayloadArchive =
  foldM (flip retainTerminalSourcePayload) emptyTerminalSourcePayloadArchive

retainTerminalSourcePayload ::
  TerminalStructuralOccurrence ->
  TerminalSourcePayloadArchive ->
  Either TerminalSourceProblem TerminalSourcePayloadArchive
retainTerminalSourcePayload occurrence (TerminalSourcePayloadArchive retained) =
  let identifier = terminalStructuralOccurrenceId occurrence
   in case Map.lookup identifier retained of
        Nothing ->
          Right
            (TerminalSourcePayloadArchive (Map.insert identifier occurrence retained))
        Just incumbent
          | incumbent == occurrence ->
              Right (TerminalSourcePayloadArchive retained)
          | otherwise ->
              Left
                ( TerminalSourcePayloadConflict
                    identifier
                    (terminalStructuralOccurrenceCanonicalBytes incumbent)
                    (terminalStructuralOccurrenceCanonicalBytes occurrence)
                )

lookupTerminalSourcePayload ::
  StructuralOccurrenceId ->
  TerminalSourcePayloadArchive ->
  Maybe TerminalStructuralOccurrence
lookupTerminalSourcePayload identifier (TerminalSourcePayloadArchive retained) =
  Map.lookup identifier retained

terminalSourcePayloadEntries ::
  TerminalSourcePayloadArchive ->
  [(StructuralOccurrenceId, TerminalStructuralOccurrence)]
terminalSourcePayloadEntries (TerminalSourcePayloadArchive retained) =
  Map.toAscList retained

-- | One owner-observed installed predecessor coordinate.  The genesis form
-- derives its unique empty vector; every dynamic form is constructed from the
-- actual retained 'TopologyCut', so a caller cannot pair one cut identity with
-- another cut's frontier.
data TerminalSourcePredecessorBase = TerminalSourcePredecessorBase
  { cut :: TopologyCutId,
    vector :: StructuralVersionVector
  }
  deriving stock (Eq, Show)

terminalSourceGenesisPredecessorBase ::
  HeraldMembershipGeneration ->
  TopologyCutId ->
  Either TerminalSourceProblem TerminalSourcePredecessorBase
terminalSourceGenesisPredecessorBase generation genesisCut = do
  case heraldMembershipGenerationPredecessor generation of
    Nothing -> pure ()
    Just _ ->
      Left
        ( TerminalSourcePredecessorNotGenesis
            (heraldMembershipGenerationId generation)
        )
  pure
    TerminalSourcePredecessorBase
      { cut = genesisCut,
        vector = emptyStructuralVersionVector generation
      }

terminalSourceInstalledPredecessorBase ::
  HeraldMembershipGeneration ->
  TopologyCut ->
  Either TerminalSourceProblem TerminalSourcePredecessorBase
terminalSourceInstalledPredecessorBase generation installed = do
  let vector =
        topologyFrontierStructuralVersionVector (topologyCutFrontier installed)
  validateVectorCoordinate generation Nothing vector
  pure
    TerminalSourcePredecessorBase
      { cut = deriveTopologyCutId installed,
        vector
      }

terminalSourcePredecessorBaseCutId ::
  TerminalSourcePredecessorBase -> TopologyCutId
terminalSourcePredecessorBaseCutId base = base.cut

terminalSourcePredecessorBaseVector ::
  TerminalSourcePredecessorBase -> StructuralVersionVector
terminalSourcePredecessorBaseVector base = base.vector

data TerminalSourcePayloadRequest = TerminalSourcePayloadRequest
  { predecessorGeneration :: HeraldMembershipGenerationId,
    successorGeneration :: HeraldMembershipGenerationId,
    retiredSource :: HeraldEpoch,
    occurrence :: StructuralOccurrenceId,
    supportingInventory :: TerminalSourceInventoryDigest
  }
  deriving stock (Eq, Show)

-- | Derive one request per missing local payload.  When several inventories
-- support a claim, the least reporting survivor is selected deterministically.
deriveTerminalSourcePayloadRequests ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  HeraldEpoch ->
  [TerminalSourceInventory] ->
  TerminalSourcePayloadArchive ->
  Either TerminalSourceProblem [TerminalSourcePayloadRequest]
deriveTerminalSourcePayloadRequests historyLineage lineage retired supplied archive = do
  validateHistoryLineage historyLineage lineage
  inventories <- normalizeInventoryMap lineage retired supplied
  supported <- foldM addInventorySupports Map.empty (Map.elems inventories)
  concat <$> traverse requestMissing (Map.toAscList supported)
  where
    requestMissing (identifier, (expectedDigest, supporters)) =
      case lookupTerminalSourcePayload identifier archive of
        Just occurrence -> do
          validateOccurrence historyLineage retired occurrence
          unless
            (terminalStructuralOccurrencePublicationDigest occurrence == expectedDigest)
            (Left (TerminalSourcePayloadDigestMismatch identifier expectedDigest (terminalStructuralOccurrencePublicationDigest occurrence)))
          pure []
        Nothing -> case Map.lookupMin supporters of
          Nothing -> error "terminal-source claim lost its supporter"
          Just (_, inventoryDigest) ->
            pure
              [ TerminalSourcePayloadRequest
                  { predecessorGeneration = heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage),
                    successorGeneration = heraldMembershipGenerationId (heraldMembershipLineageTarget lineage),
                    retiredSource = retired,
                    occurrence = identifier,
                    supportingInventory = inventoryDigest
                  }
              ]

-- | Payload repair for an established anchor advertised by a survivor which
-- has already installed a later base. The enclosing current inventory proves
-- availability; embedded historical inventories prove provenance only.
deriveTerminalSourceEvidencePayloadRequests :: HeraldMembershipLineage -> TerminalSourceInventory -> TerminalSourcePayloadArchive -> Either TerminalSourceProblem [TerminalSourcePayloadRequest]
deriveTerminalSourceEvidencePayloadRequests history inventory archive = do
  validateEvidenceInventory history inventory
  claims <- inventoryAvailableClaims inventory
  concat <$> traverse requestMissing (Map.toAscList claims)
  where
    requestMissing (identifier, expected) = case lookupTerminalSourcePayload identifier archive of
      Just occurrence -> do
        validateOccurrence history (structuralOccurrenceSourceHeraldEpoch identifier) occurrence
        unless
          (terminalStructuralOccurrencePublicationDigest occurrence == expected)
          (Left (TerminalSourcePayloadDigestMismatch identifier expected (terminalStructuralOccurrencePublicationDigest occurrence)))
        pure []
      Nothing ->
        pure
          [ TerminalSourcePayloadRequest
              { predecessorGeneration = inventory.predecessorGeneration,
                successorGeneration = inventory.successorGeneration,
                retiredSource = structuralOccurrenceSourceHeraldEpoch identifier,
                occurrence = identifier,
                supportingInventory = inventory.digest
              }
          ]

terminalSourceEvidencePayloadRelay :: HeraldMembershipLineage -> HeraldEpoch -> TerminalSourcePayloadRequest -> TerminalSourceInventory -> TerminalStructuralOccurrence -> Either TerminalSourceProblem TerminalSourcePayloadRelay
terminalSourceEvidencePayloadRelay history suppliedRelay request inventory occurrence = do
  validateEvidenceInventory history inventory
  unless (suppliedRelay == inventory.reporter) (Left (TerminalSourceRelayReporterMismatch inventory.reporter suppliedRelay))
  unless
    (request.predecessorGeneration == inventory.predecessorGeneration && request.successorGeneration == inventory.successorGeneration && request.supportingInventory == inventory.digest)
    (Left (TerminalSourceRequestCoordinateMismatch request.occurrence))
  let identifier = terminalStructuralOccurrenceId occurrence
  unless
    (identifier == request.occurrence && structuralOccurrenceSourceHeraldEpoch identifier == request.retiredSource)
    (Left (TerminalSourceRelayOccurrenceMismatch request.occurrence identifier))
  validateOccurrence history request.retiredSource occurrence
  claims <- inventoryAvailableClaims inventory
  expected <- maybe (Left (TerminalSourceRelayClaimMissing identifier)) Right (Map.lookup identifier claims)
  unless
    (expected == terminalStructuralOccurrencePublicationDigest occurrence)
    (Left (TerminalSourcePayloadDigestMismatch identifier expected (terminalStructuralOccurrencePublicationDigest occurrence)))
  pure
    TerminalSourcePayloadRelay
      { predecessorGeneration = inventory.predecessorGeneration,
        successorGeneration = inventory.successorGeneration,
        retiredSource = request.retiredSource,
        relay = suppliedRelay,
        supportingInventory = inventory.digest,
        occurrence
      }

acceptTerminalSourceEvidencePayloadRelay :: HeraldMembershipLineage -> TerminalSourcePayloadRequest -> TerminalSourceInventory -> TerminalSourcePayloadRelay -> TerminalSourcePayloadArchive -> Either TerminalSourceProblem TerminalSourcePayloadArchive
acceptTerminalSourceEvidencePayloadRelay history request inventory supplied archive = do
  expected <- terminalSourceEvidencePayloadRelay history supplied.relay request inventory supplied.occurrence
  unless (expected == supplied) (Left TerminalSourceRelayEnvelopeMismatch)
  retainTerminalSourcePayload supplied.occurrence archive

validateEvidenceInventory :: HeraldMembershipLineage -> TerminalSourceInventory -> Either TerminalSourceProblem ()
validateEvidenceInventory history inventory = do
  suffix <- either (const (Left TerminalSourceClosureLineageMismatch)) Right (heraldMembershipLineageFrom inventory.predecessorGeneration history)
  validateInventoryCoordinate suffix inventory.retiredSource inventory
  claims <- inventoryAvailableClaims inventory
  unless
    (all (\identifier -> structuralOccurrenceSourceHeraldEpoch identifier `elem` retiredSourcesFor history) (Map.keys claims))
    (Left TerminalSourceClosureLineageMismatch)

inventoryAvailableClaims :: TerminalSourceInventory -> Either TerminalSourceProblem (Map StructuralOccurrenceId StructuralPublicationDigest)
inventoryAvailableClaims inventory = foldM insert inventory.claims (Map.toAscList inventory.retainedClaims)
  where
    insert retained (identifier, digest) = case Map.lookup identifier retained of
      Just incumbent | incumbent /= digest -> Left (TerminalSourceClaimConflict identifier incumbent digest)
      _ -> Right (Map.insert identifier digest retained)

terminalSourcePayloadRequestOccurrence ::
  TerminalSourcePayloadRequest -> StructuralOccurrenceId
terminalSourcePayloadRequestOccurrence request = request.occurrence

terminalSourcePayloadRequestSupportingInventory ::
  TerminalSourcePayloadRequest -> TerminalSourceInventoryDigest
terminalSourcePayloadRequestSupportingInventory request =
  request.supportingInventory

terminalSourcePayloadRequestPredecessorGenerationId ::
  TerminalSourcePayloadRequest -> HeraldMembershipGenerationId
terminalSourcePayloadRequestPredecessorGenerationId request =
  request.predecessorGeneration

terminalSourcePayloadRequestSuccessorGenerationId ::
  TerminalSourcePayloadRequest -> HeraldMembershipGenerationId
terminalSourcePayloadRequestSuccessorGenerationId request =
  request.successorGeneration

terminalSourcePayloadRequestRetiredSource ::
  TerminalSourcePayloadRequest -> HeraldEpoch
terminalSourcePayloadRequestRetiredSource request = request.retiredSource

-- | Context-free materialization of the explicit request coordinates.  The
-- owning structural-base coordinator later resolves both generations and the
-- supporting retained inventory before answering it.
terminalSourcePayloadRequestFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  HeraldEpoch ->
  StructuralOccurrenceId ->
  TerminalSourceInventoryDigest ->
  Either TerminalSourceProblem TerminalSourcePayloadRequest
terminalSourcePayloadRequestFromClaimedCoordinates
  predecessor
  successor
  retired
  occurrence
  supportingInventory = do
    validateDistinctGenerationClaims predecessor successor
    validateRetiredOccurrence retired occurrence
    pure
      TerminalSourcePayloadRequest
        { predecessorGeneration = predecessor,
          successorGeneration = successor,
          retiredSource = retired,
          occurrence,
          supportingInventory
        }

terminalSourcePayloadRequestCanonicalBytes ::
  TerminalSourcePayloadRequest -> ByteString
terminalSourcePayloadRequestCanonicalBytes request =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-SOURCE-PAYLOAD-REQUEST"
    putSizedBytes
      (heraldMembershipGenerationIdBytes request.predecessorGeneration)
    putSizedBytes
      (heraldMembershipGenerationIdBytes request.successorGeneration)
    putSizedBytes (heraldEpochBytes request.retiredSource)
    putOccurrenceId request.occurrence
    putSizedBytes
      (terminalSourceInventoryDigestBytes request.supportingInventory)

data TerminalSourcePayloadRelay = TerminalSourcePayloadRelay
  { predecessorGeneration :: HeraldMembershipGenerationId,
    successorGeneration :: HeraldMembershipGenerationId,
    retiredSource :: HeraldEpoch,
    relay :: HeraldEpoch,
    supportingInventory :: TerminalSourceInventoryDigest,
    occurrence :: TerminalStructuralOccurrence
  }
  deriving stock (Eq, Show)

-- | Check the transport relay against one frozen supporting inventory.  The
-- relay is an active survivor while the embedded occurrence remains authored by
-- the retired source.
terminalSourcePayloadRelay ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  HeraldEpoch ->
  TerminalSourcePayloadRequest ->
  TerminalSourceInventory ->
  TerminalStructuralOccurrence ->
  Either TerminalSourceProblem TerminalSourcePayloadRelay
terminalSourcePayloadRelay historyLineage lineage suppliedRelay request inventory occurrence = do
  validateHistoryLineage historyLineage lineage
  let retired = request.retiredSource
      identifier = terminalStructuralOccurrenceId occurrence
      actualDigest = terminalStructuralOccurrencePublicationDigest occurrence
  validateTransition lineage retired
  validateSuccessorMember (heraldMembershipLineageTarget lineage) suppliedRelay
  validateInventoryCoordinate lineage retired inventory
  validateRequestCoordinate lineage retired request
  unless
    (suppliedRelay == inventory.reporter)
    (Left (TerminalSourceRelayReporterMismatch inventory.reporter suppliedRelay))
  unless
    (request.supportingInventory == inventory.digest)
    (Left (TerminalSourceRelayInventoryMismatch request.supportingInventory inventory.digest))
  unless
    (identifier == request.occurrence)
    (Left (TerminalSourceRelayOccurrenceMismatch request.occurrence identifier))
  validateOccurrence historyLineage retired occurrence
  expectedDigest <- maybe (Left (TerminalSourceRelayClaimMissing identifier)) Right (Map.lookup identifier inventory.claims)
  unless
    (actualDigest == expectedDigest)
    (Left (TerminalSourcePayloadDigestMismatch identifier expectedDigest actualDigest))
  pure
    TerminalSourcePayloadRelay
      { predecessorGeneration = inventory.predecessorGeneration,
        successorGeneration = inventory.successorGeneration,
        retiredSource = retired,
        relay = suppliedRelay,
        supportingInventory = inventory.digest,
        occurrence
      }

acceptTerminalSourcePayloadRelay ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  TerminalSourcePayloadRequest ->
  TerminalSourceInventory ->
  TerminalSourcePayloadRelay ->
  TerminalSourcePayloadArchive ->
  Either TerminalSourceProblem TerminalSourcePayloadArchive
acceptTerminalSourcePayloadRelay historyLineage lineage request inventory relay archive = do
  expected <- terminalSourcePayloadRelay historyLineage lineage relay.relay request inventory relay.occurrence
  unless (expected == relay) (Left TerminalSourceRelayEnvelopeMismatch)
  retainTerminalSourcePayload relay.occurrence archive

terminalSourcePayloadRelayHerald :: TerminalSourcePayloadRelay -> HeraldEpoch
terminalSourcePayloadRelayHerald relay = relay.relay

terminalSourcePayloadRelayOccurrence ::
  TerminalSourcePayloadRelay -> TerminalStructuralOccurrence
terminalSourcePayloadRelayOccurrence relay = relay.occurrence

terminalSourcePayloadRelayPredecessorGenerationId ::
  TerminalSourcePayloadRelay -> HeraldMembershipGenerationId
terminalSourcePayloadRelayPredecessorGenerationId relay =
  relay.predecessorGeneration

terminalSourcePayloadRelaySuccessorGenerationId ::
  TerminalSourcePayloadRelay -> HeraldMembershipGenerationId
terminalSourcePayloadRelaySuccessorGenerationId relay =
  relay.successorGeneration

terminalSourcePayloadRelayRetiredSource ::
  TerminalSourcePayloadRelay -> HeraldEpoch
terminalSourcePayloadRelayRetiredSource relay = relay.retiredSource

terminalSourcePayloadRelaySupportingInventory ::
  TerminalSourcePayloadRelay -> TerminalSourceInventoryDigest
terminalSourcePayloadRelaySupportingInventory relay = relay.supportingInventory

terminalSourcePayloadRelayCanonicalBytes ::
  TerminalSourcePayloadRelay -> ByteString
terminalSourcePayloadRelayCanonicalBytes relay =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-SOURCE-PAYLOAD-RELAY"
    putSizedBytes
      (heraldMembershipGenerationIdBytes relay.predecessorGeneration)
    putSizedBytes (heraldMembershipGenerationIdBytes relay.successorGeneration)
    putSizedBytes (heraldEpochBytes relay.retiredSource)
    putSizedBytes (heraldEpochBytes relay.relay)
    putSizedBytes
      (terminalSourceInventoryDigestBytes relay.supportingInventory)
    putSizedBytes (terminalStructuralOccurrenceCanonicalBytes relay.occurrence)

decodeTerminalSourcePayloadRelayCanonicalBytes ::
  ByteString -> Either TerminalSourceProblem TerminalSourcePayloadRelay
decodeTerminalSourcePayloadRelayCanonicalBytes =
  decodeTerminalCanonical
    getTerminalSourcePayloadRelay
    terminalSourcePayloadRelayCanonicalBytes

terminalSourcePayloadRelayFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  TerminalSourceInventoryDigest ->
  TerminalStructuralOccurrence ->
  Either TerminalSourceProblem TerminalSourcePayloadRelay
terminalSourcePayloadRelayFromClaimedCoordinates
  predecessor
  successor
  retired
  relay
  supportingInventory
  occurrence = do
    validateDistinctGenerationClaims predecessor successor
    validateRetiredOccurrence retired (terminalStructuralOccurrenceId occurrence)
    when
      (relay == retired)
      (Left (TerminalSourceRelayIsRetiredSource relay))
    pure
      TerminalSourcePayloadRelay
        { predecessorGeneration = predecessor,
          successorGeneration = successor,
          retiredSource = retired,
          relay,
          supportingInventory,
          occurrence
        }

data TerminalSourceUnion = TerminalSourceUnion
  { predecessorGeneration :: HeraldMembershipGenerationId,
    successorGeneration :: HeraldMembershipGenerationId,
    generationLineage :: [HeraldMembershipGenerationId],
    retiredSources :: Set HeraldEpoch,
    predecessorCut :: TopologyCutId,
    inventoryDigests :: Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventoryDigest,
    closedPrefixes :: Map HeraldEpoch StructuralPrefix,
    includedClaims :: Map StructuralOccurrenceId StructuralPublicationDigest,
    aheadClaims :: Map StructuralOccurrenceId StructuralPublicationDigest,
    terminalPredecessorVector :: StructuralVersionVector,
    successorInitialVector :: StructuralVersionVector,
    terminalControlPrefix :: ControlIndex,
    terminalTopologyDigest :: TopologyOccurrenceDigest,
    canonicalBytes :: ByteString,
    digest :: TerminalSourceUnionDigest
  }
  deriving stock (Eq, Show)

deriveTerminalSourceUnion ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  TerminalSourcePredecessorBase ->
  [TerminalSourceInventory] ->
  TerminalSourcePayloadArchive ->
  (StructuralVersionVector -> ControlIndex -> TopologyOccurrenceDigest) ->
  Either TerminalSourceProblem TerminalSourceUnion
deriveTerminalSourceUnion historyLineage lineage base inventories payloads reconstruct =
  deriveTerminalSourceUnionChecked
    historyLineage
    lineage
    base
    inventories
    payloads
    (\vector control -> Right (reconstruct vector control))

-- | Start from every available contiguous source prefix and descend to the
-- greatest joint causal fixed point. A missing dependency can shorten another
-- source, which can in turn shorten the first one; presentation order never
-- chooses a smaller valid frontier. All excluded bytes remain in aheadClaims.
deriveTerminalSourceUnionChecked ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  TerminalSourcePredecessorBase ->
  [TerminalSourceInventory] ->
  TerminalSourcePayloadArchive ->
  (StructuralVersionVector -> ControlIndex -> Either TerminalSourceProblem TopologyOccurrenceDigest) ->
  Either TerminalSourceProblem TerminalSourceUnion
deriveTerminalSourceUnionChecked historyLineage lineage predecessorBase supplied payloads reconstruct = do
  validateHistoryLineage historyLineage lineage
  validateClosureLineage lineage
  validateVectorCoordinate predecessor Nothing predecessorBase.vector
  inventories <- exactClosureInventoryMap lineage supplied
  traverse_
    ( \((origin, _), inventory) -> do
        required <- maybe (Left (TerminalSourceVectorMemberSetMismatch Nothing)) Right (structuralVersionVectorComponent origin predecessorBase.vector)
        unless (inventory.contiguousPrefix >= required) (Left (TerminalSourceInventoryBehindPredecessorCut inventory.reporter required inventory.contiguousPrefix))
    )
    (Map.toAscList inventories)
  normalized <- foldM (flip mergeInventoryClaims) Map.empty (Map.elems inventories)
  traverse_ (\(identifier, digest) -> validateClaimPayload historyLineage (structuralOccurrenceSourceHeraldEpoch identifier) payloads (identifier, digest)) (Map.toAscList normalized)
  historicalPrefixes <- sealedHistoricalPrefixes historyLineage lineage supplied
  let contiguous =
        Map.fromList
          [(origin, contiguousPrefixForClaims (Map.filterWithKey (\identifier _ -> structuralOccurrenceSourceHeraldEpoch identifier == origin) normalized)) | origin <- retiredSourcesFor lineage]
      closedPrefixes = jointPrefixes historicalPrefixes payloads normalized contiguous
      included = Map.filterWithKey (\identifier _ -> occurrenceWithin closedPrefixes identifier) normalized
      ahead = normalized `Map.difference` included
  terminalVector <- leastCompoundTerminalVector historyLineage lineage historicalPrefixes predecessorBase.vector closedPrefixes included payloads
  successorVector <- projectSuccessorVector successor terminalVector
  retirementControl <- successorRetirementControl successor
  topologyDigest <- reconstruct terminalVector retirementControl
  let generationLineage = fmap heraldMembershipGenerationId (NonEmpty.toList (heraldMembershipLineageGenerations lineage))
      retired = Set.fromList (retiredSourcesFor lineage)
      inventoryDigests = fmap terminalSourceInventoryDigest inventories
      predecessorId = heraldMembershipGenerationId predecessor
      successorId = heraldMembershipGenerationId successor
      bytes = terminalSourceUnionBytes predecessorId successorId generationLineage retired predecessorBase.cut inventoryDigests closedPrefixes included ahead terminalVector successorVector retirementControl topologyDigest
  pure
    TerminalSourceUnion
      { predecessorGeneration = predecessorId,
        successorGeneration = successorId,
        generationLineage,
        retiredSources = retired,
        predecessorCut = predecessorBase.cut,
        inventoryDigests,
        closedPrefixes,
        includedClaims = included,
        aheadClaims = ahead,
        terminalPredecessorVector = terminalVector,
        successorInitialVector = successorVector,
        terminalControlPrefix = retirementControl,
        terminalTopologyDigest = topologyDigest,
        canonicalBytes = bytes,
        digest = unionDigestInvariant (SHA256.hash bytes)
      }
  where
    predecessor = heraldMembershipLineageOrigin lineage
    successor = heraldMembershipLineageTarget lineage

occurrenceWithin :: Map HeraldEpoch StructuralPrefix -> StructuralOccurrenceId -> Bool
occurrenceWithin prefixes identifier =
  maybe
    False
    (>= structuralPrefixThrough (structuralOccurrenceSourceSequence identifier))
    (Map.lookup (structuralOccurrenceSourceHeraldEpoch identifier) prefixes)

-- Earlier bases are part of the proved anchor chain. Their sealed prefixes
-- bound dependencies whose sources disappeared before this closure's anchor;
-- a membership history alone supplies no such coverage.
sealedHistoricalPrefixes :: HeraldMembershipLineage -> HeraldMembershipLineage -> [TerminalSourceInventory] -> Either TerminalSourceProblem (Map HeraldEpoch StructuralPrefix)
sealedHistoricalPrefixes fullLineage lineage inventories = do
  history <- either (const (Left TerminalSourceClosureLineageMismatch)) Right (heraldMembershipHistory (heraldMembershipLineageGenerations fullLineage))
  let anchor = heraldMembershipLineageOrigin lineage
      anchorId = heraldMembershipGenerationId anchor
      earlier = takeWhile ((/= anchorId) . heraldMembershipGenerationId) (NonEmpty.toList (heraldMembershipLineageGenerations fullLineage)) <> [anchor]
      earlierIds = fmap heraldMembershipGenerationId earlier
      removed = successorMembers (heraldMembershipLineageOrigin fullLineage) `Set.difference` successorMembers anchor
      validateOne inventory established = do
        let union = terminalSourceUnionEstablishedUnion established
        unless (union.successorGeneration `elem` earlierIds) (Left TerminalSourceClosureLineageMismatch)
        priorLineage <- either (const (Left TerminalSourceClosureLineageMismatch)) Right (heraldMembershipLineage union.predecessorGeneration union.successorGeneration history)
        predecessor <- terminalSourceUnionTopologyPredecessor priorLineage union
        cut <- either (Left . TerminalSourceTopologyShapeProblem) Right (topologyCut predecessor (topologyFrontier union.successorInitialVector union.terminalControlPrefix) union.terminalTopologyDigest)
        unless (any ((== cut) . topologyCutEstablishedCut) inventory.establishedChain) (Left TerminalSourceSuccessorCutPredecessorMismatch)
        _ <- establishedSuccessorStructuralBase priorLineage established cut
        pure union.closedPrefixes
  prefixes <- Map.unions <$> traverse (uncurry validateOne) [(inventory, established) | inventory <- inventories, established <- inventory.establishedBases]
  unless (removed `Set.isSubsetOf` Map.keysSet prefixes) (Left TerminalSourceClosureLineageMismatch)
  pure (Map.restrictKeys prefixes removed)

jointPrefixes :: Map HeraldEpoch StructuralPrefix -> TerminalSourcePayloadArchive -> Map StructuralOccurrenceId StructuralPublicationDigest -> Map HeraldEpoch StructuralPrefix -> Map HeraldEpoch StructuralPrefix
jointPrefixes historical payloads claims = fixedPoint
  where
    fixedPoint current =
      let next = foldl shorten current (Map.keys claims)
       in if next == current then current else fixedPoint next
      where
        shorten retained identifier
          | not (occurrenceWithin current identifier) = retained
          | otherwise = case lookupTerminalSourcePayload identifier payloads of
              Nothing -> error "joint terminal prefix lost its checked payload"
              Just occurrence ->
                let dependencies = structuralVersionVectorEntries (structuralOccurrenceStampPredecessor occurrence.stamp)
                    covered (source, required) = maybe True (>= required) (Map.lookup source (Map.union current historical))
                 in if all covered dependencies
                      then retained
                      else Map.adjust (min (structuralPredecessorPrefix (structuralOccurrenceSourceSequence identifier))) (structuralOccurrenceSourceHeraldEpoch identifier) retained

terminalSourceUnionPredecessorGenerationId :: TerminalSourceUnion -> HeraldMembershipGenerationId
terminalSourceUnionPredecessorGenerationId union = union.predecessorGeneration

terminalSourceUnionSuccessorGenerationId :: TerminalSourceUnion -> HeraldMembershipGenerationId
terminalSourceUnionSuccessorGenerationId union = union.successorGeneration

terminalSourceUnionGenerationLineage :: TerminalSourceUnion -> [HeraldMembershipGenerationId]
terminalSourceUnionGenerationLineage union = union.generationLineage

terminalSourceUnionRetiredSources :: TerminalSourceUnion -> Set HeraldEpoch
terminalSourceUnionRetiredSources union = union.retiredSources

terminalSourceUnionPredecessorCutId :: TerminalSourceUnion -> TopologyCutId
terminalSourceUnionPredecessorCutId union = union.predecessorCut

terminalSourceUnionInventoryDigests :: TerminalSourceUnion -> [((HeraldEpoch, HeraldEpoch), TerminalSourceInventoryDigest)]
terminalSourceUnionInventoryDigests union = Map.toAscList union.inventoryDigests

terminalSourceUnionClosedPrefixes :: TerminalSourceUnion -> [(HeraldEpoch, StructuralPrefix)]
terminalSourceUnionClosedPrefixes union = Map.toAscList union.closedPrefixes

terminalSourceUnionIncludedClaims :: TerminalSourceUnion -> [TerminalSourceOccurrenceClaim]
terminalSourceUnionIncludedClaims union = claimsFromMap union.includedClaims

terminalSourceUnionAheadClaims :: TerminalSourceUnion -> [TerminalSourceOccurrenceClaim]
terminalSourceUnionAheadClaims union = claimsFromMap union.aheadClaims

terminalSourceUnionTerminalPredecessorVector :: TerminalSourceUnion -> StructuralVersionVector
terminalSourceUnionTerminalPredecessorVector union = union.terminalPredecessorVector

terminalSourceUnionSuccessorInitialVector :: TerminalSourceUnion -> StructuralVersionVector
terminalSourceUnionSuccessorInitialVector union = union.successorInitialVector

terminalSourceUnionTerminalControlPrefix :: TerminalSourceUnion -> ControlIndex
terminalSourceUnionTerminalControlPrefix union = union.terminalControlPrefix

terminalSourceUnionTerminalTopologyOccurrenceDigest :: TerminalSourceUnion -> TopologyOccurrenceDigest
terminalSourceUnionTerminalTopologyOccurrenceDigest union = union.terminalTopologyDigest

terminalSourceUnionCanonicalBytes :: TerminalSourceUnion -> ByteString
terminalSourceUnionCanonicalBytes union = union.canonicalBytes

terminalSourceUnionDigest :: TerminalSourceUnion -> TerminalSourceUnionDigest
terminalSourceUnionDigest union = union.digest

decodeTerminalSourceUnionCanonicalBytes :: ByteString -> Either TerminalSourceProblem TerminalSourceUnion
decodeTerminalSourceUnionCanonicalBytes = decodeTerminalCanonical getTerminalSourceUnion terminalSourceUnionCanonicalBytes

terminalSourceUnionTopologyPredecessor :: HeraldMembershipLineage -> TerminalSourceUnion -> Either TerminalSourceProblem TopologyPredecessor
terminalSourceUnionTopologyPredecessor lineage union = do
  validateUnionCoordinate lineage union
  either
    (Left . TerminalSourceTopologyShapeProblem)
    Right
    (membershipSuccessorPredecessor lineage union.predecessorCut union.terminalPredecessorVector union.successorInitialVector union.digest)

-- | Compare retained candidates without letting an assumed digest collision
-- alias unequal canonical union bytes.
compareTerminalSourceUnions ::
  TerminalSourceUnion ->
  TerminalSourceUnion ->
  Either TerminalSourceProblem Bool
compareTerminalSourceUnions incumbent candidate
  | incumbent.digest /= candidate.digest = Right False
  | incumbent.canonicalBytes == candidate.canonicalBytes = Right True
  | otherwise =
      Left
        ( TerminalSourceUnionDigestCollision
            incumbent.digest
            incumbent.canonicalBytes
            candidate.canonicalBytes
        )

data TerminalSourceUnionAnnounce = TerminalSourceUnionAnnounce
  { announcer :: HeraldEpoch,
    union :: TerminalSourceUnion
  }
  deriving stock (Eq, Show)

terminalSourceUnionAnnouncer :: HeraldMembershipGeneration -> HeraldEpoch
terminalSourceUnionAnnouncer =
  minimum . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs

terminalSourceUnionAnnounce ::
  HeraldMembershipGeneration ->
  HeraldEpoch ->
  TerminalSourceUnion ->
  Either TerminalSourceProblem TerminalSourceUnionAnnounce
terminalSourceUnionAnnounce successor suppliedAnnouncer union = do
  unless
    (union.successorGeneration == heraldMembershipGenerationId successor)
    ( Left
        ( TerminalSourceUnionSuccessorMismatch
            (heraldMembershipGenerationId successor)
            union.successorGeneration
        )
    )
  let expected = terminalSourceUnionAnnouncer successor
  unless
    (suppliedAnnouncer == expected)
    (Left (TerminalSourceAnnouncerMismatch expected suppliedAnnouncer))
  terminalSourceUnionAnnounceFromClaimedCoordinates suppliedAnnouncer union

terminalSourceUnionAnnounceFromClaimedCoordinates ::
  HeraldEpoch ->
  TerminalSourceUnion ->
  Either TerminalSourceProblem TerminalSourceUnionAnnounce
terminalSourceUnionAnnounceFromClaimedCoordinates suppliedAnnouncer union = do
  let expected =
        minimum
          (fmap fst (structuralVersionVectorEntries union.successorInitialVector))
  unless
    (suppliedAnnouncer == expected)
    (Left (TerminalSourceAnnouncerMismatch expected suppliedAnnouncer))
  pure (TerminalSourceUnionAnnounce suppliedAnnouncer union)

terminalSourceUnionAnnounceAnnouncer ::
  TerminalSourceUnionAnnounce -> HeraldEpoch
terminalSourceUnionAnnounceAnnouncer announce = announce.announcer

terminalSourceUnionAnnounceUnion ::
  TerminalSourceUnionAnnounce -> TerminalSourceUnion
terminalSourceUnionAnnounceUnion announce = announce.union

terminalSourceUnionAnnounceCanonicalBytes ::
  TerminalSourceUnionAnnounce -> ByteString
terminalSourceUnionAnnounceCanonicalBytes announce =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-SOURCE-UNION-ANNOUNCE"
    putSizedBytes (heraldEpochBytes announce.announcer)
    putSizedBytes (terminalSourceUnionCanonicalBytes announce.union)

-- | Local Graph evidence required before this survivor may author acceptance.
-- The applied predecessor-generation history must cover the union's complete
-- least terminal vector and the applied control prefix must cover retirement.
newtype TerminalSourceUnionReady = TerminalSourceUnionReady
  { union :: TerminalSourceUnion
  }
  deriving stock (Eq, Show)

terminalSourceUnionReady ::
  StructuralVersionVector ->
  ControlIndex ->
  TerminalSourceUnion ->
  Either TerminalSourceProblem TerminalSourceUnionReady
terminalSourceUnionReady appliedVector appliedControl union = do
  validateVectorCoordinateForUnionPredecessor Nothing union appliedVector
  unless
    (appliedVector `structuralVersionVectorCovers` union.terminalPredecessorVector)
    ( Left
        ( TerminalSourceHistoryNotApplied
            union.terminalPredecessorVector
            appliedVector
        )
    )
  unless
    (appliedControl >= union.terminalControlPrefix)
    ( Left
        ( TerminalSourceControlNotApplied
            union.terminalControlPrefix
            appliedControl
        )
    )
  pure (TerminalSourceUnionReady union)

terminalSourceUnionReadyUnion :: TerminalSourceUnionReady -> TerminalSourceUnion
terminalSourceUnionReadyUnion ready = ready.union

newtype TerminalSourceAcceptanceDigest = TerminalSourceAcceptanceDigest ByteString
  deriving stock (Eq, Ord)

instance Show TerminalSourceAcceptanceDigest where
  show = renderGroupedHex . terminalSourceAcceptanceDigestBytes

mkTerminalSourceAcceptanceDigest ::
  ByteString -> Either TerminalSourceProblem TerminalSourceAcceptanceDigest
mkTerminalSourceAcceptanceDigest bytes
  | ByteString.length bytes == digestByteCount =
      Right (TerminalSourceAcceptanceDigest bytes)
  | otherwise =
      Left
        ( TerminalSourceAcceptanceDigestWrongByteCount
            digestByteCount
            (ByteString.length bytes)
        )

terminalSourceAcceptanceDigestBytes ::
  TerminalSourceAcceptanceDigest -> ByteString
terminalSourceAcceptanceDigestBytes (TerminalSourceAcceptanceDigest bytes) = bytes

data TerminalSourceUnionAcceptance = TerminalSourceUnionAcceptance
  { predecessorGeneration :: HeraldMembershipGenerationId,
    successorGeneration :: HeraldMembershipGenerationId,
    retiredSources :: Set HeraldEpoch,
    unionDigest :: TerminalSourceUnionDigest,
    reporter :: HeraldEpoch,
    digest :: TerminalSourceAcceptanceDigest
  }
  deriving stock (Eq, Show)

terminalSourceUnionAcceptance ::
  HeraldMembershipGeneration ->
  HeraldEpoch ->
  TerminalSourceUnionReady ->
  Either TerminalSourceProblem TerminalSourceUnionAcceptance
terminalSourceUnionAcceptance successor reporter ready = do
  let union = ready.union
  validateSuccessorMember successor reporter
  unless
    (union.successorGeneration == heraldMembershipGenerationId successor)
    ( Left
        ( TerminalSourceUnionSuccessorMismatch
            (heraldMembershipGenerationId successor)
            union.successorGeneration
        )
    )
  let bytes = acceptanceBytes union.digest reporter
      digest = acceptanceDigestInvariant (SHA256.hash bytes)
  pure
    TerminalSourceUnionAcceptance
      { predecessorGeneration = union.predecessorGeneration,
        successorGeneration = union.successorGeneration,
        retiredSources = union.retiredSources,
        unionDigest = union.digest,
        reporter,
        digest
      }

validateTerminalSourceUnionAcceptance ::
  HeraldMembershipGeneration ->
  TerminalSourceUnion ->
  TerminalSourceUnionAcceptance ->
  Either TerminalSourceProblem ()
validateTerminalSourceUnionAcceptance successor union supplied = do
  validateSuccessorMember successor supplied.reporter
  unless
    (union.successorGeneration == heraldMembershipGenerationId successor)
    ( Left
        ( TerminalSourceUnionSuccessorMismatch
            (heraldMembershipGenerationId successor)
            union.successorGeneration
        )
    )
  let expectedDigest =
        acceptanceDigestInvariant
          (SHA256.hash (acceptanceBytes union.digest supplied.reporter))
  unless
    ( supplied.predecessorGeneration == union.predecessorGeneration
        && supplied.successorGeneration == union.successorGeneration
        && supplied.retiredSources == union.retiredSources
    )
    (Left TerminalSourceAcceptanceCoordinateMismatch)
  unless
    (supplied.unionDigest == union.digest)
    (Left (TerminalSourceAcceptanceUnionMismatch union.digest supplied.unionDigest))
  unless
    (supplied.digest == expectedDigest)
    ( Left
        ( TerminalSourceAcceptanceDigestMismatch
            supplied.reporter
            expectedDigest
            supplied.digest
        )
    )

terminalSourceUnionAcceptanceReporter ::
  TerminalSourceUnionAcceptance -> HeraldEpoch
terminalSourceUnionAcceptanceReporter acceptance = acceptance.reporter

terminalSourceUnionAcceptancePredecessorGenerationId ::
  TerminalSourceUnionAcceptance -> HeraldMembershipGenerationId
terminalSourceUnionAcceptancePredecessorGenerationId acceptance =
  acceptance.predecessorGeneration

terminalSourceUnionAcceptanceSuccessorGenerationId ::
  TerminalSourceUnionAcceptance -> HeraldMembershipGenerationId
terminalSourceUnionAcceptanceSuccessorGenerationId acceptance =
  acceptance.successorGeneration

terminalSourceUnionAcceptanceRetiredSources ::
  TerminalSourceUnionAcceptance -> Set HeraldEpoch
terminalSourceUnionAcceptanceRetiredSources acceptance = acceptance.retiredSources

terminalSourceUnionAcceptanceUnionDigest ::
  TerminalSourceUnionAcceptance -> TerminalSourceUnionDigest
terminalSourceUnionAcceptanceUnionDigest acceptance = acceptance.unionDigest

terminalSourceUnionAcceptanceDigest ::
  TerminalSourceUnionAcceptance -> TerminalSourceAcceptanceDigest
terminalSourceUnionAcceptanceDigest acceptance = acceptance.digest

terminalSourceUnionAcceptanceCanonicalBytes ::
  TerminalSourceUnionAcceptance -> ByteString
terminalSourceUnionAcceptanceCanonicalBytes acceptance =
  acceptanceBytes acceptance.unionDigest acceptance.reporter

terminalSourceUnionAcceptanceFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  Set HeraldEpoch ->
  TerminalSourceUnionDigest ->
  HeraldEpoch ->
  TerminalSourceAcceptanceDigest ->
  Either TerminalSourceProblem TerminalSourceUnionAcceptance
terminalSourceUnionAcceptanceFromClaimedCoordinates
  predecessor
  successor
  retired
  unionDigest
  reporter
  suppliedDigest = do
    validateDistinctGenerationClaims predecessor successor
    when (Set.null retired) (Left TerminalSourceAcceptanceCoordinateMismatch)
    when
      (Set.member reporter retired)
      (Left (TerminalSourceAcceptanceReporterIsRetiredSource reporter))
    let expectedDigest =
          acceptanceDigestInvariant
            (SHA256.hash (acceptanceBytes unionDigest reporter))
    unless
      (suppliedDigest == expectedDigest)
      ( Left
          ( TerminalSourceAcceptanceDigestMismatch
              reporter
              expectedDigest
              suppliedDigest
          )
      )
    pure
      TerminalSourceUnionAcceptance
        { predecessorGeneration = predecessor,
          successorGeneration = successor,
          retiredSources = retired,
          unionDigest,
          reporter,
          digest = suppliedDigest
        }

data TerminalSourceUnionEstablished = TerminalSourceUnionEstablished
  { union :: TerminalSourceUnion,
    acceptances :: Map HeraldEpoch TerminalSourceUnionAcceptance,
    canonicalBytes :: ByteString
  }
  deriving stock (Eq, Show)

terminalSourceUnionEstablished ::
  HeraldMembershipGeneration ->
  TerminalSourceUnion ->
  [TerminalSourceUnionAcceptance] ->
  Either TerminalSourceProblem TerminalSourceUnionEstablished
terminalSourceUnionEstablished successor union suppliedAcceptances = do
  unless
    (union.successorGeneration == heraldMembershipGenerationId successor)
    ( Left
        ( TerminalSourceUnionSuccessorMismatch
            (heraldMembershipGenerationId successor)
            union.successorGeneration
        )
    )
  acceptances <- foldM insertAcceptance Map.empty suppliedAcceptances
  let expected = successorMembers successor
      actual = Map.keysSet acceptances
      foreignReporters = actual `Set.difference` expected
      missing = expected `Set.difference` actual
  unless
    (Set.null foreignReporters)
    ( Left
        (TerminalSourceAcceptanceForeignReporters (Set.toAscList foreignReporters))
    )
  unless
    (Set.null missing)
    (Left (TerminalSourceAcceptanceMissingReporters (Set.toAscList missing)))
  let bytes =
        Serialize.runPut $ do
          putSizedBytes "ECLIPS-TERMINAL-SOURCE-UNION-ESTABLISHED"
          putSizedBytes union.canonicalBytes
          putList putAcceptanceEntry (Map.toAscList acceptances)
  pure
    TerminalSourceUnionEstablished
      { union,
        acceptances,
        canonicalBytes = bytes
      }
  where
    insertAcceptance retained acceptance = do
      validateTerminalSourceUnionAcceptance successor union acceptance
      case Map.lookup acceptance.reporter retained of
        Nothing -> Right (Map.insert acceptance.reporter acceptance retained)
        Just incumbent
          | incumbent == acceptance -> Right retained
          | otherwise ->
              Left
                ( TerminalSourceAcceptanceReporterConflict
                    acceptance.reporter
                )

terminalSourceUnionEstablishedUnion ::
  TerminalSourceUnionEstablished -> TerminalSourceUnion
terminalSourceUnionEstablishedUnion established = established.union

terminalSourceUnionEstablishedAcceptances ::
  TerminalSourceUnionEstablished -> [TerminalSourceUnionAcceptance]
terminalSourceUnionEstablishedAcceptances established =
  Map.elems established.acceptances

terminalSourceUnionEstablishedCanonicalBytes ::
  TerminalSourceUnionEstablished -> ByteString
terminalSourceUnionEstablishedCanonicalBytes established =
  established.canonicalBytes

decodeTerminalSourceUnionEstablishedCanonicalBytes :: ByteString -> Either TerminalSourceProblem TerminalSourceUnionEstablished
decodeTerminalSourceUnionEstablishedCanonicalBytes = decodeTerminalCanonical parser terminalSourceUnionEstablishedCanonicalBytes
  where
    parser = do
      requireCanonicalDomain "ECLIPS-TERMINAL-SOURCE-UNION-ESTABLISHED"
      union <- getSizedBytes >>= getAdmitted . decodeTerminalSourceUnionCanonicalBytes
      acceptances <- getCanonicalList $ do
        reporter <- getCheckedSized mkHeraldEpoch
        digest <- getCheckedSized mkTerminalSourceAcceptanceDigest
        getAdmitted (terminalSourceUnionAcceptanceFromClaimedCoordinates union.predecessorGeneration union.successorGeneration union.retiredSources union.digest reporter digest)
      getAdmitted (terminalSourceUnionEstablishedFromClaimedCoordinates union acceptances)

-- | Context-free admission of a complete candidate certificate.  The union's
-- claimed successor vector determines the exact reporter set; the owning Graph
-- transition subsequently resolves that vector against the retained successor
-- membership before installation.
terminalSourceUnionEstablishedFromClaimedCoordinates ::
  TerminalSourceUnion ->
  [TerminalSourceUnionAcceptance] ->
  Either TerminalSourceProblem TerminalSourceUnionEstablished
terminalSourceUnionEstablishedFromClaimedCoordinates union suppliedAcceptances = do
  acceptances <- foldM insertClaimedAcceptance Map.empty suppliedAcceptances
  let expected = Set.fromList (fmap fst (structuralVersionVectorEntries union.successorInitialVector))
      actual = Map.keysSet acceptances
      foreignReporters = actual `Set.difference` expected
      missing = expected `Set.difference` actual
  unless
    (Set.null foreignReporters)
    ( Left
        (TerminalSourceAcceptanceForeignReporters (Set.toAscList foreignReporters))
    )
  unless
    (Set.null missing)
    (Left (TerminalSourceAcceptanceMissingReporters (Set.toAscList missing)))
  let bytes =
        Serialize.runPut $ do
          putSizedBytes "ECLIPS-TERMINAL-SOURCE-UNION-ESTABLISHED"
          putSizedBytes union.canonicalBytes
          putList putAcceptanceEntry (Map.toAscList acceptances)
  pure
    TerminalSourceUnionEstablished
      { union,
        acceptances,
        canonicalBytes = bytes
      }
  where
    insertClaimedAcceptance retained acceptance = do
      _ <-
        terminalSourceUnionAcceptanceFromClaimedCoordinates
          acceptance.predecessorGeneration
          acceptance.successorGeneration
          acceptance.retiredSources
          acceptance.unionDigest
          acceptance.reporter
          acceptance.digest
      unless
        ( acceptance.predecessorGeneration == union.predecessorGeneration
            && acceptance.successorGeneration == union.successorGeneration
            && acceptance.retiredSources == union.retiredSources
            && acceptance.unionDigest == union.digest
        )
        (Left TerminalSourceAcceptanceCoordinateMismatch)
      case Map.lookup acceptance.reporter retained of
        Nothing -> Right (Map.insert acceptance.reporter acceptance retained)
        Just incumbent
          | incumbent == acceptance -> Right retained
          | otherwise ->
              Left
                ( TerminalSourceAcceptanceReporterConflict
                    acceptance.reporter
                )

-- | Evidence used at the Graph installation boundary.  Construct it only after
-- the supplied cut has been installed; its exact membership-successor
-- predecessor and complete union acceptance then release the corresponding
-- publication hold.
data SuccessorStructuralBase = SuccessorStructuralBase
  { lineage :: HeraldMembershipLineage,
    successorMembership :: HeraldMembershipGeneration,
    cut :: TopologyCutId,
    establishedUnion :: TerminalSourceUnionEstablished
  }
  deriving stock (Eq, Show)

establishedSuccessorStructuralBase ::
  HeraldMembershipLineage ->
  TerminalSourceUnionEstablished ->
  TopologyCut ->
  Either TerminalSourceProblem SuccessorStructuralBase
establishedSuccessorStructuralBase lineage established cut = do
  let successor = heraldMembershipLineageTarget lineage
  let union = terminalSourceUnionEstablishedUnion established
  expectedPredecessor <-
    terminalSourceUnionTopologyPredecessor lineage union
  unless
    (topologyCutPredecessor cut == expectedPredecessor)
    (Left TerminalSourceSuccessorCutPredecessorMismatch)
  expectedCut <-
    either
      (Left . TerminalSourceTopologyShapeProblem)
      Right
      ( topologyCut
          expectedPredecessor
          ( topologyFrontier
              union.successorInitialVector
              union.terminalControlPrefix
          )
          union.terminalTopologyDigest
      )
  unless
    (cut == expectedCut)
    ( Left
        ( TerminalSourceSuccessorCutMismatch
            (deriveTopologyCutId expectedCut)
            (deriveTopologyCutId cut)
        )
    )
  pure
    SuccessorStructuralBase
      { lineage,
        successorMembership = successor,
        cut = deriveTopologyCutId cut,
        establishedUnion = established
      }

successorStructuralBaseLineage :: SuccessorStructuralBase -> HeraldMembershipLineage
successorStructuralBaseLineage base = base.lineage

successorStructuralBaseGenerationId ::
  SuccessorStructuralBase -> HeraldMembershipGenerationId
successorStructuralBaseGenerationId =
  heraldMembershipGenerationId . (.successorMembership)

successorStructuralBaseCutId :: SuccessorStructuralBase -> TopologyCutId
successorStructuralBaseCutId base = base.cut

successorStructuralBaseEstablishedUnion ::
  SuccessorStructuralBase -> TerminalSourceUnionEstablished
successorStructuralBaseEstablishedUnion base = base.establishedUnion

successorStructuralBaseReleases ::
  HeraldMembershipGenerationId -> SuccessorStructuralBase -> Bool
successorStructuralBaseReleases dependency base =
  dependency `elem` fmap heraldMembershipGenerationId (NonEmpty.tail (heraldMembershipLineageGenerations base.lineage))

-- | Project one survivor's already-applied predecessor-generation live-ahead
-- vector through the installed base.  Survivor components keep their greatest
-- observed prefixes; the retired component is dropped and may not exceed the
-- sealed terminal prefix.  This carries existing application without replay or
-- changing the shared union digest.
projectSurvivorLiveAheadVector :: StructuralVersionVector -> SuccessorStructuralBase -> Either TerminalSourceProblem StructuralVersionVector
projectSurvivorLiveAheadVector oldVector base = do
  let union = terminalSourceUnionEstablishedUnion base.establishedUnion
  validateLineageVector base.lineage Nothing oldVector
  traverse_ (requireTerminal union) (structuralVersionVectorEntries oldVector)
  entries <- traverse projectEntry (structuralVersionVectorEntries union.successorInitialVector)
  either (Left . TerminalSourceVectorConstructionProblem) Right (mkStructuralVersionVector base.successorMembership entries)
  where
    requireTerminal union (origin, observed)
      | not (Set.member origin union.retiredSources) = Right ()
      | otherwise = do
          terminal <- maybe (Left (TerminalSourceVectorMemberSetMismatch Nothing)) Right (structuralVersionVectorComponent origin union.terminalPredecessorVector)
          unless (observed <= terminal) (Left (TerminalSourceRetiredLiveAheadBeyondTerminal terminal observed))
    projectEntry (origin, initialPrefix) = do
      old <- maybe (Left (TerminalSourceVectorMemberSetMismatch Nothing)) Right (structuralVersionVectorComponent origin oldVector)
      unless (old >= initialPrefix) (Left (TerminalSourceSurvivorLiveAheadBeforeBase origin initialPrefix old))
      pure (origin, old)

-- | One local successor-generation allocation step.  The predecessor vector is
-- retained explicitly so a caller can stamp either a previously unsequenced
-- stage or compare an already stamped survivor occurrence without renumbering
-- it.
data SuccessorStructuralAdvance = SuccessorStructuralAdvance
  { occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector,
    successor :: StructuralVersionVector
  }
  deriving stock (Eq, Show)

successorStructuralAdvanceOccurrence ::
  SuccessorStructuralAdvance -> StructuralOccurrenceId
successorStructuralAdvanceOccurrence advance = advance.occurrence

successorStructuralAdvancePredecessor ::
  SuccessorStructuralAdvance -> StructuralVersionVector
successorStructuralAdvancePredecessor advance = advance.predecessor

successorStructuralAdvanceVector ::
  SuccessorStructuralAdvance -> StructuralVersionVector
successorStructuralAdvanceVector advance = advance.successor

-- | Allocate the next local occurrence only from an installed successor base.
-- The opaque base witness is what prevents an unsequenced stage from minting an
-- old-generation dot while membership contraction is still open.
allocateSuccessorStructuralOccurrence ::
  HeraldEpoch ->
  StructuralVersionVector ->
  SuccessorStructuralBase ->
  Either TerminalSourceProblem SuccessorStructuralAdvance
allocateSuccessorStructuralOccurrence source predecessor base = do
  validateSuccessorAdvanceVector base predecessor
  sourcePrefix <-
    maybe
      (Left (TerminalSourceSuccessorAdvanceSourceNotMember source))
      Right
      (structuralVersionVectorComponent source predecessor)
  let sequenceNumber = nextAfterStructuralPrefix sourcePrefix
      occurrence = structuralOccurrenceId source sequenceNumber
  successor <-
    either
      (Left . TerminalSourceVectorConstructionProblem)
      Right
      ( mkStructuralVersionVector
          base.successorMembership
          [ if herald == source
              then (herald, structuralPrefixThrough sequenceNumber)
              else (herald, prefix)
          | (herald, prefix) <- structuralVersionVectorEntries predecessor
          ]
      )
  pure SuccessorStructuralAdvance {occurrence, predecessor, successor}

-- | Evidence that an immutable predecessor-generation stamp was admitted as
-- the next occurrence of its surviving source in successor live-ahead state.
-- Keeping the complete stamp in the result makes preservation, rather than
-- merely preservation of its occurrence ID, part of the API.
data CarriedStampedSurvivor = CarriedStampedSurvivor
  { stamp :: StructuralOccurrenceStamp,
    predecessor :: StructuralVersionVector,
    successor :: StructuralVersionVector
  }
  deriving stock (Eq, Show)

carriedStampedSurvivorStamp ::
  CarriedStampedSurvivor -> StructuralOccurrenceStamp
carriedStampedSurvivorStamp carried = carried.stamp

carriedStampedSurvivorPredecessor ::
  CarriedStampedSurvivor -> StructuralVersionVector
carriedStampedSurvivorPredecessor carried = carried.predecessor

carriedStampedSurvivorVector ::
  CarriedStampedSurvivor -> StructuralVersionVector
carriedStampedSurvivorVector carried = carried.successor

-- | Admit one already stamped old-generation occurrence from a surviving
-- source against a successor live-ahead vector.  Its identity and stamp remain
-- unchanged; the returned advance changes only the successor-generation
-- component that now covers it.  Retired-source dependencies must be covered by
-- the sealed terminal prefix, while surviving dependencies may be covered by
-- the supplied successor live-ahead vector.
carryStampedSurvivorOccurrence ::
  StructuralOccurrenceStamp ->
  StructuralVersionVector ->
  SuccessorStructuralBase ->
  Either TerminalSourceProblem CarriedStampedSurvivor
carryStampedSurvivorOccurrence stamp current base = do
  let identifier = structuralOccurrenceStampOccurrence stamp
      source = structuralOccurrenceSourceHeraldEpoch identifier
      oldPredecessor = structuralOccurrenceStampPredecessor stamp
      union =
        terminalSourceUnionEstablishedUnion base.establishedUnion
  validateSuccessorAdvanceVector base current
  validateLineageVector base.lineage (Just identifier) oldPredecessor
  validateHistoricalPredecessorCoverage identifier union current oldPredecessor
  advance <- allocateSuccessorStructuralOccurrence source current base
  unless
    (successorStructuralAdvanceOccurrence advance == identifier)
    ( Left
        ( TerminalSourceStampedSurvivorOccurrenceNotNext
            (successorStructuralAdvanceOccurrence advance)
            identifier
        )
    )
  pure
    CarriedStampedSurvivor
      { stamp,
        predecessor = current,
        successor = successorStructuralAdvanceVector advance
      }

-- | Finish a Graph-proved descendant admission without changing the stamp.
-- The Graph owner proves historical dependency coverage; this constructor
-- binds that immutable occurrence to the checked next-source allocation.
carriedStampedSurvivorFromAdvance :: StructuralOccurrenceStamp -> SuccessorStructuralAdvance -> Either TerminalSourceProblem CarriedStampedSurvivor
carriedStampedSurvivorFromAdvance stamp advance = do
  let identifier = structuralOccurrenceStampOccurrence stamp
  unless
    (successorStructuralAdvanceOccurrence advance == identifier)
    (Left (TerminalSourceStampedSurvivorOccurrenceNotNext (successorStructuralAdvanceOccurrence advance) identifier))
  pure CarriedStampedSurvivor {stamp, predecessor = successorStructuralAdvancePredecessor advance, successor = successorStructuralAdvanceVector advance}

data TerminalSourceProblem
  = TerminalSourceClosureLineageMismatch
  | TerminalSourceCanonicalDecodeFailed String
  | TerminalSourceCanonicalNonCanonical
  | TerminalSourceGenerationCoordinateCollapsed HeraldMembershipGenerationId
  | TerminalSourceInventoryDigestWrongByteCount Int Int
  | TerminalSourceAcceptanceDigestWrongByteCount Int Int
  | TerminalSourcePredecessorNotGenesis HeraldMembershipGenerationId
  | TerminalSourceSuccessorPredecessorMismatch
      HeraldMembershipGenerationId
      (Maybe HeraldMembershipGenerationId)
  | TerminalSourceSuccessorMissingRetirement HeraldMembershipGenerationId
  | TerminalSourceRetiredSourceMismatch HeraldEpoch HeraldEpoch
  | TerminalSourceSuccessorMembersMismatch [HeraldEpoch] [HeraldEpoch]
  | TerminalSourceHeraldNotSuccessorMember HeraldEpoch
  | TerminalSourceOccurrenceWrongSource HeraldEpoch StructuralOccurrenceId
  | TerminalSourceOccurrencePredecessorGenerationMismatch
      StructuralOccurrenceId
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TerminalSourceOccurrencePredecessorMemberSetMismatch
      StructuralOccurrenceId
  | TerminalSourceOccurrencePredecessorPrefixMismatch
      StructuralOccurrenceId
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceOccurrencePayloadDigestMismatch
      StructuralOccurrenceId
      StructuralPublicationDigest
      StructuralPublicationDigest
  | TerminalSourceClaimConflict
      StructuralOccurrenceId
      StructuralPublicationDigest
      StructuralPublicationDigest
  | TerminalSourceInventoryPrefixMismatch StructuralPrefix StructuralPrefix
  | TerminalSourceInventoryCoordinateMismatch TerminalSourceInventoryDigest
  | TerminalSourceInventoryReporterConflict HeraldEpoch
  | TerminalSourceInventoryForeignReporters [HeraldEpoch]
  | TerminalSourceInventoryMissingReporters [HeraldEpoch]
  | TerminalSourceInventoryBehindPredecessorCut
      HeraldEpoch
      StructuralPrefix
      StructuralPrefix
  | TerminalSourcePayloadConflict StructuralOccurrenceId ByteString ByteString
  | TerminalSourcePayloadMissing StructuralOccurrenceId
  | TerminalSourcePayloadDigestMismatch
      StructuralOccurrenceId
      StructuralPublicationDigest
      StructuralPublicationDigest
  | TerminalSourceRequestCoordinateMismatch StructuralOccurrenceId
  | TerminalSourceRelayReporterMismatch HeraldEpoch HeraldEpoch
  | TerminalSourceRelayInventoryMismatch
      TerminalSourceInventoryDigest
      TerminalSourceInventoryDigest
  | TerminalSourceRelayOccurrenceMismatch
      StructuralOccurrenceId
      StructuralOccurrenceId
  | TerminalSourceRelayClaimMissing StructuralOccurrenceId
  | TerminalSourceRelayEnvelopeMismatch
  | TerminalSourceRelayIsRetiredSource HeraldEpoch
  | TerminalSourceVectorGenerationMismatch
      (Maybe StructuralOccurrenceId)
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TerminalSourceVectorMemberSetMismatch (Maybe StructuralOccurrenceId)
  | TerminalSourcePredecessorBeyondClosedPrefix
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceVectorConstructionProblem StructuralVectorProblem
  | TerminalSourceUnionPredecessorMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TerminalSourceUnionSuccessorMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TerminalSourceTopologyShapeProblem TopologyShapeProblem
  | TerminalSourceUnionDigestCollision
      TerminalSourceUnionDigest
      ByteString
      ByteString
  | TerminalSourceUnionClaimPartitionOverlap
  | TerminalSourceUnionClaimPartitionMismatch
  | TerminalSourceUnionClaimedVectorCoordinateMismatch
  | TerminalSourceUnionInventoryReporterSetMismatch
  | TerminalSourceUnionControlPrefixZero
  | TerminalSourceTopologyReconstructionUnavailable
  | TerminalSourceAnnouncerMismatch HeraldEpoch HeraldEpoch
  | TerminalSourceHistoryNotApplied
      StructuralVersionVector
      StructuralVersionVector
  | TerminalSourceControlNotApplied ControlIndex ControlIndex
  | TerminalSourceAcceptanceUnionMismatch
      TerminalSourceUnionDigest
      TerminalSourceUnionDigest
  | TerminalSourceAcceptanceDigestMismatch
      HeraldEpoch
      TerminalSourceAcceptanceDigest
      TerminalSourceAcceptanceDigest
  | TerminalSourceAcceptanceReporterConflict HeraldEpoch
  | TerminalSourceAcceptanceCoordinateMismatch
  | TerminalSourceAcceptanceReporterIsRetiredSource HeraldEpoch
  | TerminalSourceAcceptanceForeignReporters [HeraldEpoch]
  | TerminalSourceAcceptanceMissingReporters [HeraldEpoch]
  | TerminalSourceSuccessorCutPredecessorMismatch
  | TerminalSourceSuccessorCutMismatch TopologyCutId TopologyCutId
  | TerminalSourceRetiredLiveAheadBeyondTerminal
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceSurvivorLiveAheadBeforeBase
      HeraldEpoch
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceSuccessorAdvanceBeforeBase
      HeraldEpoch
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceSuccessorAdvanceSourceNotMember HeraldEpoch
  | TerminalSourceStampedSurvivorDependencyNotCovered
      StructuralOccurrenceId
      HeraldEpoch
      StructuralPrefix
      StructuralPrefix
  | TerminalSourceStampedSurvivorOccurrenceNotNext
      StructuralOccurrenceId
      StructuralOccurrenceId
  deriving stock (Eq, Show)

validateTransition :: HeraldMembershipLineage -> HeraldEpoch -> Either TerminalSourceProblem ()
validateTransition lineage retired = do
  validateClosureLineage lineage
  unless
    (retired `elem` retiredSourcesFor lineage)
    (Left TerminalSourceClosureLineageMismatch)

successorRetirementControl ::
  HeraldMembershipGeneration -> Either TerminalSourceProblem ControlIndex
successorRetirementControl successor =
  maybe
    ( Left
        ( TerminalSourceSuccessorMissingRetirement
            (heraldMembershipGenerationId successor)
        )
    )
    Right
    (heraldMembershipGenerationRetirementControlIndex successor)

successorMembers :: HeraldMembershipGeneration -> Set HeraldEpoch
successorMembers =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs

validateSuccessorMember ::
  HeraldMembershipGeneration -> HeraldEpoch -> Either TerminalSourceProblem ()
validateSuccessorMember successor herald =
  unless
    (Set.member herald (successorMembers successor))
    (Left (TerminalSourceHeraldNotSuccessorMember herald))

validateOccurrence :: HeraldMembershipLineage -> HeraldEpoch -> TerminalStructuralOccurrence -> Either TerminalSourceProblem ()
validateOccurrence lineage retired occurrence = do
  let identifier = terminalStructuralOccurrenceId occurrence
      vector = structuralOccurrenceStampPredecessor occurrence.stamp
      expectedPrefix = structuralPredecessorPrefix (structuralOccurrenceSourceSequence identifier)
  validateRetiredOccurrence retired identifier
  validateLineageVector lineage (Just identifier) vector
  unless
    (structuralVersionVectorComponent retired vector == Just expectedPrefix)
    ( Left
        ( TerminalSourceOccurrencePredecessorPrefixMismatch
            identifier
            expectedPrefix
            (maybe emptyStructuralPrefix id (structuralVersionVectorComponent retired vector))
        )
    )

validateLineageVector :: HeraldMembershipLineage -> Maybe StructuralOccurrenceId -> StructuralVersionVector -> Either TerminalSourceProblem ()
validateLineageVector lineage occurrence vector =
  case filter
    ((== structuralVersionVectorMembershipGenerationId vector) . heraldMembershipGenerationId)
    (NonEmpty.toList (heraldMembershipLineageGenerations lineage)) of
    [generation] -> validateVectorCoordinate generation occurrence vector
    _ ->
      Left
        ( TerminalSourceVectorGenerationMismatch
            occurrence
            (heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage))
            (structuralVersionVectorMembershipGenerationId vector)
        )

validateVectorCoordinate ::
  HeraldMembershipGeneration ->
  Maybe StructuralOccurrenceId ->
  StructuralVersionVector ->
  Either TerminalSourceProblem ()
validateVectorCoordinate generation occurrence vector = do
  let expectedGeneration = heraldMembershipGenerationId generation
      actualGeneration = structuralVersionVectorMembershipGenerationId vector
  unless
    (actualGeneration == expectedGeneration)
    ( Left
        ( TerminalSourceVectorGenerationMismatch
            occurrence
            expectedGeneration
            actualGeneration
        )
    )
  unless
    ( structuralVersionVectorMemberSetDigest vector
        == heraldMembershipGenerationActiveMemberSetDigest generation
    )
    (Left (TerminalSourceVectorMemberSetMismatch occurrence))

-- | Admit a successor live-ahead vector against both the checked successor
-- membership and the exact initial projection established by the terminal
-- union.  A caller may advance survivor components, but it cannot regress or
-- replace the installed base.
validateSuccessorAdvanceVector ::
  SuccessorStructuralBase ->
  StructuralVersionVector ->
  Either TerminalSourceProblem ()
validateSuccessorAdvanceVector base vector = do
  validateVectorCoordinate base.successorMembership Nothing vector
  let union = terminalSourceUnionEstablishedUnion base.establishedUnion
      initial = terminalSourceUnionSuccessorInitialVector union
  traverse_
    (requireBaseCoverage vector)
    (structuralVersionVectorEntries initial)
  where
    requireBaseCoverage current (herald, required) = do
      actual <-
        maybe
          (Left (TerminalSourceVectorMemberSetMismatch Nothing))
          Right
          (structuralVersionVectorComponent herald current)
      unless
        (actual >= required)
        ( Left
            ( TerminalSourceSuccessorAdvanceBeforeBase
                herald
                required
                actual
            )
        )

-- | Resolve an old stamp only against the exact predecessor coordinate sealed
-- into the established union.  The checked stamp constructor already proves
-- its source-prefix relation; this check prevents a vector from another
-- generation or member shape from being carried through this base.
validateVectorCoordinateForUnionPredecessor ::
  Maybe StructuralOccurrenceId ->
  TerminalSourceUnion ->
  StructuralVersionVector ->
  Either TerminalSourceProblem ()
validateVectorCoordinateForUnionPredecessor occurrence union vector = do
  let expectedGeneration = union.predecessorGeneration
      actualGeneration = structuralVersionVectorMembershipGenerationId vector
      terminalVector = union.terminalPredecessorVector
      sameMemberShape =
        Map.keysSet (Map.fromList (structuralVersionVectorEntries vector))
          == Map.keysSet
            (Map.fromList (structuralVersionVectorEntries terminalVector))
  unless
    (actualGeneration == expectedGeneration)
    ( Left
        ( TerminalSourceVectorGenerationMismatch
            occurrence
            expectedGeneration
            actualGeneration
        )
    )
  unless
    ( structuralVersionVectorMemberSetDigest vector
        == structuralVersionVectorMemberSetDigest terminalVector
        && sameMemberShape
    )
    (Left (TerminalSourceVectorMemberSetMismatch occurrence))

-- | Prove that every dependency frozen into an old survivor stamp remains
-- covered after contraction.  The retired coordinate is covered only by the
-- sealed terminal vector; live successor coordinates may additionally use the
-- caller's local live-ahead vector.
validateHistoricalPredecessorCoverage ::
  StructuralOccurrenceId ->
  TerminalSourceUnion ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  Either TerminalSourceProblem ()
validateHistoricalPredecessorCoverage identifier union current oldPredecessor =
  traverse_ requireCoverage (structuralVersionVectorEntries oldPredecessor)
  where
    requireCoverage (herald, required) = do
      actual <-
        maybe
          (Left (TerminalSourceVectorMemberSetMismatch (Just identifier)))
          Right
          ( if Set.member herald union.retiredSources
              then
                structuralVersionVectorComponent
                  herald
                  union.terminalPredecessorVector
              else structuralVersionVectorComponent herald current
          )
      unless
        (actual >= required)
        ( Left
            ( TerminalSourceStampedSurvivorDependencyNotCovered
                identifier
                herald
                required
                actual
            )
        )

validateInventoryCoordinate :: HeraldMembershipLineage -> HeraldEpoch -> TerminalSourceInventory -> Either TerminalSourceProblem ()
validateInventoryCoordinate lineage retired inventory = do
  validateTransition lineage retired
  unless
    ( inventory.predecessorGeneration == heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage)
        && inventory.successorGeneration == heraldMembershipGenerationId (heraldMembershipLineageTarget lineage)
        && inventory.retiredSource == retired
    )
    (Left (TerminalSourceInventoryCoordinateMismatch inventory.digest))
  validateSuccessorMember (heraldMembershipLineageTarget lineage) inventory.reporter

validateRequestCoordinate :: HeraldMembershipLineage -> HeraldEpoch -> TerminalSourcePayloadRequest -> Either TerminalSourceProblem ()
validateRequestCoordinate lineage retired request = do
  validateTransition lineage retired
  unless
    ( request.predecessorGeneration == heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage)
        && request.successorGeneration == heraldMembershipGenerationId (heraldMembershipLineageTarget lineage)
        && request.retiredSource == retired
        && structuralOccurrenceSourceHeraldEpoch request.occurrence == retired
    )
    (Left (TerminalSourceRequestCoordinateMismatch request.occurrence))

validateUnionCoordinate :: HeraldMembershipLineage -> TerminalSourceUnion -> Either TerminalSourceProblem ()
validateUnionCoordinate lineage union = do
  validateClosureLineage lineage
  unless
    ( union.predecessorGeneration == heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage)
        && union.successorGeneration == heraldMembershipGenerationId (heraldMembershipLineageTarget lineage)
        && union.generationLineage == fmap heraldMembershipGenerationId (NonEmpty.toList (heraldMembershipLineageGenerations lineage))
        && union.retiredSources == Set.fromList (retiredSourcesFor lineage)
    )
    (Left TerminalSourceClosureLineageMismatch)

normalizeClaims ::
  HeraldEpoch ->
  [TerminalSourceOccurrenceClaim] ->
  Either
    TerminalSourceProblem
    (Map StructuralOccurrenceId StructuralPublicationDigest)
normalizeClaims retired = foldM insert Map.empty
  where
    insert retained claim = do
      let identifier = claim.occurrence
      unless
        (structuralOccurrenceSourceHeraldEpoch identifier == retired)
        (Left (TerminalSourceOccurrenceWrongSource retired identifier))
      case Map.lookup identifier retained of
        Nothing -> Right (Map.insert identifier claim.publicationDigest retained)
        Just incumbent
          | incumbent == claim.publicationDigest -> Right retained
          | otherwise ->
              Left
                ( TerminalSourceClaimConflict
                    identifier
                    incumbent
                    claim.publicationDigest
                )

claimForOccurrence :: TerminalStructuralOccurrence -> TerminalSourceOccurrenceClaim
claimForOccurrence occurrence =
  TerminalSourceOccurrenceClaim
    (terminalStructuralOccurrenceId occurrence)
    (terminalStructuralOccurrencePublicationDigest occurrence)

claimsFromMap ::
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  [TerminalSourceOccurrenceClaim]
claimsFromMap claims =
  [ TerminalSourceOccurrenceClaim occurrence digest
  | (occurrence, digest) <- Map.toAscList claims
  ]

contiguousPrefixForClaims ::
  Map StructuralOccurrenceId StructuralPublicationDigest -> StructuralPrefix
contiguousPrefixForClaims claims = go 1 emptyStructuralPrefix (Map.keys claims)
  where
    go _ prefix [] = prefix
    go expected prefix (identifier : remaining) =
      let sequenceNumber = structuralOccurrenceSourceSequence identifier
          observed = structuralSequenceWord64 sequenceNumber
       in case compare observed expected of
            LT -> go expected prefix remaining
            EQ -> go (expected + 1) (structuralPrefixThrough sequenceNumber) remaining
            GT -> prefix

normalizeInventoryMap :: HeraldMembershipLineage -> HeraldEpoch -> [TerminalSourceInventory] -> Either TerminalSourceProblem (Map HeraldEpoch TerminalSourceInventory)
normalizeInventoryMap lineage retired = foldM insert Map.empty
  where
    insert retained inventory = do
      validateInventoryCoordinate lineage retired inventory
      case Map.lookup inventory.reporter retained of
        Nothing -> Right (Map.insert inventory.reporter inventory retained)
        Just incumbent
          | incumbent == inventory -> Right retained
          | otherwise -> Left (TerminalSourceInventoryReporterConflict inventory.reporter)

exactClosureInventoryMap :: HeraldMembershipLineage -> [TerminalSourceInventory] -> Either TerminalSourceProblem (Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventory)
exactClosureInventoryMap lineage supplied = do
  traverse_ (\inventory -> validateInventoryCoordinate lineage inventory.retiredSource inventory) supplied
  entries <- traverse forOrigin (retiredSourcesFor lineage)
  pure (Map.fromList (concat entries))
  where
    forOrigin origin = do
      inventories <- normalizeInventoryMap lineage origin (filter ((== origin) . (.retiredSource)) supplied)
      let expected = successorMembers (heraldMembershipLineageTarget lineage)
          missing = expected `Set.difference` Map.keysSet inventories
      unless (Set.null missing) (Left (TerminalSourceInventoryMissingReporters (Set.toAscList missing)))
      pure [((origin, reporter), inventory) | (reporter, inventory) <- Map.toAscList inventories]

mergeInventoryClaims ::
  TerminalSourceInventory ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  Either
    TerminalSourceProblem
    (Map StructuralOccurrenceId StructuralPublicationDigest)
mergeInventoryClaims inventory incumbentClaims =
  foldM insertClaim incumbentClaims (Map.toAscList inventory.claims)
  where
    insertClaim accumulated (identifier, digest) =
      case Map.lookup identifier accumulated of
        Nothing -> Right (Map.insert identifier digest accumulated)
        Just incumbent
          | incumbent == digest -> Right accumulated
          | otherwise ->
              Left (TerminalSourceClaimConflict identifier incumbent digest)

type ClaimSupports =
  Map
    StructuralOccurrenceId
    ( StructuralPublicationDigest,
      Map HeraldEpoch TerminalSourceInventoryDigest
    )

addInventorySupports ::
  ClaimSupports ->
  TerminalSourceInventory ->
  Either TerminalSourceProblem ClaimSupports
addInventorySupports incumbentSupports suppliedInventory =
  foldM
    (insertSupport suppliedInventory)
    incumbentSupports
    (Map.toAscList suppliedInventory.claims)
  where
    insertSupport supportingInventory accumulated (identifier, digest) =
      case Map.lookup identifier accumulated of
        Nothing ->
          Right
            ( Map.insert
                identifier
                ( digest,
                  Map.singleton
                    supportingInventory.reporter
                    supportingInventory.digest
                )
                accumulated
            )
        Just (incumbent, supporters)
          | incumbent == digest ->
              Right
                ( Map.insert
                    identifier
                    ( digest,
                      Map.insert
                        supportingInventory.reporter
                        supportingInventory.digest
                        supporters
                    )
                    accumulated
                )
          | otherwise ->
              Left (TerminalSourceClaimConflict identifier incumbent digest)

validateClaimPayload ::
  HeraldMembershipLineage ->
  HeraldEpoch ->
  TerminalSourcePayloadArchive ->
  (StructuralOccurrenceId, StructuralPublicationDigest) ->
  Either TerminalSourceProblem ()
validateClaimPayload predecessor retired payloads (identifier, expectedDigest) = do
  occurrence <-
    maybe
      (Left (TerminalSourcePayloadMissing identifier))
      Right
      (lookupTerminalSourcePayload identifier payloads)
  validateOccurrence predecessor retired occurrence
  let actualDigest = terminalStructuralOccurrencePublicationDigest occurrence
  unless
    (actualDigest == expectedDigest)
    (Left (TerminalSourcePayloadDigestMismatch identifier expectedDigest actualDigest))

leastCompoundTerminalVector ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  Map HeraldEpoch StructuralPrefix ->
  StructuralVersionVector ->
  Map HeraldEpoch StructuralPrefix ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  TerminalSourcePayloadArchive ->
  Either TerminalSourceProblem StructuralVersionVector
leastCompoundTerminalVector historyLineage lineage historical anchor prefixes included payloads = do
  traverse_ requireAnchor (Map.toAscList prefixes)
  joined <- foldM cover (Map.fromList (structuralVersionVectorEntries anchor)) (Map.keys included)
  either
    (Left . TerminalSourceVectorConstructionProblem)
    Right
    (mkStructuralVersionVector (heraldMembershipLineageOrigin lineage) (Map.toAscList (Map.union prefixes joined)))
  where
    requireAnchor (origin, closed) = do
      required <- maybe (Left (TerminalSourceVectorMemberSetMismatch Nothing)) Right (structuralVersionVectorComponent origin anchor)
      unless (closed >= required) (Left (TerminalSourcePredecessorBeyondClosedPrefix required closed))
    cover retained identifier = do
      occurrence <- maybe (Left (TerminalSourcePayloadMissing identifier)) Right (lookupTerminalSourcePayload identifier payloads)
      validateOccurrence historyLineage (structuralOccurrenceSourceHeraldEpoch identifier) occurrence
      foldM coverDependency retained (structuralVersionVectorEntries (structuralOccurrenceStampPredecessor occurrence.stamp))
    coverDependency retained (source, required) = case Map.lookup source retained of
      Just current -> Right (Map.insert source (max current required) retained)
      Nothing -> do
        let covered = Map.findWithDefault emptyStructuralPrefix source historical
        unless (covered >= required) (Left (TerminalSourcePredecessorBeyondClosedPrefix required covered))
        pure retained

projectSuccessorVector ::
  HeraldMembershipGeneration ->
  StructuralVersionVector ->
  Either TerminalSourceProblem StructuralVersionVector
projectSuccessorVector successor terminalVector = do
  let terminalEntries = Map.fromList (structuralVersionVectorEntries terminalVector)
      projected =
        [ (herald, prefix)
        | herald <-
            NonEmpty.toList
              (heraldMembershipGenerationActiveHeraldEpochs successor),
          Just prefix <- [Map.lookup herald terminalEntries]
        ]
  either
    (Left . TerminalSourceVectorConstructionProblem)
    Right
    (mkStructuralVersionVector successor projected)

terminalSourceUnionBytes ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  [HeraldMembershipGenerationId] ->
  Set HeraldEpoch ->
  TopologyCutId ->
  Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventoryDigest ->
  Map HeraldEpoch StructuralPrefix ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  Map StructuralOccurrenceId StructuralPublicationDigest ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  ControlIndex ->
  TopologyOccurrenceDigest ->
  ByteString
terminalSourceUnionBytes predecessor successor lineage retired predecessorCut inventories prefixes included ahead terminalVector successorVector controlPrefix topologyDigest =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-SOURCE-UNION"
    putSizedBytes (heraldMembershipGenerationIdBytes predecessor)
    putSizedBytes (heraldMembershipGenerationIdBytes successor)
    putList (putSizedBytes . heraldMembershipGenerationIdBytes) lineage
    putList (putSizedBytes . heraldEpochBytes) (Set.toAscList retired)
    putSizedBytes (topologyCutIdBytes predecessorCut)
    putList putInventoryDigestEntry (Map.toAscList inventories)
    putList putVectorEntry (Map.toAscList prefixes)
    putList putClaim (claimsFromMap included)
    putList putClaim (claimsFromMap ahead)
    putStructuralVector terminalVector
    putStructuralVector successorVector
    Serialize.putWord64be (controlIndexWord64 controlPrefix)
    putSizedBytes (topologyOccurrenceDigestBytes topologyDigest)

acceptanceBytes ::
  TerminalSourceUnionDigest -> HeraldEpoch -> ByteString
acceptanceBytes unionDigest reporter =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-TERMINAL-SOURCE-UNION-ACCEPTED"
    putSizedBytes (terminalSourceUnionDigestBytes unionDigest)
    putSizedBytes (heraldEpochBytes reporter)

decodeTerminalCanonical ::
  SerializeGet.Get value ->
  (value -> ByteString) ->
  ByteString ->
  Either TerminalSourceProblem value
decodeTerminalCanonical getValue canonicalBytes supplied = do
  value <-
    case SerializeGet.runGetState getValue supplied 0 of
      Left problem -> Left (TerminalSourceCanonicalDecodeFailed problem)
      Right (decoded, trailing)
        | ByteString.null trailing -> Right decoded
        | otherwise ->
            Left (TerminalSourceCanonicalDecodeFailed "trailing bytes")
  if canonicalBytes value == supplied
    then Right value
    else Left TerminalSourceCanonicalNonCanonical

getTerminalStructuralOccurrence ::
  SerializeGet.Get TerminalStructuralOccurrence
getTerminalStructuralOccurrence = do
  requireCanonicalDomain "ECLIPS-TERMINAL-STRUCTURAL-OCCURRENCE"
  occurrence <- getOccurrenceId
  predecessor <- getStructuralVector
  nabla <- getCheckedSized mkNablaId
  authority <- getSizedBytes >>= getAdmitted . decodeAuthorityEpochCanonicalBytes
  source <- getCheckedSized mkHeraldEpoch
  sequenceNumber <- nablaSequence <$> SerializeGet.getWord64be
  publicationDigest <- getCheckedSized mkStructuralPublicationDigest
  carrierRole <- getStructuralCarrierRole
  payload <- getSizedBytes
  stamp <-
    getAdmitted
      ( mkStructuralOccurrenceStamp
          occurrence
          predecessor
          (publicationId nabla authority source sequenceNumber)
          publicationDigest
          carrierRole
      )
  getAdmitted (terminalStructuralOccurrence stamp payload)

getTerminalSourceInventory :: SerializeGet.Get TerminalSourceInventory
getTerminalSourceInventory = do
  requireCanonicalDomain "ECLIPS-TERMINAL-SOURCE-INVENTORY"
  predecessor <- getCheckedSized mkHeraldMembershipGenerationId
  successor <- getCheckedSized mkHeraldMembershipGenerationId
  retired <- getCheckedSized mkHeraldEpoch
  reporter <- getCheckedSized mkHeraldEpoch
  chain <- getCanonicalList $ do
    bytes <- getSizedBytes
    either (fail . show) pure (decodeTopologyCutEstablishedCanonicalBytes bytes)
  bases <- getCanonicalList (getSizedBytes >>= getAdmitted . decodeTerminalSourceUnionEstablishedCanonicalBytes)
  evidence <- getCanonicalList (getSizedBytes >>= getAdmitted . decodeTerminalSourceInventoryCanonicalBytes)
  retained <- getCanonicalList getTerminalSourceOccurrenceClaim
  prefix <- getStructuralPrefix
  claims <- getCanonicalList getTerminalSourceOccurrenceClaim
  getAdmitted (terminalSourceInventoryFromClaimedCoordinates predecessor successor retired reporter chain bases evidence retained prefix claims)

terminalSourceInventoryFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalSourceInventory] ->
  [TerminalSourceOccurrenceClaim] ->
  StructuralPrefix ->
  [TerminalSourceOccurrenceClaim] ->
  Either TerminalSourceProblem TerminalSourceInventory
terminalSourceInventoryFromClaimedCoordinates predecessor successor retired reporter chain bases evidence retained claimed supplied = do
  validateDistinctGenerationClaims predecessor successor
  when (reporter == retired) (Left (TerminalSourceInventoryReporterConflict reporter))
  normalized <- normalizeClaims retired supplied
  let expected = contiguousPrefixForClaims normalized
  unless (claimed == expected) (Left (TerminalSourceInventoryPrefixMismatch expected claimed))
  retainedMap <- normalizeCompoundClaims (Set.fromList (fmap (structuralOccurrenceSourceHeraldEpoch . (.occurrence)) retained)) retained
  let normalizedEvidence = Map.elems (Map.fromList [(entry.digest, entry) | entry <- evidence])
  pure (canonicalInventoryFromCoordinates predecessor successor retired reporter chain bases normalizedEvidence retainedMap expected normalized)

getTerminalSourcePayloadRelay :: SerializeGet.Get TerminalSourcePayloadRelay
getTerminalSourcePayloadRelay = do
  requireCanonicalDomain "ECLIPS-TERMINAL-SOURCE-PAYLOAD-RELAY"
  predecessor <- getCheckedSized mkHeraldMembershipGenerationId
  successor <- getCheckedSized mkHeraldMembershipGenerationId
  retired <- getCheckedSized mkHeraldEpoch
  relay <- getCheckedSized mkHeraldEpoch
  supportingInventory <- getCheckedSized mkTerminalSourceInventoryDigest
  occurrenceBytes <- getSizedBytes
  occurrence <-
    getAdmitted (decodeTerminalStructuralOccurrenceCanonicalBytes occurrenceBytes)
  getAdmitted
    ( terminalSourcePayloadRelayFromClaimedCoordinates
        predecessor
        successor
        retired
        relay
        supportingInventory
        occurrence
    )

getTerminalSourceUnion :: SerializeGet.Get TerminalSourceUnion
getTerminalSourceUnion = do
  requireCanonicalDomain "ECLIPS-TERMINAL-SOURCE-UNION"
  predecessor <- getCheckedSized mkHeraldMembershipGenerationId
  successor <- getCheckedSized mkHeraldMembershipGenerationId
  lineage <- getCanonicalList (getCheckedSized mkHeraldMembershipGenerationId)
  retired <- Set.fromList <$> getCanonicalList (getCheckedSized mkHeraldEpoch)
  predecessorCut <- getCheckedSized mkTopologyCutId
  inventories <- getAdmitted . normalizeInventoryDigestClaims =<< getCanonicalList getInventoryDigestEntry
  prefixes <- Map.fromList <$> getCanonicalList getStructuralVectorEntry
  included <- getCanonicalList getTerminalSourceOccurrenceClaim
  ahead <- getCanonicalList getTerminalSourceOccurrenceClaim
  terminalVector <- getStructuralVector
  successorVector <- getStructuralVector
  controlPrefix <- controlIndex <$> SerializeGet.getWord64be
  topologyDigest <- getCheckedSized mkTopologyOccurrenceDigest
  getAdmitted (terminalSourceUnionFromClaimedCoordinates predecessor successor lineage retired predecessorCut inventories prefixes included ahead terminalVector successorVector controlPrefix topologyDigest)

terminalSourceUnionFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  [HeraldMembershipGenerationId] ->
  Set HeraldEpoch ->
  TopologyCutId ->
  Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventoryDigest ->
  Map HeraldEpoch StructuralPrefix ->
  [TerminalSourceOccurrenceClaim] ->
  [TerminalSourceOccurrenceClaim] ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  ControlIndex ->
  TopologyOccurrenceDigest ->
  Either TerminalSourceProblem TerminalSourceUnion
terminalSourceUnionFromClaimedCoordinates predecessor successor lineage retired predecessorCut inventories prefixes suppliedIncluded suppliedAhead terminalVector successorVector controlPrefix topologyDigest = do
  validateDistinctGenerationClaims predecessor successor
  unless
    (not (Set.null retired) && length lineage == Set.size retired + 1 && take 1 lineage == [predecessor] && reverse (take 1 (reverse lineage)) == [successor] && Set.size (Set.fromList lineage) == length lineage)
    (Left TerminalSourceClosureLineageMismatch)
  included <- normalizeCompoundClaims retired suppliedIncluded
  ahead <- normalizeCompoundClaims retired suppliedAhead
  unless (Map.null (Map.intersection included ahead)) (Left TerminalSourceUnionClaimPartitionOverlap)
  let allClaims = Map.union included ahead
      expectedIncluded = Map.filterWithKey (\identifier _ -> occurrenceWithin prefixes identifier) allClaims
      available origin = contiguousPrefixForClaims (Map.filterWithKey (\identifier _ -> structuralOccurrenceSourceHeraldEpoch identifier == origin) allClaims)
  unless
    (Map.keysSet prefixes == retired && included == expectedIncluded && all (\(origin, prefix) -> prefix <= available origin) (Map.toAscList prefixes))
    (Left TerminalSourceUnionClaimPartitionMismatch)
  let oldMembers = Map.keysSet (Map.fromList (structuralVersionVectorEntries terminalVector))
      newMembers = Map.keysSet (Map.fromList (structuralVersionVectorEntries successorVector))
      expectedInventoryKeys = Set.fromList [(origin, reporter) | origin <- Set.toAscList retired, reporter <- Set.toAscList newMembers]
  unless
    ( structuralVersionVectorMembershipGenerationId terminalVector == predecessor
        && structuralVersionVectorMembershipGenerationId successorVector == successor
        && newMembers `Set.isSubsetOf` oldMembers
        && oldMembers `Set.difference` newMembers == retired
        && all (\(origin, prefix) -> structuralVersionVectorComponent origin terminalVector == Just prefix) (Map.toAscList prefixes)
        && all (\(origin, prefix) -> structuralVersionVectorComponent origin terminalVector == Just prefix) (structuralVersionVectorEntries successorVector)
    )
    (Left TerminalSourceUnionClaimedVectorCoordinateMismatch)
  unless (Map.keysSet inventories == expectedInventoryKeys) (Left TerminalSourceUnionInventoryReporterSetMismatch)
  when (controlIndexWord64 controlPrefix == 0) (Left TerminalSourceUnionControlPrefixZero)
  let bytes = terminalSourceUnionBytes predecessor successor lineage retired predecessorCut inventories prefixes included ahead terminalVector successorVector controlPrefix topologyDigest
  pure
    TerminalSourceUnion
      { predecessorGeneration = predecessor,
        successorGeneration = successor,
        generationLineage = lineage,
        retiredSources = retired,
        predecessorCut,
        inventoryDigests = inventories,
        closedPrefixes = prefixes,
        includedClaims = included,
        aheadClaims = ahead,
        terminalPredecessorVector = terminalVector,
        successorInitialVector = successorVector,
        terminalControlPrefix = controlPrefix,
        terminalTopologyDigest = topologyDigest,
        canonicalBytes = bytes,
        digest = unionDigestInvariant (SHA256.hash bytes)
      }

normalizeCompoundClaims :: Set HeraldEpoch -> [TerminalSourceOccurrenceClaim] -> Either TerminalSourceProblem (Map StructuralOccurrenceId StructuralPublicationDigest)
normalizeCompoundClaims retired claims = do
  unless
    (all (\claim -> Set.member (structuralOccurrenceSourceHeraldEpoch claim.occurrence) retired) claims)
    (Left TerminalSourceUnionClaimPartitionMismatch)
  Map.unions <$> traverse (\origin -> normalizeClaims origin (filter ((== origin) . structuralOccurrenceSourceHeraldEpoch . (.occurrence)) claims)) (Set.toAscList retired)

validateDistinctGenerationClaims ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  Either TerminalSourceProblem ()
validateDistinctGenerationClaims predecessor successor =
  when
    (predecessor == successor)
    (Left (TerminalSourceGenerationCoordinateCollapsed predecessor))

validateRetiredOccurrence ::
  HeraldEpoch -> StructuralOccurrenceId -> Either TerminalSourceProblem ()
validateRetiredOccurrence retired occurrence =
  unless
    (structuralOccurrenceSourceHeraldEpoch occurrence == retired)
    ( Left
        ( TerminalSourceOccurrenceWrongSource
            retired
            occurrence
        )
    )

normalizeInventoryDigestClaims ::
  [((HeraldEpoch, HeraldEpoch), TerminalSourceInventoryDigest)] ->
  Either TerminalSourceProblem (Map (HeraldEpoch, HeraldEpoch) TerminalSourceInventoryDigest)
normalizeInventoryDigestClaims = foldM insert Map.empty
  where
    insert retained (reporter, digest) =
      case Map.lookup reporter retained of
        Nothing -> Right (Map.insert reporter digest retained)
        Just _ -> Left (TerminalSourceInventoryReporterConflict (snd reporter))

getOccurrenceId :: SerializeGet.Get StructuralOccurrenceId
getOccurrenceId =
  structuralOccurrenceId
    <$> getCheckedSized mkHeraldEpoch
    <*> (SerializeGet.getWord64be >>= getAdmitted . mkStructuralSequence)

getStructuralPrefix :: SerializeGet.Get StructuralPrefix
getStructuralPrefix = do
  tag <- SerializeGet.getWord8
  sequenceNumber <- SerializeGet.getWord64be
  case tag of
    0
      | sequenceNumber == 0 -> pure emptyStructuralPrefix
      | otherwise -> fail "non-zero empty structural prefix"
    1 -> structuralPrefixThrough <$> getAdmitted (mkStructuralSequence sequenceNumber)
    _ -> fail ("unknown structural-prefix tag " <> show tag)

getStructuralVector :: SerializeGet.Get StructuralVersionVector
getStructuralVector = do
  generation <- getCheckedSized mkHeraldMembershipGenerationId
  members <- getCheckedSized mkMemberSetDigest
  entries <- getCanonicalList getStructuralVectorEntry
  nonEmpty <- maybe (fail "empty structural vector") pure (NonEmpty.nonEmpty entries)
  getAdmitted
    (structuralVersionVectorFromClaimedCoordinates generation members nonEmpty)

getStructuralVectorEntry ::
  SerializeGet.Get (HeraldEpoch, StructuralPrefix)
getStructuralVectorEntry =
  (,)
    <$> getCheckedSized mkHeraldEpoch
    <*> getStructuralPrefix

getTerminalSourceOccurrenceClaim ::
  SerializeGet.Get TerminalSourceOccurrenceClaim
getTerminalSourceOccurrenceClaim =
  terminalSourceOccurrenceClaim
    <$> getOccurrenceId
    <*> getCheckedSized mkStructuralPublicationDigest

getInventoryDigestEntry :: SerializeGet.Get ((HeraldEpoch, HeraldEpoch), TerminalSourceInventoryDigest)
getInventoryDigestEntry = do
  origin <- getCheckedSized mkHeraldEpoch
  reporter <- getCheckedSized mkHeraldEpoch
  digest <- getCheckedSized mkTerminalSourceInventoryDigest
  pure ((origin, reporter), digest)

getStructuralCarrierRole :: SerializeGet.Get StructuralCarrierRole
getStructuralCarrierRole = do
  tag <- SerializeGet.getWord8
  case tag of
    0 -> pure NeutralVertexCarrier
    1 -> pure EdgeCarrier
    2 -> pure NablaCarrier
    3 -> pure DeltaCarrier
    4 -> pure ProcessEpochCarrier
    _ -> fail ("unknown structural-carrier tag " <> show tag)

requireCanonicalDomain :: ByteString -> SerializeGet.Get ()
requireCanonicalDomain expected = do
  actual <- getSizedBytes
  unless (actual == expected) (fail "terminal-source canonical domain mismatch")

getCheckedSized ::
  (Show problem) =>
  (ByteString -> Either problem value) ->
  SerializeGet.Get value
getCheckedSized admit = getSizedBytes >>= getAdmitted . admit

getAdmitted ::
  (Show problem) =>
  Either problem value ->
  SerializeGet.Get value
getAdmitted = either (fail . show) pure

getSizedBytes :: SerializeGet.Get ByteString
getSizedBytes = do
  count <- SerializeGet.getWord64be
  if count > fromIntegral (maxBound :: Int)
    then fail "sized canonical field does not fit the host Int"
    else SerializeGet.getByteString (fromIntegral count)

getCanonicalList :: SerializeGet.Get value -> SerializeGet.Get [value]
getCanonicalList getValue = do
  count <- SerializeGet.getWord64be
  if count > fromIntegral (maxBound :: Int)
    then fail "canonical list length does not fit the host Int"
    else replicateM (fromIntegral count) getValue

putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

putList :: (value -> Serialize.Put) -> [value] -> Serialize.Put
putList putValue values = do
  Serialize.putWord64be (fromIntegral (length values))
  traverse_ putValue values

putOccurrenceId :: StructuralOccurrenceId -> Serialize.Put
putOccurrenceId identifier = do
  putSizedBytes
    (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch identifier))
  Serialize.putWord64be
    (structuralSequenceWord64 (structuralOccurrenceSourceSequence identifier))

putStructuralPrefix :: StructuralPrefix -> Serialize.Put
putStructuralPrefix prefix = case structuralPrefixSequence prefix of
  Nothing -> do
    Serialize.putWord8 0
    Serialize.putWord64be 0
  Just sequenceNumber -> do
    Serialize.putWord8 1
    Serialize.putWord64be (structuralSequenceWord64 sequenceNumber)

putStructuralVector :: StructuralVersionVector -> Serialize.Put
putStructuralVector vector = do
  putSizedBytes
    ( heraldMembershipGenerationIdBytes
        (structuralVersionVectorMembershipGenerationId vector)
    )
  putSizedBytes
    (memberSetDigestBytes (structuralVersionVectorMemberSetDigest vector))
  putList putVectorEntry (structuralVersionVectorEntries vector)

putVectorEntry :: (HeraldEpoch, StructuralPrefix) -> Serialize.Put
putVectorEntry (herald, prefix) = do
  putSizedBytes (heraldEpochBytes herald)
  putStructuralPrefix prefix

putClaim :: TerminalSourceOccurrenceClaim -> Serialize.Put
putClaim claim = do
  putOccurrenceId claim.occurrence
  putSizedBytes (structuralPublicationDigestBytes claim.publicationDigest)

putInventoryDigestEntry :: ((HeraldEpoch, HeraldEpoch), TerminalSourceInventoryDigest) -> Serialize.Put
putInventoryDigestEntry ((origin, reporter), digest) = do
  putSizedBytes (heraldEpochBytes origin)
  putSizedBytes (heraldEpochBytes reporter)
  putSizedBytes (terminalSourceInventoryDigestBytes digest)

putAcceptanceEntry ::
  (HeraldEpoch, TerminalSourceUnionAcceptance) -> Serialize.Put
putAcceptanceEntry (reporter, acceptance) = do
  putSizedBytes (heraldEpochBytes reporter)
  putSizedBytes
    ( terminalSourceAcceptanceDigestBytes
        (terminalSourceUnionAcceptanceDigest acceptance)
    )

inventoryDigestInvariant :: ByteString -> TerminalSourceInventoryDigest
inventoryDigestInvariant bytes =
  either
    (error . ("terminal-source inventory digest invariant: " <>) . show)
    id
    (mkTerminalSourceInventoryDigest bytes)

unionDigestInvariant :: ByteString -> TerminalSourceUnionDigest
unionDigestInvariant bytes =
  either
    (error . ("terminal-source union digest invariant: " <>) . show)
    id
    (mkTerminalSourceUnionDigest bytes)

acceptanceDigestInvariant :: ByteString -> TerminalSourceAcceptanceDigest
acceptanceDigestInvariant bytes =
  either
    (error . ("terminal-source acceptance digest invariant: " <>) . show)
    id
    (mkTerminalSourceAcceptanceDigest bytes)

structuralDigestInvariant :: ByteString -> StructuralPublicationDigest
structuralDigestInvariant bytes =
  either
    (error . ("terminal structural publication digest invariant: " <>) . show)
    id
    (mkStructuralPublicationDigest bytes)
