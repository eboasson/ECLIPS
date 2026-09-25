{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Canonical identities and immutable products for context alignment.
--
-- This module owns no Herald protocol or progress state.  It fixes the exact
-- cross-Herald vocabulary from which the Alignment owner later derives class
-- generations and typed evidence digests.
module Eclips.Domain.Alignment
  ( AlignmentDeliverySequence,
    alignmentDeliverySequence,
    alignmentDeliverySequenceWord64,
    firstAlignmentDeliverySequence,
    nextAlignmentDeliverySequence,
    AlignmentPlanAttempt,
    mkAlignmentPlanAttempt,
    mkAlignmentPlanAttemptAtMembership,
    initialAlignmentPlanAttempt,
    nextAlignmentPlanAttempt,
    alignmentPlanAttemptWord64,
    alignmentPlanAttemptMembershipChange,
    StoreRevision,
    storeRevisionFromWord64,
    initialStoreRevision,
    nextStoreRevision,
    storeRevisionWord64,
    PlacementRevision,
    PlacementRevisionError (..),
    mkPlacementRevision,
    firstPlacementRevision,
    nextPlacementRevision,
    placementRevisionWord64,
    PhysicalPlacementRevisionVector,
    physicalPlacementRevisionVector,
    physicalPlacementRevisionVectorFromClaimedCoordinates,
    physicalPlacementRevisionMembershipGenerationId,
    physicalPlacementRevisionMemberSetDigest,
    physicalPlacementRevisionEntries,
    HeraldPublicationPosition,
    HeraldPublicationPositionError (..),
    mkHeraldPublicationPosition,
    firstHeraldPublicationPosition,
    nextHeraldPublicationPosition,
    heraldPublicationPositionPredecessor,
    heraldPublicationPositionWord64,
    HeraldPublicationPrefix (..),
    heraldPublicationPrefixPosition,
    AlignmentMember,
    alignmentMember,
    alignmentMemberDelta,
    alignmentMemberStoreIncarnation,
    alignmentMemberHerald,
    FreshMemberBaseEvidence,
    freshMemberBaseEvidence,
    freshMemberBaseDelta,
    freshMemberBaseStoreIncarnation,
    freshMemberBaseRevision,
    AlignmentShapeError (..),
    AlignmentCut,
    alignmentCut,
    alignmentCutAtAttempt,
    alignmentCutAttempt,
    alignmentCutSortId,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutTopologyCut,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutExactMembers,
    alignmentCutPredecessorGenerationIds,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutCanonicalBytes,
    AlignmentDigestError (..),
    ContextClassGenerationId,
    mkContextClassGenerationId,
    contextClassGenerationIdBytes,
    deriveContextClassGenerationId,
    MemberReadyEvidenceDigest,
    mkMemberReadyEvidenceDigest,
    memberReadyEvidenceDigestBytes,
    deriveMemberReadyEvidenceDigest,
    HistoricalCertificateDigest,
    mkHistoricalCertificateDigest,
    historicalCertificateDigestBytes,
    deriveHistoricalCertificateDigest,
    AlignmentSnapshotDigest,
    mkAlignmentSnapshotDigest,
    alignmentSnapshotDigestBytes,
    deriveAlignmentSnapshotDigest,
    BootstrapEvidenceDigest,
    mkBootstrapEvidenceDigest,
    bootstrapEvidenceDigestBytes,
    deriveBootstrapEvidenceDigest,
    RouteCutoverEvidenceDigest,
    mkRouteCutoverEvidenceDigest,
    routeCutoverEvidenceDigestBytes,
    deriveRouteCutoverEvidenceDigest,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize.Put qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    controlIndex,
    controlIndexWord64,
    deltaIdBytes,
    heraldEpochBytes,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.MemberSet
  ( MemberSetDigest,
    deriveMemberSetDigest,
    memberSetDigestBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    topologyCutCanonicalBytes,
    topologyCutFrontier,
    topologyFrontierMemberSetDigest,
    topologyFrontierMembershipGenerationId,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

-- | Positive sequence in one directed Herald alignment-evidence delivery lane.
-- Logical delivery survives replacement of its physical peer binding.
newtype AlignmentDeliverySequence = AlignmentDeliverySequence Word64
  deriving stock (Eq, Ord, Show)

alignmentDeliverySequence :: Word64 -> Either AlignmentShapeError AlignmentDeliverySequence
alignmentDeliverySequence 0 = Left AlignmentDeliverySequenceMustBePositive
alignmentDeliverySequence sequenceNumber = Right (AlignmentDeliverySequence sequenceNumber)

alignmentDeliverySequenceWord64 :: AlignmentDeliverySequence -> Word64
alignmentDeliverySequenceWord64 (AlignmentDeliverySequence sequenceNumber) = sequenceNumber

firstAlignmentDeliverySequence :: AlignmentDeliverySequence
firstAlignmentDeliverySequence = AlignmentDeliverySequence 1

nextAlignmentDeliverySequence :: AlignmentDeliverySequence -> AlignmentDeliverySequence
nextAlignmentDeliverySequence (AlignmentDeliverySequence sequenceNumber) = AlignmentDeliverySequence (sequenceNumber + 1)

-- | One immutable alignment-plan attempt. Membership changes order distinct
-- fixed announcers before their local retry ordinal, so a successor announcer
-- need not discover every retry of its retired predecessor. Ordinary plans may
-- carry the same attempt across membership changes. Counter exhaustion is
-- outside the finite-run prototype model.
data AlignmentPlanAttempt = AlignmentPlanAttempt !ControlIndex !Word64
  deriving stock (Eq, Ord, Show)

mkAlignmentPlanAttempt :: Word64 -> AlignmentPlanAttempt
mkAlignmentPlanAttempt = mkAlignmentPlanAttemptAtMembership (controlIndex 0)

mkAlignmentPlanAttemptAtMembership :: ControlIndex -> Word64 -> AlignmentPlanAttempt
mkAlignmentPlanAttemptAtMembership = AlignmentPlanAttempt

initialAlignmentPlanAttempt :: AlignmentPlanAttempt
initialAlignmentPlanAttempt = mkAlignmentPlanAttempt 0

nextAlignmentPlanAttempt :: AlignmentPlanAttempt -> AlignmentPlanAttempt
nextAlignmentPlanAttempt (AlignmentPlanAttempt membershipChange attempt) = AlignmentPlanAttempt membershipChange (attempt + 1)

alignmentPlanAttemptWord64 :: AlignmentPlanAttempt -> Word64
alignmentPlanAttemptWord64 (AlignmentPlanAttempt _ attempt) = attempt

alignmentPlanAttemptMembershipChange :: AlignmentPlanAttempt -> ControlIndex
alignmentPlanAttemptMembershipChange (AlignmentPlanAttempt membershipChange _) = membershipChange

-- | Monotone revision of one exact store incarnation.  A fresh incarnation is
-- revision zero; profile-0.1 counter exhaustion is outside the finite-run
-- model.
newtype StoreRevision = StoreRevision Word64
  deriving stock (Eq, Ord, Show)

storeRevisionFromWord64 :: Word64 -> StoreRevision
storeRevisionFromWord64 = StoreRevision

initialStoreRevision :: StoreRevision
initialStoreRevision = StoreRevision 0

nextStoreRevision :: StoreRevision -> StoreRevision
nextStoreRevision (StoreRevision revision) = StoreRevision (revision + 1)

storeRevisionWord64 :: StoreRevision -> Word64
storeRevisionWord64 (StoreRevision revision) = revision

-- | Positive sequence of one Herald's authoritative physical-placement set.
newtype PlacementRevision = PlacementRevision Word64
  deriving stock (Eq, Ord, Show)

data PlacementRevisionError
  = PlacementRevisionMustBePositive
  deriving stock (Eq, Show)

mkPlacementRevision :: Word64 -> Either PlacementRevisionError PlacementRevision
mkPlacementRevision 0 = Left PlacementRevisionMustBePositive
mkPlacementRevision revision = Right (PlacementRevision revision)

firstPlacementRevision :: PlacementRevision
firstPlacementRevision = PlacementRevision 1

nextPlacementRevision :: PlacementRevision -> PlacementRevision
nextPlacementRevision (PlacementRevision revision) =
  PlacementRevision (revision + 1)

placementRevisionWord64 :: PlacementRevision -> Word64
placementRevisionWord64 (PlacementRevision revision) = revision

-- | Exact generation-qualified physical-placement cut in ascending Herald
-- order.
data PhysicalPlacementRevisionVector
  = PhysicalPlacementRevisionVector
      HeraldMembershipGenerationId
      MemberSetDigest
      (NonEmpty (HeraldEpoch, PlacementRevision))
  deriving stock (Eq, Ord, Show)

-- | Positive position in the local all-process accepted-publication order.
newtype HeraldPublicationPosition = HeraldPublicationPosition Word64
  deriving stock (Eq, Ord, Show)

data HeraldPublicationPositionError
  = HeraldPublicationPositionMustBePositive
  deriving stock (Eq, Show)

mkHeraldPublicationPosition ::
  Word64 -> Either HeraldPublicationPositionError HeraldPublicationPosition
mkHeraldPublicationPosition 0 = Left HeraldPublicationPositionMustBePositive
mkHeraldPublicationPosition position = Right (HeraldPublicationPosition position)

firstHeraldPublicationPosition :: HeraldPublicationPosition
firstHeraldPublicationPosition = HeraldPublicationPosition 1

nextHeraldPublicationPosition ::
  HeraldPublicationPosition -> HeraldPublicationPosition
nextHeraldPublicationPosition (HeraldPublicationPosition position) =
  HeraldPublicationPosition (position + 1)

heraldPublicationPositionPredecessor ::
  HeraldPublicationPosition -> Maybe HeraldPublicationPosition
heraldPublicationPositionPredecessor (HeraldPublicationPosition 1) = Nothing
heraldPublicationPositionPredecessor (HeraldPublicationPosition position) =
  Just (HeraldPublicationPosition (position - 1))

heraldPublicationPositionWord64 :: HeraldPublicationPosition -> Word64
heraldPublicationPositionWord64 (HeraldPublicationPosition position) = position

-- | Inclusive accepted-publication prefix.  The explicit empty constructor
-- avoids assigning sentinel meaning to numeric zero.
data HeraldPublicationPrefix
  = EmptyHeraldPublicationPrefix
  | HeraldPublicationPrefixThrough HeraldPublicationPosition
  deriving stock (Eq, Ord, Show)

heraldPublicationPrefixPosition ::
  HeraldPublicationPrefix -> Maybe HeraldPublicationPosition
heraldPublicationPrefixPosition EmptyHeraldPublicationPrefix = Nothing
heraldPublicationPrefixPosition
  (HeraldPublicationPrefixThrough position) = Just position

-- | One exact physical member of a context class.  Field and canonical order
-- is delta, store incarnation, then hosting Herald.
data AlignmentMember = AlignmentMember
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId,
    herald :: HeraldEpoch
  }
  deriving stock (Eq, Ord, Show)

alignmentMember ::
  DeltaId -> StoreIncarnationId -> HeraldEpoch -> AlignmentMember
alignmentMember = AlignmentMember

alignmentMemberDelta :: AlignmentMember -> DeltaId
alignmentMemberDelta member = member.delta

alignmentMemberStoreIncarnation :: AlignmentMember -> StoreIncarnationId
alignmentMemberStoreIncarnation member = member.storeIncarnation

alignmentMemberHerald :: AlignmentMember -> HeraldEpoch
alignmentMemberHerald member = member.herald

-- | Captured base of a genuinely fresh physical member.
data FreshMemberBaseEvidence = FreshMemberBaseEvidence
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId,
    revision :: StoreRevision
  }
  deriving stock (Eq, Ord, Show)

freshMemberBaseEvidence ::
  DeltaId -> StoreIncarnationId -> StoreRevision -> FreshMemberBaseEvidence
freshMemberBaseEvidence = FreshMemberBaseEvidence

freshMemberBaseDelta :: FreshMemberBaseEvidence -> DeltaId
freshMemberBaseDelta evidence = evidence.delta

freshMemberBaseStoreIncarnation ::
  FreshMemberBaseEvidence -> StoreIncarnationId
freshMemberBaseStoreIncarnation evidence = evidence.storeIncarnation

freshMemberBaseRevision :: FreshMemberBaseEvidence -> StoreRevision
freshMemberBaseRevision evidence = evidence.revision

data AlignmentShapeError
  = AlignmentDeliverySequenceMustBePositive
  | PhysicalPlacementRevisionDuplicateHerald HeraldEpoch
  | PhysicalPlacementRevisionMembershipMismatch
      [HeraldEpoch]
      [HeraldEpoch]
  | PhysicalPlacementRevisionClaimedMemberSetDigestMismatch
      MemberSetDigest
      MemberSetDigest
  | AlignmentTopologyPlacementGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | AlignmentTopologyPlacementMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  | AlignmentMemberConflict DeltaId AlignmentMember AlignmentMember
  | AlignmentMemberStoreIncarnationConflict
      StoreIncarnationId
      AlignmentMember
      AlignmentMember
  | AlignmentMemberHeraldOutsidePlacement HeraldEpoch
  | FreshMemberBaseConflict
      DeltaId
      FreshMemberBaseEvidence
      FreshMemberBaseEvidence
  | FreshMemberBaseNotExactMember DeltaId StoreIncarnationId
  deriving stock (Eq, Show)

physicalPlacementRevisionVector ::
  HeraldMembershipGeneration ->
  [(HeraldEpoch, PlacementRevision)] ->
  Either AlignmentShapeError PhysicalPlacementRevisionVector
physicalPlacementRevisionVector generation supplied = do
  let expected =
        NonEmpty.toList
          (heraldMembershipGenerationActiveHeraldEpochs generation)
  revisions <- foldPlacementEntries supplied
  let actual = Map.keys revisions
  if actual == expected
    then case Map.toAscList revisions of
      first : remaining ->
        Right
          ( PhysicalPlacementRevisionVector
              (heraldMembershipGenerationId generation)
              (heraldMembershipGenerationActiveMemberSetDigest generation)
              (first :| remaining)
          )
      [] -> error "non-empty checked placement membership produced an empty vector"
    else
      Left
        ( PhysicalPlacementRevisionMembershipMismatch
            expected
            actual
        )

-- | Boundary-only construction from already shape-checked wire coordinates.
--
-- This normalizes the non-empty entry presentation, rejects duplicate Heralds,
-- and verifies that the claimed member-set digest names exactly those entry
-- keys. It deliberately defers resolution of the claimed generation ID to the
-- owning state. Ordinary owner code should prefer
-- 'physicalPlacementRevisionVector' with a checked 'HeraldMembershipGeneration'.
physicalPlacementRevisionVectorFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  NonEmpty (HeraldEpoch, PlacementRevision) ->
  Either AlignmentShapeError PhysicalPlacementRevisionVector
physicalPlacementRevisionVectorFromClaimedCoordinates generation members supplied = do
  revisions <- foldPlacementEntries (NonEmpty.toList supplied)
  case Map.toAscList revisions of
    first : remaining -> do
      let normalized = first :| remaining
          derivedMembers = deriveMemberSetDigest (fmap fst normalized)
      if members == derivedMembers
        then
          Right
            ( PhysicalPlacementRevisionVector
                generation
                members
                normalized
            )
        else
          Left
            ( PhysicalPlacementRevisionClaimedMemberSetDigestMismatch
                derivedMembers
                members
            )
    [] -> error "non-empty claimed placement vector normalized to empty"

foldPlacementEntries ::
  [(HeraldEpoch, PlacementRevision)] ->
  Either AlignmentShapeError (Map HeraldEpoch PlacementRevision)
foldPlacementEntries = foldl insertRevision (Right Map.empty)
  where
    insertRevision accumulated (herald, revision) = do
      current <- accumulated
      case Map.lookup herald current of
        Nothing -> Right (Map.insert herald revision current)
        Just _ -> Left (PhysicalPlacementRevisionDuplicateHerald herald)

physicalPlacementRevisionMembershipGenerationId ::
  PhysicalPlacementRevisionVector -> HeraldMembershipGenerationId
physicalPlacementRevisionMembershipGenerationId
  (PhysicalPlacementRevisionVector generation _ _) = generation

physicalPlacementRevisionMemberSetDigest ::
  PhysicalPlacementRevisionVector -> MemberSetDigest
physicalPlacementRevisionMemberSetDigest
  (PhysicalPlacementRevisionVector _ members _) = members

physicalPlacementRevisionEntries ::
  PhysicalPlacementRevisionVector ->
  NonEmpty (HeraldEpoch, PlacementRevision)
physicalPlacementRevisionEntries
  (PhysicalPlacementRevisionVector _ _ entries) = entries

-- | One shared class-generation input.  Predecessor certificates authorize
-- construction and bootstrap transfer, but deliberately do not enter this
-- identity: only their frozen generation IDs do.
data AlignmentCut = AlignmentCut
  { attempt :: AlignmentPlanAttempt,
    sortId :: SortId,
    sortDefinitionOccurrenceId :: SortDefinitionOccurrenceId,
    topologyCut :: TopologyCut,
    physicalPlacementRevisionVector :: PhysicalPlacementRevisionVector,
    exactMembers :: NonEmpty AlignmentMember,
    predecessorGenerationIds :: [ContextClassGenerationId],
    freshMemberBases :: [FreshMemberBaseEvidence]
  }
  deriving stock (Eq, Show)

alignmentCut ::
  SortId ->
  SortDefinitionOccurrenceId ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  NonEmpty AlignmentMember ->
  [ContextClassGenerationId] ->
  [FreshMemberBaseEvidence] ->
  Either AlignmentShapeError AlignmentCut
alignmentCut = alignmentCutAtAttempt initialAlignmentPlanAttempt

alignmentCutAtAttempt ::
  AlignmentPlanAttempt ->
  SortId ->
  SortDefinitionOccurrenceId ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  NonEmpty AlignmentMember ->
  [ContextClassGenerationId] ->
  [FreshMemberBaseEvidence] ->
  Either AlignmentShapeError AlignmentCut
alignmentCutAtAttempt attempt sortId occurrence topology placement suppliedMembers suppliedPredecessors suppliedFresh = do
  let topologyGeneration =
        topologyFrontierMembershipGenerationId (topologyCutFrontier topology)
      placementGeneration =
        physicalPlacementRevisionMembershipGenerationId placement
      topologyMembers =
        topologyFrontierMemberSetDigest (topologyCutFrontier topology)
      placementMembers = physicalPlacementRevisionMemberSetDigest placement
  if placementGeneration == topologyGeneration
    then Right ()
    else
      Left
        ( AlignmentTopologyPlacementGenerationMismatch
            topologyGeneration
            placementGeneration
        )
  if placementMembers == topologyMembers
    then Right ()
    else
      Left
        ( AlignmentTopologyPlacementMemberSetMismatch
            topologyMembers
            placementMembers
        )
  membersByDelta <- normalizeMembers suppliedMembers
  validateUniqueMemberStores (Map.elems membersByDelta)
  let placementHeralds =
        Set.fromList
          (fmap fst (NonEmpty.toList (physicalPlacementRevisionEntries placement)))
  mapM_ (validateMemberHerald placementHeralds) (Map.elems membersByDelta)
  freshByDelta <- normalizeFreshBases suppliedFresh
  mapM_ (validateFreshMember membersByDelta) (Map.elems freshByDelta)
  case Map.elems membersByDelta of
    first : remaining ->
      Right
        AlignmentCut
          { attempt,
            sortId,
            sortDefinitionOccurrenceId = occurrence,
            topologyCut = topology,
            physicalPlacementRevisionVector = placement,
            exactMembers = first :| remaining,
            predecessorGenerationIds =
              Set.toAscList (Set.fromList suppliedPredecessors),
            freshMemberBases = Map.elems freshByDelta
          }
    [] -> error "non-empty supplied alignment members normalized to empty"

alignmentCutAttempt :: AlignmentCut -> AlignmentPlanAttempt
alignmentCutAttempt cut = cut.attempt

alignmentCutSortId :: AlignmentCut -> SortId
alignmentCutSortId cut = cut.sortId

alignmentCutSortDefinitionOccurrenceId ::
  AlignmentCut -> SortDefinitionOccurrenceId
alignmentCutSortDefinitionOccurrenceId cut = cut.sortDefinitionOccurrenceId

alignmentCutTopologyCut :: AlignmentCut -> TopologyCut
alignmentCutTopologyCut cut = cut.topologyCut

alignmentCutPhysicalPlacementRevisionVector ::
  AlignmentCut -> PhysicalPlacementRevisionVector
alignmentCutPhysicalPlacementRevisionVector cut =
  cut.physicalPlacementRevisionVector

alignmentCutExactMembers :: AlignmentCut -> NonEmpty AlignmentMember
alignmentCutExactMembers cut = cut.exactMembers

alignmentCutPredecessorGenerationIds ::
  AlignmentCut -> [ContextClassGenerationId]
alignmentCutPredecessorGenerationIds cut = cut.predecessorGenerationIds

alignmentCutFreshMemberBaseEvidence ::
  AlignmentCut -> [FreshMemberBaseEvidence]
alignmentCutFreshMemberBaseEvidence cut = cut.freshMemberBases

-- | Stable canonical transcript hashed directly to obtain the sole class-cut
-- identity.  Tags and field order are part of profile 0.1:
--
-- @ECLIPS-ALIGNMENT-CUT@, plan attempt, sort, definition occurrence, complete canonical
-- topology cut, placement vector, exact members, predecessor generation IDs,
-- and fresh-member bases.
alignmentCutCanonicalBytes :: AlignmentCut -> ByteString
alignmentCutCanonicalBytes cut = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-CUT"
  Serialize.putWord64be (controlIndexWord64 (alignmentPlanAttemptMembershipChange cut.attempt))
  Serialize.putWord64be (alignmentPlanAttemptWord64 cut.attempt)
  Serialize.putByteString (sortIdBytes cut.sortId)
  Serialize.putByteString
    (sortDefinitionOccurrenceIdBytes cut.sortDefinitionOccurrenceId)
  putSizedBytes (topologyCutCanonicalBytes cut.topologyCut)
  Serialize.putByteString
    ( heraldMembershipGenerationIdBytes
        ( physicalPlacementRevisionMembershipGenerationId
            cut.physicalPlacementRevisionVector
        )
    )
  Serialize.putByteString
    ( memberSetDigestBytes
        (physicalPlacementRevisionMemberSetDigest cut.physicalPlacementRevisionVector)
    )
  putCounted
    putPlacementEntry
    (NonEmpty.toList (physicalPlacementRevisionEntries cut.physicalPlacementRevisionVector))
  putCounted putAlignmentMember (NonEmpty.toList cut.exactMembers)
  putCounted
    (Serialize.putByteString . contextClassGenerationIdBytes)
    cut.predecessorGenerationIds
  putCounted putFreshMemberBase cut.freshMemberBases

data AlignmentDigestError = AlignmentDigestWrongByteCount
  { expectedAlignmentDigestByteCount :: Int,
    actualAlignmentDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype AlignmentDigest domain = AlignmentDigest ByteString
  deriving stock (Eq, Ord)

instance Show (AlignmentDigest domain) where
  show = renderGroupedHex . alignmentDigestBytes

type role AlignmentDigest nominal

data ContextClassGenerationDomain

type ContextClassGenerationId = AlignmentDigest ContextClassGenerationDomain

data MemberReadyEvidenceDomain

type MemberReadyEvidenceDigest = AlignmentDigest MemberReadyEvidenceDomain

data HistoricalCertificateDomain

type HistoricalCertificateDigest = AlignmentDigest HistoricalCertificateDomain

data AlignmentSnapshotDomain

type AlignmentSnapshotDigest = AlignmentDigest AlignmentSnapshotDomain

data BootstrapEvidenceDomain

type BootstrapEvidenceDigest = AlignmentDigest BootstrapEvidenceDomain

data RouteCutoverEvidenceDomain

type RouteCutoverEvidenceDigest = AlignmentDigest RouteCutoverEvidenceDomain

mkContextClassGenerationId ::
  ByteString -> Either AlignmentDigestError ContextClassGenerationId
mkContextClassGenerationId = checkedAlignmentDigest

contextClassGenerationIdBytes :: ContextClassGenerationId -> ByteString
contextClassGenerationIdBytes = alignmentDigestBytes

-- | The generation identity is exactly SHA-256 of the canonical cut, with no
-- second alignment-cut digest or source-certificate input.
deriveContextClassGenerationId :: AlignmentCut -> ContextClassGenerationId
deriveContextClassGenerationId =
  digestInvariant "context-class generation" mkContextClassGenerationId
    . SHA256.hash
    . alignmentCutCanonicalBytes

mkMemberReadyEvidenceDigest ::
  ByteString -> Either AlignmentDigestError MemberReadyEvidenceDigest
mkMemberReadyEvidenceDigest = checkedAlignmentDigest

memberReadyEvidenceDigestBytes :: MemberReadyEvidenceDigest -> ByteString
memberReadyEvidenceDigestBytes = alignmentDigestBytes

deriveMemberReadyEvidenceDigest :: ByteString -> MemberReadyEvidenceDigest
deriveMemberReadyEvidenceDigest =
  derivePayloadDigest
    "member-ready evidence"
    "ECLIPS-ALIGNMENT-MEMBER-READY"
    mkMemberReadyEvidenceDigest

mkHistoricalCertificateDigest ::
  ByteString -> Either AlignmentDigestError HistoricalCertificateDigest
mkHistoricalCertificateDigest = checkedAlignmentDigest

historicalCertificateDigestBytes :: HistoricalCertificateDigest -> ByteString
historicalCertificateDigestBytes = alignmentDigestBytes

deriveHistoricalCertificateDigest :: ByteString -> HistoricalCertificateDigest
deriveHistoricalCertificateDigest =
  derivePayloadDigest
    "historical certificate"
    "ECLIPS-HISTORICAL-CERTIFICATE"
    mkHistoricalCertificateDigest

mkAlignmentSnapshotDigest ::
  ByteString -> Either AlignmentDigestError AlignmentSnapshotDigest
mkAlignmentSnapshotDigest = checkedAlignmentDigest

alignmentSnapshotDigestBytes :: AlignmentSnapshotDigest -> ByteString
alignmentSnapshotDigestBytes = alignmentDigestBytes

deriveAlignmentSnapshotDigest :: ByteString -> AlignmentSnapshotDigest
deriveAlignmentSnapshotDigest =
  derivePayloadDigest
    "alignment snapshot"
    "ECLIPS-ALIGNMENT-SNAPSHOT"
    mkAlignmentSnapshotDigest

mkBootstrapEvidenceDigest ::
  ByteString -> Either AlignmentDigestError BootstrapEvidenceDigest
mkBootstrapEvidenceDigest = checkedAlignmentDigest

bootstrapEvidenceDigestBytes :: BootstrapEvidenceDigest -> ByteString
bootstrapEvidenceDigestBytes = alignmentDigestBytes

deriveBootstrapEvidenceDigest :: ByteString -> BootstrapEvidenceDigest
deriveBootstrapEvidenceDigest =
  derivePayloadDigest
    "bootstrap evidence"
    "ECLIPS-ALIGNMENT-BOOTSTRAP-EVIDENCE"
    mkBootstrapEvidenceDigest

mkRouteCutoverEvidenceDigest ::
  ByteString -> Either AlignmentDigestError RouteCutoverEvidenceDigest
mkRouteCutoverEvidenceDigest = checkedAlignmentDigest

routeCutoverEvidenceDigestBytes :: RouteCutoverEvidenceDigest -> ByteString
routeCutoverEvidenceDigestBytes = alignmentDigestBytes

deriveRouteCutoverEvidenceDigest :: ByteString -> RouteCutoverEvidenceDigest
deriveRouteCutoverEvidenceDigest =
  derivePayloadDigest
    "route-cutover evidence"
    "ECLIPS-ALIGNMENT-ROUTE-CUTOVER"
    mkRouteCutoverEvidenceDigest

normalizeMembers ::
  NonEmpty AlignmentMember ->
  Either AlignmentShapeError (Map DeltaId AlignmentMember)
normalizeMembers =
  foldl insertMember (Right Map.empty) . NonEmpty.toList
  where
    insertMember accumulated candidate = do
      current <- accumulated
      case Map.lookup candidate.delta current of
        Nothing -> Right (Map.insert candidate.delta candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise ->
              Left
                ( AlignmentMemberConflict
                    candidate.delta
                    incumbent
                    candidate
                )

validateUniqueMemberStores ::
  [AlignmentMember] -> Either AlignmentShapeError ()
validateUniqueMemberStores = go Map.empty
  where
    go _ [] = Right ()
    go stores (candidate : remaining) =
      case Map.lookup candidate.storeIncarnation stores of
        Nothing ->
          go
            (Map.insert candidate.storeIncarnation candidate stores)
            remaining
        Just incumbent
          | incumbent == candidate -> go stores remaining
          | otherwise ->
              Left
                ( AlignmentMemberStoreIncarnationConflict
                    candidate.storeIncarnation
                    incumbent
                    candidate
                )

validateMemberHerald ::
  Set.Set HeraldEpoch -> AlignmentMember -> Either AlignmentShapeError ()
validateMemberHerald placementHeralds member
  | Set.member member.herald placementHeralds = Right ()
  | otherwise = Left (AlignmentMemberHeraldOutsidePlacement member.herald)

normalizeFreshBases ::
  [FreshMemberBaseEvidence] ->
  Either
    AlignmentShapeError
    (Map DeltaId FreshMemberBaseEvidence)
normalizeFreshBases = foldl insertFresh (Right Map.empty)
  where
    insertFresh accumulated candidate = do
      current <- accumulated
      case Map.lookup candidate.delta current of
        Nothing ->
          Right (Map.insert candidate.delta candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise ->
              Left
                ( FreshMemberBaseConflict
                    candidate.delta
                    incumbent
                    candidate
                )

validateFreshMember ::
  Map DeltaId AlignmentMember ->
  FreshMemberBaseEvidence ->
  Either AlignmentShapeError ()
validateFreshMember members evidence =
  case Map.lookup evidence.delta members of
    Just member
      | member.storeIncarnation == evidence.storeIncarnation -> Right ()
    _ ->
      Left
        ( FreshMemberBaseNotExactMember
            evidence.delta
            evidence.storeIncarnation
        )

checkedAlignmentDigest ::
  ByteString -> Either AlignmentDigestError (AlignmentDigest domain)
checkedAlignmentDigest bytes
  | ByteString.length bytes == digestByteCount = Right (AlignmentDigest bytes)
  | otherwise =
      Left
        AlignmentDigestWrongByteCount
          { expectedAlignmentDigestByteCount = digestByteCount,
            actualAlignmentDigestByteCount = ByteString.length bytes
          }

alignmentDigestBytes :: AlignmentDigest domain -> ByteString
alignmentDigestBytes (AlignmentDigest bytes) = bytes

derivePayloadDigest ::
  String ->
  ByteString ->
  (ByteString -> Either AlignmentDigestError digest) ->
  ByteString ->
  digest
derivePayloadDigest label tag constructor payload =
  digestInvariant
    label
    constructor
    ( SHA256.hash
        ( Serialize.runPut $ do
            putSizedBytes tag
            putSizedBytes payload
        )
    )

digestInvariant ::
  String ->
  (ByteString -> Either AlignmentDigestError digest) ->
  ByteString ->
  digest
digestInvariant label constructor bytes =
  either (error . ((label <> " invariant: ") <>) . show) id (constructor bytes)

putPlacementEntry :: (HeraldEpoch, PlacementRevision) -> Serialize.Put
putPlacementEntry (herald, revision) = do
  Serialize.putByteString (heraldEpochBytes herald)
  Serialize.putWord64be (placementRevisionWord64 revision)

putAlignmentMember :: AlignmentMember -> Serialize.Put
putAlignmentMember member = do
  Serialize.putByteString (deltaIdBytes member.delta)
  Serialize.putByteString (storeIncarnationIdBytes member.storeIncarnation)
  Serialize.putByteString (heraldEpochBytes member.herald)

putFreshMemberBase :: FreshMemberBaseEvidence -> Serialize.Put
putFreshMemberBase evidence = do
  Serialize.putByteString (deltaIdBytes evidence.delta)
  Serialize.putByteString
    (storeIncarnationIdBytes evidence.storeIncarnation)
  Serialize.putWord64be (storeRevisionWord64 evidence.revision)

putCounted :: (value -> Serialize.Put) -> [value] -> Serialize.Put
putCounted putValue values = do
  Serialize.putWord64be (fromIntegral (length values))
  mapM_ putValue values

putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

digestByteCount :: Int
digestByteCount = 32
