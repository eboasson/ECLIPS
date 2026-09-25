-- | Package-private retained vocabulary for one accepted private environment.
--
-- This module deliberately does not import any Herald owner.  The application
-- coordinator can therefore prepare root plans, the Publication owner can add
-- immutable publication identities and Herald positions, and Application can
-- retain the resulting manifest without introducing an owner import cycle.
module Eclips.Herald.Application.Environment
  ( EnvironmentManifestKey,
    EnvironmentManifestProblem (..),
    environmentManifestKey,
    environmentManifestKeyProcess,
    environmentManifestKeyPosition,
    EnvironmentRootPlan,
    environmentRootPlan,
    environmentRootPlanSlot,
    environmentRootPlanGeneratedId,
    environmentRootPlanTargetObject,
    environmentRootPlanSourceNabla,
    environmentRootPlanSourceSequencingObject,
    environmentRootPlanSourceAuthority,
    environmentRootPlanDescriptor,
    environmentRootPlanValue,
    environmentRootPlanOccurrenceId,
    environmentRootPlanSourceStrength,
    environmentRootPlanSourceTopologyPrerequisite,
    environmentRootPlanControlPrerequisite,
    environmentRootPlanRoute,
    PositionedEnvironmentRoot,
    positionEnvironmentRoot,
    positionedEnvironmentRootPlan,
    positionedEnvironmentRootChecked,
    positionedEnvironmentRootPublicationId,
    positionedEnvironmentRootHeraldPosition,
    EnvironmentManifest,
    environmentManifest,
    environmentManifestKeyOf,
    environmentManifestProcess,
    environmentManifestPosition,
    environmentManifestMembershipGenerationId,
    environmentManifestActiveMemberSetDigest,
    environmentManifestRoots,
    environmentManifestGeneratedIds,
    environmentManifestComplete,
    environmentManifestExtends,
    EnvironmentReplyKey,
    environmentReplyKey,
    environmentReplyKeySession,
    environmentReplyKeyRequest,
    PendingEnvironment,
    pendingEnvironment,
    pendingEnvironmentManifest,
    pendingEnvironmentReplyKey,
    detachPendingEnvironmentReply,
    EnvironmentSettlementEvidenceProblem (..),
    EnvironmentSettlementEvidence,
    environmentSettlementEvidence,
    environmentSettlementEvidenceCut,
    environmentSettlementEvidenceMembershipGenerationId,
    environmentSettlementEvidenceMemberSetDigest,
    environmentSettlementEvidenceOccurrences,
    CompletedEnvironment,
    completePendingEnvironment,
    completedEnvironmentManifest,
    completedEnvironmentSettlementEvidence,
    completedEnvironmentAccess,
    completedEnvironmentReplyKey,
    detachCompletedEnvironmentReply,
    retireCompletedEnvironmentAccess,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access (EnvironmentAccess)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    heraldPublicationPositionWord64,
  )
import Eclips.Domain.Environment
  ( EnvironmentManifestShape,
    EnvironmentRootSlot,
    environmentConnectedObjectCount,
    environmentManifestRootCount,
    environmentManifestShapeRoots,
    environmentRootSlotStructuralCarrierRole,
    environmentWiringSlots,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    GlobalUniqueId,
    NablaId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    StructuralOccurrenceId,
    TopologyCutId,
    globalObjectIdFromGlobalUniqueId,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
  )
import Eclips.Domain.MemberSet (MemberSetDigest)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationSort,
    checkedPublicationValue,
  )
import Eclips.Domain.Route (FrozenRoute, ReplicaStrength)
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier),
  )
import Eclips.Domain.Value (Value)
import Eclips.Herald.Application.Request.Internal
  ( ProcessAcceptancePosition,
    RequestId,
    processAcceptancePositionProcess,
  )
import Eclips.Herald.Application.Session.Internal (ApplicationSessionId)

-- | Retry-stable identity of one environment request.  The process is retained
-- explicitly because it is the environment owner; the checked constructor
-- proves that the process-local acceptance position names that same epoch.
data EnvironmentManifestKey
  = EnvironmentManifestKey ProcessEpochId ProcessAcceptancePosition
  deriving stock (Eq, Ord, Show)

data EnvironmentManifestProblem
  = EnvironmentManifestPositionProcessMismatch
      ProcessEpochId
      ProcessEpochId
  | EnvironmentRootPublicationSourceMismatch NablaId NablaId
  | EnvironmentRootPublicationAuthorityMismatch AuthorityEpoch AuthorityEpoch
  | EnvironmentRootPublicationSortMismatch
  | EnvironmentRootPublicationValueMismatch
  | EnvironmentManifestShapeMismatch
  | EnvironmentManifestDuplicateGeneratedId GlobalUniqueId
  | EnvironmentManifestDuplicatePublicationId PublicationId
  | EnvironmentManifestPositionNotConsecutive Word64 Word64
  | EnvironmentManifestCarrierSourceMismatch StructuralCarrierRole
  | EnvironmentManifestCarrierSequenceNotPositive StructuralCarrierRole
  | EnvironmentManifestCarrierSequenceNotConsecutive
      StructuralCarrierRole
      Word64
      Word64
  deriving stock (Eq, Show)

environmentManifestKey ::
  ProcessEpochId ->
  ProcessAcceptancePosition ->
  Either EnvironmentManifestProblem EnvironmentManifestKey
environmentManifestKey process position
  | processAcceptancePositionProcess position == process =
      Right (EnvironmentManifestKey process position)
  | otherwise =
      Left
        ( EnvironmentManifestPositionProcessMismatch
            process
            (processAcceptancePositionProcess position)
        )

environmentManifestKeyProcess :: EnvironmentManifestKey -> ProcessEpochId
environmentManifestKeyProcess (EnvironmentManifestKey process _) = process

environmentManifestKeyPosition ::
  EnvironmentManifestKey -> ProcessAcceptancePosition
environmentManifestKeyPosition (EnvironmentManifestKey _ position) = position

-- | Complete root semantics before Publication allocates provenance and a
-- Herald-wide position.  Every field is already frozen at the acceptance cut.
data EnvironmentRootPlan
  = EnvironmentRootPlan
      EnvironmentRootSlot
      GlobalUniqueId
      NablaId
      (Maybe GlobalObjectId)
      AuthorityEpoch
      CanonicalDescriptor
      Value
      SortDefinitionOccurrenceId
      ReplicaStrength
      TopologyCutId
      ControlIndex
      FrozenRoute
  deriving stock (Eq, Show)

environmentRootPlan ::
  EnvironmentRootSlot ->
  GlobalUniqueId ->
  NablaId ->
  Maybe GlobalObjectId ->
  AuthorityEpoch ->
  CanonicalDescriptor ->
  Value ->
  SortDefinitionOccurrenceId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  FrozenRoute ->
  EnvironmentRootPlan
environmentRootPlan = EnvironmentRootPlan

environmentRootPlanSlot :: EnvironmentRootPlan -> EnvironmentRootSlot
environmentRootPlanSlot (EnvironmentRootPlan slot _ _ _ _ _ _ _ _ _ _ _) = slot

environmentRootPlanGeneratedId :: EnvironmentRootPlan -> GlobalUniqueId
environmentRootPlanGeneratedId (EnvironmentRootPlan _ generated _ _ _ _ _ _ _ _ _ _) = generated

environmentRootPlanTargetObject :: EnvironmentRootPlan -> GlobalObjectId
environmentRootPlanTargetObject =
  globalObjectIdFromGlobalUniqueId . environmentRootPlanGeneratedId

environmentRootPlanSourceNabla :: EnvironmentRootPlan -> NablaId
environmentRootPlanSourceNabla (EnvironmentRootPlan _ _ source _ _ _ _ _ _ _ _ _) = source

environmentRootPlanSourceSequencingObject :: EnvironmentRootPlan -> Maybe GlobalObjectId
environmentRootPlanSourceSequencingObject (EnvironmentRootPlan _ _ _ sequencing _ _ _ _ _ _ _ _) = sequencing

environmentRootPlanSourceAuthority :: EnvironmentRootPlan -> AuthorityEpoch
environmentRootPlanSourceAuthority (EnvironmentRootPlan _ _ _ _ authority _ _ _ _ _ _ _) = authority

environmentRootPlanDescriptor :: EnvironmentRootPlan -> CanonicalDescriptor
environmentRootPlanDescriptor (EnvironmentRootPlan _ _ _ _ _ descriptor _ _ _ _ _ _) = descriptor

environmentRootPlanValue :: EnvironmentRootPlan -> Value
environmentRootPlanValue (EnvironmentRootPlan _ _ _ _ _ _ value _ _ _ _ _) = value

environmentRootPlanOccurrenceId :: EnvironmentRootPlan -> SortDefinitionOccurrenceId
environmentRootPlanOccurrenceId (EnvironmentRootPlan _ _ _ _ _ _ _ occurrence _ _ _ _) = occurrence

environmentRootPlanSourceStrength :: EnvironmentRootPlan -> ReplicaStrength
environmentRootPlanSourceStrength (EnvironmentRootPlan _ _ _ _ _ _ _ _ strength _ _ _) = strength

environmentRootPlanSourceTopologyPrerequisite ::
  EnvironmentRootPlan -> TopologyCutId
environmentRootPlanSourceTopologyPrerequisite
  (EnvironmentRootPlan _ _ _ _ _ _ _ _ _ topology _ _) = topology

environmentRootPlanControlPrerequisite :: EnvironmentRootPlan -> ControlIndex
environmentRootPlanControlPrerequisite
  (EnvironmentRootPlan _ _ _ _ _ _ _ _ _ _ control _) = control

environmentRootPlanRoute :: EnvironmentRootPlan -> FrozenRoute
environmentRootPlanRoute (EnvironmentRootPlan _ _ _ _ _ _ _ _ _ _ _ route) = route

-- | One root after Publication has allocated and checked immutable provenance.
data PositionedEnvironmentRoot
  = PositionedEnvironmentRoot
      EnvironmentRootPlan
      CheckedPublication
      HeraldPublicationPosition
  deriving stock (Eq, Show)

positionEnvironmentRoot ::
  EnvironmentRootPlan ->
  CheckedPublication ->
  HeraldPublicationPosition ->
  Either EnvironmentManifestProblem PositionedEnvironmentRoot
positionEnvironmentRoot plan checked position
  | publicationNabla identifier /= environmentRootPlanSourceNabla plan =
      Left
        ( EnvironmentRootPublicationSourceMismatch
            (environmentRootPlanSourceNabla plan)
            (publicationNabla identifier)
        )
  | publicationAuthorityEpoch identifier
      /= environmentRootPlanSourceAuthority plan =
      Left
        ( EnvironmentRootPublicationAuthorityMismatch
            (environmentRootPlanSourceAuthority plan)
            (publicationAuthorityEpoch identifier)
        )
  | checkedPublicationSort checked
      /= descriptorSortId (environmentRootPlanDescriptor plan) =
      Left EnvironmentRootPublicationSortMismatch
  | checkedPublicationValue checked /= environmentRootPlanValue plan =
      Left EnvironmentRootPublicationValueMismatch
  | otherwise = Right (PositionedEnvironmentRoot plan checked position)
  where
    identifier = checkedPublicationId checked

positionedEnvironmentRootPlan ::
  PositionedEnvironmentRoot -> EnvironmentRootPlan
positionedEnvironmentRootPlan (PositionedEnvironmentRoot plan _ _) = plan

positionedEnvironmentRootChecked ::
  PositionedEnvironmentRoot -> CheckedPublication
positionedEnvironmentRootChecked (PositionedEnvironmentRoot _ checked _) = checked

positionedEnvironmentRootPublicationId ::
  PositionedEnvironmentRoot -> PublicationId
positionedEnvironmentRootPublicationId =
  checkedPublicationId . positionedEnvironmentRootChecked

positionedEnvironmentRootHeraldPosition ::
  PositionedEnvironmentRoot -> HeraldPublicationPosition
positionedEnvironmentRootHeraldPosition (PositionedEnvironmentRoot _ _ position) = position

-- | Retry-stable identities and the positioned construction prefix. The twelve
-- endpoints are positioned first; the nineteen wiring descriptions are appended
-- after a cut establishes the new environment's own source writers.
data EnvironmentManifest
  = EnvironmentManifest
      EnvironmentManifestKey
      HeraldMembershipGenerationId
      MemberSetDigest
      (NonEmpty GlobalUniqueId)
      (NonEmpty PositionedEnvironmentRoot)
  deriving stock (Eq, Show)

environmentManifest ::
  EnvironmentManifestKey ->
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  NonEmpty GlobalUniqueId ->
  NonEmpty PositionedEnvironmentRoot ->
  Either EnvironmentManifestProblem EnvironmentManifest
environmentManifest key membership members generated roots = do
  requireCanonicalShape roots
  if NonEmpty.length generated == fromIntegral environmentConnectedObjectCount
    && Set.size (Set.fromList (NonEmpty.toList generated)) == NonEmpty.length generated
    && fmap (environmentRootPlanGeneratedId . positionedEnvironmentRootPlan) (NonEmpty.toList roots)
      == take (NonEmpty.length roots) (NonEmpty.toList generated)
    then Right ()
    else Left EnvironmentManifestShapeMismatch
  requireDistinctGenerated roots
  requireDistinctPublications roots
  mapM_
    requireConsecutivePositionList
    [take (fromIntegral environmentManifestRootCount) (NonEmpty.toList roots), drop (fromIntegral environmentManifestRootCount) (NonEmpty.toList roots)]
  if and (zipWith (<) positions (drop 1 positions))
    then Right ()
    else Left EnvironmentManifestShapeMismatch
  requireCarrierSequences NablaCarrier roots
  requireCarrierSequences DeltaCarrier roots
  if NonEmpty.length roots == fromIntegral environmentConnectedObjectCount
    then requireCarrierSequences NeutralVertexCarrier roots >> requireCarrierSequences EdgeCarrier roots
    else Right ()
  Right (EnvironmentManifest key membership members generated roots)
  where
    positions = fmap positionedEnvironmentRootHeraldPosition (NonEmpty.toList roots)

environmentManifestKeyOf :: EnvironmentManifest -> EnvironmentManifestKey
environmentManifestKeyOf (EnvironmentManifest key _ _ _ _) = key

environmentManifestProcess :: EnvironmentManifest -> ProcessEpochId
environmentManifestProcess = environmentManifestKeyProcess . environmentManifestKeyOf

environmentManifestPosition :: EnvironmentManifest -> ProcessAcceptancePosition
environmentManifestPosition = environmentManifestKeyPosition . environmentManifestKeyOf

environmentManifestMembershipGenerationId :: EnvironmentManifest -> HeraldMembershipGenerationId
environmentManifestMembershipGenerationId (EnvironmentManifest _ membership _ _ _) = membership

environmentManifestActiveMemberSetDigest :: EnvironmentManifest -> MemberSetDigest
environmentManifestActiveMemberSetDigest (EnvironmentManifest _ _ members _ _) = members

environmentManifestGeneratedIds :: EnvironmentManifest -> NonEmpty GlobalUniqueId
environmentManifestGeneratedIds (EnvironmentManifest _ _ _ generated _) = generated

environmentManifestRoots :: EnvironmentManifest -> NonEmpty PositionedEnvironmentRoot
environmentManifestRoots (EnvironmentManifest _ _ _ _ roots) = roots

environmentManifestComplete :: EnvironmentManifest -> Bool
environmentManifestComplete = (== fromIntegral environmentConnectedObjectCount) . NonEmpty.length . environmentManifestRoots

-- | The only legal evolution preserves the accepted identities and every
-- positioned publication. Frozen structural stages may retain either prefix.
environmentManifestExtends :: EnvironmentManifest -> EnvironmentManifest -> Bool
environmentManifestExtends newer older =
  environmentManifestKeyOf newer == environmentManifestKeyOf older
    && environmentManifestMembershipGenerationId newer == environmentManifestMembershipGenerationId older
    && environmentManifestActiveMemberSetDigest newer == environmentManifestActiveMemberSetDigest older
    && environmentManifestGeneratedIds newer == environmentManifestGeneratedIds older
    && take (NonEmpty.length (environmentManifestRoots older)) (NonEmpty.toList (environmentManifestRoots newer))
      == NonEmpty.toList (environmentManifestRoots older)

data EnvironmentReplyKey
  = EnvironmentReplyKey ApplicationSessionId RequestId
  deriving stock (Eq, Ord, Show)

environmentReplyKey ::
  ApplicationSessionId -> RequestId -> EnvironmentReplyKey
environmentReplyKey = EnvironmentReplyKey

environmentReplyKeySession :: EnvironmentReplyKey -> ApplicationSessionId
environmentReplyKeySession (EnvironmentReplyKey session _) = session

environmentReplyKeyRequest :: EnvironmentReplyKey -> RequestId
environmentReplyKeyRequest (EnvironmentReplyKey _ request) = request

-- | Accepted work outlives its reply owner. Session retirement erases only the
-- optional reachability key; construction may append the checked wiring suffix
-- while preserving all accepted identities and publications.
data PendingEnvironment
  = PendingEnvironment EnvironmentManifest (Maybe EnvironmentReplyKey)
  deriving stock (Eq, Show)

pendingEnvironment ::
  EnvironmentManifest -> Maybe EnvironmentReplyKey -> PendingEnvironment
pendingEnvironment = PendingEnvironment

pendingEnvironmentManifest :: PendingEnvironment -> EnvironmentManifest
pendingEnvironmentManifest (PendingEnvironment manifest _) = manifest

pendingEnvironmentReplyKey ::
  PendingEnvironment -> Maybe EnvironmentReplyKey
pendingEnvironmentReplyKey (PendingEnvironment _ reply) = reply

detachPendingEnvironmentReply :: PendingEnvironment -> PendingEnvironment
detachPendingEnvironmentReply (PendingEnvironment manifest _) =
  PendingEnvironment manifest Nothing

-- | Exact structural witness selected by the settlement coordinator.  The cut
-- coordinate is retained with the terminal Application outcome so later
-- whole-state validation can check that one installed cut directly, instead of
-- searching the complete installed-cut history for some cut that would justify
-- the outcome after the fact.
--
-- Construction is package-private and deliberately performs no structural
-- lookup: the structural coordinator supplies the coordinate derived from the
-- selected installed cut, while the composed Herald invariant rechecks it
-- against the retained Graph and Publication indexes.
data EnvironmentSettlementEvidenceProblem
  = EnvironmentSettlementEvidenceOccurrenceCountMismatch Word64 Word64
  | EnvironmentSettlementEvidenceDuplicateOccurrence StructuralOccurrenceId
  deriving stock (Eq, Show)

data EnvironmentSettlementEvidence
  = EnvironmentSettlementEvidence
      TopologyCutId
      HeraldMembershipGenerationId
      MemberSetDigest
      (NonEmpty StructuralOccurrenceId)
  deriving stock (Eq, Show)

environmentSettlementEvidence ::
  EnvironmentManifest ->
  TopologyCutId ->
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  NonEmpty StructuralOccurrenceId ->
  Either EnvironmentSettlementEvidenceProblem EnvironmentSettlementEvidence
environmentSettlementEvidence manifest cut generation members occurrences
  | actualCount /= expectedCount =
      Left
        ( EnvironmentSettlementEvidenceOccurrenceCountMismatch
            expectedCount
            actualCount
        )
  | otherwise = do
      requireDistinctSettlementOccurrences occurrences
      Right (EnvironmentSettlementEvidence cut generation members occurrences)
  where
    expectedCount =
      fromIntegral (NonEmpty.length (environmentManifestRoots manifest))
    actualCount = fromIntegral (NonEmpty.length occurrences)

requireDistinctSettlementOccurrences ::
  NonEmpty StructuralOccurrenceId ->
  Either EnvironmentSettlementEvidenceProblem ()
requireDistinctSettlementOccurrences = go Set.empty . NonEmpty.toList
  where
    go _ [] = Right ()
    go seen (occurrence : remaining)
      | Set.member occurrence seen =
          Left (EnvironmentSettlementEvidenceDuplicateOccurrence occurrence)
      | otherwise = go (Set.insert occurrence seen) remaining

environmentSettlementEvidenceCut ::
  EnvironmentSettlementEvidence -> TopologyCutId
environmentSettlementEvidenceCut
  (EnvironmentSettlementEvidence cut _ _ _) = cut

environmentSettlementEvidenceMembershipGenerationId ::
  EnvironmentSettlementEvidence -> HeraldMembershipGenerationId
environmentSettlementEvidenceMembershipGenerationId
  (EnvironmentSettlementEvidence _ generation _ _) = generation

environmentSettlementEvidenceMemberSetDigest ::
  EnvironmentSettlementEvidence -> MemberSetDigest
environmentSettlementEvidenceMemberSetDigest
  (EnvironmentSettlementEvidence _ _ members _) = members

environmentSettlementEvidenceOccurrences ::
  EnvironmentSettlementEvidence -> NonEmpty StructuralOccurrenceId
environmentSettlementEvidenceOccurrences
  (EnvironmentSettlementEvidence _ _ _ occurrences) = occurrences

-- | Terminal global outcome of one accepted manifest.  A live process retains
-- the checked aliases even if the accepting session ended.  Settlement after
-- process End has neither aliases nor reply reachability, but still records the
-- published manifest and its exact common-cut witness. A forced End before
-- wiring publication leaves a terminal endpoint prefix; no later phase runs.
data CompletedEnvironment
  = CompletedEnvironment
      EnvironmentManifest
      EnvironmentSettlementEvidence
      (Maybe EnvironmentAccess)
      (Maybe EnvironmentReplyKey)
  deriving stock (Eq, Show)

completePendingEnvironment ::
  EnvironmentSettlementEvidence ->
  Maybe EnvironmentAccess ->
  PendingEnvironment ->
  CompletedEnvironment
completePendingEnvironment evidence access pending =
  CompletedEnvironment manifest evidence access retainedReply
  where
    manifest = pendingEnvironmentManifest pending
    retainedReply = case access of
      Nothing -> Nothing
      Just _ -> pendingEnvironmentReplyKey pending

completedEnvironmentManifest :: CompletedEnvironment -> EnvironmentManifest
completedEnvironmentManifest (CompletedEnvironment manifest _ _ _) = manifest

completedEnvironmentSettlementEvidence ::
  CompletedEnvironment -> EnvironmentSettlementEvidence
completedEnvironmentSettlementEvidence
  (CompletedEnvironment _ evidence _ _) = evidence

completedEnvironmentAccess :: CompletedEnvironment -> Maybe EnvironmentAccess
completedEnvironmentAccess (CompletedEnvironment _ _ access _) = access

completedEnvironmentReplyKey ::
  CompletedEnvironment -> Maybe EnvironmentReplyKey
completedEnvironmentReplyKey (CompletedEnvironment _ _ _ reply) = reply

detachCompletedEnvironmentReply ::
  CompletedEnvironment -> CompletedEnvironment
detachCompletedEnvironmentReply
  (CompletedEnvironment manifest evidence access _) =
    CompletedEnvironment manifest evidence access Nothing

-- | Process retirement removes the aliases and every route to an application
-- result while preserving the globally completed semantic manifest.
retireCompletedEnvironmentAccess ::
  CompletedEnvironment -> CompletedEnvironment
retireCompletedEnvironmentAccess
  (CompletedEnvironment manifest evidence _ _) =
    CompletedEnvironment manifest evidence Nothing Nothing

requireCanonicalShape ::
  NonEmpty PositionedEnvironmentRoot -> Either EnvironmentManifestProblem ()
requireCanonicalShape roots
  | fmap rootSlot roots `elem` [environmentManifestShapeRoots canonicalShape, environmentManifestShapeRoots canonicalShape <> environmentWiringSlots] = Right ()
  | otherwise = Left EnvironmentManifestShapeMismatch
  where
    rootSlot = environmentRootPlanSlot . positionedEnvironmentRootPlan
    canonicalShape :: EnvironmentManifestShape
    canonicalShape = profileEnvironmentManifestShape

requireDistinctGenerated ::
  NonEmpty PositionedEnvironmentRoot -> Either EnvironmentManifestProblem ()
requireDistinctGenerated roots =
  requireDistinct
    (environmentRootPlanGeneratedId . positionedEnvironmentRootPlan)
    EnvironmentManifestDuplicateGeneratedId
    roots

requireDistinctPublications ::
  NonEmpty PositionedEnvironmentRoot -> Either EnvironmentManifestProblem ()
requireDistinctPublications =
  requireDistinct
    positionedEnvironmentRootPublicationId
    EnvironmentManifestDuplicatePublicationId

requireDistinct ::
  (Ord key) =>
  (PositionedEnvironmentRoot -> key) ->
  (key -> EnvironmentManifestProblem) ->
  NonEmpty PositionedEnvironmentRoot ->
  Either EnvironmentManifestProblem ()
requireDistinct project duplicate = go Set.empty . NonEmpty.toList
  where
    go _ [] = Right ()
    go seen (root : remainder)
      | Set.member key seen = Left (duplicate key)
      | otherwise = go (Set.insert key seen) remainder
      where
        key = project root

requireConsecutivePositionList ::
  [PositionedEnvironmentRoot] -> Either EnvironmentManifestProblem ()
requireConsecutivePositionList roots =
  case fmap (heraldPublicationPositionWord64 . positionedEnvironmentRootHeraldPosition) roots of
    [] -> Right ()
    first : rest -> go (first + 1) rest
  where
    go _ [] = Right ()
    go expected (actual : remainder)
      | actual == expected = go (expected + 1) remainder
      | otherwise =
          Left (EnvironmentManifestPositionNotConsecutive expected actual)

requireCarrierSequences ::
  StructuralCarrierRole ->
  NonEmpty PositionedEnvironmentRoot ->
  Either EnvironmentManifestProblem ()
requireCarrierSequences carrier roots =
  case filter ((== carrier) . rootCarrier) (NonEmpty.toList roots) of
    [] -> Left EnvironmentManifestShapeMismatch
    first : rest
      | rootSequence first == 0 ->
          Left (EnvironmentManifestCarrierSequenceNotPositive carrier)
      | otherwise -> go first rest
  where
    rootCarrier =
      environmentRootSlotStructuralCarrierRole
        . environmentRootPlanSlot
        . positionedEnvironmentRootPlan
    rootSource root =
      let identifier = positionedEnvironmentRootPublicationId root
       in (publicationNabla identifier, publicationAuthorityEpoch identifier)
    rootSequence =
      nablaSequenceWord64
        . publicationNablaSequence
        . positionedEnvironmentRootPublicationId
    go _ [] = Right ()
    go previous (current : remainder)
      | rootSource current /= rootSource previous =
          Left (EnvironmentManifestCarrierSourceMismatch carrier)
      | actual /= expected =
          Left
            ( EnvironmentManifestCarrierSequenceNotConsecutive
                carrier
                expected
                actual
            )
      | otherwise = go current remainder
      where
        expected = rootSequence previous + 1
        actual = rootSequence current
