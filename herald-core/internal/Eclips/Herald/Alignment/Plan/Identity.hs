-- | Exact routing-plan coordinates shared by checked planning and protocol data.
-- A coordinate identifies a plan; its complete table is checked by its owner.
module Eclips.Herald.Alignment.Plan.Identity
  ( AlignmentPlanId,
    alignmentPlanIdFromClaimedCoordinates,
    alignmentPlanIdFromClaimedCoordinatesAtAttempt,
    alignmentPlanIdAtTopology,
    alignmentPlanIdAtTopologyAtAttempt,
    alignmentPlanIdAttempt,
    alignmentPlanIdSort,
    alignmentPlanIdTopology,
    alignmentPlanIdPlacement,
    alignmentPlanIdCanonicalBytes,
    AlignmentBindingDisposition (..),
    AlignmentPredecessorStatus (..),
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize.Put qualified as Serialize
import Eclips.Domain.Alignment
import Eclips.Domain.Identity
import Eclips.Domain.Membership (heraldMembershipGenerationIdBytes)
import Eclips.Domain.Topology
import Eclips.Herald.Structural.Debt

-- No record labels or exported constructor: checked identities cannot be updated.
data AlignmentPlanId = AlignmentPlanId !SortOccurrence !TopologyCutId !PhysicalPlacementRevisionVector !AlignmentPlanAttempt
  deriving stock (Eq, Ord, Show)

-- | Boundary coordinate from individually checked components. The owner resolves
-- its topology identifier and verifies the complete plan before admitting it.
alignmentPlanIdFromClaimedCoordinates :: SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> AlignmentPlanId
alignmentPlanIdFromClaimedCoordinates = alignmentPlanIdFromClaimedCoordinatesAtAttempt initialAlignmentPlanAttempt

alignmentPlanIdFromClaimedCoordinatesAtAttempt :: AlignmentPlanAttempt -> SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> AlignmentPlanId
alignmentPlanIdFromClaimedCoordinatesAtAttempt attempt occurrence topology placement = AlignmentPlanId occurrence topology placement attempt

alignmentPlanIdAtTopology :: SortOccurrence -> TopologyCut -> PhysicalPlacementRevisionVector -> Either AlignmentShapeError AlignmentPlanId
alignmentPlanIdAtTopology = alignmentPlanIdAtTopologyAtAttempt initialAlignmentPlanAttempt

alignmentPlanIdAtTopologyAtAttempt :: AlignmentPlanAttempt -> SortOccurrence -> TopologyCut -> PhysicalPlacementRevisionVector -> Either AlignmentShapeError AlignmentPlanId
alignmentPlanIdAtTopologyAtAttempt attempt occurrence topology placement
  | topologyGeneration /= placementGeneration = Left (AlignmentTopologyPlacementGenerationMismatch topologyGeneration placementGeneration)
  | topologyMembers /= placementMembers = Left (AlignmentTopologyPlacementMemberSetMismatch topologyMembers placementMembers)
  | otherwise = Right (AlignmentPlanId occurrence (deriveTopologyCutId topology) placement attempt)
  where
    frontier = topologyCutFrontier topology
    topologyGeneration = topologyFrontierMembershipGenerationId frontier
    placementGeneration = physicalPlacementRevisionMembershipGenerationId placement
    topologyMembers = topologyFrontierMemberSetDigest frontier
    placementMembers = physicalPlacementRevisionMemberSetDigest placement

alignmentPlanIdAttempt :: AlignmentPlanId -> AlignmentPlanAttempt
alignmentPlanIdAttempt (AlignmentPlanId _ _ _ attempt) = attempt

alignmentPlanIdSort :: AlignmentPlanId -> SortOccurrence
alignmentPlanIdSort (AlignmentPlanId occurrence _ _ _) = occurrence

alignmentPlanIdTopology :: AlignmentPlanId -> TopologyCutId
alignmentPlanIdTopology (AlignmentPlanId _ topology _ _) = topology

alignmentPlanIdPlacement :: AlignmentPlanId -> PhysicalPlacementRevisionVector
alignmentPlanIdPlacement (AlignmentPlanId _ _ placement _) = placement

-- | Fixed-width identity components followed by the complete ascending vector.
alignmentPlanIdCanonicalBytes :: AlignmentPlanId -> ByteString
alignmentPlanIdCanonicalBytes (AlignmentPlanId occurrence topology placement attempt) = Serialize.runPut $ do
  Serialize.putWord64be (controlIndexWord64 (alignmentPlanAttemptMembershipChange attempt))
  Serialize.putWord64be (alignmentPlanAttemptWord64 attempt)
  Serialize.putByteString (sortIdBytes (sortOccurrenceSortId occurrence))
  Serialize.putByteString (sortDefinitionOccurrenceIdBytes (sortOccurrenceDefinition occurrence))
  Serialize.putByteString (topologyCutIdBytes topology)
  Serialize.putByteString (heraldMembershipGenerationIdBytes (physicalPlacementRevisionMembershipGenerationId placement))
  Serialize.putByteString (memberSetDigestBytes (physicalPlacementRevisionMemberSetDigest placement))
  let entries = NonEmpty.toList (physicalPlacementRevisionEntries placement)
  Serialize.putWord64be (fromIntegral (length entries))
  mapM_ (\(herald, revision) -> Serialize.putByteString (heraldEpochBytes herald) >> Serialize.putWord64be (placementRevisionWord64 revision)) entries

data AlignmentBindingDisposition = AlignmentCreated | AlignmentCarried
  deriving stock (Eq, Ord, Show)

-- | Invalidated predecessors remain frontier evidence but cannot supply carry
-- or live ancestry; recovery starts from the checked fresh placement bases.
data AlignmentPredecessorStatus = AlignmentPredecessorUsable | AlignmentPredecessorInvalidated | AlignmentPredecessorReset
  deriving stock (Eq, Ord, Show)
