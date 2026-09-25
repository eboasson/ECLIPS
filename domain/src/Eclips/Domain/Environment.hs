{-# LANGUAGE OverloadedStrings #-}

-- | Closed semantic shape of one private profile-0.1 environment.
--
-- Applications never submit this shape.  The Herald uses the single checked
-- endpoint prefix and wiring suffix when it builds a connected environment.  This module deliberately owns no generated identity, process
-- position, publication, route, or wire representation.
module Eclips.Domain.Environment
  ( EnvironmentRootClaim,
    EnvironmentRootClaimView (..),
    environmentWriterRootClaim,
    environmentReaderRootClaim,
    EnvironmentEdgeDirection (..),
    environmentHubRootClaim,
    environmentEdgeRootClaim,
    environmentRootClaimView,
    EnvironmentRootSlot,
    environmentRootSlotOrdinal,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    EnvironmentManifestShape,
    EnvironmentManifestShapeProblem (..),
    checkEnvironmentManifestShape,
    profileEnvironmentManifestShape,
    environmentManifestShapeRoots,
    environmentManifestRootCount,
    environmentManifestShapeCanonicalBytes,
    environmentConnectedObjectCount,
    environmentHubOrdinal,
    environmentEdgeOrdinals,
    environmentWiringSlots,
    environmentConnectedShapeCanonicalBytes,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize.Put qualified as Serialize
import Data.Word (Word8)
import Eclips.Domain.Identity (NablaSequencing (..))
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole,
    allPredefinedSortRoles,
    predefinedSortRoleTag,
  )

-- | A symbolic manifest entry before the closed profile shape is checked.
--
-- The writer claim retains its sequencing declaration so the checker proves
-- that every environment writer starts unsequenced.  A reader has no such
-- declaration.
data EnvironmentRootClaim
  = EnvironmentWriterRootClaim PredefinedSortRole NablaSequencing
  | EnvironmentReaderRootClaim PredefinedSortRole
  | EnvironmentHubRootClaim
  | EnvironmentEdgeRootClaim PredefinedSortRole EnvironmentEdgeDirection
  deriving stock (Eq, Ord, Show)

data EnvironmentRootClaimView
  = EnvironmentWriterRootClaimView PredefinedSortRole NablaSequencing
  | EnvironmentReaderRootClaimView PredefinedSortRole
  | EnvironmentHubRootClaimView
  | EnvironmentEdgeRootClaimView PredefinedSortRole EnvironmentEdgeDirection
  deriving stock (Eq, Ord, Show)

environmentWriterRootClaim ::
  PredefinedSortRole -> NablaSequencing -> EnvironmentRootClaim
environmentWriterRootClaim = EnvironmentWriterRootClaim

environmentReaderRootClaim :: PredefinedSortRole -> EnvironmentRootClaim
environmentReaderRootClaim = EnvironmentReaderRootClaim

-- | Canonical preserving spokes for each predefined endpoint pair.
data EnvironmentEdgeDirection = WriterToHub | HubToReader | ReaderToHub
  deriving stock (Eq, Ord, Show, Enum, Bounded)

environmentHubRootClaim :: EnvironmentRootClaim
environmentHubRootClaim = EnvironmentHubRootClaim

environmentEdgeRootClaim :: PredefinedSortRole -> EnvironmentEdgeDirection -> EnvironmentRootClaim
environmentEdgeRootClaim = EnvironmentEdgeRootClaim

environmentRootClaimView ::
  EnvironmentRootClaim -> EnvironmentRootClaimView
environmentRootClaimView claim = case claim of
  EnvironmentWriterRootClaim role sequencing ->
    EnvironmentWriterRootClaimView role sequencing
  EnvironmentReaderRootClaim role -> EnvironmentReaderRootClaimView role
  EnvironmentHubRootClaim -> EnvironmentHubRootClaimView
  EnvironmentEdgeRootClaim role direction -> EnvironmentEdgeRootClaimView role direction

-- | One checked root with its exact zero-based manifest position.
data EnvironmentRootSlot
  = EnvironmentRootSlot Word8 EnvironmentRootClaim
  deriving stock (Eq, Ord, Show)

environmentRootSlotOrdinal :: EnvironmentRootSlot -> Word8
environmentRootSlotOrdinal (EnvironmentRootSlot ordinal _) = ordinal

environmentRootSlotClaim :: EnvironmentRootSlot -> EnvironmentRootClaim
environmentRootSlotClaim (EnvironmentRootSlot _ claim) = claim

-- | Structural carrier used to define the target root.  The target's data
-- role remains the role in its claim; it is never confused with this source
-- carrier role.
environmentRootSlotStructuralCarrierRole ::
  EnvironmentRootSlot -> StructuralCarrierRole
environmentRootSlotStructuralCarrierRole root =
  case environmentRootClaimView (environmentRootSlotClaim root) of
    EnvironmentWriterRootClaimView {} -> NablaCarrier
    EnvironmentReaderRootClaimView {} -> DeltaCarrier
    EnvironmentHubRootClaimView -> NeutralVertexCarrier
    EnvironmentEdgeRootClaimView {} -> EdgeCarrier

-- | The one complete, ordered profile manifest shape.
newtype EnvironmentManifestShape
  = EnvironmentManifestShape (NonEmpty EnvironmentRootSlot)
  deriving stock (Eq, Show)

data EnvironmentManifestShapeProblem
  = EnvironmentManifestWrongRootCount Int Int
  | EnvironmentManifestUnexpectedRoot
      Word8
      EnvironmentRootClaim
      EnvironmentRootClaim
  deriving stock (Eq, Show)

-- | Check exact cardinality, canonical role order, writer-before-reader order,
-- and the required unsequenced writer declaration in one admission step.
checkEnvironmentManifestShape ::
  [EnvironmentRootClaim] ->
  Either EnvironmentManifestShapeProblem EnvironmentManifestShape
checkEnvironmentManifestShape supplied
  | length supplied /= expectedCount =
      Left
        (EnvironmentManifestWrongRootCount expectedCount (length supplied))
  | otherwise = do
      checked <- traverse checkAt (zip3 [0 ..] canonicalClaims supplied)
      case NonEmpty.nonEmpty checked of
        Nothing -> error "profile environment manifest unexpectedly became empty"
        Just roots -> Right (EnvironmentManifestShape roots)
  where
    expectedCount = length canonicalClaims
    checkAt (ordinal, expected, actual)
      | expected == actual = Right (EnvironmentRootSlot ordinal actual)
      | otherwise =
          Left
            (EnvironmentManifestUnexpectedRoot ordinal expected actual)

profileEnvironmentManifestShape :: EnvironmentManifestShape
profileEnvironmentManifestShape =
  either
    (error . ("invalid closed profile environment manifest: " <>) . show)
    id
    (checkEnvironmentManifestShape canonicalClaims)

environmentManifestShapeRoots ::
  EnvironmentManifestShape -> NonEmpty EnvironmentRootSlot
environmentManifestShapeRoots (EnvironmentManifestShape roots) = roots

environmentManifestRootCount :: Word8
environmentManifestRootCount = fromIntegral (length canonicalClaims)

-- | Wire-independent canonical bytes used only by semantic digests and tests.
--
-- The manifest domain is followed by the exact count and then
-- @(ordinal, role, writer-or-reader)@ triples.  Writer is tag 0 and reader tag
-- 1; the checked shape makes the writer's unsequenced fact implicit and unique.
environmentManifestShapeCanonicalBytes ::
  EnvironmentManifestShape -> ByteString
environmentManifestShapeCanonicalBytes manifest =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-ENVIRONMENT-MANIFEST-SHAPE"
    Serialize.putWord8 environmentManifestRootCount
    traverse_ putRoot (environmentManifestShapeRoots manifest)
  where
    putRoot root = do
      Serialize.putWord8 (environmentRootSlotOrdinal root)
      case environmentRootClaimView (environmentRootSlotClaim root) of
        EnvironmentWriterRootClaimView role UnsequencedNabla -> do
          Serialize.putWord8 (predefinedSortRoleTag role)
          Serialize.putWord8 0
        EnvironmentWriterRootClaimView _ NablaSequencedBy {} ->
          error "checked environment manifest retained a sequenced writer"
        EnvironmentReaderRootClaimView role -> do
          Serialize.putWord8 (predefinedSortRoleTag role)
          Serialize.putWord8 1
        EnvironmentHubRootClaimView -> Serialize.putWord8 2
        EnvironmentEdgeRootClaimView role direction -> do
          Serialize.putWord8 3
          Serialize.putWord8 (predefinedSortRoleTag role)
          Serialize.putWord8 (fromIntegral (fromEnum direction))

-- | The twelve endpoint definitions are the first construction phase. Their
-- installed cut establishes the new writers used for these nineteen objects.
environmentHubOrdinal :: Word8
environmentHubOrdinal = environmentManifestRootCount

environmentConnectedObjectCount :: Word8
environmentConnectedObjectCount = environmentManifestRootCount + 1 + fromIntegral (length environmentEdgeOrdinals)

environmentEdgeOrdinals :: [(PredefinedSortRole, EnvironmentEdgeDirection, Word8)]
environmentEdgeOrdinals =
  [ (role, direction, ordinal)
  | (ordinal, (role, direction)) <-
      zip
        [environmentHubOrdinal + 1 ..]
        [(role, direction) | role <- allPredefinedSortRoles, direction <- [minBound .. maxBound]]
  ]

environmentWiringSlots :: NonEmpty EnvironmentRootSlot
environmentWiringSlots =
  EnvironmentRootSlot environmentHubOrdinal EnvironmentHubRootClaim
    :| [EnvironmentRootSlot ordinal (EnvironmentEdgeRootClaim role direction) | (role, direction, ordinal) <- environmentEdgeOrdinals]

-- | Shared by dynamic construction and checked genesis. This describes the
-- complete application graph, independently of either source-authority path.
environmentConnectedShapeCanonicalBytes :: ByteString
environmentConnectedShapeCanonicalBytes = Serialize.runPut $ do
  Serialize.putByteString "ECLIPS-CONNECTED-ENVIRONMENT"
  Serialize.putByteString (environmentManifestShapeCanonicalBytes profileEnvironmentManifestShape)
  Serialize.putWord8 environmentHubOrdinal
  traverse_
    ( \(role, direction, ordinal) -> do
        Serialize.putWord8 ordinal
        Serialize.putWord8 (predefinedSortRoleTag role)
        Serialize.putWord8 (fromIntegral (fromEnum direction))
    )
    environmentEdgeOrdinals

canonicalClaims :: [EnvironmentRootClaim]
canonicalClaims = concatMap rootsForRole allPredefinedSortRoles
  where
    rootsForRole role =
      [ environmentWriterRootClaim role UnsequencedNabla,
        environmentReaderRootClaim role
      ]
