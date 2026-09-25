-- | Checked low-level structural progress vocabulary.
--
-- A vector is always qualified by one exact checked Herald membership
-- generation at construction. Its opaque normalized representation therefore
-- cannot be sparse, contain a foreign component, forget its historical
-- generation, or retain presentation-order ambiguity. Progress comparison is
-- explicitly componentwise and generation-local; this type deliberately has no
-- 'Ord' instance that could be mistaken for causal coverage.
module Eclips.Domain.Structural
  ( StructuralPrefix,
    emptyStructuralPrefix,
    structuralPrefixThrough,
    structuralPrefixSequence,
    nextAfterStructuralPrefix,
    structuralPredecessorPrefix,
    StructuralVersionVector,
    StructuralVectorProblem (..),
    mkStructuralVersionVector,
    structuralVersionVectorFromClaimedCoordinates,
    emptyStructuralVersionVector,
    structuralVersionVectorMembershipGenerationId,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorEntries,
    structuralVersionVectorComponent,
    structuralVersionVectorCovers,
    projectStructuralVersionVector,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( HeraldEpoch,
    StructuralSequence,
    firstStructuralSequence,
    nextStructuralSequence,
    structuralSequencePredecessor,
  )
import Eclips.Domain.MemberSet
  ( MemberSetDigest,
    deriveMemberSetDigest,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )

-- | One contiguous source prefix with an explicit empty state.
data StructuralPrefix
  = EmptyStructuralPrefix
  | StructuralPrefixThrough StructuralSequence
  deriving stock (Eq, Ord, Show)

emptyStructuralPrefix :: StructuralPrefix
emptyStructuralPrefix = EmptyStructuralPrefix

structuralPrefixThrough :: StructuralSequence -> StructuralPrefix
structuralPrefixThrough = StructuralPrefixThrough

structuralPrefixSequence :: StructuralPrefix -> Maybe StructuralSequence
structuralPrefixSequence EmptyStructuralPrefix = Nothing
structuralPrefixSequence (StructuralPrefixThrough sequenceNumber) =
  Just sequenceNumber

nextAfterStructuralPrefix :: StructuralPrefix -> StructuralSequence
nextAfterStructuralPrefix EmptyStructuralPrefix = firstStructuralSequence
nextAfterStructuralPrefix (StructuralPrefixThrough sequenceNumber) =
  nextStructuralSequence sequenceNumber

-- | The exact source component immediately before allocating the supplied dot.
structuralPredecessorPrefix :: StructuralSequence -> StructuralPrefix
structuralPredecessorPrefix =
  maybe EmptyStructuralPrefix StructuralPrefixThrough
    . structuralSequencePredecessor

-- | A normalized exact map over one checked membership generation.
--
-- Both coordinates are retained deliberately. The generation identity preserves
-- lineage while the member-set digest names the exact component shape committed
-- by that generation.
data StructuralVersionVector
  = StructuralVersionVector
      HeraldMembershipGenerationId
      MemberSetDigest
      (Map HeraldEpoch StructuralPrefix)
  deriving stock (Eq, Show)

data StructuralVectorProblem
  = StructuralVectorDuplicateComponent HeraldEpoch
  | StructuralVectorMissingComponent HeraldEpoch
  | StructuralVectorForeignComponent HeraldEpoch
  | StructuralVectorClaimedMemberSetDigestMismatch
      MemberSetDigest
      MemberSetDigest
  | StructuralVectorOriginGenerationMismatch HeraldMembershipGenerationId HeraldMembershipGenerationId
  | StructuralVectorOriginMemberSetMismatch MemberSetDigest MemberSetDigest
  deriving stock (Eq, Show)

-- | Project exactly along admitted membership ancestry. Surviving source
-- prefixes keep their original sequence numbers and newly admitted sources
-- start empty. This is projection only;
-- settlement still requires the owner's terminal evidence for removed sources.
projectStructuralVersionVector :: HeraldMembershipLineage -> StructuralVersionVector -> Either StructuralVectorProblem StructuralVersionVector
projectStructuralVersionVector lineage vector = do
  let origin = heraldMembershipLineageOrigin lineage
      target = heraldMembershipLineageTarget lineage
      expectedGeneration = heraldMembershipGenerationId origin
      actualGeneration = structuralVersionVectorMembershipGenerationId vector
      expectedMembers = heraldMembershipGenerationActiveMemberSetDigest origin
      actualMembers = structuralVersionVectorMemberSetDigest vector
  if expectedGeneration == actualGeneration
    then Right ()
    else Left (StructuralVectorOriginGenerationMismatch expectedGeneration actualGeneration)
  if expectedMembers == actualMembers
    then Right ()
    else Left (StructuralVectorOriginMemberSetMismatch expectedMembers actualMembers)
  mkStructuralVersionVector
    target
    [ (herald, maybe emptyStructuralPrefix id (structuralVersionVectorComponent herald vector))
    | herald <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs target)
    ]

-- | Admit one complete vector against the exact checked membership generation.
--
-- Supplied vector entries must be unique. Successful construction normalizes
-- every presentation into ascending Herald-epoch order and derives both
-- membership coordinates from the checked witness rather than caller claims.
mkStructuralVersionVector ::
  HeraldMembershipGeneration ->
  [(HeraldEpoch, StructuralPrefix)] ->
  Either StructuralVectorProblem StructuralVersionVector
mkStructuralVersionVector generation suppliedEntries = do
  supplied <- foldSuppliedEntries suppliedEntries
  let expected =
        Set.fromList
          ( NonEmpty.toList
              (heraldMembershipGenerationActiveHeraldEpochs generation)
          )
  case Set.lookupMin (Map.keysSet supplied `Set.difference` expected) of
    Just foreignHerald ->
      Left (StructuralVectorForeignComponent foreignHerald)
    Nothing -> pure ()
  case Set.lookupMin (expected `Set.difference` Map.keysSet supplied) of
    Just missing -> Left (StructuralVectorMissingComponent missing)
    Nothing -> pure ()
  Right
    ( StructuralVersionVector
        (heraldMembershipGenerationId generation)
        (heraldMembershipGenerationActiveMemberSetDigest generation)
        supplied
    )

-- | Boundary-only construction from already shape-checked wire coordinates.
--
-- This normalizes the component presentation, rejects duplicate Herald entries,
-- and verifies that the claimed member-set digest names exactly those component
-- keys. It deliberately does not claim that the supplied generation ID resolves
-- to that member set. The owning state must resolve the exact retained
-- 'HeraldMembershipGeneration' and perform that admission before using the value
-- semantically. Ordinary owner code should prefer
-- 'mkStructuralVersionVector'.
structuralVersionVectorFromClaimedCoordinates ::
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  NonEmpty.NonEmpty (HeraldEpoch, StructuralPrefix) ->
  Either StructuralVectorProblem StructuralVersionVector
structuralVersionVectorFromClaimedCoordinates generation members supplied =
  do
    entries <- foldSuppliedEntries (NonEmpty.toList supplied)
    let normalized = Map.toAscList entries
        derivedMembers = deriveMemberSetDigest (fmap fst (NonEmpty.fromList normalized))
    if members == derivedMembers
      then Right (StructuralVersionVector generation members entries)
      else
        Left
          ( StructuralVectorClaimedMemberSetDigestMismatch
              derivedMembers
              members
          )

-- | Construct the all-empty vector for one checked membership generation.
emptyStructuralVersionVector ::
  HeraldMembershipGeneration -> StructuralVersionVector
emptyStructuralVersionVector generation =
  StructuralVersionVector
    (heraldMembershipGenerationId generation)
    (heraldMembershipGenerationActiveMemberSetDigest generation)
    ( Map.fromList
        [ (herald, EmptyStructuralPrefix)
        | herald <-
            NonEmpty.toList
              (heraldMembershipGenerationActiveHeraldEpochs generation)
        ]
    )

structuralVersionVectorMembershipGenerationId ::
  StructuralVersionVector -> HeraldMembershipGenerationId
structuralVersionVectorMembershipGenerationId
  (StructuralVersionVector generation _ _) = generation

structuralVersionVectorMemberSetDigest ::
  StructuralVersionVector -> MemberSetDigest
structuralVersionVectorMemberSetDigest
  (StructuralVersionVector _ members _) = members

structuralVersionVectorEntries ::
  StructuralVersionVector -> [(HeraldEpoch, StructuralPrefix)]
structuralVersionVectorEntries (StructuralVersionVector _ _ entries) =
  Map.toAscList entries

structuralVersionVectorComponent ::
  HeraldEpoch -> StructuralVersionVector -> Maybe StructuralPrefix
structuralVersionVectorComponent
  herald
  (StructuralVersionVector _ _ entries) = Map.lookup herald entries

-- | Whether the first vector componentwise covers the second.
--
-- Vectors over unequal generations or member-set digests do not cover one
-- another, even if their component maps happen to be identical.
structuralVersionVectorCovers ::
  StructuralVersionVector -> StructuralVersionVector -> Bool
structuralVersionVectorCovers
  (StructuralVersionVector coveringGeneration coveringMembers covering)
  (StructuralVersionVector requiredGeneration requiredMembers required) =
    coveringGeneration == requiredGeneration
      && coveringMembers == requiredMembers
      && Map.keysSet covering == Map.keysSet required
      && all componentCovered (Map.toAscList required)
    where
      componentCovered (herald, requiredPrefix) =
        maybe
          False
          (>= requiredPrefix)
          (Map.lookup herald covering)

foldSuppliedEntries ::
  [(HeraldEpoch, StructuralPrefix)] ->
  Either StructuralVectorProblem (Map HeraldEpoch StructuralPrefix)
foldSuppliedEntries = foldl insertEntry (Right Map.empty)
  where
    insertEntry accumulated (herald, prefix) = do
      entries <- accumulated
      case Map.lookup herald entries of
        Just _ -> Left (StructuralVectorDuplicateComponent herald)
        Nothing -> Right (Map.insert herald prefix entries)
