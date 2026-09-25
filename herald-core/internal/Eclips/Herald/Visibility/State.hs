{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Live package-private owner for visibility holds.
--
-- 'Eclips.Herald.Startup.State.HeraldState' retains this leaf alongside the
-- semantic work owners. The state records only a relation from a stable key
-- owned by another semantic leaf to the exact facts that currently prevent
-- that work from applying. It never retains the work's payload, settlement
-- state, or a copy of the owning leaf.
module Eclips.Herald.Visibility.State
  ( State,
    initialState,
    VisibilityDependency (..),
    HoldClassification (..),
    classifyOwnerHold,
    heldOwnerKeys,
    visibilityDependenciesFor,
    visibilityDependencyRelations,
    VisibilityProblem (..),
    VisibilityInvariantViolation (..),
    PreparedBatchInstall,
    prepareBatchInstall,
    preparedBatchNewlyHeldOwnerKeys,
    commitBatchInstall,
    PreparedBatchRelease,
    prepareBatchRelease,
    preparedBatchReleasedOwnerKeys,
    commitBatchRelease,
    validateVisibilityState,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    GlobalObjectId,
    LabelDecisionId,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )

-- | One exact semantic fact that prevents retained work from applying.
--
-- An authority dependency names both the controlled subject and the authority
-- epoch required by the retained work.  A target dependency names both the
-- preparing decision and its target, so releasing one workflow cannot release
-- an unrelated hold on the same object.  Fence and target dependencies are
-- deliberately distinct: post-fence work and target-affecting work are
-- reconsidered at different composition points.
data VisibilityDependency
  = LabelFenceDependency LabelDecisionId
  | AuthorityDependency GlobalObjectId AuthorityEpoch
  | TargetDependency LabelDecisionId GlobalObjectId
  deriving stock (Eq, Ord, Show)

-- | Classification against the current authoritative set of retained owner
-- keys.  A missing owner is an invariant fault, not another kind of hold.
data HoldClassification
  = OwnerUnheld
  | OwnerHeld (NonEmpty VisibilityDependency)
  | OwnerMissing
  deriving stock (Eq, Show)

-- | Only non-empty dependency sets are retained.  Consequently a key occurs
-- in this map exactly when its complete work must remain in its semantic owner.
newtype State ownerKey
  = State (Map ownerKey (Set VisibilityDependency))
  deriving stock (Eq, Show)

initialState :: State ownerKey
initialState = State Map.empty

heldOwnerKeys :: State ownerKey -> Set ownerKey
heldOwnerKeys (State relations) = Map.keysSet relations

visibilityDependenciesFor ::
  (Ord ownerKey) =>
  ownerKey ->
  State ownerKey ->
  Set VisibilityDependency
visibilityDependenciesFor ownerKey (State relations) =
  Map.findWithDefault Set.empty ownerKey relations

-- | Canonical dependency relation exposed for whole-state witnesses and
-- deterministic tests.  The payload behind each owner key remains private to
-- its semantic owner.
visibilityDependencyRelations ::
  State ownerKey ->
  Map ownerKey (NonEmpty VisibilityDependency)
visibilityDependencyRelations (State relations) =
  Map.mapMaybe (NonEmpty.nonEmpty . Set.toAscList) relations

classifyOwnerHold ::
  (Ord ownerKey) =>
  Set ownerKey ->
  ownerKey ->
  State ownerKey ->
  HoldClassification
classifyOwnerHold retainedOwnerKeys ownerKey state
  | ownerKey `Set.notMember` retainedOwnerKeys = OwnerMissing
  | otherwise =
      case NonEmpty.nonEmpty . Set.toAscList
        $ visibilityDependenciesFor ownerKey state of
        Nothing -> OwnerUnheld
        Just dependencies -> OwnerHeld dependencies

data VisibilityInvariantViolation ownerKey
  = VisibilityEmptyDependencyRows (Set ownerKey)
  | VisibilityHoldsMissingOwners (Set ownerKey)
  deriving stock (Eq, Show)

data VisibilityProblem ownerKey
  = VisibilityInvariantProblem (VisibilityInvariantViolation ownerKey)
  deriving stock (Eq, Show)

data BatchInstallOutput ownerKey = BatchInstallOutput
  { newlyHeldOwnerKeys :: Set ownerKey
  }

newtype PreparedBatchInstall ownerKey
  = PreparedBatchInstall
      (Prepared (State ownerKey) (BatchInstallOutput ownerKey))

-- | Atomically add a canonical batch of dependency relations after every
-- named item has been retained by its semantic owner.
--
-- Reinstalling an equal relation is idempotent.  Adding another dependency to
-- an already-held owner extends its blockers without reporting that owner as
-- newly held a second time.
prepareBatchInstall ::
  (Ord ownerKey) =>
  Map ownerKey (NonEmpty VisibilityDependency) ->
  Set ownerKey ->
  State ownerKey ->
  Either (VisibilityProblem ownerKey) (PreparedBatchInstall ownerKey)
prepareBatchInstall requestedRelations retainedOwnerKeys state =
  PreparedBatchInstall <$> prepareTransition install state
  where
    normalized =
      fmap (Set.fromList . NonEmpty.toList) requestedRelations

    install predecessor = do
      ensureValid retainedOwnerKeys predecessor
      let State previousRelations = predecessor
          successorRelations =
            Map.unionWith Set.union previousRelations normalized
          successor = State successorRelations
          newlyHeld =
            Map.keysSet normalized
              `Set.difference` Map.keysSet previousRelations
      ensureValid retainedOwnerKeys successor
      Right
        ( successor,
          BatchInstallOutput {newlyHeldOwnerKeys = newlyHeld}
        )

preparedBatchNewlyHeldOwnerKeys ::
  PreparedBatchInstall ownerKey ->
  Set ownerKey
preparedBatchNewlyHeldOwnerKeys (PreparedBatchInstall prepared) =
  (preparedOutput prepared).newlyHeldOwnerKeys

commitBatchInstall ::
  PreparedBatchInstall ownerKey ->
  (State ownerKey, Set ownerKey)
commitBatchInstall (PreparedBatchInstall prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.newlyHeldOwnerKeys)

data BatchReleaseOutput ownerKey = BatchReleaseOutput
  { releasedOwnerKeys :: Set ownerKey
  }

newtype PreparedBatchRelease ownerKey
  = PreparedBatchRelease
      (Prepared (State ownerKey) (BatchReleaseOutput ownerKey))

-- | Atomically remove the supplied exact dependency keys from every relation.
--
-- An owner becomes release-eligible only when its final dependency disappears;
-- unrelated dependencies and unrelated owners remain byte-for-byte unchanged.
-- Releasing an already absent dependency is an idempotent no-op.
prepareBatchRelease ::
  (Ord ownerKey) =>
  Set VisibilityDependency ->
  Set ownerKey ->
  State ownerKey ->
  Either (VisibilityProblem ownerKey) (PreparedBatchRelease ownerKey)
prepareBatchRelease releasedDependencies retainedOwnerKeys state =
  PreparedBatchRelease <$> prepareTransition release state
  where
    release predecessor = do
      ensureValid retainedOwnerKeys predecessor
      let State previousRelations = predecessor
          successorRelations =
            Map.mapMaybe
              retainNonEmpty
              (fmap (`Set.difference` releasedDependencies) previousRelations)
          successor = State successorRelations
          newlyReleased =
            Map.keysSet previousRelations
              `Set.difference` Map.keysSet successorRelations
      ensureValid retainedOwnerKeys successor
      Right
        ( successor,
          BatchReleaseOutput {releasedOwnerKeys = newlyReleased}
        )

    retainNonEmpty dependencies
      | Set.null dependencies = Nothing
      | otherwise = Just dependencies

preparedBatchReleasedOwnerKeys ::
  PreparedBatchRelease ownerKey ->
  Set ownerKey
preparedBatchReleasedOwnerKeys (PreparedBatchRelease prepared) =
  (preparedOutput prepared).releasedOwnerKeys

commitBatchRelease ::
  PreparedBatchRelease ownerKey ->
  (State ownerKey, Set ownerKey)
commitBatchRelease (PreparedBatchRelease prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.releasedOwnerKeys)

-- | Check the visibility relation against the stable keys currently retained
-- by all semantic work owners participating in the composed transition.
--
-- Since both sides are sets and the relation is keyed by owner identity, every
-- admitted row resolves exactly once.  Removing or applying work while its row
-- remains is therefore detected as a missing owner rather than silently making
-- the held work eligible.
validateVisibilityState ::
  (Ord ownerKey) =>
  Set ownerKey ->
  State ownerKey ->
  Either (VisibilityInvariantViolation ownerKey) ()
validateVisibilityState retainedOwnerKeys (State relations)
  | not (Set.null emptyRows) =
      Left (VisibilityEmptyDependencyRows emptyRows)
  | not (Set.null missingOwners) =
      Left (VisibilityHoldsMissingOwners missingOwners)
  | otherwise = Right ()
  where
    emptyRows = Map.keysSet (Map.filter Set.null relations)
    missingOwners =
      Map.keysSet relations
        `Set.difference` retainedOwnerKeys

ensureValid ::
  (Ord ownerKey) =>
  Set ownerKey ->
  State ownerKey ->
  Either (VisibilityProblem ownerKey) ()
ensureValid retainedOwnerKeys state =
  case validateVisibilityState retainedOwnerKeys state of
    Left violation -> Left (VisibilityInvariantProblem violation)
    Right () -> Right ()
