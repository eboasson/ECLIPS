{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Identifier-only dependency indexing for the existing ordered coordinator
-- stages. This module owns neither handlers nor a scheduling loop.
module Eclips.Herald.Internal.WorkIndex
  ( WorkIndex,
    empty,
    registeredKeys,
    pendingKeys,
    dependenciesFor,
    registerWork,
    replaceDependencies,
    removeWork,
    markDirty,
    consumeWork,
    notifyDependencies,
    nextWork,
    clearPendingWork,
    valid,
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data WorkIndex key dependency = WorkIndex
  { registered :: !(Set key),
    pending :: !(Set key),
    forward :: !(Map key (Set dependency)),
    inverse :: !(Map dependency (Set key))
  }
  deriving stock (Eq, Show)

empty :: WorkIndex key dependency
empty = WorkIndex Set.empty Set.empty Map.empty Map.empty

registeredKeys :: WorkIndex key dependency -> Set key
registeredKeys index = index.registered

pendingKeys :: WorkIndex key dependency -> Set key
pendingKeys index = index.pending

dependenciesFor :: (Ord key) => key -> WorkIndex key dependency -> Maybe (Set dependency)
dependenciesFor key index = Map.lookup key index.forward

registerWork :: (Ord key, Ord dependency) => key -> Set dependency -> WorkIndex key dependency -> WorkIndex key dependency
registerWork key dependencies index =
  key
    `seq` markDirty
      (Set.singleton key)
      ( replaceRegisteredDependencies
          key
          dependencies
          index
            { registered = Set.insert key index.registered
            }
      )

-- | Replacing the declaration after an evaluation does not manufacture another
-- evaluation. Only the authoritative owner may register a new work identity.
replaceDependencies :: (Ord key, Ord dependency) => key -> Set dependency -> WorkIndex key dependency -> WorkIndex key dependency
replaceDependencies key dependencies index
  | Set.member key index.registered = replaceRegisteredDependencies key dependencies index
  | otherwise = index

replaceRegisteredDependencies :: (Ord key, Ord dependency) => key -> Set dependency -> WorkIndex key dependency -> WorkIndex key dependency
replaceRegisteredDependencies key dependencies index =
  index
    { forward = Map.insert key dependencies index.forward,
      inverse =
        Set.foldl'
          (\retained dependency -> dependency `seq` Map.insertWith Set.union dependency (Set.singleton key) retained)
          (Set.foldl' (flip (removeMembership key)) index.inverse removed)
          added
    }
  where
    previous = Map.findWithDefault Set.empty key index.forward
    removed = previous Set.\\ dependencies
    added = dependencies Set.\\ previous

removeWork :: (Ord key, Ord dependency) => key -> WorkIndex key dependency -> WorkIndex key dependency
removeWork key index =
  index
    { registered = Set.delete key index.registered,
      pending = Set.delete key index.pending,
      forward = Map.delete key index.forward,
      inverse = Set.foldl' (flip (removeMembership key)) index.inverse (Map.findWithDefault Set.empty key index.forward)
    }

removeMembership :: (Ord key, Ord dependency) => key -> dependency -> Map dependency (Set key) -> Map dependency (Set key)
removeMembership key dependency = Map.update remove dependency
  where
    remove keys = let retained = Set.delete key keys in if Set.null retained then Nothing else Just retained

markDirty :: (Ord key) => Set key -> WorkIndex key dependency -> WorkIndex key dependency
markDirty keys index = index {pending = index.pending `Set.union` (keys `Set.intersection` index.registered)}

consumeWork :: (Ord key) => key -> WorkIndex key dependency -> WorkIndex key dependency
consumeWork key index = index {pending = Set.delete key index.pending}

notifyDependencies :: (Ord key, Ord dependency) => Set dependency -> WorkIndex key dependency -> (Set key, WorkIndex key dependency)
notifyDependencies dependencies index =
  let affected = Set.foldl' (\keys dependency -> keys `Set.union` Map.findWithDefault Set.empty dependency index.inverse) Set.empty dependencies
   in affected `seq` (affected, markDirty affected index)

-- | Membership is frozen by the enclosing stage. New keys and keys woken behind
-- the exclusive cursor remain dirty for its next original invocation.
nextWork :: (Ord key) => Set key -> Maybe key -> WorkIndex key dependency -> Maybe key
nextWork inventory cursor index = next cursor
  where
    next lower = do
      key <- maybe (Set.lookupMin index.pending) (`Set.lookupGT` index.pending) lower
      if Set.member key inventory then Just key else next (Just key)

clearPendingWork :: WorkIndex key dependency -> WorkIndex key dependency
clearPendingWork index = index {pending = Set.empty}

valid :: (Ord key, Ord dependency) => WorkIndex key dependency -> Bool
valid index =
  index.registered == Map.keysSet index.forward
    && index.pending `Set.isSubsetOf` index.registered
    && index.inverse
      == Map.fromListWith
        Set.union
        [(dependency, Set.singleton key) | (key, dependencies) <- Map.toAscList index.forward, dependency <- Set.toAscList dependencies]
