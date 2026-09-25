-- | Descriptor-admitted, exact-incarnation local query projections.
--
-- This neutral kernel vocabulary sits below both Store evaluation and Wait
-- retention, avoiding a leaf-to-leaf state dependency.
module Eclips.Herald.Query
  ( ResolvedQueryBranch,
    resolvedQueryBranch,
    resolvedQueryBranchDelta,
    resolvedQueryBranchStoreIncarnation,
    resolvedQueryBranchPredicate,
    ResolvedQuery,
    resolvedQuery,
    resolvedQueryBranches,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (DeltaId, StoreIncarnationId)
import Eclips.Domain.Query (CheckedQueryPredicate)

-- | One descriptor-admitted predicate tied to an exact store incarnation.
data ResolvedQueryBranch
  = ResolvedQueryBranch DeltaId StoreIncarnationId CheckedQueryPredicate
  deriving stock (Eq, Show)

resolvedQueryBranch ::
  DeltaId ->
  StoreIncarnationId ->
  CheckedQueryPredicate ->
  ResolvedQueryBranch
resolvedQueryBranch = ResolvedQueryBranch

resolvedQueryBranchDelta :: ResolvedQueryBranch -> DeltaId
resolvedQueryBranchDelta (ResolvedQueryBranch delta _ _) = delta

resolvedQueryBranchStoreIncarnation ::
  ResolvedQueryBranch -> StoreIncarnationId
resolvedQueryBranchStoreIncarnation (ResolvedQueryBranch _ incarnation _) = incarnation

resolvedQueryBranchPredicate ::
  ResolvedQueryBranch -> CheckedQueryPredicate
resolvedQueryBranchPredicate (ResolvedQueryBranch _ _ predicate) = predicate

-- | A canonical local query. Duplicate deltas collapse at construction.
newtype ResolvedQuery = ResolvedQuery (Map DeltaId ResolvedQueryBranch)
  deriving stock (Eq, Show)

resolvedQuery :: [ResolvedQueryBranch] -> ResolvedQuery
resolvedQuery branches =
  ResolvedQuery
    ( Map.fromList
        [ (resolvedQueryBranchDelta branch, branch)
        | branch <- branches
        ]
    )

resolvedQueryBranches :: ResolvedQuery -> [ResolvedQueryBranch]
resolvedQueryBranches (ResolvedQuery branches) = Map.elems branches
