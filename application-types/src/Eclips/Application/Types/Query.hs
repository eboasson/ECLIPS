{-# LANGUAGE DeriveAnyClass #-}

-- | Application-facing local query syntax.
--
-- Deltas and identity-bearing literals use process-private names. Textual
-- projections are admitted against each named delta's effective descriptor by
-- the Herald; this package deliberately has no semantic-domain dependency.
module Eclips.Application.Types.Query
  ( ApplicationProjection (..),
    ApplicationScalarComparison (..),
    ApplicationQueryLiteral (..),
    ApplicationQueryPredicate (..),
    ApplicationQuery (..),
  )
where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateUniqueId,
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationProjection (..),
    ApplicationScalarComparison (..),
  )
import Eclips.Application.Types.Value (ApplicationLabel)
import GHC.Generics (Generic)

-- | Scalar query literals.
--
-- Unlike descriptor literals, query literals may name a private unique ID or a
-- private process label. The Herald resolves those names through the caller's
-- process map before checking the predicate against a descriptor. Enum symbols
-- are checked against the selected field's closed schema. A label remains one
-- built-in scalar containing the same owner/generation pair as a sample or CAS.
data ApplicationQueryLiteral
  = QueryBool Bool
  | QueryInt64 Int64
  | QueryBytes ByteString
  | QueryText Text
  | QueryUniqueId PrivateUniqueId
  | QueryLabel ApplicationLabel
  | QueryEnum Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | Closed Boolean predicate grammar for local application queries.
data ApplicationQueryPredicate
  = QueryAlways
  | QueryNever
  | QueryCompare
      ApplicationProjection
      ApplicationScalarComparison
      ApplicationQueryLiteral
  | QueryNot ApplicationQueryPredicate
  | QueryAll (NonEmpty ApplicationQueryPredicate)
  | QueryAny (NonEmpty ApplicationQueryPredicate)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | One predicate evaluated over the visible stores of explicitly named deltas.
--
-- An empty delta set is valid and observes an empty local cut.
data ApplicationQuery = ApplicationQuery
  { applicationQueryDeltas :: Set PrivateDeltaId,
    applicationQueryPredicate :: ApplicationQueryPredicate
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
