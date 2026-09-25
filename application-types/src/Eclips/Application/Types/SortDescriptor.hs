{-# LANGUAGE DeriveAnyClass #-}

-- | Application-selectable sort descriptor syntax.
--
-- This is deliberately a draft AST rather than canonical descriptor bytes.  A
-- Herald translates it to semantic-domain syntax for admission. Typed applications
-- and the Herald use the shared canonical encoder to determine the public 'SortId'.  Herald-managed mutation,
-- structural-carrier roles, and controlled-identity literals are absent by type.
module Eclips.Application.Types.SortDescriptor
  ( ApplicationSortKind (..),
    ApplicationValueSchema (..),
    ApplicationProjection (..),
    ApplicationScalarComparison (..),
    ApplicationScalarLiteral (..),
    ApplicationPredicateExpression (..),
    ApplicationRankDirection (..),
    ApplicationRankTerm (..),
    ApplicationSortDescriptor (..),
    ApplicationSortDefinition (..),
  )
where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Access (ApplicationPredefinedSortRole)
import Eclips.Application.Types.Identity (SortId)
import GHC.Generics (Generic)

-- | Whether instances are regular values or controlled objects.
data ApplicationSortKind
  = RegularSort
  | ControlledSort
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | First-order application value shapes.
--
-- An 'EnumSchema' declares a non-empty ordered symbol list. Herald admission
-- rejects duplicates; the declared order defines enum comparisons, keys, and
-- explicit rank fields.
data ApplicationValueSchema
  = BoolSchema
  | Int64Schema
  | BytesSchema
  | TextSchema
  | UniqueIdSchema
  | LabelSchema
  | RecordSchema (Map Text ApplicationValueSchema)
  | EnumSchema (NonEmpty Text)
  | OptionalUniqueIdSchema
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | A non-empty field path shared by descriptor drafts and local queries.
newtype ApplicationProjection
  = ApplicationProjection (NonEmpty Text)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | Scalar comparison shared by descriptor predicates and local queries.
data ApplicationScalarComparison
  = ScalarEqual
  | ScalarNotEqual
  | ScalarLessThan
  | ScalarLessThanOrEqual
  | ScalarGreaterThan
  | ScalarGreaterThanOrEqual
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

-- | Predicate literal syntax. Unique identities and labels are deliberately
-- absent, so a descriptor cannot embed a controlled identity literal. An enum
-- literal must be a member of the projected field's closed enum schema.
data ApplicationScalarLiteral
  = LiteralBool Bool
  | LiteralInt64 Int64
  | LiteralBytes ByteString
  | LiteralText Text
  | LiteralEnum Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | Closed Boolean predicate grammar over scalar field projections.
data ApplicationPredicateExpression
  = AlwaysPredicate
  | NeverPredicate
  | CompareField
      ApplicationProjection
      ApplicationScalarComparison
      ApplicationScalarLiteral
  | NotPredicate ApplicationPredicateExpression
  | AllPredicates (NonEmpty ApplicationPredicateExpression)
  | AnyPredicates (NonEmpty ApplicationPredicateExpression)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | Direction of one lexicographic rank term.
data ApplicationRankDirection
  = Ascending
  | Descending
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

-- | One term in a total lexicographic application rank.
data ApplicationRankTerm
  = RankField ApplicationProjection ApplicationRankDirection
  | RankApplicationValue ApplicationRankDirection
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | All application-selectable facts in a semantic sort descriptor.
--
-- The AST is intended to be assembled directly by application code, so its
-- public fields use concise semantic names. Semantic shape and field-name checks
-- occur only at Herald admission.
data ApplicationSortDescriptor = ApplicationSortDescriptor
  { sortKind :: ApplicationSortKind,
    valueSchema :: ApplicationValueSchema,
    keyProjections :: [ApplicationProjection],
    validityPredicate :: ApplicationPredicateExpression,
    obsolescencePredicate :: ApplicationPredicateExpression,
    rankTerms :: NonEmpty ApplicationRankTerm,
    minimumRetentionMicros :: Word64,
    isImmutable :: Bool,
    labelField :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | A structured sort definition without canonical descriptor bytes.
--
-- A declared definition contains only application-selectable facts and may ask
-- the Herald to verify a public identity claim. A predefined definition selects
-- exactly one member of the immutable six-role catalogue and carries its exact
-- public identity. The closed arm lets read/take present Herald-managed
-- primordial descriptors faithfully without allowing an application to invent a
-- structural-carrier role or mutation policy.
data ApplicationSortDefinition
  = DeclaredSortDefinition ApplicationSortDescriptor (Maybe SortId)
  | PredefinedSortDefinition ApplicationPredefinedSortRole SortId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)
