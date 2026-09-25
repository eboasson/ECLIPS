-- | Globalization of private application query syntax.
--
-- Descriptor and store admission remain coordinator responsibilities because
-- they require Controlled, SortRegistry, Placement, and Store projections.
module Eclips.Herald.Application.Query
  ( GlobalizedApplicationQuery,
    globalizedQueryDeltas,
    globalizedQueryPredicate,
    globalizedQueryHasComparison,
    globalizedQueryComparisons,
    globalizedQueryApplicationProjection,
    ApplicationQueryGlobalizationError (..),
    globalizeApplicationQuery,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    privateDeltaUniqueId,
  )
import Eclips.Application.Types.Query qualified as Application
import Eclips.Domain.Identity
  ( DeltaId,
    ProcessEpochId,
    deltaIdFromGlobalObjectId,
    globalObjectIdFromGlobalUniqueId,
  )
import Eclips.Domain.Query qualified as Domain
import Eclips.Domain.Value
  ( FieldNameError,
    Projection,
    enumSymbol,
    mkFieldName,
    mkProjection,
  )
import Eclips.Herald.Application.State
  ( ApplicationValueGlobalizationError,
    ProcessRoleView,
    State,
    globalizeApplicationLabel,
    resolveApplicationPrivateUniqueId,
  )

-- | Private operands paired with their resolved semantic identities plus the
-- recursively globalized predicate.
data GlobalizedApplicationQuery
  = GlobalizedApplicationQuery
      [(PrivateDeltaId, DeltaId)]
      Domain.QueryPredicate
      [(Application.ApplicationProjection, Projection)]
  deriving stock (Eq, Show)

globalizedQueryDeltas ::
  GlobalizedApplicationQuery -> [(PrivateDeltaId, DeltaId)]
globalizedQueryDeltas (GlobalizedApplicationQuery deltas _ _) = deltas

globalizedQueryPredicate ::
  GlobalizedApplicationQuery -> Domain.QueryPredicate
globalizedQueryPredicate (GlobalizedApplicationQuery _ predicate _) = predicate

globalizedQueryHasComparison :: GlobalizedApplicationQuery -> Bool
globalizedQueryHasComparison (GlobalizedApplicationQuery _ _ comparisons) =
  not (null comparisons)

globalizedQueryComparisons ::
  GlobalizedApplicationQuery -> [Application.ApplicationProjection]
globalizedQueryComparisons (GlobalizedApplicationQuery _ _ comparisons) =
  fmap fst comparisons

globalizedQueryApplicationProjection ::
  Projection ->
  GlobalizedApplicationQuery ->
  Maybe Application.ApplicationProjection
globalizedQueryApplicationProjection projection (GlobalizedApplicationQuery _ _ comparisons) =
  lookup projection [(domainProjection, applicationProjection) | (applicationProjection, domainProjection) <- comparisons]

data ApplicationQueryGlobalizationError
  = ApplicationQueryInvalidFieldName Text FieldNameError
  | ApplicationQueryIdentityError ApplicationValueGlobalizationError
  deriving stock (Eq, Show)

globalizeApplicationQuery ::
  ProcessRoleView ->
  ProcessEpochId ->
  Application.ApplicationQuery ->
  State ->
  Either ApplicationQueryGlobalizationError GlobalizedApplicationQuery
globalizeApplicationQuery roles process query state = do
  deltas <- traverse resolveDelta (Set.toAscList (Application.applicationQueryDeltas query))
  predicate <- convertPredicate (Application.applicationQueryPredicate query)
  comparisons <-
    traverse
      ( \applicationProjection -> do
          domainProjection <- convertProjection applicationProjection
          Right (applicationProjection, domainProjection)
      )
      (comparisonProjections (Application.applicationQueryPredicate query))
  Right
    ( GlobalizedApplicationQuery
        deltas
        predicate
        comparisons
    )
  where
    resolveDelta privateDelta = do
      global <-
        either
          (Left . ApplicationQueryIdentityError)
          Right
          ( resolveApplicationPrivateUniqueId
              process
              (privateDeltaUniqueId privateDelta)
              state
          )
      Right
        ( privateDelta,
          deltaIdFromGlobalObjectId (globalObjectIdFromGlobalUniqueId global)
        )
    convertPredicate = \case
      Application.QueryAlways -> Right Domain.QueryAlways
      Application.QueryNever -> Right Domain.QueryNever
      Application.QueryCompare projection comparison literal ->
        Domain.QueryCompare
          <$> convertProjection projection
          <*> pure (convertComparison comparison)
          <*> convertLiteral literal
      Application.QueryNot child -> Domain.QueryNot <$> convertPredicate child
      Application.QueryAll children ->
        Domain.QueryAll <$> traverse convertPredicate children
      Application.QueryAny children ->
        Domain.QueryAny <$> traverse convertPredicate children
    convertLiteral = \case
      Application.QueryBool value -> Right (Domain.QueryBool value)
      Application.QueryInt64 value -> Right (Domain.QueryInt64 value)
      Application.QueryBytes value -> Right (Domain.QueryBytes value)
      Application.QueryText value -> Right (Domain.QueryText value)
      Application.QueryUniqueId privateIdentity ->
        Domain.QueryGlobalUniqueId
          <$> either
            (Left . ApplicationQueryIdentityError)
            Right
            (resolveApplicationPrivateUniqueId process privateIdentity state)
      Application.QueryLabel label ->
        Domain.QueryLabel
          <$> either
            (Left . ApplicationQueryIdentityError)
            Right
            (globalizeApplicationLabel roles process label state)
      Application.QueryEnum value ->
        Right (Domain.QueryEnum (enumSymbol value))

convertProjection ::
  Application.ApplicationProjection ->
  Either ApplicationQueryGlobalizationError Projection
convertProjection (Application.ApplicationProjection fields) =
  mkProjection <$> traverse convertField fields
  where
    convertField field =
      either
        (Left . ApplicationQueryInvalidFieldName field)
        Right
        (mkFieldName field)

convertComparison ::
  Application.ApplicationScalarComparison -> Domain.ScalarComparison
convertComparison = \case
  Application.ScalarEqual -> Domain.ScalarEqual
  Application.ScalarNotEqual -> Domain.ScalarNotEqual
  Application.ScalarLessThan -> Domain.ScalarLessThan
  Application.ScalarLessThanOrEqual -> Domain.ScalarLessThanOrEqual
  Application.ScalarGreaterThan -> Domain.ScalarGreaterThan
  Application.ScalarGreaterThanOrEqual -> Domain.ScalarGreaterThanOrEqual

comparisonProjections ::
  Application.ApplicationQueryPredicate ->
  [Application.ApplicationProjection]
comparisonProjections = \case
  Application.QueryAlways -> []
  Application.QueryNever -> []
  Application.QueryCompare projection _ _ -> [projection]
  Application.QueryNot child -> comparisonProjections child
  Application.QueryAll children -> foldMap comparisonProjections children
  Application.QueryAny children -> foldMap comparisonProjections children
