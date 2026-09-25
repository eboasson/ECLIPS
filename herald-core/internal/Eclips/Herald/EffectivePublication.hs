{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Live interpretation of immutable publications through the
-- latest applied label, process-lifecycle, and local-suppression facts.
--
-- Store and protocol owners retain the raw 'CheckedPublication'.  A
-- coordinator constructs this view from the exact live Controlled projection
-- before Store query, take, wait, snapshot, retained-change, peer, or
-- structural preparation.  The adapters consume the live owner cut,
-- but each named preparation boundary consumes this shared checked view.
-- Keeping the constructor private prevents a Store consumer from pairing an
-- effective value with unrelated raw provenance.
module Eclips.Herald.EffectivePublication
  ( PublicationHiddenReason (..),
    CheckedEffectivePublicationContext,
    checkEffectivePublicationContext,
    EffectivePublicationView,
    EffectivePublicationProblem (..),
    interpretEffectivePublication,
    projectEffectivePublicationZombieOverlay,
    effectivePublicationBindsRaw,
    effectivePublicationHiddenReason,
    effectivePublicationRawPublication,
    effectivePublicationValue,
    effectivePublicationController,
    effectivePublicationAuthority,
    effectivePublicationMatchesQuery,
    effectivePublicationVisiblePair,
    EffectivePeerPreparation,
    prepareEffectivePeerPublication,
    effectivePeerPreparationHiddenReason,
    effectivePeerPreparationVisiblePair,
    effectivePeerPreparationController,
    effectivePeerPreparationAuthority,
    EffectiveStructuralPreparation,
    prepareEffectiveStructuralPublication,
    effectiveStructuralPreparationHiddenReason,
    effectiveStructuralPreparationVisiblePair,
    effectiveStructuralPreparationController,
    effectiveStructuralPreparationAuthority,
  )
where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    GlobalObjectId,
    ProcessEpochId,
    SortId,
    globalObjectIdFromGlobalUniqueId,
    publicationAuthorityEpoch,
  )
import Eclips.Domain.Label
  ( ReleasedLabelStateView (..),
    labelRecordReleasedState,
    labelRecordRetainedAuthority,
    releasedLabelStateView,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationSort,
    checkedPublicationValue,
  )
import Eclips.Domain.Query
  ( CheckedQueryPredicate,
    QueryEvaluationError,
    matchesQueryPredicate,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( CheckedDescriptor,
    SortKind (..),
    descriptorControlledKeyProjection,
    descriptorKind,
    descriptorLabelField,
  )
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (..),
    Value,
    ValueView (..),
    directProjection,
    labelValue,
    recordValueMap,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled

data PublicationHiddenReason
  = PublicationDeleted
  | PublicationObsoleteSuppressed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data EffectivePublicationView
  = PublicationHidden CheckedPublication PublicationHiddenReason
  | PublicationVisible
      CheckedPublication
      Value
      (Maybe ProcessEpochId)
      (Maybe AuthorityEpoch)
  deriving stock (Eq, Show)

-- | Projection-bound interpretation input.  Construction consumes the exact
-- immutable raw publication together with the supplied live Controlled
-- projection. The resulting context has already resolved
-- suppression, End knowledge, embedded tenure, and any delete present in the
-- supplied overlay; no consumer can independently replace those facts within
-- this cut.
newtype CheckedEffectivePublicationContext
  = CheckedEffectivePublicationContext EffectivePublicationView

data EffectivePublicationProblem
  = EffectivePublicationDescriptorSortMismatch SortId SortId
  | EffectivePublicationControlledKeyUnavailable
  | EffectivePublicationControlledKeyShapeContradiction
  | EffectivePublicationOverlayRequiresLabel GlobalObjectId
  | EffectivePublicationControlledObjectUnavailable GlobalObjectId
  | EffectivePublicationControlledObservationUnavailable GlobalObjectId
  | EffectivePublicationControlledObservationMismatch GlobalObjectId
  | EffectivePublicationLabelShapeContradiction
  deriving stock (Eq, Show)

checkEffectivePublicationContext ::
  CanonicalDescriptor ->
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem CheckedEffectivePublicationContext
checkEffectivePublicationContext
  descriptor
  controlled
  publication = do
    let expectedSort = descriptorSortId descriptor
        observedSort = checkedPublicationSort publication
    unless
      (expectedSort == observedSort)
      (Left (EffectivePublicationDescriptorSortMismatch expectedSort observedSort))
    view <- case descriptorKind (canonicalCheckedDescriptor descriptor) of
      RegularSort -> Right (PublicationVisible publication (checkedPublicationValue publication) Nothing Nothing)
      ControlledSort -> checkedControlledView descriptor controlled publication
    Right (CheckedEffectivePublicationContext view)

-- | The shared interpreter is total after live-owner checking.
interpretEffectivePublication ::
  CheckedEffectivePublicationContext -> EffectivePublicationView
interpretEffectivePublication (CheckedEffectivePublicationContext view) = view

-- | Present one immutable abandoned-branch view without changing the retained
-- publication or its canonical Store bytes.  Predicate evaluation and later
-- localization consume this same projected value, so a zombie-label query
-- cannot disagree with the value returned to the application.  Records are
-- traversed recursively because labels may occur below a direct field.
projectEffectivePublicationZombieOverlay ::
  Set ProcessEpochId -> EffectivePublicationView -> EffectivePublicationView
projectEffectivePublicationZombieOverlay zombies view = case view of
  PublicationHidden {} -> view
  PublicationVisible publication value controller authority ->
    PublicationVisible
      publication
      (projectValue value)
      (controller >>= retainLiveController)
      (if maybe False (`Set.member` zombies) controller then Nothing else authority)
  where
    retainLiveController process
      | process `Set.member` zombies = Nothing
      | otherwise = Just process

    projectValue value = case viewValue value of
      LabelValue (ProcessLabel process, generation)
        | process `Set.member` zombies -> labelValue (ZombieLabel process, generation)
      RecordValue fields -> recordValueMap (fmap projectValue fields)
      _ -> value

checkedControlledView ::
  CanonicalDescriptor ->
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem EffectivePublicationView
checkedControlledView descriptor controlled publication = do
  let checkedDescriptor = canonicalCheckedDescriptor descriptor
      labelField = descriptorLabelField checkedDescriptor
  object <- controlledObject checkedDescriptor publication
  if Controlled.controlledObjectTerminallyDeleted object controlled
    then Right (PublicationHidden publication PublicationDeleted)
    else checkedRetainedView object labelField
  where
    checkedRetainedView object labelField = do
      record <-
        maybe
          (Left (EffectivePublicationControlledObjectUnavailable object))
          Right
          (Controlled.controlledLocalRecord object controlled)
      retained <-
        maybe
          (Left (EffectivePublicationControlledObservationUnavailable object))
          Right
          ( Controlled.controlledRecordObservation
              (checkedPublicationId publication)
              record
          )
      unless
        (retained == publication)
        (Left (EffectivePublicationControlledObservationMismatch object))
      let overlay = Controlled.controlledReleasedLabelRecord object controlled
      case overlay of
        Just _ ->
          unless
            (labelField /= Nothing)
            (Left (EffectivePublicationOverlayRequiresLabel object))
        Nothing -> Right ()
      case fmap
        (releasedLabelStateView . labelRecordReleasedState)
        overlay of
        Just (ReleasedDeletedView _) ->
          Right (PublicationHidden publication PublicationDeleted)
        _ -> case Controlled.controlledRecordSuppression record of
          Controlled.ControlledObsoleteSuppression _ ->
            Right (PublicationHidden publication PublicationObsoleteSuppressed)
          Controlled.ControlledUnsuppressed ->
            visible object controlled overlay labelField

    visible object controlledState overlay labelField = do
      (effectiveValue, effectiveLabel) <- case labelField of
        Nothing -> Right (checkedPublicationValue publication, Nothing)
        Just field -> do
          embedded <- publicationLabel field publication
          released <-
            maybe
              (Right embedded)
              ( \state -> case releasedLabelStateView state of
                  ReleasedLabelView label -> Right label
                  ReleasedDeletedView {} ->
                    Left
                      (EffectivePublicationControlledObservationMismatch object)
              )
              (Controlled.controlledEffectiveLabelState object controlledState)
          value <- replacePublicationLabel field released publication
          Right (value, Just released)
      let controller = effectiveLabel >>= liveController
          embeddedAuthority =
            publicationAuthorityEpoch (checkedPublicationId publication)
          authority = case controller of
            Nothing -> Nothing
            Just _ ->
              maybe
                (Just embeddedAuthority)
                labelRecordRetainedAuthority
                overlay
      Right
        ( PublicationVisible
            publication
            effectiveValue
            controller
            authority
        )

controlledObject ::
  CheckedDescriptor ->
  CheckedPublication ->
  Either EffectivePublicationProblem GlobalObjectId
controlledObject descriptor publication = do
  projection <-
    maybe
      (Left EffectivePublicationControlledKeyUnavailable)
      Right
      (descriptorControlledKeyProjection descriptor)
  projected <-
    either
      (const (Left EffectivePublicationControlledKeyShapeContradiction))
      Right
      (valueAt projection (checkedPublicationValue publication))
  case viewValue projected of
    GlobalUniqueIdValue identifier ->
      Right (globalObjectIdFromGlobalUniqueId identifier)
    _ -> Left EffectivePublicationControlledKeyShapeContradiction

publicationLabel ::
  FieldName ->
  CheckedPublication ->
  Either EffectivePublicationProblem Label
publicationLabel field publication = do
  projected <-
    either
      (const (Left EffectivePublicationLabelShapeContradiction))
      Right
      (valueAt (directProjection field) (checkedPublicationValue publication))
  case viewValue projected of
    LabelValue label -> Right label
    _ -> Left EffectivePublicationLabelShapeContradiction

replacePublicationLabel ::
  FieldName ->
  Label ->
  CheckedPublication ->
  Either EffectivePublicationProblem Value
replacePublicationLabel field label publication =
  case viewValue (checkedPublicationValue publication) of
    RecordValue fields
      | Map.member field fields ->
          Right (recordValueMap (Map.insert field (labelValue label) fields))
    _ -> Left EffectivePublicationLabelShapeContradiction

liveController :: Label -> Maybe ProcessEpochId
liveController label = case fst label of
  ProcessLabel process -> Just process
  VoidLabel -> Nothing
  ZombieLabel _ -> Nothing

effectivePublicationHiddenReason ::
  EffectivePublicationView -> Maybe PublicationHiddenReason
effectivePublicationHiddenReason view = case view of
  PublicationHidden _ reason -> Just reason
  PublicationVisible {} -> Nothing

-- | Exact immutable-publication binding retained even by hidden views without
-- exposing the hidden payload to consumers.
effectivePublicationBindsRaw ::
  CheckedPublication -> EffectivePublicationView -> Bool
effectivePublicationBindsRaw supplied view = case view of
  PublicationHidden retained _ -> supplied == retained
  PublicationVisible retained _ _ _ -> supplied == retained

effectivePublicationRawPublication ::
  EffectivePublicationView -> Maybe CheckedPublication
effectivePublicationRawPublication view = case view of
  PublicationHidden {} -> Nothing
  PublicationVisible publication _ _ _ -> Just publication

effectivePublicationValue :: EffectivePublicationView -> Maybe Value
effectivePublicationValue view = case view of
  PublicationHidden {} -> Nothing
  PublicationVisible _ value _ _ -> Just value

effectivePublicationController ::
  EffectivePublicationView -> Maybe ProcessEpochId
effectivePublicationController view = case view of
  PublicationHidden {} -> Nothing
  PublicationVisible _ _ controller _ -> controller

-- | Project the tenure retained by the live Controlled overlay owner.
effectivePublicationAuthority ::
  EffectivePublicationView -> Maybe AuthorityEpoch
effectivePublicationAuthority view = case view of
  PublicationHidden {} -> Nothing
  PublicationVisible _ _ _ authority -> authority

-- | The one predicate-evaluation seam.  Hidden candidates are rejected before
-- the checked predicate can inspect any raw value.
effectivePublicationMatchesQuery ::
  CheckedQueryPredicate ->
  EffectivePublicationView ->
  Either QueryEvaluationError Bool
effectivePublicationMatchesQuery predicate view = case view of
  PublicationHidden {} -> Right False
  PublicationVisible _ value _ _ -> matchesQueryPredicate predicate value

-- | Shared materialization primitive.  Concrete query/take/wait, snapshot, and
-- retained-change adapters live in 'Eclips.Herald.Store.Observation'; the
-- peer and structural preparations below retain distinct nominal boundaries.
effectivePublicationVisiblePair ::
  EffectivePublicationView -> Maybe (CheckedPublication, Value)
effectivePublicationVisiblePair view = case view of
  PublicationHidden {} -> Nothing
  PublicationVisible publication value _ _ -> Just (publication, value)

-- | Checked peer/forward preparation. A hidden publication becomes an
-- explicit terminal suppression rather than a payload whose embedded value
-- can accidentally be forwarded.  A visible preparation retains the exact
-- raw identity together with its projected value.
data EffectivePeerPreparation
  = EffectivePeerSuppressed PublicationHiddenReason
  | EffectivePeerPrepared
      CheckedPublication
      Value
      (Maybe ProcessEpochId)
      (Maybe AuthorityEpoch)
  deriving stock (Eq, Show)

prepareEffectivePeerPublication ::
  EffectivePublicationView -> EffectivePeerPreparation
prepareEffectivePeerPublication view = case view of
  PublicationHidden _ reason -> EffectivePeerSuppressed reason
  PublicationVisible publication value controller authority ->
    EffectivePeerPrepared publication value controller authority

effectivePeerPreparationHiddenReason ::
  EffectivePeerPreparation -> Maybe PublicationHiddenReason
effectivePeerPreparationHiddenReason prepared = case prepared of
  EffectivePeerSuppressed reason -> Just reason
  EffectivePeerPrepared {} -> Nothing

effectivePeerPreparationVisiblePair ::
  EffectivePeerPreparation -> Maybe (CheckedPublication, Value)
effectivePeerPreparationVisiblePair prepared = case prepared of
  EffectivePeerSuppressed {} -> Nothing
  EffectivePeerPrepared publication value _ _ -> Just (publication, value)

effectivePeerPreparationController ::
  EffectivePeerPreparation -> Maybe ProcessEpochId
effectivePeerPreparationController prepared = case prepared of
  EffectivePeerSuppressed {} -> Nothing
  EffectivePeerPrepared _ _ controller _ -> controller

effectivePeerPreparationAuthority ::
  EffectivePeerPreparation -> Maybe AuthorityEpoch
effectivePeerPreparationAuthority prepared = case prepared of
  EffectivePeerSuppressed {} -> Nothing
  EffectivePeerPrepared _ _ _ authority -> authority

-- | Checked structural decoding/reconciliation preparation. It is a
-- separate nominal boundary from peer forwarding so structural code cannot
-- accept an unprojected 'CheckedPublication' by accident.
data EffectiveStructuralPreparation
  = EffectiveStructuralSuppressed PublicationHiddenReason
  | EffectiveStructuralPrepared
      CheckedPublication
      Value
      (Maybe ProcessEpochId)
      (Maybe AuthorityEpoch)
  deriving stock (Eq, Show)

prepareEffectiveStructuralPublication ::
  EffectivePublicationView -> EffectiveStructuralPreparation
prepareEffectiveStructuralPublication view = case view of
  PublicationHidden _ reason -> EffectiveStructuralSuppressed reason
  PublicationVisible publication value controller authority ->
    EffectiveStructuralPrepared publication value controller authority

effectiveStructuralPreparationHiddenReason ::
  EffectiveStructuralPreparation -> Maybe PublicationHiddenReason
effectiveStructuralPreparationHiddenReason prepared = case prepared of
  EffectiveStructuralSuppressed reason -> Just reason
  EffectiveStructuralPrepared {} -> Nothing

effectiveStructuralPreparationVisiblePair ::
  EffectiveStructuralPreparation -> Maybe (CheckedPublication, Value)
effectiveStructuralPreparationVisiblePair prepared = case prepared of
  EffectiveStructuralSuppressed {} -> Nothing
  EffectiveStructuralPrepared publication value _ _ -> Just (publication, value)

effectiveStructuralPreparationController ::
  EffectiveStructuralPreparation -> Maybe ProcessEpochId
effectiveStructuralPreparationController prepared = case prepared of
  EffectiveStructuralSuppressed {} -> Nothing
  EffectiveStructuralPrepared _ _ controller _ -> controller

effectiveStructuralPreparationAuthority ::
  EffectiveStructuralPreparation -> Maybe AuthorityEpoch
effectiveStructuralPreparationAuthority prepared = case prepared of
  EffectiveStructuralSuppressed {} -> Nothing
  EffectiveStructuralPrepared _ _ _ authority -> authority
