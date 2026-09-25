{-# LANGUAGE ImportQualifiedPost #-}

-- | Authority-qualified, atomic controlled-object deletion consequences.
--
-- Label deletion and predefined disappearance enter through distinct checked
-- preparations.  They share only the normalized owner applicator: neither
-- entry point can construct or borrow the other's authority.
module Eclips.Herald.UseCase.ControlledRemoval
  ( ControlledRemovalProblem (..),
    ControlledRemovalOrigin (..),
    ControlledRemovalDisposition (..),
    ControlledRemovalSummary,
    controlledRemovalSummaryOrigin,
    controlledRemovalSummaryObject,
    controlledRemovalSummaryRole,
    controlledRemovalSummaryDisposition,
    controlledRemovalSummaryStorePurgedFacts,
    controlledRemovalSummaryCancelledReservations,
    controlledRemovalSummaryFalloutCount,
    controlledRemovalSummaryLogicalWork,
    PreparedControlledRemoval,
    prepareLabelControlledRemoval,
    prepareDisappearanceControlledRemoval,
    prepareImportedDisappearanceControlledRemoval,
    commitControlledRemoval,
  )
where

import Control.Monad (foldM, unless)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceProbeId,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubjectMembershipCoordinate,
    disappearanceResolutionOutcomeView,
    disappearanceSubjectMembershipCoordinate,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    StructuralOccurrenceId,
    nablaIdFromGlobalObjectId,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    labelRecordRevision,
    releasedDeleted,
    releasedLabelStateGeneration,
    releasedLabelStateIsDeleted,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseError,
    StructuralConsequenceCauseView (LabelReleaseCauseView),
    labelReleaseCause,
    predefinedDisappearanceCause,
    structuralConsequenceCauseView,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupControlledState,
    replaceStartupGraphState,
    replaceStartupLabelPatchState,
    replaceStartupPlacementState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupControlledState,
    startupDiagnosticChecks,
    startupGenesis,
    startupGraphState,
    startupLabelPatchState,
    startupOracleProjectionState,
    startupPlacementState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt qualified as StructuralDebt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Disappearance (projectedDisappearanceProbeHeaderId)

data ControlledRemovalOrigin
  = LabelControlledRemoval LabelDecisionId ControlIndex
  | DisappearanceControlledRemoval
      Disappearance.ControlledRemovalAuthority
  | ImportedDisappearanceControlledRemoval DisappearanceProbeId ControlIndex
  deriving stock (Eq, Show)

data ControlledRemovalDisposition
  = ControlledRemovalApplied
  | ControlledRemovalExactReplay
  deriving stock (Eq, Ord, Show)

data ControlledRemovalProblem
  = ControlledRemovalCauseProblem StructuralConsequenceCauseError
  | ControlledRemovalControlledProblem Controlled.ControlledRemovalError
  | ControlledRemovalStorePurgeProblem Store.ControlledObjectPurgeError
  | ControlledRemovalReconciliationProblem
      Reconciliation.StructuralReconciliationProblem
  | ControlledRemovalStorePatchProblem Store.StructuralStorePatchError
  | ControlledRemovalPlacementPatchProblem Placement.StructuralPlacementPatchError
  | ControlledRemovalProgressProblem GraphProgress.StructuralProgressProblem
  | ControlledRemovalAlignmentProblem Alignment.AlignmentDebtProblem
  | ControlledRemovalAlignmentLossProblem AlignmentLoss.AlignmentLossProblem
  | ControlledRemovalAlignmentLossInvariant
      AlignmentLoss.AlignmentLossInvariantProblem
  | ControlledRemovalAlignmentTransferProblem
      AlignmentTransfer.AlignmentTransferCoordinatorProblem
  | ControlledRemovalLabelRoleNotDeleted GlobalObjectId
  | ControlledRemovalLabelPatchMissing LabelDecisionId
  | ControlledRemovalLabelPatchRecipeMismatch LabelDecisionId
  | ControlledRemovalLabelPatchNotReleased LabelDecisionId
  | ControlledRemovalLabelPatchReleaseMismatch
      LabelDecisionId
      ControlIndex
      ControlIndex
  | ControlledRemovalCapturedOccurrenceMissing StructuralOccurrenceId
  | ControlledRemovalCapturedObjectMismatch GlobalObjectId GlobalObjectId
  | ControlledRemovalCapturedRoleIneligible GlobalObjectId StructuralCarrierRole
  | ControlledRemovalCapturedMembershipMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | ControlledRemovalExpectedLabelRevisionMismatch
      GlobalObjectId
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | ControlledRemovalTopologyUnexpectedlyHeld
  | ControlledRemovalImportedResolutionInvalid
  deriving stock (Eq, Show)

data ControlledRemovalSummary
  = ControlledRemovalSummary
      ControlledRemovalOrigin
      GlobalObjectId
      (Maybe StructuralCarrierRole)
      ControlledRemovalDisposition
      Word64
      Word64
      Word64
      Word64
      Word64
      Word64
      Word64
      Word64
      Word64
  deriving stock (Eq, Show)

controlledRemovalSummaryOrigin ::
  ControlledRemovalSummary -> ControlledRemovalOrigin
controlledRemovalSummaryOrigin
  (ControlledRemovalSummary origin _ _ _ _ _ _ _ _ _ _ _ _) = origin

controlledRemovalSummaryObject :: ControlledRemovalSummary -> GlobalObjectId
controlledRemovalSummaryObject
  (ControlledRemovalSummary _ object _ _ _ _ _ _ _ _ _ _ _) = object

controlledRemovalSummaryRole ::
  ControlledRemovalSummary -> Maybe StructuralCarrierRole
controlledRemovalSummaryRole
  (ControlledRemovalSummary _ _ role _ _ _ _ _ _ _ _ _ _) = role

controlledRemovalSummaryDisposition ::
  ControlledRemovalSummary -> ControlledRemovalDisposition
controlledRemovalSummaryDisposition
  (ControlledRemovalSummary _ _ _ disposition _ _ _ _ _ _ _ _ _) = disposition

controlledRemovalSummaryStorePurgedFacts :: ControlledRemovalSummary -> Word64
controlledRemovalSummaryStorePurgedFacts
  (ControlledRemovalSummary _ _ _ _ facts _ _ _ _ _ _ _ _) = facts

controlledRemovalSummaryCancelledReservations :: ControlledRemovalSummary -> Word64
controlledRemovalSummaryCancelledReservations
  (ControlledRemovalSummary _ _ _ _ _ _ _ reservations _ _ _ _ _) =
    reservations

-- | Exact number of owner-local consequences in the prepared transaction:
-- purged Store facts, revoked direct/Store possessions, cancelled pristine
-- Nabla reservations, graph and placement changes, structural debts, local
-- placement losses, and completed wait registrations.
controlledRemovalSummaryFalloutCount :: ControlledRemovalSummary -> Word64
controlledRemovalSummaryFalloutCount
  (ControlledRemovalSummary _ _ _ disposition purged direct store reservations graph placement debts losses completedWaits) =
    case disposition of
      ControlledRemovalExactReplay -> 0
      ControlledRemovalApplied ->
        purged + direct + store + reservations + graph + placement + debts + losses + completedWaits

-- | Increment-7 logical work contract: four fixed authority/owner steps plus
-- one unit per exact owner-local fallout.  Exact replay is a zero-work control.
controlledRemovalSummaryLogicalWork :: ControlledRemovalSummary -> Word64
controlledRemovalSummaryLogicalWork summary@(ControlledRemovalSummary _ _ _ disposition _ _ _ _ _ _ _ _ _) =
  case disposition of
    ControlledRemovalExactReplay -> 0
    ControlledRemovalApplied ->
      4 + controlledRemovalSummaryFalloutCount summary

data PreparedControlledRemoval
  = PreparedControlledRemoval HeraldState EffectBatch ControlledRemovalSummary

prepareLabelControlledRemoval ::
  ControlIndex ->
  LabelPatch.LabelPatchRecipe ->
  HeraldState ->
  Either ControlledRemovalProblem PreparedControlledRemoval
prepareLabelControlledRemoval releaseIndex recipe state = do
  let object = LabelPatch.labelPatchRecipeObject recipe
      decision = LabelPatch.labelPatchRecipeDecision recipe
      origin = LabelControlledRemoval decision releaseIndex
  authenticateLabelRemoval releaseIndex recipe state
  case LabelPatch.targetRoleView (LabelPatch.labelPatchRecipeRole recipe) of
    LabelPatch.AbsentOrdinaryTargetView ->
      prepareNormalizedRemoval origin object Nothing False state
    LabelPatch.OrdinaryControlledTargetView ->
      prepareNormalizedRemoval origin object Nothing False state
    LabelPatch.NeutralTargetView _ ->
      prepareNormalizedRemoval
        origin
        object
        (Just NeutralVertexCarrier)
        True
        state
    LabelPatch.EdgeTargetView _ ->
      prepareNormalizedRemoval
        origin
        object
        (Just EdgeCarrier)
        True
        state
    LabelPatch.NablaTargetView _ _ ->
      prepareNormalizedRemoval
        origin
        object
        (Just NablaCarrier)
        True
        state
    LabelPatch.DeltaTargetView _ _ ->
      prepareNormalizedRemoval
        origin
        object
        (Just DeltaCarrier)
        True
        state

authenticateLabelRemoval ::
  ControlIndex ->
  LabelPatch.LabelPatchRecipe ->
  HeraldState ->
  Either ControlledRemovalProblem ()
authenticateLabelRemoval releaseIndex recipe state = do
  let object = LabelPatch.labelPatchRecipeObject recipe
      decision = LabelPatch.labelPatchRecipeDecision recipe
  unless
    (releasedLabelStateIsDeleted (LabelPatch.labelPatchRecipeProposedState recipe))
    (Left (ControlledRemovalLabelRoleNotDeleted object))
  retained <-
    maybe
      (Left (ControlledRemovalLabelPatchMissing decision))
      Right
      (LabelPatch.lookupRetainedLabelPatch decision (startupLabelPatchState state))
  installed <-
    maybe
      (Left (ControlledRemovalLabelPatchNotReleased decision))
      Right
      (LabelPatch.retainedPatchViewInstallation retained)
  unless
    (LabelPatch.installedLabelMatchesRecipe installed recipe)
    (Left (ControlledRemovalLabelPatchRecipeMismatch decision))
  let retainedIndex = LabelPatch.installedLabelReleaseIndex installed
  unless
    (retainedIndex == releaseIndex)
    ( Left
        ( ControlledRemovalLabelPatchReleaseMismatch
            decision
            releaseIndex
            retainedIndex
        )
    )

prepareDisappearanceControlledRemoval ::
  Disappearance.ControlledRemovalAuthority ->
  HeraldState ->
  Either ControlledRemovalProblem PreparedControlledRemoval
prepareDisappearanceControlledRemoval authority state = do
  authenticateCapturedMembership authority state
  role <- authenticateCapturedOccurrence authority state
  authenticateExpectedRevision authority state
  prepareNormalizedRemoval
    (DisappearanceControlledRemoval authority)
    (Disappearance.controlledRemovalAuthorityObject authority)
    (Just role)
    True
    state

-- | A joining observer replays the canonical all-member resolution without
-- manufacturing a local probe, absence report, or captured-member authority.
prepareImportedDisappearanceControlledRemoval ::
  OracleProjection.ProjectedDisappearanceProbe ->
  HeraldState ->
  Either ControlledRemovalProblem PreparedControlledRemoval
prepareImportedDisappearanceControlledRemoval projected state = do
  let probe = projectedDisappearanceProbeHeaderId (OracleProjection.projectedDisappearanceHeader projected)
  unless
    ( GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)
        && OracleProjection.oracleViewDisappearanceProbe probe (OracleProjection.oracleView (startupOracleProjectionState state)) == Just projected
    )
    (Left ControlledRemovalImportedResolutionInvalid)
  case OracleProjection.projectedDisappearanceTerminal projected of
    Just (OracleProjection.ProjectedDisappearanceResolved outcome index) ->
      case disappearanceResolutionOutcomeView outcome of
        ControlledDisappearanceResolved object occurrence revision _ -> do
          role <- authenticateCapturedOccurrenceValues occurrence object state
          authenticateExpectedRevisionValues object revision state
          prepareNormalizedRemoval
            (ImportedDisappearanceControlledRemoval probe index)
            object
            (Just role)
            True
            state
        RegularSortDefinitionRetired {} -> Left ControlledRemovalImportedResolutionInvalid
    _ -> Left ControlledRemovalImportedResolutionInvalid

authenticateCapturedMembership ::
  Disappearance.ControlledRemovalAuthority ->
  HeraldState ->
  Either ControlledRemovalProblem ()
authenticateCapturedMembership authority state =
  if actual == expected
    then Right ()
    else
      Left
        (ControlledRemovalCapturedMembershipMismatch expected actual)
  where
    actual = Disappearance.controlledRemovalAuthorityCoordinate authority
    oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
    expected =
      disappearanceSubjectMembershipCoordinate
        (Disappearance.controlledRemovalAuthoritySubject authority)
        (OracleProjection.oracleViewCurrentHeraldMembership oracleView)

authenticateCapturedOccurrence ::
  Disappearance.ControlledRemovalAuthority ->
  HeraldState ->
  Either ControlledRemovalProblem StructuralCarrierRole
authenticateCapturedOccurrence authority state = do
  let occurrence =
        Disappearance.controlledRemovalAuthorityStructuralOccurrence authority
      object = Disappearance.controlledRemovalAuthorityObject authority
  authenticateCapturedOccurrenceValues occurrence object state

authenticateCapturedOccurrenceValues ::
  StructuralOccurrenceId -> GlobalObjectId -> HeraldState -> Either ControlledRemovalProblem StructuralCarrierRole
authenticateCapturedOccurrenceValues occurrence object state = do
  let reconciliation =
        GraphProgress.structuralProgressReconciliation
          (startupStructuralProgressState state)
  projection <-
    maybe
      (Left (ControlledRemovalCapturedOccurrenceMissing occurrence))
      Right
      ( Reconciliation.structuralAppliedControlledProjectionAtOccurrence
          occurrence
          reconciliation
      )
  let retainedObject =
        Reconciliation.appliedControlledProjectionObject projection
      role = Reconciliation.appliedControlledProjectionRole projection
  if retainedObject == object
    then Right ()
    else Left (ControlledRemovalCapturedObjectMismatch object retainedObject)
  case role of
    ProcessEpochCarrier ->
      Left (ControlledRemovalCapturedRoleIneligible object role)
    _ -> Right role

authenticateExpectedRevision ::
  Disappearance.ControlledRemovalAuthority ->
  HeraldState ->
  Either ControlledRemovalProblem ()
authenticateExpectedRevision authority =
  authenticateExpectedRevisionValues
    (Disappearance.controlledRemovalAuthorityObject authority)
    (Disappearance.controlledRemovalAuthorityExpectedLabelRevision authority)

authenticateExpectedRevisionValues ::
  GlobalObjectId -> Maybe LabelRevision -> HeraldState -> Either ControlledRemovalProblem ()
authenticateExpectedRevisionValues object expected state =
  if actual == expected
    then Right ()
    else
      Left
        ( ControlledRemovalExpectedLabelRevisionMismatch
            object
            expected
            actual
        )
  where
    actual =
      labelRecordRevision
        <$> Controlled.controlledReleasedLabelRecord
          object
          (startupControlledState state)

prepareNormalizedRemoval ::
  ControlledRemovalOrigin ->
  GlobalObjectId ->
  Maybe StructuralCarrierRole ->
  Bool ->
  HeraldState ->
  Either ControlledRemovalProblem PreparedControlledRemoval
prepareNormalizedRemoval origin object role structural state = do
  cause <- removalCause origin
  preparedControlled <-
    either
      (Left . ControlledRemovalControlledProblem)
      Right
      (Controlled.prepareControlledRemoval cause object (startupControlledState state))
  let controlledDisposition =
        Controlled.preparedControlledRemovalDisposition preparedControlled
      payload =
        Controlled.preparedControlledRemovalPayloadCoordinate preparedControlled
      directRevoked =
        Controlled.preparedControlledRemovalRevokedDirectPossessions
          preparedControlled
      storeRevoked =
        Controlled.preparedControlledRemovalRevokedStorePossessions
          preparedControlled
      controlled0 = Controlled.commitControlledRemoval preparedControlled
  case controlledDisposition of
    Controlled.ControlledRemovalExactReplay ->
      Right
        ( PreparedControlledRemoval
            state
            mempty
            ( emptySummary
                origin
                object
                role
                ControlledRemovalExactReplay
            )
        )
    Controlled.ControlledRemovalInstalled -> do
      let (controlled, cancelledReservations) =
            retireNablaReservations role object controlled0
          withControlled = replaceStartupControlledState controlled state
      (withTopology, topologyEffects, graphChanges, placementChanges, debts, losses, completedWaits) <-
        if structural
          then applyStructuralRemoval cause (removalControlIndex origin) object withControlled
          else Right (withControlled, mempty, 0, 0, 0, 0, 0)
      (withPurge, purgedFacts) <-
        applyStorePurge cause payload withTopology
      successor <- advanceStructuralControlProgress (removalControlIndex origin) withPurge
      let summary =
            ControlledRemovalSummary
              origin
              object
              role
              ControlledRemovalApplied
              purgedFacts
              directRevoked
              storeRevoked
              cancelledReservations
              graphChanges
              placementChanges
              debts
              losses
              completedWaits
      Right (PreparedControlledRemoval successor topologyEffects summary)

-- | The live Oracle fold normally advances this prefix after applying all
-- projection events at an index.  Detached disappearance resolution has no
-- enclosing fold, so the shared transaction must establish the same owner
-- fact itself.  A label release's later outer advance is consequently an exact
-- no-op.
advanceStructuralControlProgress ::
  ControlIndex ->
  HeraldState ->
  Either ControlledRemovalProblem HeraldState
advanceStructuralControlProgress required state
  | current >= required = Right state
  | otherwise = do
      prepared <-
        either
          (Left . ControlledRemovalProgressProblem)
          Right
          ( GraphProgress.prepareStructuralControlProgress
              required
              (startupStructuralProgressState state)
          )
      let (progress, _) = GraphProgress.commitStructuralControlProgress prepared
      Right (replaceStartupStructuralProgressState progress state)
  where
    current =
      GraphProgress.structuralAppliedControlPrefix
        (startupStructuralProgressState state)

emptySummary ::
  ControlledRemovalOrigin ->
  GlobalObjectId ->
  Maybe StructuralCarrierRole ->
  ControlledRemovalDisposition ->
  ControlledRemovalSummary
emptySummary origin object role disposition =
  ControlledRemovalSummary
    origin
    object
    role
    disposition
    0
    0
    0
    0
    0
    0
    0
    0
    0

removalCause ::
  ControlledRemovalOrigin ->
  Either ControlledRemovalProblem StructuralConsequenceCause
removalCause origin =
  either (Left . ControlledRemovalCauseProblem) Right $ case origin of
    LabelControlledRemoval decision index -> labelReleaseCause decision index
    DisappearanceControlledRemoval authority ->
      predefinedDisappearanceCause
        (Disappearance.controlledRemovalAuthorityProbe authority)
        (Disappearance.controlledRemovalAuthorityResolveIndex authority)
    ImportedDisappearanceControlledRemoval probe index ->
      predefinedDisappearanceCause probe index

removalControlIndex :: ControlledRemovalOrigin -> ControlIndex
removalControlIndex origin = case origin of
  LabelControlledRemoval _ index -> index
  DisappearanceControlledRemoval authority ->
    Disappearance.controlledRemovalAuthorityResolveIndex authority
  ImportedDisappearanceControlledRemoval _ index -> index

retireNablaReservations ::
  Maybe StructuralCarrierRole ->
  GlobalObjectId ->
  Controlled.State ->
  (Controlled.State, Word64)
retireNablaReservations role object controlled =
  case role of
    Just NablaCarrier ->
      let (successor, cancelled) =
            Controlled.commitControlledNablaReservationRetirement
              ( Controlled.prepareControlledNablaReservationRetirement
                  (nablaIdFromGlobalObjectId object)
                  controlled
              )
       in (successor, fromIntegral (length cancelled))
    _ -> (controlled, 0)

applyStructuralRemoval ::
  StructuralConsequenceCause ->
  ControlIndex ->
  GlobalObjectId ->
  HeraldState ->
  Either
    ControlledRemovalProblem
    (HeraldState, EffectBatch, Word64, Word64, Word64, Word64, Word64)
applyStructuralRemoval cause prerequisite object state = do
  preparation <-
    either
      (Left . ControlledRemovalReconciliationProblem)
      Right
      ( Reconciliation.prepareStructuralLabelControlRefresh
          (structuralReconciliationViews state)
          cause
          object
          (releasedDeleted (maybe 0 releasedLabelStateGeneration (Controlled.controlledEffectiveLabelState object (startupControlledState state))))
          Nothing
          ( GraphProgress.structuralProgressReconciliation
              (startupStructuralProgressState state)
          )
      )
  case preparation of
    Reconciliation.StructuralHeld _ ->
      Left ControlledRemovalTopologyUnexpectedlyHeld
    Reconciliation.StructuralReady prepared -> do
      let graphPatch = Reconciliation.preparedStructuralGraphPatch prepared
          placementPatch = Reconciliation.preparedStructuralPlacementPatch prepared
          debtSet = Reconciliation.preparedStructuralDebts prepared
          graphChanges =
            fromIntegral
              ( Map.size (Reconciliation.structuralGraphVertexChanges graphPatch)
                  + Map.size (Reconciliation.structuralGraphEdgeChanges graphPatch)
              )
          placementChanges =
            fromIntegral
              (Map.size (Reconciliation.placementPatchChanges placementPatch))
          debts =
            fromIntegral
              (length (StructuralDebt.structuralDebtSetEntries debtSet))
      (withOwners, losses) <-
        applyPreparedStructuralRemoval cause prerequisite prepared
          $ case structuralConsequenceCauseView cause of
            LabelReleaseCauseView {} ->
              replaceStartupLabelPatchState
                (LabelPatch.recordLabelStructuralWork (LabelPatch.structuralLabelPreparationWork True prepared) (startupLabelPatchState state))
                state
            _ -> state
      (successor, effects) <-
        either
          (Left . ControlledRemovalAlignmentTransferProblem)
          Right
          ( AlignmentTransfer.deliverAlignmentLossTransferDelta
              state
              withOwners
          )
      let predecessorWaits =
            Set.fromList
              ( fmap
                  Wait.waitRegistrationId
                  (Wait.waitRegistrations (startupWaitState withOwners))
              )
          successorWaits =
            Set.fromList
              ( fmap
                  Wait.waitRegistrationId
                  (Wait.waitRegistrations (startupWaitState successor))
              )
          completedWaits =
            fromIntegral
              (Set.size (Set.difference predecessorWaits successorWaits))
      Right
        ( successor,
          effects,
          graphChanges,
          placementChanges,
          debts,
          losses,
          completedWaits
        )

applyPreparedStructuralRemoval ::
  StructuralConsequenceCause ->
  ControlIndex ->
  Reconciliation.PreparedStructuralReconciliation ->
  HeraldState ->
  Either ControlledRemovalProblem (HeraldState, Word64)
applyPreparedStructuralRemoval cause prerequisite prepared state = do
  preparedStore <-
    either
      (Left . ControlledRemovalStorePatchProblem)
      Right
      ( Store.prepareStructuralStorePatch
          cause
          (Reconciliation.preparedStructuralStorePatch prepared)
          (startupStoreState state)
      )
  preparedPlacement <-
    either
      (Left . ControlledRemovalPlacementPatchProblem)
      Right
      ( Placement.prepareStructuralPlacementPatch
          prerequisite
          (Reconciliation.preparedStructuralPlacementPatch prepared)
          (startupPlacementState state)
      )
  preparedProgress <-
    either
      (Left . ControlledRemovalProgressProblem)
      Right
      ( GraphProgress.prepareStructuralProjectionRefresh
          prepared
          (startupGraphState state)
          (startupStructuralProgressState state)
      )
  preparedAlignment <-
    either
      (Left . ControlledRemovalAlignmentProblem)
      Right
      ( Alignment.prepareStructuralDebtRetention
          cause
          (Reconciliation.preparedStructuralDebts prepared)
          (startupAlignmentState state)
      )
  let store = Store.commitStructuralStorePatch preparedStore
      (placement, _) = Placement.commitStructuralPlacementPatch preparedPlacement
      (graph, progress) =
        GraphProgress.commitStructuralProjectionRefresh preparedProgress
      (alignment, _) = Alignment.commitStructuralDebtRetention preparedAlignment
      successor =
        replaceStartupAlignmentState alignment
          . replaceStartupStructuralProgressState progress
          . replaceStartupGraphState graph
          . replaceStartupPlacementState placement
          . replaceStartupStoreState store
          $ state
  retainVanishedLocalPlacementLosses cause state successor

retainVanishedLocalPlacementLosses ::
  StructuralConsequenceCause ->
  HeraldState ->
  HeraldState ->
  Either ControlledRemovalProblem (HeraldState, Word64)
retainVanishedLocalPlacementLosses cause predecessor successor = do
  coordinator <-
    either
      (Left . ControlledRemovalAlignmentLossInvariant)
      Right
      ( AlignmentLoss.alignmentLossCoordinatorState
          local
          (startupAlignmentState successor)
      )
  afterLoss <- foldM applyOne coordinator (Set.toAscList vanished)
  Right
    ( replaceStartupAlignmentState
        (AlignmentLoss.alignmentLossCoordinatorOwner afterLoss)
        successor,
      fromIntegral (Set.size vanished)
    )
  where
    local = checkedLocalHeraldEpoch (startupGenesis successor)
    vanished =
      localPlacementStoreCoordinates local (startupPlacementState predecessor)
        Set.\\ localPlacementStoreCoordinates local (startupPlacementState successor)
    applyOne current lost = do
      prepared <-
        either
          (Left . ControlledRemovalAlignmentLossProblem)
          Right
          (AlignmentLoss.prepareAlignmentLoss cause lost current)
      Right (AlignmentLoss.commitAlignmentLoss prepared)

localPlacementStoreCoordinates ::
  HeraldEpoch ->
  Placement.State ->
  Set.Set AlignmentLoss.QualifiedStoreCoordinate
localPlacementStoreCoordinates local placement =
  Set.fromList
    [ AlignmentLoss.qualifiedStoreCoordinate
        local
        (Placement.localPlacementDelta retained)
        (Placement.localPlacementStoreIncarnation retained)
    | retained <- Placement.localPlacements placement
    ]

applyStorePurge ::
  StructuralConsequenceCause ->
  Maybe Controlled.ControlledRemovalPayloadCoordinate ->
  HeraldState ->
  Either ControlledRemovalProblem (HeraldState, Word64)
applyStorePurge _ Nothing state = Right (state, 0)
applyStorePurge cause (Just payload) state = do
  prepared <-
    either
      (Left . ControlledRemovalStorePurgeProblem)
      Right
      ( Store.prepareControlledObjectPurge
          cause
          (Controlled.controlledRemovalPayloadSortId payload)
          (Controlled.controlledRemovalPayloadObjectKey payload)
          (startupStoreState state)
      )
  let (store, summary) = Store.commitControlledObjectPurge prepared
  Right
    ( replaceStartupStoreState store state,
      Store.controlledObjectPurgeSummaryPurgedFactCount summary
    )

structuralReconciliationViews ::
  HeraldState -> Reconciliation.ReconciliationViews
structuralReconciliationViews state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses
      ( Map.fromList
          [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
          | (process, ended) <- OracleProjection.projectedEndedProcesses oracle,
            Just residence <- [OracleProjection.oracleViewProcessResidence process oracleView]
          ]
      )
    $ Reconciliation.reconciliationViews
      (OracleProjection.oracleViewLocalHeraldEpoch oracleView)
      ( Map.fromList
          [ ( SortRegistry.registryEntrySortId entry,
              StructuralDebt.sortOccurrence
                (SortRegistry.registryEntrySortId entry)
                (SortRegistry.registryEntryOccurrenceId entry)
            )
          | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
          ]
      )
      (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
      ( Map.fromList
          [ (process, residence)
          | process <- OracleProjection.projectedProcessEpochs oracle,
            OracleProjection.oracleViewProcessIsLive process oracleView,
            Just residence <- [OracleProjection.oracleViewProcessResidence process oracleView]
          ]
      )
      ( Set.fromList
          ( Controlled.controlledNormalPossessionEntries
              (startupControlledState state)
          )
      )
  where
    oracle = startupOracleProjectionState state
    oracleView = OracleProjection.oracleView oracle

commitControlledRemoval ::
  PreparedControlledRemoval ->
  (HeraldState, EffectBatch, ControlledRemovalSummary)
commitControlledRemoval
  (PreparedControlledRemoval state effects summary) =
    (state, effects, summary)
