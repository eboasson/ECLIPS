{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Shared historical Nabla-authority resolution.
--
-- Authority is reconstructed at the publication's exact retained
-- topology/control coordinate.  Monotone current deletion and controller-End
-- facts are checked separately so a later handoff cannot reinterpret delayed
-- pre-fence work.
module Eclips.Herald.Authority
  ( NablaAuthorityResolution,
    NablaAuthorityProblem (..),
    resolveNablaAuthorityAt,
    resolveCurrentNablaAuthority,
    resolveInitialNablaAuthority,
    resolvePreparedCurrentNablaAuthority,
    nablaAuthorityResolutionNabla,
    nablaAuthorityResolutionController,
    nablaAuthorityResolutionResidence,
    nablaAuthorityResolutionAuthority,
    nablaAuthorityResolutionSort,
    nablaAuthorityResolutionSourceTopology,
    nablaAuthorityResolutionControlPrefix,
    nablaAuthorityResolutionMinimumControlPrerequisite,
    CurrentAuthorityApplicabilityProblem (..),
    checkCurrentAuthorityApplicability,
  )
where

import Control.Monad (unless)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (..),
    ControlIndex,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    TopologyCutId,
    authorityEpochView,
    genesisAuthorityEpoch,
    globalObjectIdFromNablaId,
    structuralAuthorityEpoch,
  )
import Eclips.Domain.Label
  ( LabelRecord,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    labelRecordReleasedState,
    labelRecordRetainedAuthority,
    labelRecordRevision,
    labelRevisionControlIndex,
    releasedLabelStateView,
  )
import Eclips.Domain.Value (LabelOwner (..))
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Structural.Debt (SortOccurrence, sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Oracle.Label
  ( ReleasedLabelOverlay,
    releasedLabelOverlayRetainedAuthority,
    releasedLabelOverlayRevision,
    releasedLabelOverlayState,
  )

data NablaAuthorityResolution = NablaAuthorityResolution
  { nabla :: NablaId,
    controller :: ProcessEpochId,
    residence :: HeraldEpoch,
    authority :: AuthorityEpoch,
    sort :: SortOccurrence,
    sourceTopology :: TopologyCutId,
    controlPrefix :: ControlIndex,
    minimumControlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

data NablaAuthorityProblem
  = NablaAuthorityStructuralProblem GraphProgress.StructuralProgressProblem
  | NablaAuthorityNablaUnavailable NablaId
  | NablaAuthorityPublishedBaselineUnsupported NablaId
  | NablaAuthorityStructuralTenureUnavailable NablaId
  | NablaAuthorityControlPrecedesOrigin ControlIndex ControlIndex
  | NablaAuthorityReleasedDeleted NablaId
  | NablaAuthorityReleasedVoid NablaId
  | NablaAuthorityReleasedZombie NablaId ProcessEpochId
  | NablaAuthorityControllerUnavailable NablaId
  | NablaAuthorityControllerMismatch NablaId ProcessEpochId ProcessEpochId
  | NablaAuthorityControllerResidenceMismatch
      NablaId
      ProcessEpochId
      HeraldEpoch
      HeraldEpoch
  | NablaAuthorityControllerEnded NablaId ProcessEpochId
  | NablaAuthorityReleasedAuthorityMissing NablaId
  | NablaAuthorityProjectedProvenanceMismatch
      NablaId
      AuthorityEpoch
      AuthorityEpoch
  | NablaAuthorityClaimMismatch NablaId AuthorityEpoch AuthorityEpoch
  deriving stock (Eq, Show)

-- | Resolve and authenticate one claimed authority at its historical source
-- coordinate.  The Oracle projection is consulted at the same bounded prefix,
-- never through its current-only view.
resolveNablaAuthorityAt ::
  NablaId ->
  AuthorityEpoch ->
  TopologyCutId ->
  ControlIndex ->
  GraphProgress.StructuralProgressState ->
  OracleProjection.State ->
  Either NablaAuthorityProblem NablaAuthorityResolution
resolveNablaAuthorityAt nabla claimed sourceTopology controlPrefix progress oracle = do
  let object = globalObjectIdFromNablaId nabla
      released =
        OracleProjection.oracleProjectionReleasedLabelAt
          controlPrefix
          object
          oracle
  case OracleProjection.oracleProjectionControlledDisappearanceAt controlPrefix object oracle of
    Just _ -> Left (NablaAuthorityReleasedDeleted nabla)
    Nothing -> Right ()
  case releasedLabelOverlayState <$> released of
    Just terminal
      | ReleasedDeletedView {} <- releasedLabelStateView terminal ->
          Left (NablaAuthorityReleasedDeleted nabla)
    _ -> Right ()
  projection <- historicalProjection nabla sourceTopology controlPrefix progress
  base <- baseAuthority nabla projection progress
  (controller, residence, expected, releasePrerequisite) <-
    resolveHistoricalController
      nabla
      base
      projection
      released
      controlPrefix
      oracle
  let originPrerequisite =
        Reconciliation.historicalNablaProjectionControlPrerequisite projection
      minimumPrerequisite = max originPrerequisite releasePrerequisite
  unless
    (controlPrefix >= minimumPrerequisite)
    (Left (NablaAuthorityControlPrecedesOrigin minimumPrerequisite controlPrefix))
  unless
    (claimed == expected)
    (Left (NablaAuthorityClaimMismatch nabla expected claimed))
  Right
    NablaAuthorityResolution
      { nabla,
        controller,
        residence,
        authority = expected,
        sort = Reconciliation.historicalNablaProjectionSort projection,
        sourceTopology,
        controlPrefix,
        minimumControlPrerequisite = minimumPrerequisite
      }

-- | Resolve immutable origin tenure for first-overlay evidence. Checked
-- bootstrap roots retain their index-zero provenance directly in Controlled;
-- dynamic Nablas obtain it from Graph.Progress. The current label and controller
-- lifecycle do not change this evidence.
resolveInitialNablaAuthority ::
  NablaId ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  Either NablaAuthorityProblem AuthorityEpoch
resolveInitialNablaAuthority nabla progress controlled =
  case Controlled.controlledWriterFact nabla controlled of
    Just root -> Right (Controlled.rootFactAuthority root)
    Nothing -> do
      projection <-
        historicalProjection
          nabla
          (GraphProgress.structuralLastInstalledCutId progress)
          (GraphProgress.structuralAppliedControlPrefix progress)
          progress
      baseAuthority nabla projection progress

-- | Resolve the live operation authority separately from the immutable initial
-- tenure used as first-overlay evidence. Initial evidence remains meaningful
-- for an endpoint whose controller is Void, Zombie, or canonically ended.
resolveCurrentNablaAuthority ::
  NablaId ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  Either NablaAuthorityProblem NablaAuthorityResolution
resolveCurrentNablaAuthority nabla progress controlled =
  resolveCurrentNablaAuthorityWith
    (historicalProjection nabla (GraphProgress.structuralLastInstalledCutId progress) (GraphProgress.structuralAppliedControlPrefix progress) progress)
    nabla
    progress
    controlled

-- | The same checked resolution with historical reconstruction shared by a
-- batch. Bootstrap roots never demand the prepared historical coordinate.
resolvePreparedCurrentNablaAuthority ::
  NablaId ->
  GraphProgress.PreparedStructuralQueries ->
  Controlled.State ->
  Either NablaAuthorityProblem NablaAuthorityResolution
resolvePreparedCurrentNablaAuthority nabla queries =
  resolveCurrentNablaAuthorityWith
    ( either
        (Left . NablaAuthorityStructuralProblem)
        (maybe (Left (NablaAuthorityNablaUnavailable nabla)) Right)
        (GraphProgress.lookupPreparedCurrentNablaProjection nabla queries)
    )
    nabla
    (GraphProgress.preparedStructuralProgress queries)

resolveCurrentNablaAuthorityWith ::
  Either NablaAuthorityProblem Reconciliation.HistoricalNablaProjection ->
  NablaId ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  Either NablaAuthorityProblem NablaAuthorityResolution
resolveCurrentNablaAuthorityWith preparedProjection nabla progress controlled = do
  let sourceTopology = GraphProgress.structuralLastInstalledCutId progress
      controlPrefix = GraphProgress.structuralAppliedControlPrefix progress
      object = globalObjectIdFromNablaId nabla
  releasedState <-
    maybe
      (Left (NablaAuthorityControllerUnavailable nabla))
      Right
      (Controlled.controlledEffectiveLabelState object controlled)
  case releasedLabelStateView releasedState of
    ReleasedDeletedView {} -> Left (NablaAuthorityReleasedDeleted nabla)
    _ -> Right ()
  (controller, residence, base, projectedSort, originPrerequisite) <-
    case Controlled.controlledWriterFact nabla controlled of
      Just root -> do
        controller <- applicableController nabla releasedState
        process <-
          maybe
            (Left (NablaAuthorityControllerUnavailable nabla))
            Right
            (Controlled.controlledProcessFact controller controlled)
        Right
          ( controller,
            Controlled.processFactResidence process,
            Controlled.rootFactAuthority root,
            sortOccurrence
              (Controlled.rootFactSortId root)
              (Controlled.rootFactOccurrenceId root),
            Controlled.rootFactControlPrerequisite root
          )
      Nothing -> do
        projection <- preparedProjection
        base <- baseAuthority nabla projection progress
        (controller, residence) <- liveProjectedController nabla projection
        releasedController <- applicableController nabla releasedState
        unless
          (releasedController == controller)
          (Left (NablaAuthorityControllerMismatch nabla controller releasedController))
        Right
          ( controller,
            residence,
            base,
            Reconciliation.historicalNablaProjectionSort projection,
            Reconciliation.historicalNablaProjectionControlPrerequisite projection
          )
  unless
    (not (Controlled.controlledProcessEnded controller controlled))
    (Left (NablaAuthorityControllerEnded nabla controller))
  (expected, releasePrerequisite) <-
    case Controlled.controlledReleasedLabelRecord object controlled of
      Nothing -> Right (base, originPrerequisite)
      Just record -> authorityFromCurrentRecord nabla base record
  let minimumPrerequisite = max originPrerequisite releasePrerequisite
  Right
    NablaAuthorityResolution
      { nabla,
        controller,
        residence,
        authority = expected,
        sort = projectedSort,
        sourceTopology,
        controlPrefix,
        minimumControlPrerequisite = minimumPrerequisite
      }

historicalProjection ::
  NablaId ->
  TopologyCutId ->
  ControlIndex ->
  GraphProgress.StructuralProgressState ->
  Either NablaAuthorityProblem Reconciliation.HistoricalNablaProjection
historicalProjection nabla sourceTopology controlPrefix progress =
  either
    (Left . NablaAuthorityStructuralProblem)
    (maybe (Left (NablaAuthorityNablaUnavailable nabla)) Right)
    ( GraphProgress.structuralNablaProjectionAtCoordinate
        nabla
        sourceTopology
        controlPrefix
        progress
    )

baseAuthority ::
  NablaId ->
  Reconciliation.HistoricalNablaProjection ->
  GraphProgress.StructuralProgressState ->
  Either NablaAuthorityProblem AuthorityEpoch
baseAuthority nabla projection progress =
  case Reconciliation.historicalNablaProjectionProvenance projection of
    Reconciliation.GenesisStructuralProvenance _ -> Right genesisAuthorityEpoch
    Reconciliation.BaselineStructuralProvenance _ ->
      Left (NablaAuthorityPublishedBaselineUnsupported nabla)
    Reconciliation.OccurrenceStructuralProvenance occurrence _ -> do
      cut <-
        maybe
          (Left (NablaAuthorityStructuralTenureUnavailable nabla))
          Right
          (GraphProgress.installedCutCoveringOccurrence occurrence progress)
      Right (structuralAuthorityEpoch occurrence cut)

resolveHistoricalController ::
  NablaId ->
  AuthorityEpoch ->
  Reconciliation.HistoricalNablaProjection ->
  Maybe ReleasedLabelOverlay ->
  ControlIndex ->
  OracleProjection.State ->
  Either
    NablaAuthorityProblem
    (ProcessEpochId, HeraldEpoch, AuthorityEpoch, ControlIndex)
resolveHistoricalController nabla base projection released controlPrefix oracle = do
  (controller, projectedResidence) <- liveProjectedController nabla projection
  (releasedController, expected, releasePrerequisite) <-
    case released of
      Nothing -> Right (controller, base, controlPrefixFloor)
      Just overlay -> authorityFromHistoricalOverlay nabla base overlay
  unless
    (releasedController == controller)
    (Left (NablaAuthorityControllerMismatch nabla controller releasedController))
  residence <-
    maybe
      (Left (NablaAuthorityControllerUnavailable nabla))
      Right
      ( OracleProjection.oracleProjectionProcessResidenceAt
          controlPrefix
          controller
          oracle
      )
  unless
    (residence == projectedResidence)
    ( Left
        ( NablaAuthorityControllerResidenceMismatch
            nabla
            controller
            projectedResidence
            residence
        )
    )
  unless
    (OracleProjection.oracleProjectionProcessIsLiveAt controlPrefix controller oracle)
    (Left (NablaAuthorityControllerEnded nabla controller))
  Right (controller, residence, expected, releasePrerequisite)
  where
    controlPrefixFloor =
      Reconciliation.historicalNablaProjectionControlPrerequisite projection

liveProjectedController ::
  NablaId ->
  Reconciliation.HistoricalNablaProjection ->
  Either NablaAuthorityProblem (ProcessEpochId, HeraldEpoch)
liveProjectedController nabla projection =
  case Reconciliation.historicalNablaProjectionController projection of
    Reconciliation.LiveProcessController process residence -> Right (process, residence)
    Reconciliation.VoidController -> Left (NablaAuthorityReleasedVoid nabla)
    Reconciliation.ZombieProcessController process ->
      Left (NablaAuthorityControllerEnded nabla process)

authorityFromHistoricalOverlay ::
  NablaId ->
  AuthorityEpoch ->
  ReleasedLabelOverlay ->
  Either NablaAuthorityProblem (ProcessEpochId, AuthorityEpoch, ControlIndex)
authorityFromHistoricalOverlay nabla base overlay = do
  controller <- applicableController nabla (releasedLabelOverlayState overlay)
  authority <-
    maybe
      (Left (NablaAuthorityReleasedAuthorityMissing nabla))
      Right
      (releasedLabelOverlayRetainedAuthority overlay)
  validateReleasedProvenance nabla base authority
  Right
    ( controller,
      authority,
      labelRevisionControlIndex (releasedLabelOverlayRevision overlay)
    )

authorityFromCurrentRecord ::
  NablaId ->
  AuthorityEpoch ->
  LabelRecord ->
  Either NablaAuthorityProblem (AuthorityEpoch, ControlIndex)
authorityFromCurrentRecord nabla base record = do
  _ <- applicableController nabla (labelRecordReleasedState record)
  authority <-
    maybe
      (Left (NablaAuthorityReleasedAuthorityMissing nabla))
      Right
      (labelRecordRetainedAuthority record)
  validateReleasedProvenance nabla base authority
  Right (authority, labelRevisionControlIndex (labelRecordRevision record))

validateReleasedProvenance ::
  NablaId ->
  AuthorityEpoch ->
  AuthorityEpoch ->
  Either NablaAuthorityProblem ()
validateReleasedProvenance nabla base observed =
  case authorityEpochView observed of
    LabelAuthorityEpochView _ -> Right ()
    _ ->
      unless
        (observed == base)
        (Left (NablaAuthorityProjectedProvenanceMismatch nabla base observed))

applicableController ::
  NablaId -> ReleasedLabelState -> Either NablaAuthorityProblem ProcessEpochId
applicableController nabla released =
  case releasedLabelStateView released of
    ReleasedDeletedView {} -> Left (NablaAuthorityReleasedDeleted nabla)
    ReleasedLabelView (VoidLabel, _) -> Left (NablaAuthorityReleasedVoid nabla)
    ReleasedLabelView (ZombieLabel process, _) ->
      Left (NablaAuthorityReleasedZombie nabla process)
    ReleasedLabelView (ProcessLabel process, _) -> Right process

nablaAuthorityResolutionNabla :: NablaAuthorityResolution -> NablaId
nablaAuthorityResolutionNabla resolution = resolution.nabla

nablaAuthorityResolutionController ::
  NablaAuthorityResolution -> ProcessEpochId
nablaAuthorityResolutionController resolution = resolution.controller

nablaAuthorityResolutionResidence :: NablaAuthorityResolution -> HeraldEpoch
nablaAuthorityResolutionResidence resolution = resolution.residence

nablaAuthorityResolutionAuthority ::
  NablaAuthorityResolution -> AuthorityEpoch
nablaAuthorityResolutionAuthority resolution = resolution.authority

nablaAuthorityResolutionSort :: NablaAuthorityResolution -> SortOccurrence
nablaAuthorityResolutionSort resolution = resolution.sort

nablaAuthorityResolutionSourceTopology ::
  NablaAuthorityResolution -> TopologyCutId
nablaAuthorityResolutionSourceTopology resolution = resolution.sourceTopology

nablaAuthorityResolutionControlPrefix ::
  NablaAuthorityResolution -> ControlIndex
nablaAuthorityResolutionControlPrefix resolution = resolution.controlPrefix

nablaAuthorityResolutionMinimumControlPrerequisite ::
  NablaAuthorityResolution -> ControlIndex
nablaAuthorityResolutionMinimumControlPrerequisite resolution =
  resolution.minimumControlPrerequisite

data CurrentAuthorityApplicabilityProblem
  = CurrentAuthorityDeleted NablaId
  | CurrentAuthorityControllerEnded NablaId ProcessEpochId
  deriving stock (Eq, Show)

checkCurrentAuthorityApplicability ::
  NablaAuthorityResolution ->
  Controlled.State ->
  Either CurrentAuthorityApplicabilityProblem ()
checkCurrentAuthorityApplicability resolution current = do
  case Controlled.controlledEffectiveLabelState
    (globalObjectIdFromNablaId resolution.nabla)
    current of
    Just released
      | ReleasedDeletedView {} <- releasedLabelStateView released ->
          Left (CurrentAuthorityDeleted resolution.nabla)
    _ -> Right ()
  if Controlled.controlledProcessEnded resolution.controller current
    then
      Left
        ( CurrentAuthorityControllerEnded
            resolution.nabla
            resolution.controller
        )
    else Right ()
