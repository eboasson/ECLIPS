{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared checked controlled-operation capabilities.
--
-- Genesis roots retain bootstrap RootFacts. Dynamic readers and writers instead
-- become operable only after their structural occurrence is covered by an
-- installed topology cut. These resolvers present both paths as opaque exact
-- bindings without fabricating bootstrap facts.
module Eclips.Herald.Controlled.Operate
  ( CheckedControlledOperate,
    ControlledOperateError (..),
    checkControlledOperate,
    PreparedControlledQueries,
    prepareControlledQueries,
    checkPreparedControlledOperate,
    checkPreparedControlledRead,
    controlledOperateProcess,
    controlledOperateWriter,
    controlledOperateSequencingObject,
    controlledOperateSortId,
    controlledOperateOccurrenceId,
    controlledOperateAuthority,
    controlledOperateControlPrerequisite,
    controlledOperateWriterBinding,
    CheckedControlledRead,
    ControlledReadError (..),
    checkControlledRead,
    controlledReadSortId,
    controlledReadOccurrenceId,
    controlledReadHeraldEpoch,
  )
where

import Control.Monad (unless)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (NablaSequencedBy, UnsequencedNabla),
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
  )
import Eclips.Domain.Label
  ( ReleasedLabelStateView (ReleasedLabelView),
    releasedLabelStateView,
  )
import Eclips.Domain.Publication (checkedPublicationId, checkedPublicationValue)
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, NablaCarrier),
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel),
    ValueView (OptionalGlobalUniqueIdValue),
    directProjection,
    mkFieldName,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Structural.Debt
  ( sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation
  ( ControllerProjection (LiveProcessController),
  )

data CheckedControlledOperate
  = CheckedControlledOperate !Controlled.ControlledWriterBinding !(Maybe GlobalObjectId)

data ControlledOperateError
  = ControlledOperateBootstrapError
      Controlled.ControlledBootstrapOperateError
  | ControlledOperateDynamicWriterUnavailable NablaId
  | ControlledOperateDynamicControllerMismatch
      ProcessEpochId
      ProcessEpochId
  | ControlledOperateDynamicResidenceMismatch
  | ControlledOperateDynamicWriterNotPossessed ProcessEpochId NablaId
  | ControlledOperateDynamicRecordUnavailable NablaId
  | ControlledOperateDynamicRecordNotCurrent NablaId
  | ControlledOperateDynamicRecordLabelMismatch NablaId
  | ControlledOperateDynamicRecordRoleMismatch NablaId
  | ControlledOperateDynamicRecordPublicationMismatch
      NablaId
      PublicationId
      PublicationId
  | ControlledOperateDynamicRecordSortMismatch NablaId SortId SortId
  | ControlledOperateDynamicRecordOccurrenceMismatch
      NablaId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | ControlledOperateDynamicOccurrenceUnavailable NablaId
  | ControlledOperateDynamicRecordSequencingShapeMismatch NablaId
  deriving stock (Eq, Show)

-- | Opaque authority for one exact bootstrap or installed dynamic reader.
-- Store, placement, and registry owners remain independently checked by the
-- application-call coordinator against these facts.
data CheckedControlledRead
  = CheckedControlledRead SortId SortDefinitionOccurrenceId HeraldEpoch

data ControlledReadError
  = ControlledReadRoleMismatch DeltaId
  | ControlledReadOperateNotPermitted ProcessEpochId DeltaId
  | ControlledReadInvariantContradiction DeltaId
  deriving stock (Eq, Show)

-- | A batch binds its owner inputs once, so prepared structural answers can
-- never be combined with another Controlled or Graph.Progress state. Retain
-- this value only while those inputs are unchanged.
data PreparedControlledQueries
  = PreparedControlledQueries
      Controlled.State
      GraphProgress.PreparedStructuralQueries

prepareControlledQueries ::
  Controlled.State -> Graph.State -> GraphProgress.StructuralProgressState -> PreparedControlledQueries
prepareControlledQueries controlled graph progress =
  PreparedControlledQueries controlled (GraphProgress.prepareStructuralQueries graph progress)

checkPreparedControlledOperate ::
  ProcessEpochId -> NablaId -> PreparedControlledQueries -> Either ControlledOperateError CheckedControlledOperate
checkPreparedControlledOperate process writer (PreparedControlledQueries controlled queries) =
  checkControlledOperateWith
    (GraphProgress.lookupPreparedDynamicNabla writer queries)
    (Authority.resolvePreparedCurrentNablaAuthority writer queries controlled)
    process
    writer
    controlled
    (GraphProgress.preparedStructuralProgress queries)

checkPreparedControlledRead ::
  ProcessEpochId -> DeltaId -> PreparedControlledQueries -> Either ControlledReadError CheckedControlledRead
checkPreparedControlledRead process reader (PreparedControlledQueries controlled queries) =
  checkControlledReadWith
    (GraphProgress.lookupPreparedDynamicDelta reader queries)
    process
    reader
    controlled
    (GraphProgress.preparedStructuralProgress queries)

-- | Resolve a writer against the two disjoint authority sources. Dynamic
-- authority is revalidated against the current structural projection, so an
-- End or label-control refresh that replaces the live controller immediately
-- prevents the former process from operating the writer.
checkControlledOperate ::
  ProcessEpochId ->
  NablaId ->
  Controlled.State ->
  GraphProgress.StructuralProgressState ->
  Either ControlledOperateError CheckedControlledOperate
checkControlledOperate process writer controlled progress =
  checkControlledOperateWith
    (GraphProgress.lookupInstalledDynamicNabla writer progress)
    (Authority.resolveCurrentNablaAuthority writer progress controlled)
    process
    writer
    controlled
    progress

checkControlledOperateWith ::
  Maybe GraphProgress.DynamicVertexInstallation ->
  Either Authority.NablaAuthorityProblem Authority.NablaAuthorityResolution ->
  ProcessEpochId ->
  NablaId ->
  Controlled.State ->
  GraphProgress.StructuralProgressState ->
  Either ControlledOperateError CheckedControlledOperate
checkControlledOperateWith installedVertex authority process writer controlled progress =
  case Controlled.controlledWriterFact writer controlled of
    Just root -> resolveBootstrap root
    Nothing -> case installedVertex of
      Just installation -> resolveDynamic installation
      Nothing -> resolveUnavailableBootstrap
  where
    -- The requested writer, not its owning process, selects the authority
    -- namespace. A bootstrap process may later publish and operate a dynamic
    -- Nabla. If neither exact writer authority exists, falling through the
    -- bootstrap resolver preserves its precise unavailable classification.

    resolveUnavailableBootstrap = do
      bootstrap <-
        mapLeft
          ControlledOperateBootstrapError
          (Controlled.checkControlledBootstrapOperate process writer controlled)
      Right
        ( CheckedControlledOperate
            (Controlled.controlledBootstrapOperateWriterBinding bootstrap)
            (sequencingObject (Controlled.controlledBootstrapOperateWriterSequencing bootstrap))
        )

    resolveBootstrap root = do
      resolution <- resolveAuthority
      let controller = Authority.nablaAuthorityResolutionController resolution
      unless
        (controller == process)
        ( Left
            ( ControlledOperateBootstrapError
                ( Controlled.ControlledBootstrapOperateWriterProcessMismatch
                    process
                    controller
                )
            )
        )
      _ <-
        maybe
          ( Left
              ( ControlledOperateBootstrapError
                  (Controlled.ControlledBootstrapOperateProcessUnavailable process)
              )
          )
          Right
          (Controlled.controlledProcessFact process controlled)
      unless
        (Controlled.controlledHasAnyRole (globalObjectIdFromProcessEpochId process) controlled)
        ( Left
            ( ControlledOperateBootstrapError
                (Controlled.ControlledBootstrapOperateProcessRoleContradiction process)
            )
        )
      unless
        (Controlled.controlledHasNormalPossession process (globalObjectIdFromProcessEpochId process) controlled)
        ( Left
            ( ControlledOperateBootstrapError
                (Controlled.ControlledBootstrapOperateProcessNotPossessed process)
            )
        )
      unless
        (Controlled.controlledHasNormalPossession process (globalObjectIdFromNablaId writer) controlled)
        ( Left
            ( ControlledOperateBootstrapError
                (Controlled.ControlledBootstrapOperateWriterNotPossessed process writer)
            )
        )
      Right
        ( CheckedControlledOperate
            ( Controlled.controlledWriterBinding
                process
                writer
                (Controlled.rootFactSortId root)
                (Controlled.rootFactOccurrenceId root)
                (Authority.nablaAuthorityResolutionAuthority resolution)
                (Authority.nablaAuthorityResolutionMinimumControlPrerequisite resolution)
            )
            ( case Controlled.rootFactWriterSequencing root of
                Just sequencing -> sequencingObject sequencing
                Nothing -> error "checked bootstrap writer carried a reader root"
            )
        )

    resolveDynamic installation = do
      resolution <- resolveAuthority
      let controller = Authority.nablaAuthorityResolutionController resolution
          residence = Authority.nablaAuthorityResolutionResidence resolution
      unless
        (controller == process)
        ( Left
            ( ControlledOperateDynamicControllerMismatch
                process
                controller
            )
        )
      unless
        (residence == GraphProgress.structuralProgressLocalHerald progress)
        (Left ControlledOperateDynamicResidenceMismatch)
      unless
        ( Controlled.controlledHasNormalPossession
            process
            (globalObjectIdFromNablaId writer)
            controlled
        )
        (Left (ControlledOperateDynamicWriterNotPossessed process writer))
      record <-
        maybe
          (Left (ControlledOperateDynamicRecordUnavailable writer))
          Right
          ( Controlled.controlledLocalRecord
              (globalObjectIdFromNablaId writer)
              controlled
          )
      unless
        (Controlled.controlledRecordLifecycle record == Controlled.ControlledCurrent)
        (Left (ControlledOperateDynamicRecordNotCurrent writer))
      unless
        (recordEffectivelyControlledBy process record controlled)
        (Left (ControlledOperateDynamicRecordLabelMismatch writer))
      unless
        (Controlled.controlledRecordStructuralRole record == Just NablaCarrier)
        (Left (ControlledOperateDynamicRecordRoleMismatch writer))
      let retainedPublication =
            checkedPublicationId (Controlled.controlledRecordLatestPublication record)
          installedPublication =
            GraphProgress.dynamicVertexInstallationPublication installation
      unless
        (retainedPublication == installedPublication)
        ( Left
            ( ControlledOperateDynamicRecordPublicationMismatch
                writer
                installedPublication
                retainedPublication
            )
        )
      let structuralOccurrence =
            GraphProgress.dynamicVertexInstallationOccurrence installation
      applied <-
        maybe
          (Left (ControlledOperateDynamicOccurrenceUnavailable writer))
          Right
          (GraphProgress.lookupAppliedStructuralOccurrence structuralOccurrence progress)
      let carrierSort = GraphProgress.appliedStructuralOccurrenceCarrierSort applied
          carrierSortId = sortOccurrenceSortId carrierSort
          carrierOccurrenceId = sortOccurrenceDefinition carrierSort
      unless
        (Controlled.controlledRecordSortId record == carrierSortId)
        ( Left
            ( ControlledOperateDynamicRecordSortMismatch
                writer
                carrierSortId
                (Controlled.controlledRecordSortId record)
            )
        )
      unless
        (Controlled.controlledRecordOccurrenceId record == carrierOccurrenceId)
        ( Left
            ( ControlledOperateDynamicRecordOccurrenceMismatch
                writer
                carrierOccurrenceId
                (Controlled.controlledRecordOccurrenceId record)
            )
        )
      let binding =
            Controlled.controlledWriterBinding
              process
              writer
              (sortOccurrenceSortId (Authority.nablaAuthorityResolutionSort resolution))
              (sortOccurrenceDefinition (Authority.nablaAuthorityResolutionSort resolution))
              (Authority.nablaAuthorityResolutionAuthority resolution)
              (Authority.nablaAuthorityResolutionMinimumControlPrerequisite resolution)
      sequencing <- case valueAt
        (directProjection sequencingObjectField)
        (checkedPublicationValue (Controlled.controlledRecordLatestPublication record)) of
        Right value -> case viewValue value of
          OptionalGlobalUniqueIdValue object -> Right (globalObjectIdFromGlobalUniqueId <$> object)
          _ -> sequencingShapeMismatch
        Left _ -> sequencingShapeMismatch
      Right (CheckedControlledOperate binding sequencing)
      where
        sequencingShapeMismatch = Left (ControlledOperateDynamicRecordSequencingShapeMismatch writer)

    resolveAuthority =
      mapLeft
        authorityOperateError
        authority

    authorityOperateError problem = case problem of
      Authority.NablaAuthorityNablaUnavailable _ ->
        ControlledOperateDynamicWriterUnavailable writer
      Authority.NablaAuthorityControllerMismatch _ expected observed ->
        ControlledOperateDynamicControllerMismatch expected observed
      Authority.NablaAuthorityControllerResidenceMismatch {} ->
        ControlledOperateDynamicResidenceMismatch
      Authority.NablaAuthorityReleasedDeleted _ ->
        ControlledOperateDynamicRecordNotCurrent writer
      Authority.NablaAuthorityReleasedVoid _ ->
        ControlledOperateDynamicRecordLabelMismatch writer
      Authority.NablaAuthorityReleasedZombie {} ->
        ControlledOperateDynamicRecordLabelMismatch writer
      Authority.NablaAuthorityControllerEnded {} ->
        ControlledOperateDynamicRecordLabelMismatch writer
      _ -> ControlledOperateDynamicWriterUnavailable writer

-- | Resolve an exact reader without selecting its authority namespace from the
-- owning process. A bootstrap process may publish a dynamic Delta later in the
-- same epoch, so only an exact retained ReaderFact selects the bootstrap path.
checkControlledRead ::
  ProcessEpochId ->
  DeltaId ->
  Controlled.State ->
  GraphProgress.StructuralProgressState ->
  Either ControlledReadError CheckedControlledRead
checkControlledRead process reader controlled progress =
  checkControlledReadWith
    (GraphProgress.lookupInstalledDynamicDelta reader progress)
    process
    reader
    controlled
    progress

checkControlledReadWith ::
  Maybe GraphProgress.DynamicVertexInstallation ->
  ProcessEpochId ->
  DeltaId ->
  Controlled.State ->
  GraphProgress.StructuralProgressState ->
  Either ControlledReadError CheckedControlledRead
checkControlledReadWith installedVertex process reader controlled progress =
  case Controlled.controlledReaderFact reader controlled of
    Just fact -> resolveBootstrap fact
    Nothing ->
      case installedVertex of
        Nothing -> roleMismatch
        Just installation -> resolveDynamic installation
  where
    localHerald = GraphProgress.structuralProgressLocalHerald progress
    roleMismatch :: Either ControlledReadError value
    roleMismatch = Left (ControlledReadRoleMismatch reader)
    operateNotPermitted :: Either ControlledReadError value
    operateNotPermitted =
      Left (ControlledReadOperateNotPermitted process reader)
    contradiction :: Either ControlledReadError value
    contradiction = Left (ControlledReadInvariantContradiction reader)

    resolveBootstrap fact = do
      unless (Controlled.rootFactProcessEpoch fact == process) roleMismatch
      requirePossession
      Right
        ( CheckedControlledRead
            (Controlled.rootFactSortId fact)
            (Controlled.rootFactOccurrenceId fact)
            localHerald
        )

    resolveDynamic installation = do
      case GraphProgress.dynamicVertexInstallationController installation of
        LiveProcessController controller residence
          | controller == process && residence == localHerald -> Right ()
          | otherwise -> operateNotPermitted
        _ -> operateNotPermitted
      requirePossession
      record <-
        maybe
          contradiction
          Right
          ( Controlled.controlledLocalRecord
              (globalObjectIdFromDeltaId reader)
              controlled
          )
      unless
        ( Controlled.controlledRecordLifecycle record == Controlled.ControlledCurrent
            && recordEffectivelyControlledBy process record controlled
            && Controlled.controlledRecordStructuralRole record == Just DeltaCarrier
            && checkedPublicationId (Controlled.controlledRecordLatestPublication record)
              == GraphProgress.dynamicVertexInstallationPublication installation
        )
        contradiction
      applied <-
        maybe
          contradiction
          Right
          ( GraphProgress.lookupAppliedStructuralOccurrence
              (GraphProgress.dynamicVertexInstallationOccurrence installation)
              progress
          )
      let carrierSort = GraphProgress.appliedStructuralOccurrenceCarrierSort applied
      unless
        ( Controlled.controlledRecordSortId record
            == sortOccurrenceSortId carrierSort
            && Controlled.controlledRecordOccurrenceId record
              == sortOccurrenceDefinition carrierSort
        )
        contradiction
      let installedSort = GraphProgress.dynamicVertexInstallationSort installation
      Right
        ( CheckedControlledRead
            (sortOccurrenceSortId installedSort)
            (sortOccurrenceDefinition installedSort)
            localHerald
        )

    requirePossession :: Either ControlledReadError ()
    requirePossession =
      unless
        ( Controlled.controlledHasNormalPossession
            process
            (globalObjectIdFromDeltaId reader)
            controlled
        )
        operateNotPermitted

recordEffectivelyControlledBy ::
  ProcessEpochId ->
  Controlled.ControlledLocalRecord ->
  Controlled.State ->
  Bool
recordEffectivelyControlledBy process record controlled =
  case Controlled.controlledEffectiveLabelState
    (Controlled.controlledRecordObjectId record)
    controlled of
    Just released ->
      case releasedLabelStateView released of
        ReleasedLabelView (ProcessLabel owner, _) -> owner == process
        _ -> False
    Nothing -> False

controlledOperateProcess :: CheckedControlledOperate -> ProcessEpochId
controlledOperateProcess (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingProcess binding

controlledOperateWriter :: CheckedControlledOperate -> NablaId
controlledOperateWriter (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingWriter binding

-- | The immutable sequencing association captured with accepted source authority.
controlledOperateSequencingObject :: CheckedControlledOperate -> Maybe GlobalObjectId
controlledOperateSequencingObject (CheckedControlledOperate _ sequencing) = sequencing

controlledOperateSortId :: CheckedControlledOperate -> SortId
controlledOperateSortId (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingSortId binding

controlledOperateOccurrenceId ::
  CheckedControlledOperate -> SortDefinitionOccurrenceId
controlledOperateOccurrenceId (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingOccurrenceId binding

controlledOperateAuthority :: CheckedControlledOperate -> AuthorityEpoch
controlledOperateAuthority (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingAuthority binding

controlledOperateControlPrerequisite :: CheckedControlledOperate -> ControlIndex
controlledOperateControlPrerequisite (CheckedControlledOperate binding _) =
  Controlled.controlledWriterBindingControlPrerequisite binding

controlledOperateWriterBinding ::
  CheckedControlledOperate -> Controlled.ControlledWriterBinding
controlledOperateWriterBinding (CheckedControlledOperate binding _) = binding

sequencingObject :: NablaSequencing -> Maybe GlobalObjectId
sequencingObject UnsequencedNabla = Nothing
sequencingObject (NablaSequencedBy object) = Just object

sequencingObjectField :: FieldName
sequencingObjectField = case mkFieldName "sequencing_object" of
  Right field -> field
  Left _ -> error "invalid closed Nabla sequencing field"

controlledReadSortId :: CheckedControlledRead -> SortId
controlledReadSortId (CheckedControlledRead sortId _ _) = sortId

controlledReadOccurrenceId ::
  CheckedControlledRead -> SortDefinitionOccurrenceId
controlledReadOccurrenceId (CheckedControlledRead _ occurrenceId _) = occurrenceId

controlledReadHeraldEpoch :: CheckedControlledRead -> HeraldEpoch
controlledReadHeraldEpoch (CheckedControlledRead _ _ herald) = herald

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft convert = either (Left . convert) Right
