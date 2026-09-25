{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked control-base preparation and paired carrier/progress admission.
-- The atomic joining-base installer publishes a fully audited receiver-owned
-- successor; an intermediate control preparation cannot expose its cursor.
module Eclips.Herald.UseCase.ControlBase
  ( CheckedControlBase,
    captureControlBase,
    CheckedJoiningBase,
    captureJoiningBase,
    encodeJoiningBase,
    joiningBaseHistory,
    installPortableJoiningBase,
    installPortableJoiningBases,
    installJoiningBase,
    prepareJoiningReplayContext,
    sourceAdmission,
    replacePortableJoiningBases,
    reclaimControlBasePrefix,
    controlBaseControlIndex,
    ControlBaseProblem (..),
    PreparedControlBase,
    prepareControlBase,
    preparedControlBaseProjection,
    preparedControlBaseClient,
    preparedControlBaseControlled,
    preparedControlBaseLabels,
    preparedControlBaseStructural,
    preparedControlBaseDiscovery,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Disappearance (DisappearanceResolutionOutcomeView (RegularSortDefinitionRetired), disappearanceResolutionOutcomeView, disappearanceResolutionSubject)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    controlIndex,
    deltaIdFromGlobalObjectId,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    nablaIdFromGlobalObjectId,
    processEpochIdFromGlobalObjectId,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessStart
  ( processStartProcessEpochId,
    processStartProcessId,
    processStartResidence,
  )
import Eclips.Domain.Publication (checkedPublicationKey, checkedPublicationSort)
import Eclips.Domain.Sort.Canonical (canonicalCheckedDescriptor)
import Eclips.Domain.Sort.Descriptor (SortKind (ControlledSort), controlledObjectKey, descriptorKind)
import Eclips.Domain.Sort.Profile (profileSortFor)
import Eclips.Domain.Startup (PredefinedSortRole (DeltaRole, NablaRole, ProcessEpochRole), appliedProcessEnvironmentPublications)
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.History qualified as AlignmentHistory
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Bootstrap qualified as Bootstrap
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.DiagnosticChecks (runDiagnosticCheck)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join.Bootstrap qualified as JoinBootstrap
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.Readiness qualified as Readiness
import Eclips.Herald.Join.Seal qualified as Seal
import Eclips.Herald.Join.SourceBundle qualified as SourceBundle
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleClient (oracleHelloClaims)
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.State qualified as ProcessPreparation
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault, validateHeraldState)
import Eclips.Herald.Startup.Semantic qualified as Semantic
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (sortOccurrenceDefinition, sortOccurrenceSortId)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.ControlReclamation qualified as Reclamation
import Eclips.Herald.UseCase.JoinHistory qualified as JoinReplay
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.UseCase.OracleAdvance qualified as OracleAdvance
import Eclips.Oracle.Admission
  ( HeraldAdmissionPhase (..),
    HeraldAdmissionRecord,
    admissionManifestHeraldEpoch,
    admissionRecordAttempt,
    admissionRecordBeginIndex,
    admissionRecordChangedIndex,
    admissionRecordId,
    admissionRecordManifest,
    admissionRecordPhase,
    admissionRecordPredecessor,
    admissionRecordSeal,
    admissionRecordSemanticToken,
    heraldJoinDigestBytes,
    joinSealAdmissionId,
    joinSealAttempt,
    joinSealContributionDigest,
    joinSealControlPrefix,
    joinSealMemberCuts,
    joinSealRecipeDigest,
    joinSealTopologyCut,
  )
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch)
import Eclips.Oracle.Voter qualified as Voter

-- These strict components contain facts, not a source Herald or its local
-- possessions, Stores, generator, sessions, outstanding requests or patch work.
-- Controlled and label-patch baselines derive from the checked projection.
-- Projection and Client retain their respective exact canonical archives.
data CheckedControlBase
  = CheckedControlBase
      !Projection.ProjectionBaseCapture
      !Client.CheckedJoiningClientBase
      !Reconciliation.StructuralControlBase
  deriving stock (Eq)

data ControlBaseProblem
  = ControlBaseHeraldInvariant HeraldInvariantFault
  | ControlBaseProjectionInvariant Projection.OracleProjectionInvariant
  | ControlBaseProjectionInstall Projection.ProjectionBaseInstallProblem
  | ControlBaseProjectionCodecProblem Projection.ProjectionBaseCodecProblem
  | ControlBaseRegistryCodecProblem Registry.SortRegistryBaseCodecError
  | ControlBaseCarrierCodecProblem Reconciliation.StructuralCarrierBaseCodecProblem
  | ControlBaseProgressEvidenceProblem Progress.StructuralProgressBaseProblem
  | ControlBaseEnvelopeProblem String
  | ControlBaseClientProblem Client.OracleClientProblem
  | ControlBaseLabelProblem LabelPatch.PatchProblem
  | ControlBaseLabelFactsMismatch
  | ControlBaseBootstrapProblem Controlled.ControlledBootstrapError
  | ControlBaseControlledProblem Controlled.ControlledControlBaseImportError
  | ControlBaseControlledDerivationProblem Controlled.ControlledControlBaseDerivationError
  | ControlBaseControlledFactsMismatch
  | ControlBaseDiscoveryProblem Discovery.JoiningControlBaseProblem
  | ControlBaseCarrierProblem Reconciliation.StructuralReconciliationProblem
  | ControlBaseProgressProblem Progress.StructuralProgressProblem
  | ControlBaseGraphProblem Graph.StructuralGraphPatchProblem
  | ControlBaseAdmissionGraphProblem Graph.GraphBootstrapError
  | ControlBaseRegistryProblem Registry.SortRegistryBaseImportError
  | ControlBaseObservationProblem Controlled.ControlledObservationError
  | ControlBaseObservationImportProblem Controlled.ControlledPeerObservationError
  | ControlBaseRetirementProblem Store.RegularDefinitionRetirementError
  | ControlBaseControlledPurgeProblem Store.ControlledObjectPurgeError
  | ControlBaseHistoryProblem History.JoinHistoryProblem
  | ControlBaseHistoryReplayProblem JoinReplay.JoinHistoryReplayProblem
  | ControlBaseAdmissionTailProblem Join.AdmissionControlTailProblem
  | ControlBaseIsolationProblem Isolation.IsolationInitializationProblem
  | ControlBaseFailureDetectionProblem FailureDetection.FailureDetectionInvariantViolation
  | ControlBaseProvenanceProblem String
  | ControlBaseCarrierControlMismatch
  | ControlBaseHistoryIncomplete
  | ControlBaseSourceUnavailable
  | ControlBaseReceiverNotFreshObserver
  | ControlBaseAdmissionMismatch
  deriving stock (Eq, Show)

auditHeraldState :: HeraldState -> Either ControlBaseProblem ()
auditHeraldState state =
  runDiagnosticCheck
    (startupDiagnosticChecks state)
    (mapProblem ControlBaseHeraldInvariant (validateHeraldState state))

-- | The whole-owner audit binds independently owned facts to the same cut.
-- Capturing a pending local patch is not a release promise and is rejected by
-- its owner. This operation neither prunes evidence nor acknowledges a Join.
captureControlBase :: HeraldState -> Either ControlBaseProblem CheckedControlBase
captureControlBase source = do
  auditHeraldState source
  unless
    ( heraldPhase source == HeraldServing
        && not (startupSemanticControlIsFrozen source)
        && Projection.oracleViewLocalHeraldIsCurrent (Projection.oracleView (startupOracleProjectionState source))
    )
    (Left ControlBaseSourceUnavailable)
  !projection <- mapProblem ControlBaseProjectionInvariant (Projection.captureProjectionBaseWithDiagnostics (startupDiagnosticChecks source) (startupOracleProjectionState source))
  !client <- mapProblem ControlBaseClientProblem (Client.captureJoiningClientBaseFromProjection projection)
  !labels <- mapProblem ControlBaseLabelProblem (LabelPatch.captureCheckedLabelPatchBase (startupLabelPatchState source))
  runDiagnosticCheck (startupDiagnosticChecks source) $ do
    unless
      (labels == LabelPatch.labelPatchBaseFromProjection (startupOracleProjectionState source))
      (Left ControlBaseLabelFactsMismatch)
    !controlled <- mapProblem ControlBaseControlledDerivationProblem (Controlled.controlledControlBaseFromProjection (startupOracleProjectionState source))
    unless
      (controlled == Controlled.captureControlledControlBase (startupControlledState source))
      (Left ControlBaseControlledFactsMismatch)
  let !structural = Reconciliation.captureStructuralControlBase (Progress.structuralProgressReconciliation (startupStructuralProgressState source))
  pure (CheckedControlBase projection client structural)

controlBaseControlIndex :: CheckedControlBase -> ControlIndex
controlBaseControlIndex (CheckedControlBase projection _ _) = Projection.projectionBaseControlIndex projection

-- | Build a disposable fresh context for the applicant's latest locally known
-- attempt. This is not a replacement transition: the live owner remains at its
-- observed control cursor, with all requests, timers and transport generations.
-- Only a later atomic adoption may publish a fully caught-up candidate.
prepareJoiningReplayContext :: HeraldState -> Either ControlBaseProblem HeraldState
prepareJoiningReplayContext current = do
  (original, observed) <- joiningReceiverAdmission current
  record <- latestJoiningAdmission original (maybe [] pure observed)
  prepareJoiningReplayContextFor record current

-- The startup record can be newer than the first captured donor state, but a
-- canonically cancelled or activated owner cannot use startup provenance to
-- re-enter onboarding.
joiningReceiverAdmission :: HeraldState -> Either ControlBaseProblem (HeraldAdmissionRecord, Maybe HeraldAdmissionRecord)
joiningReceiverAdmission current = do
  auditHeraldState current
  unless
    ( heraldPhase current == HeraldServing
        && Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState current)
        && Application.applicationMembershipGateClosed (startupApplicationState current)
        && not (startupSemanticControlIsFrozen current)
    )
    (Left ControlBaseReceiverNotFreshObserver)
  original <- maybe (Left ControlBaseReceiverNotFreshObserver) Right (Genesis.checkedStartupAdmission (startupGenesis current))
  let observed = Projection.oracleViewHeraldAdmission (admissionRecordId original) (Projection.oracleView (startupOracleProjectionState current))
  mapM_ requirePendingAdmission (original : maybe [] pure observed)
  pure (original, observed)

requirePendingAdmission :: HeraldAdmissionRecord -> Either ControlBaseProblem ()
requirePendingAdmission record = case admissionRecordPhase record of
  AdmissionActivated {} -> Left ControlBaseReceiverNotFreshObserver
  AdmissionCancelled {} -> Left ControlBaseReceiverNotFreshObserver
  _ -> pure ()

-- Select independently admitted canonical facts without confusing a source's
-- pre-seal capture with the later startup/current record that binds its bytes.
latestJoiningAdmission :: HeraldAdmissionRecord -> [HeraldAdmissionRecord] -> Either ControlBaseProblem HeraldAdmissionRecord
latestJoiningAdmission = foldM newer
  where
    newer previous supplied = do
      unless (sameJoiningIdentity previous supplied) (Left ControlBaseAdmissionMismatch)
      let (earlier, later) = if admissionRecordChangedIndex supplied >= admissionRecordChangedIndex previous then (previous, supplied) else (supplied, previous)
      joiningBaseRequire
        ( admissionRecordChangedIndex earlier /= admissionRecordChangedIndex later
            || earlier == later
        )
        "joining admission records conflict at the same canonical coordinate"
      joiningBaseRequire
        (admissionRecordAttempt earlier <= admissionRecordAttempt later)
        "joining admission attempt regresses"
      joiningBaseRequire
        ( admissionRecordAttempt earlier /= admissionRecordAttempt later
            || maybe True (\seal -> admissionRecordSeal later == Just seal) (admissionRecordSeal earlier)
        )
        "joining admission changed a seal without invalidation"
      pure later

prepareJoiningReplayContextFor :: HeraldAdmissionRecord -> HeraldState -> Either ControlBaseProblem HeraldState
prepareJoiningReplayContextFor record current = do
  requirePendingAdmission record
  genesis <- mapProblem (ControlBaseEnvelopeProblem . show) (Genesis.checkJoiningHeraldGenesis (startupGenesis current) record)
  let bootstraps = startupInitialBootstraps current
      projection = Projection.joiningReplayOrigin record (startupOracleProjectionState current)
      view = Projection.oracleView projection
      local = Genesis.checkedLocalHeraldEpoch genesis
      membership = Projection.oracleViewCurrentHeraldMembershipId view
      configuration = Projection.oracleViewVoterConfiguration view
      voters = maybe (Isolation.isolationWitnessVoterHosts (Isolation.stateWitness (startupIsolationState current))) (Set.fromList . map raftVoterBindingHeraldEpoch . Voter.voterConfigurationBindings) configuration
      client =
        Client.initialJoiningState
          (oracleHelloClaims (Genesis.checkedSystemId genesis) (Genesis.checkedCatalogueDigest genesis) (Genesis.checkedConfigurationDigest genesis) (Genesis.checkedInitialProjectionDigest bootstraps) (Genesis.checkedLocalHeraldId genesis) local membership)
          (Client.oracleClientContacts (startupOracleClientState current))
          (controlIndex 0)
  semantic <- mapProblem ControlBaseHeraldInvariant (Semantic.prepareInitialSemanticOwners genesis bootstraps)
  discovery <- mapProblem (ControlBaseDiscoveryProblem . Discovery.JoiningControlBaseDiscoveryInvariant) (Discovery.initialJoiningState (Discovery.discoveryStaticView (startupDiscoveryState current)) record)
  isolation <- mapProblem ControlBaseIsolationProblem (Isolation.freshJoiningReplayContext (startupLastObservedTime current) voters membership (startupIsolationState current))
  let context =
        configureHeraldDiagnosticChecks (startupDiagnosticChecks current)
          . replaceStartupPlacementState (Placement.clearAlignmentLossChange . Placement.clearAlignmentPlanChange . Placement.clearPreparationPlacementChanges $ Semantic.initialSemanticPlacement semantic)
          $ servingHeraldState
            (heraldStaticWithTiming (startupTakeoverTarget current) genesis bootstraps)
            (startupApplicationRecoveryConfiguration current)
            (startupLastObservedTime current)
            (startupIdGeneratorState current)
            client
            projection
            (Semantic.initialSemanticRegistry semantic)
            (Semantic.initialSemanticGraph semantic)
            (Semantic.initialSemanticProgress semantic)
            discovery
            (PeerLiveness.freshJoiningReplayContext (startupPeerLivenessState current))
            (FailureDetection.freshJoiningReplayContext voters (Voter.voterConfigurationId <$> configuration) (startupFailureDetectionState current))
            isolation
            (PeerStream.initialState local)
            (Semantic.initialSemanticBootstrapOwners semantic)
  auditHeraldState context
  pure context

-- | Read the donor's actual admission through its independently checked
-- Projection frame. History binds the source and capture coordinates but does
-- not supply canonical attempt authority. Other semantic frames are admitted
-- only when the complete donor set is reconstructed.
sourceAdmission :: HeraldEpoch -> BS.ByteString -> HeraldState -> Either ControlBaseProblem HeraldAdmissionRecord
sourceAdmission expectedSource bytes current = do
  (original, observed) <- joiningReceiverAdmission current
  let origin = Projection.joiningReplayOrigin original (startupOracleProjectionState current)
  JoiningProjectionParts _ source _ _ record <- decodeJoiningProjection expectedSource bytes (startupGenesis current) origin
  unless (sameJoiningIdentity original record) (Left ControlBaseAdmissionMismatch)
  _ <- latestJoiningAdmission original (maybe [] pure observed <> [record])
  mapM_
    (validateJoiningSeal source)
    [known | known <- original : maybe [] pure observed, admissionRecordAttempt known == admissionRecordAttempt record]
  pure record

-- | Atomically adopt an unobservable applicant's canonical and semantic
-- reconstruction. The candidate reaches the later donor/live cursor before
-- adoption; discarded staging need not support that replay. Receiver-local
-- protocol generations, timers, identities and request ledgers survive.
replacePortableJoiningBases :: [(HeraldEpoch, BS.ByteString)] -> HeraldState -> Either ControlBaseProblem HeraldState
replacePortableJoiningBases sources current = do
  (original, observed) <- joiningReceiverAdmission current
  records <- traverse (\(source, bytes) -> sourceAdmission source bytes current) sources
  record <- latestJoiningAdmission original (maybe [] pure observed <> records)
  context <- prepareJoiningReplayContextFor record current
  (imported, _) <- installPortableJoiningBases sources context
  let currentProjection = startupOracleProjectionState current
      currentView = Projection.oracleView currentProjection
      currentCursor = Projection.oracleViewControlIndex currentView
      importedCursor = Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState imported))
      histories = map History.sourceJoinHistory (Readiness.histories record imported)
  suffix <-
    if importedCursor >= currentCursor
      then pure []
      else maybe (Left ControlBaseHistoryIncomplete) Right (Projection.retainedControlSuffixAfter importedCursor currentProjection)
  candidate <- foldM (replay histories) imported suffix
  let candidateProjection = startupOracleProjectionState candidate
      candidateView = Projection.oracleView candidateProjection
  joiningBaseRequire
    ( Projection.oracleViewControlIndex candidateView > currentCursor
        || ( candidateView == currentView
               && Projection.projectionLastSemanticControlIndex candidateProjection == Projection.projectionLastSemanticControlIndex currentProjection
           )
    )
    "joining replacement differs from the observed canonical state"
  projectedRecord <- maybe (Left ControlBaseAdmissionMismatch) Right (Projection.oracleViewPendingHeraldAdmission candidateView)
  unless (sameJoiningAdmission record projectedRecord) (Left ControlBaseAdmissionMismatch)
  projection <- mapProblem ControlBaseProjectionInvariant (Projection.adoptJoiningReplay candidateProjection currentProjection)
  client <- mapProblem ControlBaseClientProblem (Client.adoptJoiningReplay (startupOracleClientState candidate) (startupOracleClientState current))
  discovery <- mapProblem (ControlBaseDiscoveryProblem . Discovery.JoiningControlBaseDiscoveryInvariant) (Discovery.adoptJoiningReplay (startupDiscoveryState candidate) (startupDiscoveryState current))
  -- The enclosing Herald product is lazy, and its invariant reads imported
  -- Join material only for particular consumers. Detach the owner here so an
  -- undemanded selector cannot retain the discarded whole Herald.
  retained <- mapProblem ControlBaseAdmissionTailProblem (Join.retainAdmissionControlTail projectedRecord (Join.replaceJoiningMaterial (startupJoinState candidate) (startupJoinState current)))
  -- The immutable source set ends at importedCursor. A later replacement from
  -- those same captures still needs the locally replayed suffix after that cut,
  -- even though this candidate has already reached the live observer's cursor.
  !material <- mapProblem ControlBaseAdmissionTailProblem (Join.advanceAdmissionControlTailFloor (admissionRecordId projectedRecord) (admissionManifestHeraldEpoch (admissionRecordManifest projectedRecord)) importedCursor retained)
  let preparation = ProcessPreparation.notifyPreparationDependencies (Set.singleton ProcessPreparation.StructuralReadinessChanged) (startupProcessPreparationState current)
      !adopted =
        replaceStartupOracleProjectionState projection
          . replaceStartupOracleClientState client
          . replaceStartupDiscoveryState discovery
          . replaceStartupSortRegistryState (startupSortRegistryState candidate)
          . replaceStartupControlledState (startupControlledState candidate)
          . replaceStartupGraphState (startupGraphState candidate)
          . replaceStartupStructuralProgressState (startupStructuralProgressState candidate)
          . replaceStartupStructuralBaseCoordinator (startupStructuralBaseCoordinator candidate)
          . replaceStartupAlignmentState (startupAlignmentState candidate)
          . replaceStartupAlignmentCutQueryCache (startupAlignmentCutQueryCache candidate)
          . replaceStartupPlacementState (startupPlacementState candidate)
          . replaceStartupStoreState (startupStoreState candidate)
          . replaceStartupPublicationState (startupPublicationState candidate)
          . replaceStartupLabelPatchState (startupLabelPatchState candidate)
          . replaceStartupLabelBarrierState (startupLabelBarrierState candidate)
          . replaceStartupVisibilityState (startupVisibilityState candidate)
          . replaceStartupTerminalSourceHoldState (startupTerminalSourceHoldState candidate)
          . replaceStartupDisappearanceState (startupDisappearanceState candidate)
          . replaceStartupJoinState material
          . replaceStartupProcessPreparationState preparation
          $ current
  successor <- restoreCurrentControlOwners adopted
  -- A canonically observed seal may have escaped as readiness even when the
  -- discarded staging cannot demonstrate it. A startup-only seal binds source
  -- bytes but has not authorized local readiness. Invalidation ends the earlier
  -- attempt's promise.
  mapM_
    ( \previous ->
        joiningBaseRequire
          (maybe True (\seal -> admissionRecordSeal projectedRecord == Just seal) (admissionRecordSeal previous))
          "joining replacement changed the current readiness promise"
    )
    [previous | previous <- maybe [] pure observed, admissionRecordAttempt previous == admissionRecordAttempt projectedRecord]
  requireReady successor projectedRecord
  auditHeraldState successor
  reclaimControlBasePrefix successor
  where
    replay histories state entry = do
      (before, _) <- mapProblem ControlBaseHistoryProblem (History.advanceJoinHistoriesData histories state)
      (after, _) <- mapProblem ControlBaseHistoryReplayProblem (JoinReplay.replayJoinControlHistory [entry] before)
      fst <$> mapProblem ControlBaseHistoryProblem (History.advanceJoinHistoriesData histories after)
    requireReady successor record = case admissionRecordSeal record of
      Nothing -> pure ()
      Just _ -> do
        _ <- mapProblem ControlBaseEnvelopeProblem (Readiness.localReadyReport record successor)
        pure ()

-- | A non-serving preparation, with portable structural controls still waiting
-- for checked carrier installation. Only the paired joining-base transaction
-- below may install these owners together with their dependent consequences.
data PreparedControlBase
  = PreparedControlBase
      !Projection.State
      !Client.State
      !Controlled.State
      !LabelPatch.State
      !Reconciliation.StructuralControlBase
      !Discovery.State
  deriving stock (Eq)

prepareControlBase :: CheckedControlBase -> HeraldState -> Either ControlBaseProblem PreparedControlBase
prepareControlBase (CheckedControlBase projectionBase clientBase structuralBase) receiver = do
  auditHeraldState receiver
  unless
    ( heraldPhase receiver == HeraldServing
        && Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState receiver)
        && not (startupSemanticControlIsFrozen receiver)
    )
    (Left ControlBaseReceiverNotFreshObserver)
  admission <- maybe (Left ControlBaseReceiverNotFreshObserver) Right (Genesis.checkedStartupAdmission (startupGenesis receiver))
  !projection <- mapProblem ControlBaseProjectionInstall (Projection.installProjectionBase projectionBase (startupOracleProjectionState receiver))
  let view = Projection.oracleView projection
      !labelBase = LabelPatch.labelPatchBaseFromProjection projection
  projectedAdmission <- maybe (Left ControlBaseAdmissionMismatch) Right (Projection.oracleViewPendingHeraldAdmission view)
  unless
    ( sameJoiningAdmission admission projectedAdmission
        && not (Projection.oracleViewLocalHeraldIsCurrent view)
    )
    (Left ControlBaseAdmissionMismatch)
  -- Dynamic Starts are global control facts. Genesis metadata is observed only
  -- for the selected labelled objects, never by copying donor-local root sets.
  started <- foldM observeStart (startupControlledState receiver) (Projection.projectedStartedProcesses projection)
  observed <-
    mapProblem
      ControlBaseBootstrapProblem
      ( Bootstrap.observeCanonicalBootstrapObjects
          (Projection.oracleViewSystemId view)
          (map snd (Projection.projectedBootstraps projection))
          (Set.fromList (map LabelPatch.importedLabelPatchObject (LabelPatch.checkedLabelPatchBaseEntries labelBase)))
          started
      )
  controlledBase <- mapProblem ControlBaseControlledDerivationProblem (Controlled.controlledControlBaseFromProjection projection)
  preparedControlled <- mapProblem ControlBaseControlledProblem (Controlled.prepareControlledControlBaseImport controlledBase observed)
  let !controlled = Controlled.commitControlledControlBaseImport preparedControlled
  !labels <- mapProblem ControlBaseLabelProblem (LabelPatch.installCheckedLabelPatchBase (Projection.projectionBaseControlIndex projectionBase) labelBase (startupLabelPatchState receiver))
  !client <- mapProblem ControlBaseClientProblem (Client.installJoiningClientBase clientBase (startupOracleClientState receiver))
  !discovery <- Discovery.commitJoiningControlBase <$> mapProblem ControlBaseDiscoveryProblem (Discovery.prepareJoiningControlBase (Projection.oracleViewHeraldMembershipHistoryChecked view) (Projection.projectedHeraldAdmissions projection) (startupDiscoveryState receiver))
  pure (PreparedControlBase projection client controlled labels structuralBase discovery)
  where
    observeStart controlled (_, start) =
      let fact = Projection.projectedStartedProcessBootstrap start
       in Controlled.commitControlledBootstrap
            <$> mapProblem
              ControlBaseBootstrapProblem
              ( Controlled.prepareControlledBootstrapObservation
                  ( Controlled.processFact
                      (processStartProcessId fact)
                      (processStartProcessEpochId fact)
                      (processStartResidence fact)
                      (labelAuthorityEpoch (Projection.projectedStartedProcessControlIndex start))
                  )
                  []
                  controlled
              )

preparedControlBaseProjection :: PreparedControlBase -> Projection.State
preparedControlBaseProjection (PreparedControlBase projection _ _ _ _ _) = projection

preparedControlBaseClient :: PreparedControlBase -> Client.State
preparedControlBaseClient (PreparedControlBase _ client _ _ _ _) = client

preparedControlBaseControlled :: PreparedControlBase -> Controlled.State
preparedControlBaseControlled (PreparedControlBase _ _ controlled _ _ _) = controlled

preparedControlBaseLabels :: PreparedControlBase -> LabelPatch.State
preparedControlBaseLabels (PreparedControlBase _ _ _ labels _ _) = labels

preparedControlBaseStructural :: PreparedControlBase -> Reconciliation.StructuralControlBase
preparedControlBaseStructural (PreparedControlBase _ _ _ _ structural _) = structural

preparedControlBaseDiscovery :: PreparedControlBase -> Discovery.State
preparedControlBaseDiscovery (PreparedControlBase _ _ _ _ _ discovery) = discovery

-- | Private checked-base wrapper around completed-batch reclamation. Frozen
-- sources cannot publish a new portable base; ordinary runtime reclamation
-- instead leaves their separate semantic and observed owners unchanged.
reclaimControlBasePrefix :: HeraldState -> Either ControlBaseProblem HeraldState
reclaimControlBasePrefix state = do
  unless (not (startupSemanticControlIsFrozen state)) (Left ControlBaseSourceUnavailable)
  mapProblem problem (Reclamation.reclaimControlPrefix state)
  where
    problem (Reclamation.ControlReclamationClientProblem failure) = ControlBaseClientProblem failure
    problem (Reclamation.ControlReclamationProjectionProblem failure) = ControlBaseProjectionInvariant failure

-- | Paired source capture. Canonical controls after the checked covered anchor
-- accompany the semantic base and original-payload archive. No owner state,
-- local resources or transport receipts are copied.
data CheckedJoiningBase
  = CheckedJoiningBase
      !CheckedControlBase
      !Progress.StructuralProgressBase
      !Registry.SortRegistryBase
      !History.JoinHistory
      !BS.ByteString

-- | Capture the immutable aggregate before sealing. Later delivery reuses this
-- exact capture: rebuilding it after Seal would include a later control prefix
-- and change the member-cut digest. Admission tails independently retain the
-- ordinary canonical replay bytes while a newcomer still needs them.
captureJoiningBase :: HeraldState -> Either ControlBaseProblem (Maybe CheckedJoiningBase)
captureJoiningBase source = do
  admission <- maybe (Left ControlBaseSourceUnavailable) Right (Projection.oracleViewPendingHeraldAdmission (Projection.oracleView (startupOracleProjectionState source)))
  case admissionRecordSeal admission of
    Just _ -> pure Nothing
    Nothing -> do
      history <- mapProblem ControlBaseHistoryProblem (History.captureJoinHistoryWithDiagnostics (startupDiagnosticChecks source) admission source)
      case history of
        Nothing -> pure Nothing
        Just retained -> do
          !controls <- captureControlBase source
          let !progress = Progress.captureStructuralProgressBase (startupStructuralProgressState source)
              !registry = Registry.captureSortRegistryBase (startupSortRegistryState source)
              !historyBytes = History.encodeJoinHistory retained
          -- Materialize the immutable transcript before releasing the source. This
          -- detaches its lazy lists and shares these bytes with the outer encoder.
          BS.length historyBytes `seq` pure (Just (CheckedJoiningBase controls progress registry retained historyBytes))

-- | Canonical immutable owner facts together with the original semantic
-- History transcript. All five frames form the sealed source-cut receipt;
-- none may be recaptured after sealing.
-- Client pins and structural controls derive from the admitted leaves.
encodeJoiningBase :: CheckedJoiningBase -> BS.ByteString
encodeJoiningBase (CheckedJoiningBase (CheckedControlBase projection _ _) progress registry _ historyBytes) =
  SourceBundle.encodeJoinSourceBundle
    ( SourceBundle.joinSourceBundle
        (Projection.encodeProjectionBase projection)
        (Registry.encodeSortRegistryBase registry)
        (Progress.encodeStructuralProgressBaseEvidence (Progress.captureStructuralProgressBaseEvidence progress))
        (Reconciliation.encodeStructuralCarrierBase (Progress.structuralProgressBaseCarrierBase progress))
        historyBytes
    )

joiningBaseHistory :: CheckedJoiningBase -> History.JoinHistory
joiningBaseHistory (CheckedJoiningBase _ _ _ history _) = history

-- | Independently admit the portable leaves and publish one audited atomic
-- successor. The caller supplies the expected source identity; the payload
-- cannot choose which old member it represents. Complete-set reconstruction
-- controls live adoption; receiving these bytes alone does not complete a Join.
--
-- The first adoption precedes Seal. If receiver startup or the decoded pending
-- record already carries a seal, its exact source member-cut must cover the
-- original aggregate bytes. Otherwise the retained bundle is the immutable input
-- to the later ordinary seal/ready checks; no historical proof is synthesized.
installPortableJoiningBase :: HeraldEpoch -> BS.ByteString -> HeraldState -> Either ControlBaseProblem (HeraldState, EffectBatch)
installPortableJoiningBase expectedSource bytes receiver = do
  PortableJoiningBaseParts controls evidence carriers registry source <- decodeJoiningBase expectedSource bytes receiver
  let history = History.sourceJoinHistory source
      prepareProgress reconciliation observer = do
        prepared <- mapProblem ControlBaseProgressEvidenceProblem (Progress.prepareStructuralProgressBaseImportFromEvidence evidence carriers reconciliation observer)
        let progress = Progress.commitStructuralProgressBaseImport prepared
        validateJoiningProgress history progress
        pure progress
  installJoiningBaseParts controls registry source carriers prepareProgress receiver

-- | Prepare one coherent observer from the complete frozen predecessor input.
-- A certified structural descendant selects the base; a larger control cursor
-- alone cannot select it. Other sources supply their data and obligations, plus
-- a contiguous control tail when the selected base precedes their capture.
-- The live coordinator reconstructs in this fresh private context before
-- adopting the result atomically with its canonical control owners.
installPortableJoiningBases :: [(HeraldEpoch, BS.ByteString)] -> HeraldState -> Either ControlBaseProblem (HeraldState, EffectBatch)
installPortableJoiningBases supplied receiver = do
  record <- maybe (Left ControlBaseReceiverNotFreshObserver) Right (Genesis.checkedStartupAdmission (startupGenesis receiver))
  inputs <- foldM retainInput Map.empty supplied
  sources <- traverse decodeSource (Map.toAscList inputs)
  seal <- mapProblem ControlBaseEnvelopeProblem (Seal.prepareJoinSeal record sources)
  joiningBaseRequire (maybe True (== seal) (admissionRecordSeal record)) "joining source set differs from canonical seal"
  mapM_ (`validateJoiningSeal` record) sources
  selected <- case sortOn preference [source | source <- sources, History.joinHistoryTopologyCut (History.sourceJoinHistory source) == joinSealTopologyCut seal] of
    source : _ -> pure source
    [] -> Left ControlBaseSourceUnavailable
  let selectedHistory = History.sourceJoinHistory selected
      basePrefix = History.joinHistoryControlPrefix selectedHistory
      finalPrefix = joinSealControlPrefix seal
      histories = map History.sourceJoinHistory sources
  suffix <-
    if basePrefix == finalPrefix
      then pure []
      else sourceControlSuffix record basePrefix [source | source <- sources, History.joinHistoryControlPrefix (History.sourceJoinHistory source) == finalPrefix]
  (base, baseEffects) <- installPortableJoiningBase (History.joinHistorySource selectedHistory) (History.encodeJoinSourceHistory selected) receiver
  retained <- foldM retainSource base sources
  (replayed, replayEffects) <- foldM (replayControl histories) (retained, mempty) suffix
  -- Admission checks retained exact overlap as well as the covered cursor.
  -- No source transcript is replayed from its own origin a second time.
  mapM_ (mapProblem ControlBaseHistoryProblem . (`History.validateJoinHistoryContext` replayed)) histories
  (successor, dataEffects) <- mapProblem ControlBaseHistoryProblem (History.advanceJoinHistoriesData histories replayed)
  let progress = startupStructuralProgressState successor
      view = Projection.oracleView (startupOracleProjectionState successor)
      cut = joinSealTopologyCut seal
  joiningBaseRequire
    ( Projection.oracleViewControlIndex view == finalPrefix
        && Progress.structuralLastInstalledCutId progress == Topology.deriveTopologyCutId cut
        && Progress.structuralAppliedVector progress == Topology.topologyFrontierStructuralVersionVector (Topology.topologyCutFrontier cut)
        && maybe False (sameJoiningAdmission record) (Projection.oracleViewPendingHeraldAdmission view)
    )
    "joining source import changed the selected admission or cut"
  unless (all (`History.joinHistoryInstalled` successor) histories) (Left ControlBaseHistoryIncomplete)
  auditHeraldState successor
  pure (successor, baseEffects <> replayEffects <> dataEffects)
  where
    retainInput inputs (expected, bytes) = case Map.lookup expected inputs of
      Nothing -> pure (Map.insert expected bytes inputs)
      Just prior | prior == bytes -> pure inputs
      _ -> Left (ControlBaseEnvelopeProblem "conflicting joining source inputs")
    decodeSource (expected, bytes) = do
      source <- mapProblem ControlBaseHistoryProblem (History.decodeJoinSourceHistory bytes)
      joiningBaseRequire (History.joinHistorySource (History.sourceJoinHistory source) == expected) "joining source differs from expected donor"
      pure source
    preference source = (Down (History.joinHistoryControlPrefix history), History.joinHistorySource history)
      where
        history = History.sourceJoinHistory source
    -- Coverage may already reach a donor's capture cursor while its admission
    -- tail remains physically retained below it. History keeps the exact base
    -- anchor; admit the donor's Projection before using that separate tail to
    -- reach its control cursor from the selected structural donor's older one.
    sourceControlSuffix _ _ [] = Left (ControlBaseHistoryProblem History.JoinHistoryCanonicalControlGap)
    sourceControlSuffix record after (source : remaining) = do
      let history = History.sourceJoinHistory source
      JoiningProjectionParts _ _ _ projection projectedAdmission <- decodeJoiningProjection (History.joinHistorySource history) (History.encodeJoinSourceHistory source) (startupGenesis receiver) (startupOracleProjectionState receiver)
      unless (sameJoiningAdmission record projectedAdmission) (Left ControlBaseAdmissionMismatch)
      case Projection.retainedControlSuffixAfter after projection of
        Just entries -> pure entries
        Nothing -> sourceControlSuffix record after remaining
    retainSource state source = do
      let history = History.sourceJoinHistory source
      retained <- mapProblem ControlBaseProvenanceProblem (Join.retainImportedProvenance (AlignmentHistory.alignmentHistoryPlanAcceptances (History.joinHistoryAlignmentHistory history)) (History.joinHistoryOccurrences history) (startupJoinState state))
      pure (replaceStartupJoinState (Join.retainInstalledHistory (History.joinHistoryAdmission history) (History.joinHistoryAttempt history) (History.joinHistorySource history) (History.encodeJoinSourceHistory source) retained) state)
    replayControl histories (state, effects) entry = do
      (before, beforeEffects) <- mapProblem ControlBaseHistoryProblem (History.advanceJoinHistoriesData histories state)
      (after, emitted) <- mapProblem ControlBaseHistoryReplayProblem (JoinReplay.replayJoinControlHistory [entry] before)
      pure (after, effects <> beforeEffects <> emitted)

-- The parts are private, transient admission results, not a checked whole-owner
-- product. Only installPortableJoiningBase may expose their audited successor.
data PortableJoiningBaseParts
  = PortableJoiningBaseParts
      !CheckedControlBase
      !Progress.AdmittedStructuralProgressBaseEvidence
      !Reconciliation.StructuralCarrierBase
      !Registry.SortRegistryBase
      !History.JoinSourceHistory

decodeJoiningBase :: HeraldEpoch -> BS.ByteString -> HeraldState -> Either ControlBaseProblem PortableJoiningBaseParts
decodeJoiningBase expectedSource bytes receiver = do
  auditHeraldState receiver
  JoiningProjectionParts bundle source projectionBase projection projectedAdmission <- decodeJoiningProjection expectedSource bytes (startupGenesis receiver) (startupOracleProjectionState receiver)
  let registryFrame = SourceBundle.joinSourceBundleRegistryFrame bundle
      progressFrame = SourceBundle.joinSourceBundleProgressFrame bundle
      carrierFrame = SourceBundle.joinSourceBundleCarrierFrame bundle
      history = History.sourceJoinHistory source
      genesis = startupGenesis receiver
      receiverProgress = startupStructuralProgressState receiver
      prefix = History.joinHistoryControlPrefix history
      cut = History.joinHistoryTopologyCut history
      view = Projection.oracleView projection
  startupAdmission <- maybe (Left ControlBaseReceiverNotFreshObserver) Right (Genesis.checkedStartupAdmission genesis)
  unless (sameJoiningAdmission startupAdmission projectedAdmission) (Left ControlBaseAdmissionMismatch)
  mapM_ (validateJoiningSeal source) [startupAdmission, projectedAdmission]
  case (admissionRecordSeal startupAdmission, admissionRecordSeal projectedAdmission) of
    (Just startupSeal, Just projectedSeal) -> joiningBaseRequire (startupSeal == projectedSeal) "joining admission seals differ"
    _ -> pure ()
  registryBase <- mapProblem ControlBaseRegistryCodecProblem (Registry.decodeSortRegistryBase genesis (BS.copy registryFrame))
  registry <- Registry.commitSortRegistryBaseImport <$> mapProblem ControlBaseRegistryProblem (Registry.prepareSortRegistryBaseImport registryBase (startupSortRegistryState receiver))
  progressContext <- mapProblem ControlBaseProgressEvidenceProblem (Progress.structuralProgressBaseAdmissionContext (Projection.oracleViewHeraldMembershipHistoryChecked view) (Projection.projectedHeraldAdmissions projection) prefix (Topology.deriveTopologyCutId cut) receiverProgress)
  claims <- mapProblem ControlBaseProgressEvidenceProblem (Progress.decodeStructuralProgressBaseEvidence (BS.copy progressFrame))
  evidence <- mapProblem ControlBaseProgressEvidenceProblem (Progress.admitStructuralProgressBaseEvidence progressContext claims)
  let originals = Map.fromList [(Terminal.terminalStructuralOccurrenceId original, original) | original <- History.joinHistoryOccurrences history]
  mapProblem ControlBaseProgressEvidenceProblem (Progress.validateStructuralProgressOriginalOccurrences evidence originals)
  context <-
    mapProblem
      ControlBaseCarrierCodecProblem
      ( Reconciliation.structuralCarrierBaseContext
          (Genesis.checkedSystemId genesis)
          prefix
          (Progress.admittedStructuralProgressMembershipHistory evidence)
          registry
          (Progress.admittedStructuralProgressRetirementBases evidence)
          [(lineage, recipe) | (lineage, recipe, _) <- Progress.admittedStructuralProgressAdmissions evidence]
          originals
      )
  let facts = replaceStartupOracleProjectionState projection (replaceStartupSortRegistryState registry receiver)
  carriers <- mapProblem ControlBaseCarrierCodecProblem (Reconciliation.decodeStructuralCarrierBase context (OracleAdvance.structuralReconciliationViews facts) (Progress.structuralProgressReconciliation receiverProgress) (BS.copy carrierFrame))
  client <- mapProblem ControlBaseClientProblem (Client.captureJoiningClientBaseFromProjection projectionBase)
  let controls = CheckedControlBase projectionBase client (Reconciliation.structuralCarrierBaseControlBase carriers)
  pure (PortableJoiningBaseParts controls evidence carriers registryBase source)

-- Checked canonical facts and still-unadmitted semantic frames from the same
-- envelope. Both source inspection and complete installation use this boundary.
data JoiningProjectionParts
  = JoiningProjectionParts
      !SourceBundle.JoinSourceBundle
      !History.JoinSourceHistory
      !Projection.ProjectionBaseCapture
      !Projection.State
      !HeraldAdmissionRecord

decodeJoiningProjection :: HeraldEpoch -> BS.ByteString -> Genesis.CheckedHeraldGenesis -> Projection.State -> Either ControlBaseProblem JoiningProjectionParts
decodeJoiningProjection expectedSource bytes genesis origin = do
  bundle <- mapProblem ControlBaseEnvelopeProblem (SourceBundle.decodeJoinSourceBundle bytes)
  source <- mapProblem ControlBaseHistoryProblem (History.decodeJoinSourceHistory bytes)
  let projectionFrame = SourceBundle.joinSourceBundleProjectionFrame bundle
      history = History.sourceJoinHistory source
      prefix = History.joinHistoryControlPrefix history
      cut = History.joinHistoryTopologyCut history
  joiningBaseRequire
    (History.joinHistorySource history == expectedSource && History.joinHistorySystem history == Genesis.checkedSystemId genesis)
    "joining source/system differs from receiver context"
  projectionBase <- mapProblem ControlBaseProjectionCodecProblem (Projection.decodeProjectionBase origin (BS.copy projectionFrame))
  joiningBaseRequire
    ( Projection.projectionBaseControlIndex projectionBase == prefix
        && Projection.projectionBaseCanonicalCoveredThrough projectionBase == History.joinHistoryControlAnchor history
        && Map.elems (snd (Map.split (History.joinHistoryControlAnchor history) (Projection.projectionBaseAppliedEntries projectionBase))) == History.joinHistoryControlEntries history
    )
    "joining Projection and History control cut differ"
  projection <- mapProblem ControlBaseProjectionInstall (Projection.installProjectionBase projectionBase origin)
  let view = Projection.oracleView projection
  projectedAdmission <- maybe (Left ControlBaseAdmissionMismatch) Right (Projection.oracleViewPendingHeraldAdmission view)
  joiningBaseRequire
    ( History.joinHistoryAdmission history == admissionRecordId projectedAdmission
        && History.joinHistoryAttempt history == admissionRecordAttempt projectedAdmission
        && expectedSource `elem` Membership.heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor projectedAdmission)
        && Topology.topologyFrontierMembershipGenerationId (Topology.topologyCutFrontier cut) == Membership.heraldMembershipGenerationId (admissionRecordPredecessor projectedAdmission)
        && Topology.topologyFrontierAppliedControlPrefix (Topology.topologyCutFrontier cut) >= admissionRecordSemanticToken projectedAdmission
    )
    "joining admission/attempt/predecessor differs from receiver context"
  validateJoiningSeal source projectedAdmission
  pure (JoiningProjectionParts bundle source projectionBase projection projectedAdmission)

sameJoiningAdmission :: HeraldAdmissionRecord -> HeraldAdmissionRecord -> Bool
sameJoiningAdmission first second =
  sameJoiningIdentity first second
    && admissionRecordAttempt first == admissionRecordAttempt second

sameJoiningIdentity :: HeraldAdmissionRecord -> HeraldAdmissionRecord -> Bool
sameJoiningIdentity first second =
  admissionRecordId first == admissionRecordId second
    && admissionRecordBeginIndex first == admissionRecordBeginIndex second
    && admissionRecordManifest first == admissionRecordManifest second
    && admissionRecordPredecessor first == admissionRecordPredecessor second

validateJoiningSeal :: History.JoinSourceHistory -> HeraldAdmissionRecord -> Either ControlBaseProblem ()
validateJoiningSeal source record = case admissionRecordSeal record of
  Nothing -> pure ()
  Just seal -> do
    let history = History.sourceJoinHistory source
    joiningBaseRequire
      ( joinSealAdmissionId seal == History.joinHistoryAdmission history
          && joinSealAttempt seal == History.joinHistoryAttempt history
          && lookup (History.joinHistorySource history) (joinSealMemberCuts seal) == Just (History.joinSourceMemberCut source)
          && History.joinHistoryControlPrefix history <= joinSealControlPrefix seal
      )
      "original joining source bundle differs from sealed member cut"
    -- The dominant sealed cut may be a later donor's certified descendant.
    -- Each member retains its own original cut; only the derived admission
    -- contribution and recipe use the common cut selected by the seal.
    contribution <- mapProblem (ControlBaseEnvelopeProblem . show) (JoinBootstrap.heraldBootstrapContribution (History.joinHistorySystem history) (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) Set.empty Set.empty)
    recipe <- mapProblem (ControlBaseEnvelopeProblem . show) (Topology.heraldJoinBaseRecipe (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) (admissionRecordPredecessor record) (joinSealTopologyCut seal) (JoinBootstrap.contributionDigest contribution))
    joiningBaseRequire
      ( heraldJoinDigestBytes (joinSealContributionDigest seal) == Topology.topologyOccurrenceDigestBytes (JoinBootstrap.contributionDigest contribution)
          && heraldJoinDigestBytes (joinSealRecipeDigest seal) == Topology.heraldJoinBaseRecipeDigestBytes recipe
      )
      "joining seal contribution/recipe differs from derived admission"

validateJoiningProgress :: History.JoinHistory -> Progress.StructuralProgressState -> Either ControlBaseProblem ()
validateJoiningProgress history progress = do
  let cut = History.joinHistoryTopologyCut history
      identifier = Topology.deriveTopologyCutId cut
      seals = Map.fromList [(cutId, admission) | (cutId, installed) <- Progress.structuralInstalledCutEntries progress, Just admission <- [Progress.installedTopologyCutJoinSealAdmission installed]]
  joiningBaseRequire
    ( Progress.structuralAppliedControlPrefix progress == History.joinHistoryControlPrefix history
        && Progress.structuralAppliedVector progress == Topology.topologyFrontierStructuralVersionVector (Topology.topologyCutFrontier cut)
        && Progress.structuralLastInstalledCutId progress == identifier
        && fmap Progress.installedTopologyCutCut (Progress.lookupInstalledTopologyCut identifier progress) == Just cut
        && Progress.structuralInstalledEstablishedChain progress == History.joinHistoryTopologyCertificates history
        && map Terminal.successorStructuralBaseEstablishedUnion (Progress.structuralInstalledSuccessorBases progress) == History.joinHistoryTerminalCertificates history
        && seals == Map.fromList (History.joinHistorySealCheckpoints history)
    )
    "joining Progress and original History coordinates differ"

joiningBaseRequire :: Bool -> String -> Either ControlBaseProblem ()
joiningBaseRequire condition detail = unless condition (Left (ControlBaseEnvelopeProblem detail))

-- | Publish one fully audited joining observer. The pure preparation has no
-- intermediate externally visible cursor, and activation remains a later
-- ordinary canonical control transition.
installJoiningBase :: CheckedJoiningBase -> HeraldState -> Either ControlBaseProblem (HeraldState, EffectBatch)
installJoiningBase captured@(CheckedJoiningBase controls progressBase registryBase _ _) receiver = do
  source <- mapProblem ControlBaseHistoryProblem (History.decodeJoinSourceHistory (encodeJoiningBase captured))
  installJoiningBaseParts controls registryBase source (Progress.structuralProgressBaseCarrierBase progressBase) prepareProgress receiver
  where
    prepareProgress reconciliation observer =
      Progress.commitStructuralProgressBaseImport <$> mapProblem ControlBaseProgressProblem (Progress.prepareStructuralProgressBaseImport progressBase reconciliation observer)

-- Both provenance paths stage the same receiver-owned transaction. The callback
-- runs immediately and is never retained in a checked product or owner state.
installJoiningBaseParts ::
  CheckedControlBase ->
  Registry.SortRegistryBase ->
  History.JoinSourceHistory ->
  Reconciliation.StructuralCarrierBase ->
  (Reconciliation.StructuralAppliedState -> Progress.StructuralProgressState -> Either ControlBaseProblem Progress.StructuralProgressState) ->
  HeraldState ->
  Either ControlBaseProblem (HeraldState, EffectBatch)
installJoiningBaseParts controls registryBase source carriers prepareProgress receiver = do
  let history = History.sourceJoinHistory source
  prepared <- prepareControlBase controls receiver
  mapM_
    (validateJoiningSeal source)
    ( [record | Just record <- [Genesis.checkedStartupAdmission (startupGenesis receiver)]]
        <> [record | Just record <- [Projection.oracleViewPendingHeraldAdmission (Projection.oracleView (preparedControlBaseProjection prepared))]]
    )
  registry <- Registry.commitSortRegistryBaseImport <$> mapProblem ControlBaseRegistryProblem (Registry.prepareSortRegistryBaseImport registryBase (startupSortRegistryState receiver))
  let application = Application.setApplicationMembershipGate True (startupApplicationState receiver)
      staged =
        replaceStartupApplicationState application
          . replaceStartupOracleProjectionState (preparedControlBaseProjection prepared)
          . replaceStartupOracleClientState (preparedControlBaseClient prepared)
          . replaceStartupControlledState (preparedControlBaseControlled prepared)
          . replaceStartupLabelPatchState (preparedControlBaseLabels prepared)
          . replaceStartupDiscoveryState (preparedControlBaseDiscovery prepared)
          . replaceStartupSortRegistryState registry
          $ receiver
  unless (Reconciliation.structuralCarrierBaseControlBase carriers == preparedControlBaseStructural prepared) (Left ControlBaseCarrierControlMismatch)
  carrierImport <- mapProblem ControlBaseCarrierProblem (Reconciliation.prepareStructuralCarrierBaseImport (OracleAdvance.structuralReconciliationViews staged) carriers (Progress.structuralProgressReconciliation (startupStructuralProgressState receiver)))
  carrierGraph <- Graph.commitStructuralGraphPatch <$> mapProblem ControlBaseGraphProblem (Graph.prepareStructuralGraphPatch (Reconciliation.preparedStructuralCarrierBaseGraphPatch carrierImport) (startupGraphState receiver))
  progress <- prepareProgress (Reconciliation.preparedStructuralCarrierBaseSuccessor carrierImport) (startupStructuralProgressState receiver)
  -- Activated members' private system-view destinations have canonical
  -- admission origins, separate from structural carrier publications. Restore
  -- these derived vertices without replaying their old activation protocols.
  graph <-
    foldM
      restoreAdmission
      carrierGraph
      [ (index, record)
      | record <- Projection.projectedHeraldAdmissions (preparedControlBaseProjection prepared),
        AdmissionActivated index _ <- [admissionRecordPhase record],
        index <= Progress.structuralAppliedControlPrefix progress
      ]
  controlled <- foldM (observeCarrier registry) (startupControlledState staged) (Reconciliation.structuralCarrierBasePublications carriers)
  let retirementOutcomes =
        [ (index, outcome)
        | (_, projected) <- Projection.projectedDisappearanceProbes (preparedControlBaseProjection prepared),
          Just (Projection.ProjectedDisappearanceResolved outcome index) <- [Projection.projectedDisappearanceTerminal projected],
          RegularSortDefinitionRetired {} <- [disappearanceResolutionOutcomeView outcome]
        ]
  let structural = replaceStartupControlledState controlled . replaceStartupGraphState graph . replaceStartupStructuralProgressState progress $ staged
  current <- restoreCurrentControlOwners structural
  imported <- mapProblem ControlBaseProvenanceProblem (Join.retainImportedProvenance (AlignmentHistory.alignmentHistoryPlanAcceptances (History.joinHistoryAlignmentHistory history)) (History.joinHistoryOccurrences history) (startupJoinState current))
  let retained = Join.retainInstalledHistory (History.joinHistoryAdmission history) (History.joinHistoryAttempt history) (History.joinHistorySource history) (History.encodeJoinSourceHistory source) imported
      withHistory = replaceStartupJoinState retained current
  (withObservations, effects) <- mapProblem ControlBaseHistoryProblem (History.installJoinHistory history withHistory)
  -- Admit retained internal-view evidence before suppressing its effective
  -- projection. Ordinary receipt admission discards already retired input, but
  -- this checked historical base must retain the exact source-cut receipts.
  store <- foldM importRetirement (startupStoreState withObservations) (sortOn fst retirementOutcomes)
  restored <- restoreControlledTerminalPurges (replaceStartupStoreState store withObservations)
  -- The imported owners have consumed this complete base just as they would a
  -- completed control batch. Do not retain dispensable Client copies until an
  -- unrelated later entry or exact replay happens to run that batch's cleanup.
  client <- mapProblem ControlBaseClientProblem (Client.retireSettledLabelRequests (startupOracleClientState restored) >>= Client.compactLabelDecisionEvidence)
  let !successor = replaceStartupOracleClientState client restored
  unless (History.joinHistoryInstalled history successor) (Left ControlBaseHistoryIncomplete)
  auditHeraldState successor
  pure (successor, effects)
  where
    restoreAdmission graph (index, record) = do
      contribution <- mapProblem (ControlBaseEnvelopeProblem . show) (JoinBootstrap.heraldBootstrapContribution (Genesis.checkedSystemId (startupGenesis receiver)) (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) Set.empty Set.empty)
      Graph.commitGraphMembershipAdmission <$> mapProblem ControlBaseAdmissionGraphProblem (Graph.prepareGraphMembershipAdmission index contribution graph)
    importRetirement store (index, outcome) = do
      prepared <- mapProblem ControlBaseRetirementProblem (Store.prepareRegularDefinitionRetirement (disappearanceResolutionSubject outcome) EmptyHeraldPublicationPrefix index store)
      pure (fst (Store.commitRegularDefinitionRetirement prepared))
    observeCarrier registry controlled (occurrence, publication) = do
      entry <- maybe (Left ControlBaseSourceUnavailable) Right (Registry.lookupEffectiveSort (sortOccurrenceSortId occurrence) registry)
      observation <- mapProblem ControlBaseObservationProblem (Controlled.checkControlledObservation (Registry.registryEntryDescriptor entry) (sortOccurrenceDefinition occurrence) publication)
      fst . Controlled.commitControlledPeerObservation <$> mapProblem ControlBaseObservationImportProblem (Controlled.prepareControlledStartupObservation observation controlled)

-- Store evidence is admitted into this private successor before its terminal
-- suppression ledger is reconstructed. Receipts and historical observations
-- remain intact, while deleted payloads cannot become effective in the exposed
-- observer. The evidence set mirrors the ordinary publication invariant:
-- receiver system-view observations, canonical seeds/descriptions, and checked
-- bootstrap object metadata. No donor Store coordinate or contents is copied.
restoreControlledTerminalPurges :: HeraldState -> Either ControlBaseProblem HeraldState
restoreControlledTerminalPurges state = do
  observed <- foldM purgePublication (startupStoreState state) publications
  store <- foldM purgeBootstrap observed (Controlled.controlledTerminalDeletionEntries controlled)
  pure (replaceStartupStoreState store state)
  where
    controlled = startupControlledState state
    registry = startupSortRegistryState state
    bootstraps = map snd (Projection.projectedBootstraps (startupOracleProjectionState state))
    publications =
      Store.primordialSeedPublications (startupStoreState state)
        <> [publication | bootstrap <- bootstraps, (_, _, publication) <- appliedProcessEnvironmentPublications (Genesis.checkedSystemId (startupGenesis state)) bootstraps bootstrap]
        <> [Store.retainedStoreObservationPublication observation | slot <- Store.retainedStoreSlots (startupStoreState state), observation <- Store.storeSlotApplicationObservations slot]
    purgePublication store publication = case Registry.lookupEffectiveSort (checkedPublicationSort publication) registry of
      Nothing -> pure store
      Just entry
        | descriptorKind (canonicalCheckedDescriptor (Registry.registryEntryDescriptor entry)) /= ControlledSort -> pure store
        | otherwise -> do
            observation <- mapProblem ControlBaseObservationProblem (Controlled.checkControlledObservation (Registry.registryEntryDescriptor entry) (Registry.registryEntryOccurrenceId entry) publication)
            case Controlled.controlledTerminalDeletion (Controlled.checkedControlledObservationObject observation) controlled of
              Nothing -> pure store
              Just deletion -> purge store deletion (checkedPublicationSort publication) (checkedPublicationKey publication)
    purgeBootstrap store (object, deletion) =
      let role = case (Controlled.controlledProcessFact (processEpochIdFromGlobalObjectId object) controlled, Controlled.controlledWriterFact (nablaIdFromGlobalObjectId object) controlled, Controlled.controlledReaderFact (deltaIdFromGlobalObjectId object) controlled) of
            (Just _, Nothing, Nothing) -> Just ProcessEpochRole
            (Nothing, Just _, Nothing) -> Just NablaRole
            (Nothing, Nothing, Just _) -> Just DeltaRole
            _ -> Nothing
       in case role of
            Nothing -> pure store
            Just known -> purge store deletion (profileSortFor known) (controlledObjectKey (globalUniqueIdFromGlobalObjectId object))
    purge store deletion sortId objectKey = do
      prepared <- mapProblem ControlBaseControlledPurgeProblem (Store.prepareControlledObjectPurge (Controlled.controlledTerminalDeletionCause deletion) sortId objectKey store)
      pure (fst (Store.commitControlledObjectPurge prepared))

restoreCurrentControlOwners :: HeraldState -> Either ControlBaseProblem HeraldState
restoreCurrentControlOwners state = do
  let view = Projection.oracleView (startupOracleProjectionState state)
      configuration = Projection.oracleViewVoterConfiguration view
      priorQuorums = Isolation.isolationWitnessVoterQuorums (Isolation.stateWitness (startupIsolationState state))
      (oldHosts, newHosts) = maybe priorQuorums configurationHosts configuration
  client <- mapProblem ControlBaseHeraldInvariant (OracleAdvance.refreshOracleRoutingFromRegistrations (Projection.oracleViewOracleReplicas view) view (startupOracleClientState state))
  (isolation, _) <- mapProblem ControlBaseIsolationProblem (Isolation.observeVoterConfiguration (startupLastObservedTime state) oldHosts newHosts Set.empty (startupIsolationState state))
  (failureDetection, actions) <- mapProblem ControlBaseFailureDetectionProblem (FailureDetection.observeVoterConfiguration oldHosts newHosts (Voter.voterConfigurationId <$> configuration) (Projection.oracleViewPendingVoterChange view /= Nothing) (startupFailureDetectionState state))
  unless (null actions) (Left ControlBaseReceiverNotFreshObserver)
  let application = startupApplicationState state
      preparation =
        ProcessPreparation.synchronizePreparationContext
          (ProcessPreparation.PreparationContext (Projection.oracleViewCurrentHeraldMembershipId view) False (Application.applicationGatePhase application) True (Client.oracleClientCurrentBinding client))
          (startupProcessPreparationState state)
  pure (replaceStartupOracleClientState client . replaceStartupIsolationState isolation . replaceStartupFailureDetectionState failureDetection . replaceStartupProcessPreparationState preparation $ state)
  where
    hosts = Set.fromList . map raftVoterBindingHeraldEpoch . Voter.oracleVoterBindingList
    configurationHosts configuration = case Voter.voterConfigurationView configuration of
      Voter.StableVoterConfigurationView bindings -> (hosts bindings, Nothing)
      Voter.JointVoterConfigurationView old new -> (hosts old, Just (hosts new))

mapProblem :: (problem -> ControlBaseProblem) -> Either problem value -> Either ControlBaseProblem value
mapProblem problem = either (Left . problem) Right
