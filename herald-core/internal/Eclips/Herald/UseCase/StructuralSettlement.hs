-- | Monotone settlement of source-local structural operations after an
-- established topology cut is installed.
--
-- Publication owns the durable semantic stabilization independently of the
-- application session.  Application owns a reply only while its request is
-- still retained; session retirement therefore makes terminalization a
-- successful delivery-free no-op without discarding the publication outcome.
module Eclips.Herald.UseCase.StructuralSettlement
  ( StructuralSettlementProblem (..),
    environmentStructuralCoordinateLineageEvidence,
    environmentStructuralStageLineageEvidence,
    environmentManifestIsReady,
    environmentManifestSettlementEvidence,
    settleInstalledTopologyCuts,
    settleReadyProcessStarts,
  )
where

import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Environment (EnvironmentRootClaimView (..), environmentRootClaimView, environmentRootSlotClaim)
import Eclips.Domain.Identity
  ( ProcessEpochId,
    TopologyCutId,
    deltaIdFromGlobalObjectId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.Publication (checkedPublicationId, checkedPublicationSort)
import Eclips.Domain.Route
  ( ReplicaStrength (Normal),
    destinationHerald,
    routeDestinations,
  )
import Eclips.Domain.Sort.Profile (profileSortFor)
import Eclips.Domain.Structural
  ( structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence (structuralOccurrenceCause)
import Eclips.Domain.Topology
  ( MemberSetDigest,
    topologyCutFrontier,
    topologyFrontierStructuralVersionVector,
  )
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed),
  )
import Eclips.Herald.Application.Session.Internal
  ( applicationAttachmentForProcess,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (SendApplicationReply),
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.PeerPublication
  ( structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
  )
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAdministrationState,
    replaceStartupApplicationState,
    replaceStartupConfiguredProcessState,
    replaceStartupProcessPreparationState,
    replaceStartupPublicationState,
    replaceStartupStoreState,
    startupAdministrationState,
    startupApplicationState,
    startupConfiguredProcessState,
    startupControlledState,
    startupGenesis,
    startupProcessPreparationState,
    startupPublicationState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.UseCase.NewEnvironment qualified as NewEnvironment

data StructuralSettlementProblem
  = StructuralSettlementPublicationProblem Publication.PublicationPreparationError
  | StructuralSettlementApplicationProblem Application.ApplicationSessionTransitionError
  | StructuralSettlementApplicationBootstrapProblem Application.ApplicationBootstrapError
  | StructuralSettlementAdministrationProblem Administration.AdministrationInvariant
  | StructuralSettlementConfiguredProblem Configured.ConfiguredScaffoldProblem
  | StructuralSettlementConfiguredScaffoldMissing
  | StructuralSettlementConfiguredPhaseContradiction ProcessEpochId
  | StructuralSettlementEnvironmentApplicationProblem Application.EnvironmentSettlementError
  | StructuralSettlementEnvironmentConstructionProblem NewEnvironment.NewEnvironmentFailure
  | StructuralSettlementEnvironmentStoreProblem Store.StoreTransitionError
  | StructuralSettlementEnvironmentEvidenceContradiction Environment.EnvironmentManifestKey
  deriving stock (Eq, Show)

-- | Settle the newly covered local operations for each just-installed cut.
-- Remote occurrences have no source-local stamped stage and are ignored.
-- Reoffering the same installed cut is idempotent and allocates no reply.
settleInstalledTopologyCuts ::
  [TopologyCutId] ->
  HeraldState ->
  Either StructuralSettlementProblem (HeraldState, EffectBatch)
settleInstalledTopologyCuts cuts initial = do
  (afterStabilization, reverseEffects) <- foldM settleCut (initial, []) cuts
  (afterEnvironments, environmentEffects) <-
    settleReadyEnvironments cuts afterStabilization
  Right
    ( afterEnvironments,
      orderedEffectBatch (reverse reverseEffects)
        <> environmentEffects
    )
  where
    settleCut (state, reverseEffects) cut =
      foldM
        (settleOccurrence cut)
        (state, reverseEffects)
        ( GraphProgress.topologyCutNewlyCoveredOccurrences
            cut
            (startupStructuralProgressState state)
        )

    settleOccurrence cut (state, reverseEffects) occurrence =
      case Publication.lookupStampedStructuralStage
        occurrence
        (startupPublicationState state) of
        Nothing -> Right (state, reverseEffects)
        Just stage -> do
          preparedPublication <-
            mapLeft
              StructuralSettlementPublicationProblem
              ( Publication.prepareStructuralStabilization
                  occurrence
                  cut
                  (startupPublicationState state)
              )
          case Publication.preparedStructuralStabilizationClassification preparedPublication of
            Publication.StructuralStabilizationExactRetry ->
              Right (state, reverseEffects)
            Publication.StructuralStabilizationFirstRetained ->
              case Publication.stampedStructuralApplicationRecordMaybe stage of
                Nothing ->
                  let (publication, _, _) =
                        Publication.commitStructuralStabilization preparedPublication
                   in Right
                        (replaceStartupPublicationState publication state, reverseEffects)
                Just applicationRecord -> do
                  let position =
                        Publication.applicationPublicationAcceptancePosition applicationRecord
                      result =
                        Publication.applicationPublicationTerminalResult applicationRecord
                  preparedApplication <-
                    mapLeft
                      StructuralSettlementApplicationProblem
                      ( Application.prepareApplicationOperationCompletion
                          position
                          result
                          (startupApplicationState state)
                      )
                  let (publication, _, _) =
                        Publication.commitStructuralStabilization preparedPublication
                      (application, outcome) =
                        Application.commitApplicationOperationCompletion preparedApplication
                      successor =
                        replaceStartupApplicationState application
                          . replaceStartupPublicationState publication
                          $ state
                      effects = case outcome of
                        Application.ApplicationOperationNoLiveRequest _ -> reverseEffects
                        Application.ApplicationOperationCompleted Nothing -> reverseEffects
                        Application.ApplicationOperationCompleted (Just observation) ->
                          SendApplicationReply
                            (Application.applicationOperationCompletionBinding observation)
                            ( RetainedRequestReply
                                (Application.applicationOperationCompletionCursor observation)
                                ( Completed
                                    (Application.applicationOperationCompletionRequestId observation)
                                    (Application.applicationOperationCompletionResult observation)
                                )
                            )
                            : reverseEffects
                  Right (successor, effects)

-- | Advance every construction whose positioned prefix is covered by a newly
-- installed cut. Endpoint settlement appends wiring through the fresh writers;
-- complete settlement installs the initial context and exposes private handles.
settleReadyEnvironments ::
  [TopologyCutId] ->
  HeraldState ->
  Either StructuralSettlementProblem (HeraldState, EffectBatch)
settleReadyEnvironments cuts initial =
  foldM settleOne (initial, orderedEffectBatch []) pending
  where
    pending = Application.applicationPendingEnvironmentEntries (startupApplicationState initial)

    settleOne (state, effects) (_, retained) = do
      let manifest = Environment.pendingEnvironmentManifest retained
      selected <- selectEnvironmentSettlementEvidence cuts manifest state
      case selected of
        Nothing -> Right (state, effects)
        Just evidence
          | not (Environment.environmentManifestComplete manifest),
            not (Controlled.controlledProcessEnded (Environment.environmentManifestProcess manifest) (startupControlledState state)) -> do
              seeded <- seedEnvironmentStores manifest evidence state
              successor <-
                mapLeft
                  StructuralSettlementEnvironmentConstructionProblem
                  (NewEnvironment.expandNewEnvironment manifest seeded)
              Right (successor, effects)
        Just evidence -> do
          seeded <- seedEnvironmentStores manifest evidence state
          prepared <-
            mapLeft
              StructuralSettlementEnvironmentApplicationProblem
              ( Application.prepareEnvironmentSettlement
                  evidence
                  manifest
                  (startupApplicationState seeded)
              )
          let (application, _) = Application.commitEnvironmentSettlement prepared
              successor =
                replaceStartupProcessPreparationState
                  ( Preparation.notifyPreparationDependencies
                      (Set.singleton (Preparation.ProcessChanged (Environment.environmentManifestProcess manifest)))
                      (startupProcessPreparationState seeded)
                  )
                  (replaceStartupApplicationState application seeded)
              completionEffects =
                case Application.preparedEnvironmentSettlementCompletionOutcome prepared of
                  Application.ApplicationOperationNoLiveRequest _ -> orderedEffectBatch []
                  Application.ApplicationOperationCompleted Nothing -> orderedEffectBatch []
                  Application.ApplicationOperationCompleted (Just observation) ->
                    orderedEffectBatch
                      [ SendApplicationReply
                          (Application.applicationOperationCompletionBinding observation)
                          ( RetainedRequestReply
                              (Application.applicationOperationCompletionCursor observation)
                              ( Completed
                                  (Application.applicationOperationCompletionRequestId observation)
                                  (Application.applicationOperationCompletionResult observation)
                              )
                          )
                      ]
          Right (successor, effects <> completionEffects)

-- | Seed only this environment's own structural stores with the exact accepted
-- descriptions and the shared sort catalogue. Their routes predate the connected
-- environment; this retained bundle is its initial context, not a change to those
-- frozen publication routes.
seedEnvironmentStores ::
  Environment.EnvironmentManifest ->
  Environment.EnvironmentSettlementEvidence ->
  HeraldState ->
  Either StructuralSettlementProblem HeraldState
seedEnvironmentStores manifest evidence state
  | Controlled.controlledProcessEnded process (startupControlledState state) = Right state
  | otherwise = do
      stages <- maybe contradiction Right (indexedStampedEnvironmentStages manifest evidence state)
      observations <- traverse observe stages
      destinations <- traverse (destination observations) readers
      prepared <-
        mapLeft
          StructuralSettlementEnvironmentStoreProblem
          (Store.prepareEnvironmentStoreSeed process destinations (startupStoreState state))
      Right (replaceStartupStoreState (Store.commitEnvironmentStoreSeed prepared) state)
  where
    process = Environment.environmentManifestProcess manifest
    contradiction :: Either StructuralSettlementProblem a
    contradiction = Left (StructuralSettlementEnvironmentEvidenceContradiction (Environment.environmentManifestKeyOf manifest))
    readers =
      [ (deltaIdFromGlobalObjectId (Environment.environmentRootPlanTargetObject plan), role)
      | positioned <- NonEmpty.toList (Environment.environmentManifestRoots manifest),
        let plan = Environment.positionedEnvironmentRootPlan positioned,
        EnvironmentReaderRootClaimView role <- [environmentRootClaimView (environmentRootSlotClaim (Environment.environmentRootPlanSlot plan))]
      ]
    destination observations (delta, role) = do
      slot <- maybe contradiction Right (Store.lookupStoreSlot delta (startupStoreState state))
      catalogue <-
        traverse
          ( \publication ->
              either
                (const contradiction)
                Right
                ( Store.retainedStoreObservation
                    publication
                    Normal
                    (Store.storeSlotOccurrenceId slot)
                    Store.PrimordialStoreObservation
                )
          )
          [ publication
          | publication <- Store.primordialSeedPublications (startupStoreState state),
            checkedPublicationSort publication == profileSortFor role
          ]
      Right
        ( delta,
          Store.storeSlotIncarnation slot,
          catalogue <> filter ((== profileSortFor role) . checkedPublicationSort . Store.retainedStoreObservationPublication) observations
        )
    observe stage = case Publication.stampedStructuralEnvironmentRoot stage of
      Nothing -> contradiction
      Just (_, root) ->
        let plan = Environment.positionedEnvironmentRootPlan root
            envelope =
              Store.storeObservationEnvelope
                process
                (Environment.environmentRootPlanOccurrenceId plan)
                (Environment.environmentRootPlanSourceTopologyPrerequisite plan)
                (Environment.environmentRootPlanControlPrerequisite plan)
                (Just (Publication.stampedStructuralStamp stage))
         in either
              (const contradiction)
              Right
              ( Store.retainedStoreObservation
                  (Environment.positionedEnvironmentRootChecked root)
                  Normal
                  (Environment.environmentRootPlanOccurrenceId plan)
                  (Store.RoutedStoreObservation envelope)
              )

stageRetainsEnvironmentRoot ::
  Environment.EnvironmentManifest ->
  Environment.PositionedEnvironmentRoot ->
  Publication.StampedStructuralStage ->
  Bool
stageRetainsEnvironmentRoot manifest root stage = case Publication.stampedStructuralEnvironmentRoot stage of
  Just (prefix, retained) -> Environment.environmentManifestExtends manifest prefix && retained == root
  Nothing -> False

-- | Readiness is deliberately a common-cut proof, rather than separate
-- independent stabilization observations.  The latter could otherwise admit
-- roots covered by unrelated generations without ever establishing one exact
-- complete environment.
environmentManifestIsReady ::
  [TopologyCutId] ->
  Environment.EnvironmentManifest ->
  HeraldState ->
  Either StructuralSettlementProblem Bool
environmentManifestIsReady cuts manifest state =
  maybe False (const True)
    <$> selectEnvironmentSettlementEvidence cuts manifest state

-- | Select the first offered exact common-cut witness.  The caller supplies
-- only newly installed cuts, so this path never searches retained history.
selectEnvironmentSettlementEvidence ::
  [TopologyCutId] ->
  Environment.EnvironmentManifest ->
  HeraldState ->
  Either
    StructuralSettlementProblem
    (Maybe Environment.EnvironmentSettlementEvidence)
selectEnvironmentSettlementEvidence cuts manifest state =
  case discoverStampedEnvironmentStages manifest state of
    Nothing -> Right Nothing
    Just stamped
      | not (all (stageIsStable publication) stamped) -> Right Nothing
      | otherwise -> firstAdmissible stamped cuts
  where
    publication = startupPublicationState state

    firstAdmissible _ [] = Right Nothing
    firstAdmissible stamped (cut : remaining) = do
      admitted <- cutAdmitsManifest manifest stamped cut state
      case admitted of
        Nothing -> firstAdmissible stamped remaining
        Just evidence -> Right (Just evidence)

-- | The one-time settlement path discovers the structural occurrences behind
-- the positioned manifest objects.  The selected ordered IDs are then retained in the
-- terminal witness, so validation never repeats this search.
discoverStampedEnvironmentStages ::
  Environment.EnvironmentManifest ->
  HeraldState ->
  Maybe [Publication.StampedStructuralStage]
discoverStampedEnvironmentStages manifest state =
  traverse stampedForRoot roots
  where
    roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
    stages = Publication.stampedStructuralStageEntries (startupPublicationState state)

    stampedForRoot root =
      snd
        <$> find
          (matchesRoot root . snd)
          stages

    matchesRoot root stage =
      stageRetainsEnvironmentRoot manifest root stage
        && checkedPublicationId (Publication.stampedStructuralChecked stage)
          == checkedPublicationId (Environment.positionedEnvironmentRootChecked root)

-- | Resolve the retained structural occurrence IDs through the
-- Publication index and verify their manifest/root pairing.
indexedStampedEnvironmentStages ::
  Environment.EnvironmentManifest ->
  Environment.EnvironmentSettlementEvidence ->
  HeraldState ->
  Maybe [Publication.StampedStructuralStage]
indexedStampedEnvironmentStages manifest evidence state =
  if length roots /= length occurrences
    then Nothing
    else traverse stampedForRoot (zip roots occurrences)
  where
    roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
    occurrences =
      NonEmpty.toList
        (Environment.environmentSettlementEvidenceOccurrences evidence)
    publication = startupPublicationState state

    stampedForRoot (root, occurrence) = do
      stage <-
        Publication.lookupStampedStructuralStage
          occurrence
          publication
      if stageRetainsEnvironmentRoot manifest root stage
        && checkedPublicationId (Publication.stampedStructuralChecked stage)
          == checkedPublicationId (Environment.positionedEnvironmentRootChecked root)
        then Just stage
        else Nothing

stageIsStable :: Publication.State -> Publication.StampedStructuralStage -> Bool
stageIsStable publication stage =
  Publication.lookupStructuralStabilization
    ( structuralOccurrenceStampOccurrence
        (Publication.stampedStructuralStamp stage)
    )
    publication
    /= Nothing

cutAdmitsManifest ::
  Environment.EnvironmentManifest ->
  [Publication.StampedStructuralStage] ->
  TopologyCutId ->
  HeraldState ->
  Either
    StructuralSettlementProblem
    (Maybe Environment.EnvironmentSettlementEvidence)
cutAdmitsManifest manifest stamped cut state =
  case GraphProgress.lookupInstalledTopologyCut cut progress of
    Nothing -> Right Nothing
    Just installed ->
      let vector =
            topologyFrontierStructuralVersionVector
              ( topologyCutFrontier
                  (GraphProgress.installedTopologyCutCut installed)
              )
          cutGeneration = structuralVersionVectorMembershipGenerationId vector
          cutDigest = structuralVersionVectorMemberSetDigest vector
          admissionGeneration =
            Environment.environmentManifestMembershipGenerationId manifest
          admissionDigest =
            Environment.environmentManifestActiveMemberSetDigest manifest
          coversAll =
            all
              ( \stage ->
                  GraphProgress.installedCutCoversCause
                    cut
                    ( structuralOccurrenceCause
                        ( structuralOccurrenceStampOccurrence
                            (Publication.stampedStructuralStamp stage)
                        )
                    )
                    progress
              )
              stamped
       in if not coversAll
            then Right Nothing
            else
              if cutGeneration == admissionGeneration
                then
                  if cutDigest /= admissionDigest
                    then evidenceContradiction manifest
                    else do
                      validateStampedProjection
                        manifest
                        stamped
                        state
                      Just <$> makeRetainedEvidence cutGeneration cutDigest
                else case descendantEvidence manifest cutGeneration state of
                  Nothing -> Right Nothing
                  Just (successorMembership, _) ->
                    if cutDigest
                      /= heraldMembershipGenerationActiveMemberSetDigest successorMembership
                      then evidenceContradiction manifest
                      else do
                        validateStampedProjection
                          manifest
                          stamped
                          state
                        Just <$> makeRetainedEvidence cutGeneration cutDigest
  where
    progress = startupStructuralProgressState state
    makeRetainedEvidence generation digest = do
      occurrences <-
        maybe
          (evidenceContradiction manifest)
          Right
          ( NonEmpty.nonEmpty
              ( structuralOccurrenceStampOccurrence
                  . Publication.stampedStructuralStamp
                  <$> stamped
              )
          )
      either
        (const (evidenceContradiction manifest))
        Right
        ( Environment.environmentSettlementEvidence
            manifest
            cut
            generation
            digest
            occurrences
        )

validateStampedProjection ::
  Environment.EnvironmentManifest ->
  [Publication.StampedStructuralStage] ->
  HeraldState ->
  Either StructuralSettlementProblem ()
validateStampedProjection manifest stamped state =
  if all validStage stamped
    then Right ()
    else evidenceContradiction manifest
  where
    admissionGeneration =
      Environment.environmentManifestMembershipGenerationId manifest
    admissionDigest =
      Environment.environmentManifestActiveMemberSetDigest manifest
    local = checkedLocalHeraldEpoch (startupGenesis state)

    validStage stage =
      let predecessor =
            structuralOccurrenceStampPredecessor
              (Publication.stampedStructuralStamp stage)
          stageGeneration =
            structuralVersionVectorMembershipGenerationId predecessor
          stageDigest = structuralVersionVectorMemberSetDigest predecessor
          routePeers =
            Set.fromList
              [ destinationHerald destination
              | destination <- routeDestinations (Publication.stampedStructuralRoute stage),
                destinationHerald destination /= local
              ]
          actualPeers =
            Map.keysSet (Publication.stampedStructuralPeerPublications stage)
       in if stageGeneration == admissionGeneration
            then stageDigest == admissionDigest && actualPeers == routePeers
            else case descendantEvidence manifest stageGeneration state of
              Nothing -> False
              Just (successorMembership, _) ->
                let successorDigest =
                      heraldMembershipGenerationActiveMemberSetDigest successorMembership
                    successorMembers =
                      Set.fromList
                        ( NonEmpty.toList
                            ( heraldMembershipGenerationActiveHeraldEpochs
                                successorMembership
                            )
                        )
                 in stageDigest == successorDigest
                      && actualPeers == Set.delete local successorMembers

evidenceContradiction ::
  Environment.EnvironmentManifest ->
  Either StructuralSettlementProblem value
evidenceContradiction =
  Left
    . StructuralSettlementEnvironmentEvidenceContradiction
    . Environment.environmentManifestKeyOf

-- | A stamped environment source must still name either its immutable
-- admission membership coordinate or a checked installed descendant. Every
-- retained stamp keeps the member set under which it was allocated, even when
-- a later established cut settles roots stamped in several generations.
-- This check applies before aggregate settlement, and therefore
-- covers both pending and completed manifests.
environmentStructuralStageLineageEvidence ::
  Environment.EnvironmentManifest ->
  Publication.StampedStructuralStage ->
  HeraldState ->
  Bool
environmentStructuralStageLineageEvidence manifest stage state =
  environmentStructuralCoordinateLineageEvidence
    manifest
    stageGeneration
    stageDigest
    state
  where
    predecessor =
      structuralOccurrenceStampPredecessor
        (Publication.stampedStructuralStamp stage)
    stageGeneration = structuralVersionVectorMembershipGenerationId predecessor
    stageDigest = structuralVersionVectorMemberSetDigest predecessor

-- | Package-internal coordinate law underlying the stamped-stage invariant.
-- Keeping extraction separate makes the captured/installed-descendant
-- truth table testable without fabricating contradictory copies across the
-- Publication, Graph, and PeerStream owners.
environmentStructuralCoordinateLineageEvidence ::
  Environment.EnvironmentManifest ->
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  HeraldState ->
  Bool
environmentStructuralCoordinateLineageEvidence manifest stageGeneration stageDigest state =
  if stageGeneration == admissionGeneration
    then stageDigest == admissionDigest
    else case descendantEvidence manifest stageGeneration state of
      Nothing -> False
      Just (successorMembership, _) ->
        stageDigest
          == heraldMembershipGenerationActiveMemberSetDigest successorMembership
  where
    admissionGeneration =
      Environment.environmentManifestMembershipGenerationId manifest
    admissionDigest =
      Environment.environmentManifestActiveMemberSetDigest manifest

-- | Read-only composed evidence used by the whole-Herald invariant.  Validation
-- resolves one retained cut and the fixed twelve exact stage/stabilization
-- indexes; it never searches the installed-cut chain.
environmentManifestSettlementEvidence ::
  Environment.CompletedEnvironment ->
  HeraldState ->
  Either StructuralSettlementProblem Bool
environmentManifestSettlementEvidence completed state =
  case indexedStampedEnvironmentStages manifest retainedEvidence state of
    Nothing -> Right False
    Just stamped
      | not (all (stageIsStable publication) stamped) -> Right False
      | otherwise -> do
          admitted <- cutAdmitsManifest manifest stamped retainedCut state
          Right (admitted == Just retainedEvidence)
  where
    manifest = Environment.completedEnvironmentManifest completed
    retainedEvidence =
      Environment.completedEnvironmentSettlementEvidence completed
    retainedCut = Environment.environmentSettlementEvidenceCut retainedEvidence
    publication = startupPublicationState state

-- | Membership ancestry alone cannot authorize settlement. The Graph owner
-- supplies a witness grounded in its retained established base/cut chain and
-- terminal-source evidence; completed historical targets remain valid after
-- another membership change starts a new physical closure attempt.
descendantEvidence ::
  Environment.EnvironmentManifest ->
  HeraldMembershipGenerationId ->
  HeraldState ->
  Maybe (HeraldMembershipGeneration, GraphProgress.StructuralDescendantSettlement)
descendantEvidence manifest cutGeneration state = do
  evidence <-
    GraphProgress.structuralDescendantSettlement
      (Environment.environmentManifestMembershipGenerationId manifest)
      cutGeneration
      (startupStructuralProgressState state)
  let lineage = GraphProgress.structuralDescendantSettlementLineage evidence
      origin = heraldMembershipLineageOrigin lineage
      target = heraldMembershipLineageTarget lineage
  if heraldMembershipGenerationActiveMemberSetDigest origin
    /= Environment.environmentManifestActiveMemberSetDigest manifest
    then Nothing
    else Just (target, evidence)

-- | Install empty selected access immediately after canonical Start installs
-- the process fact. Readiness has no structural-root or topology-cut dependency.
settleReadyProcessStarts ::
  HeraldState ->
  Either StructuralSettlementProblem HeraldState
settleReadyProcessStarts initial =
  foldM settleOne initial readyEntries
  where
    readyEntries =
      [ (correlation, reference)
      | ( correlation,
          _,
          _,
          Administration.ConfiguredStartProcessInstalled _ _ reference
          ) <-
          Administration.configuredStartEntries
            (startupAdministrationState initial)
      ]

    settleOne state (correlation, reference) = do
      let process = Configured.liveProcessScaffoldRefProcessEpoch reference
      _ <-
        maybe
          (Left StructuralSettlementConfiguredScaffoldMissing)
          Right
          ( Configured.lookupLiveProcessScaffold
              process
              (startupConfiguredProcessState state)
          )
      settlement <-
        maybe
          (Left (StructuralSettlementConfiguredPhaseContradiction process))
          Right
          ( Configured.configuredAttachmentSettlement
              process
              (startupConfiguredProcessState state)
          )
      case settlement of
        Configured.ConfiguredEndedBeforeAttachment {} ->
          Right state
        Configured.ConfiguredAttachmentReady ->
          Left (StructuralSettlementConfiguredPhaseContradiction process)
        Configured.ConfiguredAwaitingAttachment -> settleReady process state
      where
        settleReady process predecessor = do
          let attachment = applicationAttachmentForProcess process
          preparedConfigured <-
            mapLeft
              StructuralSettlementConfiguredProblem
              ( Configured.prepareConfiguredAttachmentReady
                  process
                  (startupConfiguredProcessState predecessor)
              )
          case Configured.preparedConfiguredAttachmentReadyClassification preparedConfigured of
            Configured.ConfiguredAttachmentReadySuppressedByEnd {} ->
              Right predecessor
            Configured.ConfiguredAttachmentReadyExactRetry ->
              Left (StructuralSettlementConfiguredPhaseContradiction process)
            Configured.ConfiguredAttachmentFirstReady -> do
              preparedApplication <-
                mapLeft
                  StructuralSettlementApplicationBootstrapProblem
                  ( Application.prepareApplicationProcessRegistration
                      attachment
                      process
                      (startupApplicationState predecessor)
                  )
              preparedAdministration <-
                mapLeft
                  StructuralSettlementAdministrationProblem
                  ( Administration.prepareConfiguredStartCompletion
                      correlation
                      reference
                      attachment
                      (startupAdministrationState predecessor)
                  )
              case Administration.preparedConfiguredStartCompletionClassification preparedAdministration of
                Administration.ConfiguredStartFirstCompleted {} -> Right ()
                _ -> Left (StructuralSettlementConfiguredPhaseContradiction process)
              let (application, _) =
                    Application.commitApplicationBootstrap preparedApplication
                  (configuredProcesses, configuredClassification) =
                    Configured.commitConfiguredAttachmentReady preparedConfigured
                  (administration, _) =
                    Administration.commitConfiguredStartCompletion preparedAdministration
                  successor =
                    replaceStartupAdministrationState administration
                      . replaceStartupApplicationState application
                      . replaceStartupConfiguredProcessState configuredProcesses
                      $ predecessor
              case configuredClassification of
                Configured.ConfiguredAttachmentFirstReady ->
                  Right successor
                _ -> Left (StructuralSettlementConfiguredPhaseContradiction process)

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
