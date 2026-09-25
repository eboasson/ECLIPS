{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module JoinAlignmentHistoryProperties (tests) where

import ConfiguredProcessProperties qualified as Configured
import Control.Monad (forM_)
import Data.ByteString qualified as Bytes
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Private
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Operation (ApplicationOperation (NewEnvironmentApplication, NewIdApplication, WriteApplication))
import Eclips.Application.Types.Result (RegularCallResult (NewEnvironmentCompleted, NewIdCompleted, WriteCompleted))
import Eclips.Application.Types.Value qualified as Value
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue), WriteResult (WriteAccepted))
import Eclips.Domain.Alignment qualified as Domain
import Eclips.Domain.Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Publication qualified as Publication
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup (appliedProcessEpochId)
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.Topology (deriveTopologyCutId, topologyCutFrontier, topologyFrontierAppliedControlPrefix, topologyFrontierMembershipGenerationId)
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.History qualified as AlignmentHistory
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Application.PrivateIdentity (witnessedProcessEpoch)
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Initialization (initialHerald, primordialApplicationAttachment)
import Eclips.Herald.Input
import Eclips.Herald.Join qualified as Join
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.State qualified as JoinState
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (SortOccurrence, sortOccurrence)
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.Alignment qualified as Coordinator
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ControlBase qualified as ControlBase
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerDelivery qualified as PeerDelivery
import Eclips.Herald.UseCase.StructuralCoordinator qualified as Structural
import Eclips.Oracle.Admission
import Eclips.Oracle.Command (endProcessEpochCommand)
import Eclips.Oracle.State
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GenesisFixtures qualified as Fixtures
import JoinHistoryProperties (advance, checked, deliverEntry, request, submit, submitCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "join after application alignment"
    [ testGroup
        name
        [ testCase "canonical genesis descriptions retain Normal and Weak strength through join history" (caseCanonicalGenesisHistory path),
          testCase "a lower-epoch newcomer inherits real application ancestry without old-owner work" (caseApplicationAncestry path FirstPlacementBeforeEnd),
          testCase "a lower-epoch newcomer receives its first live placement only after canonical End" (caseApplicationAncestry path FirstPlacementAfterEnd)
        ]
    | (name, path) <- [("history replay", ReplayHistory), ("portable source base", AdoptSourceBases)]
    ]

data ImportPath = ReplayHistory | AdoptSourceBases

importSource :: ImportPath -> History.JoinSourceHistory -> HeraldState -> IO (HeraldState, EffectBatch)
importSource ReplayHistory source observer = do
  let (imported, reply, effects) = request (Join.InstallJoinHistory (History.encodeJoinSourceHistory source)) observer
  assertEqual "real source history imports through the production coordinator" Join.JoinHistoryInstalled reply
  pure (imported, effects)
importSource AdoptSourceBases source observer =
  either
    (assertFailure . ("portable source collection: " <>) . show)
    pure
    (ControlBase.installPortableJoiningBases [(History.joinHistorySource (History.sourceJoinHistory source), History.encodeJoinSourceHistory source)] observer)

data FirstPlacementTiming = FirstPlacementBeforeEnd | FirstPlacementAfterEnd

-- A public reader-to-writer edge carries the context's canonical hub fact over
-- the writer's existing system-view route. Capture and import therefore exercise
-- real primordial-origin history after both preserving and weakening alignment.
caseCanonicalGenesisHistory :: ImportPath -> Assertion
caseCanonicalGenesisHistory path = forM_ [("preserve", Route.Normal), ("weaken", Route.Weak)] $ \(edgeStrength, strength) -> do
  let genesis = singletonGenesis
      bootstraps = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      initial = fst (checked (initialHerald (monotonicInstant 0) genesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH1 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      firstBootstrap = case Fixtures.fixtureLocalBootstrapIds of
        first : _ -> first
        [] -> error "missing canonical-history bootstrap"
      attachment = fromJust (primordialApplicationAttachment genesis bootstraps firstBootstrap)
  (bound, _, _) <- Configured.bindInitial genesis initial
  (opened, sessionEffects) <- Configured.step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane 82) attachment (Session.clientNonce 82))) bound
  (binding, session) <- case [(Session.sessionAcceptanceBinding acceptance, identifier) | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers sessionEffects, Session.SessionOpened identifier _ _ <- [Session.sessionAcceptanceReply acceptance]] of
    [one] -> pure one
    _ -> assertFailure "expected canonical-history application session"
  let process = fromJust (Application.applicationSessionProcess session (startupApplicationState opened))
      access = fromJust (Application.applicationBootstrapAccess process (startupApplicationState opened))
      entries = Access.accessEntries (Application.bootstrapAccessPrimordial access)
      writer role = case Map.lookup (Access.environmentWriterKey role) entries of
        Just (Access.Writer value) -> value
        _ -> error "canonical-history bootstrap writer missing"
      sourceReader = case Map.lookup (Access.environmentReaderKey Access.NeutralVertexRole) entries of
        Just (Access.Reader value) -> value
        _ -> error "canonical-history bootstrap reader missing"
      edgeWriter = writer Access.EdgeRole
      selected = case [bootstrap | bootstrap <- Genesis.checkedInitialBootstraps bootstraps, appliedProcessEpochId bootstrap == process] of
        [one] -> one
        _ -> error "canonical-history process bootstrap missing"
      publication = case [value | (_, _, value) <- Startup.appliedProcessEnvironmentPublications (Genesis.checkedSystemId genesis) (Genesis.checkedInitialBootstraps bootstraps) selected, Publication.checkedPublicationSort value == SortProfile.profileSortFor SortProfile.NeutralVertexRole] of
        [one] -> one
        _ -> error "canonical-history hub publication missing"
      publicationIdentifier = Publication.checkedPublicationId publication
      systemSlot state = case [slot | slot <- Store.storeSlots (startupStoreState state), Store.storeSlotProvenance slot == Store.HeraldSystemView (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) SortProfile.NeutralVertexRole] of
        [one] -> one
        _ -> error "canonical-history system view missing"
  assertEqual "system view initially lacks the application's canonical hub" Nothing (lookup publicationIdentifier (Store.storeSlotApplicationReceipts (systemSlot opened)))
  (reserved, reservationEffects) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 8201) (NewIdApplication (ControlledNewId edgeWriter)))) opened
  edgeIdentity <- case [identity | SendApplicationReply _ (Request.RetainedRequestReply _ (Request.Completed _ (NewIdCompleted identity))) <- effectBatchMembers reservationEffects] of
    [one] -> pure one
    _ -> assertFailure "canonical-history edge reservation did not complete"
  let edgeValue =
        Value.RecordValue
          $ Map.fromList
            [ ("destination_vertex", Value.UniqueIdValue (Private.privateNablaUniqueId (writer Access.NeutralVertexRole))),
              ("label", Value.LabelValue (Value.ProcessLabel (Application.bootstrapAccessProcess access), 0)),
              ("object_id", Value.UniqueIdValue edgeIdentity),
              ("source_vertex", Value.UniqueIdValue (Private.privateDeltaUniqueId sourceReader)),
              ("strength", Value.EnumValue edgeStrength)
            ]
  (pending, _) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 8202) (WriteApplication edgeWriter (PublishValue edgeValue)))) reserved
  aligned <- settle pending
  assertEqual "real alignment stores the hub at the path's exact strength" (Just strength) (lookup publicationIdentifier (Store.storeSlotApplicationReceipts (systemSlot aligned)))
  assertEqual "the application-created route preserves source invariants" (Right ()) (validateHeraldState aligned)
  let newcomer = checked (mkHeraldEpoch (Bytes.replicate 32 1))
      manifest = heraldAdmissionManifest (Genesis.checkedSystemId genesis) (checked (mkHeraldId (Bytes.replicate 32 0xed))) newcomer
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps genesis bootstraps))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews aligned) (startupStructuralProgressState aligned))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record = fromJust (oraclePendingHeraldAdmission oracle1)
  begun <- settle (deliverEntry beginEntry aligned)
  let (_, captureReply, _) = request (Join.CaptureJoinHistory (admissionRecordId record)) begun
  source <- case captureReply of
    Join.JoinHistoryReply bytes -> pure (checked (History.decodeJoinSourceHistory bytes))
    other -> assertFailure ("canonical-history capture was not ready: " <> show other)
  let joiningGenesis = checked (checkJoiningHeraldGenesis genesis record)
      observer = fst (checked (initialHerald (monotonicInstant 1) joiningGenesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  (imported, _) <- importSource path source observer
  let evidence = [value | (SortProfile.NeutralVertexRole, value) <- JoinState.importedSystemViewObservations (startupJoinState imported), Protocol.retainedPublicationEvidencePublicationId value == publicationIdentifier]
  case evidence of
    [one] -> do
      assertEqual "the imported hub retains canonical genesis provenance" Protocol.PrimordialRetainedObservation (Protocol.retainedPublicationEvidenceOrigin one)
      assertEqual "the imported hub retains its incoming strength" strength (Protocol.retainedPublicationEvidenceSourceStrength one)
    other -> assertFailure ("expected one canonical hub history observation, got " <> show other)
  assertEqual "the observer retains the exact publication receipt strength" (Just strength) (lookup publicationIdentifier (Store.storeSlotApplicationReceipts (systemSlot imported)))
  assertEqual "the observer's logical fact retains its strength" (Just strength) (DomainStore.logicalStateStrength (Publication.checkedPublicationLogicalState publication) (Store.storeSlotContents (systemSlot imported)))
  assertEqual "canonical genesis history leaves the observer valid" (Right ()) (validateHeraldState imported)

caseApplicationAncestry :: ImportPath -> FirstPlacementTiming -> Assertion
caseApplicationAncestry path firstPlacementTiming = do
  let genesis = singletonGenesis
      bootstraps = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      oldOwner = Genesis.checkedLocalHeraldEpoch genesis
      newcomer = checked (mkHeraldEpoch (Bytes.replicate 32 1))
      manifest = heraldAdmissionManifest (Genesis.checkedSystemId genesis) (checked (mkHeraldId (Bytes.replicate 32 0xed))) newcomer
      initial = fst (checked (initialHerald (monotonicInstant 0) genesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH1 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  (bound, _, _) <- Configured.bindInitial genesis initial
  let firstBootstrap = case Fixtures.fixtureLocalBootstrapIds of
        first : _ -> first
        [] -> error "missing configured application bootstrap"
      attachment = fromJust (primordialApplicationAttachment genesis bootstraps firstBootstrap)
  (opened, sessionEffects) <- Configured.step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane 81) attachment (Session.clientNonce 81))) bound
  (binding, session) <- case [(Session.sessionAcceptanceBinding acceptance, identifier) | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers sessionEffects, Session.SessionOpened identifier _ _ <- [Session.sessionAcceptanceReply acceptance]] of
    [one] -> pure one
    _ -> assertFailure "expected one real application session"
  let process = fromJust (Application.applicationSessionProcess session (startupApplicationState opened))
      environmentRequest = Request.requestId 8101
  (pending, _) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session environmentRequest NewEnvironmentApplication)) opened
  settled <- settle pending
  (_, resultEffects) <- Configured.step (ApplicationRequestInput (GetApplicationRequestResult binding session environmentRequest)) settled
  environment <- case [value | SendApplicationReply _ (Request.RetainedRequestReply _ (Request.Completed _ (NewEnvironmentCompleted value))) <- effectBatchMembers resultEffects] of
    [value] -> pure value
    _ -> assertFailure "the actual newenv call did not complete before joining"
  -- Connect the existing primordial neutral reader to the newenv reader through
  -- a real controlled Edge publication. A writer-only route change can carry
  -- the reader class; this Delta-to-Delta dependency deliberately changes its
  -- incoming history so the Join fixture contains actual created ancestry.
  access <- maybe (assertFailure "the live application lacks startup access") pure (Application.applicationBootstrapAccess process (startupApplicationState settled))
  edgeWriter <- case Map.lookup (Access.environmentWriterKey Access.EdgeRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)) of
    Just (Access.Writer writer) -> pure writer
    _ -> assertFailure "the primordial Edge writer is unavailable"
  sourceReader <- case Map.lookup (Access.environmentReaderKey Access.NeutralVertexRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)) of
    Just (Access.Reader reader) -> pure reader
    _ -> assertFailure "the primordial neutral reader is unavailable"
  endpoints <- case filter ((== Access.NeutralVertexRole) . Access.predefinedAccessRole) (Access.environmentAccessPredefined environment) of
    [value] -> pure value
    _ -> assertFailure "newenv did not return its neutral-sort endpoint pair"
  (reserved, reservationEffects) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 8102) (NewIdApplication (ControlledNewId edgeWriter)))) settled
  edgeIdentity <- case [identity | SendApplicationReply _ (Request.RetainedRequestReply _ (Request.Completed _ (NewIdCompleted identity))) <- effectBatchMembers reservationEffects] of
    [identity] -> pure identity
    _ -> assertFailure "the application did not reserve a controlled Edge identity"
  let edgeValue =
        Value.RecordValue
          ( Map.fromList
              [ ("destination_vertex", Value.UniqueIdValue (Private.privateDeltaUniqueId (Access.predefinedReader endpoints))),
                ("label", Value.LabelValue ((Value.ProcessLabel (Application.bootstrapAccessProcess access), 0))),
                ("object_id", Value.UniqueIdValue edgeIdentity),
                ("source_vertex", Value.UniqueIdValue (Private.privateDeltaUniqueId sourceReader)),
                ("strength", Value.EnumValue "preserve")
              ]
          )
      edgeRequest = Request.requestId 8103
      firstGenerationIds = Set.fromList (map fst (Alignment.alignmentGenerationEntries (startupAlignmentState settled)))
  (pendingEdge, _) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session edgeRequest (WriteApplication edgeWriter (PublishValue edgeValue)))) reserved
  connected <- settle pendingEdge
  assertEqual "the Edge is fully settled before process End" (Right ()) (validateHeraldState connected)
  (_, edgeResultEffects) <- Configured.step (ApplicationRequestInput (GetApplicationRequestResult binding session edgeRequest)) connected
  assertBool "the real controlled Edge publication completes" (not (null [() | SendApplicationReply _ (Request.RetainedRequestReply _ (Request.Completed _ (WriteCompleted WriteAccepted))) <- effectBatchMembers edgeResultEffects]))
  assertBool
    "connecting existing roots creates a generation dependent on an earlier certified reader class"
    (any (any (`Set.member` firstGenerationIds) . Domain.alignmentCutPredecessorGenerationIds . Generation.alignmentGenerationCut . snd) (Alignment.alignmentGenerationEntries (startupAlignmentState connected)))
  let oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps genesis bootstraps))
      unrelated = case [appliedProcessEpochId bootstrap | bootstrap <- Genesis.checkedInitialBootstraps bootstraps, appliedProcessEpochId bootstrap /= process] of
        otherProcess : _ -> otherProcess
        [] -> error "singleton fixture needs two configured local processes"
      (oracle1, endEntry) = submitCommand (checked (endProcessEpochCommand unrelated ExplicitAdministrativeEnd)) oracle0
  let afterEnd = deliverEntry endEntry connected
      closedDestinationReceipts = inactiveDestinationReceipts afterEnd
  assertBool "End moves an alignment destination out of the live Store view" (not (null closedDestinationReceipts))
  assertBool "every closed alignment destination retains its exact original Store receipt" (all (\(_, _, matches) -> matches) closedDestinationReceipts)
  assertEqual
    ("the normal process End transition preserves route receipts; inactive destination evidence (delta, incarnation, retained receipt matches): " <> show closedDestinationReceipts)
    (Right ())
    (validateHeraldState afterEnd)
  ended <- settle afterEnd
  let oldAlignment = startupAlignmentState ended
      oldGenerations = Alignment.alignmentGenerationEntries oldAlignment
  assertBool "real application work retains old generations" (not (null oldGenerations))
  assertBool "real local transfer has produced readiness evidence" (not (null (Alignment.alignmentLocalMemberReadinessEntries oldAlignment)))
  assertBool "real local transfer has certified old generations" (not (null (Alignment.alignmentHistoricalCertificateEntries oldAlignment)))
  assertBool
    "the fixture contains successor ancestry, not just unrelated initial cuts"
    (any (not . null . Domain.alignmentCutPredecessorGenerationIds . Generation.alignmentGenerationCut . snd) oldGenerations)
  assertBool "the newcomer sorts before the old authority" (newcomer < oldOwner)
  assertEqual "application and unrelated End preserve all owner invariants" (Right ()) (validateHeraldState ended)

  let anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews ended) (startupStructuralProgressState ended))
      (oracle2, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle1
      record = fromJust (oraclePendingHeraldAdmission oracle2)
  begun <- settle (deliverEntry beginEntry ended)
  let (captured, captureReply, _) = request (Join.CaptureJoinHistory (admissionRecordId record)) begun
  source <- case captureReply of
    Join.JoinHistoryReply bytes -> pure (checked (History.decodeJoinSourceHistory bytes))
    other -> assertFailure ("application history capture was not ready: " <> show other)
  let bytes = History.encodeJoinSourceHistory source
      history = History.sourceJoinHistory source
      exported = History.joinHistoryAlignmentHistory history
      joiningGenesis = checked (checkJoiningHeraldGenesis genesis record)
      observer = fst (checked (initialHerald (monotonicInstant 1) joiningGenesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  (imported, importEffects) <- importSource path source observer
  let importedAlignment = startupAlignmentState imported
      importedTransfer = Alignment.alignmentTransferState importedAlignment
      exportedPlans = AlignmentHistory.alignmentHistoryPlans exported
      exportedCuts = concatMap Protocol.alignmentPlanAnnounceCreatedCuts exportedPlans
  assertBool "capture includes actual old alignment plans" (not (null exportedCuts))
  assertBool "exported history exercises retained readiness payload encoding" (not (null (AlignmentHistory.alignmentHistoryReadiness exported)))
  assertBool "exported history exercises retained plan-acceptance payload encoding" (not (null (AlignmentHistory.alignmentHistoryPlanAcceptances exported)))
  assertEqual "alignment history itself roundtrips semantic evidence without delivery coordinates" (Right exported) (AlignmentHistory.decodeAlignmentHistory (AlignmentHistory.encodeAlignmentHistory exported))
  assertEqual "the enriched canonical source bundle roundtrips" (Right source) (History.decodeJoinSourceHistory bytes)
  forM_ exportedCuts $ \announce ->
    assertEqual
      "every old cut keeps its exact identity and predecessor ancestry"
      (Just (Protocol.alignmentCutAnnounceCut announce))
      (Generation.alignmentGenerationCut <$> Alignment.lookupAlignmentGeneration (Protocol.alignmentCutAnnounceGeneration announce) importedAlignment)
  forM_ exportedPlans $ \announce ->
    assertEqual
      "every routing plan retains its complete mixed-age bindings and predecessor policy"
      (Just announce)
      (Plan.alignmentPlanAnnouncement <$> Alignment.lookupAlignmentPlan (Protocol.alignmentPlanAnnounceId announce) importedAlignment)
  assertEqual "historical import creates no old-owner obligations" [] (Alignment.alignmentObligationEntries importedAlignment)
  assertEqual "historical import creates no old-owner bootstrap imports" [] (Alignment.bootstrapImportEntries importedAlignment)
  assertEqual "historical import creates no old-owner attempts" [] (Alignment.alignmentAttemptEntries importedAlignment)
  assertEqual "historical import creates no local readiness claim" [] (Alignment.alignmentLocalMemberReadinessEntries importedAlignment)
  assertEqual "historical import creates no newcomer cut acceptance" [] [accepted | ((_, herald), accepted) <- Alignment.alignmentCutAcceptanceEntries importedAlignment, herald == newcomer]
  assertEqual "historical import creates no newcomer plan acceptance" [] [accepted | ((_, herald), accepted) <- Alignment.alignmentPlanAcceptanceEntries importedAlignment, herald == newcomer]
  assertEqual "historical import creates no source subscription" [] (Transfer.sourceSubscriptionEntries importedTransfer)
  assertEqual "historical import creates no destination subscription" [] (Transfer.destinationSubscriptionEntries importedTransfer)
  assertEqual "import emits only its operator reply" [] [effect | effect <- effectBatchMembers importEffects, case effect of SendJoinReply {} -> False; _ -> True]
  assertEqual "import preserves the observer's current placement" (PlacementState.currentPlacementRouteEntries (startupPlacementState observer)) (PlacementState.currentPlacementRouteEntries (startupPlacementState imported))
  assertEqual "the passive imported owner validates" (Right ()) (validateHeraldState imported)
  let (repeated, repeatedReply, _) = request (Join.InstallJoinHistory bytes) imported
  assertEqual "repeated donor receives the same receipt" Join.JoinHistoryInstalled repeatedReply
  assertEqual "repeated donor does not alter alignment ownership" importedAlignment (startupAlignmentState repeated)
  assertBool "repeated donor does not alter the local Store" (startupStoreState imported == startupStoreState repeated)

  let seal = checked (Join.prepareJoinSeal record [source])
      sealed = advance (SealHeraldAdmission seal) (oracle2, captured, imported)
      accepted@(_, oldAccepted, newAccepted) = advance (AcceptHeraldJoinSeal (admissionRecordId record) (admissionRecordAttempt record) (joinSealDigest seal)) sealed
      ready state = case second (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply value -> value
        other -> error ("application-history join readiness failed: " <> show other)
      oldReady = advance (HeraldJoinBaseReady (ready oldAccepted)) accepted
      bothReady = advance (HeraldJoinReady (ready newAccepted)) oldReady
      (oracleFinal, oldActivated, newActivated) = advance (ActivateHerald (admissionRecordId record)) bothReady
  assertEqual "seal acceptance retains historical plans without an executable frontier" [] (Alignment.historicalAlignmentPlanFrontier (startupAlignmentState newAccepted))
  assertEqual "activation adopts the sealed donor's effective frontier" (AlignmentHistory.alignmentHistoryFrontier exported) (Alignment.historicalAlignmentPlanFrontier (startupAlignmentState newActivated))
  assertEqual "activation is a real lower-epoch membership successor" newcomer (minimum (NE.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership oracleFinal))))
  assertEqual "the old owner validates after activation" (Right ()) (validateHeraldState oldActivated)
  assertEqual "the newcomer validates after activation" (Right ()) (validateHeraldState newActivated)

  let oldWithPlacement = installRemotePlacement newActivated oldActivated
      newWithPlacement = installRemotePlacement oldActivated newActivated
      currentVector state = checked (PlacementState.currentPhysicalPlacementRevisionVector (Projection.oracleViewCurrentHeraldMembership (Projection.oracleView (startupOracleProjectionState state))) (startupPlacementState state))
      vector = currentVector oldWithPlacement
      currentCut = Progress.structuralLastInstalledCutId (startupStructuralProgressState oldWithPlacement)
      frontier = AlignmentHistory.alignmentHistoryFrontier exported
  assertEqual "both owners agree on the actual current placement vector" vector (currentVector newWithPlacement)
  assertEqual "both owners hold the same admitted topology cut" currentCut (Progress.structuralLastInstalledCutId (startupStructuralProgressState newWithPlacement))
  assertBool "the transferred effective frontier is nonempty" (not (null frontier))
  forM_ frontier $ \(occurrence, identifier) -> do
    oldPlan <- successorPlan occurrence identifier currentCut vector oldWithPlacement
    newPlan <- successorPlan occurrence identifier currentCut vector newWithPlacement
    assertEqual "both owners derive the same successor plan from real old ancestry" oldPlan newPlan
    assertBool
      "the successor belongs to the activated membership"
      (Membership.heraldMembershipGenerationId (oracleCurrentMembership oracleFinal) == topologyFrontierMembershipGenerationId (topologyCutFrontier (Plan.alignmentPlanTopologyCut newPlan)))
  announce <- case exportedPlans of
    first : _ -> pure first
    [] -> assertFailure "captured application ancestry lacks its original anchor"
  case Coordinator.applyHistoricalAlignmentPlan oldOwner announce oldWithPlacement of
    Left problem -> assertFailure ("old authority replay after lower-epoch activation: " <> show problem)
    Right (replayed, effects) -> do
      assertEqual "old-authority replay preserves the retained generation owner" (startupAlignmentState oldWithPlacement) (startupAlignmentState replayed)
      assertEqual "exact old-anchor replay emits no repeated owner work" [] (effectBatchMembers effects)
  assertBool "the new least epoch cannot impersonate the old plan authority" (isLeft (Coordinator.applyHistoricalAlignmentPlan newcomer announce oldWithPlacement))

  -- Exercise the serving driver and actual peer ingress as well as pure plan
  -- reconstruction. Imported old causes must not let the new least owner
  -- author a plan which its incumbent peer cannot admit from the same history.
  (oldReplayed, newReplayed, connectedBindings) <- case firstPlacementTiming of
    FirstPlacementBeforeEnd -> do
      (oldConnected, newConnected, oldBinding, newBinding) <- connectOwners oldActivated newActivated
      oldPlaced <- receivePlacement newConnected oldBinding oldConnected
      newPlaced <- receivePlacement oldPlaced newBinding newConnected
      (oldReplayed, newReplayed) <- exchangeAlignmentControls oldBinding newBinding oldPlaced newPlaced
      pure (oldReplayed, newReplayed, Just (oldBinding, newBinding))
    FirstPlacementAfterEnd -> do
      assertEqual "passive join history has not supplied a live incumbent placement" Nothing (PlacementState.remotePlacementSequence oldOwner (startupPlacementState newActivated))
      pure (oldActivated, newActivated, Nothing)
  let oldCapturedIds = Set.fromList (map Protocol.alignmentCutAnnounceGeneration exportedCuts)
      newcomerPlans = Map.withoutKeys (liveLatestPlans newReplayed) oldCapturedIds
  assertPlansAccepted
    "the incumbent admits every plan actually authored after importing old application causes"
    oldOwner
    newcomerPlans
    newReplayed
    oldReplayed

  -- A real later control cause makes this a positive handoff test even when
  -- correctly imported coverage produces no unnecessary post-join plan.
  -- Both owners receive the same canonical End and report that exact applied
  -- cut before ordinary placement and alignment messages are delivered.
  assertState "incumbent before the later application End" oldReplayed
  assertState "new authority before the later application End" newReplayed
  let (_, laterEndEntry) = submitCommand (checked (endProcessEpochCommand process ExplicitAdministrativeEnd)) oracleFinal
      beforeLaterEnd = Set.fromList (map fst (Alignment.alignmentGenerationEntries (startupAlignmentState newReplayed)))
      oldAfterEnd = deliverEntry laterEndEntry oldReplayed
      newAfterEnd = deliverEntry laterEndEntry newReplayed
      publishedReservations =
        [ reservation
        | reservation <- Controlled.controlledReservationWitnesses (startupControlledState oldReplayed),
          Controlled.reservationWitnessProcess reservation == process,
          Controlled.reservationWitnessPhase reservation == Controlled.Published
        ]
  assertBool "the ending application owns a real published NewId reservation" (not (null publishedReservations))
  assertBool "the canonical End records the creator as ended" (Controlled.controlledProcessEnded process (startupControlledState oldAfterEnd))
  assertBool "the live creator has its private identity namespace before End" (any ((== process) . witnessedProcessEpoch) (Application.applicationProcessWitnesses (startupApplicationState oldReplayed)))
  assertBool "End removes the creator's private identity namespace" (not (any ((== process) . witnessedProcessEpoch) (Application.applicationProcessWitnesses (startupApplicationState oldAfterEnd))))
  forM_ publishedReservations $ \reservation -> do
    let generated = Controlled.reservationWitnessGlobalUniqueId reservation
        object = globalObjectIdFromGlobalUniqueId generated
    assertEqual "End preserves the exact published reservation as history" (Just reservation) (Controlled.controlledReservationWitness generated (startupControlledState oldAfterEnd))
    assertEqual "End preserves the published controlled record" (Controlled.controlledLocalRecord object (startupControlledState oldReplayed)) (Controlled.controlledLocalRecord object (startupControlledState oldAfterEnd))
    assertBool "the live creator possessed its published object before End" (Controlled.controlledHasNormalPossession process object (startupControlledState oldReplayed))
    assertBool "the live creator has a raw direct-possession entry before End" (Controlled.controlledHasDirectPossession process object (startupControlledState oldReplayed))
    assertBool "End revokes the creator's normal possession of its published object" (not (Controlled.controlledHasNormalPossession process object (startupControlledState oldAfterEnd)))
    assertBool "End removes the raw direct-possession entry" (not (Controlled.controlledHasDirectPossession process object (startupControlledState oldAfterEnd)))
    assertEqual "End removes every raw Store-derived possession of the published object" [] (creatorStorePossessions process object oldAfterEnd)
  assertState "incumbent immediately after the later application End" oldAfterEnd
  assertState "new authority immediately after the later application End" newAfterEnd
  -- Oracle control has its own lane: the newcomer may observe the canonical
  -- End before it has established an ordinary peer binding or received any
  -- live projection from the incumbent. The first full snapshot must apply
  -- the same imported-plan loss as a withdrawal in a later snapshot.
  (oldLinked, newLinked, oldBinding, newBinding) <- case connectedBindings of
    Just (oldBinding, newBinding) -> pure (oldAfterEnd, newAfterEnd, oldBinding, newBinding)
    Nothing -> do
      assertEqual "canonical End arrived while live incumbent placement was still absent" Nothing (PlacementState.remotePlacementSequence oldOwner (startupPlacementState newAfterEnd))
      (oldConnected, newConnected, oldBinding, newBinding) <- connectOwners oldAfterEnd newAfterEnd
      oldPlaced <- receivePlacement newConnected oldBinding oldConnected
      newPlaced <- receivePlacement oldPlaced newBinding newConnected
      assertBool "the post-End first snapshot establishes a live incumbent projection" (isJust (PlacementState.remotePlacementSequence oldOwner (startupPlacementState newPlaced)))
      pure (oldPlaced, newPlaced, oldBinding, newBinding)
  (oldEnded, newEnded) <- exchangeStructuralControls oldBinding newBinding oldLinked newLinked
  assertState "incumbent after establishing the later End cut" oldEnded
  assertState "new authority after establishing the later End cut" newEnded
  assertEqual "both owners establish the actual post-join End cut" (Progress.structuralLastInstalledCutId (startupStructuralProgressState oldEnded)) (Progress.structuralLastInstalledCutId (startupStructuralProgressState newEnded))
  let endProgress = startupStructuralProgressState newEnded
      endCut = fromJust (Progress.lookupInstalledTopologyCut (Progress.structuralLastInstalledCutId endProgress) endProgress)
  assertEqual "the shared cut includes the actual later End control prefix" (Progress.structuralAppliedControlPrefix (startupStructuralProgressState newAfterEnd)) (topologyFrontierAppliedControlPrefix (topologyCutFrontier (Progress.installedTopologyCutCut endCut)))
  assertEqual "the new authority's real open cut completes through peer acknowledgements" Nothing (Progress.structuralOpenCut endProgress)
  oldEndPlaced <- receivePlacement newEnded oldBinding oldEnded
  newEndPlaced <- receivePlacement oldEndPlaced newBinding newEnded
  (oldEndReplayed, newEndReplayed) <- exchangeAlignmentControls oldBinding newBinding oldEndPlaced newEndPlaced
  let freshPlans =
        Map.filter
          ( (>= Progress.structuralAppliedControlPrefix (startupStructuralProgressState newAfterEnd))
              . topologyFrontierAppliedControlPrefix
              . topologyCutFrontier
              . Domain.alignmentCutTopologyCut
              . Generation.alignmentGenerationCut
          )
          (Map.withoutKeys (liveLatestPlans newEndReplayed) beforeLaterEnd)
  assertBool "the real post-join End requires new live alignment plans with the incumbent in their fixed placement set" (not (Map.null (Map.filter (fixedMember oldOwner) freshPlans)))
  assertPlansAccepted "the incumbent admits the new authority's real post-join End plans" oldOwner freshPlans newEndReplayed oldEndReplayed
  assertPlansAccepted "the new authority receives the incumbent's actual CutAccepted replies" oldOwner freshPlans newEndReplayed newEndReplayed
  assertPlansAccepted "the new authority retains its own exact CutAccepted" newcomer freshPlans newEndReplayed newEndReplayed
  assertPlansAccepted "the incumbent receives the new authority's actual CutAccepted" newcomer freshPlans newEndReplayed oldEndReplayed
  assertEqual "the incumbent validates after actual anchor admission" (Right ()) (validateHeraldState oldEndReplayed)
  assertEqual "the new authority validates after actual acceptance delivery" (Right ()) (validateHeraldState newEndReplayed)

singletonGenesis :: Genesis.CheckedHeraldGenesis
singletonGenesis = checked (checkHeraldGenesis deployment)
  where
    original = Fixtures.fixtureDeploymentManifest
    local = Fixtures.fixtureLocalMember
    deployment =
      original
        { deploymentActiveHeralds = [local],
          deploymentConfiguredProcesses = filter ((== heraldMemberEpoch local) . configuredProcessResidence) (deploymentConfiguredProcesses original),
          deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = [local]}
        }

settle :: HeraldState -> IO HeraldState
settle state = do
  structural <- checkedIO (Structural.advanceLocalStructuralWork state)
  aligned <- checkedIO (AlignmentTransfer.advanceAlignmentTransfers (fst structural))
  pure (fst aligned)

installRemotePlacement :: HeraldState -> HeraldState -> HeraldState
installRemotePlacement source target =
  replaceStartupPlacementState placement target
  where
    (_, retained) = PlacementState.commitLocalSnapshot (checked (PlacementState.prepareRetainedLocalSnapshot (startupPlacementState source)))
    (placement, _) = PlacementState.commitRemotePlacement (checked (PlacementState.prepareRemotePlacement (Placement.FullPlacementSnapshot (PlacementState.localSnapshotMessage retained)) (startupPlacementState target)))

connectOwners :: HeraldState -> HeraldState -> IO (HeraldState, HeraldState, Discovery.PeerBinding, Discovery.PeerBinding)
connectOwners old new = do
  (offered, offerEffects) <- stepNamed "newcomer opens reciprocal Hello candidate" (PeerInput (PeerCandidateOpened (Discovery.peerCandidateOpened (Discovery.connectionNonce 8104) Set.empty Nothing))) new
  (candidate, hello) <- sentHello offerEffects
  (oldAccepted, oldEffects) <- stepNamed "incumbent accepts newcomer's Hello" (PeerInput (Fixtures.currentPeerHelloReceived old candidate Set.empty hello)) old
  oldBinding <- acceptedBinding oldEffects
  (responseCandidate, responseHello) <- sentHello oldEffects
  assertEqual "reciprocal Hello preserves candidate ownership" candidate responseCandidate
  (newAccepted, newEffects) <- stepNamed "newcomer accepts reciprocal Hello" (PeerInput (Fixtures.currentPeerHelloReceived offered responseCandidate Set.empty responseHello)) offered
  newBinding <- acceptedBinding newEffects
  pure (oldAccepted, newAccepted, oldBinding, newBinding)
  where
    sentHello effects = case [(candidate, hello) | SendPeerCandidate candidate _ _ hello <- effectBatchMembers effects] of
      [value] -> pure value
      values -> assertFailure ("expected one actual peer Hello: " <> show values)
    acceptedBinding effects = case [binding | SetPeerCandidateDisposition _ (Discovery.PeerHelloAccepted _ binding) <- effectBatchMembers effects] of
      [value] -> pure value
      values -> assertFailure ("expected one admitted peer binding: " <> show values)

receivePlacement :: HeraldState -> Discovery.PeerBinding -> HeraldState -> IO HeraldState
receivePlacement source binding target =
  fst <$> stepNamed ("receive placement from " <> show (Genesis.checkedLocalHeraldEpoch (startupGenesis source))) (PeerInput (PeerControlReceived binding (PeerPlacementUpdate snapshot))) target
  where
    (_, retained) = PlacementState.commitLocalSnapshot (checked (PlacementState.prepareRetainedLocalSnapshot (startupPlacementState source)))
    snapshot = Placement.FullPlacementSnapshot (PlacementState.localSnapshotMessage retained)

exchangeAlignmentControls :: Discovery.PeerBinding -> Discovery.PeerBinding -> HeraldState -> HeraldState -> IO (HeraldState, HeraldState)
exchangeAlignmentControls = exchangePeerControls "alignment" retries isAlignment
  where
    retries binding source _ = [control | SendPeerControl _ control <- checked (PeerControl.reconnectRepairEffects binding source), isAlignment control]
    isAlignment (PeerAlignmentControl _) = True
    isAlignment PeerAlignmentEvidenceDelivered {} = True
    isAlignment PeerAlignmentDeliveryProgress = True
    isAlignment _ = False

-- Preserve the least owner's actual open proposal. Replaying finite reports
-- and draining the resulting announce/accept/establish/ack messages lets an
-- earlier open cut complete before it authors the End cut. Directly installing
-- a separately constructed certificate could strand that real open owner.
exchangeStructuralControls :: Discovery.PeerBinding -> Discovery.PeerBinding -> HeraldState -> HeraldState -> IO (HeraldState, HeraldState)
exchangeStructuralControls = exchangePeerControls "structural" retries isStructural
  where
    retries _ source target = PeerControl.structuralRetryControls (Genesis.checkedLocalHeraldEpoch (startupGenesis target)) (startupStructuralProgressState source)
    isStructural control = case control of
      PeerStructuralAppliedReported _ -> True
      PeerTopologyCutAnnounced _ -> True
      PeerTopologyCutAccepted _ -> True
      PeerTopologyCutEstablished _ -> True
      PeerTopologyCutEstablishedAcknowledged _ -> True
      _ -> False

exchangePeerControls :: String -> (Discovery.PeerBinding -> HeraldState -> HeraldState -> [PeerControl]) -> (PeerControl -> Bool) -> Discovery.PeerBinding -> Discovery.PeerBinding -> HeraldState -> HeraldState -> IO (HeraldState, HeraldState)
exchangePeerControls context retries include oldBinding newBinding old new = do
  (normalizedOld, oldEffects) <- normalize oldBinding old new
  (normalizedNew, newEffects) <- normalize newBinding new old
  drain
    (markSeeded oldBinding normalizedOld)
    (markSeeded newBinding normalizedNew)
    ( [(False, progress, control) | (progress, control) <- peerControlMessages oldEffects]
        <> [(True, progress, control) | (progress, control) <- peerControlMessages newEffects]
    )
  where
    markSeeded binding state
      | context == "alignment" = PeerDelivery.markSeeded binding state
      | otherwise = state
    normalize binding source target =
      either (assertFailure . show) pure (PeerDelivery.normalizeEffects source (orderedEffectBatch [SendPeerControl binding control | control <- retries binding source target]))
    drain currentOld currentNew [] = pure (currentOld, currentNew)
    drain currentOld currentNew ((toOld, progress, control) : remaining) = do
      let (binding, receiving) = if toOld then (oldBinding, currentOld) else (newBinding, currentNew)
      (successor, effects) <- stepNamed ("drain " <> context <> " control at " <> if toOld then "incumbent" else "new authority") (PeerInput (PeerControlReceivedWithProgress binding progress control)) receiving
      let replies = [(not toOld, replyProgress, reply) | (replyProgress, reply) <- peerControlMessages effects, include reply]
      if toOld
        then drain successor currentNew (remaining <> replies)
        else drain currentOld successor (remaining <> replies)

peerControlMessages :: EffectBatch -> [(ReceiptRetirement, PeerControl)]
peerControlMessages = concatMap message . effectBatchMembers
  where
    message (SendPeerControl _ control) = [(mempty, control)]
    message (SendPeerControlWithProgress _ progress control) = [(progress, control)]
    message _ = []

stepNamed :: String -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
stepNamed context body state =
  either (assertFailure . ((context <> ": ") <>) . show) pure (verifiedStepHerald (heraldInput (startupLastObservedTime state) body) state)

assertState :: String -> HeraldState -> Assertion
assertState context state = do
  assertEqual (context <> "; exact structural-progress owner validation") (Right ()) (Progress.validateStructuralProgressState (startupStructuralProgressState state))
  assertEqual (context <> "; retained reservations (process, phase, process ended, live namespace, record present, effective label, normal possession, terminal deletion): " <> reservationDiagnostics state) (Right ()) (validateHeraldState state)

creatorStorePossessions :: ProcessEpochId -> GlobalObjectId -> HeraldState -> [Controlled.ControlledStorePossessionWitness]
creatorStorePossessions process object state =
  [ witness
  | witness <- Controlled.controlledStorePossessionWitnesses (startupControlledState state),
    Controlled.controlledStorePossessionWitnessProcess witness == process,
    Controlled.controlledStorePossessionWitnessObject witness == object
  ]

reservationDiagnostics :: HeraldState -> String
reservationDiagnostics state =
  show
    [ ( process,
        Controlled.reservationWitnessPhase reservation,
        Controlled.controlledProcessEnded process controlled,
        any ((== process) . witnessedProcessEpoch) (Application.applicationProcessWitnesses (startupApplicationState state)),
        isJust (Controlled.controlledLocalRecord object controlled),
        Controlled.controlledEffectiveLabelState object controlled,
        Controlled.controlledHasNormalPossession process object controlled,
        isJust (Controlled.controlledTerminalDeletion object controlled)
      )
    | reservation <- Controlled.controlledReservationWitnesses controlled,
      let process = Controlled.reservationWitnessProcess reservation,
      let object = globalObjectIdFromGlobalUniqueId (Controlled.reservationWitnessGlobalUniqueId reservation)
    ]
  where
    controlled = startupControlledState state

assertPlansAccepted :: String -> HeraldEpoch -> Map.Map Domain.ContextClassGenerationId Generation.AlignmentGeneration -> HeraldState -> HeraldState -> Assertion
assertPlansAccepted context accepting plans source state =
  forM_ (Map.toAscList (Map.filter (fixedMember accepting) plans)) $ \(identifier, generation) -> do
    let diagnostic = alignmentAdmissionDiagnostic generation source state
    assertEqual (context <> ": exact generation " <> show identifier <> diagnostic) (Just generation) (Alignment.lookupAlignmentGeneration identifier alignment)
    assertBool (context <> ": missing CutAccepted for " <> show identifier <> diagnostic) (Alignment.lookupAlignmentCutAcceptance identifier accepting alignment /= Nothing)
  where
    alignment = startupAlignmentState state

-- Failure-only observations of the two real owners. Keep the coordinator's
-- admission law authoritative; the independent preparation below only exposes
-- whether exact topology, placement, or prior-certificate inputs are missing.
alignmentAdmissionDiagnostic :: Generation.AlignmentGeneration -> HeraldState -> HeraldState -> String
alignmentAdmissionDiagnostic generation source receiver =
  unlines
    ( ""
        : ("requested coordinate: " <> show (topology, vector, Domain.alignmentCutPredecessorGenerationIds cut))
        : ("source retry anchors (generation, topology, vector, predecessors): " <> show (map anchorSummary retryAnchors))
        : ownerDiagnostic "source" source
          <> ownerDiagnostic "receiver" receiver
    )
  where
    cut = Generation.alignmentGenerationCut generation
    occurrence = sortOccurrence (Domain.alignmentCutSortId cut) (Domain.alignmentCutSortDefinitionOccurrenceId cut)
    topology = Generation.alignmentGenerationTopologyCutId generation
    vector = Domain.alignmentCutPhysicalPlacementRevisionVector cut
    owner = Genesis.checkedLocalHeraldEpoch . startupGenesis
    sourceAlignment = startupAlignmentState source
    retryAnchors =
      [ announce
      | Protocol.AlignmentCutAnnounced announce <- Coordinator.alignmentReconnectRetryControls (owner source) (owner receiver) sourceAlignment,
        sameSort (Protocol.alignmentCutAnnounceCut announce)
      ]
    sameSort candidate = sortOccurrence (Domain.alignmentCutSortId candidate) (Domain.alignmentCutSortDefinitionOccurrenceId candidate) == occurrence
    anchorSummary announce =
      let candidate = Protocol.alignmentCutAnnounceCut announce
       in ( Protocol.alignmentCutAnnounceGeneration announce,
            deriveTopologyCutId (Domain.alignmentCutTopologyCut candidate),
            Domain.alignmentCutPhysicalPlacementRevisionVector candidate,
            Domain.alignmentCutPredecessorGenerationIds candidate
          )
    sourcePlan =
      [ candidate
      | (_, candidate) <- Alignment.alignmentGenerationEntries sourceAlignment,
        Generation.alignmentGenerationTopologyCutId candidate == topology,
        let candidateCut = Generation.alignmentGenerationCut candidate,
        sameSort candidateCut,
        Domain.alignmentCutPhysicalPlacementRevisionVector candidateCut == vector
      ]
    suppliedBases =
      Map.fromList
        [ (Domain.freshMemberBaseDelta evidence, Domain.freshMemberBaseRevision evidence)
        | candidate <- sourcePlan,
          evidence <- Domain.alignmentCutFreshMemberBaseEvidence (Generation.alignmentGenerationCut candidate)
        ]
    relevantCauses =
      Set.fromList
        ( [ Alignment.alignmentPromotionKeyCause key
          | (key, receipt) <- Alignment.alignmentPromotionEntries sourceAlignment,
            Generation.alignmentGenerationId generation `elem` Alignment.alignmentPromotionReceiptGenerationIds receipt
          ]
            <> [ Debt.structuralDebtKeyCause key
               | state <- [source, receiver],
                 debt <- Alignment.liveAlignmentStructuralDebtEntries (startupAlignmentState state),
                 let key = Debt.structuralConsequenceDebtKey debt,
                 Debt.structuralDebtKeySort key == occurrence
               ]
        )
    ownerDiagnostic label state =
      [ label <> " epoch/topology installed/exact: " <> show (owner state, Progress.structuralLastInstalledCutId progress, fmap ((== Domain.alignmentCutTopologyCut cut) . Progress.installedTopologyCutCut) (Progress.lookupInstalledTopologyCut topology progress)),
        label <> " exact placement availability (owner, route count): " <> show (fmap (map (\(herald, routes) -> (herald, length routes))) (PlacementState.placementRoutesAtVector vector (startupPlacementState state))),
        label <> " latest live (generation, topology, predecessors): " <> show [(Generation.alignmentGenerationId prior, Generation.alignmentGenerationTopologyCutId prior, Domain.alignmentCutPredecessorGenerationIds (Generation.alignmentGenerationCut prior)) | prior <- priors],
        label <> " pending anchors (remote, generation, topology, vector, predecessors): " <> show [(Alignment.pendingGenerationEvidenceRemoteHerald pending, anchorSummary announce) | pending <- Alignment.pendingGenerationEvidenceEntries alignment, Protocol.AlignmentCutAnnounced announce <- [Alignment.pendingGenerationEvidenceControl pending], sameSort (Protocol.alignmentCutAnnounceCut announce)],
        label <> " same-cause (cause, first covering cut/requested cut covers, retained/live debt counts, coverage coordinates, requested disposition): " <> show (map causeSummary (Set.toAscList relevantCauses)),
        label <> " exact preparation from latest: " <> preparationSummary
      ]
      where
        alignment = startupAlignmentState state
        progress = startupStructuralProgressState state
        priors = Coordinator.liveLatestAlignmentGenerationsForSort occurrence alignment
        priorIds = Set.fromList (map Generation.alignmentGenerationId priors)
        relations = [relation | relation <- Alignment.alignmentGenerationRelationEntries alignment, Generation.alignmentGenerationRelationSource relation `Set.member` priorIds, Generation.alignmentGenerationRelationDestination relation `Set.member` priorIds]
        certificates = Set.fromList [identifier | ((identifier, _), _) <- Alignment.alignmentHistoricalCertificateEntries alignment]
        preparationSummary = case Coordinator.activePlacementRoutesForSort state occurrence vector >>= \routes -> Coordinator.deriveAlignmentGenerationPreparationAt state occurrence topology vector routes suppliedBases priors relations certificates of
          Left problem -> show problem
          Right (Generation.AlignmentGenerationHeld missing) -> "Held " <> show missing
          Right (Generation.AlignmentGenerationReady plan) -> "Ready " <> show (map Generation.alignmentGenerationId (Generation.alignmentGenerationPlanGenerations plan))
        causeSummary cause =
          ( cause,
            (Progress.installedCutCoveringCause cause progress, Progress.installedCutCoversCause topology cause progress),
            (length (filter (matches cause) (Debt.structuralDebtSetEntries (Alignment.alignmentStructuralDebts alignment))), length (filter (matches cause) (Alignment.liveAlignmentStructuralDebtEntries alignment))),
            [ (Alignment.alignmentPromotionKeyTopologyCut key, Alignment.alignmentPromotionKeyPlacementVector key, Alignment.alignmentPlanIsInvalidated occurrence (Alignment.alignmentPromotionKeyTopologyCut key) (Alignment.alignmentPromotionKeyPlacementVector key) alignment)
            | key <- Alignment.alignmentCauseCoverageKeys alignment,
              Alignment.alignmentPromotionKeyCause key == cause,
              Alignment.alignmentPromotionKeySort key == occurrence
            ],
            Coordinator.alignmentCausePromotionDisposition cause occurrence topology vector alignment
          )
        matches cause debt =
          let key = Debt.structuralConsequenceDebtKey debt
           in Debt.structuralDebtKeyCause key == cause && Debt.structuralDebtKeySort key == occurrence

fixedMember :: HeraldEpoch -> Generation.AlignmentGeneration -> Bool
fixedMember owner = elem owner . map fst . NE.toList . Domain.physicalPlacementRevisionEntries . Domain.alignmentCutPhysicalPlacementRevisionVector . Generation.alignmentGenerationCut

liveLatestPlans :: HeraldState -> Map.Map Domain.ContextClassGenerationId Generation.AlignmentGeneration
liveLatestPlans state =
  Map.fromList
    [ (identifier, generation)
    | (identifier, generation) <- Alignment.latestAlignmentGenerationEntries alignment,
      let cut = Generation.alignmentGenerationCut generation,
      not
        ( Alignment.alignmentPlanIsInvalidated
            (sortOccurrence (Domain.alignmentCutSortId cut) (Domain.alignmentCutSortDefinitionOccurrenceId cut))
            (Generation.alignmentGenerationTopologyCutId generation)
            (Domain.alignmentCutPhysicalPlacementRevisionVector cut)
            alignment
        )
    ]
  where
    alignment = startupAlignmentState state

successorPlan :: SortOccurrence -> Plan.AlignmentPlanId -> TopologyCutId -> Domain.PhysicalPlacementRevisionVector -> HeraldState -> IO Plan.AlignmentPlan
successorPlan occurrence identifier topology placement state = do
  let alignment = startupAlignmentState state
      certificates = Set.fromList [generation | ((generation, _), _) <- Alignment.alignmentHistoricalCertificateEntries alignment]
  prior <- maybe (assertFailure "imported explicit predecessor plan missing") pure (Alignment.lookupAlignmentPlan identifier alignment)
  routes <- checkedIO (Coordinator.activePlacementRoutesForSort state occurrence placement)
  (cut, context, members) <- checkedIO (Coordinator.deriveAlignmentInputsAt state occurrence topology placement routes Map.empty)
  prepared <- checkedIO (Plan.prepareAlignmentPlan occurrence cut placement context members (Just (prior, Plan.AlignmentPredecessorUsable)) certificates)
  case prepared of
    Plan.AlignmentPlanReady plan -> pure plan
    Plan.AlignmentPlanHeld dependencies -> assertFailure ("real imported ancestry still holds successor planning: " <> show dependencies)

second :: (a, b, c) -> b
second (_, value, _) = value

checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure

-- A closed destination keeps immutable evidence and Store receipts. Report
-- only evidence which can no longer use the live lookup, so an invariant
-- failure distinguishes loss of history from consulting the wrong Store view.
inactiveDestinationReceipts :: HeraldState -> [(DeltaId, StoreIncarnationId, Bool)]
inactiveDestinationReceipts state =
  Set.toAscList
    ( Set.fromList
        [ (delta, incarnation, retainedMatches evidence retained)
        | evidence <- Transfer.appliedDestinationEvidenceEntries (Alignment.alignmentTransferState (startupAlignmentState state)),
          Transfer.appliedDestinationEvidenceDisposition evidence == Transfer.StoreAppliedOrAlreadyApplied,
          let destination = Transfer.appliedDestinationEvidenceDestination evidence,
          let delta = Protocol.destinationStoreDelta destination,
          let incarnation = Protocol.destinationStoreIncarnation destination,
          Store.lookupStoreSlot delta store == Nothing,
          let retained = Store.lookupRetainedStoreSlot incarnation store
        ]
    )
  where
    store = startupStoreState state
    retainedMatches _ Nothing = False
    retainedMatches evidence (Just slot) =
      let publication = Transfer.appliedDestinationEvidencePublication evidence
          destination = Transfer.appliedDestinationEvidenceDestination evidence
       in Store.storeSlotDelta slot == Protocol.destinationStoreDelta destination
            && Store.storeSlotIncarnation slot == Protocol.destinationStoreIncarnation destination
            && Store.storeSlotSortId slot == Protocol.retainedPublicationEvidenceSortId publication
            && Store.storeSlotOccurrenceId slot == Protocol.retainedPublicationEvidenceSortDefinitionOccurrenceId publication
            && maybe
              False
              (>= Transfer.appliedDestinationEvidenceStrength evidence)
              (lookup (Protocol.retainedPublicationEvidencePublicationId publication) (Store.storeSlotApplicationReceipts slot))
