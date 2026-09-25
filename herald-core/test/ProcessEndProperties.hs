{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessEndProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Result (RegularCallResult (NewIdCompleted))
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    StoreIncarnationId,
    controlIndex,
    firstStructuralSequence,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalObjectId,
    nablaSequence,
    publicationId,
    sortIdBytes,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label (releasedLabel)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ApplicationPermanentlyLost, ExplicitAdministrativeEnd),
  )
import Eclips.Domain.Publication (checkedPublicationCanonicalValue, mkCheckedPublication)
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (DeltaCarrier, NablaCarrier))
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRootRole (ReaderRoot, WriterRoot),
    appliedBootstrapManifestId,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.StructuralConsequence (processEndCause)
import Eclips.Domain.Value (LabelOwner (ProcessLabel, ZombieLabel))
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.Administration.State qualified as AdministrationState
import Eclips.Herald.Alignment.Protocol
  ( AlignmentCancelReason (AlignmentSourceIncarnationLost),
    AlignmentControl (..),
    AlignmentSourceProof (BootstrapProof),
    AlignmentSubscribe,
    alignmentCancelReason,
    alignmentCancelSubscriptionId,
    alignmentObligation,
    alignmentObligationId,
    alignmentSubscribe,
    alignmentSubscribeSubscriptionId,
    alignmentSubscriptionId,
    destinationStore,
    mkAlignmentObligationSequence,
    mkAlignmentSubscriptionSequence,
  )
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Application.Request
  ( ApplicationOperation (NewIdApplication, WaitApplication, WriteApplication),
    ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed),
  )
import Eclips.Herald.Application.Request.Internal (WaitId, requestId)
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.SortDefinition qualified as SortDefinition
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis (HeraldMember (..))
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedOracleControlIndex,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Initialization (HeraldState, initialHerald)
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (AdministrationInput, ApplicationRequestInput, OracleInput, PeerInput, RuntimeObserved),
    PeerControl (PeerAlignmentControl),
    RuntimeObservation (ApplicationBindingLost),
    candidateAdministrationLane,
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (..),
    OracleClientIngress (..),
    OracleRequestDispatch,
    OracleRetryPurpose (OracleSubmissionRetry),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
    oracleRequestDispatchEnvelope,
    oracleRequestDispatchRef,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerPublicationItem)
import Eclips.Herald.PeerPublication qualified as PeerPublication
import Eclips.Herald.PeerStream qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupWaitState,
    startupAdministrationState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupGraphState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerAttempt)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ControlBase qualified as ControlBase
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalOracleEnvelopeValue,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( OracleEnvelope,
    endProcessEpochCommand,
    oracleEnvelope,
    oracleEnvelopeCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Receipt qualified as Oracle
import Eclips.Oracle.State (OracleState)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedInitialBootstraps,
    fixtureCheckedOracleGenesis,
    fixtureGeneratorSeed,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
    fixtureStep14CheckedGenesis,
    initialEffectsAreOracleWatchAndGrace,
  )
import PrimordialTestAccess (conventionalStartupPairs)
import Step16RegularRetirementAcceptanceProperties (liveRegularAlignmentGateFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "live process End"
    [ testCase
        "an accepted local End retires Administration, Application, Wait, Controlled, and projection owners atomically"
        caseLocalEnd,
      testCase
        "ended writers keep valid outgoing history after retiring their live allocator"
        caseOutgoingHistoryAfterEnd,
      testCase
        "ended readers keep valid incoming receipts in their retained Store incarnation"
        caseIncomingHistoryAfterEnd,
      testCase
        "a remote End applies universal Controlled retirement without touching resident-only owners"
        caseRemoteEnd,
      testCase
        "one multi-root End emits only the exact final alignment-loss delta"
        caseProcessEndAlignmentDelta,
      QC.testProperty
        "retained Store snapshots and changes remain applicable after the original writer ends"
        (QC.withNumTests 20 (QC.conjoin [propRetainedAlignmentAfterWriterEnd False, propRetainedAlignmentAfterWriterEnd True])),
      testCase
        "an overlapping exact End entry is idempotent across every retired owner"
        caseExactOverlap,
      testCase
        "an explicit End committed first wins one application-loss End race"
        (caseApplicationLossEndOrdering ExplicitEndFirst),
      testCase
        "an automatic application-loss End committed first wins one explicit End race"
        (caseApplicationLossEndOrdering AutomaticEndFirst),
      testCase
        "a NotReady automatic End submission is retained and reoffered exactly"
        caseAutomaticEndSubmissionReoffer,
      testCase
        "canonical process retirement disposes surviving sessions after immediate sibling loss"
        caseRecoveryRetirementEffects,
      testCase
        "an accepted delta awaiting its carried sort settles after caller End and provider arrival"
        (caseUnstampedRootAfterCallerEnd False),
      testCase
        "an accepted nabla awaiting its carried sort settles after caller End and provider arrival"
        (caseUnstampedRootAfterCallerEnd True),
      testCase
        "a peer delta stamped before owner End is applied passively when it arrives after End"
        (caseLatePeerRootAfterOwnerEnd False),
      testCase
        "a peer nabla stamped before owner End is applied passively when it arrives after End"
        (caseLatePeerRootAfterOwnerEnd True)
    ]

caseLocalEnd :: Assertion
caseLocalEnd = localEndFixture >>= assertLocalEnd

-- Sort definitions use the ordinary outgoing record path. Structural carrier
-- tests retain a different owner product and do not exercise this lifetime.
caseOutgoingHistoryAfterEnd :: Assertion
caseOutgoingHistoryAfterEnd = do
  (bound, binding) <- boundInitialHerald
  opened <- checkedIO "open retained-publication writer" (Application.prepareApplicationSessionOpen fixtureLocalHerald (Session.applicationAttachmentForBootstrap (appliedBootstrapManifestId fixtureLocalBootstrap)) (Session.clientNonce 0x735) (startupApplicationState bound))
  let (application, acceptance) = Application.commitApplicationSessionAcceptance opened
      connected = replaceStartupApplicationState application bound
  (session, access) <- case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened identifier _ startup -> pure (identifier, startup)
    other -> assertFailure ("retained-publication writer did not open: " <> show other)
  writer <- maybe (assertFailure "missing sort-definition writer") (pure . Access.predefinedWriter) (find ((== Access.SortDefinitionRole) . Access.predefinedAccessRole) (conventionalStartupPairs access))
  (published, _) <- stepAt 2 (ApplicationRequestInput (CallApplicationRequest (Session.sessionAcceptanceBinding acceptance) session (requestId 1) (WriteApplication writer (PublishValue (ApplicationValue.SortDefinitionValue lateCarriedSortDefinition))))) connected
  let outgoing = Publication.outgoingPublicationEntries (startupPublicationState published)
      entry = committedCanonical fixtureOracleInitialState (endEnvelope fixtureLocalHerald 1 fixtureLocalProcess)
  assertBool "the live writer retains real ordinary outgoing publication evidence" (not (null outgoing))
  (ended, _) <- applyEntriesUnchecked 3 published binding (entry :| [])
  assertEqual "canonical End retires the live acceptance allocator" Nothing (lookup fixtureLocalProcess (Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness (startupApplicationState ended))))
  assertEqual "End preserves the exact historical outgoing records" outgoing (Publication.outgoingPublicationEntries (startupPublicationState ended))
  assertEqual "outgoing history remains valid without a live writer allocator" (Right ()) (validateHeraldState ended)
  case ControlBase.captureControlBase ended of
    Left problem -> assertFailure ("joining control-base capture after writer End: " <> show problem)
    Right _ -> pure ()
  (repeated, _) <- applyEntries 4 ended binding (entry :| [])
  assertEqual "repeated End preserves the same checked history" outgoing (Publication.outgoingPublicationEntries (startupPublicationState repeated))

-- The immutable peer outcome outlives the active reader Store that received it.
caseIncomingHistoryAfterEnd :: Assertion
caseIncomingHistoryAfterEnd = do
  (bound, oracleBinding) <- boundInitialHerald
  (connected, peerBinding) <- connectRemotePeer 2 bound
  definition <- checkedIO "admit incoming history definition" (SortDefinition.admitApplicationSortDefinition lateCarriedSortDefinition)
  (delta, incarnation) <- case [ (reader, store)
                               | root <- appliedProcessRoots fixtureLocalBootstrap,
                                 appliedRootCatalogueRole root == Profile.SortDefinitionRole,
                                 ReaderRoot reader <- [appliedRootRole root],
                                 Just (owner, store) <- [appliedRootPlacement root],
                                 owner == fixtureLocalHerald
                               ] of
    [target] -> pure target
    targets -> assertFailure ("expected one local sort-definition reader, got " <> show targets)
  item <- lateSortProviderItemFor (PeerPublication.publicationDestination delta incarnation Route.Normal) definition connected
  (received, _) <- stepAt 3 (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem item))) connected
  let identifier = PeerPublication.publicationBatchId (PeerPublication.peerPublicationBatch (PeerStream.sequencedItemPayload item))
      incoming state = Publication.lookupIncomingPublication identifier (startupPublicationState state)
      receipt state = lookup identifier . Store.storeSlotApplicationReceipts =<< Store.lookupRetainedStoreSlot incarnation (startupStoreState state)
      entry = committedCanonical fixtureOracleInitialState (endEnvelope fixtureLocalHerald 1 fixtureLocalProcess)
  record <- maybe (assertFailure "missing applied incoming publication") pure (incoming received)
  assertEqual "the live reader applied the real peer publication" Publication.Applied (Publication.incomingPublicationDisposition record)
  assertEqual "the checked Store retained the exact receipt" (Just Route.Normal) (receipt received)
  (ended, _) <- applyEntriesUnchecked 4 received oracleBinding (entry :| [])
  assertEqual "End removes the active reader Store" Nothing (Store.lookupStoreSlot delta (startupStoreState ended))
  assertEqual "the exact incarnation receipt survives End" (Just Route.Normal) (receipt ended)
  assertEqual "End does not rewrite the terminal peer outcome" (incoming received) (incoming ended)
  assertEqual "incoming history remains valid after reader End" (Right ()) (validateHeraldState ended)
  case ControlBase.captureControlBase ended of
    Left problem -> assertFailure ("joining control-base capture after reader End: " <> show problem)
    Right _ -> pure ()
  (repeated, _) <- applyEntries 5 ended oracleBinding (entry :| [])
  assertEqual "repeated End preserves incoming history" (incoming ended) (incoming repeated)

-- Unlike ready newenv endpoints, this source cannot be drained by the pre-End
-- pass: its carried sort has not arrived yet. The later provider comes from a
-- different, still-live process through the ordinary peer publication ingress.
caseUnstampedRootAfterCallerEnd :: Bool -> Assertion
caseUnstampedRootAfterCallerEnd isWriter = do
  (bound, oracleBinding) <- boundInitialHerald
  (connected, peerBinding) <- connectRemotePeer 2 bound
  preparedOpen <-
    checkedIO
      "open pending-root caller"
      ( Application.prepareApplicationSessionOpen
          fixtureLocalHerald
          (Session.applicationAttachmentForBootstrap (appliedBootstrapManifestId fixtureLocalBootstrap))
          (Session.clientNonce 0x731)
          (startupApplicationState connected)
      )
  let (application, acceptance) = Application.commitApplicationSessionAcceptance preparedOpen
      opened = replaceStartupApplicationState application connected
  (session, access) <- case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened session _ access -> pure (session, access)
    other -> assertFailure ("pending-root caller did not open: " <> show other)
  let role = if isWriter then Access.NablaRole else Access.DeltaRole
      call instant ordinal operation =
        stepAt
          instant
          ( ApplicationRequestInput
              ( CallApplicationRequest
                  (Session.sessionAcceptanceBinding acceptance)
                  session
                  (requestId ordinal)
                  operation
              )
          )
  writer <-
    Access.predefinedWriter
      <$> maybe
        (assertFailure "pending-root fixture has no carrier writer")
        pure
        (find ((== role) . Access.predefinedAccessRole) (conventionalStartupPairs access))
  (reserved, reserveEffects) <- call 3 1 (NewIdApplication (ControlledNewId writer)) opened
  object <- case [ value
                 | SendApplicationReply _ (RetainedRequestReply _ (Completed _ (NewIdCompleted value))) <- effectBatchMembers reserveEffects
                 ] of
    [value] -> pure value
    values -> assertFailure ("expected one reserved root object, got " <> show (length values))
  global <-
    checkedIO
      "resolve pending root object"
      (Application.resolveApplicationPrivateUniqueId fixtureLocalProcess object (startupApplicationState reserved))
  definition <-
    checkedIO
      "admit not-yet-published carried sort"
      (SortDefinition.admitApplicationSortDefinition lateCarriedSortDefinition)
  let carriedSort = SortDefinition.admittedApplicationSortId definition
      key = PublicationGroups.groupKey fixtureLocalProcess (globalObjectIdFromGlobalUniqueId global)
      value =
        ApplicationValue.RecordValue
          ( Map.fromList
              ( [ ("object_id", ApplicationValue.UniqueIdValue object),
                  ("label", ApplicationValue.LabelValue (ApplicationValue.ProcessLabel (Access.startupAccessProcess access), 0)),
                  ("sort_id", ApplicationValue.BytesValue (sortIdBytes carriedSort))
                ]
                  <> if isWriter then [("sequencing_object", ApplicationValue.OptionalUniqueIdValue Nothing)] else []
              )
          )
      groups = Publication.publicationGroups . startupPublicationState
  assertEqual
    "carried sort is genuinely unavailable at admission"
    Nothing
    (SortRegistry.lookupEffectiveSort carriedSort (startupSortRegistryState reserved))
  (accepted, _) <- call 4 2 (WriteApplication writer (PublishValue value)) reserved
  assertEqual
    "missing carried sort holds an actually accepted root"
    1
    (PublicationGroups.groupUnstampedCount key (groups accepted))
  assertEqual
    "accepted root has no structural stamp"
    []
    (Publication.stampedStructuralStageEntries (startupPublicationState accepted))
  let canonical = committedCanonical fixtureOracleInitialState (endEnvelope fixtureLocalHerald 1 fixtureLocalProcess)
  (ended, _) <- applyEntries 5 accepted oracleBinding (canonical :| [])
  assertBool
    "canonical End makes the caller permanently non-live"
    (Controlled.controlledProcessEnded fixtureLocalProcess (startupControlledState ended))
  assertEqual
    "pre-End drain cannot stamp a root whose carried sort is missing"
    1
    (PublicationGroups.groupUnstampedCount key (groups ended))
  provider <- lateSortProviderItem definition ended
  (provided, _) <-
    stepAt
      6
      (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem provider)))
      ended
  assertBool
    "the still-live remote provider installed the missing sort"
    (case SortRegistry.lookupEffectiveSort carriedSort (startupSortRegistryState provided) of Just _ -> True; Nothing -> False)
  (redriven, _) <-
    checkedIO
      "redrive accepted root after End and provider"
      (StructuralCoordinator.advanceLocalStructuralWork provided)
  assertEqual
    "the accepted root must leave the unstamped set after its last external prerequisite arrives"
    0
    (PublicationGroups.groupUnstampedCount key (groups redriven))
  assertEqual
    "the accepted root must release its local group token"
    0
    (PublicationGroups.groupLocalPendingCount key (groups redriven))
  assertEqual
    "the accepted root must receive one structural occurrence"
    1
    (length (Publication.stampedStructuralStageEntries (startupPublicationState redriven)))
  assertEndedRoot isWriter fixtureLocalProcess (globalObjectIdFromGlobalUniqueId global) redriven
  assertEqual
    "post-End source settlement remains a valid whole Herald"
    (Right ())
    (validateHeraldState redriven)

-- The publication and stamp are authentic pre-End facts, delayed in the live
-- Herald-to-Herald stream until the receiver has applied End for their issuer.
caseLatePeerRootAfterOwnerEnd :: Bool -> Assertion
caseLatePeerRootAfterOwnerEnd isWriter = do
  (bound, oracleBinding) <- boundInitialHerald
  (connected, peerBinding) <- connectRemotePeer 2 bound
  definition <- checkedIO "admit peer root carried sort" (SortDefinition.admitApplicationSortDefinition lateCarriedSortDefinition)
  provider <- lateSortProviderItem definition connected
  (provided, _) <- stepAt 3 (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem provider))) connected
  assertBool
    "peer root carried sort is known before End"
    (case SortRegistry.lookupEffectiveSort (SortDefinition.admittedApplicationSortId definition) (startupSortRegistryState provided) of Just _ -> True; Nothing -> False)
  object <- checkedIO "delayed peer root object" (mkGlobalObjectId (fixtureIdentifierBytes 0xee))
  delayed <- latePeerRootItem isWriter object definition provided
  let canonical = committedCanonical fixtureOracleInitialState (endEnvelope fixtureRemoteHerald 1 fixtureRemoteProcess)
  (ended, _) <- applyEntries 4 provided oracleBinding (canonical :| [])
  assertBool "remote owner has ended before peer delivery" (Controlled.controlledProcessEnded fixtureRemoteProcess (startupControlledState ended))
  (delivered, _) <- stepAt 5 (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem delayed))) ended
  assertEndedRoot isWriter fixtureRemoteProcess object delivered
  assertEqual "late peer root preserves whole-Herald invariants" (Right ()) (validateHeraldState delivered)

latePeerRootItem ::
  Bool ->
  GlobalObjectId ->
  SortDefinition.AdmittedApplicationSortDefinition ->
  HeraldState ->
  IO (PeerStream.SequencedItem PeerPublication.PeerPublication)
latePeerRootItem isWriter object definition state = do
  let role = if isWriter then Profile.NablaRole else Profile.DeltaRole
      carrier = if isWriter then NablaCarrier else DeltaCarrier
  root <-
    maybe
      (assertFailure "remote source has no root-description writer")
      pure
      (find (\candidate -> appliedRootCatalogueRole candidate == role && case appliedRootRole candidate of WriterRoot {} -> True; _ -> False) (appliedProcessRoots fixtureRemoteBootstrap))
  writer <- case appliedRootRole root of
    WriterRoot value _ -> pure value
    _ -> assertFailure "remote root source selected a reader"
  fields <-
    traverse
      (\(name, value) -> (,value) <$> checkedIO "root field name" (Value.mkFieldName name))
      ( [ ("object_id", Value.globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
          ("label", Value.labelValue (ProcessLabel fixtureRemoteProcess, 0)),
          ("sort_id", Value.bytesValue (sortIdBytes (SortDefinition.admittedApplicationSortId definition)))
        ]
          <> if isWriter then [("sequencing_object", Value.optionalGlobalUniqueIdValue Nothing)] else []
      )
  value <- checkedIO "remote root description" (Value.recordValue fields)
  let identifier = publicationId writer (appliedRootAuthority root) fixtureRemoteHerald (nablaSequence 0)
      descriptor = Profile.predefinedCatalogueDescriptor (Profile.profileEntryFor role)
      system = checkedSystemId fixtureStep14CheckedGenesis
      destination =
        PeerPublication.publicationDestination
          (deriveSystemViewDeltaId system fixtureLocalHerald role)
          (deriveSystemViewStoreIncarnationId system fixtureLocalHerald role)
          Route.Normal
  publication <- checkedIO "check delayed peer root" (mkCheckedPublication descriptor identifier value)
  batch <-
    checkedIO
      "delayed peer root batch"
      ( PeerPublication.mkPublicationBatch
          identifier
          fixtureRemoteProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Route.Normal
          (GraphProgress.structuralGenesisCutId (startupStructuralProgressState state))
          (controlIndex 0)
          (destination :| [])
      )
  digest <- checkedIO "delayed peer root structural digest" (PeerPublication.structuralPublicationDigestFor descriptor (appliedRootOccurrenceId root) publication batch)
  stamp <-
    checkedIO
      "pre-End peer root stamp"
      ( PeerPublication.mkStructuralOccurrenceStamp
          (structuralOccurrenceId fixtureRemoteHerald firstStructuralSequence)
          (GraphProgress.structuralAppliedVector (startupStructuralProgressState state))
          identifier
          digest
          carrier
      )
  peerPublication <-
    checkedIO
      "delayed structural peer root"
      (PeerPublication.mkStructuralPeerPublication descriptor (appliedRootOccurrenceId root) publication stamp batch)
  direction <- checkedIO "delayed peer root stream" (PeerStream.mkStreamDirection fixtureRemoteHerald fixtureLocalHerald)
  pure
    ( PeerStream.sequencedItem
        direction
        (PeerStream.nextStreamSequence PeerStream.firstStreamSequence)
        (PeerPublication.peerPublicationDigest peerPublication)
        peerPublication
    )

assertEndedRoot :: Bool -> ProcessEpochId -> GlobalObjectId -> HeraldState -> Assertion
assertEndedRoot isWriter owner object state = do
  let controlled = startupControlledState state
  record <- maybe (assertFailure "settled root has no Controlled record") pure (Controlled.controlledLocalRecord object controlled)
  assertEqual
    "immutable root observation retains the original process label and generation"
    (Just (ProcessLabel owner, 0))
    (Controlled.controlledRecordObservedLabel record)
  assertEqual
    "current effective label is zombie with the original generation"
    (Just (releasedLabel (ZombieLabel owner, 0)))
    (Controlled.controlledEffectiveLabelState object controlled)
  case Map.lookup object (Graph.graphStructuralVertexProjections (startupGraphState state)) of
    Just (Reconciliation.NablaVertexProjection _ _ _ controller activity) | isWriter -> assertController controller activity
    Just (Reconciliation.DeltaVertexProjection _ delta _ controller activity) | not isWriter -> do
      assertController controller activity
      assertBool
        "delayed zombie delta has no application Store"
        (case Store.lookupStoreSlot delta (startupStoreState state) of Nothing -> True; Just _ -> False)
      assertEqual
        "delayed zombie delta has no local placement"
        Nothing
        (Placement.lookupLocalPlacement delta (startupPlacementState state))
    projection -> assertFailure ("settled root has the wrong graph projection: " <> show projection)
  where
    assertController controller activity = do
      assertEqual "current graph controller reflects canonical End" (Reconciliation.ZombieProcessController owner) controller
      assertEqual "ended root is passive" Reconciliation.PassiveRoot activity

lateCarriedSortDefinition :: SortSyntax.ApplicationSortDefinition
lateCarriedSortDefinition =
  SortSyntax.DeclaredSortDefinition
    SortSyntax.ApplicationSortDescriptor
      { SortSyntax.sortKind = SortSyntax.RegularSort,
        SortSyntax.valueSchema = SortSyntax.RecordSchema (Map.singleton "late_key" SortSyntax.BoolSchema),
        SortSyntax.keyProjections = [SortSyntax.ApplicationProjection ("late_key" :| [])],
        SortSyntax.validityPredicate = SortSyntax.AlwaysPredicate,
        SortSyntax.obsolescencePredicate = SortSyntax.NeverPredicate,
        SortSyntax.rankTerms = SortSyntax.RankApplicationValue SortSyntax.Ascending :| [],
        SortSyntax.minimumRetentionMicros = 0,
        SortSyntax.isImmutable = False,
        SortSyntax.labelField = Nothing
      }
    Nothing

lateSortProviderItem ::
  SortDefinition.AdmittedApplicationSortDefinition ->
  HeraldState ->
  IO (PeerStream.SequencedItem PeerPublication.PeerPublication)
lateSortProviderItem definition state =
  lateSortProviderItemFor destination definition state
  where
    system = checkedSystemId fixtureStep14CheckedGenesis
    destination =
      PeerPublication.publicationDestination
        (deriveSystemViewDeltaId system fixtureLocalHerald Profile.SortDefinitionRole)
        (deriveSystemViewStoreIncarnationId system fixtureLocalHerald Profile.SortDefinitionRole)
        Route.Normal

lateSortProviderItemFor ::
  PeerPublication.PublicationDestination ->
  SortDefinition.AdmittedApplicationSortDefinition ->
  HeraldState ->
  IO (PeerStream.SequencedItem PeerPublication.PeerPublication)
lateSortProviderItemFor destination definition state = do
  root <-
    maybe
      (assertFailure "remote provider has no sort-definition writer")
      pure
      ( find
          ( \candidate ->
              appliedRootCatalogueRole candidate == Profile.SortDefinitionRole
                && case appliedRootRole candidate of WriterRoot {} -> True; _ -> False
          )
          (appliedProcessRoots fixtureRemoteBootstrap)
      )
  writer <- case appliedRootRole root of
    WriterRoot value _ -> pure value
    _ -> assertFailure "remote sort provider selected a reader"
  let identifier = publicationId writer (appliedRootAuthority root) fixtureRemoteHerald (nablaSequence 0)
      descriptor = Profile.predefinedCatalogueDescriptor (Profile.profileEntryFor Profile.SortDefinitionRole)
  publication <-
    checkedIO
      "check remote carried-sort publication"
      (mkCheckedPublication descriptor identifier (SortDefinition.admittedApplicationSortValue definition))
  batch <-
    checkedIO
      "batch remote carried-sort publication"
      ( PeerPublication.mkPublicationBatch
          identifier
          fixtureRemoteProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Route.Normal
          (GraphProgress.structuralGenesisCutId (startupStructuralProgressState state))
          (controlIndex 0)
          (destination :| [])
      )
  peerPublication <-
    checkedIO
      "encode ordinary carried-sort provider"
      (PeerPublication.mkOrdinaryPeerPublication descriptor (appliedRootOccurrenceId root) publication batch)
  direction <-
    checkedIO
      "carried-sort provider stream"
      (PeerStream.mkStreamDirection fixtureRemoteHerald fixtureLocalHerald)
  pure
    ( PeerStream.sequencedItem
        direction
        PeerStream.firstStreamSequence
        (PeerPublication.peerPublicationDigest peerPublication)
        peerPublication
    )

caseRemoteEnd :: Assertion
caseRemoteEnd = do
  (bound, binding) <- boundInitialHerald
  let applicationBefore = startupApplicationState bound
      waitsBefore = startupWaitState bound
      administrationBefore = startupAdministrationState bound
      envelope = endEnvelope fixtureRemoteHerald 1 fixtureRemoteProcess
      canonical = committedCanonical fixtureOracleInitialState envelope
  (ended, _) <- applyEntries 3 bound binding (canonical :| [])
  let projection = startupOracleProjectionState ended
      projected =
        OracleProjection.oracleViewEndedProcess
          fixtureRemoteProcess
          (OracleProjection.oracleView projection)
  assertBool
    "the remote epoch is monotonically retired from universal Controlled authority"
    (Controlled.controlledProcessEnded fixtureRemoteProcess (startupControlledState ended))
  assertBool "remote End retains resident Application" (applicationBefore == startupApplicationState ended)
  assertBool "remote End retains resident Waits" (waitsBefore == startupWaitState ended)
  assertBool "remote End retains resident Administration" (administrationBefore == startupAdministrationState ended)
  assertEqual
    "the global projection retains the exact remote End index"
    (Just (controlIndex 1))
    (OracleProjection.projectedEndedProcessControlIndex <$> projected)

-- The admitted transfer belongs to the surviving Herald system Store. Ending
-- the original application therefore retires its publishing right, not the
-- already admitted value being copied by that transfer. Exercise both a
-- captured snapshot and the same value in a continuing Store revision.
propRetainedAlignmentAfterWriterEnd :: Bool -> QC.Property
propRetainedAlignmentAfterWriterEnd snapshotMode =
  QC.forAll (QC.shuffle controls) $ \reordered -> QC.ioProperty $ do
    let evidence = case [transition | AlignmentChangeTransferred change <- controls, let transition = AlignmentProtocol.alignmentChangeRetainedTransition change]
          <> [AlignmentProtocol.retainedStateEvidenceRepresentative fact | AlignmentSnapshotChunkTransferred chunk <- controls, fact <- AlignmentProtocol.alignmentSnapshotChunkRetainedStates chunk] of
          retained : _ -> retained
          [] -> error "retained alignment End fixture needs one routed observation"
        identifier = AlignmentProtocol.retainedPublicationEvidencePublicationId evidence
        occurrence = AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
        sort = AlignmentProtocol.retainedPublicationEvidenceSortId evidence
        descriptor = SortRegistry.registryEntryDescriptor (required "retained alignment descriptor" (SortRegistry.lookupEffectiveSort sort (startupSortRegistryState seed)))
        value = checkedValue "retained alignment canonical value" (Value.decodeCanonicalValue (Value.canonicalValueByteString (AlignmentProtocol.retainedPublicationEvidenceCanonicalValue evidence)))
        publication = checkedValue "retained alignment publication" (mkCheckedPublication descriptor identifier value)
        (source, topology, prerequisite, stamp) = case AlignmentProtocol.retainedPublicationEvidenceOrigin evidence of
          AlignmentProtocol.RoutedRetainedObservation process cut control structural -> (process, cut, control, structural)
          _ -> error "retained alignment End fixture needs routed evidence"
        claim = PeerInput.routedPublicationAuthorityClaim descriptor occurrence publication source (AlignmentProtocol.retainedPublicationEvidenceSourceStrength evidence) topology prerequisite stamp
        destination state = required "retained alignment destination" (Transfer.lookupDestinationSubscription subscription (Alignment.alignmentTransferState (startupAlignmentState state)))
        target = case AlignmentProtocol.alignmentSubscribeDestinationStores (Transfer.destinationSubscriptionSubscribe (destination seed)) of
          one :| [] -> one
          _ -> error "retained alignment End fixture needs one destination"
        slot state = required "retained alignment system Store" (Store.lookupRetainedStoreSlot (AlignmentProtocol.destinationStoreIncarnation target) (startupStoreState state))
        receipt state = lookup identifier (Store.storeSlotApplicationReceipts (slot state))
        apply state control = fst <$> checkedIO "apply retained alignment after writer End" (AlignmentTransfer.applyAlignmentTransferControl binding control state)
        attempt = case [candidate | ConnectAndHelloOracle candidate _ <- OracleClient.oracleClientActions (startupOracleClientState seed)] of
          [one] -> one
          _ -> error "retained alignment End fixture needs one Oracle connection"
        node = oracleContactNode (oracleConnectAttemptContact attempt)
        acceptance = oracleHelloAcceptance node (oracleObservedTerm 1) (controlIndex 0) (Just node) True
    assertEqual "the transfer starts without the later publication receipt" Nothing (receipt seed)
    assertEqual "the original writer is the process being ended" fixtureLocalProcess source
    assertEqual "retained data validation needs only its immutable semantics" (Right ()) (PeerInput.validateRetainedStorePublication claim)
    (bound, effects) <- checkedIO "bind the retained alignment receiver to Oracle" (stepHerald (heraldInput (monotonicInstant 10_000) (OracleInput (OracleHelloReceived attempt acceptance))) seed)
    oracleBinding <- case [one | RunOracleClientAction (BindOracleConnection _ one) <- effectBatchMembers effects] of
      [one] -> pure one
      observed -> assertFailure ("expected one retained alignment Oracle binding, got " <> show observed)
    let canonical = committedCanonical fixtureOracleInitialState (endEnvelope fixtureLocalHerald 1 source)
    (ended, _) <- applyEntriesUnchecked 10_001 bound oracleBinding (canonical :| [])
    assertBool "the real Oracle End retired the original author" (Controlled.controlledProcessEnded source (startupControlledState ended))
    assertEqual "End leaves the surviving Store transfer active" Nothing (Transfer.destinationSubscriptionCancellation (destination ended))
    assertEqual "End does not prematurely apply the retained value" Nothing (receipt ended)
    received <- foldM apply ended (reordered <> controls)
    assertEqual "the late retained value is admitted at its original strength" (Just Route.Normal) (receipt received)
    assertBool "alignment cannot restore publishing authority" (Controlled.controlledProcessEnded source (startupControlledState received))
    replayed <- foldM apply received controls
    assertBool "duplicate alignment frames cannot reapply the Store value" (startupStoreState received == startupStoreState replayed)
    assertEqual "the resulting Store history remains valid" (Right ()) (Store.validateStoreHistoryState (startupStoreState replayed))
    assertEqual "the resulting transfer history remains valid" (Right ()) (Transfer.validateAlignmentTransferState (Alignment.alignmentTransferState (startupAlignmentState replayed)))
    pure True
  where
    (seed, binding, _, subscription, controls) = liveRegularAlignmentGateFixture snapshotMode
    required context = maybe (error context) id

caseProcessEndAlignmentDelta :: Assertion
caseProcessEndAlignmentDelta = do
  (bound, oracleBinding) <- boundInitialHerald
  (connected, peerBinding) <- connectRemotePeer 2 bound
  let placements = processOwnedPlacements fixtureLocalProcess connected
      subscribes = zipWith sourceSubscribe [1 ..] placements
      predecessorTransfer =
        foldl'
          retainSourceTranscript
          (Alignment.alignmentTransferState (startupAlignmentState connected))
          subscribes
      predecessorAlignment =
        Alignment.replaceAlignmentTransferState
          predecessorTransfer
          (startupAlignmentState connected)
      predecessor = replaceStartupAlignmentState predecessorAlignment connected
      envelope = endEnvelope fixtureLocalHerald 1 fixtureLocalProcess
      canonical = committedCanonical fixtureOracleInitialState envelope
  assertBool
    "the End fixture owns multiple independently refreshed reader Stores"
    (length placements >= 2)
  assertEqual
    "one source transcript is retained for every affected Store"
    (length placements)
    (length (Transfer.sourceSubscriptionEntries predecessorTransfer))
  assertEqual
    "the injected source-side transfer fixture is internally valid"
    (Right ())
    (Alignment.validateAlignmentState predecessorAlignment)
  (successor, effects) <- applyEntriesUnchecked 3 predecessor oracleBinding (canonical :| [])
  let successorTransfer =
        Alignment.alignmentTransferState (startupAlignmentState successor)
      expected =
        checkedValue
          "project the exact multi-root End alignment delta"
          ( AlignmentTransfer.alignmentLossTransferDeltaControls
              fixtureLocalHerald
              predecessorTransfer
              successorTransfer
          )
      observed =
        [ (fixtureRemoteHerald, control)
        | SendPeerControl observedBinding (PeerAlignmentControl control) <- effectBatchMembers effects,
          observedBinding == peerBinding
        ]
      otherAlignmentBindings =
        [ observedBinding
        | SendPeerControl observedBinding (PeerAlignmentControl _) <- effectBatchMembers effects,
          observedBinding /= peerBinding
        ]
      cancellations =
        [ cancellation
        | (_, AlignmentCancelled cancellation) <- observed
        ]
      expectedSubscriptions =
        Set.fromList (fmap alignmentSubscribeSubscriptionId subscribes)
      observedSubscriptions =
        Set.fromList (fmap alignmentCancelSubscriptionId cancellations)
  assertEqual
    "all End alignment traffic uses the exact current peer binding"
    []
    otherAlignmentBindings
  assertEqual
    "the Oracle End emits the exact predecessor-to-final-state alignment delta"
    expected
    observed
  assertEqual
    "every affected source has one final cancellation and no intermediate control"
    (length subscribes)
    (length cancellations)
  assertEqual
    "the final alignment delta contains cancellation controls only"
    (length cancellations)
    (length observed)
  assertBool
    "every final cancellation reports exact source-incarnation loss"
    (all ((== AlignmentSourceIncarnationLost) . alignmentCancelReason) cancellations)
  assertEqual
    "the final cancellations cover every and only retained source transcript"
    expectedSubscriptions
    observedSubscriptions
  assertEqual
    "no final cancellation is duplicated"
    (Set.size observedSubscriptions)
    (length cancellations)
  assertBool
    "no intermediate replacement Subscribe escapes the atomic End fold"
    ( all
        (\(_, control) -> case control of AlignmentSubscribeRequested _ -> False; _ -> True)
        observed
    )
  assertEqual
    "the final alignment owner remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState (startupAlignmentState successor))

caseExactOverlap :: Assertion
caseExactOverlap = do
  fixture <- localEndFixture
  let ended = fixtureEndedState fixture
      semanticBefore = endOwnerCut ended
  (overlapped, _) <-
    applyEntries
      6
      ended
      (fixtureOracleBinding fixture)
      (fixtureCanonicalEnd fixture :| [])
  assertBool
    "an exact overlapping entry changes no retired semantic owner"
    (semanticBefore == endOwnerCut overlapped)

data ApplicationEndOrder
  = ExplicitEndFirst
  | AutomaticEndFirst

data ApplicationEndRaceFixture = ApplicationEndRaceFixture
  { raceExpiredState :: HeraldState,
    raceOracleBinding :: OracleBinding,
    raceCorrelation :: Administration.AdminCorrelationId,
    raceSession :: Session.ApplicationSessionId,
    raceExplicitDispatch :: OracleRequestDispatch,
    raceAutomaticDispatch :: OracleRequestDispatch,
    raceExpiryEffects :: EffectBatch,
    raceDeadline :: Word64
  }

caseApplicationLossEndOrdering :: ApplicationEndOrder -> Assertion
caseApplicationLossEndOrdering order = do
  fixture <- applicationEndRaceFixture
  let (firstDispatch, secondDispatch, expectedReason) = case order of
        ExplicitEndFirst ->
          ( raceExplicitDispatch fixture,
            raceAutomaticDispatch fixture,
            ExplicitAdministrativeEnd
          )
        AutomaticEndFirst ->
          ( raceAutomaticDispatch fixture,
            raceExplicitDispatch fixture,
            ApplicationPermanentlyLost
          )
      (afterFirstOracle, firstEntry) =
        commitCanonical
          fixtureOracleInitialState
          (dispatchEnvelope firstDispatch)
      (_, secondEntry) =
        commitCanonical afterFirstOracle (dispatchEnvelope secondDispatch)
  (afterFirst, firstEffects) <-
    applyEntries
      (fromIntegral (raceDeadline fixture + 1))
      (raceExpiredState fixture)
      (raceOracleBinding fixture)
      (firstEntry :| [])
  (afterSecond, secondEffects) <-
    applyEntries
      (fromIntegral (raceDeadline fixture + 2))
      afterFirst
      (raceOracleBinding fixture)
      (secondEntry :| [])
  let projected =
        OracleProjection.oracleViewEndedProcess
          fixtureLocalProcess
          (OracleProjection.oracleView (startupOracleProjectionState afterSecond))
      disposalEffects =
        disposedSessionIds (raceExpiryEffects fixture)
          <> disposedSessionIds firstEffects
          <> disposedSessionIds secondEffects
  assertEqual
    "the first canonical End fixes the process reason"
    (Just expectedReason)
    (OracleProjection.projectedEndedProcessReason <$> projected)
  assertEqual
    "the competing End cannot dispose the same session twice"
    [raceSession fixture]
    disposalEffects
  assertEqual
    "the process remains retired after the losing canonical result"
    Nothing
    ( Application.applicationSessionProcess
        (raceSession fixture)
        (startupApplicationState afterSecond)
    )
  case order of
    ExplicitEndFirst ->
      assertEqual
        "the winning explicit request completes"
        (Just (Administration.AdminEndRequestCompleted fixtureLocalProcess))
        ( AdministrationState.lookupAdministrationResultStatus
            (raceCorrelation fixture)
            (startupAdministrationState afterSecond)
        )
    AutomaticEndFirst ->
      case AdministrationState.lookupAdministrationResultStatus
        (raceCorrelation fixture)
        (startupAdministrationState afterSecond) of
        Just
          ( Administration.AdminRequestRejected
              ( Administration.AdminEndProcessNotApplied
                  _
                  (Oracle.EndProcessAlreadyEnded process)
                )
            ) ->
            assertEqual "the losing explicit request names the retired process" fixtureLocalProcess process
        observed ->
          assertFailure
            ( "the losing explicit request did not retain AlreadyEnded: "
                <> show observed
            )

applicationEndRaceFixture :: IO ApplicationEndRaceFixture
applicationEndRaceFixture = do
  (bound, oracleBinding) <- boundInitialHerald
  (administrationOpened, administrationBinding) <- openAdministration 2 bound
  (populated, session, _, _) <- populateApplicationWait administrationOpened
  let correlation = Administration.adminCorrelationIdForBinding administrationBinding 76
      request =
        Administration.endProcessEpochRequest
          correlation
          fixtureLocalProcess
          ExplicitAdministrativeEnd
  (accepted, acceptedEffects) <-
    stepAt
      3
      (AdministrationInput (EndProcessEpoch administrationBinding request))
      populated
  explicitDispatch <-
    onlyEndDispatch
      "explicit application-loss race request"
      fixtureLocalProcess
      ExplicitAdministrativeEnd
      acceptedEffects
  (expired, expiryEffects) <- loseApplicationSession 4 session accepted
  let deadline = 4
  automaticDispatch <-
    onlyEndDispatch
      "automatic application-loss race request"
      fixtureLocalProcess
      ApplicationPermanentlyLost
      expiryEffects
  pure
    ApplicationEndRaceFixture
      { raceExpiredState = expired,
        raceOracleBinding = oracleBinding,
        raceCorrelation = correlation,
        raceSession = session,
        raceExplicitDispatch = explicitDispatch,
        raceAutomaticDispatch = automaticDispatch,
        raceExpiryEffects = expiryEffects,
        raceDeadline = deadline
      }

caseAutomaticEndSubmissionReoffer :: Assertion
caseAutomaticEndSubmissionReoffer = do
  (expired, binding, dispatch, deadline) <- automaticEndSubmissionFixture
  let request =
        OracleClient.oracleRequestRefRequestId
          (oracleRequestDispatchRef dispatch)
  (waiting, notReadyEffects) <-
    stepAt
      (fromIntegral (deadline + 1))
      ( OracleInput
          ( OracleSubmissionNotReadyReceived
              binding
              request
              (oracleObservedTerm 2)
          )
      )
      expired
  retry <- case effectBatchMembers notReadyEffects of
    [ RunOracleClientAction
        (AssociateOracleSubmissionRetry observedBinding observedRequest observedRetry),
      RunOracleClientAction (ScheduleOracleRetry OracleSubmissionRetry scheduledRetry)
      ]
        | observedBinding == binding,
          observedRequest == request,
          observedRetry == scheduledRetry ->
            pure observedRetry
    observed ->
      assertFailure
        ( "automatic End NotReady did not retain one paced reoffer: "
            <> show observed
        )
  (_, retryEffects) <-
    stepAt
      (fromIntegral (deadline + 2))
      (OracleInput (OracleRetryElapsed retry))
      waiting
  assertEqual
    "the retry cancels itself and reoffers the exact automatic End dispatch"
    [ RunOracleClientAction (CancelOracleRetry retry),
      RunOracleClientAction (SubmitOracleRequest binding dispatch)
    ]
    (effectBatchMembers retryEffects)

automaticEndSubmissionFixture :: IO (HeraldState, OracleBinding, OracleRequestDispatch, Word64)
automaticEndSubmissionFixture = do
  (bound, binding) <- boundInitialHerald
  (populated, session, _, _) <- populateApplicationWait bound
  (expired, expiryEffects) <- loseApplicationSession 2 session populated
  let deadline = 2
  dispatch <-
    onlyEndDispatch
      "automatic End submission"
      fixtureLocalProcess
      ApplicationPermanentlyLost
      expiryEffects
  pure (expired, binding, dispatch, deadline)

caseRecoveryRetirementEffects :: Assertion
caseRecoveryRetirementEffects = do
  (bound, oracleBinding) <- boundInitialHerald
  (withFirst, firstSession, attachment, _) <- populateApplicationWait bound
  (withSecond, secondSession) <- openAdditionalApplicationSession 2 attachment withFirst
  (recovering, lossEffects) <- loseApplicationSession 2 firstSession withSecond
  assertEqual "unexpected loss disposes its own session immediately" [firstSession] (disposedSessionIds lossEffects)
  let canonical =
        committedCanonical
          fixtureOracleInitialState
          (endEnvelope fixtureLocalHerald 1 fixtureLocalProcess)
  (retired, effects) <- applyEntries 4 recovering oracleBinding (canonical :| [])
  assertEqual
    "canonical retirement has no application recovery timers to cancel"
    []
    (cancelledTimerAttempts effects)
  assertEqual
    "canonical retirement disposes the surviving sibling"
    [secondSession]
    (disposedSessionIds effects)
  assertEqual
    "canonical retirement removes the resident application attachment"
    Nothing
    ( Application.applicationAttachmentProcess
        attachment
        (startupApplicationState retired)
    )

openAdditionalApplicationSession ::
  Word64 ->
  Session.ApplicationAttachment ->
  HeraldState ->
  IO (HeraldState, Session.ApplicationSessionId)
openAdditionalApplicationSession nonce attachment predecessor = do
  prepared <-
    checkedIO
      "open additional resident application session"
      ( Application.prepareApplicationSessionOpen
          fixtureLocalHerald
          attachment
          (Session.clientNonce nonce)
          (startupApplicationState predecessor)
      )
  let (application, acceptance) = Application.commitApplicationSessionAcceptance prepared
      successor = replaceStartupApplicationState application predecessor
  session <- case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened value _ _ -> pure value
    observed -> assertFailure ("expected an additional fresh session, got " <> show observed)
  checkedIO "validate additional resident application session" (validateHeraldState successor)
  pure (successor, session)

loseApplicationSession ::
  Integer ->
  Session.ApplicationSessionId ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
loseApplicationSession instant session predecessor = do
  binding <-
    maybe
      (assertFailure "application-loss fixture has no current session binding")
      pure
      ( Application.applicationSessionCurrentBinding
          session
          (startupApplicationState predecessor)
      )
  (lost, effects) <-
    stepAt
      instant
      (RuntimeObserved (ApplicationBindingLost binding))
      predecessor
  assertEqual "loss immediately disposes the session" [session] (disposedSessionIds effects)
  assertEqual "loss does not arm a recovery timer" [] [attempt | ArmTimer attempt _ <- effectBatchMembers effects]
  pure (lost, effects)

onlyEndDispatch ::
  String ->
  ProcessEpochId ->
  ProcessEndReason ->
  EffectBatch ->
  IO OracleRequestDispatch
onlyEndDispatch context process reason effects =
  case [ dispatch
       | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers effects,
         oracleEnvelopeCommand (dispatchEnvelope dispatch)
           == checkedValue
             "live End command"
             (endProcessEpochCommand process reason)
       ] of
    [dispatch] -> pure dispatch
    observed ->
      assertFailure
        ( context
            <> ": expected one matching End dispatch, got "
            <> show observed
        )

dispatchEnvelope :: OracleRequestDispatch -> OracleEnvelope
dispatchEnvelope =
  canonicalOracleEnvelopeValue . oracleRequestDispatchEnvelope

cancelledTimerAttempts :: EffectBatch -> [TimerAttempt]
cancelledTimerAttempts effects =
  [ attempt
  | CancelTimer attempt <- effectBatchMembers effects
  ]

disposedSessionIds :: EffectBatch -> [Session.ApplicationSessionId]
disposedSessionIds effects =
  [ session
  | DisposeApplicationSession session _ <- effectBatchMembers effects
  ]

data LocalEndFixture = LocalEndFixture
  { fixtureEndedState :: HeraldState,
    fixtureOracleBinding :: OracleBinding,
    fixtureCanonicalEnd :: CanonicalAppliedOracleEntry,
    fixtureCorrelation :: Administration.AdminCorrelationId,
    fixtureSession :: Session.ApplicationSessionId,
    fixtureAttachment :: Session.ApplicationAttachment,
    fixtureWait :: WaitId,
    fixtureEndIndex :: ControlIndex
  }

localEndFixture :: IO LocalEndFixture
localEndFixture = do
  (bound, oracleBinding) <- boundInitialHerald
  (opened, administrationBinding) <- openAdministration 2 bound
  (populated, session, attachment, wait) <- populateApplicationWait opened
  let correlation = Administration.adminCorrelationIdForBinding administrationBinding 73
      request =
        Administration.endProcessEpochRequest
          correlation
          fixtureLocalProcess
          ExplicitAdministrativeEnd
  (accepted, acceptedEffects) <-
    stepAt
      3
      (AdministrationInput (EndProcessEpoch administrationBinding request))
      populated
  dispatch <- case effectBatchMembers acceptedEffects of
    [ SendAdministrationReply
        observedBinding
        (Administration.RetainedAdminResult observedCorrelation Administration.AdminRequestAccepted),
      RunOracleClientAction (SubmitOracleRequest observedOracleBinding value)
      ] -> do
        assertEqual "accepted End reply binding" administrationBinding observedBinding
        assertEqual "accepted End correlation" correlation observedCorrelation
        assertEqual "End dispatch uses the active Oracle binding" oracleBinding observedOracleBinding
        pure value
    observed ->
      assertFailure ("expected accepted End plus one Oracle dispatch, got " <> show observed)
  let canonical =
        committedCanonical
          fixtureOracleInitialState
          (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch))
      endIndex = controlIndex 1
  (ended, _) <- applyEntries 4 accepted oracleBinding (canonical :| [])
  pure
    LocalEndFixture
      { fixtureEndedState = ended,
        fixtureOracleBinding = oracleBinding,
        fixtureCanonicalEnd = canonical,
        fixtureCorrelation = correlation,
        fixtureSession = session,
        fixtureAttachment = attachment,
        fixtureWait = wait,
        fixtureEndIndex = endIndex
      }

assertLocalEnd :: LocalEndFixture -> Assertion
assertLocalEnd fixture = do
  let ended = fixtureEndedState fixture
      application = startupApplicationState ended
      placement = startupPlacementState ended
      retiredReaderDeltas =
        [ delta
        | root <- appliedProcessRoots fixtureLocalBootstrap,
          ReaderRoot delta <- [appliedRootRole root]
        ]
      projection = startupOracleProjectionState ended
      projected =
        OracleProjection.oracleViewEndedProcess
          fixtureLocalProcess
          (OracleProjection.oracleView projection)
  assertEqual
    "the retained Administration result reaches terminal completion"
    (Just (Administration.AdminEndRequestCompleted fixtureLocalProcess))
    ( AdministrationState.lookupAdministrationResultStatus
        (fixtureCorrelation fixture)
        (startupAdministrationState ended)
    )
  assertEqual
    "the exact application session is retired"
    Nothing
    (Application.applicationSessionProcess (fixtureSession fixture) application)
  assertEqual
    "the process attachment is retired"
    Nothing
    (Application.applicationAttachmentProcess (fixtureAttachment fixture) application)
  assertEqual
    "the exact process-owned Wait registration is removed"
    []
    (Wait.waitRegistrations (startupWaitState ended))
  assertBool
    "the exact retired Wait ID is absent"
    ( all
        ((/= fixtureWait fixture) . Wait.waitRegistrationId)
        (Wait.waitRegistrations (startupWaitState ended))
    )
  assertBool
    "the local epoch is monotonically retired from Controlled authority"
    (Controlled.controlledProcessEnded fixtureLocalProcess (startupControlledState ended))
  assertBool
    "the retired Controlled process fact remains as immutable history"
    ( case Controlled.controlledProcessFact fixtureLocalProcess (startupControlledState ended) of
        Just _ -> True
        Nothing -> False
    )
  assertBool
    "the retired Controlled root facts remain as immutable history"
    ( any
        ((== fixtureLocalProcess) . Controlled.rootFactProcessEpoch)
        (Controlled.controlledRootFacts (startupControlledState ended))
    )
  assertBool
    "the resident End fixture owns at least one reader route"
    (not (null retiredReaderDeltas))
  assertBool
    "retired reader routes leave the live placement projection"
    ( all
        (\delta -> Placement.lookupLocalPlacement delta placement == Nothing)
        retiredReaderDeltas
    )
  assertBool
    "retired reader routes remain in the immutable bootstrap placement basis"
    ( all
        ( \delta ->
            any
              ((== delta) . Placement.localPlacementDelta)
              (Placement.bootstrapLocalPlacements placement)
        )
        retiredReaderDeltas
    )
  assertEqual
    "the live projection retains the End index"
    (Just (fixtureEndIndex fixture))
    (OracleProjection.projectedEndedProcessControlIndex <$> projected)
  assertEqual
    "the live projection retains the End reason"
    (Just ExplicitAdministrativeEnd)
    (OracleProjection.projectedEndedProcessReason <$> projected)
  checkedIO "validate live state after local End" (validateHeraldState ended)

populateApplicationWait ::
  HeraldState ->
  IO
    ( HeraldState,
      Session.ApplicationSessionId,
      Session.ApplicationAttachment,
      WaitId
    )
populateApplicationWait predecessor = do
  let attachment =
        Session.applicationAttachmentForBootstrap
          (appliedBootstrapManifestId fixtureLocalBootstrap)
  opened <-
    checkedIO
      "open resident application session"
      ( Application.prepareApplicationSessionOpen
          fixtureLocalHerald
          attachment
          (Session.clientNonce 1)
          (startupApplicationState predecessor)
      )
  let (afterOpen, acceptance) = Application.commitApplicationSessionAcceptance opened
  (session, binding) <- case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened value _ _ ->
      pure (value, Session.sessionAcceptanceBinding acceptance)
    other -> assertFailure ("expected a fresh resident session, got " <> show other)
  candidate <-
    case Application.classifyApplicationRequest
      session
      binding
      (requestId 1)
      (WaitApplication [])
      afterOpen of
      Right (Application.FirstApplicationRequest value) -> pure value
      other -> assertFailure ("expected one fresh Wait candidate, got " <> resultShape other)
  let wait = Application.applicationRequestCandidateWaitId candidate
      position = Application.applicationRequestCandidatePosition candidate
      (application, _) =
        Application.commitApplicationRequest
          (Application.prepareApplicationWaitAcceptance candidate)
      registration =
        Wait.waitRegistration
          wait
          session
          (requestId 1)
          fixtureLocalProcess
          position
          []
  preparedWait <-
    checkedIO
      "retain resident Wait registration"
      (Wait.prepareWaitRegistration registration (startupWaitState predecessor))
  let waits = Wait.commitWaitRegistration preparedWait
      populated =
        replaceStartupWaitState waits
          . replaceStartupApplicationState application
          $ predecessor
  checkedIO "validate populated resident owners" (validateHeraldState populated)
  pure (populated, session, attachment, wait)

boundInitialHerald :: IO (HeraldState, OracleBinding)
boundInitialHerald = do
  (initial, effects) <-
    checkedIO
      "initialize live End Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureStep14CheckedGenesis
          fixtureCheckedInitialBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  assertBool "initial Oracle watch and isolation grace" (initialEffectsAreOracleWatchAndGrace effects)
  attempt <- case [candidate | RunOracleClientAction (ConnectAndHelloOracle candidate _) <- effectBatchMembers effects] of
    [value] -> pure value
    observed -> assertFailure ("expected one initial Oracle connection, got " <> show observed)
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (checkedOracleControlIndex fixtureStep14CheckedGenesis)
          (Just node)
          True
  (bound, bindEffects) <-
    stepAt
      1
      (OracleInput (OracleHelloReceived attempt acceptance))
      initial
  binding <- case effectBatchMembers bindEffects of
    [ RunOracleClientAction (BindOracleConnection observedAttempt value),
      RunOracleClientAction (WatchOracle observedBinding cursor)
      ]
        | observedAttempt == attempt,
          observedBinding == value,
          cursor == controlIndex 0 ->
            pure value
    observed -> assertFailure ("expected Oracle binding and watch, got " <> show observed)
  pure (bound, binding)

connectRemotePeer ::
  Integer ->
  HeraldState ->
  IO (HeraldState, PeerBinding)
connectRemotePeer instant predecessor = do
  let HeraldMember remoteId remoteEpoch = fixtureRemoteMember
      nonce = connectionNonce 1_401
      candidate = peerCandidate remoteId remoteEpoch nonce
      hello =
        peerHello
          (checkedSystemId fixtureStep14CheckedGenesis)
          remoteId
          remoteEpoch
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureStep14CheckedGenesis)
          (checkedInitialProjectionDigest fixtureCheckedInitialBootstraps)
          Nothing
  (connected, effects) <-
    stepAt
      instant
      (PeerInput (currentPeerHelloReceived predecessor candidate Set.empty hello))
      predecessor
  binding <- case [ value
                  | SetPeerCandidateDisposition
                      observedCandidate
                      (PeerHelloAccepted _ value) <-
                      effectBatchMembers effects,
                    observedCandidate == candidate
                  ] of
    [value] -> pure value
    observed ->
      assertFailure
        ("expected one accepted remote peer binding, got " <> show observed)
  pure (connected, binding)

processOwnedPlacements :: ProcessEpochId -> HeraldState -> [Placement.LocalPlacement]
processOwnedPlacements process state =
  filter
    ((== process) . Placement.localPlacementProcessEpoch)
    (Placement.localPlacements (startupPlacementState state))

sourceSubscribe :: Word64 -> Placement.LocalPlacement -> AlignmentSubscribe
sourceSubscribe ordinal placement =
  checkedValue
    "construct process-End source Subscribe"
    ( alignmentSubscribe
        obligation
        (alignmentSubscriptionId fixtureRemoteHerald subscriptionSequence)
        (Placement.localPlacementStoreIncarnation placement)
        ( BootstrapProof
            generation
            (DomainAlignment.deriveBootstrapEvidenceDigest "process-End source transcript")
        )
    )
  where
    generation =
      checkedValue
        "process-End source generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0x76))
    obligationSequence =
      checkedValue
        "process-End source obligation sequence"
        (mkAlignmentObligationSequence ordinal)
    subscriptionSequence =
      checkedValue
        "process-End source subscription sequence"
        (mkAlignmentSubscriptionSequence ordinal)
    obligation =
      checkedValue
        "construct process-End source obligation"
        ( alignmentObligation
            (alignmentObligationId fixtureRemoteHerald obligationSequence)
            ( checkedValue
                "process-End source cause"
                (processEndCause fixtureLocalProcess (controlIndex 1))
            )
            (Placement.localPlacementSortId placement)
            (Placement.localPlacementOccurrenceId placement)
            generation
            (uncurry destinationStore fixtureRemoteDestination :| [])
            generation
            Route.Normal
        )

retainSourceTranscript :: Transfer.State -> AlignmentSubscribe -> Transfer.State
retainSourceTranscript predecessor subscribe =
  fst
    ( Transfer.commitSourceSubscription
        ( checkedValue
            "retain process-End source transcript"
            ( Transfer.prepareSourceSubscription
                subscribe
                DomainAlignment.initialStoreRevision
                []
                Transfer.SourceLiveImmediate
                predecessor
            )
        )
    )

fixtureRemoteDestination :: (DeltaId, StoreIncarnationId)
fixtureRemoteDestination =
  case [ (delta, store)
       | root <- appliedProcessRoots fixtureRemoteBootstrap,
         ReaderRoot delta <- [appliedRootRole root],
         Just (owner, store) <- [appliedRootPlacement root],
         owner == fixtureRemoteHerald
       ] of
    destination : _ -> destination
    [] -> error "process-End source fixture needs one remote reader destination"

openAdministration ::
  Integer ->
  HeraldState ->
  IO (HeraldState, Administration.AdministrationBinding)
openAdministration instant predecessor = do
  let lane = candidateAdministrationLane 9
  (opened, effects) <-
    stepAt
      instant
      (AdministrationInput (OpenAdministrationConnection lane (checkedSystemId fixtureStep14CheckedGenesis)))
      predecessor
  case effectBatchMembers effects of
    [SetAdministrationConnectionDisposition observedLane binding]
      | observedLane == lane -> pure (opened, binding)
    observed -> assertFailure ("expected one Administration binding, got " <> show observed)

applyEntries ::
  Integer ->
  HeraldState ->
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  IO (HeraldState, EffectBatch)
applyEntries instant state binding entries =
  stepAt
    instant
    (OracleInput (OracleEntriesReceived binding entries))
    state

applyEntriesUnchecked ::
  Integer ->
  HeraldState ->
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  IO (HeraldState, EffectBatch)
applyEntriesUnchecked instant state binding entries =
  checkedIO
    "apply process-End entries with a source-side alignment fixture"
    ( stepHerald
        ( heraldInput
            (monotonicInstant (fromIntegral instant))
            (OracleInput (OracleEntriesReceived binding entries))
        )
        state
    )

stepAt ::
  Integer ->
  HeraldInputBody ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
stepAt instant body =
  checkedIO
    ("step live End at " <> show instant)
    . verifiedStepHerald (heraldInput (monotonicInstant (fromIntegral instant)) body)

committedCanonical :: OracleState -> OracleEnvelope -> CanonicalAppliedOracleEntry
committedCanonical predecessor envelope = snd (commitCanonical predecessor envelope)

commitCanonical ::
  OracleState ->
  OracleEnvelope ->
  (OracleState, CanonicalAppliedOracleEntry)
commitCanonical predecessor envelope =
  case checkedValue "commit live End in Oracle" (stepOracle envelope predecessor) of
    (successor, OracleCommitted _, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] ->
        (successor, canonicalizeAppliedOracleEntry entry)
      observed -> error ("expected one applied End entry, got " <> show observed)
    (_, outcome, _) -> error ("expected committed End, got " <> show outcome)

endEnvelope :: HeraldEpoch -> Word64 -> ProcessEpochId -> OracleEnvelope
endEnvelope home sequenceNumber process =
  oracleEnvelope
    (oracleClientRequestId home sequenceNumber)
    Nothing
    home
    ( checkedValue
        "live End command"
        (endProcessEpochCommand process ExplicitAdministrativeEnd)
    )

data EndOwnerCut
  = EndOwnerCut
      AdministrationState.State
      Application.State
      Wait.State
      Controlled.State
      OracleProjection.State
  deriving stock (Eq)

endOwnerCut :: HeraldState -> EndOwnerCut
endOwnerCut state =
  EndOwnerCut
    (startupAdministrationState state)
    (startupApplicationState state)
    (startupWaitState state)
    (startupControlledState state)
    (startupOracleProjectionState state)

fixtureOracleInitialState :: OracleState
fixtureOracleInitialState =
  checkedValue "initialize live End Oracle" (initialOracle fixtureCheckedOracleGenesis)

fixtureLocalHerald, fixtureRemoteHerald :: HeraldEpoch
fixtureLocalHerald = checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
fixtureRemoteHerald = appliedProcessResidence fixtureRemoteBootstrap

fixtureLocalProcess, fixtureRemoteProcess :: ProcessEpochId
fixtureLocalProcess = appliedProcessEpochId fixtureLocalBootstrap
fixtureRemoteProcess = appliedProcessEpochId fixtureRemoteBootstrap

fixtureLocalBootstrap, fixtureRemoteBootstrap :: AppliedProcessBootstrap
fixtureLocalBootstrap =
  requiredBootstrap "local" ((== fixtureLocalHerald) . appliedProcessResidence)
fixtureRemoteBootstrap =
  requiredBootstrap "remote" ((/= fixtureLocalHerald) . appliedProcessResidence)

requiredBootstrap ::
  String ->
  (AppliedProcessBootstrap -> Bool) ->
  AppliedProcessBootstrap
requiredBootstrap context predicate =
  case filter predicate (checkedInitialBootstraps fixtureCheckedInitialBootstraps) of
    [bootstrap] -> bootstrap
    observed ->
      error
        ( context
            <> " live End fixture expected one bootstrap, got "
            <> show (length observed)
        )

resultShape :: Either problem value -> String
resultShape = either (const "Left") (const "Right")

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure
