{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DisappearanceGateProperties (tests) where

import ApplicationLabelProperties (locallyTakenNeutralDisappearanceFixtureWithRequests)
import Control.Monad (foldM)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DisappearanceLiveProperties (prepareLiveOpen, prepareLiveResolve, step, submitEnvelope)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (privateObjectUniqueId)
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToVoid))
import Eclips.Application.Types.Operation (ApplicationOperation (LabelApplication))
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Domain.Alignment qualified as AlignmentDomain
import Eclips.Domain.Disappearance qualified as Domain
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication (checkedPublicationId)
import Eclips.Herald.Alignment.Disappearance qualified as AlignmentEvidence
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Application.Request (requestId)
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch (HeraldEffect (RejectPeerConnection, RunOracleClientAction), PeerProtocolDisposition (ClosePeerControlProtocol), effectBatchMembers)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Input (ApplicationRequestIngress (CallApplicationRequest), HeraldInputBody (ApplicationRequestInput, OracleInput))
import Eclips.Herald.OracleClient qualified as ClientActions
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.PeerStream qualified as PeerStream
import Eclips.Herald.PeerStream.State qualified as PeerStreamState
import Eclips.Herald.Publication.Groups qualified as Groups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentCoordinator
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command (OracleEnvelope)
import Eclips.Oracle.Identity (oracleClientRequestId, oracleClientRequestSequence)
import Eclips.Oracle.Label qualified as Oracle
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import GenesisFixtures qualified as Fixtures
import Step16RegularRetirementAcceptanceProperties (liveRegularAlignmentGateFixture, liveRegularDisappearanceFixture, liveRegularDisappearancePeerRewrite, liveRegularDisappearanceRewriteInput, liveRegularMalformedAlignmentGateFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "live disappearance publication gates"
    [ QC.testProperty "regular rewrite waits for either serialized terminal winner and uses its floor" (QC.conjoin [propLocalRegularGate False, propLocalRegularGate True]),
      QC.testProperty "early alignment marker and post-cut snapshot or change retain exact unadmitted work" (QC.conjoin [propAlignmentGate False, propAlignmentGate True]),
      QC.testProperty "ordinary gated ingress rechecks the collecting disappearance probe when reopened" propOrdinaryGateReplay,
      QC.testProperty "a later label waits only for matching older positioned work and lets unrelated held work resume" (QC.once (QC.conjoin [propPositionedHeldLabelScope True, propPositionedHeldLabelScope False])),
      QC.testProperty "malformed deferred alignment input remains a scoped peer violation" propMalformedAlignmentReplay,
      QC.testProperty "a new cut counts matching work held by an earlier terminal probe" propEarlierHeldWork,
      QC.testProperty "post-marker peer definition waits for either terminal winner before semantic release" (QC.conjoin [propPeerGate False, propPeerGate True]),
      QC.testProperty "admission preparation preserves local candidates and rearms exactly once" propAdmissionCandidate,
      QC.testProperty "canonical Begin and Cancel reopen a local candidate with a fresh Oracle request" propAdmissionCandidateCancellation
    ]

propAdmissionCandidate :: QC.Property
propAdmissionCandidate = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
  (_, opened, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState opened) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected live probe for admission candidate"
  let leaf = startupDisappearanceState opened
      probe = Protocol.projectedProbeId projected
      after index = Identity.controlIndex (Identity.controlIndexWord64 index + 1)
      at = after (Domain.disappearanceProbeOpenControlIndex probe)
  admission <- checkedIO (Membership.deriveHeraldAdmissionId at)
  let terminal = Protocol.ProjectedDisappearanceAborted probe (Protocol.ProjectedAdmissionPreparing admission) at
      ledgerBefore = Disappearance.disappearanceWorkLedger leaf
  assertBool
    "admission cause cannot claim a different terminal coordinate"
    (isLeft (Disappearance.prepareProjectedTerminal (Protocol.ProjectedDisappearanceAborted probe (Protocol.ProjectedAdmissionPreparing admission) (after at)) leaf))
  prepared <- checkedIO (Disappearance.prepareProjectedTerminal terminal leaf)
  let (retained, disposition) = Disappearance.commitDisappearanceTransition prepared
      ledgerAfter = Disappearance.disappearanceWorkLedger retained
  assertEqual "first terminal is installed" Disappearance.TerminalInstalled disposition
  assertEqual "preparation clears old Open intentions" [] (Disappearance.openIntentions retained)
  assertEqual "preparation clears old report intentions" [] (Disappearance.reportIntentions retained)
  assertEqual "preparation clears old invalidations" [] (Disappearance.invalidationIntentions retained)
  assertEqual "preparation clears old Resolve intentions" [] (Disappearance.resolveIntentions retained)
  assertEqual "exactly one local candidate is rearmed" (ledgerBefore.candidateCreations + 1) ledgerAfter.candidateCreations
  case Disappearance.candidateWitnesses retained of
    [candidate] -> do
      assertEqual "candidate keeps its subject" subject candidate.candidateSubject
      assertBool "candidate remains eligible for cancellation or activation" candidate.candidateEligible
      assertEqual "old probe is detached" Nothing candidate.candidateProjectedProbe
    _ -> assertFailure "preparation lost the local candidate"
  repeated <- checkedIO (Disappearance.prepareProjectedTerminal terminal retained)
  assertEqual "equal terminal is inert" (retained, Disappearance.TerminalDuplicate) (Disappearance.commitDisappearanceTransition repeated)
  resumed <- checkedIO (Disappearance.prepareCandidateReevaluation (Protocol.disappearanceOpenContext (Protocol.projectedProbeMembership projected)) subject retained)
  let (reopened, _) = Disappearance.commitDisappearanceTransition resumed
  assertEqual "unchanged membership permits a fresh Open after cancellation" 1 (length (Disappearance.openIntentions reopened))
  assertEqual "retained local leaf validates" (Right ()) (Disappearance.validateState reopened)
  snapshot <- checkedIO (Evidence.disappearanceEvidenceSnapshotForHerald subject opened)
  participantOpen <- checkedIO (Disappearance.prepareProjectedOpen projected snapshot (Disappearance.initialState (Disappearance.disappearanceLocalHerald leaf)))
  let (participant, _) = Disappearance.commitDisappearanceTransition participantOpen
  participantTerminal <- checkedIO (Disappearance.prepareProjectedTerminal terminal participant)
  let (participantDone, _) = Disappearance.commitDisappearanceTransition participantTerminal
  assertEqual "participation alone does not invent a local candidate" [] (Disappearance.candidateWitnesses participantDone)
  assertEqual "participation alone records no candidate creation" 0 (Disappearance.disappearanceWorkLedger participantDone).candidateCreations
  assertEqual "participant leaf validates" (Right ()) (Disappearance.validateState participantDone)
  pure True

-- Keep the canonical watch, terminal application, client semantic key and
-- scheduler in the same serialized owner. A terminal leaf alone cannot prove
-- that the cancelled admission releases a reusable Open request identity.
propAdmissionCandidateCancellation :: QC.Property
propAdmissionCandidateCancellation = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
  (oracle, opened, binding, openEntry) <- prepareLiveOpen seed
  oldRequest <- case Oracle.appliedEntryCommand openEntry of
    Just command -> pure (Oracle.appliedEntryRequestId command)
    Nothing -> assertFailure "disappearance Open has no request identity"
  oldProbe <- case Disappearance.probeWitnesses (startupDisappearanceState opened) of
    [witness] -> pure (Protocol.projectedProbeId witness.witnessProjectedProbe)
    _ -> assertFailure "expected one canonical collecting probe"
  let genesis = startupGenesis opened
      local = Genesis.checkedLocalHeraldEpoch genesis
      home = case filter (/= local) (Genesis.checkedActiveHeraldEpochs genesis) of
        remote : _ -> remote
        [] -> local
      envelope ordinal command = Oracle.oracleEnvelope (oracleClientRequestId home ordinal) Nothing home command
  newcomer <- checkedIO (Identity.mkHeraldEpoch (Fixtures.fixtureIdentifierBytes 244))
  identifier <- checkedIO (Identity.mkHeraldId (Fixtures.fixtureIdentifierBytes 245))
  anchor <- checkedIO (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews opened) (startupStructuralProgressState opened))
  let manifest = Admission.heraldAdmissionManifest (Genesis.checkedSystemId genesis) identifier newcomer
  (preparingOracle, preparing, beginEntry) <- submitEnvelope "Begin aborts the live local candidate" binding oracle opened (envelope 900001 (Oracle.beginHeraldAdmissionCommand manifest anchor))
  admission <- case Oracle.oraclePendingHeraldAdmission preparingOracle of
    Just record -> pure (Admission.admissionRecordId record)
    Nothing -> assertFailure "Begin omitted its pending admission"
  let beginning = appliedEntryControlIndex beginEntry
      expectedTerminal = Disappearance.ProbeAbortedView (Protocol.ProjectedAdmissionPreparing admission) beginning
      currentMembership state = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView (startupOracleProjectionState state))
      pendingOpenRequests state =
        [ Client.oracleRequestDispatchRef dispatch
        | ClientActions.SubmitOracleRequest _ dispatch <- Client.oracleClientDisappearanceRequestActions (startupOracleClientState state),
          Just witness <- [Client.lookupOracleRequest (Client.oracleRequestDispatchRef dispatch) (startupOracleClientState state)],
          Client.OpenDisappearanceProbeIntent {} <- [Client.oracleRequestWitnessIntent witness]
        ]
  assertEqual "Begin installs the exact new terminal cause" (Just expectedTerminal) ((.witnessPhase) <$> Disappearance.probeWitness oldProbe (startupDisappearanceState preparing))
  assertEqual "Begin leaves semantic membership unchanged" (currentMembership opened) (currentMembership preparing)
  assertEqual "pending admission issues no fresh Open" [] (pendingOpenRequests preparing)
  assertEqual "pending admission keeps no unsent Open intention" [] (Disappearance.openIntentions (startupDisappearanceState preparing))
  assertBool "membership gate is closed while preparing" (Application.applicationMembershipGateClosed (startupApplicationState preparing))
  (replayed, _) <- step "replay Begin without creating another Open" (OracleInput (ClientActions.OracleEntriesReceived binding (canonicalizeAppliedOracleEntry beginEntry :| []))) preparing
  assertEqual "equal watch replay does not allocate another request" (Client.oracleClientNextRequestSequence (startupOracleClientState preparing)) (Client.oracleClientNextRequestSequence (startupOracleClientState replayed))
  assertEqual "equal watch replay remains gated" [] (pendingOpenRequests replayed)
  (cancelledOracle, cancelled, _) <- submitEnvelope "Cancel permits the same candidate to reopen" binding preparingOracle replayed (envelope 900002 (Oracle.cancelHeraldAdmissionCommand admission))
  assertEqual "cancellation leaves semantic membership unchanged" (currentMembership opened) (currentMembership cancelled)
  assertBool "cancellation opens the application gate" (not (Application.applicationMembershipGateClosed (startupApplicationState cancelled)))
  freshEnvelope <- envelopeFor isOpen cancelled
  let freshRequest = Oracle.oracleEnvelopeRequestId freshEnvelope
  assertBool "the same subject receives a fresh request identity" (freshRequest /= oldRequest && oracleClientRequestSequence freshRequest > oracleClientRequestSequence oldRequest)
  (_, reopened, _) <- submitEnvelope "commit fresh Open after cancellation" binding cancelledOracle cancelled freshEnvelope
  case Disappearance.candidateWitnesses (startupDisappearanceState reopened) of
    [candidate] -> do
      assertEqual "reopened candidate retains its original subject" subject candidate.candidateSubject
      assertBool "reopened candidate binds a new probe" (case candidate.candidateProjectedProbe of Just fresh -> fresh /= oldProbe; Nothing -> False)
    _ -> assertFailure "fresh Open lost the unique local candidate"
  assertEqual "old terminal remains immutable" (Just expectedTerminal) ((.witnessPhase) <$> Disappearance.probeWitness oldProbe (startupDisappearanceState reopened))
  assertEqual "canonical cancellation/reopen preserves whole-owner invariants" (Right ()) (validateHeraldState reopened)
  pure True
  where
    isOpen Client.OpenDisappearanceProbeIntent {} = True
    isOpen _ = False

propLocalRegularGate :: Bool -> QC.Property
propLocalRegularGate resolveWins = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
  (oracle, ready, binding) <- prepareLiveResolve seed subject
  original <- case subject of
    _
      | Domain.RegularSortDefinitionSubjectView sortId _ _ <- Domain.disappearanceSubjectView subject ->
          maybe (assertFailure "missing original regular registry entry") pure (Registry.lookupEffectiveSort sortId (startupSortRegistryState ready))
    _ -> assertFailure "expected regular fixture"
  let sortId = Registry.registryEntrySortId original
  resolveEnvelope <- envelopeFor isResolve ready
  (held, effects) <- step "hold a post-cut matching regular definition rewrite" liveRegularDisappearanceRewriteInput ready
  assertBool "a held application call emits only its retained Oracle intention" (all onlyOracle (effectBatchMembers effects))
  assertBool "held work preserves Store state" (startupStoreState ready == startupStoreState held)
  assertBool "held work preserves Publication state" (startupPublicationState ready == startupPublicationState held)
  assertBool "held work preserves the registry" (startupSortRegistryState ready == startupSortRegistryState held)
  assertBool
    "accepted work retains one exact disappearance gate"
    ( case Application.applicationFenceHeldEntries (startupApplicationState held) of
        [record] -> not (Set.null (Application.applicationFenceHeldDisappearanceProbes record))
        _ -> False
    )
  assertEqual "held owner validates" (Right ()) (validateHeraldState held)
  invalidateEnvelope <- envelopeFor isInvalidate held
  let winner = if resolveWins then resolveEnvelope else invalidateEnvelope
  (_, released, terminal) <- submitEnvelope "serialize the selected terminal winner" binding oracle held winner
  assertEqual "terminal redrives the accepted write" [] (Application.applicationFenceHeldEntries (startupApplicationState released))
  current <- maybe (assertFailure "held definition did not reappear after terminal") pure (Registry.lookupEffectiveSort sortId (startupSortRegistryState released))
  if resolveWins
    then do
      retirement <- maybe (assertFailure "Resolve winner did not retain retirement") pure (Registry.latestRegularSortRetirement sortId (startupSortRegistryState released))
      assertEqual
        "reintroduced definition derives the terminal successor occurrence"
        (Registry.regularSortRetirementSuccessorOccurrenceId retirement)
        (Registry.registryEntryOccurrenceId current)
      assertBool
        "successor occurrence differs from retired occurrence"
        (Registry.registryEntryOccurrenceId current /= Registry.registryEntryOccurrenceId original)
      assertBool
        "reintroduced publication carries the Resolve floor"
        ( any
            ((>= appliedEntryControlIndex terminal) . Publication.applicationPublicationControlPrerequisite . snd)
            (Publication.applicationPublicationEntries (startupPublicationState released))
        )
    else do
      assertEqual
        "Invalidate preserves the current regular occurrence"
        (Registry.registryEntryOccurrenceId original)
        (Registry.registryEntryOccurrenceId current)
      assertEqual
        "Invalidate creates no retirement history"
        Nothing
        (Registry.latestRegularSortRetirement sortId (startupSortRegistryState released))
  assertEqual "released owner validates" (Right ()) (validateHeraldState released)
  pure True
  where
    onlyOracle RunOracleClientAction {} = True
    onlyOracle _ = False
    isResolve Client.ResolveDisappearanceProbeIntent {} = True
    isResolve _ = False
    isInvalidate Client.InvalidateDisappearanceProbeIntent {} = True
    isInvalidate _ = False

propAlignmentGate :: Bool -> QC.Property
propAlignmentGate snapshotMode = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
      (destinationSeed, binding, source, subscription, controls) = liveRegularAlignmentGateFixture snapshotMode
  (_, canonicalOpened, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState canonicalOpened) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected one actual live Oracle Open"
  let probe = Protocol.projectedProbeId projected
      marker = Protocol.disappearanceAlignmentMarker probe (Protocol.outgoingAlignmentCut subscription AlignmentDomain.initialStoreRevision)
  early <- checkedIO (Live.receiveDisappearanceAlignmentMarker source marker destinationSeed)
  assertEqual
    "pre-Open marker retains its exact source and subscription"
    [(source, marker)]
    (Disappearance.pendingAlignmentMarkers (startupDisappearanceState early))
  (opened, _) <- checkedIO (Live.applyDisappearanceOpen projected early)
  assertEqual
    "Open redrives and clears the pending marker"
    []
    (Disappearance.pendingAlignmentMarkers (startupDisappearanceState opened))
  witness <- maybe (assertFailure "missing projected alignment witness") pure (Disappearance.probeWitness probe (startupDisappearanceState opened))
  assertEqual
    "Open binds the early marker to its exact captured subscription"
    (Just (Just marker))
    (fmap (.alignmentCellMarker) (lookup subscription witness.witnessIncomingAlignmentCuts))
  let transfer state = Alignment.alignmentTransferState (startupAlignmentState state)
      apply current control = fst <$> checkedIO (AlignmentCoordinator.applyAlignmentTransferControl binding control current)
  held <- foldM apply opened controls
  assertBool "held alignment evidence has no Store effect" (startupStoreState held == startupStoreState opened)
  assertEqual
    "Start and the entire post-cut range leave the admitted destination transcript untouched"
    (Transfer.destinationSubscriptionEntries (transfer opened))
    (Transfer.destinationSubscriptionEntries (transfer held))
  assertEqual
    "all raw controls retain their exact source and order"
    (fmap (source,) controls)
    (Transfer.deferredDestinationControls subscription (transfer held))
  assertEqual
    "post-cut raw frames do not add pre-cut negative-evidence blockers"
    (AlignmentEvidence.alignmentDisappearanceBlockers (AlignmentEvidence.alignmentDisappearanceView subject (startupAlignmentState opened)))
    (AlignmentEvidence.alignmentDisappearanceBlockers (AlignmentEvidence.alignmentDisappearanceView subject (startupAlignmentState held)))
  case [chunk | AlignmentProtocol.AlignmentSnapshotChunkTransferred chunk <- controls] of
    chunk : _ -> do
      let malformed =
            AlignmentProtocol.AlignmentSnapshotChunkTransferred
              (AlignmentProtocol.alignmentSnapshotChunk subscription 999 (AlignmentProtocol.alignmentSnapshotChunkRetainedStates chunk))
      assertBool
        "malformed queued chunk is rejected at remote admission before retaining"
        ( case AlignmentCoordinator.applyAlignmentTransferControl binding malformed held of
            Left AlignmentCoordinator.AlignmentTransferRemoteProtocolViolation -> True
            _ -> False
        )
    [] -> pure ()
  repeated <- foldM apply held controls
  assertBool "equal raw frame retries preserve exact owner state" (repeated == held)
  invalidation <-
    maybe
      (assertFailure "matching aligned publication did not retain Invalidate")
      pure
      (Disappearance.probeWitness probe (startupDisappearanceState held) >>= (.witnessInvalidationIntention))
  let terminal =
        Protocol.ProjectedDisappearanceInvalidated
          probe
          ( Protocol.ProjectedCommandInvalidation
              (Protocol.disappearanceInvalidationReporter invalidation)
              (Protocol.disappearanceInvalidationReason invalidation)
              (Protocol.disappearanceInvalidationWitness invalidation)
          )
          (Identity.controlIndex 2)
  prepared <- checkedIO (Disappearance.prepareProjectedTerminal terminal (startupDisappearanceState held))
  let terminalState = replaceStartupDisappearanceState (fst (Disappearance.commitDisappearanceTransition prepared)) held
  released <-
    foldM
      ( \current _ -> do
          (next, _, _) <- checkedIO (AlignmentCoordinator.redriveDeferredDestinationControls current)
          pure next
      )
      terminalState
      controls
  assertEqual
    "Invalidate releases every retained raw frame in source order"
    []
    (Transfer.deferredDestinationControls subscription (transfer released))
  assertBool "released matching alignment work applies to Store" (startupStoreState released /= startupStoreState held)
  assertEqual
    "retained queue and admitted transcript satisfy the transfer invariant"
    (Right ())
    (Transfer.validateAlignmentTransferState (transfer released))
  pure True

propOrdinaryGateReplay :: QC.Property
propOrdinaryGateReplay = QC.once . QC.ioProperty $ do
  let (seed, _) = liveRegularDisappearanceFixture
  (_, actualOpened, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState actualOpened) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected live Open witness"
  let gatedSeed = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState seed)) seed
  ingress <- case liveRegularDisappearanceRewriteInput of
    ApplicationRequestInput request -> pure request
    _ -> assertFailure "expected application fixture ingress"
  (queued, _) <- checkedIO (ApplicationCall.applyApplicationRequest ingress gatedSeed)
  assertEqual
    "ordinary gate retains the incoming operation"
    1
    (length (Application.applicationGatedIngressEntries (startupApplicationState queued)))
  (opened, _) <- checkedIO (Live.applyDisappearanceOpen projected queued)
  let beforeReplay = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState opened)) opened
  (held, _, _) <- checkedIO (ApplicationCall.advanceApplicationLabelWork beforeReplay)
  assertEqual
    "ordinary ingress replay consumes its old gate record"
    []
    (Application.applicationGatedIngressEntries (startupApplicationState held))
  assertEqual
    "replayed work now retains the collecting disappearance gate"
    [Set.singleton (Protocol.projectedProbeId projected)]
    (fmap Application.applicationFenceHeldDisappearanceProbes (Application.applicationFenceHeldEntries (startupApplicationState held)))
  assertBool
    "replayed matching work cannot publish through the newly opened probe"
    (startupStoreState beforeReplay == startupStoreState held && startupPublicationState beforeReplay == startupPublicationState held)
  assertBool
    "replayed matching work retains Invalidate"
    ( case Disappearance.probeWitness (Protocol.projectedProbeId projected) (startupDisappearanceState held) of
        Just witness -> witness.witnessInvalidationIntention /= Nothing
        Nothing -> False
    )
  pure True

-- The disappearance gate keeps an accepted application position outside the
-- Publication owner. A later label must not mistake that matching work for an
-- empty group, or stop an unrelated held provider when its probe terminates.
propPositionedHeldLabelScope :: Bool -> QC.Property
propPositionedHeldLabelScope matching = QC.once . QC.ioProperty $ do
  (seed, object, caller, matchingLabel, publication) <- locallyTakenNeutralDisappearanceFixtureWithRequests
  subject <- case [ candidate.candidateSubject
                  | candidate <- Disappearance.candidateWitnesses (startupDisappearanceState seed),
                    Domain.ControlledPredefinedSubjectView target _ _ <- [Domain.disappearanceSubjectView candidate.candidateSubject],
                    target == object
                  ] of
    [candidate] -> pure candidate
    _ -> assertFailure "expected the controlled object's actual LocalTake candidate"
  (oracle, ready, binding) <- prepareLiveResolve seed subject
  (held, _) <- step "hold the older controlled forward" (ApplicationRequestInput publication) ready
  heldEntry <- case Application.applicationFenceHeldEntries (startupApplicationState held) of
    [entry] -> pure entry
    _ -> assertFailure "expected one earlier positioned disappearance hold"
  (applicationBinding, session) <- case publication of
    CallApplicationRequest currentBinding currentSession _ _ -> pure (currentBinding, currentSession)
    _ -> assertFailure "expected application forward fixture"
  let application = startupApplicationState held
  access <- maybe (assertFailure "missing forward caller bootstrap access") pure (Application.applicationBootstrapAccess caller application)
  unrelatedTarget <- case Map.lookup Access.environmentHubKey (Access.accessEntries (Application.bootstrapAccessPrimordial access)) of
    Just (Access.Object bootstrapObject) -> pure bootstrapObject
    _ -> assertFailure "missing bootstrap label target"
  (target, labelCall) <- case matchingLabel of
    CallApplicationRequest _ _ _ operation@(LabelApplication matchingTarget _ _)
      | matching -> pure (matchingTarget, operation)
      | otherwise -> pure (unrelatedTarget, LabelApplication unrelatedTarget (ApplicationValue.ProcessLabel (Application.bootstrapAccessProcess access), 0) LabelToVoid)
    _ -> assertFailure "expected matching private label fixture"
  globalTarget <- Identity.globalObjectIdFromGlobalUniqueId <$> checkedIO (Application.resolveApplicationPrivateUniqueId caller (privateObjectUniqueId target) application)
  let heldMembership = Application.applicationFenceHeldWorkGroupMembership (Application.applicationFenceHeldSemanticWork heldEntry)
  assertEqual
    "the fixture distinguishes selected and unrelated positioned work"
    matching
    (Set.member (Groups.groupKey caller globalTarget) (Groups.membershipKeys heldMembership))
  (labelPending, _) <-
    step
      "accept or retain a label behind older positioned work"
      (ApplicationRequestInput (CallApplicationRequest applicationBinding session (requestId 5) labelCall))
      held
  let activeBefore = Application.applicationActiveLabel (startupApplicationState labelPending)
      candidatesBefore = Application.applicationLabelCandidateEntries (startupApplicationState labelPending)
  if matching
    then do
      assertEqual "matching earlier work keeps the label before acceptance" Nothing activeBefore
      assertEqual "the exact label remains a candidate" [labelCall] (fmap (Application.labelApplicationOperation . Application.applicationLabelCandidateCall) candidatesBefore)
      assertEqual "no Oracle label request can overtake the positioned work" [] (Client.oracleClientLabelRequestActions (startupOracleClientState labelPending))
    else do
      assertBool "unrelated held work permits label acceptance" (activeBefore /= Nothing)
      assertEqual "the unrelated label is not retained as a candidate" [] candidatesBefore
  invalidate <- envelopeFor isInvalidate labelPending
  (_, released, _) <- submitEnvelope "resolve the older hold by its real Invalidate" binding oracle labelPending invalidate
  assertEqual "terminal disappearance releases the older positioned work" [] (Application.applicationFenceHeldEntries (startupApplicationState released))
  assertBool
    "the original held position now owns its semantic publication"
    ( Publication.lookupApplicationPublication
        (Application.applicationFenceHeldPosition heldEntry)
        (startupPublicationState released)
        /= Nothing
    )
  if matching
    then do
      assertEqual "the provider release consumes the waiting label candidate" [] (Application.applicationLabelCandidateEntries (startupApplicationState released))
      assertBool "the label is accepted only after the earlier work enters publication accounting" (Application.applicationActiveLabel (startupApplicationState released) /= Nothing)
    else
      assertEqual
        "provider progress preserves the unrelated active invocation"
        (Application.activeLabelApplicationDecision <$> activeBefore)
        (Application.activeLabelApplicationDecision <$> Application.applicationActiveLabel (startupApplicationState released))
  assertEqual "the interleaving preserves the whole Herald invariant" (Right ()) (validateHeraldState released)
  pure True
  where
    isInvalidate Client.InvalidateDisappearanceProbeIntent {} = True
    isInvalidate _ = False

propMalformedAlignmentReplay :: QC.Property
propMalformedAlignmentReplay = QC.once . QC.ioProperty $ do
  let (seed, _) = liveRegularDisappearanceFixture
      (destinationSeed, _, source, subscription, controls) = liveRegularMalformedAlignmentGateFixture
  (_, actualOpened, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState actualOpened) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected live Open witness"
  binding <-
    maybe
      (assertFailure "missing actual remote binding")
      pure
      (Discovery.currentPeerBinding source (startupDiscoveryState actualOpened))
  let predecessor = replaceStartupDiscoveryState (startupDiscoveryState actualOpened) destinationSeed
      probe = Protocol.projectedProbeId projected
      marker = Protocol.disappearanceAlignmentMarker probe (Protocol.outgoingAlignmentCut subscription AlignmentDomain.initialStoreRevision)
  early <- checkedIO (Live.receiveDisappearanceAlignmentMarker source marker predecessor)
  (opened, _) <- checkedIO (Live.applyDisappearanceOpen projected early)
  held <- foldM (\current control -> fst <$> checkedIO (AlignmentCoordinator.applyAlignmentTransferControl binding control current)) opened controls
  prepared <-
    checkedIO
      ( Disappearance.prepareProjectedTerminal
          (Protocol.ProjectedDisappearanceAborted probe Protocol.ProjectedAuthorizedAbort (Identity.controlIndex 2))
          (startupDisappearanceState held)
      )
  let terminal = replaceStartupDisappearanceState (fst (Disappearance.commitDisappearanceTransition prepared)) held
  (rejected, effects) <-
    foldM
      ( \(current, accumulated) _ -> do
          (next, emitted, _) <- checkedIO (AlignmentCoordinator.redriveDeferredDestinationControls current)
          pure (next, accumulated <> effectBatchMembers emitted)
      )
      (terminal, [])
      controls
  assertEqual
    "semantic remote violation clears only the retained subscription queue"
    []
    (Transfer.deferredDestinationControls subscription (Alignment.alignmentTransferState (startupAlignmentState rejected)))
  assertBool
    "semantic malformed replay rejects the exact current peer connection"
    (RejectPeerConnection binding ClosePeerControlProtocol `elem` effects)
  assertBool "malformed replay never changes the Store" (startupStoreState rejected == startupStoreState held)
  pure True

propEarlierHeldWork :: QC.Property
propEarlierHeldWork = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
  (_, opened, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState opened) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected live Open witness"
  ingress <- case liveRegularDisappearanceRewriteInput of
    ApplicationRequestInput request -> pure request
    _ -> assertFailure "expected fixture request"
  (held, _) <- checkedIO (ApplicationCall.applyApplicationRequest ingress opened)
  let oldProbe = Protocol.projectedProbeId projected
  prepared <-
    checkedIO
      ( Disappearance.prepareProjectedTerminal
          (Protocol.ProjectedDisappearanceAborted oldProbe Protocol.ProjectedAuthorizedAbort (Identity.controlIndex 2))
          (startupDisappearanceState held)
      )
  let oldTerminal = replaceStartupDisappearanceState (fst (Disappearance.commitDisappearanceTransition prepared)) held
  nextCutSnapshot <- checkedIO (Evidence.disappearanceEvidenceSnapshotForHerald subject oldTerminal)
  assertBool
    "old-terminal held work remains a blocker when the next subject cut is read"
    (Protocol.HeldPublicationBlocker `elem` fmap Protocol.disappearanceBlockerClass (Evidence.disappearanceEvidenceSnapshotBlockers nextCutSnapshot))
  assertEqual
    "the next cut cannot copy the old probe's held-work exclusion"
    Nothing
    (Evidence.disappearanceEvidenceSnapshotAbsenceAttestation nextCutSnapshot)
  pure True

propPeerGate :: Bool -> QC.Property
propPeerGate resolveWins = QC.once . QC.ioProperty $ do
  let (seed, subject) = liveRegularDisappearanceFixture
  (oracle, ready, binding) <- prepareLiveResolve seed subject
  resolveEnvelope <- envelopeFor isResolve ready
  let (input, publication, direction, sequenceNumber) = liveRegularDisappearancePeerRewrite ready
      identifier = checkedPublicationId publication
  before <- checkedIO (PeerStreamState.incomingWatermarks direction (startupPeerStreamState ready))
  (held, _) <- step "admit matching peer publication after its completed probe marker" input ready
  record <-
    maybe
      (assertFailure "peer gate lost incoming publication")
      pure
      (Publication.lookupIncomingPublication identifier (startupPublicationState held))
  assertEqual
    "post-marker peer work is held before authentication or Store application"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition record)
  assertBool
    "retained peer publication names an exact probe dependency"
    (not (Set.null (Publication.incomingPublicationDisappearanceGates record)))
  assertBool "peer gate leaves Store unchanged" (startupStoreState held == startupStoreState ready)
  heldPrefix <- checkedIO (PeerStreamState.incomingWatermarks direction (startupPeerStreamState held))
  assertEqual
    "the whole peer item is received"
    (Just sequenceNumber)
    (PeerStream.streamPrefixSequence (PeerStream.streamReceivedPrefix heldPrefix))
  assertEqual
    "received post-marker item is not semantically completed"
    (PeerStream.streamCompletedPrefix before)
    (PeerStream.streamCompletedPrefix heldPrefix)
  invalidateEnvelope <- envelopeFor isInvalidate held
  (repeated, _) <- step "repeat the exact held peer item" input held
  assertBool "equal held peer input preserves retained owner state" (repeated == held)
  assertEqual
    "one probe owns only one stable Invalidate intention"
    1
    (length [() | (_, witness) <- Client.oracleClientRequestEntries (startupOracleClientState held), isInvalidate (Client.oracleRequestWitnessIntent witness)])
  (_, released, _) <-
    submitEnvelope
      "serialize peer gate terminal winner"
      binding
      oracle
      held
      (if resolveWins then resolveEnvelope else invalidateEnvelope)
  releasedRecord <-
    maybe
      (assertFailure "terminal erased immutable peer record")
      pure
      (Publication.lookupIncomingPublication identifier (startupPublicationState released))
  assertEqual
    "terminal removes the exact probe dependency"
    Set.empty
    (Publication.incomingPublicationDisappearanceGates releasedRecord)
  releasedPrefix <- checkedIO (PeerStreamState.incomingWatermarks direction (startupPeerStreamState released))
  assertEqual
    "terminal releases semantic completion for the complete retained item"
    (Just sequenceNumber)
    (PeerStream.streamPrefixSequence (PeerStream.streamCompletedPrefix releasedPrefix))
  assertBool
    "terminal outcome preserves the expected peer destination disposition"
    ( (if resolveWins then Publication.DestinationTerminallyIgnored else Publication.DestinationApplied)
        `elem` Map.elems (Publication.incomingPublicationDestinationOutcomes releasedRecord)
    )
  assertEqual "released peer owner is valid" (Right ()) (validateHeraldState released)
  pure True
  where
    isResolve Client.ResolveDisappearanceProbeIntent {} = True
    isResolve _ = False
    isInvalidate Client.InvalidateDisappearanceProbeIntent {} = True
    isInvalidate _ = False

checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure

-- Select a dispatch by its retained semantic intention rather than assuming
-- that only one independent request can be ready in the serialized owner.
envelopeFor :: (Client.OracleSemanticIntent -> Bool) -> HeraldState -> IO OracleEnvelope
envelopeFor predicate state = case [ canonicalOracleEnvelopeValue (Client.oracleRequestDispatchEnvelope dispatch)
                                   | ClientActions.SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions client,
                                     Just witness <- [Client.lookupOracleRequest (Client.oracleRequestDispatchRef dispatch) client],
                                     predicate (Client.oracleRequestWitnessIntent witness)
                                   ] of
  [envelope] -> pure envelope
  found -> assertFailure ("expected one matching request, observed " <> show (length found))
  where
    client = startupOracleClientState state
