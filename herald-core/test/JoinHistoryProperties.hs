{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module JoinHistoryProperties (tests, capturedFixture, capturedBeginEntry, activatedFixture, establishCut, request, submit, submitCommand, deliverEntry, advance, checked) where

import ApplicationLabelProperties qualified as LabelFixture
import ConfiguredProcessProperties qualified as Configured
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, forM_)
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64, Word8)
import DisappearanceLiveProperties qualified as LiveDisappearance
import Eclips.Application.Types.Operation (ApplicationOperation (NewEnvironmentApplication))
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity
import Eclips.Domain.Label (labelRecordReleasedState, releasedLabelStateGeneration)
import Eclips.Domain.Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Publication (mkCheckedPublication)
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak))
import Eclips.Domain.Startup (appliedProcessEpochId)
import Eclips.Domain.Structural (structuralPrefixSequence, structuralVersionVectorComponent)
import Eclips.Domain.StructuralConsequence (processEndCause)
import Eclips.Domain.Topology
import Eclips.Domain.Value (canonicalValueByteString, decodeCanonicalValue)
import Eclips.Herald.Administration qualified as Admin
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.History qualified as AlignmentHistory
import Eclips.Herald.Alignment.Protocol qualified as Evidence
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Request.Internal (requestId)
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Disappearance.State qualified as DisappearanceState
import Eclips.Herald.Discovery qualified as PeerDiscovery
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Initialization (initialHerald, initialHeraldWithOracleVoters, primordialApplicationAttachment)
import Eclips.Herald.Input
import Eclips.Herald.Join qualified as Join
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.State qualified as JoinState
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient
import Eclips.Herald.OracleClient.Request (OracleRequestRef (..))
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Placement (PlacementUpdate (FullPlacementSnapshot), deltaRouteDelta, deltaRouteOccurrenceId, deltaRouteSortId, placementSnapshotRoutes)
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.Alignment qualified as AlignmentCoordinator
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.ControlBase qualified as ControlBase
import Eclips.Herald.UseCase.JoinHistory qualified as JoinReplay
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.UseCase.OracleAdvance (applyFencedOracleIngress, structuralReconciliationViews)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label (LiveTerminalOutcomeView (LiveReleasedOutcomeView), liveTerminalOutcomeView)
import Eclips.Oracle.Projection qualified as OracleEvents
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.ReceiptRetirement
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GenesisFixtures qualified as Fixtures
import NativeVoterFailureFixtures qualified as NativeFailure
import NewEnvironmentProperties qualified as EnvironmentFixture
import Step16RegularRetirementAcceptanceProperties (liveRegularDisappearanceFixture, liveRegularDisappearanceRewriteInput)
import StructuralProgressProperties (assertIndependentProgressEvidence)
import System.Mem (performGC)
import System.Mem.Weak (deRefWeak, mkWeakPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Positive (..), Property, choose, conjoin, forAll, ioProperty, shuffle, sublistOf, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "Herald join semantic history"
    [ testCase "founder captures a canonical real cut and observer imports it without serving" caseCaptureReplay,
      testCase "local diagnostic policy preserves capture bytes, wire checks and receiver policy" caseCaptureDiagnostics,
      testCase "checked control-base preparation rejects serving and advanced receivers" caseControlBaseReceiver,
      testCase "a checked control base releases the donor and its owner products" caseControlBaseCaptureRetention,
      testCase "the same capture retries across delayed Begin projection without a new admission" caseCaptureWatchLag,
      testCase "a new semantic attempt releases only the previous capture barrier" caseCaptureAttemptInvalidation,
      testCase "a private replay context follows actual attempt invalidation and preserves native genesis evidence" caseJoiningReplayContextAttempt,
      testCase "a private replay context preserves the current seal and rejects ineligible owners" caseJoiningReplayContextSeal,
      testProperty "private replay contexts import compact bases across distinct control suffix lengths" propJoiningReplayContextSuffix,
      testProperty "atomic replacement preserves live bookkeeping across compact control suffix lengths" propJoiningReplacementSuffix,
      testProperty "a donor ahead advances the complete joining observer across distinct control suffix lengths" propJoiningReplacementAhead,
      testCase "a donor ahead preserves administrative bindings and retired request receipts" caseJoiningReplacementAheadReceipts,
      testCase "sealed replacement preserves administrative bindings and receipts through activation" caseJoiningReplacementActivation,
      testCase "replacement rejects an unavailable exact live control tail and ineligible receivers" caseJoiningReplacementBoundaries,
      testCase "atomic replacement releases discarded nontrivial staging owners" caseJoiningReplacementRetention,
      testCase "an incomplete accepted label workflow prevents history capture" caseCaptureDuringLabel,
      testCase "an accepted label decision invalidates a later join attempt in the same canonical entry" caseLabelDecisionInvalidatesJoin,
      testCase "original newenv stamps and End suppression survive single-source history replay" caseStructuralHistory,
      testCase "single-source regular disappearance replay suppresses without local probe evidence" (caseDisappearanceHistory False),
      testCase "retired and reintroduced regular definitions keep distinct meaning in a portable carrier base" (caseDisappearanceHistory True),
      testCase "single-source controlled disappearance replay imports deletion without a local probe" caseControlledDisappearanceHistory,
      testCase "single-source released label replay imports semantics without old-member fencing" caseLabelHistory,
      testCase "sealed history and exact later controls admit the observer" caseActivation,
      testCase "original portable preseal capture admits a sealed-startup observer" casePortableSealedStartup,
      testCase "paired base accepts the ordinary seal and activation suffix" caseBaseActivation,
      testCase "reclaimed paired control prefix supports Oracle progress after activation" caseReclaimedBaseProgress,
      testCase "joined private placement promotes old covered debt only at the successor membership cut" caseJoiningPlacementSkipsPredecessorTopology,
      testCase "cancellation releases the founder but leaves the applicant restricted" caseCancellation,
      testCase "malformed onboarding messages return one rejection without owner changes" caseMalformed,
      testCase "a missing control coordinate cannot install a frozen history" caseControlGap,
      testCase "seal builder requires exact old-member source coverage" caseSealCoverage,
      testCase "single-source replay preserves complete controls despite sparse client evidence" caseSparseControlHistory,
      testCase "a covered control anchor captures an empty suffix with exact pins kept separately" (caseCoveredHistoryCapture False),
      testCase "a covered control anchor captures and installs only the contiguous later suffix" (caseCoveredHistoryCapture True),
      testCase "admission control tails survive covered portable capture and release independently of exact receipts" caseAdmissionControlTailRetention,
      testCase "joining replacement advances the applicant's control-tail floor to its imported source" caseAdmissionControlTailReplacement,
      testProperty "paired reclamation agrees with checked owner composition across receipt pins and admission tails" propPairedControlReclamation,
      testProperty "canonical cancellation releases donor and applicant tails before or after source import" propAdmissionControlTailCancellation,
      testCase "compact History rejects malformed anchors and noncontiguous suffixes" caseCoveredHistoryCodec,
      testProperty "ordered History coverage agrees with exhaustive membership across missing and reordered occurrences" propHistoryOccurrenceCoverage,
      testCase "a compact joining base accepts the ordinary seal and activation suffix" caseCoveredHistoryActivation,
      testProperty "compact bases preserve semantics across distinct rejected control suffix lengths" propCoveredHistorySuffix,
      testCase "control synchronization reads exactly the suffix after the applied cursor" caseJoinControlTail,
      testCase "Join status preserves distinct observed and semantic cursors" caseJoinStatusCursors,
      testCase "staggered immutable captures choose an existing certified descendant cut" caseStaggeredSeal,
      testCase "live onboarding buffers complete compact donor sets before atomic adoption" caseLiveJoiningSources,
      testCase "completed live history retries do not rebuild after sealed control reclamation" caseLiveJoiningDuplicateAfterReclamation,
      testCase "new-attempt partial sources retire stale buffered envelopes before first import" caseLiveJoiningAttemptPruning,
      testCase "a second live join requires both actual predecessor members" caseLiveSecondAdmission,
      testCase "portable joining collection adopts the newest certified donor base" (casePortableJoiningSources False),
      testCase "newest topology outranks a different donor's larger control prefix" (casePortableJoiningSources True),
      testCase "portable joining collection rejects unavailable control beyond the selected base" casePortableJoiningSourceGap,
      testCase "same-cut portable donors produce a stable import in either source order" casePortableJoiningSourceTie,
      testCase "a collected joining base activates with the least member's alignment authority" casePortableJoiningSourcesActivation,
      testProperty "portable donor arrival order preserves the complete imported state"
        $ forAll (shuffle [False, True])
        $ \order ->
          ioProperty $ do
            (_, _, first, second, observer) <- portableJoiningSourcesFixture True
            let donors = [portableDonor (if later then second else first) | later <- order]
                expected = checked (ControlBase.installPortableJoiningBases [portableDonor first, portableDonor second] observer)
                actual = checked (ControlBase.installPortableJoiningBases donors observer)
            pure $ conjoin [(fst actual == fst expected) === True, effectBatchMembers (snd actual) === effectBatchMembers (snd expected)],
      testProperty "staggered source arrival order yields the same exact seal"
        $ forAll (shuffle [False, True])
        $ \order ->
          ioProperty $ do
            (record, first, second, _) <- staggeredFixture
            pure (Join.prepareJoinSeal record [if later then second else first | later <- order] === Join.prepareJoinSeal record [first, second]),
      testProperty "duplicate donors preserve the imported data and certified cut"
        $ forAll (shuffle [False, True, False, True])
        $ \order ->
          ioProperty $ do
            (record, first, second, _) <- staggeredFixture
            let genesis = checked (checkJoiningHeraldGenesis Fixtures.fixtureCheckedGenesis record)
                selected = checked (checkInitialBootstraps Fixtures.fixtureCheckedGenesis (PrimordialProcessManifest []))
                observer = fst (checked (initialHerald (monotonicInstant 1) genesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
                install state source = firstOf3 (request (Join.InstallJoinHistory (History.encodeJoinSourceHistory source)) state)
                once = foldl install observer [first, second]
                repeated = foldl install observer [if later then second else first | later <- order]
            pure
              $ conjoin
                [ (startupStoreState repeated == startupStoreState once) === True,
                  startupStructuralProgressState repeated === startupStructuralProgressState once,
                  startupPlacementState repeated === startupPlacementState once,
                  Placement.currentPlacementRouteEntries (startupPlacementState repeated) === Placement.currentPlacementRouteEntries (startupPlacementState observer),
                  JoinState.importedSystemViewObservations (startupJoinState repeated) === JoinState.importedSystemViewObservations (startupJoinState once),
                  History.joinHistoryInstalled (History.sourceJoinHistory first) repeated === True,
                  History.joinHistoryInstalled (History.sourceJoinHistory second) repeated === True,
                  validateHeraldState repeated === Right ()
                ],
      testCase "command tokens retain exact retry and reject conflicting reuse" caseCommandTokens,
      testCase "pending receipt blocks attached work and retired commands cannot run again" caseReceiptRetirement,
      testProperty "out-of-order completed Join suffix releases around a pending hole and another owner" propReceiptRetirement,
      testProperty "transfer cleanup releases exactly the selected attempts in any order without reopening" propTransferCleanup,
      testCase "Oracle bootstrap and read-only status share the checked initial configuration" caseOracleStatus,
      testCase "retained voter failure inspection survives reclamation and frozen semantic progress" caseOracleVoterHostFailure,
      testProperty "request variants keep canonical bytes in every order"
        $ forAll (shuffle requestVariants)
        $ \requests ->
          conjoin [Join.decodeJoinRequest (Join.encodeJoinRequest candidate) === Right candidate | candidate <- requests],
      testProperty "reply variants keep canonical bytes in every order"
        $ forAll (shuffle replyVariants)
        $ \replies -> conjoin [Join.decodeJoinReply (Join.encodeJoinReply reply) === Right reply | reply <- replies],
      testProperty "duplicate semantic history installation remains exact and restricted" $ \(Positive repeats) ->
        let (_, _, _, founder, applicant) = capturedFixture
            install state = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder)) state)
            once = install applicant
            repeated = iterate install once !! (repeats `mod` 5)
         in conjoin
              [ (startupStoreState repeated == startupStoreState once) === True,
                startupStructuralProgressState repeated === startupStructuralProgressState once,
                startupPlacementState repeated === startupPlacementState once,
                (startupSortRegistryState repeated == startupSortRegistryState once) === True,
                JoinState.importedSystemViewObservations (startupJoinState repeated) === JoinState.importedSystemViewObservations (startupJoinState once),
                Client.oracleClientAppliedCursor (startupOracleClientState repeated) === Client.oracleClientAppliedCursor (startupOracleClientState once),
                validateHeraldState repeated === Right (),
                serving repeated === False
              ]
    ]

caseCaptureReplay :: Assertion
caseCaptureReplay = do
  let (_, record, history, founder, observer) = capturedFixture
      bytes = sourceBytes founder
      (imported, reply, effects) = request (Join.InstallJoinHistory bytes) observer
  case JoinReplay.replayJoinHistory history imported of
    Left problem -> assertFailure ("based history replay: " <> show problem)
    Right _ -> pure ()
  assertEqual "complete canonical source roundtrip" (Right (captureSource founder)) (History.decodeJoinSourceHistory bytes)
  assertEqual "nested semantic History still roundtrips" (Right history) (History.decodeJoinHistory (History.encodeJoinHistory history))
  assertEqual "the source receipt digests the complete aggregate bytes" (deriveHeraldJoinDigest bytes) (joinMemberStoreDigest (History.joinSourceMemberCut (captureSource founder)))
  let (rawRejected, rawReply, _) = request (Join.InstallJoinHistory (History.encodeJoinHistory history)) observer
  assertEqual "raw nested History is not an onboarding wire envelope" Join.JoinRejected rawReply
  assertBool "raw History rejection leaves the observer unchanged" (rawRejected == observer)
  assertEqual
    "capture retains every exact placement snapshot"
    (Placement.retainedPlacementSnapshots (startupPlacementState founder))
    (History.joinHistoryPlacementSnapshots history)
  assertBool "the donor supplies actual retained placement coordinates" (not (null (History.joinHistoryPlacementSnapshots history)))
  assertEqual "trailing history bytes rejected" True (isLeft (History.decodeJoinSourceHistory (bytes <> "x")))
  assertEqual "capture binds the allocated admission" (admissionRecordId record) (History.joinHistoryAdmission history)
  assertEqual "idle initial source is captured through a real established cut" 1 (length (History.joinHistoryTopologyCertificates history))
  assertEqual "source cannot claim an uninstalled history" True (History.joinHistoryInstalled history founder)
  assertEqual "complete import receipt" Join.JoinHistoryInstalled reply
  assertEqual "observer imports exact data cut" True (History.joinHistoryInstalled history imported)
  assertEqual
    "placement evidence does not change the receiver's live route view"
    (Placement.currentPlacementRouteEntries (startupPlacementState observer))
    (Placement.currentPlacementRouteEntries (startupPlacementState imported))
  assertEqual
    "placement evidence does not admit a live remote owner"
    (Placement.remotePlacementOwners (startupPlacementState observer))
    (Placement.remotePlacementOwners (startupPlacementState imported))
  assertEqual
    "placement evidence does not advance the receiver's local advertisement"
    (Placement.localPlacementSequence (startupPlacementState observer))
    (Placement.localPlacementSequence (startupPlacementState imported))
  assertEqual
    "semantic data alone cannot certify a history missing its exact placement evidence"
    False
    (History.joinHistoryInstalled history (replaceStartupPlacementState (startupPlacementState observer) imported))
  assertEqual "capture does not admit the applicant" False (serving imported)
  assertEqual "observer emits no ordinary external work" [] [effect | effect <- effectBatchMembers effects, case effect of SendJoinReply {} -> False; _ -> True]
  assertEqual "observer has no old report" [] (Progress.structuralReportEntries (startupStructuralProgressState imported))
  assertEqual "complete owner invariants" (Right ()) (validateHeraldState imported)
  assertEqual "ready is unavailable before seal" Join.JoinRetry (secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) imported))
  assertPreparedControlBase founder observer imported

caseCaptureDiagnostics :: Assertion
caseCaptureDiagnostics = do
  let (_, record, history, founder, observer) = capturedFixture
      disabled = configureHeraldDiagnosticChecks DiagnosticChecksDisabled
      source = History.joinHistorySource history
      checkedBase = fromJust (checked (ControlBase.captureJoiningBase founder))
      uncheckedBase = fromJust (checked (ControlBase.captureJoiningBase (disabled founder)))
      bytes = ControlBase.encodeJoiningBase uncheckedBase
      clientCapture mode = Client.captureJoiningClientBaseWithDiagnostics mode (startupOracleProjectionState founder)
      progress = Progress.captureStructuralProgressBase (startupStructuralProgressState founder)
      independentlyEncoded =
        Serialize.encode
          ( "ECLIPS-HERALD-JOINING-BASE" :: BS.ByteString,
            Projection.encodeProjectionBase (checked (Projection.captureProjectionBase (startupOracleProjectionState founder))),
            Registry.encodeSortRegistryBase (Registry.captureSortRegistryBase (startupSortRegistryState founder)),
            Progress.encodeStructuralProgressBaseEvidence (Progress.captureStructuralProgressBaseEvidence progress),
            Reconciliation.encodeStructuralCarrierBase (Progress.structuralProgressBaseCarrierBase progress),
            History.encodeJoinHistory history
          )
  assertEqual "local capture diagnostics do not alter portable bytes" (ControlBase.encodeJoiningBase checkedBase) bytes
  assertEqual "shared capture products preserve independently encoded canonical bundle bytes" independentlyEncoded bytes
  assertEqual "nested History capture is unchanged" (History.captureJoinHistory record founder) (History.captureJoinHistoryWithDiagnostics DiagnosticChecksDisabled record founder)
  assertEqual "nested Client capture is unchanged" (clientCapture DiagnosticChecksEnabled) (clientCapture DiagnosticChecksDisabled)
  mapM_
    ( \mode -> do
        let local = configureHeraldDiagnosticChecks mode observer
            context = checked (ControlBase.prepareJoiningReplayContext local)
            successor = checked (ControlBase.replacePortableJoiningBases [(source, bytes)] local)
            (unchanged, rejection, _) = request (Join.InstallJoinHistory (bytes <> "x")) local
        assertEqual "private replay inherits only receiver diagnostic policy" mode (startupDiagnosticChecks context)
        assertEqual "replacement preserves receiver diagnostic policy" mode (startupDiagnosticChecks successor)
        assertEqual "independent whole-owner audit accepts either policy" (Right ()) (validateHeraldState successor)
        assertEqual "malformed wire remains rejected under either policy" Join.JoinRejected rejection
        assertBool "malformed wire leaves the receiver unchanged" (unchanged == local)
    )
    [DiagnosticChecksEnabled, DiagnosticChecksDisabled]

caseControlBaseReceiver :: Assertion
caseControlBaseReceiver = do
  let (_, _, _, founder, observer) = capturedFixture
      base = checked (ControlBase.captureControlBase founder)
      imported = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder)) observer)
  assertBool "serving owner cannot adopt an observer base" (isLeft (ControlBase.prepareControlBase base founder))
  assertBool "an advanced observer is not overwritten" (isLeft (ControlBase.prepareControlBase base imported))

-- Construct a complete local label before capture, so the donor's Controlled,
-- LabelPatch and application owners are real runtime allocations, not shared
-- empty-state constants. Only weak handles escape alongside the portable base.
{-# NOINLINE captureFreshControlBase #-}
captureFreshControlBase :: DiagnosticChecks -> Word64 -> IO ((ControlBase.CheckedControlBase, ControlBase.CheckedJoiningBase), HeraldState, [(String, IO Bool)])
captureFreshControlBase checks observedAt = do
  (pendingSeed, _, _, labelRequest, _) <- LabelFixture.settledNeutralControlledFixtureWithApplicationRequests
  seed <- LabelFixture.completeAssignedPeerPublications pendingSeed
  let oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis seed) (startupInitialBootstraps seed)))
      labelStep at state = fst (checked (stepHerald (heraldInput (monotonicInstant at) (ApplicationRequestInput labelRequest)) state))
      requested = labelStep observedAt seed
  (oracle1, opened, _) <- LabelFixture.commitNextLabelRequest (fromIntegral (observedAt + 1)) oracle0 requested
  (oracle2, released, _) <- LabelFixture.commitCompleteLabelWorkflow (fromIntegral (observedAt + 3)) oracle1 (labelStep (observedAt + 2) opened)
  let anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews released) (startupStructuralProgressState released))
      (oracle3, entry) = submit (BeginHeraldAdmission manifest anchor) oracle2
      record = fromJust (oraclePendingHeraldAdmission oracle3)
      genesis = checked (checkJoiningHeraldGenesis (startupGenesis seed) record)
      observer = fst (checked (initialHerald (monotonicInstant 1) genesis (startupInitialBootstraps seed) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  begun <- settleControlBaseCut (deliverEntry entry released)
  donor <- evaluate (configureHeraldDiagnosticChecks checks begun)
  witnesses <-
    sequence
      [ weakOwner "Herald" donor,
        weakOwner "Projection" (startupOracleProjectionState donor),
        weakOwner "Client" (startupOracleClientState donor),
        weakOwner "Controlled" (startupControlledState donor),
        weakOwner "LabelPatch" (startupLabelPatchState donor),
        weakOwner "Application" (startupApplicationState donor),
        weakOwner "Progress" (startupStructuralProgressState donor),
        weakOwner "Reconciliation" (Progress.structuralProgressReconciliation (startupStructuralProgressState donor))
      ]
  capture <- evaluate (checked (ControlBase.captureControlBase donor))
  joinedCapture <- evaluate (fromJust (checked (ControlBase.captureJoiningBase donor)))
  freshObserver <- evaluate observer
  pure ((capture, joinedCapture), freshObserver, witnesses)
  where
    weakOwner name value = do
      owner <- evaluate value
      witness <- mkWeakPtr owner Nothing
      pure (name, maybe False (const True) <$> deRefWeak witness)

caseControlBaseCaptureRetention :: Assertion
caseControlBaseCaptureRetention =
  mapM_
    ( \mode -> do
        observedAt <- (1000 +) . fromIntegral . hashUnique <$> newUnique
        (capture, observer, witnesses) <- captureFreshControlBase mode observedAt
        bracket (newStablePtr capture) freeStablePtr $ \root -> do
          performGC
          mapM_ (\(name, alive) -> alive >>= assertEqual (name <> " owner is not retained by the portable base with " <> show mode) False) witnesses
          (retained, joiningBase) <- deRefStablePtr root
          let (joined, _) = checked (ControlBase.installJoiningBase joiningBase observer)
              bytes = ControlBase.encodeJoiningBase joiningBase
              history = ControlBase.joiningBaseHistory joiningBase
          (_, _, _, _, _, historyBytes) <- either assertFailure pure (Serialize.decode bytes :: Either String RawPortableJoiningBase)
          assertEqual "cached History bytes survive without donor owners" (History.encodeJoinHistory history) historyBytes
          assertEqual "paired base survives without donor owners" (Right ()) (validateHeraldState joined)
          let prepared = checked (ControlBase.prepareControlBase retained observer)
          assertBool "the surviving capture retains a nonzero canonical cut" (ControlBase.controlBaseControlIndex retained > controlIndex 0)
          assertEqual "the retained capture still prepares an independently valid projection" (Right (Projection.stateWitness (ControlBase.preparedControlBaseProjection prepared))) (Projection.validateState (ControlBase.preparedControlBaseProjection prepared))
          assertBool "the retained capture preserves its completed label facts" (not (null (Controlled.controlledReleasedLabelEntries (ControlBase.preparedControlBaseControlled prepared))))
    )
    [DiagnosticChecksEnabled, DiagnosticChecksDisabled]

-- Compare both the staged control preparation and the atomic paired import
-- against the complete checked semantic import.
assertPreparedControlBase :: HeraldState -> HeraldState -> HeraldState -> Assertion
assertPreparedControlBase source observer replayed = do
  let base = checked (ControlBase.captureControlBase source)
      prepared = checked (ControlBase.prepareControlBase base observer)
      projection = ControlBase.preparedControlBaseProjection prepared
      controlled = ControlBase.preparedControlBaseControlled prepared
      client = ControlBase.preparedControlBaseClient prepared
      replayedControlled = startupControlledState replayed
      replayedClient = startupOracleClientState replayed
  assertEqual "base binds the exact shared cut" (Client.oracleClientAppliedCursor replayedClient) (ControlBase.controlBaseControlIndex base)
  assertEqual "semantic projection supplies the same predecessors as donor local installation" (LabelPatch.captureCheckedLabelPatchBase (startupLabelPatchState source)) (Right (LabelPatch.labelPatchBaseFromProjection (startupOracleProjectionState source)))
  assertEqual "semantic projection supplies donor global controls without local resources" (Right (Controlled.captureControlledControlBase (startupControlledState source))) (Controlled.controlledControlBaseFromProjection (startupOracleProjectionState source))
  assertEqual "imported projection independently validates its covered base" (Right (Projection.stateWitness projection)) (Projection.validateState projection)
  assertBool "projection meaning agrees with complete receiver import" (Projection.oracleView (startupOracleProjectionState replayed) == Projection.oracleView projection)
  assertEqual "receiver projection identity survives" (Projection.oracleViewLocalHeraldEpoch (Projection.oracleView (startupOracleProjectionState observer))) (Projection.oracleViewLocalHeraldEpoch (Projection.oracleView projection))
  assertEqual "client validates before composition" (Right ()) (Client.validateState client)
  assertEqual "client applied cursor agrees" (Client.oracleClientAppliedCursor replayedClient) (Client.oracleClientAppliedCursor client)
  assertEqual "client semantic retirement agrees" (Client.oracleClientProgressReady replayedClient) (Client.oracleClientProgressReady client)
  assertEqual "client remains receiver-owned" (Client.oracleClientHelloClaims (startupOracleClientState observer)) (Client.oracleClientHelloClaims client)
  assertEqual "donor result handles are not imported" [] (Client.oracleClientRequestEntries client)
  assertEqual "discovery membership agrees with canonical replay" (Discovery.currentMembership (startupDiscoveryState replayed)) (Discovery.currentMembership (ControlBase.preparedControlBaseDiscovery prepared))
  assertEqual "discovery catalogue agrees with canonical replay" (Discovery.liveHeraldCatalogue (startupDiscoveryState replayed)) (Discovery.liveHeraldCatalogue (ControlBase.preparedControlBaseDiscovery prepared))
  assertEqual "current labels agree with full replay" (Controlled.controlledReleasedLabelEntries replayedControlled) (Controlled.controlledReleasedLabelEntries controlled)
  assertEqual "terminal deletions agree with full replay" (Controlled.controlledTerminalDeletionEntries replayedControlled) (Controlled.controlledTerminalDeletionEntries controlled)
  assertEqual "only appropriate process metadata is observed" (Controlled.controlledProcessFacts replayedControlled) (Controlled.controlledProcessFacts controlled)
  assertEqual "only selected genesis roots are observed" (Controlled.controlledRootFacts replayedControlled) (Controlled.controlledRootFacts controlled)
  assertEqual "imported CAS predecessors agree independently of donor role" (LabelPatch.captureCheckedLabelPatchBase (startupLabelPatchState replayed)) (LabelPatch.captureCheckedLabelPatchBase (ControlBase.preparedControlBaseLabels prepared))
  assertEqual "imported proof audit accepts receiver-owned full replay without donor installation roles" (Right ()) (validateHeraldState (replaceStartupLabelPatchState (ControlBase.preparedControlBaseLabels prepared) replayed))
  assertEqual "portable structural evidence remains staged" (Reconciliation.captureStructuralControlBase (Progress.structuralProgressReconciliation (startupStructuralProgressState source))) (ControlBase.preparedControlBaseStructural prepared)
  assertBool "preparation is deterministic" (ControlBase.prepareControlBase base observer == Right prepared)
  assertEqual "preparation has not changed the observer cursor" (controlIndex 0) (Client.oracleClientAppliedCursor (startupOracleClientState observer))
  assertJoiningBase source observer replayed

assertJoiningBase :: HeraldState -> HeraldState -> HeraldState -> Assertion
assertJoiningBase source observer replayed = do
  captureResult <- either (assertFailure . ("paired base capture: " <>) . show) pure (ControlBase.captureJoiningBase source)
  captured <- maybe (assertFailure "paired base source is not ready") pure captureResult
  (imported, effects) <- either (assertFailure . ("paired base install: " <>) . show) pure (ControlBase.installJoiningBase captured observer)
  let importedProgress = startupStructuralProgressState imported
      replayedProgress = startupStructuralProgressState replayed
      history = fromJust (checked (History.captureJoinHistory (fromJust (Projection.oracleViewPendingHeraldAdmission (Projection.oracleView (startupOracleProjectionState source)))) source))
  assertEqual "atomic paired base satisfies the full Herald audit" (Right ()) (validateHeraldState imported)
  assertEqual "base adoption keeps the applicant restricted" False (serving imported)
  assertEqual "paired import covers the exact history" True (History.joinHistoryInstalled history imported)
  assertBool "carrier/control history matches interleaved replay" (Reconciliation.captureStructuralCarrierBase (Progress.structuralProgressReconciliation replayedProgress) == Reconciliation.captureStructuralCarrierBase (Progress.structuralProgressReconciliation importedProgress))
  assertEqual "applied vector matches interleaved replay" (Progress.structuralAppliedVector replayedProgress) (Progress.structuralAppliedVector importedProgress)
  assertEqual "installed topology certificates match" (Progress.structuralInstalledEstablishedChain replayedProgress) (Progress.structuralInstalledEstablishedChain importedProgress)
  assertEqual "receiver current vertices match replay" (Graph.graphStructuralVertexProjections (startupGraphState replayed)) (Graph.graphStructuralVertexProjections (startupGraphState imported))
  assertEqual "receiver current edges match replay" (Graph.graphStructuralEdgeProjections (startupGraphState replayed)) (Graph.graphStructuralEdgeProjections (startupGraphState imported))
  assertBool "effective and retired sort facts match replay" (Registry.captureSortRegistryBase (startupSortRegistryState replayed) == Registry.captureSortRegistryBase (startupSortRegistryState imported))
  assertEqual "base creates no local application Store resources" [] (Reconciliation.structuralAppliedLocalStores (Progress.structuralProgressReconciliation importedProgress))
  assertEqual "base retains no donor Store incarnations" Map.empty (Reconciliation.structuralAppliedRetainedStoreResources (Progress.structuralProgressReconciliation importedProgress))
  assertEqual "base grants no possessions" [] (Controlled.controlledNormalPossessionEntries (startupControlledState imported))
  assertEqual "base recreates no application alignment debts" [] (Debt.structuralDebtSetEntries (Alignment.alignmentStructuralDebts (startupAlignmentState imported)))
  assertEqual "base starts no application or network effects" [] (effectBatchMembers effects)
  assertBool "receiver identity generator is unchanged" (startupIdGeneratorState imported == startupIdGeneratorState observer)
  assertBool "a serving member cannot adopt a paired base" (isLeft (ControlBase.installJoiningBase captured source))
  assertBool "an advanced observer is never overwritten" (isLeft (ControlBase.installJoiningBase captured imported))
  assertReclaimedControlBase imported
  let sourceProjection = startupOracleProjectionState source
      sourceView = Projection.oracleView sourceProjection
  assertIndependentProgressEvidence
    (Projection.oracleViewHeraldMembershipHistoryChecked sourceView)
    (Projection.projectedHeraldAdmissions sourceProjection)
    (Projection.oracleViewControlIndex sourceView)
    (Progress.structuralProgressReconciliation importedProgress)
    (startupStructuralProgressState observer)
    (startupStructuralProgressState source)
  assertCarrierWire (History.joinHistoryOccurrences history) source observer
  assertPortableJoiningBase captured history source observer imported effects

type RawPortableJoiningBase = (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)

-- Extend the real donor fixtures above: exact equality to the in-memory import
-- also preserves their independent chronological-replay comparisons. Capture
-- happens before sealing in the ordinary fixture; wire admission must not
-- require a later seal to make this first observer snapshot usable.
assertPortableJoiningBase :: ControlBase.CheckedJoiningBase -> History.JoinHistory -> HeraldState -> HeraldState -> HeraldState -> EffectBatch -> Assertion
assertPortableJoiningBase captured history source observer expected expectedEffects = do
  let bytes = ControlBase.encodeJoiningBase captured
      sourceEpoch = History.joinHistorySource history
      wrongSource = Genesis.checkedLocalHeraldEpoch (startupGenesis observer)
      install candidate = ControlBase.installPortableJoiningBase sourceEpoch candidate observer
      reject :: String -> RawPortableJoiningBase -> Assertion
      reject message candidate = assertBool message (isLeft (install (Serialize.encode candidate)))
  (imported, effects) <- either (assertFailure . ("portable paired base install: " <>) . show) pure (install bytes)
  assertBool "portable paired import exactly matches the in-memory owner reconstruction" (imported == expected)
  assertEqual "portable paired import emits exactly the in-memory effects" (effectBatchMembers expectedEffects) (effectBatchMembers effects)
  assertEqual "portable paired import satisfies the complete Herald audit" (Right ()) (validateHeraldState imported)
  assertBool "portable paired import keeps the joining observer restricted" (not (serving imported))
  assertBool "portable paired import retains the original history evidence" (History.joinHistoryInstalled history imported)
  assertBool "fixture has a distinct candidate sender" (wrongSource /= sourceEpoch)
  assertBool "portable paired import binds the declared donor" (isLeft (ControlBase.installPortableJoiningBase wrongSource bytes observer))
  assertBool "portable paired import rejects a serving receiver" (isLeft (ControlBase.installPortableJoiningBase sourceEpoch bytes source))
  assertBool "portable paired import never overwrites an advanced observer" (isLeft (ControlBase.installPortableJoiningBase sourceEpoch bytes imported))
  assertBool "portable paired import rejects trailing bytes" (isLeft (install (bytes <> "x")))
  (domain, projectionFrame, registryFrame, progressFrame, carrierFrame, historyFrame) <- either assertFailure pure (Serialize.decode bytes :: Either String RawPortableJoiningBase)
  assertEqual "portable paired domain is explicit" "ECLIPS-HERALD-JOINING-BASE" domain
  reject "portable paired import rejects another domain" ("ECLIPS-OTHER-JOINING-BASE", projectionFrame, registryFrame, progressFrame, carrierFrame, historyFrame)
  reject "portable paired import rejects trailing bytes inside a nested carrier frame" (domain, projectionFrame, registryFrame, progressFrame, carrierFrame <> "x", historyFrame)
  freshProjection <- either (assertFailure . show) pure (Projection.captureProjectionBase (startupOracleProjectionState observer))
  let freshProjectionFrame = Projection.encodeProjectionBase freshProjection
      freshProgressFrame = Progress.encodeStructuralProgressBaseEvidence (Progress.captureStructuralProgressBaseEvidence (Progress.captureStructuralProgressBase (startupStructuralProgressState observer)))
  -- Both replacement frames independently decode. Their rejection here must
  -- come from the aggregate's common cut/admission binding, not malformed bytes.
  _ <- either (assertFailure . show) pure (Projection.decodeProjectionBase (startupOracleProjectionState observer) freshProjectionFrame)
  _ <- either (assertFailure . show) pure (Progress.decodeStructuralProgressBaseEvidence freshProgressFrame)
  reject "portable paired import rejects a valid Projection frame from another control cut" (domain, freshProjectionFrame, registryFrame, progressFrame, carrierFrame, historyFrame)
  reject "portable paired import rejects a valid Progress frame from another topology cut" (domain, projectionFrame, registryFrame, freshProgressFrame, carrierFrame, historyFrame)

-- A Progress certificate remains historical evidence when its source Join
-- attempt advances or is cancelled. Import its nominal graph history using
-- receiver-owned genesis and current admitted process/sort facts, without
-- asking the live Join coordinator to resurrect that old transfer attempt.
assertHistoricalProgressEvidence :: [Terminal.TerminalStructuralOccurrence] -> HeraldState -> HeraldState -> Assertion
assertHistoricalProgressEvidence originals source observer = do
  let sourceProjection = startupOracleProjectionState source
      sourceView = Projection.oracleView sourceProjection
      sourceProgress = startupStructuralProgressState source
      observerProgress = startupStructuralProgressState observer
      sourceCarriers = Reconciliation.captureStructuralCarrierBase (Progress.structuralProgressReconciliation sourceProgress)
  projectionCapture <- either (assertFailure . show) pure (Projection.captureProjectionBase sourceProjection)
  receiverProjection <- either (assertFailure . show) pure (Projection.installProjectionBase projectionCapture (startupOracleProjectionState observer))
  let receiverFacts = replaceStartupOracleProjectionState receiverProjection (replaceStartupSortRegistryState (startupSortRegistryState source) observer)
  carrierImport <- either (assertFailure . show) pure (Reconciliation.prepareStructuralCarrierBaseImport (structuralReconciliationViews receiverFacts) sourceCarriers (Progress.structuralProgressReconciliation observerProgress))
  assertIndependentProgressEvidence
    (Projection.oracleViewHeraldMembershipHistoryChecked sourceView)
    (Projection.projectedHeraldAdmissions sourceProjection)
    (Projection.oracleViewControlIndex sourceView)
    (Reconciliation.preparedStructuralCarrierBaseSuccessor carrierImport)
    observerProgress
    sourceProgress
  assertCarrierWire originals source observer

-- These are wire grammar aliases, not checked owner products. Mutations below
-- re-encode valid cereal tuples, so rejection exercises the admission boundary.
type RawCarrierSort = (BS.ByteString, BS.ByteString)

type RawCarrierController = (Word8, [BS.ByteString])

type RawCarrierVertex = (Word8, BS.ByteString)

type RawCarrierMeaning = (Word8, Maybe RawCarrierSort, Maybe RawCarrierController, Maybe (RawCarrierVertex, RawCarrierVertex))

type RawCarrierPublication = (BS.ByteString, BS.ByteString, BS.ByteString, Word64, BS.ByteString)

type RawCarrierBaseline = (Word8, RawCarrierSort, Word64, RawCarrierPublication, Maybe RawCarrierMeaning)

type RawCarrierOccurrence = ((BS.ByteString, Word64), Maybe RawCarrierMeaning, Maybe (Word8, BS.ByteString, Word64, Word64))

type RawCarrierControl = (BS.ByteString, (Word8, BS.ByteString, Word64, Word64), Maybe RawCarrierController)

type RawCarrierBase = (BS.ByteString, BS.ByteString, ([BS.ByteString], [BS.ByteString], [(Word64, BS.ByteString)]), [RawCarrierBaseline], [RawCarrierOccurrence], [RawCarrierControl])

type RawCarrierProgressVector = (BS.ByteString, BS.ByteString, [(BS.ByteString, Word64)])

type RawCarrierProgressStamp = ((BS.ByteString, Word64), RawCarrierProgressVector, (BS.ByteString, BS.ByteString, BS.ByteString, Word64), BS.ByteString, Word8)

type RawCarrierProgressOccurrence = (RawCarrierProgressStamp, BS.ByteString, RawCarrierSort, Word64)

type RawCarrierProgressBase = (BS.ByteString, (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString), (RawCarrierProgressVector, Word64), [RawCarrierProgressOccurrence], BS.ByteString)

-- Start from actual captured terminal payloads, checked Projection/Registry
-- authority, and receiver genesis. Neither decoder receives the donor owners.
assertCarrierWire :: [Terminal.TerminalStructuralOccurrence] -> HeraldState -> HeraldState -> Assertion
assertCarrierWire originals source observer = do
  let sourceProjection = startupOracleProjectionState source
      sourceView = Projection.oracleView sourceProjection
      sourceProgress = startupStructuralProgressState source
      observerProgress = startupStructuralProgressState observer
      sourceBase = Progress.captureStructuralProgressBase sourceProgress
      sourceCarriers = Progress.structuralProgressBaseCarrierBase sourceBase
      carrierBytes = Reconciliation.encodeStructuralCarrierBase sourceCarriers
      progressBytes = Progress.encodeStructuralProgressBaseEvidence (Progress.captureStructuralProgressBaseEvidence sourceBase)
      originalMap = Map.fromList [(Terminal.terminalStructuralOccurrenceId occurrence, occurrence) | occurrence <- originals]
      committed = Projection.oracleViewControlIndex sourceView
  progressContext <- either (assertFailure . show) pure (Progress.structuralProgressBaseAdmissionContext (Projection.oracleViewHeraldMembershipHistoryChecked sourceView) (Projection.projectedHeraldAdmissions sourceProjection) committed (Progress.structuralLastInstalledCutId sourceProgress) observerProgress)
  progressClaims <- either (assertFailure . show) pure (Progress.decodeStructuralProgressBaseEvidence progressBytes)
  admitted <- either (assertFailure . show) pure (Progress.admitStructuralProgressBaseEvidence progressContext progressClaims)
  either (assertFailure . show) pure (Progress.validateStructuralProgressOriginalOccurrences admitted originalMap)
  projectionCapture <- either (assertFailure . show) pure (Projection.captureProjectionBase sourceProjection)
  receiverProjection <- either (assertFailure . show) pure (Projection.installProjectionBase projectionCapture (startupOracleProjectionState observer))
  let receiverFacts = replaceStartupOracleProjectionState receiverProjection (replaceStartupSortRegistryState (startupSortRegistryState source) observer)
      views = structuralReconciliationViews receiverFacts
      freshReconciliation = Progress.structuralProgressReconciliation observerProgress
      makeContext =
        Reconciliation.structuralCarrierBaseContext
          (Projection.oracleViewSystemId sourceView)
          committed
          (Progress.admittedStructuralProgressMembershipHistory admitted)
          (startupSortRegistryState source)
          (Progress.admittedStructuralProgressRetirementBases admitted)
          [(lineage, recipe) | (lineage, recipe, _) <- Progress.admittedStructuralProgressAdmissions admitted]
  context <- either (assertFailure . show) pure (makeContext originalMap)
  case Map.toAscList originalMap of
    (identifier, original) : _ -> do
      let mismatched = structuralOccurrenceId (structuralOccurrenceSourceHeraldEpoch identifier) (checked (mkStructuralSequence (structuralSequenceWord64 (structuralOccurrenceSourceSequence identifier) + 1)))
      assertBool "carrier context checks original archive map identities" (isLeft (makeContext (Map.insert mismatched original (Map.delete identifier originalMap))))
    [] -> pure ()
  let decodeCarrier = Reconciliation.decodeStructuralCarrierBase context views freshReconciliation
      importCarrier candidate = do
        decoded <- Bifunctor.first show (decodeCarrier candidate)
        carrierImport <- Bifunctor.first show (Reconciliation.prepareStructuralCarrierBaseImport views decoded freshReconciliation)
        progressImport <- Bifunctor.first show (Progress.prepareStructuralProgressBaseImportFromEvidence admitted decoded (Reconciliation.preparedStructuralCarrierBaseSuccessor carrierImport) observerProgress)
        pure (Progress.commitStructuralProgressBaseImport progressImport)
      rejectCarrier :: String -> RawCarrierBase -> Assertion
      rejectCarrier message candidate = assertBool message (isLeft (decodeCarrier (Serialize.encode candidate)))
      rejectPair :: String -> RawCarrierBase -> Assertion
      rejectPair message candidate = assertBool message (isLeft (importCarrier (Serialize.encode candidate)))
  decoded <- either (assertFailure . show) pure (decodeCarrier carrierBytes)
  imported <- either assertFailure pure (importCarrier carrierBytes)
  assertBool "portable carrier retains the exact admitted base" (decoded == sourceCarriers)
  assertEqual "portable carrier uses one canonical encoding" carrierBytes (Reconciliation.encodeStructuralCarrierBase decoded)
  assertBool "independent carrier and Progress preserve the complete source base" (Progress.captureStructuralProgressBase imported == sourceBase)
  assertEqual "portable carrier creates no application Stores" [] (Reconciliation.structuralAppliedLocalStores (Progress.structuralProgressReconciliation imported))
  assertEqual "portable carrier retains no donor Store incarnations" Map.empty (Reconciliation.structuralAppliedRetainedStoreResources (Progress.structuralProgressReconciliation imported))
  assertEqual "portable carrier and Progress satisfy the owner audit" (Right ()) (Progress.validateStructuralProgressState imported)
  -- Some retained dominated dots have no historical meaning. Preserve both
  -- successful queries and that admitted absence; do not invent old meanings.
  mapM_
    ( \established -> do
        let cut = GraphProtocol.topologyCutEstablishedId established
        assertEqual "portable carriers preserve each historical query result" (Reconciliation.structuralProjectionSnapshotCanonicalBytes <$> Progress.structuralProjectionAtInstalledCut views cut sourceProgress) (Reconciliation.structuralProjectionSnapshotCanonicalBytes <$> Progress.structuralProjectionAtInstalledCut views cut imported)
    )
    (Progress.structuralInstalledEstablishedChain sourceProgress)
  (progressDomain, progressIdentity, progressApplied, progressRows, progressCertificates) <- either assertFailure pure (Serialize.decode progressBytes :: Either String RawCarrierProgressBase)
  let rejectOriginalBinding :: String -> [RawCarrierProgressOccurrence] -> Assertion
      rejectOriginalBinding message candidateRows = do
        claims <- either (assertFailure . show) pure (Progress.decodeStructuralProgressBaseEvidence (Serialize.encode (progressDomain, progressIdentity, progressApplied, candidateRows, progressCertificates)))
        mutated <- either (assertFailure . show) pure (Progress.admitStructuralProgressBaseEvidence progressContext claims)
        assertBool message (isLeft (Progress.validateStructuralProgressOriginalOccurrences mutated originalMap))
  case progressRows of
    [] -> pure ()
    (stamp@(identifier, predecessor, publication, digest, role), topology, sort, prerequisite) : rest -> do
      rejectOriginalBinding "Progress digest is bound to exact original payload" (((identifier, predecessor, publication, BS.map (+ 1) digest, role), topology, sort, prerequisite) : rest)
      case filter ((/= topology) . topologyCutIdBytes) (Progress.structuralGenesisCutId sourceProgress : map GraphProtocol.topologyCutEstablishedId (Progress.structuralInstalledEstablishedChain sourceProgress)) of
        different : _ -> rejectOriginalBinding "Progress source topology is bound to original payload" ((stamp, topologyCutIdBytes different, sort, prerequisite) : rest)
        [] -> pure ()
      if prerequisite < controlIndexWord64 committed
        then rejectOriginalBinding "Progress control prerequisite is bound to original payload" ((stamp, topology, sort, prerequisite + 1) : rest)
        else pure ()
  raw@(domain, generation, metadata, baselines, occurrences, controls) <- either assertFailure pure (Serialize.decode carrierBytes :: Either String RawCarrierBase)
  assertEqual "raw grammar agrees with canonical carrier bytes" carrierBytes (Serialize.encode raw)
  assertBool "carrier rejects trailing bytes" (isLeft (decodeCarrier (carrierBytes <> BS.singleton 0)))
  rejectCarrier "carrier rejects another domain" ("wrong-domain", generation, metadata, baselines, occurrences, controls)
  rejectCarrier "carrier rejects an unadmitted generation" (domain, BS.map (+ 1) generation, metadata, baselines, occurrences, controls)
  let (retirements, admissions, contributions) = metadata
  case retirements of
    retired : _ -> rejectCarrier "carrier retirement references bind admitted certificates" (domain, generation, (retired : retirements, admissions, contributions), baselines, occurrences, controls)
    [] -> pure ()
  case admissions of
    admission : _ -> rejectCarrier "carrier admission references bind admitted certificates" (domain, generation, (retirements, admission : admissions, contributions), baselines, occurrences, controls)
    [] -> pure ()
  case contributions of
    contribution : _ -> rejectCarrier "carrier contributions bind admitted activation coordinates" (domain, generation, (retirements, admissions, contribution : contributions), baselines, occurrences, controls)
    [] -> pure ()
  case baselines of
    [] -> pure ()
    row : rest -> do
      rejectCarrier "carrier rejects duplicate baseline rows" (domain, generation, metadata, row : row : rest, occurrences, controls)
      let (tag, sort, at, publication, meaning) = row
      rejectCarrier "carrier rejects an unknown baseline arm" (domain, generation, metadata, (255, sort, at, publication, meaning) : rest, occurrences, controls)
      rejectCarrier "baseline prerequisite is bounded by admitted control" (domain, generation, metadata, (tag, sort, controlIndexWord64 committed + 1, publication, meaning) : rest, occurrences, controls)
  case break (\(tag, _, _, _, _) -> tag == 0) baselines of
    (before, _ : after) -> rejectCarrier "carrier cannot omit receiver genesis" (domain, generation, metadata, before <> after, occurrences, controls)
    _ -> pure ()
  case baselines of
    firstRow : secondRow : rest -> rejectCarrier "carrier rejects reordered baseline rows" (domain, generation, metadata, secondRow : firstRow : rest, occurrences, controls)
    _ -> pure ()
  case occurrences of
    [] -> pure ()
    row@(identifier, meaning, suppression) : rest -> do
      rejectCarrier "carrier rejects duplicate occurrence rows" (domain, generation, metadata, baselines, row : row : rest, controls)
      rejectPair "Progress requires every paired carrier occurrence" (domain, generation, metadata, baselines, rest, controls)
      rejectCarrier "carrier cannot name an absent original occurrence" (domain, generation, metadata, baselines, ((BS.empty, snd identifier), meaning, suppression) : rest, controls)
      case meaning of
        Just (_, sort, controller, endpoints) -> rejectCarrier "carrier rejects an unknown meaning role" (domain, generation, metadata, baselines, (identifier, Just (255, sort, controller, endpoints), suppression) : rest, controls)
        Nothing -> pure ()
      case suppression of
        Just (_, target, sequenceNumber, floorIndex) -> rejectCarrier "carrier rejects an unknown suppression arm" (domain, generation, metadata, baselines, (identifier, meaning, Just (255, target, sequenceNumber, floorIndex)) : rest, controls)
        Nothing -> pure ()
  case occurrences of
    firstRow : secondRow : rest -> rejectCarrier "carrier rejects reordered occurrence rows" (domain, generation, metadata, baselines, secondRow : firstRow : rest, controls)
    _ -> pure ()
  case Reconciliation.structuralCarrierBaseApplications sourceCarriers of
    application : _ -> do
      let missing = Map.delete (Reconciliation.structuralApplicationOccurrence application) originalMap
      assertBool "Progress requires the retained original archive entry" (isLeft (Progress.validateStructuralProgressOriginalOccurrences admitted missing))
      assertBool "a retained carrier requires its original archive entry" (isLeft (makeContext missing >>= \partialContext -> Reconciliation.decodeStructuralCarrierBase partialContext views freshReconciliation carrierBytes))
    [] -> pure ()
  case controls of
    [] -> pure ()
    row@(object, (causeTag, causeId, opened, at), controller) : rest -> do
      rejectCarrier "carrier rejects duplicate control rows" (domain, generation, metadata, baselines, occurrences, row : row : rest)
      rejectCarrier "carrier rejects an unknown control cause" (domain, generation, metadata, baselines, occurrences, (object, (255, causeId, opened, at), controller) : rest)
      rejectCarrier "carrier control cannot exceed admitted prefix" (domain, generation, metadata, baselines, occurrences, (object, (causeTag, causeId, opened, controlIndexWord64 committed + 1), controller) : rest)
  case controls of
    firstRow : secondRow : rest -> rejectCarrier "carrier rejects reordered control rows" (domain, generation, metadata, baselines, occurrences, secondRow : firstRow : rest)
    _ -> pure ()

assertReclaimedControlBase :: HeraldState -> Assertion
assertReclaimedControlBase state = do
  compacted <- either (assertFailure . ("paired control prefix reclamation: " <>) . show) pure (ControlBase.reclaimControlBasePrefix state)
  let projection = startupOracleProjectionState compacted
      client = startupOracleClientState compacted
      original = startupOracleProjectionState state
      prefix = Projection.oracleViewControlIndex (Projection.oracleView original)
  assertEqual "reclaimed owner products satisfy the whole Herald audit" (Right ()) (validateHeraldState compacted)
  assertEqual "Projection covers the discarded canonical prefix" prefix (Projection.projectionCanonicalCoveredThrough projection)
  assertEqual "Client covers the same canonical prefix" prefix (Client.oracleClientCanonicalCoveredThrough client)
  assertBool "semantic authority is unchanged" (Projection.oracleView original == Projection.oracleView projection)
  assertEqual "only exact local pins remain in the covered prefix" (Map.toAscList (Client.oracleClientProtectedPrefixEvidence client)) (Projection.projectionWitnessAppliedEntries (checked (Projection.validateState projection)))
  assertBool "control reclamation leaves structural progress unchanged" (startupStructuralProgressState state == startupStructuralProgressState compacted)
  assertBool "control reclamation leaves application Stores unchanged" (startupStoreState state == startupStoreState compacted)
  assertBool "control reclamation is idempotent" (checked (ControlBase.reclaimControlBasePrefix compacted) == compacted)

caseCaptureWatchLag :: Assertion
caseCaptureWatchLag = do
  let behind = initialize founderGenesis
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews behind) (startupStructuralProgressState behind))
      (committed, beginEntry) = submit (BeginHeraldAdmission manifest anchor) initialOracleState
      record = fromJust (oraclePendingHeraldAdmission committed)
      capture = Join.CaptureJoinHistory (admissionRecordId record)
      (waiting, beforeReply, beforeEffects) = request capture behind
      projected = deliverEntry beginEntry waiting
      (captured, afterReply, afterEffects) = request capture projected
  assertEqual "a committed Begin missing from the local watch is retryable" Join.JoinRetry beforeReply
  assertEqual "watch lag emits only the correlated retry" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinRetry)] (effectBatchMembers beforeEffects)
  assertBool "watch lag leaves the complete local state unchanged" (waiting == behind)
  assertBool "before admission there is no promotion handoff" (not (AlignmentCoordinator.alignmentPromotionHandoffPending behind))
  assertBool "Begin alone does not freeze promotion" (not (AlignmentCoordinator.alignmentPromotionHandoffPending projected))
  assertAdmissionTail "before Begin there is no donor obligation" record Nothing behind
  assertAdmissionTail "fresh canonical Begin registers the donor tail" record (Just (admissionRecordBeginIndex record)) projected
  assertAdmissionTail "capture preserves the original Begin obligation" record (Just (admissionRecordBeginIndex record)) captured
  assertAdmissionTail "an exact Begin retry does not alter the tail" record (Just (admissionRecordBeginIndex record)) (deliverEntry beginEntry captured)
  case afterReply of
    Join.JoinHistoryReply bytes -> do
      let history = History.sourceJoinHistory (checked (History.decodeJoinSourceHistory bytes))
      assertEqual "the original admission identity is captured" (admissionRecordId record) (History.joinHistoryAdmission history)
      assertEqual "the original attempt is captured" (admissionRecordAttempt record) (History.joinHistoryAttempt history)
      assertBool "the actual retained capture freezes further spontaneous promotion" (AlignmentCoordinator.alignmentPromotionHandoffPending captured)
      assertBool "the captured cut is locally installed" (History.joinHistoryInstalled history captured)
      assertEqual "an exact repeat retains the canonical capture" afterReply (secondOf3 (request capture captured))
      let envelope = oracleEnvelope (oracleClientRequestId founderEpoch 900003) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
          idleEntry = case checked (stepOracle envelope committed) of
            (_, OracleCommitted receipt, emitted) | oracleReceiptResult receipt /= OracleAccepted -> case oracleEffects emitted of
              [EmitAppliedOracleEntry entry] -> canonicalizeAppliedOracleEntry entry
              other -> error ("capture-freeze idle entry: " <> show other)
            _ -> error "capture-freeze expected canonical rejection"
          advanced = deliverEntry idleEntry captured
          frozen = checked (History.decodeJoinSourceHistory bytes)
      assertBool "a later canonical rejection advances the real source control owner" (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState advanced)) > History.joinHistoryControlPrefix history)
      assertEqual "same-attempt capture freezes every aggregate frame across later control progress" afterReply (secondOf3 (request capture advanced))
      assertEqual "frozen source member digest covers exactly the original envelope" (deriveHeraldJoinDigest bytes) (joinMemberStoreDigest (History.joinSourceMemberCut frozen))
    other -> assertFailure ("capture did not progress after Begin projection: " <> show other)
  assertBool "capture does not allocate generated identities" (startupIdGeneratorState behind == startupIdGeneratorState captured)
  assertEqual "capture does not submit another Oracle command" [] [effect | effect@RunOracleClientAction {} <- effectBatchMembers afterEffects]
  assertEqual "captured owner invariants" (Right ()) (validateHeraldState captured)

caseCaptureAttemptInvalidation :: Assertion
caseCaptureAttemptInvalidation = do
  let original = Fixtures.fixtureDeploymentManifest
      genesis = checked (checkHeraldGenesis original {deploymentActiveHeralds = [Fixtures.fixtureLocalMember], deploymentConfiguredProcesses = filter ((== founderEpoch) . configuredProcessResidence) (deploymentConfiguredProcesses original), deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = [Fixtures.fixtureLocalMember]}})
      selected = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      initial = fst (checked (initialHerald (monotonicInstant 1) genesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis initial) (startupInitialBootstraps initial)))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews initial) (startupStructuralProgressState initial))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record = fromJust (oraclePendingHeraldAdmission oracle1)
      begun = deliverEntry beginEntry initial
      (captured, reply, _) = request (Join.CaptureJoinHistory (admissionRecordId record)) begun
      endedProcess = case Genesis.checkedInitialBootstraps selected of
        first : _ -> appliedProcessEpochId first
        [] -> error "attempt invalidation fixture has no configured process"
      end = checked (endProcessEpochCommand endedProcess ExplicitAdministrativeEnd)
      (oracle2, endEntry) = submitCommand end oracle1
      nextRecord = fromJust (oraclePendingHeraldAdmission oracle2)
      invalidated = deliverEntry endEntry captured
  bytes <- case reply of
    Join.JoinHistoryReply encoded -> pure encoded
    other -> assertFailure ("actual source capture failed: " <> show other)
  assertEqual "captured bytes are a checked semantic bundle" (Right bytes) (History.encodeJoinSourceHistory <$> History.decodeJoinSourceHistory bytes)
  assertBool "the captured attempt freezes promotion" (AlignmentCoordinator.alignmentPromotionHandoffPending captured)
  assertEqual "semantic invalidation preserves admission identity" (admissionRecordId record) (admissionRecordId nextRecord)
  assertEqual "process End advances the attempt" (admissionRecordAttempt record + 1) (admissionRecordAttempt nextRecord)
  assertEqual "attempt invalidation preserves the exact control-tail obligation" (JoinState.admissionControlTails (startupJoinState captured)) (JoinState.admissionControlTails (startupJoinState invalidated))
  let staleGenesis = checked (checkJoiningHeraldGenesis genesis record)
      staleObserver = fst (checked (initialHerald (monotonicInstant 1) staleGenesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      nextBase = checked (ControlBase.captureControlBase invalidated)
  assertEqual
    "a control base cannot cross a different observer admission attempt"
    (Left ControlBase.ControlBaseAdmissionMismatch)
    (() <$ ControlBase.prepareControlBase nextBase staleObserver)
  assertEqual
    "the previous transfer capture is released after semantic invalidation"
    Nothing
    (JoinState.lookupCapture (admissionRecordId record) (admissionRecordAttempt record) (startupJoinState invalidated))
  assertEqual
    "the new attempt has no inherited capture"
    Nothing
    (JoinState.lookupCapture (admissionRecordId nextRecord) (admissionRecordAttempt nextRecord) (startupJoinState invalidated))
  assertBool "old capture bytes cannot freeze the new attempt" (not (AlignmentCoordinator.alignmentPromotionHandoffPending invalidated))
  let (afterRetiredInstall, retiredReply, _) = request (Join.InstallJoinHistory bytes) invalidated
  assertEqual "old transfer retries have a typed retired result" (Join.JoinTransferRetired (admissionRecordId record) (admissionRecordAttempt record)) retiredReply
  assertBool "retired transfer cannot reactivate its old semantic cut" (afterRetiredInstall == invalidated)
  assertEqual "imported semantic occurrences survive envelope release" (JoinState.importedOccurrences (startupJoinState captured)) (JoinState.importedOccurrences (startupJoinState invalidated))
  assertEqual "imported alignment provenance survives envelope release" (JoinState.importedPlanAcceptances (startupJoinState captured)) (JoinState.importedPlanAcceptances (startupJoinState invalidated))
  assertEqual "the real singleton proposer preserves structural progress" (Right ()) (Progress.validateStructuralProgressState (startupStructuralProgressState invalidated))
  assertEqual "attempt invalidation preserves owner invariants" (Right ()) (validateHeraldState invalidated)
  assertHistoricalProgressEvidence (History.joinHistoryOccurrences (History.sourceJoinHistory (checked (History.decodeJoinSourceHistory bytes)))) invalidated staleObserver

caseJoiningReplayContextAttempt :: Assertion
caseJoiningReplayContextAttempt = do
  let original = Fixtures.fixtureDeploymentManifest
      genesis = checked (checkHeraldGenesis original {deploymentActiveHeralds = [Fixtures.fixtureLocalMember], deploymentConfiguredProcesses = filter ((== founderEpoch) . configuredProcessResidence) (deploymentConfiguredProcesses original), deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = [Fixtures.fixtureLocalMember]}})
      selected = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps genesis selected))
      initializeNative sourceGenesis = fst (checked (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration oracle0) (Voter.oracleReplicaRegistrations oracle0) (monotonicInstant 1) sourceGenesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration Nothing))
      source0 = initializeNative genesis
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews source0) (startupStructuralProgressState source0))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      firstRecord = fromJust (oraclePendingHeraldAdmission oracle1)
      observer0 = initializeNative (checked (checkJoiningHeraldGenesis genesis firstRecord))
      endedProcess = case Genesis.checkedInitialBootstraps selected of
        first : _ -> appliedProcessEpochId first
        [] -> error "replay context invalidation fixture requires a configured process"
      (oracle2, endEntry) = submitCommand (checked (endProcessEpochCommand endedProcess ExplicitAdministrativeEnd)) oracle1
      currentRecord = fromJust (oraclePendingHeraldAdmission oracle2)
      source1 = deliverEntry endEntry (deliverEntry beginEntry source0)
      (live, replayReply, _) = request (Join.InstallJoinControlHistory [beginEntry, endEntry]) observer0
      oldView = Projection.oracleView (startupOracleProjectionState observer0)
  assertEqual "the observer applies actual Begin and End controls" Join.JoinHistoryInstalled replayReply
  assertEqual "the real End invalidates the admission attempt" (admissionRecordAttempt firstRecord + 1) (admissionRecordAttempt currentRecord)
  assertEqual "the live observer still carries its original startup record" (Just firstRecord) (Genesis.checkedStartupAdmission (startupGenesis live))
  assertEqual "the canonical live projection carries the new attempt" (Just currentRecord) (Projection.oracleViewHeraldAdmission (admissionRecordId currentRecord) (Projection.oracleView (startupOracleProjectionState live)))
  source <- settleControlBaseCut source1
  let base = fromJust (checked (ControlBase.captureJoiningBase source))
      context = checked (ControlBase.prepareJoiningReplayContext live)
      contextView = Projection.oracleView (startupOracleProjectionState context)
      (imported, effects) = checked (ControlBase.installPortableJoiningBases [portableDonor source] context)
  assertBool "an advanced live observer cannot be overwritten by the fresh-base seam" (isLeft (ControlBase.installJoiningBase base live))
  assertBool "the original fresh observer cannot silently cross attempts" (isLeft (ControlBase.installJoiningBase base observer0))
  assertEqual "the private context binds the canonical current attempt" (Just currentRecord) (Genesis.checkedStartupAdmission (startupGenesis context))
  assertEqual "native initial voter configuration survives private reconstruction" (Projection.oracleViewVoterConfiguration oldView) (Projection.oracleViewVoterConfiguration contextView)
  assertEqual "native initial replica registrations survive private reconstruction" (Projection.oracleViewOracleReplicas oldView) (Projection.oracleViewOracleReplicas contextView)
  assertBool "the fixture contains actual native voter evidence" (Projection.oracleViewVoterConfiguration contextView /= Nothing)
  assertEqual "the original live control cursor remains advanced" (controlIndex 2) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState live)))
  assertEqual "only the disposable context begins at zero" (controlIndex 0) (Projection.oracleViewControlIndex contextView)
  assertBool "the private context remains a restricted observer" (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState context) && not (serving context))
  assertEqual "the private context passes the whole-owner audit" (Right ()) (validateHeraldState context)
  assertEqual "the new base restores the current canonical admission" (Just currentRecord) (Projection.oracleViewHeraldAdmission (admissionRecordId currentRecord) (Projection.oracleView (startupOracleProjectionState imported)))
  assertBool "the new base restores actual End semantics" (not (Projection.oracleViewProcessIsLive endedProcess (Projection.oracleView (startupOracleProjectionState imported))))
  assertBool "the new source History is complete in the private candidate" (History.joinHistoryInstalled (History.sourceJoinHistory (captureSource source)) imported)
  assertEqual "preparing the candidate emits no application or network work" [] (effectBatchMembers effects)
  assertEqual "the imported candidate satisfies all owner invariants" (Right ()) (validateHeraldState imported)
  assertEqual "candidate construction leaves the original stale startup record intact" (Just firstRecord) (Genesis.checkedStartupAdmission (startupGenesis live))
  let replacement = checked (ControlBase.replacePortableJoiningBases [portableDonor source] live)
  assertJoiningReplacementPreserves live replacement
  assertEqual "replacement preserves the original stale startup record" (Just firstRecord) (Genesis.checkedStartupAdmission (startupGenesis replacement))
  assertEqual "replacement keeps the actual canonical invalidated attempt" (Just currentRecord) (Projection.oracleViewHeraldAdmission (admissionRecordId currentRecord) (Projection.oracleView (startupOracleProjectionState replacement)))
  assertBool "replacement installs the complete new-attempt source history" (History.joinHistoryInstalled (History.sourceJoinHistory (captureSource source)) replacement)
  assertEqual "replacement after real End remains a coherent owner product" (Right ()) (validateHeraldState replacement)
  let compactSource = checked (ControlBase.reclaimControlBasePrefix source)
      compactHistory = History.sourceJoinHistory (captureSource compactSource)
      (firstImported, firstReply, firstEffects) = request (Join.InstallJoinHistory (sourceBytes compactSource)) observer0
  assertEqual "the first import starts before canonical Begin was observed" (controlIndex 0) (Projection.oracleViewControlIndex oldView)
  assertEqual "the real new-attempt donor has no exact old controls left" [] (History.joinHistoryControlEntries compactHistory)
  assertEqual "live first import adopts the newer checked attempt" Join.JoinHistoryInstalled firstReply
  assertEqual "new-attempt first import preserves original startup provenance" (Just firstRecord) (Genesis.checkedStartupAdmission (startupGenesis firstImported))
  assertEqual "new-attempt first import installs the actual current record" (Just currentRecord) (Projection.oracleViewHeraldAdmission (admissionRecordId currentRecord) (Projection.oracleView (startupOracleProjectionState firstImported)))
  assertBool "the compact first import restores the actual End" (not (Projection.oracleViewProcessIsLive endedProcess (Projection.oracleView (startupOracleProjectionState firstImported))))
  assertBool "the compact first import covers all new-attempt source facts" (History.joinHistoryInstalled compactHistory firstImported)
  assertEqual "new-attempt first import emits no old-source work" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinHistoryInstalled)] (effectBatchMembers firstEffects)
  assertEqual "new-attempt first import preserves all owner invariants" (Right ()) (validateHeraldState firstImported)

caseJoiningReplayContextSeal :: Assertion
caseJoiningReplayContextSeal = do
  let (oracle0, record, _, founder, observer0) = capturedFixture
      admission = admissionRecordId record
      seal = checked (Join.prepareJoinSeal record [captureSource founder])
      (oracle1, sealEntry) = submit (SealHeraldAdmission seal) oracle0
      sealedRecord = fromJust (oraclePendingHeraldAdmission oracle1)
      imported = fst (checked (ControlBase.installPortableJoiningBases [portableDonor founder] observer0))
      (live, sealReply, _) = request (Join.InstallJoinControlHistory [sealEntry]) imported
      context = checked (ControlBase.prepareJoiningReplayContext live)
      contextRecord = fromJust (Genesis.checkedStartupAdmission (startupGenesis context))
      candidate = fst (checked (ControlBase.installPortableJoiningBases [portableDonor founder] context))
      (caughtUp, caughtUpReply, _) = request (Join.InstallJoinControlHistory [sealEntry]) candidate
      (_, cancellation) = submit (CancelHeraldAdmission admission) oracle1
      (cancelled, cancellationReply, _) = request (Join.InstallJoinControlHistory [cancellation]) live
  assertEqual "the current observer receives the real canonical seal" Join.JoinHistoryInstalled sealReply
  assertEqual "the original startup record predates sealing" Nothing (admissionRecordSeal (fromJust (Genesis.checkedStartupAdmission (startupGenesis live))))
  assertEqual "private reconstruction uses the complete current admission record" sealedRecord contextRecord
  assertEqual "private reconstruction retains the exact current seal" (Just seal) (admissionRecordSeal contextRecord)
  assertEqual "the original preseal capture imports under the current sealed startup record" Join.JoinHistoryInstalled caughtUpReply
  assertEqual "ordinary suffix catch-up restores the exact sealed projection" (Projection.stateWitness (startupOracleProjectionState live)) (Projection.stateWitness (startupOracleProjectionState caughtUp))
  assertEqual "the original live cursor remains sealed while the context is disposable" (controlIndex 2) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState live)))
  assertBool "serving members cannot manufacture a joining replay context" (isLeft (ControlBase.prepareJoiningReplayContext founder))
  assertBool "a frozen observer cannot manufacture a joining replay context" (isLeft (ControlBase.prepareJoiningReplayContext (freezeStartupSemanticControl live)))
  assertEqual "the pending observer receives the actual cancellation" Join.JoinHistoryInstalled cancellationReply
  assertBool "a cancelled applicant cannot manufacture a joining replay context" (isLeft (ControlBase.prepareJoiningReplayContext cancelled))
  assertEqual "rejection leaves the cancelled owner coherent" (Right ()) (validateHeraldState cancelled)

propJoiningReplayContextSuffix :: Word8 -> Property
propJoiningReplayContextSuffix sample =
  let (_, record, source, _, observer, _, _) = coveredHistorySuffixFixture (fromIntegral sample `mod` 6)
      donor = portableDonor source
      history = History.sourceJoinHistory (captureSource source)
      live = fst (checked (ControlBase.installPortableJoiningBases [donor] observer))
      context = checked (ControlBase.prepareJoiningReplayContext live)
      (candidate, effects) = checked (ControlBase.installPortableJoiningBases [donor] context)
   in conjoin
        [ Genesis.checkedStartupAdmission (startupGenesis context) === Just record,
          Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState context)) === controlIndex 0,
          Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState live)) === History.joinHistoryControlPrefix history,
          (Projection.oracleView (startupOracleProjectionState candidate) == Projection.oracleView (startupOracleProjectionState live)) === True,
          Graph.graphStructuralVertexProjections (startupGraphState candidate) === Graph.graphStructuralVertexProjections (startupGraphState live),
          Graph.graphStructuralEdgeProjections (startupGraphState candidate) === Graph.graphStructuralEdgeProjections (startupGraphState live),
          History.joinHistoryInstalled history candidate === True,
          serving context === False,
          serving candidate === False,
          validateHeraldState context === Right (),
          validateHeraldState candidate === Right (),
          effectBatchMembers effects === []
        ]

-- The canonical owners may advance with a source ahead of the observer. Their
-- runtime bindings, contacts and request bookkeeping remain live authorities;
-- replacing semantic staging must not reset them to fresh-context defaults.
assertJoiningReplacementPreserves :: HeraldState -> HeraldState -> Assertion
assertJoiningReplacementPreserves before after = do
  let beforeClient = startupOracleClientState before
      afterClient = startupOracleClientState after
  forM_
    [ ("static genesis", startupGenesis before == startupGenesis after),
      ("initial bootstraps", startupInitialBootstraps before == startupInitialBootstraps after),
      ("takeover timing", startupTakeoverTarget before == startupTakeoverTarget after),
      ("recovery configuration", startupApplicationRecoveryConfiguration before == startupApplicationRecoveryConfiguration after),
      ("clock", startupLastObservedTime before == startupLastObservedTime after),
      ("identity generator", startupIdGeneratorState before == startupIdGeneratorState after),
      ("Application", startupApplicationState before == startupApplicationState after),
      ("Administration", startupAdministrationState before == startupAdministrationState after),
      ("ConfiguredProcess", startupConfiguredProcessState before == startupConfiguredProcessState after),
      ("ProcessPreparation", startupProcessPreparationState before == startupProcessPreparationState after),
      ("Oracle contacts", Client.oracleClientContacts beforeClient == Client.oracleClientContacts afterClient),
      ("Oracle binding", Client.oracleClientCurrentBinding beforeClient == Client.oracleClientCurrentBinding afterClient),
      ("Oracle local replica", Client.oracleClientLocalReplica beforeClient == Client.oracleClientLocalReplica afterClient),
      ("Oracle request sequence", Client.oracleClientNextRequestSequence beforeClient == Client.oracleClientNextRequestSequence afterClient),
      ("Oracle requests", Client.oracleClientRequestEntries beforeClient == Client.oracleClientRequestEntries afterClient),
      ("Oracle receipt retirement", Client.oracleClientReceiptRetirementProgressReady beforeClient == Client.oracleClientReceiptRetirementProgressReady afterClient),
      ("Oracle confirmed retirement", Client.oracleClientReceiptRetirementProgressConfirmed beforeClient == Client.oracleClientReceiptRetirementProgressConfirmed afterClient),
      ("Discovery peer bindings", Discovery.currentPeerBindings (startupDiscoveryState before) == Discovery.currentPeerBindings (startupDiscoveryState after)),
      ("Discovery contacts", Discovery.knownHeralds (startupDiscoveryState before) == Discovery.knownHeralds (startupDiscoveryState after)),
      ("Isolation", startupIsolationState before == startupIsolationState after),
      ("FailureDetection", startupFailureDetectionState before == startupFailureDetectionState after),
      ("PeerLiveness", startupPeerLivenessState before == startupPeerLivenessState after),
      ("PeerStream", startupPeerStreamState before == startupPeerStreamState after),
      ("PeerDelivery", startupPeerDeliveryState before == startupPeerDeliveryState after)
    ]
    $ \(owner, equal) -> assertBool (owner <> " survives atomic joining replacement exactly") equal
  assertEqual "Join command requests survive replacement" (JoinState.requestEntries (startupJoinState before)) (JoinState.requestEntries (startupJoinState after))
  assertEqual "locally frozen captures survive replacement" (JoinState.capturedHistories (startupJoinState before)) (JoinState.capturedHistories (startupJoinState after))
  forM_ [founderEpoch, applicantEpoch] $ \owner ->
    assertEqual "Join receipt retirement survives replacement" (JoinState.requestReceiptRetirement owner (startupJoinState before)) (JoinState.requestReceiptRetirement owner (startupJoinState after))

propJoiningReplacementSuffix :: Word8 -> Property
propJoiningReplacementSuffix sample =
  let suffixLength = fromIntegral sample `mod` 6
      (_, record, compactSource, fullSource, observer, _, _) = coveredHistorySuffixFixture suffixLength
      (_, _, earlierSource, _, _, _, _) = coveredHistorySuffixFixture (suffixLength `div` 2)
      donor = portableDonor compactSource
      history = History.sourceJoinHistory (captureSource compactSource)
      live = fst (checked (ControlBase.installPortableJoiningBases [portableDonor fullSource] observer))
      replaced = checked (ControlBase.replacePortableJoiningBases [donor] live)
      repeated = checked (ControlBase.replacePortableJoiningBases [donor] replaced)
      caughtUp = checked (ControlBase.replacePortableJoiningBases [portableDonor earlierSource] live)
   in conjoin
        [ (Projection.oracleView (startupOracleProjectionState replaced) == Projection.oracleView (startupOracleProjectionState live)) === True,
          Projection.projectionLastSemanticControlIndex (startupOracleProjectionState replaced) === Projection.projectionLastSemanticControlIndex (startupOracleProjectionState live),
          Client.oracleClientAppliedCursor (startupOracleClientState replaced) === Client.oracleClientAppliedCursor (startupOracleClientState live),
          Client.oracleClientRequestEntries (startupOracleClientState replaced) === Client.oracleClientRequestEntries (startupOracleClientState live),
          (startupGenesis replaced == startupGenesis live) === True,
          (startupAdministrationState replaced == startupAdministrationState live) === True,
          (startupDiscoveryState replaced == startupDiscoveryState live) === True,
          (startupIdGeneratorState replaced == startupIdGeneratorState live) === True,
          Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState replaced)) === History.joinHistoryControlPrefix history,
          Graph.graphStructuralVertexProjections (startupGraphState replaced) === Graph.graphStructuralVertexProjections (startupGraphState live),
          Graph.graphStructuralEdgeProjections (startupGraphState replaced) === Graph.graphStructuralEdgeProjections (startupGraphState live),
          History.joinHistoryInstalled history replaced === True,
          admissionTailFloor record replaced === Just (History.joinHistoryControlPrefix history),
          (repeated == replaced) === True,
          (Projection.oracleView (startupOracleProjectionState caughtUp) == Projection.oracleView (startupOracleProjectionState live)) === True,
          Client.oracleClientAppliedCursor (startupOracleClientState caughtUp) === History.joinHistoryControlPrefix history,
          History.joinHistoryInstalled (History.sourceJoinHistory (captureSource earlierSource)) caughtUp === True,
          admissionTailFloor record caughtUp === Just (History.joinHistoryControlPrefix (History.sourceJoinHistory (captureSource earlierSource))),
          serving caughtUp === False,
          validateHeraldState caughtUp === Right (),
          serving replaced === False,
          validateHeraldState replaced === Right ()
        ]

-- Every donor is an actual immutable pre-seal capture. Rejected commands move
-- its canonical prefix without changing the admission attempt, allowing the
-- receiver to lag by several entries while the complete candidate stays valid.
propJoiningReplacementAhead :: Word8 -> Property
propJoiningReplacementAhead sample =
  let (_, _, source, _, observer, beginEntry, _) = coveredHistorySuffixFixture (1 + fromIntegral sample `mod` 6)
      donor = portableDonor source
      history = History.sourceJoinHistory (captureSource source)
      live = firstOf3 (request (Join.InstallJoinControlHistory [beginEntry]) observer)
      expected = fst (checked (ControlBase.installPortableJoiningBases [donor] observer))
      replaced = checked (ControlBase.replacePortableJoiningBases [donor] live)
      repeated = checked (ControlBase.replacePortableJoiningBases [donor] replaced)
      projection = startupOracleProjectionState replaced
      expectedProjection = startupOracleProjectionState expected
   in conjoin
        [ (History.joinHistoryControlPrefix history > Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState live))) === True,
          (Projection.oracleView projection == Projection.oracleView expectedProjection) === True,
          Projection.projectionLastSemanticControlIndex projection === Projection.projectionLastSemanticControlIndex expectedProjection,
          Client.oracleClientAppliedCursor (startupOracleClientState replaced) === History.joinHistoryControlPrefix history,
          Projection.projectionCanonicalCoveredThrough projection === History.joinHistoryControlAnchor history,
          History.joinHistoryInstalled history replaced === True,
          (startupGenesis replaced == startupGenesis live) === True,
          (startupAdministrationState replaced == startupAdministrationState live) === True,
          Client.oracleClientRequestEntries (startupOracleClientState replaced) === Client.oracleClientRequestEntries (startupOracleClientState live),
          (repeated == replaced) === True,
          serving replaced === False,
          validateHeraldState replaced === Right ()
        ]

caseJoiningReplacementAheadReceipts :: Assertion
caseJoiningReplacementAheadReceipts = do
  let (_, _, source, _, observer, beginEntry, _) = coveredHistorySuffixFixture 3
      donor = portableDonor source
      begun = firstOf3 (request (Join.InstallJoinControlHistory [beginEntry]) observer)
      stepAt at body state = checked (stepHerald (heraldInput (monotonicInstant at) body) state)
      (opened, _) = stepAt 2 (AdministrationInput (OpenAdministrationConnection (candidateAdministrationLane 920) (Genesis.checkedSystemId (startupGenesis begun)))) begun
      binding = fromJust (Administration.currentConfiguredAdministrationBinding (startupAdministrationState opened))
      correlation = Admin.adminCorrelationIdForBinding binding 22
      (queried, queryEffects) = stepAt 3 (AdministrationInput (GetHeraldStatus binding correlation)) opened
      adminRetirement = receiptRetirementPrefix (Just 22)
      (adminRetired, _) = stepAt 3 (AdministrationInput (RetireAdministrationReceipts binding adminRetirement Nothing)) queried
      joinRetirement = receiptRetirementPrefix (Just 7)
      (live, retiredReply, _) = request (Join.RetireJoinReceipts applicantEpoch joinRetirement) adminRetired
      replaced = checked (ControlBase.replacePortableJoiningBases [donor] live)
      expected = fst (checked (ControlBase.installPortableJoiningBases [donor] observer))
      (_, retiredAdminEffects) = stepAt 3 (AdministrationInput (GetAdministrationResult binding correlation)) replaced
      (_, retiredJoinReply, _) = request (Join.ReadJoinCommand (Join.joinRequestId applicantEpoch 7)) replaced
  assertBool "the real administration connection served a joining status query" (case effectBatchMembers queryEffects of [SendAdministrationReply actual (Admin.AdminHeraldStatusReply _ _)] -> actual == binding; _ -> False)
  assertEqual "the live observer has not independently received the donor suffix" (controlIndex 1) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState live)))
  assertEqual "the donor actually advances three canonical entries" (controlIndex 4) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState replaced)))
  assertBool "canonical Projection comes from the complete checked donor" (Projection.oracleView (startupOracleProjectionState expected) == Projection.oracleView (startupOracleProjectionState replaced))
  assertEqual "Client advances with canonical Projection" (controlIndex 4) (Client.oracleClientAppliedCursor (startupOracleClientState replaced))
  assertJoiningReplacementPreserves live replaced
  assertEqual "Join progress was admitted before replacement" (Join.JoinReceiptsRetired applicantEpoch joinRetirement) retiredReply
  assertEqual "replacement does not reopen a retired administration correlation" [SendAdministrationReply binding (Admin.RetiredAdminResult correlation adminRetirement)] (effectBatchMembers retiredAdminEffects)
  assertEqual "replacement does not reopen a retired Join correlation" (Join.JoinRequestRetired (Join.joinRequestId applicantEpoch 7) joinRetirement) retiredJoinReply
  assertBool "receiving a donor ahead leaves the application restricted" (not (serving replaced))
  assertEqual "the coherent replacement satisfies the complete owner invariant" (Right ()) (validateHeraldState replaced)

caseJoiningReplacementActivation :: Assertion
caseJoiningReplacementActivation = do
  let (oracle0, record, _, founder0, observer0) = capturedFixture
      donors = [portableDonor founder0]
      observer1 = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder0)) observer0)
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      (oracle1, founder1, sealed) = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      stepAt at body state = checked (stepHerald (heraldInput (monotonicInstant at) body) state)
      (opened, _) = stepAt 2 (AdministrationInput (OpenAdministrationConnection (candidateAdministrationLane 919) (Genesis.checkedSystemId (startupGenesis sealed)))) sealed
      binding = fromJust (Administration.currentConfiguredAdministrationBinding (startupAdministrationState opened))
      correlation = Admin.adminCorrelationIdForBinding binding 23
      start = Admin.startProcessRequest correlation
      (rejected, rejectionEffects) = stepAt 3 (AdministrationInput (StartProcessEpoch binding start)) opened
      (inspected, statusEffects) = stepAt 3 (AdministrationInput (GetHeraldStatus binding (Admin.adminCorrelationIdForBinding binding 22))) rejected
      adminRetirement = receiptRetirementPrefix (Just 22)
      (adminRetired, adminRetirementEffects) = stepAt 3 (AdministrationInput (RetireAdministrationReceipts binding adminRetirement Nothing)) inspected
      retirement = receiptRetirementPrefix (Just 7)
      (live, retirementReply, _) = request (Join.RetireJoinReceipts applicantEpoch retirement) adminRetired
      replaced = checked (ControlBase.replacePortableJoiningBases donors live)
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("replacement readiness failed: " <> show reply)
      afterAccept@(_, acceptedFounder, acceptedObserver) = advance (AcceptHeraldJoinSeal (admissionRecordId record) (admissionRecordAttempt record) (joinSealDigest seal)) (oracle1, founder1, replaced)
      afterOld = advance (HeraldJoinBaseReady (ready acceptedFounder)) afterAccept
      afterNew = advance (HeraldJoinReady (ready acceptedObserver)) afterOld
      (_, _, active) = advance (ActivateHerald (admissionRecordId record)) afterNew
      (started, startEffects) = stepAt 4 (AdministrationInput (StartProcessEpoch binding start)) active
      (retried, retryEffects) = stepAt 4 (AdministrationInput (StartProcessEpoch binding start)) started
  assertEqual "joining observer rejects the real administrative Start" [RejectAdministrationConnection (EstablishedAdministrationDisposition binding)] (effectBatchMembers rejectionEffects)
  assertEqual "rejected Start reserves no gated request before activation" Nothing (Administration.lookupConfiguredStartStatus correlation (startupAdministrationState live))
  assertBool "the rejected Start consumes no generated identity" (startupIdGeneratorState opened == startupIdGeneratorState live)
  assertEqual "the rejected Start submits no Oracle request" [] (Client.oracleClientRequestEntries (startupOracleClientState live))
  assertBool "the joining observer serves an actual read-only administration query" (case effectBatchMembers statusEffects of [SendAdministrationReply actual (Admin.AdminHeraldStatusReply _ _)] -> actual == binding; _ -> False)
  assertEqual "administration acknowledges actual receipt progress" [SendAdministrationReply binding (Admin.AdministrationReceiptsRetired adminRetirement)] (effectBatchMembers adminRetirementEffects)
  assertEqual "the fixture has actual retired Join correlations" (Join.JoinReceiptsRetired applicantEpoch retirement) retirementReply
  assertJoiningReplacementPreserves live replaced
  assertEqual "replacement catches up the retained Seal before exposing its candidate" (ready live) (ready replaced)
  assertEqual "replacement retains the exact live control cursor" (controlIndex 2) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState replaced)))
  assertBool "replacement keeps the application gate closed" (not (serving replaced))
  assertEqual "the replacement remains coherent with live administrative work" (Right ()) (validateHeraldState replaced)
  assertBool "ordinary canonical suffix activates the replacement" (serving active)
  assertBool "the same Start is accepted after activation" (case Administration.lookupConfiguredStartStatus correlation (startupAdministrationState started) of Just Administration.ConfiguredStartAwaitingOracle {} -> True; _ -> False)
  assertEqual "post-activation Start allocates exactly one Oracle request" 1 (length [() | (_, witness) <- Client.oracleClientRequestEntries (startupOracleClientState started), Just _ <- [Client.oracleRequestWitnessBootstrap witness]])
  assertBool "retrying the accepted Start preserves every owner" (retried == started)
  assertEqual "retrying Start repeats the retained reply and submission" (effectBatchMembers startEffects) (effectBatchMembers retryEffects)
  assertEqual "the live administration binding survives activation" (Just binding) (Administration.currentConfiguredAdministrationBinding (startupAdministrationState started))
  assertEqual "activated replacement with pending Start preserves all owner invariants" (Right ()) (validateHeraldState started)

caseJoiningReplacementBoundaries :: Assertion
caseJoiningReplacementBoundaries = do
  let (oracle0, record, _, founder, observer0) = capturedFixture
      donors = [portableDonor founder]
      live = fst (checked (ControlBase.installPortableJoiningBases donors observer0))
      seal = checked (Join.prepareJoinSeal record [captureSource founder])
      (oracle1, sealEntry) = submit (SealHeraldAdmission seal) oracle0
      sealed = firstOf3 (request (Join.InstallJoinControlHistory [sealEntry]) live)
      compacted = reclaimWithoutAdmissionTail record sealed
      (_, cancelEntry) = submit (CancelHeraldAdmission (admissionRecordId record)) oracle1
      cancelled = firstOf3 (request (Join.InstallJoinControlHistory [cancelEntry]) sealed)
  assertEqual "the required live Seal tail was actually reclaimed" Nothing (Projection.appliedEntryEvidence (controlIndex 2) (startupOracleProjectionState compacted))
  assertBool "replacement cannot reconstruct a missing exact live control tail from semantic facts" (isLeft (ControlBase.replacePortableJoiningBases donors compacted))
  assertBool "a cancelled live admission cannot be revived by its old source capture" (isLeft (ControlBase.replacePortableJoiningBases donors cancelled))
  assertBool "a serving old member cannot replace its owner state through joining" (isLeft (ControlBase.replacePortableJoiningBases donors founder))
  assertBool "a frozen joining owner cannot replace its staged state" (isLeft (ControlBase.replacePortableJoiningBases donors (freezeStartupSemanticControl live)))
  forM_ [compacted, cancelled] $ \state -> assertEqual "rejected replacement inputs retain coherent owners" (Right ()) (validateHeraldState state)

{-# NOINLINE freshJoiningReplacement #-}
freshJoiningReplacement :: Word64 -> IO (HeraldState, [(String, IO Bool)])
freshJoiningReplacement observedAt = do
  let original = Fixtures.fixtureDeploymentManifest
      local = Fixtures.fixtureLocalMember
      deployment =
        original
          { deploymentActiveHeralds = [local],
            deploymentConfiguredProcesses = filter ((== heraldMemberEpoch local) . configuredProcessResidence) (deploymentConfiguredProcesses original),
            deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = [local]}
          }
      genesis = checked (checkHeraldGenesis deployment)
      selected = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      initial = fst (checked (initialHerald (monotonicInstant observedAt) genesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH1 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      bootstrap = case Fixtures.fixtureLocalBootstrapIds of first : _ -> first; [] -> error "missing retention fixture bootstrap"
      attachment = fromJust (primordialApplicationAttachment genesis selected bootstrap)
  (bound, _, _) <- Configured.bindInitial genesis initial
  (opened, sessionEffects) <- Configured.step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane observedAt) attachment (Session.clientNonce observedAt))) bound
  (binding, session) <- case [(Session.sessionAcceptanceBinding acceptance, identifier) | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers sessionEffects, Session.SessionOpened identifier _ _ <- [Session.sessionAcceptanceReply acceptance]] of
    [one] -> pure one
    _ -> assertFailure "retention fixture failed to open its real application session"
  (pending, _) <- Configured.step (ApplicationRequestInput (CallApplicationRequest binding session (requestId observedAt) NewEnvironmentApplication)) opened
  let structural = fst (checked (StructuralCoordinator.advanceLocalStructuralWork pending))
      aligned = fst (checked (AlignmentTransfer.advanceAlignmentTransfers structural))
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps genesis selected))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews aligned) (startupStructuralProgressState aligned))
      (oracle1, entry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record = fromJust (oraclePendingHeraldAdmission oracle1)
      observer = fst (checked (initialHerald (monotonicInstant observedAt) (checked (checkJoiningHeraldGenesis genesis record)) selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  source <- settleControlBaseCut (deliverEntry entry aligned)
  let history = History.sourceJoinHistory (captureSource source)
      donors = [portableDonor source]
  old <- evaluate (fst (checked (ControlBase.installPortableJoiningBases donors observer)))
  assertBool "retention fixture has actual imported structural history" (not (null (History.joinHistoryOccurrences history)))
  assertBool "retention fixture has actual passive alignment plans" (not (null (Alignment.alignmentPlanEntries (startupAlignmentState old))))
  witnesses <-
    sequence
      [ weakOwner "Herald" old,
        weakOwner "Projection" (startupOracleProjectionState old),
        weakOwner "OracleClient" (startupOracleClientState old),
        weakOwner "Discovery" (startupDiscoveryState old),
        weakOwner "Store" (startupStoreState old),
        weakOwner "Progress" (startupStructuralProgressState old),
        weakOwner "Reconciliation" (Progress.structuralProgressReconciliation (startupStructuralProgressState old)),
        weakOwner "Alignment" (startupAlignmentState old)
      ]
  successor <- evaluate (checked (ControlBase.replacePortableJoiningBases donors old))
  assertEqual "the surviving replacement is fully admitted before testing retention" (Right ()) (validateHeraldState successor)
  pure (successor, witnesses)
  where
    weakOwner name value = do
      owner <- evaluate value
      witness <- mkWeakPtr owner Nothing
      pure (name, maybe False (const True) <$> deRefWeak witness)

caseJoiningReplacementRetention :: Assertion
caseJoiningReplacementRetention = do
  observedAt <- (2000 +) . fromIntegral . hashUnique <$> newUnique
  (successor, witnesses) <- freshJoiningReplacement observedAt
  bracket (newStablePtr successor) freeStablePtr $ \root -> do
    performGC
    aliveOwners <- forM witnesses $ \(name, alive) -> (name,) <$> alive
    assertEqual "no old staging owner is retained by replacement" [(name, False) | (name, _) <- witnesses] aliveOwners
    retained <- deRefStablePtr root
    assertEqual "the replacement remains valid after old staging is reclaimed" (Right ()) (validateHeraldState retained)
    assertBool "reclamation does not admit the joining application" (not (serving retained))

caseCaptureDuringLabel :: Assertion
caseCaptureDuringLabel = do
  (pendingSeed, _, _, labelRequest, _) <- LabelFixture.settledNeutralControlledFixtureWithApplicationRequests
  seed <- LabelFixture.completeAssignedPeerPublications pendingSeed
  let oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis seed) (startupInitialBootstraps seed)))
      requested = fst (checked (stepHerald (heraldInput (monotonicInstant 100) (ApplicationRequestInput labelRequest)) seed))
  (oracle1, opened, _) <- LabelFixture.commitNextLabelRequest 101 oracle0 requested
  let anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews opened) (startupStructuralProgressState opened))
      (oracle2, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle1
      record = fromJust (oraclePendingHeraldAdmission oracle2)
      -- The accepted label has not closed yet, so no join seal cut has been
      -- established. Capture must remain retryable at this actual boundary.
      begun = deliverEntry beginEntry opened
      (waiting, reply, _) = request (Join.CaptureJoinHistory (admissionRecordId record)) begun
  assertBool "the Oracle has a real accepted unfinished label" (not (null (oracleOpenDecisions oracle2)))
  assertEqual "the pure capture waits for label completion" (Right Nothing) (History.captureJoinHistory record begun)
  assertEqual "disabling diagnostics preserves the unfinished-label capture guard" (Right Nothing) (History.captureJoinHistoryWithDiagnostics DiagnosticChecksDisabled record begun)
  assertEqual "disabling diagnostics preserves the live capture retry" Join.JoinRetry (secondOf3 (request (Join.CaptureJoinHistory (admissionRecordId record)) (configureHeraldDiagnosticChecks DiagnosticChecksDisabled begun)))
  assertEqual "the operator receives a retry while the label is open" Join.JoinRetry reply
  assertEqual "no capture is retained while the label is open" Nothing (JoinState.lookupCapture (admissionRecordId record) (admissionRecordAttempt record) (startupJoinState waiting))
  assertBool "the unfinished label is not frozen by a capture barrier" (not (AlignmentCoordinator.alignmentPromotionHandoffPending waiting))

caseLabelDecisionInvalidatesJoin :: Assertion
caseLabelDecisionInvalidatesJoin = do
  (pendingSeed, _, _, labelRequest, _) <- LabelFixture.settledNeutralControlledFixtureWithApplicationRequests
  seed <- LabelFixture.completeAssignedPeerPublications pendingSeed
  let oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis seed) (startupInitialBootstraps seed)))
      requested = fst (checked (stepHerald (heraldInput (monotonicInstant 100) (ApplicationRequestInput labelRequest)) seed))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews requested) (startupStructuralProgressState requested))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      previous = fromJust (oraclePendingHeraldAdmission oracle1)
      begun = deliverEntry beginEntry requested
  (oracle2, decided, _) <- LabelFixture.commitNextLabelRequest 101 oracle1 begun
  successor <- maybe (assertFailure "label decision lost the pending join") pure (oraclePendingHeraldAdmission oracle2)
  entry <- maybe (assertFailure "label decision has no projected canonical entry") pure (Projection.appliedEntryEvidence (oracleGreatestControlIndex oracle2) (startupOracleProjectionState decided))
  case map OracleEvents.oracleProjectionEventView (OracleEvents.appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue entry)) of
    [OracleEvents.HeraldAdmissionChangedView changed, OracleEvents.LabelDecidedView _ terminal _] -> do
      assertEqual "the admission prefix is the new canonical attempt" successor changed
      assertBool "the same entry contains a successful label decision" (case liveTerminalOutcomeView terminal of LiveReleasedOutcomeView {} -> True; _ -> False)
    events -> assertFailure ("expected join invalidation followed by the atomic label decision: " <> show events)
  assertEqual "the join identity is preserved" (admissionRecordId previous) (admissionRecordId successor)
  assertEqual "the label decision invalidates exactly one join attempt" (admissionRecordAttempt previous + 1) (admissionRecordAttempt successor)
  assertEqual "Herald projection accepts the combined canonical transition" (Right ()) (validateHeraldState decided)

-- Single-donor semantic regressions admit the covered base before replaying
-- its semantic History. Live applicant RPCs require the complete source set;
-- this private seam isolates one source's original material and provenance.
replaySingleSourceHistory :: History.JoinSourceHistory -> HeraldState -> (HeraldState, Bool)
replaySingleSourceHistory source initial =
  let history = History.sourceJoinHistory source
      based =
        if Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState initial)) < History.joinHistoryControlAnchor history
          then fst (checked (ControlBase.installPortableJoiningBase (History.joinHistorySource history) (History.encodeJoinSourceHistory source) initial))
          else initial
      (replayed, _, complete) = checked (JoinReplay.replayJoinHistory history based)
      provenance = checked (JoinState.retainImportedProvenance (AlignmentHistory.alignmentHistoryPlanAcceptances (History.joinHistoryAlignmentHistory history)) (History.joinHistoryOccurrences history) (startupJoinState replayed))
      retained = JoinState.retainInstalledHistory (History.joinHistoryAdmission history) (History.joinHistoryAttempt history) (History.joinHistorySource history) (History.encodeJoinSourceHistory source) provenance
   in (replaceStartupJoinState retained replayed, complete)

caseStructuralHistory :: Assertion
caseStructuralHistory = do
  (begun, observer, history) <- structuralHistoryFixture
  let
    (imported, complete) = replaySingleSourceHistory (captureSource begun) observer
    occurrences = History.joinHistoryOccurrences history
  case JoinReplay.replayJoinHistory history imported of
    Left problem -> assertFailure ("based structural history replay: " <> show problem)
    Right _ -> pure ()
  assertBool "newenv produces original structural source history" (not (null occurrences))
  assertEqual "exact source control and data import completes" True complete
  mapM_ (\occurrence -> assertEqual "original stamp retained" (Just (Terminal.terminalStructuralOccurrenceStamp occurrence)) (Progress.appliedStructuralOccurrenceStamp <$> Progress.lookupAppliedStructuralOccurrence (Terminal.terminalStructuralOccurrenceId occurrence) (startupStructuralProgressState imported))) occurrences
  assertEqual "End reconstructs identical surviving graph vertices" (Graph.graphVertices (startupGraphState begun)) (Graph.graphVertices (startupGraphState imported))
  assertEqual "End reconstructs identical surviving graph edges" (Graph.graphEdges (startupGraphState begun)) (Graph.graphEdges (startupGraphState imported))
  assertEqual "history leaves applicant restricted" False (serving imported)
  assertEqual "replayed full Herald invariant" (Right ()) (validateHeraldState imported)
  assertPreparedControlBase begun observer imported

structuralHistoryFixture :: IO (HeraldState, HeraldState, History.JoinHistory)
structuralHistoryFixture = do
  let fixture = EnvironmentFixture.fixture
      initial = EnvironmentFixture.fixtureState fixture
      application = CallApplicationRequest (EnvironmentFixture.fixtureBinding fixture) (EnvironmentFixture.fixtureSession fixture) (requestId 900) NewEnvironmentApplication
      accepted = fst (checked (ApplicationCall.applyApplicationRequest application initial))
      drained = fst (checked (StructuralCoordinator.advanceLocalStructuralWork accepted))
  beforeEnd <- settleControlBaseCut drained
  let
    oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis initial) (startupInitialBootstraps initial)))
    end = checked (endProcessEpochCommand (EnvironmentFixture.fixtureProcess fixture) ExplicitAdministrativeEnd)
    (oracle1, endEntry) = submitCommand end oracle0
  ended <- settleControlBaseCut (deliverEntry endEntry beforeEnd)
  let
    anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews ended) (startupStructuralProgressState ended))
    (oracle2, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle1
    record = fromJust (oraclePendingHeraldAdmission oracle2)
  begun <- settleControlBaseCut (deliverEntry beginEntry ended)
  let
    history = fromJust (checked (History.captureJoinHistory record begun))
    joiningGenesis = checked (checkJoiningHeraldGenesis (startupGenesis initial) record)
    observer = fst (checked (initialHerald (monotonicInstant 1) joiningGenesis (startupInitialBootstraps initial) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  pure (begun, observer, history)

-- The fixture supplies exact reports from every old owner for the same fully
-- applied finite source history. All semantic material itself originates from
-- the real application path above; no owner snapshots are injected.
establishCut :: Maybe HeraldAdmissionId -> HeraldState -> HeraldState
establishCut admission state =
  let progress = startupStructuralProgressState state
      view = structuralReconciliationViews state
      vector = Progress.structuralAppliedVector progress
      control = Progress.structuralAppliedControlPrefix progress
      digest = checked (checked (Progress.structuralTopologyOccurrenceDigestAt view vector control progress))
      cut = checked (topologyCut (sameGenerationPredecessor (Progress.structuralLastInstalledCutId progress)) (topologyFrontier vector control) digest)
      cutId = deriveTopologyCutId cut
      certificate = checked (GraphProtocol.topologyCutEstablished cutId cut [GraphProtocol.topologyCutAcceptance cutId (GraphProtocol.structuralAppliedReport member vector control) | member <- NE.toList (Progress.structuralProgressMembers progress)])
      (affects, bytes) = Reconciliation.structuralControlCheckpointAt control (Progress.structuralProgressReconciliation progress)
      checkpoint = maybe (Progress.topologyControlCheckpointWithConsequence control affects bytes) (\ident -> Progress.topologyJoinSealCheckpoint ident control bytes) admission
      prepared = checked (Progress.prepareTopologyCutEstablished view checkpoint certificate progress)
   in replaceStartupStructuralProgressState (Progress.commitTopologyCutEstablished prepared) state

-- Exercise the elected announcer's complete protocol using this fixture's
-- actual membership. An older open round must finish before the pending Begin
-- can select its admission checkpoint and certify the current control prefix.
settleControlBaseCut :: HeraldState -> IO HeraldState
settleControlBaseCut initial = settle initial
  where
    target = startupStructuralProgressState initial
    currentCutCoversTarget state =
      let progress = startupStructuralProgressState state
       in case lookup (Progress.structuralLastInstalledCutId progress) (Progress.structuralInstalledCutEntries progress) of
            Nothing -> False
            Just installed ->
              let frontier = topologyCutFrontier (Progress.installedTopologyCutCut installed)
               in topologyFrontierStructuralVersionVector frontier == Progress.structuralAppliedVector target
                    && topologyFrontierAppliedControlPrefix frontier >= Progress.structuralAppliedControlPrefix target
    settle predecessor = do
      let before = startupStructuralProgressState predecessor
          local = Progress.structuralProgressLocalHerald before
          remotes = filter (/= local) (NE.toList (Progress.structuralProgressMembers before))
          reportFor remote = GraphProtocol.structuralAppliedReport remote (Progress.structuralAppliedVector before) (Progress.structuralAppliedControlPrefix before)
          reported = foldl (\progress remote -> fst (Progress.commitStructuralReport (checked (Progress.prepareStructuralReport (reportFor remote) progress)))) before remotes
          (announced, _, installed) = checked (PeerControl.advanceLocalTopologyCutIfReady (replaceStartupStructuralProgressState reported predecessor))
          announcedProgress = startupStructuralProgressState announced
      (finished, cuts) <- case Progress.structuralOpenCut announcedProgress of
        Nothing -> pure (announced, installed)
        Just announce -> do
          let cut = GraphProtocol.topologyCutAnnounceId announce
              frontier = topologyCutFrontier (GraphProtocol.topologyCutAnnounceCut announce)
              acceptance remote = GraphProtocol.topologyCutAcceptance cut (GraphProtocol.structuralAppliedReport remote (topologyFrontierStructuralVersionVector frontier) (topologyFrontierAppliedControlPrefix frontier))
              established = case Progress.structuralOpenEstablished announcedProgress of
                Just _ -> announcedProgress
                Nothing ->
                  let accepted = foldl (\progress remote -> Progress.commitTopologyCutAcceptance (checked (Progress.prepareTopologyCutAcceptance (acceptance remote) progress))) announcedProgress remotes
                   in Progress.commitTopologyCutEstablishment (checked (Progress.prepareTopologyCutEstablishment accepted))
              acknowledged = foldl (\progress remote -> Progress.commitTopologyCutEstablishedAck (checked (Progress.prepareTopologyCutEstablishedAck (GraphProtocol.topologyCutEstablishedAck cut remote (Progress.structuralProgressMembershipGenerationId before)) progress))) established remotes
          pure (replaceStartupStructuralProgressState acknowledged announced, Set.toAscList (Set.fromList (cut : installed)))
      let settled = fst (checked (StructuralCoordinator.settleInstalledStructuralCuts cuts finished))
      if currentCutCoversTarget settled
        then pure settled
        else do
          assertBool
            ("topology ceremony cannot reach current source frontier: target=" <> show (Progress.structuralAppliedVector target, Progress.structuralAppliedControlPrefix target) <> "; installed=" <> show (Progress.structuralLastInstalledCutControlPrefix (startupStructuralProgressState settled)))
            (Progress.structuralLastInstalledCutId (startupStructuralProgressState settled) /= Progress.structuralLastInstalledCutId before)
          settle settled

caseDisappearanceHistory :: Bool -> Assertion
caseDisappearanceHistory reintroduce = do
  let (seed, subject) = liveRegularDisappearanceFixture
  (oracle0, preparedNetwork, _) <- LiveDisappearance.prepareLiveResolveNetwork seed subject
  let local = Genesis.checkedLocalHeraldEpoch (startupGenesis seed)
      owner network = maybe (error "live regular history fixture lost its source") fst (Map.lookup local network)
      prepared = owner preparedNetwork
  resolveEnvelope <- case [canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch) | SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions (startupOracleClientState prepared)] of
    [envelope] -> pure envelope
    other -> assertFailure ("expected one regular Resolve dispatch: " <> show other)
  (oracle1, resolvedNetwork, _, resolveMessages) <- LiveDisappearance.commitNetworkWithEffects "regular Resolve before join" oracle0 preparedNetwork resolveEnvelope
  settledResolvedNetwork <- LiveDisappearance.drainNetwork Set.empty resolvedNetwork resolveMessages
  sourceNetwork <-
    if reintroduce
      then do
        (rewritten, effects) <- LiveDisappearance.step "rewrite the retired regular definition before join" liveRegularDisappearanceRewriteInput (owner settledResolvedNetwork)
        let rewrittenNetwork = Map.adjust (\(_, binding) -> (rewritten, binding)) local settledResolvedNetwork
        LiveDisappearance.drainNetwork Set.empty rewrittenNetwork (LiveDisappearance.networkMessages local (effectBatchMembers effects))
      else pure settledResolvedNetwork
  let resolved = owner sourceNetwork
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews resolved) (startupStructuralProgressState resolved))
      identifier = oracleClientRequestId founderEpoch (100000 + controlIndexWord64 (oracleGreatestControlIndex oracle1) + 1)
      beginEnvelope = oracleEnvelope identifier Nothing founderEpoch (heraldAdmissionCommand (BeginHeraldAdmission manifest anchor))
  (oracle2, begunNetwork, _, beginMessages) <- LiveDisappearance.commitNetworkWithEffects "Begin after regular disappearance" oracle1 sourceNetwork beginEnvelope
  settledNetwork <- LiveDisappearance.drainNetwork Set.empty begunNetwork beginMessages
  let record = fromJust (oraclePendingHeraldAdmission oracle2)
      begun = owner settledNetwork
  assertEqual "regular disappearance source is independently valid" (Right ()) (validateHeraldState begun)
  history <- maybe (assertFailure "regular disappearance source history did not become ready") pure (checked (History.captureJoinHistory record begun))
  let
    genesis = checked (checkJoiningHeraldGenesis (startupGenesis seed) record)
    observer = fst (checked (initialHerald (monotonicInstant 1) genesis (startupInitialBootstraps seed) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
    (imported, complete) = replaySingleSourceHistory (captureSource begun) observer
  case JoinReplay.replayJoinHistory history imported of
    Left problem -> assertFailure ("based disappearance history replay: " <> show problem)
    Right _ -> pure ()
  assertEqual "complete retired definition history imports" True complete
  case Disappearance.disappearanceSubjectView subject of
    Disappearance.RegularSortDefinitionSubjectView sortId _ occurrence -> do
      if reintroduce
        then do
          effective <- maybe (assertFailure "rewritten regular definition was not reintroduced") pure (Registry.lookupEffectiveSort sortId (startupSortRegistryState imported))
          retirement <- maybe (assertFailure "reintroduction lost the old definition retirement") pure (Registry.lookupRegularSortRetirement sortId occurrence (startupSortRegistryState imported))
          assertEqual "rewritten definition uses the derived successor occurrence" (Registry.regularSortRetirementSuccessorOccurrenceId retirement) (Registry.registryEntryOccurrenceId effective)
          assertBool "reintroduction preserves the distinct original occurrence" (Registry.registryEntryOccurrenceId effective /= occurrence)
        else assertEqual "retired definition remains ineffective" Nothing (Registry.lookupEffectiveSort sortId (startupSortRegistryState imported))
      assertEqual "exact original retirement floor is retained" (Registry.lookupRegularSortRetirement sortId occurrence (startupSortRegistryState resolved)) (Registry.lookupRegularSortRetirement sortId occurrence (startupSortRegistryState imported))
    _ -> assertFailure "expected regular disappearance fixture"
  assertEqual "observer has no historical local probe" [] (DisappearanceState.probeWitnesses (startupDisappearanceState imported))
  assertEqual "observer has no ordinary Oracle effects" [] (Client.oracleClientActions (startupOracleClientState imported))
  assertEqual "suppressed history retains full Herald invariants" (Right ()) (validateHeraldState imported)
  assertJoiningBase begun observer imported
  if reintroduce then pure () else assertTerminalLaterDonor record settledNetwork observer

caseLabelHistory :: Assertion
caseLabelHistory = do
  (pendingSeed, _, _, labelRequest, _) <- LabelFixture.settledNeutralControlledFixtureWithApplicationRequests
  seed <- LabelFixture.completeAssignedPeerPublications pendingSeed
  let oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps (startupGenesis seed) (startupInitialBootstraps seed)))
      labelStep at state = fst (checked (stepHerald (heraldInput (monotonicInstant at) (ApplicationRequestInput labelRequest)) state))
      requested = labelStep 100 seed
  (oracle1, opened, _) <- LabelFixture.commitNextLabelRequest 101 oracle0 requested
  let ready = labelStep 102 (opened)
  (oracle2, released, _) <- LabelFixture.commitCompleteLabelWorkflow 103 oracle1 ready
  let settled = released
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews settled) (startupStructuralProgressState settled))
      (oracle3, entry) = submit (BeginHeraldAdmission manifest anchor) oracle2
      record = fromJust (oraclePendingHeraldAdmission oracle3)
  begun <- settleControlBaseCut (deliverEntry entry settled)
  let
    history = fromJust (checked (History.captureJoinHistory record begun))
    genesis = checked (checkJoiningHeraldGenesis (startupGenesis seed) record)
    observer = fst (checked (initialHerald (monotonicInstant 1) genesis (startupInitialBootstraps seed) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
    (imported, complete) = replaySingleSourceHistory (captureSource begun) observer
  case JoinReplay.replayJoinHistory history imported of
    Left problem -> assertFailure ("based label history replay: " <> show problem)
    Right _ -> pure ()
  assertEqual "complete released label history imports" True complete
  let sourceRecords = Controlled.controlledReleasedLabelEntries (startupControlledState begun)
      importedRecords = Controlled.controlledReleasedLabelEntries (startupControlledState imported)
      (replayed, replayComplete) = replaySingleSourceHistory (captureSource begun) imported
  assertEqual "join history preserves every released pair and terminal generation" sourceRecords importedRecords
  assertBool "the imported history includes a successful generated label" (any ((> 0) . releasedLabelStateGeneration . labelRecordReleasedState . snd) importedRecords)
  assertEqual "exact join-history replay completes" True replayComplete
  assertEqual "exact join replay never takes a second generation successor" importedRecords (Controlled.controlledReleasedLabelEntries (startupControlledState replayed))
  assertEqual "released label rebuilds the exact current vertices" (Graph.graphVertices (startupGraphState begun)) (Graph.graphVertices (startupGraphState imported))
  assertEqual "released label rebuilds the exact current edges" (Graph.graphEdges (startupGraphState begun)) (Graph.graphEdges (startupGraphState imported))
  assertEqual "observer has no old-member label barrier" Set.empty (LabelBarrier.barrierActiveDecisions (startupLabelBarrierState imported))
  assertEqual "observer has no old-member Oracle actions" [] (Client.oracleClientActions (startupOracleClientState imported))
  assertEqual "label history retains full Herald invariants" (Right ()) (validateHeraldState imported)
  assertPreparedControlBase begun observer imported

caseControlledDisappearanceHistory :: Assertion
caseControlledDisappearanceHistory = do
  (seed, object, _) <- LabelFixture.locallyTakenNeutralDisappearanceFixture
  subject <- case [candidate.candidateSubject | candidate <- DisappearanceState.candidateWitnesses (startupDisappearanceState seed), Disappearance.ControlledPredefinedSubjectView candidateObject _ _ <- [Disappearance.disappearanceSubjectView candidate.candidateSubject], candidateObject == object] of
    [candidate] -> pure candidate
    _ -> assertFailure "expected exact controlled disappearance candidate"
  (oracle0, preparedNetwork, _) <- LiveDisappearance.prepareLiveResolveNetwork seed subject
  let local = Genesis.checkedLocalHeraldEpoch (startupGenesis seed)
      owner network = maybe (error "live controlled history fixture lost its source") fst (Map.lookup local network)
      prepared = owner preparedNetwork
  resolveEnvelope <- case [canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch) | SubmitOracleRequest _ dispatch <- Client.oracleClientRequestActions (startupOracleClientState prepared)] of
    [envelope] -> pure envelope
    other -> assertFailure ("expected exactly one live Resolve dispatch: " <> show other)
  (oracle1, resolvedNetwork, _, resolveMessages) <- LiveDisappearance.commitNetworkWithEffects "controlled Resolve before join" oracle0 preparedNetwork resolveEnvelope
  settledResolvedNetwork <- LiveDisappearance.drainNetwork Set.empty resolvedNetwork resolveMessages
  let resolved = owner settledResolvedNetwork
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews resolved) (startupStructuralProgressState resolved))
      identifier = oracleClientRequestId founderEpoch (100000 + controlIndexWord64 (oracleGreatestControlIndex oracle1) + 1)
      beginEnvelope = oracleEnvelope identifier Nothing founderEpoch (heraldAdmissionCommand (BeginHeraldAdmission manifest anchor))
  (oracle2, begunNetwork, _, beginMessages) <- LiveDisappearance.commitNetworkWithEffects "Begin after controlled disappearance" oracle1 settledResolvedNetwork beginEnvelope
  settledNetwork <- LiveDisappearance.drainNetwork Set.empty begunNetwork beginMessages
  let record = fromJust (oraclePendingHeraldAdmission oracle2)
      begun = owner settledNetwork
  assertEqual "real old-member reports and plan admissions preserve the source invariant" (Right ()) (validateHeraldState begun)
  history <- maybe (assertFailure "controlled disappearance source history did not become ready after the live peer network drained") pure (checked (History.captureJoinHistory record begun))
  let
    genesis = checked (checkJoiningHeraldGenesis (startupGenesis seed) record)
    observer = fst (checked (initialHerald (monotonicInstant 1) genesis (startupInitialBootstraps seed) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
    (imported, complete) = replaySingleSourceHistory (captureSource begun) observer
  case JoinReplay.replayJoinHistory history imported of
    Left problem -> assertFailure ("based controlled disappearance history replay: " <> show problem)
    Right _ -> pure ()
  assertEqual "complete controlled deletion history imports" True complete
  assertBool "exact controlled object remains terminally deleted" (Controlled.controlledObjectTerminallyDeleted object (startupControlledState imported))
  assertEqual "observer has no historical local probe" [] (DisappearanceState.probeWitnesses (startupDisappearanceState imported))
  assertEqual "terminal history retains full Herald invariants" (Right ()) (validateHeraldState imported)
  assertPreparedControlBase begun observer imported

-- A later old member can retain a genuine historical observation that the
-- first imported Store never received. Current terminal suppression consumes
-- it without adding a Store receipt; only the exact Join receipt makes that
-- donor complete. Exercise two actual network owners, not edited History bytes.
assertTerminalLaterDonor :: HeraldAdmissionRecord -> LiveDisappearance.LiveNetwork -> HeraldState -> Assertion
assertTerminalLaterDonor record network observer = do
  sources <- forM (Map.elems network) $ \(source, _) -> do
    history <- maybe (assertFailure "terminal donor history was not ready") pure (checked (History.captureJoinHistory record source))
    base <- maybe (assertFailure "terminal donor paired base was not ready") pure (checked (ControlBase.captureJoiningBase source))
    let installed = fst (checked (ControlBase.installPortableJoiningBase (History.joinHistorySource history) (ControlBase.encodeJoiningBase base) observer))
    pure (captureSource source, installed)
  let candidates =
        [ (laterSource, before, after, complete, role, evidence, observation, slot)
        | (firstSource, before) <- sources,
          (laterSource, _) <- sources,
          let firstHistory = History.sourceJoinHistory firstSource,
          let laterHistory = History.sourceJoinHistory laterSource,
          History.joinHistorySource firstHistory /= History.joinHistorySource laterHistory,
          let (after, complete) = replaySingleSourceHistory laterSource before,
          (role, evidence) <- JoinState.importedSystemViewObservations (startupJoinState after),
          not (JoinState.hasImportedObservation role evidence (startupJoinState before)),
          let observation = historyStoreObservation role evidence after,
          Just slot <- [historySystemSlot role after],
          observation `notElem` Store.storeSlotApplicationObservations slot,
          Store.currentStoreObservationSuppressed (Store.storeSlotDelta slot) (Store.storeSlotIncarnation slot) observation (startupStoreState before)
        ]
  (laterSource, before, after, complete, role, evidence, observation, slot) <- case candidates of
    candidate : _ -> pure candidate
    [] -> assertFailure "real donor histories lack a distinct terminal observation absent from the imported Store"
  let history = History.sourceJoinHistory laterSource
  assertEqual "a terminally ignored later donor completes" True complete
  assertBool "later donor readiness includes its exact terminal Join receipt" (History.joinHistoryInstalled history after)
  assertBool "the new evidence was absent before this donor arrived" (not (JoinState.hasImportedObservation role evidence (startupJoinState before)))
  assertBool "terminal application did not manufacture the missing Store observation" (observation `notElem` Store.storeSlotApplicationObservations slot)
  let storeBeforeRetry = startupStoreState after
      prepared = checked (Store.prepareAlignmentStoreApplication (Store.peerStoreDestination (Store.storeSlotDelta slot) (Store.storeSlotIncarnation slot) (Store.retainedStoreObservationIncomingStrength observation) :| []) observation storeBeforeRetry)
      (storeAfterRetry, outcomes) = Store.commitPeerStoreApplication prepared
  assertEqual "the exact terminal owner application is an ignored result" [Store.PeerStoreTerminallyIgnored] (map Store.peerStoreOutcomeDisposition (NE.toList outcomes))
  assertBool "terminal application adds no Store state, revision, or publication receipt" (storeAfterRetry == storeBeforeRetry)
  let retainedJoin = startupJoinState after
      retained = JoinState.importedSystemViewObservations retainedJoin
      withoutExact = filter (/= (role, evidence)) retained
      provenance = checked (JoinState.retainImportedProvenance (JoinState.importedPlanAcceptances retainedJoin) (JoinState.importedOccurrences retainedJoin) JoinState.emptyState)
      withHistories = foldl (\state ((admission, attempt, donor), bytes) -> JoinState.retainInstalledHistory admission attempt donor bytes state) provenance (JoinState.installedHistories retainedJoin)
      withCaptures = foldl (\state ((admission, attempt), bytes) -> JoinState.retainCapture admission attempt bytes state) withHistories (JoinState.capturedHistories retainedJoin)
      replaceReceipts receipts = replaceStartupJoinState (foldl (\state (receiptRole, receipt) -> JoinState.retainImportedObservation receiptRole receipt state) withCaptures receipts) after
      missing = replaceReceipts withoutExact
      changed = replaceReceipts ((role, changeHistoryEvidenceStrength evidence) : withoutExact)
      missingSlot = replaceStartupStoreState (Store.passivateStoreForInvariantTest (Store.storeSlotDelta slot) (startupStoreState after)) after
      (repeated, repeatedComplete) = replaySingleSourceHistory laterSource after
  assertEqual "fixture carries no unrelated command request receipts" [] (JoinState.requestEntries retainedJoin)
  assertBool "current suppression alone cannot certify an unreceived observation" (not (History.joinHistoryInstalled history missing))
  assertBool "a receipt with different canonical strength cannot certify the observation" (not (History.joinHistoryInstalled history changed))
  assertBool "an exact receipt cannot replace a missing current destination" (not (History.joinHistoryInstalled history missingSlot))
  assertEqual "terminal donor retry completes" True repeatedComplete
  assertBool "terminal donor retry preserves every Store owner fact" (startupStoreState repeated == startupStoreState after)
  assertEqual "terminal donor preserves whole Herald invariants" (Right ()) (validateHeraldState after)

historySystemSlot :: PredefinedSortRole -> HeraldState -> Maybe Store.StoreSlot
historySystemSlot role state = find (\slot -> Store.storeSlotProvenance slot == Store.HeraldSystemView (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) role) (Store.storeSlots (startupStoreState state))

historyStoreObservation :: PredefinedSortRole -> Evidence.RetainedPublicationEvidence -> HeraldState -> Store.RetainedStoreObservation
historyStoreObservation role evidence state =
  let entry = fromJust (Registry.lookupPredefinedRole role (startupSortRegistryState state))
      value = checked (decodeCanonicalValue (canonicalValueByteString (Evidence.retainedPublicationEvidenceCanonicalValue evidence)))
      publication = checked (mkCheckedPublication (Registry.registryEntryDescriptor entry) (Evidence.retainedPublicationEvidencePublicationId evidence) value)
      occurrence = Evidence.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
      origin = case Evidence.retainedPublicationEvidenceOrigin evidence of
        Evidence.PrimordialRetainedObservation -> Store.PrimordialStoreObservation
        Evidence.RoutedRetainedObservation process topology control stamp -> Store.RoutedStoreObservation (Store.storeObservationEnvelope process occurrence topology control stamp)
   in checked (Store.retainedStoreObservation publication (Evidence.retainedPublicationEvidenceSourceStrength evidence) occurrence origin)

changeHistoryEvidenceStrength :: Evidence.RetainedPublicationEvidence -> Evidence.RetainedPublicationEvidence
changeHistoryEvidenceStrength evidence =
  let identifier = Evidence.retainedPublicationEvidencePublicationId evidence
      sort = Evidence.retainedPublicationEvidenceSortId evidence
      occurrence = Evidence.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
      value = Evidence.retainedPublicationEvidenceCanonicalValue evidence
      strength = if Evidence.retainedPublicationEvidenceSourceStrength evidence == Normal then Weak else Normal
   in case Evidence.retainedPublicationEvidenceOrigin evidence of
        Evidence.PrimordialRetainedObservation -> Evidence.primordialRetainedPublicationEvidence identifier sort occurrence value strength
        Evidence.RoutedRetainedObservation process topology control stamp -> checked (Evidence.retainedPublicationEvidence identifier process sort occurrence value strength topology control stamp)

caseBaseActivation :: Assertion
caseBaseActivation = assertBaseActivation False

caseReclaimedBaseProgress :: Assertion
caseReclaimedBaseProgress = assertBaseActivation True

assertBaseActivation :: Bool -> Assertion
assertBaseActivation reclaim = do
  let (oracle0, record, _, founder0, observer0) = capturedFixture
      base = fromJust (checked (ControlBase.captureJoiningBase founder0))
      observer1 = fst (checked (ControlBase.installJoiningBase base observer0))
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      afterSeal = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      afterAccept@(_, founder2, observer3) = advance (AcceptHeraldJoinSeal (admissionRecordId record) 1 (joinSealDigest seal)) afterSeal
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("paired base readiness failed: " <> show reply)
      afterOld = advance (HeraldJoinBaseReady (ready founder2)) afterAccept
      afterNew@(_, _, beforeActivation) = advance (HeraldJoinReady (ready observer3)) afterOld
      (oracleFinal, founderFinal, observerFinal) = advance (ActivateHerald (admissionRecordId record)) afterNew
  assertEqual "paired base stays restricted through readiness" False (serving beforeActivation)
  assertEqual "ordinary suffix activates imported observer" True (serving observerFinal)
  assertEqual "activation preserves whole-owner consistency" (Right ()) (validateHeraldState observerFinal)
  assertEqual "old member remains consistent" (Right ()) (validateHeraldState founderFinal)
  assertEqual "activation releases imported transfer bytes" [] (JoinState.installedHistories (startupJoinState observerFinal))
  if reclaim
    then do
      assertReclaimedControlBase observerFinal
      let compacted = checked (ControlBase.reclaimControlBasePrefix observerFinal)
          -- An already completed admission is rejected by the Oracle. It still
          -- advances the contiguous cursor and tests the ordinary watch suffix.
          envelope = oracleEnvelope (oracleClientRequestId founderEpoch 999999) Nothing founderEpoch (heraldAdmissionCommand (ActivateHerald (admissionRecordId record)))
          (_, _, oracleEffectsResult) = checked (stepOracle envelope oracleFinal)
          suffix = case oracleEffects oracleEffectsResult of
            [EmitAppliedOracleEntry entry] -> canonicalizeAppliedOracleEntry entry
            other -> error ("expected one rejected suffix entry: " <> show other)
          reference = deliverEntry suffix observerFinal
          advanced = deliverEntry suffix compacted
          retried = deliverEntry suffix advanced
          oldEntries = [capturedBeginEntry]
          overlapped = foldl (flip deliverEntry) retried oldEntries
      assertEqual "reclaimed prefix leaves later watch application valid" (Right ()) (validateHeraldState advanced)
      assertBool "suffix semantic facts agree with the full archive reference" (Projection.oracleView (startupOracleProjectionState advanced) == Projection.oracleView (startupOracleProjectionState reference))
      assertBool "retained suffix duplicates are no-ops" (retried == advanced)
      assertBool "covered old traffic cannot replay effects or resurrect archive entries" (overlapped == advanced)
      assertReclaimedControlBase advanced
    else pure ()

-- The source capsule is frozen before Seal. A receiver whose external startup
-- record already contains that seal must admit the original bytes, then use
-- ordinary canonical suffix controls through activation. Recapturing after the
-- seal would change the source member-cut digest and is deliberately deferred.
casePortableSealedStartup :: Assertion
casePortableSealedStartup = do
  let (oracle0, record, history, founder0, _) = capturedFixture
      admission = admissionRecordId record
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      (oracle1, sealEntry) = submit (SealHeraldAdmission seal) oracle0
      founder1 = deliverEntry sealEntry founder0
      sealedRecord = fromJust (oraclePendingHeraldAdmission oracle1)
      observer0 = initialize (checked (checkJoiningHeraldGenesis founderGenesis sealedRecord))
      source = History.joinHistorySource history
      memberKey = (admission, admissionRecordAttempt record, source)
      ready state = case secondOf3 (request (Join.ReadJoinReady admission) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("portable sealed-startup readiness failed: " <> show reply)
  captureResult <- either (assertFailure . ("portable preseal capture: " <>) . show) pure (ControlBase.captureJoiningBase founder0)
  captured <- maybe (assertFailure "portable preseal source is not ready") pure captureResult
  let bytes = ControlBase.encodeJoiningBase captured
  assertEqual "startup record carries the exact source seal" (Just seal) (admissionRecordSeal sealedRecord)
  (imported, effects) <- either (assertFailure . ("portable sealed-startup install: " <>) . show) pure (ControlBase.installPortableJoiningBase source bytes observer0)
  assertEqual "sealed startup imports without application or network effects" [] (effectBatchMembers effects)
  assertEqual "sealed startup keeps the original aggregate bytes" (Just (sourceBytes founder0)) (lookup memberKey (JoinState.installedHistories (startupJoinState imported)))
  assertEqual "sealed startup keeps the exact original member-cut digest" (Just (History.joinSourceMemberCut (captureSource founder0))) (lookup source (joinSealMemberCuts seal))
  assertBool "sealed startup has installed all original history evidence" (History.joinHistoryInstalled history imported)
  assertBool "sealed startup remains a restricted observer" (not (serving imported))
  assertEqual "sealed startup imported state satisfies the full audit" (Right ()) (validateHeraldState imported)
  let (liveImported, liveReply, liveEffects) = request (Join.InstallJoinHistory bytes) observer0
      (liveSealed, liveSealReply, _) = request (Join.InstallJoinControlHistory [sealEntry]) liveImported
  assertEqual "sealed-startup cursor-zero live import accepts the original preseal bytes" Join.JoinHistoryInstalled liveReply
  assertBool "live sealed-startup import preserves the externally supplied startup record" (startupGenesis liveImported == startupGenesis observer0)
  assertEqual "preseal source adoption does not synthesize the Seal control" Nothing (admissionRecordSeal =<< Projection.oracleViewHeraldAdmission admission (Projection.oracleView (startupOracleProjectionState liveImported)))
  assertEqual "externally known Seal alone does not produce local readiness" Join.JoinRetry (secondOf3 (request (Join.ReadJoinReady admission) liveImported))
  assertEqual "live sealed-startup adoption emits only its reply" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinHistoryInstalled)] (effectBatchMembers liveEffects)
  assertEqual "the real Seal control follows the live adopted base" Join.JoinHistoryInstalled liveSealReply
  assertBool "exact Seal replay permits local readiness" (case secondOf3 (request (Join.ReadJoinReady admission) liveSealed) of Join.JoinReadyReply {} -> True; _ -> False)
  (domain, projectionFrame, registryFrame, progressFrame, carrierFrame, historyFrame) <- either assertFailure pure (Serialize.decode bytes :: Either String RawPortableJoiningBase)
  let mutations =
        [ ("Projection", (domain, projectionFrame <> "x", registryFrame, progressFrame, carrierFrame, historyFrame)),
          ("Registry", (domain, projectionFrame, registryFrame <> "x", progressFrame, carrierFrame, historyFrame)),
          ("Progress", (domain, projectionFrame, registryFrame, progressFrame <> "x", carrierFrame, historyFrame)),
          ("Carrier", (domain, projectionFrame, registryFrame, progressFrame, carrierFrame <> "x", historyFrame))
        ]
      originalSource = checked (History.decodeJoinSourceHistory bytes)
  forM_ mutations $ \(ownerName, changedFrames) -> do
    let changedBytes = Serialize.encode changedFrames
        changedSource = checked (History.decodeJoinSourceHistory changedBytes)
    assertEqual (ownerName <> " mutation preserves the complete nested History") history (History.sourceJoinHistory changedSource)
    assertBool (ownerName <> " mutation changes the sealed source digest") (History.joinSourceMemberCut changedSource /= History.joinSourceMemberCut originalSource)
    assertBool (ownerName <> " mutation cannot be adopted under the original source seal") (isLeft (ControlBase.installPortableJoiningBase source changedBytes observer0))
  -- The local frozen capture is an authority distinct from an imported row.
  -- This deliberately replaces only the imported receipt to isolate that
  -- boundary; all immutable semantic History and the real local capture stay
  -- unchanged, and the alternative seal is committed by the actual Oracle.
  let changedBytes = Serialize.encode (domain, projectionFrame, registryFrame <> "x", progressFrame, carrierFrame, historyFrame)
      changedSource = checked (History.decodeJoinSourceHistory changedBytes)
      changedSeal = checked (Join.prepareJoinSeal record [changedSource])
      (_, changedSealEntry) = submit (SealHeraldAdmission changedSeal) oracle0
      changedSealed = deliverEntry changedSealEntry founder0
      replacementReceipt = JoinState.retainInstalledHistory admission (admissionRecordAttempt record) source changedBytes (startupJoinState changedSealed)
      substituted = replaceStartupJoinState replacementReceipt changedSealed
      token = Join.joinRequestId source 990009
      command sealValue = Join.SubmitJoinCommand token (AcceptHeraldJoinSeal admission (admissionRecordAttempt record) (joinSealDigest sealValue))
      (refused, refusedReply, _) = request (command changedSeal) substituted
  assertEqual "the actual frozen own source admits its exact canonical seal" Join.JoinCommandPending (secondOf3 (request (command seal) founder1))
  assertEqual "an imported equal-History source cannot replace the frozen local capture" Join.JoinRetry refusedReply
  assertEqual "failed own-source acceptance preserves the frozen aggregate" (Just bytes) (JoinState.lookupCapture admission (admissionRecordAttempt record) (startupJoinState refused))
  assertEqual "failed own-source acceptance submits no Oracle request" (startupOracleClientState substituted) (startupOracleClientState refused)
  recapture <- either (assertFailure . ("portable postseal capture: " <>) . show) pure (ControlBase.captureJoiningBase founder1)
  assertBool "postseal source cannot replace the frozen aggregate with a new capture" (maybe True (const False) recapture)
  let (observer1, sealReply, _) = request (Join.InstallJoinControlHistory [sealEntry]) imported
      afterAccept@(_, founder2, observer2) = advance (AcceptHeraldJoinSeal admission (admissionRecordAttempt record) (joinSealDigest seal)) (oracle1, founder1, observer1)
      afterOld = advance (HeraldJoinBaseReady (ready founder2)) afterAccept
      afterNew = advance (HeraldJoinReady (ready observer2)) afterOld
      (_, _, active) = advance (ActivateHerald admission) afterNew
  assertEqual "ordinary suffix installs the already externally known seal" Join.JoinHistoryInstalled sealReply
  assertBool "sealed-startup portable import reaches ordinary activation" (serving active)
  assertEqual "activation after portable sealed-startup import satisfies the full audit" (Right ()) (validateHeraldState active)

caseActivation :: Assertion
caseActivation = do
  let (oracle0, record, _, founder0, observer0) = capturedFixture
      observer1 = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder0)) observer0)
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      (oracle1, founder1, observer2) = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      (oracle2, founder2, observer3) = advance (AcceptHeraldJoinSeal (admissionRecordId record) 1 (joinSealDigest seal)) (oracle1, founder1, observer2)
      oldReport = ready founder2
      newReport = ready observer3
      afterOld = advance (HeraldJoinBaseReady oldReport) (oracle2, founder2, observer3)
      afterNew@(_, _, beforeActivation) = advance (HeraldJoinReady newReport) afterOld
      (oracleFinal, founderFinal, observerFinal) = advance (ActivateHerald (admissionRecordId record)) afterNew
  assertEqual "readiness identifies old source" founderEpoch (joinReadyReporter oldReport)
  assertEqual "newcomer identifies its own report" applicantEpoch (joinReadyReporter newReport)
  assertEqual "all readiness still leaves observer restricted" False (serving beforeActivation)
  assertBool "the captured source remains frozen through seal acceptance" (AlignmentCoordinator.alignmentPromotionHandoffPending founder2)
  assertEqual "activation admits applicant" True (serving observerFinal)
  assertEqual "founder remains active" True (serving founderFinal)
  assertBool "activation releases the source promotion barrier" (not (AlignmentCoordinator.alignmentPromotionHandoffPending founderFinal))
  assertEqual "activated membership is shared" (heraldMembershipGenerationId (oracleCurrentMembership oracleFinal)) (Progress.structuralProgressMembershipGenerationId (startupStructuralProgressState observerFinal))
  assertEqual "applicant graph owner leaves observer mode" False (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState observerFinal))
  assertEqual "activated source releases transfer bytes" [] (JoinState.capturedHistories (startupJoinState founderFinal))
  assertEqual "activated applicant releases transfer bytes" [] (JoinState.installedHistories (startupJoinState observerFinal))
  assertAdmissionTail "source import establishes the applicant reconstruction floor" record (Just (controlIndex 1)) observer1
  assertAdmissionTail "readiness retains the applicant reconstruction floor" record (Just (controlIndex 1)) beforeActivation
  assertAdmissionTail "actual local activation discharges the applicant reconstruction tail" record Nothing observerFinal
  assertAdmissionTail "donor activation alone does not acknowledge applicant consumption" record (Just (admissionRecordBeginIndex record)) founderFinal
  assertEqual "founder invariant" (Right ()) (validateHeraldState founderFinal)
  assertEqual "applicant invariant" (Right ()) (validateHeraldState observerFinal)
  let activation = oracleGreatestControlIndex oracleFinal
      beforeDonorActivation = secondOf3 afterNew
      (_, offerEffects) = checked (stepHerald (heraldInput (startupLastObservedTime observerFinal) (PeerInput (PeerCandidateOpened (PeerDiscovery.peerCandidateOpened (PeerDiscovery.connectionNonce 8123) Set.empty Nothing)))) observerFinal)
      (candidate, generation, members, offeredHello) = case [(peer, advertisedGeneration, advertisedMembers, message) | SendPeerCandidate peer advertisedGeneration advertisedMembers message <- effectBatchMembers offerEffects] of
        [offered] -> offered
        other -> error ("activated applicant did not author exactly one Hello: " <> show other)
      hello at =
        PeerDiscovery.peerHello
          (Genesis.checkedSystemId founderGenesis)
          (admissionManifestHeraldId (admissionRecordManifest record))
          applicantEpoch
          (PeerDiscovery.connectionNonce 8123)
          Set.empty
          at
          (Genesis.checkedCatalogueDigest founderGenesis)
          (Genesis.checkedInitialProjectionDigest bootstraps)
          Nothing
      receive at state = checked (stepHerald (heraldInput (startupLastObservedTime state) (PeerInput (PeerHelloReceived candidate Set.empty (if at == activation then offeredHello else hello at) generation members Nothing))) state)
      (rejectedHello, rejectedEffects) = receive activation beforeDonorActivation
      (insufficient, insufficientEffects) = receive (admissionRecordBeginIndex record) founderFinal
      (acknowledged, acknowledgedEffects) = receive activation insufficient
      (duplicate, duplicateEffects) = receive activation acknowledged
      dispositions = \effects -> [disposition | SetPeerCandidateDisposition _ disposition <- effectBatchMembers effects]
      accepted = \effects -> case dispositions effects of [PeerDiscovery.PeerHelloAccepted {}] -> True; _ -> False
  assertEqual "the activated applicant advertises its actual applied prefix" activation (PeerDiscovery.peerHelloAppliedControlIndex offeredHello)
  assertEqual "an actual activated applicant Hello is rejected until the donor observes Activate" [PeerDiscovery.PeerHelloRejected PeerDiscovery.HelloMembershipMismatch] (dispositions rejectedEffects)
  assertAdmissionTail "a rejected Hello cannot discharge the donor promise" record (Just (admissionRecordBeginIndex record)) rejectedHello
  assertBool "an admitted Hello may still advertise an insufficient control cursor" (accepted insufficientEffects)
  assertAdmissionTail "an accepted low-cursor Hello does not prove activation consumption" record (Just (admissionRecordBeginIndex record)) insufficient
  assertBool "the activation-covering Hello is admitted by ordinary peer processing" (accepted acknowledgedEffects)
  assertAdmissionTail "accepted activation consumption releases the donor tail" record Nothing acknowledged
  assertBool "the exact Hello reoffer remains admitted" (accepted duplicateEffects)
  assertAdmissionTail "duplicate acknowledgement does not recreate the discharged tail" record Nothing duplicate
  assertEqual "Hello-driven release preserves every owner" (Right ()) (validateHeraldState acknowledged)
  assertEqual "duplicate Hello preserves every owner" (Right ()) (validateHeraldState duplicate)
  let (afterHistoryRetry, historyReply, _) = request (Join.InstallJoinHistory (sourceBytes founder0)) observerFinal
  assertEqual "activated donor transfer has an explicit retired result" (Join.JoinTransferRetired (admissionRecordId record) (admissionRecordAttempt record)) historyReply
  assertBool "old donor replay preserves the current local stores" (startupStoreState afterHistoryRetry == startupStoreState observerFinal)
  assertEqual "old donor replay cannot restore the predecessor cut or membership" (startupStructuralProgressState observerFinal) (startupStructuralProgressState afterHistoryRetry)
  assertBool "old donor replay preserves current definition occurrences" (startupSortRegistryState afterHistoryRetry == startupSortRegistryState observerFinal)
  assertEqual "old donor replay preserves full owner invariants" (Right ()) (validateHeraldState afterHistoryRetry)
  let nextManifest = heraldAdmissionManifest (Genesis.checkedSystemId founderGenesis) (checked (mkHeraldId (BS.replicate 32 0xed))) (checked (mkHeraldEpoch (BS.replicate 32 0xf1)))
      nextAnchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews founderFinal) (startupStructuralProgressState founderFinal))
      (_, nextEntry) = submit (BeginHeraldAdmission nextManifest nextAnchor) oracleFinal
      (refused, refusal, _) = request (Join.InstallJoinControlHistory [nextEntry]) observerFinal
      activationEntry = snd (submit (ActivateHerald (admissionRecordId record)) (firstOf3 afterNew))
  assertEqual "activation ends authority for a later fresh callback entry" Join.JoinRetry refusal
  assertEqual "refused active suffix preserves cursor" (Client.oracleClientAppliedCursor (startupOracleClientState observerFinal)) (Client.oracleClientAppliedCursor (startupOracleClientState refused))
  assertEqual "exact activation retry remains accepted" Join.JoinHistoryInstalled (secondOf3 (request (Join.InstallJoinControlHistory [activationEntry]) observerFinal))
  let caughtUp = deliverEntry nextEntry refused
  assertEqual "ordinary Oracle lane applies the fresh suffix" (controlIndex (controlIndexWord64 (oracleGreatestControlIndex oracleFinal) + 1)) (Client.oracleClientAppliedCursor (startupOracleClientState caughtUp))
  where
    ready state = case secondOf3 (request (Join.ReadJoinReady admission) state) of
      Join.JoinReadyReply report -> report
      reply -> error ("ready request failed: " <> show reply)
    admission = admissionRecordId (secondOf5 capturedFixture)

-- Use the real admission history, certified successor base and both owners'
-- actual private placement snapshots. The injected owner-local debt isolates
-- a pre-join covered consequence whose application route sets are both empty;
-- route equality alone must not select its old one-member topology cut.
caseJoiningPlacementSkipsPredecessorTopology :: Assertion
caseJoiningPlacementSkipsPredecessorTopology = do
  let (oracle0, record, history, founder0, observer0) = capturedFixture
      observer1 = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder0)) observer0)
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      afterSeal = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      afterAccept@(_, founder1, observer2) = advance (AcceptHeraldJoinSeal (admissionRecordId record) 1 (joinSealDigest seal)) afterSeal
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("placement fixture readiness failed: " <> show reply)
      afterOld = advance (HeraldJoinBaseReady (ready founder1)) afterAccept
      afterNew = advance (HeraldJoinReady (ready observer2)) afterOld
      (_, founder, observer) = advance (ActivateHerald (admissionRecordId record)) afterNew
      (localPlacement, _) = Placement.commitLocalSnapshot (checked (Placement.prepareRetainedLocalSnapshot (startupPlacementState founder)))
      (_, remoteSnapshot) = Placement.commitLocalSnapshot (checked (Placement.prepareRetainedLocalSnapshot (startupPlacementState observer)))
      snapshot = Placement.localSnapshotMessage remoteSnapshot
      route = case placementSnapshotRoutes snapshot of
        first : _ -> first
        [] -> error "activated observer has no private placement"
      (placement, _) = Placement.commitRemotePlacement (checked (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) localPlacement))
      affectedSort = Debt.sortOccurrence (deltaRouteSortId route) (deltaRouteOccurrenceId route)
      cause = checked (processEndCause (checked (mkProcessEpochId (BS.replicate 32 0xec))) (controlIndex 1))
      debt = Debt.normalizeStructuralDebts [Debt.structuralConsequenceDebt (Debt.structuralDebtKey cause Debt.TopologyAlignmentDebt affectedSort Nothing) (Debt.structuralDebtEvidence Set.empty Set.empty Set.empty)]
      (alignment, _) = Alignment.commitStructuralDebtRetention (checked (Alignment.prepareStructuralDebtRetention cause debt (startupAlignmentState founder)))
      predecessor = replaceStartupAlignmentState alignment (replaceStartupPlacementState placement founder)
      progress = startupStructuralProgressState predecessor
      oldCut = deriveTopologyCutId (History.joinHistoryTopologyCut history)
      successorCut = Progress.structuralLastInstalledCutId progress
      oldProjection = checked (Progress.structuralProjectionAtInstalledCut (structuralReconciliationViews predecessor) oldCut progress)
      oldVertices = Graph.graphNonStructuralBaselineVertices (startupGraphState predecessor) <> Reconciliation.structuralProjectionSnapshotAdmissionVertices oldProjection
      membership = Progress.structuralProgressMembershipGenerationId progress
  assertBool "predecessor topology really covers the retained cause" (Progress.installedCutCoversCause oldCut cause progress)
  assertBool "admission installs a distinct successor topology" (oldCut /= successorCut)
  assertBool "old topology predates the incoming private view" (DeltaVertex (deltaRouteDelta route) `notElem` oldVertices)
  assertBool "the live graph contains the admitted private view" (Graph.graphHasVertex (DeltaVertex (deltaRouteDelta route)) (startupGraphState predecessor))
  (promoted, _, disposition) <- case AlignmentCoordinator.advanceAlignmentGenerationsWithDisposition predecessor of
    Left problem -> assertFailure ("advance old debt with joined private placement: " <> show problem)
    Right result -> pure result
  assertEqual "the compatible successor makes genuine promotion progress" AlignmentCoordinator.AlignmentGenerationWorkChanged disposition
  let owner = startupAlignmentState promoted
      promotedCuts = [Alignment.alignmentPromotionKeyTopologyCut key | (key, _) <- Alignment.alignmentPromotionEntries owner, Alignment.alignmentPromotionKeyCause key == cause]
      generations = [generation | (_, generation) <- Alignment.alignmentGenerationEntries owner]
  assertEqual "old covered debt chooses the membership-compatible successor" [successorCut] promotedCuts
  assertBool "the real private routes produce a generation plan" (not (null generations))
  assertBool "every new certificate agrees with current membership" (all ((== membership) . topologyFrontierMembershipGenerationId . topologyCutFrontier . DomainAlignment.alignmentCutTopologyCut . Generation.alignmentGenerationCut) generations)
  assertEqual "promotion preserves the complete alignment invariant" (Right ()) (Alignment.validateAlignmentState owner)
  case AlignmentCoordinator.advanceAlignmentGenerationsWithDisposition promoted of
    Left problem -> assertFailure ("repeat joined placement: " <> show problem)
    Right (repeated, effects, repeatedDisposition) -> do
      assertBool "repeating joined placement preserves all owners" (repeated == promoted)
      assertEqual "repeating joined placement emits no effects" [] (effectBatchMembers effects)
      assertEqual "repeating joined placement retains the exact coordinate" AlignmentCoordinator.AlignmentGenerationWorkUnchanged repeatedDisposition

caseCancellation :: Assertion
caseCancellation = do
  let (oracle, record, history, founder, observer) = capturedFixture
      imported = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder)) observer)
      (_, released, cancelled) = advance (CancelHeraldAdmission (admissionRecordId record)) (oracle, founder, imported)
  assertEqual "founder returns to serving" True (serving released)
  assertBool "the real source capture initially freezes promotion" (AlignmentCoordinator.alignmentPromotionHandoffPending founder)
  assertBool "cancellation releases the source promotion barrier" (not (AlignmentCoordinator.alignmentPromotionHandoffPending released))
  assertEqual "cancelled applicant stays restricted" False (serving cancelled)
  assertEqual "cancelled bundle cannot be reinstalled" (Join.JoinTransferRetired (admissionRecordId record) (admissionRecordAttempt record)) (secondOf3 (request (Join.InstallJoinHistory (sourceBytes founder)) cancelled))
  assertEqual "cancelled source releases transfer captures" [] (JoinState.capturedHistories (startupJoinState released))
  assertEqual "cancelled observer releases transfer envelopes" [] (JoinState.installedHistories (startupJoinState cancelled))
  assertAdmissionTail "cancellation fixture has a donor obligation" record (Just (admissionRecordBeginIndex record)) founder
  assertAdmissionTail "cancellation fixture has an applicant reconstruction obligation" record (Just (History.joinHistoryControlPrefix history)) imported
  assertAdmissionTail "canonical cancellation releases the donor obligation" record Nothing released
  assertAdmissionTail "canonical cancellation releases the applicant obligation" record Nothing cancelled
  assertEqual "cancelled observer invariant" (Right ()) (validateHeraldState cancelled)
  let (_, cancellation) = submit (CancelHeraldAdmission (admissionRecordId record)) oracle
      (cancelledEmpty, cancelledReply, _) = request (Join.InstallJoinControlHistory [capturedBeginEntry, cancellation]) observer
      (refusedEmpty, emptyReply, emptyEffects) = request (Join.InstallJoinHistory (sourceBytes founder)) cancelledEmpty
  assertEqual "the empty observer learns canonical cancellation before any source" Join.JoinHistoryInstalled cancelledReply
  assertAdmissionTail "Begin followed by cancellation leaves no observer tail" record Nothing cancelledEmpty
  assertEqual "cancelled empty observer rejects old source bytes as retired" (Join.JoinTransferRetired (admissionRecordId record) (admissionRecordAttempt record)) emptyReply
  assertBool "cancelled first import changes no owner" (refusedEmpty == cancelledEmpty)
  assertEqual "cancelled first import retains no envelope" [] (JoinState.installedHistories (startupJoinState refusedEmpty))
  assertEqual "cancelled first import emits only retirement acknowledgment" [SendJoinReply 77 (Join.encodeJoinReply emptyReply)] (effectBatchMembers emptyEffects)
  assertHistoricalProgressEvidence (History.joinHistoryOccurrences history) released observer

caseMalformed :: Assertion
caseMalformed = do
  let (_, _, _, _, observer) = capturedFixture
  mapM_ (assertRejected observer) [BS.empty, "unknown", Join.encodeJoinRequest Join.ReadJoinStatus <> "x", Join.encodeJoinRequest (Join.ReadJoinCommand (Join.joinRequestId applicantEpoch 0)) <> "x"]
  where
    assertRejected state bytes = do
      let (successor, effects) = checked (stepHerald (heraldInput (startupLastObservedTime state) (JoinInput 77 bytes)) state)
      assertEqual "one protocol rejection" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinRejected)] (effectBatchMembers effects)
      assertBool "store owner unchanged" (startupStoreState state == startupStoreState successor)
      assertEqual "graph owner unchanged" (startupStructuralProgressState state) (startupStructuralProgressState successor)
      assertEqual "malformed input cannot admit" False (serving successor)

caseControlGap :: Assertion
caseControlGap = do
  let (oracle0, record, history, founder, observer) = capturedFixture
      seal = checked (Join.prepareJoinSeal record [captureSource founder])
      (_, sealEntry) = submit (SealHeraldAdmission seal) oracle0
      (successor, reply, _) = request (Join.InstallJoinControlHistory [sealEntry]) observer
  assertEqual "control gap rejected" Join.JoinRejected reply
  assertBool "rejected gap preserves exact projection" (startupOracleProjectionState observer == startupOracleProjectionState successor)
  assertEqual "empty observer has no sealed cut" False (History.joinHistoryInstalled history successor)

caseSealCoverage :: Assertion
caseSealCoverage = do
  let (_, record, _, founder, _) = capturedFixture
  assertEqual "one canonical old source accepted" False (isLeft (Join.prepareJoinSeal record [captureSource founder]))
  assertEqual "missing old source rejected" True (isLeft (Join.prepareJoinSeal record []))
  assertEqual "duplicate old source rejected" True (isLeft (Join.prepareJoinSeal record [captureSource founder, captureSource founder]))

caseSparseControlHistory :: Assertion
caseSparseControlHistory = do
  let sourceGenesis = checked (checkHeraldGenesis (Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember))
      selected = checked (checkInitialBootstraps sourceGenesis (PrimordialProcessManifest []))
      source = fst (checked (initialHerald (monotonicInstant 1) sourceGenesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps sourceGenesis selected))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews source) (startupStructuralProgressState source))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record = fromJust (oraclePendingHeraldAdmission oracle1)
      envelope = oracleEnvelope (oracleClientRequestId founderEpoch 900001) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
      rejected = case checked (stepOracle envelope oracle1) of
        (_, OracleCommitted receipt, effects) | oracleReceiptResult receipt /= OracleAccepted -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> canonicalizeAppliedOracleEntry entry
          _ -> error "missing foreign rejection"
        _ -> error "unknown voter cancellation must be rejected"
      completed = establishCut (Just (admissionRecordId record)) (deliverEntry rejected (deliverEntry beginEntry source))
      history = fromJust (checked (History.captureJoinHistory record completed))
      index = OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue rejected)
      genesis = checked (checkJoiningHeraldGenesis sourceGenesis record)
      observer = fst (checked (initialHerald (monotonicInstant 1) genesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH3 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      (imported, complete) = replaySingleSourceHistory (captureSource completed) observer
  assertEqual "the source client actually reclaimed the unrelated rejection" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState completed))
  assertEqual "compact Join History omits all covered controls" [] (History.joinHistoryControlEntries history)
  assertEqual "control history inspection uses the promised Projection tail" (Join.JoinControlHistoryReply [rejected]) (secondOf3 (request (Join.ReadJoinControlHistory (admissionRecordBeginIndex record)) completed))
  assertEqual "a sparse donor's complete history installs" True complete
  assertEqual "imported canonical rejection has exact Projection evidence" (Just rejected) (Projection.appliedEntryEvidence index (startupOracleProjectionState imported))
  assertEqual "observer replay also reclaims the unrelated duplicate copy" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState imported))
  assertEqual "the imported sparse owner remains valid" (Right ()) (validateHeraldState imported)

-- The Begin is submitted through the local request owner, so reclamation must
-- retain its exact receipt in Projection. The compact History must omit that
-- genuine sparse pin and carry only the contiguous suffix above its anchor.
coveredHistoryFixture :: Bool -> (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState, HeraldState, CanonicalAppliedOracleEntry, [CanonicalAppliedOracleEntry])
coveredHistoryFixture withSuffix = coveredHistorySuffixFixture (if withSuffix then 1 else 0)

coveredHistorySuffixFixture :: Int -> (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState, HeraldState, CanonicalAppliedOracleEntry, [CanonicalAppliedOracleEntry])
coveredHistorySuffixFixture suffixLength =
  let initial = initialize founderGenesis
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews initial) (startupStructuralProgressState initial))
      token = Join.joinRequestId founderEpoch 900010
      requested = firstOf3 (request (Join.SubmitJoinCommand token (BeginHeraldAdmission manifest anchor)) initial)
      reference = snd (fromJust (JoinState.lookupRequest token (startupJoinState requested)))
      witness = fromJust (Client.lookupOracleRequest reference (startupOracleClientState requested))
      (oracle1, beginEntry) = commit (canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) initialOracleState
      record = fromJust (oraclePendingHeraldAdmission oracle1)
      begun = deliverEntry beginEntry requested
      compacted = checked (ControlBase.reclaimControlBasePrefix begun)
      appendRejection (previous, entries) sequenceNumber =
        let envelope = oracleEnvelope (oracleClientRequestId founderEpoch (900011 + fromIntegral sequenceNumber)) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
            (advanced, rejected) = commit envelope previous
         in (advanced, entries <> [rejected])
      (oracle, suffix) = foldl appendRejection (oracle1, []) [1 .. suffixLength]
      capture state = case request (Join.CaptureJoinHistory (admissionRecordId record)) state of
        (captured, Join.JoinHistoryReply _, _) -> captured
        (_, reply, _) -> error ("covered source capture failed: " <> show reply)
      compactSource = capture (foldl (flip deliverEntry) compacted suffix)
      fullSource = capture (foldl (flip deliverEntry) begun suffix)
      observer = initialize (checked (checkJoiningHeraldGenesis founderGenesis record))
   in (oracle, record, compactSource, fullSource, observer, beginEntry, suffix)
  where
    commit envelope oracle = case checked (stepOracle envelope oracle) of
      (successor, OracleCommitted _, effects) -> case oracleEffects effects of
        [EmitAppliedOracleEntry entry] -> (successor, canonicalizeAppliedOracleEntry entry)
        other -> error ("covered control fixture expected one canonical entry: " <> show other)
      (_, outcome, _) -> error ("covered control fixture expected a committed request: " <> show outcome)

caseCoveredHistoryCapture :: Bool -> Assertion
caseCoveredHistoryCapture withSuffix = do
  let (_, _, source, fullSource, observer, beginEntry, suffix) = coveredHistoryFixture withSuffix
      history = History.sourceJoinHistory (captureSource source)
      fullHistory = History.sourceJoinHistory (captureSource fullSource)
      covered = OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue beginEntry)
      base = fromJust (checked (ControlBase.captureJoiningBase source))
      fullBase = fromJust (checked (ControlBase.captureJoiningBase fullSource))
      (imported, effects) = checked (ControlBase.installJoiningBase base observer)
      (portable, portableEffects) = checked (ControlBase.installPortableJoiningBase (History.joinHistorySource history) (ControlBase.encodeJoiningBase base) observer)
      fullImported = fst (checked (ControlBase.installJoiningBase fullBase observer))
      readTail after = request (Join.ReadJoinControlHistory after) source
      (unbased, unbasedReply, unbasedEffects) = request (Join.InstallJoinHistory (sourceBytes source)) observer
  assertBool "the source has a real nonzero covered anchor" (covered > controlIndex 0)
  assertEqual "History records the admitted Projection coverage" (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState source)) (History.joinHistoryControlAnchor history)
  assertEqual "the covered anchor includes every completed control" (History.joinHistoryControlPrefix history) (History.joinHistoryControlAnchor history)
  assertEqual "covered admission tails remain outside History" [] (History.joinHistoryControlEntries history)
  assertEqual "the source retains its real exact Begin receipt" (Just beginEntry) (Projection.appliedEntryEvidence covered (startupOracleProjectionState source))
  assertEqual "the exact Begin pin survives independent Projection admission" (Just beginEntry) (Projection.appliedEntryEvidence covered (startupOracleProjectionState portable))
  assertEqual "compact History roundtrips canonically" (Right history) (History.decodeJoinHistory (History.encodeJoinHistory history))
  assertEqual "the compact capture is installed at its source" True (History.joinHistoryInstalled history source)
  assertBool "portable and in-memory imports are exactly equivalent" (portable == imported)
  assertEqual "base adoption emits no effects" [] (effectBatchMembers effects)
  assertEqual "portable adoption emits no effects" [] (effectBatchMembers portableEffects)
  assertBool "compaction preserves the imported semantic projection" (Projection.oracleView (startupOracleProjectionState imported) == Projection.oracleView (startupOracleProjectionState fullImported))
  assertBool "compaction preserves imported carrier meaning" (Reconciliation.captureStructuralCarrierBase (Progress.structuralProgressReconciliation (startupStructuralProgressState imported)) == Reconciliation.captureStructuralCarrierBase (Progress.structuralProgressReconciliation (startupStructuralProgressState fullImported)))
  assertEqual "compaction preserves the installed topology chain" (Progress.structuralInstalledEstablishedChain (startupStructuralProgressState fullImported)) (Progress.structuralInstalledEstablishedChain (startupStructuralProgressState imported))
  assertEqual "compaction preserves current effective vertices" (Graph.graphStructuralVertexProjections (startupGraphState fullImported)) (Graph.graphStructuralVertexProjections (startupGraphState imported))
  assertEqual "compaction preserves current effective edges" (Graph.graphStructuralEdgeProjections (startupGraphState fullImported)) (Graph.graphStructuralEdgeProjections (startupGraphState imported))
  assertEqual "the imported compact base satisfies every owner" (Right ()) (validateHeraldState imported)
  assertBool "the imported compact base remains a joining observer" (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState imported))
  assertBool "the imported compact base cannot serve" (not (serving imported))
  assertBool "the compact History is complete after atomic base adoption" (History.joinHistoryInstalled history imported)
  case JoinReplay.replayJoinHistory history observer of
    Left problem -> assertEqual "old replay explicitly rejects an unavailable anchor before touching data" (JoinReplay.JoinHistoryReplayDataProblem History.JoinHistoryCanonicalControlGap) problem
    Right _ -> assertFailure "an empty receiver replayed compact History without its base"
  assertEqual "live first import adopts the complete compact source base" Join.JoinHistoryInstalled unbasedReply
  assertBool "live adoption restores the same canonical meaning as private base import" (Projection.oracleView (startupOracleProjectionState unbased) == Projection.oracleView (startupOracleProjectionState portable))
  assertEqual "live adoption restores the covered control anchor" (History.joinHistoryControlPrefix history) (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState unbased))
  assertBool "live adoption installs the complete source History" (History.joinHistoryInstalled history unbased)
  assertBool "the imported observer cannot serve before activation" (not (serving unbased))
  assertEqual "live compact adoption emits only its acknowledgment" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinHistoryInstalled)] (effectBatchMembers unbasedEffects)
  assertEqual "live compact adoption satisfies every owner invariant" (Right ()) (validateHeraldState unbased)
  assertEqual "exact receipt evidence and the later suffix can satisfy a complete retained tail" (Join.JoinControlHistoryReply (beginEntry : suffix)) (secondOf3 (readTail (controlIndex 0)))
  assertBool "reading a retained tail does not alter the source" (firstOf3 (readTail (controlIndex 0)) == source)
  assertEqual "requests at the source floor receive the complete remaining suffix" (Join.JoinControlHistoryReply suffix) (secondOf3 (readTail covered))
  let (retried, retryEffects, complete) = checked (JoinReplay.replayJoinHistory history imported)
  assertBool "replay retries are complete on an already based observer" complete
  assertBool "retrying the same compact history changes no owner" (retried == imported)
  -- Atomic adoption includes ordinary completed-batch Client cleanup, while
  -- the independent Projection archive still proves every exact suffix row.
  assertEqual "base adoption completes Client evidence cleanup through its prefix" (History.joinHistoryControlPrefix history) (Client.oracleClientEvidenceCompactedThrough (startupOracleClientState imported))
  forM_ suffix $ \entry -> do
    let index = OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
    assertEqual "base adoption releases the redundant rejected Client row" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState imported))
    assertEqual "base adoption preserves exact Projection evidence" (Just entry) (Projection.appliedEntryEvidence index (startupOracleProjectionState imported))
  assertEqual "retrying compact history produces no effects" [] (effectBatchMembers retryEffects)
  let (settledRetry, settledEffects, settledComplete) = checked (JoinReplay.replayJoinHistory history retried)
  assertBool "subsequent compact History retries remain complete" settledComplete
  assertBool "subsequent compact History retries change no owner" (settledRetry == retried)
  assertEqual "subsequent compact History retries emit no effects" [] (effectBatchMembers settledEffects)
  let released = checked (ControlBase.reclaimControlBasePrefix imported)
      (overlapped, overlapReply, overlapEffects) = request (Join.InstallJoinControlHistory (beginEntry : suffix)) released
      (oldReplayed, oldEffects, oldComplete) = checked (JoinReplay.replayJoinHistory fullHistory released)
  assertEqual "the receiver has no donor-local request pin to retain" Nothing (Projection.appliedEntryEvidence covered (startupOracleProjectionState released))
  assertBool "covered classification satisfies the old complete History" (History.joinHistoryInstalled fullHistory released)
  assertEqual "an entirely covered control overlap completes" Join.JoinHistoryInstalled overlapReply
  assertBool "covered overlap does not restore archive entries or change state" (overlapped == released)
  assertEqual "covered overlap emits only its completion" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinHistoryInstalled)] (effectBatchMembers overlapEffects)
  assertBool "old History replay remains complete after receiver reclamation" oldComplete
  assertBool "old History retry preserves the reclaimed owners" (oldReplayed == released)
  assertEqual "old History replay emits no effects" [] (effectBatchMembers oldEffects)

-- The donor's semantic base can move beyond a still-outstanding admission's
-- Begin without releasing the exact control suffix promised to that applicant.
-- Those bytes belong to the portable Projection archive, not History's suffix.
caseAdmissionControlTailRetention :: Assertion
caseAdmissionControlTailRetention = do
  let (_, record, _, source, observer, beginEntry, suffix) = coveredHistorySuffixFixture 3
      admission = admissionRecordId record
      applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)
      begin = admissionRecordBeginIndex record
      pinned = replaceStartupJoinState (checked (JoinState.retainAdmissionControlTail record (startupJoinState source))) source
      compact = checked (ControlBase.reclaimControlBasePrefix pinned)
      projection = startupOracleProjectionState compact
      cursor = Projection.oracleViewControlIndex (Projection.oracleView projection)
      captured = fromJust (checked (ControlBase.captureJoiningBase compact))
      bytes = ControlBase.encodeJoiningBase captured
      history = History.sourceJoinHistory (checked (History.decodeJoinSourceHistory bytes))
      (imported, effects) = checked (ControlBase.installPortableJoiningBase founderEpoch bytes observer)
      released = replaceStartupJoinState (JoinState.releaseAdmissionControlTail admission applicant (startupJoinState compact)) compact
      reclaimed = checked (ControlBase.reclaimControlBasePrefix released)
      reclaimedProjection = startupOracleProjectionState reclaimed
      oldRead = request (Join.ReadJoinControlHistory begin) reclaimed
  assertEqual "the registered donor obligation is valid before reclamation" (Right ()) (validateHeraldState pinned)
  assertEqual "the semantic covered floor advances beyond Begin" cursor (Projection.projectionCanonicalCoveredThrough projection)
  assertBool "the fixture has a nonempty covered admission tail" (begin < cursor)
  assertEqual "the donor keeps its exact Begin floor" (Just begin) (JoinState.admissionControlTailFloor (startupJoinState compact))
  assertEqual "the compact donor serves every promised control exactly" (Join.JoinControlHistoryReply suffix) (secondOf3 (request (Join.ReadJoinControlHistory begin) compact))
  assertBool "a control read leaves the donor unchanged" (firstOf3 (request (Join.ReadJoinControlHistory begin) compact) == compact)
  assertEqual "an empty tail is available at the current cursor" (Join.JoinControlHistoryReply []) (secondOf3 (request (Join.ReadJoinControlHistory cursor) compact))
  assertEqual "the compact donor satisfies the composed invariant" (Right ()) (validateHeraldState compact)
  assertEqual "History uses the semantic covered floor" cursor (History.joinHistoryControlAnchor history)
  assertEqual "History does not duplicate the covered admission tail" [] (History.joinHistoryControlEntries history)
  assertEqual "the portable Projection preserves the exact covered tail" (Just suffix) (Projection.retainedControlSuffixAfter begin (startupOracleProjectionState imported))
  assertEqual "portable adoption does not transfer the donor's retention obligation" [] (JoinState.admissionControlTails (startupJoinState imported))
  assertBool "the portable History is installed despite its empty suffix" (History.joinHistoryInstalled history imported)
  assertEqual "portable adoption satisfies the Client subset and whole-state invariants" (Right ()) (validateHeraldState imported)
  assertEqual "portable adoption emits no effects" [] (effectBatchMembers effects)
  forM_ suffix $ \entry -> do
    let index = OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
    assertEqual "the donor keeps each promised exact Projection row" (Just entry) (Projection.appliedEntryEvidence index projection)
    assertEqual "the donor does not duplicate rejected rows in Client" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState compact))
    assertEqual "portable adoption does not copy rejected rows into Client" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState imported))
    assertEqual "releasing the obligation permits each unpinned row to be reclaimed" Nothing (Projection.appliedEntryEvidence index reclaimedProjection)
  assertEqual "release removes the admission obligation" Nothing (JoinState.admissionControlTailFloor (startupJoinState reclaimed))
  assertEqual "an independently pinned exact Begin receipt remains" (Just beginEntry) (Projection.appliedEntryEvidence begin reclaimedProjection)
  assertEqual "only independent Client receipt pins remain" (Map.toAscList (Client.oracleClientProtectedPrefixEvidence (startupOracleClientState reclaimed))) (Projection.projectionWitnessAppliedEntries (checked (Projection.validateState reclaimedProjection)))
  assertEqual "a genuine reclaimed gap rejects the old read" Join.JoinRejected (secondOf3 oldRead)
  assertBool "rejection does not restore bytes or otherwise change the donor" (firstOf3 oldRead == reclaimed)
  assertEqual "the current empty tail remains available after release" (Join.JoinControlHistoryReply []) (secondOf3 (request (Join.ReadJoinControlHistory cursor) reclaimed))
  assertEqual "reclamation after release satisfies every owner" (Right ()) (validateHeraldState reclaimed)

caseAdmissionControlTailReplacement :: Assertion
caseAdmissionControlTailReplacement = do
  let (_, record, _, source, observer, _, suffix) = coveredHistorySuffixFixture 3
      admission = admissionRecordId record
      applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)
      begin = admissionRecordBeginIndex record
      (firstIndex, remainingSuffix) = case suffix of
        firstEntry : rest -> (OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue firstEntry), rest)
        [] -> error "admission tail replacement fixture requires a nonempty suffix"
      donor = checked (ControlBase.reclaimControlBasePrefix (replaceStartupJoinState (checked (JoinState.retainAdmissionControlTail record (startupJoinState source))) source))
      donorBytes = ControlBase.encodeJoiningBase (fromJust (checked (ControlBase.captureJoiningBase donor)))
      imported = fst (checked (ControlBase.installPortableJoiningBases [portableDonor source] observer))
      localJoin = checked (JoinState.advanceAdmissionControlTailFloor admission applicant firstIndex (checked (JoinState.retainAdmissionControlTail record (startupJoinState imported))))
      live = replaceStartupJoinState localJoin imported
      replaced = checked (ControlBase.replacePortableJoiningBases [(founderEpoch, donorBytes)] live)
      importedCursor = Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState donor))
      reclaimed = checked (ControlBase.reclaimControlBasePrefix replaced)
      released = checked (ControlBase.reclaimControlBasePrefix (replaceStartupJoinState (JoinState.releaseAdmissionControlTail admission applicant (startupJoinState reclaimed)) reclaimed))
  assertEqual "the local applicant may advance its reconstruction floor" (Right ()) (validateHeraldState live)
  assertEqual "the fixture donor still has the earlier Begin obligation" (Just begin) (JoinState.admissionControlTailFloor (startupJoinState donor))
  assertAdmissionTail "replacement advances the receiver obligation to the actual source cursor" record (Just importedCursor) replaced
  assertEqual "replacement preserves the admission lifetime identity" (map JoinState.admissionControlTailAdmission (JoinState.admissionControlTails localJoin)) (map JoinState.admissionControlTailAdmission (JoinState.admissionControlTails (startupJoinState replaced)))
  assertBool "replacement preserves the observed canonical meaning" (Projection.oracleView (startupOracleProjectionState live) == Projection.oracleView (startupOracleProjectionState replaced))
  assertEqual "replacement with a live local tail satisfies every owner" (Right ()) (validateHeraldState replaced)
  assertEqual "reclamation honors the newly imported local floor" (Just importedCursor) (JoinState.admissionControlTailFloor (startupJoinState reclaimed))
  assertEqual "the first rejected row is no longer owed locally" Nothing (Projection.appliedEntryEvidence firstIndex (startupOracleProjectionState reclaimed))
  assertBool "the fixture has controls beyond the earlier local floor" (not (null remainingSuffix))
  assertEqual "the entire imported prefix can be reclaimed" (Join.JoinControlHistoryReply []) (secondOf3 (request (Join.ReadJoinControlHistory importedCursor) reclaimed))
  assertEqual "the earlier local floor is no longer a reconstruction promise" Join.JoinRejected (secondOf3 (request (Join.ReadJoinControlHistory firstIndex) reclaimed))
  assertEqual "the donor's earlier floor is not imported as a local promise" Join.JoinRejected (secondOf3 (request (Join.ReadJoinControlHistory begin) reclaimed))
  assertEqual "the reclaimed replacement satisfies every owner" (Right ()) (validateHeraldState reclaimed)
  assertEqual "releasing the final local promise reclaims the remaining unpinned tail" [] (Projection.projectionWitnessAppliedEntries (checked (Projection.validateState (startupOracleProjectionState released))))
  assertEqual "the fully released applicant satisfies every owner" (Right ()) (validateHeraldState released)

-- Compare the admitted-owner fast path against independently checked owner
-- transitions. Donors retain a real Begin receipt; imported applicants do not.
-- Only applicants may advance the optional admission tail above Begin.
propPairedControlReclamation :: Word8 -> Word8 -> Bool -> Bool -> Property
propPairedControlReclamation sample floorSample donorLocal retainTail =
  let count = fromIntegral sample `mod` 6
      (_, record, _, source, observer, _, _) = coveredHistorySuffixFixture count
      admission = admissionRecordId record
      applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)
      begin = admissionRecordBeginIndex record
      offset = if donorLocal then 0 else fromIntegral floorSample `mod` (count + 1)
      floorIndex = controlIndex (controlIndexWord64 begin + fromIntegral offset)
      initial = if donorLocal then source else fst (checked (ControlBase.installPortableJoiningBases [portableDonor source] observer))
      join =
        if retainTail
          then checked (JoinState.advanceAdmissionControlTailFloor admission applicant floorIndex (checked (JoinState.retainAdmissionControlTail record (startupJoinState initial))))
          else JoinState.releaseAdmissionControlTail admission applicant (startupJoinState initial)
      prepared = replaceStartupJoinState join initial
      actual = checked (ControlBase.reclaimControlBasePrefix prepared)
      expected = checkedOwnerReclamation prepared
      released = replaceStartupJoinState (JoinState.releaseAdmissionControlTail admission applicant (startupJoinState actual)) actual
      reclaimed = checked (ControlBase.reclaimControlBasePrefix released)
      actualProjection = startupOracleProjectionState actual
      finalProjection = startupOracleProjectionState reclaimed
      cursor = Projection.oracleViewControlIndex (Projection.oracleView actualProjection)
   in conjoin
        [ validateHeraldState prepared === Right (),
          (actual == expected) === True,
          validateHeraldState actual === Right (),
          (checked (ControlBase.reclaimControlBasePrefix actual) == actual) === True,
          Projection.projectionCanonicalCoveredThrough actualProjection === cursor,
          Client.oracleClientCanonicalCoveredThrough (startupOracleClientState actual) === cursor,
          not (Map.null (Client.oracleClientProtectedPrefixEvidence (startupOracleClientState actual))) === donorLocal,
          (reclaimed == checkedOwnerReclamation released) === True,
          Projection.oracleViewControlIndex (Projection.oracleView finalProjection) === cursor,
          Projection.projectionWitnessAppliedEntries (checked (Projection.validateState finalProjection)) === Map.toAscList (Client.oracleClientProtectedPrefixEvidence (startupOracleClientState reclaimed)),
          JoinState.admissionControlTails (startupJoinState reclaimed) === [],
          validateHeraldState reclaimed === Right (),
          fmap (const ()) (ControlBase.reclaimControlBasePrefix (freezeStartupSemanticControl prepared)) === Left ControlBase.ControlBaseSourceUnavailable
        ]
  where
    checkedOwnerReclamation state =
      let projectionBase = checked (Projection.advanceReplayBase (startupOracleProjectionState state))
          client = checked (Client.reclaimOracleClientCanonicalPrefix (Projection.replayBaseControlIndex projectionBase) (startupOracleClientState state))
          projection = checked (Projection.reclaimAppliedPrefixThroughBase (Map.keysSet (Client.oracleClientProtectedPrefixEvidence client)) (JoinState.admissionControlTailFloor (startupJoinState state)) projectionBase)
       in replaceStartupOracleClientState client (replaceStartupOracleProjectionState projection state)

propAdmissionControlTailCancellation :: Word8 -> Bool -> Property
propAdmissionControlTailCancellation sample importFirst =
  let (oracle, record, _, source, observer, beginEntry, suffix) = coveredHistorySuffixFixture (fromIntegral sample `mod` 6)
      sourcePrefix = Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState source))
      begin = admissionRecordBeginIndex record
      observed = firstOf3 (request (Join.InstallJoinControlHistory (beginEntry : suffix)) observer)
      prepared = if importFirst then firstOf3 (request (Join.InstallJoinHistory (sourceBytes source)) observed) else observed
      (_, cancellation) = submit (CancelHeraldAdmission (admissionRecordId record)) oracle
      donorCancelled = deliverEntry cancellation source
      (cancelled, reply, _) = request (Join.InstallJoinControlHistory [cancellation]) prepared
      (duplicate, duplicateReply, _) = request (Join.InstallJoinControlHistory [cancellation]) cancelled
   in conjoin
        [ admissionTailFloor record source === Just begin,
          admissionTailFloor record observed === Just begin,
          admissionTailFloor record prepared === Just (if importFirst then sourcePrefix else begin),
          reply === Join.JoinHistoryInstalled,
          JoinState.admissionControlTails (startupJoinState donorCancelled) === [],
          JoinState.admissionControlTails (startupJoinState cancelled) === [],
          duplicateReply === Join.JoinHistoryInstalled,
          (duplicate == cancelled) === True,
          validateHeraldState donorCancelled === Right (),
          validateHeraldState cancelled === Right (),
          serving cancelled === False
        ]

propCoveredHistorySuffix :: Word8 -> Property
propCoveredHistorySuffix sample =
  let count = fromIntegral sample `mod` 6
      (_, _, source, fullSource, observer, _, suffix) = coveredHistorySuffixFixture count
      history = History.sourceJoinHistory (captureSource source)
      fullHistory = History.sourceJoinHistory (captureSource fullSource)
      base = fromJust (checked (ControlBase.captureJoiningBase source))
      fullBase = fromJust (checked (ControlBase.captureJoiningBase fullSource))
      imported = fst (checked (ControlBase.installPortableJoiningBase founderEpoch (ControlBase.encodeJoiningBase base) observer))
      fullImported = fst (checked (ControlBase.installJoiningBase fullBase observer))
      (settled, _, complete) = checked (JoinReplay.replayJoinHistory history imported)
      (repeated, repeatedEffects, repeatedComplete) = checked (JoinReplay.replayJoinHistory history settled)
   in conjoin
        [ History.joinHistoryControlAnchor history === History.joinHistoryControlPrefix history,
          History.joinHistoryControlEntries history === [],
          Projection.retainedControlSuffixAfter (controlIndex 1) (startupOracleProjectionState source) === Just suffix,
          History.joinHistoryControlPrefix history === controlIndex (1 + fromIntegral count),
          (Projection.oracleView (startupOracleProjectionState imported) == Projection.oracleView (startupOracleProjectionState fullImported)) === True,
          Graph.graphStructuralVertexProjections (startupGraphState imported) === Graph.graphStructuralVertexProjections (startupGraphState fullImported),
          Graph.graphStructuralEdgeProjections (startupGraphState imported) === Graph.graphStructuralEdgeProjections (startupGraphState fullImported),
          History.joinHistoryInstalled fullHistory imported === True,
          validateHeraldState imported === Right (),
          serving imported === False,
          complete === True,
          (settled == imported) === True,
          repeatedComplete === True,
          (repeated == settled) === True,
          effectBatchMembers repeatedEffects === []
        ]

caseCoveredHistoryActivation :: Assertion
caseCoveredHistoryActivation = do
  let (oracle0, record, founder0, _, observer0, _, _) = coveredHistoryFixture True
      base = fromJust (checked (ControlBase.captureJoiningBase founder0))
      observer1 = fst (checked (ControlBase.installPortableJoiningBase founderEpoch (ControlBase.encodeJoiningBase base) observer0))
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      afterSeal = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      afterAccept@(_, founder2, observer3) = advance (AcceptHeraldJoinSeal (admissionRecordId record) (admissionRecordAttempt record) (joinSealDigest seal)) afterSeal
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("compact base readiness failed: " <> show reply)
      afterOld = advance (HeraldJoinBaseReady (ready founder2)) afterAccept
      afterNew@(_, _, beforeActivation) = advance (HeraldJoinReady (ready observer3)) afterOld
      (_, founderFinal, observerFinal) = advance (ActivateHerald (admissionRecordId record)) afterNew
  assertBool "compact base stays restricted through readiness" (not (serving beforeActivation))
  assertBool "ordinary canonical suffix activates the compact base" (serving observerFinal)
  assertEqual "compact base activation preserves all receiver invariants" (Right ()) (validateHeraldState observerFinal)
  assertEqual "compact base activation preserves all donor invariants" (Right ()) (validateHeraldState founderFinal)
  assertEqual "activation releases compact imported transfer envelopes" [] (JoinState.installedHistories (startupJoinState observerFinal))

-- Preserve the real captured graph, Store, placement and alignment frames; only
-- rewrite the control subframe under test. The identity rewrite checks this
-- test-side grammar against the production encoder before testing negatives.
rewriteHistoryControls :: (Word64 -> [BS.ByteString] -> (Word64, [BS.ByteString])) -> BS.ByteString -> BS.ByteString
rewriteHistoryControls rewrite bytes = checked (Serialize.runGet rewriteFrame bytes)
  where
    rewriteFrame = do
      prefix <- Serialize.get :: Serialize.Get (BS.ByteString, BS.ByteString, BS.ByteString, Word64, BS.ByteString, Word64, BS.ByteString, Word64, Word64)
      anchor <- Serialize.get
      entries <- Serialize.get
      rest <- Serialize.remaining >>= Serialize.getBytes
      let (nextAnchor, nextEntries) = rewrite anchor entries
      pure $ Serialize.runPut $ do
        Serialize.put prefix
        Serialize.put nextAnchor
        Serialize.put nextEntries
        Serialize.putByteString rest

-- Mutate only the occurrence list and source stamp of a real nonempty History.
-- The reference deliberately retains the old exhaustive membership algorithm.
propHistoryOccurrenceCoverage :: Property
propHistoryOccurrenceCoverage = ioProperty $ do
  (_, _, history) <- structuralHistoryFixture
  let bytes = History.encodeJoinHistory history
      source = History.joinHistorySource history
      originals = History.joinHistoryOccurrences history
      vector = topologyFrontierStructuralVersionVector (topologyCutFrontier (History.joinHistoryTopologyCut history))
      maximumStamp = maybe 0 (maybe 0 structuralSequenceWord64 . structuralPrefixSequence) (structuralVersionVectorComponent source vector)
  pure $ forAll (choose (0, maximumStamp)) $ \stamped ->
    forAll (sublistOf originals) $ \subset ->
      forAll (choose (0, 2 :: Int)) $ \order ->
        let occurrences = case order of
              0 -> subset
              1 -> reverse subset
              _ -> subset <> take 1 subset
            identifiers = map Terminal.terminalStructuralOccurrenceId occurrences
            expected
              | identifiers /= Set.toAscList (Set.fromList identifiers) = Left History.JoinHistoryMalformed
              | not (all (\n -> structuralOccurrenceId source (checked (mkStructuralSequence n)) `elem` identifiers) [1 .. stamped]) = Left History.JoinHistorySourceMaterialMissing
              | otherwise = Right ()
            changed = rewriteHistoryOccurrences (\_ _ -> (stamped, map Terminal.terminalStructuralOccurrenceCanonicalBytes occurrences)) bytes
         in conjoin
              [ (not (null originals) && maximumStamp > 0) === True,
                rewriteHistoryOccurrences (,) bytes === bytes,
                (() <$ History.decodeJoinHistory changed) === expected
              ]

rewriteHistoryOccurrences :: (Word64 -> [BS.ByteString] -> (Word64, [BS.ByteString])) -> BS.ByteString -> BS.ByteString
rewriteHistoryOccurrences rewrite bytes = checked (Serialize.runGet rewriteFrame bytes)
  where
    rewriteFrame = do
      (domain, system, admission, attempt, source, control, cut, stamped, input) <- Serialize.get :: Serialize.Get (BS.ByteString, BS.ByteString, BS.ByteString, Word64, BS.ByteString, Word64, BS.ByteString, Word64, Word64)
      (anchor, entries, cuts, terminals, occurrences) <- Serialize.get :: Serialize.Get (Word64, [BS.ByteString], [(Maybe BS.ByteString, BS.ByteString)], [BS.ByteString], [BS.ByteString])
      rest <- Serialize.remaining >>= Serialize.getBytes
      let (nextStamp, nextOccurrences) = rewrite stamped occurrences
      pure $ Serialize.runPut $ do
        Serialize.put (domain, system, admission, attempt, source, control, cut, nextStamp, input)
        Serialize.put (anchor, entries, cuts, terminals, nextOccurrences)
        Serialize.putByteString rest

caseCoveredHistoryCodec :: Assertion
caseCoveredHistoryCodec = do
  let (_, _, source, _, observer, beginEntry, suffix) = coveredHistoryFixture True
      history = History.sourceJoinHistory (captureSource source)
      -- Recreate the valid earlier covered anchor from the actual emitted
      -- suffix so every malformed case still tests a nonempty interval.
      bytes = rewriteHistoryControls (\_ _ -> (1, map canonicalAppliedOracleEntryBytes suffix)) (History.encodeJoinHistory history)
      rewrite = (`rewriteHistoryControls` bytes)
      reject message change = assertEqual message (Left History.JoinHistoryCanonicalControlGap) (History.decodeJoinHistory (rewrite change))
  assertEqual "test-side control framing preserves the original exact bytes" bytes (rewrite (,))
  reject "coverage cannot exceed the History control prefix" (\_ entries -> (controlIndexWord64 (History.joinHistoryControlPrefix history) + 1, entries))
  reject "coverage below the prefix requires the complete suffix" (\anchor _ -> (anchor, []))
  reject "duplicate suffix coordinates are rejected" (\anchor entries -> (anchor, entries <> entries))
  reject "a covered exact pin cannot masquerade as a suffix row" (\anchor entries -> (anchor, canonicalAppliedOracleEntryBytes beginEntry : entries))
  reject "a suffix cannot omit its first coordinate" (\_ entries -> (0, entries))
  let altered = rewrite (\_ entries -> (0, canonicalAppliedOracleEntryBytes beginEntry : entries))
      base = fromJust (checked (ControlBase.captureJoiningBase source))
      sourceEpoch = History.joinHistorySource history
  assertBool "a full prefix is an independently valid History shape" (not (isLeft (History.decodeJoinHistory altered)))
  (domain, projection, registry, progress, carriers, _) <- either assertFailure pure (Serialize.decode (ControlBase.encodeJoiningBase base) :: Either String RawPortableJoiningBase)
  assertBool
    "the aggregate rejects independently valid History whose anchor disagrees with Projection"
    (isLeft (ControlBase.installPortableJoiningBase sourceEpoch (Serialize.encode (domain, projection, registry, progress, carriers, altered)) observer))

caseJoinControlTail :: Assertion
caseJoinControlTail = do
  let (oracle, record, source, _, observer, beginEntry, suffix) = coveredHistoryFixture True
      (_, cancellation) = submit (CancelHeraldAdmission (admissionRecordId record)) oracle
      cancelledSource = deliverEntry cancellation source
      allEntries = beginEntry : suffix
      current = OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue cancellation)
      readTail after state = case request (Join.ReadJoinControlHistory after) state of
        (unchanged, Join.JoinControlHistoryReply entries, _) -> do
          assertBool "tail read preserves every owner" (unchanged == state)
          pure entries
        (_, other, _) -> assertFailure ("unexpected tail reply: " <> show other)
      applied state = case secondOf3 (request Join.ReadJoinStatus state) of
        Join.JoinStatusReply status -> Join.joinStatusAppliedControl status
        other -> error ("missing Join status: " <> show other)
      (afterBegin, firstReply, _) = request (Join.InstallJoinControlHistory [beginEntry]) observer
  assertEqual "initial emitted Begin is admitted" Join.JoinHistoryInstalled firstReply
  first <- readTail (applied afterBegin) source
  assertEqual "the donor offers only controls after the applied Begin" suffix first
  let (afterSuffix, suffixReply, _) = request (Join.InstallJoinControlHistory first) afterBegin
  assertEqual "the exact retained suffix is admitted" Join.JoinHistoryInstalled suffixReply
  assertEqual "reported cursor is exactly the semantic owner" (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState afterSuffix))) (applied afterSuffix)
  empty <- readTail (applied afterSuffix) source
  assertEqual "repeated synchronization has no old entries to serialize or replay" [] empty
  let (repeated, repeatedReply, repeatedEffects) = request (Join.InstallJoinControlHistory empty) afterSuffix
  assertEqual "empty suffix is acknowledged" Join.JoinHistoryInstalled repeatedReply
  assertBool "empty suffix changes no owner" (repeated == afterSuffix)
  assertEqual "empty synchronization produces only its reply" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinHistoryInstalled)] (effectBatchMembers repeatedEffects)
  forM_ [0 .. controlIndexWord64 (applied afterSuffix)] $ \offset -> do
    entries <- readTail (controlIndex offset) source
    assertEqual "every physically retained cursor returns its exact contiguous suffix" (drop (fromIntegral offset) allEntries) entries
  let (afterCancel, secondReply, _) = request (Join.InstallJoinControlHistory [cancellation]) afterSuffix
      (rejectedState, rejectedReply, _) = request (Join.ReadJoinControlHistory (controlIndex (controlIndexWord64 current + 1))) cancelledSource
  assertEqual "the original emitted cancellation remains admissible" Join.JoinHistoryInstalled secondReply
  assertEqual "cancellation releases the donor replay promise" Join.JoinRejected (secondOf3 (request (Join.ReadJoinControlHistory (admissionRecordBeginIndex record)) cancelledSource))
  assertEqual "the current cursor still has an empty suffix after release" (Join.JoinControlHistoryReply []) (secondOf3 (request (Join.ReadJoinControlHistory current) cancelledSource))
  assertEqual "a source cannot serve a cursor beyond its applied history" Join.JoinRejected rejectedReply
  assertBool "ahead read cannot mutate its source" (rejectedState == cancelledSource)
  assertEqual "suffix-installed observer retains valid owner composition" (Right ()) (validateHeraldState afterCancel)

caseJoinStatusCursors :: Assertion
caseJoinStatusCursors = do
  let (oracle, _, _, founder, _) = capturedFixture
      envelope = oracleEnvelope (oracleClientRequestId founderEpoch 900001) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
      rejected = case checked (stepOracle envelope oracle) of
        (_, OracleCommitted receipt, effects) | oracleReceiptResult receipt /= OracleAccepted -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> canonicalizeAppliedOracleEntry entry
          _ -> error "missing rejected control-lane entry"
        _ -> error "unknown voter cancellation must be rejected"
      binding = fromJust (Client.oracleClientCurrentBinding (startupOracleClientState founder))
      frozen = fst (checked (applyFencedOracleIngress (OracleEntriesReceived binding (rejected :| [])) (freezeStartupSemanticControl founder)))
      status = case secondOf3 (request Join.ReadJoinStatus frozen) of
        Join.JoinStatusReply value -> value
        other -> error (show other)
      impossible = status {Join.joinStatusAppliedControl = controlIndex 3}
      source = deliverEntry rejected founder
  assertEqual "the frozen semantic owner remains independently valid" (Right ()) (validateHeraldState frozen)
  assertEqual "control-only ingress advances the observed cursor" (controlIndex 2) (Join.joinStatusControl status)
  assertEqual "control-only ingress leaves the applied cursor at the semantic owner" (controlIndex 1) (Join.joinStatusAppliedControl status)
  assertEqual "the reported applied cursor requests the semantically unapplied entry" (Join.JoinControlHistoryReply [rejected]) (secondOf3 (request (Join.ReadJoinControlHistory (Join.joinStatusAppliedControl status)) source))
  assertEqual "wire preserves actual observed and applied cursors separately" (Right (Join.JoinStatusReply status)) (Join.decodeJoinReply (Join.encodeJoinReply (Join.JoinStatusReply status)))
  assertBool "malformed status cannot advertise application ahead of observation" (isLeft (Join.decodeJoinReply (Join.encodeJoinReply (Join.JoinStatusReply impossible))))

caseStaggeredSeal :: Assertion
caseStaggeredSeal = do
  (record, first, second, forked) <- staggeredFixture
  let seal = checked (Join.prepareJoinSeal record [first, second])
  assertBool "source snapshots retain distinct real cut identities" (History.joinHistoryTopologyCut (History.sourceJoinHistory first) /= History.joinHistoryTopologyCut (History.sourceJoinHistory second))
  assertEqual "seal chooses supplied descendant K2" (History.joinHistoryTopologyCut (History.sourceJoinHistory second)) (joinSealTopologyCut seal)
  assertEqual "seal preserves every source's original immutable cut digest" [(History.joinHistorySource (History.sourceJoinHistory source), History.joinSourceMemberCut source) | source <- [first, second]] (joinSealMemberCuts seal)
  assertBool "same-generation sibling certificates are incomparable" (isLeft (Join.prepareJoinSeal record [first, forked]))

portableDonor :: HeraldState -> (HeraldEpoch, BS.ByteString)
portableDonor state = (History.joinHistorySource (History.sourceJoinHistory (captureSource state)), sourceBytes state)

-- A deliberately withdrawn test obligation creates an individually valid
-- compact frame with a genuinely unavailable cross-source or local replay tail.
reclaimWithoutAdmissionTail :: HeraldAdmissionRecord -> HeraldState -> HeraldState
reclaimWithoutAdmissionTail record state =
  checked (ControlBase.reclaimControlBasePrefix (replaceStartupJoinState released state))
  where
    released = JoinState.releaseAdmissionControlTail (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) (startupJoinState state)

retainedTailAfter :: HeraldState -> HeraldState -> [CanonicalAppliedOracleEntry]
retainedTailAfter older newer =
  fromJust (Projection.retainedControlSuffixAfter (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState older))) (startupOracleProjectionState newer))

-- In the reversed case K1's owner sees later controls through B3 while its
-- certified topology stays K1 and the newer topology donor remains at B2.
portableJoiningSourcesFixture :: Bool -> IO (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState, HeraldState)
portableJoiningSourcesFixture reversePrefixes = do
  (oracle2, record, first, second, _, observer) <- staggeredOwnersFixture
  let laterControls = retainedTailAfter first second
      extraEnvelope = oracleEnvelope (oracleClientRequestId founderEpoch 900020) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
      (oracle3, extraEntry) = case checked (stepOracle extraEnvelope oracle2) of
        (advanced, OracleCommitted receipt, effects) | oracleReceiptResult receipt /= OracleAccepted -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> (advanced, canonicalizeAppliedOracleEntry entry)
          other -> error ("reversed donor prefix expected one canonical rejection: " <> show other)
        (_, outcome, _) -> error ("reversed donor prefix expected a rejected request: " <> show outcome)
      older = if reversePrefixes then foldl (flip deliverEntry) first (laterControls <> [extraEntry]) else first
      compactNewer = checked (ControlBase.reclaimControlBasePrefix second)
  assertEqual "later rejected controls leave the older donor at its actual prior K" (Progress.structuralLastInstalledCutId (startupStructuralProgressState first)) (Progress.structuralLastInstalledCutId (startupStructuralProgressState older))
  pure (if reversePrefixes then oracle3 else oracle2, record, older, compactNewer, observer)

-- A partial source set must change only the immutable envelope ledger. In
-- particular it must not replay controls, prepare cuts, or import provenance.
assertPartialJoiningImport :: HeraldState -> HeraldState -> Assertion
assertPartialJoiningImport original partial = do
  assertBool "partial source buffering changes no owner outside Join" (replaceStartupJoinState (startupJoinState original) partial == original)
  assertEqual "partial source buffering imports no source observations" (JoinState.importedSystemViewObservations (startupJoinState original)) (JoinState.importedSystemViewObservations (startupJoinState partial))
  assertEqual "partial source buffering cannot advance a local reconstruction promise" (JoinState.admissionControlTails (startupJoinState original)) (JoinState.admissionControlTails (startupJoinState partial))
  assertEqual "partial source buffering keeps all owners coherent" (Right ()) (validateHeraldState partial)

assertOnlyJoinReply :: Join.JoinReply -> EffectBatch -> Assertion
assertOnlyJoinReply reply effects = assertEqual "onboarding emits only its protocol reply" [SendJoinReply 77 (Join.encodeJoinReply reply)] (effectBatchMembers effects)

caseLiveJoiningSources :: Assertion
caseLiveJoiningSources = do
  (_, record, first, second, observer) <- portableJoiningSourcesFixture True
  let donors = [portableDonor first, portableDonor second]
      histories = map (History.sourceJoinHistory . captureSource) [first, second]
      expected = checked (ControlBase.replacePortableJoiningBases donors observer)
      install state (_, bytes) = request (Join.InstallJoinHistory bytes) state
  completed <- forM [donors, reverse donors] $ \order -> case order of
    [firstDonor, secondDonor] -> do
      let (partial, partialReply, partialEffects) = install observer firstDonor
          (repeatedPartial, repeatedReply, repeatedEffects) = install partial firstDonor
          (imported, completeReply, completeEffects) = install partial secondDonor
          key = (admissionRecordId record, admissionRecordAttempt record, fst firstDonor)
      assertEqual "one of two actual predecessor sources remains pending" Join.JoinRetry partialReply
      assertPartialJoiningImport observer partial
      assertEqual "partial import retains exactly the received immutable bytes" [(key, snd firstDonor)] (JoinState.installedHistories (startupJoinState partial))
      assertOnlyJoinReply partialReply partialEffects
      assertEqual "an exact partial duplicate remains pending" Join.JoinRetry repeatedReply
      assertBool "an exact partial duplicate changes no owner" (repeatedPartial == partial)
      assertOnlyJoinReply repeatedReply repeatedEffects
      assertEqual "the complete source set is acknowledged" Join.JoinHistoryInstalled completeReply
      assertBool "the complete live adoption equals the independently prepared replacement" (imported == expected)
      assertBool "both source histories are covered by the final state" (all (`History.joinHistoryInstalled` imported) histories)
      assertBool "source adoption cannot make the applicant serve" (not (serving imported))
      assertEqual "complete source adoption preserves the whole-owner invariant" (Right ()) (validateHeraldState imported)
      assertAdmissionTail "only the complete checked source set establishes a reconstruction floor" record (Just (maximum (map History.joinHistoryControlPrefix histories))) imported
      assertOnlyJoinReply completeReply completeEffects
      forM_ donors $ \donor -> do
        let (repeated, reply, effects) = install imported donor
        assertEqual "an exact completed duplicate is acknowledged" Join.JoinHistoryInstalled reply
        assertBool "an exact completed duplicate does not rebuild any owner" (repeated == imported)
        assertOnlyJoinReply reply effects
      pure imported
    _ -> error "two-source test fixture lost a donor"
  assertBool "both arrival orders produce exactly the same complete owner product" (all (== expected) completed)
  let changedState = reclaimWithoutAdmissionTail record first
      changedDonor = portableDonor changedState
      changedHistory = History.sourceJoinHistory (checked (History.decodeJoinSourceHistory (snd changedDonor)))
      (partial, partialReply, _) = install observer (portableDonor first)
      (conflicted, conflictReply, conflictEffects) = install partial changedDonor
      (gapPartial, gapPartialReply, _) = install observer changedDonor
      (gapRejected, gapReply, gapEffects) = install gapPartial (portableDonor second)
  assertBool "the conflicting source payload is a distinct valid compact capture" (fst changedDonor == fst (portableDonor first) && snd changedDonor /= snd (portableDonor first))
  assertEqual "the first original payload remains pending" Join.JoinRetry partialReply
  assertEqual "a changed payload cannot replace an immutable source receipt" Join.JoinRequestConflict conflictReply
  assertBool "a source conflict preserves every pending owner and byte" (conflicted == partial)
  assertOnlyJoinReply conflictReply conflictEffects
  assertBool "the individually valid compact source lacks the complete set's required later controls" (History.joinHistoryControlAnchor changedHistory > History.joinHistoryControlPrefix (History.sourceJoinHistory (captureSource second)) && null (History.joinHistoryControlEntries changedHistory) && Projection.retainedControlSuffixAfter (History.joinHistoryControlPrefix (History.sourceJoinHistory (captureSource second))) (startupOracleProjectionState changedState) == Nothing)
  assertEqual "a valid individual source can wait before cross-source preparation" Join.JoinRetry gapPartialReply
  assertPartialJoiningImport observer gapPartial
  assertEqual "an incoherent complete set cannot bridge the absent control suffix" Join.JoinRejected gapReply
  assertBool "failed complete adoption leaves the prior partial state exactly intact" (gapRejected == gapPartial)
  assertOnlyJoinReply gapReply gapEffects

caseLiveJoiningDuplicateAfterReclamation :: Assertion
caseLiveJoiningDuplicateAfterReclamation = do
  let (oracle, record, history, founder, observer) = capturedFixture
      donors = [portableDonor founder]
      (imported, importReply, _) = request (Join.InstallJoinHistory (sourceBytes founder)) observer
      seal = checked (Join.prepareJoinSeal record [captureSource founder])
      (_, sealEntry) = submit (SealHeraldAdmission seal) oracle
      (sealed, sealReply, _) = request (Join.InstallJoinControlHistory [sealEntry]) imported
      compact = checked (ControlBase.reclaimControlBasePrefix sealed)
      (retried, retryReply, retryEffects) = request (Join.InstallJoinHistory (sourceBytes founder)) compact
  assertEqual "initial source adoption completes" Join.JoinHistoryInstalled importReply
  assertEqual "the observer applies the real later seal" Join.JoinHistoryInstalled sealReply
  assertBool "reclamation covers controls beyond the original source" (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState compact) > History.joinHistoryControlPrefix history)
  assertAdmissionTail "source adoption records its original control cursor" record (Just (controlIndex 1)) imported
  assertAdmissionTail "later local Seal does not advance the reconstruction origin" record (Just (controlIndex 1)) sealed
  assertEqual "the later Seal remains exact below the newer covered floor" (Just sealEntry) (Projection.appliedEntryEvidence (controlIndex 2) (startupOracleProjectionState compact))
  let rebuilt = checked (ControlBase.replacePortableJoiningBases donors compact)
      rebuiltAgain = checked (ControlBase.replacePortableJoiningBases donors (checked (ControlBase.reclaimControlBasePrefix rebuilt)))
  assertEqual "replacement replays the later Seal to the same current cursor" (controlIndex 2) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState rebuilt)))
  assertAdmissionTail "replacement keeps the imported cursor rather than its final replay cursor" record (Just (controlIndex 1)) rebuilt
  assertAdmissionTail "another identical reconstruction retains the same replay origin" record (Just (controlIndex 1)) rebuiltAgain
  assertEqual "a reconstructed observer preserves the exact canonical seal" (admissionRecordSeal =<< Projection.oracleViewHeraldAdmission (admissionRecordId record) (Projection.oracleView (startupOracleProjectionState compact))) (admissionRecordSeal =<< Projection.oracleViewHeraldAdmission (admissionRecordId record) (Projection.oracleView (startupOracleProjectionState rebuiltAgain)))
  assertEqual "reconstructed owner composition is valid" (Right ()) (validateHeraldState rebuiltAgain)
  assertEqual "completed replay is still acknowledged without rebuilding" Join.JoinHistoryInstalled retryReply
  assertBool "the completed duplicate preserves every current owner" (retried == compact)
  assertOnlyJoinReply retryReply retryEffects
  case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) retried) of
    Join.JoinReadyReply _ -> pure ()
    reply -> assertFailure ("completed retry lost actual sealed readiness: " <> show reply)
  assertEqual "the reclaimed duplicate preserves the full invariant" (Right ()) (validateHeraldState retried)

caseLiveJoiningAttemptPruning :: Assertion
caseLiveJoiningAttemptPruning = do
  let genesis = Fixtures.fixtureCheckedGenesis
      secondGenesis = checked (checkHeraldGenesis (Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember))
      selected = checked (checkInitialBootstraps genesis (PrimordialProcessManifest Fixtures.fixtureLocalBootstrapIds))
      initializeAt sourceGenesis = fst (checked (initialHerald (monotonicInstant 1) sourceGenesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps genesis selected))
      first0 = initializeAt genesis
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews first0) (startupStructuralProgressState first0))
      (oracle1, beginEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record1 = fromJust (oraclePendingHeraldAdmission oracle1)
      observer = initializeAt (checked (checkJoiningHeraldGenesis genesis record1))
      endedProcess = case Genesis.checkedInitialBootstraps selected of
        first : _ -> appliedProcessEpochId first
        [] -> error "partial attempt fixture requires a real configured process"
  first1 <- settleControlBaseCut (deliverEntry beginEntry first0)
  let oldSource = captureSource first1
      oldBytes = History.encodeJoinSourceHistory oldSource
      (second1, oldReply, _) = request (Join.InstallJoinHistory oldBytes) (deliverEntry beginEntry (initializeAt secondGenesis))
      (oracle2, endEntry) = submitCommand (checked (endProcessEpochCommand endedProcess ExplicitAdministrativeEnd)) oracle1
      record2 = fromJust (oraclePendingHeraldAdmission oracle2)
  assertEqual "the second active source learns the first actual certified cut" Join.JoinHistoryInstalled oldReply
  first2 <- settleControlBaseCut (deliverEntry endEntry first1)
  let (second2, newReply, _) = request (Join.InstallJoinHistory (sourceBytes first2)) (deliverEntry endEntry second1)
      firstDonor = portableDonor (checked (ControlBase.reclaimControlBasePrefix first2))
      secondDonor = portableDonor (checked (ControlBase.reclaimControlBasePrefix second2))
      install state (_, bytes) = request (Join.InstallJoinHistory bytes) state
      (oldPartial, oldPartialReply, _) = request (Join.InstallJoinHistory oldBytes) observer
      (newPartial, newPartialReply, newPartialEffects) = install oldPartial secondDonor
      (staleRetry, staleReply, staleEffects) = request (Join.InstallJoinHistory oldBytes) newPartial
      (newRetry, newRetryReply, _) = install newPartial secondDonor
      (complete, completeReply, completeEffects) = install newPartial firstDonor
      retained = JoinState.installedHistories . startupJoinState
  assertEqual "the real End invalidates the pending attempt" (admissionRecordAttempt record1 + 1) (admissionRecordAttempt record2)
  assertEqual "the second source learns the actual replacement cut" Join.JoinHistoryInstalled newReply
  assertEqual "the old source alone waits" Join.JoinRetry oldPartialReply
  assertPartialJoiningImport observer oldPartial
  assertEqual "a new-attempt source alone still waits" Join.JoinRetry newPartialReply
  assertPartialJoiningImport observer newPartial
  assertEqual "the new source retires every old-attempt envelope immediately" [((admissionRecordId record2, admissionRecordAttempt record2, fst secondDonor), snd secondDonor)] (retained newPartial)
  assertOnlyJoinReply newPartialReply newPartialEffects
  assertEqual "a stale arrival cannot reopen the retired buffered attempt" (Join.JoinTransferRetired (admissionRecordId record1) (admissionRecordAttempt record1)) staleReply
  assertBool "a stale source changes no owner" (staleRetry == newPartial)
  assertOnlyJoinReply staleReply staleEffects
  assertEqual "an exact new-attempt partial retry remains pending" Join.JoinRetry newRetryReply
  assertBool "the partial retry changes no owner" (newRetry == newPartial)
  assertEqual "the complete current attempt imports" Join.JoinHistoryInstalled completeReply
  assertEqual "first import retains original startup provenance" (Just record1) (Genesis.checkedStartupAdmission (startupGenesis complete))
  assertEqual "first import adopts the checked newer canonical attempt" (Just record2) (Projection.oracleViewHeraldAdmission (admissionRecordId record2) (Projection.oracleView (startupOracleProjectionState complete)))
  assertAdmissionTail "new-attempt source completion establishes its actual import cursor" record2 (Just (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState complete)))) complete
  assertBool "first import restores real process termination" (not (Projection.oracleViewProcessIsLive endedProcess (Projection.oracleView (startupOracleProjectionState complete))))
  assertBool "no old attempt survives completion" (all (\((_, attempt, _), _) -> attempt == admissionRecordAttempt record2) (retained complete))
  assertOnlyJoinReply completeReply completeEffects
  assertEqual "replacement after real invalidation preserves the whole invariant" (Right ()) (validateHeraldState complete)

caseLiveSecondAdmission :: Assertion
caseLiveSecondAdmission = do
  let (oracle0, record1, _, founder0, observer0) = capturedFixture
      observer1 = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder0)) observer0)
      seal = checked (Join.prepareJoinSeal record1 [captureSource founder0])
      afterSeal = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      afterAccept@(_, founderAccepted, observerAccepted) = advance (AcceptHeraldJoinSeal (admissionRecordId record1) (admissionRecordAttempt record1) (joinSealDigest seal)) afterSeal
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record1)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("second admission fixture readiness failed: " <> show reply)
      afterOld = advance (HeraldJoinBaseReady (ready founderAccepted)) afterAccept
      afterNew = advance (HeraldJoinReady (ready observerAccepted)) afterOld
      (oracleActive, founderActive, secondActive) = advance (ActivateHerald (admissionRecordId record1)) afterNew
      nextManifest = heraldAdmissionManifest (Genesis.checkedSystemId founderGenesis) (checked (mkHeraldId (BS.replicate 32 0xed))) (checked (mkHeraldEpoch (BS.replicate 32 0xf1)))
      nextAnchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews founderActive) (startupStructuralProgressState founderActive))
      (oracleNext, nextEntry) = submit (BeginHeraldAdmission nextManifest nextAnchor) oracleActive
      record2 = fromJust (oraclePendingHeraldAdmission oracleNext)
      genesis = checked (checkJoiningHeraldGenesis founderGenesis record2)
      observer = fst (checked (initialHerald (monotonicInstant 1) genesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
  first <- settleControlBaseCut (deliverEntry nextEntry founderActive)
  let (second, sourceReply, _) = request (Join.InstallJoinHistory (sourceBytes first)) (deliverEntry nextEntry secondActive)
      firstDonor = portableDonor (checked (ControlBase.reclaimControlBasePrefix first))
      secondDonor = portableDonor (checked (ControlBase.reclaimControlBasePrefix second))
      (partial, partialReply, _) = request (Join.InstallJoinHistory (snd firstDonor)) observer
      (complete, completeReply, completeEffects) = request (Join.InstallJoinHistory (snd secondDonor)) partial
  assertEqual "the activated second member receives the next actual certified cut" Join.JoinHistoryInstalled sourceReply
  assertEqual "the immutable genesis still has its single original member" 1 (length (Genesis.checkedActiveHeralds genesis))
  assertEqual "the actual second admission has two predecessor members" 2 (NE.length (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record2)))
  assertEqual "genesis membership cannot prematurely complete the next live source set" Join.JoinRetry partialReply
  assertPartialJoiningImport observer partial
  forM_ [firstDonor, secondDonor] $ \(source, bytes) ->
    case ControlBase.sourceAdmission source bytes observer of
      Left problem -> assertFailure ("dynamic second admission source validation: " <> show problem)
      Right actualRecord -> assertEqual "each real source binds the next admission" record2 actualRecord
  case ControlBase.replacePortableJoiningBases [firstDonor, secondDonor] partial of
    Left problem -> assertFailure ("dynamic second admission replacement: " <> show problem)
    Right _ -> pure ()
  assertEqual "the second real predecessor completes live source adoption" Join.JoinHistoryInstalled completeReply
  assertEqual "the adopted admission is the actual second join" (Just record2) (Projection.oracleViewHeraldAdmission (admissionRecordId record2) (Projection.oracleView (startupOracleProjectionState complete)))
  assertEqual "the applicant adopts the actual two-member predecessor generation" (heraldMembershipGenerationId (admissionRecordPredecessor record2)) (Progress.structuralProgressMembershipGenerationId (startupStructuralProgressState complete))
  let activationIndex = oracleGreatestControlIndex oracleActive
      oldAdmissionVertices = Graph.graphMembershipAdmissionVerticesAt activationIndex (startupGraphState founderActive)
  assertBool "the earlier admission contributes real private system-view vertices" (not (null oldAdmissionVertices))
  assertEqual "the new applicant restores exactly the earlier activated membership vertices" oldAdmissionVertices (Graph.graphMembershipAdmissionVerticesAt activationIndex (startupGraphState complete))
  assertEqual "restored admission vertices retain their original activation coordinate" [] (Graph.graphMembershipAdmissionVerticesAt (controlIndex (controlIndexWord64 activationIndex - 1)) (startupGraphState complete))
  assertEqual "historical admissions do not rewrite timeless graph baseline vertices" (Graph.graphBaselineVertices (startupGraphState observer)) (Graph.graphBaselineVertices (startupGraphState complete))
  assertBool "the third Herald stays restricted before its own activation" (not (serving complete))
  assertOnlyJoinReply completeReply completeEffects
  assertEqual "the second live admission preserves every owner invariant" (Right ()) (validateHeraldState complete)

casePortableJoiningSources :: Bool -> Assertion
casePortableJoiningSources reversePrefixes = do
  (_, record, first, second, observer) <- portableJoiningSourcesFixture reversePrefixes
  let firstHistory = History.sourceJoinHistory (captureSource first)
      secondHistory = History.sourceJoinHistory (captureSource second)
      donors = [portableDonor first, portableDonor second]
      install = (`ControlBase.installPortableJoiningBases` observer)
      (imported, effects) = checked (install donors)
      (reversed, reversedEffects) = checked (install (reverse donors))
      seal = checked (Join.prepareJoinSeal record [captureSource first, captureSource second])
      baseFloor = Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState second)
      finalPrefix = max (History.joinHistoryControlPrefix firstHistory) (History.joinHistoryControlPrefix secondHistory)
  assertBool "the sources have distinct certified K coordinates" (History.joinHistoryTopologyCut firstHistory /= History.joinHistoryTopologyCut secondHistory)
  assertBool "the selected donor supplies an independently compacted Projection" (baseFloor > controlIndex 0)
  if reversePrefixes
    then do
      assertBool "the older topology donor actually has the larger B" (History.joinHistoryControlPrefix firstHistory > History.joinHistoryControlPrefix secondHistory)
      assertBool "the maximum-prefix donor History starts beyond the selected snapshot" (History.joinHistoryControlAnchor firstHistory > History.joinHistoryControlPrefix secondHistory && null (History.joinHistoryControlEntries firstHistory))
      assertBool "the exact cross-source suffix remains in the donor Projection tail" (not (null (retainedTailAfter second first)))
    else pure ()
  assertEqual "collection installs the certified common descendant K" (deriveTopologyCutId (joinSealTopologyCut seal)) (Progress.structuralLastInstalledCutId (startupStructuralProgressState imported))
  assertEqual "scratch import preserves the selected covered base until publication" baseFloor (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState imported))
  assertEqual "later control is replayed above the selected base" finalPrefix (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState imported)))
  assertBool "both exact donor histories are installed" (all (`History.joinHistoryInstalled` imported) [firstHistory, secondHistory])
  assertBool "every donor's exact immutable envelope is retained" (all (\((epoch, bytes), history) -> lookup (admissionRecordId record, admissionRecordAttempt record, epoch) (JoinState.installedHistories (startupJoinState imported)) == Just bytes && History.joinHistorySource history == epoch) (zip donors [firstHistory, secondHistory]))
  assertEqual "collection preserves the complete owner invariant" (Right ()) (validateHeraldState imported)
  assertBool "collection keeps the new Herald restricted" (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState imported) && not (serving imported))
  assertEqual "collection emits no external work" [] (effectBatchMembers effects)
  assertBool "source order cannot affect any imported owner" (reversed == imported)
  assertEqual "source order cannot affect effects" (effectBatchMembers effects) (effectBatchMembers reversedEffects)
  assertBool "an incomplete donor set is rejected" (isLeft (install [portableDonor first]))
  let (duplicate, duplicateEffects) = checked (install (portableDonor first : donors))
      conflicting = portableDonor (reclaimWithoutAdmissionTail record (if reversePrefixes then first else second))
  assertBool "identical repeated donor bytes are idempotent" (duplicate == imported)
  assertEqual "identical donor repeats emit no extra effects" (effectBatchMembers effects) (effectBatchMembers duplicateEffects)
  assertBool "the duplicate conflict has different valid bytes for the same source" (conflicting /= if reversePrefixes then portableDonor first else portableDonor second)
  assertBool "conflicting duplicate source payloads are rejected" (isLeft (install (conflicting : donors)))
  assertBool "a declared donor cannot substitute another member's envelope" (isLeft (install [(fst (portableDonor first), snd (portableDonor second)), portableDonor second]))
  assertBool "a serving member cannot adopt the collection" (isLeft (ControlBase.installPortableJoiningBases donors first))
  assertBool "an already populated observer cannot be overwritten by this private seam" (isLeft (ControlBase.installPortableJoiningBases donors imported))

casePortableJoiningSourceGap :: Assertion
casePortableJoiningSourceGap = do
  (_, record, first, second, observer) <- portableJoiningSourcesFixture True
  let unavailable = reclaimWithoutAdmissionTail record first
      history = History.sourceJoinHistory (captureSource unavailable)
      selectedPrefix = Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState second))
  assertBool "the later control interval is genuinely absent from the compact donor" (History.joinHistoryControlAnchor history > selectedPrefix && null (History.joinHistoryControlEntries history) && Projection.retainedControlSuffixAfter selectedPrefix (startupOracleProjectionState unavailable) == Nothing)
  assertBool "selection cannot bridge an unavailable control suffix or fall back to older K" (isLeft (ControlBase.installPortableJoiningBases [portableDonor unavailable, portableDonor second] observer))

casePortableJoiningSourceTie :: Assertion
casePortableJoiningSourceTie = do
  (_, _, first, second, _, observer) <- joiningSourceOwnersFixture False
  let firstAtB = foldl (flip deliverEntry) first (retainedTailAfter first second)
      firstCompact = checked (ControlBase.reclaimControlBasePrefix firstAtB)
      firstDonor = portableDonor firstCompact
      secondDonor = portableDonor second
      (imported, _) = checked (ControlBase.installPortableJoiningBases [secondDonor, firstDonor] observer)
  assertEqual "the active source has coherent Progress at the common cut" (Right ()) (Progress.validateStructuralProgressState (startupStructuralProgressState firstAtB))
  assertEqual "the tie fixture has equal K" (Progress.structuralLastInstalledCutId (startupStructuralProgressState firstCompact)) (Progress.structuralLastInstalledCutId (startupStructuralProgressState second))
  assertEqual "the tie fixture has equal B" (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState firstCompact))) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState second)))
  let (reversed, _) = checked (ControlBase.installPortableJoiningBases [firstDonor, secondDonor] observer)
  assertBool "a complete K/B tie imports identically in either source order" (imported == reversed)
  assertEqual "both fully covered tied bases retain the same final coverage" (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState firstCompact)) (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState imported))

casePortableJoiningSourcesActivation :: Assertion
casePortableJoiningSourcesActivation = do
  (oracle0, record, first, second, _, observer0) <- joiningSourceOwnersFixture False
  let freeze state = case request (Join.CaptureJoinHistory (admissionRecordId record)) state of
        (frozen, Join.JoinHistoryReply _, _) -> frozen
        (_, reply, _) -> error ("multi-source freeze failed: " <> show reply)
      sources = map freeze [first, second]
      donors = map portableDonor sources
      caughtUpSources = [foldl (flip deliverEntry) source (retainedTailAfter source second) | source <- sources]
      exchange state = foldl receive state (map snd donors)
      receive state bytes = case request (Join.InstallJoinHistory bytes) state of
        (next, Join.JoinHistoryInstalled, _) -> next
        (_, reply, _) -> error ("multi-source old-member exchange failed: " <> show reply)
      exchangedSources = map exchange caughtUpSources
      (observer1, observerReplies, observerEffects) = case donors of
        [(_, firstBytes), (_, secondBytes)] ->
          let (partial, firstReply, firstEffects) = request (Join.InstallJoinHistory firstBytes) observer0
              (complete, secondReply, secondEffects) = request (Join.InstallJoinHistory secondBytes) partial
           in (complete, [firstReply, secondReply], [firstEffects, secondEffects])
        _ -> error "multi-source activation fixture requires two immutable donors"
      seal = checked (Join.prepareJoinSeal record (map captureSource sources))
      commitFrom source command oracle =
        let envelope = oracleEnvelope (oracleClientRequestId source (100000 + controlIndexWord64 (oracleGreatestControlIndex oracle) + 1)) Nothing source (heraldAdmissionCommand command)
         in case checked (stepOracle envelope oracle) of
              (next, OracleCommitted receipt, effects) | oracleReceiptResult receipt == OracleAccepted -> case oracleEffects effects of
                [EmitAppliedOracleEntry accepted] -> (next, canonicalizeAppliedOracleEntry accepted)
                other -> error (show other)
              (_, outcome, _) -> error ("multi-source command failed: " <> show outcome)
      advanceSources command (oracle, old, observer) =
        let source = case command of HeraldJoinBaseReady report -> joinReadyReporter report; _ -> founderEpoch
            (nextOracle, entry) = commitFrom source command oracle
            nextOld = map (deliverEntry entry) old
            (nextObserver, reply, _) = request (Join.InstallJoinControlHistory [entry]) observer
         in if reply == Join.JoinHistoryInstalled then (nextOracle, nextOld, nextObserver) else error ("multi-source control suffix failed: " <> show reply)
      afterSeal = advanceSources (SealHeraldAdmission seal) (oracle0, exchangedSources, observer1)
      -- Each old member accepts its own exact frozen contribution.
      acceptSource (oracle, old, observer) source =
        let (nextOracle, entry) = commitFrom source (AcceptHeraldJoinSeal (admissionRecordId record) (admissionRecordAttempt record) (joinSealDigest seal)) oracle
            (nextObserver, reply, _) = request (Join.InstallJoinControlHistory [entry]) observer
         in if reply == Join.JoinHistoryInstalled then (nextOracle, map (deliverEntry entry) old, nextObserver) else error ("multi-source acceptance replay failed: " <> show reply)
      afterAccept@(_, readySources, readyObserver) = foldl acceptSource afterSeal (map fst donors)
      ready state = case secondOf3 (request (Join.ReadJoinReady (admissionRecordId record)) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("multi-source readiness failed for " <> show (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) <> ": " <> show reply)
      afterOld = foldl (\state source -> advanceSources (HeraldJoinBaseReady (ready source)) state) afterAccept readySources
      afterNew = advanceSources (HeraldJoinReady (ready readyObserver)) afterOld
      (_, finalSources, active) = advanceSources (ActivateHerald (admissionRecordId record)) afterNew
      leastMember = minimum (map fst donors)
      authority = captureSource (fromJust (find ((== leastMember) . fst . portableDonor) sources))
      staged = thirdOf3 afterNew
  assertEqual "live multi-source adoption waits for the complete predecessor set" [Join.JoinRetry, Join.JoinHistoryInstalled] observerReplies
  mapM_ (uncurry assertOnlyJoinReply) (zip observerReplies observerEffects)
  assertBool "the complete donor collection remains unobservable until activation" (not (serving staged))
  assertBool "ordinary suffix activates the multi-donor base" (serving active)
  assertEqual "all activated receiver owners remain consistent" (Right ()) (validateHeraldState active)
  forM_ finalSources $ \source -> assertEqual "all donor owners remain consistent after activation" (Right ()) (validateHeraldState source)
  assertBool "the least member's frozen history was available at activation" (History.joinHistoryInstalled (History.sourceJoinHistory authority) staged)
  assertBool "the least member's frozen alignment frontier remains admissible" (not (isLeft (History.adoptJoinAlignmentFrontier authority staged)))
  forM_ [captureSource source | source <- sources, fst (portableDonor source) /= leastMember] $ \other ->
    assertBool "choosing a different structural base cannot transfer alignment authority" (isLeft (History.adoptJoinAlignmentFrontier other staged))
  assertEqual "activation releases every imported donor envelope" [] (JoinState.installedHistories (startupJoinState active))
  where
    thirdOf3 (_, _, value) = value

staggeredFixture :: IO (HeraldAdmissionRecord, History.JoinSourceHistory, History.JoinSourceHistory, History.JoinSourceHistory)
staggeredFixture = do
  (_, record, first, second, forked, _) <- staggeredOwnersFixture
  pure (record, captureSource first, captureSource second, captureSource forked)

staggeredOwnersFixture :: IO (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState, HeraldState, HeraldState)
staggeredOwnersFixture = joiningSourceOwnersFixture True

joiningSourceOwnersFixture :: Bool -> IO (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState, HeraldState, HeraldState)
joiningSourceOwnersFixture laterTopology = do
  let firstGenesis = Fixtures.fixtureCheckedGenesis
      secondGenesis = checked (checkHeraldGenesis (Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember))
      selected = checked (checkInitialBootstraps firstGenesis (PrimordialProcessManifest []))
      initializeAt genesis = fst (checked (initialHerald (monotonicInstant 1) genesis selected Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))
      initial = initializeAt firstGenesis
      oracle0 = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps firstGenesis selected))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews initial) (startupStructuralProgressState initial))
      (oracle1, begunEntry) = submit (BeginHeraldAdmission manifest anchor) oracle0
      record = fromJust (oraclePendingHeraldAdmission oracle1)
      admission = Just (admissionRecordId record)
      secondBeforeCut = deliverEntry begunEntry (initializeAt secondGenesis)
      envelope = oracleEnvelope (oracleClientRequestId founderEpoch 900001) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
      (oracle2, extraEntry) = case checked (stepOracle envelope oracle1) of
        (advanced, OracleCommitted receipt, effects) | oracleReceiptResult receipt /= OracleAccepted -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> (advanced, canonicalizeAppliedOracleEntry entry)
          _ -> error "no canonical idle control entry"
        _ -> error "unknown voter change cancellation should be rejected without semantic change"
  -- The proposer may already have an open round from Begin. Finish its real
  -- acceptance/acknowledgement ceremony before capturing, then transfer that
  -- exact certified chain to the second owner through the production path.
  first <- settleControlBaseCut (deliverEntry begunEntry initial)
  let firstSource = captureSource first
      (secondAtFirstCut, firstReply, _) = request (Join.InstallJoinHistory (History.encodeJoinSourceHistory firstSource)) secondBeforeCut
      secondInstalled = (if laterTopology then establishCut admission else id) (deliverEntry extraEntry secondAtFirstCut)
      forkInstalled = establishCut admission (deliverEntry extraEntry secondBeforeCut)
      settleInstalled state = fst (checked (StructuralCoordinator.settleInstalledStructuralCuts [Progress.structuralLastInstalledCutId (startupStructuralProgressState state)] state))
      second = settleInstalled secondInstalled
      forked = settleInstalled forkInstalled
      observer = initializeAt (checked (checkJoiningHeraldGenesis firstGenesis record))
  assertEqual "the second source receives the first source's actual certified chain" Join.JoinHistoryInstalled firstReply
  mapM_ (\(name, state) -> assertEqual (name <> " staggered source has coherent owners before aggregate capture") (Right ()) (validateHeraldState state)) [("first", first), ("second", second), ("forked", forked)]
  pure (oracle2, record, first, second, forked, observer)

requestVariants :: [Join.JoinRequest]
requestVariants =
  let (_, record, history, founder, _) = capturedFixture
      admission = admissionRecordId record
   in [Join.ReadJoinStatus, Join.SubmitJoinCommand (Join.joinRequestId applicantEpoch 7) (CancelHeraldAdmission admission), Join.ReadJoinCommand (Join.joinRequestId applicantEpoch 8), Join.CaptureJoinHistory admission, Join.InstallJoinHistory (sourceBytes founder), Join.ReadJoinControlHistory (controlIndex 0), Join.ReadJoinControlHistory (controlIndex 7), Join.ReadJoinReady admission, Join.InstallJoinControlHistory (History.joinHistoryControlEntries history), Join.ReadOracleStatus, Join.ReadOracleVoterChange (checked (Voter.mkVoterChangeId (controlIndex 1))), Join.ReadOracleVoterHostFailure (Failure.acceptedVoterHostFailureResolution voterFailureCertificate), Join.SubmitJoinCommandWithRetirement (Join.joinRequestId applicantEpoch 3) (CancelHeraldAdmission admission) (checked (receiptRetirement (Just 2) (Set.singleton 0))), Join.ReadJoinCommandWithRetirement (Join.joinRequestId applicantEpoch 0) (receiptRetirementPrefix (Just 2)), Join.RetireJoinReceipts applicantEpoch mempty]

replyVariants :: [Join.JoinReply]
replyVariants =
  let (_, record, history, founder, _) = capturedFixture
      seal = checked (Join.prepareJoinSeal record [captureSource founder])
      ready = heraldJoinReadyReport (admissionRecordId record) 1 applicantEpoch (joinSealDigest seal) (joinSealControlPrefix seal) (joinSealRecipeDigest seal)
   in [secondOf3 (request Join.ReadJoinStatus founder), Join.JoinCommandPending, Join.JoinCommandComplete [record], Join.JoinHistoryReply (sourceBytes founder), Join.JoinHistoryInstalled, Join.JoinControlHistoryReply (History.joinHistoryControlEntries history), Join.JoinReadyReply ready, Join.JoinRetry, Join.JoinRejected, Join.JoinRequestConflict, secondOf3 (request Join.ReadOracleStatus founder), secondOf3 (request Join.ReadOracleStatus oracleConfiguredFounder), Join.OracleVoterChangeReply Nothing, Join.OracleVoterHostFailureReply Nothing, Join.OracleVoterHostFailureReply (Just voterFailureCertificate), Join.JoinRequestRetired (Join.joinRequestId applicantEpoch 0) (receiptRetirementPrefix (Just 0)), Join.JoinReceiptsRetired applicantEpoch (checked (receiptRetirement (Just 2) (Set.singleton 0))), Join.JoinRetirementNotReady, Join.JoinTransferRetired (admissionRecordId record) (admissionRecordAttempt record)]

oracleConfiguredFounder :: HeraldState
oracleConfiguredFounder =
  fst
    ( checked
        ( initialHeraldWithOracleVoters
            (Voter.oracleVoterConfiguration initialOracleState)
            (Voter.oracleReplicaRegistrations initialOracleState)
            (monotonicInstant 1)
            founderGenesis
            bootstraps
            Fixtures.fixtureOracleContacts
            Fixtures.fixtureGeneratorSeed
            Fixtures.fixtureApplicationRecoveryConfiguration
            Fixtures.fixturePeerRecoveryConfiguration
            Nothing
        )
    )

caseOracleStatus :: Assertion
caseOracleStatus = do
  let founder = oracleConfiguredFounder
      (afterStatus, statusReply, _) = request Join.ReadOracleStatus founder
      identifier = checked (Voter.mkVoterChangeId (controlIndex 1))
      (afterLookup, changeReply, _) = request (Join.ReadOracleVoterChange identifier) afterStatus
  case statusReply of
    Join.OracleStatusReply status -> do
      assertEqual "status names the local applied prefix" (controlIndex 0) (Join.heraldOracleStatusControlIndex status)
      assertEqual "configuration comes from the same immutable Oracle genesis" (Just (Voter.oracleVoterConfiguration initialOracleState)) (Join.heraldOracleStatusConfiguration status)
      assertEqual "all index-zero registrations are projected" (Voter.oracleReplicaRegistrations initialOracleState) (Join.heraldOracleStatusReplicas status)
      assertEqual "bootstrap invents no pending change" Nothing (Join.heraldOracleStatusPendingChange status)
    other -> assertFailure ("Oracle status query failed: " <> show other)
  assertEqual "a not-yet-applied change is explicitly absent" (Join.OracleVoterChangeReply Nothing) changeReply
  assertBool "queries never advance or alter the Oracle projection" (startupOracleProjectionState founder == startupOracleProjectionState afterLookup)
  assertEqual "configured initial owner remains valid" (Right ()) (validateHeraldState afterLookup)

voterFailureTranscript :: NativeFailure.NativeVoterFailureTranscript
voterFailureTranscript = NativeFailure.nativeVoterFailureTranscript Fixtures.fixtureCheckedOracleGenesis [] (heraldMemberEpoch Fixtures.fixtureStep14ThirdMember)

voterFailureCertificate :: Failure.AcceptedVoterHostFailure
voterFailureCertificate = NativeFailure.nativeFailureCertificate voterFailureTranscript

caseOracleVoterHostFailure :: Assertion
caseOracleVoterHostFailure = do
  let oracle = checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
      initial = fst (checked (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle) (monotonicInstant 1) Fixtures.fixtureStep14CheckedGenesis Fixtures.fixtureCheckedInitialBootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration Nothing))
      entries = map snd (NativeFailure.nativeFailureEvidence voterFailureTranscript)
      opened = deliverEntry (firstEntry entries) initial
      accepted = foldl (flip deliverEntry) initial entries
      reclaimed = checked (ControlBase.reclaimControlBasePrefix accepted)
      binding = fromJust (Client.oracleClientCurrentBinding (startupOracleClientState opened))
      frozen = fst (checked (applyFencedOracleIngress (OracleEntriesReceived binding (NE.fromList (drop 1 entries))) (freezeStartupSemanticControl opened)))
      resolution = Failure.acceptedVoterHostFailureResolution voterFailureCertificate
      inspect state = secondOf3 (request (Join.ReadOracleVoterHostFailure resolution) state)
      otherResolution = deriveFailureProbeResolutionId (failureProbeResolutionProbeId resolution) DismissFailureProbe
  assertEqual "a not-yet-observed certificate is absent" (Join.OracleVoterHostFailureReply Nothing) (inspect initial)
  assertEqual "an open probe has no accepted certificate" (Join.OracleVoterHostFailureReply Nothing) (inspect opened)
  assertEqual "the same probe with a different resolution is absent" (Join.OracleVoterHostFailureReply Nothing) (secondOf3 (request (Join.ReadOracleVoterHostFailure otherResolution) reclaimed))
  assertEqual "reclamation no longer offers the full physical transcript" Join.JoinRejected (secondOf3 (request (Join.ReadJoinControlHistory (controlIndex 0)) reclaimed))
  assertEqual "frozen semantic work stays at the open probe" (controlIndex 1) (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState frozen)))
  assertEqual "the frozen control lane observes actual accepted evidence" (Failure.acceptedVoterHostFailureControlIndex voterFailureCertificate) (Projection.oracleViewControlIndex (Projection.oracleView (startupControlOracleProjectionState frozen)))
  forM_ [("accepted", accepted), ("reclaimed", reclaimed), ("frozen", frozen)] $ \(name, state) -> do
    let (after, reply, effects) = request (Join.ReadOracleVoterHostFailure resolution) state
    assertEqual (name <> " certificate retains the original exact evidence") (Join.OracleVoterHostFailureReply (Just voterFailureCertificate)) reply
    assertBool (name <> " inspection leaves both Oracle projections unchanged") (startupOracleProjectionState after == startupOracleProjectionState state && startupControlOracleProjectionState after == startupControlOracleProjectionState state)
    assertEqual (name <> " inspection only emits its requested reply") [SendJoinReply 77 (Join.encodeJoinReply reply)] (effectBatchMembers effects)
    assertEqual (name <> " owner composition remains valid") (Right ()) (validateHeraldState after)
  where
    firstEntry (entry : _) = entry
    firstEntry [] = error "native failure fixture omitted the open probe"

caseCommandTokens :: Assertion
caseCommandTokens = do
  let (oracle, record, _, founder, observer) = capturedFixture
      -- The coordinator survives cancellation; the applicant's own request
      -- receipts end automatically with its cancelled admission.
      token = Join.joinRequestId founderEpoch 9
      command = Join.SubmitJoinCommand token (CancelHeraldAdmission (admissionRecordId record))
      (submitted, result, _) = request command founder
  assertEqual "authorized old home queues once" Join.JoinCommandPending result
  assertEqual "exact token retry remains pending" Join.JoinCommandPending (secondOf3 (request command submitted))
  assertEqual "token conflict is terminal" Join.JoinRequestConflict (secondOf3 (request (Join.SubmitJoinCommand token (ActivateHerald (admissionRecordId record))) submitted))
  assertEqual "pending newcomer cannot submit ordinary Oracle commands" Join.JoinRejected (secondOf3 (request command observer))
  reference <- maybe (assertFailure "submitted Join command has no retained reference") (pure . snd) (JoinState.lookupRequest token (startupJoinState submitted))
  witness <- maybe (assertFailure "submitted Join command has no Oracle witness") pure (Client.lookupOracleRequest reference (startupOracleClientState submitted))
  assertEqual "uncommitted Join command has no observed result" Nothing (Client.oracleClientRequestEvidence reference (startupOracleClientState submitted))
  let (cancelledOracle, cancelledEntry) = commit (canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness)) oracle
      cancelled = deliverEntry cancelledEntry submitted
      (completed, completedReply, _) = request (Join.ReadJoinCommand token) cancelled
      unrelatedEnvelope = oracleEnvelope (oracleClientRequestId founderEpoch 900001) Nothing founderEpoch (Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999))))
      (_, unrelatedEntry) = commit unrelatedEnvelope cancelledOracle
      advanced = deliverEntry unrelatedEntry completed
      (queried, repeatedReply, _) = request (Join.ReadJoinCommand token) advanced
  assertBool ("the actual Join command completes: " <> show completedReply) (case completedReply of Join.JoinCommandComplete [_] -> True; _ -> False)
  assertEqual "later unrelated control cannot change the exact Join result" completedReply repeatedReply
  assertBool "repeated Join result reads are state-identical" (queried == advanced)
  assertEqual "Join discovers only its directly indexed canonical entry" (Just cancelledEntry) (Client.oracleClientRequestEvidence reference (startupOracleClientState advanced))
  let progress = receiptRetirementPrefix (Just 9)
      (retired, _, _) = request (Join.RetireJoinReceipts founderEpoch progress) advanced
      (_, retiredReply, _) = request (Join.ReadJoinCommand token) retired
  assertEqual "retired RPC result remains retired despite retained semantic evidence" (Join.JoinRequestRetired token progress) retiredReply
  assertEqual "retiring the Join receipt does not erase canonical request evidence" (Just cancelledEntry) (Client.oracleClientRequestEvidence reference (startupOracleClientState retired))
  assertEqual "Join result lookup and retirement preserve whole-Herald invariants" (Right ()) (validateHeraldState retired)
  where
    commit envelope oracle = case checked (stepOracle envelope oracle) of
      (successor, OracleCommitted _, effects) -> case oracleEffects effects of
        [EmitAppliedOracleEntry entry] -> (successor, canonicalizeAppliedOracleEntry entry)
        other -> error ("Join result fixture expected one canonical entry: " <> show other)
      (_, outcome, _) -> error ("Join result fixture expected a committed request: " <> show outcome)

caseReceiptRetirement :: Assertion
caseReceiptRetirement = do
  let (_, record, _, founder, _) = capturedFixture
      zero = Join.joinRequestId applicantEpoch 0
      one = Join.joinRequestId applicantEpoch 1
      command = CancelHeraldAdmission (admissionRecordId record)
      pending = firstOf3 (request (Join.SubmitJoinCommand zero command) founder)
      (unchanged, rejected, effects) = request (Join.SubmitJoinCommandWithRetirement one command (receiptRetirementPrefix (Just 0))) pending
  assertEqual "pending receipt rejects the atomic attached submission" Join.JoinRetirementNotReady rejected
  assertBool "pending rejection preserves every owner" (pending == unchanged)
  assertEqual "pending rejection emits only its negative acknowledgment" [SendJoinReply 77 (Join.encodeJoinReply Join.JoinRetirementNotReady)] (effectBatchMembers effects)
  let progress = receiptRetirementPrefix (Just 1)
      (retired, acknowledgment, _) = request (Join.RetireJoinReceipts applicantEpoch progress) founder
      (repeated, result, replayEffects) = request (Join.SubmitJoinCommand one command) retired
  assertEqual "standalone progress returns the owning namespace" (Join.JoinReceiptsRetired applicantEpoch progress) acknowledgment
  assertEqual "released request cannot allocate fresh semantic work" (Join.JoinRequestRetired one progress) result
  assertBool "expired duplicate preserves owner state" (retired == repeated)
  assertEqual "expired duplicate emits only its typed result" [SendJoinReply 77 (Join.encodeJoinReply result)] (effectBatchMembers replayEffects)

propReceiptRetirement :: Word8 -> Property
propReceiptRetirement sample =
  let count = 1 + fromIntegral sample `mod` 40
   in forAll (shuffle [1 .. count]) $ \order ->
        let (_, record, _, _, _) = capturedFixture
            command = CancelHeraldAdmission (admissionRecordId record)
            reference n = OracleRequestRef (oracleClientRequestId founderEpoch n)
            key = Join.joinRequestId applicantEpoch
            other = Join.joinRequestId founderEpoch 1
            start =
              foldl
                (\state n -> JoinState.retainRequest (key n) command (reference n) state)
                (JoinState.retainRequest other command (reference 0) JoinState.emptyState)
                [0 .. count]
            release (state, unresolved) completed =
              let remaining = Set.delete completed unresolved
                  progress = checked (receiptRetirement (Just completed) (Set.filter (<= completed) remaining))
                  next = checked (JoinState.retireRequestReceipts applicantEpoch progress (/= reference 0) state)
               in (next, remaining)
            states = scanl release (start, Set.fromList [0 .. count]) order
            final = foldl (\_ state -> state) (start, Set.fromList [0 .. count]) states
            laws (state, unresolved) =
              conjoin
                [ length (JoinState.requestEntries state) === Set.size unresolved + 1,
                  JoinState.lookupRequest other state === Just (command, reference 0),
                  conjoin [JoinState.requestIsRetired (key n) state === not (Set.member n unresolved) | n <- [0 .. count]],
                  JoinState.retireRequestReceipts applicantEpoch (receiptRetirementPrefix (Just count)) (/= reference 0) state === Left ()
                ]
            (finished, _) = final
            stale = checked (receiptRetirement (Just count) (Set.fromList [0 .. count]))
            closed = checked (JoinState.retireClosedRequestReceipts (== applicantEpoch) (/= reference 0) start)
            settledClosed = checked (JoinState.retireClosedRequestReceipts (== applicantEpoch) (const True) closed)
         in conjoin
              ( [laws state | state <- states]
                  <> [ length (JoinState.requestEntries finished) === 2,
                       JoinState.retireRequestReceipts applicantEpoch stale (/= reference 0) finished === Right finished,
                       closed === finished,
                       JoinState.requestEntries settledClosed === [(other, command, reference 0)],
                       JoinState.requestIsRetired (key 0) settledClosed === True,
                       JoinState.retireClosedRequestReceipts (== applicantEpoch) (const True) settledClosed === Right settledClosed
                     ]
              )

propTransferCleanup :: Word8 -> Property
propTransferCleanup seed =
  let count = 1 + fromIntegral seed `mod` 20
   in forAll (shuffle [1 .. count]) $ \order ->
        let admission = checked (deriveHeraldAdmissionId (controlIndex 1))
            unrelated = checked (deriveHeraldAdmissionId (controlIndex 2))
            requestKey = Join.joinRequestId applicantEpoch 0
            reference = OracleRequestRef (oracleClientRequestId founderEpoch 1)
            pending = JoinState.retainRequest requestKey (CancelHeraldAdmission admission) reference JoinState.emptyState
            retain state attempt =
              let payload = BS.pack [fromIntegral attempt]
               in JoinState.retainCapture
                    admission
                    attempt
                    payload
                    (JoinState.retainInstalledHistory admission attempt founderEpoch payload state)
            original = foldl retain (JoinState.retainCapture unrelated 1 "other capture" (JoinState.retainInstalledHistory unrelated 1 applicantEpoch "other installation" pending)) [1 .. count]
            release (state, released) attempt =
              ( JoinState.releaseTransferHistories (\owner retainedAttempt -> owner == admission && retainedAttempt == attempt) state,
                Set.insert attempt released
              )
            states = scanl release (original, Set.empty) order
            laws (state, released) =
              let selected owner attempt = owner == admission && Set.member attempt released
               in conjoin
                    [ JoinState.capturedHistories state === [entry | entry@((owner, attempt), _) <- JoinState.capturedHistories original, not (selected owner attempt)],
                      JoinState.installedHistories state === [entry | entry@((owner, attempt, _), _) <- JoinState.installedHistories original, not (selected owner attempt)],
                      JoinState.requestEntries state === JoinState.requestEntries original,
                      JoinState.requestReceiptRetirement applicantEpoch state === mempty,
                      JoinState.releaseTransferHistories selected state === state,
                      JoinState.releaseTransferHistories (\_ _ -> False) state === state
                    ]
         in conjoin (map laws states)

captureSource :: HeraldState -> History.JoinSourceHistory
captureSource state =
  let record = fromJust (Projection.oracleViewPendingHeraldAdmission (Projection.oracleView (startupOracleProjectionState state)))
      bytes = case JoinState.lookupCapture (admissionRecordId record) (admissionRecordAttempt record) (startupJoinState state) of
        Just retained -> retained
        Nothing -> ControlBase.encodeJoiningBase (fromJust (checked (ControlBase.captureJoiningBase state)))
   in checked (History.decodeJoinSourceHistory bytes)

sourceBytes :: HeraldState -> BS.ByteString
sourceBytes = History.encodeJoinSourceHistory . captureSource

admissionTailFloor :: HeraldAdmissionRecord -> HeraldState -> Maybe ControlIndex
admissionTailFloor record state =
  JoinState.admissionControlTailExclusiveFloor
    <$> JoinState.lookupAdmissionControlTail (admissionManifestHeraldEpoch (admissionRecordManifest record)) (startupJoinState state)

assertAdmissionTail :: String -> HeraldAdmissionRecord -> Maybe ControlIndex -> HeraldState -> Assertion
assertAdmissionTail message record expected state = assertEqual message expected (admissionTailFloor record state)

capturedFixture :: (OracleState, HeraldAdmissionRecord, History.JoinHistory, HeraldState, HeraldState)
capturedFixture =
  let founder0 = initialize founderGenesis
      (oracle, beginEntry) = capturedBegin
      record = fromJust (oraclePendingHeraldAdmission oracle)
      begun = deliverEntry beginEntry founder0
      (captured, reply, _) = request (Join.CaptureJoinHistory (admissionRecordId record)) begun
      history = case reply of
        Join.JoinHistoryReply bytes -> History.sourceJoinHistory (checked (History.decodeJoinSourceHistory bytes))
        other -> error ("history capture failed: " <> show other)
      observer = initialize (checked (checkJoiningHeraldGenesis founderGenesis record))
   in (oracle, record, history, captured, observer)

-- Keep the emitted input independently of the donor's reclaimed archive.
capturedBeginEntry :: CanonicalAppliedOracleEntry
capturedBeginEntry = snd capturedBegin

capturedBegin :: (OracleState, CanonicalAppliedOracleEntry)
capturedBegin =
  let founder = initialize founderGenesis
      anchor = checked (Progress.structuralAdmissionAnchorClaim (structuralReconciliationViews founder) (startupStructuralProgressState founder))
   in submit (BeginHeraldAdmission manifest anchor) initialOracleState

-- Both members have applied Activate, before any ordinary applicant Hello has
-- acknowledged it to the donor. The returned record includes actual activation.
activatedFixture :: (OracleState, HeraldAdmissionRecord, HeraldState, HeraldState)
activatedFixture =
  let (oracle0, record, _, founder0, observer0) = capturedFixture
      observer1 = firstOf3 (request (Join.InstallJoinHistory (sourceBytes founder0)) observer0)
      admission = admissionRecordId record
      seal = checked (Join.prepareJoinSeal record [captureSource founder0])
      afterSeal = advance (SealHeraldAdmission seal) (oracle0, founder0, observer1)
      afterAccept@(_, founder2, observer2) = advance (AcceptHeraldJoinSeal admission (admissionRecordAttempt record) (joinSealDigest seal)) afterSeal
      ready state = case secondOf3 (request (Join.ReadJoinReady admission) state) of
        Join.JoinReadyReply report -> report
        reply -> error ("activated fixture readiness failed: " <> show reply)
      afterOld = advance (HeraldJoinBaseReady (ready founder2)) afterAccept
      afterNew = advance (HeraldJoinReady (ready observer2)) afterOld
      (oracle, founder, observer) = advance (ActivateHerald admission) afterNew
      activated = fromJust (Projection.oracleViewHeraldAdmission admission (Projection.oracleView (startupOracleProjectionState observer)))
   in (oracle, activated, founder, observer)

advance :: HeraldAdmissionCommand -> (OracleState, HeraldState, HeraldState) -> (OracleState, HeraldState, HeraldState)
advance command (oracle, founder, observer) =
  let (nextOracle, entry) = submit command oracle
      nextFounder = deliverEntry entry founder
      (nextObserver, reply, _) = request (Join.InstallJoinControlHistory [entry]) observer
   in if reply == Join.JoinHistoryInstalled then (nextOracle, nextFounder, nextObserver) else error ("join control import failed: " <> show reply)

submit :: HeraldAdmissionCommand -> OracleState -> (OracleState, CanonicalAppliedOracleEntry)
submit command = submitCommand (heraldAdmissionCommand command)

submitCommand :: OracleCommand -> OracleState -> (OracleState, CanonicalAppliedOracleEntry)
submitCommand command oracle =
  let identifier = oracleClientRequestId founderEpoch (100000 + controlIndexWord64 (oracleGreatestControlIndex oracle) + 1)
      envelope = oracleEnvelope identifier Nothing founderEpoch command
   in case checked (stepOracle envelope oracle) of
        (successor, OracleCommitted receipt, effects) | oracleReceiptResult receipt == OracleAccepted -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> (successor, canonicalizeAppliedOracleEntry entry)
          other -> error (show other)
        (_, outcome, _) -> error ("Oracle join command failed: " <> show outcome)

deliverEntry :: CanonicalAppliedOracleEntry -> HeraldState -> HeraldState
deliverEntry entry state =
  let connected = case Client.oracleClientCurrentBinding (startupOracleClientState state) of
        Just _ -> state
        Nothing -> case Client.oracleClientActions (startupOracleClientState state) of
          [ConnectAndHelloOracle attempt _] ->
            let node = oracleContactNode (oracleConnectAttemptContact attempt)
             in apply (OracleHelloReceived attempt (oracleHelloAcceptance node (oracleObservedTerm 1) (Client.oracleClientAppliedCursor (startupOracleClientState state)) (Just node) True)) state
          other -> error ("Oracle connection unavailable: " <> show other)
      binding = fromJust (Client.oracleClientCurrentBinding (startupOracleClientState connected))
   in apply (OracleEntriesReceived binding (entry :| [])) connected
  where
    apply ingress predecessor = fst (checked (stepHerald (heraldInput (startupLastObservedTime predecessor) (OracleInput ingress)) predecessor))

request :: Join.JoinRequest -> HeraldState -> (HeraldState, Join.JoinReply, EffectBatch)
request command state =
  let (successor, effects) = checked (stepHerald (heraldInput (startupLastObservedTime state) (JoinInput 77 (Join.encodeJoinRequest command))) state)
   in case [checked (Join.decodeJoinReply bytes) | SendJoinReply 77 bytes <- effectBatchMembers effects] of
        [reply] -> (successor, reply, effects)
        other -> error ("join request reply cardinality: " <> show other)

serving :: HeraldState -> Bool
serving state = case secondOf3 (request Join.ReadJoinStatus state) of
  Join.JoinStatusReply status -> Join.joinStatusServing status
  other -> error ("join status failed: " <> show other)

initialize :: CheckedHeraldGenesis -> HeraldState
initialize genesis = fst (checked (initialHerald (monotonicInstant 1) genesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration))

founderGenesis :: CheckedHeraldGenesis
founderGenesis = checked (checkHeraldGenesis deployment)
  where
    original = Fixtures.fixtureDeploymentManifest
    deployment =
      original
        { deploymentActiveHeralds = [Fixtures.fixtureLocalMember],
          deploymentConfiguredProcesses = [],
          deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = [Fixtures.fixtureLocalMember]}
        }

bootstraps :: CheckedInitialBootstraps
bootstraps = checked (checkInitialBootstraps founderGenesis (PrimordialProcessManifest []))
initialOracleState :: OracleState
initialOracleState = checked (initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps founderGenesis bootstraps))
founderEpoch :: HeraldEpoch
founderEpoch = heraldMemberEpoch Fixtures.fixtureLocalMember
applicantEpoch :: HeraldEpoch
applicantEpoch = checked (mkHeraldEpoch (BS.replicate 32 0xf0))
manifest :: HeraldAdmissionManifest
manifest = heraldAdmissionManifest (Genesis.checkedSystemId founderGenesis) (checked (mkHeraldId (BS.replicate 32 0xef))) applicantEpoch
firstOf3 :: (a, b, c) -> a
firstOf3 (value, _, _) = value
secondOf3 :: (a, b, c) -> b
secondOf3 (_, value, _) = value
secondOf5 :: (a, b, c, d, e) -> b
secondOf5 (_, value, _, _, _) = value
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
