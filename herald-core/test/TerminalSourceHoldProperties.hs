{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TerminalSourceHoldProperties
  ( tests,
  )
where

import ApplicationLabelProperties qualified
import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Types.Access qualified as Access
import Eclips.Domain.Identity
  ( HeraldEpoch,
    TopologyCutId,
    controlIndex,
    firstStructuralSequence,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipHistory,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipHistory,
    heraldMembershipLineage,
    heraldMembershipLineageOrigin,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Publication (mkCheckedPublication)
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
  )
import Eclips.Domain.Startup
  ( AppliedRootRole (WriterRoot),
    HeraldMember (..),
  )
import Eclips.Domain.Structural
  ( emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
  )
import Eclips.Domain.Topology (mkTopologyOccurrenceDigest)
import Eclips.Domain.Value
  ( LabelOwner (VoidLabel),
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    recordValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerBindingRemoteHeraldEpoch,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( DeploymentManifest (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( AppliedProcessBootstrap,
    AppliedRoot,
    CheckedInitialBootstraps,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootOccurrenceId,
    appliedRootRole,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( structuralAppliedReport,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( TerminalSourceInventory,
    TerminalSourcePayloadRelay,
    TerminalSourcePayloadRequest,
    TerminalSourceProblem (TerminalSourceTopologyReconstructionUnavailable),
    TerminalSourceUnionAnnounce,
    TerminalSourceUnionEstablished,
    TerminalStructuralOccurrence,
    deriveTerminalSourceUnion,
    deriveTerminalSourceUnionChecked,
    deriveTerminalStructuralPayloadDigest,
    emptyTerminalSourcePayloadArchive,
    sealTerminalSourceInventory,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInventory,
    terminalSourcePayloadRelay,
    terminalSourceUnionAcceptance,
    terminalSourceUnionAcceptanceReporter,
    terminalSourceUnionAnnounce,
    terminalSourceUnionEstablished,
    terminalSourceUnionReady,
    terminalStructuralOccurrence,
    terminalStructuralOccurrenceId,
  )
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( HeraldInputBody (PeerInput),
    PeerControl
      ( PeerStructuralAppliedReported,
        PeerTerminalSourceInventoryAdvertised,
        PeerTerminalSourcePayloadRelayed,
        PeerTerminalSourcePayloadRequested,
        PeerTerminalSourceUnionAccepted,
        PeerTerminalSourceUnionAnnounced,
        PeerTerminalSourceUnionEstablished
      ),
    PeerIngress (PeerControlReceived),
    heraldInput,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle),
    OracleClientIngress (OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPublication
  ( mkStructuralOccurrenceStamp,
    structuralPublicationCanonicalBytesForSemantics,
  )
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault (HeraldStartupInvariant), StartupInvariantSubject (StartupStatic), StartupInvariantViolation (StructuralProgressInvariant), validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupOracleClientState,
    replaceStartupStructuralBaseCoordinator,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupLabelBarrierState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
    startupWaitState,
  )
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedInitialBootstraps,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureHeraldAfterProbePrefix,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    fixtureRetirementResolution,
    fixtureStep14CheckedGenesis,
  )
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
    "terminal-source generation-ordering hold"
    [ QC.testProperty
        "without a coordinator, progressed owners and reordered ahead-control replays preserve the complete readiness result"
        propNoCoordinatorReadiness,
      testCase
        "an authenticated successor inventory outrunning Oracle is retained without closing its peer"
        caseInventoryOutrunsProjection,
      testCase
        "duplicates are singular and only the exact authoritative successor releases them"
        caseDuplicateAndResolution,
      testCase
        "closure origin history must agree with its Oracle target"
        caseClosureHistoryInvariant,
      testCase
        "later membership controls remain independent of current readiness and skipped watches"
        caseRepeatedRetirementHolds,
      testCase
        "an exact successor structural report is held once and replayed only for its base"
        caseSuccessorStructuralReportHold,
      testCase
        "a same-generation announce waits on its stable binding and accepts once after topology readiness"
        caseAnnounceReadinessArrivalOrder,
      testCase
        "same-generation established evidence waits on its stable binding and installs once after topology readiness"
        caseEstablishedReadinessArrivalOrder,
      testCase
        "announce before inventory and payload stays bound and releases once after both arrive"
        caseAnnounceBeforeInventoryAndPayload,
      testCase
        "a valid payload archive may wait for source-topology readiness without a Store receipt"
        casePayloadArchiveHeldBeforeSourceTopology,
      testCase
        "an exact terminal payload relay replay is an effect-free no-op"
        caseExactDuplicatePayloadRelay
    ]

-- These histories keep their future membership controls ahead of the genuine
-- Oracle prefix. Structural publication advances Graph and Controlled first;
-- the generated prefix position also exercises Oracle progress with a hold.
-- The checked fixtures supply owner state; each complete successor is validated.
propNoCoordinatorReadiness :: QC.Property
propNoCoordinatorReadiness =
  QC.forAll histories $ \(role, history) -> QC.ioProperty $ do
    fixture <- holdFixture
    (settled, _, _) <- ApplicationLabelProperties.settledStructuralControlledFixture role
    reporter <-
      maybe
        (assertFailure "ahead-control fixture reporter is absent")
        pure
        ( find
            ((== peerBindingRemoteHeraldEpoch fixture.binding) . heraldMemberEpoch)
            (checkedActiveHeralds fixtureStep14CheckedGenesis)
        )
    (connected, binding) <- bindPeer reporter settled
    assertBool
      "ordinary structural publication advanced Graph"
      (startupGraphState connected /= startupGraphState fixture.predecessorState)
    assertBool
      "ordinary structural publication advanced Controlled"
      (startupControlledState connected /= startupControlledState fixture.predecessorState)
    assertNoCoordinatorReadiness "unchanged owners" fixture.predecessorState fixture.predecessorState
    assertNoCoordinatorReadiness "progressed structural owners" fixture.predecessorState connected
    final <-
      foldM
        ( \predecessor advanceOracle -> do
            successor <-
              if advanceOracle
                then do
                  let progressed = fixtureHeraldAfterProbePrefix predecessor
                  assertBool
                    "the genuine Oracle entry advances its projection"
                    (startupOracleProjectionState progressed /= startupOracleProjectionState predecessor)
                  pure progressed
                else
                  fst
                    <$> checkedIO
                      "receive or replay the future terminal inventory"
                      ( verifiedStepHerald
                          ( heraldInput
                              (startupLastObservedTime predecessor)
                              (PeerInput (PeerControlReceived binding fixture.control))
                          )
                          predecessor
                      )
            assertNoCoordinatorReadiness
              (if advanceOracle then "Oracle progress" else "ahead-control arrival or replay")
              predecessor
              successor
            pure successor
        )
        connected
        history
    assertEqual
      "the generated history retains exactly one future control on its live binding"
      [(binding, fixture.control)]
      (TerminalSourceHold.heldTerminalSourceControls (startupTerminalSourceHoldState final))
    pure True
  where
    histories = do
      role <- QC.elements [Access.NeutralVertexRole, Access.EdgeRole, Access.NablaRole, Access.DeltaRole]
      replays <- QC.chooseInt (1, 6)
      history <- QC.shuffle (True : replicate replays False)
      pure (role, history)

assertNoCoordinatorReadiness :: String -> HeraldState -> HeraldState -> Assertion
assertNoCoordinatorReadiness context predecessor successor = do
  assertEqual (context <> ": predecessor admitted") (Right ()) (validateHeraldState predecessor)
  assertEqual (context <> ": successor admitted") (Right ()) (validateHeraldState successor)
  assertBool (context <> ": no successor coordinator") (startupStructuralBaseCoordinator successor == Nothing)
  (actual, effects, cuts) <-
    checkedIO context (PeerControl.redriveTerminalSourceReadinessIfProgressed predecessor successor)
  assertBool (context <> ": complete successor unchanged") (actual == successor)
  assertEqual (context <> ": effects unchanged") [] (effectBatchMembers effects)
  assertEqual (context <> ": no installed cuts") [] cuts

caseInventoryOutrunsProjection :: Assertion
caseInventoryOutrunsProjection = do
  fixture <- holdFixture
  (held, effects) <-
    checkedIO
      "retain ahead terminal inventory"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 1)
              (PeerInput (PeerControlReceived fixture.binding fixture.control))
          )
          fixture.predecessorState
      )
  assertEqual
    "the exact checked control is retained once"
    [(fixture.binding, fixture.control)]
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState held)
    )
  assertBool
    "a valid ahead-generation inventory does not reject or close the survivor"
    (all (not . rejects fixture.binding) (effectBatchMembers effects))

caseDuplicateAndResolution :: Assertion
caseDuplicateAndResolution = do
  fixture <- holdFixture
  let current = fixture.predecessorMembership
      (first, firstDisposition) =
        TerminalSourceHold.retainAheadControl
          (genesisHistory current)
          fixture.binding
          fixture.control
          TerminalSourceHold.initialState
      (duplicate, duplicateDisposition) =
        TerminalSourceHold.retainAheadControl
          (genesisHistory current)
          fixture.binding
          fixture.control
          first
  assertEqual
    "first occurrence is held"
    TerminalSourceHold.TerminalSourceControlHeld
    firstDisposition
  assertEqual
    "exact replay is recognized"
    TerminalSourceHold.TerminalSourceControlDuplicate
    duplicateDisposition
  assertEqual
    "exact replay does not widen the hold"
    [(fixture.binding, fixture.control)]
    (TerminalSourceHold.heldTerminalSourceControls duplicate)
  assertEqual
    "the matching authoritative successor releases the occurrence in arrival order"
    ( TerminalSourceHold.TerminalSourceHoldMatched
        TerminalSourceHold.initialState
        [(fixture.binding, fixture.control)]
    )
    ( TerminalSourceHold.resolveForMembershipAdvance
        (membershipHistory current fixture.successorMembership)
        duplicate
    )
  assertEqual
    "retirement of the supplying peer discards its hold without a protocol-close result"
    (TerminalSourceHold.TerminalSourceHoldMatched TerminalSourceHold.initialState [])
    ( TerminalSourceHold.resolveForMembershipAdvance
        (membershipHistory current fixture.otherSuccessorMembership)
        duplicate
    )
  let (_, staleDisposition) =
        TerminalSourceHold.retainAheadControl
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          fixture.control
          TerminalSourceHold.initialState
  assertEqual
    "the same predecessor claim is stale after projection"
    TerminalSourceHold.TerminalSourceControlRejected
    staleDisposition

caseClosureHistoryInvariant :: Assertion
caseClosureHistoryInvariant = do
  fixture <- terminalArrivalFixture
  assertEqual "the checked current closure is composed-valid" (Right ()) (validateHeraldState fixture.initialState)
  closure <- case startupStructuralBaseCoordinator fixture.initialState of
    Just retained -> pure retained
    Nothing -> assertFailure "expected a retained terminal closure"
  let origin = heraldMembershipLineageOrigin closure.lineage
      wrongHistory =
        checked
          "valid history interval with the wrong closure target"
          (heraldMembershipLineage (heraldMembershipGenerationId origin) (heraldMembershipGenerationId origin) (genesisHistory origin))
      mismatched = closure {TerminalSource.historyLineage = wrongHistory}
      corrupted = replaceStartupStructuralBaseCoordinator (Just mismatched) fixture.initialState
  assertEqual
    "an individually checked lineage cannot substitute for the closure full history"
    (Left (HeraldStartupInvariant StartupStatic StructuralProgressInvariant))
    (validateHeraldState corrupted)

caseRepeatedRetirementHolds :: Assertion
caseRepeatedRetirementHolds = do
  fixture <- holdFixture
  let origin = fixture.predecessorMembership
      first = fixture.successorMembership
      reporter = peerBindingRemoteHeraldEpoch fixture.binding
      retiredNext = case [member | member <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs first), member /= reporter] of
        [member] -> member
        _ -> error "two-survivor fixture has no unique other member"
      second =
        checked
          "second retirement for independent hold"
          (retireHeraldMembershipGeneration (controlIndex 4) (fixtureRetirementResolution (controlIndex 4)) retiredNext first)
      firstHistory = membershipHistory origin first
      secondHistory = checked "complete two-retirement history" (heraldMembershipHistory (origin NonEmpty.:| [first, second]))
      lineage = checked "successor origin to later target" (heraldMembershipLineage (heraldMembershipGenerationId first) (heraldMembershipGenerationId second) secondHistory)
      future =
        PeerTerminalSourceInventoryAdvertised
          ( checked
              "later inventory"
              (terminalSourceInventory lineage retiredNext reporter [] [] [])
          )
      (futureHeld, futureDisposition) =
        TerminalSourceHold.retainAheadControl
          (genesisHistory origin)
          fixture.binding
          future
          TerminalSourceHold.initialState
  assertEqual "G2 evidence can precede the G1 watch" TerminalSourceHold.TerminalSourceControlHeld futureDisposition
  assertEqual
    "G1 does not release or discard a G2 occurrence"
    (TerminalSourceHold.TerminalSourceHoldUnchanged futureHeld)
    (TerminalSourceHold.resolveForMembershipAdvance firstHistory futureHeld)
  let (withCurrent, readinessDisposition) = TerminalSourceHold.retainReadinessControl firstHistory fixture.binding fixture.control futureHeld
  assertEqual "an exact G1 inventory can independently await local evidence" TerminalSourceHold.TerminalSourceControlHeld readinessDisposition
  assertEqual "both independently retained phases satisfy their history invariant" (Right ()) (TerminalSourceHold.validateState firstHistory withCurrent)
  assertEqual
    "G2 releases its own occurrence and makes G1 readiness inert"
    (TerminalSourceHold.TerminalSourceHoldMatched TerminalSourceHold.initialState [(fixture.binding, future)])
    (TerminalSourceHold.resolveForMembershipAdvance secondHistory withCurrent)
  assertEqual
    "G1 readiness releases its own occurrence while preserving the later hold"
    (TerminalSourceHold.TerminalSourceReadinessMatched futureHeld [(fixture.binding, fixture.control)])
    (TerminalSourceHold.resolveForReadinessAdvance firstHistory withCurrent)
  let (_, historicalDisposition) = TerminalSourceHold.retainReadinessControl secondHistory fixture.binding fixture.control TerminalSourceHold.initialState
  assertEqual "replayed historical readiness is inert" TerminalSourceHold.TerminalSourceControlDuplicate historicalDisposition
  let compoundLineage = checked "compound inventory anchor" (heraldMembershipLineage (heraldMembershipGenerationId origin) (heraldMembershipGenerationId second) secondHistory)
      compoundInventory =
        PeerTerminalSourceInventoryAdvertised
          ( checked
              "compound earlier-anchor inventory"
              (terminalSourceInventory compoundLineage retiredNext reporter [] [] [])
          )
      (compoundHeld, compoundDisposition) = TerminalSourceHold.retainReadinessControl secondHistory fixture.binding compoundInventory TerminalSourceHold.initialState
  assertEqual "known non-immediate anchor inventory can await local proof installation" TerminalSourceHold.TerminalSourceControlHeld compoundDisposition
  assertEqual "compound readiness retains valid checked ancestry" (Right ()) (TerminalSourceHold.validateState secondHistory compoundHeld)

caseSuccessorStructuralReportHold :: Assertion
caseSuccessorStructuralReportHold = do
  fixture <- holdFixture
  let reporter = peerBindingRemoteHeraldEpoch fixture.binding
      report =
        structuralAppliedReport
          reporter
          (emptyStructuralVersionVector fixture.successorMembership)
          (controlIndex 8)
      advanced =
        structuralAppliedReport
          reporter
          (emptyStructuralVersionVector fixture.successorMembership)
          (controlIndex 9)
      regressed =
        structuralAppliedReport
          reporter
          (emptyStructuralVersionVector fixture.successorMembership)
          (controlIndex 7)
      memberEpochs =
        heraldMembershipGenerationActiveHeraldEpochs fixture.successorMembership
      members = NonEmpty.toList memberEpochs
      incomparableVector =
        checked
          "derive incomparable held successor vector"
          ( mkStructuralVersionVector
              fixture.successorMembership
              [ (member, if member == NonEmpty.head memberEpochs then structuralPrefixThrough firstStructuralSequence else emptyStructuralPrefix)
              | member <- members
              ]
          )
      incomparable =
        structuralAppliedReport
          reporter
          incomparableVector
          (controlIndex 8)
      control = PeerStructuralAppliedReported report
      advancedControl = PeerStructuralAppliedReported advanced
      (held, heldDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          report
          TerminalSourceHold.initialState
      (duplicate, duplicateDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          report
          held
      (afterAdvance, advanceDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          advanced
          duplicate
      (afterRegression, regressionDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          regressed
          afterAdvance
      (afterConflict, conflictDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          incomparable
          afterRegression
      (_, staleDisposition) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          report
          TerminalSourceHold.initialState
  assertEqual "the first exact successor report is held" TerminalSourceHold.TerminalSourceControlHeld heldDisposition
  assertEqual "an exact replay is a duplicate" TerminalSourceHold.TerminalSourceControlDuplicate duplicateDisposition
  assertEqual
    "the hold retains one occurrence per reporter"
    [(fixture.binding, control)]
    (TerminalSourceHold.heldTerminalSourceControls duplicate)
  assertEqual "a newer covering report advances the held occurrence" TerminalSourceHold.TerminalSourceControlAdvanced advanceDisposition
  assertEqual "an older covered report is inert" TerminalSourceHold.TerminalSourceControlDuplicate regressionDisposition
  assertEqual "a vector/control-incomparable report remains a conflict" TerminalSourceHold.TerminalSourceControlRejected conflictDisposition
  assertEqual "a conflict cannot widen retained work" afterRegression afterConflict
  assertEqual "advance and regression retain only the newest report" [(fixture.binding, advancedControl)] (TerminalSourceHold.heldTerminalSourceControls afterRegression)
  assertEqual "the exact installed successor releases only the latest report" (TerminalSourceHold.TerminalSourceReadinessMatched TerminalSourceHold.initialState [(fixture.binding, advancedControl)]) (TerminalSourceHold.resolveForReadinessAdvance (membershipHistory fixture.predecessorMembership fixture.successorMembership) afterConflict)
  assertEqual "a current report may await its local base" TerminalSourceHold.TerminalSourceControlHeld staleDisposition
  readiness <- readinessFixture
  let readinessReport =
        structuralAppliedReport
          (peerBindingRemoteHeraldEpoch readiness.binding)
          (emptyStructuralVersionVector readiness.successorMembership)
          (controlIndex 7)
      readinessReportControl = PeerStructuralAppliedReported readinessReport
      establishedControl = PeerTerminalSourceUnionEstablished readiness.established
      (reportFirst, _) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory readiness.predecessorMembership readiness.successorMembership)
          readiness.binding
          readinessReport
          TerminalSourceHold.initialState
      (reportThenEstablished, _) =
        TerminalSourceHold.retainReadinessControl
          (membershipHistory readiness.predecessorMembership readiness.successorMembership)
          readiness.binding
          establishedControl
          reportFirst
      (establishedFirst, _) =
        TerminalSourceHold.retainReadinessControl
          (membershipHistory readiness.predecessorMembership readiness.successorMembership)
          readiness.binding
          establishedControl
          TerminalSourceHold.initialState
      (establishedThenReport, _) =
        TerminalSourceHold.retainSuccessorStructuralReport
          (membershipHistory readiness.predecessorMembership readiness.successorMembership)
          readiness.binding
          readinessReport
          establishedFirst
  assertEqual
    "report then Established replay in causal arrival order"
    ( TerminalSourceHold.TerminalSourceReadinessMatched
        TerminalSourceHold.initialState
        [ (readiness.binding, readinessReportControl),
          (readiness.binding, establishedControl)
        ]
    )
    ( TerminalSourceHold.resolveForReadinessAdvance
        (membershipHistory readiness.predecessorMembership readiness.successorMembership)
        reportThenEstablished
    )
  assertEqual
    "an existing terminal-readiness hold composes with the exact successor report"
    [ (readiness.binding, establishedControl),
      (readiness.binding, readinessReportControl)
    ]
    (TerminalSourceHold.heldTerminalSourceControls establishedThenReport)

caseAnnounceReadinessArrivalOrder :: Assertion
caseAnnounceReadinessArrivalOrder = do
  fixture <- readinessFixture
  case StructuralBase.acceptStructuralBaseUnionAnnouncement
    unavailableTopology
    (emptyStructuralVersionVector fixture.predecessorMembership)
    (controlIndex 2)
    fixture.announce
    fixture.coordinator of
    Left
      ( StructuralBase.StructuralBaseTerminalSourceProblem
          TerminalSourceTopologyReconstructionUnavailable
        ) -> pure ()
    observed ->
      assertFailure
        ("expected typed announce readiness hold, got " <> show observed)
  let control = PeerTerminalSourceUnionAnnounced fixture.announce
      (held, firstDisposition) =
        TerminalSourceHold.retainReadinessControl
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          control
          TerminalSourceHold.initialState
      (duplicate, duplicateDisposition) =
        TerminalSourceHold.retainReadinessControl
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          control
          held
  assertEqual
    "the valid but temporarily unavailable announce is held"
    TerminalSourceHold.TerminalSourceControlHeld
    firstDisposition
  assertEqual
    "the same stable-binding occurrence is idempotent"
    TerminalSourceHold.TerminalSourceControlDuplicate
    duplicateDisposition
  assertEqual
    "an unrelated Oracle replay does not release a current-generation readiness hold"
    (TerminalSourceHold.TerminalSourceHoldUnchanged duplicate)
    ( TerminalSourceHold.resolveForMembershipAdvance
        (membershipHistory fixture.predecessorMembership fixture.successorMembership)
        duplicate
    )
  released <- case TerminalSourceHold.resolveForReadinessAdvance
    (membershipHistory fixture.predecessorMembership fixture.successorMembership)
    duplicate of
    TerminalSourceHold.TerminalSourceReadinessMatched _ [observed]
      | observed == (fixture.binding, control) -> pure observed
    observed -> assertFailure ("expected one stable announce release, got " <> show observed)
  assertEqual "readiness replay keeps the admitted binding" fixture.binding (fst released)
  (_, acceptance) <-
    checkedIO
      "accept the retained announce after topology readiness"
      ( StructuralBase.acceptStructuralBaseUnionAnnouncement
          fixture.availableTopology
          (emptyStructuralVersionVector fixture.predecessorMembership)
          (controlIndex 2)
          fixture.announce
          fixture.coordinator
      )
  assertEqual
    "exactly the receiver's acceptance is produced"
    fixture.receiver
    (terminalSourceUnionAcceptanceReporter acceptance)
  assertEqual
    "clearing before replay makes the discharge one-shot"
    ( TerminalSourceHold.TerminalSourceReadinessUnchanged
        TerminalSourceHold.initialState
    )
    ( TerminalSourceHold.resolveForReadinessAdvance
        (membershipHistory fixture.predecessorMembership fixture.successorMembership)
        ( TerminalSourceHold.consumeReadinessControl
            fixture.binding
            control
            duplicate
        )
    )

caseEstablishedReadinessArrivalOrder :: Assertion
caseEstablishedReadinessArrivalOrder = do
  fixture <- readinessFixture
  case StructuralBase.receiveStructuralBaseUnionEstablished
    unavailableTopology
    (emptyStructuralVersionVector fixture.predecessorMembership)
    (controlIndex 2)
    fixture.established
    fixture.acceptedCoordinator of
    Left
      ( StructuralBase.StructuralBaseTerminalSourceProblem
          TerminalSourceTopologyReconstructionUnavailable
        ) -> pure ()
    observed ->
      assertFailure
        ("expected typed established readiness hold, got " <> show observed)
  let control = PeerTerminalSourceUnionEstablished fixture.established
      (held, disposition) =
        TerminalSourceHold.retainReadinessControl
          (membershipHistory fixture.predecessorMembership fixture.successorMembership)
          fixture.binding
          control
          TerminalSourceHold.initialState
  assertEqual
    "valid established evidence is retained without rejecting its binding"
    TerminalSourceHold.TerminalSourceControlHeld
    disposition
  case TerminalSourceHold.resolveForReadinessAdvance
    (membershipHistory fixture.predecessorMembership fixture.successorMembership)
    held of
    TerminalSourceHold.TerminalSourceReadinessMatched _ [(observedBinding, observedControl)] -> do
      assertEqual "established replay keeps the same binding" fixture.binding observedBinding
      assertEqual "established replay keeps the exact evidence" control observedControl
    observed -> assertFailure ("expected one established release, got " <> show observed)
  received <-
    checkedIO
      "receive retained established evidence after topology readiness"
      ( StructuralBase.receiveStructuralBaseUnionEstablished
          fixture.availableTopology
          (emptyStructuralVersionVector fixture.predecessorMembership)
          (controlIndex 2)
          fixture.established
          fixture.acceptedCoordinator
      )
  cut <-
    checkedIO
      "derive the exact successor cut once"
      (StructuralBase.expectedSuccessorStructuralCut received)
  installed <-
    checkedIO
      "install the released successor base once"
      (StructuralBase.recordInstalledSuccessorStructuralCut cut received)
  assertBool
    "the released established evidence yields successor-base evidence"
    (StructuralBase.structuralBaseEvidence installed /= Nothing)
  assertEqual
    "the consumed hold cannot install a second time"
    ( TerminalSourceHold.TerminalSourceReadinessUnchanged
        TerminalSourceHold.initialState
    )
    ( TerminalSourceHold.resolveForReadinessAdvance
        (membershipHistory fixture.predecessorMembership fixture.successorMembership)
        ( TerminalSourceHold.consumeReadinessControl
            fixture.binding
            control
            held
        )
    )

caseAnnounceBeforeInventoryAndPayload :: Assertion
caseAnnounceBeforeInventoryAndPayload = do
  fixture <- terminalArrivalFixture
  (heldBeforeInventory, announceEffects) <-
    stepTerminalControl
      "retain announce before its inventory"
      10
      fixture.binding
      (PeerTerminalSourceUnionAnnounced fixture.announce)
      fixture.initialState
  assertNoPeerClose "announce before inventory" fixture.binding announceEffects
  assertEqual
    "the remote announce is retained exactly once while its reporter is absent"
    [(fixture.binding, PeerTerminalSourceUnionAnnounced fixture.announce)]
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState heldBeforeInventory)
    )

  (heldBeforePayload, inventoryEffects) <-
    stepTerminalControl
      "receive the delayed terminal inventory"
      11
      fixture.binding
      (PeerTerminalSourceInventoryAdvertised fixture.inventory)
      heldBeforeInventory
  assertNoPeerClose "inventory after announce" fixture.binding inventoryEffects
  assertEqual
    "the announce and its supporting inventory retain their exact missing payload"
    [ (fixture.binding, PeerTerminalSourceUnionAnnounced fixture.announce),
      (fixture.binding, PeerTerminalSourceInventoryAdvertised fixture.inventory)
    ]
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState heldBeforePayload)
    )
  assertEqual
    "inventory arrival emits the one deterministic repair request"
    [fixture.request]
    [ request
    | SendPeerControl observedBinding (PeerTerminalSourcePayloadRequested request) <-
        effectBatchMembers inventoryEffects,
      observedBinding == fixture.binding
    ]

  (released, relayEffects) <-
    stepTerminalControl
      "receive the requested terminal payload"
      12
      fixture.binding
      (PeerTerminalSourcePayloadRelayed fixture.relay)
      heldBeforePayload
  assertNoPeerClose "payload after retained announce" fixture.binding relayEffects
  assertEqual
    "the payload edge consumes the retained announce"
    []
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState released)
    )
  assertEqual
    "the retained announce produces exactly one acceptance"
    [fixture.announce]
    [ fixture.announce
    | SendPeerControl observedBinding (PeerTerminalSourceUnionAccepted _) <-
        effectBatchMembers relayEffects,
      observedBinding == fixture.binding
    ]
  assertBool
    "the relayed occurrence is materialized before the announce is discharged"
    ( GraphProgress.lookupAppliedStructuralOccurrence
        (terminalStructuralOccurrenceId fixture.occurrence)
        (startupStructuralProgressState released)
        /= Nothing
    )
  assertEqual
    "the retained terminal archive is valid checked publication and route provenance"
    (Right ())
    (validateHeraldState released)

casePayloadArchiveHeldBeforeSourceTopology :: Assertion
casePayloadArchiveHeldBeforeSourceTopology = do
  fixture <- terminalArrivalFixture
  (withInventory, inventoryEffects) <-
    stepTerminalControl
      "receive topology-held payload inventory"
      20
      fixture.binding
      (PeerTerminalSourceInventoryAdvertised fixture.topologyHeldInventory)
      fixture.initialState
  assertNoPeerClose
    "topology-held payload inventory"
    fixture.binding
    inventoryEffects
  let predecessorStore = startupStoreState withInventory
      predecessorProgress = startupStructuralProgressState withInventory
      predecessorGraph = startupGraphState withInventory
  (held, relayEffects) <-
    stepTerminalControl
      "retain payload before source topology"
      21
      fixture.binding
      (PeerTerminalSourcePayloadRelayed fixture.topologyHeldRelay)
      withInventory
  assertNoPeerClose
    "payload before source topology"
    fixture.binding
    relayEffects
  coordinator <-
    maybe
      (assertFailure "topology-held relay lost its structural-base coordinator")
      pure
      (startupStructuralBaseCoordinator held)
  assertEqual
    "the exact relay remains retained as pending repair provenance"
    [ ( terminalStructuralOccurrenceId fixture.topologyHeldOccurrence,
        fixture.topologyHeldOccurrence
      )
    ]
    (StructuralBase.structuralBasePayloadEntries coordinator)
  assertBool
    "the unready relay cannot write its mandatory Store receipt"
    (startupStoreState held == predecessorStore)
  assertBool
    "the unready relay cannot advance structural progress"
    (startupStructuralProgressState held == predecessorProgress)
  assertBool
    "the unready relay cannot mutate Graph"
    (startupGraphState held == predecessorGraph)
  assertEqual
    "the unready occurrence is not structurally applied"
    Nothing
    ( GraphProgress.lookupAppliedStructuralOccurrence
        (terminalStructuralOccurrenceId fixture.topologyHeldOccurrence)
        (startupStructuralProgressState held)
    )
  assertEqual
    "the exact held archive remains a valid whole-state owner"
    (Right ())
    (validateHeraldState held)

caseExactDuplicatePayloadRelay :: Assertion
caseExactDuplicatePayloadRelay = do
  fixture <- terminalArrivalFixture
  (withInventory, _) <-
    stepTerminalControl
      "receive duplicate-relay supporting inventory"
      20
      fixture.binding
      (PeerTerminalSourceInventoryAdvertised fixture.inventory)
      fixture.initialState
  (withPayload, _) <-
    stepTerminalControl
      "receive the first exact terminal payload relay"
      21
      fixture.binding
      (PeerTerminalSourcePayloadRelayed fixture.relay)
      withInventory
  (afterDuplicate, duplicateEffects) <-
    stepTerminalControl
      "replay the exact terminal payload relay"
      22
      fixture.binding
      (PeerTerminalSourcePayloadRelayed fixture.relay)
      withPayload
  assertNoPeerClose "exact payload relay replay" fixture.binding duplicateEffects
  assertEqual
    "an exact archived relay emits no second acknowledgement or repair work"
    []
    (effectBatchMembers duplicateEffects)
  assertEqual
    "the duplicate preserves the terminal coordinator exactly"
    (startupStructuralBaseCoordinator withPayload)
    (startupStructuralBaseCoordinator afterDuplicate)
  assertEqual
    "the duplicate cannot materialize the structural occurrence twice"
    (startupStructuralProgressState withPayload)
    (startupStructuralProgressState afterDuplicate)
  assertBool
    "the duplicate cannot mutate Graph twice"
    (startupGraphState withPayload == startupGraphState afterDuplicate)

data TerminalArrivalFixture = TerminalArrivalFixture
  { initialState :: HeraldState,
    binding :: PeerBinding,
    inventory :: TerminalSourceInventory,
    request :: TerminalSourcePayloadRequest,
    relay :: TerminalSourcePayloadRelay,
    announce :: TerminalSourceUnionAnnounce,
    occurrence :: TerminalStructuralOccurrence,
    topologyHeldInventory :: TerminalSourceInventory,
    topologyHeldRelay :: TerminalSourcePayloadRelay,
    topologyHeldOccurrence :: TerminalStructuralOccurrence
  }

terminalArrivalFixture :: IO TerminalArrivalFixture
terminalArrivalFixture = do
  let third =
        HeraldMember
          (checked "terminal-arrival local Herald id" (mkHeraldId (fixtureIdentifierBytes 0x71)))
          (checked "terminal-arrival local Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x72)))
      members = [fixtureLocalMember, fixtureRemoteMember, third]
      baseDeployment = fixtureDeploymentAt third
      deployment =
        baseDeployment
          { deploymentActiveHeralds = members,
            deploymentOracleGenesis =
              (deploymentOracleGenesis baseDeployment)
                { oracleGenesisActiveHeralds = members
                }
          }
      genesis =
        checked "terminal-arrival Herald genesis" (checkHeraldGenesis deployment)
      selectedBootstraps = take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
      bootstraps =
        checked
          "terminal-arrival initial bootstraps"
          (checkInitialBootstraps genesis (PrimordialProcessManifest selectedBootstraps))
      initial =
        fst
          ( checked
              "initialize terminal-arrival receiver"
              ( initialHerald
                  (monotonicInstant 0)
                  genesis
                  bootstraps
                  fixtureOracleContacts
                  fixtureGeneratorSeed
                  fixtureApplicationRecoveryConfiguration
                  fixturePeerRecoveryConfiguration
              )
          )
      announcerMember = fixtureLocalMember
      retired = heraldMemberEpoch fixtureRemoteMember
  oracleBound <- bindOracle initial
  (beforePrefix, binding) <- bindPeer announcerMember oracleBound
  let connected = fixtureHeraldAfterProbePrefix beforePrefix
  let projection = startupOracleProjectionState connected
      view = OracleProjection.oracleView projection
      predecessorMembership =
        OracleProjection.oracleViewCurrentHeraldMembership view
      retirementIndex = controlIndex 2
      successorMembership =
        checked
          "terminal-arrival successor membership"
          (retireHeraldMembershipGeneration retirementIndex (fixtureRetirementResolution retirementIndex) retired predecessorMembership)
      processEnds =
        [ OracleProjection.membershipAdvanceProcessEnd
            process
            retirementIndex
            fixtureHeraldRetirementReason
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleViewProcessResidence process view == Just retired,
          OracleProjection.oracleViewProcessIsLive process view
        ]
  preparedAdvance <-
    checkedIO
      "prepare terminal-arrival membership advance"
      ( MembershipAdvance.prepareMembershipAdvance
          retirementIndex
          successorMembership
          processEnds
          connected
      )
  let afterMembership = MembershipAdvance.commitMembershipAdvance preparedAdvance
      predecessorCut =
        GraphProgress.structuralLastInstalledCutId
          (startupStructuralProgressState connected)
      unavailableSourceTopology =
        checked
          "terminal-arrival unavailable source topology"
          (mkTopologyCutId (fixtureIdentifierBytes 0xf3))
  occurrence <-
    terminalOccurrence
      bootstraps
      retired
      predecessorCut
      connected
  topologyHeldOccurrence <-
    terminalOccurrence
      bootstraps
      retired
      unavailableSourceTopology
      connected
  coordinator <-
    checkedIO
      "begin terminal-arrival coordinator"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          connected
          afterMembership
      )
  let initialState =
        replaceStartupStructuralBaseCoordinator (Just coordinator) afterMembership
      announcer = heraldMemberEpoch announcerMember
      localInventory = case StructuralBase.structuralBaseLocalInventories coordinator of
        [singleInventory] -> singleInventory
        _ -> error "direct retirement fixture has no unique local inventory"
      (inventory, archive) =
        checked
          "seal announcer terminal inventory"
          ( sealTerminalSourceInventory
              (membershipLineage predecessorMembership successorMembership)
              (membershipLineage predecessorMembership successorMembership)
              retired
              announcer
              []
              []
              [occurrence]
          )
      (topologyHeldInventory, _) =
        checked
          "seal topology-held terminal inventory"
          ( sealTerminalSourceInventory
              (membershipLineage predecessorMembership successorMembership)
              (membershipLineage predecessorMembership successorMembership)
              retired
              announcer
              []
              []
              [topologyHeldOccurrence]
          )
      predecessorBase =
        checked
          "terminal-arrival predecessor base"
          (terminalSourceGenesisPredecessorBase predecessorMembership predecessorCut)
  shadowOwners <-
    materializedTerminalOwners initialState occurrence
  let union =
        checked
          "derive terminal-arrival exact union"
          ( deriveTerminalSourceUnionChecked
              (membershipLineage predecessorMembership successorMembership)
              (membershipLineage predecessorMembership successorMembership)
              predecessorBase
              [localInventory, inventory]
              archive
              (terminalTopologyAt initialState shadowOwners)
          )
      announce =
        checked
          "derive terminal-arrival announce"
          (terminalSourceUnionAnnounce successorMembership announcer union)
  coordinatorWithInventory <-
    checkedIO
      "retain terminal-arrival remote inventory"
      (StructuralBase.receiveStructuralBaseInventory inventory coordinator)
  requests <-
    checkedIO
      "derive terminal-arrival payload request"
      (StructuralBase.structuralBasePayloadRequests coordinatorWithInventory)
  request <- case requests of
    [single] -> pure single
    observed ->
      assertFailure
        ("expected one terminal-arrival payload request, got " <> show observed)
  let relay =
        checked
          "derive terminal-arrival payload relay"
          ( terminalSourcePayloadRelay
              (membershipLineage predecessorMembership successorMembership)
              (membershipLineage predecessorMembership successorMembership)
              announcer
              request
              inventory
              occurrence
          )
  coordinatorWithTopologyHeldInventory <-
    checkedIO
      "retain topology-held terminal inventory"
      ( StructuralBase.receiveStructuralBaseInventory
          topologyHeldInventory
          coordinator
      )
  topologyHeldRequests <-
    checkedIO
      "derive topology-held terminal payload request"
      (StructuralBase.structuralBasePayloadRequests coordinatorWithTopologyHeldInventory)
  topologyHeldRequest <- case topologyHeldRequests of
    [single] -> pure single
    observed ->
      assertFailure
        ("expected one topology-held terminal payload request, got " <> show observed)
  let topologyHeldRelay =
        checked
          "derive topology-held terminal payload relay"
          ( terminalSourcePayloadRelay
              (membershipLineage predecessorMembership successorMembership)
              (membershipLineage predecessorMembership successorMembership)
              announcer
              topologyHeldRequest
              topologyHeldInventory
              topologyHeldOccurrence
          )
  pure
    TerminalArrivalFixture
      { initialState,
        binding,
        inventory,
        request,
        relay,
        announce,
        occurrence,
        topologyHeldInventory,
        topologyHeldRelay,
        topologyHeldOccurrence
      }

terminalOccurrence ::
  CheckedInitialBootstraps ->
  HeraldEpoch ->
  TopologyCutId ->
  HeraldState ->
  IO TerminalStructuralOccurrence
terminalOccurrence bootstraps retired sourceTopology predecessor = do
  source <- case filter ((== retired) . appliedProcessResidence) (checkedInitialBootstraps bootstraps) of
    [single] -> pure single
    observed ->
      assertFailure
        ("expected one retired-source process, got " <> show (length observed))
  root <- requireNeutralWriter source
  object <-
    checkedIO
      "terminal-arrival neutral object"
      (mkGlobalObjectId (fixtureIdentifierBytes 0xe1))
  objectField <- checkedIO "terminal-arrival object field" (mkFieldName "object_id")
  labelField <- checkedIO "terminal-arrival label field" (mkFieldName "label")
  value <-
    checkedIO
      "terminal-arrival neutral value"
      ( recordValue
          [ ( objectField,
              globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
            ),
            (labelField, labelValue (VoidLabel, 0))
          ]
      )
  sourceNabla <- case appliedRootRole root of
    WriterRoot writer _ -> pure writer
    _ -> assertFailure "terminal-arrival root unexpectedly became a reader"
  let identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          retired
          (nablaSequence 1)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
  publication <-
    checkedIO
      "terminal-arrival checked publication"
      (mkCheckedPublication descriptor identifier value)
  payload <-
    checkedIO
      "terminal-arrival structural transcript"
      ( structuralPublicationCanonicalBytesForSemantics
          descriptor
          (appliedRootOccurrenceId root)
          publication
          (appliedProcessEpochId source)
          Normal
          sourceTopology
          (appliedRootControlPrerequisite root)
      )
  stamp <-
    checkedIO
      "terminal-arrival structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId retired firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (startupStructuralProgressState predecessor)
          )
          identifier
          (deriveTerminalStructuralPayloadDigest payload)
          NeutralVertexCarrier
      )
  checkedIO
    "terminal-arrival structural occurrence"
    (terminalStructuralOccurrence stamp payload)

requireNeutralWriter :: AppliedProcessBootstrap -> IO AppliedRoot
requireNeutralWriter source =
  maybe
    (assertFailure "retired-source process has no neutral-vertex writer")
    pure
    ( find
        ( \root ->
            appliedRootCatalogueRole root == NeutralVertexRole
              && case appliedRootRole root of
                WriterRoot _ _ -> True
                _ -> False
        )
        (appliedProcessRoots source)
    )

materializedTerminalOwners ::
  HeraldState ->
  TerminalStructuralOccurrence ->
  IO PeerInput.PeerInputState
materializedTerminalOwners state occurrence = do
  (owners, result) <-
    checkedIO
      "materialize terminal-arrival digest witness"
      ( PeerInput.materializeTerminalStructuralOccurrence
          ( PeerInput.peerInputContext
              (startupGenesis state)
              (startupOracleProjectionState state)
              (startupDiscoveryState state)
              (startupPlacementState state)
          )
          occurrence
          (peerInputOwners state)
      )
  assertEqual
    "the digest witness uses a real terminal materialization"
    PeerInput.TerminalStructuralMaterialized
    (PeerInput.terminalStructuralMaterializationDisposition result)
  pure owners

peerInputOwners :: HeraldState -> PeerInput.PeerInputState
peerInputOwners state =
  PeerInput.peerInputState
    (startupPublicationState state)
    (startupPeerStreamState state)
    (startupStoreState state)
    (startupGraphState state)
    (startupIdGeneratorState state)
    (startupStructuralProgressState state)
    (startupAlignmentState state)
    (startupPlacementState state)
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupApplicationState state)
    (startupWaitState state)
    (startupLabelBarrierState state)
    (startupDisappearanceState state)

terminalTopologyAt ::
  HeraldState ->
  PeerInput.PeerInputState ->
  StructuralBase.TopologyDigestReconstructor
terminalTopologyAt state owners vector control =
  case GraphProgress.structuralTopologyOccurrenceDigestAt
    (terminalReconciliationViews state owners)
    vector
    control
    (PeerInput.peerInputStructuralProgressState owners) of
    Right (Right digest) -> Right digest
    _ -> Left TerminalSourceTopologyReconstructionUnavailable

terminalReconciliationViews ::
  HeraldState ->
  PeerInput.PeerInputState ->
  Reconciliation.ReconciliationViews
terminalReconciliationViews state owners =
  Reconciliation.reconciliationViews
    (checkedLocalHeraldEpoch (startupGenesis state))
    ( Map.fromList
        [ ( SortRegistry.registryEntrySortId entry,
            sortOccurrence
              (SortRegistry.registryEntrySortId entry)
              (SortRegistry.registryEntryOccurrenceId entry)
          )
        | entry <-
            SortRegistry.registryEntries
              (PeerInput.peerInputSortRegistryState owners)
        ]
    )
    ( Set.fromList
        ( Graph.graphNonStructuralBaselineVertices
            (PeerInput.peerInputGraphState owners)
        )
    )
    ( Map.fromList
        [ (process, residence)
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleViewProcessIsLive process view,
          Just residence <- [OracleProjection.oracleViewProcessResidence process view]
        ]
    )
    ( Set.fromList
        ( Controlled.controlledNormalPossessionEntries
            (PeerInput.peerInputControlledState owners)
        )
    )
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection

stepTerminalControl ::
  String ->
  Word ->
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
stepTerminalControl context observedAt binding control =
  checkedIO
    context
    . verifiedStepHerald
      ( heraldInput
          (monotonicInstant (fromIntegral observedAt))
          (PeerInput (PeerControlReceived binding control))
      )

assertNoPeerClose ::
  String ->
  PeerBinding ->
  EffectBatch ->
  Assertion
assertNoPeerClose context binding effects =
  assertBool
    (context <> " preserves the admitted peer binding")
    (all (not . rejects binding) (effectBatchMembers effects))

data ReadinessFixture = ReadinessFixture
  { predecessorMembership :: HeraldMembershipGeneration,
    successorMembership :: HeraldMembershipGeneration,
    receiver :: HeraldEpoch,
    binding :: PeerBinding,
    announce :: TerminalSourceUnionAnnounce,
    established :: TerminalSourceUnionEstablished,
    coordinator :: StructuralBase.MembershipBaseClosure,
    acceptedCoordinator :: StructuralBase.MembershipBaseClosure,
    availableTopology :: StructuralBase.TopologyDigestReconstructor
  }

readinessFixture :: IO ReadinessFixture
readinessFixture = do
  hold <- holdFixture
  members <- case NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs hold.predecessorMembership) of
    [retired, announcer, receiver] -> pure (retired, announcer, receiver)
    observed -> assertFailure ("expected three readiness members, got " <> show observed)
  let (retired, announcer, receiver) = members
      successor =
        checked
          "derive readiness successor"
          (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution (controlIndex 2)) retired hold.predecessorMembership)
      predecessorBase =
        checked
          "derive readiness predecessor base"
          ( terminalSourceGenesisPredecessorBase
              hold.predecessorMembership
              (checked "readiness predecessor cut" (mkTopologyCutId (fixtureIdentifierBytes 0xd1)))
          )
      inventories =
        [ checked
            "derive readiness inventory"
            ( terminalSourceInventory
                (membershipLineage hold.predecessorMembership successor)
                retired
                reporter
                []
                []
                []
            )
        | reporter <- [announcer, receiver]
        ]
      topologyDigest =
        checked
          "derive readiness topology digest"
          (mkTopologyOccurrenceDigest (fixtureIdentifierBytes 0xd2))
      availableTopology _ _ = Right topologyDigest
      union =
        checked
          "derive empty readiness union"
          ( deriveTerminalSourceUnion
              (membershipLineage hold.predecessorMembership successor)
              (membershipLineage hold.predecessorMembership successor)
              predecessorBase
              inventories
              emptyTerminalSourcePayloadArchive
              (\_ _ -> topologyDigest)
          )
      announce =
        checked
          "derive readiness announce"
          (terminalSourceUnionAnnounce successor announcer union)
      ready =
        checked
          "derive ready union coordinate"
          ( terminalSourceUnionReady
              (emptyStructuralVersionVector hold.predecessorMembership)
              (controlIndex 2)
              union
          )
      acceptances =
        [ checked
            "derive readiness acceptance"
            (terminalSourceUnionAcceptance successor reporter ready)
        | reporter <- [announcer, receiver]
        ]
      established =
        checked
          "derive established readiness union"
          (terminalSourceUnionEstablished successor union acceptances)
      initialCoordinator =
        checked
          "begin readiness coordinator"
          ( StructuralBase.beginMembershipBaseClosure
              (membershipLineage hold.predecessorMembership successor)
              (membershipLineage hold.predecessorMembership successor)
              receiver
              predecessorBase
              []
              []
              []
          )
  coordinator <-
    foldM
      (\state inventory -> checkedIO "retain readiness inventory" (StructuralBase.receiveStructuralBaseInventory inventory state))
      initialCoordinator
      inventories
  (acceptedCoordinator, _) <-
    checkedIO
      "prepare receiver coordinator for established arrival"
      ( StructuralBase.acceptStructuralBaseUnionAnnouncement
          availableTopology
          (emptyStructuralVersionVector hold.predecessorMembership)
          (controlIndex 2)
          announce
          coordinator
      )
  pure
    ReadinessFixture
      { predecessorMembership = hold.predecessorMembership,
        successorMembership = successor,
        receiver,
        binding = hold.binding,
        announce,
        established,
        coordinator,
        acceptedCoordinator,
        availableTopology
      }

unavailableTopology :: StructuralBase.TopologyDigestReconstructor
unavailableTopology _ _ = Left TerminalSourceTopologyReconstructionUnavailable

data HoldFixture = HoldFixture
  { predecessorState :: HeraldState,
    binding :: PeerBinding,
    control :: PeerControl,
    predecessorMembership :: HeraldMembershipGeneration,
    successorMembership :: HeraldMembershipGeneration,
    otherSuccessorMembership :: HeraldMembershipGeneration
  }

holdFixture :: IO HoldFixture
holdFixture = do
  let initial =
        fst
          ( checked
              "initialize three-Herald fixture"
              ( initialHerald
                  (monotonicInstant 0)
                  fixtureStep14CheckedGenesis
                  fixtureCheckedInitialBootstraps
                  fixtureOracleContacts
                  fixtureGeneratorSeed
                  fixtureApplicationRecoveryConfiguration
                  fixturePeerRecoveryConfiguration
              )
          )
      members = checkedActiveHeralds fixtureStep14CheckedGenesis
  (reporter, retired) <- case members of
    [_local, suppliedReporter, suppliedRetired] ->
      pure (suppliedReporter, suppliedRetired)
    observed -> assertFailure ("expected three Herald members, got " <> show observed)
  (connected, binding) <- bindPeer reporter initial
  let predecessorMembership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState connected))
      successorMembership =
        checked
          "derive advertised retirement successor"
          ( retireHeraldMembershipGeneration
              (controlIndex 2)
              (fixtureRetirementResolution (controlIndex 2))
              (heraldMemberEpoch retired)
              predecessorMembership
          )
      otherSuccessorMembership =
        checked
          "derive different authoritative successor"
          ( retireHeraldMembershipGeneration
              (controlIndex 2)
              (fixtureRetirementResolution (controlIndex 2))
              (heraldMemberEpoch reporter)
              predecessorMembership
          )
      inventory =
        checked
          "derive empty survivor inventory"
          ( terminalSourceInventory
              (membershipLineage predecessorMembership successorMembership)
              (heraldMemberEpoch retired)
              (heraldMemberEpoch reporter)
              []
              []
              []
          )
  pure
    HoldFixture
      { predecessorState = connected,
        binding,
        control = PeerTerminalSourceInventoryAdvertised inventory,
        predecessorMembership,
        successorMembership,
        otherSuccessorMembership
      }

bindOracle :: HeraldState -> IO HeraldState
bindOracle state = do
  let client = startupOracleClientState state
  attempt <- case OracleClient.oracleClientActions client of
    [ConnectAndHelloOracle current _] -> pure current
    actions ->
      assertFailure
        ( "expected one initial Oracle connection attempt, got "
            <> show actions
        )
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (controlIndex 0)
          (Just node)
          True
  prepared <-
    checkedIO
      "bind terminal-arrival Oracle client"
      ( OracleClient.prepareClientIngress
          (OracleHelloReceived attempt acceptance)
          client
      )
  pure
    ( replaceStartupOracleClientState
        (OracleClient.commitClientIngress prepared)
        state
    )

bindPeer :: HeraldMember -> HeraldState -> IO (HeraldState, PeerBinding)
bindPeer member state = do
  let remoteId = heraldMemberId member
      remoteEpoch = heraldMemberEpoch member
      nonce = connectionNonce 77
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
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
  (successor, effects) <-
    checkedIO
      "admit terminal-source survivor peer"
      ( PeerControl.applyPeerHello
          candidate
          Set.empty
          hello
          (heraldMembershipGenerationId membership)
          (heraldMembershipGenerationActiveMemberSetDigest membership)
          Nothing
          state
      )
  binding <- case [ observed
                  | SetPeerCandidateDisposition _ (PeerHelloAccepted _ observed) <-
                      effectBatchMembers effects
                  ] of
    [observed] -> pure observed
    observed -> assertFailure ("expected one accepted peer binding, got " <> show observed)
  assertEqual
    "Discovery retains the accepted binding"
    (Just binding)
    (Discovery.currentPeerBinding remoteEpoch (startupDiscoveryState successor))
  pure (successor, binding)

rejects :: PeerBinding -> HeraldEffect -> Bool
rejects expected effect = case effect of
  RejectPeerConnection observed _ -> observed == expected
  ClosePeerBinding observed -> observed == expected
  _ -> False

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

-- These fixtures retain the complete canonical ancestry, even for a direct
-- contraction, instead of treating two unrelated generation values as proof.
genesisHistory :: HeraldMembershipGeneration -> HeraldMembershipHistory
genesisHistory origin = checked "genesis membership history" (heraldMembershipHistory (origin NonEmpty.:| []))

membershipHistory :: HeraldMembershipGeneration -> HeraldMembershipGeneration -> HeraldMembershipHistory
membershipHistory origin target = checked "direct membership history" (heraldMembershipHistory (origin NonEmpty.:| [target]))

membershipLineage :: HeraldMembershipGeneration -> HeraldMembershipGeneration -> HeraldMembershipLineage
membershipLineage origin target = checked "checked fixture lineage" (heraldMembershipLineage (heraldMembershipGenerationId origin) (heraldMembershipGenerationId target) (membershipHistory origin target))
