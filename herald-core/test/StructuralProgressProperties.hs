{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module StructuralProgressProperties
  ( tests,
    assertCoverageAgainstReference,
    assertInstalledCoverageAgainstReference,
    assertPortableProgressImport,
    assertIndependentProgressEvidence,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Codec
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Graph (VertexId (NablaVertex))
import Eclips.Domain.Identity
  ( AuthorityEpochView (StructuralAuthorityEpochView),
    ControlIndex,
    HeraldEpoch,
    StructuralOccurrenceId,
    StructuralSequence,
    SystemId,
    TopologyCutId,
    authorityEpochView,
    controlIndex,
    controlIndexWord64,
    genesisAuthorityEpoch,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkStructuralSequence,
    mkSystemId,
    mkTopologyCutId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    publicationId,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Label (releasedLabel)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    deriveHeraldAdmissionId,
    genesisHeraldMembershipGeneration,
    heraldAdmissionIdCanonicalBytes,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication (mkCheckedPublication)
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (NablaCarrier, NeutralVertexCarrier))
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (NablaRole, NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    initialProjectionDigestBytes,
    mkInitialProjectionDigest,
    predefinedOccurrenceFor,
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVersionVector,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorEntries,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (..),
    labelReleaseCause,
    predefinedDisappearanceCause,
    processEndCause,
    structuralConsequenceCauseView,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology
  ( TopologyPredecessorView (..),
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyCutFrontier,
    topologyCutOccurrenceDigest,
    topologyCutPredecessor,
    topologyFrontier,
    topologyFrontierAppliedControlPrefix,
    topologyFrontierStructuralVersionVector,
    topologyPredecessorCutId,
    topologyPredecessorView,
  )
import Eclips.Domain.Value
  ( LabelOwner (ProcessLabel, VoidLabel),
    bytesValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Discovery.Internal
  ( BindingAdmission (FirstBinding),
    ConnectionNonce (ConnectionNonce),
    PeerBinding (PeerBinding),
    PeerCandidate (PeerCandidate),
    PeerHelloDisposition (PeerHelloAccepted),
    firstPeerBindingGeneration,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendPeerControl, SetPeerCandidateDisposition),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedPredefinedOccurrenceSet,
  )
import Eclips.Herald.Graph.Progress
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAnnounce,
    topologyCutAnnounceCut,
    topologyCutAnnounceId,
    topologyCutEstablished,
    topologyCutEstablishedAck,
    topologyCutEstablishedCanonicalBytes,
    topologyCutEstablishedCut,
    topologyCutEstablishedId,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Input
  ( PeerControl
      ( PeerStructuralAppliedReported,
        PeerTopologyCutAccepted,
        PeerTopologyCutEstablished
      ),
  )
import Eclips.Herald.PeerPublication
  ( mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
  )
import Eclips.Herald.Structural.Debt
  ( emptyStructuralDebtSet,
    sortOccurrence,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Oracle.Admission qualified as Admission
import GenesisFixtures (fixtureCheckedGenesis, fixtureIdentifierBytes)
import GenesisFixtures qualified as MembershipFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "structural progress and topology cuts"
    [ testCase
        "the full report matrix drives one retained announce/accept/establish/ack chain"
        caseCutChain,
      QC.testProperty
        "portable progress preserves old cuts and source links without donor participation"
        propStructuralProgressBase,
      testCase
        "local resource refresh preserves admitted meanings at old and new certified cuts"
        caseCarrierMeaningRefreshPreservesCuts,
      testCase
        "portable progress requires a fresh observer and its exact paired carrier history"
        caseStructuralProgressBaseAdmission,
      testCase
        "an unchanged join checkpoint remains valid while its cut is open"
        caseOpenJoinCheckpoint,
      QC.testProperty
        "preparation notifications coalesce actual control advances and ignore report/replay bookkeeping"
        propPreparationControlChanges,
      QC.testProperty
        "topology projection reuse agrees with reconstruction across repeated and cursor-only reports"
        propTopologyProjectionReuse,
      testCase
        "topology projection reuse observes baseline inputs and immutable sort validation"
        caseTopologyProjectionViewInputs,
      QC.testProperty
        "topology projection reuse is invalidated by retained label and End consequences"
        (QC.forAll (QC.chooseInt (0, 12)) (\offset -> QC.ioProperty (caseTopologyProjectionControlChanges offset >> pure True))),
      QC.testProperty
        "every cause selects its first covering cut across generated checked chains"
        propEarliestCoveringCut,
      testCase
        "one reporter cannot replace its vector with an incomparable successor"
        caseIncomparableReport,
      testCase
        "candidate cuts reject causal holes and component regression"
        caseCandidateFrontierMutants,
      QC.testProperty
        "candidate summaries preserve exhaustive closure and control rejection order"
        propCandidateOccurrenceSummary,
      testCase
        "a held established cut releases after local frontier progress without another wire delivery"
        caseHeldEstablishedRelease,
      testCase
        "an ordered held Established chain releases without another reconnect"
        caseHeldEstablishedChainRelease,
      testCase
        "empty occurrence debt retention is exact and idempotent"
        caseEmptyDebtRetention,
      testCase
        "one transition emits only its final snapshot for incomparable intermediate reports"
        caseNormalizeIncomparableReportSlots,
      testCase
        "the normalized final report keeps the first report slot before topology controls"
        caseNormalizeReportOrdering,
      testCase
        "a newly accepted binding defers structural reporting"
        caseAcceptedBindingDefersReport,
      testCase
        "an exact already-offered final report remains suppressed"
        caseExactFinalReportSuppressed,
      testCase
        "four members send cumulative reports only to the announcer"
        caseAnnouncerOnlyReports,
      QC.testProperty
        "direct reports preserve the all-member stable frontier across cursor and delivery orders"
        propDirectReportStableFrontier,
      testCase
        "joining observers cannot report before their structural base"
        caseJoiningObserverReport,
      testCase
        "a predecessor-generation report remains before successor-generation normalization"
        casePredecessorReportSlotPreserved
    ]

propStructuralProgressBase :: QC.Property
propStructuralProgressBase =
  QC.forAll (QC.chooseInt (2, 6)) $ \count -> QC.ioProperty $ do
    fixture <- progressFixture
    system <- checked "base system" (mkSystemId (fixtureIdentifierBytes 0x31))
    applicant <- checked "base applicant" (mkHeraldEpoch (fixtureIdentifierBytes 0x81))
    let views herald =
          Reconciliation.reconciliationViews
            herald
            (Map.fromList [(profileSortFor role, sortOccurrence (profileSortFor role) (predefinedOccurrenceFor role (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))) | role <- [NeutralVertexRole, NablaRole]])
            Set.empty
            Map.empty
            Set.empty
        empty = Reconciliation.emptyStructuralAppliedState fixture.emptyVector
    observer <- checked "fresh base observer" (initialJoiningStructuralProgressState system applicant fixture.membership fixture.initialProjection empty)
    (_, source, _, reference) <- foldM (install (views fixture.local) (views applicant) fixture) (Graph.emptyState, fixture.state, Graph.emptyState, observer) [1 .. count]
    -- A donor can have a later report and pending announce. They do not become
    -- the applicant's participation in that round or change its installed chain.
    let next = controlIndex (fromIntegral (count + 1))
    sourceAdvanced <- advance next source
    referenceAdvanced <- advance next reference
    admission <- checked "base pending admission" (deriveHeraldAdmissionId next)
    report <- checked "base pending report" (prepareStructuralReport (structuralAppliedReport fixture.remote (structuralAppliedVector sourceAdvanced) next) sourceAdvanced)
    proposal <- checked "base pending proposal" (prepareTopologyCutProposal (views fixture.local) (topologyJoinSealCheckpoint admission next "base-pending") (fst (commitStructuralReport report)))
    pending <- case proposal of
      TopologyCutProposalReady ready -> pure (commitTopologyCutProposal ready)
      other -> assertFailure ("expected pending base round: " <> showPreparation other)
    checked "source progress before capture" (validateStructuralProgressState pending)
    let base = captureStructuralProgressBase pending
    carriers <- checked "prepare paired observer carriers" (Reconciliation.prepareStructuralCarrierBaseImport (views applicant) (structuralProgressBaseCarrierBase base) empty)
    prepared <- checked "prepare portable progress" (prepareStructuralProgressBaseImport base (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer)
    let imported = commitStructuralProgressBaseImport prepared
    assertProgressCertificateCodec base (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer pending
    admittedHistory <- progressMembershipHistory pending
    assertIndependentProgressEvidence admittedHistory [] next (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer pending
    assertEqual "portable capture agrees with independent chronological observer replay" (captureStructuralProgressBase referenceAdvanced) (captureStructuralProgressBase imported)
    assertEqual "complete imported progress invariant" (Right ()) (validateStructuralProgressState imported)
    assertEqual "receiver identity remains local" applicant (structuralProgressLocalHerald imported)
    assertEqual "observer status remains restricted" True (structuralProgressIsJoiningObserver imported)
    assertEqual "genesis identity remains receiver-owned" (structuralGenesisCutId observer) (structuralGenesisCutId imported)
    assertEqual "no donor reports imported" [] (structuralReportEntries imported)
    assertEqual "no donor open round imported" Nothing (structuralOpenCut imported)
    assertEqual "no pending announce imported" Nothing (structuralPendingAnnounce imported)
    assertEqual "no pending established imported" [] (structuralPendingEstablishedChain imported)
    assertCurrentNablaQueriesAgainstReference imported
    forM_ (structuralInstalledCutEntries imported) $ \(cut, installed) -> do
      assertEqual "observer has no fabricated old acceptance" Nothing (installedTopologyCutOwnAcceptance installed)
      assertEqual "observer has no fabricated old acknowledgement" Nothing (installedTopologyCutOwnAcknowledgement installed)
      assertEqual "historical projection agrees with chronological replay" (structuralProjectionAtInstalledCut (views applicant) cut referenceAdvanced) (structuralProjectionAtInstalledCut (views applicant) cut imported)
      assertEqual "exact original certificate survives" (installedTopologyCutEstablished <$> lookupInstalledTopologyCut cut source) (Just (installedTopologyCutEstablished installed))
      assertInstalledCoverageAgainstReference [] [cut] imported
    forM_ [1 .. count] $ \position -> do
      sequenceNumber <- checked "base sequence" (mkStructuralSequence (fromIntegral position))
      let occurrence = structuralOccurrenceId fixture.local sequenceNumber
          expected = lookupAppliedStructuralOccurrence occurrence referenceAdvanced
          actual = lookupAppliedStructuralOccurrence occurrence imported
      assertEqual "original occurrence and source-cut link survive" expected actual
      assertBool "later occurrences retain a non-genesis source prerequisite" (position == 1 || maybe False ((/= structuralGenesisCutId observer) . appliedStructuralOccurrenceSourceTopologyPrerequisite) actual)
      assertCoverageAgainstReference [structuralOccurrenceCause occurrence] [] imported
    pure True
  where
    advance index state = fst . commitStructuralControlProgress <$> checked "base control progress" (prepareStructuralControlProgress index state)
    install sourceViews observerViews fixture (sourceGraph, source, observerGraph, observer) position = do
      sequenceNumber <- checked "base occurrence sequence" (mkStructuralSequence (fromIntegral position))
      let index = controlIndex (fromIntegral position)
          prerequisite = structuralLastInstalledCutId source
          predecessor = structuralAppliedVector source
          apply views = applyVertexOccurrenceAt prerequisite index NablaRole NablaCarrier (fixture {sequenceOne = sequenceNumber}) views fixture.local predecessor (fromIntegral (0x90 + position)) (fromIntegral (0xa0 + position)) (fromIntegral position)
      sourceControlled <- advance index source
      observerControlled <- advance index observer
      (nextSourceGraph, applied) <- apply sourceViews sourceGraph sourceControlled
      (nextObserverGraph, observerApplied) <- apply observerViews observerGraph observerControlled
      let remoteReport = structuralAppliedReport fixture.remote (structuralAppliedVector applied) index
          checkpoint = topologyControlCheckpoint index "base-checked-cut"
      reported <- checked "base report" (prepareStructuralReport remoteReport applied)
      proposal <- checked "base proposal" (prepareTopologyCutProposal sourceViews checkpoint (fst (commitStructuralReport reported)))
      ready <- case proposal of
        TopologyCutProposalReady value -> pure value
        other -> assertFailure ("base cut unavailable: " <> showPreparation other)
      let cut = topologyCutAnnounceId (preparedTopologyCutProposalAnnounce ready)
      accepted <- checked "base acceptance" (prepareTopologyCutAcceptance (topologyCutAcceptance cut remoteReport) (commitTopologyCutProposal ready))
      established <- checked "base establishment" (prepareTopologyCutEstablishment (commitTopologyCutAcceptance accepted))
      acknowledged <- checked "base acknowledgement" (prepareTopologyCutEstablishedAck (topologyCutEstablishedAck cut fixture.remote (heraldMembershipGenerationId fixture.membership)) (commitTopologyCutEstablishment established))
      replayed <- checked "independent observer cut replay" (prepareTopologyCutEstablished observerViews checkpoint (preparedTopologyCutEstablished established) observerApplied)
      pure (nextSourceGraph, commitTopologyCutEstablishedAck acknowledged, nextObserverGraph, commitTopologyCutEstablished replayed)
    showPreparation TopologyCutProposalUnavailable = "unavailable"
    showPreparation (TopologyCutProposalHeld dependencies) = show dependencies
    showPreparation (TopologyCutProposalReady _) = "ready"

-- A changed current definition is not permission to reinterpret an admitted
-- occurrence. The old certificate and every reconstructed cut keep its original
-- meaning while the local resource refresh reads the receiver's current views.
caseCarrierMeaningRefreshPreservesCuts :: Assertion
caseCarrierMeaningRefreshPreservesCuts = do
  fixture <- progressFixture
  let carriedSort = profileSortFor NeutralVertexRole
      s0 = sortOccurrence carriedSort (predefinedOccurrenceFor NeutralVertexRole (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))
      s1 = sortOccurrence carriedSort (predefinedOccurrenceFor NablaRole (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))
      views herald typed = Reconciliation.reconciliationViews herald (Map.fromList [(carriedSort, typed), (profileSortFor NablaRole, sortOccurrence (profileSortFor NablaRole) (predefinedOccurrenceFor NablaRole (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)))]) Set.empty Map.empty Set.empty
      originalViews = views fixture.local s0
      changedViews = views fixture.local s1
      occurrence = structuralOccurrenceId fixture.local fixture.sequenceOne
      control at state = fst . commitStructuralControlProgress <$> checked "retained meaning control" (prepareStructuralControlProgress (controlIndex at) state)
      installCut currentViews at state = do
        let report = structuralAppliedReport fixture.remote (structuralAppliedVector state) (controlIndex at)
            checkpoint = topologyControlCheckpointWithConsequence (controlIndex at) True "retained-carrier-meaning"
        reported <- fst . commitStructuralReport <$> checked "retained meaning remote report" (prepareStructuralReport report state)
        proposal <- checked "retained meaning proposal" (prepareTopologyCutProposal currentViews checkpoint reported)
        ready <- case proposal of
          TopologyCutProposalReady value -> pure value
          TopologyCutProposalHeld dependencies -> assertFailure ("retained meaning proposal held: " <> show dependencies)
          TopologyCutProposalUnavailable -> assertFailure "retained meaning proposal unavailable"
        let cut = topologyCutAnnounceId (preparedTopologyCutProposalAnnounce ready)
        accepted <- commitTopologyCutAcceptance <$> checked "retained meaning acceptance" (prepareTopologyCutAcceptance (topologyCutAcceptance cut report) (commitTopologyCutProposal ready))
        established <- checked "retained meaning establishment" (prepareTopologyCutEstablishment accepted)
        acknowledged <- commitTopologyCutEstablishedAck <$> checked "retained meaning acknowledgement" (prepareTopologyCutEstablishedAck (topologyCutEstablishedAck cut fixture.remote (heraldMembershipGenerationId fixture.membership)) (commitTopologyCutEstablishment established))
        pure (cut, acknowledged)
      digestAt currentViews cut state = do
        installed <- maybe (assertFailure "retained meaning installed cut missing") pure (lookupInstalledTopologyCut cut state)
        let frontier = topologyCutFrontier (installedTopologyCutCut installed)
        digest <- checked "retained meaning historical digest" (structuralTopologyOccurrenceDigestAt currentViews (topologyFrontierStructuralVersionVector frontier) (topologyFrontierAppliedControlPrefix frontier) state)
        either (assertFailure . show) pure digest
      snapshotAt currentViews cut state = Reconciliation.structuralProjectionSnapshotCanonicalBytes <$> checked "retained meaning historical projection" (structuralProjectionAtInstalledCut currentViews cut state)
      typedProjection state = [typed | Reconciliation.NablaVertexProjection _ _ typed _ _ <- Map.elems (Reconciliation.structuralAppliedVertexProjections (structuralProgressReconciliation state))]
  controlled <- control 1 fixture.state
  (graph0, applied) <- applyVertexOccurrence NablaRole NablaCarrier fixture originalViews fixture.local fixture.emptyVector 0x35 0x36 0x37 Graph.emptyState controlled
  (k0, original) <- installCut originalViews 1 applied
  originalBytes <- snapshotAt originalViews k0 original
  originalDigest <- digestAt originalViews k0 original
  refresh <- checked "retained meaning generic refresh" (Reconciliation.prepareStructuralProjectionRefresh changedViews occurrence Nothing (structuralProgressReconciliation original))
  ready <- case refresh of
    Reconciliation.StructuralReady prepared -> pure prepared
    Reconciliation.StructuralHeld dependencies -> assertFailure ("retained meaning refresh held: " <> show dependencies)
  committed <- checked "retained meaning Graph/Progress refresh" (prepareStructuralProjectionRefresh ready graph0 original)
  let (_, refreshed) = commitStructuralProjectionRefresh committed
  changedBytes <- snapshotAt changedViews k0 refreshed
  changedDigest <- digestAt changedViews k0 refreshed
  assertEqual "original Progress owner validates" (Right ()) (validateStructuralProgressState original)
  assertEqual "refreshed Progress owner validates" (Right ()) (validateStructuralProgressState refreshed)
  assertEqual "original root uses S0" [s0] (typedProjection original)
  assertEqual "refresh preserves admitted S0 despite a replacement current definition" [s0] (typedProjection refreshed)
  assertEqual "original stamp and all source coordinates are unchanged" (lookupAppliedStructuralOccurrence occurrence original) (lookupAppliedStructuralOccurrence occurrence refreshed)
  assertEqual "original publication, intrinsic public SortId and carrier identity are unchanged" (Reconciliation.structuralCarrierBaseApplications (structuralProgressBaseCarrierBase (captureStructuralProgressBase original))) (Reconciliation.structuralCarrierBaseApplications (structuralProgressBaseCarrierBase (captureStructuralProgressBase refreshed)))
  assertEqual "installed K0 retains its original canonical projection" originalBytes changedBytes
  assertEqual "installed K0 retains its original topology digest" originalDigest changedDigest
  oldCertificate <- maybe (assertFailure "retained meaning K0 certificate missing") pure (lookupInstalledTopologyCut k0 refreshed)
  assertEqual "retained K0 certificate still binds S0" originalDigest (topologyCutOccurrenceDigest (installedTopologyCutCut oldCertificate))
  advanced <- control 2 refreshed
  sequenceTwo <- checked "second structural sequence" (mkStructuralSequence 2)
  (_, newlyApplied) <- applyVertexOccurrenceAt k0 (controlIndex 2) NablaRole NablaCarrier (fixture {sequenceOne = sequenceTwo}) changedViews fixture.local (structuralAppliedVector advanced) 0x45 0x46 0x47 graph0 advanced
  (k1, later) <- installCut changedViews 2 newlyApplied
  laterOldBytes <- snapshotAt changedViews k0 later
  assertEqual "a later occurrence cannot reinterpret K0" originalBytes laterOldBytes
  assertEqual "each original retains its own admitted definition" (Set.fromList [s0, s1]) (Set.fromList (typedProjection later))
  newDigest <- digestAt changedViews k1 later
  newCertificate <- maybe (assertFailure "retained meaning K1 certificate missing") pure (lookupInstalledTopologyCut k1 later)
  assertEqual "new certified K1 retains the original occurrence meaning" newDigest (topologyCutOccurrenceDigest (installedTopologyCutCut newCertificate))
  assertEqual "extended Progress owner still validates" (Right ()) (validateStructuralProgressState later)
  system <- checked "retained meaning system" (mkSystemId (fixtureIdentifierBytes 0x31))
  applicant <- checked "retained meaning applicant" (mkHeraldEpoch (fixtureIdentifierBytes 0x81))
  let empty = Reconciliation.emptyStructuralAppliedState fixture.emptyVector
      observerViews = views applicant s1
  observer <- checked "retained meaning fresh observer" (initialJoiningStructuralProgressState system applicant fixture.membership fixture.initialProjection empty)
  forM_ [original, later] $ \source -> do
    let base = captureStructuralProgressBase source
        carriers = structuralProgressBaseCarrierBase base
    preparedCarrier <- checked "retained meaning in-memory carrier import" (Reconciliation.prepareStructuralCarrierBaseImport observerViews carriers empty)
    preparedProgress <- checked "retained meaning in-memory Progress import" (prepareStructuralProgressBaseImport base (Reconciliation.preparedStructuralCarrierBaseSuccessor preparedCarrier) observer)
    let imported = commitStructuralProgressBaseImport preparedProgress
    assertEqual "in-memory import preserves each captured interpretation" base (captureStructuralProgressBase imported)
    assertEqual "in-memory observer passes the Progress audit" (Right ()) (validateStructuralProgressState imported)

caseStructuralProgressBaseAdmission :: Assertion
caseStructuralProgressBaseAdmission = do
  fixture <- progressFixture
  system <- checked "base admission system" (mkSystemId (fixtureIdentifierBytes 0x31))
  applicant <- checked "base admission applicant" (mkHeraldEpoch (fixtureIdentifierBytes 0x81))
  let empty = Reconciliation.emptyStructuralAppliedState fixture.emptyVector
      observerViews = Reconciliation.reconciliationViews applicant Map.empty Set.empty Map.empty Set.empty
      initialize digest = checked "base admission observer" (initialJoiningStructuralProgressState system applicant fixture.membership digest empty)
  observer <- initialize fixture.initialProjection
  controlled <- checked "base admission control" (prepareStructuralControlProgress (controlIndex 1) fixture.state)
  let source = fst (commitStructuralControlProgress controlled)
  (_, first) <- applyNeutralOccurrence fixture fixture.views fixture.local fixture.emptyVector 0x91 0xa1 1 Graph.emptyState source
  (_, different) <- applyNeutralOccurrence fixture fixture.views fixture.local fixture.emptyVector 0x92 0xa1 1 Graph.emptyState source
  let base = captureStructuralProgressBase first
      wrongBase = captureStructuralProgressBase different
  paired <- checked "base paired carriers" (Reconciliation.prepareStructuralCarrierBaseImport observerViews (structuralProgressBaseCarrierBase base) empty)
  differentCarriers <- checked "base different carriers" (Reconciliation.prepareStructuralCarrierBaseImport observerViews (structuralProgressBaseCarrierBase wrongBase) empty)
  let reconciliation = Reconciliation.preparedStructuralCarrierBaseSuccessor paired
  assertEqual "same occurrence IDs cannot substitute different carrier bytes" (Left StructuralProgressBaseCarrierMismatch) (() <$ prepareStructuralProgressBaseImport base (Reconciliation.preparedStructuralCarrierBaseSuccessor differentCarriers) observer)
  assertEqual "serving participant cannot adopt an observer base" (Left StructuralProgressBaseImportRequiresFreshObserver) (() <$ prepareStructuralProgressBaseImport base reconciliation fixture.receiverState)
  imported <- checked "base valid admission" (prepareStructuralProgressBaseImport base reconciliation observer)
  assertEqual "an advanced observer cannot be overwritten" (Left StructuralProgressBaseImportRequiresFreshObserver) (() <$ prepareStructuralProgressBaseImport base reconciliation (commitStructuralProgressBaseImport imported))
  otherDigest <- checked "other base genesis digest" (mkInitialProjectionDigest (ByteString.replicate 32 0xf1))
  otherObserver <- initialize otherDigest
  assertEqual "a different genesis cannot share the carrier base" (Left StructuralProgressBaseGenesisMismatch) (() <$ prepareStructuralProgressBaseImport base reconciliation otherObserver)

-- The protocol-specific fixtures supply real membership certificates and a
-- fresh observer. Check the portable import against their chronological owner,
-- including historical projections that do not belong to the current generation.
assertPortableProgressImport ::
  Reconciliation.ReconciliationViews ->
  StructuralProgressState ->
  StructuralProgressState ->
  Assertion
assertPortableProgressImport views observer source = do
  checked "portable source progress invariant" (validateStructuralProgressState source)
  let base = captureStructuralProgressBase source
  carriers <- checked "portable historical carriers" (Reconciliation.prepareStructuralCarrierBaseImport views (structuralProgressBaseCarrierBase base) (structuralProgressReconciliation observer))
  prepared <- checked "portable historical progress" (prepareStructuralProgressBaseImport base (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer)
  let imported = commitStructuralProgressBaseImport prepared
      occurrences = Reconciliation.structuralAppliedHistoryOccurrences (structuralProgressReconciliation source)
  assertProgressCertificateCodec base (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer source
  admittedHistory <- progressMembershipHistory source
  (_, rawCuts) <- checked "synthetic certificate admission records" (Codec.decode (encodeStructuralProgressCertificates (captureStructuralProgressCertificates source)) :: Either String (ByteString, [(ByteString, Maybe ByteString, Maybe (ByteString, ByteString), Maybe ByteString)]))
  records <- traverse (checked "checked historical admission" . Admission.decodeAdmissionRecord) [record | (_, _, Just (_, record), _) <- rawCuts]
  assertIndependentProgressEvidence admittedHistory records (structuralAppliedControlPrefix source) (Reconciliation.preparedStructuralCarrierBaseSuccessor carriers) observer source
  assertEqual "portable facts match the chronological owner" base (captureStructuralProgressBase imported)
  assertEqual "full portable progress invariant" (Right ()) (validateStructuralProgressState imported)
  assertEqual "the fresh observer keeps its own identity" (structuralProgressLocalHerald observer) (structuralProgressLocalHerald imported)
  assertBool "historical membership does not admit the observer" (structuralProgressIsJoiningObserver imported)
  assertEqual "the original ordered cut chain survives" (structuralInstalledCutIds source) (structuralInstalledCutIds imported)
  assertEqual "the original membership history survives" (structuralProgressMembershipHistory source) (structuralProgressMembershipHistory imported)
  assertEqual "no donor reports become observer reports" [] (structuralReportEntries imported)
  assertEqual "no donor open round survives" Nothing (structuralOpenCut imported)
  forM_ (structuralInstalledCutEntries source) $ \(cut, original) -> do
    installed <- maybe (assertFailure "portable import lost an installed cut") pure (lookupInstalledTopologyCut cut imported)
    assertEqual "the exact cut certificate survives" (installedTopologyCutEstablished original) (installedTopologyCutEstablished installed)
    assertEqual "the historical Join seal marker survives" (installedTopologyCutJoinSealAdmission original) (installedTopologyCutJoinSealAdmission installed)
    assertEqual "the historical newly covered set survives" (installedTopologyCutNewlyCoveredOccurrences original) (installedTopologyCutNewlyCoveredOccurrences installed)
    assertEqual "no historical acceptance is fabricated" Nothing (installedTopologyCutOwnAcceptance installed)
    assertEqual "no historical acknowledgement is fabricated" Nothing (installedTopologyCutOwnAcknowledgement installed)
    expectedProjection <- checked "chronological historical projection" (structuralProjectionAtInstalledCut views cut source)
    actualProjection <- checked "portable historical projection" (structuralProjectionAtInstalledCut views cut imported)
    assertEqual "historical projection matches chronological reconstruction" expectedProjection actualProjection
    assertInstalledCoverageAgainstReference [] [cut] imported
  forM_ occurrences $ \occurrence -> do
    assertEqual "exact occurrence and source-cut link survive" (lookupAppliedStructuralOccurrence occurrence source) (lookupAppliedStructuralOccurrence occurrence imported)
    assertEqual "earliest occurrence coverage survives" (installedCutCoveringOccurrence occurrence source) (installedCutCoveringOccurrence occurrence imported)
    assertEqual "current occurrence coverage survives" (currentInstalledCutCoveringOccurrence occurrence source) (currentInstalledCutCoveringOccurrence occurrence imported)
    assertCoverageAgainstReference [structuralOccurrenceCause occurrence] [] imported

-- Exercise the wire leaf with the same generated ordinary chains, two-step
-- retirement chain and historical admission fixture used by base import.
-- Mutations preserve nested codec shape so owner admission, rather than merely
-- deserialization, must reject the changed certificate authority or ancestry.
assertProgressCertificateCodec ::
  StructuralProgressBase ->
  Reconciliation.StructuralAppliedState ->
  StructuralProgressState ->
  StructuralProgressState ->
  Assertion
assertProgressCertificateCodec base reconciliation observer source = do
  let evidence = captureStructuralProgressCertificates source
      bytes = encodeStructuralProgressCertificates evidence
  claims <- checked "decode portable certificate claims" (decodeStructuralProgressCertificates bytes)
  prepared <- checked "admit portable certificates against paired base" (prepareStructuralProgressBaseImportWithCertificates claims base reconciliation observer)
  let imported = commitStructuralProgressBaseImport prepared
  assertEqual "wire certificates reproduce the complete portable base" base (captureStructuralProgressBase imported)
  assertEqual "wire certificate transcript roundtrips exactly" bytes (encodeStructuralProgressCertificates (captureStructuralProgressCertificates imported))
  assertLeft "trailing certificate bytes are rejected" (decodeStructuralProgressCertificates (ByteString.snoc bytes 0))
  (_, rawCuts) <- checked "inspect certificate transcript" (Codec.decode bytes :: Either String (ByteString, [(ByteString, Maybe ByteString, Maybe (ByteString, ByteString), Maybe ByteString)]))
  assertLeft "wrong certificate domain is rejected" (decodeStructuralProgressCertificates (Codec.encode ("wrong-domain" :: ByteString, rawCuts)))
  let rejectChanged label changed = do
        decoded <- checked (label <> " remains canonically shaped") (decodeStructuralProgressCertificates (Codec.encode ("ECLIPS-STRUCTURAL-PROGRESS-CERTIFICATES" :: ByteString, changed)))
        assertLeft label (prepareStructuralProgressBaseImportWithCertificates decoded base reconciliation observer)
  case rawCuts of
    [] -> pure ()
    first : rest -> do
      rejectChanged "duplicate cut cannot acquire a second chain position" (first : first : rest)
      rejectChanged "omitted final cut cannot change the paired base anchor" (init rawCuts)
      case rest of
        [] -> pure ()
        _ -> rejectChanged "a cut cannot skip its retained predecessor" rest
      let established = case structuralInstalledEstablishedChain source of
            certificate : _ -> certificate
            [] -> error "certificate fixture has no installed cut"
      missingAcceptances <- checked "intrinsically shaped certificate without reporters" (topologyCutEstablished (topologyCutEstablishedId established) (topologyCutEstablishedCut established) [])
      let (_, successor, admission, seal) = first
      rejectChanged "intrinsic certificate shape cannot replace exact member acceptance" ((topologyCutEstablishedCanonicalBytes missingAcceptances, successor, admission, seal) : rest)
  forM_ (zip [0 :: Int ..] rawCuts) $ \(position, (established, successor, admission, seal)) -> do
    let replaceCut changed = take position rawCuts <> [changed] <> drop (position + 1) rawCuts
    case (successor, admission) of
      (Nothing, Nothing) -> do
        let cut = structuralInstalledEstablishedChain source !! position
            prefix = topologyFrontierAppliedControlPrefix (topologyCutFrontier (topologyCutEstablishedCut cut))
            rejectSeal changedSeal = do
              decoded <- checked "changed seal marker remains canonically shaped" (decodeStructuralProgressCertificates (Codec.encode ("ECLIPS-STRUCTURAL-PROGRESS-CERTIFICATES" :: ByteString, replaceCut (established, successor, admission, changedSeal))))
              assertEqual
                "an unhashed seal marker cannot replace admitted metadata"
                (Left (StructuralProgressCertificateMetadataMismatch (topologyCutEstablishedId cut)))
                (() <$ prepareStructuralProgressBaseImportWithCertificates decoded base reconciliation observer)
        case seal of
          Just _ -> rejectSeal Nothing
          Nothing | prefix > controlIndex 0 -> do
            otherAdmission <- checked "intrinsically valid unrelated admission" (deriveHeraldAdmissionId prefix)
            rejectSeal (Just (heraldAdmissionIdCanonicalBytes otherAdmission))
          Nothing -> pure ()
      _ ->
        rejectChanged
          "membership cuts retain their original successor/admission proof"
          (replaceCut (established, Nothing, Nothing, seal))
  where
    assertLeft :: String -> Either problem value -> Assertion
    assertLeft label = either (const (pure ())) (const (assertFailure label))

type RawProgressEvidenceVector = (ByteString, ByteString, [(ByteString, Word64)])
type RawProgressEvidenceStamp = ((ByteString, Word64), RawProgressEvidenceVector, (ByteString, ByteString, ByteString, Word64), ByteString, Word8)
type RawProgressEvidenceOccurrence = (RawProgressEvidenceStamp, ByteString, (ByteString, ByteString), Word64)
type RawProgressEvidenceCut = (ByteString, Maybe ByteString, Maybe (ByteString, ByteString), Maybe ByteString)
type RawProgressEvidence = (ByteString, (ByteString, ByteString, ByteString, ByteString), (RawProgressEvidenceVector, Word64), [RawProgressEvidenceOccurrence], ByteString)

-- This boundary receives the decoded evidence and independently admitted
-- Oracle facts. The source Progress base is used only for capture and expected
-- results, never as an argument to decoded-evidence admission.
assertIndependentProgressEvidence ::
  Membership.HeraldMembershipHistory ->
  [Admission.HeraldAdmissionRecord] ->
  ControlIndex ->
  Reconciliation.StructuralAppliedState ->
  StructuralProgressState ->
  StructuralProgressState ->
  Assertion
assertIndependentProgressEvidence history admissions committed reconciliation observer source = do
  let expected = captureStructuralProgressBase source
      evidence = captureStructuralProgressBaseEvidence expected
      bytes = encodeStructuralProgressBaseEvidence evidence
      carriers = Reconciliation.captureStructuralCarrierBase reconciliation
      requiredCut = structuralLastInstalledCutId source
  claims <- checked "decode independent Progress evidence" (decodeStructuralProgressBaseEvidence bytes)
  context <- checked "construct Progress context from admitted Oracle facts" (structuralProgressBaseAdmissionContext history admissions committed requiredCut observer)
  admitted <- checked "admit Progress evidence without the donor base" (admitStructuralProgressBaseEvidence context claims)
  prepared <- checked "pair independently admitted Progress with reconstructed carriers" (prepareStructuralProgressBaseImportFromEvidence admitted carriers reconciliation observer)
  let imported = commitStructuralProgressBaseImport prepared
  assertEqual "independent evidence recreates the exact immutable Progress base" expected (captureStructuralProgressBase imported)
  assertEqual "independent wire evidence is canonical" bytes (encodeStructuralProgressBaseEvidence (captureStructuralProgressBaseEvidence (captureStructuralProgressBase imported)))
  assertEqual "independent import keeps receiver identity" (structuralProgressLocalHerald observer) (structuralProgressLocalHerald imported)
  assertEqual "independent import remains an observer" True (structuralProgressIsJoiningObserver imported)
  assertEqual "independent import creates no donor reports" [] (structuralReportEntries imported)
  assertEqual "independent imported Progress validates" (Right ()) (validateStructuralProgressState imported)
  assertEvidenceRejected "independent evidence rejects trailing bytes" (decodeStructuralProgressBaseEvidence (ByteString.snoc bytes 0))
  raw@(domain, identity@(membership, historyFrame, initial, genesis), applied@(vector, _), rows, certificates) <- checked "inspect independent evidence transcript" (Codec.decode bytes :: Either String RawProgressEvidence)
  let rejectRaw :: String -> RawProgressEvidence -> Assertion
      rejectRaw label changed = assertEvidenceRejected label $ do
        decoded <- decodeStructuralProgressBaseEvidence (Codec.encode changed)
        accepted <- admitStructuralProgressBaseEvidence context decoded
        prepareStructuralProgressBaseImportFromEvidence accepted carriers reconciliation observer
      mutateFirst f = case rows of first : rest -> (domain, identity, applied, f first : rest, certificates); [] -> raw
      otherIdentifier = ByteString.replicate 32 0xf4
  rejectRaw "independent evidence binds its domain" ("wrong-progress-domain", identity, applied, rows, certificates)
  rejectRaw "independent evidence binds receiver initial projection" (domain, (membership, historyFrame, otherIdentifier, genesis), applied, rows, certificates)
  rejectRaw "independent evidence binds receiver genesis cut" (domain, (membership, historyFrame, initial, otherIdentifier), applied, rows, certificates)
  rejectRaw "independent evidence cannot exceed the admitted control bound" (domain, identity, (vector, controlIndexWord64 committed + 1), rows, certificates)
  case rows of
    [] -> pure ()
    first : rest -> do
      rejectRaw "independent evidence rejects duplicate occurrence rows" (domain, identity, applied, first : first : rest, certificates)
      rejectRaw "independent evidence cannot omit an applied occurrence" (domain, identity, applied, rest, certificates)
      if null rest then pure () else rejectRaw "independent evidence requires canonical occurrence ordering" (domain, identity, applied, reverse rows, certificates)
      rejectRaw "occurrence source-cut links must name admitted history" (mutateFirst (\(stamp, _, carrier, prerequisite) -> (stamp, otherIdentifier, carrier, prerequisite)))
      rejectRaw "occurrence control prerequisites cannot exceed the imported prefix" (mutateFirst (\(stamp, sourceCut, carrier, _) -> (stamp, sourceCut, carrier, controlIndexWord64 committed + 1)))
      rejectRaw "occurrence carrier definitions must match reconstructed facts" (mutateFirst (\(stamp, sourceCut, (sort, _), prerequisite) -> (stamp, sourceCut, (sort, otherIdentifier), prerequisite)))
      rejectRaw "exact publication identity must match reconstructed carrier" (mutateFirst (\((dot, predecessor, (_, authority, herald, sequenceNumber), digest, role), sourceCut, carrier, prerequisite) -> ((dot, predecessor, (otherIdentifier, authority, herald, sequenceNumber), digest, role), sourceCut, carrier, prerequisite)))
      rejectRaw "unsupported carrier roles fail nominal decoding" (mutateFirst (\((dot, predecessor, publication, digest, _), sourceCut, carrier, prerequisite) -> ((dot, predecessor, publication, digest, 255), sourceCut, carrier, prerequisite)))
  (certificateDomain, cutRows) <- checked "inspect independently embedded cut evidence" (Codec.decode certificates :: Either String (ByteString, [RawProgressEvidenceCut]))
  let withCuts cutRows' = (domain, identity, applied, rows, Codec.encode (certificateDomain, cutRows'))
  case cutRows of
    [] -> pure ()
    first : rest -> do
      rejectRaw "independent evidence rejects duplicated cut identities" (withCuts (first : first : rest))
      rejectRaw "independent evidence binds the externally required final cut" (withCuts (take (length cutRows - 1) cutRows))
      if null rest then pure () else rejectRaw "independent cut evidence preserves predecessor order" (withCuts (reverse cutRows))
      case structuralInstalledEstablishedChain source of
        established : _ -> do
          missing <- checked "nominal cut certificate without acceptances" (topologyCutEstablished (topologyCutEstablishedId established) (topologyCutEstablishedCut established) [])
          let (_, successor, admission, marker) = first
          rejectRaw "independent certificate admission requires exact member acceptances" (withCuts ((topologyCutEstablishedCanonicalBytes missing, successor, admission, marker) : rest))
        [] -> assertFailure "encoded cuts differ from the source chain"
  if any (\(_, _, admission, marker) -> admission /= Nothing || marker /= Nothing) cutRows
    then do
      missingContext <- checked "context without required historical admission facts" (structuralProgressBaseAdmissionContext history [] committed requiredCut observer)
      assertEvidenceRejected "historical Join cuts require independently admitted Oracle records" (admitStructuralProgressBaseEvidence missingContext claims)
    else pure ()
  if structuralAppliedControlPrefix source > controlIndex 0 || not (null (Reconciliation.structuralAppliedHistoryOccurrences reconciliation)) || not (null (structuralInstalledCutIds source))
    then assertEvidenceRejected "independent import cannot overwrite an advanced observer" (prepareStructuralProgressBaseImportFromEvidence admitted carriers reconciliation imported)
    else pure ()
  if requiredCut /= structuralGenesisCutId observer
    then do
      wrongAnchor <- checked "alternate independently required cut" (structuralProgressBaseAdmissionContext history admissions committed (structuralGenesisCutId observer) observer)
      assertEvidenceRejected "wire evidence cannot select a different required final cut" (admitStructuralProgressBaseEvidence wrongAnchor claims)
    else pure ()
  if null (Reconciliation.structuralAppliedHistoryOccurrences reconciliation)
    then pure ()
    else do
      let emptyReconciliation = structuralProgressReconciliation observer
          emptyCarriers = Reconciliation.captureStructuralCarrierBase emptyReconciliation
      assertEvidenceRejected "exact Progress occurrences cannot be paired with an empty carrier history" (prepareStructuralProgressBaseImportFromEvidence admitted emptyCarriers emptyReconciliation observer)
      assertEvidenceRejected "carrier capture cannot substitute a different reconstruction" (prepareStructuralProgressBaseImportFromEvidence admitted carriers emptyReconciliation observer)

progressMembershipHistory :: StructuralProgressState -> IO Membership.HeraldMembershipHistory
progressMembershipHistory source = do
  generations <- case sortOn (maybe (controlIndex 0) id . Membership.heraldMembershipGenerationChangeControlIndex) (map snd (structuralProgressMembershipHistory source)) of
    first : rest -> pure (first :| rest)
    [] -> assertFailure "Progress fixture lost its genesis membership"
  checked "ordered independently supplied membership history" (Membership.heraldMembershipHistory generations)

assertEvidenceRejected :: String -> Either problem value -> Assertion
assertEvidenceRejected label = either (const (pure ())) (const (assertFailure label))

propPreparationControlChanges :: QC.Property
propPreparationControlChanges =
  QC.forAll (QC.listOf (QC.chooseInt (0, 3))) $ \increments -> QC.ioProperty $ do
    fixture <- progressFixture
    assertEqual "initial structural assembly has no observers to wake" False (fst (takePreparationReadinessChange fixture.state))
    let advance (state, cursor, observed) increment = do
          let next = cursor + increment
              index = controlIndex (fromIntegral next)
          prepared <- checked "preparation control advance" (prepareStructuralControlProgress index (clearPreparationReadinessChange state))
          let (advanced, classification) = commitStructuralControlProgress prepared
              (changed, preparationClean) = takePreparationReadinessChange advanced
              (planChanged, clean) = takeAlignmentPlanChange preparationClean
          assertEqual "only a strict control advance notifies" (increment /= 0) changed
          assertEqual "cursor-only progress preserves alignment plans" False planChanged
          assertCurrentNablaQueriesAgainstReference advanced
          assertEqual "draining plans preserves preparation notification" changed (fst (takePreparationReadinessChange (clearAlignmentPlanChange advanced)))
          assertEqual "notification agrees with the semantic disposition" (classification == StructuralControlAdvanced) changed
          report <- checked "preparation report" (prepareStructuralReport (structuralAppliedReport fixture.remote (structuralAppliedVector clean) index) clean)
          let reported = fst (commitStructuralReport report)
          assertEqual "report progress is bookkeeping" False (fst (takePreparationReadinessChange reported))
          assertEqual "report progress does not wake generation plans" False (fst (takeAlignmentPlanChange reported))
          replay <- checked "preparation report replay" (prepareStructuralReport (structuralAppliedReport fixture.remote (structuralAppliedVector clean) index) reported)
          assertEqual "report replay is silent" reported (fst (commitStructuralReport replay))
          assertEqual "explicit clearing preserves the control prefix" (structuralAppliedControlPrefix advanced) (structuralAppliedControlPrefix clean)
          assertEqual "ordinary equality includes a true notification" changed (advanced /= clean)
          pure (reported, next, observed || changed)
    (_, _, expected) <- foldM advance (fixture.state, 0 :: Int, False) increments
    let coalesce state increment = do
          prepared <- checked "coalesced preparation control" (prepareStructuralControlProgress (controlIndex (controlIndexWord64 (structuralAppliedControlPrefix state) + fromIntegral increment)) state)
          pure (fst (commitStructuralControlProgress prepared))
    coalesced <- foldM coalesce fixture.state increments
    assertEqual "several mutations retain just one pending notification" expected (fst (takePreparationReadinessChange coalesced))
    assertEqual "cursor-only advances do not accumulate plan notifications" False (fst (takeAlignmentPlanChange coalesced))
    pure True

-- Compare the complete externally meaningful preparation, including a ready
-- proposal's successor, rather than only the digest selected by the memo.
assertProposalEquivalent :: TopologyCutProposalPreparation -> TopologyCutProposalPreparation -> Assertion
assertProposalEquivalent expected actual = case (expected, actual) of
  (TopologyCutProposalUnavailable, TopologyCutProposalUnavailable) -> pure ()
  (TopologyCutProposalHeld before, TopologyCutProposalHeld after) -> assertEqual "held dependencies" before after
  (TopologyCutProposalReady before, TopologyCutProposalReady after) -> do
    assertEqual "announcement" (preparedTopologyCutProposalAnnounce before) (preparedTopologyCutProposalAnnounce after)
    assertEqual "local acceptance" (preparedTopologyCutProposalLocalAcceptance before) (preparedTopologyCutProposalLocalAcceptance after)
    assertEqual "semantic successor" (commitTopologyCutProposal before) (commitTopologyCutProposal after)
  _ -> assertFailure "cached proposal classification differs from reconstruction"

propTopologyProjectionReuse :: QC.Property
propTopologyProjectionReuse =
  QC.forAll (QC.listOf (QC.chooseInt (0, 3))) $ \increments -> QC.ioProperty $ do
    fixture <- progressFixture
    let checkpoint index = topologyControlCheckpoint index "cursor-only progress"
        reportAt index state = do
          controlled <- checked "advance cached control" (prepareStructuralControlProgress index state)
          let advanced = fst (commitStructuralControlProgress controlled)
          report <- checked "cached remote report" (prepareStructuralReport (structuralAppliedReport fixture.remote (structuralAppliedVector advanced) index) advanced)
          pure (fst (commitStructuralReport report))
        compareAt expectedWork index state = do
          reference <- checked "reconstructed proposal" (prepareTopologyCutProposal fixture.views (checkpoint index) state)
          (work, retained, actual) <- checked "cached proposal" (prepareTopologyCutProposalCached fixture.views (checkpoint index) state)
          assertEqual "projection work" expectedWork work
          assertEqual "populating derived data preserves semantic state" state retained
          assertProposalEquivalent reference actual
          pure retained
        step (state, cursor) increment = do
          let next = cursor + increment
              index = controlIndex (fromIntegral next)
          reported <- reportAt index state
          retained <- compareAt TopologyProjectionReused index reported
          pure (retained, next)
    reported <- reportAt (controlIndex 1) fixture.state
    warm <- compareAt TopologyProjectionReconstructed (controlIndex 1) reported
    (retained, cursor) <- foldM step (warm, 1 :: Int) (0 : increments)
    applied <- applyLocalNeutral fixture retained
    let index = controlIndex (fromIntegral cursor)
    advanced <- reportAt index applied
    changed <- compareAt TopologyProjectionReconstructed index advanced
    _ <- compareAt TopologyProjectionReused index changed
    pure True

caseTopologyProjectionViewInputs :: Assertion
caseTopologyProjectionViewInputs = do
  fixture <- progressFixture
  let index = controlIndex 0
      checkpoint = topologyControlCheckpoint index "unchanged control"
  prepared <- checked "complete empty matrix" (prepareStructuralReport (structuralAppliedReport fixture.remote fixture.emptyVector index) fixture.state)
  let reported = fst (commitStructuralReport prepared)
  (_, warm, _) <- checked "warm empty projection" (prepareTopologyCutProposalCached fixture.views checkpoint reported)
  vertex <- checked "late baseline vertex" (mkNablaId (fixtureIdentifierBytes 0x73))
  let changedViews = Reconciliation.reconciliationViews fixture.local Map.empty (Set.singleton (NablaVertex vertex)) Map.empty Set.empty
  reference <- checked "reference changed external inputs" (prepareTopologyCutProposal changedViews checkpoint warm)
  (work, retained, actual) <- checked "changed external inputs" (prepareTopologyCutProposalCached changedViews checkpoint warm)
  assertEqual "baseline endpoint availability invalidates projection reuse" TopologyProjectionReconstructed work
  assertProposalEquivalent reference actual
  (reused, _, _) <- checked "repeat changed inputs" (prepareTopologyCutProposalCached changedViews checkpoint retained)
  assertEqual "the replacement key is reusable" TopologyProjectionReused reused
  let malformedViews =
        Reconciliation.reconciliationViews
          fixture.local
          (Map.singleton (profileSortFor NeutralVertexRole) (sortOccurrence (profileSortFor NablaRole) (predefinedOccurrenceFor NeutralVertexRole (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))))
          (Set.singleton (NablaVertex vertex))
          Map.empty
          Set.empty
  case prepareTopologyCutProposalCached malformedViews checkpoint retained of
    Left (StructuralProgressReconciliationProblem Reconciliation.StructuralEffectiveSortKeyMismatch {}) -> pure ()
    Left problem -> assertFailure ("wrong invalid-sort rejection on a cache hit: " <> show problem)
    Right _ -> assertFailure "cached projection bypassed immutable sort shape validation"

caseTopologyProjectionControlChanges :: Int -> Assertion
caseTopologyProjectionControlChanges offset = do
  fixture <- progressFixture
  process <- checked "projection controller" (mkProcessEpochId (fixtureIdentifierBytes 0x74))
  decision <- checked "projection label decision" (mkLabelDecisionId (fixtureIdentifierBytes 0x75))
  subject <- checked "projection neutral object" (mkGlobalObjectId (fixtureIdentifierBytes 0x35))
  let labelIndex = controlIndex (2 + fromIntegral offset)
      endIndex = controlIndex (3 + fromIntegral offset)
      views =
        Reconciliation.reconciliationViews
          fixture.local
          ( Map.fromList
              [ (profileSortFor role, sortOccurrence (profileSortFor role) (predefinedOccurrenceFor role (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)))
              | role <- [NeutralVertexRole, NablaRole]
              ]
          )
          Set.empty
          (Map.singleton process fixture.local)
          Set.empty
      reportAt index state = do
        controlled <- checked "advance consequence control" (prepareStructuralControlProgress index state)
        let advanced = fst (commitStructuralControlProgress controlled)
        report <- checked "consequence remote report" (prepareStructuralReport (structuralAppliedReport fixture.remote (structuralAppliedVector advanced) index) advanced)
        pure (fst (commitStructuralReport report))
      compareAt expectedWork currentViews index state = do
        let (affects, bytes) = Reconciliation.structuralControlCheckpointAt index (structuralProgressReconciliation state)
            checkpoint = topologyControlCheckpointWithConsequence index affects bytes
        reference <- checked "reference control consequence" (prepareTopologyCutProposal currentViews checkpoint state)
        (work, retained, result) <- checked "cached control consequence" (prepareTopologyCutProposalCached currentViews checkpoint state)
        assertEqual "control consequence work" expectedWork work
        assertProposalEquivalent reference result
        pure retained
      applyRefresh (graph, state) prepared =
        commitStructuralProjectionRefresh <$> checked "commit structural consequence" (prepareStructuralProjectionRefresh prepared graph state)
  controlled <- reportAt (controlIndex 1) fixture.state
  labelCause <- checked "projection label cause" (labelReleaseCause decision labelIndex)
  (neutralGraph, neutralApplied) <- applyNeutralOccurrence fixture views fixture.local fixture.emptyVector 0x35 0x36 0x37 Graph.emptyState controlled
  neutralReported <- reportAt (controlIndex 1) neutralApplied
  neutralWarm <- compareAt TopologyProjectionReconstructed views (controlIndex 1) neutralReported
  neutralRelease <- checked "prepare neutral authority change" (Reconciliation.prepareStructuralLabelControlRefresh views labelCause subject (releasedLabel (ProcessLabel process, 1)) Nothing (structuralProgressReconciliation neutralWarm))
  neutralPrepared <- case neutralRelease of
    Reconciliation.StructuralReady prepared -> pure prepared
    Reconciliation.StructuralHeld dependencies -> assertFailure ("neutral release unexpectedly held: " <> show dependencies)
  assertEqual "neutral labels have no structural consequence" False (Reconciliation.preparedStructuralChangesState neutralPrepared)
  (_, neutralUnchanged) <- applyRefresh (neutralGraph, neutralWarm) neutralPrepared
  _ <- compareAt TopologyProjectionReused views (controlIndex 1) neutralUnchanged
  (graph, applied) <- applyVertexOccurrence NablaRole NablaCarrier fixture views fixture.local fixture.emptyVector 0x35 0x36 0x37 Graph.emptyState controlled
  reported <- reportAt (controlIndex 1) applied
  warm <- compareAt TopologyProjectionReconstructed views (controlIndex 1) reported
  _ <- compareAt TopologyProjectionReused views (controlIndex 1) warm
  labelPreparation <- checked "prepare topology label" (Reconciliation.prepareStructuralLabelControlRefresh views labelCause subject (releasedLabel (ProcessLabel process, 1)) Nothing (structuralProgressReconciliation warm))
  labelPrepared <- case labelPreparation of
    Reconciliation.StructuralReady prepared -> pure prepared
    Reconciliation.StructuralHeld dependencies -> assertFailure ("label unexpectedly held: " <> show dependencies)
  assertEqual "a Nabla controller change has structural consequences" True (Reconciliation.preparedStructuralChangesState labelPrepared)
  (labelGraph, labelled) <- applyRefresh (graph, warm) labelPrepared
  -- Reconciliation changed before the cumulative report/control coordinate did.
  -- Invalidation must therefore not rely only on the frontier key changing.
  _ <- compareAt TopologyProjectionReconstructed views (controlIndex 1) labelled
  labelReported <- reportAt labelIndex (clearAlignmentPlanChange labelled)
  assertEqual "crossing a genuine controller consequence wakes plans" True (fst (takeAlignmentPlanChange labelReported))
  assertCurrentNablaQueriesAgainstReference labelReported
  labelWarm <- compareAt TopologyProjectionReconstructed views labelIndex labelReported
  assertEqual "an earlier cursor excludes the retained label overlay" (controlIndex 0) (Reconciliation.structuralProjectionControlCoordinate (controlIndex 1) (structuralProgressReconciliation labelWarm))
  assertEqual "label changes the effective control coordinate" labelIndex (Reconciliation.structuralProjectionControlCoordinate labelIndex (structuralProgressReconciliation labelWarm))
  endCause <- checked "projection End cause" (processEndCause process endIndex)
  let endedViews = Reconciliation.reconciliationViewsWithEndedProcesses (Map.singleton process (fixture.local, endIndex)) views
  ends <- checked "prepare topology End" (Reconciliation.prepareStructuralProcessEndRefreshes endedViews endCause process (structuralProgressReconciliation labelWarm))
  assertEqual "End changes the one controlled object" 1 (length ends)
  (_, ended) <- foldM applyRefresh (labelGraph, labelWarm) (map snd ends)
  endReported <- reportAt endIndex (clearAlignmentPlanChange ended)
  assertEqual "crossing a genuine End consequence wakes plans" True (fst (takeAlignmentPlanChange endReported))
  assertCurrentNablaQueriesAgainstReference endReported
  endWarm <- compareAt TopologyProjectionReconstructed endedViews endIndex endReported
  _ <- compareAt TopologyProjectionReused endedViews endIndex endWarm
  assertEqual "End changes the effective control coordinate" endIndex (Reconciliation.structuralProjectionControlCoordinate (controlIndex (4 + fromIntegral offset)) (structuralProgressReconciliation endWarm))
  -- Reference the public retained history, independently of the optimized
  -- minimum-key guard. Generated gaps exercise covered and uncovered prefixes.
  forM_ [warm, labelWarm, endWarm] $ \snapshot ->
    forM_ [0 .. 5 + offset] $ \cursor -> do
      let requested = controlIndex (fromIntegral cursor)
          reconciliation = structuralProgressReconciliation snapshot
          expected = any (\(index, _, _) -> index <= requested) (Reconciliation.structuralAppliedControlHistory reconciliation)
      assertEqual
        "checkpoint consequence flag agrees with retained history"
        expected
        (fst (Reconciliation.structuralControlCheckpointAt requested reconciliation))

propEarliestCoveringCut :: QC.Property
propEarliestCoveringCut =
  QC.forAll batches $ \steps -> QC.ioProperty $ do
    fixture <- progressFixture
    decision <- checked "coverage label decision" (mkLabelDecisionId (fixtureIdentifierBytes 0x71))
    process <- checked "coverage process" (mkProcessEpochId (fixtureIdentifierBytes 0x72))
    probe <- checked "coverage disappearance probe" (deriveDisappearanceProbeId (controlIndex 1))
    (_, final, finalSequence, finalControl, boundaries) <-
      foldM (installBatch fixture) (Graph.emptyState, fixture.state, 0, 0, []) steps
    checked "generated installed-chain invariant" (validateStructuralProgressState final)
    assertEqual "one installed cut per generated batch" (length steps) (length (structuralInstalledCutIds final))
    -- These expected cut identities come from the ceremony and the generated
    -- batch boundaries, independently of both production coverage queries.
    let expected select position =
          (\(_, _, cut) -> cut) <$> find ((>= position) . select) boundaries
        assertCoverage cause earliest = do
          assertCoverageAgainstReference [cause] [] final
          assertEqual "genesis alone covers no cause" Nothing (installedCutCoveringCause cause fixture.state)
          assertEqual "first covering batch" earliest (installedCutCoveringCause cause final)
          assertEqual
            "earliest search agrees with inherited arbitrary-cut coverage"
            (find (\cut -> installedCutCoversCause cut cause final) (structuralInstalledCutIds final))
            earliest
          assertEqual
            "current cut inherits every earlier covered cause"
            (structuralLastInstalledCutId final <$ earliest)
            (currentInstalledCutCoveringCause cause final)
    forM_ [1 .. finalSequence + 1] $ \position -> do
      sequenceNumber <- checked "coverage sequence" (mkStructuralSequence (fromIntegral position))
      assertCoverage
        (structuralOccurrenceCause (structuralOccurrenceId fixture.local sequenceNumber))
        (expected (\(sequenceLimit, _, _) -> sequenceLimit) position)
    forM_ [1 .. finalControl + 1] $ \position -> do
      let index = controlIndex (fromIntegral position)
      causes <-
        sequence
          [ checked "coverage label cause" (labelReleaseCause decision index),
            checked "coverage End cause" (processEndCause process index),
            checked "coverage disappearance cause" (predefinedDisappearanceCause probe index)
          ]
      forM_ causes $ \cause ->
        assertCoverage cause (expected (\(_, controlLimit, _) -> controlLimit) position)
    pure True
  where
    batches = do
      count <- QC.chooseInt (3, 8)
      QC.vectorOf count ((,) <$> QC.chooseInt (1, 3) <*> QC.chooseInt (1, 3))

    installBatch fixture (graph, state, previousSequence, previousControl, boundaries) (structuralCount, controlCount) = do
      assertCurrentNablaQueriesAgainstReference state
      let lastSequence = previousSequence + structuralCount
          lastControl = previousControl + controlCount
          index = controlIndex (fromIntegral lastControl)
          checkpoint = topologyControlCheckpoint index "generated-coverage-checkpoint"
      controlled <- checked "generated control progress" (prepareStructuralControlProgress index (clearPreparationReadinessChange state))
      assertEqual "generated strict control progress wakes preparation" True (fst (takePreparationReadinessChange (fst (commitStructuralControlProgress controlled))))
      assertCurrentNablaQueriesAgainstReference (fst (commitStructuralControlProgress controlled))
      (nextGraph, applied) <-
        foldM
          ( \(oldGraph, oldState) position -> do
              sequenceNumber <- checked "generated structural sequence" (mkStructuralSequence (fromIntegral position))
              let apply =
                    applyNeutralOccurrence
                      (fixture {sequenceOne = sequenceNumber})
                      fixture.views
                      fixture.local
                      (structuralAppliedVector oldState)
                      0x35
                      (fromIntegral position)
                      (fromIntegral position)
              (newGraph, newState) <- apply oldGraph (clearPreparationReadinessChange oldState)
              assertEqual "new structural occurrence wakes preparation" True (fst (takePreparationReadinessChange newState))
              (replayedGraph, replayedState) <- apply newGraph (clearPreparationReadinessChange newState)
              assertEqual "exact structural occurrence replay is silent" False (fst (takePreparationReadinessChange replayedState))
              pure (replayedGraph, replayedState)
          )
          (graph, fst (commitStructuralControlProgress controlled))
          [previousSequence + 1 .. lastSequence]
      let remoteReport = structuralAppliedReport fixture.remote (structuralAppliedVector applied) index
      assertCurrentNablaQueriesAgainstReference applied
      report <- checked "generated remote report" (prepareStructuralReport remoteReport (clearPreparationReadinessChange applied))
      assertEqual "new remote report alone does not wake preparation" False (fst (takePreparationReadinessChange (fst (commitStructuralReport report))))
      assertCurrentNablaQueriesAgainstReference (fst (commitStructuralReport report))
      proposal <- checked "generated cut proposal" (prepareTopologyCutProposal fixture.views checkpoint (fst (commitStructuralReport report)))
      ready <- case proposal of
        TopologyCutProposalReady value -> pure value
        TopologyCutProposalUnavailable -> assertFailure "generated cut proposal unavailable"
        TopologyCutProposalHeld dependencies -> assertFailure ("generated cut proposal held: " <> show dependencies)
      let proposed = commitTopologyCutProposal ready
          cutId = topologyCutAnnounceId (preparedTopologyCutProposalAnnounce ready)
      assertEqual "open proposal alone does not wake preparation" False (fst (takePreparationReadinessChange proposed))
      newest <- checked "generated newest sequence" (mkStructuralSequence (fromIntegral lastSequence))
      assertEqual
        "an uninstalled proposal cannot cover its new occurrence"
        Nothing
        (installedCutCoveringOccurrence (structuralOccurrenceId fixture.local newest) proposed)
      acceptance <- checked "generated cut acceptance" (prepareTopologyCutAcceptance (topologyCutAcceptance cutId remoteReport) proposed)
      assertEqual "acceptance alone does not wake preparation" False (fst (takePreparationReadinessChange (commitTopologyCutAcceptance acceptance)))
      establishment <- checked "generated cut establishment" (prepareTopologyCutEstablishment (commitTopologyCutAcceptance acceptance))
      let installed = commitTopologyCutEstablishment establishment
      assertEqual "installed cut wakes preparation" True (fst (takePreparationReadinessChange installed))
      assertInstalledCoverageAgainstReference [] [cutId] proposed
      assertInstalledCoverageAgainstReference [] [] installed
      acknowledgement <-
        checked
          "generated cut acknowledgement"
          (prepareTopologyCutEstablishedAck (topologyCutEstablishedAck cutId fixture.remote (heraldMembershipGenerationId fixture.membership)) (clearPreparationReadinessChange installed))
      assertEqual "installed acknowledgement alone does not wake preparation" False (fst (takePreparationReadinessChange (commitTopologyCutEstablishedAck acknowledgement)))
      assertCurrentNablaQueriesAgainstReference (commitTopologyCutEstablishedAck acknowledgement)
      pure (nextGraph, commitTopologyCutEstablishedAck acknowledgement, lastSequence, lastControl, boundaries <> [(lastSequence, lastControl, cutId)])

caseOpenJoinCheckpoint :: Assertion
caseOpenJoinCheckpoint = do
  fixture <- progressFixture
  let firstIndex = controlIndex 1
      joinIndex = controlIndex 2
      firstCheckpoint = topologyControlCheckpoint firstIndex "original topology"
      plainCheckpoint = topologyControlCheckpoint joinIndex "unrelated control"
      advanceControl index state = fst . commitStructuralControlProgress <$> checked "advance control" (prepareStructuralControlProgress index state)
      retainReport report state = fst . commitStructuralReport <$> checked "retain remote report" (prepareStructuralReport report state)
      propose checkpoint state = do
        prepared <- checked "propose checked cut" (prepareTopologyCutProposal fixture.views checkpoint state)
        case prepared of
          TopologyCutProposalReady ready -> pure ready
          TopologyCutProposalUnavailable -> assertFailure "checked cut was unexpectedly unavailable"
          TopologyCutProposalHeld dependencies -> assertFailure ("checked cut was unexpectedly held: " <> show dependencies)
  localControlled <- advanceControl firstIndex fixture.state
  localApplied <- applyLocalNeutral fixture localControlled
  receiverControlled <- advanceControl firstIndex fixture.receiverState
  receiverApplied <- applyNeutralWithViews fixture fixture.receiverViews receiverControlled
  let firstReport = structuralAppliedReport fixture.remote (structuralAppliedVector localApplied) firstIndex
  matrix <- retainReport firstReport localApplied
  firstProposal <- propose firstCheckpoint matrix
  let firstAnnounce = preparedTopologyCutProposalAnnounce firstProposal
      firstId = topologyCutAnnounceId firstAnnounce
      firstOpen = commitTopologyCutProposal firstProposal
  accepted <- checked "accept original topology" (prepareTopologyCutAcceptance (topologyCutAcceptance firstId firstReport) firstOpen)
  established <- checked "establish original topology" (prepareTopologyCutEstablishment (commitTopologyCutAcceptance accepted))
  acknowledged <- checked "close original topology acknowledgement gate" (prepareTopologyCutEstablishedAck (topologyCutEstablishedAck firstId fixture.remote (heraldMembershipGenerationId fixture.membership)) (commitTopologyCutEstablishment established))
  let original = commitTopologyCutEstablishedAck acknowledged
      firstEstablished = preparedTopologyCutEstablished established
  receiverInstalled <- checked "install original topology at receiver" (prepareTopologyCutEstablished fixture.receiverViews firstCheckpoint firstEstablished receiverApplied)
  let originalReceiver = commitTopologyCutEstablished receiverInstalled
  assertEqual "the prior dynamic topology is valid" (Right ()) (validateStructuralProgressState original)
  assertEqual "the receiver has the same prior dynamic topology" firstId (structuralLastInstalledCutId originalReceiver)

  localAdvanced <- advanceControl joinIndex original
  receiverAdvanced <- advanceControl joinIndex originalReceiver
  let joinReport = structuralAppliedReport fixture.remote (structuralAppliedVector receiverAdvanced) joinIndex
  joinMatrix <- retainReport joinReport localAdvanced
  (_, cachedJoinMatrix, ordinary) <- checked "ordinary control progress cannot repeat the topology" (prepareTopologyCutProposalCached fixture.views plainCheckpoint joinMatrix)
  case ordinary of
    TopologyCutProposalUnavailable -> pure ()
    _ -> assertFailure "an unchanged ordinary control checkpoint proposed a cut"
  admission <- checked "join admission coordinate" (deriveHeraldAdmissionId joinIndex)
  let joinCheckpoint = topologyJoinSealCheckpoint admission joinIndex "join barrier"
  (joinWork, _, joinPreparation) <- checked "reuse graph for join barrier" (prepareTopologyCutProposalCached fixture.views joinCheckpoint cachedJoinMatrix)
  assertEqual "join seal reuses the projection while changing proposal eligibility" TopologyProjectionReused joinWork
  joinReference <- checked "reference join barrier" (prepareTopologyCutProposal fixture.views joinCheckpoint cachedJoinMatrix)
  assertProposalEquivalent joinReference joinPreparation
  joinProposal <- case joinPreparation of
    TopologyCutProposalReady ready -> pure ready
    _ -> assertFailure "cached unchanged join checkpoint did not propose a cut"
  let announce = preparedTopologyCutProposalAnnounce joinProposal
      cut = topologyCutAnnounceCut announce
      cutId = topologyCutAnnounceId announce
      proposed = commitTopologyCutProposal joinProposal
  assertEqual "join changes only the control frontier" (structuralAppliedVector original) (topologyFrontierStructuralVersionVector (topologyCutFrontier cut))
  assertEqual "join preserves the established topology digest" (topologyCutOccurrenceDigest (topologyCutEstablishedCut firstEstablished)) (topologyCutOccurrenceDigest cut)
  assertEqual "the new cut names its real installed predecessor" firstId (topologyPredecessorCutId (topologyCutPredecessor cut))
  assertEqual "the proposal does not install before fixed-member acceptance" firstId (structuralLastInstalledCutId proposed)
  assertEqual "the join checkpoint is valid before establishment" (Right ()) (validateStructuralProgressState proposed)
  case prepareTopologyCutAnnounce fixture.receiverViews plainCheckpoint announce receiverAdvanced of
    Left StructuralProgressCutDoesNotAdvance -> pure ()
    Left problem -> assertFailure ("wrong ordinary checkpoint rejection: " <> show problem)
    Right _ -> assertFailure "the same cut entered without its join checkpoint"
  futureAdmission <- checked "future admission coordinate" (deriveHeraldAdmissionId (controlIndex 3))
  case prepareTopologyCutAnnounce fixture.receiverViews (topologyJoinSealCheckpoint futureAdmission joinIndex "future join") announce receiverAdvanced of
    Left StructuralProgressCutDoesNotAdvance -> pure ()
    Left problem -> assertFailure ("wrong future checkpoint rejection: " <> show problem)
    Right _ -> assertFailure "a future admission authorized this earlier checkpoint"
  received <- checked "receiver admits the exact join checkpoint" (prepareTopologyCutAnnounce fixture.receiverViews joinCheckpoint announce receiverAdvanced)
  let receiverOpen = commitTopologyCutAnnounce received
  assertEqual "the receiver's open join checkpoint is valid" (Right ()) (validateStructuralProgressState receiverOpen)
  acceptance <- case preparedTopologyCutAnnounceClassification received of
    TopologyCutAnnounceAccepted value -> pure value
    other -> assertFailure ("join checkpoint did not produce its local acceptance: " <> show other)
  retained <- checked "retain the receiver's real acceptance" (prepareTopologyCutAcceptance acceptance proposed)
  completed <- checked "establish join checkpoint" (prepareTopologyCutEstablishment (commitTopologyCutAcceptance retained))
  assertEqual "the same witness remains valid after establishment" (Right ()) (validateStructuralProgressState (commitTopologyCutEstablishment completed))
  receivedEstablished <- checked "receiver installs the join checkpoint" (prepareTopologyCutEstablished fixture.receiverViews joinCheckpoint (preparedTopologyCutEstablished completed) receiverOpen)
  assertEqual "receiver installation remains valid" (Right ()) (validateStructuralProgressState (commitTopologyCutEstablished receivedEstablished))
  final <- checked "acknowledge the installed join checkpoint" (prepareTopologyCutEstablishedAck (topologyCutEstablishedAck cutId fixture.remote (heraldMembershipGenerationId fixture.membership)) (commitTopologyCutEstablishment completed))
  assertEqual "acknowledgement closes the valid join checkpoint" Nothing (structuralOpenCut (commitTopologyCutEstablishedAck final))
  assertEqual "the complete checkpoint remains valid" (Right ()) (validateStructuralProgressState (commitTopologyCutEstablishedAck final))

caseCutChain :: Assertion
caseCutChain = do
  fixture <- progressFixture
  localControl <-
    checked
      "local control advance"
      (prepareStructuralControlProgress (controlIndex 1) fixture.state)
  let (withLocalControl, controlClassification) =
        commitStructuralControlProgress localControl
  assertEqual
    "control progress classification"
    StructuralControlAdvanced
    controlClassification
  let remoteControlReport =
        structuralAppliedReport
          fixture.remote
          fixture.emptyVector
          (controlIndex 1)
  preparedRemoteControl <-
    checked
      "remote control-only report"
      (prepareStructuralReport remoteControlReport withLocalControl)
  let (withControlMatrix, _) = commitStructuralReport preparedRemoteControl
  controlOnly <-
    checked
      "control-only proposal"
      ( prepareTopologyCutProposal
          fixture.views
          (topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1")
          withControlMatrix
      )
  case controlOnly of
    TopologyCutProposalUnavailable -> pure ()
    TopologyCutProposalHeld dependencies ->
      assertFailure ("control-only proposal unexpectedly held: " <> show dependencies)
    TopologyCutProposalReady _ ->
      assertFailure "control-only Start progress created a topology cut"
  withOccurrence <- applyLocalNeutral fixture withControlMatrix
  let
    remoteReport =
      structuralAppliedReport
        fixture.remote
        (structuralAppliedVector withOccurrence)
        (controlIndex 1)
  retainedReport <-
    checked "remote report" (prepareStructuralReport remoteReport withOccurrence)
  let (withMatrix, reportClassification) = commitStructuralReport retainedReport
  assertEqual
    "remote report classification"
    StructuralReportAdvanced
    reportClassification
  assertBool
    "the complete matrix exposes a stable frontier"
    (structuralStableFrontier withMatrix /= Nothing)
  proposed <-
    checked
      "topology proposal"
      ( prepareTopologyCutProposal
          fixture.views
          (topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1")
          withMatrix
      )
  preparedProposal <- case proposed of
    TopologyCutProposalReady ready -> pure ready
    TopologyCutProposalUnavailable -> assertFailure "control-changing proposal unavailable"
    TopologyCutProposalHeld dependencies ->
      assertFailure ("empty projection unexpectedly held: " <> show dependencies)
  let announced = preparedTopologyCutProposalAnnounce preparedProposal
      afterProposal = commitTopologyCutProposal preparedProposal
      cutId = topologyCutAnnounceId announced
      firstCut = topologyCutAnnounceCut announced
      remoteAcceptance = topologyCutAcceptance cutId remoteReport
      prematureAcknowledgement =
        topologyCutEstablishedAck
          cutId
          fixture.remote
          (heraldMembershipGenerationId fixture.membership)
      behindReport =
        structuralAppliedReport
          fixture.remote
          fixture.emptyVector
          (controlIndex 1)
      behindAcceptance = topologyCutAcceptance cutId behindReport
  projected <-
    checked
      "reference cut projection"
      ( Reconciliation.prepareStructuralProjectionAtVector
          fixture.views
          (topologyFrontierStructuralVersionVector (topologyCutFrontier firstCut))
          (topologyFrontierAppliedControlPrefix (topologyCutFrontier firstCut))
          (structuralProgressReconciliation withMatrix)
      )
  snapshot <- case projected of
    Left dependencies ->
      assertFailure ("reference cut projection was held: " <> show dependencies)
    Right ready -> pure ready
  let referenceInput = TopologyDigestReferenceInput fixture.initialProjection snapshot
      referenceDigest =
        deriveTopologyOccurrenceDigest
          (encodeTopologyDigestReferenceInput referenceInput)
      occurrence = structuralOccurrenceId fixture.local fixture.sequenceOne
      derivedAuthority = structuralAuthorityEpoch occurrence cutId
  assertEqual
    "the cut digest input is exactly the immutable genesis digest and induced projection"
    referenceDigest
    (topologyCutOccurrenceDigest firstCut)
  assertEqual
    "the structural authority is derived only from the completed occurrence and cut"
    (StructuralAuthorityEpochView occurrence cutId)
    (authorityEpochView derivedAuthority)
  assertEqual
    "the first dynamic cut names the distinguished genesis predecessor"
    (sameGenerationPredecessor (structuralGenesisCutId afterProposal))
    (topologyCutPredecessor (topologyCutAnnounceCut announced))
  checked "open proposal invariant" (validateStructuralProgressState afterProposal)
  case prepareTopologyCutEstablishedAck prematureAcknowledgement afterProposal of
    Left (StructuralProgressAckForUnknownCut rejected) ->
      assertEqual "premature ack names the uninstalled cut" cutId rejected
    Left problem -> assertFailure ("wrong premature-ack fault: " <> show problem)
    Right _ -> assertFailure "an ack closed a cut before establishment"
  case prepareTopologyCutAcceptance behindAcceptance afterProposal of
    Left (StructuralProgressAcceptanceDoesNotCover reporter) ->
      assertEqual "under-covering acceptance names its reporter" fixture.remote reporter
    Left problem -> assertFailure ("wrong under-covering acceptance fault: " <> show problem)
    Right _ -> assertFailure "an under-covering acceptance entered the retained evidence"
  retainedAcceptance <-
    checked
      "remote acceptance"
      (prepareTopologyCutAcceptance remoteAcceptance afterProposal)
  assertEqual
    "the second fixed member completes evidence"
    TopologyCutAcceptanceCompletesSet
    (preparedTopologyCutAcceptanceClassification retainedAcceptance)
  established <-
    checked
      "local establishment"
      (prepareTopologyCutEstablishment (commitTopologyCutAcceptance retainedAcceptance))
  let afterEstablishment = commitTopologyCutEstablishment established
  assertEqual
    "the announcer installs without loopback"
    cutId
    (structuralLastInstalledCutId afterEstablishment)
  assertEqual
    "the installed cut exposes its exact control frontier"
    (controlIndex 1)
    (structuralLastInstalledCutControlPrefix afterEstablishment)
  assertEqual
    "an older writer authority minimum is floored by the installed cut"
    (controlIndex 1)
    ( structuralPublicationControlPrerequisite
        (controlIndex 0)
        afterEstablishment
    )
  assertEqual
    "a newer writer authority minimum remains authoritative"
    (controlIndex 2)
    ( structuralPublicationControlPrerequisite
        (controlIndex 2)
        afterEstablishment
    )
  laterControl <-
    checked
      "post-install control advance"
      (prepareStructuralControlProgress (controlIndex 2) afterEstablishment)
  let (withLaterAppliedControl, _) = commitStructuralControlProgress laterControl
  assertEqual
    "the publication floor remains the selected cut frontier when applied control advances"
    (controlIndex 1)
    (structuralLastInstalledCutControlPrefix withLaterAppliedControl)
  assertEqual
    "publication stamping does not over-constrain to unrelated applied progress"
    (controlIndex 1)
    ( structuralPublicationControlPrerequisite
        (controlIndex 0)
        withLaterAppliedControl
    )
  assertEqual
    "the cut covers the one stable structural occurrence"
    [structuralOccurrenceId fixture.local fixture.sequenceOne]
    (topologyCutNewlyCoveredOccurrences cutId afterEstablishment)
  endedProcess <-
    checked
      "coverage process epoch"
      (mkProcessEpochId (ByteString.replicate 32 0x37))
  coveredEndCause <-
    checked
      "covered End cause"
      (processEndCause endedProcess (controlIndex 1))
  futureEndCause <-
    checked
      "future End cause"
      (processEndCause endedProcess (controlIndex 2))
  assertEqual
    "a structural cause is covered by the installed vector"
    (Just cutId)
    ( installedCutCoveringCause
        (structuralOccurrenceCause occurrence)
        afterEstablishment
    )
  assertEqual
    "a control cause is covered by the installed control prefix"
    (Just cutId)
    (installedCutCoveringCause coveredEndCause afterEstablishment)
  assertEqual
    "a later control cause remains uncovered"
    Nothing
    (installedCutCoveringCause futureEndCause afterEstablishment)
  assertEqual
    "reconnect repair reoffers the installed predecessor chain before any successor"
    [PeerTopologyCutEstablished (preparedTopologyCutEstablished established)]
    (PeerControl.structuralRetryControls fixture.remote afterEstablishment)
  checked
    "partially acknowledged installed-cut invariant"
    (validateStructuralProgressState afterEstablishment)
  remoteAck <-
    checked
      "remote established acknowledgement"
      ( prepareTopologyCutEstablishedAck
          ( topologyCutEstablishedAck
              cutId
              fixture.remote
              (heraldMembershipGenerationId fixture.membership)
          )
          afterEstablishment
      )
  assertEqual
    "the last acknowledgement closes the one-open gate"
    TopologyCutAckAllMembers
    (preparedTopologyCutAckClassification remoteAck)
  let complete = commitTopologyCutEstablishedAck remoteAck
  assertEqual "no open successor remains" Nothing (structuralOpenCut complete)
  delayedFirst <-
    checked
      "delayed first announce"
      ( prepareTopologyCutAnnounce
          fixture.views
          (topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1")
          announced
          complete
      )
  assertEqual
    "a delayed genesis-predecessor announce reoffers the installed cut"
    (TopologyCutAnnounceStale (preparedTopologyCutEstablished established))
    (preparedTopologyCutAnnounceClassification delayedFirst)
  assertEqual
    "repairing the delayed first announce does not reopen or retain it"
    complete
    (commitTopologyCutAnnounce delayedFirst)
  regressedCut <-
    checked
      "regressed successor cut"
      ( topologyCut
          (sameGenerationPredecessor cutId)
          ( topologyFrontier
              fixture.emptyVector
              (controlIndex 1)
          )
          (topologyCutOccurrenceDigest firstCut)
      )
  let regressedAnnounce = topologyCutAnnounce fixture.local regressedCut
  case prepareTopologyCutAnnounce
    fixture.views
    (topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1")
    regressedAnnounce
    complete of
    Left StructuralProgressCutFrontierRegressed -> pure ()
    Left problem -> assertFailure ("wrong regressing-frontier fault: " <> show problem)
    Right _ -> assertFailure "a component-regressing successor cut was admitted"
  checked "complete owner invariant" (validateStructuralProgressState complete)

caseCandidateFrontierMutants :: Assertion
caseCandidateFrontierMutants = do
  fixture <- progressFixture
  preparedControl <-
    checked
      "causal fixture control advance"
      (prepareStructuralControlProgress (controlIndex 1) fixture.state)
  let (withControl, _) = commitStructuralControlProgress preparedControl
  (remoteGraph, withRemote) <-
    applyNeutralOccurrence
      fixture
      fixture.views
      fixture.remote
      fixture.emptyVector
      0x40
      0x41
      0x42
      Graph.emptyState
      withControl
  let remotePredecessor = structuralAppliedVector withRemote
  (_, causallyClosed) <-
    applyNeutralOccurrence
      fixture
      fixture.views
      fixture.local
      remotePredecessor
      0x43
      0x44
      0x45
      remoteGraph
      withRemote
  nonClosedVector <-
    checked
      "bounded but causally nonclosed vector"
      ( mkStructuralVersionVector
          fixture.membership
          [ (fixture.local, structuralPrefixThrough fixture.sequenceOne),
            (fixture.remote, fixture.emptyPrefix)
          ]
      )
  nonClosedCut <-
    checked
      "causally nonclosed cut"
      ( topologyCut
          (sameGenerationPredecessor (structuralGenesisCutId causallyClosed))
          ( topologyFrontier
              nonClosedVector
              (controlIndex 1)
          )
          (deriveTopologyOccurrenceDigest "causally-nonclosed-candidate")
      )
  let nonClosedAnnounce = topologyCutAnnounce fixture.local nonClosedCut
  case prepareTopologyCutAnnounce
    fixture.views
    (topologyControlCheckpoint (controlIndex 1) "causal-checkpoint")
    nonClosedAnnounce
    causallyClosed of
    Left StructuralProgressPredecessorNotCovered -> pure ()
    Left problem -> assertFailure ("wrong causal-hole fault: " <> show problem)
    Right _ -> assertFailure "a merely bounded, causally nonclosed cut was admitted"

-- Original prerequisites may fall even while source sequences and causal
-- predecessors advance. A candidate must retain every earlier requirement,
-- including the original scan's first error when different rows fail.
propCandidateOccurrenceSummary :: QC.Property
propCandidateOccurrenceSummary =
  QC.forAll ((,) <$> QC.chooseInt (2, 6) <*> QC.chooseInt (0, 5)) $ \(count, offset) -> QC.ioProperty $ do
    fixture <- progressFixture
    let high = controlIndex (fromIntegral (count + offset + 2))
        genesis = structuralGenesisCutId fixture.state
        apply :: HeraldEpoch -> Int -> ControlIndex -> Word8 -> Word8 -> Word8 -> Graph.State -> StructuralProgressState -> IO (Graph.State, StructuralProgressState)
        apply source ordinal prerequisite objectByte sourceByte digestByte graph state = do
          sequenceNumber <- checked "candidate summary source sequence" (mkStructuralSequence (fromIntegral ordinal))
          applyVertexOccurrenceAt
            genesis
            prerequisite
            NeutralVertexRole
            NeutralVertexCarrier
            (fixture {sequenceOne = sequenceNumber})
            fixture.views
            source
            (structuralAppliedVector state)
            objectByte
            sourceByte
            digestByte
            graph
            state
        prefixFor :: Int -> IO StructuralPrefix
        prefixFor 0 = pure fixture.emptyPrefix
        prefixFor count' = structuralPrefixThrough <$> checked "candidate summary prefix" (mkStructuralSequence (fromIntegral count'))
        vectorFor :: Int -> Int -> IO StructuralVersionVector
        vectorFor localCount remoteCount = do
          localPrefix <- prefixFor localCount
          remotePrefix <- prefixFor remoteCount
          checked "candidate summary vector" (mkStructuralVersionVector fixture.membership [(fixture.local, localPrefix), (fixture.remote, remotePrefix)])
    control <- checked "candidate summary control advance" (prepareStructuralControlProgress high fixture.state)
    let controlled = fst (commitStructuralControlProgress control)
    (remoteGraph, firstRemote) <- apply fixture.remote (1 :: Int) (controlIndex (controlIndexWord64 high - 1)) 0x50 0x51 0x52 Graph.emptyState controlled
    (localGraph, firstLocal) <- apply fixture.local 1 high 0x60 0x70 0x80 remoteGraph firstRemote
    (bothGraph, bothSources) <- apply fixture.remote 2 (controlIndex 0) 0x53 0x54 0x55 localGraph firstLocal
    (_, complete) <-
      foldM
        ( \(graph, state) ordinal ->
            apply fixture.local ordinal (controlIndex (controlIndexWord64 high - fromIntegral ordinal)) (fromIntegral (0x60 + ordinal)) (fromIntegral (0x70 + ordinal)) (fromIntegral (0x80 + ordinal)) graph state
        )
        (bothGraph, bothSources)
        [2 .. count]
    checked "candidate summary owner invariant" (validateStructuralProgressState complete)
    forM_ [0 .. count + 1] $ \localCount ->
      forM_ [0 .. 3 :: Int] $ \remoteCount -> do
        vector <- vectorFor localCount remoteCount
        forM_ [0 .. controlIndexWord64 high + 1] $ \rawControl -> do
          let candidate = topologyFrontier vector (controlIndex rawControl)
          assertEqual
            ("candidate summary matches original scan at " <> show (localCount, remoteCount, rawControl))
            (validateCandidateOccurrencesExhaustive candidate complete)
            (validateCandidateOccurrences candidate complete)
    let fullVector = structuralAppliedVector complete
        belowGreatest = controlIndex (controlIndexWord64 high - 1)
    assertEqual "empty candidate needs no occurrence or control" (Right ()) (validateCandidateOccurrences (topologyFrontier fixture.emptyVector (controlIndex 0)) complete)
    assertEqual "complete candidate accepts the original greatest prerequisite" (Right ()) (validateCandidateOccurrences (topologyFrontier fullVector high) complete)
    assertEqual
      "the latest lower prerequisite cannot hide an earlier greater one"
      (Left (StructuralProgressControlPrerequisiteNotCovered belowGreatest high))
      (validateCandidateOccurrences (topologyFrontier fullVector belowGreatest) complete)
    causalHole <- vectorFor (1 :: Int) (0 :: Int)
    assertEqual "the first row checks closure before its control prerequisite" (Left StructuralProgressPredecessorNotCovered) (validateCandidateOccurrences (topologyFrontier causalHole (controlIndex 0)) complete)
    laterHole <- vectorFor count (1 :: Int)
    assertEqual
      "an earlier control failure precedes a later row's closure failure"
      (Left (StructuralProgressControlPrerequisiteNotCovered (controlIndex 0) high))
      (validateCandidateOccurrences (topologyFrontier laterHole (controlIndex 0)) complete)
    pure True

caseIncomparableReport :: Assertion
caseIncomparableReport = do
  fixture <- progressFixture
  leftVector <-
    checked
      "left vector"
      ( mkStructuralVersionVector
          fixture.membership
          [ (fixture.local, structuralPrefixThrough fixture.sequenceOne),
            (fixture.remote, fixture.emptyPrefix)
          ]
      )
  rightVector <-
    checked
      "right vector"
      ( mkStructuralVersionVector
          fixture.membership
          [ (fixture.local, fixture.emptyPrefix),
            (fixture.remote, structuralPrefixThrough fixture.sequenceOne)
          ]
      )
  let firstReport =
        structuralAppliedReport fixture.remote leftVector (controlIndex 0)
      incomparable =
        structuralAppliedReport fixture.remote rightVector (controlIndex 0)
  prepared <- checked "first report" (prepareStructuralReport firstReport fixture.state)
  case prepareStructuralReport incomparable (fst (commitStructuralReport prepared)) of
    Left (StructuralProgressIncomparableReport reporter) ->
      assertEqual "fault names the reporter" fixture.remote reporter
    Left problem -> assertFailure ("wrong report fault: " <> show problem)
    Right _ -> assertFailure "incomparable report was admitted"

-- Routing does not change report contents or the all-member cut predicate.
-- A reporter offers one cumulative snapshot even when three peers are bound.
caseAnnouncerOnlyReports :: Assertion
caseAnnouncerOnlyReports = do
  (system, membership, initialProjection, heralds) <- reportRoutingFixture
  states <- traverse (reportRoutingState system membership initialProjection) heralds
  let announcer = NonEmpty.head heralds
  sends <- traverse (reportOffers announcer (NonEmpty.toList heralds)) states
  assertEqual "four reporters generate three direct offers, without self send" 3 (sum (fmap length sends))
  forM_ states $ \state -> do
    let local = structuralProgressLocalHerald state
    forM_ (filter (/= local) (NonEmpty.toList heralds)) $ \remote -> do
      let expected = [PeerStructuralAppliedReported (structuralLocalReport state) | local /= announcer, remote == announcer]
      assertEqual "reconnect only reoffers the report to its announcer" expected (PeerControl.structuralRetryControls remote state)
      assertEqual "an explicit reoffer repeats the unchanged current snapshot" expected (PeerControl.structuralRetryControls remote state)
  where
    reportOffers announcer heralds state = do
      bindings <- traverse reportBinding (filter (/= structuralProgressLocalHerald state) heralds)
      let destinations = StructuralProgress.structuralReportBindings state bindings
          report = structuralLocalReport state
          offered = effectBatchMembers (StructuralProgress.normalizeStructuralReportEffects Map.empty destinations report (orderedEffectBatch []))
      expectedBindings <- if structuralProgressLocalHerald state == announcer then pure [] else pure <$> reportBinding announcer
      assertEqual "the routing target is exactly the remote announcer" expectedBindings destinations
      assertEqual "quiescent normalization offers the complete current report" [SendPeerControl binding (PeerStructuralAppliedReported report) | binding <- expectedBindings] offered
      pure offered

propDirectReportStableFrontier :: QC.Property
propDirectReportStableFrontier =
  QC.forAll ((:|) <$> QC.chooseInt (0, 50) <*> QC.vectorOf 3 (QC.chooseInt (0, 50))) $ \cursors ->
    QC.forAll (QC.shuffle [0 .. 3]) $ \deliveryOrder -> QC.ioProperty $ do
      (system, membership, initialProjection, heralds) <- reportRoutingFixture
      states <- traverse (reportRoutingState system membership initialProjection) heralds
      advanced <- traverse advance (NonEmpty.zip states cursors)
      let announcer = NonEmpty.head heralds
          initialAnnouncer = NonEmpty.head advanced
      offered <- traverse (directReports (NonEmpty.toList heralds)) advanced
      let delivered = concat [NonEmpty.toList offered !! index | index <- deliveryOrder]
      routed <- foldM retain initialAnnouncer delivered
      reference <- foldM retain initialAnnouncer (fmap structuralLocalReport advanced)
      assertEqual "direct routing keeps the exact all-to-all announcer frontier" (structuralStableFrontier reference) (structuralStableFrontier routed)
      assertEqual
        "no report is lost when non-minimum cursors advance first"
        (Just (topologyFrontier (emptyStructuralVersionVector membership) (controlIndex (fromIntegral (minimum cursors)))))
        (structuralStableFrontier routed)
      assertEqual "only the announcer collects every reporter" 3 (length delivered)
      assertEqual "routing fixture keeps the first reporter as announcer" announcer (structuralProgressLocalHerald routed)
  where
    advance (state, cursor) = fst . commitStructuralControlProgress <$> checked "generated report progress" (prepareStructuralControlProgress (controlIndex (fromIntegral cursor)) state)
    directReports heralds state = do
      bindings <- traverse reportBinding (filter (/= structuralProgressLocalHerald state) heralds)
      pure [structuralLocalReport state | _ <- StructuralProgress.structuralReportBindings state bindings]
    retain state report = fst . commitStructuralReport <$> checked "generated direct report admission" (prepareStructuralReport report state)

caseJoiningObserverReport :: Assertion
caseJoiningObserverReport = do
  (system, membership, initialProjection, heralds) <- reportRoutingFixture
  applicant <- checked "report applicant" (mkHeraldEpoch (fixtureIdentifierBytes 0x50))
  observer <- checked "observer report state" (initialJoiningStructuralProgressState system applicant membership initialProjection (Reconciliation.emptyStructuralAppliedState (emptyStructuralVersionVector membership)))
  bindings <- traverse reportBinding (NonEmpty.toList heralds)
  assertEqual "an applicant does not offer a report for its predecessor base" [] (StructuralProgress.structuralReportBindings observer bindings)
  forM_ heralds $ \remote -> assertEqual "observer reconnect carries no current-generation report" [] (PeerControl.structuralRetryControls remote observer)

reportRoutingFixture :: IO (SystemId, HeraldMembershipGeneration, InitialProjectionDigest, NonEmpty HeraldEpoch)
reportRoutingFixture = do
  system <- checked "routing system" (mkSystemId (fixtureIdentifierBytes 0x31))
  heralds <- traverse (checked "routing Herald" . mkHeraldEpoch . fixtureIdentifierBytes) (0x32 :| [0x33 .. 0x35])
  membership <- checked "routing membership" (genesisHeraldMembershipGeneration system heralds)
  initialProjection <- checked "routing initial projection" (mkInitialProjectionDigest (ByteString.replicate 32 0x36))
  pure (system, membership, initialProjection, heralds)

reportRoutingState :: SystemId -> HeraldMembershipGeneration -> InitialProjectionDigest -> HeraldEpoch -> IO StructuralProgressState
reportRoutingState system membership initialProjection local =
  checked "routing state" (initialStructuralProgressState system local membership initialProjection (Reconciliation.emptyStructuralAppliedState (emptyStructuralVersionVector membership)))

caseNormalizeIncomparableReportSlots :: Assertion
caseNormalizeIncomparableReportSlots = do
  fixture <- progressFixture
  binding <- reportBinding fixture.remote
  (intermediate, finalReport) <- incomparableReports fixture
  let initialReport =
        structuralAppliedReport fixture.local fixture.emptyVector (controlIndex 0)
      normalized =
        StructuralProgress.normalizeStructuralReportEffects
          (Map.singleton binding initialReport)
          [binding]
          finalReport
          ( orderedEffectBatch
              [ SendPeerControl binding (PeerStructuralAppliedReported intermediate),
                SendPeerControl binding (PeerStructuralAppliedReported finalReport)
              ]
          )
  assertEqual
    "only the authoritative final snapshot escapes the transition"
    [SendPeerControl binding (PeerStructuralAppliedReported finalReport)]
    (effectBatchMembers normalized)
  first <- checked "retain incomparable intermediate" (prepareStructuralReport intermediate fixture.state)
  case prepareStructuralReport finalReport (fst (commitStructuralReport first)) of
    Left (StructuralProgressIncomparableReport _) -> pure ()
    Left problem ->
      assertFailure
        ("the report pair produced the wrong Graph fault: " <> show problem)
    Right _ ->
      assertFailure "the incomparable report pair was admitted by Graph"

caseNormalizeReportOrdering :: Assertion
caseNormalizeReportOrdering = do
  fixture <- progressFixture
  binding <- reportBinding fixture.remote
  (intermediate, finalReport) <- incomparableReports fixture
  let topologyEffect =
        SendPeerControl
          binding
          ( PeerTopologyCutAccepted
              (topologyCutAcceptance (structuralGenesisCutId fixture.state) intermediate)
          )
      normalized =
        StructuralProgress.normalizeStructuralReportEffects
          Map.empty
          [binding]
          finalReport
          ( orderedEffectBatch
              [ SendPeerControl binding (PeerStructuralAppliedReported intermediate),
                topologyEffect,
                SendPeerControl binding (PeerStructuralAppliedReported finalReport)
              ]
          )
  assertEqual
    "the final snapshot replaces the first same-generation slot before topology replay"
    [ SendPeerControl binding (PeerStructuralAppliedReported finalReport),
      topologyEffect
    ]
    (effectBatchMembers normalized)

caseAcceptedBindingDefersReport :: Assertion
caseAcceptedBindingDefersReport = do
  fixture <- progressFixture
  binding@(PeerBinding _ _ _ candidate) <- reportBinding fixture.remote
  let finalReport =
        structuralAppliedReport fixture.local fixture.emptyVector (controlIndex 0)
      accepted =
        SetPeerCandidateDisposition
          candidate
          (PeerHelloAccepted FirstBinding binding)
      normalized =
        StructuralProgress.normalizeStructuralReportEffects
          Map.empty
          [binding]
          finalReport
          ( orderedEffectBatch
              [ accepted,
                SendPeerControl binding (PeerStructuralAppliedReported finalReport)
              ]
          )
  assertEqual
    "Hello acceptance exposes no semantic catalogue before stream resume"
    [accepted]
    (effectBatchMembers normalized)

caseExactFinalReportSuppressed :: Assertion
caseExactFinalReportSuppressed = do
  fixture <- progressFixture
  binding <- reportBinding fixture.remote
  let finalReport =
        structuralAppliedReport fixture.local fixture.emptyVector (controlIndex 0)
      normalized =
        StructuralProgress.normalizeStructuralReportEffects
          (Map.singleton binding finalReport)
          [binding]
          finalReport
          ( orderedEffectBatch
              [SendPeerControl binding (PeerStructuralAppliedReported finalReport)]
          )
  assertEqual
    "an exact already-assumed snapshot is not reoffered"
    []
    (effectBatchMembers normalized)

casePredecessorReportSlotPreserved :: Assertion
casePredecessorReportSlotPreserved = do
  fixture <- progressFixture
  binding <- reportBinding fixture.remote
  successorMembership <-
    checked
      "single-survivor membership"
      ( retireHeraldMembershipGeneration
          (controlIndex 2)
          (MembershipFixtures.fixtureRetirementResolution (controlIndex 2))
          fixture.remote
          fixture.membership
      )
  successorVector <-
    pure (emptyStructuralVersionVector successorMembership)
  let predecessorReport =
        structuralAppliedReport fixture.local fixture.emptyVector (controlIndex 0)
      finalReport =
        structuralAppliedReport fixture.local successorVector (controlIndex 2)
      topologyEffect =
        SendPeerControl
          binding
          ( PeerTopologyCutAccepted
              (topologyCutAcceptance (structuralGenesisCutId fixture.state) predecessorReport)
          )
      normalized =
        StructuralProgress.normalizeStructuralReportEffects
          Map.empty
          [binding]
          finalReport
          ( orderedEffectBatch
              [ SendPeerControl binding (PeerStructuralAppliedReported predecessorReport),
                topologyEffect,
                SendPeerControl binding (PeerStructuralAppliedReported finalReport)
              ]
          )
  assertEqual
    "the predecessor report is not rewritten across the generation boundary"
    [ SendPeerControl binding (PeerStructuralAppliedReported predecessorReport),
      topologyEffect,
      SendPeerControl binding (PeerStructuralAppliedReported finalReport)
    ]
    (effectBatchMembers normalized)

incomparableReports ::
  ProgressFixture -> IO (StructuralAppliedReport, StructuralAppliedReport)
incomparableReports fixture = do
  leftVector <-
    checked
      "normalization left vector"
      ( mkStructuralVersionVector
          fixture.membership
          [ (fixture.local, structuralPrefixThrough fixture.sequenceOne),
            (fixture.remote, fixture.emptyPrefix)
          ]
      )
  rightVector <-
    checked
      "normalization right vector"
      ( mkStructuralVersionVector
          fixture.membership
          [ (fixture.local, fixture.emptyPrefix),
            (fixture.remote, structuralPrefixThrough fixture.sequenceOne)
          ]
      )
  pure
    ( structuralAppliedReport fixture.remote leftVector (controlIndex 0),
      structuralAppliedReport fixture.remote rightVector (controlIndex 1)
    )

reportBinding :: HeraldEpoch -> IO PeerBinding
reportBinding remote = do
  remoteId <- checked "report binding Herald id" (mkHeraldId (ByteString.replicate 32 0x42))
  let candidate = PeerCandidate remoteId remote (ConnectionNonce 17)
  pure (PeerBinding remoteId remote firstPeerBindingGeneration candidate)

caseHeldEstablishedRelease :: Assertion
caseHeldEstablishedRelease = do
  fixture <- progressFixture
  localControl <-
    checked
      "source control advance"
      (prepareStructuralControlProgress (controlIndex 1) fixture.state)
  let (withLocalControl, _) = commitStructuralControlProgress localControl
  withOccurrence <- applyLocalNeutral fixture withLocalControl
  let remoteReport =
        structuralAppliedReport
          fixture.remote
          (structuralAppliedVector withOccurrence)
          (controlIndex 1)
  retainedReport <-
    checked
      "source remote report"
      (prepareStructuralReport remoteReport withOccurrence)
  let (withMatrix, _) = commitStructuralReport retainedReport
  proposal <-
    checked
      "source proposal"
      ( prepareTopologyCutProposal
          fixture.views
          checkpoint
          withMatrix
      )
  preparedProposal <- case proposal of
    TopologyCutProposalReady ready -> pure ready
    TopologyCutProposalUnavailable -> assertFailure "source proposal was unavailable"
    TopologyCutProposalHeld dependencies ->
      assertFailure ("source proposal was held: " <> show dependencies)
  let afterProposal = commitTopologyCutProposal preparedProposal
      cutId = topologyCutAnnounceId (preparedTopologyCutProposalAnnounce preparedProposal)
  acceptance <-
    checked
      "source remote acceptance"
      ( prepareTopologyCutAcceptance
          (topologyCutAcceptance cutId remoteReport)
          afterProposal
      )
  establishment <-
    checked
      "source establishment"
      ( prepareTopologyCutEstablishment
          (commitTopologyCutAcceptance acceptance)
      )
  let established = preparedTopologyCutEstablished establishment
  held <-
    checked
      "ahead established admission"
      ( prepareTopologyCutEstablished
          fixture.receiverViews
          checkpoint
          established
          fixture.receiverState
      )
  assertEqual
    "the ahead frontier is dependency-held"
    TopologyCutEstablishedHeld
    (preparedTopologyCutEstablishedClassification held)
  let heldState = commitTopologyCutEstablished held
  assertEqual
    "the exact established fact is retained"
    (Just established)
    (structuralPendingEstablished heldState)
  receiverControl <-
    checked
      "receiver control advance"
      (prepareStructuralControlProgress (controlIndex 1) heldState)
  let (controlledReceiver, _) = commitStructuralControlProgress receiverControl
  progressedReceiver <-
    applyNeutralWithViews fixture fixture.receiverViews controlledReceiver
  released <-
    checked
      "retained established release"
      ( prepareTopologyCutEstablished
          fixture.receiverViews
          checkpoint
          established
          progressedReceiver
      )
  case preparedTopologyCutEstablishedClassification released of
    TopologyCutEstablishedInstalled _ -> pure ()
    classification ->
      assertFailure ("retained establishment did not install: " <> show classification)
  let installed = commitTopologyCutEstablished released
  assertEqual
    "release clears only the matching retained fact"
    Nothing
    (structuralPendingEstablished installed)
  assertEqual
    "the repair transcript is retained in predecessor order"
    [established]
    (structuralInstalledEstablishedChain installed)
  assertEqual
    "a non-announcer reconnect retries only its current local structural report"
    [PeerStructuralAppliedReported (structuralLocalReport installed)]
    (PeerControl.structuralRetryControls fixture.local installed)
  checked "released owner invariant" (validateStructuralProgressState installed)
  where
    checkpoint = topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1"

caseHeldEstablishedChainRelease :: Assertion
caseHeldEstablishedChainRelease = do
  fixture <- progressFixture
  localControl <-
    checked
      "chain source control advance"
      (prepareStructuralControlProgress (controlIndex 1) fixture.state)
  let (controlledSource, _) = commitStructuralControlProgress localControl
  (sourceGraph, sourceWithFirstOccurrence) <-
    applyNeutralOccurrence
      fixture
      fixture.views
      fixture.local
      fixture.emptyVector
      0x35
      0x36
      0x37
      Graph.emptyState
      controlledSource
  (firstEstablished, sourceAfterFirst) <-
    establishNextCut fixture sourceWithFirstOccurrence
  firstAck <-
    checked
      "chain source first remote acknowledgement"
      ( prepareTopologyCutEstablishedAck
          ( topologyCutEstablishedAck
              (topologyCutEstablishedId firstEstablished)
              fixture.remote
              (heraldMembershipGenerationId fixture.membership)
          )
          sourceAfterFirst
      )
  let sourceWithClosedFirst = commitTopologyCutEstablishedAck firstAck
      firstFrontier = structuralAppliedVector sourceWithClosedFirst
  (_, sourceWithSecondOccurrence) <-
    applyNeutralOccurrence
      fixture
      fixture.views
      fixture.remote
      firstFrontier
      0x38
      0x39
      0x3a
      sourceGraph
      sourceWithClosedFirst
  (secondEstablished, _) <-
    establishNextCut fixture sourceWithSecondOccurrence

  heldFirst <-
    checked
      "chain receiver first Established"
      ( prepareTopologyCutEstablished
          fixture.receiverViews
          checkpoint
          firstEstablished
          fixture.receiverState
      )
  assertEqual
    "the first Established waits for its local frontier"
    TopologyCutEstablishedHeld
    (preparedTopologyCutEstablishedClassification heldFirst)
  heldSecond <-
    checked
      "chain receiver second Established"
      ( prepareTopologyCutEstablished
          fixture.receiverViews
          checkpoint
          secondEstablished
          (commitTopologyCutEstablished heldFirst)
      )
  let retainedChain = commitTopologyCutEstablished heldSecond
  assertEqual
    "both reconnect facts remain in predecessor order"
    [firstEstablished, secondEstablished]
    (structuralPendingEstablishedChain retainedChain)
  assertEqual
    "held replay does not cover its unapplied occurrence"
    Nothing
    (installedCutCoveringOccurrence (structuralOccurrenceId fixture.local fixture.sequenceOne) retainedChain)
  duplicateSecond <-
    checked
      "duplicate second held Established"
      ( prepareTopologyCutEstablished
          fixture.receiverViews
          checkpoint
          secondEstablished
          retainedChain
      )
  assertEqual
    "an exact retry neither grows nor reorders the hold"
    retainedChain
    (commitTopologyCutEstablished duplicateSecond)
  conflictingSecond <-
    checked
      "conflicting second Established fixture"
      ( topologyCutEstablished
          (topologyCutEstablishedId secondEstablished)
          (topologyCutEstablishedCut secondEstablished)
          [ topologyCutAcceptance
              (topologyCutEstablishedId secondEstablished)
              (structuralAppliedReport fixture.local fixture.emptyVector (controlIndex 0)),
            topologyCutAcceptance
              (topologyCutEstablishedId secondEstablished)
              (structuralAppliedReport fixture.remote fixture.emptyVector (controlIndex 0))
          ]
      )
  case prepareTopologyCutEstablished
    fixture.receiverViews
    checkpoint
    conflictingSecond
    retainedChain of
    Left (StructuralProgressEstablishedConflict rejected) ->
      assertEqual
        "the conflict names the retained second cut"
        (topologyCutEstablishedId secondEstablished)
        rejected
    Left problem ->
      assertFailure ("conflicting held Established produced the wrong fault: " <> show problem)
    Right _ -> assertFailure "non-exact evidence replaced a retained Established"
  checked
    "held Established chain invariant"
    (validateStructuralProgressState retainedChain)

  receiverControl <-
    checked
      "chain receiver control advance"
      (prepareStructuralControlProgress (controlIndex 1) retainedChain)
  let (controlledReceiver, _) = commitStructuralControlProgress receiverControl
  (receiverGraph, receiverWithFirstOccurrence) <-
    applyNeutralOccurrence
      fixture
      fixture.receiverViews
      fixture.local
      fixture.emptyVector
      0x35
      0x36
      0x37
      Graph.emptyState
      controlledReceiver
  (_, readyReceiver) <-
    applyNeutralOccurrence
      fixture
      fixture.receiverViews
      fixture.remote
      (structuralAppliedVector receiverWithFirstOccurrence)
      0x38
      0x39
      0x3a
      receiverGraph
      receiverWithFirstOccurrence
  released <- releaseRetainedChain fixture readyReceiver
  assertEqual
    "one dependency release installs X followed by Y"
    [firstEstablished, secondEstablished]
    (structuralInstalledEstablishedChain released)
  assertEqual
    "the ordered hold is empty after the fixed point"
    []
    (structuralPendingEstablishedChain released)
  assertEqual
    "replayed local occurrence selects the first Established"
    (Just (topologyCutEstablishedId firstEstablished))
    (installedCutCoveringOccurrence (structuralOccurrenceId fixture.local fixture.sequenceOne) released)
  assertEqual
    "replayed remote occurrence selects the second Established"
    (Just (topologyCutEstablishedId secondEstablished))
    (installedCutCoveringOccurrence (structuralOccurrenceId fixture.remote fixture.sequenceOne) released)
  assertInstalledCoverageAgainstReference [] (map topologyCutEstablishedId [firstEstablished, secondEstablished]) retainedChain
  assertInstalledCoverageAgainstReference [] [] released
  checked "released Established chain invariant" (validateStructuralProgressState released)
  where
    checkpoint = topologyControlCheckpoint (controlIndex 1) "start-checkpoint-1"

    establishNextCut fixture state = do
      let remoteReport =
            structuralAppliedReport
              fixture.remote
              (structuralAppliedVector state)
              (controlIndex 1)
      retainedReport <-
        checked
          "chain source remote report"
          (prepareStructuralReport remoteReport state)
      proposal <-
        checked
          "chain source proposal"
          ( prepareTopologyCutProposal
              fixture.views
              checkpoint
              (fst (commitStructuralReport retainedReport))
          )
      preparedProposal <- case proposal of
        TopologyCutProposalReady ready -> pure ready
        TopologyCutProposalUnavailable ->
          assertFailure "chain source proposal was unavailable"
        TopologyCutProposalHeld dependencies ->
          assertFailure ("chain source proposal was held: " <> show dependencies)
      let afterProposal = commitTopologyCutProposal preparedProposal
          cutId = topologyCutAnnounceId (preparedTopologyCutProposalAnnounce preparedProposal)
      acceptance <-
        checked
          "chain source remote acceptance"
          ( prepareTopologyCutAcceptance
              (topologyCutAcceptance cutId remoteReport)
              afterProposal
          )
      establishment <-
        checked
          "chain source establishment"
          ( prepareTopologyCutEstablishment
              (commitTopologyCutAcceptance acceptance)
          )
      pure
        ( preparedTopologyCutEstablished establishment,
          commitTopologyCutEstablishment establishment
        )

    releaseRetainedChain fixture state =
      case structuralPendingEstablished state of
        Nothing -> pure state
        Just retained -> do
          prepared <-
            checked
              "release retained chain member"
              ( prepareTopologyCutEstablished
                  fixture.receiverViews
                  checkpoint
                  retained
                  state
              )
          case preparedTopologyCutEstablishedClassification prepared of
            TopologyCutEstablishedInstalled _ ->
              releaseRetainedChain fixture (commitTopologyCutEstablished prepared)
            classification ->
              assertFailure
                ("retained chain member did not install: " <> show classification)

caseEmptyDebtRetention :: Assertion
caseEmptyDebtRetention = do
  fixture <- progressFixture
  let occurrence =
        structuralOccurrenceId fixture.local fixture.sequenceOne
  first <-
    checked
      "empty retention"
      ( Alignment.prepareStructuralDebtRetention
          (structuralOccurrenceCause occurrence)
          emptyStructuralDebtSet
          Alignment.emptyState
      )
  assertEqual
    "empty consequence is a no-op"
    Alignment.StructuralDebtRetentionUnchanged
    (Alignment.preparedStructuralDebtRetention first)

-- The reference input deliberately has no prospective cut or authority field.
-- Its encoder independently mirrors the two-field production transcript so the
-- cut-chain property detects either an added self-reference or any other drift.
data TopologyDigestReferenceInput
  = TopologyDigestReferenceInput
      InitialProjectionDigest
      Reconciliation.StructuralProjectionSnapshot

encodeTopologyDigestReferenceInput :: TopologyDigestReferenceInput -> ByteString
encodeTopologyDigestReferenceInput (TopologyDigestReferenceInput initialProjection snapshot) =
  ByteString.Lazy.toStrict
    ( Builder.toLazyByteString
        ( putSizedBytes (ByteString.Char8.pack "ECLIPS-TOPOLOGY-PROJECTION")
            <> putSizedBytes (initialProjectionDigestBytes initialProjection)
            <> putSizedBytes (Reconciliation.structuralProjectionSnapshotCanonicalBytes snapshot)
        )
    )
  where
    putSizedBytes bytes =
      Builder.word64BE (fromIntegral (ByteString.length bytes))
        <> Builder.byteString bytes

data ProgressFixture = ProgressFixture
  { local :: HeraldEpoch,
    remote :: HeraldEpoch,
    membership :: HeraldMembershipGeneration,
    sequenceOne :: StructuralSequence,
    emptyPrefix :: StructuralPrefix,
    emptyVector :: StructuralVersionVector,
    initialProjection :: InitialProjectionDigest,
    state :: StructuralProgressState,
    views :: Reconciliation.ReconciliationViews,
    receiverState :: StructuralProgressState,
    receiverViews :: Reconciliation.ReconciliationViews
  }

progressFixture :: IO ProgressFixture
progressFixture = do
  system <- checked "system" (mkSystemId (fixtureIdentifierBytes 0x31))
  local <- checked "local Herald" (mkHeraldEpoch (fixtureIdentifierBytes 0x32))
  remote <- checked "remote Herald" (mkHeraldEpoch (fixtureIdentifierBytes 0x33))
  initialProjection <-
    checked
      "initial projection"
      (mkInitialProjectionDigest (ByteString.replicate 32 0x34))
  let members = local :| [remote]
  membership <-
    checked
      "membership generation"
      (genesisHeraldMembershipGeneration system members)
  let emptyVector = emptyStructuralVersionVector membership
  state <-
    checked
      "initial progress"
      ( initialStructuralProgressState
          system
          local
          membership
          initialProjection
          (Reconciliation.emptyStructuralAppliedState emptyVector)
      )
  receiverState <-
    checked
      "initial receiver progress"
      ( initialStructuralProgressState
          system
          remote
          membership
          initialProjection
          (Reconciliation.emptyStructuralAppliedState emptyVector)
      )
  sequenceOne <-
    checked
      "sequence one"
      (mkStructuralSequence 1)
  emptyPrefix <- case structuralVersionVectorComponent local emptyVector of
    Nothing -> assertFailure "local component missing" >> error "unreachable"
    Just prefix -> pure prefix
  pure
    ProgressFixture
      { local,
        remote,
        membership,
        sequenceOne,
        emptyPrefix,
        emptyVector,
        initialProjection,
        state,
        views = viewsFor local,
        receiverState,
        receiverViews = viewsFor remote
      }
  where
    viewsFor herald =
      Reconciliation.reconciliationViews
        herald
        ( Map.singleton
            (profileSortFor NeutralVertexRole)
            ( sortOccurrence
                (profileSortFor NeutralVertexRole)
                ( predefinedOccurrenceFor
                    NeutralVertexRole
                    (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)
                )
            )
        )
        Set.empty
        Map.empty
        Set.empty

applyLocalNeutral :: ProgressFixture -> StructuralProgressState -> IO StructuralProgressState
applyLocalNeutral fixture = applyNeutralWithViews fixture fixture.views

applyNeutralWithViews ::
  ProgressFixture ->
  Reconciliation.ReconciliationViews ->
  StructuralProgressState ->
  IO StructuralProgressState
applyNeutralWithViews fixture views =
  fmap snd
    . applyNeutralOccurrence
      fixture
      views
      fixture.local
      fixture.emptyVector
      0x35
      0x36
      0x37
      Graph.emptyState

applyNeutralOccurrence ::
  ProgressFixture ->
  Reconciliation.ReconciliationViews ->
  HeraldEpoch ->
  StructuralVersionVector ->
  Word8 ->
  Word8 ->
  Word8 ->
  Graph.State ->
  StructuralProgressState ->
  IO (Graph.State, StructuralProgressState)
applyNeutralOccurrence = applyVertexOccurrence NeutralVertexRole NeutralVertexCarrier

applyVertexOccurrence ::
  PredefinedSortRole ->
  StructuralCarrierRole ->
  ProgressFixture ->
  Reconciliation.ReconciliationViews ->
  HeraldEpoch ->
  StructuralVersionVector ->
  Word8 ->
  Word8 ->
  Word8 ->
  Graph.State ->
  StructuralProgressState ->
  IO (Graph.State, StructuralProgressState)
applyVertexOccurrence role carrierRole fixture views source predecessor objectByte sourceByte digestByte graph state = do
  applyVertexOccurrenceAt (structuralGenesisCutId state) (controlIndex 1) role carrierRole fixture views source predecessor objectByte sourceByte digestByte graph state

applyVertexOccurrenceAt ::
  TopologyCutId ->
  ControlIndex ->
  PredefinedSortRole ->
  StructuralCarrierRole ->
  ProgressFixture ->
  Reconciliation.ReconciliationViews ->
  HeraldEpoch ->
  StructuralVersionVector ->
  Word8 ->
  Word8 ->
  Word8 ->
  Graph.State ->
  StructuralProgressState ->
  IO (Graph.State, StructuralProgressState)
applyVertexOccurrenceAt sourceTopology prerequisite role carrierRole fixture views source predecessor objectByte sourceByte digestByte graph state = do
  object <- checked "neutral object" (mkGlobalObjectId (fixtureIdentifierBytes objectByte))
  sourceNabla <- checked "structural source" (mkNablaId (fixtureIdentifierBytes sourceByte))
  objectField <- checked "object field" (mkFieldName "object_id")
  labelField <- checked "label field" (mkFieldName "label")
  sortField <- checked "root sort field" (mkFieldName "sort_id")
  sequencingField <- checked "sequencing object field" (mkFieldName "sequencing_object")
  let rootFields = case role of
        NablaRole -> [(sortField, bytesValue (sortIdBytes (profileSortFor NeutralVertexRole))), (sequencingField, optionalGlobalUniqueIdValue Nothing)]
        _ -> []
  value <-
    checked
      "neutral value"
      ( recordValue
          ( [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
              (labelField, labelValue (VoidLabel, 0))
            ]
              <> rootFields
          )
      )
  let carrierSortId = profileSortFor role
      carrierOccurrenceId =
        predefinedOccurrenceFor
          role
          (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)
      carrierSort = sortOccurrence carrierSortId carrierOccurrenceId
      publication =
        publicationId
          sourceNabla
          genesisAuthorityEpoch
          source
          (nablaSequence 1)
  checkedPublication <-
    checked
      "neutral publication"
      ( mkCheckedPublication
          (predefinedCatalogueDescriptor (profileEntryFor role))
          publication
          value
      )
  let occurrence = structuralOccurrenceId source fixture.sequenceOne
      application =
        Reconciliation.structuralApplication
          occurrence
          predecessor
          carrierSort
          checkedPublication
          prerequisite
  prepared <-
    case Reconciliation.prepareStructuralReconciliation
      views
      application
      Nothing
      (structuralProgressReconciliation state) of
      Right (Reconciliation.StructuralReady ready) -> pure ready
      Right (Reconciliation.StructuralHeld dependencies) ->
        assertFailure ("neutral occurrence held: " <> show dependencies)
      Left problem -> assertFailure ("neutral occurrence rejected: " <> show problem)
  digest <-
    checked
      "structural publication digest"
      (mkStructuralPublicationDigest (ByteString.replicate 32 digestByte))
  stamp <-
    checked
      "structural occurrence stamp"
      ( mkStructuralOccurrenceStamp
          occurrence
          predecessor
          publication
          digest
          carrierRole
      )
  preparedApplication <-
    checked
      "structural progress application"
      ( prepareStructuralApplication
          stamp
          sourceTopology
          prepared
          graph
          state
      )
  let (graphSuccessor, successor, _) = commitStructuralApplication preparedApplication
  pure (graphSuccessor, successor)

checked :: (Show problem) => String -> Either problem value -> IO value
checked label result = case result of
  Left problem -> assertFailure (label <> ": " <> show problem)
  Right value -> pure value

-- Independent reference: walk immutable installed cut predecessors and inspect
-- their domain frontiers directly. This deliberately never calls a production
-- coverage query, reads the new index, or reproduces its breakpoint algorithm.
referenceInstalledCutCoversCause :: TopologyCutId -> StructuralConsequenceCause -> StructuralProgressState -> Bool
referenceInstalledCutCoversCause cutId cause state =
  case lookupInstalledTopologyCut cutId state of
    Nothing -> False
    Just installed ->
      let cut = installedTopologyCutCut installed
          frontier = topologyCutFrontier cut
          direct = case structuralConsequenceCauseView cause of
            StructuralOccurrenceCauseView occurrence ->
              vectorCovers occurrence (topologyFrontierStructuralVersionVector frontier)
                || case topologyPredecessorView (topologyCutPredecessor cut) of
                  MembershipSuccessorPredecessorView _ _ _ terminalVector _ _ -> vectorCovers occurrence terminalVector
                  _ -> False
            LabelReleaseCauseView _ index -> index <= topologyFrontierAppliedControlPrefix frontier
            ProcessEndCauseView _ index -> index <= topologyFrontierAppliedControlPrefix frontier
            PredefinedDisappearanceCauseView _ index -> index <= topologyFrontierAppliedControlPrefix frontier
       in direct || referenceInstalledCutCoversCause (topologyPredecessorCutId (topologyCutPredecessor cut)) cause state
  where
    vectorCovers occurrence vector =
      maybe
        False
        (>= structuralPrefixThrough (structuralOccurrenceSourceSequence occurrence))
        (structuralVersionVectorComponent (structuralOccurrenceSourceHeraldEpoch occurrence) vector)

assertCoverageAgainstReference :: [StructuralConsequenceCause] -> [TopologyCutId] -> StructuralProgressState -> Assertion
assertCoverageAgainstReference causes additionalTargets state = do
  unknown <- checked "reference unknown cut identity" (mkTopologyCutId (ByteString.replicate 32 0xfe))
  assertEqual "reference target is not an installed cut" Nothing (lookupInstalledTopologyCut unknown state)
  let installed = structuralInstalledCutIds state
      targets = structuralGenesisCutId state : unknown : installed <> additionalTargets
  forM_ causes $ \cause -> do
    let earliest = find (\cut -> referenceInstalledCutCoversCause cut cause state) installed
        current = structuralLastInstalledCutId state
    assertEqual ("reference earliest: " <> show cause) earliest (installedCutCoveringCause cause state)
    assertEqual
      ("reference current: " <> show cause)
      (if referenceInstalledCutCoversCause current cause state then Just current else Nothing)
      (currentInstalledCutCoveringCause cause state)
    forM_ targets $ \target ->
      assertEqual
        ("reference arbitrary target: " <> show (target, cause))
        (referenceInstalledCutCoversCause target cause state)
        (installedCutCoversCause target cause state)

-- Exercise every retained occurrence and its next position, every live source
-- at its first position, a missing source, and all three control-cause variants
-- through one beyond applied progress. Extra targets name held/abandoned cuts.
assertInstalledCoverageAgainstReference :: [StructuralOccurrenceId] -> [TopologyCutId] -> StructuralProgressState -> Assertion
assertInstalledCoverageAgainstReference additionalOccurrences additionalTargets state = do
  assertCurrentNablaQueriesAgainstReference state
  decision <- checked "reference decision" (mkLabelDecisionId (fixtureIdentifierBytes 0x71))
  process <- checked "reference process" (mkProcessEpochId (fixtureIdentifierBytes 0x72))
  probe <- checked "reference probe" (deriveDisappearanceProbeId (controlIndex 1))
  missingSource <- checked "reference absent source" (mkHeraldEpoch (fixtureIdentifierBytes 0xfd))
  first <- checked "reference first sequence" (mkStructuralSequence 1)
  let history = Reconciliation.structuralAppliedHistoryOccurrences (structuralProgressReconciliation state)
      live = [structuralOccurrenceId source first | (source, _) <- structuralVersionVectorEntries (structuralAppliedVector state)]
  successors <-
    mapM
      ( \occurrence -> do
          next <- checked "reference next sequence" (mkStructuralSequence (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence) + 1))
          pure (structuralOccurrenceId (structuralOccurrenceSourceHeraldEpoch occurrence) next)
      )
      history
  let structural = map structuralOccurrenceCause (Set.toList (Set.fromList (structuralOccurrenceId missingSource first : additionalOccurrences <> history <> successors <> live)))
  control <-
    fmap concat
      $ mapM
        ( \position -> do
            let index = controlIndex position
            sequence
              [ checked "reference label cause" (labelReleaseCause decision index),
                checked "reference End cause" (processEndCause process index),
                checked "reference disappearance cause" (predefinedDisappearanceCause probe index)
              ]
        )
        [1 .. controlIndexWord64 (structuralAppliedControlPrefix state) + 1]
  assertCoverageAgainstReference (structural <> control) additionalTargets state

-- Share the retained current-coordinate calculation across repeated questions,
-- including wrong-role and absent objects, and compare it with independent
-- scalar reconstruction. Graph.emptyState is intentional: historical authority
-- never reads the separately maintained current vertex index. This helper also
-- runs at the admission and membership-successor coverage audit points.
assertCurrentNablaQueriesAgainstReference :: StructuralProgressState -> Assertion
assertCurrentNablaQueriesAgainstReference state = do
  absent <- checked "absent historical writer" (mkNablaId (fixtureIdentifierBytes 0xfc))
  let writers =
        absent
          : [ nablaIdFromGlobalObjectId object
            | (object, _) <- Reconciliation.structuralAppliedRetainedObjectRoles (structuralProgressReconciliation state)
            ]
      prepared = prepareStructuralQueries Graph.emptyState state
  forM_ (writers <> reverse writers <> writers) $ \writer ->
    assertEqual
      "retained historical authority agrees with scalar reconstruction after owner transition"
      (structuralNablaProjectionAtCoordinate writer (structuralLastInstalledCutId state) (structuralAppliedControlPrefix state) state)
      (lookupPreparedCurrentNablaProjection writer prepared)
