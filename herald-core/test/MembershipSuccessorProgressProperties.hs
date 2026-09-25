{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module MembershipSuccessorProgressProperties
  ( tests,
    baseForLineageWithPayloads,
  )
where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    TopologyCutId,
    controlIndex,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSystemId,
    nablaSequence,
    publicationId,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication (mkCheckedPublication)
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Startup (mkInitialProjectionDigest)
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorFromClaimedCoordinates,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence (processEndCause, structuralOccurrenceCause)
import Eclips.Domain.Topology
  ( TopologyCut,
    TopologyOccurrenceDigest,
    deriveTopologyCutId,
    membershipSuccessorPredecessor,
    mkTopologyOccurrenceDigest,
    topologyCut,
    topologyCutFrontier,
    topologyCutPredecessor,
    topologyFrontier,
    topologyFrontierStructuralVersionVector,
    topologyPredecessorCutId,
  )
import Eclips.Domain.Value
  ( LabelOwner (VoidLabel),
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    recordValue,
  )
import Eclips.Herald.Discovery.Internal
  ( ConnectionNonce (ConnectionNonce),
    PeerBinding (PeerBinding),
    PeerCandidate (PeerCandidate),
    firstPeerBindingGeneration,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendPeerControl),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Graph.Progress
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAcceptanceReporter,
    topologyCutAnnounce,
    topologyCutAnnounceCut,
    topologyCutAnnounceId,
    topologyCutEstablished,
    topologyCutEstablishedAcceptances,
    topologyCutEstablishedAck,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( SuccessorStructuralBase,
    TerminalSourcePredecessorBase,
    deriveTerminalSourceUnion,
    deriveTerminalStructuralPayloadDigest,
    emptyTerminalSourcePayloadArchive,
    establishedSuccessorStructuralBase,
    sealTerminalSourceInventory,
    successorStructuralBaseEstablishedUnion,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInstalledPredecessorBase,
    terminalSourceInventory,
    terminalSourceUnionAcceptance,
    terminalSourceUnionDigest,
    terminalSourceUnionEstablished,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionReady,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTerminalPredecessorVector,
    terminalSourceUnionTopologyPredecessor,
    terminalStructuralOccurrence,
  )
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Input (PeerControl (PeerStructuralAppliedReported))
import Eclips.Herald.PeerPublication (mkStructuralOccurrenceStamp)
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.Alignment qualified as Alignment
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import GenesisFixtures qualified as MembershipFixtures
import StructuralProgressProperties (assertInstalledCoverageAgainstReference, assertPortableProgressImport)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "membership-successor structural progress"
    [ testCase "admission observer imports historical retirement without local acknowledgements" caseObserverRetirement,
      testCase "repeated bases preserve historical payload and admit descendant survivor work once" caseRepeatedRetirementLineage,
      testCase "repeated bases retain abandoned cuts and survivor-live-ahead payload" caseRepeatedAbandonedCuts,
      testCase "overlapping retirement establishes one compound base without an intermediate cut" caseOverlappingRetirementLineage,
      testCase
        "the exact terminal base atomically rebases progress and retains the historical cut chain"
        caseInstallSuccessorBase,
      testCase
        "a replacement announcer receives current progress without another input"
        caseReplacementAnnouncerReport,
      testCase
        "an agreed base cannot install ahead of local retirement-control application"
        caseBaseHeldUntilControlApplied,
      testCase
        "Alignment admits repaired retired-source history only through installed successor lineage"
        caseNonEmptyBaseHeldUntilHistoryApplied,
      testCase
        "the membership-successor base supersedes a fully accepted unfinished ordinary cut"
        caseSuccessorBaseSupersedesUnfinishedCut,
      testCase
        "a membership successor retains and follows a real dynamic predecessor cut"
        caseInstallSuccessorAfterDynamicCut,
      testCase
        "ordinary cuts reject a membership-successor predecessor transcript"
        caseOrdinaryCutRejectsMembershipSuccessorPredecessor,
      testCase
        "an exact active-survivor predecessor report is stale after successor cutover"
        casePredecessorReportIsStale,
      testCase
        "an exact installed predecessor acceptance is stale after successor cutover"
        casePredecessorAcceptanceIsStale,
      testCase
        "reconnect replay recognizes only the exact installed predecessor establishment"
        casePredecessorEstablishmentIsDuplicate,
      testCase
        "an exact abandoned predecessor-cut acceptance is stale after successor cutover"
        caseAbandonedPredecessorAcceptanceIsStale,
      testCase
        "an exact installed predecessor acknowledgement is stale after successor cutover"
        casePredecessorAcknowledgementIsStale,
      testCase
        "an exact membership-successor base acknowledgement is stale without an ordinary ack phase"
        caseSuccessorBaseAcknowledgementIsStale
    ]

caseObserverRetirement :: Assertion
caseObserverRetirement = do
  fixture <- progressFixture
  let local = checkedValue "future admission" (mkHeraldEpoch (identifierBytes 0x75))
      system = checkedValue "original system" (mkSystemId (identifierBytes 0x40))
      projection = checkedValue "original initial projection" (mkInitialProjectionDigest (identifierBytes 0x51))
  initial <- checked "joining observer" (initialJoiningStructuralProgressState system local fixture.predecessorMembership projection (Reconciliation.emptyStructuralAppliedState fixture.emptyPredecessorVector))
  applied <- checked "historical retirement control" (prepareStructuralControlProgress (controlIndex 99) initial)
  let (ready, _) = commitStructuralControlProgress applied
  prepared <- checked "import historical retirement" (prepareMembershipSuccessorBaseInstallation fixture.successorMembership fixture.base fixture.successorCut ready)
  let successor = commitMembershipSuccessorBaseInstallation prepared
  assertBool "observer role remains restricted" (structuralProgressIsJoiningObserver successor)
  assertEqual "observer cannot report" [] (structuralReportEntries successor)
  assertEqual "ahead applied control is retained" (controlIndex 99) (structuralAppliedControlPrefix successor)
  installed <- maybe (assertFailure "missing imported base" >> error "missing imported base") pure (lookupInstalledTopologyCut (deriveTopologyCutId fixture.successorCut) successor)
  assertEqual "no local union acceptance" Nothing (installedTopologyCutOwnAcceptance installed)
  assertEqual "no local acknowledgement" Nothing (installedTopologyCutOwnAcknowledgement installed)
  assertInstalledCoverageAgainstReference [] [] successor
  checked "historical observer retirement invariant" (validateStructuralProgressState successor)

caseRepeatedRetirementLineage :: Assertion
caseRepeatedRetirementLineage = do
  f <- progressFixture
  (firstCut, firstBase) <- nonEmptyTerminalBase f
  withHistory <- applyRetiredTerminalOccurrence f f.readyState
  firstPrepared <- checked "first retirement base" (prepareMembershipSuccessorBaseInstallation f.successorMembership firstBase firstCut withHistory)
  let first = commitMembershipSuccessorBaseInstallation firstPrepared
      g2 = checkedValue "second retirement" (retireHeraldMembershipGeneration (controlIndex 9) (MembershipFixtures.fixtureRetirementResolution (controlIndex 9)) f.h3 f.successorMembership)
      history = checkedValue "retained three-generation history" (Membership.heraldMembershipHistory (f.predecessorMembership :| [f.successorMembership, g2]))
      lineage = checkedValue "second base lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.successorMembership) (heraldMembershipGenerationId g2) history)
  control <- checked "second retirement control" (prepareStructuralControlProgress (controlIndex 9) first)
  let (ready, _) = commitStructuralControlProgress control
  (secondCut, secondBase) <- emptyBaseForLineage (checkedValue "full closure history" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.predecessorMembership) (heraldMembershipGenerationId g2) history)) lineage ready f.terminalDigest
  prepared <- checked "second base installs after its established predecessor" (prepareMembershipSuccessorBaseInstallation g2 secondBase secondCut ready)
  let final = commitMembershipSuccessorBaseInstallation prepared
      origin = heraldMembershipGenerationId f.predecessorMembership
      target = heraldMembershipGenerationId g2
      retiredCause = structuralOccurrenceCause (structuralOccurrenceId f.h4 firstStructuralSequence)
  assertEqual "the established cut chain is preserved" [deriveTopologyCutId firstCut, deriveTopologyCutId secondCut] (structuralInstalledCutIds final)
  assertBool "the final cut still covers first-retirement payload" (installedCutCoversCause (deriveTopologyCutId secondCut) retiredCause final)
  assertEqual "earliest coverage remains the first retirement base" (Just (deriveTopologyCutId firstCut)) (installedCutCoveringCause retiredCause final)
  assertEqual "current coverage inherits the first retirement's payload" (Just (deriveTopologyCutId secondCut)) (currentInstalledCutCoveringCause retiredCause final)
  assertBool "settlement composes both actual installed bases" (isJust (structuralDescendantSettlement origin target final))
  assertBool "historical established target stays provable" (isJust (structuralDescendantSettlement origin (heraldMembershipGenerationId f.successorMembership) final))
  let stamp =
        checkedValue
          "old accepted survivor stamp"
          ( mkStructuralOccurrenceStamp
              (structuralOccurrenceId f.h1 firstStructuralSequence)
              f.emptyPredecessorVector
              (publicationId (checkedValue "surviving Nabla" (mkNablaId (identifierBytes 0x79))) genesisAuthorityEpoch f.h1 (nablaSequence 1))
              (deriveTerminalStructuralPayloadDigest "old-survivor")
              NeutralVertexCarrier
          )
  carried <- checked "carry original stamp across both bases" (carryDescendantStampedSurvivorOccurrence stamp (structuralAppliedVector final) final)
  assertEqual "immutable old stamp is preserved" stamp (Terminal.carriedStampedSurvivorStamp carried)
  assertRejected "the same dot cannot allocate twice" (carryDescendantStampedSurvivorOccurrence stamp (Terminal.carriedStampedSurvivorVector carried) final)
  assertInstalledCoverageAgainstReference [] [] first
  assertInstalledCoverageAgainstReference [] [] final
  checked "repeated retirement Graph invariant" (validateStructuralProgressState final)
  let observerEpoch = checkedValue "portable retirement observer" (mkHeraldEpoch (identifierBytes 0x75))
      system = checkedValue "portable retirement system" (mkSystemId (identifierBytes 0x40))
      initialProjection = checkedValue "portable retirement genesis" (mkInitialProjectionDigest (identifierBytes 0x51))
  observer <- checked "fresh retirement base observer" (initialJoiningStructuralProgressState system observerEpoch f.predecessorMembership initialProjection (Reconciliation.emptyStructuralAppliedState f.emptyPredecessorVector))
  assertPortableProgressImport (emptyViews observerEpoch) observer final

caseRepeatedAbandonedCuts :: Assertion
caseRepeatedAbandonedCuts = do
  f <- progressFixture
  (firstOpen, firstAbandoned) <- openFullyAcceptedDynamicCut f
  firstPrepared <- checked "install first base over its open cut" (prepareMembershipSuccessorBaseInstallation f.successorMembership f.base f.successorCut firstOpen)
  let first = commitMembershipSuccessorBaseInstallation firstPrepared
      g2 = checkedValue "second open-cut retirement" (retireHeraldMembershipGeneration (controlIndex 9) (MembershipFixtures.fixtureRetirementResolution (controlIndex 9)) f.h3 f.successorMembership)
      history = checkedValue "open-cut retirement history" (Membership.heraldMembershipHistory (f.predecessorMembership :| [f.successorMembership, g2]))
      fullLineage = checkedValue "full open-cut lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.predecessorMembership) (heraldMembershipGenerationId g2) history)
      lineage = checkedValue "second open-cut lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.successorMembership) (heraldMembershipGenerationId g2) history)
  control <- checked "advance second retirement control" (prepareStructuralControlProgress (controlIndex 9) first)
  let (controlled, _) = commitStructuralControlProgress control
  withPayload <- applyTerminalOccurrenceFrom f f.h1 controlled
  (secondOpen, secondAbandoned) <- openFullyAcceptedCut (terminalOccurrenceViews f) (topologyControlCheckpointWithConsequence (controlIndex 9) True "second-retirement-open-cut") withPayload
  (secondCut, secondBase) <- emptyBaseForLineage fullLineage lineage secondOpen f.terminalDigest
  assertBool "the unestablished survivor proposal is ahead of the least terminal base" (topologyFrontierStructuralVersionVector (topologyCutFrontier secondAbandoned) /= terminalSourceUnionTerminalPredecessorVector (terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion secondBase)))
  secondPrepared <- checked "install second base preserving both abandoned attempts" (prepareMembershipSuccessorBaseInstallation g2 secondBase secondCut secondOpen)
  let final = commitMembershipSuccessorBaseInstallation secondPrepared
      occurrence = structuralOccurrenceId f.h1 firstStructuralSequence
      late cut vector index = topologyCutAcceptance (deriveTopologyCutId cut) (structuralAppliedReport f.h2 vector index)
      acceptances = [late firstAbandoned f.emptyPredecessorVector (controlIndex 7), late secondAbandoned (structuralAppliedVector secondOpen) (controlIndex 9)]
  assertEqual "only established bases enter the chain" [deriveTopologyCutId f.successorCut, deriveTopologyCutId secondCut] (structuralInstalledCutIds final)
  assertEqual "survivor payload survives without being falsely covered by the base" (lookupAppliedStructuralOccurrence occurrence secondOpen) (lookupAppliedStructuralOccurrence occurrence final)
  assertEqual "the surviving applied prefix remains live-ahead" (Just (structuralPrefixThrough firstStructuralSequence)) (structuralVersionVectorComponent f.h1 (structuralAppliedVector final))
  assertBool "the terminal base does not certify the abandoned survivor occurrence" (not (installedCutCoversCause (deriveTopologyCutId secondCut) (structuralOccurrenceCause occurrence) final))
  assertEqual "abandoned proposals never win earliest coverage" Nothing (installedCutCoveringOccurrence occurrence final)
  assertEqual "current coverage excludes abandoned survivor work" Nothing (currentInstalledCutCoveringOccurrence occurrence final)
  mapM_ (assertStale final) acceptances
  assertInstalledCoverageAgainstReference [occurrence] (map deriveTopologyCutId [firstAbandoned, secondAbandoned]) final
  checked "repeated abandoned-cut invariant" (validateStructuralProgressState final)
  where
    assertStale final acceptance = do
      prepared <- checked "admit exact delayed abandoned-cut acceptance" (prepareTopologyCutAcceptance acceptance final)
      assertEqual "both historical attempts remain recognizable" TopologyCutAcceptanceHistoricalStale (preparedTopologyCutAcceptanceClassification prepared)
      assertBool "historical acceptance is owner-inert" (commitTopologyCutAcceptance prepared == final)

caseOverlappingRetirementLineage :: Assertion
caseOverlappingRetirementLineage = do
  f <- progressFixture
  let g2 = checkedValue "overlapping retirement" (retireHeraldMembershipGeneration (controlIndex 9) (MembershipFixtures.fixtureRetirementResolution (controlIndex 9)) f.h3 f.successorMembership)
      history = checkedValue "overlapping history" (Membership.heraldMembershipHistory (f.predecessorMembership :| [f.successorMembership, g2]))
      lineage = checkedValue "compound anchor lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.predecessorMembership) (heraldMembershipGenerationId g2) history)
  control <- checked "both retirements are already authoritative" (prepareStructuralControlProgress (controlIndex 9) f.readyState)
  let (ready, _) = commitStructuralControlProgress control
  (cut, base) <- emptyBaseForLineage (checkedValue "full closure history" (Membership.heraldMembershipLineage (heraldMembershipGenerationId f.predecessorMembership) (heraldMembershipGenerationId g2) history)) lineage ready f.terminalDigest
  prepared <- checked "compound base skips the impossible intermediate all-member cut" (prepareMembershipSuccessorBaseInstallation g2 base cut ready)
  let final = commitMembershipSuccessorBaseInstallation prepared
  assertEqual "only one compound successor cut was established" [deriveTopologyCutId cut] (structuralInstalledCutIds final)
  assertBool "an origin within the compound closure has settlement evidence" (isJust (structuralDescendantSettlement (heraldMembershipGenerationId f.successorMembership) (heraldMembershipGenerationId g2) final))
  assertEqual "an intermediate target was never established" Nothing (structuralDescendantSettlement (heraldMembershipGenerationId f.predecessorMembership) (heraldMembershipGenerationId f.successorMembership) final)
  assertBool "an old pending successor hold is released by the compound base" (Terminal.successorStructuralBaseReleases (heraldMembershipGenerationId f.successorMembership) base)
  assertInstalledCoverageAgainstReference [] [] final
  checked "compound Graph invariant" (validateStructuralProgressState final)

emptyBaseForLineage :: Membership.HeraldMembershipLineage -> Membership.HeraldMembershipLineage -> StructuralProgressState -> TopologyOccurrenceDigest -> IO (TopologyCut, SuccessorStructuralBase)
emptyBaseForLineage fullLineage lineage progress digest = baseForLineageWithPayloads fullLineage lineage progress digest []

baseForLineageWithPayloads :: Membership.HeraldMembershipLineage -> Membership.HeraldMembershipLineage -> StructuralProgressState -> TopologyOccurrenceDigest -> [Terminal.TerminalStructuralOccurrence] -> IO (TopologyCut, SuccessorStructuralBase)
baseForLineageWithPayloads fullLineage lineage progress digest retained = do
  let origin = Membership.heraldMembershipLineageOrigin lineage
      target = Membership.heraldMembershipLineageTarget lineage
      anchorId = structuralLastInstalledCutId progress
      anchor =
        if anchorId == structuralGenesisCutId progress
          then checkedValue "genesis anchor" (terminalSourceGenesisPredecessorBase origin anchorId)
          else case lookupInstalledTopologyCut anchorId progress of
            Nothing -> error "fixture lost installed anchor"
            Just installed -> checkedValue "installed anchor" (terminalSourceInstalledPredecessorBase origin (installedTopologyCutCut installed))
      chain = structuralInstalledEstablishedChain progress
      bases = fmap successorStructuralBaseEstablishedUnion (structuralInstalledSuccessorBases progress)
      inventories =
        [ fst (checkedValue "compound inventory" (sealTerminalSourceInventory fullLineage lineage retired reporter chain bases (filter ((== retired) . structuralOccurrenceSourceHeraldEpoch . Terminal.terminalStructuralOccurrenceId) retained)))
        | retired <- Membership.heraldMembershipLineageRetiredHeraldEpochs lineage,
          reporter <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs target)
        ]
      union = checkedValue "compound union" (deriveTerminalSourceUnion fullLineage lineage anchor inventories (checkedValue "retained archive" (Terminal.terminalSourcePayloadArchive retained)) (constantDigest digest))
      ready = checkedValue "compound history readiness" (terminalSourceUnionReady (structuralAppliedVector progress) (structuralAppliedControlPrefix progress) union)
      acceptances = [checkedValue "current survivor acceptance" (terminalSourceUnionAcceptance target reporter ready) | reporter <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs target)]
      established = checkedValue "compound certificate" (terminalSourceUnionEstablished target union acceptances)
      predecessor = checkedValue "compound topology predecessor" (terminalSourceUnionTopologyPredecessor lineage union)
      cut = checkedValue "compound cut" (topologyCut predecessor (topologyFrontier (terminalSourceUnionSuccessorInitialVector union) (terminalSourceUnionTerminalControlPrefix union)) digest)
      base = checkedValue "compound base" (establishedSuccessorStructuralBase lineage established cut)
  pure (cut, base)

-- The new generation has no inherited remote matrix. Installing its checked
-- terminal base must create a fresh report offer without later progress.
caseReplacementAnnouncerReport :: Assertion
caseReplacementAnnouncerReport = do
  fixture <- progressFixtureWithEndpointEpochs 0x44 0x11
  let before = fixture.readyState
  assertEqual "the retired Herald was the predecessor announcer" fixture.h4 (NonEmpty.head (structuralProgressMembers before))
  prepared <-
    checked
      "install the replacement announcer's terminal base"
      (prepareMembershipSuccessorBaseInstallation fixture.successorMembership fixture.base fixture.successorCut before)
  let after = commitMembershipSuccessorBaseInstallation prepared
      finalReport = structuralLocalReport after
  assertEqual "the successor announcer is a different surviving Herald" fixture.h2 (NonEmpty.head (structuralProgressMembers after))
  assertEqual "the reporter remains the same surviving member" fixture.h1 (structuralProgressLocalHerald after)
  assertEqual "the exact successor generation is installed" (heraldMembershipGenerationId fixture.successorMembership) (structuralProgressMembershipGenerationId after)
  assertBool "installation changes the report's generation" (structuralLocalReport before /= finalReport)
  assertEqual "no later control progress is needed" (structuralAppliedControlPrefix before) (structuralAppliedControlPrefix after)
  assertEqual "the new generation seeds only its local report" [(fixture.h1, finalReport)] (structuralReportEntries after)
  oldBinding <- replacementReportBinding fixture.h4
  newBinding <- replacementReportBinding fixture.h2
  survivorBinding <- replacementReportBinding fixture.h3
  let normalized =
        StructuralProgress.normalizeStructuralReportEffects
          (Map.singleton oldBinding (structuralLocalReport before))
          (StructuralProgress.structuralReportBindings after [oldBinding, newBinding, survivorBinding])
          finalReport
          (orderedEffectBatch [])
  assertEqual "installing the successor immediately sends only to its announcer" [SendPeerControl newBinding (PeerStructuralAppliedReported finalReport)] (effectBatchMembers normalized)
  assertEqual "replacement-announcer reconnect reoffers the unchanged fresh report" [PeerStructuralAppliedReported finalReport] (PeerControl.structuralRetryControls fixture.h2 after)
  assertEqual "the retired announcer receives no successor report" [] (PeerControl.structuralRetryControls fixture.h4 after)
  assertEqual "another surviving member receives no report" [] (PeerControl.structuralRetryControls fixture.h3 after)
  checked "replacement report progress invariant" (validateStructuralProgressState after)

replacementReportBinding :: HeraldEpoch -> IO PeerBinding
replacementReportBinding remote = do
  remoteId <- checked "report binding Herald id" (mkHeraldId (identifierBytes 0x42))
  let candidate = PeerCandidate remoteId remote (ConnectionNonce 17)
  pure (PeerBinding remoteId remote firstPeerBindingGeneration candidate)

caseInstallSuccessorBase :: Assertion
caseInstallSuccessorBase = do
  fixture <- progressFixture
  prepared <-
    checked
      "prepare membership-successor installation"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          fixture.base
          fixture.successorCut
          fixture.readyState
      )
  assertEqual
    "prepared evidence names the exact cut"
    fixture.successorCut
    (preparedMembershipSuccessorBaseCut prepared)
  let successor = commitMembershipSuccessorBaseInstallation prepared
      successorCutId = deriveTopologyCutId fixture.successorCut
  assertEqual
    "the current generation switches once"
    (heraldMembershipGenerationId fixture.successorMembership)
    (structuralProgressMembershipGenerationId successor)
  assertEqual
    "the applied vector is requalified to the successor generation"
    (heraldMembershipGenerationId fixture.successorMembership)
    (structuralVersionVectorMembershipGenerationId (structuralAppliedVector successor))
  assertEqual
    "reconciliation moves with the same exact coordinate"
    (heraldMembershipGenerationId fixture.successorMembership)
    ( Reconciliation.structuralAppliedMembershipGenerationId
        (structuralProgressReconciliation successor)
    )
  assertEqual
    "the base cut installs at the retirement control prefix"
    (controlIndex 7)
    (structuralLastInstalledCutControlPrefix successor)
  assertEqual
    "the membership-successor cut follows the distinguished historical genesis root"
    [successorCutId]
    (structuralInstalledCutIds successor)
  assertEqual
    "the successor lineage names the predecessor genesis cut"
    fixture.predecessorCut
    (topologyPredecessorCutId (topologyCutPredecessor fixture.successorCut))
  assertEqual
    "the predecessor and successor membership witnesses remain retained"
    ( Set.fromList
        [ heraldMembershipGenerationId fixture.predecessorMembership,
          heraldMembershipGenerationId fixture.successorMembership
        ]
    )
    (Set.fromList (fmap fst (structuralProgressMembershipHistory successor)))
  assertEqual
    "the installed successor retains the exact cut"
    (Just fixture.successorCut)
    (installedTopologyCutCut <$> lookupInstalledTopologyCut successorCutId successor)
  assertEqual
    "predecessor-generation reports are discarded"
    [fixture.h1]
    (fmap fst (structuralReportEntries successor))
  assertEqual
    "three-member stability waits for fresh successor reports"
    Nothing
    (structuralStableFrontier successor)
  assertEqual "no predecessor cut phase remains open" Nothing (structuralOpenCut successor)
  checked "successor owner invariant" (validateStructuralProgressState successor)

caseBaseHeldUntilControlApplied :: Assertion
caseBaseHeldUntilControlApplied = do
  fixture <- progressFixture
  case prepareMembershipSuccessorBaseInstallation
    fixture.successorMembership
    fixture.base
    fixture.successorCut
    fixture.preRetirementControlState of
    Left (StructuralProgressSuccessorBaseControlAhead applied required) -> do
      assertEqual "locally applied control" (controlIndex 0) applied
      assertEqual
        "required retirement control"
        (controlIndex 7)
        required
    observed ->
      assertFailure
        ("expected retirement-control hold, got " <> showPrepared observed)

caseNonEmptyBaseHeldUntilHistoryApplied :: Assertion
caseNonEmptyBaseHeldUntilHistoryApplied = do
  fixture <- progressFixture
  (successorCut, base) <- nonEmptyTerminalBase fixture
  case prepareMembershipSuccessorBaseInstallation
    fixture.successorMembership
    base
    successorCut
    fixture.readyState of
    Left StructuralProgressSuccessorBaseAhead -> pure ()
    observed ->
      assertFailure
        ("expected unapplied terminal-history hold, got " <> showPrepared observed)
  withRetiredHistory <- applyRetiredTerminalOccurrence fixture fixture.readyState
  prepared <-
    checked
      "install non-empty successor after retired history applies"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          base
          successorCut
          withRetiredHistory
      )
  let successor = commitMembershipSuccessorBaseInstallation prepared
      successorCutId = deriveTopologyCutId successorCut
      retiredOccurrence =
        structuralOccurrenceId fixture.h4 firstStructuralSequence
      retiredCause = structuralOccurrenceCause retiredOccurrence
      successorFrontier = topologyCutFrontier successorCut
  assertEqual
    "the raw successor frontier deliberately has no retired component"
    Nothing
    ( structuralVersionVectorComponent
        fixture.h4
        (topologyFrontierStructuralVersionVector successorFrontier)
    )
  assertBool
    "the installed successor cut covers retired history through its terminal predecessor"
    (installedCutCoversCause successorCutId retiredCause successor)
  assertEqual
    "earliest coverage includes the terminal vector absent from the successor frontier"
    (Just successorCutId)
    (installedCutCoveringCause retiredCause successor)
  assertInstalledCoverageAgainstReference [retiredOccurrence] [] successor
  case Alignment.alignmentCausesInProjectionOrder
    successorFrontier
    (Set.singleton retiredCause) of
    Left (Alignment.AlignmentCoordinationCauseOutsideTopologyCoordinate observed) ->
      assertEqual
        "the raw successor frontier rejects the deliberately omitted retired cause"
        retiredCause
        observed
    observed ->
      assertFailure
        ("expected raw-frontier rejection of the retired cause, got " <> show observed)
  orderedCauses <-
    checked
      "order the retired cause through the installed membership-successor lineage"
      ( Alignment.alignmentCausesInInstalledCutProjectionOrder
          successorCutId
          successor
          (Set.singleton retiredCause)
      )
  assertEqual
    "Alignment admits the pending retired cause through installed successor lineage"
    [retiredCause]
    orderedCauses
  checked
    "non-empty successor coverage invariant"
    (validateStructuralProgressState successor)

caseSuccessorBaseSupersedesUnfinishedCut :: Assertion
caseSuccessorBaseSupersedesUnfinishedCut = do
  fixture <- progressFixture
  (unfinished, ordinaryCut) <- openFullyAcceptedDynamicCut fixture
  _ <-
    checked
      "the ordinary cut has every acceptance but remains unestablished"
      (prepareTopologyCutEstablishment unfinished)
  assertEqual
    "the accepted ordinary cut is still open"
    (Just (deriveTopologyCutId ordinaryCut))
    (topologyCutAnnounceId <$> structuralOpenCut unfinished)
  assertEqual
    "the ordinary cut has not retained establishment evidence"
    Nothing
    (structuralOpenEstablished unfinished)

  prepared <-
    checked
      "prepare successor base over the unfinished ordinary cut"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          fixture.base
          fixture.successorCut
          unfinished
      )
  let successor = commitMembershipSuccessorBaseInstallation prepared
      ordinaryCutId = deriveTopologyCutId ordinaryCut
      successorCutId = deriveTopologyCutId fixture.successorCut
  assertEqual
    "the membership successor clears the obsolete ordinary phase"
    Nothing
    (structuralOpenCut successor)
  assertEqual
    "the unfinished ordinary cut never enters installed history"
    Nothing
    (lookupInstalledTopologyCut ordinaryCutId successor)
  assertEqual
    "only the membership-successor cut is appended"
    [successorCutId]
    (structuralInstalledCutIds successor)
  assertEqual
    "the exact successor base is retained on that cut"
    True
    (structuralSuccessorBaseInstalled fixture.base successor)
  checked
    "successor-over-unfinished-cut owner invariant"
    (validateStructuralProgressState successor)

caseInstallSuccessorAfterDynamicCut :: Assertion
caseInstallSuccessorAfterDynamicCut = do
  fixture <- progressFixture
  (dynamicPredecessor, dynamicCut) <- installDynamicPredecessorCut fixture
  predecessorBase <-
    checked
      "installed predecessor base"
      ( terminalSourceInstalledPredecessorBase
          fixture.predecessorMembership
          dynamicCut
      )
  (successorCut, base) <- emptyTerminalBaseAfter fixture predecessorBase
  prepared <-
    checked
      "prepare successor after dynamic predecessor"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          base
          successorCut
          dynamicPredecessor
      )
  let successor = commitMembershipSuccessorBaseInstallation prepared
      dynamicCutId = deriveTopologyCutId dynamicCut
      successorCutId = deriveTopologyCutId successorCut
  process <- checked "dynamic-cut coverage process" (mkProcessEpochId (identifierBytes 0x7a))
  cause <- checked "dynamic-cut covered End" (processEndCause process (controlIndex 7))
  assertEqual
    "the membership successor does not replace the first covering dynamic cut"
    (Just dynamicCutId)
    (installedCutCoveringCause cause successor)
  assertEqual
    "current coverage advances to the membership successor"
    (Just successorCutId)
    (currentInstalledCutCoveringCause cause successor)
  assertEqual
    "the membership-successor predecessor is the installed dynamic cut"
    dynamicCutId
    (topologyPredecessorCutId (topologyCutPredecessor successorCut))
  assertEqual
    "both cuts remain in exact predecessor order"
    [dynamicCutId, successorCutId]
    (structuralInstalledCutIds successor)
  assertEqual
    "the dynamic predecessor remains reconstructible by identity"
    (Just dynamicCut)
    (installedTopologyCutCut <$> lookupInstalledTopologyCut dynamicCutId successor)
  assertEqual
    "the successor remains reconstructible by identity"
    (Just successorCut)
    (installedTopologyCutCut <$> lookupInstalledTopologyCut successorCutId successor)
  _ <-
    checked
      "reconstruct dynamic predecessor projection"
      (structuralProjectionAtInstalledCut (emptyViews fixture.h1) dynamicCutId successor)
  _ <-
    checked
      "reconstruct membership-successor projection"
      (structuralProjectionAtInstalledCut (emptyViews fixture.h1) successorCutId successor)
  assertInstalledCoverageAgainstReference [] [] dynamicPredecessor
  assertInstalledCoverageAgainstReference [] [] successor
  checked "dynamic-lineage successor invariant" (validateStructuralProgressState successor)

casePredecessorReportIsStale :: Assertion
casePredecessorReportIsStale = do
  (fixture, successor, _) <- successorAfterDynamicPredecessor
  let survivorLiveAheadVector =
        checkedValue
          "survivor-live-ahead predecessor report vector"
          ( mkStructuralVersionVector
              fixture.predecessorMembership
              [ (fixture.h1, structuralPrefixThrough firstStructuralSequence),
                (fixture.h2, emptyStructuralPrefix),
                (fixture.h3, emptyStructuralPrefix),
                (fixture.h4, emptyStructuralPrefix)
              ]
          )
      retiredBeyondTerminalVector =
        checkedValue
          "retired-beyond-terminal predecessor report vector"
          ( mkStructuralVersionVector
              fixture.predecessorMembership
              [ (fixture.h1, emptyStructuralPrefix),
                (fixture.h2, emptyStructuralPrefix),
                (fixture.h3, emptyStructuralPrefix),
                (fixture.h4, structuralPrefixThrough firstStructuralSequence)
              ]
          )
  prepared <-
    checked
      "classify exact predecessor report"
      (prepareStructuralReport (predecessorReport fixture fixture.h2) successor)
  let (retained, classification) = commitStructuralReport prepared
  assertEqual
    "the exact retained predecessor membership classifies the late report as stale"
    StructuralReportHistoricalStale
    classification
  assertBool "a historical stale report is owner-inert" (retained == successor)
  assertRejected
    "the retired reporter cannot use predecessor staleness"
    (prepareStructuralReport (predecessorReport fixture fixture.h4) successor)
  survivorAhead <-
    checked
      "classify survivor-live-ahead predecessor report"
      ( prepareStructuralReport
          (structuralAppliedReport fixture.h2 survivorLiveAheadVector (controlIndex 7))
          successor
      )
  assertEqual
    "a surviving component may legitimately run ahead of the terminal base"
    (successor, StructuralReportHistoricalStale)
    (commitStructuralReport survivorAhead)
  controlAhead <-
    checked
      "classify control-live-ahead predecessor report"
      ( prepareStructuralReport
          ( structuralAppliedReport
              fixture.h2
              fixture.emptyPredecessorVector
              (controlIndex 8)
          )
          successor
      )
  assertEqual
    "the global control prefix may legitimately run ahead of the terminal cut"
    (successor, StructuralReportHistoricalStale)
    (commitStructuralReport controlAhead)
  retiredAhead <-
    checked
      "classify ahead-claim predecessor report"
      ( prepareStructuralReport
          (structuralAppliedReport fixture.h2 retiredBeyondTerminalVector (controlIndex 7))
          successor
      )
  assertEqual
    "the union's least vector does not cap a predecessor report backed by ahead claims"
    (successor, StructuralReportHistoricalStale)
    (commitStructuralReport retiredAhead)

casePredecessorAcceptanceIsStale :: Assertion
casePredecessorAcceptanceIsStale = do
  (fixture, successor, dynamicCutId) <- successorAfterDynamicPredecessor
  let acceptance =
        topologyCutAcceptance
          dynamicCutId
          (predecessorReport fixture fixture.h2)
  prepared <-
    checked
      "classify exact predecessor acceptance"
      (prepareTopologyCutAcceptance acceptance successor)
  assertEqual
    "the retained installed-cut acceptance is historical stale evidence"
    TopologyCutAcceptanceHistoricalStale
    (preparedTopologyCutAcceptanceClassification prepared)
  assertBool
    "a historical stale acceptance is owner-inert"
    (commitTopologyCutAcceptance prepared == successor)
  assertRejected
    "a retired reporter's predecessor acceptance remains rejected"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            dynamicCutId
            (predecessorReport fixture fixture.h4)
        )
        successor
    )
  assertRejected
    "a different report is not the exact retained installed-cut acceptance"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            dynamicCutId
            ( structuralAppliedReport
                fixture.h2
                fixture.emptyPredecessorVector
                (controlIndex 8)
            )
        )
        successor
    )

casePredecessorEstablishmentIsDuplicate :: Assertion
casePredecessorEstablishmentIsDuplicate = do
  (fixture, successor, dynamicCutId) <- successorAfterDynamicPredecessor
  installed <-
    maybe
      (assertFailure "successor lost its installed predecessor cut")
      pure
      (lookupInstalledTopologyCut dynamicCutId successor)
  let established = installedTopologyCutEstablished installed
      checkpoint =
        topologyControlCheckpointWithConsequence
          (controlIndex 7)
          True
          "late-predecessor-established"
  prepared <-
    checked
      "classify exact predecessor establishment"
      ( prepareTopologyCutEstablished
          (emptyViews fixture.h1)
          checkpoint
          established
          successor
      )
  assertEqual
    "terminal cutover followed by the retained predecessor chain is a deterministic duplicate"
    (maybe TopologyCutEstablishedImported TopologyCutEstablishedDuplicate (installedTopologyCutOwnAcknowledgement installed))
    (preparedTopologyCutEstablishedClassification prepared)
  assertBool
    "an exact predecessor establishment replay is owner-inert"
    (commitTopologyCutEstablished prepared == successor)

  assertInstalledCoverageAgainstReference [] [] (commitTopologyCutEstablished prepared)

  let changedAcceptances =
        [ if topologyCutAcceptanceReporter acceptance == fixture.h2
            then
              topologyCutAcceptance
                dynamicCutId
                ( structuralAppliedReport
                    fixture.h2
                    fixture.emptyPredecessorVector
                    (controlIndex 8)
                )
            else acceptance
        | acceptance <- topologyCutEstablishedAcceptances established
        ]
      changed =
        checkedValue
          "changed predecessor establishment"
          (topologyCutEstablished dynamicCutId (installedTopologyCutCut installed) changedAcceptances)
  assertRejected
    "changed evidence under an installed predecessor cut id remains conflicting"
    ( prepareTopologyCutEstablished
        (emptyViews fixture.h1)
        checkpoint
        changed
        successor
    )

caseOrdinaryCutRejectsMembershipSuccessorPredecessor :: Assertion
caseOrdinaryCutRejectsMembershipSuccessorPredecessor = do
  fixture <- progressFixture
  preparedBase <-
    checked
      "install base before malformed ordinary cut"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          fixture.base
          fixture.successorCut
          fixture.readyState
      )
  let successor = commitMembershipSuccessorBaseInstallation preparedBase
      incumbent = structuralLastInstalledCutId successor
      successorVector = emptyStructuralVersionVector fixture.successorMembership
      establishedUnion = successorStructuralBaseEstablishedUnion fixture.base
      union = terminalSourceUnionEstablishedUnion establishedUnion
  malformedPredecessor <-
    checked
      "shape-valid but misplaced membership-successor predecessor"
      ( membershipSuccessorPredecessor
          (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
          incumbent
          fixture.emptyPredecessorVector
          successorVector
          (terminalSourceUnionDigest union)
      )
  malformedCut <-
    checked
      "malformed ordinary cut"
      ( topologyCut
          malformedPredecessor
          (topologyFrontier successorVector (controlIndex 8))
          (checkedValue "malformed ordinary digest" (mkTopologyOccurrenceDigest (identifierBytes 0x68)))
      )
  let announce = topologyCutAnnounce fixture.h1 malformedCut
      cutId = topologyCutAnnounceId announce
      report reporter = structuralAppliedReport reporter successorVector (controlIndex 8)
      acceptances =
        [topologyCutAcceptance cutId (report reporter) | reporter <- [fixture.h1, fixture.h2, fixture.h3]]
      checkpoint =
        topologyControlCheckpointWithConsequence
          (controlIndex 8)
          True
          "malformed-ordinary-predecessor"
  established <-
    checked
      "malformed ordinary Established"
      (topologyCutEstablished cutId malformedCut acceptances)
  assertOrdinaryRejected
    incumbent
    ( prepareTopologyCutAnnounce
        (emptyViews fixture.h1)
        checkpoint
        announce
        successor
    )
  assertOrdinaryRejected
    incumbent
    ( prepareTopologyCutEstablished
        (emptyViews fixture.h1)
        checkpoint
        established
        successor
    )
  where
    assertOrdinaryRejected expected result = case result of
      Left (StructuralProgressOrdinaryCutRequiresSameGenerationPredecessor observed) ->
        assertEqual "rejection names the embedded predecessor cut" expected observed
      Left problem ->
        assertFailure ("misplaced membership-successor predecessor produced the wrong fault: " <> show problem)
      Right _ ->
        assertFailure "an ordinary path admitted membership-successor authority"

caseAbandonedPredecessorAcceptanceIsStale :: Assertion
caseAbandonedPredecessorAcceptanceIsStale = do
  fixture <- progressFixture
  (unfinished, ordinaryCut) <- openFullyAcceptedDynamicCut fixture
  preparedSuccessor <-
    checked
      "install successor over abandoned predecessor cut"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          fixture.base
          fixture.successorCut
          unfinished
      )
  let successor = commitMembershipSuccessorBaseInstallation preparedSuccessor
      ordinaryCutId = deriveTopologyCutId ordinaryCut
      acceptance =
        topologyCutAcceptance
          ordinaryCutId
          (predecessorReport fixture fixture.h2)
      fabricatedCut =
        checkedValue
          "fabricated predecessor-generation cut"
          ( topologyCut
              (topologyCutPredecessor ordinaryCut)
              (topologyCutFrontier ordinaryCut)
              ( checkedValue
                  "fabricated topology digest"
                  (mkTopologyOccurrenceDigest (identifierBytes 0x67))
              )
          )
      fabricatedCutId = deriveTopologyCutId fabricatedCut
      survivorLiveAheadVector =
        checkedValue
          "survivor-live-ahead predecessor vector"
          ( mkStructuralVersionVector
              fixture.predecessorMembership
              [ (fixture.h1, structuralPrefixThrough firstStructuralSequence),
                (fixture.h2, emptyStructuralPrefix),
                (fixture.h3, emptyStructuralPrefix),
                (fixture.h4, emptyStructuralPrefix)
              ]
          )
      retiredBeyondTerminalVector =
        checkedValue
          "retired-beyond-terminal predecessor vector"
          ( mkStructuralVersionVector
              fixture.predecessorMembership
              [ (fixture.h1, emptyStructuralPrefix),
                (fixture.h2, emptyStructuralPrefix),
                (fixture.h3, emptyStructuralPrefix),
                (fixture.h4, structuralPrefixThrough firstStructuralSequence)
              ]
          )
      wrongMemberSetVector =
        checkedValue
          "wrong predecessor member-set vector"
          ( structuralVersionVectorFromClaimedCoordinates
              (heraldMembershipGenerationId fixture.predecessorMembership)
              (structuralVersionVectorMemberSetDigest (structuralAppliedVector successor))
              ( (fixture.h1, emptyStructuralPrefix)
                  :| [ (fixture.h2, emptyStructuralPrefix),
                       (fixture.h3, emptyStructuralPrefix)
                     ]
              )
          )
  prepared <-
    checked
      "classify abandoned predecessor-cut acceptance"
      (prepareTopologyCutAcceptance acceptance successor)
  assertEqual
    "a survivor's exact abandoned-cut acceptance is historical stale evidence"
    TopologyCutAcceptanceHistoricalStale
    (preparedTopologyCutAcceptanceClassification prepared)
  assertBool
    "an abandoned predecessor-cut acceptance is owner-inert"
    (commitTopologyCutAcceptance prepared == successor)
  assertRejected
    "an untombstoned predecessor cut id remains unknown"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            fabricatedCutId
            (predecessorReport fixture fixture.h2)
        )
        successor
    )
  survivorAhead <-
    checked
      "classify survivor-live-ahead abandoned-cut acceptance"
      ( prepareTopologyCutAcceptance
          ( topologyCutAcceptance
              ordinaryCutId
              ( structuralAppliedReport
                  fixture.h2
                  survivorLiveAheadVector
                  (controlIndex 7)
              )
          )
          successor
      )
  assertEqual
    "the exact tombstoned cut admits a surviving component live-ahead as stale"
    TopologyCutAcceptanceHistoricalStale
    (preparedTopologyCutAcceptanceClassification survivorAhead)
  controlAhead <-
    checked
      "classify control-live-ahead abandoned-cut acceptance"
      ( prepareTopologyCutAcceptance
          ( topologyCutAcceptance
              ordinaryCutId
              ( structuralAppliedReport
                  fixture.h2
                  fixture.emptyPredecessorVector
                  (controlIndex 8)
              )
          )
          successor
      )
  assertEqual
    "the exact tombstoned cut admits a global control live-ahead as stale"
    TopologyCutAcceptanceHistoricalStale
    (preparedTopologyCutAcceptanceClassification controlAhead)
  retiredAhead <-
    checked
      "classify ahead-claim abandoned-cut acceptance"
      ( prepareTopologyCutAcceptance
          ( topologyCutAcceptance
              ordinaryCutId
              ( structuralAppliedReport
                  fixture.h2
                  retiredBeyondTerminalVector
                  (controlIndex 7)
              )
          )
          successor
      )
  assertEqual
    "the exact tombstoned cut admits an ahead-claim vector as stale"
    TopologyCutAcceptanceHistoricalStale
    (preparedTopologyCutAcceptanceClassification retiredAhead)
  assertRejected
    "the exact tombstoned cut still requires the acceptance report to cover its frontier"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            ordinaryCutId
            ( structuralAppliedReport
                fixture.h2
                fixture.emptyPredecessorVector
                (controlIndex 6)
            )
        )
        successor
    )
  assertRejected
    "a predecessor-generation report with the successor member-set digest is rejected"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            ordinaryCutId
            (structuralAppliedReport fixture.h2 wrongMemberSetVector (controlIndex 7))
        )
        successor
    )
  assertRejected
    "a successor-generation acceptance for the abandoned cut remains unknown"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            ordinaryCutId
            (structuralLocalReport successor)
        )
        successor
    )
  assertRejected
    "an acknowledgement cannot be fabricated for an unestablished superseded cut"
    ( prepareTopologyCutEstablishedAck
        ( topologyCutEstablishedAck
            ordinaryCutId
            fixture.h2
            (heraldMembershipGenerationId fixture.predecessorMembership)
        )
        successor
    )
  assertRejected
    "the retired reporter cannot use abandoned predecessor-cut staleness"
    ( prepareTopologyCutAcceptance
        ( topologyCutAcceptance
            ordinaryCutId
            (predecessorReport fixture fixture.h4)
        )
        successor
    )

casePredecessorAcknowledgementIsStale :: Assertion
casePredecessorAcknowledgementIsStale = do
  (fixture, successor, dynamicCutId) <- successorAfterDynamicPredecessor
  let generation = heraldMembershipGenerationId fixture.predecessorMembership
      acknowledgement =
        topologyCutEstablishedAck dynamicCutId fixture.h2 generation
  prepared <-
    checked
      "classify exact predecessor acknowledgement"
      (prepareTopologyCutEstablishedAck acknowledgement successor)
  assertEqual
    "the deterministic installed-cut acknowledgement is historical stale evidence"
    TopologyCutAckStale
    (preparedTopologyCutAckClassification prepared)
  assertBool
    "a historical stale acknowledgement is owner-inert"
    (commitTopologyCutEstablishedAck prepared == successor)
  assertRejected
    "a retired reporter's predecessor acknowledgement remains rejected"
    ( prepareTopologyCutEstablishedAck
        (topologyCutEstablishedAck dynamicCutId fixture.h4 generation)
        successor
    )

caseSuccessorBaseAcknowledgementIsStale :: Assertion
caseSuccessorBaseAcknowledgementIsStale = do
  (fixture, successor, predecessorCutId) <- successorAfterDynamicPredecessor
  let successorCutId = structuralLastInstalledCutId successor
      successorGeneration =
        heraldMembershipGenerationId fixture.successorMembership
      acknowledgement =
        topologyCutEstablishedAck
          successorCutId
          fixture.h2
          successorGeneration
  prepared <-
    checked
      "classify exact membership-successor base acknowledgement"
      (prepareTopologyCutEstablishedAck acknowledgement successor)
  assertEqual
    "the exact successor-base acknowledgement is stale despite its intentionally absent retained ack"
    TopologyCutAckStale
    (preparedTopologyCutAckClassification prepared)
  assertBool
    "a stale membership-successor acknowledgement is owner-inert"
    (commitTopologyCutEstablishedAck prepared == successor)
  assertRejected
    "a successor-generation acknowledgement for the predecessor cut remains conflicting"
    ( prepareTopologyCutEstablishedAck
        ( topologyCutEstablishedAck
            predecessorCutId
            fixture.h2
            successorGeneration
        )
        successor
    )
  assertRejected
    "a retired reporter cannot use successor-base acknowledgement staleness"
    ( prepareTopologyCutEstablishedAck
        ( topologyCutEstablishedAck
            successorCutId
            fixture.h4
            successorGeneration
        )
        successor
    )

successorAfterDynamicPredecessor ::
  IO (ProgressFixture, StructuralProgressState, TopologyCutId)
successorAfterDynamicPredecessor = do
  fixture <- progressFixture
  (dynamicPredecessor, dynamicCut) <- installDynamicPredecessorCut fixture
  predecessorBase <-
    checked
      "derive historical-control predecessor base"
      ( terminalSourceInstalledPredecessorBase
          fixture.predecessorMembership
          dynamicCut
      )
  (successorCut, base) <- emptyTerminalBaseAfter fixture predecessorBase
  prepared <-
    checked
      "install historical-control successor base"
      ( prepareMembershipSuccessorBaseInstallation
          fixture.successorMembership
          base
          successorCut
          dynamicPredecessor
      )
  pure
    ( fixture,
      commitMembershipSuccessorBaseInstallation prepared,
      deriveTopologyCutId dynamicCut
    )

predecessorReport ::
  ProgressFixture -> HeraldEpoch -> StructuralAppliedReport
predecessorReport fixture reporter =
  structuralAppliedReport
    reporter
    fixture.emptyPredecessorVector
    (controlIndex 7)

assertRejected :: String -> Either problem value -> Assertion
assertRejected _ (Left _) = pure ()
assertRejected context (Right _) =
  assertFailure (context <> ": unexpectedly admitted")

data ProgressFixture = ProgressFixture
  { h1 :: HeraldEpoch,
    h2 :: HeraldEpoch,
    h3 :: HeraldEpoch,
    h4 :: HeraldEpoch,
    predecessorMembership :: HeraldMembershipGeneration,
    successorMembership :: HeraldMembershipGeneration,
    predecessorCut :: TopologyCutId,
    emptyPredecessorVector :: StructuralVersionVector,
    terminalDigest :: TopologyOccurrenceDigest,
    successorCut :: TopologyCut,
    base :: SuccessorStructuralBase,
    preRetirementControlState :: StructuralProgressState,
    readyState :: StructuralProgressState
  }

progressFixture :: IO ProgressFixture
progressFixture = progressFixtureWithEndpointEpochs 0x11 0x44

progressFixtureWithEndpointEpochs :: Word -> Word -> IO ProgressFixture
progressFixtureWithEndpointEpochs localEpoch retiredEpoch = do
  let system = checkedValue "system" (mkSystemId (identifierBytes 0x40))
      h1 = checkedValue "H1" (mkHeraldEpoch (identifierBytes localEpoch))
      h2 = checkedValue "H2" (mkHeraldEpoch (identifierBytes 0x22))
      h3 = checkedValue "H3" (mkHeraldEpoch (identifierBytes 0x33))
      h4 = checkedValue "H4" (mkHeraldEpoch (identifierBytes retiredEpoch))
      predecessorMembership =
        checkedValue
          "predecessor membership"
          (genesisHeraldMembershipGeneration system (h1 :| [h2, h3, h4]))
      successorMembership =
        checkedValue
          "successor membership"
          (retireHeraldMembershipGeneration (controlIndex 7) (MembershipFixtures.fixtureRetirementResolution (controlIndex 7)) h4 predecessorMembership)
      emptyPredecessorVector = emptyStructuralVersionVector predecessorMembership
      initialProjection =
        checkedValue
          "initial projection"
          (mkInitialProjectionDigest (identifierBytes 0x51))
      initialState =
        checkedValue
          "initial structural progress"
          ( initialStructuralProgressState
              system
              h1
              predecessorMembership
              initialProjection
              (Reconciliation.emptyStructuralAppliedState emptyPredecessorVector)
          )
  preparedControl <-
    checked
      "apply retirement control"
      (prepareStructuralControlProgress (controlIndex 7) initialState)
  let (readyState, _) = commitStructuralControlProgress preparedControl
      predecessorCut = structuralLastInstalledCutId initialState
      predecessorBase =
        checkedValue
          "genesis predecessor base"
          (terminalSourceGenesisPredecessorBase predecessorMembership predecessorCut)
      inventories =
        [ checkedValue
            "empty terminal inventory"
            (terminalSourceInventory (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership) h4 reporter [] [] [])
        | reporter <- [h1, h2, h3]
        ]
      terminalDigest =
        checkedValue
          "terminal topology digest"
          (mkTopologyOccurrenceDigest (identifierBytes 0x66))
      union =
        checkedValue
          "empty terminal union"
          ( deriveTerminalSourceUnion
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              predecessorBase
              inventories
              emptyTerminalSourcePayloadArchive
              (constantDigest terminalDigest)
          )
      ready =
        checkedValue
          "terminal union readiness"
          (terminalSourceUnionReady emptyPredecessorVector (controlIndex 7) union)
      acceptances =
        [ checkedValue
            "terminal union acceptance"
            (terminalSourceUnionAcceptance successorMembership reporter ready)
        | reporter <- [h1, h2, h3]
        ]
      established =
        checkedValue
          "terminal union establishment"
          (terminalSourceUnionEstablished successorMembership union acceptances)
      predecessor =
        checkedValue
          "membership-successor predecessor"
          ( terminalSourceUnionTopologyPredecessor
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              union
          )
      successorCut =
        checkedValue
          "membership-successor cut"
          ( topologyCut
              predecessor
              ( topologyFrontier
                  (terminalSourceUnionSuccessorInitialVector union)
                  (terminalSourceUnionTerminalControlPrefix union)
              )
              terminalDigest
          )
      base =
        checkedValue
          "successor structural base"
          ( establishedSuccessorStructuralBase
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              established
              successorCut
          )
  pure
    ProgressFixture
      { h1,
        h2,
        h3,
        h4,
        predecessorMembership,
        successorMembership,
        predecessorCut,
        emptyPredecessorVector,
        terminalDigest,
        successorCut,
        base,
        preRetirementControlState = initialState,
        readyState
      }

installDynamicPredecessorCut ::
  ProgressFixture -> IO (StructuralProgressState, TopologyCut)
installDynamicPredecessorCut fixture = do
  (withAllAcceptances, cut) <- openFullyAcceptedDynamicCut fixture
  establishment <-
    checked
      "establish dynamic predecessor cut"
      (prepareTopologyCutEstablishment withAllAcceptances)
  let installed = commitTopologyCutEstablishment establishment
      cutId = deriveTopologyCutId cut
  assertEqual
    "the same-generation cut installs before membership contraction"
    cutId
    (structuralLastInstalledCutId installed)
  checked "dynamic predecessor invariant" (validateStructuralProgressState installed)
  pure (installed, cut)

openFullyAcceptedDynamicCut ::
  ProgressFixture -> IO (StructuralProgressState, TopologyCut)
openFullyAcceptedDynamicCut fixture =
  openFullyAcceptedCut
    (emptyViews fixture.h1)
    (topologyControlCheckpointWithConsequence (controlIndex 7) True "retirement-topology-checkpoint")
    fixture.readyState

openFullyAcceptedCut :: Reconciliation.ReconciliationViews -> TopologyControlCheckpoint -> StructuralProgressState -> IO (StructuralProgressState, TopologyCut)
openFullyAcceptedCut views checkpoint initial = do
  withMatrix <- foldM (flip recordReport) initial remotes
  proposal <- checked "prepare ordinary open cut" (prepareTopologyCutProposal views checkpoint withMatrix)
  preparedProposal <- case proposal of
    TopologyCutProposalReady prepared -> pure prepared
    TopologyCutProposalUnavailable -> assertFailure "advancing ordinary frontier produced no cut"
    TopologyCutProposalHeld dependencies -> assertFailure ("ordinary projection held: " <> show dependencies)
  let announce = preparedTopologyCutProposalAnnounce preparedProposal
      cut = topologyCutAnnounceCut announce
      cutId = topologyCutAnnounceId announce
      proposed = commitTopologyCutProposal preparedProposal
  withAllAcceptances <- foldM (recordAcceptance cutId) proposed remotes
  pure (withAllAcceptances, cut)
  where
    remotes = filter (/= structuralProgressLocalHerald initial) (NonEmpty.toList (structuralProgressMembers initial))
    report reporter = structuralAppliedReport reporter (structuralAppliedVector initial) (structuralAppliedControlPrefix initial)
    recordReport reporter state = do
      prepared <- checked "record exact ordinary frontier" (prepareStructuralReport (report reporter) state)
      pure (fst (commitStructuralReport prepared))
    recordAcceptance cutId state reporter = do
      prepared <- checked "record exact ordinary acceptance" (prepareTopologyCutAcceptance (topologyCutAcceptance cutId (report reporter)) state)
      pure (commitTopologyCutAcceptance prepared)

emptyTerminalBaseAfter ::
  ProgressFixture ->
  TerminalSourcePredecessorBase ->
  IO (TopologyCut, SuccessorStructuralBase)
emptyTerminalBaseAfter fixture predecessorBase = do
  let inventories =
        [ checkedValue
            "dynamic-predecessor terminal inventory"
            ( terminalSourceInventory
                (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
                fixture.h4
                reporter
                []
                []
                []
            )
        | reporter <- [fixture.h1, fixture.h2, fixture.h3]
        ]
      union =
        checkedValue
          "dynamic-predecessor terminal union"
          ( deriveTerminalSourceUnion
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              predecessorBase
              inventories
              emptyTerminalSourcePayloadArchive
              (constantDigest fixture.terminalDigest)
          )
      ready =
        checkedValue
          "dynamic-predecessor union readiness"
          ( terminalSourceUnionReady
              fixture.emptyPredecessorVector
              (controlIndex 7)
              union
          )
      acceptances =
        [ checkedValue
            "dynamic-predecessor union acceptance"
            (terminalSourceUnionAcceptance fixture.successorMembership reporter ready)
        | reporter <- [fixture.h1, fixture.h2, fixture.h3]
        ]
      established =
        checkedValue
          "dynamic-predecessor union establishment"
          (terminalSourceUnionEstablished fixture.successorMembership union acceptances)
      predecessor =
        checkedValue
          "dynamic membership-successor predecessor"
          ( terminalSourceUnionTopologyPredecessor
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              union
          )
      cut =
        checkedValue
          "dynamic membership-successor cut"
          ( topologyCut
              predecessor
              ( topologyFrontier
                  (terminalSourceUnionSuccessorInitialVector union)
                  (terminalSourceUnionTerminalControlPrefix union)
              )
              fixture.terminalDigest
          )
      base =
        checkedValue
          "dynamic successor structural base"
          ( establishedSuccessorStructuralBase
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              established
              cut
          )
  pure (cut, base)

emptyViews :: HeraldEpoch -> Reconciliation.ReconciliationViews
emptyViews local =
  Reconciliation.reconciliationViews
    local
    Map.empty
    Set.empty
    Map.empty
    Set.empty

applyRetiredTerminalOccurrence ::
  ProgressFixture ->
  StructuralProgressState ->
  IO StructuralProgressState
applyRetiredTerminalOccurrence fixture = applyTerminalOccurrenceFrom fixture fixture.h4

applyTerminalOccurrenceFrom :: ProgressFixture -> HeraldEpoch -> StructuralProgressState -> IO StructuralProgressState
applyTerminalOccurrenceFrom fixture origin state = do
  object <-
    checked
      "retired terminal neutral object"
      (mkGlobalObjectId (identifierBytes 0x70))
  source <-
    checked
      "retired terminal source Nabla"
      (mkNablaId (identifierBytes 0x71))
  objectField <- checked "retired terminal object field" (mkFieldName "object_id")
  labelField <- checked "retired terminal label field" (mkFieldName "label")
  value <-
    checked
      "retired terminal neutral value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue (VoidLabel, 0))
          ]
      )
  let publication =
        publicationId
          source
          genesisAuthorityEpoch
          origin
          (nablaSequence 1)
      carrierSort =
        sortOccurrence
          (profileSortFor NeutralVertexRole)
          ( checkedValue
              "retired terminal carrier sort occurrence"
              (mkSortDefinitionOccurrenceId (identifierBytes 0x69))
          )
      occurrenceId =
        structuralOccurrenceId origin firstStructuralSequence
      views = terminalOccurrenceViews fixture
      payload = "retained-H4-occurrence"
  checkedPublication <-
    checked
      "retired terminal checked publication"
      ( mkCheckedPublication
          (predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole))
          publication
          value
      )
  let application =
        Reconciliation.structuralApplication
          occurrenceId
          (structuralAppliedVector state)
          carrierSort
          checkedPublication
          (controlIndex 7)
  preparedReconciliation <-
    case Reconciliation.prepareStructuralReconciliation
      views
      application
      Nothing
      (structuralProgressReconciliation state) of
      Right (Reconciliation.StructuralReady ready) -> pure ready
      Right (Reconciliation.StructuralHeld dependencies) ->
        assertFailure
          ("retired terminal occurrence held: " <> show dependencies)
      Left problem ->
        assertFailure
          ("retired terminal occurrence rejected: " <> show problem)
  stamp <-
    checked
      "retired terminal occurrence stamp"
      ( mkStructuralOccurrenceStamp
          occurrenceId
          (structuralAppliedVector state)
          publication
          (deriveTerminalStructuralPayloadDigest payload)
          NeutralVertexCarrier
      )
  preparedApplication <-
    checked
      "apply retired terminal occurrence"
      ( prepareStructuralApplication
          stamp
          (structuralGenesisCutId state)
          preparedReconciliation
          Graph.emptyState
          state
      )
  let (_, successor, _) = commitStructuralApplication preparedApplication
  pure successor

terminalOccurrenceViews :: ProgressFixture -> Reconciliation.ReconciliationViews
terminalOccurrenceViews fixture =
  Reconciliation.reconciliationViews
    fixture.h1
    (Map.singleton (profileSortFor NeutralVertexRole) (sortOccurrence (profileSortFor NeutralVertexRole) (checkedValue "terminal carrier definition" (mkSortDefinitionOccurrenceId (identifierBytes 0x69)))))
    Set.empty
    Map.empty
    Set.empty

nonEmptyTerminalBase ::
  ProgressFixture -> IO (TopologyCut, SuccessorStructuralBase)
nonEmptyTerminalBase fixture = do
  let payload = "retained-H4-occurrence"
      sourceNabla =
        checkedValue
          "retired source Nabla"
          (mkNablaId (identifierBytes 0x71))
      occurrenceId =
        structuralOccurrenceId fixture.h4 firstStructuralSequence
      publication =
        publicationId
          sourceNabla
          genesisAuthorityEpoch
          fixture.h4
          (nablaSequence 1)
      digest = deriveTerminalStructuralPayloadDigest payload
      stamp =
        checkedValue
          "retired occurrence stamp"
          ( mkStructuralOccurrenceStamp
              occurrenceId
              fixture.emptyPredecessorVector
              publication
              digest
              NeutralVertexCarrier
          )
      retained =
        checkedValue
          "retired terminal occurrence"
          (terminalStructuralOccurrence stamp payload)
      (h1Inventory, archive) =
        checkedValue
          "retained terminal inventory"
          ( sealTerminalSourceInventory
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              fixture.h4
              fixture.h1
              []
              []
              [retained]
          )
      emptyInventory reporter =
        checkedValue
          "empty terminal inventory"
          ( terminalSourceInventory
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              fixture.h4
              reporter
              []
              []
              []
          )
      predecessorBase =
        checkedValue
          "non-empty genesis predecessor base"
          ( terminalSourceGenesisPredecessorBase
              fixture.predecessorMembership
              fixture.predecessorCut
          )
      union =
        checkedValue
          "non-empty terminal union"
          ( deriveTerminalSourceUnion
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              predecessorBase
              [h1Inventory, emptyInventory fixture.h2, emptyInventory fixture.h3]
              archive
              (constantDigest fixture.terminalDigest)
          )
      ready =
        checkedValue
          "non-empty union readiness at a repaired owner"
          ( terminalSourceUnionReady
              (terminalSourceUnionTerminalPredecessorVector union)
              (controlIndex 7)
              union
          )
      acceptances =
        [ checkedValue
            "non-empty union acceptance"
            (terminalSourceUnionAcceptance fixture.successorMembership reporter ready)
        | reporter <- [fixture.h1, fixture.h2, fixture.h3]
        ]
      established =
        checkedValue
          "non-empty union establishment"
          (terminalSourceUnionEstablished fixture.successorMembership union acceptances)
      predecessor =
        checkedValue
          "non-empty membership-successor predecessor"
          ( terminalSourceUnionTopologyPredecessor
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              union
          )
      cut =
        checkedValue
          "non-empty membership-successor cut"
          ( topologyCut
              predecessor
              ( topologyFrontier
                  (terminalSourceUnionSuccessorInitialVector union)
                  (terminalSourceUnionTerminalControlPrefix union)
              )
              fixture.terminalDigest
          )
      base =
        checkedValue
          "non-empty successor structural base"
          ( establishedSuccessorStructuralBase
              (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessorMembership fixture.successorMembership)
              established
              cut
          )
  pure (cut, base)

constantDigest ::
  TopologyOccurrenceDigest ->
  StructuralVersionVector ->
  ControlIndex ->
  TopologyOccurrenceDigest
constantDigest digest _ _ = digest

identifierBytes :: Word -> ByteString
identifierBytes = ByteString.replicate 32 . fromIntegral

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id

showPrepared ::
  Either StructuralProgressProblem PreparedMembershipSuccessorBaseInstallation ->
  String
showPrepared = either show (const "successful preparation")
