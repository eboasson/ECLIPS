{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module TerminalSourceUnionProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    StructuralSequence,
    TopologyCutId,
    controlIndex,
    genesisAuthorityEpoch,
    mkHeraldEpoch,
    mkNablaId,
    mkStructuralSequence,
    mkSystemId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipLineage,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    heraldMembershipHistory,
    heraldMembershipLineage,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (NeutralVertexCarrier),
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
  )
import Eclips.Domain.Topology
  ( TopologyOccurrenceDigest,
    TopologyPredecessorView (MembershipSuccessorPredecessorView),
    mkTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyFrontier,
    topologyPredecessorView,
  )
import Eclips.Herald.Graph.TerminalSource
import Eclips.Herald.PeerPublication
  ( StructuralPublicationDigest,
    mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
  )
import GenesisFixtures qualified as MembershipFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "terminal structural-source union"
    [ testProperty
        "inventory union is commutative, associative, and idempotent"
        propUnionPresentationIndependent,
      testCase
        "a gap closes only the contiguous prefix and retains ahead evidence"
        caseGapAndLeastVector,
      testCase
        "unequal digests for one occurrence are a conflict"
        caseClaimConflict,
      testCase
        "missing payload is requested from a deterministic survivor and relayed"
        caseMissingPayloadRelay,
      testCase
        "payload bytes are digest-bound and unequal admitted copies conflict"
        casePayloadConflict,
      testCase
        "an inventory cannot choose a lower or higher contiguous prefix"
        caseInventoryPrefixChecked,
      testCase
        "every survivor inventory covers the predecessor cut's retired prefix"
        caseInventoryCoversPredecessorCut,
      testCase
        "the empty union still establishes an exact successor base"
        caseEmptyUnionSuccessorBase
    ]

propUnionPresentationIndependent :: [Int] -> Property
propUnionPresentationIndependent presentation =
  let fixture = terminalFixture
      occurrenceOne = occurrence fixture 1 0 "one"
      occurrenceTwo = occurrence fixture 2 1 "two"
      (inventoryOne, archiveOne) =
        sealed fixture fixture.h1 [occurrenceOne]
      (inventoryTwo, archiveTwo) =
        sealed fixture fixture.h2 [occurrenceOne, occurrenceTwo]
      (inventoryThree, archiveThree) =
        sealed fixture fixture.h3 [occurrenceTwo]
      inventories = [inventoryOne, inventoryTwo, inventoryThree]
      archive = mergeArchives [archiveOne, archiveTwo, archiveThree]
      selectInventory value = case value `mod` 3 of
        0 -> inventoryOne
        1 -> inventoryTwo
        _ -> inventoryThree
      repeated = fmap selectInventory presentation
      expected = unionFor fixture inventories archive fixture.emptyOldVector
      observed =
        unionFor
          fixture
          (repeated <> reverse inventories)
          archive
          fixture.emptyOldVector
   in counterexample
        ("expected " <> show expected <> ", observed " <> show observed)
        (observed === expected)

caseGapAndLeastVector :: Assertion
caseGapAndLeastVector = do
  let fixture = terminalFixture
      occurrenceOne = occurrence fixture 1 2 "one-with-H2-dependency"
      occurrenceThree = occurrence fixture 3 0 "three-ahead"
      (inventoryOne, archiveOne) =
        sealed fixture fixture.h1 [occurrenceThree, occurrenceOne]
      (inventoryTwo, archiveTwo) =
        sealed fixture fixture.h2 [occurrenceOne]
      (inventoryThree, archiveThree) =
        sealed fixture fixture.h3 [occurrenceThree]
      union =
        checkedValue
          "gapped union"
          ( deriveTerminalSourceUnion
              (fixtureLineage fixture)
              (fixtureLineage fixture)
              (predecessorBaseFor fixture fixture.emptyOldVector)
              [inventoryThree, inventoryOne, inventoryTwo]
              (mergeArchives [archiveOne, archiveTwo, archiveThree])
              (constantTopologyDigest fixture)
          )
  assertEqual "closed prefix" (prefix 1) (maybe emptyStructuralPrefix id (lookup fixture.retired (terminalSourceUnionClosedPrefixes union)))
  assertEqual
    "included occurrence"
    [terminalSourceOccurrenceClaimId (claimFor occurrenceOne)]
    (fmap terminalSourceOccurrenceClaimId (terminalSourceUnionIncludedClaims union))
  assertEqual
    "ahead occurrence"
    [terminalSourceOccurrenceClaimId (claimFor occurrenceThree)]
    (fmap terminalSourceOccurrenceClaimId (terminalSourceUnionAheadClaims union))
  assertEqual
    "least vector covers the included H2 dependency"
    (Just (prefix 2))
    ( structuralVersionVectorComponent
        fixture.h2
        (terminalSourceUnionTerminalPredecessorVector union)
    )
  assertEqual
    "least vector closes H exactly through one"
    (Just (prefix 1))
    ( structuralVersionVectorComponent
        fixture.retired
        (terminalSourceUnionTerminalPredecessorVector union)
    )
  assertEqual
    "successor projection carries H2 without renumbering"
    (Just (prefix 2))
    ( structuralVersionVectorComponent
        fixture.h2
        (terminalSourceUnionSuccessorInitialVector union)
    )
  assertEqual
    "successor projection drops retired H"
    Nothing
    ( structuralVersionVectorComponent
        fixture.retired
        (terminalSourceUnionSuccessorInitialVector union)
    )
  assertEqual
    "retirement control is derived from membership"
    (controlIndex 7)
    (terminalSourceUnionTerminalControlPrefix union)

caseClaimConflict :: Assertion
caseClaimConflict = do
  let fixture = terminalFixture
      identifier = structuralOccurrenceId fixture.retired (sequenceNumber 1)
      firstDigest = digest 0x91
      secondDigest = digest 0x92
      inventoryOne =
        inventory fixture fixture.h1 [terminalSourceOccurrenceClaim identifier firstDigest]
      inventoryTwo =
        inventory fixture fixture.h2 [terminalSourceOccurrenceClaim identifier secondDigest]
      inventoryThree = inventory fixture fixture.h3 []
  case deriveTerminalSourceUnion
    (fixtureLineage fixture)
    (fixtureLineage fixture)
    (predecessorBaseFor fixture fixture.emptyOldVector)
    [inventoryOne, inventoryTwo, inventoryThree]
    emptyTerminalSourcePayloadArchive
    (constantTopologyDigest fixture) of
    Left (TerminalSourceClaimConflict actual incumbent conflicting) -> do
      assertEqual "conflicting occurrence" identifier actual
      assertEqual "incumbent digest" firstDigest incumbent
      assertEqual "conflicting digest" secondDigest conflicting
    observed -> assertFailure ("expected occurrence conflict, got " <> show observed)

caseMissingPayloadRelay :: Assertion
caseMissingPayloadRelay = do
  let fixture = terminalFixture
      retained = occurrence fixture 1 0 "retained-only-at-H2"
      claim = claimFor retained
      inventoryOne = inventory fixture fixture.h1 []
      inventoryTwo = inventory fixture fixture.h2 [claim]
      inventoryThree = inventory fixture fixture.h3 [claim]
      inventories = [inventoryThree, inventoryTwo, inventoryOne]
  requests <-
    checked
      "missing-payload requests"
      ( deriveTerminalSourcePayloadRequests
          (fixtureLineage fixture)
          (fixtureLineage fixture)
          fixture.retired
          inventories
          emptyTerminalSourcePayloadArchive
      )
  request <- case requests of
    [single] -> pure single
    observed -> assertFailure ("expected one request, got " <> show observed)
  assertEqual
    "request names missing occurrence"
    (terminalStructuralOccurrenceId retained)
    (terminalSourcePayloadRequestOccurrence request)
  assertEqual
    "least supporting reporter is selected"
    (terminalSourceInventoryDigest inventoryTwo)
    (terminalSourcePayloadRequestSupportingInventory request)
  case deriveTerminalSourceUnion
    (fixtureLineage fixture)
    (fixtureLineage fixture)
    (predecessorBaseFor fixture fixture.emptyOldVector)
    inventories
    emptyTerminalSourcePayloadArchive
    (constantTopologyDigest fixture) of
    Left (TerminalSourcePayloadMissing identifier) ->
      assertEqual
        "missing identifier"
        (terminalStructuralOccurrenceId retained)
        identifier
    observed -> assertFailure ("expected missing payload, got " <> show observed)
  relay <-
    checked
      "payload relay"
      ( terminalSourcePayloadRelay
          (fixtureLineage fixture)
          (fixtureLineage fixture)
          fixture.h2
          request
          inventoryTwo
          retained
      )
  repaired <-
    checked
      "accept payload relay"
      ( acceptTerminalSourcePayloadRelay
          (fixtureLineage fixture)
          (fixtureLineage fixture)
          request
          inventoryTwo
          relay
          emptyTerminalSourcePayloadArchive
      )
  let union = unionFor fixture inventories repaired fixture.emptyOldVector
  assertEqual "repair closes the source prefix" (prefix 1) (maybe emptyStructuralPrefix id (lookup fixture.retired (terminalSourceUnionClosedPrefixes union)))

casePayloadConflict :: Assertion
casePayloadConflict = do
  let fixture = terminalFixture
      incumbent = occurrence fixture 1 0 "canonical-one"
      conflicting = occurrence fixture 1 0 "canonical-two"
  case terminalStructuralOccurrence
    (terminalStructuralOccurrenceStamp incumbent)
    "canonical-two" of
    Left (TerminalSourceOccurrencePayloadDigestMismatch identifier expected actual) -> do
      assertEqual
        "mismatched payload occurrence"
        (terminalStructuralOccurrenceId incumbent)
        identifier
      assertEqual
        "stamp digest remains authoritative"
        (terminalStructuralOccurrencePublicationDigest incumbent)
        expected
      assertEqual
        "mismatched bytes derive the conflicting digest"
        (terminalStructuralOccurrencePublicationDigest conflicting)
        actual
    observed ->
      assertFailure
        ("expected payload/digest construction rejection, got " <> show observed)
  case terminalSourcePayloadArchive [incumbent, conflicting] of
    Left (TerminalSourcePayloadConflict identifier incumbentBytes conflictingBytes) -> do
      assertEqual
        "payload conflict occurrence"
        (terminalStructuralOccurrenceId incumbent)
        identifier
      assertBool "complete canonical bytes differ" (incumbentBytes /= conflictingBytes)
      assertBool
        "different canonical payloads have different structural digests"
        ( terminalStructuralOccurrencePublicationDigest incumbent
            /= terminalStructuralOccurrencePublicationDigest conflicting
        )
    observed -> assertFailure ("expected canonical payload conflict, got " <> show observed)

caseInventoryPrefixChecked :: Assertion
caseInventoryPrefixChecked = do
  let fixture = terminalFixture
      retained = occurrence fixture 1 0 "one"
      claim = claimFor retained
  assertPrefixRejected fixture emptyStructuralPrefix [claim] (prefix 1)
  assertPrefixRejected fixture (prefix 2) [claim] (prefix 1)

caseInventoryCoversPredecessorCut :: Assertion
caseInventoryCoversPredecessorCut = do
  let fixture = terminalFixture
      retained = occurrence fixture 1 0 "one"
      (inventoryOne, archive) = sealed fixture fixture.h1 [retained]
      inventoryTwo = inventory fixture fixture.h2 []
      inventoryThree = inventory fixture fixture.h3 []
      predecessorVector = oldVectorFor fixture 0 0 0 1
  case deriveTerminalSourceUnion
    (fixtureLineage fixture)
    (fixtureLineage fixture)
    (predecessorBaseFor fixture predecessorVector)
    [inventoryOne, inventoryTwo, inventoryThree]
    archive
    (constantTopologyDigest fixture) of
    Left (TerminalSourceInventoryBehindPredecessorCut reporter required observed) -> do
      assertEqual "behind reporter" fixture.h2 reporter
      assertEqual "installed retired prefix" (prefix 1) required
      assertEqual "reported retired prefix" emptyStructuralPrefix observed
    observed ->
      assertFailure
        ("expected behind-inventory rejection, got " <> show observed)

caseEmptyUnionSuccessorBase :: Assertion
caseEmptyUnionSuccessorBase = do
  let fixture = terminalFixture
      oldVector = oldVectorFor fixture 0 2 1 0
      inventories =
        [ inventory fixture fixture.h1 [],
          inventory fixture fixture.h2 [],
          inventory fixture fixture.h3 []
        ]
      union = unionFor fixture inventories emptyTerminalSourcePayloadArchive oldVector
  assertEqual "empty closed prefix" emptyStructuralPrefix (maybe emptyStructuralPrefix id (lookup fixture.retired (terminalSourceUnionClosedPrefixes union)))
  assertEqual "no included claims" [] (terminalSourceUnionIncludedClaims union)
  assertEqual "no ahead claims" [] (terminalSourceUnionAheadClaims union)
  assertEqual
    "survivor progress survives empty union"
    (Just (prefix 2))
    ( structuralVersionVectorComponent
        fixture.h2
        (terminalSourceUnionSuccessorInitialVector union)
    )
  case deriveTerminalSourceUnion
    (fixtureLineage fixture)
    (fixtureLineage fixture)
    (predecessorBaseFor fixture oldVector)
    (drop 1 inventories)
    emptyTerminalSourcePayloadArchive
    (constantTopologyDigest fixture) of
    Left (TerminalSourceInventoryMissingReporters [missing]) ->
      assertEqual "exact inventory set names missing reporter" fixture.h1 missing
    observed -> assertFailure ("expected exact-inventory failure, got " <> show observed)
  let expectedAnnouncer = min fixture.h1 (min fixture.h2 fixture.h3)
  announce <-
    checked
      "successor announce"
      (terminalSourceUnionAnnounce fixture.successor expectedAnnouncer union)
  assertEqual
    "successor announcer is deterministic"
    expectedAnnouncer
    (terminalSourceUnionAnnounceAnnouncer announce)
  case terminalSourceUnionReady fixture.emptyOldVector (controlIndex 7) union of
    Left (TerminalSourceHistoryNotApplied required observed) -> do
      assertEqual "required terminal history" oldVector required
      assertEqual "stale local history" fixture.emptyOldVector observed
    observed ->
      assertFailure
        ("expected terminal-history readiness rejection, got " <> show observed)
  case terminalSourceUnionReady oldVector (controlIndex 6) union of
    Left (TerminalSourceControlNotApplied required observed) -> do
      assertEqual "required retirement control" (controlIndex 7) required
      assertEqual "stale applied control" (controlIndex 6) observed
    observed ->
      assertFailure
        ("expected terminal-control readiness rejection, got " <> show observed)
  ready <-
    checked
      "local union readiness"
      (terminalSourceUnionReady oldVector (controlIndex 7) union)
  acceptances <-
    traverse
      ( \reporter ->
          checked
            "union acceptance"
            (terminalSourceUnionAcceptance fixture.successor reporter ready)
      )
      [fixture.h3, fixture.h1, fixture.h2]
  case terminalSourceUnionEstablished fixture.successor union (drop 1 acceptances) of
    Left (TerminalSourceAcceptanceMissingReporters [_]) -> pure ()
    observed -> assertFailure ("expected incomplete acceptance failure, got " <> show observed)
  established <-
    checked
      "union established"
      ( terminalSourceUnionEstablished
          fixture.successor
          union
          (acceptances <> take 1 acceptances)
      )
  predecessor <-
    checked
      "membership successor predecessor"
      ( terminalSourceUnionTopologyPredecessor
          (fixtureLineage fixture)
          union
      )
  case topologyPredecessorView predecessor of
    MembershipSuccessorPredecessorView
      predecessorCut
      predecessorGeneration
      successorGeneration
      terminalVector
      successorVector
      unionDigest -> do
        assertEqual
          "historical cut"
          ( terminalSourcePredecessorBaseCutId
              (predecessorBaseFor fixture oldVector)
          )
          predecessorCut
        assertEqual
          "historical generation"
          (heraldMembershipGenerationId fixture.predecessor)
          predecessorGeneration
        assertEqual
          "successor generation"
          (heraldMembershipGenerationId fixture.successor)
          successorGeneration
        assertEqual "terminal vector" oldVector terminalVector
        assertEqual
          "successor vector"
          (terminalSourceUnionSuccessorInitialVector union)
          successorVector
        assertEqual "union digest" (terminalSourceUnionDigest union) unionDigest
    observed -> assertFailure ("expected successor predecessor, got " <> show observed)
  cut <-
    checked
      "first successor topology cut"
      ( topologyCut
          predecessor
          ( topologyFrontier
              (terminalSourceUnionSuccessorInitialVector union)
              (controlIndex 7)
          )
          fixture.topologyDigest
      )
  base <-
    checked
      "installed successor base"
      ( establishedSuccessorStructuralBase
          (fixtureLineage fixture)
          established
          cut
      )
  assertBool
    "installed base releases exactly the successor hold"
    ( successorStructuralBaseReleases
        (heraldMembershipGenerationId fixture.successor)
        base
    )
  assertBool
    "installed base does not release the predecessor generation"
    ( not
        ( successorStructuralBaseReleases
            (heraldMembershipGenerationId fixture.predecessor)
            base
        )
    )
  let successorInitial = terminalSourceUnionSuccessorInitialVector union
      belowBase = emptyStructuralVersionVector fixture.successor
  case allocateSuccessorStructuralOccurrence fixture.h2 belowBase base of
    Left (TerminalSourceSuccessorAdvanceBeforeBase herald required actual) -> do
      assertEqual "regressed component" fixture.h2 herald
      assertEqual "installed component" (prefix 2) required
      assertEqual "supplied component" emptyStructuralPrefix actual
    observed ->
      assertFailure
        ("expected below-base successor cursor rejection, got " <> show observed)
  advance <-
    checked
      "allocate after carried H2 prefix"
      (allocateSuccessorStructuralOccurrence fixture.h2 successorInitial base)
  assertEqual
    "membership change does not reset a survivor's source sequence"
    (structuralOccurrenceId fixture.h2 (sequenceNumber 3))
    (successorStructuralAdvanceOccurrence advance)
  assertEqual
    "only H2 advances"
    (Just (prefix 3))
    ( structuralVersionVectorComponent
        fixture.h2
        (successorStructuralAdvanceVector advance)
    )
  assertEqual
    "H3 remains at its projected prefix"
    (Just (prefix 1))
    ( structuralVersionVectorComponent
        fixture.h3
        (successorStructuralAdvanceVector advance)
    )
  case allocateSuccessorStructuralOccurrence fixture.retired successorInitial base of
    Left (TerminalSourceSuccessorAdvanceSourceNotMember retired) ->
      assertEqual "retired allocator remains sealed" fixture.retired retired
    observed ->
      assertFailure
        ("expected retired-source allocation rejection, got " <> show observed)
  wrongCut <-
    checked
      "same-predecessor but wrong-frontier cut"
      ( topologyCut
          predecessor
          (topologyFrontier successorInitial (controlIndex 8))
          fixture.topologyDigest
      )
  case establishedSuccessorStructuralBase
    (fixtureLineage fixture)
    established
    wrongCut of
    Left (TerminalSourceSuccessorCutMismatch expected observed) ->
      assertBool "wrong frontier changes the cut identity" (expected /= observed)
    observed ->
      assertFailure
        ("expected exact successor-cut rejection, got " <> show observed)
  case projectSurvivorLiveAheadVector
    (oldVectorFor fixture 0 2 1 1)
    base of
    Left (TerminalSourceRetiredLiveAheadBeyondTerminal terminal observed) -> do
      assertEqual "sealed retired prefix" emptyStructuralPrefix terminal
      assertEqual "unsealed retired live-ahead" (prefix 1) observed
    observed ->
      assertFailure
        ("expected retired live-ahead rejection, got " <> show observed)
  case projectSurvivorLiveAheadVector fixture.emptyOldVector base of
    Left (TerminalSourceSurvivorLiveAheadBeforeBase herald required actual) -> do
      assertEqual "missing repaired survivor component" fixture.h2 herald
      assertEqual "shared successor base component" (prefix 2) required
      assertEqual "stale local applied component" emptyStructuralPrefix actual
    observed ->
      assertFailure
        ("expected unrepaired survivor-history rejection, got " <> show observed)

assertPrefixRejected ::
  TerminalFixture ->
  StructuralPrefix ->
  [TerminalSourceOccurrenceClaim] ->
  StructuralPrefix ->
  Assertion
assertPrefixRejected fixture claimed claims expected =
  case terminalSourceInventoryWithClaimedPrefix
    (fixtureLineage fixture)
    fixture.retired
    fixture.h1
    []
    []
    claimed
    claims of
    Left (TerminalSourceInventoryPrefixMismatch actual supplied) -> do
      assertEqual "derived prefix" expected actual
      assertEqual "rejected supplied prefix" claimed supplied
    observed -> assertFailure ("expected prefix mismatch, got " <> show observed)

data TerminalFixture = TerminalFixture
  { predecessor :: HeraldMembershipGeneration,
    successor :: HeraldMembershipGeneration,
    h1 :: HeraldEpoch,
    h2 :: HeraldEpoch,
    h3 :: HeraldEpoch,
    retired :: HeraldEpoch,
    predecessorCut :: TopologyCutId,
    topologyDigest :: TopologyOccurrenceDigest,
    emptyOldVector :: StructuralVersionVector
  }

terminalFixture :: TerminalFixture
terminalFixture =
  let system = checkedValue "system" (mkSystemId (identifierBytes 0x40))
      h1 = checkedValue "H1" (mkHeraldEpoch (identifierBytes 0x11))
      h2 = checkedValue "H2" (mkHeraldEpoch (identifierBytes 0x22))
      h3 = checkedValue "H3" (mkHeraldEpoch (identifierBytes 0x33))
      retired = checkedValue "H4" (mkHeraldEpoch (identifierBytes 0x44))
      predecessor =
        checkedValue
          "predecessor generation"
          (genesisHeraldMembershipGeneration system (h1 :| [h2, h3, retired]))
      successor =
        checkedValue
          "successor generation"
          (retireHeraldMembershipGeneration (controlIndex 7) (MembershipFixtures.fixtureRetirementResolution (controlIndex 7)) retired predecessor)
      predecessorCut =
        checkedValue "predecessor cut" (mkTopologyCutId (identifierBytes 0x55))
      topologyDigest =
        checkedValue
          "terminal topology digest"
          (mkTopologyOccurrenceDigest (identifierBytes 0x66))
      fixture =
        TerminalFixture
          { predecessor,
            successor,
            h1,
            h2,
            h3,
            retired,
            predecessorCut,
            topologyDigest,
            emptyOldVector = emptyStructuralVersionVector predecessor
          }
   in fixture

oldVectorFor ::
  TerminalFixture ->
  Word ->
  Word ->
  Word ->
  Word ->
  StructuralVersionVector
oldVectorFor fixture h1 h2 h3 retired =
  checkedValue
    "old structural vector"
    ( mkStructuralVersionVector
        fixture.predecessor
        [ (fixture.h1, prefix h1),
          (fixture.h2, prefix h2),
          (fixture.h3, prefix h3),
          (fixture.retired, prefix retired)
        ]
    )

occurrence ::
  TerminalFixture ->
  Word ->
  Word ->
  ByteString ->
  TerminalStructuralOccurrence
occurrence fixture sequenceIndex h2Dependency payload =
  let identifier =
        structuralOccurrenceId fixture.retired (sequenceNumber sequenceIndex)
      predecessor =
        oldVectorFor fixture 0 h2Dependency 0 (sequenceIndex - 1)
      source =
        checkedValue
          "terminal occurrence source nabla"
          (mkNablaId (identifierBytes (0x70 + sequenceIndex)))
      stamp =
        checkedValue
          "terminal occurrence stamp"
          ( mkStructuralOccurrenceStamp
              identifier
              predecessor
              ( publicationId
                  source
                  genesisAuthorityEpoch
                  fixture.retired
                  (nablaSequence (fromIntegral sequenceIndex))
              )
              (digestForPayload payload)
              NeutralVertexCarrier
          )
   in checkedValue
        "terminal structural occurrence"
        (terminalStructuralOccurrence stamp payload)

claimFor :: TerminalStructuralOccurrence -> TerminalSourceOccurrenceClaim
claimFor retainedOccurrence =
  terminalSourceOccurrenceClaim
    (terminalStructuralOccurrenceId retainedOccurrence)
    (terminalStructuralOccurrencePublicationDigest retainedOccurrence)

sealed ::
  TerminalFixture ->
  HeraldEpoch ->
  [TerminalStructuralOccurrence] ->
  (TerminalSourceInventory, TerminalSourcePayloadArchive)
sealed fixture reporter retained =
  checkedValue
    "sealed terminal-source inventory"
    ( sealTerminalSourceInventory
        (fixtureLineage fixture)
        (fixtureLineage fixture)
        fixture.retired
        reporter
        []
        []
        retained
    )

inventory ::
  TerminalFixture ->
  HeraldEpoch ->
  [TerminalSourceOccurrenceClaim] ->
  TerminalSourceInventory
inventory fixture reporter claims =
  checkedValue
    "terminal-source inventory"
    ( terminalSourceInventory
        (fixtureLineage fixture)
        fixture.retired
        reporter
        []
        []
        claims
    )

mergeArchives ::
  [TerminalSourcePayloadArchive] -> TerminalSourcePayloadArchive
mergeArchives = foldl merge emptyTerminalSourcePayloadArchive
  where
    merge retained supplied =
      foldl
        ( \archive (_, retainedOccurrence) ->
            checkedValue
              "merge payload archive"
              (retainTerminalSourcePayload retainedOccurrence archive)
        )
        retained
        (terminalSourcePayloadEntries supplied)

unionFor ::
  TerminalFixture ->
  [TerminalSourceInventory] ->
  TerminalSourcePayloadArchive ->
  StructuralVersionVector ->
  TerminalSourceUnion
unionFor fixture inventories payloads predecessorVector =
  checkedValue
    "terminal-source union"
    ( deriveTerminalSourceUnion
        (fixtureLineage fixture)
        (fixtureLineage fixture)
        (predecessorBaseFor fixture predecessorVector)
        inventories
        payloads
        (constantTopologyDigest fixture)
    )

predecessorBaseFor ::
  TerminalFixture ->
  StructuralVersionVector ->
  TerminalSourcePredecessorBase
predecessorBaseFor fixture vector
  | vector == fixture.emptyOldVector =
      checkedValue
        "genesis predecessor base"
        (terminalSourceGenesisPredecessorBase fixture.predecessor fixture.predecessorCut)
  | otherwise =
      let installed =
            checkedValue
              "installed predecessor cut"
              ( topologyCut
                  (sameGenerationPredecessor fixture.predecessorCut)
                  (topologyFrontier vector (controlIndex 0))
                  fixture.topologyDigest
              )
       in checkedValue
            "installed predecessor base"
            (terminalSourceInstalledPredecessorBase fixture.predecessor installed)

constantTopologyDigest ::
  TerminalFixture ->
  StructuralVersionVector ->
  ControlIndex ->
  TopologyOccurrenceDigest
constantTopologyDigest fixture _ _ = fixture.topologyDigest

prefix :: Word -> StructuralPrefix
prefix 0 = emptyStructuralPrefix
prefix value = structuralPrefixThrough (sequenceNumber value)

sequenceNumber :: Word -> StructuralSequence
sequenceNumber =
  checkedValue "structural sequence" . mkStructuralSequence . fromIntegral

digest :: Word -> StructuralPublicationDigest
digest tag =
  checkedValue
    "structural publication digest"
    (mkStructuralPublicationDigest (identifierBytes tag))

digestForPayload :: ByteString -> StructuralPublicationDigest
digestForPayload payload =
  deriveTerminalStructuralPayloadDigest payload

identifierBytes :: Word -> ByteString
identifierBytes = ByteString.replicate 32 . fromIntegral

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id

fixtureLineage :: TerminalFixture -> HeraldMembershipLineage
fixtureLineage fixture = checkedValue "fixture lineage" $ do
  history <- heraldMembershipHistory (fixture.predecessor :| [fixture.successor])
  heraldMembershipLineage (heraldMembershipGenerationId fixture.predecessor) (heraldMembershipGenerationId fixture.successor) history
