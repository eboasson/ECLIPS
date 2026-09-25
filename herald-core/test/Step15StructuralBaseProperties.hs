{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Step15StructuralBaseProperties
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
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
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
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Topology
  ( TopologyOccurrenceDigest,
    mkTopologyOccurrenceDigest,
  )
import Eclips.Herald.Graph.TerminalSource
  ( TerminalSourceInventory,
    TerminalSourceProblem (..),
    TerminalSourceUnionAcceptance,
    TerminalSourceUnionAnnounce,
    TerminalStructuralOccurrence,
    allocateSuccessorStructuralOccurrence,
    carriedStampedSurvivorPredecessor,
    carriedStampedSurvivorStamp,
    carriedStampedSurvivorVector,
    carryStampedSurvivorOccurrence,
    deriveTerminalStructuralPayloadDigest,
    projectSurvivorLiveAheadVector,
    successorStructuralAdvanceOccurrence,
    successorStructuralAdvancePredecessor,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInventoryReporter,
    terminalSourcePayloadRequestOccurrence,
    terminalSourceUnionAnnounceAnnouncer,
    terminalSourceUnionAnnounceUnion,
    terminalSourceUnionAnnouncer,
    terminalSourceUnionClosedPrefixes,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTerminalPredecessorVector,
    terminalStructuralOccurrence,
    terminalStructuralOccurrenceId,
  )
import Eclips.Herald.PeerPublication
  ( StructuralOccurrenceStamp,
    StructuralPublicationDigest,
    mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
    structuralOccurrenceStampPredecessor,
  )
import Eclips.Herald.UseCase.Step15StructuralBase
import GenesisFixtures qualified as MembershipFixtures
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
    "Step-15 structural successor-base coordinator"
    [ testCase
        "a late H2-only payload is repaired before the exact successor cut releases work"
        caseLateH2OnlyPayloadRelay,
      testCase
        "retired old announcer is replaced by the least successor for an empty union"
        caseRetiredAnnouncerAndEmptyUnion
    ]

caseLateH2OnlyPayloadRelay :: Assertion
caseLateH2OnlyPayloadRelay = do
  let fixture = structuralBaseFixture
      retained = occurrence fixture 1 "retained-only-at-H2"
      h1Start = begin fixture fixture.h1 []
      h2Start = begin fixture fixture.h2 [retained]
      h3Start = begin fixture fixture.h3 []
      h1Inventory = singleLocalInventory h1Start
      h2Inventory = singleLocalInventory h2Start
      h3Inventory = singleLocalInventory h3Start
  assertEqual
    "the frozen inventories retain their semantic reporters"
    [fixture.h1, fixture.h2, fixture.h3]
    ( fmap
        terminalSourceInventoryReporter
        [h1Inventory, h2Inventory, h3Inventory]
    )

  h1BeforeLate <-
    checked
      "H1 receives H3 before H2"
      (receiveStructuralBaseInventory h3Inventory h1Start)
  requestsBefore <-
    checked "requests before late inventory" (structuralBasePayloadRequests h1BeforeLate)
  assertEqual "H2-only work is unknown before H2 reports" [] requestsBefore
  h1AllInventories <-
    checked
      "late H2 inventory"
      (receiveStructuralBaseInventory h2Inventory h1BeforeLate)
  requests <-
    checked "requests after late inventory" (structuralBasePayloadRequests h1AllInventories)
  request <- case requests of
    [single] -> pure single
    observed -> assertFailure ("expected one missing-payload request, got " <> show observed)
  assertEqual
    "the request preserves H4's occurrence identity"
    (terminalStructuralOccurrenceId retained)
    (terminalSourcePayloadRequestOccurrence request)

  relay <-
    checked
      "H2 relays its frozen payload"
      (prepareStructuralBasePayloadRelay request h2Start)
  h1Repaired <-
    checked
      "H1 accepts H2 relay"
      (receiveStructuralBasePayloadRelay request relay h1AllInventories)

  h2All <-
    addInventories "H2 completes inventory set" [h1Inventory, h3Inventory] h2Start
  h3All <-
    addInventories "H3 completes inventory set" [h1Inventory, h2Inventory] h3Start
  h3Repaired <-
    checked
      "H3 accepts the same H2 relay"
      (receiveStructuralBasePayloadRelay request relay h3All)

  (h1Announced, announce) <-
    checked
      "H1 announces canonical union"
      (prepareStructuralBaseUnionAnnouncement (constantDigest fixture) h1Repaired)
  assertEqual
    "the successor announcer is H1"
    fixture.h1
    (terminalSourceUnionAnnounceAnnouncer announce)
  case acceptStructuralBaseUnionAnnouncement
    (constantDigest fixture)
    fixture.predecessorVector
    (controlIndex 7)
    announce
    h1Announced of
    Left
      ( StructuralBaseTerminalSourceProblem
          (TerminalSourceHistoryNotApplied _ _)
        ) -> pure ()
    observed ->
      assertFailure
        ("expected pre-repair history gate, got " <> show observed)
  (h1Accepted, h1Acceptance) <-
    checked
      "H1 accepts its announcement"
      (acceptAnnouncement fixture announce h1Announced)
  (h2Accepted, h2Acceptance) <-
    checked
      "H2 accepts H1 announcement"
      (acceptAnnouncement fixture announce h2All)
  (_, h3Acceptance) <-
    checked
      "H3 accepts H1 announcement"
      (acceptAnnouncement fixture announce h3Repaired)
  h1WithH2 <-
    checked
      "H1 retains H2 acceptance"
      (receiveStructuralBaseUnionAcceptance h2Acceptance h1Accepted)
  h1WithAll <-
    checked
      "H1 retains H3 acceptance"
      (receiveStructuralBaseUnionAcceptance h3Acceptance h1WithH2)
  -- Exact duplicate acceptance is inert rather than counted twice.
  h1WithDuplicate <-
    checked
      "H1 acceptance replay"
      (receiveStructuralBaseUnionAcceptance h1Acceptance h1WithAll)
  (establishedCoordinator, established) <-
    checked "establish accepted union" (establishStructuralBaseUnion h1WithDuplicate)
  h2Established <-
    checked
      "H2 accepts established evidence"
      ( receiveStructuralBaseUnionEstablished
          (constantDigest fixture)
          ( terminalSourceUnionTerminalPredecessorVector
              (terminalSourceUnionEstablishedUnion established)
          )
          ( terminalSourceUnionTerminalControlPrefix
              (terminalSourceUnionEstablishedUnion established)
          )
          established
          h2Accepted
      )
  assertEqual
    "late payload closes the retired source through one"
    [(fixture.h4, prefix 1)]
    (terminalSourceUnionClosedPrefixes (terminalSourceUnionEstablishedUnion established))
  assertEqual
    "union establishment alone yields no successor-base evidence"
    Nothing
    (structuralBaseEvidence establishedCoordinator)
  assertBool
    "the exact held dependency remains blocked before cut installation"
    ( not
        ( structuralBaseReleases
            (heraldMembershipGenerationId fixture.successor)
            establishedCoordinator
        )
    )
  assertEqual
    "receiving established evidence also remains pre-installation"
    Nothing
    (structuralBaseEvidence h2Established)
  cut <-
    checked
      "derive exact membership-successor cut"
      (expectedSuccessorStructuralCut establishedCoordinator)
  installed <-
    checked
      "Graph reports exact cut installed"
      (recordInstalledSuccessorStructuralCut cut establishedCoordinator)
  assertBool
    "installation yields successor-base evidence"
    (structuralBaseEvidence installed /= Nothing)
  assertBool
    "installation releases the exact successor dependency"
    ( structuralBaseReleases
        (heraldMembershipGenerationId fixture.successor)
        installed
    )
  assertBool
    "installation does not release an old-generation dependency"
    ( not
        ( structuralBaseReleases
            (heraldMembershipGenerationId fixture.predecessor)
            installed
        )
    )

  base <-
    maybe
      (assertFailure "installed cut did not expose successor-base evidence")
      pure
      (structuralBaseEvidence installed)
  let stampedSurvivor = survivorStamp fixture
      oldLiveAhead = survivorLiveAhead fixture
  projectedLiveAhead <-
    checked
      "project already-applied survivor live-ahead state"
      (projectSurvivorLiveAheadVector oldLiveAhead base)
  assertEqual
    "projection preserves H3's already-applied prefix without replay"
    (Just (prefix 2))
    (structuralVersionVectorComponent fixture.h3 projectedLiveAhead)
  assertEqual
    "projection drops only retired H4"
    Nothing
    (structuralVersionVectorComponent fixture.h4 projectedLiveAhead)
  carried <-
    checked
      "carry old-stamped H2 occurrence through successor lineage"
      (carryStampedSurvivorOccurrence stampedSurvivor projectedLiveAhead base)
  assertEqual
    "the carried occurrence keeps its complete old stamp"
    stampedSurvivor
    (carriedStampedSurvivorStamp carried)
  assertEqual
    "the carry is based on successor live-ahead above the exact projection"
    projectedLiveAhead
    (carriedStampedSurvivorPredecessor carried)
  assertEqual
    "the old stamp itself remains predecessor-generation evidence"
    (heraldMembershipGenerationId fixture.predecessor)
    ( structuralVersionVectorMembershipGenerationId
        (structuralOccurrenceStampPredecessor stampedSurvivor)
    )
  assertEqual
    "the carried H2 occurrence advances its successor component"
    (Just (prefix 1))
    ( structuralVersionVectorComponent
        fixture.h2
        (carriedStampedSurvivorVector carried)
    )
  assertEqual
    "the retired H4 coordinate is absent from the successor vector"
    Nothing
    ( structuralVersionVectorComponent
        fixture.h4
        (carriedStampedSurvivorVector carried)
    )

  allocated <-
    checked
      "allocate previously unsequenced H1 work from installed base"
      ( allocateSuccessorStructuralOccurrence
          fixture.h1
          (carriedStampedSurvivorVector carried)
          base
      )
  assertEqual
    "new work receives the next successor-generation occurrence"
    (structuralOccurrenceId fixture.h1 (sequenceNumber 1))
    (successorStructuralAdvanceOccurrence allocated)
  assertEqual
    "new work's predecessor is successor-generation evidence"
    (heraldMembershipGenerationId fixture.successor)
    ( structuralVersionVectorMembershipGenerationId
        (successorStructuralAdvancePredecessor allocated)
    )

  case carryStampedSurvivorOccurrence
    (survivorStampWith fixture 1 2 0 0)
    projectedLiveAhead
    base of
    Left
      ( TerminalSourceStampedSurvivorDependencyNotCovered
          identifier
          herald
          required
          available
        ) -> do
        assertEqual
          "retired dependency rejection names the pending stamp"
          (structuralOccurrenceId fixture.h2 (sequenceNumber 1))
          identifier
        assertEqual "retired dependency source" fixture.h4 herald
        assertEqual "required retired prefix" (prefix 2) required
        assertEqual "sealed retired prefix" (prefix 1) available
    observed ->
      assertFailure
        ("expected retired dependency rejection, got " <> show observed)
  case carryStampedSurvivorOccurrence
    (survivorStampWith fixture 1 1 0 3)
    projectedLiveAhead
    base of
    Left
      ( TerminalSourceStampedSurvivorDependencyNotCovered
          _
          herald
          required
          available
        ) -> do
        assertEqual "survivor dependency source" fixture.h3 herald
        assertEqual "required survivor prefix" (prefix 3) required
        assertEqual "local live-ahead prefix" (prefix 2) available
    observed ->
      assertFailure
        ("expected survivor dependency rejection, got " <> show observed)
  alreadyApplied <-
    checked
      "project already-applied H2 occurrence"
      ( projectSurvivorLiveAheadVector
          (survivorLiveAheadWith fixture 2 2)
          base
      )
  case carryStampedSurvivorOccurrence
    (survivorStampWith fixture 2 1 1 0)
    alreadyApplied
    base of
    Left (TerminalSourceStampedSurvivorOccurrenceNotNext expected observed) -> do
      assertEqual
        "already-applied work is projected rather than replayed"
        (structuralOccurrenceId fixture.h2 (sequenceNumber 3))
        expected
      assertEqual
        "the old occurrence retains its immutable identity"
        (structuralOccurrenceId fixture.h2 (sequenceNumber 2))
        observed
    outcome ->
      assertFailure
        ("expected already-covered occurrence rejection, got " <> show outcome)

caseRetiredAnnouncerAndEmptyUnion :: Assertion
caseRetiredAnnouncerAndEmptyUnion = do
  let fixture = structuralBaseFixture
      h1Start = begin fixture fixture.h1 []
      h2Start = begin fixture fixture.h2 []
      h3Start = begin fixture fixture.h3 []
      inventories =
        fmap singleLocalInventory [h1Start, h2Start, h3Start]
  assertEqual
    "H4 was the predecessor generation's announcer"
    fixture.h4
    (terminalSourceUnionAnnouncer fixture.predecessor)
  assertEqual
    "retirement selects H1 from retained successor state"
    fixture.h1
    (terminalSourceUnionAnnouncer fixture.successor)
  h1All <- addForeignInventories "H1 empty inventories" fixture.h1 inventories h1Start
  h2All <- addForeignInventories "H2 empty inventories" fixture.h2 inventories h2Start
  h3All <- addForeignInventories "H3 empty inventories" fixture.h3 inventories h3Start
  noRequests <-
    traverse
      (checked "empty-union payload requests" . structuralBasePayloadRequests)
      [h1All, h2All, h3All]
  assertEqual "the empty union needs no repair" [[], [], []] noRequests
  case prepareStructuralBaseUnionAnnouncement (constantDigest fixture) h2All of
    Left
      ( StructuralBaseTerminalSourceProblem
          (TerminalSourceAnnouncerMismatch expected observed)
        ) -> do
        assertEqual "required successor announcer" fixture.h1 expected
        assertEqual "non-announcer rejected" fixture.h2 observed
    other -> assertFailure ("expected deterministic-announcer rejection, got " <> show other)
  (h1Announced, announce) <-
    checked
      "H1 announces empty union"
      (prepareStructuralBaseUnionAnnouncement (constantDigest fixture) h1All)
  (h1Accepted, acceptance1) <-
    checked
      "H1 accepts empty union"
      (acceptAnnouncement fixture announce h1Announced)
  (_, acceptance2) <-
    checked
      "H2 accepts empty union"
      (acceptAnnouncement fixture announce h2All)
  (_, acceptance3) <-
    checked
      "H3 accepts empty union"
      (acceptAnnouncement fixture announce h3All)
  withH2 <-
    checked
      "retain H2 empty-union acceptance"
      (receiveStructuralBaseUnionAcceptance acceptance2 h1Accepted)
  withH3 <-
    checked
      "retain H3 empty-union acceptance"
      (receiveStructuralBaseUnionAcceptance acceptance3 withH2)
  withOwnReplay <-
    checked
      "retain H1 empty-union acceptance replay"
      (receiveStructuralBaseUnionAcceptance acceptance1 withH3)
  (establishedCoordinator, established) <-
    checked "establish empty union" (establishStructuralBaseUnion withOwnReplay)
  assertEqual
    "empty union closes no H4 occurrence"
    [(fixture.h4, emptyStructuralPrefix)]
    (terminalSourceUnionClosedPrefixes (terminalSourceUnionEstablishedUnion established))
  assertEqual
    "empty union still waits for cut installation"
    Nothing
    (structuralBaseEvidence establishedCoordinator)
  cut <-
    checked "derive empty successor cut" (expectedSuccessorStructuralCut establishedCoordinator)
  installed <-
    checked
      "install empty successor cut"
      (recordInstalledSuccessorStructuralCut cut establishedCoordinator)
  assertBool
    "even the empty union yields a base only after its strict cut"
    ( structuralBaseReleases
        (heraldMembershipGenerationId fixture.successor)
        installed
    )

data StructuralBaseFixture = StructuralBaseFixture
  { predecessor :: HeraldMembershipGeneration,
    successor :: HeraldMembershipGeneration,
    h1 :: HeraldEpoch,
    h2 :: HeraldEpoch,
    h3 :: HeraldEpoch,
    h4 :: HeraldEpoch,
    predecessorCut :: TopologyCutId,
    predecessorVector :: StructuralVersionVector,
    topologyDigest :: TopologyOccurrenceDigest
  }

structuralBaseFixture :: StructuralBaseFixture
structuralBaseFixture =
  let system = checkedValue "system" (mkSystemId (bytes 0x50))
      -- H4 is deliberately least in the predecessor.  Its retirement must not
      -- leave a hole in announcer ownership.
      h4 = checkedValue "H4" (mkHeraldEpoch (bytes 0x04))
      h1 = checkedValue "H1" (mkHeraldEpoch (bytes 0x11))
      h2 = checkedValue "H2" (mkHeraldEpoch (bytes 0x22))
      h3 = checkedValue "H3" (mkHeraldEpoch (bytes 0x33))
      predecessor =
        checkedValue
          "predecessor membership"
          (genesisHeraldMembershipGeneration system (h4 :| [h1, h2, h3]))
      successor =
        checkedValue
          "successor membership"
          (retireHeraldMembershipGeneration (controlIndex 7) (MembershipFixtures.fixtureRetirementResolution (controlIndex 7)) h4 predecessor)
   in StructuralBaseFixture
        { predecessor,
          successor,
          h1,
          h2,
          h3,
          h4,
          predecessorCut =
            checkedValue "predecessor cut" (mkTopologyCutId (bytes 0x61)),
          predecessorVector = emptyStructuralVersionVector predecessor,
          topologyDigest =
            checkedValue
              "terminal topology digest"
              (mkTopologyOccurrenceDigest (bytes 0x71))
        }

begin ::
  StructuralBaseFixture ->
  HeraldEpoch ->
  [TerminalStructuralOccurrence] ->
  MembershipBaseClosure
begin fixture local retained =
  let predecessorBase =
        checkedValue
          "genesis predecessor base"
          ( terminalSourceGenesisPredecessorBase
              fixture.predecessor
              fixture.predecessorCut
          )
   in checkedValue
        "begin structural-base coordinator"
        ( beginMembershipBaseClosure
            (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessor fixture.successor)
            (MembershipFixtures.fixtureGenesisRetirementLineage fixture.predecessor fixture.successor)
            local
            predecessorBase
            []
            []
            retained
        )

addInventories ::
  String ->
  [TerminalSourceInventory] ->
  MembershipBaseClosure ->
  IO MembershipBaseClosure
addInventories context supplied initial =
  foldl
    ( \accumulator inventory -> do
        state <- accumulator
        checked context (receiveStructuralBaseInventory inventory state)
    )
    (pure initial)
    supplied

addForeignInventories ::
  String ->
  HeraldEpoch ->
  [TerminalSourceInventory] ->
  MembershipBaseClosure ->
  IO MembershipBaseClosure
addForeignInventories context local =
  addInventories context
    . filter ((/= local) . terminalSourceInventoryReporter)

occurrence ::
  StructuralBaseFixture ->
  Word ->
  ByteString ->
  TerminalStructuralOccurrence
occurrence fixture sequenceIndex payload =
  let identifier = structuralOccurrenceId fixture.h4 (sequenceNumber sequenceIndex)
      source =
        checkedValue
          "terminal occurrence source"
          (mkNablaId (bytes (0x80 + sequenceIndex)))
      predecessor =
        checkedValue
          "terminal occurrence predecessor"
          ( mkStructuralVersionVector
              fixture.predecessor
              [ (fixture.h4, prefix (sequenceIndex - 1)),
                (fixture.h1, emptyStructuralPrefix),
                (fixture.h2, emptyStructuralPrefix),
                (fixture.h3, emptyStructuralPrefix)
              ]
          )
      stamp =
        checkedValue
          "terminal occurrence stamp"
          ( mkStructuralOccurrenceStamp
              identifier
              predecessor
              ( publicationId
                  source
                  genesisAuthorityEpoch
                  fixture.h4
                  (nablaSequence (fromIntegral sequenceIndex))
              )
              (digestForPayload payload)
              NeutralVertexCarrier
          )
   in checkedValue
        "terminal structural occurrence"
        (terminalStructuralOccurrence stamp payload)

-- | A survivor-authored occurrence stamped before contraction.  Its dependency
-- on H4:1 can be discharged only by the sealed terminal predecessor, while its
-- immutable H2:1 identity must be carried into successor live-ahead state.
survivorStamp :: StructuralBaseFixture -> StructuralOccurrenceStamp
survivorStamp fixture = survivorStampWith fixture 1 1 0 0

survivorStampWith ::
  StructuralBaseFixture ->
  Word ->
  Word ->
  Word ->
  Word ->
  StructuralOccurrenceStamp
survivorStampWith fixture sequenceIndex retiredDependency h2Dependency h3Dependency =
  let identifier = structuralOccurrenceId fixture.h2 (sequenceNumber sequenceIndex)
      source = checkedValue "survivor occurrence source" (mkNablaId (bytes 0xa2))
      predecessor =
        checkedValue
          "survivor occurrence predecessor"
          ( mkStructuralVersionVector
              fixture.predecessor
              [ (fixture.h4, prefix retiredDependency),
                (fixture.h1, emptyStructuralPrefix),
                (fixture.h2, prefix h2Dependency),
                (fixture.h3, prefix h3Dependency)
              ]
          )
   in checkedValue
        "survivor occurrence stamp"
        ( mkStructuralOccurrenceStamp
            identifier
            predecessor
            ( publicationId
                source
                genesisAuthorityEpoch
                fixture.h2
                (nablaSequence (fromIntegral sequenceIndex))
            )
            (digest 0x92)
            NeutralVertexCarrier
        )

survivorLiveAhead :: StructuralBaseFixture -> StructuralVersionVector
survivorLiveAhead fixture = survivorLiveAheadWith fixture 0 2

survivorLiveAheadWith ::
  StructuralBaseFixture ->
  Word ->
  Word ->
  StructuralVersionVector
survivorLiveAheadWith fixture h2Prefix h3Prefix =
  checkedValue
    "predecessor-generation survivor live-ahead vector"
    ( mkStructuralVersionVector
        fixture.predecessor
        [ (fixture.h4, prefix 1),
          (fixture.h1, emptyStructuralPrefix),
          (fixture.h2, prefix h2Prefix),
          (fixture.h3, prefix h3Prefix)
        ]
    )

acceptAnnouncement ::
  StructuralBaseFixture ->
  TerminalSourceUnionAnnounce ->
  MembershipBaseClosure ->
  Either
    StructuralBaseProblem
    (MembershipBaseClosure, TerminalSourceUnionAcceptance)
acceptAnnouncement fixture announce =
  let union = terminalSourceUnionAnnounceUnion announce
   in acceptStructuralBaseUnionAnnouncement
        (constantDigest fixture)
        (terminalSourceUnionTerminalPredecessorVector union)
        (terminalSourceUnionTerminalControlPrefix union)
        announce

constantDigest ::
  StructuralBaseFixture ->
  StructuralVersionVector ->
  ControlIndex ->
  Either TerminalSourceProblem TopologyOccurrenceDigest
constantDigest fixture _ _ = Right fixture.topologyDigest

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
    (mkStructuralPublicationDigest (bytes tag))

digestForPayload :: ByteString -> StructuralPublicationDigest
digestForPayload payload =
  deriveTerminalStructuralPayloadDigest payload

bytes :: Word -> ByteString
bytes = ByteString.replicate 32 . fromIntegral

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id

singleLocalInventory :: MembershipBaseClosure -> TerminalSourceInventory
singleLocalInventory state = case structuralBaseLocalInventories state of
  [inventory] -> inventory
  _ -> error "single-retirement fixture did not produce one inventory"
