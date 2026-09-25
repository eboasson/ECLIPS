{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CompoundTerminalSourceProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (NeutralVertexCarrier))
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Herald.Graph.Protocol qualified as Protocol
import Eclips.Herald.Graph.TerminalSource
import Eclips.Herald.PeerPublication
import Eclips.Herald.UseCase.Step15StructuralBase qualified as Base
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, (===))

-- The reference enumerates all bounded frontiers and selects the greatest
-- feasible one. Production instead reaches its fixed point by descending from
-- available contiguous prefixes; the implementations do not share a solver.
tests :: TestTree
tests =
  testGroup
    "compound terminal closure"
    [ testCase "a later anchor repairs historical bytes from actual survivor retention" caseHistoricalAnchorRepair,
      testProperty "joint replayable prefixes equal an exhaustive causal reference" propReference,
      testCase "overlapping retirement retains payloads and abandons only the agreement attempt" caseRebase,
      testCase "a missing retired dependency preserves the whole nonreplayable tail" caseDependentGap
    ]

propReference :: [Bool] -> Property
propReference flags =
  let f = fixture
      present i = case drop i (flags <> repeat True) of x : _ -> x; [] -> True
      kept = [occurrence f source n | (i, (source, n)) <- zip [0 ..] [(source, n) | n <- [1 .. 4], source <- [f.c, f.d]], present i]
      union = derive f kept
      observed = (wordPrefix f.c union, wordPrefix f.d union)
      has source n = any ((== structuralOccurrenceId source (sequenceAt n)) . terminalStructuralOccurrenceId) kept
      feasible c d =
        all (has f.c) [1 .. c]
          && all (has f.d) [1 .. d]
          && all (\n -> n <= d) [1 .. c]
          && all (\n -> n - 1 <= c) [1 .. d]
      frontiers = [(c, d) | c <- [0 .. 4], d <- [0 .. 4], feasible c d]
      expected = (maximum (fmap fst frontiers), maximum (fmap snd frontiers))
      roundtrip = decodeTerminalSourceUnionCanonicalBytes (terminalSourceUnionCanonicalBytes union)
   in counterexample (show (kept, union)) ((observed, roundtrip) === (expected, Right union))

caseDependentGap :: Assertion
caseDependentGap = do
  let f = fixture
      kept = [occurrence f f.c 1, occurrence f f.c 2, occurrence f f.d 2]
      union = derive f kept
  assertEqual "no mutually unsupported prefix is replayed" [(f.c, prefix 0), (f.d, prefix 0)] (terminalSourceUnionClosedPrefixes union)
  assertEqual
    "all retained bytes remain separately named"
    (Set.fromList (fmap terminalStructuralOccurrenceId kept))
    (Set.fromList (fmap terminalSourceOccurrenceClaimId (terminalSourceUnionAheadClaims union)))
  assertEqual "control is the final retirement, not the abandoned first attempt" (controlIndex 4) (terminalSourceUnionTerminalControlPrefix union)

caseRebase :: Assertion
caseRebase = do
  let f = fixture
      firstLineage = checked (heraldMembershipLineage (heraldMembershipGenerationId f.g0) (heraldMembershipGenerationId f.g1) f.history)
      first = checked (beginTerminalStructuralCoordinator firstLineage firstLineage f.a f.base [] [] [occurrence f f.c 1])
      extended = checked (extendTerminalStructuralCoordinator f.lineage f.lineage [] [] [occurrence f f.d 1] first)
  assertEqual "anchor is unchanged while the base was unfinished" f.base extended.predecessorBase
  assertEqual "both origins survive in the exact payload archive" 2 (length (terminalSourcePayloadEntries extended.payloads))
  assertEqual "both sources are recollected for the new target" (Set.fromList [f.c, f.d]) (Map.keysSet extended.localInventories)
  assertEqual "old agreement cannot satisfy the new reporter set" Nothing extended.agreedUnion
  assertBool "original inventory provenance remains retained" (all (`Map.member` extended.retainedInventories) (Map.keys first.retainedInventories))
  assertBool "new inventories belong to the new target" (all ((== heraldMembershipGenerationId f.g2) . terminalSourceInventorySuccessorGenerationId) (structuralBaseCoordinatorLocalInventories extended))

caseHistoricalAnchorRepair :: Assertion
caseHistoricalAnchorRepair = do
  let f = fixture
      firstLineage = checked (heraldMembershipLineage (heraldMembershipGenerationId f.g0) (heraldMembershipGenerationId f.g1) f.history)
      nextLineage = checked (heraldMembershipLineage (heraldMembershipGenerationId f.g1) (heraldMembershipGenerationId f.g2) f.history)
      c = simpleOccurrence f.g0 f.c "retained-before-first-retirement"
      d = oldSurvivorOccurrence f 1 "retained-before-first-base"
      initialInventories = [fst (checked (sealTerminalSourceInventory firstLineage firstLineage f.c reporter [] [] [c])) | reporter <- [f.a, f.b, f.d]]
      firstUnion = checked (deriveTerminalSourceUnion firstLineage firstLineage f.base initialInventories (checked (terminalSourcePayloadArchive [c])) (\_ _ -> checked (mkTopologyOccurrenceDigest (bytes 10))))
      firstReady = checked (terminalSourceUnionReady (terminalSourceUnionTerminalPredecessorVector firstUnion) (controlIndex 2) firstUnion)
      firstAcceptance = checked (terminalSourceUnionAcceptance f.g1 f.a firstReady)
      firstEstablished = checked (terminalSourceUnionEstablished f.g1 firstUnion [checked (terminalSourceUnionAcceptance f.g1 reporter firstReady) | reporter <- [f.a, f.b, f.d]])
      firstPredecessor = checked (terminalSourceUnionTopologyPredecessor firstLineage firstUnion)
      firstCut = checked (topologyCut firstPredecessor (topologyFrontier (terminalSourceUnionSuccessorInitialVector firstUnion) (controlIndex 2)) (terminalSourceUnionTerminalTopologyOccurrenceDigest firstUnion))
      firstCutId = deriveTopologyCutId firstCut
      certificate =
        checked
          ( Protocol.topologyCutEstablished
              firstCutId
              firstCut
              [Protocol.topologyCutAcceptance firstCutId (Protocol.structuralAppliedReport reporter (terminalSourceUnionSuccessorInitialVector firstUnion) (controlIndex 2)) | reporter <- [f.a, f.b, f.d]]
          )
      anchor = checked (terminalSourceInstalledPredecessorBase f.g1 firstCut)
      sender = checked (beginTerminalStructuralCoordinator f.lineage nextLineage f.a anchor [certificate] [firstEstablished] [c, d])
      inventory = case structuralBaseCoordinatorLocalInventories sender of [entry] -> entry; _ -> error "expected one later-source inventory"
      receiver = checked (beginTerminalStructuralCoordinator f.lineage f.lineage f.b f.base [] [] [])
      retainedReceiver = checked (Base.retainStructuralBaseInventoryEvidence f.lineage inventory receiver)
      requests = checked (deriveTerminalSourceEvidencePayloadRequests f.lineage inventory receiver.payloads)
      relay archive request =
        let retainedOccurrence = case lookupTerminalSourcePayload (terminalSourcePayloadRequestOccurrence request) sender.payloads of
              Just payload -> payload
              Nothing -> error "current reporter did not retain advertised bytes"
            offered = checked (terminalSourceEvidencePayloadRelay f.lineage f.a request inventory retainedOccurrence)
         in checked (acceptTerminalSourceEvidencePayloadRelay f.lineage request inventory offered archive)
      repaired = foldl relay receiver.payloads requests
      repairOwner owner request =
        let offered = checked (Base.prepareStructuralBaseEvidencePayloadRelay f.lineage inventory request sender)
         in checked (Base.receiveStructuralBaseEvidencePayloadRelay f.lineage inventory request offered owner)
      repairedOwner = foldl repairOwner retainedReceiver requests
      laterUnion retained =
        let survivors = [checked (beginTerminalStructuralCoordinator f.lineage nextLineage reporter anchor [certificate] [firstEstablished] [c, retained]) | reporter <- [f.a, f.b]]
            offeredInventories = concatMap structuralBaseCoordinatorLocalInventories survivors
         in checked (deriveTerminalSourceUnion f.lineage nextLineage anchor offeredInventories (checked (terminalSourcePayloadArchive [c, retained])) (\_ _ -> checked (mkTopologyOccurrenceDigest (bytes 11))))
      replayable = laterUnion d
      unsupported = oldSurvivorOccurrence f 2 "old-stamp-with-unsealed-dependency"
      preservedTail = laterUnion unsupported
      provenanceOnly = checked (retainTerminalSourceInventoryEvidence initialInventories [d] inventory)
      provenanceRequests = checked (deriveTerminalSourceEvidencePayloadRequests f.lineage provenanceOnly emptyTerminalSourcePayloadArchive)
  assertEqual "later anchor inventory carries complete terminal-base certificates" [firstEstablished] (terminalSourceInventoryEstablishedBases inventory)
  assertEqual
    "a claimed acceptance cannot erase the nonempty retired-source coordinate"
    (Left TerminalSourceAcceptanceCoordinateMismatch)
    (terminalSourceUnionAcceptanceFromClaimedCoordinates (heraldMembershipGenerationId f.g0) (heraldMembershipGenerationId f.g1) Set.empty (terminalSourceUnionDigest firstUnion) f.a (terminalSourceUnionAcceptanceDigest firstAcceptance))
  assertEqual "retaining a repair supporter does not seal the current matrix" receiver.inventories retainedReceiver.inventories
  assertEqual "repair needs no further requests once every payload arrived" (Right []) (Base.structuralBasePayloadRequestsForInventory f.lineage inventory repairedOwner)
  assertEqual "completed repair still retains its supporter for in-flight replies" (Just inventory) (lookup (terminalSourceInventoryDigest inventory) (Base.structuralBaseRetainedInventoryEntries repairedOwner))
  assertEqual "an exact duplicate relay is still admitted after repair completes" repairedOwner (foldl repairOwner repairedOwner requests)
  assertEqual "actual retained payloads repair both historical generations" (Set.fromList [terminalStructuralOccurrenceId c, terminalStructuralOccurrenceId d]) (Set.fromList (fmap fst (terminalSourcePayloadEntries repaired)))
  assertEqual "an original G0 survivor stamp closes at a G1 anchor using its proved C prefix" [(f.d, prefix 1)] (terminalSourceUnionClosedPrefixes replayable)
  assertEqual "an old dependency beyond the earlier sealed prefix stays nonreplayable" [(f.d, prefix 0)] (terminalSourceUnionClosedPrefixes preservedTail)
  assertEqual "the unsupported old stamp remains in retained tail evidence" [terminalStructuralOccurrenceId unsupported] (fmap terminalSourceOccurrenceClaimId (terminalSourceUnionAheadClaims preservedTail))
  assertEqual "current inventory and full evidence roundtrip canonically" (Right inventory) (decodeTerminalSourceInventoryCanonicalBytes (terminalSourceInventoryCanonicalBytes inventory))
  assertEqual "old provenance alone does not advertise missing bytes" [terminalStructuralOccurrenceId d] (fmap terminalSourcePayloadRequestOccurrence provenanceRequests)

simpleOccurrence :: HeraldMembershipGeneration -> HeraldEpoch -> ByteString.ByteString -> TerminalStructuralOccurrence
simpleOccurrence generation source payload =
  let publication = publicationId (checked (mkNablaId (bytes 30))) genesisAuthorityEpoch source (nablaSequence 1)
      stamp = checked (mkStructuralOccurrenceStamp (structuralOccurrenceId source (sequenceAt 1)) (emptyStructuralVersionVector generation) publication (deriveTerminalStructuralPayloadDigest payload) NeutralVertexCarrier)
   in checked (terminalStructuralOccurrence stamp payload)

oldSurvivorOccurrence :: Fixture -> Word -> ByteString.ByteString -> TerminalStructuralOccurrence
oldSurvivorOccurrence f dependency payload =
  let predecessor = checked (mkStructuralVersionVector f.g0 [(f.a, prefix 0), (f.b, prefix 0), (f.c, prefix dependency), (f.d, prefix 0)])
      publication = publicationId (checked (mkNablaId (bytes 31))) genesisAuthorityEpoch f.d (nablaSequence 1)
      stamp = checked (mkStructuralOccurrenceStamp (structuralOccurrenceId f.d (sequenceAt 1)) predecessor publication (deriveTerminalStructuralPayloadDigest payload) NeutralVertexCarrier)
   in checked (terminalStructuralOccurrence stamp payload)

data Fixture = Fixture
  { a :: HeraldEpoch,
    b :: HeraldEpoch,
    c :: HeraldEpoch,
    d :: HeraldEpoch,
    g0 :: HeraldMembershipGeneration,
    g1 :: HeraldMembershipGeneration,
    g2 :: HeraldMembershipGeneration,
    history :: HeraldMembershipHistory,
    lineage :: HeraldMembershipLineage,
    base :: TerminalSourcePredecessorBase
  }

fixture :: Fixture
fixture =
  let a = epoch 1
      b = epoch 2
      c = epoch 3
      d = epoch 4
      g0 = checked (genesisHeraldMembershipGeneration (checked (mkSystemId (bytes 5))) (a :| [b, c, d]))
      g1 = checked (retireHeraldMembershipGeneration (controlIndex 2) (deriveFailureProbeResolutionId (checked (deriveHeraldFailureProbeId (controlIndex 1))) RetireFailureProbeTarget) c g0)
      g2 = checked (retireHeraldMembershipGeneration (controlIndex 4) (deriveFailureProbeResolutionId (checked (deriveHeraldFailureProbeId (controlIndex 3))) RetireFailureProbeTarget) d g1)
      history = checked (heraldMembershipHistory (g0 :| [g1, g2]))
      lineage = checked (heraldMembershipLineage (heraldMembershipGenerationId g0) (heraldMembershipGenerationId g2) history)
      base = checked (terminalSourceGenesisPredecessorBase g0 (checked (mkTopologyCutId (bytes 8))))
   in Fixture a b c d g0 g1 g2 history lineage base

occurrence :: Fixture -> HeraldEpoch -> Word -> TerminalStructuralOccurrence
occurrence f source n =
  let payload = ByteString.pack [fromIntegral n, if source == f.c then 3 else 4]
      predecessor =
        checked
          ( mkStructuralVersionVector
              f.g0
              [(f.a, prefix 0), (f.b, prefix 0), (f.c, prefix (if source == f.c then n - 1 else n - 1)), (f.d, prefix (if source == f.c then n else n - 1))]
          )
      publication = publicationId (checked (mkNablaId (bytes (20 + n)))) genesisAuthorityEpoch source (nablaSequence (fromIntegral n))
      stamp = checked (mkStructuralOccurrenceStamp (structuralOccurrenceId source (sequenceAt n)) predecessor publication (deriveTerminalStructuralPayloadDigest payload) NeutralVertexCarrier)
   in checked (terminalStructuralOccurrence stamp payload)

derive :: Fixture -> [TerminalStructuralOccurrence] -> TerminalSourceUnion
derive f kept =
  let archive = checked (terminalSourcePayloadArchive kept)
      inventories = [fst (checked (sealTerminalSourceInventory f.lineage f.lineage origin reporter [] [] (filter ((== origin) . structuralOccurrenceSourceHeraldEpoch . terminalStructuralOccurrenceId) kept))) | origin <- [f.c, f.d], reporter <- [f.a, f.b]]
   in checked (deriveTerminalSourceUnion f.lineage f.lineage f.base inventories archive (\_ _ -> checked (mkTopologyOccurrenceDigest (bytes 9))))

wordPrefix :: HeraldEpoch -> TerminalSourceUnion -> Word
wordPrefix source union = maybe 0 (maybe 0 (fromIntegral . structuralSequenceWord64) . structuralPrefixSequence) (lookup source (terminalSourceUnionClosedPrefixes union))

prefix :: Word -> StructuralPrefix
prefix 0 = emptyStructuralPrefix
prefix n = structuralPrefixThrough (sequenceAt n)

sequenceAt :: Word -> StructuralSequence
sequenceAt = checked . mkStructuralSequence . fromIntegral

epoch :: Word -> HeraldEpoch
epoch = checked . mkHeraldEpoch . bytes

bytes :: Word -> ByteString.ByteString
bytes = ByteString.replicate 32 . fromIntegral

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
