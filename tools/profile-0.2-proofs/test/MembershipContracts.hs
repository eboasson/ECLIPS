module MembershipContracts (tests) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

-- This is a test-only contract model, not a Herald implementation. Members and
-- sequence positions are small enumeration coordinates; no production capacity
-- or counter limit follows from the finite generated examples.
data Member = A | B | C | D
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data OccurrenceId = OccurrenceId Member Int
  deriving stock (Eq, Ord, Show)

data Occurrence = Occurrence OccurrenceId (Set OccurrenceId)
  deriving stock (Eq, Ord, Show)

type Inventory = Map OccurrenceId Occurrence
type Prefix = Map Member Int
type Work = Map Int (Set OccurrenceId)

-- A report seals an actual holder's complete inventory for one target lineage.
-- Retaining a report from an earlier lineage is not possession of its payloads.
data Report = Report [Member] Member Inventory
  deriving stock (Eq, Show)

data Attempt = Attempt
  { anchorMembers :: Set Member,
    anchorPrefix :: Prefix,
    retirements :: [Member],
    currentReports :: Map Member Report,
    previousReports :: [Report],
    acceptedWork :: Work,
    completedWork :: Set Int
  }
  deriving stock (Eq, Show)

data ReportDisposition = Accepted | EqualRetry | Stale | Conflicting
  deriving stock (Eq, Show)

data Closure = Closure
  { terminalPrefix :: Prefix,
    replayable :: Set OccurrenceId,
    retainedAhead :: Set OccurrenceId
  }
  deriving stock (Eq, Show)

allMembers :: Set Member
allMembers = Set.fromList [A, B, C, D]

emptyAnchor :: Prefix
emptyAnchor = Map.fromList [(member, 0) | member <- Set.toList allMembers]

initialAttempt :: Prefix -> Work -> Attempt
initialAttempt base work = Attempt allMembers base [] Map.empty [] work Set.empty

activeMembers :: Attempt -> Set Member
activeMembers attempt = anchorMembers attempt Set.\\ Set.fromList (retirements attempt)

retire :: Member -> Attempt -> Maybe Attempt
retire member attempt
  | member `Set.notMember` activeMembers attempt = Nothing
  | Set.size (activeMembers attempt) == 1 = Nothing
  | otherwise =
      Just
        attempt
          { retirements = retirements attempt <> [member],
            currentReports = Map.empty,
            previousReports = previousReports attempt <> Map.elems (currentReports attempt)
          }

observeReport :: Report -> Attempt -> (Attempt, ReportDisposition)
observeReport report@(Report lineage reporter _) attempt
  | lineage /= retirements attempt = (attempt, Stale)
  | reporter `Set.notMember` activeMembers attempt = (attempt, Stale)
  | otherwise = case Map.lookup reporter (currentReports attempt) of
      Nothing ->
        (attempt {currentReports = Map.insert reporter report (currentReports attempt)}, Accepted)
      Just old
        | old == report -> (attempt, EqualRetry)
        | otherwise -> (attempt, Conflicting)

observeAll :: [Report] -> Attempt -> Attempt
observeAll reports attempt = foldl' (\state report -> fst (observeReport report state)) attempt reports

reportInventory :: Report -> Inventory
reportInventory (Report _ _ inventory) = inventory

currentInventory :: Attempt -> Inventory
currentInventory = Map.unions . fmap reportInventory . Map.elems . currentReports

hasAllCurrentReports :: Attempt -> Bool
hasAllCurrentReports attempt = Map.keysSet (currentReports attempt) == activeMembers attempt

position :: Member -> Prefix -> Int
position member = Map.findWithDefault 0 member

covered :: Prefix -> OccurrenceId -> Bool
covered prefix (OccurrenceId member sequenceNumber) = sequenceNumber <= position member prefix

occurrenceId :: Occurrence -> OccurrenceId
occurrenceId (Occurrence identity _) = identity

dependencies :: Occurrence -> Set OccurrenceId
dependencies (Occurrence _ required) = required

inventoryOf :: [Occurrence] -> Inventory
inventoryOf values = Map.fromList [(occurrenceId value, value) | value <- values]

-- Candidate algorithm: find the greatest causally supported prefixes across
-- every source, including the repair histories available from surviving owners.
-- Keep maximal terminal prefixes, then extend survivor coordinates only as far
-- as those terminal prefixes and their transitive prerequisites require.
-- The independent oracle below enumerates every feasible vector instead.
greatestClosure :: Prefix -> Set Member -> Inventory -> Closure
greatestClosure base retiredSources inventory = classify (leastRequired seed)
  where
    sources = Map.keys base
    initial = foldl' extend base sources
    extend vector member = Map.insert member (contiguous member (position member base)) vector
    contiguous member current
      | Map.member (OccurrenceId member (current + 1)) inventory = contiguous member (current + 1)
      | otherwise = current
    fixed = stabilize initial
    stabilize vector =
      let next = foldl' (reduce vector) vector sources
       in if next == vector then vector else stabilize next
    reduce previous vector member =
      let candidates = [position member base + 1 .. position member previous]
          valid sequenceNumber =
            maybe
              False
              (all (covered previous) . dependencies)
              (Map.lookup (OccurrenceId member sequenceNumber) inventory)
          kept = takeWhile valid candidates
       in Map.insert member (position member base + length kept) vector
    seed = Map.mapWithKey (\member n -> if member `Set.member` retiredSources then position member fixed else n) base
    leastRequired vector =
      let required = [value | (key, value) <- Map.toList inventory, covered vector key]
          next = foldl' addDependencies vector required
       in if next == vector then vector else leastRequired next
    addDependencies vector value = foldl' extendRequired vector (dependencies value)
    extendRequired vector (OccurrenceId member n) = Map.insertWith max member n vector
    classify prefix =
      let included = Set.filter (covered prefix) (Map.keysSet inventory)
       in Closure prefix included (Map.keysSet inventory Set.\\ included)

establish :: Attempt -> (Attempt, Maybe Closure, Set Int)
establish attempt
  | not (hasAllCurrentReports attempt) = (attempt, Nothing, Set.empty)
  | otherwise =
      let closure = greatestClosure (anchorPrefix attempt) (Set.fromList (retirements attempt)) (currentInventory attempt)
          satisfied = Map.keysSet (Map.filter (all (covered (terminalPrefix closure))) (acceptedWork attempt))
          emitted = satisfied Set.\\ completedWork attempt
       in (attempt {completedWork = completedWork attempt `Set.union` emitted}, Just closure, emitted)

-- Deliberately faulty policy used by the concrete mutation witness: retain the
-- first successor's reporter barrier even after a second retirement. Executing
-- this policy strands the same complete current-survivor schedule below.
establishWithFirstSuccessorBarrier :: Attempt -> (Attempt, Maybe Closure, Set Int)
establishWithFirstSuccessorBarrier attempt
  | Map.keysSet (currentReports attempt) /= obsoleteRequired = (attempt, Nothing, Set.empty)
  | otherwise = establish attempt
  where
    obsoleteRequired = case retirements attempt of
      [] -> anchorMembers attempt
      first : _ -> Set.delete first (anchorMembers attempt)

-- Independent specification: enumerate every source coordinate, including
-- survivors. Among feasible vectors maximize terminal coordinates; among all
-- vectors with those maxima choose the least survivor extension.
feasibleVectors :: Prefix -> Inventory -> [Prefix]
feasibleVectors base inventory = filter feasible (enumerate (Map.keys base) base)
  where
    upper member = foldl' max (position member base) [n | OccurrenceId source n <- Map.keys inventory, source == member]
    enumerate [] vector = [vector]
    enumerate (member : rest) vector =
      concat [enumerate rest (Map.insert member n vector) | n <- [position member base .. upper member]]
    feasible vector =
      and
        [ case Map.lookup (OccurrenceId member n) inventory of
            Nothing -> False
            Just value -> all (covered vector) (dependencies value)
        | member <- Map.keys base,
          n <- [position member base + 1 .. position member vector]
        ]

oracleGreatest :: Prefix -> Set Member -> Inventory -> Prefix
oracleGreatest base sources inventory = foldl' (Map.unionWith min) upper maximalTerminal
  where
    feasible = feasibleVectors base inventory
    upper = foldl' (Map.unionWith max) base feasible
    maximalTerminal = filter (\vector -> all (\member -> position member vector == position member upper) sources) feasible

terminalSources :: Set Member
terminalSources = Set.fromList [C, D]

-- Partial accessors are avoided even in the contract. Failure to admit the
-- expected retirements makes the property fail, rather than throwing an error.
secondAttempt :: Prefix -> Work -> Inventory -> Maybe Attempt
secondAttempt base work inherited = do
  first <- retire D (initialAttempt base work)
  let reported = fst (observeReport (Report [D] C inherited) first)
  retire C reported

tests :: TestTree
tests =
  testGroup
    "P00 membership closure contracts"
    [ testCase "second retirement supersedes unfinished first base and settles from current survivors" caseSecondRetirement,
      testCase "an old reporter's digest does not manufacture a surviving payload" caseRetiredReporter,
      testCase "terminal closure repairs a survivor prerequisite beyond the anchor without taking its unrelated tail" caseSurvivorRepair,
      QC.testProperty "fixed point equals independent exhaustive feasible-prefix oracle" propGreatest,
      QC.testProperty "closure preserves every retained ahead occurrence and never fabricates a payload" propPartition,
      QC.testProperty "report reorder and equal retries preserve closure and emit settlement once" propSchedules,
      QC.testProperty "retained relay cannot change source identity or remove existing evidence" propRelay
    ]

c1, c2, c3, d1, d2, d3, d4 :: Occurrence
c1 = Occurrence (OccurrenceId C 1) Set.empty
d1 = Occurrence (OccurrenceId D 1) Set.empty
d2 = Occurrence (OccurrenceId D 2) (Set.singleton (occurrenceId d1))
c2 = Occurrence (OccurrenceId C 2) (Set.fromList [occurrenceId c1, occurrenceId d2])
d3 = Occurrence (OccurrenceId D 3) (Set.singleton (occurrenceId d2))
c3 = Occurrence (OccurrenceId C 3) (Set.fromList [occurrenceId c2, occurrenceId d3])
d4 = Occurrence (OccurrenceId D 4) (Set.singleton (occurrenceId d3))

fixtureWork :: Work
fixtureWork = Map.fromList [(1, Set.fromList [occurrenceId c2, occurrenceId d2]), (2, Set.singleton (occurrenceId c3))]

fixtureA, fixtureB, fixtureC :: Inventory
fixtureA = inventoryOf [c1, d1, c2, c3, d4]
fixtureB = inventoryOf [c1, d1]
fixtureC = inventoryOf [d2, d3]

caseSecondRetirement :: IO ()
caseSecondRetirement = case retire D (initialAttempt emptyAnchor fixtureWork) of
  Nothing -> assertBool "first retirement must admit" False
  Just first -> do
    let firstReported = observeAll [Report [D] A fixtureA, Report [D] C fixtureC] first
        (_, firstClosure, _) = establish firstReported
    assertEqual "B has not reported: the first base remains unfinished" Nothing firstClosure
    case retire C firstReported of
      Nothing -> assertBool "second retirement must admit before first closure" False
      Just second -> do
        assertEqual "second attempt requires fresh evidence" Map.empty (currentReports second)
        let oldReport = Report [D] C fixtureC
            (unchanged, disposition) = observeReport oldReport second
        assertEqual "old reporter/attempt cannot discharge a current barrier" Stale disposition
        assertEqual "stale evidence is an exact state no-op" second unchanged
        -- B received the complete D:2 payload before C failed; D:3 was never
        -- copied to a surviving owner. A retains C:3 and D:4 ahead of that gap.
        let afterRelay = Map.insert (occurrenceId d2) d2 fixtureB
            ready = observeAll [Report [D, C] B afterRelay, Report [D, C] A fixtureA] second
            (installed, closure, emitted) = establish ready
        assertBool "current-survivor closure makes progress despite missing old C report" (hasAllCurrentReports ready)
        assertEqual "only fully covered accepted work settles" (Set.singleton 1) emitted
        let (_, strandedClosure, strandedEffects) = establishWithFirstSuccessorBarrier ready
        assertEqual "executing the old first-successor barrier strands establishment" Nothing strandedClosure
        assertEqual "the faulty policy emits no completion for covered work" Set.empty strandedEffects
        assertBool "the schedule distinguishes actual establishment observations" (closure /= strandedClosure)
        assertBool "the schedule distinguishes actual settlement observations" (emitted /= strandedEffects)
        case closure of
          Nothing -> assertBool "second base must establish" False
          Just value -> do
            assertEqual "joint closure covers exactly C:2 and D:2" (Map.fromList [(A, 0), (B, 0), (C, 2), (D, 2)]) (terminalPrefix value)
            assertEqual "ahead evidence is retained, not described as lost" (Set.fromList [occurrenceId c3, occurrenceId d4]) (retainedAhead value)
        let (_, repeatedClosure, repeatedEffects) = establish installed
        assertEqual "repeated establishment retains the exact closure" closure repeatedClosure
        assertEqual "repeated settlement emits no duplicate completion" Set.empty repeatedEffects

caseRetiredReporter :: IO ()
caseRetiredReporter = case retire D (initialAttempt emptyAnchor fixtureWork) of
  Nothing -> assertBool "first retirement must admit" False
  Just first ->
    let reported = observeAll [Report [D] C fixtureC] first
     in case retire C reported of
          Nothing -> assertBool "second retirement must admit" False
          Just second -> do
            let ready = observeAll [Report [D, C] A fixtureA, Report [D, C] B fixtureB] second
                correct = greatestClosure emptyAnchor terminalSources (currentInventory ready)
                -- This intentionally wrong evidence policy demonstrates the
                -- concrete schedule's power to detect stale-reporter reuse.
                advertisedOnly = Map.unions (currentInventory ready : fmap reportInventory (previousReports ready))
                fabricated = greatestClosure emptyAnchor terminalSources advertisedOnly
            assertEqual "without the relay the real surviving D prefix stops at one" 1 (position D (terminalPrefix correct))
            assertEqual "old reports remain reference history" [Report [D] C fixtureC] (previousReports ready)
            assertBool "counting a retired holder invents a larger closure" (terminalPrefix fabricated /= terminalPrefix correct)
            assertBool "D:3 has no surviving holder" (Map.notMember (occurrenceId d3) (currentInventory ready))

caseSurvivorRepair :: IO ()
caseSurvivorRepair = do
  let a1 = Occurrence (OccurrenceId A 1) Set.empty
      a2 = Occurrence (OccurrenceId A 2) (Set.singleton (occurrenceId a1))
      retired = Occurrence (OccurrenceId D 1) (Set.singleton (occurrenceId a1))
      inventory = inventoryOf [a1, a2, retired]
      closure = greatestClosure emptyAnchor terminalSources inventory
      expected = Map.fromList [(A, 1), (B, 0), (C, 0), (D, 1)]
  assertEqual "available A:1 repairs D:1 despite anchor A:0" expected (terminalPrefix closure)
  assertEqual "independent all-source oracle finds the same least extension" expected (oracleGreatest emptyAnchor terminalSources inventory)
  assertEqual "unneeded A:2 is retained without extending the common base" (Set.singleton (occurrenceId a2)) (retainedAhead closure)
  case secondAttempt emptyAnchor (Map.singleton 1 (Set.singleton (occurrenceId retired))) Map.empty of
    Nothing -> assertBool "two-retirement setup must admit" False
    Just second -> do
      let ready = observeAll [Report [D, C] A inventory, Report [D, C] B Map.empty] second
          (_, established, emitted) = establish ready
      assertEqual "repair establishes the exact closure" (Just closure) established
      assertEqual "the accepted work settles after survivor repair" (Set.singleton 1) emitted

data Scenario = Scenario Prefix Inventory Inventory Inventory
  deriving stock (Show)

genScenario :: QC.Gen Scenario
genScenario = do
  -- Interleaving by position makes generated cross-source dependencies acyclic.
  -- A/B contribute their actual complete source histories and prerequisites;
  -- remotely retained C/D material can still have gaps and ahead occurrences.
  depth <- QC.chooseInt (1, 3)
  aDepth <- QC.chooseInt (0, depth)
  bDepth <- QC.chooseInt (0, depth)
  let sourceDepth source = case source of
        A -> aDepth
        B -> bDepth
        C -> depth
        D -> depth
      keys = [OccurrenceId source n | n <- [1 .. depth], source <- [A, B, C, D], n <= sourceDepth source]
  values <- traverse (generateOccurrence keys) keys
  anchored <- QC.arbitrary
  let baseCount = if anchored then 1 else 0
      base = Map.fromList [(source, min baseCount (sourceDepth source)) | source <- [A, B, C, D]]
      anchorValues = filter (covered base . occurrenceId) values
      tails = filter terminalTail values
      terminalTail value = case occurrenceId value of
        OccurrenceId source _ -> source `Set.member` terminalSources && not (covered base (occurrenceId value))
      available = inventoryOf values
      repaired source = repairClosure available (Set.fromList [key | key@(OccurrenceId author _) <- keys, author == source])
  aTail <- QC.sublistOf tails
  bTail <- QC.sublistOf tails
  oldTail <- QC.sublistOf tails
  pure
    ( Scenario
        base
        (Map.union (repaired A) (inventoryOf (anchorValues <> aTail)))
        (Map.union (repaired B) (inventoryOf (anchorValues <> bTail)))
        (inventoryOf (anchorValues <> oldTail))
    )
  where
    generateOccurrence keys identity = do
      selected <- QC.sublistOf (takeWhile (/= identity) keys)
      pure (Occurrence identity (Set.fromList selected))

-- Generator-side repair materialization models what a surviving source can
-- supply from its own retained history. It includes the complete predecessor
-- prefixes and recursively held dependencies of every authored occurrence.
repairClosure :: Inventory -> Set OccurrenceId -> Inventory
repairClosure available initial = Map.restrictKeys available (expand initial)
  where
    expand required =
      let prefixKeys = Set.fromList [OccurrenceId source n | OccurrenceId source lastRequired <- Set.toList required, n <- [1 .. lastRequired]]
          retained = Map.restrictKeys available prefixKeys
          next = Set.unions (prefixKeys : fmap dependencies (Map.elems retained))
       in if next == required then required else expand next

propGreatest :: QC.Property
propGreatest = QC.forAll genScenario $ \(Scenario base a b _) ->
  let inventory = Map.union a b
      got = terminalPrefix (greatestClosure base terminalSources inventory)
      feasible = feasibleVectors base inventory
      expected = oracleGreatest base terminalSources inventory
      dominates candidate = and [position member candidate <= position member got | member <- Set.toList terminalSources]
      sameTerminal candidate = all (\member -> position member candidate == position member got) terminalSources
      leastSurvivor candidate = all (\member -> position member got <= position member candidate) (allMembers Set.\\ terminalSources)
   in QC.counterexample (show (inventory, feasible))
        $ QC.conjoin
          [ got QC.=== expected,
            QC.property (got `elem` feasible),
            QC.property (all dominates feasible),
            QC.property (all leastSurvivor (filter sameTerminal feasible))
          ]

propPartition :: QC.Property
propPartition = QC.forAll genScenario $ \(Scenario base a b _) ->
  let inventory = Map.union a b
      closure = greatestClosure base terminalSources inventory
   in QC.conjoin
        [ (replayable closure `Set.union` retainedAhead closure) QC.=== Map.keysSet inventory,
          (replayable closure `Set.intersection` retainedAhead closure) QC.=== Set.empty,
          QC.property (all (covered (terminalPrefix closure)) (replayable closure))
        ]

propSchedules :: QC.Property
propSchedules = QC.forAll genScenario $ \(Scenario base a b old) ->
  QC.forAll (QC.shuffle [Report [D, C] A a, Report [D, C] B b, Report [D, C] A a, Report [D] C old, Report [D, C] C old]) $ \schedule ->
    let inventory = Map.union a b
        work = Map.fromList (zip [1 ..] (fmap Set.singleton (Map.keys inventory)))
     in case secondAttempt base work old of
          Nothing -> QC.counterexample "checked two-retirement setup failed" False
          Just initial ->
            let step (state, emitted) report =
                  let afterReport = fst (observeReport report state)
                      (afterEstablish, _, newlyEmitted) = establish afterReport
                   in (afterEstablish, emitted <> Set.toList newlyEmitted)
                (final, effects) = foldl' step (initial, []) schedule
                expectedClosure = greatestClosure base terminalSources inventory
                expected = Map.keysSet (Map.filter (all (covered (terminalPrefix expectedClosure))) work)
             in QC.conjoin
                  [ QC.property (hasAllCurrentReports final),
                    currentInventory final QC.=== inventory,
                    previousReports final QC.=== [Report [D] C old],
                    completedWork final QC.=== expected,
                    Set.fromList effects QC.=== expected,
                    length effects QC.=== Set.size expected
                  ]

propRelay :: QC.Property
propRelay = QC.forAll genScenario $ \(Scenario base a b old) ->
  QC.forAll (QC.sublistOf (Map.elems old)) $ \copied ->
    let before = Map.union a b
        after = Map.unions [a, b, inventoryOf copied]
        beforeClosure = greatestClosure base terminalSources before
        afterClosure = greatestClosure base terminalSources after
     in QC.conjoin
          [ QC.property (Map.keysSet before `Set.isSubsetOf` Map.keysSet after),
            QC.property (all (\(key, value) -> Map.lookup key after == Just value) (Map.toList before)),
            QC.property (all (\member -> position member (terminalPrefix beforeClosure) <= position member (terminalPrefix afterClosure)) (Set.toList terminalSources)),
            (replayable afterClosure `Set.union` retainedAhead afterClosure) QC.=== Map.keysSet after
          ]
