-- | An independent finite model of object-scoped generation, CAS, and exact
-- request replay. This package imports no production ECLIPS implementation.
-- Preparation is invisible; only a successful Release adds a history event.
module LabelGenerationContracts (tests) where

import Data.List (find, permutations)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

data Process = A | B deriving (Eq, Ord, Show)
data Owner = Void | Owned Process | Zombie Process deriving (Eq, Show)
type Label = (Owner, Integer)
data Current = Live Label | Deleted Integer deriving (Eq, Show)
data Target = To Process | ToVoid | ToDelete deriving (Eq, Show)
newtype RequestId = RequestId Int deriving (Eq, Ord, Show)
data Request = Request Process Label Target deriving (Eq, Show)
data Action
  = Prepare RequestId Request
  | Release RequestId
  | Abort RequestId
  | End Process
  | Disappear
  deriving (Eq, Show)
data Result = Applied Current | NotApplied | Rejected | Aborted deriving (Eq, Show)
data Reply = Prepared | Completed Result | Conflict | Missing | Silent deriving (Eq, Show)
data Receipt = Pending Request | Finished Request Result deriving (Eq, Show)
data Policy = Correct | OwnerOnlyCAS | ElideSameOwner | IncrementReplay | IncrementEnd
  deriving (Eq, Show)
data State = State Current (Set Process) (Map RequestId Receipt)

initial :: State
initial = State (Live (Owned A, 0)) Set.empty Map.empty

effective :: Set Process -> Current -> Current
effective ended (Live (Owned process, currentGeneration))
  | Set.member process ended = Live (Zombie process, currentGeneration)
effective _ current = current

generation :: Current -> Integer
generation (Live (_, value)) = value
generation (Deleted value) = value

targetState :: Integer -> Target -> Current
targetState next (To process) = Live (Owned process, next)
targetState next ToVoid = Live (Void, next)
targetState next ToDelete = Deleted next

admitted :: Set Process -> Owner -> Request -> Bool
admitted ended owner (Request caller _ target) =
  Set.notMember caller ended
    && case target of
      To process | Set.member process ended -> False
      _ -> case owner of
        Owned process -> caller == process
        _ -> case target of
          To process -> caller == process
          _ -> True

modelStep :: Policy -> State -> Action -> (State, Reply)
modelStep policy (State stored ended receipts) action = case action of
  Prepare requestId request -> case Map.lookup requestId receipts of
    Just receipt
      | request /= receiptRequest receipt -> (state, Conflict)
    Just (Finished _ result) -> (state, Completed result)
    Just (Pending _) -> (state, Prepared)
    Nothing -> (State stored ended (Map.insert requestId (Pending request) receipts), Prepared)
  Release requestId -> case Map.lookup requestId receipts of
    Nothing -> (state, Missing)
    Just (Finished request result)
      | policy == IncrementReplay,
        Applied _ <- result ->
          let Request _ _ target = request
              next = targetState (generation stored + 1) target
           in (State next ended receipts, Completed (Applied next))
      | otherwise -> (state, Completed result)
    Just (Pending request@(Request _ expected target)) ->
      let result = case current of
            Deleted _ -> Rejected
            Live actual
              | not (matches actual expected) -> NotApplied
              | not (admitted ended (fst actual) request) -> Rejected
              | otherwise ->
                  let unchanged = targetState (snd actual) target == current
                      increment = if policy == ElideSameOwner && unchanged then 0 else 1
                   in Applied (targetState (snd actual + increment) target)
          next = case result of
            Applied value -> value
            _ -> stored
       in (State next ended (Map.insert requestId (Finished request result) receipts), Completed result)
  Abort requestId -> case Map.lookup requestId receipts of
    Nothing -> (state, Missing)
    Just (Finished _ result) -> (state, Completed result)
    Just (Pending request) ->
      (State stored ended (Map.insert requestId (Finished request Aborted) receipts), Completed Aborted)
  End process ->
    let next = case stored of
          Live (Owned owner, value)
            | policy == IncrementEnd && owner == process && Set.notMember process ended ->
                Live (Owned owner, value + 1)
          _ -> stored
     in (State next (Set.insert process ended) receipts, Silent)
  Disappear -> (State (Deleted (generation stored)) ended receipts, Silent)
  where
    state = State stored ended receipts
    current = effective ended stored
    matches actual expected
      | policy == OwnerOnlyCAS = fst actual == fst expected
      | otherwise = actual == expected

receiptRequest :: Receipt -> Request
receiptRequest (Pending request) = request
receiptRequest (Finished request _) = request

-- The oracle has neither a mutable generation nor a receipt map. It derives the
-- current generation by counting successful release events, and answers replay
-- by locating the original request and its first completion in event history.
data Event
  = Started RequestId Request
  | Resolved RequestId Result
  | Ended Process
  | Removed
  deriving (Eq, Show)

historyCurrent :: [Event] -> Current
historyCurrent history
  | Removed `elem` history = Deleted successes
  | otherwise = case latestApplied of
      Just (Resolved _ (Applied (Deleted _))) -> Deleted successes
      Just (Resolved _ (Applied (Live (owner, _)))) -> project owner
      _ -> project (Owned A)
  where
    successes = fromIntegral (length [() | Resolved _ (Applied _) <- history])
    latestApplied = find isApplied (reverse history)
    isApplied (Resolved _ (Applied _)) = True
    isApplied _ = False
    project owner = case owner of
      Owned process | Ended process `elem` history -> Live (Zombie process, successes)
      _ -> Live (owner, successes)

historyStep :: [Event] -> Action -> ([Event], Reply)
historyStep history action = case action of
  Prepare requestId request -> case original requestId of
    Just prior
      | prior /= request -> (history, Conflict)
      | Just result <- completion requestId -> (history, Completed result)
      | otherwise -> (history, Prepared)
    Nothing -> (history <> [Started requestId request], Prepared)
  Release requestId
    | Just result <- completion requestId -> (history, Completed result)
    | Just request@(Request caller expected target) <- original requestId ->
        let result = case historyCurrent history of
              Deleted _ -> Rejected
              Live actual
                | actual /= expected -> NotApplied
                | Ended caller `elem` history -> Rejected
                | To process <- target, Ended process `elem` history -> Rejected
                | Owned owner <- fst actual, caller /= owner -> Rejected
                | fst actual /= Owned caller,
                  To process <- target,
                  process /= caller ->
                    Rejected
                | otherwise ->
                    let next = fromIntegral (length [() | Resolved _ (Applied _) <- history]) + 1
                     in Applied (targetState next (requestTarget request))
         in resolve requestId result
    | otherwise -> (history, Missing)
  Abort requestId
    | Just result <- completion requestId -> (history, Completed result)
    | Just _ <- original requestId -> resolve requestId Aborted
    | otherwise -> (history, Missing)
  End process -> (history <> [Ended process], Silent)
  Disappear -> (history <> [Removed], Silent)
  where
    original requestId =
      case find (isStarted requestId) history of
        Just (Started _ request) -> Just request
        _ -> Nothing
    completion requestId =
      case find (isResolved requestId) history of
        Just (Resolved _ result) -> Just result
        _ -> Nothing
    resolve requestId result = (history <> [Resolved requestId result], Completed result)
    requestTarget (Request _ _ target) = target
    isStarted requestId (Started candidate _) = requestId == candidate
    isStarted _ _ = False
    isResolved requestId (Resolved candidate _) = requestId == candidate
    isResolved _ _ = False

modelTrace :: Policy -> [Action] -> [(Reply, Current)]
modelTrace policy = go initial
  where
    go _ [] = []
    go state (action : rest) =
      let (next@(State current ended _), reply) = modelStep policy state action
       in (reply, effective ended current) : go next rest

historyTrace :: [Action] -> [(Reply, Current)]
historyTrace = go []
  where
    go _ [] = []
    go history (action : rest) =
      let (next, reply) = historyStep history action
       in (reply, historyCurrent next) : go next rest

call :: Int -> Process -> Label -> Target -> [Action]
call requestId caller expected target =
  [Prepare (RequestId requestId) (Request caller expected target), Release (RequestId requestId)]

sameOwner :: [Action]
sameOwner = call 0 A (Owned A, 0) (To A)

aba :: [Action]
aba = call 0 A (Owned A, 0) (To B) <> call 1 B (Owned B, 1) (To A) <> call 2 A (Owned A, 0) ToVoid

tests :: TestTree
tests =
  testGroup
    "object label generation contracts"
    [ testCase "same-owner and void-to-void each release a successor"
        $ check (sameOwner <> call 1 A (Owned A, 1) ToVoid <> call 2 A (Void, 2) ToVoid),
      testCase "ABA rejects a stale expected owner and generation" $ check aba,
      testCase "two prepared callers cannot release the same expected pair twice"
        $ mapM_ check (raceSchedules [Release (RequestId 0), Release (RequestId 1)]),
      testCase "lost reply, reconnect, and committed Release replay retain one exact result"
        $ check (sameOwner <> [Release (RequestId 0), Prepare (RequestId 0) (Request A (Owned A, 0) (To A)), Release (RequestId 0)]),
      testCase "reused request identity with changed generation is a conflict"
        $ check (sameOwner <> [Prepare (RequestId 0) (Request A (Owned A, 1) (To A))]),
      testCase "abort and End before Release leave the generation unchanged"
        $ mapM_
          check
          [ [Prepare (RequestId 0) (Request A (Owned A, 0) ToVoid), Abort (RequestId 0), Release (RequestId 0)],
            [Prepare (RequestId 0) (Request A (Owned A, 0) ToVoid), End A, Release (RequestId 0)]
          ],
      testCase "End preserves generation and zombie adoption advances it"
        $ check (sameOwner <> [End A] <> call 1 B (Zombie A, 1) (To B)),
      testCase "label deletion advances once and remains terminal"
        $ check (call 0 A (Owned A, 0) ToDelete <> [Release (RequestId 0)] <> call 1 A (Owned A, 1) (To A)),
      testCase "automatic disappearance preserves the last generation"
        $ check (sameOwner <> [Disappear] <> call 1 A (Owned A, 1) ToVoid),
      testCase "counterexample: owner-only comparison permits ABA" $ distinguishes OwnerOnlyCAS aba,
      testCase "counterexample: eliding same-owner success loses an operation" $ distinguishes ElideSameOwner sameOwner,
      testCase "counterexample: incrementing during replay applies one request twice" $ distinguishes IncrementReplay (sameOwner <> [Release (RequestId 0)]),
      testCase "counterexample: incrementing on End invents an operation" $ distinguishes IncrementEnd [End A],
      QC.testProperty "generated prepare/release/replay/abort/End histories agree with event counting"
        $ QC.forAll (QC.resize 80 (QC.listOf actionGenerator))
        $ \actions ->
          modelTrace Correct actions QC.=== historyTrace actions
    ]
  where
    check actions = modelTrace Correct actions @?= historyTrace actions
    distinguishes policy actions =
      assertBool
        (show policy <> " must produce a different observation")
        (modelTrace policy actions /= historyTrace actions)
    raceSchedules releases =
      [ [ Prepare (RequestId 0) (Request A (Owned A, 0) (To A)),
          Prepare (RequestId 1) (Request A (Owned A, 0) ToVoid)
        ]
          <> order
      | order <- permutations releases
      ]

actionGenerator :: QC.Gen Action
actionGenerator =
  QC.frequency
    [ (6, Prepare <$> requestId <*> request),
      (4, Release <$> requestId),
      (1, Abort <$> requestId),
      (1, End <$> process),
      (1, pure Disappear)
    ]
  where
    requestId = RequestId <$> QC.chooseInt (0, 7)
    process = QC.elements [A, B]
    owner = QC.elements [Void, Owned A, Owned B, Zombie A, Zombie B]
    request = Request <$> process <*> ((,) <$> owner <*> QC.chooseInteger (0, 5)) <*> QC.elements [To A, To B, ToVoid, ToDelete]
