-- | Independent finite P00 contracts, not implementations of startup or join.
-- Each transition model is checked against an observable event-history oracle.
-- The intentionally wrong policies demonstrate that the named schedules expose
-- lost receipts and stale join cuts before production owners are extended.
module LifecycleContracts (tests) where

import Data.List (find, permutations)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "lifecycle proof contracts"
    [ attachmentTests,
      joinTests
    ]

-- The child, its private map, and readiness already exist at the start of this
-- model. Physical delivery is independent of atomic logical attachment. Session
-- IDs use unbounded mathematical integers; no exhaustion behavior is modeled.
newtype ChildId = ChildId Integer deriving (Eq, Show)
newtype SessionId = SessionId Integer deriving (Eq, Show)
newtype PrivateId = PrivateId Integer deriving (Eq, Show)
data ClaimId = ClaimA | ClaimB deriving (Eq, Show)
data Delivery = Arrives | Lost deriving (Eq, Show)
data StartupAccess = StartupAccess PrivateId (Map String PrivateId)
  deriving (Eq, Show)
data Attachment = Attachment ChildId SessionId StartupAccess
  deriving (Eq, Show)
data ClaimEvent = InitialClaim ClaimId Delivery | ProcessEnd
  deriving (Eq, Show)
data ClaimResult = Opened Attachment | ClaimedElsewhere | ChildEnded
  deriving (Eq, Show)
data ClaimObservation = Allocated Attachment | Delivered ClaimId ClaimResult
  deriving (Eq, Show)

data ReceiptPolicy = RetainReceipt | ForgetOnConsume | ReplayAfterEnd
  deriving (Eq, Show)
data AttachmentState = AttachmentState
  { ended :: Bool,
    consumed :: Bool,
    receipt :: Maybe (ClaimId, Attachment),
    nextSession :: Integer
  }

startupAccess :: StartupAccess
startupAccess =
  StartupAccess
    (PrivateId 0)
    (Map.fromList [("in", PrivateId 1), ("out", PrivateId 2)])

initialAttachment :: AttachmentState
initialAttachment = AttachmentState False False Nothing 1

attachmentStep ::
  ReceiptPolicy ->
  AttachmentState ->
  ClaimEvent ->
  (AttachmentState, [ClaimObservation])
attachmentStep _ state ProcessEnd = (state {ended = True}, [])
attachmentStep policy state (InitialClaim claim delivery)
  | ended state && policy /= ReplayAfterEnd =
      (state, reply ChildEnded)
  | Just (winner, result) <- receipt state =
      (state, reply (if claim == winner then Opened result else ClaimedElsewhere))
  | consumed state = (state, reply ClaimedElsewhere)
  | ended state = (state, reply ChildEnded)
  | otherwise =
      let result = Attachment (ChildId 7) (SessionId (nextSession state)) startupAccess
          retained = if policy == ForgetOnConsume then Nothing else Just (claim, result)
          successor =
            state
              { consumed = True,
                receipt = retained,
                nextSession = nextSession state + 1
              }
       in (successor, Allocated result : reply (Opened result))
  where
    reply result = [Delivered claim result | delivery == Arrives]

attachmentTrace :: ReceiptPolicy -> [ClaimEvent] -> [ClaimObservation]
attachmentTrace policy = observe (attachmentStep policy) initialAttachment

-- This oracle has no allocation state or receipt map. The first claim before
-- End wins by position in the input history, regardless of whether its physical
-- reply arrives. Every later answer is classified relative to that event.
attachmentOracle :: [ClaimEvent] -> [ClaimObservation]
attachmentOracle = concatMap expected . withPast
  where
    result = Attachment (ChildId 7) (SessionId 1) startupAccess
    expected (_, ProcessEnd) = []
    expected (past, InitialClaim claim delivery)
      | ProcessEnd `elem` past = delivered ChildEnded
      | Just winner <- firstClaim past =
          delivered (if claim == winner then Opened result else ClaimedElsewhere)
      | otherwise = Allocated result : delivered (Opened result)
      where
        delivered answer = [Delivered claim answer | delivery == Arrives]
    firstClaim = fmap fst . find (const True) . mapMaybe asClaim
    asClaim (InitialClaim claim delivery) = Just (claim, delivery)
    asClaim ProcessEnd = Nothing

lostFirstReply :: [ClaimEvent]
lostFirstReply = [InitialClaim ClaimA Lost, InitialClaim ClaimA Arrives]

lostReplyThenEnd :: [ClaimEvent]
lostReplyThenEnd =
  [ InitialClaim ClaimA Lost,
    ProcessEnd,
    InitialClaim ClaimA Arrives,
    InitialClaim ClaimB Arrives
  ]

attachmentTests :: TestTree
attachmentTests =
  testGroup
    "lost first child-attachment reply"
    [ testCase "first claim allocates despite reply loss; exact retry recovers all access"
        $ attachmentTrace RetainReceipt lostFirstReply @?= attachmentOracle lostFirstReply,
      testCase "a distinct initial claim cannot consume the preparation twice"
        $ let schedule =
                [ InitialClaim ClaimA Lost,
                  InitialClaim ClaimB Arrives,
                  InitialClaim ClaimA Arrives
                ]
           in attachmentTrace RetainReceipt schedule @?= attachmentOracle schedule,
      testCase "End wins over a retained successful attachment result"
        $ attachmentTrace RetainReceipt lostReplyThenEnd @?= attachmentOracle lostReplyThenEnd,
      testCase "counterexample: forget-on-consume loses the first reply permanently"
        $ distinguishes lostFirstReply attachmentOracle (attachmentTrace ForgetOnConsume),
      testCase "counterexample: replay-after-End exposes access to an ended child"
        $ distinguishes lostReplyThenEnd attachmentOracle (attachmentTrace ReplayAfterEnd),
      testCase "all permutations of two claims, retry, physical loss, and End"
        $ mapM_
          check
          ( permutations
              [ InitialClaim ClaimA Lost,
                InitialClaim ClaimA Arrives,
                InitialClaim ClaimB Arrives,
                ProcessEnd
              ]
          ),
      QC.testProperty "generated histories preserve winner, full result, and terminal precedence"
        $ QC.forAll (QC.listOf claimEvent)
        $ \schedule ->
          attachmentTrace RetainReceipt schedule QC.=== attachmentOracle schedule
    ]
  where
    check schedule = attachmentTrace RetainReceipt schedule @?= attachmentOracle schedule
    claimEvent =
      QC.frequency
        [ ( 4,
            InitialClaim
              <$> QC.elements [ClaimA, ClaimB]
              <*> QC.elements [Arrives, Lost]
          ),
          (1, pure ProcessEnd)
        ]

-- A CaptureCut input abstracts completed, checked old-member seal collection.
-- It does not require applicant readiness. A fresh attempt supersedes an old
-- attempt; repeated attempt IDs cannot recapture a changed cut. Reports name
-- exact evidence. End is the representative semantic-cut-invalidating command.
data OldMember = OldA | OldB deriving (Eq, Ord, Show)
data Attempt = First | Second | Third deriving (Eq, Ord, Show)
newtype SemanticToken = SemanticToken Integer deriving (Eq, Show)
data Candidate = RedBase | BlueBase deriving (Eq, Show)
data Seal = Seal Attempt SemanticToken Candidate deriving (Eq, Show)
data JoinEvent
  = CaptureCut Attempt Candidate
  | OldBaseReady OldMember Seal
  | NewcomerReady Seal
  | EndDuringJoin
  | TryActivate
  deriving (Eq, Show)
newtype JoinObservation = Activated Seal deriving (Eq, Show)
data JoinPolicy = InvalidateOnEnd | KeepStaleSeal | OmitNewcomer
  deriving (Eq, Show)
data JoinState = JoinState
  { token :: SemanticToken,
    attempts :: Set Attempt,
    seal :: Maybe Seal,
    oldReady :: Set OldMember,
    newcomerReady :: Bool,
    activated :: Bool
  }

oldMembers :: Set OldMember
oldMembers = Set.fromList [OldA, OldB]

initialJoin :: JoinState
initialJoin = JoinState (SemanticToken 0) Set.empty Nothing Set.empty False False

joinStep :: JoinPolicy -> JoinState -> JoinEvent -> (JoinState, [JoinObservation])
joinStep _ state _ | activated state = (state, [])
joinStep _ state (CaptureCut attempt candidate)
  | attempt `Set.member` attempts state = (state, [])
  | otherwise =
      ( state
          { attempts = Set.insert attempt (attempts state),
            seal = Just (Seal attempt (token state) candidate),
            oldReady = Set.empty,
            newcomerReady = False
          },
        []
      )
joinStep _ state (OldBaseReady member evidence)
  | seal state == Just evidence =
      (state {oldReady = Set.insert member (oldReady state)}, [])
  | otherwise = (state, [])
joinStep _ state (NewcomerReady evidence)
  | seal state == Just evidence = (state {newcomerReady = True}, [])
  | otherwise = (state, [])
joinStep policy state EndDuringJoin =
  let SemanticToken n = token state
      successor = state {token = SemanticToken (n + 1)}
   in ( if policy == KeepStaleSeal
          then successor
          else
            successor
              { seal = Nothing,
                oldReady = Set.empty,
                newcomerReady = False
              },
        []
      )
joinStep policy state TryActivate
  | Just evidence <- seal state,
    oldReady state == oldMembers,
    newcomerReady state || policy == OmitNewcomer =
      (state {activated = True}, [Activated evidence])
  | otherwise = (state, [])

joinTrace :: JoinPolicy -> [JoinEvent] -> [JoinObservation]
joinTrace policy = observe (joinStep policy) initialJoin

-- The join oracle searches event positions rather than replaying JoinState.
-- Activation must follow the most recent fresh seal capture and every required
-- report, with no intervening End. Its token counts Ends before that capture.
-- The first qualifying activation is the only semantic membership transition.
joinOracle :: [JoinEvent] -> [JoinObservation]
joinOracle = take 1 . mapMaybe activation . withPast
  where
    activation (past, TryActivate) = do
      (before, attempt, candidate, after) <- lastFreshCapture past
      let evidence = Seal attempt (SemanticToken (endCount before)) candidate
          reports =
            Set.fromList
              [member | OldBaseReady member reported <- after, reported == evidence]
      if EndDuringJoin `notElem` after
        && reports == oldMembers
        && NewcomerReady evidence `elem` after
        then Just (Activated evidence)
        else Nothing
    activation _ = Nothing
    endCount = toInteger . length . filter (== EndDuringJoin)
    lastFreshCapture history =
      lastMaybe
        [ (before, attempt, candidate, drop (length before + 1) history)
        | (before, CaptureCut attempt candidate) <- withPast history,
          not (any (sameAttempt attempt) before)
        ]
    sameAttempt expected (CaptureCut actual _) = expected == actual
    sameAttempt _ _ = False

firstSeal :: Seal
firstSeal = Seal First (SemanticToken 0) RedBase

firstReady :: [JoinEvent]
firstReady =
  [ OldBaseReady OldA firstSeal,
    OldBaseReady OldB firstSeal,
    NewcomerReady firstSeal
  ]

staleSealActivation :: [JoinEvent]
staleSealActivation =
  [CaptureCut First RedBase] ++ firstReady ++ [EndDuringJoin, TryActivate]

joinTests :: TestTree
joinTests =
  testGroup
    "join activation racing Process End"
    [ testCase "a closed old-member cut and separate complete readiness allow activation"
        $ let schedule = CaptureCut First RedBase : firstReady ++ [TryActivate]
           in joinTrace InvalidateOnEnd schedule @?= [Activated firstSeal],
      testCase "End after readiness prevents stale activation"
        $ joinTrace InvalidateOnEnd staleSealActivation @?= [],
      testCase "counterexample: retaining a stale seal activates the pre-End base"
        $ distinguishes staleSealActivation joinOracle (joinTrace KeepStaleSeal),
      testCase "counterexample: old-member reports alone do not include the applicant"
        $ let schedule =
                [ CaptureCut First RedBase,
                  OldBaseReady OldA firstSeal,
                  OldBaseReady OldB firstSeal,
                  TryActivate
                ]
           in distinguishes schedule joinOracle (joinTrace OmitNewcomer),
      testCase "a new attempt and complete post-End cut evidence restore progress" $ do
        let secondSeal = Seal Second (SemanticToken 1) BlueBase
            schedule =
              staleSealActivation
                ++ [ CaptureCut Second BlueBase,
                     OldBaseReady OldA firstSeal,
                     OldBaseReady OldB firstSeal,
                     NewcomerReady firstSeal,
                     TryActivate,
                     OldBaseReady OldA secondSeal,
                     NewcomerReady secondSeal,
                     TryActivate,
                     OldBaseReady OldB secondSeal,
                     TryActivate
                   ]
        joinTrace InvalidateOnEnd schedule @?= [Activated secondSeal]
        joinTrace InvalidateOnEnd schedule @?= joinOracle schedule,
      testCase "every report, End, and activation ordering at one seal"
        $ mapM_
          (check . (CaptureCut First RedBase :))
          (permutations (firstReady ++ [EndDuringJoin, TryActivate])),
      QC.testProperty "generated duplicate and out-of-order evidence has one current cut"
        $ QC.forAll (QC.listOf joinEvent)
        $ \schedule ->
          joinTrace InvalidateOnEnd schedule QC.=== joinOracle schedule,
      QC.testProperty "generated interleavings of two attempts settle only complete current evidence"
        $ QC.forAll (QC.shuffle twoAttemptEvents)
        $ \schedule ->
          joinTrace InvalidateOnEnd schedule QC.=== joinOracle schedule
    ]
  where
    check schedule = joinTrace InvalidateOnEnd schedule @?= joinOracle schedule
    evidence =
      Seal
        <$> QC.elements [First, Second, Third]
        <*> (SemanticToken <$> QC.elements [0, 1, 2])
        <*> QC.elements [RedBase, BlueBase]
    joinEvent =
      QC.oneof
        [ CaptureCut
            <$> QC.elements [First, Second, Third]
            <*> QC.elements [RedBase, BlueBase],
          OldBaseReady <$> QC.elements [OldA, OldB] <*> evidence,
          NewcomerReady <$> evidence,
          pure EndDuringJoin,
          pure TryActivate
        ]
    second = Seal Second (SemanticToken 1) BlueBase
    twoAttemptEvents =
      [CaptureCut First RedBase]
        ++ firstReady
        ++ [ EndDuringJoin,
             TryActivate,
             CaptureCut Second BlueBase,
             OldBaseReady OldA second,
             OldBaseReady OldB second,
             NewcomerReady second,
             TryActivate
           ]

observe :: (state -> event -> (state, [output])) -> state -> [event] -> [output]
observe _ _ [] = []
observe step state (event : rest) =
  let (next, output) = step state event
   in output ++ observe step next rest

withPast :: [event] -> [([event], event)]
withPast = go []
  where
    go _ [] = []
    go reversed (event : rest) =
      (reverse reversed, event) : go (event : reversed) rest

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

distinguishes ::
  (Eq output, Show output, Show event) =>
  [event] ->
  ([event] -> output) ->
  ([event] -> output) ->
  Assertion
distinguishes schedule oracle mutant
  | oracle schedule /= mutant schedule = pure ()
  | otherwise =
      assertFailure
        ( "counterexample failed to distinguish policy: "
            ++ show schedule
            ++ "\nshared observation: "
            ++ show (oracle schedule)
        )
