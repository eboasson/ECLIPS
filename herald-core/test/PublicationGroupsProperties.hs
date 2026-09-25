{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PublicationGroupsProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List (scanl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
    genesisAuthorityEpoch,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    nablaSequence,
    publicationId,
  )
import Eclips.Herald.PeerStream
  ( StreamPrefix,
    StreamSequence,
    emptyStreamPrefix,
    mkStreamSequence,
    streamPrefixThrough,
  )
import Eclips.Herald.Publication.Groups qualified as Groups
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "publication group frontiers"
    [ QC.testProperty "indexed owner agrees with an independent publication-history model" propHistory,
      testCase "one issuer and both predicate arms give at most two memberships" caseMembership,
      testCase "unstamped local work is not settled by an empty destination frontier" caseLocalPending,
      testCase "crossed frontiers cannot erase earlier local work or its later assignments" casePendingInterleaving,
      testCase "a larger frontier replaces its index and prefix progress touches only crossed groups" caseFrontierReplacement,
      testCase "the contiguous prefix conservatively waits behind an unrelated hole" casePrefixOnly,
      testCase "retirement settles one destination without manufacturing completion" caseRetirement,
      testCase "already known progress and repeated quiescent publication cycles leave no invocation history" caseCleanup,
      testCase "fresh duplicate and repeated local settlement are checked contradictions" caseDuplicate
    ]

caseMembership :: Assertion
caseMembership = do
  let same = Groups.membership (process 1) (Just (object 1)) (Just (object 1))
      distinct = Groups.membership (process 1) (Just (object 1)) (Just (object 2))
      none = Groups.membership (process 1) Nothing Nothing
      otherIssuer = Groups.membership (process 2) (Just (object 1)) Nothing
  assertEqual "duplicate predicate arm contributes once" (Set.singleton (key 1 1)) (Groups.membershipKeys same)
  assertEqual "distinct arms contribute twice" (Set.fromList [key 1 1, key 1 2]) (Groups.membershipKeys distinct)
  assertEqual "caller remains part of identity" (Set.singleton (key 2 1)) (Groups.membershipKeys otherIssuer)
  assertEqual "group accessors" (process 1, object 2) (Groups.groupProcess (key 1 2), Groups.groupObject (key 1 2))
  assertEqual "empty membership retains no token" (Right Groups.initialState) (Groups.acceptPublication (publication 1) none True Groups.initialState)

caseLocalPending :: Assertion
caseLocalPending = do
  let members = Groups.membership (process 1) (Just (object 1)) (Just (object 2))
      admitted = checked (Groups.acceptPublication (publication 1) members True Groups.initialState)
      afterProgress = fst (Groups.advanceCompletedPrefix (peer 1) (prefix 20) admitted)
      (settled, changed) = checked (Groups.settleLocalPublication (publication 1) Map.empty afterProgress)
  assertEqual "unstamped stage participates in both groups" [1, 1] [Groups.groupUnstampedCount group admitted | group <- [key 1 1, key 1 2]]
  assertEqual "remote progress cannot settle unassigned work" False (Groups.groupReady (key 1 1) afterProgress)
  assertEqual "local completion notifies both groups" (Set.fromList [key 1 1, key 1 2]) changed
  assertEqual "local-only completion removes both groups" (0, 0, True) (Groups.retainedGroupCount settled, Groups.retainedPublicationCount settled, Groups.valid settled)

casePendingInterleaving :: Assertion
casePendingInterleaving = do
  let group = key 1 1
      members = Groups.membership (process 1) (Just (object 1)) Nothing
      first = checked (Groups.acceptPublication (publication 1) members True Groups.initialState)
      second = publishTo 2 group [(1, 3)] first
      (crossed, changed) = Groups.advanceCompletedPrefix (peer 1) (prefix 3) second
      (later, _) = checked (Groups.settleLocalPublication (publication 1) (Map.singleton (peer 1) (sequenceAt 5)) crossed)
      (done, released) = Groups.advanceCompletedPrefix (peer 1) (prefix 5) later
  assertEqual "completion touches only remote readiness" (1, 1, 0, False) (Groups.groupLocalPendingCount group crossed, Groups.groupUnstampedCount group crossed, Groups.groupPendingPeerCount group crossed, Groups.groupReady group crossed)
  assertEqual "frontier change is still notified" (Set.singleton group) changed
  assertEqual "earlier accepted stage can receive a later assignment" (0, 0, Map.singleton (peer 1) (sequenceAt 5)) (Groups.groupLocalPendingCount group later, Groups.groupUnstampedCount group later, Groups.groupFrontiers group later)
  assertEqual "duplicate old completion cannot release new frontier" (later, Set.empty) (Groups.advanceCompletedPrefix (peer 1) (prefix 3) later)
  assertEqual "last obligation reclaims group" (True, Set.singleton group, True) (Groups.groupReady group done, released, Groups.valid done)

caseFrontierReplacement :: Assertion
caseFrontierReplacement = do
  let at3 = publishTo 1 (key 1 1) [(1, 3)] Groups.initialState
      at7 = publishTo 2 (key 1 1) [(1, 7)] at3
      at5 = publishTo 3 (key 2 1) [(1, 5)] at7
      (through3, change3) = Groups.advanceCompletedPrefix (peer 1) (prefix 3) at5
      (through5, change5) = Groups.advanceCompletedPrefix (peer 1) (prefix 5) through3
      (through7, change7) = Groups.advanceCompletedPrefix (peer 1) (prefix 7) through5
  assertEqual "max frontier replaces earlier sequence" (Map.singleton (peer 1) (sequenceAt 7)) (Groups.groupFrontiers (key 1 1) at7)
  assertEqual "only one frontier per group and destination" 2 (Groups.retainedFrontierCount at5)
  assertEqual "crossing superseded frontier emits no wake" Set.empty change3
  assertEqual "other caller's frontier wakes independently" (Set.singleton (key 2 1)) change5
  assertEqual "larger frontier remains" False (Groups.groupReady (key 1 1) through5)
  assertEqual "last frontier wakes" (Set.singleton (key 1 1)) change7
  assertEqual "all index buckets reclaimed" (0, 0, True) (Groups.retainedGroupCount through7, Groups.retainedFrontierCount through7, Groups.valid through7)

casePrefixOnly :: Assertion
casePrefixOnly = do
  let sent = publishTo 1 (key 1 1) [(1, 2)] Groups.initialState
      -- The peer may have already selectively completed assignment 2. This
      -- owner intentionally sees only the contiguous prefix behind assignment 1.
      (held, changed) = Groups.advanceCompletedPrefix (peer 1) emptyStreamPrefix sent
      (complete, released) = Groups.advanceCompletedPrefix (peer 1) (prefix 2) held
  assertEqual "unchanged empty prefix is inert" (sent, Set.empty) (held, changed)
  assertEqual "completed selected item is conservatively held" False (Groups.groupReady (key 1 1) held)
  assertEqual "hole closure releases it" (True, Set.singleton (key 1 1)) (Groups.groupReady (key 1 1) complete, released)

caseRetirement :: Assertion
caseRetirement = do
  let sent = publishTo 1 (key 1 1) [(1, 3), (2, 4)] Groups.initialState
      (first, changed) = Groups.retirePeer (peer 1) sent
      (done, completed) = Groups.advanceCompletedPrefix (peer 2) (prefix 4) first
  assertEqual "retirement touches its dependent group" (Set.singleton (key 1 1)) changed
  assertEqual "surviving destination still blocks" (1, False) (Groups.groupPendingPeerCount (key 1 1) first, Groups.groupReady (key 1 1) first)
  assertEqual "duplicate retirement emits no wake" (first, Set.empty) (Groups.retirePeer (peer 1) first)
  assertEqual "survivor completion releases group" (True, Set.singleton (key 1 1)) (Groups.groupReady (key 1 1) done, completed)
  assertEqual "retiring remaining peer discards its cached prefix" Groups.initialState (fst (Groups.retirePeer (peer 2) done))

caseCleanup :: Assertion
caseCleanup = do
  let progressed = fst (Groups.advanceCompletedPrefix (peer 1) (prefix 1000) Groups.initialState)
      cycles = scanl' (\state ordinal -> publishTo ordinal (key 1 1) [(1, ordinal)] state) progressed [1 .. 250]
  assertEqual "no per-invocation group or publication history" (replicate (length cycles) (0, 0, 0, True)) [(Groups.retainedGroupCount state, Groups.retainedPublicationCount state, Groups.retainedFrontierCount state, Groups.valid state) | state <- cycles]
  assertEqual "regressed progress is inert" (last cycles, Set.empty) (Groups.advanceCompletedPrefix (peer 1) (prefix 1) (last cycles))
  assertEqual "canonical peer retirement reclaims final progress coordinate" Groups.initialState (fst (Groups.retirePeer (peer 1) (last cycles)))

caseDuplicate :: Assertion
caseDuplicate = do
  let identifier = publication 1
      members = Groups.membership (process 1) (Just (object 1)) Nothing
      admitted = checked (Groups.acceptPublication identifier members False Groups.initialState)
      settled = fst (checked (Groups.settleLocalPublication identifier Map.empty admitted))
  assertEqual "duplicate outstanding publication" (Left (Groups.PublicationAlreadyPending identifier)) (Groups.acceptPublication identifier members False admitted)
  assertEqual "second local completion is not silently double-counted" (Left (Groups.PublicationNotPending identifier)) (Groups.settleLocalPublication identifier Map.empty settled)

-- This intentionally retains every publication and recomputes readiness from
-- scratch. Production's group counters and frontier index are not reused.
data ReferencePublication = ReferencePublication
  { identifier :: !PublicationId,
    keys :: !(Set Groups.GroupKey),
    unstamped :: !Bool,
    destinations :: !(Maybe (Map HeraldEpoch StreamSequence))
  }
  deriving stock (Eq, Show)

data Reference = Reference
  { history :: ![ReferencePublication],
    completed :: !(Map HeraldEpoch StreamPrefix),
    retired :: !(Set HeraldEpoch),
    nextPublication :: !Word64
  }
  deriving stock (Eq, Show)

data Operation
  = Accept Word8 (Maybe Word8) (Maybe Word8) Bool
  | Settle Int [(Word8, Word64)]
  | Complete Word8 Word64
  | Retire
  deriving stock (Show)

operation :: QC.Gen Operation
operation =
  QC.frequency
    [ (4, Accept <$> QC.choose (1, 2) <*> optionalObject <*> optionalObject <*> QC.arbitrary),
      (4, Settle <$> QC.chooseInt (0, 12) <*> QC.listOf ((,) <$> QC.choose (1, 3) <*> QC.choose (1, 30))),
      (3, Complete <$> QC.choose (1, 3) <*> QC.choose (0, 30)),
      (1, pure Retire)
    ]
  where
    optionalObject = QC.frequency [(1, pure Nothing), (4, Just <$> QC.choose (1, 4))]

propHistory :: QC.Property
propHistory = QC.forAll (QC.vectorOf 90 operation) $ \schedule ->
  let states = scanl' apply (Groups.initialState, Reference [] Map.empty Set.empty 1, Set.empty, Set.empty) schedule
   in QC.conjoin
        [ QC.counterexample ("after operation " <> show ordinal <> ": " <> show reference)
            $ QC.conjoin
              ( [ QC.property (Groups.valid state),
                  actualWake QC.=== expectedWake,
                  Groups.retainedPublicationCount state QC.=== length (localPending reference),
                  Groups.retainedGroupCount state QC.=== length [group | group <- allKeys, not (referenceReady group reference)],
                  Groups.retainedFrontierCount state QC.=== sum [Map.size (referenceFrontiers group reference) | group <- allKeys]
                ]
                  <> [ QC.conjoin
                         [ Groups.groupLocalPendingCount group state QC.=== length [entry | entry <- localPending reference, group `Set.member` entry.keys],
                           Groups.groupUnstampedCount group state QC.=== length [entry | entry <- localPending reference, entry.unstamped, group `Set.member` entry.keys],
                           Groups.groupFrontiers group state QC.=== referenceFrontiers group reference,
                           Groups.groupPendingPeerCount group state QC.=== Map.size (referenceFrontiers group reference),
                           Groups.groupReady group state QC.=== referenceReady group reference
                         ]
                     | group <- allKeys
                     ]
              )
        | (ordinal, (state, reference, actualWake, expectedWake)) <- zip [0 :: Int ..] states
        ]
  where
    allKeys = [key caller objectNumber | caller <- [1, 2], objectNumber <- [1 .. 4]]
    apply (state, reference, _, _) change = case change of
      Accept caller sequencer published isUnstamped ->
        let identifier = publication reference.nextPublication
            processId = process caller
            objectIds = catMaybes [object <$> sequencer, object <$> published]
            members = Groups.membership processId (object <$> sequencer) (object <$> published)
            entry = ReferencePublication identifier (Set.fromList (map (Groups.groupKey processId) objectIds)) isUnstamped Nothing
         in ( checked (Groups.acceptPublication identifier members isUnstamped state),
              reference {history = entry : reference.history, nextPublication = reference.nextPublication + 1},
              Set.empty,
              Set.empty
            )
      Settle selection destinations -> case localPending reference of
        [] -> (state, reference, Set.empty, Set.empty)
        pending ->
          let chosen = pending !! (selection `mod` length pending)
              assignments = Map.fromListWith max [(peer destination, sequenceAt sequenceNumber) | (destination, sequenceNumber) <- destinations, Set.notMember (peer destination) reference.retired]
              (successor, changed) = checked (Groups.settleLocalPublication chosen.identifier assignments state)
              replace entry
                | entry.identifier == chosen.identifier = entry {destinations = Just assignments}
                | otherwise = entry
           in (successor, reference {history = map replace reference.history}, changed, chosen.keys)
      Complete peerNumber value
        | Set.member (peer peerNumber) reference.retired -> (state, reference, Set.empty, Set.empty)
        | otherwise ->
            let destination = peer peerNumber
                (successor, changed) = Groups.advanceCompletedPrefix destination (prefix value) state
                next = reference {completed = Map.insertWith max destination (prefix value) reference.completed}
             in (successor, next, changed, frontierChanges destination reference next)
      Retire ->
        let destination = peer 3
            (successor, changed) = Groups.retirePeer destination state
            next = reference {completed = Map.delete destination reference.completed, retired = Set.insert destination reference.retired}
         in (successor, next, changed, frontierChanges destination reference next)
    frontierChanges destination before after =
      Set.fromList
        [ group
        | group <- allKeys,
          Map.lookup destination (referenceFrontiers group before) /= Map.lookup destination (referenceFrontiers group after)
        ]

localPending :: Reference -> [ReferencePublication]
localPending reference = [entry | entry <- reference.history, entry.destinations == Nothing, not (Set.null entry.keys)]

referenceFrontiers :: Groups.GroupKey -> Reference -> Map HeraldEpoch StreamSequence
referenceFrontiers group reference =
  Map.fromListWith
    max
    [ (destination, sequenceNumber)
    | entry <- reference.history,
      group `Set.member` entry.keys,
      Just destinations <- [entry.destinations],
      (destination, sequenceNumber) <- Map.toList destinations,
      Set.notMember destination reference.retired,
      streamPrefixThrough sequenceNumber > Map.findWithDefault emptyStreamPrefix destination reference.completed
    ]

referenceReady :: Groups.GroupKey -> Reference -> Bool
referenceReady group reference =
  null [entry | entry <- localPending reference, group `Set.member` entry.keys]
    && Map.null (referenceFrontiers group reference)

publishTo :: Word64 -> Groups.GroupKey -> [(Word8, Word64)] -> Groups.State -> Groups.State
publishTo ordinal group destinations state =
  let identifier = publication ordinal
      members = Groups.membership (Groups.groupProcess group) (Just (Groups.groupObject group)) Nothing
      admitted = checked (Groups.acceptPublication identifier members False state)
   in fst (checked (Groups.settleLocalPublication identifier (Map.fromList [(peer destination, sequenceAt sequenceNumber) | (destination, sequenceNumber) <- destinations]) admitted))

key :: Word8 -> Word8 -> Groups.GroupKey
key processNumber objectNumber = Groups.groupKey (process processNumber) (object objectNumber)

process :: Word8 -> ProcessEpochId
process number = checked (mkProcessEpochId (ByteString.replicate 32 number))

object :: Word8 -> GlobalObjectId
object number = checked (mkGlobalObjectId (ByteString.replicate 32 (number + 20)))

peer :: Word8 -> HeraldEpoch
peer number = checked (mkHeraldEpoch (ByteString.replicate 32 (number + 40)))

publication :: Word64 -> PublicationId
publication ordinal = publicationId writer genesisAuthorityEpoch (peer 0) (nablaSequence ordinal)
  where
    writer = checked (mkNablaId (ByteString.replicate 32 80))

sequenceAt :: Word64 -> StreamSequence
sequenceAt = checked . mkStreamSequence

prefix :: Word64 -> StreamPrefix
prefix 0 = emptyStreamPrefix
prefix value = streamPrefixThrough (sequenceAt value)

checked :: (Show problem) => Either problem result -> result
checked = either (error . show) id
