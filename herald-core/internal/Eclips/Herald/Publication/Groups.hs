{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Conservative caller/object publication readiness. A destination's greatest
-- assigned sequence replaces all earlier remote obligations for the same group.
-- Only a contiguous completed prefix (or authoritative peer retirement) releases
-- that frontier; the peer owner still retains its existing selective completion
-- and payload reclamation semantics.
--
-- This owner has no request, gate, timer, transport or completed-publication
-- history. Its parent classifies exact publication retries before fresh admission
-- and prevents new assignment/progress from retired peer epochs.
module Eclips.Herald.Publication.Groups
  ( State,
    initialState,
    GroupKey,
    groupKey,
    groupProcess,
    groupObject,
    Membership,
    membership,
    membershipKeys,
    Problem (..),
    acceptPublication,
    publicationPending,
    settleLocalPublication,
    advanceCompletedPrefix,
    retirePeer,
    groupLocalPendingCount,
    groupUnstampedCount,
    groupPendingPeerCount,
    groupFrontiers,
    groupReady,
    retainedGroupCount,
    retainedPublicationCount,
    retainedFrontierCount,
    valid,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
  )
import Eclips.Herald.PeerStream
  ( StreamPrefix,
    StreamSequence,
    emptyStreamPrefix,
    streamPrefixSequence,
    streamPrefixThrough,
  )

data GroupKey = GroupKey !ProcessEpochId !GlobalObjectId
  deriving stock (Eq, Ord, Show)

groupKey :: ProcessEpochId -> GlobalObjectId -> GroupKey
groupKey = GroupKey

groupProcess :: GroupKey -> ProcessEpochId
groupProcess (GroupKey process _) = process

groupObject :: GroupKey -> GlobalObjectId
groupObject (GroupKey _ object) = object

-- | Only this constructor function can form membership: one issuer and at most
-- two distinct objects. The second optional object is the published controlled
-- object itself; the first is its writer's acceptance-time sequencing object.
newtype Membership = Membership (Set GroupKey)
  deriving stock (Eq, Show)

membership :: ProcessEpochId -> Maybe GlobalObjectId -> Maybe GlobalObjectId -> Membership
membership process sequencing published =
  Membership (Set.fromList (map (GroupKey process) (catMaybes [sequencing, published])))

membershipKeys :: Membership -> Set GroupKey
membershipKeys (Membership keys) = keys

data PendingPublication = PendingPublication !Membership !Bool
  deriving stock (Eq, Show)

data GroupState = GroupState
  { localPending :: !Int,
    unstamped :: !Int,
    frontiers :: !(Map HeraldEpoch StreamSequence)
  }
  deriving stock (Eq, Show)

data State = State
  { groups :: !(Map GroupKey GroupState),
    pending :: !(Map PublicationId PendingPublication),
    byPeer :: !(Map HeraldEpoch (Map StreamSequence (Set GroupKey))),
    completed :: !(Map HeraldEpoch StreamPrefix)
  }
  deriving stock (Eq, Show)

initialState :: State
initialState = State Map.empty Map.empty Map.empty Map.empty

emptyGroup :: GroupState
emptyGroup = GroupState 0 0 Map.empty

data Problem
  = PublicationAlreadyPending PublicationId
  | PublicationNotPending PublicationId
  deriving stock (Eq, Show)

-- | Register only a fresh admitted publication, before local effects can become
-- visible. The Boolean identifies an unstamped structural stage. A later request
-- held by a label gate is not yet an admitted publication and must not enter here.
-- Publications with no membership need no readiness bookkeeping at all.
acceptPublication :: PublicationId -> Membership -> Bool -> State -> Either Problem State
acceptPublication identifier members isUnstamped state
  | Map.member identifier state.pending = Left (PublicationAlreadyPending identifier)
  | Set.null keys = Right state
  | otherwise =
      Right
        state
          { pending = Map.insert identifier (PendingPublication members isUnstamped) state.pending,
            groups = Set.foldl' add state.groups keys
          }
  where
    keys = membershipKeys members
    add groups key = Map.alter (Just . increment . maybe emptyGroup id) key groups
    increment group =
      group
        { localPending = group.localPending + 1,
          unstamped = group.unstamped + if isUnstamped then 1 else 0
        }

publicationPending :: PublicationId -> State -> Bool
publicationPending identifier state = Map.member identifier state.pending

-- | The parent commits this together with local Store/structural application,
-- context-debt registration and certification of the exact remote assignments.
-- No remote assignment is not enough on its own: unassigned/unstamped work stays
-- pending until this complete local transaction can run. A structural stage is
-- stamped and locally applied in that same transaction.
--
-- The frontier map contains the maximum certified sequence per destination, not
-- payloads or per-assignment links. Fresh retries are classified by the parent;
-- calling this twice for the same local token is an invariant contradiction.
settleLocalPublication :: PublicationId -> Map HeraldEpoch StreamSequence -> State -> Either Problem (State, Set GroupKey)
settleLocalPublication identifier assignments state = case Map.lookup identifier state.pending of
  Nothing -> Left (PublicationNotPending identifier)
  Just (PendingPublication members isUnstamped) ->
    let keys = membershipKeys members
        withoutPending = state {pending = Map.delete identifier state.pending}
        settled = Set.foldl' (settleGroup isUnstamped assignments) withoutPending keys
     in Right (settled, keys)

settleGroup :: Bool -> Map HeraldEpoch StreamSequence -> State -> GroupKey -> State
settleGroup isUnstamped assignments state key =
  Map.foldlWithKey' (addFrontier key) locallySettled assignments
  where
    locallySettled = alterGroup key decrement state
    decrement group =
      group
        { localPending = group.localPending - 1,
          unstamped = group.unstamped - if isUnstamped then 1 else 0
        }

addFrontier :: GroupKey -> State -> HeraldEpoch -> StreamSequence -> State
addFrontier key state peer sequenceNumber
  | streamPrefixThrough sequenceNumber <= Map.findWithDefault emptyStreamPrefix peer state.completed = state
  | maybe False (>= sequenceNumber) previous = state
  | otherwise =
      withOldRemoved
        { groups = Map.insert key group {frontiers = Map.insert peer sequenceNumber group.frontiers} withOldRemoved.groups,
          byPeer = Map.alter (Just . Map.insertWith Set.union sequenceNumber (Set.singleton key) . maybe Map.empty id) peer withOldRemoved.byPeer
        }
  where
    group = Map.findWithDefault emptyGroup key state.groups
    previous = Map.lookup peer group.frontiers
    withOldRemoved = maybe state (removePeerIndex key peer state) previous

removePeerIndex :: GroupKey -> HeraldEpoch -> State -> StreamSequence -> State
removePeerIndex key peer state sequenceNumber =
  state {byPeer = Map.update removeGroup peer state.byPeer}
  where
    removeGroup entries = nonemptyMap (Map.update (nonemptySet . Set.delete key) sequenceNumber entries)

-- | Only the newly crossed prefix is visited. Duplicate or regressed prefix
-- reports produce neither state changes nor wakeups. Sparse completion beyond
-- the prefix deliberately does not satisfy a group frontier.
advanceCompletedPrefix :: HeraldEpoch -> StreamPrefix -> State -> (State, Set GroupKey)
advanceCompletedPrefix peer prefix state
  | prefix <= Map.findWithDefault emptyStreamPrefix peer state.completed = (state, Set.empty)
  | otherwise =
      let (crossed, remaining) = splitThrough prefix (Map.findWithDefault Map.empty peer state.byPeer)
          changed = Set.unions (Map.elems crossed)
          updated =
            state
              { byPeer = Map.update (const (nonemptyMap remaining)) peer state.byPeer,
                completed = Map.insert peer prefix state.completed
              }
       in (Set.foldl' (clearFrontier peer) updated changed, changed)

splitThrough :: StreamPrefix -> Map StreamSequence value -> (Map StreamSequence value, Map StreamSequence value)
splitThrough prefix entries = case streamPrefixSequence prefix of
  Nothing -> (Map.empty, entries)
  Just sequenceNumber ->
    let (before, equal, after) = Map.splitLookup sequenceNumber entries
     in (maybe before (\value -> Map.insert sequenceNumber value before) equal, after)

-- | Called only on canonical retirement. This discharges that destination's
-- obligations without claiming semantic completion and discards the cached
-- prefix. The authoritative parent must exclude subsequent work/progress from
-- this retired epoch; no duplicate retired-membership history is retained here.
retirePeer :: HeraldEpoch -> State -> (State, Set GroupKey)
retirePeer peer state =
  let changed = Set.unions (Map.elems (Map.findWithDefault Map.empty peer state.byPeer))
      updated = state {byPeer = Map.delete peer state.byPeer, completed = Map.delete peer state.completed}
   in (Set.foldl' (clearFrontier peer) updated changed, changed)

clearFrontier :: HeraldEpoch -> State -> GroupKey -> State
clearFrontier peer state key = alterGroup key (\group -> group {frontiers = Map.delete peer group.frontiers}) state

alterGroup :: GroupKey -> (GroupState -> GroupState) -> State -> State
alterGroup key change state =
  state {groups = Map.update (retainGroup . change) key state.groups}

retainGroup :: GroupState -> Maybe GroupState
retainGroup group
  | group.localPending == 0 && Map.null group.frontiers = Nothing
  | otherwise = Just group

nonemptyMap :: Map key value -> Maybe (Map key value)
nonemptyMap values
  | Map.null values = Nothing
  | otherwise = Just values

nonemptySet :: Set value -> Maybe (Set value)
nonemptySet values
  | Set.null values = Nothing
  | otherwise = Just values

groupLocalPendingCount :: GroupKey -> State -> Int
groupLocalPendingCount key state = maybe 0 (.localPending) (Map.lookup key state.groups)

groupUnstampedCount :: GroupKey -> State -> Int
groupUnstampedCount key state = maybe 0 (.unstamped) (Map.lookup key state.groups)

groupPendingPeerCount :: GroupKey -> State -> Int
groupPendingPeerCount key state = maybe 0 (Map.size . (.frontiers)) (Map.lookup key state.groups)

groupFrontiers :: GroupKey -> State -> Map HeraldEpoch StreamSequence
groupFrontiers key state = maybe Map.empty (.frontiers) (Map.lookup key state.groups)

groupReady :: GroupKey -> State -> Bool
groupReady key state = Map.notMember key state.groups

retainedGroupCount :: State -> Int
retainedGroupCount state = Map.size state.groups

retainedPublicationCount :: State -> Int
retainedPublicationCount state = Map.size state.pending

-- | Diagnostic only; readiness and progress paths never sum every group.
retainedFrontierCount :: State -> Int
retainedFrontierCount state = sum (map (Map.size . (.frontiers)) (Map.elems state.groups))

-- | Exhaustive test/debug oracle. Never called by normal owner transitions.
valid :: State -> Bool
valid state =
  localCounts == expectedLocalCounts
    && all validGroup (Map.elems state.groups)
    && all validMembership (Map.elems state.pending)
    && state.byPeer == expectedPeerIndex
    && and
      [ streamPrefixThrough sequenceNumber > Map.findWithDefault emptyStreamPrefix peer state.completed
      | group <- Map.elems state.groups,
        (peer, sequenceNumber) <- Map.toList group.frontiers
      ]
  where
    localCounts = Map.map (\group -> (group.localPending, group.unstamped)) (Map.filter ((> 0) . (.localPending)) state.groups)
    expectedLocalCounts =
      foldl'
        (\counts (PendingPublication members isUnstamped) -> Set.foldl' (\acc key -> Map.insertWith addCounts key (1, if isUnstamped then 1 else 0) acc) counts (membershipKeys members))
        Map.empty
        (Map.elems state.pending)
    addCounts (left, leftUnstamped) (right, rightUnstamped) = (left + right, leftUnstamped + rightUnstamped)
    validGroup group = group.localPending >= 0 && group.unstamped >= 0 && group.unstamped <= group.localPending && retainGroup group /= Nothing
    validMembership (PendingPublication members _) = let count = Set.size (membershipKeys members) in count > 0 && count <= 2
    expectedPeerIndex =
      Map.foldlWithKey'
        (\index key group -> Map.foldlWithKey' (\acc peer sequenceNumber -> Map.insertWith (Map.unionWith Set.union) peer (Map.singleton sequenceNumber (Set.singleton key)) acc) index group.frontiers)
        Map.empty
        state.groups
