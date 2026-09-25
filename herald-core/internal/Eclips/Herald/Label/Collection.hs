{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Historical local installation and direct survivor collection. Reports touch
-- one decision; peer and collector indices select membership and binding work.
module Eclips.Herald.Label.Collection
  ( State,
    CollectionProblem (..),
    emptyState,
    begin,
    refreshMembership,
    refreshBindings,
    observe,
    stage,
    takeReady,
    currentReport,
    reports,
    collector,
    outstanding,
    completionReady,
    offeredGeneration,
    markCompletionOffered,
    reportNeedsDispatch,
    markReportDispatched,
    pendingWork,
    consumeWork,
    wakeCollector,
    wakeDecision,
    complete,
    valid,
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, LabelDecisionId)
import Eclips.Domain.Label
  ( LabelInstallationReport,
    labelInstallationControlIndex,
    labelInstallationDecisionId,
    labelInstallationOutcomeDigest,
    labelInstallationReporter,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)

data CollectionProblem
  = CollectionAlreadyActive
  | LocalInstallerNotCaptured
  | LocalTerminalMissing
  | InstallationDecisionMismatch
  | InstallationIndexMismatch
  | InstallationOutcomeMismatch
  | InstallationReporterNotCaptured
  | ConflictingEarlyReport
  deriving stock (Eq, Show)

-- A report retains no payload and is not erased by transport delivery.
data Collection = Collection
  { local :: !LabelInstallationReport,
    home :: !HeraldEpoch,
    captured :: !(Set HeraldEpoch),
    assigned :: !(Maybe HeraldEpoch),
    installed :: !(Set HeraldEpoch),
    remaining :: !(Set HeraldEpoch),
    dispatched :: !(Maybe (HeraldEpoch, Word64)),
    offered :: !(Maybe HeraldMembershipGenerationId)
  }
  deriving stock (Eq, Show)

data State = State
  { current :: !(Map LabelDecisionId Collection),
    membership :: !(Maybe (HeraldMembershipGenerationId, Set HeraldEpoch)),
    byOutstandingPeer :: !(Map HeraldEpoch (Set LabelDecisionId)),
    byCollector :: !(Map HeraldEpoch (Set LabelDecisionId)),
    locallyReady :: !(Set LabelDecisionId),
    work :: !(Set LabelDecisionId),
    bindings :: !(Map HeraldEpoch Word64),
    ahead :: !(Map ControlIndex (Map (LabelDecisionId, HeraldEpoch) LabelInstallationReport))
  }
  deriving stock (Eq, Show)

emptyState :: State
emptyState = State Map.empty Nothing Map.empty Map.empty Set.empty Set.empty Map.empty Map.empty

begin :: LabelInstallationReport -> HeraldEpoch -> Set HeraldEpoch -> HeraldMembershipGenerationId -> Set HeraldEpoch -> State -> Either CollectionProblem State
begin report home captured generation active state
  | Map.member decision state.current = Left CollectionAlreadyActive
  | Set.notMember (labelInstallationReporter report) captured = Left LocalInstallerNotCaptured
  | otherwise =
      let refreshed = refreshMembership generation active state
       in Right
            refreshed
              { current = Map.insert decision collection refreshed.current,
                membership = Just (generation, active),
                byOutstandingPeer = foldl' (flip (insertIndex decision)) refreshed.byOutstandingPeer collection.remaining,
                byCollector = maybe refreshed.byCollector (\peer -> insertIndex decision peer refreshed.byCollector) collection.assigned,
                locallyReady = if isLocallyReady collection then Set.insert decision refreshed.locallyReady else refreshed.locallyReady,
                work = Set.insert decision refreshed.work
              }
  where
    decision = labelInstallationDecisionId report
    required = Set.intersection captured active
    installed = Set.singleton (labelInstallationReporter report)
    collection = Collection report home captured (assign home required) installed (Set.difference required installed) Nothing Nothing

assign :: HeraldEpoch -> Set HeraldEpoch -> Maybe HeraldEpoch
assign home required
  | Set.member home required = Just home
  | otherwise = Set.lookupMin required

-- | Newly admitted epochs do not join an older decision's captured set.
-- Retirement visits only decisions waiting for that peer or assigned to it.
-- Ready local collectors are re-offered under every new canonical generation.
refreshMembership :: HeraldMembershipGenerationId -> Set HeraldEpoch -> State -> State
refreshMembership generation active state
  | Map.null state.current = state
  | Just (previous, _) <- state.membership, previous == generation = state
  | otherwise =
      let previousActive = maybe Set.empty snd state.membership
          retired = Set.difference previousActive active
          affected = Set.unions [Map.findWithDefault Set.empty peer state.byOutstandingPeer `Set.union` Map.findWithDefault Set.empty peer state.byCollector | peer <- Set.toList retired]
          refreshed = foldl' refreshOne state affected
       in refreshed
            { membership = Just (generation, active),
              work = refreshed.work `Set.union` refreshed.locallyReady
            }
  where
    refreshOne currentState decision =
      case Map.lookup decision currentState.current of
        Nothing -> error "label collection index names no decision"
        Just old ->
          let required = Set.intersection old.captured active
              replacement = old {assigned = assign old.home required, remaining = Set.difference required old.installed}
              removeOld = foldl' (flip (deleteIndex decision)) currentState.byOutstandingPeer (Set.difference old.remaining replacement.remaining)
              addNew = foldl' (flip (insertIndex decision)) removeOld (Set.difference replacement.remaining old.remaining)
              withoutCollector = maybe currentState.byCollector (\peer -> deleteIndex decision peer currentState.byCollector) old.assigned
              collectors = maybe withoutCollector (\peer -> insertIndex decision peer withoutCollector) replacement.assigned
           in currentState
                { current = Map.insert decision replacement currentState.current,
                  byOutstandingPeer = addNew,
                  byCollector = collectors,
                  locallyReady = updateReady decision replacement currentState.locallyReady,
                  work = Set.insert decision currentState.work
                }

-- | Bindings are few and independent of completed label history. Only a new or
-- replacement connection wakes the decisions assigned to that destination.
refreshBindings :: Map HeraldEpoch Word64 -> State -> State
refreshBindings bindings state
  | bindings == state.bindings = state
  | otherwise =
      let changed = Map.keysSet (Map.filterWithKey (\peer binding -> Map.lookup peer state.bindings /= Just binding) bindings)
          refreshed = foldl' (flip wakeCollector) state changed
       in refreshed {bindings}

-- | Admission occurs after the report's canonical prerequisite is projected.
-- Reports received before a collector reassignment may be retained by a captured
-- Herald; only the assigned collector can issue the completion attestation.
observe :: LabelInstallationReport -> State -> Either CollectionProblem State
observe report state = case Map.lookup decision state.current of
  Nothing -> Left LocalTerminalMissing
  Just current
    | labelInstallationControlIndex report /= labelInstallationControlIndex current.local -> Left InstallationIndexMismatch
    | labelInstallationOutcomeDigest report /= labelInstallationOutcomeDigest current.local -> Left InstallationOutcomeMismatch
    | Set.notMember reporter current.captured -> Left InstallationReporterNotCaptured
    | Set.member reporter current.installed -> Right state
    | otherwise ->
        let replacement = current {installed = Set.insert reporter current.installed, remaining = Set.delete reporter current.remaining}
         in Right
              state
                { current = Map.insert decision replacement state.current,
                  byOutstandingPeer = deleteIndex decision reporter state.byOutstandingPeer,
                  locallyReady = updateReady decision replacement state.locallyReady,
                  work = Set.insert decision state.work
                }
  where
    decision = labelInstallationDecisionId report
    reporter = labelInstallationReporter report

stage :: LabelInstallationReport -> State -> Either CollectionProblem State
stage report state =
  case Map.lookup index state.ahead >>= Map.lookup key of
    Just retained | retained /= report -> Left ConflictingEarlyReport
    Just _ -> Right state
    Nothing -> Right state {ahead = Map.alter (Just . Map.insert key report . maybe Map.empty id) index state.ahead}
  where
    index = labelInstallationControlIndex report
    key = (labelInstallationDecisionId report, labelInstallationReporter report)

takeReady :: ControlIndex -> State -> (State, [LabelInstallationReport])
takeReady through state
  | maybe True ((> through) . fst) (Map.lookupMin state.ahead) = (state, [])
  | otherwise =
      let (before, at, after) = Map.splitLookup through state.ahead
          ready = Map.elems before <> maybe [] pure at
       in (state {ahead = after}, concatMap Map.elems ready)

currentReport :: LabelDecisionId -> State -> Maybe LabelInstallationReport
currentReport decision state = (.local) <$> Map.lookup decision state.current

-- | Inspection only; runtime dispatch uses 'pendingWork'.
reports :: State -> [LabelInstallationReport]
reports state = map (.local) (Map.elems state.current)

collector :: LabelDecisionId -> State -> Maybe HeraldEpoch
collector decision state = Map.lookup decision state.current >>= (.assigned)

outstanding :: LabelDecisionId -> State -> Set HeraldEpoch
outstanding decision state = maybe Set.empty (.remaining) (Map.lookup decision state.current)

completionReady :: LabelDecisionId -> State -> Maybe HeraldMembershipGenerationId
completionReady decision state = do
  current <- Map.lookup decision state.current
  (generation, _) <- state.membership
  if isLocallyReady current && current.offered /= Just generation
    then Just generation
    else Nothing

offeredGeneration :: LabelDecisionId -> State -> Maybe HeraldMembershipGenerationId
offeredGeneration decision state = Map.lookup decision state.current >>= (.offered)

markCompletionOffered :: LabelDecisionId -> State -> State
markCompletionOffered decision state = case state.membership of
  Nothing -> state
  Just (generation, _) -> state {current = Map.adjust (\current -> current {offered = Just generation}) decision state.current}

reportNeedsDispatch :: LabelDecisionId -> HeraldEpoch -> Word64 -> State -> Bool
reportNeedsDispatch decision destination binding state = case Map.lookup decision state.current of
  Just current ->
    current.assigned == Just destination
      && destination /= labelInstallationReporter current.local
      && current.dispatched /= Just (destination, binding)
  Nothing -> False

markReportDispatched :: LabelDecisionId -> HeraldEpoch -> Word64 -> State -> State
markReportDispatched decision destination binding state = state {current = Map.adjust (\current -> current {dispatched = Just (destination, binding)}) decision state.current}

pendingWork :: State -> Set LabelDecisionId
pendingWork state = state.work

consumeWork :: LabelDecisionId -> State -> State
consumeWork decision state = state {work = Set.delete decision state.work}

wakeCollector :: HeraldEpoch -> State -> State
wakeCollector destination state = state {work = state.work `Set.union` Map.findWithDefault Set.empty destination state.byCollector}

wakeDecision :: LabelDecisionId -> State -> State
wakeDecision decision state
  | Map.member decision state.current = state {work = Set.insert decision state.work}
  | otherwise = state

complete :: LabelDecisionId -> State -> State
complete decision state = case Map.lookup decision state.current of
  Nothing -> state
  Just current ->
    let remaining = Map.delete decision state.current
     in state
          { current = remaining,
            membership = if Map.null remaining then Nothing else state.membership,
            byOutstandingPeer = foldl' (flip (deleteIndex decision)) state.byOutstandingPeer current.remaining,
            byCollector = maybe state.byCollector (\peer -> deleteIndex decision peer state.byCollector) current.assigned,
            locallyReady = Set.delete decision state.locallyReady,
            work = Set.delete decision state.work
          }

isLocallyReady :: Collection -> Bool
isLocallyReady current = current.assigned == Just (labelInstallationReporter current.local) && Set.null current.remaining

updateReady :: LabelDecisionId -> Collection -> Set LabelDecisionId -> Set LabelDecisionId
updateReady decision current = if isLocallyReady current then Set.insert decision else Set.delete decision

insertIndex :: LabelDecisionId -> HeraldEpoch -> Map HeraldEpoch (Set LabelDecisionId) -> Map HeraldEpoch (Set LabelDecisionId)
insertIndex decision peer = Map.insertWith Set.union peer (Set.singleton decision)

deleteIndex :: LabelDecisionId -> HeraldEpoch -> Map HeraldEpoch (Set LabelDecisionId) -> Map HeraldEpoch (Set LabelDecisionId)
deleteIndex decision peer = Map.update (\previous -> let remaining = Set.delete decision previous in if Set.null remaining then Nothing else Just remaining) peer

-- | Independently reconstruct derived indices at test/invariant boundaries.
valid :: State -> Bool
valid state =
  state.byOutstandingPeer == Map.fromListWith Set.union [(peer, Set.singleton decision) | (decision, current) <- entries, peer <- Set.toList current.remaining]
    && state.byCollector == Map.fromListWith Set.union [(peer, Set.singleton decision) | (decision, current) <- entries, Just peer <- [current.assigned]]
    && state.locallyReady == Set.fromList [decision | (decision, current) <- entries, isLocallyReady current]
    && state.work `Set.isSubsetOf` Map.keysSet state.current
    && all collectionValid entries
    && all (\(index, bucket) -> all (\((decision, reporter), report) -> index == labelInstallationControlIndex report && decision == labelInstallationDecisionId report && reporter == labelInstallationReporter report) (Map.toList bucket)) (Map.toList state.ahead)
  where
    entries = Map.toList state.current
    collectionValid (decision, current) = case state.membership of
      Nothing -> False
      Just (_, active) ->
        let required = Set.intersection current.captured active
         in decision == labelInstallationDecisionId current.local
              && labelInstallationReporter current.local `Set.member` current.captured
              && current.installed `Set.isSubsetOf` current.captured
              && current.remaining == required `Set.difference` current.installed
              && current.assigned == assign current.home required
