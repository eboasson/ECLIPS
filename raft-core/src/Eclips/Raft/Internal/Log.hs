module Eclips.Raft.Internal.Log
  ( RaftCommittedEntry,
    sealRaftCommittedEntry,
    committedEntryIndex,
    committedEntryPayload,
    committedEntryTerm,
    SealedRaftBatch,
    sealRaftBatch,
    sealedRaftBatchEffects,
    RaftLog,
    RaftLogFault (..),
    initialRaftLog,
    raftLogEntries,
    raftLogBaseIndex,
    raftLogBaseConfiguration,
    raftLogInstallCheckpoint,
    raftLogLastIndex,
    raftLogLastTerm,
    raftLogConfigurationAt,
    raftLogConfigurationIndexValid,
    raftLogTermAt,
    raftLogEntryAt,
    raftLogAppend,
    raftLogEntriesFrom,
    raftLogMerge,
    raftLogFirstIndexOfTerm,
    raftLogLastIndexOfTerm,
    raftLogCommittedEntriesBetween,
    raftLogIsContiguous,
    raftLogTermRegression,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Raft.Checkpoint
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftTerm,
    raftLogIndex,
    raftLogIndexWord64,
    raftTerm,
  )
import Eclips.Raft.Input
  ( RaftEntry (..),
    RaftLogEntry,
    raftLogEntry,
    raftLogEntryIndex,
    raftLogEntryPayload,
    raftLogEntryTerm,
  )
import Eclips.Raft.Internal.Configuration
  ( RaftConfigurationFault,
    RaftConfigurationRef (..),
    RaftVotingConfiguration,
    validateRaftConfigurationSuccessor,
  )

-- | Only the committed-log owner constructs these ordered observations.
data RaftCommittedEntry bytes = RaftCommittedEntry RaftLogIndex RaftTerm (RaftEntry bytes)
  deriving stock (Eq, Show)

sealRaftCommittedEntry :: RaftLogEntry bytes -> RaftCommittedEntry bytes
sealRaftCommittedEntry entry = RaftCommittedEntry (raftLogEntryIndex entry) (raftLogEntryTerm entry) (raftLogEntryPayload entry)

committedEntryIndex :: RaftCommittedEntry bytes -> RaftLogIndex
committedEntryIndex (RaftCommittedEntry index _ _) = index

committedEntryTerm :: RaftCommittedEntry bytes -> RaftTerm
committedEntryTerm (RaftCommittedEntry _ term _) = term

committedEntryPayload :: RaftCommittedEntry bytes -> RaftEntry bytes
committedEntryPayload (RaftCommittedEntry _ _ payload) = payload

-- | Package-internal indivisible effect handoff.  Keeping this constructor
-- below the public module boundary prevents applications from fabricating a
-- batch that did not accompany a checked successor state.
newtype SealedRaftBatch effect = SealedRaftBatch [effect]
  deriving stock (Eq, Show)

sealRaftBatch :: [effect] -> SealedRaftBatch effect
sealRaftBatch = SealedRaftBatch

sealedRaftBatchEffects :: SealedRaftBatch effect -> [effect]
sealedRaftBatchEffects (SealedRaftBatch effects) = effects

-- | Each constructor below preserves contiguous indexes, monotone terms and
-- the configuration transition chain. The second map indexes configuration
-- entries only; ordinary payloads never need replay for membership queries.
data RaftLog bytes
  = RaftLog
      RaftVotingConfiguration
      RaftConfigurationRef
      RaftVotingConfiguration
      (Map RaftLogIndex (RaftLogEntry bytes))
      (Map RaftLogIndex (RaftTerm, RaftVotingConfiguration))
  deriving stock (Eq, Show)

data RaftLogFault
  = RaftCommittedPrefixConflict RaftLogIndex RaftTerm RaftTerm
  | RaftEqualTermEntryConflict RaftLogIndex RaftTerm
  | RaftLogLostSentinel
  | RaftLogBecameNonContiguous
  | RaftLogTermOrderInvalid RaftLogIndex RaftTerm RaftTerm
  | RaftLogConfigurationTransitionInvalid RaftLogIndex RaftConfigurationFault
  | RaftCheckpointConfigurationConflict RaftLogIndex
  deriving stock (Eq, Show)

initialRaftLog :: RaftVotingConfiguration -> RaftLog bytes
initialRaftLog configuration =
  RaftLog
    configuration
    GenesisRaftConfigurationRef
    configuration
    ( Map.singleton
        (raftLogIndex 0)
        (raftLogEntry (raftLogIndex 0) (raftTerm 0) LeaderNoOp)
    )
    Map.empty

raftLogEntries :: RaftLog bytes -> [RaftLogEntry bytes]
raftLogEntries (RaftLog _ _ _ entries _) = Map.elems entries

raftLogBaseIndex :: RaftLog bytes -> RaftLogIndex
raftLogBaseIndex (RaftLog _ _ _ entries _) = maybe (raftLogIndex 0) fst (Map.lookupMin entries)

raftLogBaseConfiguration :: RaftLog bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
raftLogBaseConfiguration (RaftLog _ reference configuration _ _) = (reference, configuration)

-- | The base entry retains only the committed index and term. A matching local
-- suffix remains useful; an incompatible uncommitted suffix is discarded.
raftLogInstallCheckpoint :: RaftCheckpoint bytes -> RaftLog bytes -> Either RaftLogFault (RaftLog bytes)
raftLogInstallCheckpoint checkpoint original@(RaftLog genesis _ _ entries configurations)
  | matching && raftLogConfigurationAt index original /= (reference, configuration) = Left (RaftCheckpointConfigurationConflict index)
  | otherwise =
      Right
        ( RaftLog
            genesis
            reference
            configuration
            (Map.insert index (raftLogEntry index term LeaderNoOp) retainedEntries)
            retainedConfigurations
        )
  where
    index = raftCheckpointIndex checkpoint
    term = raftCheckpointTerm checkpoint
    reference = raftCheckpointConfigurationRef checkpoint
    configuration = raftCheckpointConfiguration checkpoint
    matching = raftLogTermAt index original == Just term
    retainedEntries = if matching then snd (Map.split index entries) else Map.empty
    retainedConfigurations = if matching then snd (Map.split index configurations) else Map.empty

raftLogLastIndex :: RaftLog bytes -> RaftLogIndex
raftLogLastIndex (RaftLog _ _ _ entries _) = case Map.lookupMax entries of
  Nothing -> raftLogIndex 0
  Just (index, _) -> index

raftLogLastTerm :: RaftLog bytes -> RaftTerm
raftLogLastTerm logState = case raftLogEntryAt (raftLogLastIndex logState) logState of
  Nothing -> raftTerm 0
  Just entry -> raftLogEntryTerm entry

raftLogConfigurationAt :: RaftLogIndex -> RaftLog bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
raftLogConfigurationAt limit (RaftLog _ reference configuration _ configurations) = case Map.lookupLE limit configurations of
  Nothing -> (reference, configuration)
  Just (index, (term, indexedConfiguration)) -> (RaftConfigurationEntryRef index term, indexedConfiguration)

-- | Exhaustive audit of the retained index, deliberately absent from ordinary
-- transitions. The independent property model also folds the exposed full log.
raftLogConfigurationIndexValid :: RaftVotingConfiguration -> RaftLog bytes -> Bool
raftLogConfigurationIndexValid expectedGenesis (RaftLog genesis _ _ entries configurations) =
  genesis == expectedGenesis
    && configurations == Map.mapMaybe select entries
  where
    select entry = case raftLogEntryPayload entry of
      Configuration configuration _ -> Just (raftLogEntryTerm entry, configuration)
      _ -> Nothing

raftLogTermAt :: RaftLogIndex -> RaftLog bytes -> Maybe RaftTerm
raftLogTermAt index logState = raftLogEntryTerm <$> raftLogEntryAt index logState

raftLogEntryAt :: RaftLogIndex -> RaftLog bytes -> Maybe (RaftLogEntry bytes)
raftLogEntryAt index (RaftLog _ _ _ entries _) = Map.lookup index entries

raftLogAppend ::
  RaftTerm ->
  RaftEntry bytes ->
  RaftLog bytes ->
  Either RaftLogFault (RaftLog bytes, RaftLogEntry bytes)
raftLogAppend term payload logState = do
  let index = raftLogIndex (raftLogIndexWord64 (raftLogLastIndex logState) + 1)
      entry = raftLogEntry index term payload
  appended <- insertCheckedEntries [entry] logState
  pure (appended, entry)

raftLogEntriesFrom :: RaftLogIndex -> RaftLog bytes -> [RaftLogEntry bytes]
raftLogEntriesFrom firstIndex (RaftLog _ _ _ entries _) =
  Map.elems (snd (Map.split (predecessor firstIndex) entries))

predecessor :: RaftLogIndex -> RaftLogIndex
predecessor index
  | raftLogIndexWord64 index == 0 = raftLogIndex 0
  | otherwise = raftLogIndex (raftLogIndexWord64 index - 1)

raftLogMerge ::
  (Eq bytes) =>
  RaftLogIndex ->
  [RaftLogEntry bytes] ->
  RaftLog bytes ->
  Either RaftLogFault (RaftLog bytes)
raftLogMerge committed incoming original = mergeEntries incoming original
  where
    mergeEntries [] logState = Right logState
    mergeEntries remaining@(entry : rest) logState@(RaftLog genesis baseReference baseConfiguration entries configurations) =
      case Map.lookup (raftLogEntryIndex entry) entries of
        Nothing -> insertCheckedEntries remaining logState
        Just local
          | raftLogEntryTerm local == raftLogEntryTerm entry,
            raftLogEntryPayload local == raftLogEntryPayload entry ->
              mergeEntries rest logState
          | raftLogEntryTerm local == raftLogEntryTerm entry ->
              Left
                ( RaftEqualTermEntryConflict
                    (raftLogEntryIndex entry)
                    (raftLogEntryTerm entry)
                )
          | raftLogEntryIndex entry <= committed ->
              Left
                ( RaftCommittedPrefixConflict
                    (raftLogEntryIndex entry)
                    (raftLogEntryTerm local)
                    (raftLogEntryTerm entry)
                )
          | otherwise ->
              insertCheckedEntries
                remaining
                (RaftLog genesis baseReference baseConfiguration (fst (Map.split (raftLogEntryIndex entry) entries)) (fst (Map.split (raftLogEntryIndex entry) configurations)))

-- Only the new or replaced suffix is admitted. A duplicate matching prefix
-- retains its original checked log, and truncation keeps both indexes together.
insertCheckedEntries :: [RaftLogEntry bytes] -> RaftLog bytes -> Either RaftLogFault (RaftLog bytes)
insertCheckedEntries incoming original = go (raftLogLastIndex original) (raftLogLastTerm original) (snd (raftLogConfigurationAt (raftLogLastIndex original) original)) original incoming
  where
    go _ _ _ logState [] = Right logState
    go precedingIndex precedingTerm precedingConfiguration (RaftLog genesis baseReference baseConfiguration entries configurations) (entry : rest)
      | raftLogEntryIndex entry /= raftLogIndex (raftLogIndexWord64 precedingIndex + 1) = Left RaftLogBecameNonContiguous
      | raftLogEntryTerm entry < precedingTerm = Left (RaftLogTermOrderInvalid (raftLogEntryIndex entry) precedingTerm (raftLogEntryTerm entry))
      | otherwise = do
          (configuration, indexed) <- case raftLogEntryPayload entry of
            Configuration successor _ -> do
              either (Left . RaftLogConfigurationTransitionInvalid (raftLogEntryIndex entry)) Right (validateRaftConfigurationSuccessor precedingConfiguration successor)
              pure (successor, Map.insert (raftLogEntryIndex entry) (raftLogEntryTerm entry, successor) configurations)
            _ -> pure (precedingConfiguration, configurations)
          go (raftLogEntryIndex entry) (raftLogEntryTerm entry) configuration (RaftLog genesis baseReference baseConfiguration (Map.insert (raftLogEntryIndex entry) entry entries) indexed) rest

raftLogFirstIndexOfTerm :: RaftTerm -> RaftLog bytes -> Maybe RaftLogIndex
raftLogFirstIndexOfTerm term (RaftLog _ _ _ entries _) =
  fst
    <$> Map.lookupMin
      (Map.filter ((== term) . raftLogEntryTerm) entries)

raftLogLastIndexOfTerm :: RaftTerm -> RaftLog bytes -> Maybe RaftLogIndex
raftLogLastIndexOfTerm term (RaftLog _ _ _ entries _) =
  fst
    <$> Map.lookupMax
      (Map.filter ((== term) . raftLogEntryTerm) entries)

raftLogCommittedEntriesBetween ::
  RaftLogIndex ->
  RaftLogIndex ->
  RaftLog bytes ->
  [RaftCommittedEntry bytes]
raftLogCommittedEntriesBetween lowerExclusive upperInclusive (RaftLog _ _ _ entries _) =
  let (_, afterLower) = Map.split lowerExclusive entries
      (beforeUpper, atUpper, _) = Map.splitLookup upperInclusive afterLower
   in map sealRaftCommittedEntry (Map.elems beforeUpper <> maybe [] pure atUpper)

raftLogIsContiguous :: RaftLog bytes -> Bool
raftLogIsContiguous logState =
  map raftLogEntryIndex (raftLogEntries logState)
    == map raftLogIndex [raftLogIndexWord64 (raftLogBaseIndex logState) .. raftLogIndexWord64 (raftLogLastIndex logState)]

raftLogTermRegression ::
  RaftLog bytes ->
  Maybe (RaftLogIndex, RaftTerm, RaftTerm)
raftLogTermRegression logState = go (raftLogEntries logState)
  where
    go (previous : current : remaining)
      | raftLogEntryTerm current < raftLogEntryTerm previous =
          Just
            ( raftLogEntryIndex current,
              raftLogEntryTerm previous,
              raftLogEntryTerm current
            )
      | otherwise = go (current : remaining)
    go _ = Nothing
