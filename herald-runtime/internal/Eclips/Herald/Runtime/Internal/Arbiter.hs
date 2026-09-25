-- | Explicit cyclic fairness for typed runtime ingress.
module Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (..),
    SourceId (..),
    ReadyRing,
    emptyReadyRing,
    readyRingMembers,
    insertReady,
    removeReady,
    selectReady,
    ArbiterModel,
    emptyArbiterModel,
    enqueueModel,
    pauseSourceModel,
    resumeSourceModel,
    removeSourceModel,
    selectModel,
    ArbiterSelection (..),
    Arbiter,
    newArbiter,
    enqueueArbiter,
    pauseArbiterSource,
    resumeArbiterSource,
    removeArbiterSource,
    selectArbiter,
    snapshotArbiter,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    newTVar,
    readTVar,
    retry,
    writeTVar,
  )
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)

data SourceFamily
  = ApplicationSources
  | AdministrationSources
  | ConfiguredAdministrationSources
  | PeerSources
  | OracleSources
  | RuntimeCompletionSources
  | LocalPeerWorkSources
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data SourceId = SourceId SourceFamily Word64
  deriving stock (Eq, Ord, Show)

newtype ReadyRing value = ReadyRing [value]
  deriving stock (Eq, Show)

emptyReadyRing :: ReadyRing value
emptyReadyRing = ReadyRing []

readyRingMembers :: ReadyRing value -> [value]
readyRingMembers (ReadyRing values) = values

insertReady :: (Eq value) => value -> ReadyRing value -> ReadyRing value
insertReady value ring@(ReadyRing values)
  | value `elem` values = ring
  | otherwise = ReadyRing (values <> [value])

removeReady :: (Eq value) => value -> ReadyRing value -> ReadyRing value
removeReady value (ReadyRing values) = ReadyRing (filter (/= value) values)

selectReady :: ReadyRing value -> Maybe (value, ReadyRing value)
selectReady (ReadyRing []) = Nothing
selectReady (ReadyRing (value : values)) = Just (value, ReadyRing values)

data ArbiterModel value = ArbiterModel
  { modelFamilyRing :: ReadyRing SourceFamily,
    modelSourceRings :: Map SourceFamily (ReadyRing SourceId),
    modelQueues :: Map SourceId (Seq value),
    modelPaused :: Set SourceId
  }
  deriving stock (Eq, Show)

emptyArbiterModel :: ArbiterModel value
emptyArbiterModel =
  ArbiterModel
    { modelFamilyRing = emptyReadyRing,
      modelSourceRings = Map.empty,
      modelQueues = Map.empty,
      modelPaused = Set.empty
    }

enqueueModel :: SourceId -> value -> ArbiterModel value -> ArbiterModel value
enqueueModel source value model =
  activateIfEligible
    source
    model
      { modelQueues = Map.alter appendValue source (modelQueues model)
      }
  where
    appendValue Nothing = Just (Seq.singleton value)
    appendValue (Just values) = Just (values |> value)

pauseSourceModel :: SourceId -> ArbiterModel value -> ArbiterModel value
pauseSourceModel source = deactivate source . markPaused
  where
    markPaused model = model {modelPaused = Set.insert source (modelPaused model)}

resumeSourceModel :: SourceId -> ArbiterModel value -> ArbiterModel value
resumeSourceModel source model =
  activateIfEligible source model {modelPaused = Set.delete source (modelPaused model)}

removeSourceModel :: SourceId -> ArbiterModel value -> ([value], ArbiterModel value)
removeSourceModel source model =
  ( maybe [] toList (Map.lookup source (modelQueues model)),
    deactivate
      source
      model
        { modelQueues = Map.delete source (modelQueues model),
          modelPaused = Set.delete source (modelPaused model)
        }
  )

data ArbiterSelection value = ArbiterSelection SourceId value
  deriving stock (Eq, Show)

selectModel :: ArbiterModel value -> Maybe (ArbiterSelection value, ArbiterModel value)
selectModel model = do
  (family, remainingFamilies) <- selectReady (modelFamilyRing model)
  sources <- Map.lookup family (modelSourceRings model)
  (source, remainingSources) <- selectReady sources
  queue <- Map.lookup source (modelQueues model)
  case queue of
    Empty -> Nothing
    value :<| remainingQueue ->
      let sourcesAfterSelection =
            if Seq.null remainingQueue
              then remainingSources
              else insertReady source remainingSources
          sourceRingsAfterSelection =
            setSourceRing family sourcesAfterSelection (modelSourceRings model)
          familiesAfterSelection =
            if null (readyRingMembers sourcesAfterSelection)
              then remainingFamilies
              else insertReady family remainingFamilies
          queuesAfterSelection =
            if Seq.null remainingQueue
              then Map.delete source (modelQueues model)
              else Map.insert source remainingQueue (modelQueues model)
       in Just
            ( ArbiterSelection source value,
              model
                { modelFamilyRing = familiesAfterSelection,
                  modelSourceRings = sourceRingsAfterSelection,
                  modelQueues = queuesAfterSelection
                }
            )

newtype Arbiter value = Arbiter (TVar (ArbiterModel value))

newArbiter :: STM (Arbiter value)
newArbiter = Arbiter <$> newTVar emptyArbiterModel

enqueueArbiter :: Arbiter value -> SourceId -> value -> STM ()
enqueueArbiter (Arbiter state) source value =
  modifyArbiter state (enqueueModel source value)

pauseArbiterSource :: Arbiter value -> SourceId -> STM ()
pauseArbiterSource (Arbiter state) source =
  modifyArbiter state (pauseSourceModel source)

resumeArbiterSource :: Arbiter value -> SourceId -> STM ()
resumeArbiterSource (Arbiter state) source =
  modifyArbiter state (resumeSourceModel source)

removeArbiterSource :: Arbiter value -> SourceId -> STM [value]
removeArbiterSource (Arbiter state) source = do
  model <- readTVar state
  let (discarded, successor) = removeSourceModel source model
  writeTVar state successor
  pure discarded

selectArbiter :: Arbiter value -> STM (ArbiterSelection value)
selectArbiter (Arbiter state) = do
  model <- readTVar state
  case selectModel model of
    Nothing -> retry
    Just (selection, successor) -> do
      writeTVar state successor
      pure selection

snapshotArbiter :: Arbiter value -> STM (ArbiterModel value)
snapshotArbiter (Arbiter state) = readTVar state

modifyArbiter :: TVar (ArbiterModel value) -> (ArbiterModel value -> ArbiterModel value) -> STM ()
modifyArbiter state update = do
  model <- readTVar state
  writeTVar state (update model)

sourceFamily :: SourceId -> SourceFamily
sourceFamily (SourceId family _) = family

activateIfEligible :: SourceId -> ArbiterModel value -> ArbiterModel value
activateIfEligible source model
  | source `Set.member` modelPaused model = model
  | maybe True Seq.null (Map.lookup source (modelQueues model)) = model
  | otherwise =
      let family = sourceFamily source
          sources = Map.findWithDefault emptyReadyRing family (modelSourceRings model)
          activeSources = insertReady source sources
       in model
            { modelFamilyRing = insertReady family (modelFamilyRing model),
              modelSourceRings = Map.insert family activeSources (modelSourceRings model)
            }

deactivate :: SourceId -> ArbiterModel value -> ArbiterModel value
deactivate source model =
  let family = sourceFamily source
      sources = Map.findWithDefault emptyReadyRing family (modelSourceRings model)
      activeSources = removeReady source sources
      families =
        if null (readyRingMembers activeSources)
          then removeReady family (modelFamilyRing model)
          else modelFamilyRing model
   in model
        { modelFamilyRing = families,
          modelSourceRings = setSourceRing family activeSources (modelSourceRings model)
        }

setSourceRing ::
  SourceFamily ->
  ReadyRing SourceId ->
  Map SourceFamily (ReadyRing SourceId) ->
  Map SourceFamily (ReadyRing SourceId)
setSourceRing family ring
  | null (readyRingMembers ring) = Map.delete family
  | otherwise = Map.insert family ring
