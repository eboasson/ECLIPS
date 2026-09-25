{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure convergent storage for one typed delta incarnation.
--
-- A store has a visible projection and a conservative retained projection. A
-- local take changes only the former.
module Eclips.Domain.Store
  ( DeltaStore,
    StoreError (..),
    StoredInstance,
    RetainedSnapshot,
    RetainedSnapshotError (..),
    RetainedApplyResult (..),
    RetainedTransition,
    emptyDeltaStore,
    deltaStoreSort,
    applyPublication,
    applyPublicationDetailed,
    applyRetainedSnapshot,
    localTake,
    purgeObjectKey,
    lookupVisible,
    lookupRetained,
    visibleInstances,
    retainedInstances,
    logicalStateStrength,
    mkRetainedSnapshot,
    retainedSnapshot,
    retainedSnapshotSort,
    retainedSnapshotWinnerFacts,
    retainedSnapshotLogicalStrengthFacts,
    retainedTransitionPublication,
    retainedTransitionIncomingStrength,
    retainedTransitionWinnerBefore,
    retainedTransitionWinnerAfter,
    retainedTransitionLogicalFactBefore,
    retainedTransitionLogicalFactAfter,
    storedPublication,
    storedStrength,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Domain.Identity (SortId)
import Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationLogicalState,
    checkedPublicationKey,
    checkedPublicationLogicalState,
    checkedPublicationSort,
    checkedPublicationWinnerKey,
  )
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Sort.Descriptor (ObjectKey)

-- | A visible or retained instance.  Provenance and effective strength are
-- separate: equal logical state selects the newer provenance while strength is
-- joined monotonically.
data StoredInstance = StoredInstance
  { publication :: CheckedPublication,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Show)

-- | One delta store, permanently tied to one sort.
--
-- Constructors stay private so visible, retained, and historical strength cannot
-- drift apart.
data DeltaStore = DeltaStore
  { sortId :: SortId,
    visible :: Map ObjectKey StoredInstance,
    retained :: Map ObjectKey StoredInstance,
    logicalStrengths :: Map PublicationLogicalState StoredInstance
  }
  deriving stock (Eq, Show)

-- | The complete transport-independent hidden state of one store.
--
-- The winner projection is retained explicitly for inspection and canonical
-- transfer.  The independent logical-state projection also retains one checked
-- publication witness: provenance and strength are separate max operations, so
-- even a losing state can later contribute its historical strength when that
-- logical state wins.
data RetainedSnapshot = RetainedSnapshot
  { snapshotSort :: SortId,
    snapshotWinners :: Map ObjectKey StoredInstance,
    snapshotLogicalStrengths :: Map PublicationLogicalState StoredInstance
  }
  deriving stock (Eq, Show)

-- | Rejection while rebuilding an opaque snapshot from checked publication
-- facts received by an alignment boundary.
data RetainedSnapshotError
  = RetainedSnapshotSortMismatch SortId SortId
  | RetainedSnapshotDuplicateLogicalState PublicationLogicalState
  deriving stock (Eq, Show)

-- | Whether one ordinary winner-law application changed hidden retained state.
-- Visible-only changes, including re-strengthening a visible copy after a local
-- take/reappearance history, deliberately do not create a retained transition.
data RetainedApplyResult
  = NoRetainedChange
  | RetainedChanged RetainedTransition
  deriving stock (Eq, Show)

-- | One exact replayable retained transition.  The triggering observation is
-- preserved alongside both independently joined projections before and after
-- the application.
data RetainedTransition = RetainedTransition
  { observation :: StoredInstance,
    winnerBefore :: Maybe StoredInstance,
    winnerAfter :: Maybe StoredInstance,
    logicalFactBefore :: Maybe StoredInstance,
    logicalFactAfter :: StoredInstance
  }
  deriving stock (Eq, Show)

-- | Applying a publication can fail only when its checked sort contradicts the
-- store's fixed sort.
data StoreError = StoreSortMismatch SortId SortId
  deriving stock (Eq, Show)

emptyDeltaStore :: SortId -> DeltaStore
emptyDeltaStore sortId =
  DeltaStore
    { sortId = sortId,
      visible = Map.empty,
      retained = Map.empty,
      logicalStrengths = Map.empty
    }

deltaStoreSort :: DeltaStore -> SortId
deltaStoreSort store = store.sortId

storedPublication :: StoredInstance -> CheckedPublication
storedPublication stored = stored.publication

storedStrength :: StoredInstance -> ReplicaStrength
storedStrength stored = stored.strength

-- | Apply one checked observation.
--
-- The retained winner key is a max operation.  For equal logical state,
-- provenance and strength form independent max operations.  A hidden value is
-- re-exposed only when the candidate strictly advances provenance; its visible
-- strength then starts at the incoming strength rather than inheriting hidden
-- normal possession.
applyPublication ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  Either StoreError DeltaStore
applyPublication incomingStrength candidate store =
  fst <$> applyPublicationDetailed incomingStrength candidate store

-- | Apply one checked observation and report its exact hidden-state delta.
--
-- The logical-state projection joins the greatest publication provenance and
-- strength independently.  Its update happens before winner evaluation so a
-- newly winning provenance inherits all strength previously observed for that
-- same logical state.  The result reports no retained change only when neither
-- retained projection changed; the visible projection may still have changed.
applyPublicationDetailed ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  Either StoreError (DeltaStore, RetainedApplyResult)
applyPublicationDetailed incomingStrength candidate store
  | checkedPublicationSort candidate /= store.sortId =
      Left
        ( StoreSortMismatch
            store.sortId
            (checkedPublicationSort candidate)
        )
  | otherwise =
      Right (successor, result)
  where
    key = checkedPublicationKey candidate
    logicalState = checkedPublicationLogicalState candidate
    beforeWinner = Map.lookup key store.retained
    beforeLogicalFact = Map.lookup logicalState store.logicalStrengths
    withLogicalFact =
      store
        { logicalStrengths =
            Map.insert
              logicalState
              (joinLogicalFact incomingStrength candidate beforeLogicalFact)
              store.logicalStrengths
        }
    successor = applyChecked incomingStrength candidate withLogicalFact
    afterWinner = Map.lookup key successor.retained
    afterLogicalFact =
      Map.findWithDefault
        (storeAtStrength candidate incomingStrength)
        logicalState
        successor.logicalStrengths
    result
      | beforeWinner == afterWinner
          && beforeLogicalFact == Just afterLogicalFact =
          NoRetainedChange
      | otherwise =
          RetainedChanged
            RetainedTransition
              { observation = storeAtStrength candidate incomingStrength,
                winnerBefore = beforeWinner,
                winnerAfter = afterWinner,
                logicalFactBefore = beforeLogicalFact,
                logicalFactAfter = afterLogicalFact
              }

-- | Rebuild a canonical hidden snapshot from one unique checked witness per
-- logical state.  Applying those facts to an empty store through the ordinary
-- winner law derives (and therefore validates) the retained-winner projection.
mkRetainedSnapshot ::
  SortId ->
  [(CheckedPublication, ReplicaStrength)] ->
  Either RetainedSnapshotError RetainedSnapshot
mkRetainedSnapshot sortId facts = do
  logicalFacts <- foldM insertFact Map.empty facts
  rebuilt <- foldM applyFact (emptyDeltaStore sortId) (Map.elems logicalFacts)
  Right (retainedSnapshot rebuilt)
  where
    insertFact retainedFacts (publication, strength)
      | checkedPublicationSort publication /= sortId =
          Left
            ( RetainedSnapshotSortMismatch
                sortId
                (checkedPublicationSort publication)
            )
      | Map.member logicalState retainedFacts =
          Left (RetainedSnapshotDuplicateLogicalState logicalState)
      | otherwise =
          Right
            ( Map.insert
                logicalState
                (storeAtStrength publication strength)
                retainedFacts
            )
      where
        logicalState = checkedPublicationLogicalState publication
    applyFact rebuilt fact =
      case applyPublication
        (storedStrength fact)
        (storedPublication fact)
        rebuilt of
        Left (StoreSortMismatch _ actual) ->
          Left (RetainedSnapshotSortMismatch sortId actual)
        Right successor -> Right successor

-- | Capture only hidden retained state.  Local visibility and local-take state
-- are intentionally absent.
retainedSnapshot :: DeltaStore -> RetainedSnapshot
retainedSnapshot store =
  RetainedSnapshot
    { snapshotSort = store.sortId,
      snapshotWinners = store.retained,
      snapshotLogicalStrengths = store.logicalStrengths
    }

retainedSnapshotSort :: RetainedSnapshot -> SortId
retainedSnapshotSort snapshot = snapshot.snapshotSort

-- | Canonical object-key order, with full checked provenance and strength.
retainedSnapshotWinnerFacts ::
  RetainedSnapshot ->
  [(CheckedPublication, ReplicaStrength)]
retainedSnapshotWinnerFacts snapshot =
  fmap storedFact (Map.elems snapshot.snapshotWinners)

-- | Canonical logical-state order, with the greatest checked publication
-- witness and greatest observed strength for each state.
retainedSnapshotLogicalStrengthFacts ::
  RetainedSnapshot ->
  [(CheckedPublication, ReplicaStrength)]
retainedSnapshotLogicalStrengthFacts snapshot =
  fmap storedFact (Map.elems snapshot.snapshotLogicalStrengths)

-- | Merge a checked hidden snapshot through the same ordinary winner law as a
-- live publication.  The returned transitions are in canonical logical-state
-- order and contain only actual retained changes at the destination.
applyRetainedSnapshot ::
  RetainedSnapshot ->
  DeltaStore ->
  Either StoreError (DeltaStore, [RetainedTransition])
applyRetainedSnapshot snapshot store
  | snapshot.snapshotSort /= store.sortId =
      Left (StoreSortMismatch store.sortId snapshot.snapshotSort)
  | otherwise =
      foldM applyFact (store, []) (Map.elems snapshot.snapshotLogicalStrengths)
  where
    applyFact (current, transitions) fact = do
      (successor, result) <-
        applyPublicationDetailed
          (storedStrength fact)
          (storedPublication fact)
          current
      Right
        ( successor,
          case result of
            NoRetainedChange -> transitions
            RetainedChanged transition -> transitions <> [transition]
        )

retainedTransitionPublication :: RetainedTransition -> CheckedPublication
retainedTransitionPublication = storedPublication . (.observation)

retainedTransitionIncomingStrength :: RetainedTransition -> ReplicaStrength
retainedTransitionIncomingStrength = storedStrength . (.observation)

retainedTransitionWinnerBefore :: RetainedTransition -> Maybe StoredInstance
retainedTransitionWinnerBefore transition = transition.winnerBefore

retainedTransitionWinnerAfter :: RetainedTransition -> Maybe StoredInstance
retainedTransitionWinnerAfter transition = transition.winnerAfter

retainedTransitionLogicalFactBefore ::
  RetainedTransition ->
  Maybe StoredInstance
retainedTransitionLogicalFactBefore transition = transition.logicalFactBefore

retainedTransitionLogicalFactAfter :: RetainedTransition -> StoredInstance
retainedTransitionLogicalFactAfter transition = transition.logicalFactAfter

-- | Atomically return and remove one visible object. Retained winner,
-- suppression, and joined historical strength are unchanged.
localTake :: ObjectKey -> DeltaStore -> (Maybe StoredInstance, DeltaStore)
localTake key store =
  ( Map.lookup key store.visible,
    store {visible = Map.delete key store.visible}
  )

-- | Remove every live or hidden fact for one object key while retaining no
-- capability for an older observation to reconstruct it.  The returned count
-- is the number of entries removed across the visible winner, retained winner,
-- and per-logical-state strength projections.
--
-- Terminal suppression is owned by the Herald Store slot rather than this
-- transport-independent winner law.  Consequently this operation is an exact,
-- idempotent physical purge; callers must separately remember that later
-- observations for the key are terminally ignored.
purgeObjectKey :: ObjectKey -> DeltaStore -> (DeltaStore, Word64)
purgeObjectKey key store =
  ( store
      { visible = visible,
        retained = retained,
        logicalStrengths = logicalStrengths
      },
    removedCount
  )
  where
    visible = Map.delete key store.visible
    retained = Map.delete key store.retained
    logicalStrengths =
      Map.filter
        ((/= key) . checkedPublicationKey . storedPublication)
        store.logicalStrengths
    removedCount =
      fromIntegral
        ( Map.size store.visible
            - Map.size visible
            + Map.size store.retained
            - Map.size retained
            + Map.size store.logicalStrengths
            - Map.size logicalStrengths
        )

lookupVisible :: ObjectKey -> DeltaStore -> Maybe StoredInstance
lookupVisible key store = Map.lookup key store.visible

lookupRetained :: ObjectKey -> DeltaStore -> Maybe StoredInstance
lookupRetained key store = Map.lookup key store.retained

visibleInstances :: DeltaStore -> [(ObjectKey, StoredInstance)]
visibleInstances store = Map.toAscList store.visible

retainedInstances :: DeltaStore -> [(ObjectKey, StoredInstance)]
retainedInstances store = Map.toAscList store.retained

-- | Greatest strength observed for one logical state, including observations
-- that did not win at the time they arrived.
logicalStateStrength ::
  PublicationLogicalState ->
  DeltaStore ->
  Maybe ReplicaStrength
logicalStateStrength logicalState store =
  storedStrength <$> Map.lookup logicalState store.logicalStrengths

applyChecked ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  DeltaStore
applyChecked incomingStrength candidate store =
  case Map.lookup key store.retained of
    Nothing -> installFresh incomingStrength candidate store
    Just incumbent ->
      case compare
        (checkedPublicationWinnerKey candidate)
        (checkedPublicationWinnerKey (storedPublication incumbent)) of
        GT -> installWinner incomingStrength incumbent candidate store
        _ -> joinLosingObservation incomingStrength incumbent candidate store
  where
    key = checkedPublicationKey candidate

installFresh ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  DeltaStore
installFresh incomingStrength candidate store =
  store
    { visible = Map.insert key stored store.visible,
      retained = Map.insert key stored store.retained
    }
  where
    key = checkedPublicationKey candidate
    stored =
      storeAtStrength
        candidate
        (retainedStrength incomingStrength candidate store)

installWinner ::
  ReplicaStrength ->
  StoredInstance ->
  CheckedPublication ->
  DeltaStore ->
  DeltaStore
installWinner incomingStrength incumbent candidate store =
  store
    { visible =
        Map.insert
          key
          visibleWinner
          store.visible,
      retained =
        Map.insert
          key
          retainedWinner
          store.retained
    }
  where
    key = checkedPublicationKey candidate
    sameLogicalState =
      checkedPublicationLogicalState (storedPublication incumbent)
        == checkedPublicationLogicalState candidate
    joinedStrength = retainedStrength incomingStrength candidate store
    retainedWinner = storeAtStrength candidate joinedStrength
    visibleWinner =
      case Map.lookup key store.visible of
        Nothing ->
          -- Advancing provenance after local take re-exposes at only the
          -- incoming strength.
          storeAtStrength candidate incomingStrength
        Just visible
          | sameLogicalState ->
              storeAtStrength
                candidate
                (max (storedStrength visible) incomingStrength)
          | otherwise -> storeAtStrength candidate joinedStrength

joinLosingObservation ::
  ReplicaStrength ->
  StoredInstance ->
  CheckedPublication ->
  DeltaStore ->
  DeltaStore
joinLosingObservation incomingStrength incumbent candidate store
  | checkedPublicationLogicalState (storedPublication incumbent)
      /= checkedPublicationLogicalState candidate =
      store
  | otherwise =
      store
        { visible = Map.adjust joinVisible key store.visible,
          retained =
            Map.insert key joinedRetained store.retained
        }
  where
    key = checkedPublicationKey candidate
    joinedRetained =
      storeAtStrength
        (storedPublication incumbent)
        (retainedStrength incomingStrength candidate store)
    joinVisible visible =
      storeAtStrength
        (storedPublication visible)
        (max (storedStrength visible) incomingStrength)

retainedStrength ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  ReplicaStrength
retainedStrength incomingStrength publication store =
  maybe
    incomingStrength
    storedStrength
    (Map.lookup (checkedPublicationLogicalState publication) store.logicalStrengths)

joinLogicalFact ::
  ReplicaStrength ->
  CheckedPublication ->
  Maybe StoredInstance ->
  StoredInstance
joinLogicalFact incomingStrength candidate incumbent = case incumbent of
  Nothing -> storeAtStrength candidate incomingStrength
  Just retained ->
    storeAtStrength
      ( greaterPublication
          (storedPublication retained)
          candidate
      )
      (max (storedStrength retained) incomingStrength)

greaterPublication ::
  CheckedPublication ->
  CheckedPublication ->
  CheckedPublication
greaterPublication left right
  | checkedPublicationWinnerKey left >= checkedPublicationWinnerKey right = left
  | otherwise = right

storedFact :: StoredInstance -> (CheckedPublication, ReplicaStrength)
storedFact stored = (storedPublication stored, storedStrength stored)

storeAtStrength :: CheckedPublication -> ReplicaStrength -> StoredInstance
storeAtStrength publication strength =
  StoredInstance publication strength
