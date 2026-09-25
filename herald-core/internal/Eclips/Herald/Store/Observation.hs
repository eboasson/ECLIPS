{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Live label-aware Store observation preparations.
--
-- The live Store continues to retain immutable raw publications.  This module
-- captures an exact raw candidate/history cut without evaluating a predicate,
-- then requires a coordinator-supplied effective view for every captured item.
-- Hidden items are filtered before predicate evaluation and historical change
-- revisions remain contiguous through explicit terminal-ignore dispositions.
-- These adapters cover Store-backed read and wait plans, local take, snapshots,
-- and retained alignment changes; none accepts an unprojected value.
module Eclips.Herald.Store.Observation
  ( EffectiveCandidateKey,
    effectiveCandidateKeyDelta,
    effectiveCandidateKeyIncarnation,
    effectiveCandidateKeyObjectKey,
    effectiveCandidateKeyPublicationId,
    EffectiveQueryCut,
    captureEffectiveQueryCut,
    effectiveQueryCutKeys,
    effectiveQueryCutRawCandidates,
    EffectiveStoreEvidence,
    effectiveStoreEvidence,
    EffectiveStoreMatch,
    effectiveStoreMatchKey,
    effectiveStoreMatchStrength,
    effectiveStoreMatchRawPublication,
    effectiveStoreMatchValue,
    EffectiveStoreProblem (..),
    evaluateEffectiveQueryCut,
    EffectiveReadPlan,
    prepareEffectiveReadPlan,
    effectiveReadMatches,
    EffectiveWaitPlan,
    prepareEffectiveWaitPlan,
    effectiveWaitMatches,
    EffectiveLocalTakePlan,
    prepareEffectiveLocalTakePlan,
    effectiveLocalTakeMatches,
    effectiveLocalTakeKeys,
    SnapshotEvidenceRole (..),
    EffectiveSnapshotEvidenceKey,
    effectiveSnapshotEvidenceKeys,
    effectiveSnapshotRawEvidence,
    EffectiveSnapshotFact,
    effectiveSnapshotFactRepresentative,
    effectiveSnapshotFactStrengthWitness,
    effectiveSnapshotFactValue,
    effectiveSnapshotFactStrength,
    EffectiveSnapshot,
    effectiveSnapshotIncarnation,
    effectiveSnapshotRevision,
    effectiveSnapshotSortId,
    effectiveSnapshotSortOccurrence,
    effectiveSnapshotFacts,
    projectEffectiveSnapshot,
    filterEffectiveSnapshot,
    EffectiveChangeKey,
    effectiveChangeKeys,
    effectiveChangeRawEvidence,
    EffectiveChangeDisposition (..),
    projectEffectiveChanges,
  )
where

import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment (StoreRevision)
import Eclips.Domain.Identity
  ( DeltaId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationWinnerKey,
  )
import Eclips.Domain.Query (CheckedQueryPredicate)
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Sort.Descriptor (ObjectKey)
import Eclips.Domain.Store
  ( storedPublication,
    storedStrength,
    visibleInstances,
  )
import Eclips.Domain.Value (Value)
import Eclips.Herald.EffectivePublication
  ( EffectivePublicationView,
    PublicationHiddenReason,
    effectivePublicationBindsRaw,
    effectivePublicationHiddenReason,
    effectivePublicationMatchesQuery,
    effectivePublicationVisiblePair,
  )
import Eclips.Herald.Query
  ( ResolvedQuery,
    resolvedQueryBranchDelta,
    resolvedQueryBranchPredicate,
    resolvedQueryBranchStoreIncarnation,
    resolvedQueryBranches,
  )
import Eclips.Herald.Store.State
  ( ExactLocalTakeKey,
    RetainedStoreChange,
    RetainedStoreObservation,
    RetainedStoreSnapshot,
    State,
    exactLocalTakeKey,
    lookupStoreSlot,
    retainedStoreChangeObservation,
    retainedStoreChangeRevision,
    retainedStoreObservationPublication,
    retainedStoreSnapshotFactRepresentative,
    retainedStoreSnapshotFactStrength,
    retainedStoreSnapshotFactStrengthWitness,
    retainedStoreSnapshotFacts,
    retainedStoreSnapshotIncarnation,
    retainedStoreSnapshotRevision,
    retainedStoreSnapshotSortId,
    retainedStoreSnapshotSortOccurrence,
    storeSlotContents,
    storeSlotIncarnation,
  )

data EffectiveCandidateKey = EffectiveCandidateKey
  { delta :: DeltaId,
    incarnation :: StoreIncarnationId,
    objectKey :: ObjectKey,
    publicationId :: PublicationId
  }
  deriving stock (Eq, Ord, Show)

effectiveCandidateKeyDelta :: EffectiveCandidateKey -> DeltaId
effectiveCandidateKeyDelta key = key.delta

effectiveCandidateKeyIncarnation :: EffectiveCandidateKey -> StoreIncarnationId
effectiveCandidateKeyIncarnation key = key.incarnation

effectiveCandidateKeyObjectKey :: EffectiveCandidateKey -> ObjectKey
effectiveCandidateKeyObjectKey key = key.objectKey

effectiveCandidateKeyPublicationId :: EffectiveCandidateKey -> PublicationId
effectiveCandidateKeyPublicationId key = key.publicationId

data QueryCandidate = QueryCandidate
  { key :: EffectiveCandidateKey,
    strength :: ReplicaStrength,
    publication :: CheckedPublication,
    predicate :: CheckedQueryPredicate
  }

newtype EffectiveQueryCut = EffectiveQueryCut (Map EffectiveCandidateKey QueryCandidate)

data EffectiveStoreProblem
  = EffectiveStoreDeltaMissing DeltaId
  | EffectiveStoreIncarnationMismatch
      DeltaId
      StoreIncarnationId
      StoreIncarnationId
  | EffectiveStoreDuplicateCandidate EffectiveCandidateKey
  | EffectiveStoreEvidenceRawMismatch
  | EffectiveStoreViewKeySetMismatch
      (Set EffectiveCandidateKey)
      (Set EffectiveCandidateKey)
  | EffectiveStoreVisibleRawMismatch EffectiveCandidateKey
  | EffectiveStoreQueryContradiction EffectiveCandidateKey
  | EffectiveStoreSnapshotViewKeySetMismatch
      (Set EffectiveSnapshotEvidenceKey)
      (Set EffectiveSnapshotEvidenceKey)
  | EffectiveStoreSnapshotVisibleRawMismatch EffectiveSnapshotEvidenceKey
  | EffectiveStoreSnapshotVisibilityMismatch
      EffectiveSnapshotEvidenceKey
      EffectiveSnapshotEvidenceKey
  | EffectiveStoreSnapshotStrengthValueMismatch EffectiveSnapshotEvidenceKey
  | EffectiveStoreChangeViewKeySetMismatch
      (Set EffectiveChangeKey)
      (Set EffectiveChangeKey)
  | EffectiveStoreChangeVisibleRawMismatch EffectiveChangeKey
  deriving stock (Eq, Show)

-- | An explicitly bound raw/effective pair.  Hidden views intentionally do
-- not expose raw provenance, so the Store seam carries it separately and
-- checks it against the captured candidate before interpreting visibility.
data EffectiveStoreEvidence = EffectiveStoreEvidence
  { rawPublication :: CheckedPublication,
    view :: EffectivePublicationView
  }
  deriving stock (Eq, Show)

effectiveStoreEvidence ::
  CheckedPublication ->
  EffectivePublicationView ->
  Either EffectiveStoreProblem EffectiveStoreEvidence
effectiveStoreEvidence raw view = do
  unless
    (effectivePublicationBindsRaw raw view)
    (Left EffectiveStoreEvidenceRawMismatch)
  Right (EffectiveStoreEvidence raw view)

-- | Capture every visible candidate in the exact resolved branches without
-- running the branch predicate against the embedded value.
captureEffectiveQueryCut ::
  ResolvedQuery -> State -> Either EffectiveStoreProblem EffectiveQueryCut
captureEffectiveQueryCut query state = do
  candidates <- concat <$> traverse captureBranch (resolvedQueryBranches query)
  candidateMap <- foldl insertCandidate (Right Map.empty) candidates
  Right (EffectiveQueryCut candidateMap)
  where
    captureBranch branch = do
      let delta = resolvedQueryBranchDelta branch
          expectedIncarnation = resolvedQueryBranchStoreIncarnation branch
      slot <-
        maybe
          (Left (EffectiveStoreDeltaMissing delta))
          Right
          (lookupStoreSlot delta state)
      let actualIncarnation = storeSlotIncarnation slot
      unless
        (actualIncarnation == expectedIncarnation)
        ( Left
            ( EffectiveStoreIncarnationMismatch
                delta
                expectedIncarnation
                actualIncarnation
            )
        )
      Right
        [ QueryCandidate
            { key =
                EffectiveCandidateKey
                  delta
                  actualIncarnation
                  objectKey
                  (checkedPublicationId publication),
              strength = storedStrength stored,
              publication,
              predicate = resolvedQueryBranchPredicate branch
            }
        | (objectKey, stored) <- visibleInstances (storeSlotContents slot),
          let publication = storedPublication stored
        ]

    insertCandidate accumulated candidate = do
      candidates <- accumulated
      if Map.member candidate.key candidates
        then Left (EffectiveStoreDuplicateCandidate candidate.key)
        else Right (Map.insert candidate.key candidate candidates)

effectiveQueryCutKeys :: EffectiveQueryCut -> Set EffectiveCandidateKey
effectiveQueryCutKeys (EffectiveQueryCut candidates) = Map.keysSet candidates

effectiveQueryCutRawCandidates ::
  EffectiveQueryCut -> [(EffectiveCandidateKey, CheckedPublication)]
effectiveQueryCutRawCandidates (EffectiveQueryCut candidates) =
  [ (key, candidate.publication)
  | (key, candidate) <- Map.toAscList candidates
  ]

data EffectiveStoreMatch = EffectiveStoreMatch
  { key :: EffectiveCandidateKey,
    strength :: ReplicaStrength,
    rawPublication :: CheckedPublication,
    value :: Value
  }
  deriving stock (Eq, Show)

effectiveStoreMatchKey :: EffectiveStoreMatch -> EffectiveCandidateKey
effectiveStoreMatchKey match = match.key

effectiveStoreMatchStrength :: EffectiveStoreMatch -> ReplicaStrength
effectiveStoreMatchStrength match = match.strength

effectiveStoreMatchRawPublication :: EffectiveStoreMatch -> CheckedPublication
effectiveStoreMatchRawPublication match = match.rawPublication

effectiveStoreMatchValue :: EffectiveStoreMatch -> Value
effectiveStoreMatchValue match = match.value

evaluateEffectiveQueryCut ::
  Map EffectiveCandidateKey EffectiveStoreEvidence ->
  EffectiveQueryCut ->
  Either EffectiveStoreProblem [EffectiveStoreMatch]
evaluateEffectiveQueryCut supplied (EffectiveQueryCut candidates) = do
  requireExactKeys
    EffectiveStoreViewKeySetMismatch
    (Map.keysSet candidates)
    (Map.keysSet supplied)
  fmap concat . traverse evaluateOne $ Map.toAscList candidates
  where
    evaluateOne (key, candidate) = do
      let evidence = supplied Map.! key
          view = evidence.view
      unless
        (evidence.rawPublication == candidate.publication)
        (Left (EffectiveStoreVisibleRawMismatch key))
      case effectivePublicationVisiblePair view of
        Nothing -> Right []
        Just (raw, effectiveValue) -> do
          unless
            (raw == candidate.publication)
            (Left (EffectiveStoreVisibleRawMismatch key))
          matches <-
            either
              (const (Left (EffectiveStoreQueryContradiction key)))
              Right
              (effectivePublicationMatchesQuery candidate.predicate view)
          Right
            [ EffectiveStoreMatch key candidate.strength raw effectiveValue
            | matches
            ]

newtype EffectiveReadPlan = EffectiveReadPlan [EffectiveStoreMatch]
  deriving stock (Eq, Show)

-- | Application read/result localization prepared only from the projected
-- candidate cut.
prepareEffectiveReadPlan ::
  Map EffectiveCandidateKey EffectiveStoreEvidence ->
  EffectiveQueryCut ->
  Either EffectiveStoreProblem EffectiveReadPlan
prepareEffectiveReadPlan supplied cut =
  EffectiveReadPlan <$> evaluateEffectiveQueryCut supplied cut

effectiveReadMatches :: EffectiveReadPlan -> [EffectiveStoreMatch]
effectiveReadMatches (EffectiveReadPlan matches) = matches

newtype EffectiveWaitPlan = EffectiveWaitPlan [EffectiveStoreMatch]
  deriving stock (Eq, Show)

-- | Wait readiness is evaluated against the same effective values and hidden
-- precedence as read/take, but retained as a distinct preparation boundary.
prepareEffectiveWaitPlan ::
  Map EffectiveCandidateKey EffectiveStoreEvidence ->
  EffectiveQueryCut ->
  Either EffectiveStoreProblem EffectiveWaitPlan
prepareEffectiveWaitPlan supplied cut =
  EffectiveWaitPlan <$> evaluateEffectiveQueryCut supplied cut

effectiveWaitMatches :: EffectiveWaitPlan -> [EffectiveStoreMatch]
effectiveWaitMatches (EffectiveWaitPlan matches) = matches

newtype EffectiveLocalTakePlan = EffectiveLocalTakePlan [EffectiveStoreMatch]
  deriving stock (Eq, Show)

-- | Prepare the exact keys which the live Store owner will hide.  Projection
-- remains separate from mutation so no hidden candidate can reach the owner.
prepareEffectiveLocalTakePlan ::
  Map EffectiveCandidateKey EffectiveStoreEvidence ->
  EffectiveQueryCut ->
  Either EffectiveStoreProblem EffectiveLocalTakePlan
prepareEffectiveLocalTakePlan supplied cut =
  EffectiveLocalTakePlan
    <$> evaluateEffectiveQueryCut supplied cut

effectiveLocalTakeMatches :: EffectiveLocalTakePlan -> [EffectiveStoreMatch]
effectiveLocalTakeMatches (EffectiveLocalTakePlan matches) = matches

-- | Exact raw keys to recheck and hide in the Store owner after effective
-- visibility and predicate evaluation selected the take result.
effectiveLocalTakeKeys :: EffectiveLocalTakePlan -> [ExactLocalTakeKey]
effectiveLocalTakeKeys (EffectiveLocalTakePlan matches) =
  [ exactLocalTakeKey
      match.key.delta
      match.key.incarnation
      match.key.objectKey
      match.rawPublication
  | match <- matches
  ]

data SnapshotEvidenceRole
  = SnapshotRepresentative
  | SnapshotStrengthWitness
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data EffectiveSnapshotEvidenceKey = EffectiveSnapshotEvidenceKey
  { ordinal :: Int,
    role :: SnapshotEvidenceRole,
    publicationId :: PublicationId
  }
  deriving stock (Eq, Ord, Show)

snapshotEvidence ::
  Int -> SnapshotEvidenceRole -> RetainedStoreObservation -> EffectiveSnapshotEvidenceKey
snapshotEvidence ordinal role observation =
  EffectiveSnapshotEvidenceKey
    ordinal
    role
    (checkedPublicationId (retainedStoreObservationPublication observation))

effectiveSnapshotEvidenceKeys ::
  RetainedStoreSnapshot -> Set EffectiveSnapshotEvidenceKey
effectiveSnapshotEvidenceKeys snapshot =
  Set.fromList
    [ key
    | (ordinal, fact) <- zip [0 ..] (retainedStoreSnapshotFacts snapshot),
      key <-
        [ snapshotEvidence
            ordinal
            SnapshotRepresentative
            (retainedStoreSnapshotFactRepresentative fact),
          snapshotEvidence
            ordinal
            SnapshotStrengthWitness
            (retainedStoreSnapshotFactStrengthWitness fact)
        ]
    ]

effectiveSnapshotRawEvidence ::
  RetainedStoreSnapshot ->
  [(EffectiveSnapshotEvidenceKey, CheckedPublication)]
effectiveSnapshotRawEvidence snapshot =
  [ (snapshotEvidence ordinal role observation, retainedStoreObservationPublication observation)
  | (ordinal, fact) <- zip [0 ..] (retainedStoreSnapshotFacts snapshot),
    (role, observation) <-
      [ (SnapshotRepresentative, retainedStoreSnapshotFactRepresentative fact),
        (SnapshotStrengthWitness, retainedStoreSnapshotFactStrengthWitness fact)
      ]
  ]

data EffectiveSnapshotFact = EffectiveSnapshotFact
  { representative :: RetainedStoreObservation,
    strengthWitness :: RetainedStoreObservation,
    value :: Value,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Show)

effectiveSnapshotFactRepresentative ::
  EffectiveSnapshotFact -> RetainedStoreObservation
effectiveSnapshotFactRepresentative fact = fact.representative

effectiveSnapshotFactStrengthWitness ::
  EffectiveSnapshotFact -> RetainedStoreObservation
effectiveSnapshotFactStrengthWitness fact = fact.strengthWitness

effectiveSnapshotFactValue :: EffectiveSnapshotFact -> Value
effectiveSnapshotFactValue fact = fact.value

effectiveSnapshotFactStrength :: EffectiveSnapshotFact -> ReplicaStrength
effectiveSnapshotFactStrength fact = fact.strength

data EffectiveSnapshot = EffectiveSnapshot
  { incarnation :: StoreIncarnationId,
    revision :: StoreRevision,
    sortId :: SortId,
    sortOccurrence :: SortDefinitionOccurrenceId,
    facts :: [EffectiveSnapshotFact]
  }
  deriving stock (Eq, Show)

effectiveSnapshotIncarnation :: EffectiveSnapshot -> StoreIncarnationId
effectiveSnapshotIncarnation snapshot = snapshot.incarnation

effectiveSnapshotRevision :: EffectiveSnapshot -> StoreRevision
effectiveSnapshotRevision snapshot = snapshot.revision

effectiveSnapshotSortId :: EffectiveSnapshot -> SortId
effectiveSnapshotSortId snapshot = snapshot.sortId

effectiveSnapshotSortOccurrence ::
  EffectiveSnapshot -> SortDefinitionOccurrenceId
effectiveSnapshotSortOccurrence snapshot = snapshot.sortOccurrence

effectiveSnapshotFacts :: EffectiveSnapshot -> [EffectiveSnapshotFact]
effectiveSnapshotFacts snapshot = snapshot.facts

-- | Project hidden snapshot facts away while preserving the raw historical
-- revision.  Facts which collapse to the same effective value join strength.
-- Representative provenance remains the greatest publication winner, while
-- the independent strength witness remains an observation which actually
-- supplied the joined strength; the winner is never relabelled as that witness.
projectEffectiveSnapshot ::
  Map EffectiveSnapshotEvidenceKey EffectiveStoreEvidence ->
  RetainedStoreSnapshot ->
  Either EffectiveStoreProblem EffectiveSnapshot
projectEffectiveSnapshot supplied snapshot =
  projectSnapshotWith collapse supplied snapshot
  where
    collapse projected =
      Map.elems
        ( Map.fromListWith
            mergeSnapshotFact
            [(fact.value, fact) | fact <- projected]
        )

-- | Filter hidden facts for raw alignment transport without combining
-- distinct authenticated raw values which happen to share one effective
-- projection. The raw history partition remains intact for re-admission.
filterEffectiveSnapshot ::
  Map EffectiveSnapshotEvidenceKey EffectiveStoreEvidence ->
  RetainedStoreSnapshot ->
  Either EffectiveStoreProblem EffectiveSnapshot
filterEffectiveSnapshot = projectSnapshotWith id

projectSnapshotWith ::
  ([EffectiveSnapshotFact] -> [EffectiveSnapshotFact]) ->
  Map EffectiveSnapshotEvidenceKey EffectiveStoreEvidence ->
  RetainedStoreSnapshot ->
  Either EffectiveStoreProblem EffectiveSnapshot
projectSnapshotWith normalize supplied snapshot = do
  let expectedKeys = effectiveSnapshotEvidenceKeys snapshot
  requireExactKeys
    EffectiveStoreSnapshotViewKeySetMismatch
    expectedKeys
    (Map.keysSet supplied)
  projected <-
    fmap concat . traverse projectOne
      $ zip [0 ..] (retainedStoreSnapshotFacts snapshot)
  Right
    EffectiveSnapshot
      { incarnation = retainedStoreSnapshotIncarnation snapshot,
        revision = retainedStoreSnapshotRevision snapshot,
        sortId = retainedStoreSnapshotSortId snapshot,
        sortOccurrence = retainedStoreSnapshotSortOccurrence snapshot,
        facts = normalize projected
      }
  where
    projectOne (ordinal, fact) = do
      let representative = retainedStoreSnapshotFactRepresentative fact
          witness = retainedStoreSnapshotFactStrengthWitness fact
          representativeKey = snapshotEvidence ordinal SnapshotRepresentative representative
          witnessKey = snapshotEvidence ordinal SnapshotStrengthWitness witness
          representativeEvidence = supplied Map.! representativeKey
          witnessEvidence = supplied Map.! witnessKey
          representativeView = representativeEvidence.view
          witnessView = witnessEvidence.view
      unless
        (representativeEvidence.rawPublication == retainedStoreObservationPublication representative)
        (Left (EffectiveStoreSnapshotVisibleRawMismatch representativeKey))
      unless
        (witnessEvidence.rawPublication == retainedStoreObservationPublication witness)
        (Left (EffectiveStoreSnapshotVisibleRawMismatch witnessKey))
      case ( effectivePublicationVisiblePair representativeView,
             effectivePublicationVisiblePair witnessView
           ) of
        (Nothing, Nothing) -> Right []
        (Nothing, Just _) ->
          Left
            ( EffectiveStoreSnapshotVisibilityMismatch
                representativeKey
                witnessKey
            )
        (Just _, Nothing) ->
          Left
            ( EffectiveStoreSnapshotVisibilityMismatch
                representativeKey
                witnessKey
            )
        (Just (rawRepresentative, value), Just (rawWitness, witnessValue)) -> do
          unless
            (rawRepresentative == retainedStoreObservationPublication representative)
            (Left (EffectiveStoreSnapshotVisibleRawMismatch representativeKey))
          unless
            (rawWitness == retainedStoreObservationPublication witness)
            (Left (EffectiveStoreSnapshotVisibleRawMismatch witnessKey))
          unless
            (witnessValue == value)
            (Left (EffectiveStoreSnapshotStrengthValueMismatch witnessKey))
          Right
            [ EffectiveSnapshotFact
                representative
                witness
                value
                (retainedStoreSnapshotFactStrength fact)
            ]

mergeSnapshotFact :: EffectiveSnapshotFact -> EffectiveSnapshotFact -> EffectiveSnapshotFact
mergeSnapshotFact left right =
  EffectiveSnapshotFact
    { representative =
        if checkedPublicationWinnerKey (retainedStoreObservationPublication left.representative)
          >= checkedPublicationWinnerKey (retainedStoreObservationPublication right.representative)
          then left.representative
          else right.representative,
      strengthWitness = strongerWitness left right,
      value = left.value,
      strength = max left.strength right.strength
    }

strongerWitness ::
  EffectiveSnapshotFact -> EffectiveSnapshotFact -> RetainedStoreObservation
strongerWitness left right =
  case compare left.strength right.strength of
    GT -> left.strengthWitness
    LT -> right.strengthWitness
    EQ ->
      if checkedPublicationWinnerKey
        (retainedStoreObservationPublication left.strengthWitness)
        >= checkedPublicationWinnerKey
          (retainedStoreObservationPublication right.strengthWitness)
        then left.strengthWitness
        else right.strengthWitness

data EffectiveChangeKey = EffectiveChangeKey
  { revision :: StoreRevision,
    publicationId :: PublicationId
  }
  deriving stock (Eq, Ord, Show)

changeKey :: RetainedStoreChange -> EffectiveChangeKey
changeKey change =
  EffectiveChangeKey
    (retainedStoreChangeRevision change)
    ( checkedPublicationId
        (retainedStoreObservationPublication (retainedStoreChangeObservation change))
    )

effectiveChangeKeys :: [RetainedStoreChange] -> Set EffectiveChangeKey
effectiveChangeKeys = Set.fromList . fmap changeKey

effectiveChangeRawEvidence ::
  [RetainedStoreChange] -> [(EffectiveChangeKey, CheckedPublication)]
effectiveChangeRawEvidence changes =
  [ ( changeKey change,
      retainedStoreObservationPublication (retainedStoreChangeObservation change)
    )
  | change <- changes
  ]

data EffectiveChangeDisposition
  = EffectiveChangeVisible RetainedStoreChange Value
  | EffectiveChangeTerminallyIgnored
      RetainedStoreChange
      PublicationHiddenReason
  deriving stock (Eq, Show)

-- | Preserve one disposition for every raw change.  A hidden revision remains
-- acknowledged as a terminal no-op; later revisions are never renumbered.
projectEffectiveChanges ::
  Map EffectiveChangeKey EffectiveStoreEvidence ->
  [RetainedStoreChange] ->
  Either EffectiveStoreProblem [EffectiveChangeDisposition]
projectEffectiveChanges supplied changes = do
  let expectedKeys = effectiveChangeKeys changes
  requireExactKeys
    EffectiveStoreChangeViewKeySetMismatch
    expectedKeys
    (Map.keysSet supplied)
  traverse projectOne changes
  where
    projectOne change = do
      let key = changeKey change
          raw = retainedStoreObservationPublication (retainedStoreChangeObservation change)
          evidence = supplied Map.! key
          view = evidence.view
      unless
        (evidence.rawPublication == raw)
        (Left (EffectiveStoreChangeVisibleRawMismatch key))
      case effectivePublicationVisiblePair view of
        Just (observedRaw, value) -> do
          unless
            (observedRaw == raw)
            (Left (EffectiveStoreChangeVisibleRawMismatch key))
          Right (EffectiveChangeVisible change value)
        Nothing -> case effectivePublicationHiddenReason view of
          Just reason -> Right (EffectiveChangeTerminallyIgnored change reason)
          Nothing -> Left (EffectiveStoreChangeVisibleRawMismatch key)

requireExactKeys ::
  (Eq key) =>
  (Set key -> Set key -> problem) ->
  Set key ->
  Set key ->
  Either problem ()
requireExactKeys problem expected actual =
  unless (expected == actual) (Left (problem expected actual))
