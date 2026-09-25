{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module StoreObservationProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (StoreRevision, initialStoreRevision)
import Eclips.Domain.Identity
  ( GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    StoreIncarnationId,
    TopologyCutId,
    controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    PreparedLabelFacts,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    mkPreparedLabelFacts,
    preparedAuthorityDisposition,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateGeneration,
    releasedLabelStateView,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationLogicalState,
    checkedPublicationValue,
    checkedPublicationWinnerKey,
    mkCheckedPublication,
  )
import Eclips.Domain.Query qualified as Query
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    canonicalizeCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor),
    DescriptorSpec (..),
    PredicateExpression (..),
    RankDirection (Ascending),
    RankTerm (RankApplicationValue),
    SortKind (ControlledSort),
    checkDescriptor,
    descriptorSchema,
  )
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    PredefinedSortRole (SortDefinitionRole),
    mkInitialProjectionDigest,
    profileCatalogueDigest,
  )
import Eclips.Domain.Topology
  ( MemberSetDigest,
    deriveMemberSetDigest,
  )
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (..),
    Value,
    ValueView (..),
    directProjection,
    globalUniqueIdSchema,
    globalUniqueIdValue,
    int64Schema,
    int64Value,
    labelSchema,
    labelValue,
    mkFieldName,
    recordSchema,
    recordValue,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectivePublication
  ( EffectivePublicationProblem,
    EffectivePublicationView,
    PublicationHiddenReason (PublicationDeleted),
    checkEffectivePublicationContext,
    interpretEffectivePublication,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Query
  ( ResolvedQuery,
    resolvedQuery,
    resolvedQueryBranch,
  )
import Eclips.Herald.Store.Observation
import Eclips.Herald.Store.State qualified as Store
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureIdentifierBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    ioProperty,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "live label-aware Store observation"
    [ testCase "changed labels and delete govern raw query candidates" controlledQueryProjection,
      testCase "alignment change preparation preserves raw revisions" controlledChangeProjection,
      testCase "alignment snapshot preparation uses the same raw/effective binding" controlledSnapshotProjection,
      testCase "a hidden view cannot be rebound to another raw publication" hiddenViewRebind,
      testProperty "generated query/read/wait/take plans match an independent reference" propQueryFamilyReference,
      testProperty "generated alignment changes match an independent reference" propChangesReference,
      testProperty "source notifications coalesce exact changed retained prefixes" propSourcePrefixNotifications,
      testProperty "preparation slot notifications follow actual activation changes" propPreparationSlotChanges,
      testProperty "generated effective-only snapshot collapse preserves independent provenance" propSnapshotReference,
      testProperty "exact query/change evidence mutations are rejected" propEvidenceMutationReference,
      testProperty "exact snapshot key/raw evidence mutations are rejected" propSnapshotEvidenceMutation,
      testProperty "snapshot witness mutations are invariant errors" propSnapshotWitnessMutation
    ]

controlledQueryProjection :: Assertion
controlledQueryProjection = do
  fixture <- populatedControlledStore
  queryB <- resolvedLabelQuery fixture ((ProcessLabel processB, 1))
  queryA <- resolvedLabelQuery fixture ((ProcessLabel processA, 0))
  cutB <- checked "capture B query cut" (captureEffectiveQueryCut queryB fixture.state)
  cutA <- checked "capture A query cut" (captureEffectiveQueryCut queryA fixture.state)
  assertEqual "the immutable candidate coordinate is shared" (effectiveQueryCutKeys cutA) (effectiveQueryCutKeys cutB)
  assertEqual "one raw candidate was captured" 1 (Set.size (effectiveQueryCutKeys cutB))

  changed <- evidenceFor fixture.changedControlled (effectiveQueryCutRawCandidates cutB)
  matchesB <- checked "evaluate changed B query" (evaluateEffectiveQueryCut changed cutB)
  matchesA <- checked "evaluate old A query" (evaluateEffectiveQueryCut changed cutA)
  assertEqual "the changed label matches" 1 (length matchesB)
  assertEqual "the embedded label no longer matches" [] matchesA
  case matchesB of
    [match] -> do
      assertEqual "query returns the projected B label" (Just ((ProcessLabel processB, 1))) (labelFromValue (effectiveStoreMatchValue match))
      assertEqual "query retains raw publication provenance" fixture.publication (effectiveStoreMatchRawPublication match)
    _ -> assertFailure "expected exactly one changed-label match"

  deleted <- evidenceFor fixture.deletedControlled (effectiveQueryCutRawCandidates cutB)
  deletedMatches <- checked "evaluate deleted query" (evaluateEffectiveQueryCut deleted cutB)
  assertEqual "the supplied retained-overlay delete filters the raw candidate" [] deletedMatches
  readPlan <- checked "prepare changed-label read" (prepareEffectiveReadPlan changed cutB)
  waitPlan <- checked "prepare changed-label wait" (prepareEffectiveWaitPlan changed cutB)
  takePlan <- checked "prepare changed-label take" (prepareEffectiveLocalTakePlan changed cutB)
  assertEqual "read localizes the same projected cut" matchesB (effectiveReadMatches readPlan)
  assertEqual "wait observes the same projected cut" matchesB (effectiveWaitMatches waitPlan)
  assertEqual "take consumes the same projected cut" matchesB (effectiveLocalTakeMatches takePlan)
  preparedTake <-
    checked
      "prepare exact effective take"
      ( Store.prepareExactLocalTake
          (effectiveLocalTakeKeys takePlan)
          fixture.state
      )
  let (afterTake, takenRaw) = Store.commitExactLocalTake preparedTake
  assertEqual
    "the live Store hides the exact retained raw publication"
    [fixture.publication]
    takenRaw
  afterTakeCut <- checked "capture post-take cut" (captureEffectiveQueryCut queryB afterTake)
  assertEqual
    "the selected raw key is no longer visible"
    Set.empty
    (effectiveQueryCutKeys afterTakeCut)
  deletedRead <- checked "prepare deleted read" (prepareEffectiveReadPlan deleted cutB)
  deletedWait <- checked "prepare deleted wait" (prepareEffectiveWaitPlan deleted cutB)
  deletedTake <- checked "prepare deleted take" (prepareEffectiveLocalTakePlan deleted cutB)
  assertEqual "delete suppresses read" [] (effectiveReadMatches deletedRead)
  assertEqual "delete suppresses wait" [] (effectiveWaitMatches deletedWait)
  assertEqual "delete suppresses take" [] (effectiveLocalTakeMatches deletedTake)
  deletedPrepared <-
    checked
      "prepare empty deleted take"
      ( Store.prepareExactLocalTake
          (effectiveLocalTakeKeys deletedTake)
          fixture.state
      )
  let (afterDeletedTake, deletedRaw) = Store.commitExactLocalTake deletedPrepared
  assertEqual "delete exposes no raw take target" [] deletedRaw
  assertBool
    "delete never rewrites Store state"
    (fixture.state == afterDeletedTake)

controlledChangeProjection :: Assertion
controlledChangeProjection = do
  fixture <- populatedControlledStore
  changes <-
    checked
      "raw Store changes"
      ( Store.retainedStoreChangesAfter
          fixture.incarnation
          initialStoreRevision
          fixture.state
      )
  assertEqual "one raw retained change" 1 (length changes)
  changedEvidence <- evidenceFor fixture.changedControlled (effectiveChangeRawEvidence changes)
  changed <- checked "project changed-label history" (projectEffectiveChanges changedEvidence changes)
  case (changes, changed) of
    ([rawChange], [EffectiveChangeVisible projectedChange value]) -> do
      assertEqual "the raw change is retained exactly" rawChange projectedChange
      assertEqual "the value is projected under B" (Just ((ProcessLabel processB, 1))) (labelFromValue value)
      assertEqual
        "effective projection does not renumber the Store revision"
        (Store.retainedStoreChangeRevision rawChange)
        (Store.retainedStoreChangeRevision projectedChange)
    _ -> assertFailure ("unexpected visible change projection: " <> show changed)

  deletedEvidence <- evidenceFor fixture.deletedControlled (effectiveChangeRawEvidence changes)
  deleted <- checked "project deleted history" (projectEffectiveChanges deletedEvidence changes)
  case (changes, deleted) of
    ([rawChange], [EffectiveChangeTerminallyIgnored projectedChange reason]) -> do
      assertEqual "delete retains the exact raw acknowledgement" rawChange projectedChange
      assertEqual "terminal reason" PublicationDeleted reason
      assertEqual
        "terminal ignore does not renumber the revision"
        (Store.retainedStoreChangeRevision rawChange)
        (Store.retainedStoreChangeRevision projectedChange)
    _ -> assertFailure ("unexpected terminal change projection: " <> show deleted)

controlledSnapshotProjection :: Assertion
controlledSnapshotProjection = do
  fixture <- populatedControlledStore
  snapshot <-
    checked
      "raw current snapshot"
      ( Store.retainedStoreSnapshotAt
          fixture.incarnation
          (Store.storeSlotRevision fixture.slot)
          fixture.state
      )
  changedEvidence <- evidenceFor fixture.changedControlled (effectiveSnapshotRawEvidence snapshot)
  changed <- checked "project changed snapshot" (projectEffectiveSnapshot changedEvidence snapshot)
  assertEqual "incarnation preserved" fixture.incarnation (effectiveSnapshotIncarnation changed)
  assertEqual "revision preserved" (Store.storeSlotRevision fixture.slot) (effectiveSnapshotRevision changed)
  assertEqual "sort preserved" (descriptorSortId descriptor) (effectiveSnapshotSortId changed)
  assertEqual "occurrence preserved" occurrenceId (effectiveSnapshotSortOccurrence changed)
  case effectiveSnapshotFacts changed of
    [fact] ->
      assertEqual "snapshot carries the projected B label" (Just ((ProcessLabel processB, 1))) (labelFromValue (effectiveSnapshotFactValue fact))
    facts -> assertFailure ("expected one projected fact, got " <> show facts)
  deletedEvidence <- evidenceFor fixture.deletedControlled (effectiveSnapshotRawEvidence snapshot)
  deleted <- checked "project deleted snapshot" (projectEffectiveSnapshot deletedEvidence snapshot)
  assertEqual "delete hides snapshot facts without changing its coordinate" [] (effectiveSnapshotFacts deleted)
  assertEqual "deleted snapshot revision retained" (effectiveSnapshotRevision changed) (effectiveSnapshotRevision deleted)

hiddenViewRebind :: Assertion
hiddenViewRebind = do
  fixture <- populatedControlledStore
  view <- checked "deleted view" (publicationView fixture.deletedControlled fixture.publication)
  let other = controlledPublication 2 ((ProcessLabel processA, 0)) 20
  assertEqual
    "hidden A cannot serve as evidence for raw B"
    (Left EffectiveStoreEvidenceRawMismatch)
    (effectiveStoreEvidence other view)

data MatchReference
  = MatchReference
      EffectiveCandidateKey
      ReplicaStrength
      CheckedPublication
      Value
  deriving stock (Eq, Show)

projectMatch :: EffectiveStoreMatch -> MatchReference
projectMatch match =
  MatchReference
    (effectiveStoreMatchKey match)
    (effectiveStoreMatchStrength match)
    (effectiveStoreMatchRawPublication match)
    (effectiveStoreMatchValue match)

propQueryFamilyReference :: Bool -> Bool -> Bool -> Bool -> Property
propQueryFamilyReference deleteReleased voidReleased queryForB queryForVoid = ioProperty $ do
  fixture <- populatedControlledStore
  let queryLabel
        | queryForVoid = (VoidLabel, 1)
        | queryForB = (ProcessLabel processB, 1)
        | otherwise = (ProcessLabel processA, 0)
  query <-
    resolvedLabelQuery
      fixture
      queryLabel
  cut <- checked "generated query cut" (captureEffectiveQueryCut query fixture.state)
  let controlled = scenarioControlled fixture.controlled deleteReleased voidReleased
  evidence <- evidenceFor controlled (effectiveQueryCutRawCandidates cut)
  queryMatches <- checked "generated query evaluation" (evaluateEffectiveQueryCut evidence cut)
  readPlan <- checked "generated read plan" (prepareEffectiveReadPlan evidence cut)
  waitPlan <- checked "generated wait plan" (prepareEffectiveWaitPlan evidence cut)
  takePlan <- checked "generated take plan" (prepareEffectiveLocalTakePlan evidence cut)
  let actualQuery = fmap projectMatch queryMatches
      actualRead = fmap projectMatch (effectiveReadMatches readPlan)
      actualWait = fmap projectMatch (effectiveWaitMatches waitPlan)
      actualTake = fmap projectMatch (effectiveLocalTakeMatches takePlan)
      expected =
        [ MatchReference
            key
            Normal
            fixture.publication
            ( controlledValue
                objectIdentity
                (if voidReleased then (VoidLabel, 1) else (ProcessLabel processB, 1))
                10
            )
        | not deleteReleased,
          (voidReleased && queryForVoid)
            || (not voidReleased && queryForB && not queryForVoid),
          key <- Set.toAscList (effectiveQueryCutKeys cut)
        ]
      context =
        "delete="
          <> show deleteReleased
          <> ", void="
          <> show voidReleased
          <> ", queryForB="
          <> show queryForB
          <> ", queryForVoid="
          <> show queryForVoid
  pure
    ( counterexample
        context
        ( conjoin
            [ actualQuery === expected,
              actualRead === expected,
              actualWait === expected,
              actualTake === expected
            ]
        )
    )

data ChangeReference
  = ChangeVisibleReference
      Store.RetainedStoreChange
      StoreRevision
      CheckedPublication
      Value
  | ChangeIgnoredReference
      Store.RetainedStoreChange
      StoreRevision
      PublicationHiddenReason
  deriving stock (Eq, Show)

projectChange :: EffectiveChangeDisposition -> ChangeReference
projectChange disposition = case disposition of
  EffectiveChangeVisible change value ->
    ChangeVisibleReference
      change
      (Store.retainedStoreChangeRevision change)
      ( Store.retainedStoreObservationPublication
          (Store.retainedStoreChangeObservation change)
      )
      value
  EffectiveChangeTerminallyIgnored change reason ->
    ChangeIgnoredReference
      change
      (Store.retainedStoreChangeRevision change)
      reason

propChangesReference :: Bool -> Bool -> Property
propChangesReference deleteReleased voidReleased = ioProperty $ do
  fixture <- populatedControlledStore
  changes <-
    checked
      "generated raw changes"
      ( Store.retainedStoreChangesAfter
          fixture.incarnation
          initialStoreRevision
          fixture.state
      )
  let controlled = scenarioControlled fixture.controlled deleteReleased voidReleased
  evidence <- evidenceFor controlled (effectiveChangeRawEvidence changes)
  projected <- checked "generated change projection" (projectEffectiveChanges evidence changes)
  let expected = case changes of
        [change]
          | deleteReleased ->
              [ ChangeIgnoredReference
                  change
                  (Store.retainedStoreChangeRevision change)
                  PublicationDeleted
              ]
          | otherwise ->
              [ ChangeVisibleReference
                  change
                  (Store.retainedStoreChangeRevision change)
                  fixture.publication
                  ( controlledValue
                      objectIdentity
                      (if voidReleased then (VoidLabel, 1) else (ProcessLabel processB, 1))
                      10
                  )
              ]
        _ -> []
  pure
    ( counterexample
        ("changes=" <> show changes <> ", projected=" <> show projected)
        (fmap projectChange projected === expected)
    )

-- The source scheduler observes raw retained prefixes, not effective query
-- visibility. Exercise creation, duplicate ingress, multiple appends, draining
-- and a local take against the same checked Store fixture.
propSourcePrefixNotifications :: Word8 -> Property
propSourcePrefixNotifications count = ioProperty $ do
  fixture <- populatedControlledStore
  let drained = Store.clearAlignmentLossChange (Store.clearAlignmentMemberStoreChanges (Store.clearPreparationSlotChanges (Store.clearSourceStoreChanges fixture.state)))
      expected = Set.singleton fixture.incarnation
      apply publication state = do
        prepared <-
          checked
            "source notification observation"
            ( Store.preparePeerStoreApplication
                (Store.storeObservationEnvelope fixtureSourceProcess occurrenceId fixtureSourceTopology (controlIndex 1) Nothing)
                (Store.peerStoreDestination (Store.storeSlotDelta fixture.slot) fixture.incarnation Normal :| [])
                publication
                state
            )
        pure (fst (Store.commitPeerStoreApplication prepared))
      newPublications =
        [ controlledPublication ordinal (ProcessLabel processA, 0) (fromIntegral ordinal)
        | ordinal <- [2 .. 2 + fromIntegral (count `mod` 12)]
        ]
  duplicate <- apply fixture.publication drained
  appended <- foldM (flip apply) drained newPublications
  appendedDuplicate <- apply (last newPublications) appended
  query <- resolvedLabelQuery fixture (ProcessLabel processA, 0)
  cut <- checked "source notification take cut" (captureEffectiveQueryCut query drained)
  evidence <- evidenceFor fixture.controlled (effectiveQueryCutRawCandidates cut)
  takePlan <- checked "source notification take plan" (prepareEffectiveLocalTakePlan evidence cut)
  preparedTake <- checked "source notification local take" (Store.prepareExactLocalTake (effectiveLocalTakeKeys takePlan) drained)
  let (taken, _) = Store.commitExactLocalTake preparedTake
      creationIncarnations = Set.fromList (fmap Store.storeSlotIncarnation (Store.retainedStoreSlots fixture.state))
  pure
    ( conjoin
        [ Store.changedSourceStoreIncarnations fixture.state === creationIncarnations,
          Store.changedSourceStoreIncarnations duplicate === Set.empty,
          Store.changedSourceStoreIncarnations appended === expected,
          Store.changedSourceStoreIncarnations appendedDuplicate === expected,
          Store.changedSourceStoreIncarnations (Store.clearSourceStoreChanges appended) === Set.empty,
          counterexample "draining twice changes no owner facts" (Store.clearSourceStoreChanges drained == drained),
          Store.changedSourceStoreIncarnations taken === Set.empty,
          fst (Store.takeAlignmentMemberStoreChanges fixture.state) === creationIncarnations,
          fst (Store.takeAlignmentMemberStoreChanges duplicate) === Set.empty,
          fst (Store.takeAlignmentMemberStoreChanges appended) === expected,
          fst (Store.takeAlignmentMemberStoreChanges appendedDuplicate) === expected,
          fst (Store.takeAlignmentMemberStoreChanges taken) === Set.empty,
          fst (Store.takeAlignmentMemberStoreChanges (Store.clearSourceStoreChanges appended)) === expected,
          Store.changedSourceStoreIncarnations (snd (Store.takeAlignmentMemberStoreChanges appended)) === expected,
          fst (Store.takeAlignmentMemberStoreChanges (snd (Store.takeAlignmentMemberStoreChanges appended))) === Set.empty,
          counterexample "content changes affect member evidence but never active Store loss"
            $ conjoin [fst (Store.takeAlignmentLossChange state) === False | state <- [duplicate, appended, appendedDuplicate, taken]],
          Store.retainedStoreSlots taken === Store.retainedStoreSlots (Store.clearSourceStoreChanges taken),
          fst (Store.takePreparationSlotChanges fixture.state) === Set.fromList (map Store.storeSlotDelta (Store.retainedStoreSlots fixture.state)),
          fst (Store.takePreparationSlotChanges (Store.clearSourceStoreChanges fixture.state)) === fst (Store.takePreparationSlotChanges fixture.state),
          Store.changedSourceStoreIncarnations (Store.clearPreparationSlotChanges fixture.state) === creationIncarnations,
          counterexample "content append and exact take do not change slot readiness"
            $ conjoin [fst (Store.takePreparationSlotChanges state) === Set.empty | state <- [duplicate, appended, appendedDuplicate, taken]]
        ]
    )

-- Generate active replays, closure, and fresh-incarnation replacement against an
-- independent active-bit model. A closed incarnation can never reactivate.
propPreparationSlotChanges :: [Bool] -> Property
propPreparationSlotChanges rawOperations = ioProperty $ do
  fixture <- populatedControlledStore
  let delta = Store.storeSlotDelta fixture.slot
      initial = Store.clearPreparationSlotChanges fixture.state
      operations = zip [1 :: Int ..] ([True, False, False, True, True] <> take 40 rawOperations)
      step (state, active, incarnation, expectedPending) (ordinal, desiredActive) = do
        fresh <- checked "fresh preparation slot" (mkStoreIncarnationId (fixtureIdentifierBytes (fromIntegral ordinal + 10)))
        let drained = Store.clearAlignmentLossChange (Store.clearAlignmentMemberStoreChanges (Store.clearPreparationSlotChanges state))
            nextIncarnation = if desiredActive && not active then fresh else incarnation
            transition predecessor
              | not desiredActive = pure (Store.passivateStoreForInvariantTest delta predecessor)
              | active = checked "preparation active slot replay" (Store.reactivateRetainedStoreForInvariantTest delta incarnation predecessor)
              | otherwise =
                  Store.commitStoreBootstrap
                    <$> checked
                      "fresh preparation slot birth"
                      ( Store.prepareStoreBootstrap
                          [ Store.localStoreSpec
                              (Store.storeSlotProvenance fixture.slot)
                              delta
                              (Store.storeSlotSortId fixture.slot)
                              (Store.storeSlotOccurrenceId fixture.slot)
                              fresh
                          ]
                          predecessor
                      )
            changed = desiredActive /= active
            expectedStep = if changed then Set.singleton delta else Set.empty
            expectedTotal = Set.union expectedPending expectedStep
        if active
          then pure ()
          else case Store.reactivateRetainedStoreForInvariantTest delta incarnation drained of
            Left (Store.StructuralStoreClosedIncarnationReuse actualDelta actualIncarnation) ->
              assertEqual "closed incarnation replay remains rejected" (delta, incarnation) (actualDelta, actualIncarnation)
            Left problem -> assertFailure ("unexpected closed-incarnation error: " <> show problem)
            Right _ -> assertFailure "closed incarnation unexpectedly admitted"
        successor <- transition state
        isolated <- transition drained
        assertEqual "each actual slot change notifies once; exact active replay is silent" expectedStep (fst (Store.takePreparationSlotChanges isolated))
        assertEqual "unconsumed changes coalesce" expectedTotal (fst (Store.takePreparationSlotChanges successor))
        assertEqual "only active lifetime changes wake the atomic loss batch" changed (fst (Store.takeAlignmentLossChange isolated))
        assertEqual "member evidence observes retained birth, not active/passive bookkeeping" (if desiredActive && not active then Set.singleton fresh else Set.empty) (fst (Store.takeAlignmentMemberStoreChanges isolated))
        assertEqual "loss and preparation consumers are independent" expectedStep (fst (Store.takePreparationSlotChanges (snd (Store.takeAlignmentLossChange isolated))))
        assertEqual "consumed loss changes stay consumed" False (fst (Store.takeAlignmentLossChange (snd (Store.takeAlignmentLossChange isolated))))
        assertEqual
          "the observed slot agrees with the independent lifetime model"
          (if desiredActive then Just nextIncarnation else Nothing)
          (Store.storeSlotIncarnation <$> Store.lookupStoreSlot delta successor)
        let (observed, consumed) = Store.takePreparationSlotChanges successor
        assertBool "taking and clearing agree" (Store.clearPreparationSlotChanges successor == consumed)
        assertEqual "taking twice is empty" Set.empty (fst (Store.takePreparationSlotChanges consumed))
        assertEqual "consumption leaves all retained Store facts intact" (Store.retainedStoreSlots successor) (Store.retainedStoreSlots consumed)
        pure (successor, desiredActive, nextIncarnation, observed)
  _ <- foldM step (initial, True, fixture.incarnation, Set.empty) operations
  pure True

data SnapshotFactReference
  = SnapshotFactReference
      Store.RetainedStoreObservation
      Store.RetainedStoreObservation
      Value
      ReplicaStrength
  deriving stock (Eq, Show)

projectSnapshotFact :: EffectiveSnapshotFact -> SnapshotFactReference
projectSnapshotFact fact =
  SnapshotFactReference
    (effectiveSnapshotFactRepresentative fact)
    (effectiveSnapshotFactStrengthWitness fact)
    (effectiveSnapshotFactValue fact)
    (effectiveSnapshotFactStrength fact)

propSnapshotReference :: Bool -> Bool -> Bool -> Property
propSnapshotReference deleteReleased voidReleased reverseInsertion = ioProperty $ do
  let publicationA = controlledPublication 2 ((ProcessLabel processA, 0)) 10
      publicationB = controlledPublication 1 ((ProcessLabel processB, 0)) 10
      (representativePublication, strengthPublication)
        | checkedPublicationWinnerKey publicationA
            >= checkedPublicationWinnerKey publicationB =
            (publicationA, publicationB)
        | otherwise = (publicationB, publicationA)
      observations =
        if reverseInsertion
          then
            [ (strengthPublication, Normal),
              (representativePublication, Weak)
            ]
          else
            [ (representativePublication, Weak),
              (strengthPublication, Normal)
            ]
  fixture <-
    populatedStoreWithControlledProjection
      [representativePublication]
      observations
  snapshot <-
    checked
      "generated effective-only collapsed snapshot"
      ( Store.retainedStoreSnapshotAt
          fixture.incarnation
          (Store.storeSlotRevision fixture.slot)
          fixture.state
      )
  let representativeObservation =
        expectedRoutedObservation
          representativePublication
          Weak
          (if reverseInsertion then 2 else 1)
      strengthObservation =
        expectedRoutedObservation
          strengthPublication
          Normal
          (if reverseInsertion then 1 else 2)
  evidence <-
    evidenceForSeparateControlledProjections
      deleteReleased
      voidReleased
      (effectiveSnapshotRawEvidence snapshot)
  projected <- checked "generated snapshot projection" (projectEffectiveSnapshot evidence snapshot)
  filtered <-
    checked
      "generated raw-partition snapshot filter"
      (filterEffectiveSnapshot evidence snapshot)
  let expectedFacts =
        [ SnapshotFactReference
            representativeObservation
            strengthObservation
            ( controlledValue
                objectIdentity
                (if voidReleased then (VoidLabel, 1) else (ProcessLabel processB, 1))
                10
            )
            Normal
        | not deleteReleased
        ]
      actualFacts = fmap projectSnapshotFact (effectiveSnapshotFacts projected)
      coordinatePreserved =
        effectiveSnapshotIncarnation projected == fixture.incarnation
          && effectiveSnapshotRevision projected == Store.storeSlotRevision fixture.slot
          && effectiveSnapshotSortId projected == descriptorSortId descriptor
          && effectiveSnapshotSortOccurrence projected == occurrenceId
      distinctRawFacts = length (Store.retainedStoreSnapshotFacts snapshot) == 2
      rawLogicalStatesDistinct =
        checkedPublicationLogicalState representativePublication
          /= checkedPublicationLogicalState strengthPublication
      effectiveFactsCollapsed =
        length (effectiveSnapshotFacts projected)
          == if deleteReleased then 0 else 1
      rawPartitionsPreserved =
        length (effectiveSnapshotFacts filtered)
          == if deleteReleased then 0 else 2
      provenanceSeparated = representativeObservation /= strengthObservation
  pure
    ( counterexample
        ( "delete="
            <> show deleteReleased
            <> ", void="
            <> show voidReleased
            <> ", reverseInsertion="
            <> show reverseInsertion
            <> ", facts="
            <> show actualFacts
        )
        ( conjoin
            [ actualFacts === expectedFacts,
              coordinatePreserved === True,
              distinctRawFacts === True,
              rawLogicalStatesDistinct === True,
              effectiveFactsCollapsed === True,
              rawPartitionsPreserved === True,
              provenanceSeparated === True
            ]
        )
    )

expectedRoutedObservation ::
  CheckedPublication ->
  ReplicaStrength ->
  Word64 ->
  Store.RetainedStoreObservation
expectedRoutedObservation publication strength ordinal =
  checkedPure
    "expected routed observation"
    ( Store.retainedStoreObservation
        publication
        strength
        occurrenceId
        ( Store.RoutedStoreObservation
            ( Store.storeObservationEnvelope
                fixtureSourceProcess
                occurrenceId
                fixtureSourceTopology
                (controlIndex ordinal)
                Nothing
            )
        )
    )

propEvidenceMutationReference :: Bool -> Bool -> Property
propEvidenceMutationReference mutateChanges removeEvidence = ioProperty $ do
  fixture <- populatedControlledStore
  let other = controlledPublication 2 ((ProcessLabel processA, 0)) 20
      expandedControlled =
        scenarioControlled
          (controlledState (fixture.publications <> [other]))
          False
          False
  otherView <-
    checked
      "other exact effective view"
      ( interpretEffectivePublication
          <$> checkEffectivePublicationContext
            descriptor
            expandedControlled
            other
      )
  otherEvidence <- checked "other exact evidence" (effectiveStoreEvidence other otherView)
  if mutateChanges
    then do
      changes <-
        checked
          "mutation raw changes"
          ( Store.retainedStoreChangesAfter
              fixture.incarnation
              initialStoreRevision
              fixture.state
          )
      correct <- evidenceFor fixture.changedControlled (effectiveChangeRawEvidence changes)
      let expectedKeys = effectiveChangeKeys changes
          supplied =
            if removeEvidence
              then Map.empty
              else Map.map (const otherEvidence) correct
          expectedProblem =
            if removeEvidence
              then EffectiveStoreChangeViewKeySetMismatch expectedKeys Set.empty
              else case Set.toAscList expectedKeys of
                key : _ -> EffectiveStoreChangeVisibleRawMismatch key
                [] -> error "generated changes unexpectedly empty"
      pure
        ( counterexample
            ("change mutation result=" <> show (projectEffectiveChanges supplied changes))
            ((() <$ projectEffectiveChanges supplied changes) === Left expectedProblem)
        )
    else do
      query <- resolvedLabelQuery fixture ((ProcessLabel processB, 1))
      cut <- checked "mutation query cut" (captureEffectiveQueryCut query fixture.state)
      correct <- evidenceFor fixture.changedControlled (effectiveQueryCutRawCandidates cut)
      let expectedKeys = effectiveQueryCutKeys cut
          supplied =
            if removeEvidence
              then Map.empty
              else Map.map (const otherEvidence) correct
          expectedProblem =
            if removeEvidence
              then EffectiveStoreViewKeySetMismatch expectedKeys Set.empty
              else case Set.toAscList expectedKeys of
                key : _ -> EffectiveStoreVisibleRawMismatch key
                [] -> error "generated query cut unexpectedly empty"
      pure
        ( counterexample
            ("query mutation result=" <> show (evaluateEffectiveQueryCut supplied cut))
            ((() <$ evaluateEffectiveQueryCut supplied cut) === Left expectedProblem)
        )

propSnapshotWitnessMutation :: Bool -> Bool -> Property
propSnapshotWitnessMutation mixedVisibility hideRepresentative = ioProperty $ do
  let weakerRepresentative = controlledPublication 2 ((ProcessLabel processA, 0)) 10
      strongerWitness = controlledPublication 1 ((ProcessLabel processA, 0)) 10
  fixture <-
    populatedControlledStoreWith
      [(strongerWitness, Normal), (weakerRepresentative, Weak)]
  snapshot <-
    checked
      "witness mutation snapshot"
      ( Store.retainedStoreSnapshotAt
          fixture.incarnation
          (Store.storeSlotRevision fixture.slot)
          fixture.state
      )
  case effectiveSnapshotRawEvidence snapshot of
    [(representativeKey, representativeRaw), (witnessKey, witnessRaw)] -> do
      base <- evidenceFor fixture.changedControlled (effectiveSnapshotRawEvidence snapshot)
      let (replacementKey, replacementRaw)
            | mixedVisibility && hideRepresentative =
                (representativeKey, representativeRaw)
            | otherwise = (witnessKey, witnessRaw)
      replacement <-
        evidenceFor
          (if mixedVisibility then fixture.deletedControlled else fixture.controlled)
          [(replacementKey, replacementRaw)]
      let supplied =
            Map.insert
              replacementKey
              (replacement Map.! replacementKey)
              base
          expectedProblem =
            if mixedVisibility
              then
                EffectiveStoreSnapshotVisibilityMismatch
                  representativeKey
                  witnessKey
              else EffectiveStoreSnapshotStrengthValueMismatch witnessKey
          result = projectEffectiveSnapshot supplied snapshot
      pure
        ( counterexample
            ( "snapshot witness mutation result="
                <> show result
                <> ", hideRepresentative="
                <> show hideRepresentative
            )
            ((() <$ result) === Left expectedProblem)
        )
    evidence ->
      pure
        ( counterexample
            ("expected distinct representative/witness evidence, got " <> show evidence)
            (False === True)
        )

propSnapshotEvidenceMutation :: Bool -> Property
propSnapshotEvidenceMutation removeEvidence = ioProperty $ do
  let weakerRepresentative = controlledPublication 2 ((ProcessLabel processA, 0)) 10
      strongerWitness = controlledPublication 1 ((ProcessLabel processA, 0)) 10
  fixture <-
    populatedControlledStoreWith
      [(strongerWitness, Normal), (weakerRepresentative, Weak)]
  snapshot <-
    checked
      "snapshot evidence mutation"
      ( Store.retainedStoreSnapshotAt
          fixture.incarnation
          (Store.storeSlotRevision fixture.slot)
          fixture.state
      )
  correct <-
    evidenceFor fixture.changedControlled (effectiveSnapshotRawEvidence snapshot)
  case effectiveSnapshotRawEvidence snapshot of
    [(representativeKey, _), (witnessKey, _)] -> do
      let expectedKeys = effectiveSnapshotEvidenceKeys snapshot
          supplied
            | removeEvidence = Map.delete witnessKey correct
            | otherwise =
                Map.insert
                  witnessKey
                  (correct Map.! representativeKey)
                  (Map.insert representativeKey (correct Map.! witnessKey) correct)
          expectedProblem
            | removeEvidence =
                EffectiveStoreSnapshotViewKeySetMismatch
                  expectedKeys
                  (Set.delete witnessKey expectedKeys)
            | otherwise =
                EffectiveStoreSnapshotVisibleRawMismatch representativeKey
          result = projectEffectiveSnapshot supplied snapshot
      pure
        ( counterexample
            ("snapshot evidence mutation result=" <> show result)
            ((() <$ result) === Left expectedProblem)
        )
    evidence ->
      pure
        ( counterexample
            ("expected distinct representative/witness evidence, got " <> show evidence)
            (False === True)
        )

scenarioControlled :: Controlled.State -> Bool -> Bool -> Controlled.State
scenarioControlled controlled deleteReleased voidReleased =
  installControlledRelease
    controlled
    4
    (releasedLabel ((ProcessLabel processA, 0)))
    Nothing
    proposed
  where
    proposed
      | deleteReleased = (releasedDeleted 0)
      | voidReleased = releasedLabel (VoidLabel, 0)
      | otherwise = releasedLabel ((ProcessLabel processB, 0))

data ControlledStoreFixture = ControlledStoreFixture
  { state :: Store.State,
    slot :: Store.StoreSlot,
    incarnation :: StoreIncarnationId,
    publication :: CheckedPublication,
    publications :: [CheckedPublication],
    controlled :: Controlled.State,
    changedControlled :: Controlled.State,
    deletedControlled :: Controlled.State
  }

populatedControlledStore :: IO ControlledStoreFixture
populatedControlledStore =
  populatedControlledStoreWith
    [(controlledPublication 1 ((ProcessLabel processA, 0)) 10, Normal)]

populatedControlledStoreWith ::
  [(CheckedPublication, ReplicaStrength)] -> IO ControlledStoreFixture
populatedControlledStoreWith observations =
  populatedStoreWithControlledProjection (fmap fst observations) observations

-- | Populate the immutable raw Store and the one live Controlled projection
-- from the same checked observations. Label releases then derive two complete
-- owner cuts without modifying the Store history.
populatedStoreWithControlledProjection ::
  [CheckedPublication] ->
  [(CheckedPublication, ReplicaStrength)] ->
  IO ControlledStoreFixture
populatedStoreWithControlledProjection controlledPublications observations = do
  (publication, _) <- case observations of
    first : _ -> pure first
    [] -> assertFailure "controlled Store fixture requires an observation"
  delta <- checked "delta" (mkDeltaId (fixtureIdentifierBytes 0xc1))
  incarnation <- checked "Store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xc2))
  initial <- checked "initial Store" (Store.initialState fixtureCheckedGenesis)
  bootstrap <-
    checked
      "custom Store bootstrap"
      ( Store.prepareStoreBootstrap
          [ Store.localStoreSpec
              (Store.HeraldSystemView (checkedLocalHeraldEpoch fixtureCheckedGenesis) SortDefinitionRole)
              delta
              (descriptorSortId descriptor)
              occurrenceId
              incarnation
          ]
          initial
      )
  let publications = controlledPublications
      controlled = controlledState controlledPublications
  state <-
    foldM
      ( applyObservation
          fixtureSourceProcess
          fixtureSourceTopology
          delta
          incarnation
      )
      (Store.commitStoreBootstrap bootstrap)
      (zip [1 :: Word64 ..] observations)
  slot <-
    maybe
      (assertFailure "populated Store slot disappeared")
      pure
      (Store.lookupStoreSlot delta state)
  let initialLabel =
        maybe
          (error "controlled Store fixture publication has no label")
          id
          (labelFromValue (checkedPublicationValue publication))
      changed =
        installControlledRelease
          controlled
          4
          (releasedLabel initialLabel)
          Nothing
          (releasedLabel ((ProcessLabel processB, 0)))
      deleted =
        installControlledRelease
          controlled
          4
          (releasedLabel initialLabel)
          Nothing
          (releasedDeleted 0)
  pure
    ControlledStoreFixture
      { state,
        slot,
        incarnation,
        publication,
        publications,
        controlled,
        changedControlled = changed,
        deletedControlled = deleted
      }
  where
    applyObservation
      sourceProcess
      sourceTopology
      delta
      incarnation
      predecessor
      (ordinal, (publication, strength)) = do
        prepared <-
          checked
            "peer Store application"
            ( Store.preparePeerStoreApplication
                ( Store.storeObservationEnvelope
                    sourceProcess
                    occurrenceId
                    sourceTopology
                    (controlIndex ordinal)
                    Nothing
                )
                (Store.peerStoreDestination delta incarnation strength :| [])
                publication
                predecessor
            )
        pure (fst (Store.commitPeerStoreApplication prepared))

resolvedLabelQuery :: ControlledStoreFixture -> Label -> IO ResolvedQuery
resolvedLabelQuery fixture label = do
  predicate <-
    checked
      "label predicate"
      ( Query.checkQueryPredicate
          (descriptorSchema (canonicalCheckedDescriptor descriptor))
          ( Query.QueryCompare
              (directProjection labelField)
              Query.ScalarEqual
              (Query.QueryLabel label)
          )
      )
  pure
    ( resolvedQuery
        [ resolvedQueryBranch
            (Store.storeSlotDelta fixture.slot)
            fixture.incarnation
            predicate
        ]
    )

evidenceFor ::
  (Ord key) =>
  Controlled.State ->
  [(key, CheckedPublication)] ->
  IO (Map key EffectiveStoreEvidence)
evidenceFor controlled evidence =
  Map.fromList <$> traverse one evidence
  where
    one (key, publication) = do
      view <- checked "effective publication view" (publicationView controlled publication)
      bound <- checked "raw/effective Store evidence" (effectiveStoreEvidence publication view)
      pure (key, bound)

-- | Build each view from an exact singleton live Controlled projection. This
-- preserves the independent representative/strength-witness evidence used by
-- the effective-only snapshot collapse property.
evidenceForSeparateControlledProjections ::
  (Ord key) =>
  Bool ->
  Bool ->
  [(key, CheckedPublication)] ->
  IO (Map key EffectiveStoreEvidence)
evidenceForSeparateControlledProjections deleteReleased voidReleased evidence =
  Map.fromList <$> traverse one evidence
  where
    one (key, publication) = do
      let initial = controlledState [publication]
          prior =
            maybe
              (error "controlled publication has no label")
              id
              (labelFromValue (checkedPublicationValue publication))
          proposed
            | deleteReleased = (releasedDeleted 0)
            | voidReleased = releasedLabel (VoidLabel, 0)
            | otherwise = releasedLabel ((ProcessLabel processB, 0))
          controlled =
            installControlledRelease
              initial
              4
              (releasedLabel prior)
              Nothing
              proposed
      view <-
        checked
          "singleton live effective view"
          (publicationView controlled publication)
      bound <- checked "live raw/effective evidence" (effectiveStoreEvidence publication view)
      pure (key, bound)

publicationView ::
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem EffectivePublicationView
publicationView = publicationViewFor

publicationViewFor ::
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem EffectivePublicationView
publicationViewFor controlled publication =
  interpretEffectivePublication
    <$> checkEffectivePublicationContext descriptor controlled publication

controlledState :: [CheckedPublication] -> Controlled.State
controlledState =
  checkedPure "Controlled fixture retention"
    . foldM retain Controlled.emptyState
  where
    retain predecessor publication = do
      observation <-
        either
          (Left . show)
          Right
          (Controlled.checkControlledObservation descriptor occurrenceId publication)
      prepared <-
        either
          (Left . show)
          Right
          (Controlled.prepareControlledPeerObservation observation predecessor)
      Right (fst (Controlled.commitControlledPeerObservation prepared))

installControlledRelease ::
  Controlled.State ->
  Word64 ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  Controlled.State
installControlledRelease controlled releaseIndex prior priorRevision proposed =
  Controlled.commitControlledLabelRelease
    ( checkedPure
        "live Controlled label release"
        ( Controlled.prepareControlledLabelRelease
            initialProjection
            (preparedFacts (fromIntegral releaseIndex) releaseIndex prior priorRevision proposed)
            (controlIndex releaseIndex)
            controlled
        )
    )

preparedFacts ::
  Word8 ->
  Word64 ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  PreparedLabelFacts
preparedFacts decisionByte resolveIndex prior priorRevision proposed =
  checkedPure
    "prepared overlay facts"
    ( mkPreparedLabelFacts
        (decisionId decisionByte)
        (controlIndex resolveIndex)
        objectId
        (successorState prior proposed)
        prior
        priorRevision
        profileCatalogueDigest
        -- This ordinary controlled target has no operation tenure. Its source
        -- publication retains its own genesis writer authority independently.
        Nothing
        Nothing
        (preparedAuthorityDisposition Nothing (releasedLabelValue prior) proposed)
        memberDigest
    )

releasedLabelValue :: ReleasedLabelState -> Label
releasedLabelValue state = case releasedLabelStateView state of
  ReleasedLabelView label -> label
  (ReleasedDeletedView _) -> error "deleted prior is excluded by PreparedLabelFacts"

labelFromValue :: Value -> Maybe Label
labelFromValue value = do
  projected <- either (const Nothing) Just (valueAt (directProjection labelField) value)
  case viewValue projected of
    LabelValue label -> Just label
    _ -> Nothing

controlledPublication :: Word64 -> Label -> Integer -> CheckedPublication
controlledPublication sequenceNumber label payload =
  checkedPure
    "controlled publication"
    ( mkCheckedPublication
        descriptor
        (publicationIdentifier sequenceNumber)
        (controlledValue objectIdentity label payload)
    )

publicationIdentifier :: Word64 -> PublicationId
publicationIdentifier sequenceNumber =
  publicationId writer genesisAuthorityEpoch herald (nablaSequence sequenceNumber)

controlledValue :: GlobalUniqueId -> Label -> Integer -> Value
controlledValue object label payload =
  checkedPure
    "controlled value"
    ( recordValue
        [ (objectField, globalUniqueIdValue object),
          (labelField, labelValue label),
          (payloadField, int64Value (fromInteger payload))
        ]
    )

descriptor :: CanonicalDescriptor
descriptor =
  canonicalizeCheckedDescriptor
    ( checkedPure
        "controlled descriptor"
        ( checkDescriptor
            ApplicationDescriptor
            DescriptorSpec
              { descriptorSpecKind = ControlledSort,
                descriptorSpecSchema =
                  checkedPure
                    "controlled schema"
                    ( recordSchema
                        [ (objectField, globalUniqueIdSchema),
                          (labelField, labelSchema),
                          (payloadField, int64Schema)
                        ]
                    ),
                descriptorSpecKeyProjections = [directProjection objectField],
                descriptorSpecValidity = AlwaysPredicate,
                descriptorSpecObsolescence = NeverPredicate,
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = 0,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Just labelField,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = Nothing
              }
        )
    )

objectField, labelField, payloadField :: FieldName
objectField = checkedPure "object field" (mkFieldName "object")
labelField = checkedPure "label field" (mkFieldName "label")
payloadField = checkedPure "payload field" (mkFieldName "payload")

objectIdentity :: GlobalUniqueId
objectIdentity = checkedPure "object identity" (mkGlobalUniqueId (fixtureIdentifierBytes 0xd1))

objectId :: GlobalObjectId
objectId = globalObjectIdFromGlobalUniqueId objectIdentity

processA, processB :: ProcessEpochId
processA = checkedPure "process A" (mkProcessEpochId (fixtureIdentifierBytes 0xd2))
processB = checkedPure "process B" (mkProcessEpochId (fixtureIdentifierBytes 0xd3))

fixtureSourceProcess :: ProcessEpochId
fixtureSourceProcess =
  checkedPure "source process" (mkProcessEpochId (fixtureIdentifierBytes 0xc3))

fixtureSourceTopology :: TopologyCutId
fixtureSourceTopology =
  checkedPure "source topology" (mkTopologyCutId (fixtureIdentifierBytes 0xc4))

writer :: NablaId
writer = checkedPure "writer" (mkNablaId (fixtureIdentifierBytes 0xd4))

herald :: HeraldEpoch
herald = checkedPure "herald" (mkHeraldEpoch (fixtureIdentifierBytes 0xd5))

occurrenceId :: SortDefinitionOccurrenceId
occurrenceId = checkedPure "occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xd6))

decisionId :: Word8 -> LabelDecisionId
decisionId byte = checkedPure "decision" (mkLabelDecisionId (ByteString.replicate 32 byte))

initialProjection :: InitialProjectionDigest
initialProjection = checkedPure "initial projection" (mkInitialProjectionDigest (fixtureIdentifierBytes 0xd7))

memberDigest :: MemberSetDigest
memberDigest =
  deriveMemberSetDigest
    (herald :| [checkedPure "other herald" (mkHeraldEpoch (fixtureIdentifierBytes 0xd8))])

checkedPure :: (Show error) => String -> Either error value -> value
checkedPure label = either (error . ((label <> ": ") <>) . show) id

checked :: (Show error) => String -> Either error value -> IO value
checked label = either (\problem -> assertFailure (label <> ": " <> show problem)) pure

-- A fixture destination is given the one successor of its captured prior.
successorState :: ReleasedLabelState -> ReleasedLabelState -> ReleasedLabelState
successorState prior destination =
  let next = releasedLabelStateGeneration prior + 1
   in case releasedLabelStateView destination of
        ReleasedLabelView (owner, _) -> releasedLabel (owner, next)
        ReleasedDeletedView _ -> releasedDeleted next
