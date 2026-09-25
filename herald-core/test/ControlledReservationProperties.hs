{-# LANGUAGE OverloadedStrings #-}

module ControlledReservationProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    ProcessId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    controlIndex,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkSortDefinitionOccurrenceId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationWinnerKey,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalizeCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor, PrimordialDescriptor),
    DescriptorSpec (..),
    PredicateExpression (AlwaysPredicate, CompareField, NeverPredicate),
    RankDirection (Ascending),
    RankTerm (RankApplicationValue),
    ScalarComparison (ScalarEqual),
    ScalarLiteral (LiteralBool),
    SortKind (ControlledSort, RegularSort),
    StructuralCarrierRole,
    checkDescriptor,
  )
import Eclips.Domain.Startup (PredefinedSortRole (NeutralVertexRole))
import Eclips.Domain.Store qualified as Store
import Eclips.Domain.StructuralConsequence qualified as Consequence
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (ProcessLabel, VoidLabel),
    Value,
    boolSchema,
    boolValue,
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
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Time (MonotonicInstant, monotonicInstant)
import GenesisFixtures qualified as Fixtures
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
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "controlled local owner"
    [ testGroup
        "current control base"
        [ testProperty "latest exact label and End facts import without donor observations or grants" propCurrentControlBase,
          testCase "control import applies receiver deletion and preserves unrelated local work" caseControlBaseLocalConsequences,
          testCase "a different current control base cannot overwrite an installed observer" caseControlBaseConflict
        ],
      testGroup
        "preparation notifications"
        [ testProperty "generated observation histories notify only first or winning object facts" propPreparationObservationChanges,
          testCase "possession-only, replay and retirement notifications stay scoped" casePreparationPossessionChanges,
          testCase "bootstrap observation discards temporary grants but preserves earlier notifications" casePreparationBootstrapObservation
        ],
      testGroup
        "reservation first use"
        [ testCase "pristine reservation commits exact owner evidence" casePristineReservation,
          testCase "delete has one terminal winner and distinguishes invariant authority" caseDeleteWinner,
          testCase "first publication wins once and exact retry is idempotent" caseFirstUseWinner,
          testCase "competing first use and immutable-binding failures change nothing" caseFirstUseConflict,
          testProperty "fresh controlled admission rejects every supplied nonzero generation" propFirstUseGeneration
        ],
      testGroup
        "reservation retirement"
        [ testCase "nabla passivation atomically cancels every pristine reservation" caseNablaReservationPassivation,
          testCase "nabla handoff preserves another nabla's pristine reservation" caseNablaReservationRetirementPreservesUnrelated,
          testCase "nabla handoff preserves both terminal reservation branches" caseNablaReservationRetirementPreservesConsumed,
          testCase "nabla retirement replay is state-idempotent" caseNablaReservationRetirementReplay
        ],
      testGroup
        "local lifecycle"
        [ testCase "current updates use the total winner and preserve the object binding" caseCurrentUpdates,
          testCase "updates require target possession and the retained label overlay" caseUpdateCapabilityAndLabel,
          testCase "obsolete suppression cannot resurrect" caseNoResurrection,
          testCase "peer first observation is replay-stable and binding-strict" casePeerObservation,
          testCase "checked controlled evidence rejects a regular descriptor" caseObservationKindBoundary,
          testProperty "generated peer observations never regress lifecycle" propPeerLifecycleMonotone,
          testProperty "a generated competing first use never replaces its winner" propFirstUseWinnerStable
        ],
      testGroup
        "Store-derived possession"
        [ testCase "controlled reads retain their exact Normal or Weak strength" caseStoreReadStrength,
          testCase "multiple deltas aggregate and local-take removes only its exact source" caseStoreSourceAggregation,
          testCase "publisher evidence survives local-take and hidden suppression grants nothing" caseStoreTakeEvidenceBoundaries,
          testCase "peer and Store schedules converge to obsolete and take preserves suppression" casePeerStoreObsolescenceSchedules,
          testProperty "Store source aggregation is independent of read order" propStoreSourceReadOrder
        ],
      testGroup
        "Forward capability facts"
        [ testCase "effective possession returns the exact strongest source" caseEffectivePossessionStrength,
          testProperty "normal-possession entries are canonical and query-sound" propNormalPossessionEntries,
          testCase "only a winning obsolete publication records the first local instant" caseObsoleteObservationTime,
          testCase
            "process retirement is monotone across historical and late configured sources"
            caseProcessRetirementMonotone
        ]
    ]

propCurrentControlBase :: Word8 -> Property
propCurrentControlBase sample =
  let count = 1 + fromIntegral sample `mod` 6
      first = publication generated (ProcessLabel process, 0) 1 False 10
      (published, _) = commitFirst first reservedState
      initial = reserve process nabla pristineGenerated (commitStoreRead deltaA Normal first published)
      target = globalObjectIdFromNablaId nabla
      labelled = foldl (\current index -> installControlBaseLabel index target (Label.releasedLabel (ProcessLabel process, index)) current) initial [1 .. count]
      ended = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement process labelled)
      base = Controlled.captureControlledControlBase ended
      imported = importControlBase base Controlled.emptyState
      clean = Controlled.clearPreparationChanges imported
      initialDigest = Genesis.checkedInitialProjectionDigest Fixtures.fixtureCheckedInitialBootstraps
   in conjoin
        [ Controlled.captureControlledControlBase imported === base,
          Controlled.controlledReleasedLabelEntries imported === Controlled.controlledReleasedLabelEntries ended,
          Controlled.controlledEffectiveLabelState target imported === Controlled.controlledEffectiveLabelState target ended,
          Controlled.checkControlledLabelPrior initialDigest target imported === Controlled.checkControlledLabelPrior initialDigest target ended,
          fmap Controlled.controlledLabelPriorAuthority (Controlled.checkControlledLabelPrior initialDigest target imported) === Right (Just authority),
          Controlled.controlledProcessEnded process imported === True,
          Controlled.controlledProcessFacts imported === [],
          Controlled.controlledRootFacts imported === [],
          Controlled.controlledLocalRecords imported === [],
          Controlled.controlledNormalPossessionEntries imported === [],
          Controlled.controlledStorePossessionWitnesses imported === [],
          Controlled.controlledReservationWitnesses imported === [],
          counterexample "base import differs from full receiver-local label and End replay" (importControlBase base initial == ended),
          counterexample "exact base retry changes receiver state" (importControlBase base clean == clean)
        ]

caseControlBaseLocalConsequences :: Assertion
caseControlBaseLocalConsequences = do
  let first = publication generated (ProcessLabel process, 0) 1 False 10
      donorObserved = applyPeer descriptor occurrence first controlledState
      deletedLabel = installControlBaseLabel 1 object (Label.releasedDeleted 1) donorObserved
      cause = checked "control base deletion cause" (Consequence.labelReleaseCause (controlBaseDecision 1) (controlIndex 1))
      donor = Controlled.commitControlledRemoval (checked "control base donor removal" (Controlled.prepareControlledRemoval cause object deletedLabel))
      base = Controlled.captureControlledControlBase donor
      receiverRecord = publication peerGenerated (ProcessLabel process, 0) 2 False 15
      receiver = reserve process nabla pristineGenerated (commitStoreRead deltaA Normal first (applyPeer descriptor occurrence receiverRecord donorObserved))
      imported = importControlBase base receiver
      reference = Controlled.commitControlledRemoval (checked "control base reference removal" (Controlled.prepareControlledRemoval cause object (installControlBaseLabel 1 object (Label.releasedDeleted 1) receiver)))
  assertBool "base installation agrees with full local release/removal replay" (imported == reference)
  assertEqual "exact terminal deletion cause and generation are retained" (Controlled.controlledTerminalDeletionEntries donor) (Controlled.controlledTerminalDeletionEntries imported)
  assertEqual "the receiver's deleted record is removed" Nothing (Controlled.controlledLocalRecord object imported)
  assertEqual "the receiver's deleted Store possession is revoked" Nothing (Controlled.controlledStorePossessionStrength process deltaA object imported)
  assertEqual "an unrelated receiver record survives unchanged" (Controlled.controlledLocalRecord peerObject receiver) (Controlled.controlledLocalRecord peerObject imported)
  assertEqual "unrelated receiver reservations survive unchanged" (Controlled.controlledReservationWitnesses receiver) (Controlled.controlledReservationWitnesses imported)
  assertEqual "receiver bootstrap facts and capabilities are preserved" (Controlled.controlledRootFacts receiver) (Controlled.controlledRootFacts imported)
  assertBool "donor state does not overwrite local notifications" (fst (Controlled.takePreparationChanges imported) == fst (Controlled.takePreparationChanges reference))

caseControlBaseConflict :: Assertion
caseControlBaseConflict = do
  let first = publication generated (ProcessLabel process, 0) 1 False 10
      initial = applyPeer descriptor occurrence first controlledState
      one = installControlBaseLabel 1 object (Label.releasedLabel (ProcessLabel process, 1)) initial
      two = installControlBaseLabel 2 object (Label.releasedLabel (ProcessLabel process, 2)) one
      imported = importControlBase (Controlled.captureControlledControlBase one) Controlled.emptyState
  case Controlled.prepareControlledControlBaseImport (Controlled.captureControlledControlBase two) imported of
    Left problem -> assertEqual "only a fresh observer or exact retry accepts the base" Controlled.ControlledControlBaseAlreadyAdvanced problem
    Right _ -> assertFailure "a different base replaced existing current control facts"

importControlBase :: Controlled.ControlledControlBase -> Controlled.State -> Controlled.State
importControlBase base = Controlled.commitControlledControlBaseImport . checked "import current controlled base" . Controlled.prepareControlledControlBaseImport base

controlBaseDecision :: Word64 -> LabelDecisionId
controlBaseDecision index = checked "control base decision" (mkLabelDecisionId (identifierBytes (fromIntegral index)))

installControlBaseLabel :: Word64 -> GlobalObjectId -> Label.ReleasedLabelState -> Controlled.State -> Controlled.State
installControlBaseLabel index target successor state =
  Controlled.commitControlledLabelRelease (checked "release control base label" (Controlled.prepareControlledLabelRelease initialDigest facts (controlIndex index) state))
  where
    initialDigest = Genesis.checkedInitialProjectionDigest Fixtures.fixtureCheckedInitialBootstraps
    prior = checked "control base label prior" (Controlled.checkControlledLabelPrior initialDigest target state)
    priorState = Controlled.controlledLabelPriorEffectiveState prior
    priorLabel = case Label.releasedLabelStateView priorState of Label.ReleasedLabelView value -> value; Label.ReleasedDeletedView _ -> error "control base cannot relabel a deletion"
    membership = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView Fixtures.fixtureOracleProjectionState)
    facts = checked "control base label facts" (Label.mkPreparedLabelFacts (controlBaseDecision index) (controlIndex index) target successor priorState (Controlled.controlledLabelPriorRevision prior) (Genesis.checkedCatalogueDigest Fixtures.fixtureStep14CheckedGenesis) (Controlled.controlledLabelPriorAuthority prior) (Controlled.controlledLabelPriorAuthorityJustification prior) (Label.preparedAuthorityDisposition (Controlled.controlledLabelPriorAuthority prior) priorLabel successor) (Membership.heraldMembershipGenerationActiveMemberSetDigest membership))

-- Arbitrary order and repeats include losing, obsolete and suppressed-current
-- observations. The reference uses only public currentness/winner queries.
propPreparationObservationChanges :: [Word8] -> Property
propPreparationObservationChanges schedule =
  let initial = Controlled.clearPreparationChanges controlledState
      publicationFor byte = publication peerGenerated (ProcessLabel process, 0) (200 + fromIntegral byte) (byte `mod` 5 == 0) (fromIntegral byte)
      advance state byte = applyPeer descriptor occurrence (publicationFor byte) (Controlled.clearPreparationChanges state)
      states = scanl advance initial schedule
      view state = fmap (\record -> (Controlled.controlledRecordLifecycle record, checkedPublicationId (Controlled.controlledRecordLatestPublication record))) (Controlled.controlledLocalRecord peerObject state)
      expected before after = Controlled.PreparationChanges (if view before == view after then Set.empty else Set.singleton peerObject) Set.empty Set.empty
      checks =
        [ counterexample
            ("observation notification at " <> show byte)
            (fst (Controlled.takePreparationChanges after) === expected before after)
        | (byte, before, after) <- zip3 schedule states (drop 1 states)
        ]
      coalesced = foldl (\state byte -> applyPeer descriptor occurrence (publicationFor byte) state) initial schedule
      expectedCoalesced = Controlled.PreparationChanges (if null schedule then Set.empty else Set.singleton peerObject) Set.empty Set.empty
   in conjoin ((fst (Controlled.takePreparationChanges coalesced) === expectedCoalesced) : checks)

casePreparationPossessionChanges :: Assertion
casePreparationPossessionChanges = do
  let emptyChanges = Controlled.PreparationChanges Set.empty Set.empty Set.empty
      possessionChange = Controlled.PreparationChanges Set.empty Set.empty (Set.singleton (process, object))
      first = publication generated (ProcessLabel process, 0) 80 False 100
      lower = publication generated (ProcessLabel process, 0) 81 False 1
      observed = Controlled.clearPreparationChanges (applyPeer descriptor occurrence first controlledState)
      readOnce = commitStoreRead deltaA Normal first observed
      readClean = Controlled.clearPreparationChanges readOnce
      readAgain = commitStoreRead deltaA Normal first readClean
      (published, result) = commitUpdate lower readClean
      (publishedChanges, publishedClean) = Controlled.takePreparationChanges published
      (duplicate, _) = commitUpdate lower publishedClean
      taken = commitStoreTake deltaA Normal first publishedClean
      directGrant = checked "preparation test direct grant" (Controlled.checkControlledGrant process object publishedClean)
      grantReplay = Controlled.commitControlledGrants (checked "preparation test grant replay" (Controlled.prepareControlledGrants process [directGrant, directGrant] publishedClean))
      retired = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement process publishedClean)
      (retirementChanges, retiredClean) = Controlled.takePreparationChanges retired
      retiredAgain = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement process retiredClean)
      obsolete = publication generated (ProcessLabel process, 0) 82 True 1000
      obsoleteState = Controlled.clearPreparationChanges (applyPeer descriptor occurrence obsolete publishedClean)
      (timestamped, _) = Controlled.commitControlledObsoleteObservationTime (checked "preparation test obsolete timestamp" (prepareObsoleteObservationTime 10 obsolete obsoleteState))
      (firstUse, _) = commitFirst (publication generated (ProcessLabel process, 0) 1 False 10) (Controlled.clearPreparationChanges reservedState)
  assertEqual "first read notifies its exact possession" possessionChange (fst (Controlled.takePreparationChanges readOnce))
  assertEqual "same Store evidence has no notification" emptyChanges (fst (Controlled.takePreparationChanges readAgain))
  assertEqual "the lower publication really loses" Controlled.ControlledUpdateWinnerRetained result
  assertEqual "a losing publication can add direct possession without changing the object winner" possessionChange publishedChanges
  assertEqual "exact local retry is silent" emptyChanges (fst (Controlled.takePreparationChanges duplicate))
  assertEqual "removing one Store source notifies possession even with a surviving direct grant" possessionChange (fst (Controlled.takePreparationChanges taken))
  assertEqual "repeated direct grants are silent" emptyChanges (fst (Controlled.takePreparationChanges grantReplay))
  assertEqual "first End uses process scope without enumerating every revoked possession" (Controlled.PreparationChanges (Set.singleton (globalObjectIdFromProcessEpochId process)) (Set.singleton process) Set.empty) retirementChanges
  assertEqual "repeated End is silent" emptyChanges (fst (Controlled.takePreparationChanges retiredAgain))
  assertEqual "obsolete observation time is not readiness" emptyChanges (fst (Controlled.takePreparationChanges timestamped))
  assertEqual "first use registers object and publisher possession" (Controlled.PreparationChanges (Set.singleton object) Set.empty (Set.singleton (process, object))) (fst (Controlled.takePreparationChanges firstUse))
  assertBool "ordinary equality exposes journal consumption" (published /= publishedClean)
  assertBool "clearing changes leaves possession evidence intact" (Controlled.controlledHasDirectPossession process object publishedClean)

casePreparationBootstrapObservation :: Assertion
casePreparationBootstrapObservation = do
  let first = publication generated (ProcessLabel process, 0) 1 False 10
      (initial, _) = commitFirst first (Controlled.clearPreparationChanges reservedState)
      remoteFact = Controlled.processFact processIdentity otherProcess herald authority
      remoteRoot = Controlled.writerRootFact otherProcess NeutralVertexRole sortId occurrence authority (controlIndex 1) otherNabla UnsequencedNabla
      observe state = Controlled.commitControlledBootstrap (checked "preparation bootstrap observation" (Controlled.prepareControlledBootstrapObservation remoteFact [remoteRoot] state))
      observed = observe initial
      (changes, clean) = Controlled.takePreparationChanges observed
      replayed = observe clean
      expectedObjects = Set.fromList [object, globalObjectIdFromProcessEpochId otherProcess, globalObjectIdFromNablaId otherNabla]
      grant = checked "preparation bootstrap grant" (Controlled.checkControlledGrant process object clean)
      granted = Controlled.commitControlledGrants (checked "preparation new recipient grant" (Controlled.prepareControlledGrants otherProcess [grant] clean))
  assertEqual "real metadata and earlier pending grant survive" (Controlled.PreparationChanges expectedObjects (Set.singleton otherProcess) (Set.singleton (process, object))) changes
  assertBool "observing bootstrap metadata grants neither temporary capability" (not (Controlled.controlledHasDirectPossession otherProcess (globalObjectIdFromProcessEpochId otherProcess) clean) && not (Controlled.controlledHasDirectPossession otherProcess (globalObjectIdFromNablaId otherNabla) clean))
  assertEqual "metadata replay adds no grant or notification" (Controlled.PreparationChanges Set.empty Set.empty Set.empty) (fst (Controlled.takePreparationChanges replayed))
  assertEqual "later explicit grant notifies its actual recipient" (Controlled.PreparationChanges Set.empty Set.empty (Set.singleton (otherProcess, object))) (fst (Controlled.takePreparationChanges granted))

casePristineReservation :: Assertion
casePristineReservation = do
  witness <- oneWitness reservedState
  assertEqual "generated key" generated (Controlled.reservationWitnessGlobalUniqueId witness)
  assertEqual "process" process (Controlled.reservationWitnessProcess witness)
  assertEqual "nabla" nabla (Controlled.reservationWitnessNabla witness)
  assertEqual "sort" sortId (Controlled.reservationWitnessSortId witness)
  assertEqual "authority" authority (Controlled.reservationWitnessAuthority witness)
  assertEqual "phase" Controlled.Reserved (Controlled.reservationWitnessPhase witness)
  assertEqual "no first publication" Nothing (Controlled.reservationWitnessFirstPublicationId witness)

caseDeleteWinner :: Assertion
caseDeleteWinner = do
  assertEqual
    "wrong authority is an invariant contradiction"
    (Left Controlled.ControlledReservationAuthorityContradiction)
    (deleteError process nabla alternateAuthority generated reservedState)
  assertEqual
    "wrong process is unavailable"
    (Left Controlled.ControlledReservationUnavailable)
    (deleteError otherProcess nabla authority generated reservedState)
  prepared <-
    checkedIO
      "valid reservation delete"
      ( Controlled.prepareControlledReservationDelete
          process
          nabla
          authority
          generated
          reservedState
      )
  let consumed = Controlled.commitControlledReservationDelete prepared
  witness <- oneWitness consumed
  assertEqual
    "winner retains terminal evidence"
    Controlled.ConsumedByDelete
    (Controlled.reservationWitnessPhase witness)
  assertEqual
    "delete creates no controlled object"
    Nothing
    (Controlled.controlledLocalRecord object consumed)
  assertEqual
    "second delete is unavailable"
    (Left Controlled.ControlledReservationUnavailable)
    (deleteError process nabla authority generated consumed)

caseFirstUseWinner :: Assertion
caseFirstUseWinner = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
  prepared <- checkedIO "first controlled use" (prepareFirst first reservedState)
  assertEqual
    "prepared result"
    Controlled.ControlledFirstUseWon
    (Controlled.preparedControlledFirstUseResult prepared)
  let (published, result) = Controlled.commitControlledFirstUse prepared
  assertEqual "commit result" Controlled.ControlledFirstUseWon result
  witness <- oneWitness published
  assertEqual "reservation is published" Controlled.Published (Controlled.reservationWitnessPhase witness)
  assertEqual
    "reservation retains first publication"
    (Just (checkedPublicationId first))
    (Controlled.reservationWitnessFirstPublicationId witness)
  record <- oneRecord object published
  assertEqual "object binding" object (Controlled.controlledRecordObjectId record)
  assertEqual "sort binding" sortId (Controlled.controlledRecordSortId record)
  assertEqual "occurrence binding" occurrence (Controlled.controlledRecordOccurrenceId record)
  assertEqual "initial lifecycle" Controlled.ControlledCurrent (Controlled.controlledRecordLifecycle record)
  assertEqual "initial label" (Just ((ProcessLabel process, 0))) (Controlled.controlledRecordObservedLabel record)
  assertEqual "first winner" first (Controlled.controlledRecordFirstPublication record)
  assertEqual "latest winner" first (Controlled.controlledRecordLatestPublication record)
  assertEqual "no initial suppression" Controlled.ControlledUnsuppressed (Controlled.controlledRecordSuppression record)
  assertEqual "publisher possession" (Set.singleton process) (Controlled.controlledRecordPossessionSources record)
  assertBool "object is possessed" (Controlled.controlledHasNormalPossession process object published)
  retry <- checkedIO "exact first-use retry" (prepareFirst first published)
  let (retried, retryResult) = Controlled.commitControlledFirstUse retry
  assertEqual "retry classification" Controlled.ControlledFirstUseRetry retryResult
  assertBool "exact retry is state-idempotent" (retried == published)

caseFirstUseConflict :: Assertion
caseFirstUseConflict = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      competitor = publication generated ((ProcessLabel process, 0)) 2 False 11
      sameIdentityConflict = publication generated ((ProcessLabel process, 0)) 1 False 99
      wrongSort = publicationWithDescriptor alternateDescriptor generated ((ProcessLabel process, 0)) 3 False 12
      wrongObject = publication peerGenerated ((ProcessLabel process, 0)) 4 False 13
      obsoleteFirst = publication generated ((ProcessLabel process, 0)) 5 True 14
      (published, _) = commitFirst first reservedState
      wrongReservedSort =
        reservedStateWith (descriptorSortId alternateDescriptor)
  assertFirstUseRejected "competing first publication" competitor published
  assertFirstUseRejected "same publication id with unequal payload" sameIdentityConflict published
  case prepareFirstWith alternateDescriptor occurrence wrongSort reservedState of
    Left Controlled.ControlledFirstUseSortMismatch {} -> pure ()
    other -> assertFailure ("expected first-use sort mismatch, got " <> resultShape other)
  case prepareFirstWith descriptor alternateOccurrence first reservedState of
    Left (Controlled.ControlledFirstUseOccurrenceMismatch expected actual) -> do
      assertEqual "current writer occurrence" occurrence expected
      assertEqual "publication occurrence" alternateOccurrence actual
    other ->
      assertFailure
        ("expected current occurrence mismatch, got " <> resultShape other)
  case prepareFirst wrongObject reservedState of
    Left Controlled.ControlledFirstUseObjectConflict {} -> pure ()
    other -> assertFailure ("expected first-use object conflict, got " <> resultShape other)
  case prepareFirst obsoleteFirst reservedState of
    Left Controlled.ControlledFirstUseMustBeCurrent -> pure ()
    other -> assertFailure ("expected obsolete-first rejection, got " <> resultShape other)
  case prepareFirst first wrongReservedSort of
    Left (Controlled.ControlledFirstUseSortMismatch reserved actual) -> do
      assertEqual "frozen reserved sort" (descriptorSortId alternateDescriptor) reserved
      assertEqual "live writer sort" sortId actual
    other ->
      assertFailure
        ("expected frozen reservation sort mismatch, got " <> resultShape other)
  assertEqual
    "failed premises leave the pristine reservation unchanged"
    Controlled.Reserved
    (Controlled.reservationWitnessPhase (singleWitness reservedState))
  assertEqual "failed competing preparations leave the winner unchanged" first (latestPublication object published)
  assertEqual
    "failed competing preparations leave first-use evidence unchanged"
    (Just (checkedPublicationId first))
    (Controlled.reservationWitnessFirstPublicationId (singleWitness published))

caseNablaReservationPassivation :: Assertion
caseNablaReservationPassivation = do
  let prepared =
        Controlled.prepareControlledNablaReservationRetirement
          nabla
          reservationRetirementState
      expected = [cancelledGeneratedLow, cancelledGeneratedHigh]
  assertEqual
    "preparation exposes cancelled identities in canonical order"
    expected
    (Controlled.preparedControlledNablaReservationRetirementCancelled prepared)
  let (retired, cancelled) =
        Controlled.commitControlledNablaReservationRetirement prepared
  assertEqual "commit returns the same cancellation report" expected cancelled
  assertEqual
    "the first matching pristine reservation is absent"
    Nothing
    (Controlled.controlledReservationWitness cancelledGeneratedLow retired)
  assertEqual
    "the second matching pristine reservation is absent"
    Nothing
    (Controlled.controlledReservationWitness cancelledGeneratedHigh retired)

caseNablaReservationRetirementPreservesUnrelated :: Assertion
caseNablaReservationRetirementPreservesUnrelated = do
  let before =
        Controlled.controlledReservationWitness
          unrelatedGenerated
          reservationRetirementState
      (retired, _) =
        Controlled.commitControlledNablaReservationRetirement
          ( Controlled.prepareControlledNablaReservationRetirement
              nabla
              reservationRetirementState
          )
  assertEqual
    "an unrelated Nabla's pristine reservation is unchanged"
    before
    (Controlled.controlledReservationWitness unrelatedGenerated retired)
  unrelated <-
    maybe
      (assertFailure "the unrelated pristine reservation was unexpectedly absent")
      pure
      (Controlled.controlledReservationWitness unrelatedGenerated retired)
  assertEqual
    "the unrelated reservation remains pristine"
    Controlled.Reserved
    (Controlled.reservationWitnessPhase unrelated)

caseNablaReservationRetirementPreservesConsumed :: Assertion
caseNablaReservationRetirementPreservesConsumed = do
  let (retired, _) =
        Controlled.commitControlledNablaReservationRetirement
          ( Controlled.prepareControlledNablaReservationRetirement
              nabla
              reservationRetirementState
          )
      assertPreserved context target =
        assertEqual
          context
          (Controlled.controlledReservationWitness target reservationRetirementState)
          (Controlled.controlledReservationWitness target retired)
  assertPreserved "the published first-use winner is unchanged" generated
  assertPreserved
    "the delete-consumed winner is unchanged"
    consumedGenerated
  published <-
    maybe
      (assertFailure "the published reservation was unexpectedly absent")
      pure
      (Controlled.controlledReservationWitness generated retired)
  consumed <-
    maybe
      (assertFailure "the delete-consumed reservation was unexpectedly absent")
      pure
      (Controlled.controlledReservationWitness consumedGenerated retired)
  assertEqual
    "published remains terminal"
    Controlled.Published
    (Controlled.reservationWitnessPhase published)
  assertEqual
    "delete-consumed remains terminal"
    Controlled.ConsumedByDelete
    (Controlled.reservationWitnessPhase consumed)

caseNablaReservationRetirementReplay :: Assertion
caseNablaReservationRetirementReplay = do
  let (retired, _) =
        Controlled.commitControlledNablaReservationRetirement
          ( Controlled.prepareControlledNablaReservationRetirement
              nabla
              reservationRetirementState
          )
      replay =
        Controlled.prepareControlledNablaReservationRetirement nabla retired
  assertEqual
    "replay reports no newly cancelled identities"
    []
    (Controlled.preparedControlledNablaReservationRetirementCancelled replay)
  let (replayed, replayCancelled) =
        Controlled.commitControlledNablaReservationRetirement replay
  assertEqual "replay commit has an empty report" [] replayCancelled
  assertBool "replay leaves the committed state unchanged" (replayed == retired)

caseCurrentUpdates :: Assertion
caseCurrentUpdates = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      lower = publication generated ((ProcessLabel process, 0)) 2 False 1
      higher = publication generated ((ProcessLabel process, 0)) 3 False 20
      (published, _) = commitFirst first reservedState
  lowerPrepared <- checkedIO "lower current update" (prepareUpdate lower published)
  let (afterLower, lowerResult) = Controlled.commitControlledUpdate lowerPrepared
  assertEqual "lower rank is retained but does not win" Controlled.ControlledUpdateWinnerRetained lowerResult
  assertEqual "lower rank cannot replace winner" first (latestPublication object afterLower)
  higherPrepared <- checkedIO "higher current update" (prepareUpdate higher afterLower)
  let (afterHigher, higherResult) = Controlled.commitControlledUpdate higherPrepared
  assertEqual "higher rank advances" Controlled.ControlledUpdateWinnerAdvanced higherResult
  assertEqual "higher rank becomes current winner" higher (latestPublication object afterHigher)
  record <- oneRecord object afterHigher
  assertEqual "object sort is immutable" sortId (Controlled.controlledRecordSortId record)
  assertEqual "object occurrence is immutable" occurrence (Controlled.controlledRecordOccurrenceId record)
  assertEqual "application update reuses observed label" (Just ((ProcessLabel process, 0))) (Controlled.controlledRecordObservedLabel record)
  assertEqual "first publication stays frozen" first (Controlled.controlledRecordFirstPublication record)
  assertEqual
    "a non-winning observation remains available to composed invariants"
    (Just lower)
    (Controlled.controlledRecordObservation (checkedPublicationId lower) record)
  assertEqual
    "an unobserved publication has no retained evidence"
    Nothing
    ( Controlled.controlledRecordObservation
        (checkedPublicationId (publication generated ((ProcessLabel process, 0)) 99 False 99))
        record
    )
  retry <- checkedIO "current update retry" (prepareUpdate higher afterHigher)
  let (afterRetry, retryResult) = Controlled.commitControlledUpdate retry
  assertEqual "current retry classification" Controlled.ControlledUpdateRetry retryResult
  assertBool "current retry is idempotent" (afterRetry == afterHigher)

caseUpdateCapabilityAndLabel :: Assertion
caseUpdateCapabilityAndLabel = do
  let peerFirst = publication peerGenerated ((ProcessLabel process, 0)) 30 False 10
      peerUpdate = publication peerGenerated ((ProcessLabel process, 0)) 31 False 11
      first = publication generated ((ProcessLabel process, 0)) 1 False 10
      labelDrift = publication generated (VoidLabel, 0) 32 False 11
      generationDrift = publication generated (ProcessLabel process, 1) 33 False 12
      (published, _) = commitFirst first reservedState
      peerEstablished = applyPeer descriptor occurrence peerFirst controlledState
  case prepareUpdate peerUpdate peerEstablished of
    Left (Controlled.ControlledUpdateObjectNotPossessed observedProcess observedObject) -> do
      assertEqual "missing possession names process" process observedProcess
      assertEqual "missing possession names object" peerObject observedObject
    other -> assertFailure ("expected target-possession rejection, got " <> resultShape other)
  case prepareUpdate labelDrift published of
    Left (Controlled.ControlledUpdateLabelMismatch retained supplied) -> do
      assertEqual "retained overlay label" (Just ((ProcessLabel process, 0))) retained
      assertEqual "publication label drift" (Just (VoidLabel, 0)) supplied
    other -> assertFailure ("expected label-overlay rejection, got " <> resultShape other)
  assertEqual
    "a caller cannot replace the generation while retaining the owner"
    (Left (Controlled.ControlledUpdateLabelMismatch (Just (ProcessLabel process, 0)) (Just (ProcessLabel process, 1))))
    (() <$ prepareUpdate generationDrift published)
  assertEqual "capability failure changes no peer winner" peerFirst (latestPublication peerObject peerEstablished)
  assertEqual "label failure changes no local winner" first (latestPublication object published)

caseNoResurrection :: Assertion
caseNoResurrection = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      obsolete = publication generated ((ProcessLabel process, 0)) 2 True 0
      lateCurrent = publication generated ((ProcessLabel process, 0)) 3 False 100
      (published, _) = commitFirst first reservedState
      (obsoleted, obsoleteResult) = commitUpdate obsolete published
  assertEqual "obsolete advances over current" Controlled.ControlledUpdateWinnerAdvanced obsoleteResult
  obsoleteRecord <- oneRecord object obsoleted
  assertEqual "obsolete lifecycle" Controlled.ControlledObsolete (Controlled.controlledRecordLifecycle obsoleteRecord)
  assertEqual
    "obsolete winner installs hidden suppression"
    (Controlled.ControlledObsoleteSuppression (checkedPublicationWinnerKey obsolete))
    (Controlled.controlledRecordSuppression obsoleteRecord)
  case prepareUpdate lateCurrent obsoleted of
    Left (Controlled.ControlledUpdateNotCurrent observedObject Controlled.ControlledObsolete) ->
      assertEqual "rejection names the object" object observedObject
    other -> assertFailure ("expected obsolete local rejection, got " <> resultShape other)
  assertEqual "failed current update cannot revive" obsolete (latestPublication object obsoleted)
  obsoleteRetry <- checkedIO "obsolete exact retry" (prepareUpdate obsolete obsoleted)
  let (afterObsoleteRetry, obsoleteRetryResult) = Controlled.commitControlledUpdate obsoleteRetry
  assertEqual "obsolete retry remains exact" Controlled.ControlledUpdateRetry obsoleteRetryResult
  assertBool "obsolete retry changes nothing" (afterObsoleteRetry == obsoleted)

  let peerObsolete = publication peerGenerated ((ProcessLabel process, 0)) 40 True 0
      peerCurrent = publication peerGenerated ((ProcessLabel process, 0)) 41 False 100
      peerObsoleted = applyPeer descriptor occurrence peerObsolete controlledState
  latePrepared <- checkedIO "peer current after obsolete" (preparePeer descriptor occurrence peerCurrent peerObsoleted)
  let (suppressed, lateResult) = Controlled.commitControlledPeerObservation latePrepared
  assertEqual "peer current is terminally ignored" Controlled.ControlledPeerCurrentSuppressed lateResult
  suppressedRecord <- oneRecord peerObject suppressed
  assertEqual "obsolete-first remains obsolete" Controlled.ControlledObsolete (Controlled.controlledRecordLifecycle suppressedRecord)
  assertEqual "obsolete winner remains retained" peerObsolete (Controlled.controlledRecordLatestPublication suppressedRecord)

casePeerObservation :: Assertion
casePeerObservation = do
  let first = publication peerGenerated ((ProcessLabel process, 0)) 10 False 10
      obsolete = publication peerGenerated ((ProcessLabel process, 0)) 11 True 11
      currentAfterObsolete = publication peerGenerated ((ProcessLabel process, 0)) 12 False 100
  firstPrepared <- checkedIO "first peer observation" (preparePeer descriptor occurrence first controlledState)
  let (established, firstResult) = Controlled.commitControlledPeerObservation firstPrepared
  assertEqual "peer establishes local binding" Controlled.ControlledPeerEstablished firstResult
  replay <- checkedIO "peer replay" (preparePeer descriptor occurrence first established)
  let (replayed, replayResult) = Controlled.commitControlledPeerObservation replay
  assertEqual "equal replay" Controlled.ControlledPeerReplay replayResult
  assertBool "equal replay is idempotent" (replayed == established)
  case preparePeer descriptor alternateOccurrence (publication peerGenerated ((ProcessLabel process, 0)) 13 False 13) established of
    Left Controlled.ControlledPeerOccurrenceMismatch {} -> pure ()
    other -> assertFailure ("expected peer occurrence fault, got " <> resultShape other)
  let wrongSort = publicationWithDescriptor alternateDescriptor peerGenerated ((ProcessLabel process, 0)) 14 False 14
  case preparePeer alternateDescriptor occurrence wrongSort established of
    Left Controlled.ControlledPeerSortMismatch {} -> pure ()
    other -> assertFailure ("expected peer sort fault, got " <> resultShape other)
  let wrongLabel = publication peerGenerated (VoidLabel, 0) 15 False 15
  case preparePeer descriptor occurrence wrongLabel established of
    Left Controlled.ControlledPeerLabelMismatch {} -> pure ()
    other -> assertFailure ("expected peer label fault, got " <> resultShape other)
  case preparePeer descriptor occurrence (publication peerGenerated ((ProcessLabel process, 0)) 10 False 99) established of
    Left Controlled.ControlledPeerPublicationConflict {} -> pure ()
    other -> assertFailure ("expected peer identity conflict, got " <> resultShape other)
  obsoletePrepared <- checkedIO "peer obsolete" (preparePeer descriptor occurrence obsolete established)
  let (obsoleted, obsoleteResult) = Controlled.commitControlledPeerObservation obsoletePrepared
  assertEqual "peer obsolete advances" Controlled.ControlledPeerWinnerAdvanced obsoleteResult
  latePrepared <- checkedIO "peer current after obsolete" (preparePeer descriptor occurrence currentAfterObsolete obsoleted)
  let (suppressed, lateResult) = Controlled.commitControlledPeerObservation latePrepared
  assertEqual "peer current terminal ignore" Controlled.ControlledPeerCurrentSuppressed lateResult
  record <- oneRecord peerObject suppressed
  assertEqual "peer object remains obsolete" Controlled.ControlledObsolete (Controlled.controlledRecordLifecycle record)
  assertEqual "obsolete winner remains" obsolete (Controlled.controlledRecordLatestPublication record)
  case preparePeer descriptor occurrence (publication generated ((ProcessLabel process, 0)) 16 False 16) reservedState of
    Left (Controlled.ControlledPeerReservationConflict observedObject Controlled.Reserved) ->
      assertEqual "reservation conflict names object" object observedObject
    other -> assertFailure ("expected local reservation conflict, got " <> resultShape other)

caseProcessRetirementMonotone :: Assertion
caseProcessRetirementMonotone = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      (published, _) = commitFirst first reservedState
      withPristine =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              sortId
              authority
              pristineGenerated
              published
          )
      withConsumedReservation =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              sortId
              authority
              consumedGenerated
              withPristine
          )
      withConsumed =
        Controlled.commitControlledReservationDelete
          ( checked
              "consume reservation before End"
              ( Controlled.prepareControlledReservationDelete
                  process
                  nabla
                  authority
                  consumedGenerated
                  withConsumedReservation
              )
          )
      peerCurrent = publication peerGenerated ((ProcessLabel process, 0)) 80 False 10
      withPeer = applyPeer descriptor occurrence peerCurrent withConsumed
      withStore = commitStoreRead deltaA Normal peerCurrent withPeer
      retired =
        Controlled.commitControlledProcessRetirement
          (Controlled.prepareControlledProcessRetirement process withStore)
  assertBool "the exact epoch is monotonically ended" (Controlled.controlledProcessEnded process retired)
  assertBool
    "direct bootstrap and publication possessions are revoked"
    ( all
        (\target -> not (Controlled.controlledHasNormalPossession process target retired))
        [ globalObjectIdFromNablaId nabla,
          object,
          peerObject
        ]
    )
  assertBool
    "no effective Normal possession entry names the ended epoch"
    (all ((/= process) . fst) (Controlled.controlledNormalPossessionEntries retired))
  assertEqual
    "the exact Store possession source is removed"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA peerObject retired)
  assertEqual
    "the pristine reservation is cancelled"
    Nothing
    (Controlled.controlledReservationWitness pristineGenerated retired)
  let afterLateReservation =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              sortId
              authority
              pristineGenerated
              retired
          )
  assertBool
    "late reservation preparation for an ended epoch is state-identical"
    (afterLateReservation == retired)
  used <-
    maybe
      (assertFailure "used reservation history was unexpectedly removed")
      pure
      (Controlled.controlledReservationWitness generated retired)
  assertEqual "used reservation history remains terminal" Controlled.Published (Controlled.reservationWitnessPhase used)
  consumed <-
    maybe
      (assertFailure "delete-consumed reservation history was unexpectedly removed")
      pure
      (Controlled.controlledReservationWitness consumedGenerated retired)
  assertEqual
    "delete-consumed reservation history remains terminal"
    Controlled.ConsumedByDelete
    (Controlled.reservationWitnessPhase consumed)
  lateStorePrepared <-
    checkedIO
      "late Store observation after End"
      ( Controlled.prepareControlledStoreRead
          process
          deltaA
          Normal
          (observation descriptor occurrence peerCurrent)
          retired
      )
  let afterLateStore = Controlled.commitControlledStoreObservation lateStorePrepared
  assertEqual
    "late Store history cannot recreate an effective possession source"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA peerObject afterLateStore)

  let latePeer = publication generated ((ProcessLabel process, 0)) 83 False 100
  latePeerPrepared <-
    checkedIO
      "late peer observation of historical local source"
      (preparePeer descriptor occurrence latePeer afterLateStore)
  let (afterLatePeer, _) = Controlled.commitControlledPeerObservation latePeerPrepared
  assertBool
    "a peer callback that reinstalls the historical record cannot resurrect authority"
    (not (Controlled.controlledHasNormalPossession process object afterLatePeer))
  let retiredAgain =
        Controlled.commitControlledProcessRetirement
          (Controlled.prepareControlledProcessRetirement process afterLatePeer)
  assertBool "repeated retirement is state-identical" (retiredAgain == afterLatePeer)

caseObservationKindBoundary :: Assertion
caseObservationKindBoundary = do
  let regular = regularPublication peerGenerated 70 False 1
  assertEqual
    "a regular descriptor cannot produce checked controlled evidence"
    (Left Controlled.ControlledObservationNotControlled)
    (Controlled.checkControlledObservation regularDescriptor occurrence regular)

caseStoreReadStrength :: Assertion
caseStoreReadStrength = do
  let current = publication peerGenerated ((ProcessLabel process, 0)) 80 False 10
      observed = applyPeer descriptor occurrence current controlledState
      normalRead = commitStoreRead deltaA Normal current observed
      weakRead = commitStoreRead deltaA Weak current observed
  case Controlled.prepareControlledStoreRead
    process
    deltaA
    Normal
    (observation descriptor occurrence current)
    controlledState of
    Left (Controlled.ControlledStoreRecordUnavailable missing) ->
      assertEqual "missing record identity" peerObject missing
    other -> assertFailure ("expected missing-record rejection, got " <> resultShape other)
  assertBool
    "the matching controlled record alone grants no possession"
    (not (Controlled.controlledHasNormalPossession process peerObject observed))
  assertEqual
    "Normal is retained exactly"
    (Just Normal)
    (Controlled.controlledStorePossessionStrength process deltaA peerObject normalRead)
  assertBool
    "a Normal Store source grants normal possession"
    (Controlled.controlledHasNormalPossession process peerObject normalRead)
  assertEqual
    "Weak is retained exactly"
    (Just Weak)
    (Controlled.controlledStorePossessionStrength process deltaA peerObject weakRead)
  assertBool
    "a Weak Store source does not grant normal possession"
    (not (Controlled.controlledHasNormalPossession process peerObject weakRead))
  assertEqual
    "a Weak Store source cannot authorize a selected grant"
    (Left (Controlled.ControlledGrantSourceNotPossessed process peerObject))
    (Controlled.checkControlledGrant process peerObject weakRead)
  grant <- case Controlled.checkControlledGrant process peerObject normalRead of
    Right accepted -> pure accepted
    Left problem -> assertFailure ("normal source grant rejected: " <> show problem)
  assertEqual
    "a Normal source authorizes the exact granted object"
    peerObject
    (Controlled.controlledGrantObject grant)

caseStoreSourceAggregation :: Assertion
caseStoreSourceAggregation = do
  let current = publication peerGenerated ((ProcessLabel process, 0)) 81 False 10
      observed = applyPeer descriptor occurrence current controlledState
      afterWeak = commitStoreRead deltaA Weak current observed
      afterBoth = commitStoreRead deltaB Normal current afterWeak
      afterNormalTake = commitStoreTake deltaB Normal current afterBoth
      afterWeakTake = commitStoreTake deltaA Weak current afterNormalTake
  assertEqual
    "the first delta retains Weak"
    (Just Weak)
    (Controlled.controlledStorePossessionStrength process deltaA peerObject afterBoth)
  assertEqual
    "the second delta independently retains Normal"
    (Just Normal)
    (Controlled.controlledStorePossessionStrength process deltaB peerObject afterBoth)
  assertBool
    "any Normal delta is sufficient"
    (Controlled.controlledHasNormalPossession process peerObject afterBoth)
  assertEqual
    "taking the Normal delta leaves the Weak source"
    (Just Weak)
    (Controlled.controlledStorePossessionStrength process deltaA peerObject afterNormalTake)
  assertEqual
    "only the exact taken delta is removed"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaB peerObject afterNormalTake)
  assertBool
    "remaining Weak evidence recomputes to no normal possession"
    (not (Controlled.controlledHasNormalPossession process peerObject afterNormalTake))
  assertEqual
    "taking the remaining delta removes its source"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA peerObject afterWeakTake)

caseStoreTakeEvidenceBoundaries :: Assertion
caseStoreTakeEvidenceBoundaries = do
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      (published, _) = commitFirst first reservedState
      withStoreSource = commitStoreRead deltaA Normal first published
      afterTake = commitStoreTake deltaA Normal first withStoreSource
  assertEqual
    "take removes the Store-derived publisher source"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA object afterTake)
  assertBool
    "the separately retained publisher evidence survives"
    (Controlled.controlledHasNormalPossession process object afterTake)
  publishedRecord <- oneRecord object afterTake
  assertEqual
    "publisher provenance remains separate"
    (Set.singleton process)
    (Controlled.controlledRecordPossessionSources publishedRecord)

  let current = publication peerGenerated ((ProcessLabel process, 0)) 82 False 10
      obsolete = publication peerGenerated ((ProcessLabel process, 0)) 83 True 0
      obsoleted = applyPeer descriptor occurrence obsolete (applyPeer descriptor occurrence current controlledState)
      suppressedRead = commitStoreRead deltaA Normal current obsoleted
  suppressedRecord <- oneRecord peerObject obsoleted
  assertBool
    "suppression without a Store source grants no possession"
    (not (Controlled.controlledHasNormalPossession process peerObject obsoleted))
  assertEqual
    "a suppressed current read installs no source"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA peerObject suppressedRead)
  assertBool
    "a suppressed current still grants no possession"
    (not (Controlled.controlledHasNormalPossession process peerObject suppressedRead))
  let obsoleteRead = commitStoreRead deltaA Normal obsolete obsoleted
      obsoleteTaken = commitStoreTake deltaA Normal obsolete obsoleteRead
  takenRecord <- oneRecord peerObject obsoleteTaken
  assertEqual
    "local-take preserves the exact hidden suppression"
    (Controlled.controlledRecordSuppression suppressedRecord)
    (Controlled.controlledRecordSuppression takenRecord)

casePeerStoreObsolescenceSchedules :: Assertion
casePeerStoreObsolescenceSchedules = do
  let current = publication peerGenerated ((ProcessLabel process, 0)) 90 False 10
      obsolete = publication peerGenerated ((ProcessLabel process, 0)) 91 True 0
      lateCurrent = publication peerGenerated ((ProcessLabel process, 0)) 92 False 100
      currentThenObsolete = applyPeerStore [(Normal, current), (Weak, obsolete)]
      obsoleteThenCurrent = applyPeerStore [(Weak, obsolete), (Normal, current)]
  assertObsoleteSchedule "current then obsolete" obsolete currentThenObsolete
  assertObsoleteSchedule "obsolete then current" obsolete obsoleteThenCurrent

  let (controlledBeforeTake, storeBeforeTake) = currentThenObsolete
      key = checkedPublicationKey obsolete
      (taken, hiddenStore) = Store.localTake key storeBeforeTake
  stored <- maybe (assertFailure "expected visible obsolete Store winner") pure taken
  let withSource =
        commitStoreRead
          deltaA
          (Store.storedStrength stored)
          (Store.storedPublication stored)
          controlledBeforeTake
      controlledAfterTake =
        commitStoreTake
          deltaA
          (Store.storedStrength stored)
          (Store.storedPublication stored)
          withSource
      suppressionBefore =
        Controlled.controlledRecordSuppression (recordOrError peerObject controlledBeforeTake)
      suppressionAfter =
        Controlled.controlledRecordSuppression (recordOrError peerObject controlledAfterTake)
      controlledAfterLate = applyPeer descriptor occurrence lateCurrent controlledAfterTake
      storeAfterLate =
        checked
          "late current after local-take"
          (Store.applyPublication Weak lateCurrent hiddenStore)
  assertEqual "local-take hides the visible Store object" Nothing (Store.lookupVisible key hiddenStore)
  assertEqual "local-take does not remove Controlled suppression" suppressionBefore suppressionAfter
  assertEqual
    "the exact Store possession source is gone"
    Nothing
    (Controlled.controlledStorePossessionStrength process deltaA peerObject controlledAfterTake)
  assertEqual
    "a late current remains suppressed in Controlled"
    Controlled.ControlledObsolete
    (Controlled.controlledRecordLifecycle (recordOrError peerObject controlledAfterLate))
  assertEqual "a late current remains hidden in Store" Nothing (Store.lookupVisible key storeAfterLate)

caseEffectivePossessionStrength :: Assertion
caseEffectivePossessionStrength = do
  let current = publication peerGenerated ((ProcessLabel process, 0)) 94 False 10
      observed = applyPeer descriptor occurrence current controlledState
      afterWeak = commitStoreRead deltaA Weak current observed
      afterBoth = commitStoreRead deltaB Normal current afterWeak
      afterNormalTake = commitStoreTake deltaB Normal current afterBoth
      afterEveryTake = commitStoreTake deltaA Weak current afterNormalTake
      first = publication generated ((ProcessLabel process, 0)) 1 False 10
      (published, _) = commitFirst first reservedState
      obsolete = publication peerGenerated ((ProcessLabel process, 0)) 95 True 10
      suppressed = applyPeer descriptor occurrence obsolete observed
  assertEqual
    "missing object has no effective possession"
    Nothing
    (Controlled.controlledEffectivePossessionStrength process object controlledState)
  assertEqual
    "another process cannot borrow the caller's source"
    Nothing
    (Controlled.controlledEffectivePossessionStrength otherProcess peerObject afterBoth)
  assertEqual
    "a sole weak Store source remains weak"
    (Just Weak)
    (Controlled.controlledEffectivePossessionStrength process peerObject afterWeak)
  assertEqual
    "the strongest Store source wins"
    (Just Normal)
    (Controlled.controlledEffectivePossessionStrength process peerObject afterBoth)
  assertEqual
    "removing the Normal source reveals the retained Weak source"
    (Just Weak)
    (Controlled.controlledEffectivePossessionStrength process peerObject afterNormalTake)
  assertEqual
    "removing every Store source removes effective possession"
    Nothing
    (Controlled.controlledEffectivePossessionStrength process peerObject afterEveryTake)
  assertEqual
    "publisher evidence is independently Normal"
    (Just Normal)
    (Controlled.controlledEffectivePossessionStrength process object published)
  assertEqual
    "obsolete suppression alone grants no possession"
    Nothing
    (Controlled.controlledEffectivePossessionStrength process peerObject suppressed)

propNormalPossessionEntries :: Bool -> Property
propNormalPossessionEntries reverseOrder =
  let current = publication peerGenerated ((ProcessLabel process, 0)) 96 False 10
      observed = applyPeer descriptor occurrence current controlledState
      readSchedule =
        [ (deltaA, Normal),
          (deltaB, Normal)
        ]
      ordered = if reverseOrder then reverse readSchedule else readSchedule
      successor =
        foldl
          (\state (delta, strength) -> commitStoreRead delta strength current state)
          observed
          ordered
      entries = Controlled.controlledNormalPossessionEntries successor
      canonicalEntries = Set.toAscList (Set.fromList entries)
      peerEntries = filter (== (process, peerObject)) entries
      querySound (owner, target) =
        Controlled.controlledEffectivePossessionStrength owner target successor
          == Just Normal
   in counterexample
        ("reverseOrder=" <> show reverseOrder <> ", entries=" <> show entries)
        ( entries == canonicalEntries
            && peerEntries == [(process, peerObject)]
            && all querySound entries
        )

caseObsoleteObservationTime :: Assertion
caseObsoleteObservationTime = do
  let current = publication peerGenerated ((ProcessLabel process, 0)) 97 False 10
      winningObsolete = publication peerGenerated ((ProcessLabel process, 0)) 98 True 100
      nonwinningObsolete = publication peerGenerated ((ProcessLabel process, 0)) 99 True 0
      laterWinner = publication peerGenerated ((ProcessLabel process, 0)) 100 True 200
      currentState = applyPeer descriptor occurrence current controlledState
      obsoleteState =
        applyPeer
          descriptor
          occurrence
          nonwinningObsolete
          (applyPeer descriptor occurrence winningObsolete currentState)
  assertBool
    "the fixture has a strictly non-winning obsolete publication"
    (checkedPublicationWinnerKey nonwinningObsolete < checkedPublicationWinnerKey winningObsolete)
  assertEqual
    "current state has no obsolete observation instant"
    Nothing
    (obsoleteObservedAt peerObject currentState)
  case prepareObsoleteObservationTime 40 current currentState of
    Left (Controlled.ControlledObsoleteObservationPublicationCurrent identifier) ->
      assertEqual "current publication identity" (checkedPublicationId current) identifier
    other ->
      assertFailure
        ("expected current-publication rejection, got " <> resultShape other)
  case prepareObsoleteObservationTime 41 nonwinningObsolete obsoleteState of
    Left (Controlled.ControlledObsoleteObservationPublicationNotWinner observed winner) -> do
      assertEqual "non-winner identity" (checkedPublicationId nonwinningObsolete) observed
      assertEqual "retained winner identity" (checkedPublicationId winningObsolete) winner
    other ->
      assertFailure
        ("expected non-winning-obsolete rejection, got " <> resultShape other)
  assertEqual
    "rejected observations install no instant"
    Nothing
    (obsoleteObservedAt peerObject obsoleteState)

  prepared <-
    checkedIO
      "winning obsolete observation"
      (prepareObsoleteObservationTime 50 winningObsolete obsoleteState)
  assertEqual
    "first observation is classified as recorded"
    Controlled.ControlledObsoleteObservationTimeRecorded
    (Controlled.preparedControlledObsoleteObservationTimeResult prepared)
  let (recorded, recordedResult) =
        Controlled.commitControlledObsoleteObservationTime prepared
  assertEqual
    "commit records the first observation"
    Controlled.ControlledObsoleteObservationTimeRecorded
    recordedResult
  assertEqual
    "first local monotonic instant is retained"
    (Just (monotonicInstant 50))
    (obsoleteObservedAt peerObject recorded)

  replay <-
    checkedIO
      "winning obsolete replay"
      (prepareObsoleteObservationTime 60 winningObsolete recorded)
  let (replayed, replayResult) =
        Controlled.commitControlledObsoleteObservationTime replay
  assertEqual
    "a later replay retains the original instant"
    Controlled.ControlledObsoleteObservationTimeRetained
    replayResult
  assertBool "a replay is state-idempotent" (replayed == recorded)

  earlierReplay <-
    checkedIO
      "earlier-valued runtime replay"
      (prepareObsoleteObservationTime 49 winningObsolete replayed)
  let (earlierReplayed, _) =
        Controlled.commitControlledObsoleteObservationTime earlierReplay
  assertEqual
    "the owner never replaces the first retained instant"
    (Just (monotonicInstant 50))
    (obsoleteObservedAt peerObject earlierReplayed)

  let advanced = applyPeer descriptor occurrence laterWinner earlierReplayed
  advancedObservation <-
    checkedIO
      "later obsolete winner observation"
      (prepareObsoleteObservationTime 70 laterWinner advanced)
  let (afterLaterWinner, laterResult) =
        Controlled.commitControlledObsoleteObservationTime advancedObservation
  assertEqual
    "a later obsolete winner cannot restart retention"
    Controlled.ControlledObsoleteObservationTimeRetained
    laterResult
  assertEqual
    "the object-level first obsolete instant remains fixed"
    (Just (monotonicInstant 50))
    (obsoleteObservedAt peerObject afterLaterWinner)

propStoreSourceReadOrder :: Bool -> Property
propStoreSourceReadOrder reverseOrder =
  let current = publication peerGenerated ((ProcessLabel process, 0)) 93 False 10
      observed = applyPeer descriptor occurrence current controlledState
      sourceReads =
        [ (deltaA, Weak),
          (deltaB, Normal)
        ]
      ordered = if reverseOrder then reverse sourceReads else sourceReads
      successor =
        foldl
          (\state (delta, strength) -> commitStoreRead delta strength current state)
          observed
          ordered
   in counterexample
        ("reverseOrder=" <> show reverseOrder)
        ( Controlled.controlledStorePossessionStrength process deltaA peerObject successor == Just Weak
            && Controlled.controlledStorePossessionStrength process deltaB peerObject successor == Just Normal
            && Controlled.controlledEffectivePossessionStrength process peerObject successor == Just Normal
            && Controlled.controlledHasNormalPossession process peerObject successor
        )

assertObsoleteSchedule ::
  String ->
  CheckedPublication ->
  (Controlled.State, Store.DeltaStore) ->
  Assertion
assertObsoleteSchedule context obsolete (controlled, store) = do
  assertEqual
    (context <> ": Controlled lifecycle")
    Controlled.ControlledObsolete
    (Controlled.controlledRecordLifecycle (recordOrError peerObject controlled))
  assertEqual
    (context <> ": Store winner")
    (Just obsolete)
    (Store.storedPublication <$> Store.lookupVisible (checkedPublicationKey obsolete) store)

applyPeerStore ::
  [(ReplicaStrength, CheckedPublication)] ->
  (Controlled.State, Store.DeltaStore)
applyPeerStore =
  foldl applyOne (controlledState, Store.emptyDeltaStore sortId)
  where
    applyOne (controlled, store) (strength, checkedPublication) =
      ( applyPeer descriptor occurrence checkedPublication controlled,
        checked
          "peer Store observation"
          (Store.applyPublication strength checkedPublication store)
      )

commitStoreRead ::
  DeltaId ->
  ReplicaStrength ->
  CheckedPublication ->
  Controlled.State ->
  Controlled.State
commitStoreRead delta strength checkedPublication state =
  Controlled.commitControlledStoreObservation
    ( checked
        "controlled Store read"
        ( Controlled.prepareControlledStoreRead
            process
            delta
            strength
            (observation descriptor occurrence checkedPublication)
            state
        )
    )

commitStoreTake ::
  DeltaId ->
  ReplicaStrength ->
  CheckedPublication ->
  Controlled.State ->
  Controlled.State
commitStoreTake delta strength checkedPublication state =
  Controlled.commitControlledStoreObservation
    ( checked
        "controlled Store local-take"
        ( Controlled.prepareControlledStoreLocalTake
            process
            delta
            strength
            (observation descriptor occurrence checkedPublication)
            state
        )
    )

propPeerLifecycleMonotone :: [Bool] -> Property
propPeerLifecycleMonotone obsoleteFlags =
  let flags = False : obsoleteFlags
      publications =
        zipWith
          ( \sequenceNumber obsolete ->
              publication
                peerGenerated
                ((ProcessLabel process, 0))
                sequenceNumber
                obsolete
                (fromIntegral sequenceNumber)
          )
          [100 ..]
          flags
      final = foldl (flip (applyPeer descriptor occurrence)) controlledState publications
      finalLifecycle = Controlled.controlledRecordLifecycle (recordOrError peerObject final)
      expected =
        if or flags
          then Controlled.ControlledObsolete
          else Controlled.ControlledCurrent
   in counterexample
        ("flags=" <> show flags <> ", lifecycle=" <> show finalLifecycle)
        (finalLifecycle == expected)

propFirstUseGeneration :: Property
propFirstUseGeneration =
  forAll (chooseInt (1, 100000)) $ \generation ->
    forAll (elements [ProcessLabel process, VoidLabel]) $ \owner ->
      let supplied = (owner, fromIntegral generation)
          first = publication generated supplied 1 False 10
       in conjoin
            [ (() <$ prepareFirst first reservedState)
                === Left (Controlled.ControlledFirstUseInitialLabelUnavailable supplied),
              Controlled.reservationWitnessPhase (singleWitness reservedState) === Controlled.Reserved,
              Controlled.controlledLocalRecord object reservedState === Nothing
            ]

propFirstUseWinnerStable :: [Bool] -> Property
propFirstUseWinnerStable choices =
  let first = publication generated ((ProcessLabel process, 0)) 1 False 10
      (published, _) = commitFirst first reservedState
      competitors =
        zipWith
          ( \sequenceNumber obsolete ->
              publication
                generated
                ((ProcessLabel process, 0))
                sequenceNumber
                obsolete
                (fromIntegral sequenceNumber)
          )
          [2 ..]
          choices
      allRejected = all (isLeft . (`prepareFirst` published)) competitors
   in counterexample
        ("winner=" <> show (checkedPublicationId (latestPublication object published)))
        ( allRejected
            && latestPublication object published == first
            && Controlled.reservationWitnessFirstPublicationId (singleWitness published)
              == Just (checkedPublicationId first)
        )

prepareFirst ::
  CheckedPublication ->
  Controlled.State ->
  Either Controlled.ControlledFirstUseError Controlled.PreparedControlledFirstUse
prepareFirst = prepareFirstWith descriptor occurrence

prepareFirstWith ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Controlled.State ->
  Either Controlled.ControlledFirstUseError Controlled.PreparedControlledFirstUse
prepareFirstWith canonical effectiveOccurrence checkedPublication =
  Controlled.prepareControlledFirstUse
    process
    nabla
    authority
    generated
    (observation canonical effectiveOccurrence checkedPublication)

prepareUpdate ::
  CheckedPublication ->
  Controlled.State ->
  Either Controlled.ControlledUpdateError Controlled.PreparedControlledUpdate
prepareUpdate checkedPublication =
  Controlled.prepareControlledUpdate
    process
    nabla
    authority
    (observation descriptor occurrence checkedPublication)

preparePeer ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Controlled.State ->
  Either Controlled.ControlledPeerObservationError Controlled.PreparedControlledPeerObservation
preparePeer canonical effectiveOccurrence checkedPublication =
  Controlled.prepareControlledPeerObservation
    (observation canonical effectiveOccurrence checkedPublication)

observation ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Controlled.CheckedControlledObservation
observation canonical effectiveOccurrence checkedPublication =
  checked
    "controlled observation"
    (Controlled.checkControlledObservation canonical effectiveOccurrence checkedPublication)

prepareObsoleteObservationTime ::
  Word64 ->
  CheckedPublication ->
  Controlled.State ->
  Either
    Controlled.ControlledObsoleteObservationTimeError
    Controlled.PreparedControlledObsoleteObservationTime
prepareObsoleteObservationTime observedAt checkedPublication =
  Controlled.prepareControlledObsoleteObservationTime
    (monotonicInstant observedAt)
    (observation descriptor occurrence checkedPublication)

obsoleteObservedAt :: GlobalObjectId -> Controlled.State -> Maybe MonotonicInstant
obsoleteObservedAt target =
  Controlled.controlledRecordObsoleteObservedAt . recordOrError target

commitFirst :: CheckedPublication -> Controlled.State -> (Controlled.State, Controlled.ControlledFirstUseResult)
commitFirst checkedPublication state =
  Controlled.commitControlledFirstUse
    (either (error . show) id (prepareFirst checkedPublication state))

commitUpdate :: CheckedPublication -> Controlled.State -> (Controlled.State, Controlled.ControlledUpdateResult)
commitUpdate checkedPublication state =
  Controlled.commitControlledUpdate
    (either (error . show) id (prepareUpdate checkedPublication state))

applyPeer ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Controlled.State ->
  Controlled.State
applyPeer canonical effectiveOccurrence checkedPublication state =
  fst
    ( Controlled.commitControlledPeerObservation
        ( either
            (error . show)
            id
            (preparePeer canonical effectiveOccurrence checkedPublication state)
        )
    )

assertFirstUseRejected :: String -> CheckedPublication -> Controlled.State -> Assertion
assertFirstUseRejected context checkedPublication state =
  case prepareFirst checkedPublication state of
    Left Controlled.ControlledFirstUseConflict {} -> pure ()
    other -> assertFailure (context <> ": expected conflict, got " <> resultShape other)

deleteError ::
  ProcessEpochId ->
  NablaId ->
  AuthorityEpoch ->
  GlobalUniqueId ->
  Controlled.State ->
  Either Controlled.ControlledReservationDeleteError ()
deleteError owner writer currentAuthority target state =
  case Controlled.prepareControlledReservationDelete
    owner
    writer
    currentAuthority
    target
    state of
    Left problem -> Left problem
    Right _ -> Right ()

latestPublication :: GlobalObjectId -> Controlled.State -> CheckedPublication
latestPublication target = Controlled.controlledRecordLatestPublication . recordOrError target

oneRecord :: GlobalObjectId -> Controlled.State -> IO Controlled.ControlledLocalRecord
oneRecord target state =
  maybe
    (assertFailure "expected controlled local record")
    pure
    (Controlled.controlledLocalRecord target state)

recordOrError :: GlobalObjectId -> Controlled.State -> Controlled.ControlledLocalRecord
recordOrError target state =
  maybe (error "expected controlled local record") id (Controlled.controlledLocalRecord target state)

oneWitness :: Controlled.State -> IO Controlled.ReservationWitness
oneWitness state = case Controlled.controlledReservationWitnesses state of
  [witness] -> pure witness
  actual -> assertFailure ("expected one reservation, got " <> show (length actual))

singleWitness :: Controlled.State -> Controlled.ReservationWitness
singleWitness state = case Controlled.controlledReservationWitnesses state of
  [witness] -> witness
  actual -> error ("expected one reservation, got " <> show (length actual))

reservedState :: Controlled.State
reservedState = reservedStateWith sortId

reservedStateWith :: SortId -> Controlled.State
reservedStateWith reservedSort =
  Controlled.commitControlledReservation
    ( Controlled.prepareControlledReservation
        process
        nabla
        reservedSort
        authority
        generated
        controlledState
    )

reservationRetirementState :: Controlled.State
reservationRetirementState =
  reserve
    process
    nabla
    cancelledGeneratedLow
    ( reserve
        process
        nabla
        cancelledGeneratedHigh
        (reserve process otherNabla unrelatedGenerated withConsumed)
    )
  where
    first = publication generated ((ProcessLabel process, 0)) 1 False 10
    (published, _) = commitFirst first reservedState
    withConsumedReservation =
      reserve process nabla consumedGenerated published
    withConsumed =
      Controlled.commitControlledReservationDelete
        ( checked
            "consume retirement-fixture reservation"
            ( Controlled.prepareControlledReservationDelete
                process
                nabla
                authority
                consumedGenerated
                withConsumedReservation
            )
        )

reserve ::
  ProcessEpochId ->
  NablaId ->
  GlobalUniqueId ->
  Controlled.State ->
  Controlled.State
reserve owner writer target state =
  Controlled.commitControlledReservation
    ( Controlled.prepareControlledReservation
        owner
        writer
        sortId
        authority
        target
        state
    )

controlledState :: Controlled.State
controlledState =
  Controlled.commitControlledBootstrap
    ( checked
        "controlled bootstrap"
        ( Controlled.prepareControlledBootstrap
            processFact
            [writerFact, readerFact deltaA, readerFact deltaB]
            Controlled.emptyState
        )
    )

processFact :: Controlled.ProcessFact
processFact =
  Controlled.processFact
    processIdentity
    process
    herald
    authority

writerFact :: Controlled.RootFact
writerFact =
  Controlled.writerRootFact
    process
    NeutralVertexRole
    sortId
    occurrence
    authority
    (controlIndex 1)
    nabla
    UnsequencedNabla

readerFact :: DeltaId -> Controlled.RootFact
readerFact delta =
  Controlled.readerRootFact
    process
    NeutralVertexRole
    sortId
    occurrence
    authority
    (controlIndex 1)
    delta

publication ::
  GlobalUniqueId ->
  Label ->
  Word64 ->
  Bool ->
  Integer ->
  CheckedPublication
publication = publicationWithDescriptor descriptor

publicationWithDescriptor ::
  CanonicalDescriptor ->
  GlobalUniqueId ->
  Label ->
  Word64 ->
  Bool ->
  Integer ->
  CheckedPublication
publicationWithDescriptor canonical target publicationLabel sequenceNumber obsolete payload =
  checked
    "checked controlled publication"
    ( mkCheckedPublication
        canonical
        (publicationIdentifier sequenceNumber)
        (controlledValue target publicationLabel obsolete payload)
    )

regularPublication ::
  GlobalUniqueId ->
  Word64 ->
  Bool ->
  Integer ->
  CheckedPublication
regularPublication target sequenceNumber obsolete payload =
  checked
    "checked regular publication"
    ( mkCheckedPublication
        regularDescriptor
        (publicationIdentifier sequenceNumber)
        (regularValue target obsolete payload)
    )

publicationIdentifier :: Word64 -> PublicationId
publicationIdentifier sequenceNumber =
  publicationId nabla authority herald (nablaSequence sequenceNumber)

controlledValue :: GlobalUniqueId -> Label -> Bool -> Integer -> Value
controlledValue target publicationLabel obsolete payload =
  checked
    "controlled value"
    ( recordValue
        [ (objectField, globalUniqueIdValue target),
          (labelField, labelValue publicationLabel),
          (obsoleteField, boolValue obsolete),
          (payloadField, int64Value (fromInteger payload))
        ]
    )

regularValue :: GlobalUniqueId -> Bool -> Integer -> Value
regularValue target obsolete payload =
  checked
    "regular value"
    ( recordValue
        [ (objectField, globalUniqueIdValue target),
          (obsoleteField, boolValue obsolete),
          (payloadField, int64Value (fromInteger payload))
        ]
    )

descriptor :: CanonicalDescriptor
descriptor = canonicalControlledDescriptor 0

alternateDescriptor :: CanonicalDescriptor
alternateDescriptor = canonicalControlledDescriptor 1

canonicalControlledDescriptor :: Word64 -> CanonicalDescriptor
canonicalControlledDescriptor minimumRetention =
  canonicalControlledDescriptorWithRole minimumRetention Nothing

canonicalControlledDescriptorWithRole ::
  Word64 ->
  Maybe StructuralCarrierRole ->
  CanonicalDescriptor
canonicalControlledDescriptorWithRole minimumRetention structuralRole =
  canonicalizeCheckedDescriptor
    ( checked
        "controlled descriptor"
        ( checkDescriptor
            ( maybe
                ApplicationDescriptor
                (const PrimordialDescriptor)
                structuralRole
            )
            DescriptorSpec
              { descriptorSpecKind = ControlledSort,
                descriptorSpecSchema =
                  checked
                    "controlled schema"
                    ( recordSchema
                        [ (objectField, globalUniqueIdSchema),
                          (labelField, labelSchema),
                          (obsoleteField, boolSchema),
                          (payloadField, int64Schema)
                        ]
                    ),
                descriptorSpecKeyProjections = [directProjection objectField],
                descriptorSpecValidity = AlwaysPredicate,
                descriptorSpecObsolescence = case structuralRole of
                  Nothing ->
                    CompareField
                      (directProjection obsoleteField)
                      ScalarEqual
                      (LiteralBool True)
                  Just _ -> NeverPredicate,
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = minimumRetention,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Just labelField,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = structuralRole
              }
        )
    )

regularDescriptor :: CanonicalDescriptor
regularDescriptor =
  canonicalizeCheckedDescriptor
    ( checked
        "regular descriptor"
        ( checkDescriptor
            ApplicationDescriptor
            DescriptorSpec
              { descriptorSpecKind = RegularSort,
                descriptorSpecSchema =
                  checked
                    "regular schema"
                    ( recordSchema
                        [ (objectField, globalUniqueIdSchema),
                          (obsoleteField, boolSchema),
                          (payloadField, int64Schema)
                        ]
                    ),
                descriptorSpecKeyProjections = [directProjection objectField],
                descriptorSpecValidity = AlwaysPredicate,
                descriptorSpecObsolescence =
                  CompareField
                    (directProjection obsoleteField)
                    ScalarEqual
                    (LiteralBool True),
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = 0,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Nothing,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = Nothing
              }
        )
    )

sortId :: SortId
sortId = descriptorSortId descriptor

objectField, labelField, obsoleteField, payloadField :: FieldName
objectField = checked "object field" (mkFieldName "object")
labelField = checked "label field" (mkFieldName "label")
obsoleteField = checked "obsolete field" (mkFieldName "obsolete")
payloadField = checked "payload field" (mkFieldName "payload")

process :: ProcessEpochId
process = checked "process" (mkProcessEpochId (identifierBytes 0x11))

otherProcess :: ProcessEpochId
otherProcess = checked "other process" (mkProcessEpochId (identifierBytes 0x12))

processIdentity :: ProcessId
processIdentity = checked "process identity" (mkProcessId (identifierBytes 0x13))

herald :: HeraldEpoch
herald = checked "herald" (mkHeraldEpoch (identifierBytes 0x14))

nabla :: NablaId
nabla = checked "nabla" (mkNablaId (identifierBytes 0x21))

otherNabla :: NablaId
otherNabla = checked "other nabla" (mkNablaId (identifierBytes 0x24))

deltaA, deltaB :: DeltaId
deltaA = checked "delta A" (mkDeltaId (identifierBytes 0x22))
deltaB = checked "delta B" (mkDeltaId (identifierBytes 0x23))

generated :: GlobalUniqueId
generated = checked "generated" (mkGlobalUniqueId (identifierBytes 0x31))

peerGenerated :: GlobalUniqueId
peerGenerated = checked "peer generated" (mkGlobalUniqueId (identifierBytes 0x41))

pristineGenerated :: GlobalUniqueId
pristineGenerated = checked "pristine generated" (mkGlobalUniqueId (identifierBytes 0x42))

consumedGenerated :: GlobalUniqueId
consumedGenerated = checked "delete-consumed generated" (mkGlobalUniqueId (identifierBytes 0x45))

cancelledGeneratedLow :: GlobalUniqueId
cancelledGeneratedLow = checked "cancelled generated low" (mkGlobalUniqueId (identifierBytes 0x32))

cancelledGeneratedHigh :: GlobalUniqueId
cancelledGeneratedHigh = checked "cancelled generated high" (mkGlobalUniqueId (identifierBytes 0x46))

unrelatedGenerated :: GlobalUniqueId
unrelatedGenerated = checked "unrelated generated" (mkGlobalUniqueId (identifierBytes 0x47))

object :: GlobalObjectId
object = globalObjectIdFromGlobalUniqueId generated

peerObject :: GlobalObjectId
peerObject = globalObjectIdFromGlobalUniqueId peerGenerated

occurrence :: SortDefinitionOccurrenceId
occurrence = checked "occurrence" (mkSortDefinitionOccurrenceId (identifierBytes 0x51))

alternateOccurrence :: SortDefinitionOccurrenceId
alternateOccurrence = checked "alternate occurrence" (mkSortDefinitionOccurrenceId (identifierBytes 0x52))

authority :: AuthorityEpoch
authority = genesisAuthorityEpoch

alternateAuthority :: AuthorityEpoch
alternateAuthority =
  structuralAuthorityEpoch
    (structuralOccurrenceId herald firstStructuralSequence)
    (checked "topology cut" (mkTopologyCutId (identifierBytes 0x61)))

identifierBytes :: Word8 -> ByteString.ByteString
identifierBytes byte = ByteString.replicate 32 byte

isLeft :: Either left right -> Bool
isLeft = either (const True) (const False)

resultShape :: Either error value -> String
resultShape = either (const "Left") (const "Right")

checkedIO :: (Show error) => String -> Either error value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

checked :: (Show error) => String -> Either error value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
