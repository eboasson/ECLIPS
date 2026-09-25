{-# LANGUAGE OverloadedRecordDot #-}

module PublicationProperties
  ( tests,
  )
where

import GenesisFixtures (fixtureRetirementResolution)

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    HeraldEpoch,
    NablaId,
    NablaSequence,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndex,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    mkDeltaId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (..),
    freezeRoute,
    routeDestination,
  )
import Eclips.Domain.Sort.Profile (sortDefinitionValue)
import Eclips.Domain.Startup
  ( PredefinedSortRole (SortDefinitionRole),
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
  )
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    bytesValue,
    canonicalValueBytes,
  )
import Eclips.Herald.Application.Request.Internal
  ( ProcessAcceptancePosition,
    processAcceptancePosition,
  )
import Eclips.Herald.Genesis.Internal
  ( PrimordialDefinitionReplica,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PublicationBatch,
    PublicationBatchProblem (..),
    PublicationDestination,
    mkPublicationBatch,
    peerPublicationDigest,
    presentedOrdinaryPeerPublication,
    publicationBatchCanonicalValue,
    publicationBatchControlPrerequisite,
    publicationBatchDestinations,
    publicationBatchHasFenceDependency,
    publicationBatchId,
    publicationBatchOccurrenceId,
    publicationBatchSortId,
    publicationBatchSourceProcess,
    publicationBatchSourceStrength,
    publicationBatchSourceTopologyPrerequisite,
    publicationDestination,
    publicationDestinationDelta,
    publicationDestinationStoreIncarnation,
  )
import Eclips.Herald.PeerStream
  ( PeerItemDigest,
    StreamDirection,
    StreamSequence,
    firstStreamSequence,
    mkPeerItemDigest,
    mkStreamDirection,
    mkStreamSequence,
    nextStreamSequence,
    peerItemDigestBytes,
    sequencedItem,
    sequencedItemDigest,
  )
import Eclips.Herald.Publication.State
  ( DestinationOutcome (..),
    IncomingDisposition (..),
    IncomingPublicationClassification (..),
    IncomingPublicationProblem (..),
    IncomingPublicationResolution,
    OutgoingPublicationCandidate,
    OutgoingPublicationRecord,
    PreparedIncomingPublicationCandidate,
    PreparedOutgoingPublication,
    PublicationDependency (..),
    PublicationPreparationError (..),
    State,
    commitIncomingPublication,
    commitIncomingPublicationProgress,
    commitIncomingPublicationRejection,
    commitOutgoingPublication,
    finalizeIncomingPublication,
    heraldPublicationPositionWord64,
    incomingDependencyWaiters,
    incomingPeerPublication,
    incomingPublicationAssignments,
    incomingPublicationDependencies,
    incomingPublicationDestinationOutcomes,
    incomingPublicationDigest,
    incomingPublicationDisposition,
    incomingPublicationMembershipGenerationId,
    incomingPublicationResolution,
    incomingResolutionDisposition,
    initialState,
    lookupIncomingPublication,
    lookupOutgoingPublication,
    outgoingCandidateHeraldPosition,
    outgoingCandidatePublicationId,
    outgoingPublicationAcceptancePosition,
    outgoingPublicationChecked,
    outgoingPublicationControlPrerequisite,
    outgoingPublicationHasFenceDependency,
    outgoingPublicationHeraldPosition,
    outgoingPublicationId,
    outgoingPublicationOccurrenceId,
    outgoingPublicationRemoteBatches,
    outgoingPublicationRoute,
    outgoingPublicationSourceProcess,
    outgoingPublicationSourceStrength,
    prepareIncomingPublicationCandidate,
    prepareIncomingPublicationProgress,
    prepareIncomingPublicationRejection,
    prepareOutgoingPublication,
    preparedIncomingPublicationClassification,
    preparedOutgoingPublicationRecord,
    proposeOutgoingPublication,
    publicationStateWitness,
    publicationWitnessHeraldEpoch,
    publicationWitnessIncoming,
    publicationWitnessNextHeraldPosition,
    publicationWitnessNextSequences,
    publicationWitnessOutgoing,
  )
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    arbitrary,
    counterexample,
    forAll,
    listOf,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "publication evidence"
    [ testGroup
        "peer publication batch"
        [ testCase "destination cuts normalize and conflicts reject" caseBatchDestinationNormalization,
          testCase "the digest transcript is golden and field-sensitive" caseBatchDigest,
          testProperty "destination presentation cannot affect batch identity" propBatchPresentationCanonical
        ],
      testGroup
        "outgoing owner"
        [ testCase "a fresh tenure proposes sequence zero and Herald position one" caseInitialCandidate,
          testCase "commit atomically retains the complete record and advances both supplies" caseAtomicCommit,
          testCase "tenure sequences are independent while Herald positions are shared" caseIndependentTenures,
          testCase "candidate, checked identity, and source-position contradictions reject" caseContradictions,
          testCase "an empty frozen cut is an accepted publication record" caseEmptyRoute,
          testProperty "generated allocations are gap-free per tenure and Herald" propAllocationTrace
        ],
      testGroup
        "incoming owner"
        [ testCase "equal assignments and later sequences share one semantic record" caseIncomingDeduplication,
          testCase "dependency and destination progress is monotone" caseIncomingProgress,
          testCase "protocol rejection detaches a held record without rewriting its evidence" caseIncomingRejection,
          testCase "direction, digest, batch, and assignment conflicts reject" caseIncomingConflicts,
          testProperty "generated equal assignments share one semantic owner" propIncomingAssignmentTrace
        ]
    ]

caseBatchDestinationNormalization :: Assertion
caseBatchDestinationNormalization = do
  fixture <- checkedFixture
  publication <- fixturePublication fixture
  firstDestination <- fixturePeerDestination 232 233 Normal
  secondDestination <- fixturePeerDestination 234 235 Weak
  batch <-
    checkedBatch
      publication
      fixture.process
      fixture.occurrence
      fixture.controlPrerequisite
      (secondDestination :| [firstDestination, firstDestination])
  canonical <-
    checkedBatch
      publication
      fixture.process
      fixture.occurrence
      fixture.controlPrerequisite
      (firstDestination :| [secondDestination])
  assertEqual "presentation and exact duplicates normalize" canonical batch
  assertEqual
    "the destination cut is ascending by Delta"
    [firstDestination, secondDestination]
    (nonEmptyToList (publicationBatchDestinations batch))
  conflictingIncarnation <-
    checked "conflicting incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 236))
  let conflict =
        publicationDestination
          (publicationDestinationDelta firstDestination)
          conflictingIncarnation
          Normal
  assertEqual
    "one Delta cannot name unequal destination facts"
    ( Left
        ( ConflictingPublicationDestination
            (publicationDestinationDelta firstDestination)
            firstDestination
            conflict
        )
    )
    ( mkBatch
        publication
        fixture.process
        fixture.occurrence
        fixture.controlPrerequisite
        (firstDestination :| [conflict])
    )

caseBatchDigest :: Assertion
caseBatchDigest = do
  fixture <- checkedFixture
  publication <- fixturePublication fixture
  destination <- fixturePeerDestination 232 233 Normal
  batch <-
    checkedBatch
      publication
      fixture.process
      fixture.occurrence
      fixture.controlPrerequisite
      (destination :| [])
  assertEqual "the fixed source strength is normal" Normal (publicationBatchSourceStrength batch)
  assertEqual "source topology is retained" fixtureTopologyCut (publicationBatchSourceTopologyPrerequisite batch)
  assertEqual "the Step-7 fence arm is explicitly absent" False (publicationBatchHasFenceDependency batch)
  assertEqual "publication identity is retained" (checkedPublicationId publication) (publicationBatchId batch)
  assertEqual "source process is retained" fixture.process (publicationBatchSourceProcess batch)
  assertEqual "sort identity is retained" (checkedPublicationSort publication) (publicationBatchSortId batch)
  assertEqual "occurrence is retained" fixture.occurrence (publicationBatchOccurrenceId batch)
  assertEqual "canonical value bytes are retained" (checkedPublicationCanonicalValue publication) (publicationBatchCanonicalValue batch)
  assertEqual "control prerequisite is retained" fixture.controlPrerequisite (publicationBatchControlPrerequisite batch)
  assertEqual
    "the normalized cereal/SHA-256 transcript is golden"
    [ 32,
      39,
      23,
      46,
      73,
      1,
      195,
      106,
      254,
      88,
      209,
      77,
      150,
      122,
      238,
      204,
      60,
      179,
      0,
      245,
      132,
      252,
      63,
      215,
      79,
      112,
      90,
      153,
      233,
      102,
      60,
      235
    ]
    (ByteString.unpack (peerItemDigestBytes (batchDigest batch)))

  otherNabla <- checked "other nabla" (mkNablaId (fixtureIdentifierBytes 237))
  otherProcess <- checked "other process" (mkProcessEpochId (fixtureIdentifierBytes 238))
  otherSort <- checked "other sort" (mkSortId (fixtureIdentifierBytes 239))
  otherOccurrence <-
    checked
      "other occurrence"
      (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 240))
  otherDelta <- checked "other delta" (mkDeltaId (fixtureIdentifierBytes 241))
  otherIncarnation <-
    checked "other incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 242))
  let identifier = checkedPublicationId publication
      changedIdentifiers =
        [ publicationId otherNabla authorityA fixture.herald (nablaSequence 0),
          publicationId fixture.nabla authorityB fixture.herald (nablaSequence 0),
          publicationId fixture.nabla authorityA fixture.peer (nablaSequence 0),
          publicationId fixture.nabla authorityA fixture.herald (nablaSequence 1)
        ]
      variantFacts =
        [ ("source process", identifier, otherProcess, checkedPublicationSort publication, fixture.occurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, destination),
          ("sort", identifier, fixture.process, otherSort, fixture.occurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, destination),
          ("occurrence", identifier, fixture.process, checkedPublicationSort publication, otherOccurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, destination),
          ("canonical value", identifier, fixture.process, checkedPublicationSort publication, fixture.occurrence, canonicalValueBytes (bytesValue (ByteString.singleton 19)), fixture.controlPrerequisite, destination),
          ("control prerequisite", identifier, fixture.process, checkedPublicationSort publication, fixture.occurrence, checkedPublicationCanonicalValue publication, controlIndex 12, destination),
          ("destination delta", identifier, fixture.process, checkedPublicationSort publication, fixture.occurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, publicationDestination otherDelta (publicationDestinationStoreIncarnation destination) Normal),
          ("destination incarnation", identifier, fixture.process, checkedPublicationSort publication, fixture.occurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, publicationDestination (publicationDestinationDelta destination) otherIncarnation Normal),
          ("destination strength", identifier, fixture.process, checkedPublicationSort publication, fixture.occurrence, checkedPublicationCanonicalValue publication, fixture.controlPrerequisite, publicationDestination (publicationDestinationDelta destination) (publicationDestinationStoreIncarnation destination) Weak)
        ]
  identifierVariants <-
    mapM
      ( \changedIdentifier ->
          checkedRawBatch
            changedIdentifier
            fixture.process
            (checkedPublicationSort publication)
            fixture.occurrence
            (checkedPublicationCanonicalValue publication)
            fixture.controlPrerequisite
            (destination :| [])
      )
      changedIdentifiers
  weakSourceBatch <-
    let weakDestination =
          publicationDestination
            (publicationDestinationDelta destination)
            (publicationDestinationStoreIncarnation destination)
            Weak
     in checked
          "weak-source publication batch"
          ( mkPublicationBatch
              identifier
              fixture.process
              (checkedPublicationSort publication)
              fixture.occurrence
              (checkedPublicationCanonicalValue publication)
              Weak
              fixtureTopologyCut
              fixture.controlPrerequisite
              (weakDestination :| [])
          )
  factVariants <-
    mapM
      ( \(label, changedIdentifier, process, sortId, occurrence, value, prerequisite, changedDestination) -> do
          variant <-
            checkedRawBatch
              changedIdentifier
              process
              sortId
              occurrence
              value
              prerequisite
              (changedDestination :| [])
          pure (label, variant)
      )
      variantFacts
  let baselineDigest = batchDigest batch
  mapM_
    ( \(label, variant) ->
        assertBoolNotEqual
          (label <> " participates in the digest transcript")
          baselineDigest
          (batchDigest variant)
    )
    ( ("source strength", weakSourceBatch)
        : zip (fmap (("publication identity component " <>) . show) [1 :: Int ..]) identifierVariants
          <> factVariants
    )

  firstDirection <- checked "first direction" (mkStreamDirection fixture.herald fixture.peer)
  otherDestinationHerald <- checked "other destination Herald" (mkHeraldEpoch (fixtureIdentifierBytes 243))
  secondDirection <- checked "second direction" (mkStreamDirection fixture.herald otherDestinationHerald)
  let digest = batchDigest batch
      firstAssignment = sequencedItem firstDirection firstStreamSequence digest batch
      secondAssignment =
        sequencedItem
          secondDirection
          (nextStreamSequence firstStreamSequence)
          digest
          batch
  assertEqual "stream direction does not redefine semantic digest" digest (sequencedItemDigest secondAssignment)
  assertEqual "stream sequence does not redefine semantic digest" (sequencedItemDigest firstAssignment) (sequencedItemDigest secondAssignment)

propBatchPresentationCanonical :: Bool -> Property
propBatchPresentationCanonical reversePresentation =
  case publicationFixture of
    Left problem -> counterexample problem False
    Right fixture ->
      case batchPair fixture reversePresentation of
        Left problem -> counterexample problem False
        Right (baseline, presented) ->
          counterexample (show presented) (baseline === presented)
  where
    batchPair fixture reversed = do
      publication <- checkedPublicationEither fixture (proposeOutgoingPublication fixture.nabla authorityA (initialState fixture.herald))
      firstDestination <- fixturePeerDestinationEither 232 233 Normal
      secondDestination <- fixturePeerDestinationEither 234 235 Weak
      let supplied =
            if reversed
              then secondDestination :| [firstDestination, secondDestination]
              else firstDestination :| [secondDestination, firstDestination]
      baseline <-
        Bifunctor.first
          show
          ( mkBatch
              publication
              fixture.process
              fixture.occurrence
              fixture.controlPrerequisite
              (firstDestination :| [secondDestination])
          )
      presented <-
        Bifunctor.first
          show
          ( mkBatch
              publication
              fixture.process
              fixture.occurrence
              fixture.controlPrerequisite
              supplied
          )
      Right (baseline, presented)

caseIncomingDeduplication :: Assertion
caseIncomingDeduplication = do
  fixture <- checkedFixture
  batch <- fixtureBatch fixture
  direction <- checked "incoming direction" (mkStreamDirection fixture.herald fixture.peer)
  resolution <- dependencyResolution batch
  successorMembership <-
    checked
      "successor membership for cross-generation retry"
      ( retireHeraldMembershipGeneration
          (controlIndex 2)
          (fixtureRetirementResolution (controlIndex 2))
          (NonEmpty.last (heraldMembershipGenerationActiveHeraldEpochs fixtureHeraldMembershipGeneration))
          fixtureHeraldMembershipGeneration
      )
  let identifier = publicationBatchId batch
      digest = batchDigest batch
      predecessor = initialState fixture.peer
      firstMembershipId = heraldMembershipGenerationId fixtureHeraldMembershipGeneration
      retryMembershipId = heraldMembershipGenerationId successorMembership
  firstCandidate <-
    checked
      "first incoming candidate"
      (prepareIncomingBatchCandidate direction firstStreamSequence digest batch predecessor)
  assertEqual "the first semantic identity is new" IncomingPublicationFirstSeen (preparedIncomingPublicationClassification firstCandidate)
  assertIncomingProblem
    "first observation needs lifecycle evidence"
    (IncomingPublicationResolutionRequired identifier)
    (finalizeIncomingPublication Nothing firstCandidate)
  firstPrepared <- checked "first incoming publication" (finalizeIncomingPublication (Just resolution) firstCandidate)
  let (afterFirst, firstRecord) = commitIncomingPublication firstPrepared
      firstWitness = publicationStateWitness afterFirst
  assertEqual "one semantic record is retained" 1 (length (publicationWitnessIncoming firstWitness))
  assertEqual
    "first admission retains its exact membership generation"
    firstMembershipId
    (incomingPublicationMembershipGenerationId firstRecord)
  assertEqual "one exact assignment is retained" 1 (Set.size (incomingPublicationAssignments firstRecord))
  assertEqual "the missing definition owns the publication" (Set.singleton (batchDependency batch)) (incomingPublicationDependencies firstRecord)
  assertEqual
    "the reverse dependency index names the semantic publication"
    (Set.singleton identifier)
    (incomingDependencyWaiters (batchDependency batch) afterFirst)
  assertEqual "dependency retention permits received but not completed progress" DependencyHeld (incomingPublicationDisposition firstRecord)

  exactCandidate <-
    checked
      "exact duplicate candidate"
      (prepareIncomingBatchCandidateAt retryMembershipId direction firstStreamSequence digest batch afterFirst)
  assertEqual "the same assignment is exact duplicate" IncomingPublicationAssignmentDuplicate (preparedIncomingPublicationClassification exactCandidate)
  assertIncomingProblem
    "a duplicate cannot replace inherited lifecycle evidence"
    (IncomingPublicationResolutionUnexpected identifier)
    (finalizeIncomingPublication (Just resolution) exactCandidate)
  exactPrepared <- checked "exact duplicate" (finalizeIncomingPublication Nothing exactCandidate)
  let (afterExact, exactRecord) = commitIncomingPublication exactPrepared
  assertEqual "exact redelivery changes no retained owner evidence" firstWitness (publicationStateWitness afterExact)
  assertEqual "exact redelivery returns the same record" firstRecord exactRecord
  assertEqual
    "exact redelivery under a successor generation retains first admission"
    firstMembershipId
    (incomingPublicationMembershipGenerationId exactRecord)

  let secondSequence = nextStreamSequence firstStreamSequence
  laterCandidate <-
    checked
      "later duplicate candidate"
      (prepareIncomingBatchCandidateAt retryMembershipId direction secondSequence digest batch afterExact)
  assertEqual "an equal later sequence is one semantic duplicate" IncomingPublicationSemanticDuplicate (preparedIncomingPublicationClassification laterCandidate)
  laterPrepared <- checked "later semantic duplicate" (finalizeIncomingPublication Nothing laterCandidate)
  let (_, laterRecord) = commitIncomingPublication laterPrepared
  assertEqual "the later sequence adds only one assignment" 2 (Set.size (incomingPublicationAssignments laterRecord))
  assertEqual "the later assignment inherits dependencies" (incomingPublicationDependencies firstRecord) (incomingPublicationDependencies laterRecord)
  assertEqual "the later assignment inherits destination outcomes" (incomingPublicationDestinationOutcomes firstRecord) (incomingPublicationDestinationOutcomes laterRecord)
  assertEqual "the later assignment inherits disposition" (incomingPublicationDisposition firstRecord) (incomingPublicationDisposition laterRecord)
  assertEqual
    "a successor-generation semantic duplicate retains first admission"
    firstMembershipId
    (incomingPublicationMembershipGenerationId laterRecord)

caseIncomingProgress :: Assertion
caseIncomingProgress = do
  fixture <- checkedFixture
  batch <- fixtureBatch fixture
  direction <- checked "incoming direction" (mkStreamDirection fixture.herald fixture.peer)
  let controlDependency = ControlIndexDependency (controlIndex 12)
      dependencies = Set.fromList [batchDependency batch, controlDependency]
  heldResolution <-
    checked
      "dual-dependency resolution"
      ( incomingPublicationResolution
          dependencies
          ( Map.fromList
              [ (destination, DestinationPending)
              | destination <- nonEmptyToList (publicationBatchDestinations batch)
              ]
          )
      )
  let identifier = publicationBatchId batch
      digest = batchDigest batch
  candidate <-
    checked
      "incoming candidate"
      (prepareIncomingBatchCandidate direction firstStreamSequence digest batch (initialState fixture.peer))
  prepared <- checked "held incoming publication" (finalizeIncomingPublication (Just heldResolution) candidate)
  let (heldState, _) = commitIncomingPublication prepared
      pending = Map.fromList [(destination, DestinationPending) | destination <- nonEmptyToList (publicationBatchDestinations batch)]
      applied = Map.fromList [(destination, DestinationApplied) | destination <- nonEmptyToList (publicationBatchDestinations batch)]
      admittedMembershipId = heraldMembershipGenerationId fixtureHeraldMembershipGeneration
  controlHeld <-
    checked
      "control dependency remains"
      (incomingPublicationResolution (Set.singleton controlDependency) pending)
  assertEqual
    "an explicit control dependency remains held"
    DependencyHeld
    (incomingResolutionDisposition controlHeld)
  controlPrepared <- checked "control-held progress" (prepareIncomingPublicationProgress identifier controlHeld heldState)
  let (controlState, controlRecord) = commitIncomingPublicationProgress controlPrepared
  assertEqual
    "definition release retains only the independent control dependency"
    (Set.singleton controlDependency)
    (incomingPublicationDependencies controlRecord)
  assertEqual
    "definition release atomically removes the reverse waiter"
    Set.empty
    (incomingDependencyWaiters (batchDependency batch) controlState)
  assertEqual
    "the control reverse waiter remains exact"
    (Set.singleton identifier)
    (incomingDependencyWaiters controlDependency controlState)
  assertEqual "destination work remains pending" DependencyHeld (incomingPublicationDisposition controlRecord)
  assertEqual
    "partial progress preserves first-admission membership"
    admittedMembershipId
    (incomingPublicationMembershipGenerationId controlRecord)

  assertEqual
    "pending outcomes without an explicit dependency reject"
    (Left IncomingDependencyOutcomeContradiction)
    (incomingPublicationResolution Set.empty pending)

  appliedResolution <- checked "applied resolution" (incomingPublicationResolution Set.empty applied)
  appliedPrepared <- checked "applied progress" (prepareIncomingPublicationProgress identifier appliedResolution controlState)
  let (appliedState, appliedRecord) = commitIncomingPublicationProgress appliedPrepared
  assertEqual "an applied destination completes the semantic publication" Applied (incomingPublicationDisposition appliedRecord)
  assertEqual
    "terminal progress preserves first-admission membership"
    admittedMembershipId
    (incomingPublicationMembershipGenerationId appliedRecord)

  ignored <-
    checked
      "terminally ignored resolution"
      ( incomingPublicationResolution
          Set.empty
          (Map.fromList [(destination, DestinationTerminallyIgnored) | destination <- nonEmptyToList (publicationBatchDestinations batch)])
      )
  assertEqual "all stale destinations are terminally ignored" TerminallyIgnored (incomingResolutionDisposition ignored)
  assertIncomingProblem
    "an applied destination cannot change to terminal ignore"
    ( IncomingDestinationOutcomeRegression
        identifier
        (headDestination batch)
        DestinationApplied
        DestinationTerminallyIgnored
    )
    (prepareIncomingPublicationProgress identifier ignored appliedState)

caseIncomingRejection :: Assertion
caseIncomingRejection = do
  fixture <- checkedFixture
  batch <- fixtureBatch fixture
  direction <-
    checked
      "rejected incoming direction"
      (mkStreamDirection fixture.herald fixture.peer)
  let identifier = publicationBatchId batch
      dependencies =
        Set.fromList
          [ batchDependency batch,
            ControlIndexDependency (controlIndex 12)
          ]
      pendingOutcomes =
        Map.fromList
          [ (destination, DestinationPending)
          | destination <- nonEmptyToList (publicationBatchDestinations batch)
          ]
      predecessor = initialState fixture.peer
      digest = batchDigest batch
      unknownIdentifier =
        publicationId
          fixture.nabla
          authorityA
          fixture.herald
          (nablaSequence 99)
  heldResolution <-
    checked
      "rejected dependency-held resolution"
      (incomingPublicationResolution dependencies pendingOutcomes)
  candidate <-
    checked
      "rejected incoming candidate"
      ( prepareIncomingBatchCandidate
          direction
          firstStreamSequence
          digest
          batch
          predecessor
      )
  prepared <-
    checked
      "dependency-held incoming publication"
      (finalizeIncomingPublication (Just heldResolution) candidate)
  let (heldState, heldRecord) = commitIncomingPublication prepared
  rejection <-
    checked
      "reject dependency-held incoming publication"
      (prepareIncomingPublicationRejection identifier heldState)
  let (rejectedState, rejectedRecord) =
        commitIncomingPublicationRejection rejection
  assertEqual
    "protocol rejection clears every semantic dependency"
    Set.empty
    (incomingPublicationDependencies rejectedRecord)
  mapM_
    ( \dependency ->
        assertEqual
          "protocol rejection clears each reverse waiter"
          Set.empty
          (incomingDependencyWaiters dependency rejectedState)
    )
    (Set.toAscList dependencies)
  assertEqual
    "protocol rejection preserves the exact peer bytes"
    (incomingPeerPublication heldRecord)
    (incomingPeerPublication rejectedRecord)
  assertEqual
    "protocol rejection preserves the authenticated digest"
    (incomingPublicationDigest heldRecord)
    (incomingPublicationDigest rejectedRecord)
  assertEqual
    "protocol rejection preserves every stream assignment"
    (incomingPublicationAssignments heldRecord)
    (incomingPublicationAssignments rejectedRecord)
  assertEqual
    "protocol rejection preserves pending destination outcomes"
    pendingOutcomes
    (incomingPublicationDestinationOutcomes rejectedRecord)
  assertEqual
    "protocol rejection records the terminal disposition"
    ProtocolRejected
    (incomingPublicationDisposition rejectedRecord)
  assertEqual
    "the rejected record remains the publication owner's exact record"
    (Just rejectedRecord)
    (lookupIncomingPublication identifier rejectedState)
  exactRejectedCandidate <-
    checked
      "exact rejected assignment retry"
      ( prepareIncomingBatchCandidate
          direction
          firstStreamSequence
          digest
          batch
          rejectedState
      )
  assertEqual
    "an exact rejected assignment remains a transport replay"
    IncomingPublicationAssignmentDuplicate
    (preparedIncomingPublicationClassification exactRejectedCandidate)
  exactRejectedPrepared <-
    checked
      "retain exact rejected assignment retry"
      (finalizeIncomingPublication Nothing exactRejectedCandidate)
  let (afterExactRejectedRetry, exactRejectedRecord) =
        commitIncomingPublication exactRejectedPrepared
  assertEqual
    "an exact rejected assignment changes no owner evidence"
    (publicationStateWitness rejectedState)
    (publicationStateWitness afterExactRejectedRetry)
  assertEqual
    "an exact rejected assignment returns the terminal record"
    rejectedRecord
    exactRejectedRecord
  assertIncomingProblem
    "a rejected semantic publication cannot acquire a fresh stream assignment"
    (IncomingPublicationRejectedSemanticReassignment identifier)
    ( prepareIncomingBatchCandidate
        direction
        (nextStreamSequence firstStreamSequence)
        digest
        batch
        rejectedState
    )
  appliedResolution <-
    checked
      "non-rejectable applied resolution"
      ( incomingPublicationResolution
          Set.empty
          (Map.map (const DestinationApplied) pendingOutcomes)
      )
  appliedProgress <-
    checked
      "advance the held record to applied"
      (prepareIncomingPublicationProgress identifier appliedResolution heldState)
  let (appliedState, _) = commitIncomingPublicationProgress appliedProgress
  assertIncomingProblem
    "an applied publication cannot be protocol-rejected"
    (IncomingPublicationRejectionContradiction identifier)
    (prepareIncomingPublicationRejection identifier appliedState)
  assertIncomingProblem
    "an unknown publication cannot be rejected"
    (IncomingPublicationUnknown unknownIdentifier)
    (prepareIncomingPublicationRejection unknownIdentifier rejectedState)
  assertIncomingProblem
    "a protocol-rejected publication cannot be rejected twice"
    (IncomingPublicationRejectionContradiction identifier)
    (prepareIncomingPublicationRejection identifier rejectedState)

caseIncomingConflicts :: Assertion
caseIncomingConflicts = do
  fixture <- checkedFixture
  batch <- fixtureBatch fixture
  direction <- checked "incoming direction" (mkStreamDirection fixture.herald fixture.peer)
  otherSource <- checked "other source" (mkHeraldEpoch (fixtureIdentifierBytes 243))
  otherDestination <- checked "other destination" (mkHeraldEpoch (fixtureIdentifierBytes 244))
  wrongSourceDirection <- checked "wrong source direction" (mkStreamDirection otherSource fixture.peer)
  wrongDestinationDirection <- checked "wrong destination direction" (mkStreamDirection fixture.herald otherDestination)
  let digest = batchDigest batch
      predecessor = initialState fixture.peer
      identifier = publicationBatchId batch
  assertIncomingProblem
    "the destination owner is exact"
    (IncomingPublicationWrongDestination fixture.peer otherDestination)
    (prepareIncomingBatchCandidate wrongDestinationDirection firstStreamSequence digest batch predecessor)
  assertIncomingProblem
    "publication provenance fixes the source direction"
    (IncomingPublicationWrongSource fixture.herald otherSource)
    (prepareIncomingBatchCandidate wrongSourceDirection firstStreamSequence digest batch predecessor)
  wrongDigest <- checked "wrong digest" (mkPeerItemDigest (ByteString.replicate 32 99))
  assertIncomingProblem
    "the supplied digest is recomputed from the exact batch"
    (IncomingPublicationDigestMismatch digest wrongDigest)
    (prepareIncomingBatchCandidate direction firstStreamSequence wrongDigest batch predecessor)

  resolution <- dependencyResolution batch
  candidate <- checked "first candidate" (prepareIncomingBatchCandidate direction firstStreamSequence digest batch predecessor)
  prepared <- checked "first publication" (finalizeIncomingPublication (Just resolution) candidate)
  let (retained, _) = commitIncomingPublication prepared
  changedBatch <-
    checkedRawBatch
      identifier
      (publicationBatchSourceProcess batch)
      (publicationBatchSortId batch)
      (publicationBatchOccurrenceId batch)
      (publicationBatchCanonicalValue batch)
      (controlIndex 12)
      (publicationBatchDestinations batch)
  assertIncomingProblem
    "one publication identity cannot name another semantic digest"
    ( IncomingPublicationSemanticDigestConflict
        identifier
        digest
        (batchDigest changedBatch)
    )
    ( prepareIncomingBatchCandidate
        direction
        (nextStreamSequence firstStreamSequence)
        (batchDigest changedBatch)
        changedBatch
        retained
    )

  let otherIdentifier =
        publicationId fixture.nabla authorityA fixture.herald (nablaSequence 91)
  otherBatch <-
    checkedRawBatch
      otherIdentifier
      (publicationBatchSourceProcess batch)
      (publicationBatchSortId batch)
      (publicationBatchOccurrenceId batch)
      (publicationBatchCanonicalValue batch)
      (publicationBatchControlPrerequisite batch)
      (publicationBatchDestinations batch)
  assertIncomingProblem
    "one stream assignment cannot belong to two semantic publications"
    ( IncomingAssignmentAlreadyOwned
        direction
        firstStreamSequence
        identifier
        otherIdentifier
    )
    ( prepareIncomingBatchCandidate
        direction
        firstStreamSequence
        (batchDigest otherBatch)
        otherBatch
        retained
    )

propIncomingAssignmentTrace :: [Word8] -> Property
propIncomingAssignmentTrace suppliedSequences =
  case publicationFixture >>= incomingTraceFixture of
    Left problem -> counterexample problem False
    Right (batch, direction, resolution, fixture) ->
      let sequenceWords = 1 : fmap ((+ 1) . fromIntegral) suppliedSequences
       in case traverse checkedSequence sequenceWords of
            Left problem -> counterexample problem False
            Right sequences ->
              case retainAssignments direction batch resolution sequences (initialState fixture.peer) of
                Left problem -> counterexample problem False
                Right successor ->
                  case lookupIncomingPublication (publicationBatchId batch) successor of
                    Nothing -> counterexample "incoming record disappeared" False
                    Just record ->
                      let expectedAssignmentCount = Set.size (Set.fromList sequences)
                       in counterexample (show (publicationStateWitness successor))
                            $ ( length (publicationWitnessIncoming (publicationStateWitness successor)),
                                Set.size (incomingPublicationAssignments record),
                                incomingPublicationDependencies record,
                                incomingDependencyWaiters (batchDependency batch) successor
                              )
                              === ( 1,
                                    expectedAssignmentCount,
                                    Set.singleton (batchDependency batch),
                                    Set.singleton (publicationBatchId batch)
                                  )
  where
    checkedSequence = Bifunctor.first show . mkStreamSequence

incomingTraceFixture ::
  PublicationFixture ->
  Either
    String
    ( PublicationBatch,
      StreamDirection,
      IncomingPublicationResolution,
      PublicationFixture
    )
incomingTraceFixture fixture = do
  publication <-
    checkedPublicationEither
      fixture
      ( proposeOutgoingPublication
          fixture.nabla
          authorityA
          (initialState fixture.herald)
      )
  destination <- fixturePeerDestinationEither 234 235 Weak
  batch <-
    Bifunctor.first
      show
      ( mkBatch
          publication
          fixture.process
          fixture.occurrence
          fixture.controlPrerequisite
          (destination :| [])
      )
  direction <-
    Bifunctor.first show (mkStreamDirection fixture.herald fixture.peer)
  resolution <-
    Bifunctor.first
      show
      ( incomingPublicationResolution
          (Set.singleton (batchDependency batch))
          (Map.singleton destination DestinationPending)
      )
  Right (batch, direction, resolution, fixture)

retainAssignments ::
  StreamDirection ->
  PublicationBatch ->
  IncomingPublicationResolution ->
  [StreamSequence] ->
  State ->
  Either String State
retainAssignments direction batch resolution = go True
  where
    digest = batchDigest batch
    go _ [] state = Right state
    go isFirst (sequenceNumber : remaining) state = do
      candidate <-
        Bifunctor.first
          show
          ( prepareIncomingBatchCandidate
              direction
              sequenceNumber
              digest
              batch
              state
          )
      prepared <-
        Bifunctor.first
          show
          ( finalizeIncomingPublication
              (if isFirst then Just resolution else Nothing)
              candidate
          )
      let successor = fst (commitIncomingPublication prepared)
      go False remaining successor

caseInitialCandidate :: Assertion
caseInitialCandidate = do
  fixture <- checkedFixture
  let state = initialState fixture.herald
      candidate = proposeOutgoingPublication fixture.nabla authorityA state
      identifier = outgoingCandidatePublicationId candidate
      witness = publicationStateWitness state
  assertEqual "the owner is tied to one Herald epoch" fixture.herald (publicationWitnessHeraldEpoch witness)
  assertEqual "no tenure counter exists before allocation" [] (publicationWitnessNextSequences witness)
  assertEqual
    "the first Herald-wide position is one"
    1
    (heraldPublicationPositionWord64 (outgoingCandidateHeraldPosition candidate))
  assertEqual "the first tenure sequence is zero" 0 (nablaSequenceWord64 (publicationNablaSequence identifier))
  assertEqual "the proposal retains its nabla" fixture.nabla (publicationNabla identifier)
  assertEqual "the proposal retains its authority" authorityA (publicationAuthorityEpoch identifier)
  assertEqual "the proposal uses the owner Herald" fixture.herald (publicationSourceHeraldEpoch identifier)
  assertEqual "proposing changes no retained record" [] (publicationWitnessOutgoing witness)

caseAtomicCommit :: Assertion
caseAtomicCommit = do
  fixture <- checkedFixture
  let predecessor = initialState fixture.herald
      candidate = proposeOutgoingPublication fixture.nabla authorityA predecessor
  publication <- checkedPublication fixture candidate
  prepared <-
    checked
      "outgoing publication preparation"
      (prepare fixture candidate publication fixture.route predecessor)
  let preparedRecord = preparedOutgoingPublicationRecord prepared
      (successor, committedRecord) = commitOutgoingPublication prepared
      witness = publicationStateWitness successor
      identifier = outgoingCandidatePublicationId candidate
  assertEqual "prepare and commit expose the same immutable record" preparedRecord committedRecord
  assertEqual "the checked publication is retained exactly" publication (outgoingPublicationChecked committedRecord)
  assertEqual "the source process is retained" fixture.process (outgoingPublicationSourceProcess committedRecord)
  assertEqual "the source position is retained" fixture.position (outgoingPublicationAcceptancePosition committedRecord)
  assertEqual "the carrier occurrence is retained" fixture.occurrence (outgoingPublicationOccurrenceId committedRecord)
  assertEqual "the frozen route is retained" fixture.route (outgoingPublicationRoute committedRecord)
  assertEqual "the source strength is the closed normal fact" Normal (outgoingPublicationSourceStrength committedRecord)
  assertEqual
    "the exact control prerequisite is retained"
    fixture.controlPrerequisite
    (outgoingPublicationControlPrerequisite committedRecord)
  assertEqual
    "Step 7 retains explicit fence absence"
    False
    (outgoingPublicationHasFenceDependency committedRecord)
  remoteBatch <-
    maybe
      (assertFailure "the frozen remote cut produced no peer batch")
      pure
      (Map.lookup fixture.peer (outgoingPublicationRemoteBatches committedRecord))
  assertEqual "one remote host owns one retained batch" 1 (Map.size (outgoingPublicationRemoteBatches committedRecord))
  assertEqual "the batch retains publication identity" identifier (publicationBatchId remoteBatch)
  assertEqual "the batch retains source process" fixture.process (publicationBatchSourceProcess remoteBatch)
  assertEqual "the batch retains carrier occurrence" fixture.occurrence (publicationBatchOccurrenceId remoteBatch)
  assertEqual "the batch retains checked canonical bytes" (checkedPublicationCanonicalValue publication) (publicationBatchCanonicalValue remoteBatch)
  assertEqual "the batch retains the prerequisite" fixture.controlPrerequisite (publicationBatchControlPrerequisite remoteBatch)
  assertEqual "the record is indexed by checked provenance" identifier (outgoingPublicationId committedRecord)
  assertEqual "lookup returns the immutable record" (Just committedRecord) (lookupOutgoingPublication identifier successor)
  assertEqual
    "the tenure advances exactly once"
    [((fixture.nabla, authorityA), nablaSequence 1)]
    (publicationWitnessNextSequences witness)
  assertEqual
    "the Herald-wide supply advances exactly once"
    2
    (heraldPublicationPositionWord64 (publicationWitnessNextHeraldPosition witness))
  assertEqual "the predecessor remains untouched" [] (publicationWitnessOutgoing (publicationStateWitness predecessor))

caseIndependentTenures :: Assertion
caseIndependentTenures = do
  fixture <- checkedFixture
  (afterFirst, first) <- allocate fixture authorityA (initialState fixture.herald)
  (afterSecond, second) <- allocate fixture authorityA afterFirst
  (successor, third) <- allocate fixture authorityB afterSecond
  assertEqual
    "one tenure is contiguous and another begins independently at zero"
    [0, 1, 0]
    ( fmap
        (nablaSequenceWord64 . publicationNablaSequence . outgoingPublicationId)
        [first, second, third]
    )
  assertEqual
    "one Herald order spans both tenures"
    [1, 2, 3]
    (fmap (heraldPublicationPositionWord64 . outgoingPublicationHeraldPosition) [first, second, third])
  assertEqual
    "both next-tenure counters are retained in canonical key order"
    ( sort
        [ ((fixture.nabla, authorityA), nablaSequence 2),
          ((fixture.nabla, authorityB), nablaSequence 1)
        ]
    )
    (publicationWitnessNextSequences (publicationStateWitness successor))
  assertEqual "all three immutable records remain" 3 (length (publicationWitnessOutgoing (publicationStateWitness successor)))

caseContradictions :: Assertion
caseContradictions = do
  fixture <- checkedFixture
  otherProcess <- checked "second process" (mkProcessEpochId (fixtureIdentifierBytes 241))
  let predecessor = initialState fixture.herald
      candidate = proposeOutgoingPublication fixture.nabla authorityA predecessor
      otherCandidate = proposeOutgoingPublication fixture.nabla authorityB predecessor
  publication <- checkedPublication fixture candidate
  otherPublication <- checkedPublication fixture otherCandidate
  assertPreparationError
    "a checked publication cannot replace the proposed provenance"
    ( PublicationCheckedIdentityMismatch
        (outgoingCandidatePublicationId candidate)
        (outgoingCandidatePublicationId otherCandidate)
    )
    (prepare fixture candidate otherPublication fixture.route predecessor)
  let wrongPosition = processAcceptancePosition otherProcess 1
  assertPreparationError
    "the source and process-qualified acceptance position must agree"
    (PublicationSourcePositionMismatch fixture.process otherProcess)
    ( prepareWithPosition
        fixture
        candidate
        publication
        wrongPosition
        fixture.route
        predecessor
    )
  (successor, _) <- allocate fixture authorityA predecessor
  let nextCandidate = proposeOutgoingPublication fixture.nabla authorityA successor
  assertPreparationError
    "a candidate cannot be finalized after its predecessor advanced"
    ( PublicationCandidateStale
        (outgoingCandidatePublicationId candidate)
        (outgoingCandidateHeraldPosition candidate)
        (outgoingCandidatePublicationId nextCandidate)
        (outgoingCandidateHeraldPosition nextCandidate)
    )
    (prepare fixture candidate publication fixture.route successor)

caseEmptyRoute :: Assertion
caseEmptyRoute = do
  fixture <- checkedFixture
  empty <- checked "empty frozen route" (freezeRoute [])
  let predecessor = initialState fixture.herald
      candidate = proposeOutgoingPublication fixture.nabla authorityA predecessor
  publication <- checkedPublication fixture candidate
  prepared <- checked "empty-cut publication" (prepare fixture candidate publication empty predecessor)
  let (successor, record) = commitOutgoingPublication prepared
  assertEqual "the exact empty cut is retained" empty (outgoingPublicationRoute record)
  assertEqual "an empty cut creates no empty peer batch" Map.empty (outgoingPublicationRemoteBatches record)
  assertEqual "the accepted record is still retained" 1 (length (publicationWitnessOutgoing (publicationStateWitness successor)))

propAllocationTrace :: Property
propAllocationTrace =
  forAll (listOf arbitrary) $ \tenures ->
    case publicationFixture of
      Left problem -> counterexample problem False
      Right fixture ->
        case allocateTrace fixture tenures of
          Left problem -> counterexample problem False
          Right (state, observedSequences, observedPositions) ->
            let expectedSequences = expectedTenureSequences tenures
                expectedPositions = [1 .. fromIntegral (length tenures)]
                expectedNext = expectedNextSequences fixture tenures
                witness = publicationStateWitness state
                actualNextPosition =
                  heraldPublicationPositionWord64
                    (publicationWitnessNextHeraldPosition witness)
             in counterexample (show witness)
                  $ ( observedSequences,
                      observedPositions,
                      publicationWitnessNextSequences witness,
                      actualNextPosition,
                      length (publicationWitnessOutgoing witness)
                    )
                    === ( expectedSequences,
                          expectedPositions,
                          expectedNext,
                          fromIntegral (length tenures) + 1,
                          length tenures
                        )

data PublicationFixture = PublicationFixture
  { herald :: HeraldEpoch,
    peer :: HeraldEpoch,
    nabla :: NablaId,
    process :: ProcessEpochId,
    position :: ProcessAcceptancePosition,
    occurrence :: SortDefinitionOccurrenceId,
    controlPrerequisite :: ControlIndex,
    route :: FrozenRoute,
    carrier :: PrimordialDefinitionReplica
  }

publicationFixture :: Either String PublicationFixture
publicationFixture = do
  carrier <-
    maybe
      (Left "fixture genesis has no sort-definition carrier")
      Right
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  nabla <- identity "NablaId" mkNablaId 230
  process <- identity "ProcessEpochId" mkProcessEpochId 231
  delta <- identity "DeltaId" mkDeltaId 232
  incarnation <- identity "StoreIncarnationId" mkStoreIncarnationId 233
  remoteDelta <- identity "remote DeltaId" mkDeltaId 234
  remoteIncarnation <-
    identity "remote StoreIncarnationId" mkStoreIncarnationId 235
  peer <- identity "peer HeraldEpoch" mkHeraldEpoch 236
  route <-
    Bifunctor.first
      show
      ( freezeRoute
          [ routeDestination
              delta
              incarnation
              (checkedLocalHeraldEpoch fixtureCheckedGenesis)
              Normal,
            routeDestination
              remoteDelta
              remoteIncarnation
              peer
              Weak
          ]
      )
  Right
    PublicationFixture
      { herald = checkedLocalHeraldEpoch fixtureCheckedGenesis,
        peer,
        nabla = nabla,
        process = process,
        position = processAcceptancePosition process 1,
        occurrence = primordialReplicaOccurrenceId carrier,
        controlPrerequisite = controlIndex 11,
        route = route,
        carrier = carrier
      }

checkedFixture :: IO PublicationFixture
checkedFixture = checked "publication fixture" publicationFixture

authorityA :: AuthorityEpoch
authorityA = genesisAuthorityEpoch

authorityB :: AuthorityEpoch
authorityB =
  structuralAuthorityEpoch
    fixtureAuthorityOccurrence
    fixtureTopologyCut

fixtureAuthorityOccurrence :: StructuralOccurrenceId
fixtureAuthorityOccurrence =
  structuralOccurrenceId fixtureAuthoritySource firstStructuralSequence

fixtureAuthoritySource :: HeraldEpoch
fixtureAuthoritySource =
  either
    (error . ("invalid fixture authority source: " <>) . show)
    id
    (mkHeraldEpoch (fixtureIdentifierBytes 0xf9))

fixtureTopologyCut :: TopologyCutId
fixtureTopologyCut =
  either
    (error . ("invalid fixture topology cut: " <>) . show)
    id
    (mkTopologyCutId (fixtureIdentifierBytes 0xfa))

checkedPublication ::
  PublicationFixture ->
  OutgoingPublicationCandidate ->
  IO CheckedPublication
checkedPublication fixture candidate =
  checked
    "checked outgoing publication"
    (checkedPublicationEither fixture candidate)

checkedPublicationEither ::
  PublicationFixture ->
  OutgoingPublicationCandidate ->
  Either String CheckedPublication
checkedPublicationEither fixture candidate =
  Bifunctor.first
    show
    ( mkCheckedPublication
        (primordialReplicaDescriptor fixture.carrier)
        (outgoingCandidatePublicationId candidate)
        (sortDefinitionValue (primordialReplicaDescriptor fixture.carrier))
    )

prepare ::
  PublicationFixture ->
  OutgoingPublicationCandidate ->
  CheckedPublication ->
  FrozenRoute ->
  State ->
  Either PublicationPreparationError PreparedOutgoingPublication
prepare fixture candidate publication =
  prepareWithPosition fixture candidate publication fixture.position

prepareWithPosition ::
  PublicationFixture ->
  OutgoingPublicationCandidate ->
  CheckedPublication ->
  ProcessAcceptancePosition ->
  FrozenRoute ->
  State ->
  Either PublicationPreparationError PreparedOutgoingPublication
prepareWithPosition fixture candidate publication position =
  prepareOutgoingPublication
    candidate
    publication
    fixture.process
    position
    fixture.occurrence
    Normal
    fixtureTopologyCut
    fixture.controlPrerequisite

allocate ::
  PublicationFixture ->
  AuthorityEpoch ->
  State ->
  IO (State, OutgoingPublicationRecord)
allocate fixture authority state = do
  let candidate = proposeOutgoingPublication fixture.nabla authority state
  publication <- checkedPublication fixture candidate
  prepared <- checked "outgoing publication" (prepare fixture candidate publication fixture.route state)
  pure (commitOutgoingPublication prepared)

allocateEither ::
  PublicationFixture ->
  AuthorityEpoch ->
  State ->
  Either String (State, OutgoingPublicationRecord)
allocateEither fixture authority state = do
  let candidate = proposeOutgoingPublication fixture.nabla authority state
  publication <- checkedPublicationEither fixture candidate
  prepared <-
    Bifunctor.first
      show
      (prepare fixture candidate publication fixture.route state)
  Right (commitOutgoingPublication prepared)

allocateTrace ::
  PublicationFixture ->
  [Bool] ->
  Either String (State, [Word64], [Word64])
allocateTrace fixture = go (initialState fixture.herald) [] []
  where
    go state sequences positions [] =
      Right (state, reverse sequences, reverse positions)
    go state sequences positions (choice : remaining) = do
      let authority = if choice then authorityA else authorityB
          candidate = proposeOutgoingPublication fixture.nabla authority state
          identifier = outgoingCandidatePublicationId candidate
          sequenceNumber = nablaSequenceWord64 (publicationNablaSequence identifier)
          heraldPosition =
            heraldPublicationPositionWord64
              (outgoingCandidateHeraldPosition candidate)
      (successor, _) <- allocateEither fixture authority state
      go
        successor
        (sequenceNumber : sequences)
        (heraldPosition : positions)
        remaining

expectedTenureSequences :: [Bool] -> [Word64]
expectedTenureSequences = go Map.empty
  where
    go _ [] = []
    go counts (choice : remaining) =
      let current = Map.findWithDefault 0 choice counts
       in current : go (Map.insert choice (current + 1) counts) remaining

expectedNextSequences ::
  PublicationFixture ->
  [Bool] ->
  [((NablaId, AuthorityEpoch), NablaSequence)]
expectedNextSequences fixture tenures =
  sort
    [ ((fixture.nabla, if choice then authorityA else authorityB), nablaSequence count)
    | (choice, count) <- Map.toList counts
    ]
  where
    counts = Map.fromListWith (+) [(choice, 1 :: Word64) | choice <- tenures]

fixturePublication :: PublicationFixture -> IO CheckedPublication
fixturePublication fixture =
  checkedPublication
    fixture
    ( proposeOutgoingPublication
        fixture.nabla
        authorityA
        (initialState fixture.herald)
    )

fixtureBatch :: PublicationFixture -> IO PublicationBatch
fixtureBatch fixture = do
  publication <- fixturePublication fixture
  destination <- fixturePeerDestination 234 235 Weak
  checkedBatch
    publication
    fixture.process
    fixture.occurrence
    fixture.controlPrerequisite
    (destination :| [])

fixturePeerDestination ::
  Word8 ->
  Word8 ->
  ReplicaStrength ->
  IO PublicationDestination
fixturePeerDestination deltaByte incarnationByte strength =
  checked
    "peer destination"
    (fixturePeerDestinationEither deltaByte incarnationByte strength)

fixturePeerDestinationEither ::
  Word8 ->
  Word8 ->
  ReplicaStrength ->
  Either String PublicationDestination
fixturePeerDestinationEither deltaByte incarnationByte strength = do
  delta <- identity "DeltaId" mkDeltaId deltaByte
  incarnation <-
    identity "StoreIncarnationId" mkStoreIncarnationId incarnationByte
  Right (publicationDestination delta incarnation strength)

checkedBatch ::
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  IO PublicationBatch
checkedBatch publication process occurrence prerequisite destinations =
  checked
    "publication batch"
    (mkBatch publication process occurrence prerequisite destinations)

mkBatch ::
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  Either PublicationBatchProblem PublicationBatch
mkBatch publication process occurrence prerequisite =
  mkPublicationBatch
    (checkedPublicationId publication)
    process
    (checkedPublicationSort publication)
    occurrence
    (checkedPublicationCanonicalValue publication)
    Normal
    fixtureTopologyCut
    prerequisite

checkedRawBatch ::
  PublicationId ->
  ProcessEpochId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  IO PublicationBatch
checkedRawBatch identifier process sortId occurrence value prerequisite destinations =
  checked
    "raw publication batch"
    ( mkPublicationBatch
        identifier
        process
        sortId
        occurrence
        value
        Normal
        fixtureTopologyCut
        prerequisite
        destinations
    )

batchPeerPublication :: PublicationBatch -> PeerPublication
batchPeerPublication = presentedOrdinaryPeerPublication

batchDigest :: PublicationBatch -> PeerItemDigest
batchDigest = peerPublicationDigest . batchPeerPublication

prepareIncomingBatchCandidate ::
  StreamDirection ->
  StreamSequence ->
  PeerItemDigest ->
  PublicationBatch ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationCandidate
prepareIncomingBatchCandidate direction sequenceNumber digest batch =
  prepareIncomingBatchCandidateAt
    (heraldMembershipGenerationId fixtureHeraldMembershipGeneration)
    direction
    sequenceNumber
    digest
    batch

prepareIncomingBatchCandidateAt ::
  HeraldMembershipGenerationId ->
  StreamDirection ->
  StreamSequence ->
  PeerItemDigest ->
  PublicationBatch ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationCandidate
prepareIncomingBatchCandidateAt membership direction sequenceNumber digest batch =
  prepareIncomingPublicationCandidate
    membership
    direction
    sequenceNumber
    digest
    (batchPeerPublication batch)

batchDependency :: PublicationBatch -> PublicationDependency
batchDependency batch =
  EffectiveSortDependency
    (publicationBatchSortId batch)
    (publicationBatchOccurrenceId batch)

dependencyResolution ::
  PublicationBatch ->
  IO IncomingPublicationResolution
dependencyResolution batch =
  checked
    "dependency-held resolution"
    ( incomingPublicationResolution
        (Set.singleton (batchDependency batch))
        ( Map.fromList
            [ (destination, DestinationPending)
            | destination <- nonEmptyToList (publicationBatchDestinations batch)
            ]
        )
    )

headDestination :: PublicationBatch -> PublicationDestination
headDestination batch = case publicationBatchDestinations batch of
  first :| _ -> first

nonEmptyToList :: NonEmpty value -> [value]
nonEmptyToList (first :| remaining) = first : remaining

assertBoolNotEqual ::
  (Eq value, Show value) =>
  String ->
  value ->
  value ->
  Assertion
assertBoolNotEqual context left right
  | left /= right = pure ()
  | otherwise = assertFailure (context <> ": both were " <> show left)

assertIncomingProblem ::
  String ->
  IncomingPublicationProblem ->
  Either IncomingPublicationProblem value ->
  Assertion
assertIncomingProblem context expected result = case result of
  Left actual -> assertEqual context expected actual
  Right _ -> assertFailure (context <> ": preparation unexpectedly succeeded")

identity ::
  (Show problem) =>
  String ->
  (ByteString -> Either problem value) ->
  Word8 ->
  Either String value
identity label constructor byte =
  Bifunctor.first
    ((label <> ": ") <>)
    (Bifunctor.first show (constructor (fixtureIdentifierBytes byte)))

checked :: (Show problem) => String -> Either problem value -> IO value
checked context =
  either
    (assertFailure . ((context <> ": ") <>) . show)
    pure

assertPreparationError ::
  String ->
  PublicationPreparationError ->
  Either PublicationPreparationError PreparedOutgoingPublication ->
  Assertion
assertPreparationError context expected result =
  case result of
    Left actual -> assertEqual context expected actual
    Right _ -> assertFailure (context <> ": preparation unexpectedly succeeded")
