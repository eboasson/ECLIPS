{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module EffectivePublicationProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (..),
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    authorityEpochView,
    controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    labelAuthorityEpoch,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkSortDefinitionOccurrenceId,
    nablaSequence,
    publicationId,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    PreparedLabelFacts,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    checkedGenesisAuthority,
    existingReleasedAuthority,
    initialBootstrapLabelEvidence,
    initialPublicationLabelEvidence,
    labelRecordRetainedAuthority,
    labelRecordRevision,
    mkLabelRevision,
    mkPreparedLabelFacts,
    preparedAuthorityDisposition,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateGeneration,
    releasedLabelStateView,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Query qualified as Query
import Eclips.Domain.Route (ReplicaStrength (Normal))
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
    ScalarComparison (ScalarEqual),
    ScalarLiteral (LiteralBool),
    SortKind (..),
    StructuralCarrierRole (NablaCarrier),
    checkDescriptor,
    descriptorSchema,
  )
import Eclips.Domain.Sort.Profile (PredefinedSortRole (NablaRole, NeutralVertexRole), profileSortFor)
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    mkInitialProjectionDigest,
    profileCatalogueDigest,
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.StructuralConsequence (predefinedDisappearanceCause)
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
    valueAt,
    viewValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectivePublication
  ( EffectivePublicationProblem (..),
    EffectivePublicationView,
    PublicationHiddenReason (..),
    checkEffectivePublicationContext,
    effectivePeerPreparationAuthority,
    effectivePeerPreparationController,
    effectivePeerPreparationHiddenReason,
    effectivePeerPreparationVisiblePair,
    effectivePublicationAuthority,
    effectivePublicationController,
    effectivePublicationHiddenReason,
    effectivePublicationMatchesQuery,
    effectivePublicationRawPublication,
    effectivePublicationValue,
    effectivePublicationVisiblePair,
    effectiveStructuralPreparationAuthority,
    effectiveStructuralPreparationController,
    effectiveStructuralPreparationHiddenReason,
    effectiveStructuralPreparationVisiblePair,
    interpretEffectivePublication,
    prepareEffectivePeerPublication,
    prepareEffectiveStructuralPublication,
    projectEffectivePublicationZombieOverlay,
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
  ( Positive (..),
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    property,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "live effective publication"
    [ testCase "a released delete dominates obsolete suppression" caseHiddenPrecedence,
      testCase "released labels drive query and every consumer adapter" caseReleasedLabelConsumers,
      testCase "Controlled End knowledge projects process labels to zombie" caseProcessZombieProjection,
      testCase "isolation overlay governs both predicate matching and materialization" caseIsolationZombieProjection,
      testCase "ordinary label release preserves its controller without inventing operation authority" caseEffectiveAuthority,
      testProperty "initial evidence retains the first checked publication across updates and End" propInitialEvidence,
      testCase "initial evidence comes from bootstrap roots and yields to canonical overlays" caseInitialEvidenceSources,
      testCase "interpretation preserves immutable raw publication history" caseRawImmutability,
      testCase "the live Controlled owner enforces prior, replay, and terminal delete" caseLiveOwner,
      testCase "canonical first Release precedes receiver knowledge without granting possession" caseAbsentCanonicalRelease,
      testCase "live release records retain the complete authority chain" caseLabelAuthorityChain,
      testCase "a long live release schedule retains the complete authority chain" caseLongAuthoritySchedule,
      testCase "interpretation requires the exact Controlled observation" caseOwnerBinding,
      testCase "a payload-free terminal tombstone hides retained history" caseTerminalTombstone,
      testProperty "generated release/end/delete schedules match an independent reference" propGeneratedSchedule,
      testProperty "Store materialization equals the independent reference" propMaterializationReference,
      testProperty "intermediate-generation observations arrive and replay after later releases" propIntermediateGenerationReplay,
      testProperty "delayed generations preserve current authority across changing owners" propDelayedGenerationOwners,
      testCase "same-generation observations compare labels after known End only" caseEndNormalizedObservation
    ]

caseHiddenPrecedence :: Assertion
caseHiddenPrecedence = do
  let current = controlledPublication 1 ((ProcessLabel processA, 0)) False 10
      obsolete = controlledPublication 2 ((ProcessLabel processA, 0)) True 20
      controlled = controlledState [current, obsolete]
      deleted = installRelease controlled 4 (releasedLabel ((ProcessLabel processA, 0))) Nothing (releasedDeleted 0)
  hiddenReason
    "Controlled suppression hides every retained observation"
    PublicationObsoleteSuppressed
    (effectiveView controlled current)
  hiddenReason
    "the retained obsolete winner is hidden too"
    PublicationObsoleteSuppressed
    (effectiveView controlled obsolete)
  let deletedView = checked "deleted adapter view" (effectiveView deleted current)
  hiddenReason
    "the live released delete dominates suppression"
    PublicationDeleted
    (effectiveView deleted current)
  assertEqual
    "peer preparation suppresses a released delete"
    (Just PublicationDeleted)
    (effectivePeerPreparationHiddenReason (prepareEffectivePeerPublication deletedView))
  assertEqual
    "structural preparation suppresses a released delete"
    (Just PublicationDeleted)
    (effectiveStructuralPreparationHiddenReason (prepareEffectiveStructuralPublication deletedView))

caseReleasedLabelConsumers :: Assertion
caseReleasedLabelConsumers = do
  let raw = controlledPublication 4 ((ProcessLabel processA, 0)) False 30
      controlled0 = controlledState [raw]
      controlled =
        installRelease
          controlled0
          4
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel ((ProcessLabel processB, 1)))
      view = checked "released effective view" (effectiveView controlled raw)
      queryB = labelQuery ((ProcessLabel processB, 1))
      queryA = labelQuery ((ProcessLabel processA, 0))
      expectedValue = controlledValue objectIdentity ((ProcessLabel processB, 1)) False 30
      peer = prepareEffectivePeerPublication view
      structural = prepareEffectiveStructuralPublication view
      expectedAuthority = Nothing
  assertEqual "effective label replaces the embedded label" (Just ((ProcessLabel processB, 1))) (labelFromView view)
  assertEqual "released-label query matches" (Right True) (effectivePublicationMatchesQuery queryB view)
  assertEqual "old-label query no longer matches" (Right False) (effectivePublicationMatchesQuery queryA view)
  assertEqual
    "the immutable raw value still has the old label"
    (Right False)
    (Query.matchesQueryPredicate queryB (checkedPublicationValue raw))
  assertEqual "Store materialization uses exact raw provenance" (Just (raw, expectedValue)) (effectivePublicationVisiblePair view)
  assertEqual "peer preparation uses the same projection" (Just (raw, expectedValue)) (effectivePeerPreparationVisiblePair peer)
  assertEqual "peer preparation routes under the released controller" (Just processB) (effectivePeerPreparationController peer)
  assertEqual "peer preparation grants no ordinary-target operation tenure" expectedAuthority (authorityView <$> effectivePeerPreparationAuthority peer)
  assertEqual "structural preparation uses the same projection" (Just (raw, expectedValue)) (effectiveStructuralPreparationVisiblePair structural)
  assertEqual "structural preparation routes under the released controller" (Just processB) (effectiveStructuralPreparationController structural)
  assertEqual "structural preparation grants no ordinary-target operation tenure" expectedAuthority (authorityView <$> effectiveStructuralPreparationAuthority structural)

caseProcessZombieProjection :: Assertion
caseProcessZombieProjection = do
  let raw = controlledPublication 5 ((ProcessLabel processA, 0)) False 40
      retained = controlledState [raw]
      embeddedEnded = endProcess processA retained
      embeddedView = checked "embedded zombie view" (effectiveView embeddedEnded raw)
      released =
        installRelease
          retained
          4
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel ((ProcessLabel processB, 1)))
      releasedEnded = endProcess processB released
      releasedView = checked "released zombie view" (effectiveView releasedEnded raw)
  assertEqual "embedded process becomes zombie" (Just ((ZombieLabel processA, 0))) (labelFromView embeddedView)
  assertEqual "released process becomes zombie" (Just ((ZombieLabel processB, 1))) (labelFromView releasedView)
  assertEqual "embedded zombie has no controller" Nothing (effectivePublicationController embeddedView)
  assertEqual "embedded zombie has no authority" Nothing (effectivePublicationAuthority embeddedView)
  assertEqual "released zombie has no controller" Nothing (effectivePublicationController releasedView)
  assertEqual "released zombie has no authority" Nothing (effectivePublicationAuthority releasedView)

caseIsolationZombieProjection :: Assertion
caseIsolationZombieProjection = do
  let raw = controlledPublication 17 ((ProcessLabel processA, 0)) False 120
      liveView = checked "live isolation source view" (effectiveView (controlledState [raw]) raw)
      projected =
        projectEffectivePublicationZombieOverlay
          (Set.singleton processA)
          liveView
      zombieQuery = labelQuery ((ZombieLabel processA, 0))
      liveQuery = labelQuery ((ProcessLabel processA, 0))
      expectedValue = controlledValue objectIdentity ((ZombieLabel processA, 0)) False 120
  assertEqual
    "the immutable publication remains the provenance of the zombie presentation"
    (Just (raw, expectedValue))
    (effectivePublicationVisiblePair projected)
  assertEqual
    "the zombie predicate observes the same value later returned to the application"
    (Right True)
    (effectivePublicationMatchesQuery zombieQuery projected)
  assertEqual
    "the pre-isolation process predicate cannot match a zombie presentation"
    (Right False)
    (effectivePublicationMatchesQuery liveQuery projected)
  assertEqual "the abandoned process is no longer a live controller" Nothing (effectivePublicationController projected)
  assertEqual "the abandoned process exposes no live authority" Nothing (effectivePublicationAuthority projected)

caseEffectiveAuthority :: Assertion
caseEffectiveAuthority = do
  let raw = controlledPublication 6 ((ProcessLabel processA, 0)) False 50
      controlled0 = controlledState [raw]
      controlled =
        installRelease
          controlled0
          6
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel ((ProcessLabel processB, 0)))
      view = checked "live authority projection" (effectiveView controlled raw)
      invalidFacts =
        preparedFacts
          controlled0
          9
          6
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel ((ProcessLabel processB, 0)))
  assertEqual "live controller" (Just processB) (effectivePublicationController view)
  assertEqual
    "the ordinary target has no operation tenure after its release"
    Nothing
    (authorityView <$> effectivePublicationAuthority view)
  assertEqual
    "installation must use its canonical decision position"
    (Left (Controlled.ControlledLabelDecisionIndexMismatch (controlIndex 6) (controlIndex 7)))
    (() <$ Controlled.prepareControlledLabelRelease initialProjection invalidFacts (controlIndex 7) controlled0)

propInitialEvidence :: Positive Word64 -> Bool -> Bool -> Property
propInitialEvidence (Positive supplied) nonInitial ended =
  let generation = if nonInitial then supplied else 0
      raw = controlledPublication 1 (ProcessLabel processA, generation) False 60
      newer = controlledPublication 2 (ProcessLabel processA, generation) False 70
      owner = (if ended then endProcess processA else id) (controlledState [raw, newer])
      prior = checked "initial evidence after newer publication" (Controlled.checkControlledLabelPrior initialProjection objectId owner)
   in conjoin
        [ Controlled.controlledLabelPriorInitialEvidence prior
            === initialPublicationLabelEvidence objectId (ProcessLabel processA, generation) (checkedPublicationId raw),
          Controlled.controlledLabelPriorEffectiveState prior
            === releasedLabel (if ended then (ZombieLabel processA, generation) else (ProcessLabel processA, generation))
        ]

caseInitialEvidenceSources :: Assertion
caseInitialEvidenceSources = do
  root <- checkedIO "bootstrap prior after End" (Controlled.checkControlledLabelPrior initialProjection objectId (endProcess processA nablaControlledState))
  assertEqual
    "bootstrap evidence retains its raw owner after End"
    (Just (initialBootstrapLabelEvidence objectId processA))
    (Controlled.controlledLabelPriorInitialEvidence root)
  let raw = controlledPublication 7 (ProcessLabel processA, 0) False 60
      owner =
        installRelease
          (controlledState [raw])
          8
          (releasedLabel (ProcessLabel processA, 0))
          Nothing
          (releasedLabel (VoidLabel, 0))
  overlaid <- checkedIO "canonical overlay prior" (Controlled.checkControlledLabelPrior initialProjection objectId owner)
  assertEqual "canonical overlay needs no initial evidence" Nothing (Controlled.controlledLabelPriorInitialEvidence overlaid)

caseRawImmutability :: Assertion
caseRawImmutability = do
  let raw = controlledPublication 7 ((ProcessLabel processA, 0)) False 60
      rawValueBefore = checkedPublicationValue raw
      rawCanonicalBefore = checkedPublicationCanonicalValue raw
      controlled0 = controlledState [raw]
      controlled =
        installRelease
          controlled0
          8
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel (VoidLabel, 0))
      storeBefore =
        checked
          "raw Store application"
          (DomainStore.applyPublication Normal raw (DomainStore.emptyDeltaStore (descriptorSortId descriptor)))
      snapshotBefore = DomainStore.retainedSnapshot storeBefore
      view = checked "immutable raw view" (effectiveView controlled raw)
  assertEqual "raw publication is retained exactly" (Just raw) (effectivePublicationRawPublication view)
  assertEqual "raw value remains embedded" rawValueBefore (checkedPublicationValue raw)
  assertEqual "raw canonical bytes remain embedded" rawCanonicalBefore (checkedPublicationCanonicalValue raw)
  assertBool "effective materialization differs from raw" (effectivePublicationValue view /= Just rawValueBefore)
  assertEqual "pure interpretation does not alter Store" snapshotBefore (DomainStore.retainedSnapshot storeBefore)

-- Source evidence and a canonical decision exist independently of this
-- receiver's publication view. The receiver must not repeat the caller's CAS
-- against absence, and later observations must use the installed overlay.
caseAbsentCanonicalRelease :: Assertion
caseAbsentCanonicalRelease =
  mapM_
    installBeforeObservation
    [ releasedLabel (VoidLabel, 0),
      releasedLabel (ProcessLabel processB, 0),
      releasedDeleted 0
    ]
  where
    installBeforeObservation proposed = do
      let raw = controlledPublication 8 (ProcessLabel processA, 0) False 70
          source = controlledState [raw]
          receiver = Controlled.emptyState
          facts = preparedFacts source 2 2 (releasedLabel (ProcessLabel processA, 0)) Nothing proposed
      assertEqual
        "absence remains insufficient for caller admission"
        (Left (Controlled.ControlledLabelPriorObjectUnavailable objectId))
        (Controlled.checkControlledLabelPrior initialProjection objectId receiver)
      prepared <-
        checkedIO
          "install first canonical Release at an uninvolved receiver"
          (Controlled.prepareControlledLabelRelease initialProjection facts (controlIndex 2) receiver)
      let released = Controlled.commitControlledLabelRelease prepared
      assertEqual "canonical installation materializes no payload" Nothing (Controlled.controlledLocalRecord objectId released)
      assertBool "canonical installation creates no local label capability" (not (Controlled.controlledObjectLabelable objectId released))
      assertBool "canonical installation grants no target possession" (not (Controlled.controlledHasNormalPossession processB objectId released))
      replay <-
        checkedIO
          "replay first canonical Release before materialization"
          (Controlled.prepareControlledLabelRelease initialProjection facts (controlIndex 2) released)
      assertEqual "exact replay is inert" Controlled.ControlledLabelReleaseExactReplay (Controlled.preparedControlledLabelReleaseDisposition replay)
      assertBool "exact replay retains the receiver exactly" (Controlled.commitControlledLabelRelease replay == released)
      let beforeObservation = case releasedLabelStateView proposed of
            ReleasedLabelView (ProcessLabel _, _) -> endProcess processB released
            _ -> released
      observation <- checkedIO "check later old-generation publication" (Controlled.checkControlledObservation descriptor occurrenceId raw)
      preparedObservation <- checkedIO "observe after canonical Release" (Controlled.prepareControlledPeerObservation observation beforeObservation)
      let (materialized, _) = Controlled.commitControlledPeerObservation preparedObservation
      assertEqual
        "later publication cannot rewrite canonical release evidence"
        (Controlled.controlledReleasedLabelEntries released)
        (Controlled.controlledReleasedLabelEntries materialized)
      view <- checkedIO "interpret later materialization" (effectiveView materialized raw)
      case releasedLabelStateView proposed of
        ReleasedDeletedView _ -> do
          assertEqual "canonical deletion hides delayed materialization" (Just PublicationDeleted) (effectivePublicationHiddenReason view)
          assertEqual "deleted payload never becomes visible" Nothing (effectivePublicationVisiblePair view)
        ReleasedLabelView (VoidLabel, _) ->
          assertEqual "delayed publication uses released Void generation" (Just (VoidLabel, 1)) (labelFromView view)
        ReleasedLabelView (ProcessLabel _, _) ->
          assertEqual "End after canonical Release survives delayed materialization" (Just (ZombieLabel processB, 1)) (labelFromView view)
        _ -> assertFailure "unexpected absent-release fixture outcome"

caseLiveOwner :: Assertion
caseLiveOwner = do
  let raw = controlledPublication 8 ((ProcessLabel processA, 0)) False 70
      initial = controlledState [raw]
      firstFacts =
        preparedFacts
          initial
          2
          2
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel (VoidLabel, 0))
  preparedFirst <-
    checkedIO
      "first live release"
      (Controlled.prepareControlledLabelRelease initialProjection firstFacts (controlIndex 2) initial)
  assertEqual
    "first disposition"
    Controlled.ControlledLabelReleaseInstalled
    (Controlled.preparedControlledLabelReleaseDisposition preparedFirst)
  let afterFirst = Controlled.commitControlledLabelRelease preparedFirst
      firstRecord = Controlled.controlledReleasedLabelRecord objectId afterFirst
  assertBool "first released record is retained" (firstRecord /= Nothing)

  replay <-
    checkedIO
      "exact live replay"
      (Controlled.prepareControlledLabelRelease initialProjection firstFacts (controlIndex 2) afterFirst)
  assertEqual
    "replay disposition"
    Controlled.ControlledLabelReleaseExactReplay
    (Controlled.preparedControlledLabelReleaseDisposition replay)
  let afterReplay = Controlled.commitControlledLabelRelease replay
  assertBool "replay is state-idempotent" (afterFirst == afterReplay)

  let wrongPriorFacts =
        preparedFacts
          afterReplay
          4
          4
          (releasedLabel ((ProcessLabel processA, 0)))
          (Just (revisionAt 2))
          (releasedLabel ((ProcessLabel processB, 0)))
  case Controlled.prepareControlledLabelRelease initialProjection wrongPriorFacts (controlIndex 4) afterReplay of
    Left (Controlled.ControlledLabelReleasePriorStateMismatch _ _) -> pure ()
    other -> assertFailure ("expected live prior-state mismatch, got " <> releasePreparationShape other)

  let conflictingFacts =
        preparedFacts
          afterReplay
          3
          2
          (releasedLabel (VoidLabel, 1))
          (Just (revisionAt 2))
          (releasedLabel ((ProcessLabel processB, 0)))
  case Controlled.prepareControlledLabelRelease initialProjection conflictingFacts (controlIndex 2) afterReplay of
    Left (Controlled.ControlledLabelReleaseRecordConflict object revision) -> do
      assertEqual "conflict names the object" objectId object
      assertEqual "conflict names the retained revision" (revisionAt 2) revision
    other -> assertFailure ("expected same-revision conflict, got " <> releasePreparationShape other)

  let afterSecond =
        installRelease
          afterReplay
          4
          (releasedLabel (VoidLabel, 1))
          (Just (revisionAt 2))
          (releasedLabel ((ProcessLabel processB, 0)))
      afterDelete =
        installRelease
          afterSecond
          6
          (releasedLabel ((ProcessLabel processB, 2)))
          (Just (revisionAt 4))
          (releasedDeleted 0)
      resurrectionFacts =
        preparedFacts
          afterDelete
          8
          8
          (releasedLabel ((ProcessLabel processB, 0)))
          (Just (revisionAt 6))
          (releasedLabel (VoidLabel, 0))
  case Controlled.prepareControlledLabelRelease initialProjection resurrectionFacts (controlIndex 8) afterDelete of
    Left
      ( Controlled.ControlledLabelReleasePriorProblem
          (Controlled.ControlledLabelPriorDeleted object revision)
        ) -> do
        assertEqual "deleted object" objectId object
        assertEqual "deleted revision" (revisionAt 6) revision
    other -> assertFailure ("expected terminal-delete rejection, got " <> releasePreparationShape other)

caseLabelAuthorityChain :: Assertion
caseLabelAuthorityChain = do
  let initial = nablaControlledState
      changed =
        installRelease
          initial
          4
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          (releasedLabel ((ProcessLabel processB, 0)))
      same =
        installRelease
          changed
          6
          (releasedLabel ((ProcessLabel processB, 1)))
          (Just (revisionAt 4))
          (releasedLabel ((ProcessLabel processB, 0)))
      passivated =
        installRelease
          changed
          6
          (releasedLabel ((ProcessLabel processB, 1)))
          (Just (revisionAt 4))
          (releasedLabel (VoidLabel, 0))
      reactivated =
        installRelease
          passivated
          8
          (releasedLabel (VoidLabel, 2))
          (Just (revisionAt 6))
          (releasedLabel ((ProcessLabel processB, 0)))
      forgedFacts =
        checked
          "facts with an unrelated existing authority"
          ( mkPreparedLabelFacts
              (decisionId 10)
              (controlIndex 10)
              objectId
              (releasedLabel ((ProcessLabel processB, 2)))
              (releasedLabel ((ProcessLabel processB, 1)))
              (Just (revisionAt 4))
              profileCatalogueDigest
              (Just genesisAuthorityEpoch)
              (Just (existingReleasedAuthority (revisionAt 4)))
              (preparedAuthorityDisposition (Just genesisAuthorityEpoch) ((ProcessLabel processB, 0)) (releasedLabel ((ProcessLabel processB, 0))))
              memberDigest
          )
  assertEqual "A to B derives release tenure" (Just (LabelAuthorityEpochView (controlIndex 4))) (releasedAuthorityView changed)
  assertEqual "B to B retains exact prior release tenure" (Just (LabelAuthorityEpochView (controlIndex 4))) (releasedAuthorityView same)
  assertEqual "B to void retains historical tenure" (Just (LabelAuthorityEpochView (controlIndex 4))) (releasedAuthorityView passivated)
  assertEqual "void to B derives a new tenure" (Just (LabelAuthorityEpochView (controlIndex 8))) (releasedAuthorityView reactivated)
  case Controlled.prepareControlledLabelRelease initialProjection forgedFacts (controlIndex 10) changed of
    Left (Controlled.ControlledLabelReleasePriorAuthorityMismatch expected actual) -> do
      assertEqual "forged expected authority" (Just genesisAuthorityEpoch) expected
      assertEqual "actual release authority" (Just (labelAuthorityEpoch (controlIndex 4))) actual
    other -> assertFailure ("expected live authority mismatch, got " <> releasePreparationShape other)

caseLongAuthoritySchedule :: Assertion
caseLongAuthoritySchedule = do
  let initial = nablaControlledState
      afterB = installRelease initial 2 (releasedLabel ((ProcessLabel processA, 0))) Nothing (releasedLabel ((ProcessLabel processB, 0)))
      afterSame = installRelease afterB 4 (releasedLabel ((ProcessLabel processB, 1))) (Just (revisionAt 2)) (releasedLabel ((ProcessLabel processB, 0)))
      afterVoid = installRelease afterSame 6 (releasedLabel ((ProcessLabel processB, 2))) (Just (revisionAt 4)) (releasedLabel (VoidLabel, 0))
      afterVoidAgain = installRelease afterVoid 8 (releasedLabel (VoidLabel, 3)) (Just (revisionAt 6)) (releasedLabel (VoidLabel, 0))
      afterC = installRelease afterVoidAgain 10 (releasedLabel (VoidLabel, 4)) (Just (revisionAt 8)) (releasedLabel ((ProcessLabel processC, 5)))
      afterA = installRelease afterC 12 (releasedLabel ((ProcessLabel processC, 5))) (Just (revisionAt 10)) (releasedLabel ((ProcessLabel processA, 0)))
      afterDelete = installRelease afterA 14 (releasedLabel ((ProcessLabel processA, 6))) (Just (revisionAt 12)) (releasedDeleted 0)
  assertEqual
    "every release retains or derives the expected tenure"
    [ LabelAuthorityEpochView (controlIndex 2),
      LabelAuthorityEpochView (controlIndex 2),
      LabelAuthorityEpochView (controlIndex 2),
      LabelAuthorityEpochView (controlIndex 2),
      LabelAuthorityEpochView (controlIndex 10),
      LabelAuthorityEpochView (controlIndex 12),
      LabelAuthorityEpochView (controlIndex 12)
    ]
    ( fmap
        (maybe (error "release lost retained authority") id . releasedAuthorityView)
        [afterB, afterSame, afterVoid, afterVoidAgain, afterC, afterA, afterDelete]
    )
  assertBool "terminal schedule result remains deleted" (Controlled.controlledObjectReleasedDeleted objectId afterDelete)

caseOwnerBinding :: Assertion
caseOwnerBinding = do
  let retained = controlledPublication 12 ((ProcessLabel processA, 0)) False 90
      conflicting = controlledPublication 12 ((ProcessLabel processA, 0)) False 91
      controlled = controlledState [retained]
      regular = regularPublication 13 92
  case checkEffectivePublicationContext descriptor Controlled.emptyState retained of
    Left problem ->
      assertEqual
        "a controlled publication cannot be interpreted without its owner record"
        (EffectivePublicationControlledObjectUnavailable objectId)
        problem
    Right _ -> assertFailure "missing Controlled record was accepted"
  case checkEffectivePublicationContext descriptor controlled conflicting of
    Left problem ->
      assertEqual
        "equal publication identity cannot be rebound to a different checked payload"
        (EffectivePublicationControlledObservationMismatch objectId)
        problem
    Right _ -> assertFailure "conflicting checked payload was rebound"
  let visible = checked "regular pass-through" (effectiveViewFor regularDescriptor Controlled.emptyState regular)
  assertEqual "regular raw publication" (Just regular) (effectivePublicationRawPublication visible)
  assertEqual "regular effective value" (Just (checkedPublicationValue regular)) (effectivePublicationValue visible)

caseTerminalTombstone :: Assertion
caseTerminalTombstone = do
  let retained = controlledPublication 14 ((ProcessLabel processA, 0)) False 93
      controlled = controlledState [retained]
      probe = checked "terminal tombstone probe" (deriveDisappearanceProbeId (controlIndex 16))
      cause = checked "terminal tombstone cause" (predefinedDisappearanceCause probe (controlIndex 17))
  prepared <-
    checkedIO
      "prepare payload-free terminal removal"
      (Controlled.prepareControlledRemoval cause objectId controlled)
  let terminal = Controlled.commitControlledRemoval prepared
  assertEqual
    "terminal removal purges the live Controlled record"
    Nothing
    (Controlled.controlledLocalRecord objectId terminal)
  hiddenReason
    "the tombstone dominates immutable retained payload history"
    PublicationDeleted
    (effectiveView terminal retained)

propGeneratedSchedule :: [Bool] -> Bool -> Bool -> Property
propGeneratedSchedule processLabels deleteAtEnd ended =
  case runGeneratedSchedule processLabels deleteAtEnd ended of
    Left problem -> counterexample problem (property False)
    Right actual ->
      let expected = referenceSchedule processLabels deleteAtEnd ended
       in counterexample
            ("labels=" <> show processLabels <> ", delete=" <> show deleteAtEnd <> ", end=" <> show ended <> ", actual=" <> show actual <> ", expected=" <> show expected)
            (actual === expected)

data ScheduleProjection = ScheduleProjection
  { hidden :: Maybe PublicationHiddenReason,
    label :: Maybe Label,
    controller :: Maybe ProcessEpochId,
    authority :: Maybe AuthorityEpochView,
    rawVisible :: Bool,
    retainedReleaseCount :: Int
  }
  deriving stock (Eq, Show)

runGeneratedSchedule :: [Bool] -> Bool -> Bool -> Either String ScheduleProjection
runGeneratedSchedule processLabels deleteAtEnd ended = do
  let raw = controlledPublication 15 ((ProcessLabel processA, 0)) False 100
      controlled0 = controlledState [raw]
  (released, prior, priorRevision) <-
    foldM
      applyFlag
      (controlled0, releasedLabel ((ProcessLabel processA, 0)), Nothing)
      (zip [0 :: Word64 ..] processLabels)
  finalReleased <-
    if deleteAtEnd
      then do
        let index = 2 + 2 * fromIntegral (length processLabels)
        installReleaseEither released index prior priorRevision (releasedDeleted 0)
      else Right released
  let finalState
        | ended = endProcess processA (endProcess processB finalReleased)
        | otherwise = finalReleased
  view <- mapProblem (effectiveView finalState raw)
  Right
    ScheduleProjection
      { hidden = effectivePublicationHiddenReason view,
        label = labelFromView view,
        controller = effectivePublicationController view,
        authority = authorityView <$> effectivePublicationAuthority view,
        rawVisible = effectivePublicationRawPublication view == Just raw,
        retainedReleaseCount = length (Controlled.controlledReleasedLabelEntries finalState)
      }
  where
    applyFlag (state, prior, priorRevision) (ordinal, useProcess) = do
      let index = 2 + 2 * ordinal
          proposed = releasedLabel (if useProcess then (ProcessLabel processB, ordinal + 1) else (VoidLabel, ordinal + 1))
      successor <- installReleaseEither state index prior priorRevision proposed
      Right (successor, proposed, Just (revisionAt index))

referenceSchedule :: [Bool] -> Bool -> Bool -> ScheduleProjection
referenceSchedule processLabels deleteAtEnd ended =
  let steps = zip [0 :: Word64 ..] processLabels
      finalLabel = foldl referenceStep ((ProcessLabel processA, 0)) steps
      hasRelease = not (null processLabels) || deleteAtEnd
      effectiveLabel = if ended then zombie finalLabel else finalLabel
      live = case effectiveLabel of
        (ProcessLabel process, _) -> Just process
        _ -> Nothing
   in if deleteAtEnd
        then ScheduleProjection (Just PublicationDeleted) Nothing Nothing Nothing False 1
        else
          ScheduleProjection
            Nothing
            (Just effectiveLabel)
            live
            (if live == Nothing || hasRelease then Nothing else Just GenesisAuthorityEpochView)
            True
            (if hasRelease then 1 else 0)
  where
    referenceStep _ (ordinal, useProcess) = if useProcess then (ProcessLabel processB, ordinal + 1) else (VoidLabel, ordinal + 1)

    zombie supplied = case supplied of
      (ProcessLabel process, currentGeneration) -> (ZombieLabel process, currentGeneration)
      other -> other

propMaterializationReference :: Bool -> Bool -> Property
propMaterializationReference deleteReleased endReleasedProcess =
  let raw = controlledPublication 16 ((ProcessLabel processA, 0)) False 110
      initial = controlledState [raw]
      proposed = if deleteReleased then (releasedDeleted 0) else releasedLabel ((ProcessLabel processB, 0))
      released = installRelease initial 4 (releasedLabel ((ProcessLabel processA, 0))) Nothing proposed
      retained = if endReleasedProcess then endProcess processB released else released
      view = checked "generated consumer view" (effectiveView retained raw)
      expectedValue = controlledValue objectIdentity (if endReleasedProcess then (ZombieLabel processB, 1) else (ProcessLabel processB, 1)) False 110
      expectedPair = if deleteReleased then Nothing else Just (raw, expectedValue)
      expectedHidden = if deleteReleased then Just PublicationDeleted else Nothing
      expectedController
        | deleteReleased || endReleasedProcess = Nothing
        | otherwise = Just processB
      expectedAuthority = Nothing
      peer = prepareEffectivePeerPublication view
      structural = prepareEffectiveStructuralPublication view
   in counterexample
        ("view=" <> show view <> ", expected=" <> show expectedPair)
        ( conjoin
            [ effectivePublicationVisiblePair view === expectedPair,
              effectivePeerPreparationVisiblePair peer === expectedPair,
              effectiveStructuralPreparationVisiblePair structural === expectedPair,
              effectivePeerPreparationHiddenReason peer === expectedHidden,
              effectiveStructuralPreparationHiddenReason structural === expectedHidden,
              effectivePeerPreparationController peer === expectedController,
              effectiveStructuralPreparationController structural === expectedController,
              (authorityView <$> effectivePeerPreparationAuthority peer) === expectedAuthority,
              (authorityView <$> effectiveStructuralPreparationAuthority structural) === expectedAuthority
            ]
        )

-- Interleave successful same-owner releases and raw updates, then deliver both
-- exact retries and an unseen intermediate update after a further release.
-- Effective materialization keeps the latest complete pair in both cases.
propIntermediateGenerationReplay :: Property
propIntermediateGenerationReplay =
  forAll (chooseInt (1, 12)) $ \count ->
    case schedule (fromIntegral count) of
      Left problem -> counterexample problem False
      Right results -> conjoin results
  where
    schedule count = do
      let rawInitial = controlledState [controlledPublication 1 (ProcessLabel processA, 0) False 0]
      bootstrap <-
        mapProblem
          ( Controlled.prepareControlledBootstrap
              (Controlled.processFact (checked "replay process" (mkProcessId (identifierBytes 0x32))) processA herald genesisAuthorityEpoch)
              [Controlled.readerRootFact processA NeutralVertexRole (descriptorSortId descriptor) occurrenceId genesisAuthorityEpoch (controlIndex 0) reader]
              rawInitial
          )
      let initial = Controlled.commitControlledBootstrap bootstrap
      (updated, observations) <- foldM releaseAndObserve (initial, []) [1 .. count]
      final <-
        installReleaseEither
          updated
          (2 * (count + 1))
          (releasedLabel (ProcessLabel processA, count))
          (Just (revisionAt (2 * count)))
          (releasedLabel (ProcessLabel processA, count + 1))
      results <- traverse (replay final count) observations
      let unseenPublication = controlledPublication 1000 (ProcessLabel processA, count) False 999
      unseen <-
        mapProblem
          ( Controlled.checkControlledObservation
              descriptor
              occurrenceId
              unseenPublication
          )
      delayed <- mapProblem (Controlled.prepareControlledPeerObservation unseen final)
      let (withDelayed, _) = Controlled.commitControlledPeerObservation delayed
      delayedView <- mapProblem (effectiveView withDelayed unseenPublication)
      delayedReplay <- replay withDelayed count unseenPublication
      Right
        ( (labelFromView delayedView === Just (ProcessLabel processA, count + 1))
            : delayedReplay
            : results
        )
    reader = checked "replay reader" (mkDeltaId (identifierBytes 0x44))
    releaseAndObserve (owner, observations) ordinal = do
      let priorRevision = if ordinal == 1 then Nothing else Just (revisionAt (2 * (ordinal - 1)))
          publication = controlledPublication (ordinal + 1) (ProcessLabel processA, ordinal) False (fromIntegral ordinal)
      released <-
        installReleaseEither
          owner
          (2 * ordinal)
          (releasedLabel (ProcessLabel processA, ordinal - 1))
          priorRevision
          (releasedLabel (ProcessLabel processA, ordinal))
      observation <- mapProblem (Controlled.checkControlledObservation descriptor occurrenceId publication)
      prepared <- mapProblem (Controlled.prepareControlledPeerObservation observation released)
      Right (fst (Controlled.commitControlledPeerObservation prepared), publication : observations)
    replay retainedFinal count publication = do
      -- Replay itself is inert. The following real read/take round trip must
      -- notify its possession changes even though its semantic owner returns.
      let final = Controlled.clearPreparationChanges retainedFinal
          expectedPossessionChanges =
            Controlled.PreparationChanges Set.empty Set.empty (Set.singleton (processA, objectId))
      observation <- mapProblem (Controlled.checkControlledObservation descriptor occurrenceId publication)
      prepared <- mapProblem (Controlled.prepareControlledPeerObservation observation final)
      let (replayed, result) = Controlled.commitControlledPeerObservation prepared
      view <- mapProblem (effectiveView replayed publication)
      readPlan <- mapProblem (Controlled.prepareControlledStoreRead processA reader Normal observation replayed)
      let readOwner = Controlled.commitControlledStoreObservation readPlan
          (readChanges, readDrained) = Controlled.takePreparationChanges readOwner
      takePlan <- mapProblem (Controlled.prepareControlledStoreLocalTake processA reader Normal observation readDrained)
      let taken = Controlled.commitControlledStoreObservation takePlan
          (takeChanges, takenDrained) = Controlled.takePreparationChanges taken
      Right
        ( conjoin
            [ result === Controlled.ControlledPeerReplay,
              Controlled.controlledStorePossessionStrength processA reader objectId readOwner === Just Normal,
              readChanges === expectedPossessionChanges,
              takeChanges === expectedPossessionChanges,
              (takenDrained == final) === True,
              (replayed == final) === True,
              labelFromView view === Just (ProcessLabel processA, count + 1)
            ]
        )

-- Every old pair below was actually released, but the corresponding payload
-- reaches this owner for the first time only after the final release. An old
-- generation grants no permission to replace the current owner; its value and
-- immutable publication identity remain available to ordinary Store delivery.
propDelayedGenerationOwners :: Property
propDelayedGenerationOwners =
  forAll (chooseInt (2, 12)) $ \count ->
    forAll (vectorOf count (elements owners)) $ \releasedOwners ->
      case schedule releasedOwners of
        Left problem -> counterexample problem False
        Right results -> conjoin results
  where
    owners = [VoidLabel, ProcessLabel processA, ProcessLabel processB]
    initialLabel = (ProcessLabel processA, 0)
    schedule releasedOwners = do
      let initial = controlledState [controlledPublication 1 initialLabel False 0]
          releasedPairs = zip releasedOwners [1 ..]
      final <- foldM release initial (zip (initialLabel : releasedPairs) releasedPairs)
      let finalLabel = last releasedPairs
      delayed <- traverse (observe final finalLabel) (init releasedPairs)
      let invalidPairs =
            [(owner, snd finalLabel) | owner <- owners, owner /= fst finalLabel]
              <> [(owner, snd finalLabel + 1) | owner <- owners]
      invalid <- traverse (reject final) invalidPairs
      Right (delayed <> invalid)
    release owner (prior, proposed) =
      let generation = snd proposed
       in installReleaseEither
            owner
            (2 * generation)
            (releasedLabel prior)
            (if generation == 1 then Nothing else Just (revisionAt (2 * (generation - 1))))
            (releasedLabel proposed)
    observe final finalLabel supplied = do
      let publication = controlledPublication (1000 + snd supplied) supplied False (900 + fromIntegral (snd supplied))
      observation <- mapProblem (Controlled.checkControlledObservation descriptor occurrenceId publication)
      prepared <- mapProblem (Controlled.prepareControlledPeerObservation observation final)
      let (observed, _) = Controlled.commitControlledPeerObservation prepared
      view <- mapProblem (effectiveView observed publication)
      retry <- mapProblem (Controlled.prepareControlledPeerObservation observation observed)
      let (replayed, disposition) = Controlled.commitControlledPeerObservation retry
      Right
        ( conjoin
            [ labelFromView view === Just finalLabel,
              effectivePublicationRawPublication view === Just publication,
              (Controlled.controlledLocalRecord objectId observed >>= Controlled.controlledRecordObservation (checkedPublicationId publication)) === Just publication,
              Controlled.controlledReleasedLabelEntries observed === Controlled.controlledReleasedLabelEntries final,
              disposition === Controlled.ControlledPeerReplay,
              (replayed == observed) === True
            ]
        )
    reject final supplied = do
      observation <- mapProblem (Controlled.checkControlledObservation descriptor occurrenceId (controlledPublication 2000 supplied False 999))
      Right
        ( (() <$ Controlled.prepareControlledPeerObservation observation final)
            === Left (Controlled.ControlledPeerLabelMismatch objectId (Just initialLabel) (Just supplied))
        )

caseEndNormalizedObservation :: Assertion
caseEndNormalizedObservation = do
  let initialLabel = (ProcessLabel processA, 0)
      liveLabel = (ProcessLabel processB, 1)
      zombieLabel = (ZombieLabel processB, 1)
      initial = controlledState [controlledPublication 1 initialLabel False 0]
      released = installRelease initial 2 (releasedLabel initialLabel) Nothing (releasedLabel liveLabel)
      ended = endProcess processB released
      observe state supplied = do
        let publication = controlledPublication 10 supplied False 999
        observation <- checkedIO "End-normalized observation" (Controlled.checkControlledObservation descriptor occurrenceId publication)
        prepared <- checkedIO "admit End-normalized observation" (Controlled.prepareControlledPeerObservation observation state)
        let (observed, _) = Controlled.commitControlledPeerObservation prepared
        view <- checkedIO "End-normalized effective view" (effectiveView observed publication)
        assertEqual "End remains current authority" (Just zombieLabel) (labelFromView view)
        assertEqual "historical raw pair is unchanged" (Just publication) (effectivePublicationRawPublication view)
        assertEqual "ended owner grants no operation authority" Nothing (effectivePublicationAuthority view)
        assertEqual "ended owner remains passive" Nothing (effectivePublicationController view)
      reject state supplied = do
        observation <- checkedIO "contradictory observation" (Controlled.checkControlledObservation descriptor occurrenceId (controlledPublication 11 supplied False 999))
        assertEqual
          "unexplained same/future generation remains rejected"
          (Left (Controlled.ControlledPeerLabelMismatch objectId (Just initialLabel) (Just supplied)))
          (() <$ Controlled.prepareControlledPeerObservation observation state)
  observe ended liveLabel
  observe ended zombieLabel
  reject released zombieLabel
  reject ended (ProcessLabel processA, 1)
  reject ended (VoidLabel, 1)
  reject ended (ZombieLabel processB, 2)

effectiveView ::
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem EffectivePublicationView
effectiveView = effectiveViewFor descriptor

effectiveViewFor ::
  CanonicalDescriptor ->
  Controlled.State ->
  CheckedPublication ->
  Either EffectivePublicationProblem EffectivePublicationView
effectiveViewFor suppliedDescriptor controlled publication =
  interpretEffectivePublication
    <$> checkEffectivePublicationContext suppliedDescriptor controlled publication

controlledState :: [CheckedPublication] -> Controlled.State
controlledState =
  checked "controlled owner state"
    . foldM retain Controlled.emptyState
  where
    retain state publication = do
      observation <- mapProblem (Controlled.checkControlledObservation descriptor occurrenceId publication)
      prepared <- mapProblem (Controlled.prepareControlledPeerObservation observation state)
      Right (fst (Controlled.commitControlledPeerObservation prepared))

-- Operation-tenure chains belong to a real Nabla target. The ordinary
-- controlled-publication fixtures above intentionally have no such tenure.
nablaControlledState :: Controlled.State
nablaControlledState =
  Controlled.commitControlledBootstrap
    ( checked
        "Nabla authority fixture"
        ( Controlled.prepareControlledBootstrap
            (Controlled.processFact (checked "fixture process" (mkProcessId (identifierBytes 0x32))) processA herald genesisAuthorityEpoch)
            [ Controlled.writerRootFact
                processA
                NablaRole
                (profileSortFor NablaRole)
                occurrenceId
                genesisAuthorityEpoch
                (controlIndex 0)
                (checked "target Nabla" (mkNablaId (identifierBytes 0x31)))
                UnsequencedNabla
            ]
            Controlled.emptyState
        )
    )

endProcess :: ProcessEpochId -> Controlled.State -> Controlled.State
endProcess process =
  Controlled.commitControlledProcessRetirement
    . Controlled.prepareControlledProcessRetirement process

installRelease ::
  Controlled.State ->
  Word64 ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  Controlled.State
installRelease owner releaseIndex prior priorRevision proposed =
  checked "install live label release" (installReleaseEither owner releaseIndex prior priorRevision proposed)

installReleaseEither ::
  Controlled.State ->
  Word64 ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  Either String Controlled.State
installReleaseEither owner releaseIndex prior priorRevision proposed = do
  prepared <-
    mapProblem
      ( Controlled.prepareControlledLabelRelease
          initialProjection
          ( preparedFacts
              owner
              (fromIntegral releaseIndex)
              releaseIndex
              prior
              priorRevision
              proposed
          )
          (controlIndex releaseIndex)
          owner
      )
  Right (Controlled.commitControlledLabelRelease prepared)

preparedFacts ::
  Controlled.State ->
  Word8 ->
  Word64 ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  PreparedLabelFacts
preparedFacts owner decisionByte resolveIndex prior priorRevision proposed =
  checked
    "prepared live label facts"
    ( mkPreparedLabelFacts
        (decisionId decisionByte)
        (controlIndex resolveIndex)
        objectId
        (successorState prior proposed)
        prior
        priorRevision
        profileCatalogueDigest
        expectedAuthority
        justification
        (preparedAuthorityDisposition expectedAuthority (releasedLabelValue prior) proposed)
        memberDigest
    )
  where
    (expectedAuthority, justification) =
      case Controlled.controlledReleasedLabelRecord objectId owner of
        Nothing
          | Controlled.controlledCurrentStructuralRole objectId owner == Just NablaCarrier ->
              (Just genesisAuthorityEpoch, Just (checkedGenesisAuthority initialProjection))
        Nothing ->
          -- This ordinary controlled target has no initial operation tenure;
          -- the raw publication still retains its source writer's authority.
          (Nothing, Nothing)
        Just incumbent ->
          let authority = labelRecordRetainedAuthority incumbent
           in ( authority,
                existingReleasedAuthority (labelRecordRevision incumbent) <$ authority
              )

releasedLabelValue :: ReleasedLabelState -> Label
releasedLabelValue state = case releasedLabelStateView state of
  ReleasedLabelView label -> label
  (ReleasedDeletedView _) -> error "deleted prior is excluded by PreparedLabelFacts"

releasedAuthorityView :: Controlled.State -> Maybe AuthorityEpochView
releasedAuthorityView owner =
  authorityView
    <$> ( Controlled.controlledReleasedLabelRecord objectId owner
            >>= labelRecordRetainedAuthority
        )

authorityView :: AuthorityEpoch -> AuthorityEpochView
authorityView = authorityEpochView

hiddenReason ::
  String ->
  PublicationHiddenReason ->
  Either EffectivePublicationProblem EffectivePublicationView ->
  Assertion
hiddenReason context expected supplied = do
  view <- checkedIO context supplied
  assertEqual context (Just expected) (effectivePublicationHiddenReason view)
  assertEqual (context <> ": no raw escape") Nothing (effectivePublicationRawPublication view)
  assertEqual (context <> ": no value escape") Nothing (effectivePublicationValue view)

labelFromView :: EffectivePublicationView -> Maybe Label
labelFromView view = do
  value <- effectivePublicationValue view
  projected <- either (const Nothing) Just (valueAt (directProjection labelField) value)
  case viewValue projected of
    LabelValue label -> Just label
    _ -> Nothing

labelQuery :: Label -> Query.CheckedQueryPredicate
labelQuery supplied =
  checked
    "label query"
    ( Query.checkQueryPredicate
        (descriptorSchema (canonicalCheckedDescriptor descriptor))
        ( Query.QueryCompare
            (directProjection labelField)
            Query.ScalarEqual
            (Query.QueryLabel supplied)
        )
    )

revisionAt :: Word64 -> LabelRevision
revisionAt = checked "label revision" . mkLabelRevision . controlIndex

controlledPublication :: Word64 -> Label -> Bool -> Integer -> CheckedPublication
controlledPublication sequenceNumber supplied obsolete payload =
  checked
    "controlled publication"
    ( mkCheckedPublication
        descriptor
        (publicationIdentifier sequenceNumber)
        (controlledValue objectIdentity supplied obsolete payload)
    )

regularPublication :: Word64 -> Integer -> CheckedPublication
regularPublication sequenceNumber payload =
  checked
    "regular publication"
    ( mkCheckedPublication
        regularDescriptor
        (publicationIdentifier sequenceNumber)
        (regularValue objectIdentity payload)
    )

publicationIdentifier :: Word64 -> PublicationId
publicationIdentifier sequenceNumber =
  publicationId writer genesisAuthorityEpoch herald (nablaSequence sequenceNumber)

controlledValue :: GlobalUniqueId -> Label -> Bool -> Integer -> Value
controlledValue object supplied obsolete payload =
  checked
    "controlled value"
    ( recordValue
        [ (objectField, globalUniqueIdValue object),
          (labelField, labelValue supplied),
          (obsoleteField, boolValue obsolete),
          (payloadField, int64Value (fromInteger payload))
        ]
    )

regularValue :: GlobalUniqueId -> Integer -> Value
regularValue object payload =
  checked
    "regular value"
    ( recordValue
        [ (objectField, globalUniqueIdValue object),
          (payloadField, int64Value (fromInteger payload))
        ]
    )

descriptor :: CanonicalDescriptor
descriptor =
  canonicalizeCheckedDescriptor
    ( checked
        "controlled descriptor"
        ( checkDescriptor
            ApplicationDescriptor
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
                descriptorSpecObsolescence =
                  CompareField
                    (directProjection obsoleteField)
                    ScalarEqual
                    (LiteralBool True),
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = 0,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Just labelField,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = Nothing
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
                          (payloadField, int64Schema)
                        ]
                    ),
                descriptorSpecKeyProjections = [directProjection objectField],
                descriptorSpecValidity = AlwaysPredicate,
                descriptorSpecObsolescence = NeverPredicate,
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = 0,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Nothing,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = Nothing
              }
        )
    )

objectField, labelField, obsoleteField, payloadField :: FieldName
objectField = checked "object field" (mkFieldName "object")
labelField = checked "label field" (mkFieldName "label")
obsoleteField = checked "obsolete field" (mkFieldName "obsolete")
payloadField = checked "payload field" (mkFieldName "payload")

objectIdentity :: GlobalUniqueId
objectIdentity = checked "object identity" (mkGlobalUniqueId (identifierBytes 0x31))

objectId :: GlobalObjectId
objectId = globalObjectIdFromGlobalUniqueId objectIdentity

processA, processB, processC :: ProcessEpochId
processA = checked "process A" (mkProcessEpochId (identifierBytes 0x41))
processB = checked "process B" (mkProcessEpochId (identifierBytes 0x42))
processC = checked "process C" (mkProcessEpochId (identifierBytes 0x43))

writer :: NablaId
writer = checked "writer" (mkNablaId (identifierBytes 0x51))

herald :: HeraldEpoch
herald = checked "herald" (mkHeraldEpoch (identifierBytes 0x61))

occurrenceId :: SortDefinitionOccurrenceId
occurrenceId = checked "sort occurrence" (mkSortDefinitionOccurrenceId (identifierBytes 0x71))

decisionId :: Word8 -> LabelDecisionId
decisionId byte = checked "label decision" (mkLabelDecisionId (identifierBytes byte))

initialProjection :: InitialProjectionDigest
initialProjection = checked "initial projection" (mkInitialProjectionDigest (identifierBytes 0x81))

memberDigest :: MemberSetDigest
memberDigest =
  deriveMemberSetDigest
    (herald :| [checked "other herald" (mkHeraldEpoch (identifierBytes 0x62))])

identifierBytes :: Word8 -> ByteString.ByteString
identifierBytes = ByteString.replicate 32

mapProblem :: (Show problem) => Either problem value -> Either String value
mapProblem = either (Left . show) Right

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

releasePreparationShape ::
  Either Controlled.ControlledLabelReleaseError Controlled.PreparedControlledLabelRelease -> String
releasePreparationShape supplied = case supplied of
  Left problem -> "Left " <> show problem
  Right _ -> "Right PreparedControlledLabelRelease"

-- A fixture destination is given the one successor of its captured prior.
successorState :: ReleasedLabelState -> ReleasedLabelState -> ReleasedLabelState
successorState prior destination =
  let next = releasedLabelStateGeneration prior + 1
   in case releasedLabelStateView destination of
        ReleasedLabelView (owner, _) -> releasedLabel (owner, next)
        ReleasedDeletedView _ -> releasedDeleted next
