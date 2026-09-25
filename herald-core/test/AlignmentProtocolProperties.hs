{-# LANGUAGE OverloadedStrings #-}

module AlignmentProtocolProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Alignment.Plan.Identity qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as AlignmentTransfer
import Eclips.Herald.Input qualified as Input
import Eclips.Herald.Peer.RPC
  ( EstablishedPeerInbound (..),
    admitEstablishedPeerEnvelope,
    projectPeerControl,
  )
import Eclips.Herald.PeerDelivery qualified as PeerDelivery
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Protocol.Peer.Frame
  ( PeerFrameContinuation (..),
    PeerFrameFeedResult (..),
    encodePeerFrame,
    feedPeerFrame,
    finishPeerFrameDecoder,
    initialPeerFrameDecoder,
  )
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck (Property, choose, forAll, ioProperty, testProperty)

tests :: TestTree
tests =
  testGroup
    "alignment protocol"
    [ testCase "every alignment control arm survives DTO framing and RPC admission" caseEveryControlArm,
      testCase "semantic snapshot identity ignores chunks and preserves two witnesses" caseSemanticSnapshot,
      testCase "primordial and routed origins remain nominally distinct" caseObservationOrigins,
      testCase "route-cutover evidence is retained idempotently" caseRouteCutoverIdempotence,
      testCase "subscribe carries the complete immutable obligation and exact proof" caseCompleteSubscribe,
      testCase "certificate and ready-set digests bind their complete evidence" caseCertificateDigests,
      testCase "plan announcements admit empty and carried tables but reject incomplete claims" casePlanClaims,
      testCase "plan acceptance and route markers bind current routing coordinates" casePlanQualification,
      testProperty "nonzero plan attempts survive framing and reject mismatched created cuts" propPlanAttemptProtocol,
      testCase "obsolete reports retain exact canonical lost Stores on the reliable evidence lane" casePlanObsolete,
      testCase "reset announcements require fresh complete cuts and no ancestry" caseResetClaims
    ]

caseEveryControlArm :: Assertion
caseEveryControlArm = mapM_ assertRoundTrip alignmentControls

caseSemanticSnapshot :: Assertion
caseSemanticSnapshot = do
  let digest =
        Alignment.alignmentSemanticSnapshotDigest
          generation
          storeIncarnation
          DomainAlignment.initialStoreRevision
          [snapshotFact]
      sameFactsDifferentlyChunked =
        concat
          [ Alignment.alignmentSnapshotChunkRetainedStates
              ( Alignment.alignmentSnapshotChunk
                  subscriptionId
                  0
                  []
              ),
            Alignment.alignmentSnapshotChunkRetainedStates
              ( Alignment.alignmentSnapshotChunk
                  subscriptionId
                  1
                  [snapshotFact]
              )
          ]
      rechunkedDigest =
        Alignment.alignmentSemanticSnapshotDigest
          generation
          storeIncarnation
          DomainAlignment.initialStoreRevision
          sameFactsDifferentlyChunked
  assertEqual "chunk boundaries are absent from the digest" digest rechunkedDigest
  assertEqual
    "representative provenance remains weak observation A"
    publicationA
    ( Alignment.retainedPublicationEvidencePublicationId
        (Alignment.retainedStateEvidenceRepresentative snapshotFact)
    )
  assertEqual
    "joined strength is justified by distinct normal observation B"
    publicationB
    ( Alignment.retainedPublicationEvidencePublicationId
        (Alignment.retainedStateEvidenceStrengthWitness snapshotFact)
    )
  assertEqual
    "the explicit joined strength is retained"
    Route.Normal
    (Alignment.retainedStateEvidenceJoinedStrength snapshotFact)
  let replacedWitness = checked "same-observation fact" (Alignment.retainedStateEvidence weakEvidence weakEvidence Route.Weak)
      changed =
        Alignment.alignmentSemanticSnapshotDigest
          generation
          storeIncarnation
          DomainAlignment.initialStoreRevision
          [replacedWitness]
  assertBool "changing witness and joined strength changes semantic identity" (changed /= digest)

caseObservationOrigins :: Assertion
caseObservationOrigins = do
  assertEqual
    "primordial has no fabricated process"
    Nothing
    (Alignment.retainedPublicationEvidenceSourceProcess weakEvidence)
  assertEqual
    "primordial has no fabricated topology prerequisite"
    Nothing
    (Alignment.retainedPublicationEvidenceSourceTopologyPrerequisite weakEvidence)
  assertEqual
    "routed source is exact"
    (Just processEpoch)
    (Alignment.retainedPublicationEvidenceSourceProcess routedEvidence)
  assertBool
    "origin tags enter the canonical evidence transcript"
    ( Alignment.retainedPublicationEvidenceCanonicalBytes weakEvidence
        /= Alignment.retainedPublicationEvidenceCanonicalBytes routedEvidence
    )
  assertRoundTrip (Alignment.AlignmentSnapshotChunkTransferred snapshotChunk)

caseRouteCutoverIdempotence :: Assertion
caseRouteCutoverIdempotence = do
  let first =
        checked
          "first route cutover retention"
          (AlignmentTransfer.prepareRouteCutoverMarker routeCutover AlignmentTransfer.emptyState)
  let (retained, firstDisposition) = AlignmentTransfer.commitRouteCutoverMarker first
  assertEqual "first marker is retained" AlignmentTransfer.AlignmentTransferRetained firstDisposition
  let duplicate =
        checked
          "duplicate route cutover retention"
          (AlignmentTransfer.prepareRouteCutoverMarker routeCutover retained)
  let (unchanged, duplicateDisposition) =
        AlignmentTransfer.commitRouteCutoverMarker duplicate
  assertEqual "equal duplicate is a no-op" AlignmentTransfer.AlignmentTransferUnchanged duplicateDisposition
  assertEqual "equal duplicate preserves exact state" retained unchanged
  assertEqual
    "one evidence entry is retained"
    [ ( ( generation,
          heraldA,
          heraldB
        ),
        routeCutover
      )
    ]
    (AlignmentTransfer.routeCutoverEntries unchanged)

caseCompleteSubscribe :: Assertion
caseCompleteSubscribe = do
  assertEqual
    "the complete obligation is carried"
    obligation
    (Alignment.alignmentSubscribeObligation subscribe)
  assertEqual
    "destination stores are projected only from that obligation"
    (Alignment.alignmentObligationDestinationStores obligation)
    (Alignment.alignmentSubscribeDestinationStores subscribe)
  assertEqual
    "the final proof binds the source generation and certificate"
    ( Alignment.FinalCertificate
        generation
        (Alignment.historicalCertificateDigest certificate)
    )
    (Alignment.alignmentSubscribeSourceProof subscribe)
  assertRoundTrip (Alignment.AlignmentSubscribeRequested subscribe)
  let changedCause =
        StructuralConsequence.structuralOccurrenceCause
          ( Identity.structuralOccurrenceId
              heraldA
              (checked "changed structural sequence" (Identity.mkStructuralSequence 2))
          )
      obligationWith changedOccurrence changedStrength =
        checked
          "mutated obligation"
          ( Alignment.alignmentObligation
              obligationId
              changedOccurrence
              sortId
              sortOccurrence
              generation
              (Alignment.destinationStore deltaId storeIncarnation :| [])
              generation
              changedStrength
          )
      subscribeWith changedObligation =
        checked
          "mutated subscribe"
          ( Alignment.alignmentSubscribe
              changedObligation
              subscriptionId
              storeIncarnation
              ( Alignment.FinalCertificate
                  generation
                  (Alignment.historicalCertificateDigest certificate)
              )
          )
      causeMutated = subscribeWith (obligationWith changedCause Route.Weak)
      strengthMutated =
        subscribeWith
          ( obligationWith
              (StructuralConsequence.structuralOccurrenceCause cause)
              Route.Normal
          )
  assertBool "changing the carried cause changes the request" (causeMutated /= subscribe)
  assertBool "changing frozen path strength changes the request" (strengthMutated /= subscribe)
  mapM_
    (assertRoundTrip . Alignment.AlignmentSubscribeRequested)
    [causeMutated, strengthMutated]
  let wrongProof =
        Alignment.FinalCertificate
          generationB
          (Alignment.historicalCertificateDigest certificate)
  assertEqual
    "a proof for another source generation is intrinsically rejected"
    ( Left
        ( Alignment.AlignmentSubscribeSourceProofGenerationMismatch
            generation
            generationB
        )
    )
    ( Alignment.alignmentSubscribe
        obligation
        subscriptionId
        storeIncarnation
        wrongProof
    )

caseCertificateDigests :: Assertion
caseCertificateDigests = do
  let laterReady =
        Alignment.classMemberReady
          (Alignment.nextAlignmentEvidenceSequence evidenceSequence)
          generation
          storeIncarnation
          memberPrefixDigest
          (DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision)
  assertBool
    "ready-set order is canonical"
    ( Alignment.classMemberReadySetDigest [memberReady, laterReady]
        == Alignment.classMemberReadySetDigest [laterReady, memberReady]
    )
  assertBool
    "ready sequence/revision mutations change the complete digest"
    ( Alignment.classMemberReadySetDigest [memberReady]
        /= Alignment.classMemberReadySetDigest [laterReady]
    )
  let changedCertificate =
        Alignment.historicalCertificate
          generation
          storeIncarnation
          (Alignment.classMemberReadySetDigest [laterReady])
          bootstrapDigest
          DomainAlignment.initialStoreRevision
  assertBool
    "certificate digest binds exact member-ready evidence"
    ( Alignment.historicalCertificateDigest certificate
        /= Alignment.historicalCertificateDigest changedCertificate
    )

assertRoundTrip :: Alignment.AlignmentControl -> Assertion
assertRoundTrip alignment = do
  let control = case alignment of
        Alignment.AlignmentPlanAcceptanceAdvertised {} -> delivered
        Alignment.AlignmentPlanObsoleteAdvertised {} -> delivered
        Alignment.AlignmentCutAcceptanceAdvertised {} -> delivered
        Alignment.AlignmentMemberReadyAdvertised {} -> delivered
        _ -> Input.PeerAlignmentControl alignment
      delivered = Input.PeerAlignmentEvidenceDelivered DomainAlignment.firstAlignmentDeliverySequence alignment
  envelope <- either (assertFailure . show) pure (projectPeerControl control)
  framed <- case feedPeerFrame initialPeerFrameDecoder (encodePeerFrame envelope) of
    PeerFrameFeedResult [decoded] (NeedPeerFrameBytes decoder) -> do
      assertEqual "complete frame" (Right ()) (finishPeerFrameDecoder decoder)
      pure decoded
    other -> assertFailure ("unexpected framed alignment result: " <> show other)
  assertEqual
    (show alignment)
    (Right (EstablishedPeerControl mempty control))
    (admitEstablishedPeerEnvelope framed)

alignmentControls :: [Alignment.AlignmentControl]
alignmentControls =
  [ Alignment.AlignmentPlanAnnounced planAnnounce,
    Alignment.AlignmentPlanAcceptanceAdvertised planAccepted,
    Alignment.AlignmentCutAnnounced cutAnnounce,
    Alignment.AlignmentCutAcceptanceAdvertised cutAccepted,
    Alignment.AlignmentMemberReadyAdvertised memberReady,
    Alignment.AlignmentHistoricalCertificateAdvertised certificate,
    Alignment.AlignmentSubscribeRequested subscribe,
    Alignment.AlignmentSnapshotStarted snapshotStart,
    Alignment.AlignmentSnapshotChunkTransferred snapshotChunk,
    Alignment.AlignmentSnapshotEnded snapshotEnd,
    Alignment.AlignmentChangeTransferred change,
    Alignment.AlignmentLiveAdvertised live,
    Alignment.AlignmentAcknowledged acknowledgement,
    Alignment.AlignmentCancelled cancellation
  ]

planId :: Plan.AlignmentPlanId
planId = checked "plan identity" (Plan.alignmentPlanIdAtTopology (Debt.sortOccurrence sortId sortOccurrence) topologyCut placementVector)

planBinding :: Alignment.AlignmentPlanBindingClaim
planBinding = checked "plan binding" (Alignment.alignmentPlanBindingClaim (deltaId :| []) Alignment.AlignmentCreated generation)

planAnnounce :: Alignment.AlignmentPlanAnnounce
planAnnounce = checked "plan announcement" (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [planBinding] [] [cutAnnounce])

planAccepted :: Alignment.AlignmentPlanAccepted
planAccepted = checked "plan accepted" (Alignment.alignmentPlanAccepted planId heraldA DomainAlignment.EmptyHeraldPublicationPrefix)

laterPlanId :: Plan.AlignmentPlanId
laterPlanId = checked "later plan identity" (Plan.alignmentPlanIdAtTopology (Debt.sortOccurrence sortId sortOccurrence) topologyCut laterPlacement)
  where
    laterPlacement = checked "later placement" (DomainAlignment.physicalPlacementRevisionVector membership [(heraldA, DomainAlignment.nextPlacementRevision DomainAlignment.firstPlacementRevision)])

casePlanClaims :: Assertion
casePlanClaims = do
  let empty = checked "empty plan announcement" (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [] [] [])
      carriedBinding = checked "carried binding" (Alignment.alignmentPlanBindingClaim (deltaId :| []) Alignment.AlignmentCarried generation)
      carried = checked "carried plan announcement" (Alignment.alignmentPlanAnnounce laterPlanId topologyCut (Just planId) Alignment.AlignmentPredecessorUsable [carriedBinding] [] [])
  assertRoundTrip (Alignment.AlignmentPlanAnnounced empty)
  assertRoundTrip (Alignment.AlignmentPlanAnnounced carried)
  assertEqual "created generation requires its exact birth cut" (Left Alignment.AlignmentPlanAnnouncementCreatedCutsMismatch) (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [planBinding] [] [])
  assertEqual "overlapping class membership rejects" (Left Alignment.AlignmentPlanAnnouncementDuplicateMembers) (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [planBinding, planBinding] [] [cutAnnounce])
  assertEqual "relations cannot reference an absent class" (Left Alignment.AlignmentPlanAnnouncementRelationInvalid) (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [planBinding] [Alignment.alignmentPlanRelationClaim generation generationB Route.Normal] [cutAnnounce])
  assertEqual "carried bindings do not replace their birth cuts" (Left Alignment.AlignmentPlanAnnouncementCreatedCutsMismatch) (Alignment.alignmentPlanAnnounce laterPlanId topologyCut (Just planId) Alignment.AlignmentPredecessorUsable [carriedBinding] [] [cutAnnounce])

casePlanQualification :: Assertion
casePlanQualification = do
  assertEqual "outsider cannot accept the fixed plan" (Left (Alignment.AlignmentPlanAcceptanceHeraldOutsidePlacement heraldB)) (Alignment.alignmentPlanAccepted planId heraldB DomainAlignment.EmptyHeraldPublicationPrefix)
  let later = checked "later marker" (Alignment.alignmentRouteCutoverMarker laterPlanId generation heraldA heraldB)
  assertBool "same generation and endpoints at different plans have distinct evidence" (Alignment.alignmentRouteCutoverMarkerEvidenceDigest later /= Alignment.alignmentRouteCutoverMarkerEvidenceDigest routeCutover)
  assertRoundTrip (Alignment.AlignmentPlanAcceptanceAdvertised (checked "later accepted" (Alignment.alignmentPlanAccepted laterPlanId heraldA DomainAlignment.EmptyHeraldPublicationPrefix)))

propPlanAttemptProtocol :: Property
propPlanAttemptProtocol =
  forAll ((,) <$> choose (0, 1024) <*> choose (1, 1024)) $ \(membershipChange, ordinal) -> ioProperty $ do
    let attempt = DomainAlignment.mkAlignmentPlanAttemptAtMembership (Identity.controlIndex membershipChange) ordinal
        attemptedCut = alignmentCutAt attempt
        attemptedAnnounce = Alignment.alignmentCutAnnounce attemptedCut
        attemptedGeneration = Alignment.alignmentCutAnnounceGeneration attemptedAnnounce
        attemptedId = checked "attempted plan identity" (Plan.alignmentPlanIdAtTopologyAtAttempt attempt (Debt.sortOccurrence sortId sortOccurrence) topologyCut placementVector)
        attemptedBinding = checked "attempted plan binding" (Alignment.alignmentPlanBindingClaim (deltaId :| []) Alignment.AlignmentCreated attemptedGeneration)
        attemptedPlan = checked "attempted plan announcement" (Alignment.alignmentPlanAnnounce attemptedId topologyCut Nothing Alignment.AlignmentPredecessorUsable [attemptedBinding] [] [attemptedAnnounce])
        attemptedAcceptance = checked "attempted plan acceptance" (Alignment.alignmentPlanAccepted attemptedId heraldA DomainAlignment.EmptyHeraldPublicationPrefix)
        attemptedMarker = checked "attempted route marker" (Alignment.alignmentRouteCutoverMarker attemptedId attemptedGeneration heraldA heraldB)
        initialMarker = checked "initial route marker" (Alignment.alignmentRouteCutoverMarker planId attemptedGeneration heraldA heraldB)
        otherMembershipAttempt = DomainAlignment.mkAlignmentPlanAttemptAtMembership (Identity.controlIndex (membershipChange + 1)) ordinal
        otherMembershipId = checked "other-membership attempt" (Plan.alignmentPlanIdAtTopologyAtAttempt otherMembershipAttempt (Debt.sortOccurrence sortId sortOccurrence) topologyCut placementVector)
        attemptedObsolete = checked "attempted obsolete report" (Alignment.alignmentPlanObsolete attemptedId heraldA (Alignment.alignmentLostStore heraldA deltaId storeIncarnation DomainAlignment.firstPlacementRevision :| []))
    assertRoundTrip (Alignment.AlignmentCutAnnounced attemptedAnnounce)
    assertRoundTrip (Alignment.AlignmentPlanAnnounced attemptedPlan)
    assertRoundTrip (Alignment.AlignmentPlanAcceptanceAdvertised attemptedAcceptance)
    assertRoundTrip (Alignment.AlignmentPlanObsoleteAdvertised attemptedObsolete)
    assertEqual
      "the same ordinal from another membership cannot announce this created cut"
      (Left Alignment.AlignmentPlanAnnouncementCreatedCutsMismatch)
      (Alignment.alignmentPlanAnnounce otherMembershipId topologyCut Nothing Alignment.AlignmentPredecessorUsable [attemptedBinding] [] [attemptedAnnounce])
    assertEqual
      "a later attempt cannot announce the initial attempt's created cut"
      (Left Alignment.AlignmentPlanAnnouncementCreatedCutsMismatch)
      (Alignment.alignmentPlanAnnounce attemptedId topologyCut Nothing Alignment.AlignmentPredecessorUsable [planBinding] [] [cutAnnounce])
    assertEqual
      "an initial attempt cannot announce a later attempt's created cut"
      (Left Alignment.AlignmentPlanAnnouncementCreatedCutsMismatch)
      (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorUsable [attemptedBinding] [] [attemptedAnnounce])
    assertBool
      "a route marker binds the attempt even when its generation and endpoints agree"
      (Alignment.alignmentRouteCutoverMarkerEvidenceDigest attemptedMarker /= Alignment.alignmentRouteCutoverMarkerEvidenceDigest initialMarker)
    pure True

casePlanObsolete :: Assertion
casePlanObsolete = do
  let lost = Alignment.alignmentLostStore heraldA deltaId storeIncarnation DomainAlignment.firstPlacementRevision
      later = Alignment.alignmentLostStore heraldB deltaId storeIncarnation (DomainAlignment.nextPlacementRevision DomainAlignment.firstPlacementRevision)
      report facts = checked "obsolete report" (Alignment.alignmentPlanObsolete planId heraldA facts)
      canonical = report (lost :| [later])
      duplicated = report (later :| [lost, lost])
      control = Alignment.AlignmentPlanObsoleteAdvertised canonical
  assertEqual "fact order and duplicates do not alter the report" canonical duplicated
  assertEqual "canonical evidence bytes are stable" (Alignment.alignmentPlanObsoleteCanonicalBytes canonical) (Alignment.alignmentPlanObsoleteCanonicalBytes duplicated)
  assertEqual "exact home is retained" heraldA (Alignment.alignmentLostStoreHome lost)
  assertEqual "exact delta is retained" deltaId (Alignment.alignmentLostStoreDelta lost)
  assertEqual "exact incarnation is retained" storeIncarnation (Alignment.alignmentLostStoreIncarnation lost)
  assertEqual "withdrawal placement revision is retained" DomainAlignment.firstPlacementRevision (Alignment.alignmentLostStorePlacementRevision lost)
  assertEqual "an outsider cannot report for this plan" (Left (Alignment.AlignmentPlanObsoleteHeraldOutsidePlacement heraldB)) (Alignment.alignmentPlanObsolete planId heraldB (lost :| []))
  assertRoundTrip control
  assertBool "obsolete reports cannot use an unsequenced control envelope" (isLeft (projectPeerControl (Input.PeerAlignmentControl control)))
  assertBool "obsolete reports have a reliable delivery key" (PeerDelivery.evidenceKey control /= Nothing)
  assertEqual "delivery identity is plan/reporter, independently of the frozen payload" (PeerDelivery.evidenceKey control) (PeerDelivery.evidenceKey (Alignment.AlignmentPlanObsoleteAdvertised (report (lost :| []))))

caseResetClaims :: Assertion
caseResetClaims = do
  let attempt = DomainAlignment.nextAlignmentPlanAttempt DomainAlignment.initialAlignmentPlanAttempt
      identifier = checked "reset identity" (Plan.alignmentPlanIdAtTopologyAtAttempt attempt (Debt.sortOccurrence sortId sortOccurrence) topologyCut placementVector)
      freshCut = alignmentCutAt attempt
      announceFor selectedCut prior =
        let announced = Alignment.alignmentCutAnnounce selectedCut
            binding = checked "reset binding" (Alignment.alignmentPlanBindingClaim (deltaId :| []) Alignment.AlignmentCreated (Alignment.alignmentCutAnnounceGeneration announced))
         in Alignment.alignmentPlanAnnounce identifier topologyCut prior Alignment.AlignmentPredecessorReset [binding] [] [announced]
      malformedCut ancestors fresh = checked "changed reset cut" (DomainAlignment.alignmentCutAtAttempt attempt sortId sortOccurrence topologyCut placementVector (DomainAlignment.alignmentCutExactMembers freshCut) ancestors fresh)
      reset = checked "reset announcement" (announceFor freshCut Nothing)
  assertRoundTrip (Alignment.AlignmentPlanAnnounced reset)
  assertEqual "reset has an explicit predecessor mode" Alignment.AlignmentPredecessorReset (Alignment.alignmentPlanAnnouncePredecessorStatus reset)
  assertEqual "attempt zero cannot reset" (Left Alignment.AlignmentPlanAnnouncementResetShapeMismatch) (Alignment.alignmentPlanAnnounce planId topologyCut Nothing Alignment.AlignmentPredecessorReset [] [] [])
  assertEqual "reset cannot retain a predecessor claim" (Left Alignment.AlignmentPlanAnnouncementResetShapeMismatch) (announceFor freshCut (Just planId))
  assertEqual "reset cannot retain generation ancestry" (Left Alignment.AlignmentPlanAnnouncementResetShapeMismatch) (announceFor (malformedCut [generation] (DomainAlignment.alignmentCutFreshMemberBaseEvidence freshCut)) Nothing)
  assertEqual "every reset member requires its exact fresh base" (Left Alignment.AlignmentPlanAnnouncementResetShapeMismatch) (announceFor (malformedCut [] []) Nothing)
  assertEqual "reset retains its complete member set" [deltaId] (concatMap (NonEmpty.toList . Alignment.alignmentPlanBindingClaimMembers) (Alignment.alignmentPlanAnnounceBindings reset))

cutAnnounce :: Alignment.AlignmentCutAnnounce
cutAnnounce = Alignment.alignmentCutAnnounce alignmentCut

cutAccepted :: Alignment.AlignmentCutAccepted
cutAccepted =
  Alignment.alignmentCutAccepted
    generation
    heraldA
    topologyCutId
    placementVector
    DomainAlignment.EmptyHeraldPublicationPrefix

memberReady :: Alignment.ClassMemberReady
memberReady =
  Alignment.classMemberReady
    evidenceSequence
    generation
    storeIncarnation
    memberPrefixDigest
    DomainAlignment.initialStoreRevision

certificate :: Alignment.HistoricalCertificate
certificate =
  Alignment.historicalCertificate
    generation
    storeIncarnation
    (Alignment.classMemberReadySetDigest [memberReady])
    bootstrapDigest
    DomainAlignment.initialStoreRevision

subscribe :: Alignment.AlignmentSubscribe
subscribe =
  checked
    "subscribe"
    ( Alignment.alignmentSubscribe
        obligation
        subscriptionId
        storeIncarnation
        ( Alignment.FinalCertificate
            generation
            (Alignment.historicalCertificateDigest certificate)
        )
    )

snapshotStart :: Alignment.AlignmentSnapshotStart
snapshotStart =
  Alignment.alignmentSnapshotStart
    subscriptionId
    DomainAlignment.initialStoreRevision
    snapshotDigest
    1

snapshotChunk :: Alignment.AlignmentSnapshotChunk
snapshotChunk = Alignment.alignmentSnapshotChunk subscriptionId 0 [snapshotFact]

snapshotEnd :: Alignment.AlignmentSnapshotEnd
snapshotEnd =
  Alignment.alignmentSnapshotEnd
    subscriptionId
    DomainAlignment.initialStoreRevision
    snapshotDigest

change :: Alignment.AlignmentChange
change =
  Alignment.alignmentChange
    subscriptionId
    (DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision)
    routedEvidence

live :: Alignment.AlignmentLive
live = Alignment.alignmentLive subscriptionId DomainAlignment.initialStoreRevision

acknowledgement :: Alignment.AlignmentAck
acknowledgement =
  Alignment.alignmentAck subscriptionId DomainAlignment.initialStoreRevision

cancellation :: Alignment.AlignmentCancel
cancellation =
  Alignment.alignmentCancel subscriptionId Alignment.AlignmentRelationRemoved

snapshotDigest :: DomainAlignment.AlignmentSnapshotDigest
snapshotDigest =
  Alignment.alignmentSemanticSnapshotDigest
    generation
    storeIncarnation
    DomainAlignment.initialStoreRevision
    [snapshotFact]

snapshotFact :: Alignment.RetainedStateEvidence
snapshotFact =
  checked
    "two-witness snapshot fact"
    (Alignment.retainedStateEvidence weakEvidence normalEvidence Route.Normal)

weakEvidence :: Alignment.RetainedPublicationEvidence
weakEvidence =
  Alignment.primordialRetainedPublicationEvidence
    publicationA
    sortId
    sortOccurrence
    canonicalValue
    Route.Weak

normalEvidence :: Alignment.RetainedPublicationEvidence
normalEvidence =
  Alignment.primordialRetainedPublicationEvidence
    publicationB
    sortId
    sortOccurrence
    canonicalValue
    Route.Normal

routedEvidence :: Alignment.RetainedPublicationEvidence
routedEvidence =
  checked
    "routed evidence"
    ( Alignment.retainedPublicationEvidence
        publicationA
        processEpoch
        sortId
        sortOccurrence
        canonicalValue
        Route.Weak
        topologyCutId
        (Identity.controlIndex 1)
        Nothing
    )

obligation :: Alignment.AlignmentObligation
obligation =
  checked
    "obligation"
    ( Alignment.alignmentObligation
        obligationId
        (StructuralConsequence.structuralOccurrenceCause cause)
        sortId
        sortOccurrence
        generation
        (Alignment.destinationStore deltaId storeIncarnation :| [])
        generation
        Route.Weak
    )

alignmentCut :: DomainAlignment.AlignmentCut
alignmentCut = alignmentCutAt DomainAlignment.initialAlignmentPlanAttempt

alignmentCutAt :: DomainAlignment.AlignmentPlanAttempt -> DomainAlignment.AlignmentCut
alignmentCutAt attempt =
  checked
    "alignment cut"
    ( DomainAlignment.alignmentCutAtAttempt
        attempt
        sortId
        sortOccurrence
        topologyCut
        placementVector
        (DomainAlignment.alignmentMember deltaId storeIncarnation heraldA :| [])
        []
        [ DomainAlignment.freshMemberBaseEvidence
            deltaId
            storeIncarnation
            DomainAlignment.initialStoreRevision
        ]
    )

placementVector :: DomainAlignment.PhysicalPlacementRevisionVector
placementVector =
  checked
    "placement vector"
    ( DomainAlignment.physicalPlacementRevisionVector
        membership
        [(heraldA, DomainAlignment.firstPlacementRevision)]
    )

topologyCut :: Topology.TopologyCut
topologyCut =
  checked
    "topology cut"
    ( Topology.topologyCut
        (Topology.sameGenerationPredecessor predecessorTopologyCutId)
        ( Topology.topologyFrontier
            structuralVector
            (Identity.controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "alignment-protocol-fixture")
    )

topologyCutId :: Identity.TopologyCutId
topologyCutId = Topology.deriveTopologyCutId topologyCut

structuralVector :: Structural.StructuralVersionVector
structuralVector =
  Structural.emptyStructuralVersionVector membership

membership :: Membership.HeraldMembershipGeneration
membership =
  checked
    "membership"
    (Membership.genesisHeraldMembershipGeneration systemId (heraldA :| []))

generation :: DomainAlignment.ContextClassGenerationId
generation = DomainAlignment.deriveContextClassGenerationId alignmentCut

generationB :: DomainAlignment.ContextClassGenerationId
generationB =
  checked
    "generation B"
    (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xa2))

memberPrefixDigest :: DomainAlignment.MemberReadyEvidenceDigest
memberPrefixDigest = DomainAlignment.deriveMemberReadyEvidenceDigest "member-prefix"

bootstrapDigest :: DomainAlignment.BootstrapEvidenceDigest
bootstrapDigest = DomainAlignment.deriveBootstrapEvidenceDigest "bootstrap-prefix"

routeCutover :: Alignment.AlignmentRouteCutoverMarker
routeCutover =
  checked
    "route cutover"
    (Alignment.alignmentRouteCutoverMarker planId generation heraldA heraldB)

obligationId :: Alignment.AlignmentObligationId
obligationId = Alignment.alignmentObligationId heraldA obligationSequence

subscriptionId :: Alignment.AlignmentSubscriptionId
subscriptionId = Alignment.alignmentSubscriptionId heraldA subscriptionSequence

obligationSequence :: Alignment.AlignmentObligationSequence
obligationSequence = Alignment.firstAlignmentObligationSequence

subscriptionSequence :: Alignment.AlignmentSubscriptionSequence
subscriptionSequence = Alignment.firstAlignmentSubscriptionSequence

evidenceSequence :: Alignment.AlignmentEvidenceSequence
evidenceSequence = Alignment.firstAlignmentEvidenceSequence

cause :: Identity.StructuralOccurrenceId
cause =
  Identity.structuralOccurrenceId
    heraldA
    (checked "structural sequence" (Identity.mkStructuralSequence 1))

publicationA :: Identity.PublicationId
publicationA =
  Identity.publicationId
    nablaId
    Identity.genesisAuthorityEpoch
    heraldA
    (Identity.nablaSequence 1)

publicationB :: Identity.PublicationId
publicationB =
  Identity.publicationId
    nablaId
    Identity.genesisAuthorityEpoch
    heraldB
    (Identity.nablaSequence 2)

canonicalValue :: Value.CanonicalValueBytes
canonicalValue = Value.canonicalValueBytes (Value.boolValue True)

heraldA, heraldB :: Identity.HeraldEpoch
heraldA = identity "herald A" Identity.mkHeraldEpoch 0x11
heraldB = identity "herald B" Identity.mkHeraldEpoch 0x12

nablaId :: Identity.NablaId
nablaId = identity "nabla" Identity.mkNablaId 0x21

deltaId :: Identity.DeltaId
deltaId = identity "delta" Identity.mkDeltaId 0x22

processEpoch :: Identity.ProcessEpochId
processEpoch = identity "process" Identity.mkProcessEpochId 0x23

sortId :: Identity.SortId
sortId = identity "sort" Identity.mkSortId 0x31

sortOccurrence :: Identity.SortDefinitionOccurrenceId
sortOccurrence = identity "sort occurrence" Identity.mkSortDefinitionOccurrenceId 0x32

storeIncarnation :: Identity.StoreIncarnationId
storeIncarnation = identity "store incarnation" Identity.mkStoreIncarnationId 0x41

systemId :: Identity.SystemId
systemId = identity "system" Identity.mkSystemId 0x42

predecessorTopologyCutId :: Identity.TopologyCutId
predecessorTopologyCutId = identity "topology predecessor" Identity.mkTopologyCutId 0x51

identity ::
  (Show error) =>
  String ->
  (ByteString.ByteString -> Either error value) ->
  Word ->
  value
identity label constructor marker =
  checked label (constructor (fixtureIdentifierBytes (fromIntegral marker)))

checked :: (Show error) => String -> Either error value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
