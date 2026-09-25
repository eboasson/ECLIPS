{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Step16DisappearanceProperties
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.List (isInfixOf, permutations, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word16, Word64, Word8)
import DisappearanceReferenceDriver qualified as Target
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (HeraldPublicationPrefixThrough),
    mkHeraldPublicationPosition,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceOpenResultView (AliasedDisappearanceProbe, OpenedDisappearanceProbe),
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    DisappearanceSubjectView (..),
    deriveDisappearanceProbeId,
    disappearanceEvidenceClaimCanonicalBytes,
    disappearanceOpenResultView,
    disappearanceResolutionOutcomeView,
    disappearanceResolutionSubject,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
    resolveDisappearanceSubject,
    reviseControlledDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    controlIndex,
    mkGlobalObjectId,
    mkLabelDecisionId,
  )
import Eclips.Domain.Label (mkLabelRevision)
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    FailureProbeResolutionId,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    retireHeraldMembershipGeneration,
  )
import Eclips.Herald.Alignment.Protocol
  ( AlignmentSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
  )
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.OracleClient.Request qualified as OracleRequest
import Eclips.Herald.PeerStream
  ( ReceiveDisposition (..),
    SequencedItem,
    emptyStreamPrefix,
    receiveResultDisposition,
    receiveResultWatermarks,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamDirectionDestination,
    streamDirectionSource,
    streamPrefixThrough,
    streamReceivedPrefix,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceUseCase
import Step16ProspectiveFixtures
  ( checked,
    fixtureAlignmentSubscription,
    fixtureAttestation,
    fixtureAttestationEntries,
    fixtureBytes,
    fixtureControlledSubject,
    fixtureEvidenceDigest,
    fixtureEvidenceSnapshot,
    fixtureH1,
    fixtureH2,
    fixtureH3,
    fixtureH4,
    fixtureMembers,
    fixtureMembership,
    fixturePublicationPrefix,
    fixtureRegularSubject,
    fixtureRequestId,
    fixtureStoreRevision,
    fixtureStreamDirection,
    fixtureSystemId,
  )
import Step16ProspectiveHarness
  ( ProspectiveOracle,
    ProspectiveRequestDisposition (..),
    ProspectiveRequestProblem (..),
    ProspectiveRequestRelease (..),
    ProspectiveRequestSettlement (..),
    commitProspectiveHerald,
    committedTargetEvents,
    heldMarkerCount,
    heldMarkerProbeIds,
    initialProspectiveHerald,
    initialProspectiveOracle,
    initialProspectiveRequestLedger,
    projectedClaimFromReport,
    projectedProbeFromOpen,
    prospectiveHeraldPublicationStream,
    prospectiveHeraldState,
    prospectiveOracleState,
    prospectiveOutstandingRequestCount,
    prospectiveRequestCount,
    prospectiveRequestDispositionRef,
    prospectiveRequestSubmissionOutcome,
    prospectiveTerminalFromSubmission,
    receiveAlignmentMarker,
    receivePublicationMarker,
    redriveMarkersAfterOpen,
    releaseProspectiveOpenRequest,
    retainInvalidationRequest,
    retainOpenRequest,
    retainReportRequest,
    retainResolveRequest,
    settleProspectiveRequest,
    submitInvalidationIntention,
    submitLocalReport,
    submitOpenIntention,
    submitResolveIntention,
    submitRetainedInvalidationRequest,
    submitRetainedOpenRequest,
    submitRetainedReportRequest,
    submitRetainedResolveRequest,
    submitTargetCommand,
  )
import Step16ProspectiveProjection qualified as Projection
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
    counterexample,
    testProperty,
    (===),
  )

-- The concrete schedules are added against the private State/Protocol API in
-- the same increment.  Keeping the component separate ensures the temporary
-- prospective Oracle dependency cannot leak into the ordinary Herald suite.
tests :: TestTree
tests =
  testGroup
    "Step-16 prospective Herald disappearance"
    [ testGroup
        "private protocol vocabulary"
        [ testCase
            "the deterministic fixtures satisfy the checked domain shapes"
            caseFixtureValidity,
          testCase
            "local-take is the explicit candidate observation"
            caseLocalTakeCandidateObservation,
          testCase
            "projected probes reject a mismatched subject coordinate"
            caseProjectedProbeCoordinate,
          testCase
            "publication markers bind their assigned direction and sequence"
            casePublicationMarkerSequenceBinding,
          testCase
            "alignment markers bind the exact subscription revision"
            caseAlignmentMarkerBinding,
          testCase
            "local report canonicalization ignores marker arrival order"
            caseLocalReportCanonicalPermutation,
          testCase
            "absence attestations enforce each subject arm's exact checked class set"
            caseAbsenceAttestationAdmission,
          testCase
            "completed alignment evidence binds its authenticated source"
            caseCompletedAlignmentEvidenceSourceBinding,
          testCase
            "the test-only Target adapter opens the exact private intention"
            caseTargetAdapterOpen,
          testCase
            "the projection translator covers every command and compound context arm"
            caseProjectionTranslatorCoverage
        ],
      testGroup
        "candidate and projection scheduling"
        [ testCase
            "only local take creates a candidate and stable tickets are inert"
            caseCandidateScheduling,
          testCase
            "one local candidate projects work to every captured Herald"
            caseAllMemberProjection,
          testCase
            "a local take after a peer-home Open joins the existing probe"
            caseLateLocalTakeJoinsProjectedProbe,
          testCase
            "publication and alignment markers outrunning Open are held and redriven"
            caseMarkersBeforeOpen,
          testCase
            "prospective Oracle intentions bind, settle, and retain stable request references"
            caseProspectiveRequestOwnership,
          testCase
            "racing Resolve requests settle but only the first carries terminal authority"
            caseProspectiveResolveRequestRace,
          testCase
            "projected Open atomically installs the leaf-owned matching-write gate"
            caseMatchingWriteGateOwnership,
          testCase
            "one exact coordinate excludes competitors and terminal release requires settled authority"
            caseLeafCoordinateExclusion
        ],
      testGroup
        "cut readiness and invalidation"
        [ testCase
            "every publication and alignment cut is required before Report"
            caseCutReadiness,
          testCase
            "destination marker completion waits for contiguous peer-stream completion"
            casePublicationMarkerPeerStreamOrdering,
          testCase
            "repair replay preserves marker identity and cannot duplicate Report"
            caseMarkerRepairReplay,
          testCase
            "a marker first received after terminalization drains as obsolete"
            caseMarkerAfterTerminal,
          testCase
            "alignment marker repair replay preserves identity and charges work once"
            caseAlignmentMarkerRepairReplay,
          testCase
            "every alignment subscription mutation retains one invalidation"
            caseAlignmentMutations,
          testCase
            "common and regular-only blocker classes obey their subject boundaries"
            caseBlockerClasses,
          testCase
            "owner evidence snapshots charge exact fact deltas and replay is inert"
            caseEvidenceObservationWorkAccounting,
          testCase
            "only gate-qualified strictly post-cut matching work invalidates"
            caseMatchingWork,
          testCase
            "a post-Report contradiction retains history and traverses Target Invalidate"
            caseReportThenInvalidate
        ],
      testGroup
        "composed prospective vertical"
        [ testCase
            "Report permutations converge at every Herald and the Target"
            caseReportProjectionPermutations,
          testCase
            "Target Open and Report events alone drive a peer leaf to Resolve"
            caseSelfSufficientTargetProjection,
          testCase
            "both subject arms traverse Open, Report, Resolve, and terminal replay"
            caseBothSubjectArms,
          testCase
            "regular Resolve yields one exact retirement authority and replay yields none"
            caseRegularResolutionAuthority,
          testCase
            "terminal candidate policy parks, reconstructs, or consumes exactly as specified"
            caseTerminalCandidatePolicy,
          testCase
            "label terminals consume or rekey only the exact invalidated candidate"
            caseLabelTerminalCandidateFollowThrough,
          testCase
            "the successful three-Herald schedule exactly meets its work formula"
            caseSuccessfulWorkBound,
          testCase
            "three racing opener intentions and alias projections reach the worst-case bound"
            caseRacingOpenersWorkBound,
          testProperty
            "the symbolic work formula keeps each dimension independent"
            propSymbolicWorkBound,
          testProperty
            "generated marker/report permutations and repair replays converge"
            propGeneratedCutTrace,
          testProperty
            "generated remote Reports interleave with local cut completion and converge"
            propGeneratedCutReportInterleaving
        ],
      testCase
        "empty, irrelevant, and disconnected control observations do zero work"
        caseZeroWorkControls
    ]

caseFixtureValidity :: Assertion
caseFixtureValidity = do
  assertEqual
    "membership contains exactly the three fixture Heralds"
    (sort fixtureMembers)
    ( sort
        ( toList
            (heraldMembershipGenerationActiveHeraldEpochs fixtureMembership)
        )
    )
  assertBool
    "the two fixture subject arms are distinct"
    (fixtureControlledSubject /= fixtureRegularSubject)
  where
    toList (first :| rest) = first : rest

caseLocalTakeCandidateObservation :: Assertion
caseLocalTakeCandidateObservation = do
  let observation =
        Protocol.localTakeCandidateObservation fixtureControlledSubject
  assertEqual
    "the candidate retains its exact disappearance subject"
    fixtureControlledSubject
    (Protocol.disappearanceCandidateSubject observation)
  assertEqual
    "the Open intention uses the current membership coordinate"
    ( Protocol.disappearanceOpenIntention
        fixtureControlledSubject
        fixtureMembership
    )
    ( Protocol.disappearanceOpenIntention
        (Protocol.disappearanceCandidateSubject observation)
        fixtureMembership
    )

caseProjectedProbeCoordinate :: Assertion
caseProjectedProbeCoordinate = do
  let probe = fixtureProbe
      coordinate = fixtureCoordinate
      wrongCoordinate =
        disappearanceSubjectMembershipCoordinate
          fixtureRegularSubject
          fixtureMembership
  projected <-
    pure
      ( checked
          "projected fixture probe"
          ( Protocol.projectedDisappearanceProbe
              probe
              fixtureControlledSubject
              coordinate
              fixtureMembership
          )
      )
  assertEqual
    "the checked projection retains the exact probe"
    probe
    (Protocol.projectedProbeId projected)
  assertEqual
    "the checked projection retains every captured member"
    (sort fixtureMembers)
    (sort (toList (Protocol.projectedProbeMembers projected)))
  assertBool
    "a coordinate for another subject is rejected"
    ( case Protocol.projectedDisappearanceProbe
        probe
        fixtureControlledSubject
        wrongCoordinate
        fixtureMembership of
        Left Protocol.DisappearanceProjectedCoordinateMismatch {} -> True
        _ -> False
    )
  where
    toList (first :| rest) = first : rest

casePublicationMarkerSequenceBinding :: Assertion
casePublicationMarkerSequenceBinding = do
  let probe = fixtureProbe
      coordinate = fixtureCoordinate
      source0 = PeerStream.initialState fixtureH1
      prepared =
        checked
          "sequence-bound publication marker"
          ( PeerStream.prepareSequenceBoundEnqueue
              ( Map.singleton
                  fixtureH2
                  (Protocol.disappearancePublicationMarkerItem probe coordinate :| [])
              )
              source0
          )
      (source1, assignments) = PeerStream.commitEnqueue prepared
      assigned = case Map.lookup fixtureH2 assignments of
        Just (item :| []) -> item
        _ -> error "one destination did not receive exactly one marker"
      marker = sequencedItemPayload assigned
      replayPrepared =
        checked
          "second sequence-bound publication marker"
          ( PeerStream.prepareSequenceBoundEnqueue
              ( Map.singleton
                  fixtureH2
                  (Protocol.disappearancePublicationMarkerItem probe coordinate :| [])
              )
              source1
          )
      (_, replayAssignments) = PeerStream.commitEnqueue replayPrepared
      replayMarker = case Map.lookup fixtureH2 replayAssignments of
        Just (item :| []) -> sequencedItemPayload item
        _ -> error "one destination did not receive exactly one replay marker"
  assertEqual
    "the marker is bound to the source/destination stream"
    fixtureH1
    ( streamDirectionSource
        (Protocol.disappearancePublicationMarkerDirection marker)
    )
  assertEqual
    "the marker is bound to the requested destination"
    fixtureH2
    ( streamDirectionDestination
        (Protocol.disappearancePublicationMarkerDirection marker)
    )
  assertBool
    "a later assignment changes the marker's canonical identity"
    ( Protocol.disappearancePublicationMarkerCanonicalBytes marker
        /= Protocol.disappearancePublicationMarkerCanonicalBytes replayMarker
    )
  assertEqual
    "the payload digest is the sequence-bound PeerStream digest"
    (Protocol.disappearancePublicationMarkerDigest marker)
    (sequencedItemDigest assigned)

caseAlignmentMarkerBinding :: Assertion
caseAlignmentMarkerBinding = do
  let probe = fixtureProbe
      subscription = fixtureAlignmentSubscription fixtureH2 1
      marker =
        Protocol.disappearanceAlignmentMarker
          probe
          (Protocol.outgoingAlignmentCut subscription (fixtureStoreRevision 7))
      later =
        Protocol.disappearanceAlignmentMarker
          probe
          (Protocol.outgoingAlignmentCut subscription (fixtureStoreRevision 8))
  assertEqual
    "the marker retains the exact probe"
    probe
    (Protocol.disappearanceAlignmentMarkerProbe marker)
  assertEqual
    "the marker retains the exact subscription"
    subscription
    (Protocol.disappearanceAlignmentMarkerSubscription marker)
  assertEqual
    "the marker retains the source cut revision"
    (fixtureStoreRevision 7)
    (Protocol.disappearanceAlignmentMarkerThroughRevision marker)
  assertBool
    "a changed revision changes canonical identity"
    ( Protocol.disappearanceAlignmentMarkerCanonicalBytes marker
        /= Protocol.disappearanceAlignmentMarkerCanonicalBytes later
    )
  assertBool
    "an unrelated fixture Herald remains distinct"
    (fixtureH3 /= fixtureH2)

caseLocalReportCanonicalPermutation :: Assertion
caseLocalReportCanonicalPermutation = do
  let projected = fixtureProjectedProbe
      publicationMarkers =
        [ assignedPublicationMarker fixtureH2 fixtureH1,
          assignedPublicationMarker fixtureH3 fixtureH1
        ]
      alignmentEvidence =
        [ fixtureCompletedAlignmentEvidence
            projected
            fixtureH2
            (fixtureAlignmentSubscription fixtureH1 1)
            3,
          fixtureCompletedAlignmentEvidence
            projected
            fixtureH3
            (fixtureAlignmentSubscription fixtureH1 2)
            5
        ]
      attestation = fixtureAttestation fixtureControlledSubject
      reports =
        [ checked
            "permuted local report"
            ( Protocol.localAbsenceReportForOwner
                projected
                fixtureH1
                fixturePublicationPrefix
                publications
                alignments
                attestation
            )
        | publications <- permutations publicationMarkers,
          alignments <- permutations alignmentEvidence
        ]
  case reports of
    [] -> error "fixed non-empty permutation set disappeared"
    first : remaining -> do
      mapM_
        ( \report ->
            assertEqual
              "all arrival orders normalize to one canonical report"
              (Protocol.localAbsenceReportCanonicalBytes first)
              (Protocol.localAbsenceReportCanonicalBytes report)
        )
        remaining
      mapM_
        ( \report ->
            assertEqual
              "all arrival orders derive one evidence claim"
              (Protocol.localAbsenceReportClaim first)
              (Protocol.localAbsenceReportClaim report)
        )
        remaining

caseAbsenceAttestationAdmission :: Assertion
caseAbsenceAttestationAdmission = do
  forM_ [fixtureControlledSubject, fixtureRegularSubject] $ \subject -> do
    let entries = fixtureAttestationEntries subject
        admitted = fixtureAttestation subject
    assertEqual
      ("attestation retains its exact subject arm: " <> show subject)
      subject
      (Protocol.disappearanceAbsenceAttestationSubject admitted)
    forM_ entries $ \removed ->
      assertBool
        ("missing required class is rejected: " <> show (fst removed))
        ( case Protocol.disappearanceAbsenceAttestation subject (filter (/= removed) entries) of
            Left Protocol.DisappearanceAttestationClassSetMismatch {} -> True
            _ -> False
        )
    case entries of
      first : _ ->
        assertEqual
          "a duplicate class is rejected before normalization"
          (Left (Protocol.DisappearanceAttestationDuplicateClass (fst first)))
          (Protocol.disappearanceAbsenceAttestation subject (first : entries))
      [] -> assertFailure "each subject arm must require at least one attestation class"
    let canonicalForms =
          [ Protocol.disappearanceAbsenceAttestationCanonicalBytes
              (checked "permuted absence attestation" (Protocol.disappearanceAbsenceAttestation subject permutation))
          | permutation <- permutations entries
          ]
    assertBool
      "all admitted class permutations have one canonical identity"
      (all (== headCanonical canonicalForms) canonicalForms)
  let regularAttestation = fixtureAttestation fixtureRegularSubject
      controlledWithExtra =
        Protocol.disappearanceAbsenceAttestation
          fixtureControlledSubject
          ( fixtureAttestationEntries fixtureControlledSubject
              <> [(Protocol.GraphDependencyAbsence, fixtureEvidenceDigest 199)]
          )
  assertBool
    "an otherwise complete controlled attestation rejects a regular-only extra class"
    (case controlledWithExtra of Left Protocol.DisappearanceAttestationClassSetMismatch {} -> True; _ -> False)
  assertEqual
    "a report cannot reuse an attestation for the other subject arm"
    (Left Protocol.DisappearanceAttestationSubjectMismatch)
    ( Protocol.localAbsenceReportForOwner
        fixtureProjectedProbe
        fixtureH1
        fixturePublicationPrefix
        []
        []
        regularAttestation
    )
  where
    headCanonical (first : _) = first
    headCanonical [] = error "attestation permutation set unexpectedly empty"

caseCompletedAlignmentEvidenceSourceBinding :: Assertion
caseCompletedAlignmentEvidenceSourceBinding = do
  let subscription = fixtureAlignmentSubscription fixtureH1 77
      marker =
        Protocol.disappearanceAlignmentMarker
          fixtureProbe
          (Protocol.outgoingAlignmentCut subscription (fixtureStoreRevision 17))
      evidenceH2 =
        fixtureCompletedAlignmentEvidence
          fixtureProjectedProbe
          fixtureH2
          subscription
          17
      evidenceH3 =
        fixtureCompletedAlignmentEvidence
          fixtureProjectedProbe
          fixtureH3
          subscription
          17
      mkReport evidence =
        checked
          "source-qualified local report"
          ( Protocol.localAbsenceReportForOwner
              fixtureProjectedProbe
              fixtureH1
              fixturePublicationPrefix
              []
              [evidence]
              (fixtureAttestation fixtureControlledSubject)
          )
  assertEqual
    "the normalized alignment evidence retains the authenticated source"
    fixtureH2
    (Protocol.completedIncomingAlignmentEvidenceSource evidenceH2)
  assertBool
    "changing only the authenticated source changes alignment evidence identity"
    ( Protocol.completedIncomingAlignmentEvidenceCanonicalBytes evidenceH2
        /= Protocol.completedIncomingAlignmentEvidenceCanonicalBytes evidenceH3
    )
  assertBool
    "changing only the authenticated source changes the report claim"
    ( Protocol.localAbsenceReportClaim (mkReport evidenceH2)
        /= Protocol.localAbsenceReportClaim (mkReport evidenceH3)
    )
  assertEqual
    "checked evidence rejects a source outside the captured member set"
    (Left Protocol.DisappearanceAlignmentEvidenceMemberMismatch)
    ( Protocol.completedIncomingAlignmentEvidenceForOwner
        fixtureProjectedProbe
        (Protocol.incomingAlignmentCut subscription fixtureH4)
        marker
        (fixtureStoreRevision 17)
    )
  let network = prepareCutNetwork fixtureControlledSubject
      state = lookupMap "source-bound receipt state" fixtureH1 (cutStates network)
      (_, _, capturedMarker) = case [ edge
                                    | edge@(_, destination, _) <- cutAlignmentMarkers network,
                                      destination == fixtureH1
                                    ] of
        [edge] -> edge
        other -> error ("expected one captured H1 alignment marker, got " <> show other)
  assertEqual
    "the leaf rejects a marker presented under the wrong authenticated source"
    (Left (Disappearance.DisappearanceAlignmentSourceNotCaptured fixtureH2))
    ( Disappearance.preparedDisappearanceOutput
        <$> Disappearance.prepareAlignmentMarkerReceipt fixtureH2 capturedMarker state
    )

caseTargetAdapterOpen :: Assertion
caseTargetAdapterOpen = do
  let oracle0 =
        initialProspectiveOracle
          fixtureSystemId
          fixtureMembership
      intention =
        Protocol.disappearanceOpenIntention
          fixtureControlledSubject
          fixtureMembership
      (oracle1, outcome) =
        submitOpenIntention
          (fixtureRequestId fixtureH1 1)
          (Just (controlIndex 0))
          fixtureH1
          intention
          oracle0
  assertEqual
    "the adapter leaves Target on the fixture membership"
    fixtureMembership
    (Target.targetCurrentMembership (prospectiveOracleState oracle1))
  case outcome of
    Target.TargetSubmissionCommitted receipt [event@(Target.TargetOpenProjected header result)] -> do
      assertEqual
        "the Target commits the first command at index one"
        (controlIndex 1)
        (Target.targetReceiptControlIndex receipt)
      case disappearanceOpenResultView result of
        OpenedDisappearanceProbe probe ->
          assertEqual "the projected Open uses the index-derived probe" fixtureProbe probe
        _ -> error "the first Open unexpectedly aliased"
      assertEqual
        "the event itself carries the complete projected probe"
        (cutProjectedProbe (prepareCutNetwork fixtureControlledSubject))
        (checked "translate self-sufficient Open event" (projectedProbeFromOpen outcome))
      assertEqual "the event carries the exact subject" fixtureControlledSubject (Target.targetProjectedProbeHeaderSubject header)
      assertEqual "the event carries the exact coordinate" fixtureCoordinate (Target.targetProjectedProbeHeaderCoordinate header)
      assertEqual "the event carries the captured membership body" fixtureMembership (Target.targetProjectedProbeHeaderMembership header)
      assertEqual
        "the adapter exposes the committed event vector"
        [event]
        (committedTargetEvents outcome)
    _ -> error ("the Target adapter rejected a valid Open: " <> show outcome)

caseProjectionTranslatorCoverage :: Assertion
caseProjectionTranslatorCoverage = do
  let network = prepareCutNetwork fixtureControlledSubject
      openOutcome = case cutOpenOutcomes network of
        [outcome] -> outcome
        outcomes -> error ("expected one translator Open, got " <> show outcomes)
      (reportedStates, reports) =
        completeNetworkEvidence fixtureMembers network
      h1Report = lookupMap "translator H1 Report" fixtureH1 reports
      (_, reportOutcome) =
        submitLocalReport
          (fixtureRequestId fixtureH1 210)
          Nothing
          fixtureH1
          h1Report
          (cutOracle network)
      invalidation =
        Protocol.disappearanceInvalidationIntention
          fixtureProbe
          fixtureH1
          Protocol.LocalEvidenceContradicted
          (fixtureEvidenceDigest 211)
      (_, invalidationOutcome) =
        submitInvalidationIntention
          (fixtureRequestId fixtureH1 211)
          Nothing
          invalidation
          (cutOracle network)
      (_, abortOutcome) =
        submitTargetCommand
          (fixtureRequestId fixtureH1 212)
          Nothing
          fixtureH1
          (Target.targetAbortCommand fixtureProbe Target.targetAuthorizedAbortReason)
          (cutOracle network)
      projectedStates =
        projectClaimsEverywhere fixtureMembers reports reportedStates
      resolve = case Disappearance.resolveIntentions (lookupMap "translator Resolve" fixtureH1 projectedStates) of
        [intention] -> intention
        other -> error ("expected one translator Resolve, got " <> show other)
      (reportedOracle, _) =
        submitReportsToOracle fixtureMembers reports (cutOracle network)
      (_, resolveOutcome) =
        submitResolveIntention
          (fixtureRequestId fixtureH1 213)
          Nothing
          fixtureH1
          resolve
          reportedOracle

  case Projection.translateTargetSubmissionOutcome openOutcome of
    Right [Projection.DisappearanceProjectedOpen index projected] -> do
      assertEqual "translated Open retains its enclosing index" (controlIndex 1) index
      assertEqual "translated Open retains its complete header" (cutProjectedProbe network) projected
    translated -> assertFailure ("Open translation shape changed: " <> show translated)
  case Projection.translateTargetSubmissionOutcome reportOutcome of
    Right [Projection.DisappearanceProjectedReport index claim] -> do
      assertEqual "translated Report retains its enclosing index" (controlIndex 2) index
      assertEqual "translated Report retains the complete claim" (Protocol.localAbsenceReportClaim h1Report) claim
    translated -> assertFailure ("Report translation shape changed: " <> show translated)
  case Projection.translateTargetSubmissionOutcome invalidationOutcome of
    Right [Projection.DisappearanceProjectedTerminal index (Protocol.ProjectedDisappearanceInvalidated probe cause terminalIndex)] -> do
      assertEqual "translated Invalidate retains its probe" fixtureProbe probe
      assertEqual "translated Invalidate uses one enclosing index" index terminalIndex
      assertEqual
        "translated Invalidate retains the exact checked cause"
        ( Protocol.ProjectedCommandInvalidation
            fixtureH1
            Protocol.LocalEvidenceContradicted
            (fixtureEvidenceDigest 211)
        )
        cause
    translated -> assertFailure ("Invalidate translation shape changed: " <> show translated)
  case Projection.translateTargetSubmissionOutcome resolveOutcome of
    Right [Projection.DisappearanceProjectedTerminal index (Protocol.ProjectedDisappearanceResolved probe outcome terminalIndex)] -> do
      assertEqual "translated Resolve retains its probe" fixtureProbe probe
      assertEqual "translated Resolve uses one enclosing index" index terminalIndex
      assertEqual "translated Resolve retains its subject" fixtureControlledSubject (disappearanceResolutionSubject outcome)
    translated -> assertFailure ("Resolve translation shape changed: " <> show translated)
  case Projection.translateTargetSubmissionOutcome abortOutcome of
    Right [Projection.DisappearanceProjectedTerminal index (Protocol.ProjectedDisappearanceAborted probe Protocol.ProjectedAuthorizedAbort terminalIndex)] -> do
      assertEqual "translated Abort retains its probe" fixtureProbe probe
      assertEqual "translated Abort uses one enclosing index" index terminalIndex
    translated -> assertFailure ("Abort translation shape changed: " <> show translated)

  case openOutcome of
    Target.TargetSubmissionCommitted receipt _ ->
      assertEqual
        "an accepted result without its event cannot become leaf authority"
        (Left Projection.TargetProjectionEventless)
        ( Projection.translateTargetSubmissionOutcome
            (Target.TargetSubmissionCommitted receipt [])
        )
    _ -> assertFailure "translator Open fixture did not commit"

  let successor =
        checked
          "translator membership successor"
          (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution 2) fixtureH3 fixtureMembership)
      (_, membershipOutcome) =
        Target.applyTargetContext
          (Target.targetMembershipSuccessorInput successor)
          (prospectiveOracleState (cutOracle network))
  case Projection.translateTargetContextOutcome membershipOutcome of
    Right [Projection.DisappearanceProjectedTerminal index (Protocol.ProjectedDisappearanceAborted probe (Protocol.ProjectedMembershipSuperseded projectedSuccessor) terminalIndex)] -> do
      assertEqual "membership translation aborts the collecting probe" fixtureProbe probe
      assertEqual "membership translation retains the complete successor" successor projectedSuccessor
      assertEqual "membership translation uses the retirement index" (controlIndex 2) index
      assertEqual "membership terminal uses the same index" index terminalIndex
    translated -> assertFailure ("membership translation shape changed: " <> show translated)

  let object = case disappearanceSubjectView fixtureControlledSubject of
        ControlledPredefinedSubjectView retained _ _ -> retained
        RegularSortDefinitionSubjectView {} -> error "translator controlled fixture changed arm"
      invalidatingDecision =
        checked "translator invalidating label decision" (mkLabelDecisionId (fixtureBytes 214))
      (_, labelOpenOutcome) =
        Target.applyTargetContext
          (Target.targetLabelOpenInput fixtureControlledSubject invalidatingDecision)
          (prospectiveOracleState (cutOracle network))
  case Projection.translateTargetContextOutcome labelOpenOutcome of
    Right [Projection.DisappearanceProjectedTerminal index (Protocol.ProjectedDisappearanceInvalidated probe (Protocol.ProjectedLabelInvalidation decision projectedObject) terminalIndex)] -> do
      assertEqual "label-Open translation invalidates the collecting probe" fixtureProbe probe
      assertEqual "label-Open translation retains the decision" invalidatingDecision decision
      assertEqual "label-Open translation binds the subject object" object projectedObject
      assertEqual "label-Open terminal uses the enclosing index" index terminalIndex
    translated -> assertFailure ("label-Open translation shape changed: " <> show translated)

  forM_
    [ (Target.TargetLabelNotApplied, Protocol.ProjectedLabelNotApplied),
      ( Target.TargetLabelReleased (labelRevisionAt 2),
        Protocol.ProjectedLabelReleased (labelRevisionAt 2)
      ),
      ( Target.TargetLabelDeleted (labelRevisionAt 2),
        Protocol.ProjectedLabelDeleted (labelRevisionAt 2)
      )
    ]
    $ \(targetTerminal, expectedTerminal) -> do
      let decision =
            checked
              "translator terminal label decision"
              (mkLabelDecisionId (fixtureBytes (220 + fromIntegral (fromEnumTerminal targetTerminal))))
          initial = Target.initialTargetState fixtureSystemId fixtureMembership
          (opened, openedOutcome) =
            Target.applyTargetContext
              (Target.targetLabelOpenInput fixtureControlledSubject decision)
              initial
          (_, terminalOutcome) =
            Target.applyTargetContext
              (Target.targetLabelTerminalInput decision targetTerminal)
              opened
      assertEqual
        "a context-only label Open emits no disappearance leaf input"
        (Right [])
        (Projection.translateTargetContextOutcome openedOutcome)
      case Projection.translateTargetContextOutcome terminalOutcome of
        Right [Projection.DisappearanceProjectedLabelTerminal index terminal] -> do
          assertEqual "label terminal retains its enclosing index" (controlIndex 2) index
          assertEqual "label terminal retains its decision" decision (Protocol.projectedLabelTerminalDecision terminal)
          assertEqual "label terminal retains its subject object" object (Protocol.projectedLabelTerminalObject terminal)
          assertEqual "label terminal retains its exact arm" expectedTerminal (Protocol.projectedLabelTerminalOutcome terminal)
        translated -> assertFailure ("label-terminal translation shape changed: " <> show translated)
  where
    labelRevisionAt ordinal =
      checked "translator label revision" (mkLabelRevision (controlIndex ordinal))
    fromEnumTerminal terminal = case terminal of
      Target.TargetLabelNotApplied -> 0 :: Int
      Target.TargetLabelReleased {} -> 1
      Target.TargetLabelDeleted {} -> 2

fixtureProbe :: DisappearanceProbeId
fixtureProbe =
  checked
    "fixture disappearance probe"
    (deriveDisappearanceProbeId (controlIndex 1))

fixtureCoordinate :: DisappearanceSubjectMembershipCoordinate
fixtureCoordinate =
  disappearanceSubjectMembershipCoordinate
    fixtureControlledSubject
    fixtureMembership

fixtureProjectedProbe :: Protocol.ProjectedDisappearanceProbe
fixtureProjectedProbe =
  checked
    "fixture projected disappearance probe"
    ( Protocol.projectedDisappearanceProbe
        fixtureProbe
        fixtureControlledSubject
        fixtureCoordinate
        fixtureMembership
    )

fixtureCompletedAlignmentEvidence ::
  Protocol.ProjectedDisappearanceProbe ->
  HeraldEpoch ->
  AlignmentSubscriptionId ->
  Word64 ->
  Protocol.CompletedIncomingAlignmentEvidence
fixtureCompletedAlignmentEvidence projected source subscription revision =
  checked
    "fixture completed incoming alignment evidence"
    ( Protocol.completedIncomingAlignmentEvidenceForOwner
        projected
        (Protocol.incomingAlignmentCut subscription source)
        ( Protocol.disappearanceAlignmentMarker
            (Protocol.projectedProbeId projected)
            ( Protocol.outgoingAlignmentCut
                subscription
                (fixtureStoreRevision revision)
            )
        )
        (fixtureStoreRevision revision)
    )

assignedPublicationMarker ::
  HeraldEpoch ->
  HeraldEpoch ->
  Protocol.DisappearancePublicationMarker
assignedPublicationMarker source destination =
  sequencedItemPayload (assignedPublicationItem source destination)

assignedPublicationItem ::
  HeraldEpoch ->
  HeraldEpoch ->
  SequencedItem Protocol.DisappearancePublicationMarker
assignedPublicationItem source destination =
  singleAssignment destination assignments
  where
    prepared =
      checked
        "assigned publication marker"
        ( PeerStream.prepareSequenceBoundEnqueue
            ( Map.singleton
                destination
                ( Protocol.disappearancePublicationMarkerItem
                    fixtureProbe
                    fixtureCoordinate
                    :| []
                )
            )
            (PeerStream.initialState source)
        )
    (_, assignments) = PeerStream.commitEnqueue prepared

singleAssignment ::
  (Ord key, Show key) =>
  key ->
  Map.Map key (NonEmpty (SequencedItem payload)) ->
  SequencedItem payload
singleAssignment key assignments = case Map.lookup key assignments of
  Just (item :| []) -> item
  other -> error ("expected one assignment for " <> show key <> ", got " <> show (fmap length other))

data CutNetwork = CutNetwork
  { cutProjectedProbe :: Protocol.ProjectedDisappearanceProbe,
    cutOracle :: ProspectiveOracle,
    cutOpenOutcomes :: [Target.TargetSubmissionOutcome],
    cutStates :: Map HeraldEpoch Disappearance.State,
    cutPeerStreams :: Map HeraldEpoch (PeerStream.State Protocol.DisappearancePublicationMarker),
    cutPublicationItems :: Map (HeraldEpoch, HeraldEpoch) (SequencedItem Protocol.DisappearancePublicationMarker),
    cutPublicationMarkers :: Map (HeraldEpoch, HeraldEpoch) Protocol.DisappearancePublicationMarker,
    cutAlignmentMarkers :: [(HeraldEpoch, HeraldEpoch, Protocol.DisappearanceAlignmentMarker)]
  }

prepareCutNetwork :: DisappearanceSubject -> CutNetwork
prepareCutNetwork subject = prepareCutNetworkWithOpeners subject Map.empty [fixtureH1]

prepareCutNetworkWithBlockers ::
  DisappearanceSubject ->
  Map HeraldEpoch [Protocol.DisappearanceBlockerWitness] ->
  CutNetwork
prepareCutNetworkWithBlockers subject blockersByHerald =
  prepareCutNetworkWithOpeners subject blockersByHerald [fixtureH1]

prepareCutNetworkWithOpeners ::
  DisappearanceSubject ->
  Map HeraldEpoch [Protocol.DisappearanceBlockerWitness] ->
  [HeraldEpoch] ->
  CutNetwork
prepareCutNetworkWithOpeners subject blockersByHerald openers =
  CutNetwork
    { cutProjectedProbe = projected,
      cutOracle = oracle1,
      cutOpenOutcomes = openOutcomes,
      cutStates = assignedStates,
      cutPeerStreams = sourceStreams,
      cutPublicationItems = publicationItems,
      cutPublicationMarkers = publicationMarkers,
      cutAlignmentMarkers = alignmentMarkers
    }
  where
    initialStates = Map.fromList [(local, Disappearance.initialState local) | local <- fixtureMembers]
    candidateStates = foldl prepareOpener initialStates openers
    prepareOpener states local =
      let initial = lookupMap "candidate opener state" local states
          (candidate, _) =
            commitExpected
              "create local candidate"
              Disappearance.CandidateCreated
              ( Disappearance.prepareLocalTakeCandidate
                  (Protocol.localTakeCandidateObservation subject)
                  initial
              )
          (opened, _) =
            commitExpected
              "retain local Open intention"
              Disappearance.CandidateOpenPrepared
              ( Disappearance.prepareCandidateReevaluation
                  (Protocol.disappearanceOpenContext fixtureMembership)
                  subject
                  candidate
              )
       in Map.insert local opened states
    oracle0 = initialProspectiveOracle fixtureSystemId fixtureMembership
    (oracle1, openOutcomes) = foldl submitOpener (oracle0, []) openers
    submitOpener (oracle, outcomes) local =
      let intention = case Disappearance.openIntentions (lookupMap "opener intention state" local candidateStates) of
            [retained] -> retained
            other -> error ("expected exactly one Open intention, got " <> show other)
          (successor, outcome) =
            submitOpenIntention
              (fixtureRequestId local 1)
              Nothing
              local
              intention
              oracle
       in case outcome of
            Target.TargetSubmissionCommitted {} -> (successor, outcomes <> [outcome])
            _ -> error ("prospective Oracle rejected racing Open: " <> show outcome)
    openOutcome = case openOutcomes of
      first : _ -> first
      [] -> error "cut network requires at least one opener"
    projected =
      checked
        "translate projected Oracle Open"
        (projectedProbeFromOpen openOutcome)
    (assignedStates, coordinations, sourceStreams) =
      foldl coordinateOne (candidateStates, Map.empty, Map.empty) fixtureMembers
    coordinateOne (states, retainedCoordinations, retainedStreams) local =
      let state = lookupMap "projected Herald state" local states
          (successor, stream, coordination) =
            checked
              "coordinate projected Open and publication assignment"
              ( DisappearanceUseCase.coordinateProjectedOpen
                  projected
                  ( fixtureEvidenceSnapshot
                      subject
                      fixturePublicationPrefix
                      (incomingAlignmentCuts local)
                      (outgoingAlignmentCuts local)
                      (Map.findWithDefault [] local blockersByHerald)
                      []
                  )
                  state
                  (PeerStream.initialState local)
              )
       in ( Map.insert local successor states,
            Map.insert local coordination retainedCoordinations,
            Map.insert local stream retainedStreams
          )
    publicationItems =
      Map.fromList
        [ ((source, destination), item)
        | source <- fixtureMembers,
          (destination, item) <-
            Map.toAscList
              ( DisappearanceUseCase.projectedOpenCoordinationPublicationAssignments
                  (lookupMap "projected Open coordination" source coordinations)
              )
        ]
    publicationMarkers = sequencedItemPayload <$> publicationItems
    alignmentMarkers =
      [ (source, alignmentSubscriptionIdDestinationHerald (Protocol.disappearanceAlignmentMarkerSubscription marker), marker)
      | source <- fixtureMembers,
        marker <-
          DisappearanceUseCase.projectedOpenCoordinationAlignmentMarkers
            (lookupMap "alignment marker coordination" source coordinations)
      ]

alignmentEdges ::
  [(HeraldEpoch, HeraldEpoch, AlignmentSubscriptionId, Word64)]
alignmentEdges =
  [ (fixtureH1, fixtureH2, fixtureAlignmentSubscription fixtureH2 101, 11),
    (fixtureH2, fixtureH3, fixtureAlignmentSubscription fixtureH3 102, 12),
    (fixtureH3, fixtureH1, fixtureAlignmentSubscription fixtureH1 103, 13)
  ]

incomingAlignmentCuts :: HeraldEpoch -> [Protocol.IncomingAlignmentCut]
incomingAlignmentCuts local =
  [ Protocol.incomingAlignmentCut subscription source
  | (source, destination, subscription, _) <- alignmentEdges,
    destination == local
  ]

outgoingAlignmentCuts :: HeraldEpoch -> [Protocol.OutgoingAlignmentCut]
outgoingAlignmentCuts local =
  [ Protocol.outgoingAlignmentCut subscription (fixtureStoreRevision revision)
  | (source, _, subscription, revision) <- alignmentEdges,
    source == local
  ]

lookupMap :: (Ord key, Show key) => String -> key -> Map key value -> value
lookupMap context key retained = case Map.lookup key retained of
  Nothing -> error (context <> " missing " <> show key)
  Just value -> value

commitChecked ::
  String ->
  Either
    Disappearance.DisappearanceProblem
    (Disappearance.PreparedDisappearanceTransition output) ->
  (Disappearance.State, output)
commitChecked context = Disappearance.commitDisappearanceTransition . checked context

commitExpected ::
  (Eq output, Show output) =>
  String ->
  output ->
  Either
    Disappearance.DisappearanceProblem
    (Disappearance.PreparedDisappearanceTransition output) ->
  (Disappearance.State, output)
commitExpected context expected prepared =
  let result@(_, actual) = commitChecked context prepared
   in if actual == expected
        then result
        else error (context <> ": expected " <> show expected <> ", got " <> show actual)

assertStateValid :: String -> Disappearance.State -> Assertion
assertStateValid context state =
  assertEqual context (Right ()) (Disappearance.validateState state)

assertLeftEqual ::
  (Eq problem, Show problem) =>
  String ->
  problem ->
  Either problem success ->
  Assertion
assertLeftEqual context expected result = case result of
  Left actual -> assertEqual context expected actual
  Right _ -> assertFailure (context <> ": unexpectedly succeeded")

prepareFixtureReport ::
  Disappearance.State ->
  Either
    Disappearance.DisappearanceProblem
    (Disappearance.PreparedDisappearanceTransition Disappearance.ReportDisposition)
prepareFixtureReport state =
  Disappearance.prepareLocalAbsenceReport
    fixtureProbe
    (fixtureAttestation subject)
    state
  where
    subject = case Disappearance.probeWitness fixtureProbe state of
      Nothing -> error "fixture report state has no projected probe"
      Just witness ->
        Protocol.projectedProbeSubject witness.witnessProjectedProbe

completeLocalEvidence ::
  [HeraldEpoch] ->
  HeraldEpoch ->
  CutNetwork ->
  (Disappearance.State, Protocol.LocalAbsenceReport)
completeLocalEvidence sourceOrder local network =
  case reportDisposition of
    Disappearance.LocalReportPrepared report -> (reported, report)
    other -> error ("fresh complete evidence did not prepare Report: " <> show other)
  where
    withAlignmentCuts = completeLocalCuts sourceOrder local network
    (reported, reportDisposition) =
      commitChecked
        "prepare complete local absence report"
        (prepareFixtureReport withAlignmentCuts)

completeLocalCuts ::
  [HeraldEpoch] ->
  HeraldEpoch ->
  CutNetwork ->
  Disappearance.State
completeLocalCuts sourceOrder local network = withAlignmentCuts
  where
    state0 = lookupMap "cut-network Herald" local (cutStates network)
    (withLocalCut, _) =
      commitExpected
        "complete local publication cut"
        Disappearance.MarkerCompleted
        ( Disappearance.prepareLocalPublicationCutCompletion
            fixtureProbe
            fixturePublicationPrefix
            state0
        )
    remoteSources = filter (/= local) sourceOrder
    localStream = lookupMap "cut-network peer stream" local (cutPeerStreams network)
    (withPublicationCuts, _) =
      foldl applyPublicationCut (withLocalCut, localStream) remoteSources
    applyPublicationCut (state, stream) source =
      let item =
            lookupMap
              "incoming publication stream item"
              (source, local)
              (cutPublicationItems network)
          (received, receivedStream, receipt) =
            checked
              "receive contiguous publication marker"
              (DisappearanceUseCase.coordinatePublicationMarkerReceipt item state stream)
          admissions = DisappearanceUseCase.publicationMarkerReceiptAdmissions receipt
          (completed, completedStream, completion) =
            checked
              "complete contiguous publication marker"
              ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
                  item
                  received
                  receivedStream
              )
       in case (admissions, completion) of
            ( [(admitted, Disappearance.MarkerRetained)],
              DisappearanceUseCase.PublicationMarkerCompletionInstalled
                _
                Disappearance.MarkerCompleted
              )
                | admitted == item -> (completed, completedStream)
            other -> error ("unexpected publication marker transition: " <> show other)
    incomingAlignment =
      sortOn
        (Protocol.disappearanceAlignmentMarkerSubscription . third)
        [ edge
        | edge@(_, destination, _) <- cutAlignmentMarkers network,
          destination == local
        ]
    withAlignmentCuts = foldl applyAlignmentCut withPublicationCuts incomingAlignment
    applyAlignmentCut state (source, _, marker) =
      let (received, _) =
            commitExpected
              "receive alignment marker"
              Disappearance.MarkerRetained
              (Disappearance.prepareAlignmentMarkerReceipt source marker state)
          (completed, _) =
            commitExpected
              "complete alignment marker"
              Disappearance.MarkerCompleted
              ( Disappearance.prepareAlignmentMarkerCompletion
                  marker
                  (Protocol.disappearanceAlignmentMarkerThroughRevision marker)
                  received
              )
       in completed

completeNetworkEvidence ::
  [HeraldEpoch] ->
  CutNetwork ->
  (Map HeraldEpoch Disappearance.State, Map HeraldEpoch Protocol.LocalAbsenceReport)
completeNetworkEvidence sourceOrder network =
  unzipMap
    ( Map.fromList
        [ (local, completeLocalEvidence sourceOrder local network)
        | local <- fixtureMembers
        ]
    )
  where
    unzipMap retained =
      (fmap fst retained, fmap snd retained)

projectClaimsEverywhere ::
  [HeraldEpoch] ->
  Map HeraldEpoch Protocol.LocalAbsenceReport ->
  Map HeraldEpoch Disappearance.State ->
  Map HeraldEpoch Disappearance.State
projectClaimsEverywhere reporterOrder reports states0 =
  foldl projectOneReporter states0 reporterOrder
  where
    projectOneReporter states reporter =
      let report = lookupMap "local report" reporter reports
          claim = Protocol.localAbsenceReportClaim report
       in fmap (projectClaim claim) states
    projectClaim claim state =
      fst
        ( commitChecked
            "apply projected report"
            (Disappearance.prepareProjectedReport claim state)
        )

submitReportsToOracle ::
  [HeraldEpoch] ->
  Map HeraldEpoch Protocol.LocalAbsenceReport ->
  ProspectiveOracle ->
  (ProspectiveOracle, [Target.TargetSubmissionOutcome])
submitReportsToOracle reporterOrder reports oracle0 =
  foldl submitOne (oracle0, []) reporterOrder
  where
    submitOne (oracle, outcomes) reporter =
      let report = lookupMap "Oracle report" reporter reports
          (successor, outcome) =
            submitLocalReport
              (fixtureRequestId reporter 2)
              Nothing
              reporter
              report
              oracle
       in case outcome of
            Target.TargetSubmissionCommitted {} ->
              (successor, outcomes <> [outcome])
            _ -> error ("prospective Oracle rejected complete Report: " <> show outcome)

third :: (first, second, third) -> third
third (_, _, value) = value

caseCandidateScheduling :: Assertion
caseCandidateScheduling = do
  let state0 = Disappearance.initialState fixtureH1
      observation = Protocol.localTakeCandidateObservation fixtureControlledSubject
      context = Protocol.disappearanceOpenContext fixtureMembership
  assertEqual
    "bare re-evaluation cannot synthesize a candidate"
    (Left (Disappearance.DisappearanceCandidateMissing fixtureControlledSubject))
    ( Disappearance.preparedDisappearanceOutput
        <$> Disappearance.prepareCandidateReevaluation
          context
          fixtureControlledSubject
          state0
    )
  let (state1, created) =
        commitChecked
          "explicit local-take candidate"
          (Disappearance.prepareLocalTakeCandidate observation state0)
      (state1Replay, retained) =
        commitChecked
          "replayed local-take candidate"
          (Disappearance.prepareLocalTakeCandidate observation state1)
  assertEqual "the first local take creates the candidate" Disappearance.CandidateCreated created
  assertEqual "the repeated local take is classified" Disappearance.CandidateAlreadyRetained retained
  assertEqual "the repeated local take is state-identical" state1 state1Replay
  let (state2, prepared) =
        commitChecked
          "candidate Open scheduling"
          ( Disappearance.prepareCandidateReevaluation
              context
              fixtureControlledSubject
              state1
          )
      (state2Replay, repeated) =
        commitChecked
          "repeated candidate Open scheduling"
          ( Disappearance.prepareCandidateReevaluation
              context
              fixtureControlledSubject
              state2
          )
  assertEqual "the first ticket prepares one Open" Disappearance.CandidateOpenPrepared prepared
  assertEqual
    "the stable ticket is classified as already prepared"
    Disappearance.CandidateOpenAlreadyPrepared
    repeated
  assertEqual "the stable ticket does no additional work" state2 state2Replay
  let successorMembership =
        checked
          "candidate successor membership"
          (retireHeraldMembershipGeneration (controlIndex 50) (fixtureRetirementResolution 50) fixtureH3 fixtureMembership)
      successorContext = Protocol.disappearanceOpenContext successorMembership
      (state3, successorPrepared) =
        commitChecked
          "successor-coordinate candidate Open scheduling"
          ( Disappearance.prepareCandidateReevaluation
              successorContext
              fixtureControlledSubject
              state2
          )
  assertEqual
    "a different established membership replaces the stale Open intention"
    Disappearance.CandidateOpenPrepared
    successorPrepared
  assertEqual
    "the candidate retains only the successor membership coordinate"
    [ Protocol.disappearanceOpenIntentionCoordinate
        (Protocol.disappearanceOpenIntention fixtureControlledSubject successorMembership)
    ]
    ( fmap
        Protocol.disappearanceOpenIntentionCoordinate
        (Disappearance.openIntentions state3)
    )
  let (withRegularCandidate, _) =
        commitChecked
          "second independent candidate"
          ( Disappearance.prepareLocalTakeCandidate
              (Protocol.localTakeCandidateObservation fixtureRegularSubject)
              state3
          )
      (withBothOpen, _) =
        commitChecked
          "second independent Open intention"
          ( Disappearance.prepareCandidateReevaluation
              context
              fixtureRegularSubject
              withRegularCandidate
          )
  assertEqual
    "controlled precedes regular in canonical local intention order"
    [fixtureControlledSubject, fixtureRegularSubject]
    ( fmap
        Protocol.disappearanceOpenIntentionSubject
        (Disappearance.openIntentions withBothOpen)
    )
  assertStateValid "candidate scheduling retains a valid leaf" withBothOpen

caseAllMemberProjection :: Assertion
caseAllMemberProjection = do
  let network = prepareCutNetwork fixtureControlledSubject
      states = cutStates network
  forM_ fixtureMembers $ \local -> do
    let state = lookupMap "captured projection state" local states
    assertBool
      ("captured Herald has projected probe: " <> show local)
      (isJust (Disappearance.probeWitness fixtureProbe state))
    assertEqual
      "each source assigned markers to every other captured member"
      2
      ( maybe
          (-1)
          (\witness -> length witness.witnessOutgoingPublicationMarkers)
          (Disappearance.probeWitness fixtureProbe state)
      )
    assertStateValid "all-member projected leaf" state
  assertEqual
    "only the local-take Herald has a candidate"
    [1, 0, 0]
    [ length
        ( Disappearance.candidateWitnesses
            (lookupMap "candidate ownership" local states)
        )
    | local <- fixtureMembers
    ]

caseLateLocalTakeJoinsProjectedProbe :: Assertion
caseLateLocalTakeJoinsProjectedProbe = do
  let network = prepareCutNetwork fixtureControlledSubject
      peerOnly = lookupMap "peer-home projected state" fixtureH2 (cutStates network)
      (afterTake, takeDisposition) =
        commitChecked
          "late local take"
          ( Disappearance.prepareLocalTakeCandidate
              (Protocol.localTakeCandidateObservation fixtureControlledSubject)
              peerOnly
          )
      (afterReevaluation, reevaluationDisposition) =
        commitChecked
          "late local take reevaluation"
          ( Disappearance.prepareCandidateReevaluation
              (Protocol.disappearanceOpenContext fixtureMembership)
              fixtureControlledSubject
              afterTake
          )
      candidate = case Disappearance.candidateWitnesses afterReevaluation of
        [retained] -> retained
        other -> error ("expected one late candidate witness, got " <> show other)
  assertEqual "the accepted local take creates one eligible local candidate" Disappearance.CandidateCreated takeDisposition
  assertEqual "reevaluation recognizes the already projected subject" Disappearance.CandidateAlreadyRetained reevaluationDisposition
  assertEqual "the late candidate joins the existing probe" (Just fixtureProbe) candidate.candidateProjectedProbe
  assertEqual "the joined candidate schedules no redundant Open" [] (Disappearance.openIntentions afterReevaluation)
  assertStateValid "late local take joined state" afterReevaluation

caseMarkersBeforeOpen :: Assertion
caseMarkersBeforeOpen = do
  let publicationItem = assignedPublicationItem fixtureH2 fixtureH1
      publication = sequencedItemPayload publicationItem
      subscription = fixtureAlignmentSubscription fixtureH1 41
      alignment =
        Protocol.disappearanceAlignmentMarker
          fixtureProbe
          (Protocol.outgoingAlignmentCut subscription (fixtureStoreRevision 9))
      herald0 = initialProspectiveHerald fixtureH1
      herald1 =
        checked
          "hold publication marker before Open"
          (receivePublicationMarker publicationItem herald0)
      herald2 =
        checked
          "hold alignment marker before Open"
          (receiveAlignmentMarker fixtureH2 alignment herald1)
      herald2Replay =
        checked
          "deduplicate publication marker hold"
          (receivePublicationMarker publicationItem herald2)
  assertEqual "both early marker arms share one exact prerequisite" [fixtureProbe] (heldMarkerProbeIds herald2)
  assertEqual "both distinct marker arms remain held" 2 (heldMarkerCount herald2)
  assertEqual "an equal early marker is held only once" 2 (heldMarkerCount herald2Replay)
  let earlyWatermarks =
        checked
          "early marker peer-stream watermarks"
          ( PeerStream.incomingWatermarks
              (Protocol.disappearancePublicationMarkerDirection publication)
              (prospectiveHeraldPublicationStream herald2Replay)
          )
  assertEqual
    "the early marker is retained through the ordinary contiguous peer-stream owner"
    (streamPrefixThrough (sequencedItemSequence publicationItem))
    (streamReceivedPrefix earlyWatermarks)
  assertEqual
    "early marker receipt cannot admit a subject or probe"
    ([], [])
    ( Disappearance.candidateWitnesses (prospectiveHeraldState herald2Replay),
      Disappearance.probeWitnesses (prospectiveHeraldState herald2Replay)
    )
  assertEqual
    "the exact early publication marker is retained without creating probe work"
    [publication]
    (Disappearance.pendingPublicationMarkers (prospectiveHeraldState herald2Replay))
  let preparedOpen =
        checked
          "project Open after markers"
          ( Disappearance.prepareProjectedOpen
              fixtureProjectedProbe
              ( fixtureEvidenceSnapshot
                  fixtureControlledSubject
                  fixturePublicationPrefix
                  [Protocol.incomingAlignmentCut subscription fixtureH2]
                  []
                  []
                  []
              )
              (prospectiveHeraldState herald2Replay)
          )
      (herald3, _) = commitProspectiveHerald preparedOpen herald2Replay
      herald4 =
        checked
          "redrive early markers after Open"
          (redriveMarkersAfterOpen fixtureProbe herald3)
      witness =
        maybe
          (error "redriven probe disappeared")
          id
          (Disappearance.probeWitness fixtureProbe (prospectiveHeraldState herald4))
  assertEqual "redrive drains the exact prerequisite hold" [] (heldMarkerProbeIds herald4)
  assertEqual
    "redrive retains the publication marker under its authenticated source"
    [fixtureH2]
    (fmap fst witness.witnessIncomingPublicationMarkers)
  assertEqual
    "redrive retains the captured alignment marker"
    [Just alignment]
    ( fmap
        (\(_, cell) -> cell.alignmentCellMarker)
        witness.witnessIncomingAlignmentCuts
    )
  assertStateValid "redriven early markers retain a valid leaf" (prospectiveHeraldState herald4)

caseProspectiveRequestOwnership :: Assertion
caseProspectiveRequestOwnership = do
  let h1Open = Protocol.disappearanceOpenIntention fixtureControlledSubject fixtureMembership
      h2Open = Protocol.disappearanceOpenIntention fixtureControlledSubject fixtureMembership
      ledger0 = initialProspectiveRequestLedger
      (ledger1, retainedOpen) = retainOpenRequest fixtureH1 h1Open ledger0
      (ledger1Replay, duplicateOpen) = retainOpenRequest fixtureH1 h1Open ledger1
      oracle0 = initialProspectiveOracle fixtureSystemId fixtureMembership
      (ownedOpenOracle, retainedOpenSubmission) =
        checked
          "dispatch request-owned Open"
          ( submitRetainedOpenRequest
              (Just (controlIndex 0))
              fixtureH1
              h1Open
              ledger1
              oracle0
          )
  assertRequestReplay "Open" retainedOpen duplicateOpen
  assertEqual "exact Open retry cannot allocate" ledger1 ledger1Replay
  assertBool
    "submission consumes the retained request binding"
    ( case prospectiveRequestSubmissionOutcome retainedOpenSubmission of
        Target.TargetSubmissionCommitted {} -> True
        _ -> False
    )
  let changedMembership =
        checked
          "request-key successor membership"
          (retireHeraldMembershipGeneration (controlIndex 50) (fixtureRetirementResolution 50) fixtureH3 fixtureMembership)
      changedCoordinateOpen =
        Protocol.disappearanceOpenIntention fixtureControlledSubject changedMembership
      (changedCoordinateLedger, changedCoordinateRequest) =
        retainOpenRequest fixtureH1 changedCoordinateOpen ledger1
      (_, rejectedCoordinateSubmission) =
        checked
          "dispatch rejected changed-coordinate Open"
          ( submitRetainedOpenRequest
              (Just (controlIndex 99))
              fixtureH1
              changedCoordinateOpen
              changedCoordinateLedger
              oracle0
          )
  assertBool
    "a changed membership coordinate is an independent Open key"
    (prospectiveRequestDispositionRef changedCoordinateRequest /= prospectiveRequestDispositionRef retainedOpen)
  assertEqual "the changed coordinate adds one retained request" 2 (prospectiveRequestCount changedCoordinateLedger)
  let (rejectedLedger, rejectedSettlement) =
        checked
          "settle committed changed-coordinate rejection"
          (settleProspectiveRequest rejectedCoordinateSubmission changedCoordinateLedger)
      rejectedReference = prospectiveRequestDispositionRef changedCoordinateRequest
  assertEqual
    "a committed semantic rejection terminally settles its request"
    ProspectiveRequestSettledNow
    rejectedSettlement
  assertEqual
    "a rejected request remains immutable history without outstanding dispatch"
    1
    (prospectiveOutstandingRequestCount rejectedLedger)
  assertLeftEqual
    "a settled rejection cannot spin by redispatching"
    (ProspectiveRequestAlreadySettled rejectedReference)
    ( submitRetainedOpenRequest
        (Just (controlIndex 99))
        fixtureH1
        changedCoordinateOpen
        rejectedLedger
        oracle0
    )
  let (ledger2, h2OpenRequest) = retainOpenRequest fixtureH2 h2Open ledger1
      network = prepareCutNetwork fixtureControlledSubject
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      completeStates = projectClaimsEverywhere fixtureMembers reports reportedStates
      h1Report = lookupMap "request-owned H1 Report" fixtureH1 reports
      h2Report = lookupMap "request-owned H2 Report" fixtureH2 reports
      h1Resolve = case Disappearance.resolveIntentions (lookupMap "request-owned H1 state" fixtureH1 completeStates) of
        [intention] -> intention
        other -> error ("expected one request-owned Resolve, got " <> show other)
      (ledger3, h1ReportRequest) = retainReportRequest fixtureH1 h1Report ledger2
      (ledger3Replay, h1ReportReplay) = retainReportRequest fixtureH1 h1Report ledger3
      (ledger4, h2ReportRequest) = retainReportRequest fixtureH2 h2Report ledger3
      (ledger5, h1ResolveRequest) = retainResolveRequest fixtureH1 h1Resolve ledger4
      invalidation =
        Protocol.disappearanceInvalidationIntention
          fixtureProbe
          fixtureH1
          Protocol.LocalEvidenceContradicted
          (fixtureEvidenceDigest 130)
      conflictingInvalidation =
        Protocol.disappearanceInvalidationIntention
          fixtureProbe
          fixtureH1
          Protocol.LocalEvidenceContradicted
          (fixtureEvidenceDigest 131)
      (ledger6, invalidationRequest) = retainInvalidationRequest invalidation ledger5
      (ledger6Conflict, conflict) = retainInvalidationRequest conflictingInvalidation ledger6
      (_, boundReportSubmission) =
        checked
          "dispatch request-owned Report"
          ( submitRetainedReportRequest
              Nothing
              fixtureH1
              h1Report
              ledger6
              (cutOracle network)
          )
      (_, boundInvalidationSubmission) =
        checked
          "dispatch request-owned Invalidate"
          ( submitRetainedInvalidationRequest
              Nothing
              invalidation
              ledger6
              (cutOracle network)
          )
      reportedOracle = fst (submitReportsToOracle fixtureMembers reports (cutOracle network))
      (_, boundResolveSubmission) =
        checked
          "dispatch request-owned Resolve"
          ( submitRetainedResolveRequest
              Nothing
              fixtureH1
              h1Resolve
              ledger6
              reportedOracle
          )
  assertRequestReplay "Report" h1ReportRequest h1ReportReplay
  assertEqual "exact Report retry cannot allocate" ledger3 ledger3Replay
  assertEqual
    "conflicting semantic reuse names the retained request"
    (ProspectiveRequestConflict (prospectiveRequestDispositionRef invalidationRequest))
    conflict
  assertEqual "conflicting intention cannot mutate ownership" ledger6 ledger6Conflict
  forM_
    [ retainedOpenSubmission,
      boundReportSubmission,
      boundInvalidationSubmission,
      boundResolveSubmission
    ]
    (assertCommitted "each command-specific submission consumes its retained binding")
  assertEqual "two members and three command kinds remain independent" 6 (prospectiveRequestCount ledger6)
  assertBool
    "different captured members own distinct references"
    (requestId h1ReportRequest /= requestId h2ReportRequest)
  assertBool
    "different command kinds at one member own distinct references"
    ( requestId h1ReportRequest /= requestId h1ResolveRequest
        && requestId h1ResolveRequest /= requestId invalidationRequest
    )
  assertBool
    "racing Open homes retain independent request references"
    (requestId retainedOpen /= requestId h2OpenRequest)
  assertEqual "all retained requests initially await projection" 6 (prospectiveOutstandingRequestCount ledger6)
  let settledReference = prospectiveRequestDispositionRef retainedOpen
  let
    (ledger7, settlement) =
      checked
        "settle request-owned Open"
        (settleProspectiveRequest retainedOpenSubmission ledger6)
    (ledger8, reportSettlement) =
      checked
        "settle request-owned Report"
        (settleProspectiveRequest boundReportSubmission ledger7)
    (ledger9, invalidationSettlement) =
      checked
        "settle request-owned Invalidate"
        (settleProspectiveRequest boundInvalidationSubmission ledger8)
    (ledger10, resolveSettlement) =
      checked
        "settle request-owned Resolve"
        (settleProspectiveRequest boundResolveSubmission ledger9)
    (ledger10Replay, repeatedSettlement) =
      checked
        "replay request settlement"
        (settleProspectiveRequest retainedOpenSubmission ledger10)
    (ledger10Retry, settledRetry) = retainOpenRequest fixtureH1 h1Open ledger10
  assertEqual "first projection explicitly settles its request" ProspectiveRequestSettledNow settlement
  assertEqual
    "every command-specific projection explicitly settles"
    [ProspectiveRequestSettledNow, ProspectiveRequestSettledNow, ProspectiveRequestSettledNow]
    [reportSettlement, invalidationSettlement, resolveSettlement]
  assertEqual "settled history is retained for exact retry" 6 (prospectiveRequestCount ledger10)
  assertEqual "four settlements leave only the undispatched bindings outstanding" 2 (prospectiveOutstandingRequestCount ledger10)
  assertEqual "settlement replay is classified duplicate" ProspectiveRequestSettlementDuplicate repeatedSettlement
  assertEqual "settlement replay is state-identical" ledger10 ledger10Replay
  assertEqual
    "semantic retry after settlement still names the retained reference"
    (ProspectiveRequestDuplicate settledReference)
    settledRetry
  assertEqual "post-settlement semantic retry cannot allocate" ledger10 ledger10Retry
  assertLeftEqual
    "a settled request cannot be dispatched again"
    (ProspectiveRequestAlreadySettled settledReference)
    ( submitRetainedOpenRequest
        Nothing
        fixtureH1
        h1Open
        ledger10
        ownedOpenOracle
    )
  let (ledger11, freshAfterSettlement) =
        retainOpenRequest
          fixtureH1
          (Protocol.disappearanceOpenIntention fixtureRegularSubject fixtureMembership)
          ledger10
  assertBool
    "a fresh semantic key cannot reuse a settled request reference"
    (prospectiveRequestDispositionRef freshAfterSettlement /= settledReference)
  assertEqual "fresh binding extends retained history" 7 (prospectiveRequestCount ledger11)
  where
    requestId =
      OracleRequest.oracleRequestRefRequestId
        . prospectiveRequestDispositionRef
    assertRequestReplay context retained duplicate = do
      let reference = prospectiveRequestDispositionRef retained
      assertEqual
        (context <> " retry reuses the same request reference")
        reference
        (prospectiveRequestDispositionRef duplicate)
      assertEqual
        (context <> " first ownership classification")
        (ProspectiveRequestRetained reference)
        retained
      assertEqual
        (context <> " retry classification")
        (ProspectiveRequestDuplicate reference)
        duplicate
    assertCommitted context submission =
      assertBool context (case prospectiveRequestSubmissionOutcome submission of Target.TargetSubmissionCommitted {} -> True; _ -> False)

caseProspectiveResolveRequestRace :: Assertion
caseProspectiveResolveRequestRace = do
  let network = prepareCutNetwork fixtureControlledSubject
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      projectedStates = projectClaimsEverywhere fixtureMembers reports reportedStates
      resolve = case Disappearance.resolveIntentions (lookupMap "racing Resolve state" fixtureH1 projectedStates) of
        [retained] -> retained
        other -> error ("expected one racing Resolve intention, got " <> show other)
      (ledger1, firstRequest) =
        retainResolveRequest fixtureH2 resolve initialProspectiveRequestLedger
      (ledger2, secondRequest) = retainResolveRequest fixtureH3 resolve ledger1
      reportedOracle = fst (submitReportsToOracle fixtureMembers reports (cutOracle network))
      (terminalOracle, firstSubmission) =
        checked
          "submit first racing Resolve"
          ( submitRetainedResolveRequest
              Nothing
              fixtureH2
              resolve
              ledger2
              reportedOracle
          )
      (_, secondSubmission) =
        checked
          "submit eventless semantic Resolve duplicate"
          ( submitRetainedResolveRequest
              Nothing
              fixtureH3
              resolve
              ledger2
              terminalOracle
          )
      (ledger3, firstSettlement) =
        checked
          "settle first racing Resolve"
          (settleProspectiveRequest firstSubmission ledger2)
      (ledger4, secondSettlement) =
        checked
          "settle eventless racing Resolve"
          (settleProspectiveRequest secondSubmission ledger3)
      firstReference = prospectiveRequestDispositionRef firstRequest
      secondReference = prospectiveRequestDispositionRef secondRequest
  assertBool "racing homes retain distinct Resolve references" (firstReference /= secondReference)
  assertEqual
    "first Resolve carries the one projected terminal"
    1
    (length (committedTargetEvents (prospectiveRequestSubmissionOutcome firstSubmission)))
  assertEqual
    "later accepted Resolve is an eventless semantic duplicate"
    []
    (committedTargetEvents (prospectiveRequestSubmissionOutcome secondSubmission))
  assertEqual "first Resolve request settles" ProspectiveRequestSettledNow firstSettlement
  assertEqual "eventless accepted Resolve request also settles" ProspectiveRequestSettledNow secondSettlement
  assertEqual "both settled references remain in immutable history" 2 (prospectiveRequestCount ledger4)
  assertEqual "neither accepted Resolve keeps outstanding request work" 0 (prospectiveOutstandingRequestCount ledger4)
  case prospectiveTerminalFromSubmission firstSubmission of
    Left problem -> assertFailure ("first Resolve lost terminal authority: " <> show problem)
    Right _ -> pure ()
  assertLeftEqual
    "an eventless later receipt cannot synthesize terminal authority at its own index"
    (ProspectiveRequestHasNoTerminalProjection secondReference)
    (prospectiveTerminalFromSubmission secondSubmission)

caseMatchingWriteGateOwnership :: Assertion
caseMatchingWriteGateOwnership = do
  let network = prepareCutNetwork fixtureControlledSubject
      projected = cutProjectedProbe network
  forM_ fixtureMembers $ \local -> do
    let state = lookupMap "matching-write gate state" local (cutStates network)
        gate = case Disappearance.matchingWriteGateForProbe fixtureProbe state of
          Nothing -> error "projected Open did not install its matching-write gate"
          Just retained -> retained
        (replayed, output) =
          commitChecked
            "replay projected Open with matching-write gate"
            ( Disappearance.prepareProjectedOpen
                projected
                ( fixtureEvidenceSnapshot
                    fixtureControlledSubject
                    fixturePublicationPrefix
                    (incomingAlignmentCuts local)
                    (outgoingAlignmentCuts local)
                    []
                    []
                )
                state
            )
    assertEqual "the gate binds the exact projected probe" fixtureProbe (Disappearance.matchingWriteGateProbe gate)
    assertEqual "the gate binds the exact subject" fixtureControlledSubject (Disappearance.matchingWriteGateSubject gate)
    assertEqual "the gate binds the exact coordinate" fixtureCoordinate (Disappearance.matchingWriteGateCoordinate gate)
    assertEqual "the gate binds the captured local cut" fixturePublicationPrefix (Disappearance.matchingWriteGateLocalCut gate)
    assertEqual "an equal projection is classified duplicate" Disappearance.ProjectedOpenDuplicate output.projectedOpenDisposition
    assertEqual "projection replay cannot replace or duplicate the gate" state replayed
    assertEqual
      "the owner exposes the same opaque gate after replay"
      (Just gate)
      (Disappearance.matchingWriteGateForProbe fixtureProbe replayed)
    assertStateValid "matching-write gate owner remains valid" replayed

caseLeafCoordinateExclusion :: Assertion
caseLeafCoordinateExclusion = do
  let network = prepareCutNetwork fixtureControlledSubject
      state0 = lookupMap "coordinate exclusion H1" fixtureH1 (cutStates network)
      conflictingProbe = checked "second probe ID" (deriveDisappearanceProbeId (controlIndex 20))
      conflictingProjected =
        checked
          "second projected probe"
          ( Protocol.projectedDisappearanceProbe
              conflictingProbe
              fixtureControlledSubject
              fixtureCoordinate
              fixtureMembership
          )
  assertLeftEqual
    "a second collecting probe cannot occupy the exact subject"
    ( Disappearance.DisappearanceSubjectAlreadyProjected
        fixtureControlledSubject
        fixtureProbe
    )
    ( Disappearance.prepareProjectedOpen
        conflictingProjected
        ( fixtureEvidenceSnapshot
            fixtureControlledSubject
            fixturePublicationPrefix
            (incomingAlignmentCuts fixtureH1)
            (outgoingAlignmentCuts fixtureH1)
            []
            []
        )
        state0
    )
  let gate = requiredMatchingWriteGate "coordinate exclusion" fixtureProbe state0
      witness = fixtureEvidenceDigest 132
      (withInvalidation, _) =
        commitChecked
          "coordinate exclusion invalidation intention"
          ( Disappearance.prepareMatchingPublicationObservation
              gate
              ( Protocol.matchingPublicationObservation
                  fixtureControlledSubject
                  (fixtureHeraldPublicationPosition 8)
                  witness
              )
              state0
          )
      originalIntention =
        Protocol.disappearanceOpenIntention
          fixtureControlledSubject
          fixtureMembership
      (requestLedger1, originalRequest) =
        retainOpenRequest fixtureH1 originalIntention initialProspectiveRequestLedger
      originalReference = prospectiveRequestDispositionRef originalRequest
      (openOracle, originalSubmission) =
        checked
          "submit original request-owned Open"
          ( submitRetainedOpenRequest
              (Just (controlIndex 0))
              fixtureH1
              originalIntention
              requestLedger1
              (initialProspectiveOracle fixtureSystemId fixtureMembership)
          )
      (requestLedger2, _) =
        checked
          "settle original projected Open request"
          (settleProspectiveRequest originalSubmission requestLedger1)
      invalidation =
        Protocol.disappearanceInvalidationIntention
          fixtureProbe
          fixtureH1
          Protocol.MatchingPublicationObserved
          witness
      (requestLedger3, invalidationRequest) =
        retainInvalidationRequest invalidation requestLedger2
      (terminalOracle, invalidationSubmission) =
        checked
          "submit request-owned terminal Invalidate"
          ( submitRetainedInvalidationRequest
              Nothing
              invalidation
              requestLedger3
              openOracle
          )
      terminalProjection =
        checked
          "extract exact Target terminal projection"
          (prospectiveTerminalFromSubmission invalidationSubmission)
      (requestLedger4, _) =
        checked
          "settle request-owned terminal Invalidate"
          (settleProspectiveRequest invalidationSubmission requestLedger3)
      (terminalState, requestLedger5, terminalDisposition, releaseDisposition) =
        checked
          "project terminal and release its exact Open binding"
          ( releaseProspectiveOpenRequest
              originalReference
              terminalProjection
              withInvalidation
              requestLedger4
          )
      (rearmed, _) =
        commitExpected
          "coordinate exclusion explicit rearm"
          Disappearance.CandidateRearmed
          ( Disappearance.prepareLocalTakeCandidate
              (Protocol.localTakeCandidateObservation fixtureControlledSubject)
              terminalState
          )
      (rescheduled, _) =
        commitExpected
          "coordinate exclusion fresh Open intention"
          Disappearance.CandidateOpenPrepared
          ( Disappearance.prepareCandidateReevaluation
              (Protocol.disappearanceOpenContext fixtureMembership)
              fixtureControlledSubject
              rearmed
          )
      freshIntention = case Disappearance.openIntentions rescheduled of
        [retained] -> retained
        other -> error ("expected one post-terminal Open intention, got " <> show other)
      (requestLedger6, freshRequest) =
        retainOpenRequest fixtureH1 freshIntention requestLedger5
      (staleTerminalState, requestLedger6Replay, staleTerminalDisposition, staleReleaseDisposition) =
        checked
          "replay stale terminal after fresh same-key retention"
          ( releaseProspectiveOpenRequest
              originalReference
              terminalProjection
              terminalState
              requestLedger6
          )
      (_, freshSubmission) =
        checked
          "submit fresh same-key Open after terminal"
          ( submitRetainedOpenRequest
              Nothing
              fixtureH1
              freshIntention
              requestLedger6
              terminalOracle
          )
      freshProjected =
        checked
          "extract fresh post-terminal Open projection"
          (projectedProbeFromOpen (prospectiveRequestSubmissionOutcome freshSubmission))
      nextProbe = Protocol.projectedProbeId freshProjected
      (requestLedger7, _) =
        checked
          "settle fresh same-key Open"
          (settleProspectiveRequest freshSubmission requestLedger6)
      (reopened, disposition) =
        commitChecked
          "coordinate exclusion new projection"
          ( Disappearance.prepareProjectedOpen
              freshProjected
              ( fixtureEvidenceSnapshot
                  fixtureControlledSubject
                  fixturePublicationPrefix
                  (incomingAlignmentCuts fixtureH1)
                  (outgoingAlignmentCuts fixtureH1)
                  []
                  []
              )
              rescheduled
          )
      (reopenedReplay, requestLedger7Replay, staleSettledTerminalDisposition, staleSettledReleaseDisposition) =
        checked
          "replay stale terminal after fresh projection settlement"
          ( releaseProspectiveOpenRequest
              originalReference
              terminalProjection
              reopened
              requestLedger7
          )
  assertLeftEqual
    "terminal release refuses the exact source request until that request settles"
    ( ProspectiveRequestNotSettled
        (prospectiveRequestDispositionRef invalidationRequest)
    )
    ( releaseProspectiveOpenRequest
        originalReference
        terminalProjection
        withInvalidation
        requestLedger3
    )
  assertEqual "fresh post-terminal projection installs" Disappearance.ProjectedOpenInstalled disposition.projectedOpenDisposition
  assertEqual "the Target terminal is installed before release" Disappearance.TerminalInstalled terminalDisposition
  assertEqual "stale terminal replay is classified duplicate" Disappearance.TerminalDuplicate staleTerminalDisposition
  assertEqual "stale terminal after fresh projection is still duplicate" Disappearance.TerminalDuplicate staleSettledTerminalDisposition
  assertBool
    "terminal release lets the same subject/coordinate bind a fresh request"
    (prospectiveRequestDispositionRef freshRequest /= originalReference)
  assertBool
    "Invalidate and Open retain independent exact request references"
    (prospectiveRequestDispositionRef invalidationRequest /= originalReference)
  assertEqual "the exact old reference releases its active key" ProspectiveRequestReleasedNow releaseDisposition
  assertEqual "a stale exact-ref release is classified duplicate" ProspectiveRequestReleaseDuplicate staleReleaseDisposition
  assertEqual "a stale exact-ref release cannot delete the fresh ABA binding" requestLedger6 requestLedger6Replay
  assertEqual "a stale terminal replay cannot mutate the leaf" terminalState staleTerminalState
  assertEqual "stale release after fresh settlement is still duplicate" ProspectiveRequestReleaseDuplicate staleSettledReleaseDisposition
  assertEqual "stale release cannot delete a settled ABA binding" requestLedger7 requestLedger7Replay
  assertEqual "stale terminal cannot retire the fresh probe" reopened reopenedReplay
  assertEqual "Open, Invalidate, and fresh Open identities remain in history" 3 (prospectiveRequestCount requestLedger6)
  assertEqual "only the fresh post-terminal Open remains outstanding" 1 (prospectiveOutstandingRequestCount requestLedger6)
  assertBool "fresh probe is retained" (isJust (Disappearance.probeWitness nextProbe reopened))
  assertStateValid "fresh post-terminal coordinate" reopened

caseCutReadiness :: Assertion
caseCutReadiness = do
  let network = prepareCutNetwork fixtureControlledSubject
      state0 = lookupMap "cut readiness H1" fixtureH1 (cutStates network)
      incomingPublication source =
        lookupMap
          "cut readiness publication marker"
          (source, fixtureH1)
          (cutPublicationMarkers network)
      alignmentEdge = case [ (source, marker)
                           | (source, destination, marker) <- cutAlignmentMarkers network,
                             destination == fixtureH1
                           ] of
        [edge] -> edge
        other -> error ("expected one H1 alignment marker, got " <> show other)
  assertReportBlocked "before any cut completes" state0
  let (state1, _) =
        commitChecked
          "cut readiness local publication completion"
          ( Disappearance.prepareLocalPublicationCutCompletion
              fixtureProbe
              fixturePublicationPrefix
              state0
          )
  assertReportBlocked "after only the local cut" state1
  let markerH2 = incomingPublication fixtureH2
      (state2, _) =
        commitChecked
          "cut readiness H2 marker receipt"
          (Disappearance.preparePublicationMarkerReceipt markerH2 state1)
  assertReportBlocked "after receipt but before contiguous marker completion" state2
  let (state3, _) =
        commitChecked
          "cut readiness H2 marker completion"
          (Disappearance.preparePublicationMarkerCompletion markerH2 state2)
  assertReportBlocked "while the other remote source is missing" state3
  let markerH3 = incomingPublication fixtureH3
      (state4, _) =
        commitChecked
          "cut readiness H3 marker receipt"
          (Disappearance.preparePublicationMarkerReceipt markerH3 state3)
      (state5, _) =
        commitChecked
          "cut readiness H3 marker completion"
          (Disappearance.preparePublicationMarkerCompletion markerH3 state4)
  assertReportBlocked "while the captured alignment watermark is missing" state5
  let (alignmentSource, alignmentMarker) = alignmentEdge
      (state6, _) =
        commitChecked
          "cut readiness alignment marker receipt"
          ( Disappearance.prepareAlignmentMarkerReceipt
              alignmentSource
              alignmentMarker
              state5
          )
  assertReportBlocked "before the alignment revision has applied" state6
  assertBool
    "a short alignment completion is rejected"
    ( case Disappearance.prepareAlignmentMarkerCompletion
        alignmentMarker
        (fixtureStoreRevision 0)
        state6 of
        Left Disappearance.DisappearanceAlignmentCompletionShort {} -> True
        _ -> False
    )
  let (state7, _) =
        commitChecked
          "cut readiness alignment marker completion"
          ( Disappearance.prepareAlignmentMarkerCompletion
              alignmentMarker
              (Protocol.disappearanceAlignmentMarkerThroughRevision alignmentMarker)
              state6
          )
      (reported, disposition) =
        commitChecked
          "cut readiness final Report"
          (prepareFixtureReport state7)
  case disposition of
    Disappearance.LocalReportPrepared {} -> pure ()
    other -> assertFailure ("complete cut did not prepare Report: " <> show other)
  assertStateValid "complete cut retains a valid report witness" reported

casePublicationMarkerPeerStreamOrdering :: Assertion
casePublicationMarkerPeerStreamOrdering = do
  let network = prepareCutNetwork fixtureControlledSubject
      direction = fixtureStreamDirection fixtureH2 fixtureH1
      firstItem =
        lookupMap
          "first H2-to-H1 publication marker item"
          (fixtureH2, fixtureH1)
          (cutPublicationItems network)
      secondProbe =
        checked
          "second stream-ordering probe"
          (deriveDisappearanceProbeId (controlIndex 60))
      secondCoordinate =
        disappearanceSubjectMembershipCoordinate fixtureRegularSubject fixtureMembership
      secondProjected =
        checked
          "second stream-ordering projection"
          ( Protocol.projectedDisappearanceProbe
              secondProbe
              fixtureRegularSubject
              secondCoordinate
              fixtureMembership
          )
      source0 = lookupMap "H2 stream-ordering source" fixtureH2 (cutPeerStreams network)
      preparedSecond =
        checked
          "assign second H2-to-H1 publication marker"
          ( PeerStream.prepareSequenceBoundEnqueue
              ( Map.singleton
                  fixtureH1
                  (Protocol.disappearancePublicationMarkerItem secondProbe secondCoordinate :| [])
              )
              source0
          )
      (_, secondAssignments) = PeerStream.commitEnqueue preparedSecond
      secondItem = singleAssignment fixtureH1 secondAssignments
      leaf0 = lookupMap "H1 stream-ordering leaf" fixtureH1 (cutStates network)
      destination0 = lookupMap "H1 stream-ordering peer stream" fixtureH1 (cutPeerStreams network)
      (afterGapLeaf, afterGapStream, gapReceipt) =
        checked
          "retain marker N before N-1"
          ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
              secondItem
              leaf0
              destination0
          )
  assertEqual "the gap does not touch disappearance state" leaf0 afterGapLeaf
  assertEqual
    "the future marker has no leaf admission"
    []
    (DisappearanceUseCase.publicationMarkerReceiptAdmissions gapReceipt)
  assertEqual
    "the generic stream reports the missing predecessor"
    (ReceiveGap (sequencedItemSequence firstItem))
    ( receiveResultDisposition
        (DisappearanceUseCase.publicationMarkerReceiptStreamResult gapReceipt)
    )
  assertEqual
    "the generic stream retains marker N as a gap"
    [(secondItem, PeerStream.GapRetained)]
    (checked "retained H2-to-H1 gap" (PeerStream.incomingRetainedItems direction afterGapStream))

  let (afterReceiptLeaf, afterReceiptStream, contiguousReceipt) =
        checked
          "fill N-1 and expose the contiguous marker batch"
          ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
              firstItem
              afterGapLeaf
              afterGapStream
          )
      contiguousResult =
        DisappearanceUseCase.publicationMarkerReceiptStreamResult contiguousReceipt
  assertEqual "filling the gap is contiguous" ReceiveContiguous (receiveResultDisposition contiguousResult)
  assertEqual
    "both contiguous markers are retained pending semantic completion"
    [ (firstItem, PeerStream.IncomingReceivedPending),
      (secondItem, PeerStream.IncomingReceivedPending)
    ]
    ( checked
        "pending H2-to-H1 markers"
        (PeerStream.incomingRetainedItems direction afterReceiptStream)
    )
  assertEqual
    "the received prefix covers marker N"
    (streamPrefixThrough (sequencedItemSequence secondItem))
    (streamReceivedPrefix (receiveResultWatermarks contiguousResult))
  assertEqual
    "the completed prefix cannot cross either pending marker"
    emptyStreamPrefix
    (streamCompletedPrefix (receiveResultWatermarks contiguousResult))
  assertEqual
    "only the known probe admits its marker before OracleAdvance"
    [ (firstItem, Disappearance.MarkerRetained),
      (secondItem, Disappearance.MarkerAwaitingProbeProjection secondProbe)
    ]
    (DisappearanceUseCase.publicationMarkerReceiptAdmissions contiguousReceipt)
  assertEqual
    "the early marker still cannot invent its subject probe"
    Nothing
    (Disappearance.probeWitness secondProbe afterReceiptLeaf)

  let (blockedLeaf, blockedStream, blockedCompletion) =
        checked
          "attempt marker N completion before N-1"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              secondItem
              afterReceiptLeaf
              afterReceiptStream
          )
  assertEqual "blocked marker N completion leaves the leaf identical" afterReceiptLeaf blockedLeaf
  assertEqual "blocked marker N completion leaves the stream identical" afterReceiptStream blockedStream
  case blockedCompletion of
    DisappearanceUseCase.PublicationMarkerCompletionAwaitingPriorStreamWork watermarks ->
      assertEqual
        "the attempted completion proof remains below marker N"
        emptyStreamPrefix
        (streamCompletedPrefix watermarks)
    other -> assertFailure ("marker N completed before N-1: " <> show other)

  let (withSecondProbe, projectedOutput) =
        commitChecked
          "project the early marker's probe"
          ( Disappearance.prepareProjectedOpen
              secondProjected
              ( fixtureEvidenceSnapshot
                  fixtureRegularSubject
                  fixturePublicationPrefix
                  []
                  []
                  []
                  []
              )
              blockedLeaf
          )
      (afterRedrive, redriveDisposition) =
        commitExpected
          "redrive marker N after OracleAdvance"
          Disappearance.MarkerRetained
          ( Disappearance.preparePublicationMarkerReceipt
              (sequencedItemPayload secondItem)
              withSecondProbe
          )
  assertEqual
    "the second projected Open installs before redrive"
    Disappearance.ProjectedOpenInstalled
    projectedOutput.projectedOpenDisposition
  assertEqual "redrive admits the exact held marker" Disappearance.MarkerRetained redriveDisposition

  let (afterFirstLeaf, afterFirstStream, firstCompletion) =
        checked
          "complete marker N-1"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              firstItem
              afterRedrive
              blockedStream
          )
  case firstCompletion of
    DisappearanceUseCase.PublicationMarkerCompletionInstalled watermarks Disappearance.MarkerCompleted ->
      assertEqual
        "completion advances exactly through N-1"
        (streamPrefixThrough (sequencedItemSequence firstItem))
        (streamCompletedPrefix watermarks)
    other -> assertFailure ("marker N-1 did not complete: " <> show other)
  assertEqual
    "completing N-1 releases only its payload receipt"
    [(secondItem, PeerStream.IncomingReceivedPending)]
    (checked "marker N still pending" (PeerStream.incomingRetainedItems direction afterFirstStream))
  assertBool
    "compact completion remembers N-1 without crossing pending N"
    ( PeerStream.incomingSequenceCompleted (direction, sequencedItemSequence firstItem) afterFirstStream
        && not (PeerStream.incomingSequenceCompleted (direction, sequencedItemSequence secondItem) afterFirstStream)
    )

  let (afterSecondLeaf, afterSecondStream, secondCompletion) =
        checked
          "complete marker N after N-1"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              secondItem
              afterFirstLeaf
              afterFirstStream
          )
  case secondCompletion of
    DisappearanceUseCase.PublicationMarkerCompletionInstalled watermarks Disappearance.MarkerCompleted ->
      assertEqual
        "completion advances through marker N only after its predecessor"
        (streamPrefixThrough (sequencedItemSequence secondItem))
        (streamCompletedPrefix watermarks)
    other -> assertFailure ("marker N did not complete after N-1: " <> show other)
  assertEqual
    "both completed payload receipts have been reclaimed"
    []
    ( checked
        "completed H2-to-H1 marker records"
        (PeerStream.incomingRetainedItems direction afterSecondStream)
    )
  forM_ [firstItem, secondItem] $ \item ->
    assertBool
      "compact progress remembers each completed marker"
      (PeerStream.incomingSequenceCompleted (direction, sequencedItemSequence item) afterSecondStream)
  assertStateValid "peer-stream ordered marker completion" afterSecondLeaf

caseMarkerRepairReplay :: Assertion
caseMarkerRepairReplay = do
  let network = prepareCutNetwork fixtureControlledSubject
      sourceStream = lookupMap "retained H2 peer stream" fixtureH2 (cutPeerStreams network)
      destinationStream =
        PeerStream.initialState fixtureH1 :: PeerStream.State Protocol.DisappearancePublicationMarker
      offer =
        checked
          "fresh H1 reconnect offer"
          (PeerStream.resumeOfferForPeer fixtureH2 destinationStream)
      resume = checked "H2 reconnect reconciliation" (PeerStream.prepareResume offer sourceStream)
      retransmissions = PeerStream.preparedResumeRetransmissions resume
      expectedItems =
        checked
          "retained H2-to-H1 marker assignment"
          ( PeerStream.activeOutgoingItems
              (fixtureStreamDirection fixtureH2 fixtureH1)
              sourceStream
          )
      (resumedSource, _, committedRetransmissions) = PeerStream.commitResume resume
      repairedItem = case retransmissions of
        [item] -> item
        other -> error ("expected one reconnect retransmission, got " <> show (length other))
      marker = sequencedItemPayload repairedItem
      state0 = lookupMap "repair destination state" fixtureH1 (cutStates network)
      (state1, destination1, receipt) =
        checked
          "first marker receipt"
          ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
              repairedItem
              state0
              destinationStream
          )
      (state1Replay, destination1Replay, replayReceipt) =
        checked
          "repaired marker receipt"
          ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
              repairedItem
              state1
              destination1
          )
      (state2, destination2, completion) =
        checked
          "first marker completion"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              repairedItem
              state1
              destination1
          )
      (state2Replay, destination2Replay, replayCompletion) =
        checked
          "repaired marker completion"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              repairedItem
              state2
              destination2
          )
  assertEqual "reconnect selects the exact retained stream item" expectedItems retransmissions
  assertEqual "resume commit returns the same exact retransmission" retransmissions committedRetransmissions
  assertEqual
    "reconnect keeps the logical stream assignment retained"
    expectedItems
    ( checked
        "active H2-to-H1 marker after resume"
        ( PeerStream.activeOutgoingItems
            (fixtureStreamDirection fixtureH2 fixtureH1)
            resumedSource
        )
    )
  assertEqual
    "reconnect reuses the original marker identity"
    ( lookupMap
        "original marker"
        (fixtureH2, fixtureH1)
        (cutPublicationMarkers network)
    )
    marker
  assertEqual
    "first receipt is retained through the generic stream"
    [(repairedItem, Disappearance.MarkerRetained)]
    (DisappearanceUseCase.publicationMarkerReceiptAdmissions receipt)
  assertEqual
    "repair receipt deduplicates in the generic stream before touching the leaf"
    ReceiveDuplicate
    ( receiveResultDisposition
        (DisappearanceUseCase.publicationMarkerReceiptStreamResult replayReceipt)
    )
  assertEqual
    "duplicate stream receipt has no leaf admission"
    []
    (DisappearanceUseCase.publicationMarkerReceiptAdmissions replayReceipt)
  assertEqual "repair receipt does not mutate state" state1 state1Replay
  assertEqual "repair receipt does not mutate stream state" destination1 destination1Replay
  case completion of
    DisappearanceUseCase.PublicationMarkerCompletionInstalled _ Disappearance.MarkerCompleted -> pure ()
    other -> assertFailure ("first repaired marker did not complete: " <> show other)
  case replayCompletion of
    DisappearanceUseCase.PublicationMarkerCompletionInstalled
      _
      Disappearance.MarkerCompletionDuplicate -> pure ()
    other -> assertFailure ("repair completion did not deduplicate: " <> show other)
  assertEqual "repair completion does not mutate state" state2 state2Replay
  assertEqual "repair completion does not mutate stream state" destination2 destination2Replay
  let (reported, report) = completeLocalEvidence fixtureMembers fixtureH1 network
      (replayedReportState, reportReplay) =
        commitChecked
          "replayed complete Report"
          (prepareFixtureReport reported)
  assertEqual
    "a repaired stream cannot create another Report"
    (Disappearance.LocalReportDuplicate report)
    reportReplay
  assertEqual "Report replay is state-identical" reported replayedReportState
  let conflictingAttestation =
        checked
          "conflicting complete attestation"
          ( Protocol.disappearanceAbsenceAttestation
              fixtureControlledSubject
              ( (Protocol.ApplicationStoreAbsence, fixtureEvidenceDigest 201)
                  : filter
                    ((/= Protocol.ApplicationStoreAbsence) . fst)
                    (fixtureAttestationEntries fixtureControlledSubject)
              )
          )
  assertLeftEqual
    "a conflicting post-Report attestation cannot replace retained evidence"
    (Disappearance.DisappearanceReportAlreadyPrepared fixtureProbe)
    ( Disappearance.prepareLocalAbsenceReport
        fixtureProbe
        conflictingAttestation
        reported
    )

caseMarkerAfterTerminal :: Assertion
caseMarkerAfterTerminal = do
  let network = prepareCutNetwork fixtureControlledSubject
      item = lookupMap "late H2-to-H1 marker" (fixtureH2, fixtureH1) (cutPublicationItems network)
      direction = sequencedItemDirection item
      state0 = lookupMap "late-marker H1 state" fixtureH1 (cutStates network)
      terminal =
        Protocol.ProjectedDisappearanceInvalidated
          fixtureProbe
          ( Protocol.ProjectedCommandInvalidation
              fixtureH2
              Protocol.MatchingPublicationObserved
              (fixtureEvidenceDigest 208)
          )
          (controlIndex 209)
      (terminalState, terminalDisposition) =
        commitChecked
          "terminalize before the marker arrives"
          (Disappearance.prepareProjectedTerminal terminal state0)
      stream0 =
        PeerStream.initialState fixtureH1 :: PeerStream.State Protocol.DisappearancePublicationMarker
      (receivedState, receivedStream, receipt) =
        checked
          "receive a marker after terminalization"
          ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
              item
              terminalState
              stream0
          )
      receiptResult =
        DisappearanceUseCase.publicationMarkerReceiptStreamResult receipt
      (completedState, completedStream, completion) =
        checked
          "repeat completion for the terminally obsolete marker"
          ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
              item
              receivedState
              receivedStream
          )
  assertEqual "the Invalidate projection terminalizes the probe" Disappearance.TerminalInstalled terminalDisposition
  assertEqual
    "the late marker is explicitly terminally obsolete"
    [(item, Disappearance.MarkerTerminallyObsolete)]
    (DisappearanceUseCase.publicationMarkerReceiptAdmissions receipt)
  assertEqual
    "terminally obsolete receipt advances the completed stream prefix"
    (streamPrefixThrough (sequencedItemSequence item))
    (streamCompletedPrefix (receiveResultWatermarks receiptResult))
  assertEqual
    "the generic owner releases the terminally obsolete payload receipt"
    []
    ( checked
        "terminally obsolete incoming marker"
        (PeerStream.incomingRetainedItems direction receivedStream)
    )
  assertBool
    "compact completion remembers the terminally obsolete assignment"
    (PeerStream.incomingSequenceCompleted (direction, sequencedItemSequence item) receivedStream)
  assertEqual "terminally obsolete receipt does not mutate the leaf" terminalState receivedState
  case completion of
    DisappearanceUseCase.PublicationMarkerCompletionInstalled
      watermarks
      Disappearance.MarkerCompletionDuplicate ->
        assertEqual
          "repeated completion retains the terminal prefix"
          (streamPrefixThrough (sequencedItemSequence item))
          (streamCompletedPrefix watermarks)
    other -> assertFailure ("terminally obsolete completion was not idempotent: " <> show other)
  assertEqual "terminal completion replay does not mutate the leaf" receivedState completedState
  assertEqual "terminal completion replay does not mutate the stream" receivedStream completedStream
  assertStateValid "terminally obsolete marker receipt" completedState

caseAlignmentMarkerRepairReplay :: Assertion
caseAlignmentMarkerRepairReplay = do
  let network = prepareCutNetwork fixtureControlledSubject
      state0 = lookupMap "alignment repair destination state" fixtureH1 (cutStates network)
      (source, original) = case [ (markerSource, marker)
                                | (markerSource, destination, marker) <- cutAlignmentMarkers network,
                                  destination == fixtureH1
                                ] of
        [retained] -> retained
        other -> error ("expected one H1 alignment marker, got " <> show (length other))
      repaired =
        Protocol.disappearanceAlignmentMarker
          (Protocol.disappearanceAlignmentMarkerProbe original)
          ( Protocol.outgoingAlignmentCut
              (Protocol.disappearanceAlignmentMarkerSubscription original)
              (Protocol.disappearanceAlignmentMarkerThroughRevision original)
          )
      (received, receiptDisposition) =
        checked
          "receive repaired alignment marker"
          (DisappearanceUseCase.coordinateAlignmentMarkerReceipt source repaired state0)
      (receiptReplay, replayReceiptDisposition) =
        checked
          "replay repaired alignment marker receipt"
          (DisappearanceUseCase.coordinateAlignmentMarkerReceipt source repaired received)
      through = Protocol.disappearanceAlignmentMarkerThroughRevision repaired
      (completed, completionDisposition) =
        checked
          "complete repaired alignment marker"
          (DisappearanceUseCase.coordinateAlignmentMarkerCompletion repaired through received)
      (completionReplay, replayCompletionDisposition) =
        checked
          "replay repaired alignment marker completion"
          (DisappearanceUseCase.coordinateAlignmentMarkerCompletion repaired through completed)
      workLedger = Disappearance.disappearanceWorkLedger
      logicalWork = Disappearance.disappearanceLogicalWork . workLedger
  assertEqual "repair reconstructs the exact marker identity" original repaired
  assertEqual "the first repaired receipt is retained" Disappearance.MarkerRetained receiptDisposition
  assertEqual "receipt replay is classified duplicate" Disappearance.MarkerDuplicate replayReceiptDisposition
  assertEqual "receipt replay is state-identical" received receiptReplay
  assertEqual
    "one fresh alignment receipt charges exactly one unit"
    1
    (logicalWork received - logicalWork state0)
  assertEqual
    "alignment receipt replay charges no work"
    0
    (logicalWork receiptReplay - logicalWork received)
  assertEqual "the first repaired marker completes" Disappearance.MarkerCompleted completionDisposition
  assertEqual
    "completion replay is classified duplicate"
    Disappearance.MarkerCompletionDuplicate
    replayCompletionDisposition
  assertEqual "completion replay is state-identical" completed completionReplay
  assertEqual
    "one fresh alignment completion charges exactly one unit"
    1
    (logicalWork completed - logicalWork received)
  assertEqual
    "alignment completion replay charges no work"
    0
    (logicalWork completionReplay - logicalWork completed)
  assertStateValid "alignment marker repair replay" completionReplay

caseAlignmentMutations :: Assertion
caseAlignmentMutations = do
  let state0 = lookupMap "alignment mutation state" fixtureH1 (cutStates network)
      subscription = fixtureAlignmentSubscription fixtureH1 103
      witness =
        Protocol.disappearanceBlockerWitness
          Protocol.AlignmentSubscriptionBlocker
          (fixtureEvidenceDigest 71)
      mutations =
        [ Disappearance.AlignmentSubscriptionCreated subscription,
          Disappearance.AlignmentSubscriptionReplaced subscription,
          Disappearance.AlignmentSubscriptionCancelled subscription,
          Disappearance.AlignmentSubscriptionSourceLost subscription fixtureH3
        ]
      network = prepareCutNetwork fixtureControlledSubject
  forM_ mutations $ \mutation -> do
    let (invalidated, intention) =
          commitChecked
            "alignment mutation invalidation"
            ( Disappearance.prepareAlignmentMutation
                fixtureProbe
                mutation
                witness
                state0
            )
        (replayed, repeatedIntention) =
          commitChecked
            "alignment mutation replay"
            ( Disappearance.prepareAlignmentMutation
                fixtureProbe
                mutation
                witness
                invalidated
            )
    assertBool ("mutation retained an intention: " <> show mutation) (isJust intention)
    assertEqual "equal mutation returns the retained intention" intention repeatedIntention
    assertEqual "equal mutation is state-identical" invalidated replayed
    assertEqual "one invalidation intention is retained" 1 (length (Disappearance.invalidationIntentions invalidated))
    assertStateValid "alignment mutation leaf" invalidated

caseBlockerClasses :: Assertion
caseBlockerClasses = do
  forM_ [fixtureControlledSubject, fixtureRegularSubject] $ \subject ->
    forM_ commonBlockerClasses (exerciseBlocker subject)
  forM_ regularOnlyBlockerClasses (exerciseBlocker fixtureRegularSubject)
  forM_ regularOnlyBlockerClasses $ \blockerClass -> do
    let witness =
          Protocol.disappearanceBlockerWitness
            blockerClass
            (fixtureEvidenceDigest (fromIntegral (fromEnum blockerClass + 80)))
    assertLeftEqual
      ("controlled subject rejects regular-only blocker " <> show blockerClass)
      ( Disappearance.DisappearanceBlockerNotApplicable
          fixtureControlledSubject
          blockerClass
      )
      ( Disappearance.prepareProjectedOpen
          fixtureProjectedProbe
          ( fixtureEvidenceSnapshot
              fixtureControlledSubject
              fixturePublicationPrefix
              (incomingAlignmentCuts fixtureH1)
              (outgoingAlignmentCuts fixtureH1)
              [witness]
              []
          )
          (Disappearance.initialState fixtureH1)
      )
  assertBool
    "publisher-retained controlled capability is deliberately not a blocker arm"
    ( all
        (not . isInfixOf "PublisherRetainedCapability" . show)
        ([minBound .. maxBound] :: [Protocol.DisappearanceBlockerClass])
    )
  where
    exerciseBlocker subject blockerClass = do
      let witness =
            Protocol.disappearanceBlockerWitness
              blockerClass
              (fixtureEvidenceDigest (fromIntegral (fromEnum blockerClass + 80)))
          blockedNetwork =
            prepareCutNetworkWithBlockers
              subject
              (Map.singleton fixtureH1 [witness])
          blocked = completeLocalCuts fixtureMembers fixtureH1 blockedNetwork
      assertReportBlocked ("captured blocker " <> show blockerClass) blocked
      let (unblocked, noInvalidation) =
            commitChecked
              "remove captured blocker"
              (Disappearance.prepareBlockerObservation fixtureProbe [] blocked)
      assertEqual "removing a captured blocker is not contradictory" Nothing noInvalidation
      case snd
        ( commitChecked
            "report after blocker removal"
            (prepareFixtureReport unblocked)
        ) of
        Disappearance.LocalReportPrepared {} -> pure ()
        other -> assertFailure ("removed blocker still prevented Report: " <> show other)
      let clearNetwork = prepareCutNetwork subject
          clear = completeLocalCuts fixtureMembers fixtureH1 clearNetwork
          (contradicted, invalidation) =
            commitChecked
              "introduce blocker after capture"
              ( Disappearance.prepareBlockerObservation
                  fixtureProbe
                  [witness]
                  clear
              )
      assertBool "new blocker retains an invalidation intention" (isJust invalidation)
      assertReportBlocked ("introduced blocker " <> show blockerClass) contradicted

caseEvidenceObservationWorkAccounting :: Assertion
caseEvidenceObservationWorkAccounting = do
  let network = prepareCutNetwork fixtureControlledSubject
      state0 = lookupMap "owner evidence work state" fixtureH1 (cutStates network)
      blockers =
        [ Protocol.disappearanceBlockerWitness
            Protocol.VisibleApplicationCopyBlocker
            (fixtureEvidenceDigest 151),
          Protocol.disappearanceBlockerWitness
            Protocol.HeldPublicationBlocker
            (fixtureEvidenceDigest 152),
          Protocol.disappearanceBlockerWitness
            Protocol.PeerOutboxBlocker
            (fixtureEvidenceDigest 153)
        ]
      evidenceSnapshot retainedBlockers matching =
        fixtureEvidenceSnapshot
          fixtureControlledSubject
          fixturePublicationPrefix
          (incomingAlignmentCuts fixtureH1)
          (outgoingAlignmentCuts fixtureH1)
          retainedBlockers
          matching
      introducedPrepared =
        checked
          "prepare a simultaneous three-fact owner evidence observation"
          ( Disappearance.prepareEvidenceObservation
              fixtureProbe
              (evidenceSnapshot blockers [])
              state0
          )
      preparedIntentions = Disappearance.preparedDisappearanceOutput introducedPrepared
      (introduced, introducedIntentions) =
        Disappearance.commitDisappearanceTransition introducedPrepared
      (removed, removalIntentions) =
        checked
          "coordinate simultaneous owner blocker removal"
          ( DisappearanceUseCase.coordinateEvidenceObservation
              fixtureProbe
              (evidenceSnapshot [] [])
              introduced
          )
      (removalReplay, removalReplayIntentions) =
        checked
          "replay the exact clear owner evidence snapshot"
          ( DisappearanceUseCase.coordinateEvidenceObservation
              fixtureProbe
              (evidenceSnapshot [] [])
              removed
          )
      matchingObservation =
        Protocol.matchingPublicationObservation
          fixtureControlledSubject
          (fixtureHeraldPublicationPosition 8)
          (fixtureEvidenceDigest 154)
      (matched, matchingIntentions) =
        checked
          "coordinate one post-cut matching owner observation"
          ( DisappearanceUseCase.coordinateEvidenceObservation
              fixtureProbe
              (evidenceSnapshot [] [matchingObservation])
              state0
          )
      (matchingReplay, matchingReplayIntentions) =
        checked
          "replay the exact matching owner evidence snapshot"
          ( DisappearanceUseCase.coordinateEvidenceObservation
              fixtureProbe
              (evidenceSnapshot [] [matchingObservation])
              matched
          )
      ledger = Disappearance.disappearanceWorkLedger
      logicalWork = Disappearance.disappearanceLogicalWork . ledger
  assertEqual "prepare exposes the exact eventual intention output" preparedIntentions introducedIntentions
  assertEqual "three simultaneous blocker facts share one stable Invalidate" 1 (length introducedIntentions)
  assertEqual
    "three introduced owner facts charge exactly three blocker observations"
    3
    ( (ledger introduced).blockerObservations
        - (ledger state0).blockerObservations
    )
  assertEqual
    "their first contradiction charges one stable Invalidate creation"
    1
    ( (ledger introduced).invalidationIntentionCreations
        - (ledger state0).invalidationIntentionCreations
    )
  assertEqual
    "three introduced facts plus their shared Invalidate charge four work units"
    4
    (logicalWork introduced - logicalWork state0)
  assertEqual "removal reoffers the same retained Invalidate" introducedIntentions removalIntentions
  assertEqual
    "three removed owner facts charge exactly three blocker observations"
    3
    ( (ledger removed).blockerObservations
        - (ledger introduced).blockerObservations
    )
  assertEqual
    "blocker removal creates no further Invalidate"
    0
    ( (ledger removed).invalidationIntentionCreations
        - (ledger introduced).invalidationIntentionCreations
    )
  assertEqual
    "three removed facts charge three work units"
    3
    (logicalWork removed - logicalWork introduced)
  assertEqual "exact snapshot replay reoffers the same stable Invalidate" removalIntentions removalReplayIntentions
  assertEqual "exact clear snapshot replay is state-identical" removed removalReplay
  assertEqual
    "exact clear snapshot replay charges no work"
    0
    (logicalWork removalReplay - logicalWork removed)
  assertEqual "one matching fact retains one Invalidate" 1 (length matchingIntentions)
  assertEqual
    "one matching fact charges its observation and stable Invalidate (2*W)"
    2
    (logicalWork matched - logicalWork state0)
  assertEqual "exact matching snapshot replay reoffers the same stable Invalidate" matchingIntentions matchingReplayIntentions
  assertEqual "exact matching snapshot replay is state-identical" matched matchingReplay
  assertEqual
    "exact matching snapshot replay charges no work"
    0
    (logicalWork matchingReplay - logicalWork matched)
  assertStateValid "multi-fact owner evidence introduction and removal" removalReplay
  assertStateValid "matching owner evidence replay" matchingReplay

commonBlockerClasses :: [Protocol.DisappearanceBlockerClass]
commonBlockerClasses =
  [ Protocol.VisibleApplicationCopyBlocker,
    Protocol.UnsequencedPublicationBlocker,
    Protocol.HeldPublicationBlocker,
    Protocol.PeerInboxBlocker,
    Protocol.PeerOutboxBlocker,
    Protocol.RetainedPublicationStageBlocker,
    Protocol.AlignmentDebtBlocker,
    Protocol.AlignmentObligationBlocker,
    Protocol.AlignmentAttemptBlocker,
    Protocol.AlignmentSubscriptionBlocker,
    Protocol.AlignmentSnapshotBlocker,
    Protocol.AlignmentChangeLogBlocker,
    Protocol.AlignmentCertificateBlocker,
    Protocol.UncertainSemanticPayloadBlocker
  ]

regularOnlyBlockerClasses :: [Protocol.DisappearanceBlockerClass]
regularOnlyBlockerClasses =
  [ Protocol.RetainedStoreValueBlocker,
    Protocol.CurrentControlledUseBlocker,
    Protocol.ObsoleteControlledUseBlocker,
    Protocol.ActiveNablaBlocker,
    Protocol.PassiveNablaBlocker,
    Protocol.ActiveDeltaBlocker,
    Protocol.PassiveDeltaBlocker,
    Protocol.UnresolvedStructuralReferenceBlocker,
    Protocol.PublicationDependencyHoldBlocker,
    Protocol.RouteDependencyBlocker,
    Protocol.PlacementDependencyBlocker
  ]

fixtureHeraldPublicationPosition :: Word64 -> HeraldPublicationPosition
fixtureHeraldPublicationPosition raw =
  checked
    ("fixture Herald publication position " <> show raw)
    (mkHeraldPublicationPosition raw)

requiredMatchingWriteGate ::
  String ->
  DisappearanceProbeId ->
  Disappearance.State ->
  Disappearance.MatchingWriteGate
requiredMatchingWriteGate context identifier state =
  case Disappearance.matchingWriteGateForProbe identifier state of
    Nothing -> error (context <> ": matching-write gate is absent")
    Just gate -> gate

caseMatchingWork :: Assertion
caseMatchingWork = do
  let empty = Disappearance.initialState fixtureH1
  assertEqual
    "an empty leaf cannot supply a gate for unrelated matching work"
    Nothing
    (Disappearance.matchingWriteGateForProbe fixtureProbe empty)
  assertEqual "an unrelated empty leaf still does zero work" 0 (logicalWork empty)
  let network = prepareCutNetwork fixtureControlledSubject
      state0 = lookupMap "matching work state" fixtureH1 (cutStates network)
      gate = requiredMatchingWriteGate "matching work" fixtureProbe state0
      beforeCut = fixtureHeraldPublicationPosition 6
      atCut = fixtureHeraldPublicationPosition 7
      afterCut = fixtureHeraldPublicationPosition 8
      irrelevantObservation =
        Protocol.matchingPublicationObservation
          fixtureRegularSubject
          afterCut
          (fixtureEvidenceDigest 111)
  assertLeftEqual
    "a gate cannot classify work for another subject"
    ( Disappearance.DisappearanceMatchingPublicationSubjectMismatch
        fixtureProbe
        fixtureControlledSubject
        fixtureRegularSubject
    )
    (Disappearance.prepareMatchingPublicationObservation gate irrelevantObservation state0)
  forM_ [beforeCut, atCut] $ \position ->
    assertLeftEqual
      "at-or-before-cut work cannot be classified as a matching invalidation"
      ( Disappearance.DisappearanceMatchingPublicationAtOrBeforeCut
          fixtureProbe
          position
          fixturePublicationPrefix
      )
      ( Disappearance.prepareMatchingPublicationObservation
          gate
          ( Protocol.matchingPublicationObservation
              fixtureControlledSubject
              position
              (fixtureEvidenceDigest 112)
          )
          state0
      )
  let observation =
        Protocol.matchingPublicationObservation
          fixtureControlledSubject
          afterCut
          (fixtureEvidenceDigest 112)
      (invalidated, intentions) =
        checked
          "coordinate matching publication invalidation"
          ( DisappearanceUseCase.coordinateMatchingPublicationObservation
              gate
              observation
              state0
          )
      (replayed, repeatedIntentions) =
        checked
          "coordinate matching publication replay"
          ( DisappearanceUseCase.coordinateMatchingPublicationObservation
              gate
              observation
              invalidated
          )
  assertEqual
    "the matching observation retains its exact accepted Herald position"
    afterCut
    (Protocol.matchingPublicationPosition observation)
  assertEqual "one matching probe yields one intention" 1 (length intentions)
  assertEqual
    "one fresh exact-subject witness contributes observation plus stable Invalidate (2*W)"
    2
    (logicalWork invalidated - logicalWork state0)
  assertEqual "replay emits no second intention" [] repeatedIntentions
  assertEqual "matching replay is state-identical" invalidated replayed
  assertEqual "equal witness replay contributes zero work" 0 (logicalWork replayed - logicalWork invalidated)
  where
    logicalWork =
      Disappearance.disappearanceLogicalWork
        . Disappearance.disappearanceWorkLedger

caseReportThenInvalidate :: Assertion
caseReportThenInvalidate = do
  let network = prepareCutNetwork fixtureControlledSubject
      (reported, report) = completeLocalEvidence fixtureMembers fixtureH1 network
      claim = Protocol.localAbsenceReportClaim report
      (reportProjected, _) =
        commitChecked
          "project H1's already-sent Report"
          (Disappearance.prepareProjectedReport claim reported)
      gate = requiredMatchingWriteGate "post-Report matching work" fixtureProbe reportProjected
      matchingWitness = fixtureEvidenceDigest 121
      (contradicted, newIntentions) =
        commitChecked
          "matching work after Report"
          ( Disappearance.prepareMatchingPublicationObservation
              gate
              ( Protocol.matchingPublicationObservation
                  fixtureControlledSubject
                  (fixtureHeraldPublicationPosition 8)
                  matchingWitness
              )
              reportProjected
          )
      intention = case newIntentions of
        [retained] -> retained
        other -> error ("expected one post-Report Invalidate intention, got " <> show other)
      retainedWitness =
        maybe
          (error "post-Report probe disappeared")
          id
          (Disappearance.probeWitness fixtureProbe contradicted)
  assertEqual "the Report remains immutable historical cut evidence" (Just report) retainedWitness.witnessReport
  assertEqual "the sent Report intention is settled" [] (Disappearance.reportIntentions contradicted)
  assertEqual "the later contradiction is retained independently" [intention] (Disappearance.invalidationIntentions contradicted)
  let blocker =
        Protocol.disappearanceBlockerWitness
          Protocol.HeldPublicationBlocker
          (fixtureEvidenceDigest 122)
      (blockedAfterReport, blockerIntention) =
        commitChecked
          "blocker introduced after Report"
          ( Disappearance.prepareBlockerObservation
              fixtureProbe
              [blocker]
              reportProjected
          )
      blockedWitness =
        maybe
          (error "post-Report blocker probe disappeared")
          id
          (Disappearance.probeWitness fixtureProbe blockedAfterReport)
  assertBool "a post-Report blocker also retains Invalidate" (isJust blockerIntention)
  assertEqual "blocker invalidation cannot erase historical Report" (Just report) blockedWitness.witnessReport
  let (oracleAfterReport, reportOutcome) =
        submitLocalReport
          (fixtureRequestId fixtureH1 2)
          Nothing
          fixtureH1
          report
          (cutOracle network)
  case reportOutcome of
    Target.TargetSubmissionCommitted {} -> pure ()
    other -> assertFailure ("Target rejected the preceding Report: " <> show other)
  let (_, invalidationOutcome) =
        submitInvalidationIntention
          (fixtureRequestId fixtureH1 3)
          Nothing
          intention
          oracleAfterReport
      (terminalIndex, targetCause) = case invalidationOutcome of
        Target.TargetSubmissionCommitted receipt [Target.TargetInvalidatedProjected probe cause]
          | probe == fixtureProbe -> (Target.targetReceiptControlIndex receipt, cause)
        other -> error ("Target rejected generated Invalidate: " <> show other)
  assertEqual
    "Target projects the generated reporter/reason/witness"
    ( Target.TargetCommandInvalidation
        fixtureH1
        Target.TargetMatchingPublicationObserved
        matchingWitness
    )
    targetCause
  let terminal =
        Protocol.ProjectedDisappearanceInvalidated
          fixtureProbe
          ( Protocol.ProjectedCommandInvalidation
              fixtureH1
              Protocol.MatchingPublicationObserved
              matchingWitness
          )
          terminalIndex
      terminalH1 = installOne terminal contradicted
      h2WithReport =
        fst
          ( commitChecked
              "project Report at peer-only H2"
              ( Disappearance.prepareProjectedReport
                  claim
                  (lookupMap "post-Report H2" fixtureH2 (cutStates network))
              )
          )
      terminalH2 = installOne terminal h2WithReport
  assertEqual "Invalidate parks H1's candidate" [False] (candidateEligibility terminalH1)
  assertEqual "Invalidate invents no peer-only candidate" [] (candidateEligibility terminalH2)
  forM_ [terminalH1, terminalH2] (assertStateValid "post-Report Invalidate terminal")
  where
    installOne terminal state =
      fst
        ( commitExpected
            "install Target Invalidate projection"
            Disappearance.TerminalInstalled
            (Disappearance.prepareProjectedTerminal terminal state)
        )
    candidateEligibility state =
      fmap (\candidate -> candidate.candidateEligible) (Disappearance.candidateWitnesses state)

assertReportBlocked :: String -> Disappearance.State -> Assertion
assertReportBlocked context state =
  case prepareFixtureReport state of
    Left (Disappearance.DisappearanceReportBlocked identifier) ->
      assertEqual (context <> ": blocked the expected probe") fixtureProbe identifier
    other -> assertFailure (context <> ": expected ReportBlocked, got " <> showPrepared other)

showPrepared ::
  (Show problem, Show output) =>
  Either problem (Disappearance.PreparedDisappearanceTransition output) ->
  String
showPrepared result = case result of
  Left problem -> "Left " <> show problem
  Right prepared ->
    "Right " <> show (Disappearance.preparedDisappearanceOutput prepared)

caseReportProjectionPermutations :: Assertion
caseReportProjectionPermutations = do
  let network = prepareCutNetwork fixtureControlledSubject
      orders = permutations fixtureMembers
      completedRuns =
        [ (order, completeNetworkEvidence order network)
        | order <- orders
        ]
  case completedRuns of
    [] -> assertFailure "the three-member permutation set is unexpectedly empty"
    (_, (baselineStates, baselineReports)) : remaining -> do
      forM_ remaining $ \(order, (states, reports)) -> do
        assertEqual
          ("marker arrival order converges at all Heralds: " <> show order)
          baselineStates
          states
        assertEqual
          ("marker arrival order derives the same reports: " <> show order)
          (fmap Protocol.localAbsenceReportCanonicalBytes baselineReports)
          (fmap Protocol.localAbsenceReportCanonicalBytes reports)
      let projectedRuns =
            [ (order, projectClaimsEverywhere order baselineReports baselineStates)
            | order <- orders
            ]
      case projectedRuns of
        [] -> assertFailure "the report permutation set is unexpectedly empty"
        (_, baselineProjected) : projectedRemaining ->
          forM_ projectedRemaining $ \(order, states) ->
            assertEqual
              ("Report projection order converges at all Heralds: " <> show order)
              baselineProjected
              states
      let oracleRuns =
            [ ( order,
                prospectiveOracleState
                  (fst (submitReportsToOracle order baselineReports (cutOracle network)))
              )
            | order <- orders
            ]
      case oracleRuns of
        [] -> assertFailure "the Oracle report permutation set is unexpectedly empty"
        (_, baselineOracle) : oracleRemaining ->
          forM_ oracleRemaining $ \(order, oracle) -> do
            assertEqual
              ("Target report set is order-independent: " <> show order)
              (Target.targetProbe fixtureProbe baselineOracle)
              (Target.targetProbe fixtureProbe oracle)
            assertEqual
              "every ordering consumes the same number of control entries"
              (Target.targetGreatestControlIndex baselineOracle)
              (Target.targetGreatestControlIndex oracle)

caseSelfSufficientTargetProjection :: Assertion
caseSelfSufficientTargetProjection = do
  let network = prepareCutNetwork fixtureControlledSubject
      peer = fixtureH2
      projectedPeer = lookupMap "self-sufficient projected peer" peer (cutStates network)
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      (reportedOracle, reportSubmissions) =
        submitReportsToOracle fixtureMembers reports (cutOracle network)
      projectedClaims =
        fmap
          (checked "translate self-sufficient Target Report" . projectedClaimFromReport)
          reportSubmissions
      peerWithLocalReport = lookupMap "self-sufficient reported peer" peer reportedStates
      resolvedPeer =
        foldl
          ( \state claim ->
              fst
                ( commitChecked
                    "project self-sufficient Target Report"
                    (Disappearance.prepareProjectedReport claim state)
                )
          )
          peerWithLocalReport
          projectedClaims
      expectedClaims =
        fmap
          (Protocol.localAbsenceReportClaim . (reports Map.!))
          fixtureMembers
  assertEqual
    "the Open event installs a peer participant without inventing a candidate"
    []
    (Disappearance.candidateWitnesses projectedPeer)
  assertEqual
    "Report events carry every checked claim without a home-side lookup"
    (fmap disappearanceEvidenceClaimCanonicalBytes expectedClaims)
    (fmap disappearanceEvidenceClaimCanonicalBytes projectedClaims)
  assertEqual
    "event-only Report projection creates one complete Resolve intention"
    1
    (length (Disappearance.resolveIntentions resolvedPeer))
  assertBool
    "the Target reaches the same complete probe"
    (isJust (Target.targetProbe fixtureProbe (prospectiveOracleState reportedOracle)))
  assertStateValid "self-sufficient Target projection peer" resolvedPeer

caseBothSubjectArms :: Assertion
caseBothSubjectArms =
  forM_ [fixtureControlledSubject, fixtureRegularSubject] $ \subject -> do
    let network = prepareCutNetwork subject
        (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
        projectedStates =
          projectClaimsEverywhere fixtureMembers reports reportedStates
        intentionsByHerald = fmap Disappearance.resolveIntentions projectedStates
    assertBool
      "each captured Herald independently retains the same complete Resolve intention"
      ( case Map.elems intentionsByHerald of
          [first, second, thirdIntention] ->
            length first == 1 && first == second && second == thirdIntention
          _ -> False
      )
    let intention = case lookupMap "H1 Resolve intention" fixtureH1 intentionsByHerald of
          [retained] -> retained
          other -> error ("expected one H1 Resolve intention, got " <> show other)
        (reportedOracle, _) =
          submitReportsToOracle fixtureMembers reports (cutOracle network)
        (_, resolveOutcome) =
          submitResolveIntention
            (fixtureRequestId fixtureH1 3)
            Nothing
            fixtureH1
            intention
            reportedOracle
        (index, outcome) = targetResolvedOutcome resolveOutcome
        terminal = Protocol.ProjectedDisappearanceResolved fixtureProbe outcome index
    assertEqual "Resolve retains the exact subject arm" subject (disappearanceResolutionSubject outcome)
    forM_ (Map.toAscList projectedStates) $ \(local, state) -> do
      let (terminalState, disposition) =
            commitExpected
              "install projected Resolve terminal"
              Disappearance.TerminalInstalled
              (Disappearance.prepareProjectedTerminal terminal state)
          (replayed, replayDisposition) =
            commitExpected
              "replay projected Resolve terminal"
              Disappearance.TerminalDuplicate
              (Disappearance.prepareProjectedTerminal terminal terminalState)
      assertEqual ("terminal replay is state-identical at " <> show local) terminalState replayed
      assertEqual "fresh terminal installs" Disappearance.TerminalInstalled disposition
      assertEqual "terminal replay deduplicates" Disappearance.TerminalDuplicate replayDisposition
      assertEqual "Resolve consumes any local candidate" [] (Disappearance.candidateWitnesses terminalState)
      assertStateValid "resolved terminal leaf" terminalState

caseRegularResolutionAuthority :: Assertion
caseRegularResolutionAuthority = do
  let network = prepareCutNetwork fixtureRegularSubject
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      projectedStates =
        projectClaimsEverywhere fixtureMembers reports reportedStates
      reportedPredecessor =
        lookupMap "regular retirement H1 predecessor" fixtureH1 projectedStates
      refreshedPublicationPrefix =
        HeraldPublicationPrefixThrough
          (checked "refreshed Herald publication position" (mkHeraldPublicationPosition 8))
      refreshedEvidenceSnapshot =
        fixtureEvidenceSnapshot
          fixtureRegularSubject
          refreshedPublicationPrefix
          (incomingAlignmentCuts fixtureH1)
          (outgoingAlignmentCuts fixtureH1)
          []
          []
      (predecessor, refreshIntentions) =
        commitChecked
          "refresh evidence after an unrelated publication"
          ( Disappearance.prepareEvidenceObservation
              fixtureProbe
              refreshedEvidenceSnapshot
              reportedPredecessor
          )
      resolve = case Disappearance.resolveIntentions predecessor of
        [retained] -> retained
        other -> error ("expected one regular Resolve intention, got " <> show other)
      (reportedOracle, _) =
        submitReportsToOracle fixtureMembers reports (cutOracle network)
      (_, resolveSubmission) =
        submitResolveIntention
          (fixtureRequestId fixtureH1 3)
          Nothing
          fixtureH1
          resolve
          reportedOracle
      (resolveIndex, outcome) = targetResolvedOutcome resolveSubmission
      terminal =
        Protocol.ProjectedDisappearanceResolved fixtureProbe outcome resolveIndex
      expectedCoordinate =
        disappearanceSubjectMembershipCoordinate
          fixtureRegularSubject
          fixtureMembership
      expectedEvidenceSnapshot =
        refreshedEvidenceSnapshot
      prepared =
        checked
          "prepare fresh regular retirement"
          (Disappearance.prepareProjectedRegularResolution terminal predecessor)
      authority = case Disappearance.preparedProjectedRegularResolutionAuthority prepared of
        Just retained -> retained
        Nothing -> error "fresh regular retirement carried no authority"

  assertEqual
    "fresh regular Resolve is classified fresh"
    Disappearance.ProjectedRegularResolutionFresh
    (Disappearance.preparedProjectedRegularResolutionDisposition prepared)
  assertEqual
    "an unrelated publication refresh creates no disappearance intention"
    []
    refreshIntentions
  assertEqual "authority binds the exact probe" fixtureProbe (Disappearance.regularRetirementAuthorityProbe authority)
  assertEqual "authority binds the exact subject" fixtureRegularSubject (Disappearance.regularRetirementAuthoritySubject authority)
  assertEqual "authority binds the exact coordinate" expectedCoordinate (Disappearance.regularRetirementAuthorityCoordinate authority)
  assertEqual "authority binds the exact Resolve index" resolveIndex (Disappearance.regularRetirementAuthorityResolveIndex authority)
  assertEqual
    "authority retains the latest refreshed evidence snapshot"
    expectedEvidenceSnapshot
    (Disappearance.regularRetirementAuthorityEvidenceSnapshot authority)
  assertEqual
    "authority separately freezes the Open-time local publication cut"
    fixturePublicationPrefix
    (Disappearance.regularRetirementAuthorityLocalPublicationCut authority)
  case (disappearanceSubjectView fixtureRegularSubject, disappearanceResolutionOutcomeView outcome) of
    ( RegularSortDefinitionSubjectView expectedSort expectedDigest expectedOccurrence,
      RegularSortDefinitionRetired actualSort actualDigest actualOccurrence actualIndex successorOccurrence
      ) -> do
        assertEqual "outcome retains the Resolve index" resolveIndex actualIndex
        assertEqual "authority binds the regular sort" expectedSort (Disappearance.regularRetirementAuthoritySortId authority)
        assertEqual "authority binds the descriptor digest" expectedDigest (Disappearance.regularRetirementAuthorityDescriptorDigest authority)
        assertEqual "authority binds the old occurrence" expectedOccurrence (Disappearance.regularRetirementAuthorityOccurrence authority)
        assertEqual "outcome sort agrees with its subject" expectedSort actualSort
        assertEqual "outcome digest agrees with its subject" expectedDigest actualDigest
        assertEqual "outcome occurrence agrees with its subject" expectedOccurrence actualOccurrence
        assertEqual
          "authority binds the resolve-derived successor occurrence"
          successorOccurrence
          (Disappearance.regularRetirementAuthoritySuccessorOccurrence authority)
    views -> assertFailure ("regular retirement changed arm: " <> show views)

  let (terminalState, disposition, committedAuthority) =
        Disappearance.commitProjectedRegularResolution prepared
  assertEqual "fresh regular terminal commits" Disappearance.ProjectedRegularResolutionFresh disposition
  assertEqual "commit returns the same checked authority" (Just authority) committedAuthority
  assertStateValid "committed regular retirement leaf" terminalState

  let replayPrepared =
        checked
          "prepare exact regular retirement replay"
          (Disappearance.prepareProjectedRegularResolution terminal terminalState)
      (replayed, replayDisposition, replayAuthority) =
        Disappearance.commitProjectedRegularResolution replayPrepared
  assertEqual "regular terminal replay is classified duplicate" Disappearance.ProjectedRegularResolutionDuplicate replayDisposition
  assertEqual "regular terminal replay has no reusable authority" Nothing replayAuthority
  assertEqual "regular terminal replay is state-identical" terminalState replayed

  assertLeftEqual
    "generic leaf coordination requires regular-owner coordination"
    (DisappearanceUseCase.DisappearanceRegularResolutionRequiresOwnerCoordination fixtureProbe)
    (DisappearanceUseCase.coordinateProjectedTerminal terminal predecessor)

  let controlledOutcome =
        checked
          "controlled outcome for regular-resolution rejection"
          (resolveDisappearanceSubject fixtureSystemId resolveIndex fixtureControlledSubject)
      controlledTerminal =
        Protocol.ProjectedDisappearanceResolved
          fixtureProbe
          controlledOutcome
          resolveIndex
      invalidationCause =
        Protocol.ProjectedCommandInvalidation
          fixtureH1
          Protocol.MatchingPublicationObserved
          (fixtureEvidenceDigest 137)
      invalidatedTerminal =
        Protocol.ProjectedDisappearanceInvalidated
          fixtureProbe
          invalidationCause
          resolveIndex
      abortedTerminal =
        Protocol.ProjectedDisappearanceAborted
          fixtureProbe
          Protocol.ProjectedAuthorizedAbort
          resolveIndex
  assertLeftEqual
    "regular-resolution entry point rejects a controlled outcome"
    ( Disappearance.DisappearanceRegularResolutionSubjectControlled
        fixtureProbe
        fixtureControlledSubject
    )
    (Disappearance.prepareProjectedRegularResolution controlledTerminal predecessor)
  assertLeftEqual
    "regular-resolution entry point rejects Invalidate"
    (Disappearance.DisappearanceRegularResolutionTerminalInvalidated fixtureProbe)
    (Disappearance.prepareProjectedRegularResolution invalidatedTerminal predecessor)
  assertLeftEqual
    "regular-resolution entry point rejects Abort"
    (Disappearance.DisappearanceRegularResolutionTerminalAborted fixtureProbe)
    (Disappearance.prepareProjectedRegularResolution abortedTerminal predecessor)

targetResolvedOutcome ::
  Target.TargetSubmissionOutcome ->
  (ControlIndex, DisappearanceResolutionOutcome)
targetResolvedOutcome submission = case submission of
  Target.TargetSubmissionCommitted receipt [Target.TargetResolvedProjected probe outcome]
    | probe == fixtureProbe -> (Target.targetReceiptControlIndex receipt, outcome)
  other -> error ("expected one committed Resolve projection, got " <> show other)

caseTerminalCandidatePolicy :: Assertion
caseTerminalCandidatePolicy = do
  let network = prepareCutNetwork fixtureControlledSubject
      stateAt local = lookupMap "terminal policy state" local (cutStates network)
      invalidationCause =
        Protocol.ProjectedCommandInvalidation
          fixtureH1
          Protocol.MatchingPublicationObserved
          (fixtureEvidenceDigest 125)
      invalidatedTerminal =
        Protocol.ProjectedDisappearanceInvalidated
          fixtureProbe
          invalidationCause
          (controlIndex 10)
      authorizedTerminal =
        Protocol.ProjectedDisappearanceAborted
          fixtureProbe
          Protocol.ProjectedAuthorizedAbort
          (controlIndex 10)
      successor =
        checked
          "terminal-policy membership successor"
          ( retireHeraldMembershipGeneration
              (controlIndex 10)
              (fixtureRetirementResolution 10)
              fixtureH3
              fixtureMembership
          )
      supersededTerminal =
        Protocol.ProjectedDisappearanceAborted
          fixtureProbe
          (Protocol.ProjectedMembershipSuperseded successor)
          (controlIndex 10)
      wrongActiveSetPredecessor =
        checked
          "wrong-active-set predecessor"
          ( genesisHeraldMembershipGeneration
              fixtureSystemId
              (fixtureH1 :| [fixtureH2, fixtureH3, fixtureH4])
          )
      wrongActiveSetSuccessor =
        checked
          "wrong-active-set terminal successor"
          (retireHeraldMembershipGeneration (controlIndex 10) (fixtureRetirementResolution 10) fixtureH3 wrongActiveSetPredecessor)
      wrongIndexSuccessor =
        checked
          "wrong-index terminal successor"
          ( retireHeraldMembershipGeneration
              (controlIndex 11)
              (fixtureRetirementResolution 11)
              fixtureH3
              fixtureMembership
          )
      malformedTerminal malformed =
        Protocol.ProjectedDisappearanceAborted
          fixtureProbe
          (Protocol.ProjectedMembershipSuperseded malformed)
          (controlIndex 10)
  forM_ [wrongActiveSetSuccessor, wrongIndexSuccessor] $ \malformed ->
    assertLeftEqual
      "membership terminal rejects a non-exact successor body/index"
      (Disappearance.DisappearanceMembershipSuccessorMismatch fixtureProbe)
      (Disappearance.prepareProjectedTerminal (malformedTerminal malformed) (stateAt fixtureH1))
  forM_ [invalidatedTerminal, authorizedTerminal] $ \terminal -> do
    let h1State = stateAt fixtureH1
        gate = requiredMatchingWriteGate "terminal policy invalidation" fixtureProbe h1State
        h1Predecessor =
          if terminal == invalidatedTerminal
            then
              fst
                ( commitChecked
                    "retain local invalidation intention before projection"
                    ( Disappearance.prepareMatchingPublicationObservation
                        gate
                        ( Protocol.matchingPublicationObservation
                            fixtureControlledSubject
                            (fixtureHeraldPublicationPosition 8)
                            (fixtureEvidenceDigest 125)
                        )
                        h1State
                    )
                )
            else h1State
        h1Terminal = installTerminal terminal h1Predecessor
        h2Terminal = installTerminal terminal (stateAt fixtureH2)
    assertEqual "Invalidate/authorized Abort parks the existing local candidate" [False] (candidateEligibility h1Terminal)
    assertEqual "peer-only participant receives no invented candidate" [] (candidateEligibility h2Terminal)
    assertEqual "parking clears every Open intention" [] (Disappearance.openIntentions h1Terminal)
    let (notReopened, disposition) =
          commitChecked
            "parked candidate re-evaluation"
            ( Disappearance.prepareCandidateReevaluation
                (Protocol.disappearanceOpenContext fixtureMembership)
                fixtureControlledSubject
                h1Terminal
            )
    assertEqual "ordinary re-evaluation cannot rearm a parked candidate" Disappearance.CandidateNotEligible disposition
    assertEqual "ordinary re-evaluation is state-identical" h1Terminal notReopened
    let (rearmed, rearmDisposition) =
          commitChecked
            "fresh local take after terminal"
            ( Disappearance.prepareLocalTakeCandidate
                (Protocol.localTakeCandidateObservation fixtureControlledSubject)
                h1Terminal
            )
        (reopened, openDisposition) =
          commitChecked
            "explicit post-terminal re-evaluation"
            ( Disappearance.prepareCandidateReevaluation
                (Protocol.disappearanceOpenContext fixtureMembership)
                fixtureControlledSubject
                rearmed
            )
    assertEqual "fresh local take is the meaningful rearm" Disappearance.CandidateRearmed rearmDisposition
    assertEqual "only explicit rearm permits a fresh Open" Disappearance.CandidateOpenPrepared openDisposition
    assertEqual "fresh Open is retained after rearm" 1 (length (Disappearance.openIntentions reopened))
    assertStateValid "park/rearm terminal policy" reopened
  -- This pins the narrow leaf terminal law only. Increment 10 still owns the
  -- MembershipAdvance/Graph predecessor-successor and structural-base schedule.
  let h1Superseded = installTerminal supersededTerminal (stateAt fixtureH1)
      h2Superseded = installTerminal supersededTerminal (stateAt fixtureH2)
      h3Superseded = installTerminal supersededTerminal (stateAt fixtureH3)
  assertEqual "surviving local candidate is rearmed by supersession" [True] (candidateEligibility h1Superseded)
  assertEqual "surviving peer reconstructs successor candidate" [True] (candidateEligibility h2Superseded)
  assertEqual "retired Herald reconstructs no candidate" [] (candidateEligibility h3Superseded)
  forM_ [h1Superseded, h2Superseded] $ \state -> do
    let (scheduled, disposition) =
          commitChecked
            "successor candidate re-evaluation"
            ( Disappearance.prepareCandidateReevaluation
                (Protocol.disappearanceOpenContext successor)
                fixtureControlledSubject
                state
            )
    assertEqual "successor establishment permits reoffer" Disappearance.CandidateOpenPrepared disposition
    assertEqual "successor reoffer uses the new coordinate" 1 (length (Disappearance.openIntentions scheduled))
    assertStateValid "membership-superseded survivor" scheduled
  where
    installTerminal terminal state =
      let (terminalState, disposition) =
            commitExpected
              "install terminal candidate policy"
              Disappearance.TerminalInstalled
              (Disappearance.prepareProjectedTerminal terminal state)
          (replayed, replayDisposition) =
            commitExpected
              "replay terminal candidate policy"
              Disappearance.TerminalDuplicate
              (Disappearance.prepareProjectedTerminal terminal terminalState)
       in if replayDisposition == Disappearance.TerminalDuplicate && replayed == terminalState
            then terminalState
            else error ("terminal replay mutated state: " <> show disposition)
    candidateEligibility state =
      fmap (\candidate -> candidate.candidateEligible) (Disappearance.candidateWitnesses state)

caseLabelTerminalCandidateFollowThrough :: Assertion
caseLabelTerminalCandidateFollowThrough = do
  let network = prepareCutNetwork fixtureControlledSubject
      stateAt local = lookupMap "label-terminal state" local (cutStates network)
      originalObject = case disappearanceSubjectView fixtureControlledSubject of
        ControlledPredefinedSubjectView object _ _ -> object
        RegularSortDefinitionSubjectView {} -> error "controlled fixture changed subject arm"
      wrongObject =
        checked
          "wrong projected label-terminal object"
          (mkGlobalObjectId (fixtureBytes 222))
      decisionNotApplied =
        checked
          "NotApplied label decision"
          (mkLabelDecisionId (fixtureBytes 223))
      decisionReleased =
        checked
          "Released label decision"
          (mkLabelDecisionId (fixtureBytes 224))
      decisionDeleted =
        checked
          "Deleted label decision"
          (mkLabelDecisionId (fixtureBytes 225))
      unrelatedDecision =
        checked
          "unrelated label decision"
          (mkLabelDecisionId (fixtureBytes 226))
      releasedRevision =
        checked
          "released label revision"
          (mkLabelRevision (controlIndex 30))
      deletedRevision =
        checked
          "deleted label revision"
          (mkLabelRevision (controlIndex 31))
      expectedReleasedSubject =
        checked
          "revise controlled disappearance subject"
          (reviseControlledDisappearanceSubject releasedRevision fixtureControlledSubject)
      invalidated decision local =
        fst
          ( commitExpected
              "install label-caused disappearance invalidation"
              Disappearance.TerminalInstalled
              ( Disappearance.prepareProjectedTerminal
                  ( Protocol.ProjectedDisappearanceInvalidated
                      fixtureProbe
                      (Protocol.ProjectedLabelInvalidation decision originalObject)
                      (controlIndex 20)
                  )
                  (stateAt local)
              )
          )
      applyLabel projectedTerminal state =
        commitChecked
          "apply projected label terminal"
          (Disappearance.prepareProjectedLabelTerminal projectedTerminal state)
      labelTerminal decision outcome =
        Protocol.projectedLabelTerminal decision originalObject outcome
      candidateSubjects state =
        fmap (\candidate -> candidate.candidateSubject) (Disappearance.candidateWitnesses state)
      candidateEligibility state =
        fmap (\candidate -> candidate.candidateEligible) (Disappearance.candidateWitnesses state)
      candidateLabelWaits state =
        fmap (\candidate -> candidate.candidateAwaitingLabelDecision) (Disappearance.candidateWitnesses state)

  case ( disappearanceSubjectView fixtureControlledSubject,
         disappearanceSubjectView expectedReleasedSubject
       ) of
    ( ControlledPredefinedSubjectView originalObject' originalOccurrence _,
      ControlledPredefinedSubjectView revisedObject revisedOccurrence revisedLabel
      ) -> do
        assertEqual "revision helper preserves the controlled object" originalObject' revisedObject
        assertEqual "revision helper preserves the structural occurrence" originalOccurrence revisedOccurrence
        assertEqual "revision helper installs the resulting label revision" (Just releasedRevision) revisedLabel
    views -> assertFailure ("controlled revision changed subject arm: " <> show views)
  assertLeftEqual
    "label invalidation is bound to the controlled object immediately"
    ( Disappearance.DisappearanceLabelInvalidationObjectMismatch
        fixtureProbe
        wrongObject
        originalObject
    )
    ( Disappearance.prepareProjectedTerminal
        ( Protocol.ProjectedDisappearanceInvalidated
            fixtureProbe
            (Protocol.ProjectedLabelInvalidation decisionNotApplied wrongObject)
            (controlIndex 20)
        )
        (stateAt fixtureH1)
    )
  let regularState =
        lookupMap
          "regular label-invalidation state"
          fixtureH1
          (cutStates (prepareCutNetwork fixtureRegularSubject))
  assertLeftEqual
    "label invalidation cannot terminalize a regular subject"
    ( Disappearance.DisappearanceLabelInvalidationSubjectNotControlled
        fixtureProbe
        fixtureRegularSubject
    )
    ( Disappearance.prepareProjectedTerminal
        ( Protocol.ProjectedDisappearanceInvalidated
            fixtureProbe
            (Protocol.ProjectedLabelInvalidation decisionNotApplied originalObject)
            (controlIndex 20)
        )
        regularState
    )
  let notAppliedPredecessor = invalidated decisionNotApplied fixtureH1
      peerPredecessor = invalidated decisionNotApplied fixtureH2
      unrelatedTerminal =
        labelTerminal unrelatedDecision Protocol.ProjectedLabelNotApplied
      (unrelatedState, unrelatedDisposition) =
        applyLabel unrelatedTerminal notAppliedPredecessor
  assertEqual "label invalidation parks H1's retained candidate" [False] (candidateEligibility notAppliedPredecessor)
  assertEqual
    "the parked candidate retains the exact label decision"
    [Just decisionNotApplied]
    (candidateLabelWaits notAppliedPredecessor)
  assertEqual "label invalidation invents no peer-only candidate" [] (candidateEligibility peerPredecessor)
  let (earlyLocalTake, earlyLocalTakeDisposition) =
        commitChecked
          "local take while label terminal is pending"
          ( Disappearance.prepareLocalTakeCandidate
              (Protocol.localTakeCandidateObservation fixtureControlledSubject)
              notAppliedPredecessor
          )
  assertEqual
    "local take cannot bypass the required label terminal"
    Disappearance.CandidateNotEligible
    earlyLocalTakeDisposition
  assertEqual
    "local take while awaiting the label terminal is state-identical"
    notAppliedPredecessor
    earlyLocalTake
  assertEqual "another decision is unrelated" Disappearance.LabelTerminalUnrelated unrelatedDisposition
  assertEqual "an unrelated label terminal does zero leaf work" notAppliedPredecessor unrelatedState
  assertLeftEqual
    "a matching decision cannot name another object"
    ( Disappearance.DisappearanceLabelTerminalObjectMismatch
        decisionNotApplied
        wrongObject
        originalObject
    )
    ( Disappearance.prepareProjectedLabelTerminal
        (Protocol.projectedLabelTerminal decisionNotApplied wrongObject Protocol.ProjectedLabelNotApplied)
        notAppliedPredecessor
    )
  let notAppliedTerminal =
        labelTerminal decisionNotApplied Protocol.ProjectedLabelNotApplied
      (notAppliedState, notAppliedDisposition) =
        applyLabel notAppliedTerminal notAppliedPredecessor
      (notAppliedReplay, notAppliedReplayDisposition) =
        applyLabel notAppliedTerminal notAppliedState
      (peerNotApplied, peerDisposition) =
        applyLabel notAppliedTerminal peerPredecessor
  assertEqual "NotApplied settles the matching invalidation" Disappearance.LabelTerminalApplied notAppliedDisposition
  assertEqual "NotApplied rearms the exact prior subject" [fixtureControlledSubject] (candidateSubjects notAppliedState)
  assertEqual "NotApplied makes the retained candidate eligible" [True] (candidateEligibility notAppliedState)
  assertEqual "NotApplied clears the exact label wait" [Nothing] (candidateLabelWaits notAppliedState)
  assertEqual "label settlement does not schedule an Open itself" [] (Disappearance.openIntentions notAppliedState)
  assertEqual "equal label-terminal replay deduplicates" Disappearance.LabelTerminalDuplicate notAppliedReplayDisposition
  assertEqual "equal label-terminal replay is state-identical" notAppliedState notAppliedReplay
  assertEqual "a captured peer applies the terminal history" Disappearance.LabelTerminalApplied peerDisposition
  assertEqual "a captured peer still receives no invented candidate" [] (Disappearance.candidateWitnesses peerNotApplied)
  assertLeftEqual
    "the same decision cannot replay with another outcome"
    (Disappearance.DisappearanceLabelTerminalConflict decisionNotApplied)
    ( Disappearance.prepareProjectedLabelTerminal
        (labelTerminal decisionNotApplied (Protocol.ProjectedLabelReleased releasedRevision))
        notAppliedState
    )
  let (notAppliedScheduled, notAppliedScheduleDisposition) =
        commitChecked
          "explicitly re-evaluate NotApplied candidate"
          ( Disappearance.prepareCandidateReevaluation
              (Protocol.disappearanceOpenContext fixtureMembership)
              fixtureControlledSubject
              notAppliedState
          )
  assertEqual "NotApplied reopens only on explicit reevaluation" Disappearance.CandidateOpenPrepared notAppliedScheduleDisposition
  assertEqual "explicit NotApplied reevaluation retains one Open" 1 (length (Disappearance.openIntentions notAppliedScheduled))

  let releasedPredecessor = invalidated decisionReleased fixtureH1
      releasedTerminal =
        labelTerminal decisionReleased (Protocol.ProjectedLabelReleased releasedRevision)
      (releasedState, releasedDisposition) = applyLabel releasedTerminal releasedPredecessor
      (releasedReplay, releasedReplayDisposition) = applyLabel releasedTerminal releasedState
  assertEqual "Released settles the matching invalidation" Disappearance.LabelTerminalApplied releasedDisposition
  assertEqual "Released removes the old candidate key" Nothing (findCandidate fixtureControlledSubject releasedState)
  assertEqual "Released rekeys object and occurrence to the new revision" [expectedReleasedSubject] (candidateSubjects releasedState)
  assertEqual "Released rearms the rekeyed candidate" [True] (candidateEligibility releasedState)
  assertEqual "Released clears the exact label wait" [Nothing] (candidateLabelWaits releasedState)
  assertEqual "Released creates no Open without reevaluation" [] (Disappearance.openIntentions releasedState)
  assertEqual "Released replay deduplicates" Disappearance.LabelTerminalDuplicate releasedReplayDisposition
  assertEqual "Released replay is state-identical" releasedState releasedReplay
  let (releasedScheduled, releasedScheduleDisposition) =
        commitChecked
          "explicitly re-evaluate Released candidate"
          ( Disappearance.prepareCandidateReevaluation
              (Protocol.disappearanceOpenContext fixtureMembership)
              expectedReleasedSubject
              releasedState
          )
  assertEqual "Released reopens only on explicit reevaluation" Disappearance.CandidateOpenPrepared releasedScheduleDisposition
  case Disappearance.openIntentions releasedScheduled of
    [intention] ->
      assertEqual
        "Released Open uses the revised subject coordinate"
        (disappearanceSubjectMembershipCoordinate expectedReleasedSubject fixtureMembership)
        (Protocol.disappearanceOpenIntentionCoordinate intention)
    other -> assertFailure ("expected one Released Open intention, got " <> show other)

  let deletedPredecessor = invalidated decisionDeleted fixtureH1
      deletedTerminal =
        labelTerminal decisionDeleted (Protocol.ProjectedLabelDeleted deletedRevision)
      (deletedState, deletedDisposition) = applyLabel deletedTerminal deletedPredecessor
      (deletedReplay, deletedReplayDisposition) = applyLabel deletedTerminal deletedState
  assertEqual "Deleted settles the matching invalidation" Disappearance.LabelTerminalApplied deletedDisposition
  assertEqual "Deleted consumes the retained candidate" [] (Disappearance.candidateWitnesses deletedState)
  assertEqual "Deleted cannot schedule an Open in the leaf" [] (Disappearance.openIntentions deletedState)
  assertEqual "Deleted replay deduplicates" Disappearance.LabelTerminalDuplicate deletedReplayDisposition
  assertEqual "Deleted replay is state-identical" deletedState deletedReplay

  -- A local take can happen before an already-open label fence closes, before
  -- any disappearance probe exists. Its terminal follows the same candidate
  -- lifecycle without inventing an Oracle invalidation or a peer candidate.
  forM_
    [ (decisionNotApplied, Protocol.ProjectedLabelNotApplied, [fixtureControlledSubject]),
      (decisionReleased, Protocol.ProjectedLabelReleased releasedRevision, [expectedReleasedSubject]),
      (decisionDeleted, Protocol.ProjectedLabelDeleted deletedRevision, [])
    ]
    $ \(decision, outcome, expectedSubjects) -> do
      let (taken, _) =
            commitChecked
              "take before an active label closes its gate"
              (Disappearance.prepareLocalTakeCandidate (Protocol.localTakeCandidateObservation fixtureControlledSubject) (Disappearance.initialState fixtureH1))
          (waiting, _) =
            commitChecked
              "retain the projected label wait without a probe"
              (Disappearance.prepareCandidateLabelWait decision originalObject fixtureControlledSubject taken)
          (sameWait, _) =
            commitChecked
              "repeat exact projected label wait"
              (Disappearance.prepareCandidateLabelWait decision originalObject fixtureControlledSubject waiting)
          terminal = labelTerminal decision outcome
          (settled, disposition) = applyLabel terminal waiting
          (replay, replayDisposition) = applyLabel terminal settled
      assertEqual "candidate wait has no projected probes" [] (Disappearance.probeWitnesses waiting)
      assertEqual "candidate wait emits no Open" [] (Disappearance.openIntentions waiting)
      assertEqual "candidate-only wait is exact-replay inert" waiting sameWait
      assertEqual "candidate-only terminal applies" Disappearance.LabelTerminalApplied disposition
      assertEqual "candidate-only terminal consumes or rekeys exact take" expectedSubjects (candidateSubjects settled)
      assertEqual "candidate-only terminal clears wait" (fmap (const Nothing) expectedSubjects) (candidateLabelWaits settled)
      assertEqual "candidate-only terminal replay deduplicates" Disappearance.LabelTerminalDuplicate replayDisposition
      assertEqual "candidate-only terminal replay is state-identical" settled replay
      mapM_ (assertStateValid "candidate-only label wait lifecycle") [taken, waiting, sameWait, settled, replay]

  case reviseControlledDisappearanceSubject releasedRevision fixtureRegularSubject of
    Left _ -> pure ()
    Right subject ->
      assertFailure ("regular subject accepted controlled revision: " <> show subject)
  forM_
    [ notAppliedState,
      notAppliedReplay,
      peerNotApplied,
      notAppliedScheduled,
      releasedState,
      releasedReplay,
      releasedScheduled,
      deletedState,
      deletedReplay
    ]
    (assertStateValid "label-terminal candidate follow-through")
  where
    findCandidate subject state =
      case filter (\candidate -> candidate.candidateSubject == subject) (Disappearance.candidateWitnesses state) of
        [] -> Nothing
        candidate : _ -> Just candidate

caseSuccessfulWorkBound :: Assertion
caseSuccessfulWorkBound = do
  let network = prepareCutNetwork fixtureControlledSubject
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      completeStates = projectClaimsEverywhere fixtureMembers reports reportedStates
      ledgerTotals = fmap Disappearance.disappearanceWorkLedger (Map.elems completeStates)
      actual =
        sum
          [ Disappearance.disappearanceLogicalWork
              (Disappearance.disappearanceWorkLedger state)
          | state <- Map.elems completeStates
          ]
      dimensions = Disappearance.disappearanceWorkDimensions 3 6 3 0
      exactOneOpener = 2 + 5 * 3 + 4 * 6 + 3 * 3
      worstCaseBound = Disappearance.disappearanceSuccessfulWorkBound dimensions
      sumField field = sum (fmap field ledgerTotals)
  assertEqual "the one-opener exact formula is pinned" 50 exactOneOpener
  assertEqual "the worst-case three-opener bound is pinned" 54 worstCaseBound
  assertEqual "the pre-terminal one-opener cut/evidence schedule is exact" exactOneOpener actual
  assertBool "the one-opener schedule remains below the normative bound" (actual <= worstCaseBound)
  assertEqual "one explicit local-take candidate" 1 (sumField (\ledger -> ledger.candidateCreations))
  assertEqual "one stable Open intention" 1 (sumField (\ledger -> ledger.openIntentionCreations))
  assertEqual "all three projections install" 3 (sumField (\ledger -> ledger.projectedOpenInstallations))
  assertEqual "six directed markers are assigned" 6 (sumField (\ledger -> ledger.publicationMarkerAssignments))
  assertEqual "six directed markers are received" 6 (sumField (\ledger -> ledger.publicationMarkerReceipts))
  assertEqual "six directed markers complete" 6 (sumField (\ledger -> ledger.publicationMarkerCompletions))
  assertEqual "three local source cuts complete" 3 (sumField (\ledger -> ledger.localPublicationCutCompletions))
  assertEqual "three alignment markers are assigned" 3 (sumField (\ledger -> ledger.alignmentMarkerAssignments))
  assertEqual "three alignment markers are received" 3 (sumField (\ledger -> ledger.alignmentMarkerReceipts))
  assertEqual "three alignment markers complete" 3 (sumField (\ledger -> ledger.alignmentMarkerCompletions))
  assertEqual "one local Report per member" 3 (sumField (\ledger -> ledger.reportCreations))
  assertEqual "every Report projects to every member" 9 (sumField (\ledger -> ledger.projectedReportInstallations))
  assertEqual "one stable Resolve intention per member" 3 (sumField (\ledger -> ledger.resolveIntentionCreations))
  assertEqual "the measured family stops before terminals" 0 (sumField (\ledger -> ledger.terminalInstallations))

caseRacingOpenersWorkBound :: Assertion
caseRacingOpenersWorkBound = do
  let network =
        prepareCutNetworkWithOpeners
          fixtureControlledSubject
          Map.empty
          fixtureMembers
      (reportedStates, reports) = completeNetworkEvidence fixtureMembers network
      completeStates = projectClaimsEverywhere fixtureMembers reports reportedStates
      actual =
        sum
          [ Disappearance.disappearanceLogicalWork
              (Disappearance.disappearanceWorkLedger state)
          | state <- Map.elems completeStates
          ]
      bound =
        Disappearance.disappearanceSuccessfulWorkBound
          (Disappearance.disappearanceWorkDimensions 3 6 3 0)
      openResults = fmap projectedOpenResult (cutOpenOutcomes network)
  assertEqual
    "one racing Open wins and the other homes alias its exact probe"
    [ OpenedDisappearanceProbe fixtureProbe,
      AliasedDisappearanceProbe fixtureProbe,
      AliasedDisappearanceProbe fixtureProbe
    ]
    openResults
  assertEqual "all-M racing openers exactly reach the normative bound" bound actual
  forM_ fixtureMembers $ \local -> do
    let state = lookupMap "racing-opener projection state" local (cutStates network)
        (replayed, output) =
          commitChecked
            "aliased Open projection replay"
            ( Disappearance.prepareProjectedOpen
                (cutProjectedProbe network)
                ( fixtureEvidenceSnapshot
                    fixtureControlledSubject
                    fixturePublicationPrefix
                    (incomingAlignmentCuts local)
                    (outgoingAlignmentCuts local)
                    []
                    []
                )
                state
            )
    assertEqual "aliased projection is classified duplicate" Disappearance.ProjectedOpenDuplicate output.projectedOpenDisposition
    assertEqual "aliased projection does no leaf work" state replayed
  where
    projectedOpenResult outcome = case outcome of
      Target.TargetSubmissionCommitted _ [Target.TargetOpenProjected _ result] ->
        disappearanceOpenResultView result
      other -> error ("expected racing Open projection, got " <> show other)

propSymbolicWorkBound ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Property
propSymbolicWorkBound
  members
  publications
  alignments
  matching =
    counterexample
      ( "dimensions="
          <> show (members, publications, alignments, matching)
      )
      ( Disappearance.disappearanceSuccessfulWorkBound dimensions
          === 7 * m + 4 * p + 3 * a + 2 * w
      )
    where
      m = fromIntegral members :: Word64
      p = fromIntegral publications :: Word64
      a = fromIntegral alignments :: Word64
      w = fromIntegral matching :: Word64
      dimensions = Disappearance.disappearanceWorkDimensions m p a w

propGeneratedCutTrace :: [Word8] -> [Word8] -> Property
propGeneratedCutTrace markerChoices reportChoices =
  counterexample
    ( "markerOrder="
        <> show markerOrder
        <> ", reportOrder="
        <> show reportOrder
    )
    ( ( fmap Protocol.localAbsenceReportCanonicalBytes reports,
        projectedStates,
        targetProbe,
        fmap Disappearance.validateState (Map.elems projectedStates),
        repairStable
      )
        === ( fmap Protocol.localAbsenceReportCanonicalBytes baselineReports,
              baselineProjected,
              baselineTargetProbe,
              replicate 3 (Right ()),
              True
            )
    )
  where
    markerOrder = weightedPermutation markerChoices fixtureMembers
    reportOrder = weightedPermutation reportChoices fixtureMembers
    network = prepareCutNetwork fixtureControlledSubject
    (states, reports) = completeNetworkEvidence markerOrder network
    projectedStates = projectClaimsEverywhere reportOrder reports states
    targetProbe =
      Target.targetProbe
        fixtureProbe
        ( prospectiveOracleState
            (fst (submitReportsToOracle reportOrder reports (cutOracle network)))
        )
    (baselineStates, baselineReports) = completeNetworkEvidence fixtureMembers network
    baselineProjected =
      projectClaimsEverywhere fixtureMembers baselineReports baselineStates
    baselineTargetProbe =
      Target.targetProbe
        fixtureProbe
        ( prospectiveOracleState
            (fst (submitReportsToOracle fixtureMembers baselineReports (cutOracle network)))
        )
    repairSource = case filter (/= fixtureH1) markerOrder of
      source : _ -> source
      [] -> error "three-member generated order lost both remote sources"
    repairedItem =
      lookupMap
        "generated repair marker item"
        (repairSource, fixtureH1)
        (cutPublicationItems network)
    repairInitial = lookupMap "generated repair state" fixtureH1 (cutStates network)
    repairStream = lookupMap "generated repair stream" fixtureH1 (cutPeerStreams network)
    (received, receivedStream, _) =
      checked
        "generated marker receipt"
        ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
            repairedItem
            repairInitial
            repairStream
        )
    (receivedReplay, receivedStreamReplay, _) =
      checked
        "generated marker receipt replay"
        ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
            repairedItem
            received
            receivedStream
        )
    (completed, completedStream, _) =
      checked
        "generated marker completion"
        ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
            repairedItem
            received
            receivedStream
        )
    (completedReplay, completedStreamReplay, _) =
      checked
        "generated marker completion replay"
        ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
            repairedItem
            completed
            completedStream
        )
    repairStable =
      received == receivedReplay
        && receivedStream == receivedStreamReplay
        && completed == completedReplay
        && completedStream == completedStreamReplay

data InterleavedCutAction
  = CompleteLocalPublicationCut
  | CompleteRemotePublicationCut HeraldEpoch
  | CompleteIncomingAlignmentCut HeraldEpoch
  | ProjectRemoteReport HeraldEpoch
  deriving stock (Eq, Ord, Show)

-- Remote Reports are valid Oracle history independently of whether this
-- Herald has finished its local publication/alignment cut. This generated
-- schedule mixes those projections through the local completions and requires
-- the same final leaf, PeerStream prefix, report, and Resolve intention.
propGeneratedCutReportInterleaving :: Word16 -> Property
propGeneratedCutReportInterleaving choice =
  counterexample
    ("interleaved cut/report order=" <> show generatedOrder)
    ( ( generatedResult,
        Disappearance.validateState generatedState
      )
        === (baselineResult, Right ())
    )
  where
    network = prepareCutNetwork fixtureControlledSubject
    remoteReports =
      Map.fromList
        [ (reporter, snd (completeLocalEvidence fixtureMembers reporter network))
        | reporter <- [fixtureH2, fixtureH3]
        ]
    actions =
      [ CompleteLocalPublicationCut,
        CompleteRemotePublicationCut fixtureH2,
        CompleteRemotePublicationCut fixtureH3,
        CompleteIncomingAlignmentCut fixtureH3,
        ProjectRemoteReport fixtureH2,
        ProjectRemoteReport fixtureH3
      ]
    allOrders = permutations actions
    generatedOrder = allOrders !! (fromIntegral choice `mod` length allOrders)
    baselineOrder =
      [ CompleteLocalPublicationCut,
        CompleteRemotePublicationCut fixtureH2,
        CompleteRemotePublicationCut fixtureH3,
        CompleteIncomingAlignmentCut fixtureH3,
        ProjectRemoteReport fixtureH2,
        ProjectRemoteReport fixtureH3
      ]
    generatedResult@(generatedState, _, _, _) =
      runInterleavedCutReportSchedule generatedOrder remoteReports network
    baselineResult =
      runInterleavedCutReportSchedule baselineOrder remoteReports network

runInterleavedCutReportSchedule ::
  [InterleavedCutAction] ->
  Map HeraldEpoch Protocol.LocalAbsenceReport ->
  CutNetwork ->
  ( Disappearance.State,
    PeerStream.State Protocol.DisappearancePublicationMarker,
    Protocol.LocalAbsenceReport,
    [Protocol.DisappearanceResolveIntention]
  )
runInterleavedCutReportSchedule actions remoteReports network =
  (complete, stream, localReport, Disappearance.resolveIntentions complete)
  where
    initial = lookupMap "interleaved H1 state" fixtureH1 (cutStates network)
    initialStream = lookupMap "interleaved H1 PeerStream" fixtureH1 (cutPeerStreams network)
    (interleaved, stream) =
      foldl
        (applyInterleavedCutAction remoteReports network)
        (initial, initialStream)
        actions
    (withLocalReport, disposition) =
      commitChecked
        "prepare interleaved local Report"
        (prepareFixtureReport interleaved)
    localReport = case disposition of
      Disappearance.LocalReportPrepared report -> report
      other -> error ("interleaved cut did not prepare its Report: " <> show other)
    complete =
      fst
        ( commitChecked
            "project interleaved local Report"
            ( Disappearance.prepareProjectedReport
                (Protocol.localAbsenceReportClaim localReport)
                withLocalReport
            )
        )

applyInterleavedCutAction ::
  Map HeraldEpoch Protocol.LocalAbsenceReport ->
  CutNetwork ->
  (Disappearance.State, PeerStream.State Protocol.DisappearancePublicationMarker) ->
  InterleavedCutAction ->
  (Disappearance.State, PeerStream.State Protocol.DisappearancePublicationMarker)
applyInterleavedCutAction reports network (state, stream) action = case action of
  CompleteLocalPublicationCut ->
    ( fst
        ( commitExpected
            "interleaved local publication completion"
            Disappearance.MarkerCompleted
            ( Disappearance.prepareLocalPublicationCutCompletion
                fixtureProbe
                fixturePublicationPrefix
                state
            )
        ),
      stream
    )
  CompleteRemotePublicationCut source ->
    let item =
          lookupMap
            "interleaved remote publication item"
            (source, fixtureH1)
            (cutPublicationItems network)
        (received, receivedStream, receipt) =
          checked
            "interleaved remote publication receipt"
            (DisappearanceUseCase.coordinatePublicationMarkerReceipt item state stream)
        (completed, completedStream, completion) =
          checked
            "interleaved remote publication completion"
            ( DisappearanceUseCase.coordinatePublicationMarkerCompletion
                item
                received
                receivedStream
            )
     in case ( DisappearanceUseCase.publicationMarkerReceiptAdmissions receipt,
               completion
             ) of
          ( [(admitted, Disappearance.MarkerRetained)],
            DisappearanceUseCase.PublicationMarkerCompletionInstalled
              _
              Disappearance.MarkerCompleted
            )
              | admitted == item -> (completed, completedStream)
          other -> error ("unexpected interleaved publication transition: " <> show other)
  CompleteIncomingAlignmentCut source ->
    let marker = case [ retained
                      | (retainedSource, destination, retained) <- cutAlignmentMarkers network,
                        retainedSource == source,
                        destination == fixtureH1
                      ] of
          [retained] -> retained
          other -> error ("expected one interleaved alignment marker, got " <> show other)
        received =
          fst
            ( commitExpected
                "interleaved alignment receipt"
                Disappearance.MarkerRetained
                (Disappearance.prepareAlignmentMarkerReceipt source marker state)
            )
        completed =
          fst
            ( commitExpected
                "interleaved alignment completion"
                Disappearance.MarkerCompleted
                ( Disappearance.prepareAlignmentMarkerCompletion
                    marker
                    (Protocol.disappearanceAlignmentMarkerThroughRevision marker)
                    received
                )
            )
     in (completed, stream)
  ProjectRemoteReport reporter ->
    let report = lookupMap "interleaved remote Report" reporter reports
        projected =
          fst
            ( commitChecked
                "project Report during local cut"
                ( Disappearance.prepareProjectedReport
                    (Protocol.localAbsenceReportClaim report)
                    state
                )
            )
     in (projected, stream)

weightedPermutation :: [Word8] -> [value] -> [value]
weightedPermutation choices values =
  fmap
    snd
    ( sortOn
        fst
        (zip (take (length values) (choices <> repeat 0)) values)
    )

caseZeroWorkControls :: Assertion
caseZeroWorkControls = do
  let states0 = fmap Disappearance.initialState fixtureMembers
  assertEqual
    "empty Heralds contain neither candidates nor probes"
    [(0, 0), (0, 0), (0, 0)]
    [ (length (Disappearance.candidateWitnesses state), length (Disappearance.probeWitnesses state))
    | state <- states0
    ]
  assertEqual
    "empty/time/disconnect have no leaf input and therefore do no logical work"
    [0, 0, 0]
    [ Disappearance.disappearanceLogicalWork (Disappearance.disappearanceWorkLedger state)
    | state <- states0
    ]
  forM_ states0 $ \state -> do
    assertEqual
      "an unrelated empty leaf exposes no matching-write gate"
      Nothing
      (Disappearance.matchingWriteGateForProbe fixtureProbe state)
    assertEqual
      "a missing gate cannot manufacture matching-work state"
      0
      (Disappearance.disappearanceLogicalWork (Disappearance.disappearanceWorkLedger state))

-- The prospective leaf receives an already checked membership context. Its
-- retirement identity names a prior probe coordinate; the retirement itself
-- retains the explicit control index used by each admission assertion above.
fixtureRetirementResolution :: Word64 -> FailureProbeResolutionId
fixtureRetirementResolution index =
  deriveFailureProbeResolutionId
    (checked "prospective retirement probe" (deriveHeraldFailureProbeId (controlIndex (index - 1))))
    RetireFailureProbeTarget
