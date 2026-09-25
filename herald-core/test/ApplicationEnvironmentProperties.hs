{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module ApplicationEnvironmentProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word8)
import Eclips.Application.Types.Access
  ( allApplicationPredefinedSortRoles,
    environmentAccessEdges,
    environmentAccessHub,
    environmentAccessPredefined,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
  )
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    mkPrivateUniqueId,
    privateDeltaUniqueId,
    privateNablaUniqueId,
    privateObjectUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement)
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewIdApplication),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (NewIdCompleted),
  )
import Eclips.Application.Types.Value qualified as Application
import Eclips.Domain.Alignment
  ( firstHeraldPublicationPosition,
    nextHeraldPublicationPosition,
  )
import Eclips.Domain.Environment
  ( EnvironmentEdgeDirection (..),
    EnvironmentRootClaimView (..),
    EnvironmentRootSlot,
    environmentConnectedObjectCount,
    environmentManifestShapeRoots,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    environmentWiringSlots,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Graph (EdgeStrength (Preserve), edgeStrengthSymbol)
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    StructuralOccurrenceId,
    controlIndex,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    mkBootstrapManifestId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkNablaId,
    mkProcessEpochId,
    mkTopologyCutId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    nextStructuralSequence,
    processEpochIdBytes,
    publicationId,
    publicationSourceHeraldEpoch,
    sortIdBytes,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( checkedPublicationId,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (Normal),
    freezeRoute,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( profileSortFor,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole),
    allPredefinedSortRoles,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
  )
import Eclips.Domain.Value qualified as DomainValue
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.PrivateIdentity
  ( witnessedNextPrivateUniqueId,
    witnessedProcessEpoch,
  )
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    ProcessAcceptancePosition,
    RequestId,
    processAcceptancePosition,
    requestId,
    requestIdWord64,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (ApplicationReceiptRetirementNotReady),
    ApplicationSessionReply (SessionOpened),
    applicationAttachmentForBootstrap,
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Genesis.Internal
  ( PrimordialDefinitionReplica,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
  )
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
  )
import PrimordialTestAccess (fixtureEnvironmentEdges, fixtureEnvironmentHub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Positive (Positive),
    Property,
    counterexample,
    ioProperty,
    property,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "private application environment owner"
    [ testCase "receipt retirement preserves completed environment semantics and detaches its reply" caseReceiptRetirement,
      testCase
        "candidate retry is allocation-free and owns the shared request namespace"
        caseCandidateRetryAndNamespace,
      testCase
        "accepted retry retains exactly one manifest and one process position"
        caseAcceptedRetry,
      testCase
        "binding loss and resume preserve request and reply reachability"
        caseBindingLossAndResume,
      testCase
        "session and process End detach only accepted reply reachability"
        caseEndLifecycle,
      testCase
        "settlement localizes canonical aliases once and exact replay is inert"
        caseSettlementAliasesAndRetry,
      testCase
        "session End before settlement completes globally without a reply"
        caseSettlementAfterSessionEnd,
      testCase
        "process End before settlement records completion without aliases"
        caseSettlementAfterProcessEnd,
      testCase
        "End after settlement retires only application reachability"
        caseEndAfterSettlement,
      testCase
        "settlement evidence rejects the wrong count and duplicate occurrences"
        caseSettlementEvidenceShape,
      testProperty
        "arbitrarily many candidate retries consume no process positions"
        propCandidateRetriesDoNotAllocate,
      testProperty
        "checked settlement evidence preserves the supplied occurrence order"
        propSettlementEvidencePreservesOrder
    ]

caseCandidateRetryAndNamespace :: Assertion
caseCandidateRetryAndNamespace = do
  let fixture = applicationFixture
  first <- firstEnvironmentRequest fixture.state fixture
  candidatePreparation <-
    checked
      "retain environment candidate"
      (ApplicationState.prepareEnvironmentRequestCandidate first)
  let retained =
        ApplicationState.commitEnvironmentRequestCandidate candidatePreparation
  retry <- retainedEnvironmentCandidate retained fixture
  retryPreparation <-
    checked
      "retain exact environment candidate retry"
      (ApplicationState.prepareEnvironmentRequestCandidate retry)
  assertBool
    "candidate retry is an exact no-op"
    (ApplicationState.commitEnvironmentRequestCandidate retryPreparation == retained)
  assertEqual
    "candidate owns no global semantic work"
    []
    (ApplicationState.applicationPendingEnvironmentEntries retained)
  case ApplicationState.applicationEnvironmentRequestEntries fixture.session retained of
    [(request, witness)] -> do
      assertEqual "candidate request key" fixture.request request
      assertEqual
        "candidate request identity"
        fixture.request
        (ApplicationState.environmentRequestWitnessId witness)
      assertEqual
        "candidate process"
        fixture.process
        (ApplicationState.environmentRequestWitnessProcess witness)
      assertEqual
        "candidate retains no process position"
        ApplicationState.EnvironmentRequestCandidateStage
        (ApplicationState.environmentRequestWitnessStage witness)
    other ->
      assertFailure
        ("expected one retained environment candidate, got " <> show (length other))
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    fixture.request
    (NewIdApplication BareNewId)
    retained of
    Right ApplicationState.ConflictingApplicationRequest {} -> pure ()
    other ->
      assertFailure
        ( "ordinary request did not conflict with environment candidate: "
            <> either show (const "unexpected classification") other
        )
  let publicationCall =
        ApplicationState.applicationPublicationCall
          fixture.privateNabla
          (Application.BoolValue False)
  case ApplicationState.classifyApplicationPublicationRequest
    fixture.session
    fixture.binding
    fixture.request
    publicationCall
    retained of
    Right ApplicationState.ConflictingApplicationPublicationRequest {} -> pure ()
    other ->
      assertFailure
        ( "publication request did not conflict with environment candidate: "
            <> either show (const "unexpected classification") other
        )

  ordinaryCandidate <-
    case ApplicationState.classifyApplicationRequest
      fixture.session
      fixture.binding
      fixture.request
      (NewIdApplication BareNewId)
      fixture.state of
      Right (ApplicationState.FirstApplicationRequest candidate) -> pure candidate
      other ->
        fail
          ( "ordinary reverse-namespace fixture did not classify first: "
              <> either show (const "unexpected classification") other
          )
  let (ordinaryRetained, _) =
        ApplicationState.commitApplicationRequest
          ( ApplicationState.prepareApplicationRequestCompletion
              ordinaryCandidate
              ( NewIdCompleted
                  (checkedPure "ordinary private result" (mkPrivateUniqueId 1))
              )
          )
  assertEnvironmentConflict
    "ordinary request owns the reverse namespace"
    ordinaryRetained
    fixture

  publicationCandidate <-
    case ApplicationState.classifyApplicationPublicationRequest
      fixture.session
      fixture.binding
      fixture.request
      publicationCall
      fixture.state of
      Right (ApplicationState.FirstApplicationPublicationRequest candidate) -> pure candidate
      other ->
        fail
          ( "publication reverse-namespace fixture did not classify first: "
              <> either show (const "unexpected classification") other
          )
  let publicationRetained =
        ApplicationState.commitApplicationPublicationRequest
          ( ApplicationState.prepareApplicationPublicationRequestAcceptance
              publicationCandidate
          )
  assertEnvironmentConflict
    "publication request owns the reverse namespace"
    publicationRetained
    fixture

caseAcceptedRetry :: Assertion
caseAcceptedRetry = do
  let fixture = applicationFixture
  first <- firstEnvironmentRequest fixture.state fixture
  manifest <- manifestFor fixture 0x50 (ApplicationState.environmentRequestPosition first)
  prepared <-
    checked
      "accept private environment"
      (ApplicationState.prepareEnvironmentRequestAcceptance first manifest)
  let accepted = ApplicationState.commitEnvironmentRequestAcceptance prepared
  assertEqual
    "one accepted environment is retained"
    [(fixture.position, Environment.pendingEnvironment manifest (Just fixture.replyKey))]
    (ApplicationState.applicationPendingEnvironmentEntries accepted)
  retry <- retainedAcceptedEnvironment accepted fixture
  retryPrepared <-
    checked
      "prepare accepted retry"
      (ApplicationState.prepareEnvironmentRequestAcceptance retry manifest)
  assertBool
    "accepted exact retry changes no owner state"
    (ApplicationState.commitEnvironmentRequestAcceptance retryPrepared == accepted)
  conflictingManifest <- manifestFor fixture 0x70 fixture.position
  case ApplicationState.prepareEnvironmentRequestAcceptance retry conflictingManifest of
    Left (ApplicationState.EnvironmentRequestManifestConflict position) ->
      assertEqual "manifest conflict position" fixture.position position
    Left other -> assertFailure ("unexpected manifest conflict: " <> show other)
    Right _ -> assertFailure "changed accepted manifest was treated as an exact retry"
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    fixture.request
    (NewIdApplication BareNewId)
    accepted of
    Right ApplicationState.ConflictingApplicationRequest {} -> pure ()
    other ->
      assertFailure
        ( "accepted environment did not retain request namespace: "
            <> either show (const "unexpected classification") other
        )
  next <-
    case ApplicationState.classifyEnvironmentRequest
      fixture.session
      fixture.binding
      (requestId 2)
      ApplicationState.newEnvironmentCoordinatorInput
      accepted of
      Right (ApplicationState.FirstEnvironmentRequest request) -> pure request
      other ->
        fail
          ( "second environment request did not classify first: "
              <> either show (const "unexpected classification") other
          )
  assertEqual
    "accepted request advanced the process-wide supply once"
    (processAcceptancePosition fixture.process 2)
    (ApplicationState.environmentRequestPosition next)

caseBindingLossAndResume :: Assertion
caseBindingLossAndResume = do
  let fixture = applicationFixture
  accepted <- acceptedEnvironment fixture
  let afterLoss =
        ApplicationState.commitApplicationBindingLoss
          ( ApplicationState.prepareApplicationBindingLoss
              fixtureApplicationRecoveryConfiguration
              (monotonicInstant 0)
              fixture.binding
              accepted
          )
  assertEqual
    "binding loss retains the manifest and reply key"
    (ApplicationState.applicationPendingEnvironmentEntries accepted)
    (ApplicationState.applicationPendingEnvironmentEntries afterLoss)
  resumedPreparation <-
    checked
      "resume environment session"
      ( ApplicationState.prepareApplicationSessionResume
          fixture.session
          fixture.resumeToken
          fixture.cursor
          afterLoss
      )
  let (resumed, acceptance) =
        ApplicationState.commitApplicationSessionAcceptance resumedPreparation
      resumedBinding = sessionAcceptanceBinding acceptance
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    resumedBinding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    resumed of
    Right ApplicationState.RetainedEnvironmentRequest {} -> pure ()
    other ->
      assertFailure
        ( "resumed request did not find accepted retained work: "
            <> either show (const "unexpected classification") other
        )

caseEndLifecycle :: Assertion
caseEndLifecycle = do
  let fixture = applicationFixture
  first <- firstEnvironmentRequest fixture.state fixture
  candidatePreparation <-
    checked
      "retain candidate before session End"
      (ApplicationState.prepareEnvironmentRequestCandidate first)
  let candidate =
        ApplicationState.commitEnvironmentRequestCandidate candidatePreparation
  endedCandidatePreparation <-
    checked
      "end candidate session"
      ( ApplicationState.prepareApplicationSessionEnd
          fixture.session
          fixture.binding
          candidate
      )
  let endedCandidate =
        ApplicationState.commitApplicationSessionEnd endedCandidatePreparation
  assertEqual
    "session End discards an unaccepted candidate"
    []
    (ApplicationState.applicationEnvironmentRequestEntries fixture.session endedCandidate)
  assertEqual
    "unaccepted candidate creates no retained manifest"
    []
    (ApplicationState.applicationPendingEnvironmentEntries endedCandidate)

  accepted <- acceptedEnvironment fixture
  endedPreparation <-
    checked
      "end accepted environment session"
      ( ApplicationState.prepareApplicationSessionEnd
          fixture.session
          fixture.binding
          accepted
      )
  let ended = ApplicationState.commitApplicationSessionEnd endedPreparation
  assertDetachedPending "session End" ended

  let (retired, _) =
        ApplicationState.commitApplicationProcessRetirement
          ( ApplicationState.prepareApplicationProcessRetirement
              fixture.process
              accepted
          )
  assertDetachedPending "process End" retired
  where
    assertDetachedPending description state =
      case ApplicationState.applicationPendingEnvironmentEntries state of
        [(_, pending)] -> do
          assertEqual
            (description <> " removes only reply reachability")
            Nothing
            (Environment.pendingEnvironmentReplyKey pending)
          assertEqual
            (description <> " retains the accepted process")
            applicationFixture.process
            ( Environment.environmentManifestProcess
                (Environment.pendingEnvironmentManifest pending)
            )
        other ->
          assertFailure
            (description <> " did not retain exactly one manifest: " <> show (length other))

caseSettlementAliasesAndRetry :: Assertion
caseSettlementAliasesAndRetry = do
  let fixture = applicationFixture
  (accepted, manifest) <- acceptedEnvironmentWithManifest fixture
  changedManifest <- manifestFor fixture 0x70 fixture.position
  case ApplicationState.prepareEnvironmentSettlement
    (settlementEvidenceFor manifest)
    changedManifest
    accepted of
    Left (ApplicationState.EnvironmentSettlementManifestConflict position) ->
      assertEqual "settlement conflict position" fixture.position position
    Left other ->
      assertFailure ("unexpected settlement conflict: " <> show other)
    Right _ ->
      assertFailure "settlement accepted a changed manifest at the retained position"
  preparation <-
    checked
      "prepare live environment settlement"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          accepted
      )
  assertEqual
    "first settlement is applied"
    ApplicationState.EnvironmentSettlementApplied
    (ApplicationState.preparedEnvironmentSettlementDisposition preparation)
  let (settled, completed) =
        ApplicationState.commitEnvironmentSettlement preparation
  assertEqual
    "settlement releases the pending relation"
    []
    (ApplicationState.applicationPendingEnvironmentEntries settled)
  assertEqual
    "settlement retains one terminal global outcome"
    [(fixture.position, completed)]
    (ApplicationState.applicationCompletedEnvironmentEntries settled)
  assertEqual
    "terminal manifest is not rewritten"
    manifest
    (Environment.completedEnvironmentManifest completed)
  assertEqual
    "terminal completion retains the selected exact cut witness"
    (settlementEvidenceFor manifest)
    (Environment.completedEnvironmentSettlementEvidence completed)
  assertEqual
    "live request retains reply reachability"
    (Just fixture.replyKey)
    (Environment.completedEnvironmentReplyKey completed)
  access <-
    maybe
      (assertFailure "live settlement did not construct access" >> fail "missing access")
      pure
      (Environment.completedEnvironmentAccess completed)
  let pairs = environmentAccessPredefined access
      privateAliases =
        concatMap
          ( \pair ->
              [ privateNablaUniqueId (predefinedWriter pair),
                privateDeltaUniqueId (predefinedReader pair)
              ]
          )
          pairs
          <> [privateObjectUniqueId (environmentAccessHub access)]
          <> fmap privateObjectUniqueId (environmentAccessEdges access)
  assertEqual
    "localized access keeps canonical application role order"
    allApplicationPredefinedSortRoles
    (fmap predefinedAccessRole pairs)
  assertEqual
    "all environment objects consume the next thirty-one aliases in canonical order"
    [33 .. 63]
    (fmap privateUniqueIdWord64 privateAliases)
  case find
    ((== fixture.process) . witnessedProcessEpoch)
    (ApplicationState.applicationProcessWitnesses settled) of
    Just identity ->
      assertEqual
        "environment localization advances the private counter exactly thirty-one times"
        64
        (privateUniqueIdWord64 (witnessedNextPrivateUniqueId identity))
    Nothing -> assertFailure "settled environment lost its process identity witness"
  assertEqual
    "every private endpoint, hub and edge resolves to its exact manifest target"
    ( Right
        ( fmap
            ( globalUniqueIdFromGlobalObjectId
                . Environment.environmentRootPlanTargetObject
                . Environment.positionedEnvironmentRootPlan
            )
            (NonEmpty.toList (Environment.environmentManifestRoots manifest))
        )
    )
    ( traverse
        ( \privateIdentity ->
            ApplicationState.resolveApplicationPrivateUniqueId
              fixture.process
              privateIdentity
              settled
        )
        privateAliases
    )
  sameProcessSessionPreparation <-
    checked
      "open a second session for the completed environment process"
      ( ApplicationState.prepareApplicationSessionOpen
          fixture.localHerald
          (applicationAttachment fixture.process)
          (clientNonce 2)
          settled
      )
  let (withSameProcessSession, sameProcessAcceptance) =
        ApplicationState.commitApplicationSessionAcceptance sameProcessSessionPreparation
      sameProcessSession = case sessionAcceptanceReply sameProcessAcceptance of
        SessionOpened openedSession _ _ -> openedSession
        reply -> error ("expected second same-process session, got " <> show reply)
  sameProcess <-
    maybe
      (assertFailure "second session lost its process" >> fail "missing process")
      pure
      (ApplicationState.applicationSessionProcess sameProcessSession withSameProcessSession)
  assertEqual
    "the second session belongs to the completed environment process"
    fixture.process
    sameProcess
  assertEqual
    "completed aliases resolve through a second session of the same process"
    ( Right
        ( fmap
            ( globalUniqueIdFromGlobalObjectId
                . Environment.environmentRootPlanTargetObject
                . Environment.positionedEnvironmentRootPlan
            )
            (NonEmpty.toList (Environment.environmentManifestRoots manifest))
        )
    )
    ( traverse
        (\privateIdentity -> ApplicationState.resolveApplicationPrivateUniqueId sameProcess privateIdentity withSameProcessSession)
        privateAliases
    )
  let otherProcess = checkedIdentity "other environment process" mkProcessEpochId 0x42
      otherAttachment = applicationAttachment otherProcess
      (withOtherProcess, _) =
        ApplicationState.commitApplicationBootstrap
          ( checkedPure
              "other environment process bootstrap"
              ( ApplicationState.prepareApplicationBootstrap
                  otherAttachment
                  otherProcess
                  (applicationRoots 0xc0 0xd0)
                  fixtureEnvironmentHub
                  (fixtureEnvironmentEdges (applicationRoots 0xc0 0xd0))
                  withSameProcessSession
              )
          )
  otherSessionPreparation <-
    checked
      "open a session for a different environment process"
      ( ApplicationState.prepareApplicationSessionOpen
          fixture.localHerald
          otherAttachment
          (clientNonce 3)
          withOtherProcess
      )
  let (withOtherSession, otherAcceptance) =
        ApplicationState.commitApplicationSessionAcceptance otherSessionPreparation
      otherSession = case sessionAcceptanceReply otherAcceptance of
        SessionOpened openedSession _ _ -> openedSession
        reply -> error ("expected different-process session, got " <> show reply)
  sessionOtherProcess <-
    maybe
      (assertFailure "different-process session lost its process" >> fail "missing process")
      pure
      (ApplicationState.applicationSessionProcess otherSession withOtherSession)
  assertBool
    "the comparison session belongs to a different process"
    (sessionOtherProcess /= fixture.process)
  assertBool
    "completed aliases are absent from a different process namespace"
    ( all
        ( \privateIdentity ->
            ApplicationState.resolveApplicationPrivateUniqueId
              sessionOtherProcess
              privateIdentity
              withOtherSession
              == Left
                ( ApplicationState.ApplicationValueRejected
                    (ApplicationState.ApplicationValueUnknownPrivateId privateIdentity)
                )
        )
        privateAliases
    )
  case ApplicationState.applicationEnvironmentRequestEntries fixture.session settled of
    [(_, witness)] ->
      assertEqual
        "live request advances to its terminal stage"
        (ApplicationState.EnvironmentRequestCompletedStage fixture.position)
        (ApplicationState.environmentRequestWitnessStage witness)
    other ->
      assertFailure
        ("expected one completed environment request, got " <> show (length other))
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    fixture.binding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    settled of
    Right ApplicationState.RetainedCompletedEnvironment {} -> pure ()
    other ->
      assertFailure
        ( "completed request did not classify as terminal retry: "
            <> either show (const "unexpected classification") other
        )
  replay <-
    checked
      "prepare exact settlement replay"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          settled
      )
  assertEqual
    "terminal replay is classified exactly"
    ApplicationState.EnvironmentSettlementExactRetry
    (ApplicationState.preparedEnvironmentSettlementDisposition replay)
  assertBool
    "terminal replay changes neither aliases nor owner state"
    (fst (ApplicationState.commitEnvironmentSettlement replay) == settled)
  let conflictingEvidence =
        checkedPure
          "conflicting environment settlement evidence"
          ( Environment.environmentSettlementEvidence
              manifest
              (checkedIdentity "conflicting settlement cut" mkTopologyCutId 0x62)
              (Environment.environmentManifestMembershipGenerationId manifest)
              (Environment.environmentManifestActiveMemberSetDigest manifest)
              ( Environment.environmentSettlementEvidenceOccurrences
                  (settlementEvidenceFor manifest)
              )
          )
  case ApplicationState.prepareEnvironmentSettlement
    conflictingEvidence
    manifest
    settled of
    Left (ApplicationState.EnvironmentSettlementEvidenceConflict position) ->
      assertEqual "settlement evidence conflict position" fixture.position position
    Left other ->
      assertFailure ("unexpected settlement-evidence conflict: " <> show other)
    Right _ ->
      assertFailure "completed settlement accepted a different exact-cut witness"
  case ApplicationState.prepareEnvironmentSettlement
    (settlementEvidenceFor manifest)
    changedManifest
    settled of
    Left (ApplicationState.EnvironmentSettlementManifestConflict position) ->
      assertEqual "completed settlement conflict position" fixture.position position
    Left other ->
      assertFailure ("unexpected completed settlement conflict: " <> show other)
    Right _ ->
      assertFailure "completed settlement accepted a changed manifest"

caseSettlementAfterSessionEnd :: Assertion
caseSettlementAfterSessionEnd = do
  let fixture = applicationFixture
  (accepted, manifest) <- acceptedEnvironmentWithManifest fixture
  endedPreparation <-
    checked
      "end accepting session before settlement"
      ( ApplicationState.prepareApplicationSessionEnd
          fixture.session
          fixture.binding
          accepted
      )
  let ended = ApplicationState.commitApplicationSessionEnd endedPreparation
  settlement <-
    checked
      "settle after session End"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          ended
      )
  let (settled, completed) = ApplicationState.commitEnvironmentSettlement settlement
  assertBool
    "the still-live process receives stable aliases"
    (Environment.completedEnvironmentAccess completed /= Nothing)
  assertEqual
    "session End prevents reply recreation"
    Nothing
    (Environment.completedEnvironmentReplyKey completed)
  assertEqual
    "the ended session remains absent"
    []
    (ApplicationState.applicationEnvironmentRequestEntries fixture.session settled)

caseSettlementAfterProcessEnd :: Assertion
caseSettlementAfterProcessEnd = do
  let fixture = applicationFixture
  (accepted, manifest) <- acceptedEnvironmentWithManifest fixture
  let (retired, _) =
        ApplicationState.commitApplicationProcessRetirement
          ( ApplicationState.prepareApplicationProcessRetirement
              fixture.process
              accepted
          )
  settlement <-
    checked
      "settle after process End"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          retired
      )
  let (settled, completed) = ApplicationState.commitEnvironmentSettlement settlement
  assertEqual
    "process End prevents alias localization"
    Nothing
    (Environment.completedEnvironmentAccess completed)
  assertEqual
    "process End prevents reply recreation"
    Nothing
    (Environment.completedEnvironmentReplyKey completed)
  assertEqual
    "global completion releases pending work"
    []
    (ApplicationState.applicationPendingEnvironmentEntries settled)
  assertEqual
    "retired process map stays retired"
    Nothing
    (ApplicationState.applicationBootstrapAccess fixture.process settled)

caseReceiptRetirement :: Assertion
caseReceiptRetirement = do
  let fixture = applicationFixture
      progress = applicationReceiptRetirement (Just (requestIdWord64 fixture.request)) Nothing
      retire = ApplicationState.prepareApplicationReceiptRetirement fixture.binding fixture.session progress
  (accepted, manifest) <- acceptedEnvironmentWithManifest fixture
  case retire accepted of
    Left (ApplicationState.ApplicationSessionRejected ApplicationReceiptRetirementNotReady) -> pure ()
    Left problem -> assertFailure ("wrong retirement rejection: " <> show problem)
    Right _ -> assertFailure "retirement discarded a pending environment"
  prepared <-
    checked
      "settle before receipt retirement"
      (ApplicationState.prepareEnvironmentSettlement (settlementEvidenceFor manifest) manifest accepted)
  let (settled, completion) = ApplicationState.commitEnvironmentSettlement prepared
  retirement <- checked "retire completed environment reply" (retire settled)
  let retired = ApplicationState.commitApplicationReceiptRetirement retirement
  assertEqual "completed request correlation is reclaimed" [] (ApplicationState.applicationRequestEntries fixture.session retired)
  case ApplicationState.applicationCompletedEnvironmentEntries retired of
    [(_, completed)] -> do
      assertEqual "global manifest survives" manifest (Environment.completedEnvironmentManifest completed)
      assertEqual "settlement evidence survives" (Environment.completedEnvironmentSettlementEvidence completion) (Environment.completedEnvironmentSettlementEvidence completed)
      assertEqual "application-owned namespace aliases survive" (Environment.completedEnvironmentAccess completion) (Environment.completedEnvironmentAccess completed)
      assertEqual "only the completed reply route is detached" Nothing (Environment.completedEnvironmentReplyKey completed)
    other -> assertFailure ("expected one completed manifest, got " <> show (length other))

caseEndAfterSettlement :: Assertion
caseEndAfterSettlement = do
  let fixture = applicationFixture
  (accepted, manifest) <- acceptedEnvironmentWithManifest fixture
  prepared <-
    checked
      "settle before End"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          accepted
      )
  let (settled, _) = ApplicationState.commitEnvironmentSettlement prepared
  endedSessionPreparation <-
    checked
      "end session after settlement"
      ( ApplicationState.prepareApplicationSessionEnd
          fixture.session
          fixture.binding
          settled
      )
  let endedSession =
        ApplicationState.commitApplicationSessionEnd endedSessionPreparation
  case ApplicationState.applicationCompletedEnvironmentEntries endedSession of
    [(_, completed)] -> do
      assertBool
        "session End preserves process aliases"
        (Environment.completedEnvironmentAccess completed /= Nothing)
      assertEqual
        "session End detaches the completed reply"
        Nothing
        (Environment.completedEnvironmentReplyKey completed)
    other ->
      assertFailure
        ("expected one completion after session End, got " <> show (length other))
  let
    (retired, _) =
      ApplicationState.commitApplicationProcessRetirement
        ( ApplicationState.prepareApplicationProcessRetirement
            fixture.process
            settled
        )
  case ApplicationState.applicationCompletedEnvironmentEntries retired of
    [(_, completed)] -> do
      assertEqual
        "End preserves the completed global manifest"
        manifest
        (Environment.completedEnvironmentManifest completed)
      assertEqual
        "End retires completed aliases"
        Nothing
        (Environment.completedEnvironmentAccess completed)
      assertEqual
        "End detaches the completed reply"
        Nothing
        (Environment.completedEnvironmentReplyKey completed)
    other ->
      assertFailure
        ("expected one completion after End, got " <> show (length other))
  replay <-
    checked
      "prepare exact settlement replay after process End"
      ( ApplicationState.prepareEnvironmentSettlement
          (settlementEvidenceFor manifest)
          manifest
          retired
      )
  assertEqual
    "post-End settlement replay remains exact"
    ApplicationState.EnvironmentSettlementExactRetry
    (ApplicationState.preparedEnvironmentSettlementDisposition replay)
  let (replayed, replayedCompletion) =
        ApplicationState.commitEnvironmentSettlement replay
  assertBool
    "post-End exact replay is state-identical"
    (replayed == retired)
  assertEqual
    "post-End exact replay does not resurrect aliases"
    Nothing
    (Environment.completedEnvironmentAccess replayedCompletion)

caseSettlementEvidenceShape :: Assertion
caseSettlementEvidenceShape = do
  let fixture = applicationFixture
  (_, manifest) <- acceptedEnvironmentWithManifest fixture
  let expected = settlementOccurrenceIdsFor manifest
      expectedCount = fromIntegral (NonEmpty.length expected)
      short =
        expectNonEmpty
          "short settlement witness"
          (take (NonEmpty.length expected - 1) (NonEmpty.toList expected))
      repeated =
        NonEmpty.head expected
          NonEmpty.:| replicate
            (NonEmpty.length expected - 1)
            (NonEmpty.head expected)
  case rebuildSettlementEvidence manifest short of
    Left
      ( Environment.EnvironmentSettlementEvidenceOccurrenceCountMismatch
          reportedExpected
          reportedActual
        ) -> do
        assertEqual "settlement witness expected count" expectedCount reportedExpected
        assertEqual "settlement witness short count" (expectedCount - 1) reportedActual
    Left other ->
      assertFailure ("unexpected settlement-witness count rejection: " <> show other)
    Right _ ->
      assertFailure "settlement witness accepted fewer occurrences than the manifest"
  case rebuildSettlementEvidence manifest repeated of
    Left (Environment.EnvironmentSettlementEvidenceDuplicateOccurrence occurrence) ->
      assertEqual
        "settlement witness reports the repeated occurrence"
        (NonEmpty.head expected)
        occurrence
    Left other ->
      assertFailure ("unexpected settlement-witness duplicate rejection: " <> show other)
    Right _ ->
      assertFailure "settlement witness accepted duplicate occurrences"

propSettlementEvidencePreservesOrder :: Positive Int -> Property
propSettlementEvidencePreservesOrder (Positive suppliedRotation) =
  ioProperty $ do
    (_, manifest) <- acceptedEnvironmentWithManifest applicationFixture
    let canonical = settlementOccurrenceIdsFor manifest
        occurrences = NonEmpty.toList canonical
        rotation = suppliedRotation `mod` length occurrences
        rotatedList = drop rotation occurrences <> take rotation occurrences
        rotated = expectNonEmpty "rotated settlement witness" rotatedList
    pure $ case rebuildSettlementEvidence manifest rotated of
      Left problem -> counterexample (show problem) False
      Right evidence ->
        counterexample
          "checked settlement evidence changed manifest-order occurrence IDs"
          ( property
              ( Environment.environmentSettlementEvidenceOccurrences evidence
                  == rotated
              )
          )

propCandidateRetriesDoNotAllocate :: Positive Int -> Property
propCandidateRetriesDoNotAllocate (Positive suppliedCount) =
  let fixture = applicationFixture
      count = 1 + suppliedCount `mod` 100
   in case firstEnvironmentRequestEither fixture.state fixture of
        Left problem -> counterexample problem False
        Right first ->
          case ApplicationState.prepareEnvironmentRequestCandidate first of
            Left problem -> counterexample (show problem) False
            Right preparation ->
              let retained =
                    ApplicationState.commitEnvironmentRequestCandidate preparation
               in case repeatCandidate count retained fixture of
                    Left problem -> counterexample problem False
                    Right final ->
                      case ApplicationState.classifyEnvironmentRequest
                        fixture.session
                        fixture.binding
                        (requestId 2)
                        ApplicationState.newEnvironmentCoordinatorInput
                        final of
                        Right (ApplicationState.FirstEnvironmentRequest next) ->
                          counterexample
                            "candidate retries advanced the process acceptance supply"
                            ( property
                                ( ApplicationState.environmentRequestPosition next
                                    == fixture.position
                                )
                            )
                        other ->
                          counterexample
                            ( "fresh request failed after candidate retries: "
                                <> either show (const "unexpected classification") other
                            )
                            False

repeatCandidate ::
  Int ->
  ApplicationState.State ->
  Fixture ->
  Either String ApplicationState.State
repeatCandidate remaining state fixture
  | remaining <= 0 = Right state
  | otherwise = do
      retry <- retainedEnvironmentCandidateEither state fixture
      preparation <-
        mapLeft show (ApplicationState.prepareEnvironmentRequestCandidate retry)
      repeatCandidate
        (remaining - 1)
        (ApplicationState.commitEnvironmentRequestCandidate preparation)
        fixture

data Fixture = Fixture
  { localHerald :: HeraldEpoch,
    process :: ProcessEpochId,
    writerSource :: NablaId,
    readerSource :: NablaId,
    privateNabla :: PrivateNablaId,
    request :: RequestId,
    position :: ProcessAcceptancePosition,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    resumeToken :: ApplicationResumeToken,
    cursor :: ApplicationReplyCursor,
    replyKey :: Environment.EnvironmentReplyKey,
    state :: ApplicationState.State
  }

applicationFixture :: Fixture
applicationFixture =
  Fixture
    { localHerald,
      process,
      writerSource,
      readerSource,
      privateNabla,
      request,
      position,
      session,
      binding,
      resumeToken,
      cursor,
      replyKey = Environment.environmentReplyKey session request,
      state = opened
    }
  where
    localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
    process = checkedIdentity "environment process" mkProcessEpochId 0x41
    request = requestId 1
    position = processAcceptancePosition process 1
    attachment = applicationAttachment process
    writerFor role =
      case [ writer
           | ApplicationState.ApplicationWriterRoot candidateRole writer _ <- roots,
             candidateRole == role
           ] of
        [writer] -> writer
        _ -> error "environment fixture root writer is not unique"
    writerSource = writerFor NablaRole
    readerSource = writerFor DeltaRole
    roots = applicationRoots 0x80 0xa0
    (bootstrapped, access) =
      ApplicationState.commitApplicationBootstrap
        ( checkedPure
            "environment application bootstrap"
            ( ApplicationState.prepareApplicationBootstrap
                attachment
                process
                roots
                fixtureEnvironmentHub
                (fixtureEnvironmentEdges roots)
                ApplicationState.emptyState
            )
        )
    privateNabla =
      case [ writer
           | ApplicationState.WriterAccess role writer _ <-
               ApplicationState.bootstrapAccessRoots access,
             role == NablaRole
           ] of
        [writer] -> writer
        _ -> error "environment fixture private Nabla is not unique"
    (opened, acceptance) =
      ApplicationState.commitApplicationSessionAcceptance
        ( checkedPure
            "environment application session"
            ( ApplicationState.prepareApplicationSessionOpen
                localHerald
                attachment
                (clientNonce 1)
                bootstrapped
            )
        )
    binding = sessionAcceptanceBinding acceptance
    cursor = sessionAcceptanceCursor acceptance
    (session, resumeToken) = case sessionAcceptanceReply acceptance of
      SessionOpened openedSession token _ -> (openedSession, token)
      reply -> error ("expected opened environment session, got " <> show reply)

acceptedEnvironment :: Fixture -> IO ApplicationState.State
acceptedEnvironment fixture = do
  (accepted, _) <- acceptedEnvironmentWithManifest fixture
  pure accepted

acceptedEnvironmentWithManifest ::
  Fixture -> IO (ApplicationState.State, Environment.EnvironmentManifest)
acceptedEnvironmentWithManifest fixture = do
  first <- firstEnvironmentRequest fixture.state fixture
  manifest <- manifestFor fixture 0x50 (ApplicationState.environmentRequestPosition first)
  preparation <-
    checked
      "accept environment fixture"
      (ApplicationState.prepareEnvironmentRequestAcceptance first manifest)
  pure
    ( ApplicationState.commitEnvironmentRequestAcceptance preparation,
      manifest
    )

settlementEvidenceFor ::
  Environment.EnvironmentManifest ->
  Environment.EnvironmentSettlementEvidence
settlementEvidenceFor manifest =
  checkedPure
    "environment settlement evidence"
    (rebuildSettlementEvidence manifest (settlementOccurrenceIdsFor manifest))

rebuildSettlementEvidence ::
  Environment.EnvironmentManifest ->
  NonEmpty.NonEmpty StructuralOccurrenceId ->
  Either
    Environment.EnvironmentSettlementEvidenceProblem
    Environment.EnvironmentSettlementEvidence
rebuildSettlementEvidence manifest occurrences =
  Environment.environmentSettlementEvidence
    manifest
    cut
    (Environment.environmentManifestMembershipGenerationId manifest)
    (Environment.environmentManifestActiveMemberSetDigest manifest)
    occurrences
  where
    firstRoot = NonEmpty.head (Environment.environmentManifestRoots manifest)
    cut =
      Environment.environmentRootPlanSourceTopologyPrerequisite
        (Environment.positionedEnvironmentRootPlan firstRoot)

settlementOccurrenceIdsFor ::
  Environment.EnvironmentManifest ->
  NonEmpty.NonEmpty StructuralOccurrenceId
settlementOccurrenceIdsFor manifest =
  structuralOccurrenceId source
    <$> ( firstStructuralSequence
            NonEmpty.:| take
              (NonEmpty.length (Environment.environmentManifestRoots manifest) - 1)
              (drop 1 (iterate nextStructuralSequence firstStructuralSequence))
        )
  where
    firstRoot = NonEmpty.head (Environment.environmentManifestRoots manifest)
    source =
      publicationSourceHeraldEpoch
        (checkedPublicationId (Environment.positionedEnvironmentRootChecked firstRoot))

firstEnvironmentRequest ::
  ApplicationState.State -> Fixture -> IO ApplicationState.EnvironmentRequest
firstEnvironmentRequest state fixture =
  either fail pure (firstEnvironmentRequestEither state fixture)

firstEnvironmentRequestEither ::
  ApplicationState.State -> Fixture -> Either String ApplicationState.EnvironmentRequest
firstEnvironmentRequestEither state fixture =
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    fixture.binding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    state of
    Left problem -> Left (show problem)
    Right (ApplicationState.FirstEnvironmentRequest request) -> Right request
    Right _ -> Left "expected first environment request"

retainedEnvironmentCandidate ::
  ApplicationState.State -> Fixture -> IO ApplicationState.EnvironmentRequest
retainedEnvironmentCandidate state fixture =
  either fail pure (retainedEnvironmentCandidateEither state fixture)

retainedEnvironmentCandidateEither ::
  ApplicationState.State -> Fixture -> Either String ApplicationState.EnvironmentRequest
retainedEnvironmentCandidateEither state fixture =
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    fixture.binding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    state of
    Left problem -> Left (show problem)
    Right (ApplicationState.RetainedEnvironmentCandidate request) -> Right request
    Right _ -> Left "expected retained environment candidate"

retainedAcceptedEnvironment ::
  ApplicationState.State -> Fixture -> IO ApplicationState.EnvironmentRequest
retainedAcceptedEnvironment state fixture =
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    fixture.binding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    state of
    Left problem -> fail (show problem)
    Right (ApplicationState.RetainedEnvironmentRequest request _) -> pure request
    Right _ -> fail "expected retained accepted environment"

assertEnvironmentConflict ::
  String -> ApplicationState.State -> Fixture -> Assertion
assertEnvironmentConflict description state fixture =
  case ApplicationState.classifyEnvironmentRequest
    fixture.session
    fixture.binding
    fixture.request
    ApplicationState.newEnvironmentCoordinatorInput
    state of
    Right ApplicationState.ConflictingEnvironmentRequest {} -> pure ()
    Left problem -> assertFailure (description <> ": " <> show problem)
    Right _ -> assertFailure (description <> ": expected request conflict")

manifestFor ::
  Fixture ->
  Word8 ->
  ProcessAcceptancePosition ->
  IO Environment.EnvironmentManifest
manifestFor fixture generatedSeed position = do
  key <-
    checked
      "environment manifest key"
      (Environment.environmentManifestKey fixture.process position)
  roots <- traverse rootAt numberedSlots
  nonEmptyRoots <-
    maybe (fail "closed environment shape unexpectedly empty") pure (NonEmpty.nonEmpty roots)
  checked
    "environment manifest"
    ( Environment.environmentManifest
        key
        (heraldMembershipGenerationId fixtureHeraldMembershipGeneration)
        ( heraldMembershipGenerationActiveMemberSetDigest
            fixtureHeraldMembershipGeneration
        )
        (checkedIdentity "environment generated hub seed" mkGlobalUniqueId generatedSeed NonEmpty.:| [checkedIdentity "environment generated object" mkGlobalUniqueId (generatedSeed + ordinal) | ordinal <- [1 .. environmentConnectedObjectCount - 1]])
        nonEmptyRoots
    )
  where
    numberedSlots =
      zip [0 ..] (NonEmpty.toList (environmentManifestShapeRoots profileEnvironmentManifestShape <> environmentWiringSlots))
    emptyRoute = checkedPure "empty environment route" (freezeRoute [])
    topology = checkedIdentity "environment topology cut" mkTopologyCutId 0x61
    sourceAt ordinal = nablaIdFromGlobalObjectId (globalObjectIdFromGlobalUniqueId (checkedIdentity "environment source" mkGlobalUniqueId (generatedSeed + ordinal)))
    rootAt (ordinal, slot) = do
      let carrier = environmentRootSlotStructuralCarrierRole slot
          carrierReplica = carrierFor carrier
          source = case carrier of
            NablaCarrier -> fixture.writerSource
            DeltaCarrier -> fixture.readerSource
            NeutralVertexCarrier -> sourceAt 2
            EdgeCarrier -> sourceAt 4
            _ -> error "checked environment slot acquired a non-root carrier"
          identifier =
            publicationId
              source
              genesisAuthorityEpoch
              fixture.localHerald
              (nablaSequence (case carrier of EdgeCarrier -> fromIntegral ordinal - 12; NeutralVertexCarrier -> 1; _ -> fromIntegral ordinal `div` 2 + 1))
          descriptor = primordialReplicaDescriptor carrierReplica
          generated =
            checkedIdentity
              "environment generated root"
              mkGlobalUniqueId
              (generatedSeed + ordinal)
          value = environmentRootValue fixture.process generatedSeed slot generated
          plan =
            Environment.environmentRootPlan
              slot
              generated
              source
              Nothing
              genesisAuthorityEpoch
              descriptor
              value
              (primordialReplicaOccurrenceId carrierReplica)
              Normal
              topology
              (controlIndex 0)
              emptyRoute
          checkedPublication =
            checkedPure
              "environment checked root publication"
              (mkCheckedPublication descriptor identifier value)
          heraldPosition =
            iterate nextHeraldPublicationPosition firstHeraldPublicationPosition
              !! fromIntegral ordinal
      checked
        "position environment root"
        ( Environment.positionEnvironmentRoot
            plan
            checkedPublication
            heraldPosition
        )

environmentRootValue ::
  ProcessEpochId -> Word8 -> EnvironmentRootSlot -> GlobalUniqueId -> DomainValue.Value
environmentRootValue process generatedSeed slot generated =
  checkedPure "environment root value" (DomainValue.recordValue (common <> fields))
  where
    common =
      [ (field "object_id", DomainValue.globalUniqueIdValue generated),
        (field "label", DomainValue.labelValue (DomainValue.ProcessLabel process, 0))
      ]
    fields = case environmentRootClaimView (environmentRootSlotClaim slot) of
      EnvironmentWriterRootClaimView role _ ->
        [ (field "sort_id", DomainValue.bytesValue (sortIdBytes (profileSortFor role))),
          (field "sequencing_object", DomainValue.optionalGlobalUniqueIdValue Nothing)
        ]
      EnvironmentReaderRootClaimView role ->
        [(field "sort_id", DomainValue.bytesValue (sortIdBytes (profileSortFor role)))]
      EnvironmentHubRootClaimView -> []
      EnvironmentEdgeRootClaimView role direction ->
        let ordinal = fromIntegral (fromEnum role) * 2
            writer = generatedAt ordinal
            reader = generatedAt (ordinal + 1)
            hub = generatedAt 12
            (source, destination) = case direction of
              WriterToHub -> (writer, hub)
              HubToReader -> (hub, reader)
              ReaderToHub -> (reader, hub)
         in [ (field "source_vertex", DomainValue.globalUniqueIdValue source),
              (field "destination_vertex", DomainValue.globalUniqueIdValue destination),
              (field "strength", DomainValue.enumValue (edgeStrengthSymbol Preserve))
            ]
    generatedAt ordinal = checkedIdentity "environment endpoint" mkGlobalUniqueId (generatedSeed + ordinal)
    field name = checkedPure ("environment root field " <> show name) (DomainValue.mkFieldName name)

carrierFor :: StructuralCarrierRole -> PrimordialDefinitionReplica
carrierFor carrier =
  let role = case carrier of
        NablaCarrier -> NablaRole
        DeltaCarrier -> DeltaRole
        NeutralVertexCarrier -> NeutralVertexRole
        EdgeCarrier -> EdgeRole
        _ -> error "environment test requested a non-root carrier"
   in maybe
        (error ("missing environment carrier " <> show carrier))
        id
        (find ((== role) . primordialReplicaRole) (checkedPrimordialReplicas fixtureCheckedGenesis))

applicationAttachment :: ProcessEpochId -> ApplicationAttachment
applicationAttachment process =
  applicationAttachmentForBootstrap
    ( checkedPure
        "environment bootstrap manifest"
        (mkBootstrapManifestId (processEpochIdBytes process))
    )

applicationRoots :: Word8 -> Word8 -> [ApplicationState.ApplicationRoot]
applicationRoots writerSeed readerSeed =
  concat
    [ [ ApplicationState.ApplicationWriterRoot
          role
          (checkedIdentity "environment startup writer" mkNablaId (writerSeed + ordinal))
          UnsequencedNabla,
        ApplicationState.ApplicationReaderRoot
          role
          (checkedIdentity "environment startup reader" mkDeltaId (readerSeed + ordinal))
      ]
    | (ordinal, role) <- zip [0 ..] allPredefinedSortRoles
    ]

checkedIdentity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem value) ->
  Word8 ->
  value
checkedIdentity description constructor seed =
  checkedPure description (constructor (fixtureIdentifierBytes seed))

checked :: (Show problem) => String -> Either problem value -> IO value
checked description = either (fail . ((description <> ": ") <>) . show) pure

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description =
  either (error . ((description <> ": ") <>) . show) id

expectNonEmpty :: String -> [value] -> NonEmpty.NonEmpty value
expectNonEmpty description values =
  case NonEmpty.nonEmpty values of
    Nothing -> error (description <> " unexpectedly became empty")
    Just nonEmptyValues -> nonEmptyValues

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft convert = either (Left . convert) Right
