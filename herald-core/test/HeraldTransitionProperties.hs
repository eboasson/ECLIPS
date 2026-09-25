{-# LANGUAGE OverloadedStrings #-}

module HeraldTransitionProperties
  ( tests,
    SuccessorStructuralHoldFixture (..),
    successorStructuralHeldFixture,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import Data.List (find, isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16, Word64)
import Eclips.Application.Types.Access
  ( applicationStartupAccess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( asPrivateProcessId,
    mkPrivateUniqueId,
  )
import Eclips.Application.Types.Operation (ApplicationOperation (WriteApplication))
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    controlIndex,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Administration
  ( FinalAdminReply (..),
    adminCorrelationId,
    checkedInitialAdministrationBinding,
  )
import Eclips.Herald.Administration.Internal qualified as AdministrationInternal
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.PrivateIdentity (witnessedProcessEpoch)
import Eclips.Herald.Application.Request
  ( requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (..),
    ApplicationSessionReply (..),
    ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
    sessionReplySessionId,
  )
import Eclips.Herald.Application.Session.Internal qualified as SessionInternal
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Discovery
  ( BindingAdmission (ReofferedBinding),
    HelloRejection (HelloInactiveHerald),
    PeerBinding,
    PeerCandidate,
    PeerHello,
    PeerHelloDisposition (PeerHelloAccepted, PeerHelloRejected),
    connectionNonce,
    peerCandidate,
    peerCandidateOpened,
    peerCandidateOpenedConnectionNonce,
    peerHello,
  )
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (..),
    EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (ClosePeerPublicationProtocol),
    effectBatchIsEmpty,
    effectBatchMembers,
    emptyEffectBatch,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    HeraldMember (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    PredefinedSortRole (SortDefinitionRole),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    CandidateApplicationLane,
    HeraldInput,
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (..),
    candidateAdministrationLane,
    candidateApplicationLane,
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle),
    OracleClientIngress (OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch
  ( peerDispatchAttemptItem,
    peerDispatchTicketDestinationHeraldEpoch,
  )
import Eclips.Herald.PeerDispatch.Internal (peerPublicationItem)
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    peerPublicationBatch,
    publicationBatchId,
  )
import Eclips.Herald.PeerStream
  ( PeerDispatchOutcome (..),
    SequencedItem,
    gapSummaryEntries,
    resumeOfferGapSummary,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( HeraldPhase (..),
    HeraldState,
    heraldPhase,
    replaceStartupAdministrationState,
    replaceStartupApplicationState,
    replaceStartupDrainIdForInvariantTest,
    replaceStartupLastObservedTime,
    replaceStartupOracleProjectionState,
    startupAdministrationState,
    startupApplicationState,
    startupDrainWitness,
    startupGraphState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPublicationState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureHeraldRetirementReason,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    fixtureStep14CheckedGenesis,
    initialEffectsAreOracleWatchAndGrace,
  )
import PeerInputProperties qualified
import PrimordialTestAccess (conventionalStartupPairs)
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
  )

tests :: TestTree
tests =
  testGroup
    "Herald transition"
    [ testCase "initialization installs a serving administration owner" caseInitialServing,
      testProperty "equal initialized state and input are deterministic" propDeterministic,
      testCase "a remote-initiated Hello is offered before binding disposition and control" caseRemoteInitiatedHelloOrder,
      testCase "local membership retirement fences fresh peer work while a survivor is unchanged" caseLocalMembershipFence,
      testCase "a successor-generation structural batch waits for the successor base" caseSuccessorStructuralBaseHold,
      testCase "a retained incoming gap suppresses unchanged structural progress" caseIncomingGapRepairOrder,
      testCase "every reachable publication protocol violation closes only its binding" casePeerPublicationProtocolMatrix,
      testCase "open success emits one atomic candidate disposition" caseOpenDisposition,
      testCase "open rejection targets the candidate lane without owner mutation" caseOpenRejection,
      testCase "resume success advances and emits one atomic disposition" caseResumeDisposition,
      testProperty "an exact live handover retry reoffers the retained disposition" propLiveResumeRetry,
      testCase "stale binding loss preserves its replacement and current loss ends the session" caseApplicationBindingLoss,
      testCase "end success is silent and removes only the session" caseEndSession,
      testCase "serving rejects a lower monotonic observation" caseServingTimeRegression,
      testCase "orderly shutdown retains one request and begins draining" caseBeginDrain,
      testCase "post-cut semantic ingress advances only observed time" casePostCutIngress,
      testCase "post-cut application open resume and end are not admitted" casePostCutApplicationIngress,
      testCase "draining application binding loss settles delivery only" caseDrainingApplicationBindingLoss,
      testProperty "a pre-cut peer attempt must settle before the drain barrier" propDrainingPeerAttemptSettlement,
      testCase "a nonmatching barrier advances only observed time" caseNonmatchingBarrier,
      testCase "stale binding observations advance only observed time" caseStaleBindingObservations,
      testCase "draining rejects a lower monotonic observation" caseDrainingTimeRegression,
      testCase "matching barrier stops and carries the final reply" caseFinishDrain,
      testCase "administration binding loss suppresses only the final reply" caseLostFinalReply,
      testCase "stopped is identical and empty even for lower time" caseStoppedTotal,
      testProperty "end mismatch emits a binding-targeted typed rejection" propEndMismatchRejection,
      testCase "phase and administration drain mismatch is an invariant fault" caseDrainMismatchInvariant,
      testCase "foreign administration binding is an invariant fault" caseAdminBindingInvariant,
      testCase "administration allocation mismatch is an invariant fault" caseAdminCounterInvariant,
      testCase "attachment contradiction is detected" caseAttachmentInvariant,
      testCase "session retry-index contradiction is detected" caseSessionRetryInvariant,
      testCase "session startup-access contradiction is detected" caseSessionAccessInvariant,
      testCase "session set contradiction is detected" caseSessionSetInvariant,
      testCase "session identity contradiction is detected" caseSessionIdentityInvariant,
      testCase "session allocation contradiction is detected" caseSessionAllocationInvariant,
      testCase "whole-state validation accepts later localized aliases" caseComposedLocalizationInvariant
    ]

caseInitialServing :: Assertion
caseInitialServing = do
  let state = emptyInitializedAt 10
      witness =
        Administration.administrationStateWitness
          (startupAdministrationState state)
  assertEqual "serving phase" HeraldServing (heraldPhase state)
  assertEqual "no drain reference" Nothing (startupDrainWitness state)
  assertEqual
    "checked logical binding"
    (checkedInitialAdministrationBinding fixtureCheckedGenesis)
    (Administration.administrationWitnessBinding witness)
  assertEqual "no shutdown record" Nothing (Administration.administrationWitnessShutdown witness)
  assertEqual "first drain ordinal" 1 (Administration.administrationWitnessNextDrainOrdinal witness)

caseLocalMembershipFence :: Assertion
caseLocalMembershipFence = do
  let bootstraps = checkedInitial fixtureLocalBootstrapIds
      openCandidate = candidateApplicationLane 905
      (survivor, openEffects) = openedSession openCandidate 905
      acceptance = acceptedDispositionValue "pre-retirement session" openCandidate openEffects
      (session, token) = openedIdentity acceptance
      attachment =
        maybe
          (error "missing local primordial application attachment")
          id
          (primordialApplicationAttachment fixtureCheckedGenesis bootstraps firstLocalBootstrapId)
      bodies =
        [ PeerInput (PeerCandidateOpened (peerCandidateOpened (connectionNonce 901) Set.empty Nothing)),
          AdministrationInput
            ( OpenAdministrationConnection
                (candidateAdministrationLane 902)
                (checkedSystemId fixtureCheckedGenesis)
            ),
          ApplicationSessionInput
            ( OpenApplicationSession
                (candidateApplicationLane 903)
                attachment
                (SessionInternal.clientNonce 904)
            ),
          ApplicationSessionInput
            ( ResumeApplicationSession
                (candidateApplicationLane 906)
                session
                token
                (sessionAcceptanceCursor acceptance)
            )
        ]
      projection = startupOracleProjectionState (fixtureHeraldAfterProbePrefix survivor)
      view = OracleProjection.oracleView projection
      index = controlIndex 2
      local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      successor =
        checkedEither
          "local-retired generation"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) local (OracleProjection.oracleViewCurrentHeraldMembership view))
      ends =
        [ OracleProjection.membershipAdvanceProcessEnd process index fixtureHeraldRetirementReason
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleProjectionProcessResidenceAt (OracleProjection.oracleViewControlIndex view) process projection == Just local,
          OracleProjection.oracleProjectionProcessIsLiveAt (OracleProjection.oracleViewControlIndex view) process projection
        ]
      prepared =
        checkedEither
          "local-retired projection"
          (OracleProjection.prepareMembershipAdvance index successor ends projection)
      retired = replaceStartupOracleProjectionState (OracleProjection.commitMembershipAdvance prepared) (fixtureHeraldAfterProbePrefix survivor)
  mapM_
    ( \body -> do
        let input = heraldInput (monotonicInstant 12) body
        case stepHerald input survivor of
          Right (_, effects) ->
            assertBool ("a survivor remains active for " <> show body) (not (effectBatchIsEmpty effects))
          Left problem -> assertFailure ("survivor transition failed: " <> show problem)
        case stepHerald input retired of
          Right (after, effects) -> do
            case body of
              PeerInput (PeerCandidateOpened opened) ->
                assertEqual
                  "retired peer candidate receives exactly one closing disposition"
                  [RejectPeerCandidateOpened (peerCandidateOpenedConnectionNonce opened)]
                  (effectBatchMembers effects)
              PeerInput (PeerHelloReceived candidate _ _ _ _ _) ->
                assertEqual
                  "retired peer Hello receives exactly one inactive disposition"
                  [SetPeerCandidateDisposition candidate (PeerHelloRejected HelloInactiveHerald)]
                  (effectBatchMembers effects)
              ApplicationSessionInput (OpenApplicationSession candidate _ _) -> expectRejected candidate effects
              ApplicationSessionInput (ResumeApplicationSession candidate _ _ _) -> expectRejected candidate effects
              AdministrationInput (OpenAdministrationConnection candidate _) -> case effectBatchMembers effects of
                [SetAdministrationConnectionDisposition observed binding] -> do
                  assertEqual "retired operator candidate is admitted for local inspection" candidate observed
                  assertEqual "inspection operator becomes current" (Just binding) (Administration.currentConfiguredAdministrationBinding (startupAdministrationState after))
                other -> assertFailure ("retired operator disposition: " <> show other)
              _ -> assertBool ("retired local Herald is inert for " <> show body) (effectBatchIsEmpty effects)
            case body of
              AdministrationInput (OpenAdministrationConnection _ _) -> assertOnlyTimeAdvancedTo 12 retired (replaceStartupAdministrationState (startupAdministrationState retired) after)
              _ -> assertOnlyTimeAdvancedTo 12 retired after
          Left problem -> assertFailure ("retired transition failed: " <> show problem)
    )
    bodies
  where
    expectRejected candidate effects =
      assertEqual
        "retired application candidate receives exactly one closing rejection"
        [RejectApplicationConnection (CandidateApplicationDisposition candidate) ApplicationSessionNotLive]
        (effectBatchMembers effects)

caseSuccessorStructuralBaseHold :: Assertion
caseSuccessorStructuralBaseHold = do
  fixture <- successorStructuralHeldFixture
  ordinaryItem <- PeerInputProperties.fixtureSuccessorGenerationOrdinaryItem
  let structuralItem = successorStructuralHeldItem fixture
      afterMembership = successorStructuralHeldPredecessor fixture
      structuralHeld = successorStructuralHeldState fixture
      successorMembership = successorStructuralHeldMembership fixture
      structuralBefore = startupStructuralProgressState afterMembership
      (ordinaryApplied, _) =
        stepped
          "admit ordinary successor-generation publication"
          ( heraldInput
              (monotonicInstant 3)
              (PeerInput (peerPublicationReceived (successorStructuralHeldBinding fixture) (peerPublicationItem ordinaryItem)))
          )
          afterMembership
      ordinaryIdentifier =
        publicationBatchId
          (peerPublicationBatch (sequencedItemPayload ordinaryItem))
      structuralIdentifier =
        publicationBatchId
          (peerPublicationBatch (sequencedItemPayload structuralItem))
      successorId = heraldMembershipGenerationId successorMembership
  ordinaryRecord <-
    maybe
      (assertFailure "ordinary successor-generation publication was not retained")
      pure
      (Publication.lookupIncomingPublication ordinaryIdentifier (startupPublicationState ordinaryApplied))
  assertEqual
    "contraction does not freeze ordinary publication work"
    Publication.Applied
    (Publication.incomingPublicationDisposition ordinaryRecord)
  assertEqual
    "ordinary successor-generation control remains whole-state valid"
    (Right ())
    (validateHeraldState ordinaryApplied)
  structuralRecord <-
    maybe
      (assertFailure "successor-generation structural publication was not retained")
      pure
      (Publication.lookupIncomingPublication structuralIdentifier (startupPublicationState structuralHeld))
  assertEqual
    "first successor-generation admission captures the exact successor"
    successorId
    (Publication.incomingPublicationMembershipGenerationId structuralRecord)
  assertEqual
    "structural work waits on the missing successor structural base"
    (Set.singleton (Publication.SuccessorStructuralBaseDependency successorId))
    (Publication.incomingPublicationDependencies structuralRecord)
  assertEqual
    "successor structural work remains dependency-held"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition structuralRecord)
  assertBool
    "every frozen structural destination remains pending"
    ( all
        (== Publication.DestinationPending)
        (Map.elems (Publication.incomingPublicationDestinationOutcomes structuralRecord))
    )
  retainedStream <-
    pure
      ( checkedEither
          "read successor-generation structural stream"
          ( PeerStream.incomingRetainedItems
              (sequencedItemDirection structuralItem)
              (startupPeerStreamState structuralHeld)
          )
      )
  assertEqual
    "the structural item advances received but not semantic completion"
    [PeerStream.IncomingReceivedPending]
    (snd <$> retainedStream)
  assertEqual
    "the direct membership commit leaves its readiness notification pending"
    True
    (fst (GraphProgress.takePreparationReadinessChange structuralBefore))
  assertEqual
    "the serving driver consumes that prior readiness notification"
    False
    (fst (GraphProgress.takePreparationReadinessChange (startupStructuralProgressState structuralHeld)))
  assertEqual
    "the direct membership commit leaves its alignment plan notification pending"
    True
    (fst (GraphProgress.takeAlignmentPlanChange structuralBefore))
  assertEqual
    "the serving driver consumes that prior alignment plan notification"
    False
    (fst (GraphProgress.takeAlignmentPlanChange (startupStructuralProgressState structuralHeld)))
  assertBool
    "the staging hold only drains the two structural scheduling notifications"
    ( startupStructuralProgressState structuralHeld
        == GraphProgress.clearAlignmentPlanChange (GraphProgress.clearPreparationReadinessChange structuralBefore)
    )
  assertBool
    "the staging hold mutates neither Store nor Graph"
    ( startupStoreState structuralHeld == startupStoreState afterMembership
        && startupGraphState structuralHeld == startupGraphState afterMembership
    )
  assertEqual
    "the successor-generation hold preserves every whole-state invariant"
    (Right ())
    (validateHeraldState structuralHeld)

data SuccessorStructuralHoldFixture = SuccessorStructuralHoldFixture
  { successorStructuralHeldMembershipPredecessor :: HeraldState,
    successorStructuralHeldPredecessor :: HeraldState,
    successorStructuralHeldState :: HeraldState,
    successorStructuralHeldMembership :: HeraldMembershipGeneration,
    successorStructuralHeldBinding :: PeerBinding,
    successorStructuralHeldItem :: SequencedItem PeerPublication
  }

successorStructuralHeldFixture :: IO SuccessorStructuralHoldFixture
successorStructuralHeldFixture = do
  structuralItem <- PeerInputProperties.fixtureSuccessorGenerationStructuralItem
  let selectedBootstraps = take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
      bootstraps =
        checkedEither
          "successor-base checked bootstraps"
          ( checkInitialBootstraps
              fixtureStep14CheckedGenesis
              (PrimordialProcessManifest selectedBootstraps)
          )
      initial =
        fst
          ( checkedEither
              "successor-base initial Herald"
              ( initialHerald
                  (monotonicInstant 0)
                  fixtureStep14CheckedGenesis
                  bootstraps
                  fixtureOracleContacts
                  fixtureGeneratorSeed
                  fixtureApplicationRecoveryConfiguration
                  fixturePeerRecoveryConfiguration
              )
          )
      client = startupOracleClientState initial
  oracleAttempt <- case OracleClient.oracleClientActions client of
    [ConnectAndHelloOracle attempt _] -> pure attempt
    actions -> assertFailure ("expected one initial Oracle attempt, got " <> show actions)
  let oracleNode = oracleContactNode (oracleConnectAttemptContact oracleAttempt)
      oracleAcceptance =
        oracleHelloAcceptance
          oracleNode
          (oracleObservedTerm 1)
          (oracleConnectAttemptFromExclusive oracleAttempt)
          (Just oracleNode)
          True
      (oracleBound, _) =
        stepped
          "bind Oracle before successor-base fixture"
          ( heraldInput
              (monotonicInstant 1)
              (OracleInput (OracleHelloReceived oracleAttempt oracleAcceptance))
          )
          initial
      source = heraldMemberEpoch fixtureRemoteMember
      nonce = connectionNonce 905
      candidate =
        peerCandidate
          (heraldMemberId fixtureRemoteMember)
          source
          nonce
      hello =
        peerHello
          (checkedSystemId fixtureStep14CheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          source
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureStep14CheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      (beforePrefix, helloEffects) =
        stepped
          "connect survivor source before successor-base fixture"
          ( heraldInput
              (monotonicInstant 2)
              (PeerInput (currentPeerHelloReceived oracleBound candidate Set.empty hello))
          )
          oracleBound
  binding <- case effectBatchMembers helloEffects of
    SendPeerCandidate _ _ _ _
      : SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted)
      : _ -> pure accepted
    actual -> assertFailure ("successor-base Hello emitted unexpected effects: " <> show actual)
  retiredMember <-
    case filter
      ( \member ->
          heraldMemberEpoch member
            /= checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
            && heraldMemberEpoch member /= source
      )
      (checkedActiveHeralds fixtureStep14CheckedGenesis) of
      [member] -> pure member
      observed -> assertFailure ("expected one non-source retirement target, got " <> show observed)
  let connected = fixtureHeraldAfterProbePrefix beforePrefix
      projection = startupOracleProjectionState connected
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView projection)
      index = controlIndex 2
      successorMembership =
        checkedEither
          "successor-base membership"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) (heraldMemberEpoch retiredMember) membership)
      preparedAdvance =
        checkedEither
          "successor-base membership advance"
          (MembershipAdvance.prepareMembershipAdvance index successorMembership [] connected)
      afterMembership = MembershipAdvance.commitMembershipAdvance preparedAdvance
      (structuralHeld, _) =
        stepped
          "retain successor-generation structural publication"
          ( heraldInput
              (monotonicInstant 3)
              (PeerInput (peerPublicationReceived binding (peerPublicationItem structuralItem)))
          )
          afterMembership
  pure
    SuccessorStructuralHoldFixture
      { successorStructuralHeldMembershipPredecessor = connected,
        successorStructuralHeldPredecessor = afterMembership,
        successorStructuralHeldState = structuralHeld,
        successorStructuralHeldMembership = successorMembership,
        successorStructuralHeldBinding = binding,
        successorStructuralHeldItem = structuralItem
      }

propDeterministic :: Word16 -> Property
propDeterministic offset =
  counterexample "equal pure inputs diverged"
    $ first == second
  where
    state = emptyInitializedAt 10
    input =
      shutdownInput
        (11 + fromIntegral offset)
        (fromIntegral offset)
    first = verifiedStepHerald input state
    second = verifiedStepHerald input state

caseRemoteInitiatedHelloOrder :: Assertion
caseRemoteInitiatedHelloOrder = do
  let bootstraps = checkedInitial fixtureLocalBootstrapIds
      predecessor = initializedAt 10 fixtureLocalBootstrapIds
      nonce = connectionNonce 91
      candidate =
        peerCandidate
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
      hello =
        peerHello
          (checkedSystemId fixtureCheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureCheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      (successor, effects) =
        stepped
          "remote-initiated Hello"
          ( heraldInput
              (monotonicInstant 11)
              (PeerInput (currentPeerHelloReceived predecessor candidate Set.empty hello))
          )
          predecessor
  binding <- case effectBatchMembers effects of
    SendPeerCandidate offeredCandidate _ _ _
      : SetPeerCandidateDisposition disposedCandidate (PeerHelloAccepted _ binding)
      : [ SendPeerControl knownBinding (PeerKnownHeralds _),
          SendPeerControl placementBinding (PeerPlacementUpdate _),
          SendPeerControl resumeBinding (PeerStreamResumeOffered _)
          ] -> do
        assertEqual "the reciprocal Hello retains the candidate correlation" candidate offeredCandidate
        assertEqual "the disposition follows the reciprocal Hello" candidate disposedCandidate
        assertEqual
          "Hello emits KnownHeralds, placement, and ResumeOffer under the admitted binding"
          [binding, binding, binding]
          [knownBinding, placementBinding, resumeBinding]
        pure binding
    actual ->
      assertFailure
        ("unexpected remote-initiated Hello effects: " <> show actual)
  let (_, reofferEffects) =
        stepped
          "remote-initiated Hello reoffer"
          ( heraldInput
              (monotonicInstant 12)
              (PeerInput (currentPeerHelloReceived successor candidate Set.empty hello))
          )
          successor
  case effectBatchMembers reofferEffects of
    SendPeerCandidate offeredCandidate _ _ _
      : SetPeerCandidateDisposition disposedCandidate (PeerHelloAccepted ReofferedBinding reofferedBinding)
      : [ SendPeerControl knownBinding (PeerKnownHeralds _),
          SendPeerControl placementBinding (PeerPlacementUpdate _),
          SendPeerControl resumeBinding (PeerStreamResumeOffered _)
          ] -> do
        assertEqual "the reciprocal reoffer retains the candidate correlation" candidate offeredCandidate
        assertEqual "the reoffered disposition retains the candidate correlation" candidate disposedCandidate
        assertEqual "the logical binding is reoffered unchanged" binding reofferedBinding
        assertEqual
          "Hello reoffer keeps the same bounded control order under the binding"
          [binding, binding, binding]
          [knownBinding, placementBinding, resumeBinding]
    actual ->
      assertFailure
        ("unexpected remote-initiated Hello reoffer effects: " <> show actual)

caseIncomingGapRepairOrder :: Assertion
caseIncomingGapRepairOrder = do
  futureItem <- PeerInputProperties.fixtureFuturePublicationItem
  let selectedBootstraps = take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
      bootstraps = checkedInitial selectedBootstraps
      predecessor = initializedAt 10 selectedBootstraps
      nonce = connectionNonce 92
      candidate =
        peerCandidate
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
      hello =
        peerHello
          (checkedSystemId fixtureCheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureCheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      (connected, helloEffects) =
        stepped
          "connect before incoming gap"
          ( heraldInput
              (monotonicInstant 11)
              (PeerInput (currentPeerHelloReceived predecessor candidate Set.empty hello))
          )
          predecessor
  binding <- case effectBatchMembers helloEffects of
    SendPeerCandidate _ _ _ _
      : SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted)
      : _ -> pure accepted
    actual ->
      assertFailure
        ("incoming gap connection emitted unexpected effects: " <> show actual)
  let (successor, effects) =
        stepped
          "retain future incoming publication"
          ( heraldInput
              (monotonicInstant 12)
              (PeerInput (peerPublicationReceived binding (peerPublicationItem futureItem)))
          )
          connected
      direction = sequencedItemDirection futureItem
      expectedGap =
        [(sequencedItemSequence futureItem, sequencedItemDigest futureItem)]
  assertEqual
    "retaining an incoming gap leaves the local structural report unchanged"
    (GraphProgress.structuralLocalReport (startupStructuralProgressState connected))
    (GraphProgress.structuralLocalReport (startupStructuralProgressState successor))
  case effectBatchMembers effects of
    [ SendPeerControl completedBinding (PeerStreamCompleted completedDirection completed),
      SendPeerControl repairBinding (PeerStreamResumeOffered offer)
      ] -> do
        assertEqual "completed acknowledgement binding" binding completedBinding
        assertEqual "repair offer binding" binding repairBinding
        assertEqual "completed acknowledgement direction" direction completedDirection
        assertEqual "completed prefix remains before the gap" mempty completed
        assertEqual
          "repair offer carries the committed incoming gap"
          expectedGap
          (gapSummaryEntries (resumeOfferGapSummary offer))
    actual ->
      assertFailure
        ("incoming gap emitted unexpected effects: " <> show actual)

casePeerPublicationProtocolMatrix :: Assertion
casePeerPublicationProtocolMatrix = do
  malformed <- PeerInputProperties.fixtureProtocolViolationItems
  let selectedBootstraps = take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
      bootstraps = checkedInitial selectedBootstraps
      predecessor = initializedAt 10 selectedBootstraps
      nonce = connectionNonce 93
      candidate =
        peerCandidate
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
      hello =
        peerHello
          (checkedSystemId fixtureCheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureCheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      (connected, helloEffects) =
        stepped
          "connect before publication protocol matrix"
          ( heraldInput
              (monotonicInstant 11)
              (PeerInput (currentPeerHelloReceived predecessor candidate Set.empty hello))
          )
          predecessor
  binding <- case effectBatchMembers helloEffects of
    SendPeerCandidate _ _ _ _
      : SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted)
      : _ -> pure accepted
    actual -> assertFailure ("protocol-matrix connection emitted unexpected effects: " <> show actual)
  mapM_
    ( \(ordinal, (label, item)) ->
        case verifiedStepHerald
          ( heraldInput
              (monotonicInstant (12 + fromIntegral ordinal))
              (PeerInput (peerPublicationReceived binding (peerPublicationItem item)))
          )
          connected of
          Right (successor, effects) -> do
            assertBool
              (label <> ": only the observed timestamp advances")
              ( replaceStartupLastObservedTime
                  (monotonicInstant (12 + fromIntegral ordinal))
                  connected
                  == successor
              )
            assertEqual
              (label <> ": exact binding-local close")
              [RejectPeerConnection binding ClosePeerPublicationProtocol]
              (effectBatchMembers effects)
          Left fault -> assertFailure (label <> ": malformed peer claim became invariant fault: " <> show fault)
    )
    (zip [(0 :: Int) ..] malformed)

caseOpenDisposition :: Assertion
caseOpenDisposition = do
  let candidate = candidateApplicationLane 21
      (state, effects) = openedSession candidate 1
  acceptance <- acceptedDisposition "open" candidate effects
  case sessionAcceptanceReply acceptance of
    SessionOpened session token _ -> do
      assertEqual
        "binding names opened session"
        session
        (SessionInternal.sessionBindingSessionId (sessionAcceptanceBinding acceptance))
      assertEqual
        "token shares the session allocation"
        (SessionInternal.applicationSessionIdOrdinal session)
        (SessionInternal.applicationResumeTokenOrdinal token)
    SessionResumed _ _ -> assertFailure "open emitted a resume acknowledgement"
  assertEqual
    "one retained session"
    1
    ( length
        ( Application.applicationWitnessSessions
            (Application.applicationStateWitness (startupApplicationState state))
        )
    )

caseOpenRejection :: Assertion
caseOpenRejection = do
  let predecessor = initializedAt 10 fixtureLocalBootstrapIds
      candidate = candidateApplicationLane 22
      unknownAttachment =
        SessionInternal.applicationAttachmentForBootstrap fixtureRemoteBootstrapId
      input =
        heraldInput
          (monotonicInstant 11)
          ( ApplicationSessionInput
              ( OpenApplicationSession
                  candidate
                  unknownAttachment
                  (SessionInternal.clientNonce 2)
              )
          )
      (successor, effects) = stepped "rejected open" input predecessor
  assertEqual
    "candidate-targeted rejection"
    [ RejectApplicationConnection
        (CandidateApplicationDisposition candidate)
        ApplicationAttachmentNotAdmitted
    ]
    (effectBatchMembers effects)
  assertOnlyTimeAdvancedTo 11 predecessor successor

caseResumeDisposition :: Assertion
caseResumeDisposition = do
  let openCandidate = candidateApplicationLane 23
      resumeCandidate = candidateApplicationLane 24
      (afterOpen, openEffects) = openedSession openCandidate 3
  opened <- acceptedDisposition "open before resume" openCandidate openEffects
  let (session, token) = openedIdentity opened
      previousBinding = sessionAcceptanceBinding opened
      lastObserved = sessionAcceptanceCursor opened
      resumeInput =
        heraldInput
          (monotonicInstant 12)
          ( ApplicationSessionInput
              ( ResumeApplicationSession
                  resumeCandidate
                  session
                  token
                  lastObserved
              )
          )
      (afterResume, resumeEffects) = stepped "resume" resumeInput afterOpen
  resumed <- acceptedDisposition "resume" resumeCandidate resumeEffects
  case sessionAcceptanceReply resumed of
    SessionResumed resumedSession _ ->
      assertEqual "resumed session" session resumedSession
    reply -> assertFailure ("expected resume acknowledgement, got " <> show reply)
  assertEqual
    "binding advances once"
    (SessionInternal.advanceSessionBinding previousBinding)
    (sessionAcceptanceBinding resumed)
  assertBool "resume changes the application owner" (startupApplicationState afterResume /= startupApplicationState afterOpen)

propLiveResumeRetry :: Word16 -> Property
propLiveResumeRetry laneBits =
  counterexample "exact live handover retry changed the retained disposition"
    $ case ( effectBatchMembers firstResumeEffects,
             effectBatchMembers retryEffects,
             effectBatchMembers newResumeEffects,
             effectBatchMembers staleEffects
           ) of
      ( [SetApplicationConnectionDisposition firstCandidate firstAcceptance],
        [SetApplicationConnectionDisposition retryCandidate retryAcceptance],
        [SetApplicationConnectionDisposition newCandidate newAcceptance],
        [SetApplicationConnectionDisposition staleCandidate staleAcceptance]
        ) ->
          firstCandidate == resumeCandidate
            && retryCandidate == retryLane
            && newCandidate == newResumeLane
            && staleCandidate == staleLane
            && staleAcceptance == newAcceptance
            && sessionAcceptanceBinding retryAcceptance
              == sessionAcceptanceBinding firstAcceptance
            && sessionAcceptanceReply retryAcceptance
              == sessionAcceptanceReply firstAcceptance
            && onlySessionBinding afterRetry
              == sessionAcceptanceBinding firstAcceptance
            && onlySessionDeliveryLive afterRetry
            && sessionAcceptanceBinding newAcceptance
              == SessionInternal.advanceSessionBinding currentBinding
            && replaceStartupLastObservedTime
              (monotonicInstant 15)
              afterStale
              == afterNewResume
            && Application.applicationWitnessNextSessionOrdinal
              (Application.applicationStateWitness (startupApplicationState afterRetry))
              == Application.applicationWitnessNextSessionOrdinal
                (Application.applicationStateWitness (startupApplicationState afterOpen))
      _ -> False
  where
    openCandidate = candidateApplicationLane (fromIntegral laneBits)
    resumeCandidate = candidateApplicationLane (fromIntegral laneBits + 1)
    retryLane = candidateApplicationLane (fromIntegral laneBits + 2)
    newResumeLane = candidateApplicationLane (fromIntegral laneBits + 3)
    staleLane = candidateApplicationLane (fromIntegral laneBits + 4)
    (afterOpen, openEffects) = openedSession openCandidate 33
    opened = acceptedDispositionValue "open before resume retry" openCandidate openEffects
    (session, token) = openedIdentity opened
    originalCursor = sessionAcceptanceCursor opened
    firstResumeInput =
      heraldInput
        (monotonicInstant 12)
        ( ApplicationSessionInput
            ( ResumeApplicationSession
                resumeCandidate
                session
                token
                originalCursor
            )
        )
    (afterFirstResume, firstResumeEffects) =
      stepped "first resume" firstResumeInput afterOpen
    currentBinding = onlySessionBinding afterFirstResume
    currentCursor =
      sessionAcceptanceCursor
        (acceptedDispositionValue "first resume" resumeCandidate firstResumeEffects)
    retryInput =
      heraldInput
        (monotonicInstant 14)
        ( ApplicationSessionInput
            ( ResumeApplicationSession
                retryLane
                session
                token
                originalCursor
            )
        )
    (afterRetry, retryEffects) = stepped "exact live resume retry" retryInput afterFirstResume
    newResumeInput =
      heraldInput
        (monotonicInstant 15)
        ( ApplicationSessionInput
            ( ResumeApplicationSession
                newResumeLane
                session
                token
                currentCursor
            )
        )
    (afterNewResume, newResumeEffects) =
      stepped "new resume after retry" newResumeInput afterRetry
    staleInput =
      heraldInput
        (monotonicInstant 16)
        ( ApplicationSessionInput
            ( ResumeApplicationSession
                staleLane
                session
                token
                originalCursor
            )
        )
    (afterStale, staleEffects) =
      stepped "older stale resume" staleInput afterNewResume

caseApplicationBindingLoss :: Assertion
caseApplicationBindingLoss = do
  let openCandidate = candidateApplicationLane 25
      resumeCandidate = candidateApplicationLane 26
      deadResumeCandidate = candidateApplicationLane 27
      (afterOpen, openEffects) = openedSession openCandidate 4
  opened <- acceptedDisposition "open before handover" openCandidate openEffects
  let (session, token) = openedIdentity opened
      oldBinding = sessionAcceptanceBinding opened
      lastObserved = sessionAcceptanceCursor opened
      (afterResume, resumeEffects) =
        stepped
          "live application handover"
          ( heraldInput
              (monotonicInstant 12)
              ( ApplicationSessionInput
                  (ResumeApplicationSession resumeCandidate session token lastObserved)
              )
          )
          afterOpen
  resumed <- acceptedDisposition "live application handover" resumeCandidate resumeEffects
  let binding = sessionAcceptanceBinding resumed
      (afterStaleLoss, staleEffects) =
        stepped
          "old physical binding loss"
          (heraldInput (monotonicInstant 13) (RuntimeObserved (ApplicationBindingLost oldBinding)))
          afterResume
  assertBool "stale loss emits nothing" (effectBatchIsEmpty staleEffects)
  assertOnlyTimeAdvancedTo 13 afterResume afterStaleLoss
  assertEqual "stale loss cannot clear the replacement binding" binding (onlySessionBinding afterStaleLoss)
  let (afterLoss, lossEffects) =
        stepped
          "current application connection loss"
          (heraldInput (monotonicInstant 14) (RuntimeObserved (ApplicationBindingLost binding)))
          afterStaleLoss
  assertEqual
    "current connection loss immediately disposes its logical session"
    [DisposeApplicationSession session ApplicationSessionNoLongerLive]
    (effectBatchMembers lossEffects)
  assertEqual
    "no session survives current connection loss"
    Nothing
    (Application.applicationSessionProcess session (startupApplicationState afterLoss))
  let (afterRejected, rejectedEffects) =
        stepped
          "Resume after terminal loss"
          ( heraldInput
              (monotonicInstant 15)
              ( ApplicationSessionInput
                  (ResumeApplicationSession deadResumeCandidate session token lastObserved)
              )
          )
          afterLoss
  assertEqual
    "Resume cannot resurrect a lost application lifetime"
    [RejectApplicationConnection (CandidateApplicationDisposition deadResumeCandidate) ApplicationSessionNotLive]
    (effectBatchMembers rejectedEffects)
  let (afterDuplicate, duplicateEffects) =
        stepped
          "duplicate current loss"
          (heraldInput (monotonicInstant 16) (RuntimeObserved (ApplicationBindingLost binding)))
          afterRejected
  assertBool "duplicate terminal loss emits nothing" (effectBatchIsEmpty duplicateEffects)
  assertOnlyTimeAdvancedTo 16 afterRejected afterDuplicate

caseEndSession :: Assertion
caseEndSession = do
  let candidate = candidateApplicationLane 27
      (afterOpen, openEffects) = openedSession candidate 5
  opened <- acceptedDisposition "open before end" candidate openEffects
  let binding = sessionAcceptanceBinding opened
      session = sessionReplySessionId (sessionAcceptanceReply opened)
      endInput =
        heraldInput
          (monotonicInstant 12)
          (ApplicationSessionInput (EndApplicationSession binding session))
      (afterEnd, effects) = stepped "end session" endInput afterOpen
  assertBool "successful end emits nothing" (effectBatchIsEmpty effects)
  assertEqual
    "session removed"
    []
    ( Application.applicationWitnessSessions
        (Application.applicationStateWitness (startupApplicationState afterEnd))
    )

caseServingTimeRegression :: Assertion
caseServingTimeRegression =
  assertTransitionFault
    HeraldObservationTimeRegressed
    ( verifiedStepHerald
        ( heraldInput
            (monotonicInstant 9)
            (RuntimeObserved (DrainBarrierObserved (AdministrationInternal.drainId 1)))
        )
        (emptyInitializedAt 10)
    )

caseBeginDrain :: Assertion
caseBeginDrain = do
  let (state, effects) = begunDrain
      expectedDrain = AdministrationInternal.drainId 1
      witness =
        Administration.administrationStateWitness
          (startupAdministrationState state)
  assertEqual "draining phase" HeraldDraining (heraldPhase state)
  assertEqual "one begin effect" [BeginDrain expectedDrain] (effectBatchMembers effects)
  assertEqual
    "pending owner record"
    ( Just
        ( adminCorrelationId 7,
          expectedDrain,
          Administration.AdministrationShutdownPending
        )
    )
    (Administration.administrationWitnessShutdown witness)
  assertEqual "one allocation" 2 (Administration.administrationWitnessNextDrainOrdinal witness)

casePostCutIngress :: Assertion
casePostCutIngress = do
  let (draining, _) = begunDrain
      duplicate = shutdownInput 12 8
      (successor, effects) = stepped "post-cut administration ingress" duplicate draining
  assertOnlyTimeAdvanced draining successor
  assertBool "post-cut ingress emits nothing" (effectBatchIsEmpty effects)

casePostCutApplicationIngress :: Assertion
casePostCutApplicationIngress = do
  let openCandidate = candidateApplicationLane 41
      (afterOpen, openEffects) = openedSession openCandidate 41
  opened <- acceptedDisposition "open before drain cut" openCandidate openEffects
  let (session, token) = openedIdentity opened
      binding = sessionAcceptanceBinding opened
      lastObserved = sessionAcceptanceCursor opened
      (draining, beginEffects) =
        stepped "drain after open" (shutdownInput 12 41) afterOpen
  assertEqual
    "drain began"
    [BeginDrain (AdministrationInternal.drainId 1)]
    (effectBatchMembers beginEffects)
  let ignoredOpen =
        heraldInput
          (monotonicInstant 13)
          ( ApplicationSessionInput
              ( OpenApplicationSession
                  (candidateApplicationLane 42)
                  firstLocalAttachment
                  (SessionInternal.clientNonce 42)
              )
          )
      (afterIgnoredOpen, openEffectsAfterCut) =
        stepped "open after drain cut" ignoredOpen draining
  assertOnlyTimeAdvancedTo 13 draining afterIgnoredOpen
  assertBool "post-cut open emits nothing" (effectBatchIsEmpty openEffectsAfterCut)
  let ignoredResume =
        heraldInput
          (monotonicInstant 14)
          ( ApplicationSessionInput
              ( ResumeApplicationSession
                  (candidateApplicationLane 43)
                  session
                  token
                  lastObserved
              )
          )
      (afterIgnoredResume, resumeEffectsAfterCut) =
        stepped "resume after drain cut" ignoredResume afterIgnoredOpen
  assertOnlyTimeAdvancedTo 14 afterIgnoredOpen afterIgnoredResume
  assertBool "post-cut resume emits nothing" (effectBatchIsEmpty resumeEffectsAfterCut)
  let ignoredEnd =
        heraldInput
          (monotonicInstant 15)
          (ApplicationSessionInput (EndApplicationSession binding session))
      (afterIgnoredEnd, endEffectsAfterCut) =
        stepped "end after drain cut" ignoredEnd afterIgnoredResume
  assertOnlyTimeAdvancedTo 15 afterIgnoredResume afterIgnoredEnd
  assertBool "post-cut end emits nothing" (effectBatchIsEmpty endEffectsAfterCut)
  assertEqual "phase remains draining" HeraldDraining (heraldPhase afterIgnoredEnd)

caseDrainingApplicationBindingLoss :: Assertion
caseDrainingApplicationBindingLoss = do
  let candidate = candidateApplicationLane 44
      (afterOpen, openEffects) = openedSession candidate 44
  opened <- acceptedDisposition "open before draining loss" candidate openEffects
  let binding = sessionAcceptanceBinding opened
      (draining, _) = stepped "drain before binding loss" (shutdownInput 12 44) afterOpen
      beforeApplication = startupApplicationState draining
      beforeSession = onlySessionWitness draining
      lossInput =
        heraldInput
          (monotonicInstant 13)
          (RuntimeObserved (ApplicationBindingLost binding))
      (afterLoss, effects) = stepped "draining application binding loss" lossInput draining
      afterApplication = startupApplicationState afterLoss
      afterSession = onlySessionWitness afterLoss
  assertBool "draining loss emits nothing" (effectBatchIsEmpty effects)
  assertEqual "phase remains draining" HeraldDraining (heraldPhase afterLoss)
  assertEqual "drain reference is retained" (startupDrainWitness draining) (startupDrainWitness afterLoss)
  assertEqual
    "process-private map is retained"
    (Application.applicationProcessWitnesses beforeApplication)
    (Application.applicationProcessWitnesses afterApplication)
  assertOnlyTimeAdvancedTo 13 draining afterLoss
  assertBool
    "the drain-cut application owner is unchanged"
    (beforeApplication == afterApplication)
  assertEqual "the drain-cut session is unchanged" beforeSession afterSession
  assertBool
    "the orderly drain had already removed delivery reachability"
    (not (Application.applicationSessionWitnessDeliveryLive afterSession))

propDrainingPeerAttemptSettlement :: Word16 -> Property
propDrainingPeerAttemptSettlement seed =
  counterexample diagnostic (result == Right ())
  where
    outcome = case seed `mod` 3 of
      0 -> PeerDispatchWritten
      1 -> PeerDispatchDeferred
      _ -> PeerDispatchFailed
    result = drainingPeerAttemptTrace seed outcome
    diagnostic =
      "pre-cut peer attempt drain trace failed for "
        <> show outcome
        <> ": "
        <> either id (const "unexpected success comparison") result

-- Exercise the phase boundary through only ordinary product inputs: a checked
-- two-Herald handshake supplies remote placement, an application write creates
-- the retained publication, and dispatch selection hands one exact attempt to
-- the runtime before shutdown takes the admission cut.
drainingPeerAttemptTrace ::
  Word16 ->
  PeerDispatchOutcome ->
  Either String ()
drainingPeerAttemptTrace seed outcome = do
  remoteGenesis <-
    checkedResult
      "remote Herald genesis"
      (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  localBootstraps <- checkedStep7Topology fixtureCheckedGenesis
  remoteBootstraps <- checkedStep7Topology remoteGenesis
  localInitial <- initializedWithTopology fixtureCheckedGenesis localBootstraps
  remoteInitial <- initializedWithTopology remoteGenesis remoteBootstraps

  (localOffered, localOfferEffects) <-
    stepResult
      "offer peer Hello"
      ( heraldInput
          (monotonicInstant 1)
          ( PeerInput
              ( PeerCandidateOpened
                  (peerCandidateOpened (connectionNonce (seedWord + 1)) Set.empty Nothing)
              )
          )
      )
      localInitial
  (candidate, hello) <-
    exactlyOneResult "local candidate Hello" (candidateEffects localOfferEffects)
  (remoteAccepted, remoteHelloEffects) <-
    stepResult
      "remote accepts peer Hello"
      ( heraldInput
          (monotonicInstant 1)
          (PeerInput (currentPeerHelloReceived remoteInitial candidate Set.empty hello))
      )
      remoteInitial
  (responseCandidate, responseHello) <-
    exactlyOneResult "reciprocal peer Hello" (candidateEffects remoteHelloEffects)
  requireResult
    "reciprocal Hello changed candidate correlation"
    (responseCandidate == candidate)
  (localConnected, localHelloEffects) <-
    stepResult
      "local accepts reciprocal Hello"
      ( heraldInput
          (monotonicInstant 2)
          (PeerInput (currentPeerHelloReceived localOffered responseCandidate Set.empty responseHello))
      )
      localOffered
  localBinding <-
    exactlyOneResult "local peer binding" (acceptedPeerBindings localHelloEffects)
  remotePlacement <-
    exactlyOneResult "remote placement snapshot" (placementControls remoteHelloEffects)
  (localPlaced, _) <-
    stepResult
      "local applies remote placement"
      ( heraldInput
          (monotonicInstant 3)
          (PeerInput (PeerControlReceived localBinding remotePlacement))
      )
      localConnected

  attachment <-
    maybe
      (Left "missing local primordial application attachment")
      Right
      ( primordialApplicationAttachment
          fixtureCheckedGenesis
          localBootstraps
          firstLocalBootstrapId
      )
  (localOpened, openEffects) <-
    stepResult
      "open local application"
      ( heraldInput
          (monotonicInstant 4)
          ( ApplicationSessionInput
              ( OpenApplicationSession
                  (candidateApplicationLane (seedWord + 100))
                  attachment
                  (SessionInternal.clientNonce (seedWord + 200))
              )
          )
      )
      localPlaced
  acceptance <-
    exactlyOneResult
      "local application acceptance"
      [ accepted
      | SetApplicationConnectionDisposition _ accepted <- effectBatchMembers openEffects
      ]
  (session, access) <- case sessionAcceptanceReply acceptance of
    SessionOpened acceptedSession _ startupAccess -> Right (acceptedSession, startupAccess)
    other -> Left ("expected opened application session, got " <> show other)
  sortAccess <-
    maybe
      (Left "local application has no sort-definition access")
      Right
      ( find
          ((== Access.SortDefinitionRole) . Access.predefinedAccessRole)
          (conventionalStartupPairs access)
      )
  let binding = sessionAcceptanceBinding acceptance
      write =
        WriteApplication
          (Access.predefinedWriter sortAccess)
          ( PublishValue
              (SortDefinitionValue (DeclaredSortDefinition drainDeclaredDescriptor Nothing))
          )
  (localWritten, writeEffects) <-
    stepResult
      "write remotely routed definition"
      ( heraldInput
          (monotonicInstant 5)
          ( ApplicationRequestInput
              ( CallApplicationRequest
                  binding
                  session
                  (requestId 1)
                  write
              )
          )
      )
      localOpened
  ticket <-
    exactlyOneResult
      "remote publication dispatch ticket"
      [current | SchedulePeerDispatch current <- effectBatchMembers writeEffects]
  requireResult
    "public ticket destination does not name the remote Herald epoch"
    (peerDispatchTicketDestinationHeraldEpoch ticket == heraldMemberEpoch fixtureRemoteMember)
  requireResult
    "public ticket rendering exposed owner correlations"
    (show ticket == "PeerDispatchTicket")
  requireResult
    "surrounding ticket effect rendering exposed direction or generation"
    (renderingOmits ["StreamDirection", "PeerDispatchTicket "] (show (SchedulePeerDispatch ticket)))
  (localAttempting, sendEffects) <-
    stepResult
      "select peer dispatch attempt"
      ( heraldInput
          (monotonicInstant 6)
          (PeerInput (PeerDispatchSelected ticket localBinding))
      )
      localWritten
  attempt <-
    exactlyOneResult
      "selected peer dispatch attempt"
      [current | SendPeerItem _ current <- effectBatchMembers sendEffects]
  let publicationItem = peerDispatchAttemptItem attempt
      relayedIngress = peerPublicationReceived localBinding publicationItem
  requireResult
    "public dispatch-attempt rendering exposed owner correlations or payload"
    (show attempt == "PeerDispatchAttempt")
  requireResult
    "public publication-item rendering exposed owner correlations or payload"
    (show publicationItem == "PeerLogicalItem")
  requireResult
    "surrounding peer ingress did not preserve opaque publication rendering"
    ( "PeerPublicationReceived" `isInfixOf` show relayedIngress
        && renderingOmits
          ["SequencedItem", "PeerItemDigest", "PublicationBatch", "StreamDirection"]
          (show relayedIngress)
    )
  requireResult
    "surrounding attempt effect or batch rendering exposed publication fields"
    ( renderingOmits
        ["SequencedItem", "PeerItemDigest", "PublicationBatch", "StreamDirection"]
        (show (SendPeerItem localBinding attempt) <> show sendEffects)
    )
  requireResult
    "surrounding outcome rendering exposed attempt correlations"
    ( renderingOmits
        ["SequencedItem", "PeerItemDigest", "PublicationBatch", "StreamDirection"]
        (show (PeerDispatchObserved attempt PeerDispatchWritten))
    )

  (draining, beginEffects) <-
    stepResult
      "take drain cut with handed peer attempt"
      (shutdownInput 7 (seedWord + 300))
      localAttempting
  requireResult
    "shutdown did not emit exactly one drain request"
    (effectBatchMembers beginEffects == [BeginDrain (AdministrationInternal.drainId 1)])
  case verifiedStepHerald
    ( heraldInput
        (monotonicInstant 8)
        (RuntimeObserved (DrainBarrierObserved (AdministrationInternal.drainId 1)))
    )
    draining of
    Left (HeraldStartupInvariant StartupStatic PeerStreamOwnerInvariant) -> Right ()
    Left other -> Left ("premature drain barrier returned the wrong fault: " <> show other)
    Right _ -> Left "premature drain barrier completed with a handed peer attempt"

  (settled, settlementEffects) <-
    stepResult
      "settle the handed peer attempt while draining"
      ( heraldInput
          (monotonicInstant 8)
          (RuntimeObserved (PeerDispatchObserved attempt outcome))
      )
      draining
  requireResult
    "draining attempt settlement emitted replacement work"
    (effectBatchIsEmpty settlementEffects)
  (stopped, finishEffects) <-
    stepResult
      "complete drain after peer attempt settlement"
      ( heraldInput
          (monotonicInstant 9)
          (RuntimeObserved (DrainBarrierObserved (AdministrationInternal.drainId 1)))
      )
      settled
  requireResult "matching barrier did not stop the Herald" (heraldPhase stopped == HeraldStopped)
  requireResult
    "matching barrier did not emit the final drain effect"
    ( case effectBatchMembers finishEffects of
        [FinishDrain drain _] -> drain == AdministrationInternal.drainId 1
        _ -> False
    )
  requireResult
    "remote peer state was unexpectedly needed after its typed placement effect"
    (heraldPhase remoteAccepted == HeraldServing)
  where
    seedWord = fromIntegral seed * 16

caseNonmatchingBarrier :: Assertion
caseNonmatchingBarrier = do
  let (draining, _) = begunDrain
      input =
        heraldInput
          (monotonicInstant 12)
          ( RuntimeObserved
              (DrainBarrierObserved (AdministrationInternal.drainId 99))
          )
      (successor, effects) = stepped "nonmatching drain barrier" input draining
  assertOnlyTimeAdvanced draining successor
  assertBool "nonmatching barrier emits nothing" (effectBatchIsEmpty effects)

caseStaleBindingObservations :: Assertion
caseStaleBindingObservations = do
  let (draining, _) = begunDrain
      foreignBinding =
        AdministrationInternal.initialAdministrationBinding
          (heraldMemberEpoch fixtureRemoteMember)
      input =
        heraldInput
          (monotonicInstant 12)
          (RuntimeObserved (AdministrationBindingLost foreignBinding))
      (successor, effects) = stepped "stale administration binding loss" input draining
  assertOnlyTimeAdvanced draining successor
  assertBool "stale loss emits nothing" (effectBatchIsEmpty effects)

caseDrainingTimeRegression :: Assertion
caseDrainingTimeRegression =
  let (draining, _) = begunDrain
   in assertTransitionFault
        HeraldObservationTimeRegressed
        ( verifiedStepHerald
            ( heraldInput
                (monotonicInstant 10)
                ( RuntimeObserved
                    (DrainBarrierObserved (AdministrationInternal.drainId 1))
                )
            )
            draining
        )

caseFinishDrain :: Assertion
caseFinishDrain = do
  let (stopped, effects) = finishedDrain begunDrain
      expectedDrain = AdministrationInternal.drainId 1
  assertEqual "stopped phase" HeraldStopped (heraldPhase stopped)
  assertEqual
    "one finish effect with final reply"
    [ FinishDrain
        expectedDrain
        (Just (OrderlyHeraldShutdownCompleted (adminCorrelationId 7)))
    ]
    (effectBatchMembers effects)
  let witness =
        Administration.administrationStateWitness
          (startupAdministrationState stopped)
  assertEqual
    "owner records immutable completion"
    ( Just
        ( adminCorrelationId 7,
          expectedDrain,
          Administration.AdministrationShutdownCompleted
        )
    )
    (Administration.administrationWitnessShutdown witness)

caseLostFinalReply :: Assertion
caseLostFinalReply = do
  let (draining, _) = begunDrain
      binding = checkedInitialAdministrationBinding fixtureCheckedGenesis
      loss =
        heraldInput
          (monotonicInstant 12)
          (RuntimeObserved (AdministrationBindingLost binding))
      (afterLoss, lossEffects) = stepped "administration binding loss" loss draining
      (stopped, finishEffects) = finishedDrain (afterLoss, lossEffects)
  assertBool "binding loss emits nothing" (effectBatchIsEmpty lossEffects)
  assertEqual
    "finish remains one indivisible directive without reply"
    [FinishDrain (AdministrationInternal.drainId 1) Nothing]
    (effectBatchMembers finishEffects)
  assertEqual "stopped phase" HeraldStopped (heraldPhase stopped)

caseStoppedTotal :: Assertion
caseStoppedTotal = do
  let (stopped, _) = finishedDrain begunDrain
      lowerInput = shutdownInput 0 99
      (successor, effects) = stepped "stopped totality" lowerInput stopped
  assertBool "identical stopped state" (successor == stopped)
  assertEqual "canonical empty batch" emptyEffectBatch effects
  assertEqual "retained stopped observation" (monotonicInstant 13) (startupLastObservedTime successor)

propEndMismatchRejection :: Word16 -> Property
propEndMismatchRejection candidateBits =
  counterexample "end mismatch was not retained as an established-binding rejection"
    $ case effectBatchMembers openEffects of
      [SetApplicationConnectionDisposition _ acceptance] ->
        let binding = sessionAcceptanceBinding acceptance
            wrongBinding = SessionInternal.advanceSessionBinding binding
            session = sessionReplySessionId (sessionAcceptanceReply acceptance)
            endInput =
              heraldInput
                (monotonicInstant 12)
                ( ApplicationSessionInput
                    (EndApplicationSession wrongBinding session)
                )
            (afterEnd, endEffects) = stepped "end mismatch" endInput afterOpen
         in replaceStartupLastObservedTime (monotonicInstant 11) afterEnd == afterOpen
              && effectBatchMembers endEffects
                == [ RejectApplicationConnection
                       (EstablishedApplicationDisposition wrongBinding)
                       ApplicationEndSessionBindingMismatch
                   ]
      _ -> False
  where
    selected = checkedInitial fixtureLocalBootstrapIds
    state = initializedAt 10 fixtureLocalBootstrapIds
    attachment =
      checkedMaybe
        "first selected attachment"
        ( primordialApplicationAttachment
            fixtureCheckedGenesis
            selected
            firstLocalBootstrapId
        )
    openInput =
      heraldInput
        (monotonicInstant 11)
        ( ApplicationSessionInput
            ( OpenApplicationSession
                (candidateApplicationLane (fromIntegral candidateBits))
                attachment
                (SessionInternal.clientNonce 1)
            )
        )
    (afterOpen, openEffects) = stepped "open for end mismatch" openInput state

caseDrainMismatchInvariant :: Assertion
caseDrainMismatchInvariant = do
  let (draining, _) = begunDrain
      corrupted = replaceStartupDrainIdForInvariantTest (AdministrationInternal.drainId 99) draining
  assertEqual
    "drain relation"
    (Left (HeraldTransitionInvariant HeraldDrainOwnerContradiction))
    (validateHeraldState corrupted)

caseAdminBindingInvariant :: Assertion
caseAdminBindingInvariant = do
  let state = emptyInitializedAt 10
      administration = startupAdministrationState state
      foreignBinding =
        AdministrationInternal.initialAdministrationBinding
          (heraldMemberEpoch fixtureRemoteMember)
      corrupted =
        replaceStartupAdministrationState
          ( Administration.replaceAdministrationBindingForInvariantTest
              foreignBinding
              administration
          )
          state
  assertEqual
    "foreign binding"
    (Left (HeraldTransitionInvariant HeraldDrainOwnerContradiction))
    (validateHeraldState corrupted)

caseAdminCounterInvariant :: Assertion
caseAdminCounterInvariant = do
  let state = emptyInitializedAt 10
      corrupted =
        replaceStartupAdministrationState
          ( Administration.replaceAdministrationNextDrainOrdinalForInvariantTest
              4
              (startupAdministrationState state)
          )
          state
  assertEqual
    "wrong drain allocation"
    (Left (HeraldTransitionInvariant HeraldDrainOwnerContradiction))
    (validateHeraldState corrupted)

caseAttachmentInvariant :: Assertion
caseAttachmentInvariant = do
  let state = initializedAt 10 fixtureLocalBootstrapIds
      application = startupApplicationState state
      Application.ApplicationStateWitness attachments _ _ _ =
        Application.applicationStateWitness application
  case attachments of
    (attachment, _) : (_, wrongProcess) : _ ->
      assertEqual
        "attachment owner contradiction"
        (Left (HeraldStartupInvariant StartupStatic ApplicationAttachmentInvariant))
        ( validateHeraldState
            ( replaceStartupApplicationState
                ( Application.replaceApplicationAttachmentProcessForInvariantTest
                    attachment
                    wrongProcess
                    application
                )
                state
            )
        )
    _ -> assertFailure "fixture needs two resident attachments"

caseSessionRetryInvariant :: Assertion
caseSessionRetryInvariant = do
  let selected = checkedInitial fixtureLocalBootstrapIds
      state = initializedAt 10 fixtureLocalBootstrapIds
      attachment =
        checkedMaybe
          "first selected attachment"
          ( primordialApplicationAttachment
              fixtureCheckedGenesis
              selected
              firstLocalBootstrapId
          )
      openInput =
        heraldInput
          (monotonicInstant 11)
          ( ApplicationSessionInput
              ( OpenApplicationSession
                  (candidateApplicationLane 2)
                  attachment
                  (SessionInternal.clientNonce 2)
              )
          )
      (afterOpen, _) = stepped "open for retry contradiction" openInput state
      application = startupApplicationState afterOpen
      Application.ApplicationStateWitness _ sessions _ _ =
        Application.applicationStateWitness application
  case sessions of
    Application.ApplicationSessionWitness session process _ _ _ _ _ _ : _ ->
      let corruptedApplication =
            Application.replaceApplicationOpenRetryIndexForInvariantTest
              process
              (SessionInternal.clientNonce 999)
              session
              application
       in assertEqual
            "retry-index contradiction"
            ( Left
                ( HeraldStartupInvariant
                    StartupStatic
                    ApplicationSessionRetryIndexInvariant
                )
            )
            ( validateHeraldState
                (replaceStartupApplicationState corruptedApplication afterOpen)
            )
    [] -> assertFailure "open did not retain a session"

caseSessionAccessInvariant :: Assertion
caseSessionAccessInvariant = do
  let candidate = candidateApplicationLane 31
      (afterOpen, _) = openedSession candidate 31
      application = startupApplicationState afterOpen
      sessions =
        Application.applicationWitnessSessions
          (Application.applicationStateWitness application)
  case sessions of
    [sessionWitness] -> do
      let session = Application.applicationSessionWitnessId sessionWitness
          process = Application.applicationSessionWitnessProcess sessionWitness
          retainedAccess =
            Application.applicationSessionWitnessStartupAccess sessionWitness
          wrongProcess =
            asPrivateProcessId
              (checkedEither "private contradiction ID" (mkPrivateUniqueId 999))
          wrongAccess =
            applicationStartupAccess wrongProcess (Access.startupAccessPrimordial retainedAccess)
          corruptedApplication =
            Application.replaceApplicationSessionStartupAccessForInvariantTest
              session
              wrongAccess
              application
      assertEqual
        "startup access contradiction"
        ( Left
            ( HeraldStartupInvariant
                (StartupProcess process)
                ApplicationSessionAccessInvariant
            )
        )
        ( validateHeraldState
            (replaceStartupApplicationState corruptedApplication afterOpen)
        )
    _ -> assertFailure "fixture did not retain exactly one session"

caseSessionSetInvariant :: Assertion
caseSessionSetInvariant = do
  let (afterOpen, _) = openedSession (candidateApplicationLane 51) 51
      application = startupApplicationState afterOpen
      sessionWitness = onlySessionWitness afterOpen
      session = Application.applicationSessionWitnessId sessionWitness
      process = Application.applicationSessionWitnessProcess sessionWitness
      otherProcesses =
        filter
          (/= process)
          (fmap witnessedProcessEpoch (Application.applicationProcessWitnesses application))
  case otherProcesses of
    otherProcess : _ ->
      assertEqual
        "session/process/attachment relation"
        ( Left
            ( HeraldStartupInvariant
                (StartupProcess otherProcess)
                ApplicationSessionSetInvariant
            )
        )
        ( validateHeraldState
            ( replaceStartupApplicationState
                ( Application.replaceApplicationSessionProcessForInvariantTest
                    session
                    otherProcess
                    application
                )
                afterOpen
            )
        )
    [] -> assertFailure "fixture needs another resident process"

caseSessionIdentityInvariant :: Assertion
caseSessionIdentityInvariant = do
  let (afterOpen, _) = openedSession (candidateApplicationLane 52) 52
      application = startupApplicationState afterOpen
      sessionWitness = onlySessionWitness afterOpen
      session = Application.applicationSessionWitnessId sessionWitness
      process = Application.applicationSessionWitnessProcess sessionWitness
      (_, _, foreignBinding) =
        SessionInternal.initialSessionAllocation
          (heraldMemberEpoch fixtureRemoteMember)
          99
      corrupted =
        Application.replaceApplicationSessionBindingForInvariantTest
          session
          foreignBinding
          application
  assertEqual
    "binding names a different session"
    ( Left
        ( HeraldStartupInvariant
            (StartupProcess process)
            ApplicationSessionIdentityInvariant
        )
    )
    (validateHeraldState (replaceStartupApplicationState corrupted afterOpen))

caseSessionAllocationInvariant :: Assertion
caseSessionAllocationInvariant = do
  let (afterOpen, _) = openedSession (candidateApplicationLane 53) 53
      application = startupApplicationState afterOpen
      corrupted =
        Application.replaceApplicationNextSessionOrdinalForInvariantTest
          1
          application
  assertEqual
    "next session ordinal must dominate every allocation"
    ( Left
        ( HeraldStartupInvariant
            StartupStatic
            ApplicationSessionAllocationInvariant
        )
    )
    (validateHeraldState (replaceStartupApplicationState corrupted afterOpen))

caseComposedLocalizationInvariant :: Assertion
caseComposedLocalizationInvariant = do
  let state = initializedAt 10 fixtureLocalBootstrapIds
      application = startupApplicationState state
      processWitnesses = Application.applicationProcessWitnesses application
  case processWitnesses of
    firstWitness : secondWitness : _ -> do
      let process = witnessedProcessEpoch firstWitness
          otherProcess = witnessedProcessEpoch secondWitness
          globalIdentity =
            globalUniqueIdFromGlobalObjectId
              (globalObjectIdFromProcessEpochId otherProcess)
          internalValue = Domain.globalUniqueIdValue globalIdentity
      prepared <-
        either
          (assertFailure . ("localization preparation: " <>) . show)
          pure
          ( Application.prepareOrdinaryApplicationValueLocalization
              (Application.processRoleView [process, otherProcess])
              process
              internalValue
              application
          )
      let (successorApplication, _) =
            Application.commitOrdinaryApplicationValueLocalization prepared
          successor =
            replaceStartupApplicationState successorApplication state
      assertEqual
        "additional coherent aliases remain a valid whole Herald"
        (Right ())
        (validateHeraldState successor)
    _ -> assertFailure "fixture needs two resident process identities"

begunDrain :: (HeraldState, EffectBatch)
begunDrain =
  stepped
    "begin drain"
    (shutdownInput 11 7)
    (emptyInitializedAt 10)

finishedDrain :: (HeraldState, EffectBatch) -> (HeraldState, EffectBatch)
finishedDrain (draining, _) =
  stepped
    "finish drain"
    ( heraldInput
        (monotonicInstant 13)
        ( RuntimeObserved
            (DrainBarrierObserved (AdministrationInternal.drainId 1))
        )
    )
    draining

shutdownInput :: Word64 -> Word64 -> HeraldInput
shutdownInput observed correlation =
  heraldInput
    (monotonicInstant observed)
    ( AdministrationInput
        ( OrderlyHeraldShutdown
            (checkedInitialAdministrationBinding fixtureCheckedGenesis)
            (adminCorrelationId correlation)
        )
    )

assertOnlyTimeAdvanced :: HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvanced = assertOnlyTimeAdvancedTo 12

assertOnlyTimeAdvancedTo :: Word64 -> HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvancedTo observed predecessor successor = do
  assertEqual "new observation retained" (monotonicInstant observed) (startupLastObservedTime successor)
  assertBool
    "semantic product and phase unchanged"
    ( replaceStartupLastObservedTime
        (startupLastObservedTime predecessor)
        successor
        == predecessor
    )

openedSession ::
  CandidateApplicationLane ->
  Word64 ->
  (HeraldState, EffectBatch)
openedSession candidate nonce =
  stepped
    "open session"
    ( heraldInput
        (monotonicInstant 11)
        ( ApplicationSessionInput
            ( OpenApplicationSession
                candidate
                firstLocalAttachment
                (SessionInternal.clientNonce nonce)
            )
        )
    )
    (initializedAt 10 fixtureLocalBootstrapIds)

firstLocalAttachment :: SessionInternal.ApplicationAttachment
firstLocalAttachment =
  checkedMaybe
    "first selected attachment"
    ( primordialApplicationAttachment
        fixtureCheckedGenesis
        (checkedInitial fixtureLocalBootstrapIds)
        firstLocalBootstrapId
    )

acceptedDisposition ::
  String ->
  CandidateApplicationLane ->
  EffectBatch ->
  IO ApplicationSessionAcceptance
acceptedDisposition context candidate effects =
  case effectBatchMembers effects of
    [SetApplicationConnectionDisposition actual acceptance]
      | actual == candidate -> pure acceptance
    actual ->
      assertFailure
        (context <> ": unexpected effects " <> show actual)

acceptedDispositionValue ::
  String ->
  CandidateApplicationLane ->
  EffectBatch ->
  ApplicationSessionAcceptance
acceptedDispositionValue context candidate effects =
  case effectBatchMembers effects of
    [SetApplicationConnectionDisposition actual acceptance]
      | actual == candidate -> acceptance
    actual -> error (context <> ": unexpected effects " <> show actual)

openedIdentity ::
  ApplicationSessionAcceptance ->
  (ApplicationSessionId, ApplicationResumeToken)
openedIdentity acceptance = case sessionAcceptanceReply acceptance of
  SessionOpened session token _ -> (session, token)
  SessionResumed _ _ -> error "expected opened session acceptance"

onlySessionBinding :: HeraldState -> ApplicationSessionBinding
onlySessionBinding state = case sessions of
  [session] -> Application.applicationSessionWitnessBinding session
  _ -> error "expected exactly one application session"
  where
    sessions =
      Application.applicationWitnessSessions
        (Application.applicationStateWitness (startupApplicationState state))

onlySessionDeliveryLive :: HeraldState -> Bool
onlySessionDeliveryLive state = case sessions of
  [session] -> Application.applicationSessionWitnessDeliveryLive session
  _ -> error "expected exactly one application session"
  where
    sessions =
      Application.applicationWitnessSessions
        (Application.applicationStateWitness (startupApplicationState state))

onlySessionWitness :: HeraldState -> Application.ApplicationSessionWitness
onlySessionWitness state = case sessions of
  [session] -> session
  _ -> error "expected exactly one application session"
  where
    sessions =
      Application.applicationWitnessSessions
        (Application.applicationStateWitness (startupApplicationState state))

assertTransitionFault ::
  HeraldTransitionInvariantViolation ->
  Either HeraldInvariantFault (HeraldState, EffectBatch) ->
  Assertion
assertTransitionFault expected result = case result of
  Left (HeraldTransitionInvariant actual) -> assertEqual "transition fault" expected actual
  Left other -> assertFailure ("unexpected fault: " <> show other)
  Right _ -> assertFailure "transition unexpectedly returned a successor"

emptyInitializedAt :: Word64 -> HeraldState
emptyInitializedAt observed = initializedAt observed []

initializedAt :: Word64 -> [BootstrapManifestId] -> HeraldState
initializedAt observed selected =
  fst
    ( checkedEither
        "initial Herald"
        ( initialHerald
            (monotonicInstant observed)
            fixtureCheckedGenesis
            (checkedInitial selected)
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

checkedInitial :: [BootstrapManifestId] -> CheckedInitialBootstraps
checkedInitial selected =
  checkedEither
    "checked bootstraps"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest selected)
    )

checkedStep7Topology ::
  CheckedHeraldGenesis ->
  Either String CheckedInitialBootstraps
checkedStep7Topology genesis =
  checkedResult
    "checked Step-7 topology"
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest [firstLocalBootstrapId, fixtureRemoteBootstrapId])
        ( InitialTopologyManifest
            [ InitialTopologyProcessEdge
                firstLocalBootstrapId
                fixtureRemoteBootstrapId
                SortDefinitionRole,
              InitialTopologySystemViewEdge
                firstLocalBootstrapId
                (heraldMemberEpoch fixtureRemoteMember)
                SortDefinitionRole
            ]
        )
    )

initializedWithTopology ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  Either String HeraldState
initializedWithTopology genesis bootstraps = do
  (state, effects) <-
    checkedResult
      "initial Herald with Step-7 topology"
      (initialHerald (monotonicInstant 0) genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  requireResult "initial Herald must emit one Oracle watch and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace effects)
  Right state

stepResult ::
  String ->
  HeraldInput ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
stepResult context input state = checkedResult context (verifiedStepHerald input state)

-- | Production transitions rely on their owner-local preparation checks. This
-- test-only boundary composes them with the exhaustive whole-state verifier so
-- every successful transition exercised here still proves the closed invariant.
verifiedStepHerald ::
  HeraldInput ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
verifiedStepHerald input predecessor = do
  result@(successor, _) <- stepHerald input predecessor
  validateHeraldState successor
  Right result

checkedResult ::
  (Show problem) =>
  String ->
  Either problem value ->
  Either String value
checkedResult context = either (Left . ((context <> ": ") <>) . show) Right

requireResult :: String -> Bool -> Either String ()
requireResult _ True = Right ()
requireResult problem False = Left problem

renderingOmits :: [String] -> String -> Bool
renderingOmits privateNames rendered =
  all (not . (`isInfixOf` rendered)) privateNames

exactlyOneResult :: String -> [value] -> Either String value
exactlyOneResult _ [value] = Right value
exactlyOneResult context values =
  Left
    ( context
        <> ": expected exactly one value, observed "
        <> show (length values)
    )

candidateEffects :: EffectBatch -> [(PeerCandidate, PeerHello)]
candidateEffects effects =
  [ (candidate, hello)
  | SendPeerCandidate candidate _ _ hello <- effectBatchMembers effects
  ]

acceptedPeerBindings :: EffectBatch -> [PeerBinding]
acceptedPeerBindings effects =
  [ binding
  | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects
  ]

placementControls :: EffectBatch -> [PeerControl]
placementControls effects =
  [ control
  | SendPeerControl _ control@PeerPlacementUpdate {} <- effectBatchMembers effects
  ]

drainDeclaredDescriptor :: ApplicationSortDescriptor
drainDeclaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections =
        [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

stepped ::
  String ->
  HeraldInput ->
  HeraldState ->
  (HeraldState, EffectBatch)
stepped context input state = checkedEither context (verifiedStepHerald input state)

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error (context <> ": missing")) id

firstLocalBootstrapId :: BootstrapManifestId
firstLocalBootstrapId = case fixtureLocalBootstrapIds of
  first : _ -> first
  [] -> error "fixture has no local bootstrap"

checkedEither :: (Show problem) => String -> Either problem value -> value
checkedEither context = either (error . ((context <> ": ") <>) . show) id
