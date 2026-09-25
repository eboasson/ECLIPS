{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module EnvironmentSuccessorProperties
  ( tests,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import Control.Monad (foldM, forM_, when)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access (EnvironmentAccess, environmentAccessEdges, environmentAccessHub, environmentAccessPredefined)
import Eclips.Application.Types.Identity (privateObjectUniqueId)
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewEnvironmentApplication),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (EnvironmentStabilizationPending),
    RegularCallResult (NewEnvironmentCompleted),
  )
import Eclips.Domain.Environment (EnvironmentRootClaimView (..), environmentRootClaimView, environmentRootSlotClaim)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    HeraldEpoch,
    ProcessEpochId,
    TopologyCutId,
    controlIndex,
    deltaIdFromGlobalObjectId,
    globalUniqueIdFromGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    nablaIdFromGlobalObjectId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Domain.Publication (checkedPublicationId, checkedPublicationSort)
import Eclips.Domain.Route
  ( destinationHerald,
    routeDestinations,
  )
import Eclips.Domain.Sort.Profile (PredefinedSortRole (EdgeRole, NeutralVertexRole, SortDefinitionRole), profileSortFor)
import Eclips.Domain.Startup (primordialReplicaPublication)
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.Structural
  ( structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence (structuralOccurrenceCause)
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Request.Internal
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Completed, OperationAccepted),
    requestId,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect
      ( RunOracleClientAction,
        SendApplicationReply,
        SetPeerCandidateDisposition
      ),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    DeploymentManifest (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAnnounceId,
    topologyCutEstablishedAck,
  )
import Eclips.Herald.IdGenerator qualified as IdGenerator
import Eclips.Herald.Initialization
  ( HeraldState,
    initialHerald,
    primordialApplicationAttachment,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest, GetApplicationRequestResult),
    HeraldInputBody (OracleInput, PeerInput),
    PeerControl (PeerTopologyCutEstablished),
    PeerIngress (PeerControlReceived, PeerHelloReceived),
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction
      ( BindOracleConnection,
        ConnectAndHelloOracle,
        WatchOracle
      ),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    OracleConnectAttempt,
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerLogicalItem)
import Eclips.Herald.PeerPublication
  ( structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
  )
import Eclips.Herald.PeerStream (mkStreamDirection)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( replaceStartupApplicationState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    startupApplicationState,
    startupControlledState,
    startupPeerStreamState,
    startupPublicationState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Startup.State qualified as Startup
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Time
  ( monotonicInstant,
    monotonicInstantWord64,
  )
import Eclips.Herald.Timer
  ( TimerOutcome (TimerFired),
    timerSpecAbsoluteDeadline,
  )
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.NewEnvironment qualified as NewEnvironment
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15RetirementClosure qualified as RetirementClosure
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Herald.UseCase.StructuralSettlement qualified as StructuralSettlement
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( OracleEnvelope,
    endProcessEpochCommand,
    oracleEnvelope,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkOracleGenesis,
    checkedOraclePredefinedDescriptors,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
    raftVoterBindingNode,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State (OracleState)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedOracleGenesis,
    fixtureDeploymentManifest,
    fixtureGeneratorSeedH1,
    fixtureGeneratorSeedH2,
    fixtureGeneratorSeedH3,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureMembers,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
  )
import Step15RetirementClosureProperties (establishEmptyStructuralBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "private environment successor membership"
    [ testCase
        "public newenv settles to a typed completion and retained result"
        casePublicEnvironmentCompletion,
      testCase
        "post-retirement acceptance captures the current H1/H2/H3 successor"
        caseCurrentSuccessorCapture,
      testCase
        "H1 predecessor acceptance settles through the direct successor without H4"
        casePredecessorAcceptanceSettlesAfterH4,
      testCase
        "completed recovery expiry detaches only the lost session while a sibling remains"
        caseCompletedRecoveryExpiry,
      testCase
        "forced End settles the published endpoint prefix without constructing wiring"
        caseProcessEndAfterPartialEnvironmentProgress,
      testCase
        "forced End settles already-positioned wiring without restoring access"
        caseProcessEndAfterWiringPositioned,
      testCase
        "the complete four-Herald private schedule is deterministic"
        caseDeterministicMultiHeraldSchedule,
      testCase
        "the selected acceptance/application/delivery/final settlement ledger is 2 + R + E with zero retry work"
        caseKernelWorkFormula
    ]

casePublicEnvironmentCompletion :: Assertion
casePublicEnvironmentCompletion = do
  fixture <- fourHeraldFixture
  let opened = fixture.opened
      request = environmentRequest
      call =
        CallApplicationRequest
          opened.binding
          opened.session
          request
          NewEnvironmentApplication
  (accepted, acceptanceEffects) <-
    checkedIO
      "accept public multi-Herald newenv"
      (ApplicationCall.applyApplicationRequest call opened.state)
  acceptanceReply <-
    soleReplyFor "public newenv acceptance" opened.binding acceptanceEffects
  case acceptanceReply of
    RetainedRequestReply _ (OperationAccepted retained EnvironmentStabilizationPending) ->
      assertEqual "public pending reply request" request retained
    reply -> assertFailure ("unexpected public newenv acceptance reply: " <> show reply)
  manifest <- case pendingManifests accepted of
    [retained] -> pure retained
    retained -> assertFailure ("public newenv retained " <> show (length retained) <> " manifests")
  assertEnvironmentStamped
    "public environment"
    manifest
    (Set.fromList [fixture.h2, fixture.h3, fixture.h4])
    accepted

  (settled, _, settlementEffects) <-
    establishCompleteEnvironmentWithEffects [fixture.h2, fixture.h3, fixture.h4] accepted
  completionReply <-
    soleReplyFor "public newenv settlement" opened.binding settlementEffects
  access <- case completionReply of
    RetainedRequestReply _ (Completed retained (NewEnvironmentCompleted completedAccess)) -> do
      assertEqual "public completion reply request" request retained
      pure completedAccess
    reply -> assertFailure ("unexpected public newenv completion reply: " <> show reply)
  assertEqual "public completion has the canonical access bundle" 6 (length (environmentAccessPredefined access))
  assertEqual "public completion consumes pending environment work" [] (pendingManifests settled)
  completed <- soleCompletedEnvironment settled
  assertOwnDescriptions (Environment.completedEnvironmentManifest completed) settled
  assertEnvironmentWiringAccess opened.process access (Environment.completedEnvironmentManifest completed) settled
  assertValid "public environment settlement" settled

  (lookedUp, lookupEffects) <-
    checkedIO
      "look up retained public newenv completion"
      ( ApplicationCall.applyApplicationRequest
          (GetApplicationRequestResult opened.binding opened.session request)
          settled
      )
  lookupReply <- soleReplyFor "public newenv result lookup" opened.binding lookupEffects
  assertEqual "result lookup reoffers the exact terminal reply" completionReply lookupReply
  assertBool "terminal result lookup is state-identical" (lookedUp == settled)

soleReplyFor :: String -> ApplicationSessionBinding -> EffectBatch -> IO ApplicationRequestReply
soleReplyFor context expectedBinding effects =
  case [ reply
       | SendApplicationReply binding reply <- effectBatchMembers effects,
         binding == expectedBinding
       ] of
    [reply] -> pure reply
    replies ->
      assertFailure
        (context <> " emitted " <> show (length replies) <> " matching replies")
        >> fail "missing sole application reply"

assertOwnDescriptions :: Environment.EnvironmentManifest -> HeraldState -> Assertion
assertOwnDescriptions manifest state = mapM_ checkReader readers
  where
    roots = manifestRoots manifest
    readers =
      [ (Environment.environmentRootPlanTargetObject plan, role)
      | root <- roots,
        let plan = Environment.positionedEnvironmentRootPlan root,
        EnvironmentReaderRootClaimView role <- [environmentRootClaimView (environmentRootSlotClaim (Environment.environmentRootPlanSlot plan))]
      ]
    checkReader (object, role) = do
      slot <-
        maybe
          (assertFailure "completed environment has no local reader store")
          pure
          (Store.lookupStoreSlot (deltaIdFromGlobalObjectId object) (Startup.startupStoreState state))
      let expected =
            Set.fromList
              [ checkedPublicationId publication
              | root <- roots,
                let publication = Environment.positionedEnvironmentRootChecked root,
                checkedPublicationSort publication == profileSortFor role
              ]
          visible =
            Set.fromList
              (checkedPublicationId . DomainStore.storedPublication . snd <$> DomainStore.visibleInstances (Store.storeSlotContents slot))
      assertBool
        ("the environment's own descriptions are visible through " <> show role)
        (expected `Set.isSubsetOf` visible)
      when (role == SortDefinitionRole) $ do
        let catalogue = fmap primordialReplicaPublication (checkedPrimordialReplicas (Startup.startupGenesis state))
            actual = DomainStore.storedPublication . snd <$> DomainStore.visibleInstances (Store.storeSlotContents slot)
            byIdentity publications = Map.fromList [(checkedPublicationId publication, publication) | publication <- publications]
        assertEqual "the shared bootstrap catalogue contains all six predefined descriptions" 6 (length catalogue)
        assertEqual
          "newenv's SortDefinition reader contains exactly the shared bootstrap catalogue descriptions"
          (byIdentity catalogue)
          (byIdentity actual)

assertEnvironmentWiringAccess :: ProcessEpochId -> EnvironmentAccess -> Environment.EnvironmentManifest -> HeraldState -> Assertion
assertEnvironmentWiringAccess process access manifest state = do
  let privateIds =
        privateObjectUniqueId (environmentAccessHub access)
          : fmap privateObjectUniqueId (environmentAccessEdges access)
      expected =
        [ globalUniqueIdFromGlobalObjectId (Environment.environmentRootPlanTargetObject (Environment.positionedEnvironmentRootPlan root))
        | root <- drop 12 (manifestRoots manifest)
        ]
  assertEqual "ordinary hub and edge handles cover all nineteen wiring objects" 19 (length privateIds)
  assertEqual
    "hub and edge handles resolve through the caller's ordinary private identity map"
    (Right expected)
    (traverse (\identifier -> Application.resolveApplicationPrivateUniqueId process identifier (startupApplicationState state)) privateIds)

-- The membership tests deliberately use the complete Step-15 retirement
-- closure.  H4 is an application-free fourth member; the unchanged three-node
-- Oracle contact set models its non-voting status at this Herald boundary.
caseCurrentSuccessorCapture :: Assertion
caseCurrentSuccessorCapture = do
  fixture <- fourHeraldFixture
  (retired, successorMembership) <- retireH4 fixture fixture.opened.state
  (accepted, manifest) <- acceptEnvironment fixture.opened retired
  let successorMembers =
        Set.fromList
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successorMembership))
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
  assertEqual
    "post-retirement manifest captures the current successor generation"
    (heraldMembershipGenerationId successorMembership)
    (Environment.environmentManifestMembershipGenerationId manifest)
  assertEqual
    "post-retirement manifest captures the current successor member digest"
    (heraldMembershipGenerationActiveMemberSetDigest successorMembership)
    (Environment.environmentManifestActiveMemberSetDigest manifest)
  assertEqual
    "post-retirement fixture retains exactly H1/H2/H3"
    (Set.fromList [fixture.h1, fixture.h2, fixture.h3])
    successorMembers
  assertBool
    "post-retirement routes contain every current member and no H4 destination"
    ( all
        (\root -> routeMembers root == successorMembers && Set.notMember fixture.h4 (routeMembers root))
        roots
    )
  (progressed, _) <-
    checkedIO
      "advance post-retirement environment roots"
      (StructuralCoordinator.advanceLocalStructuralWork accepted)
  assertEnvironmentStamped
    "post-retirement environment"
    manifest
    (Set.fromList [fixture.h2, fixture.h3])
    progressed
  assertValid "post-retirement environment acceptance" progressed

casePredecessorAcceptanceSettlesAfterH4 :: Assertion
casePredecessorAcceptanceSettlesAfterH4 = do
  fixture <- fourHeraldFixture
  let prefixed = fixtureHeraldAfterProbePrefix fixture.opened.state
  (accepted, manifest) <- acceptEnvironment fixture.opened prefixed
  let capturedMembership = currentMembership accepted
      capturedMembers =
        Set.fromList
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs capturedMembership))
  assertEqual
    "pre-retirement acceptance captures H1/H2/H3/H4"
    (Set.fromList [fixture.h1, fixture.h2, fixture.h3, fixture.h4])
    capturedMembers
  assertBool
    "every frozen pre-retirement root route contains H4"
    (all (Set.member fixture.h4 . routeMembers) (manifestRoots manifest))

  firstStep <-
    checkedIO
      "stamp one predecessor-generation environment root"
      (StructuralProgress.advanceNextReadyApplicationStructuralStage accepted)
  (beforeRetirement, predecessorStage) <- case firstStep of
    Just (successor, _) -> case environmentStampedStages successor of
      [stage] -> pure (successor, stage)
      observed ->
        assertFailure
          ("single predecessor step stamped " <> show (length observed) <> " environment roots")
    Nothing -> assertFailure "first predecessor-generation environment root was held"
  let predecessorOccurrence =
        structuralOccurrenceStampOccurrence
          (Publication.stampedStructuralStamp predecessorStage)
      predecessorPeers = Publication.stampedStructuralPeerPublications predecessorStage
  assertEqual
    "the predecessor stamp retains live H2/H3/H4 receiver work"
    (Set.fromList [fixture.h2, fixture.h3, fixture.h4])
    (Map.keysSet predecessorPeers)

  (afterRetirement, successorMembership) <- retireH4 fixture beforeRetirement
  assertEqual
    "retirement does not rewrite the Application manifest"
    [manifest]
    (pendingManifests afterRetirement)
  assertEqual
    "retirement does not rewrite the Publication manifest"
    (Just manifest)
    ( Publication.lookupEnvironmentManifest
        (Environment.environmentManifestKeyOf manifest)
        (startupPublicationState afterRetirement)
    )
  assertEqual
    "accepted provenance remains predecessor-generation evidence"
    (heraldMembershipGenerationId capturedMembership)
    (Environment.environmentManifestMembershipGenerationId manifest)
  let stampedAfterRetirement = environmentStampedStages afterRetirement
      capturedGeneration = heraldMembershipGenerationId capturedMembership
      capturedDigest = heraldMembershipGenerationActiveMemberSetDigest capturedMembership
      successorGeneration = heraldMembershipGenerationId successorMembership
      successorDigest = heraldMembershipGenerationActiveMemberSetDigest successorMembership
      unrelatedMembership =
        checkedValue
          "environment unrelated membership"
          ( genesisHeraldMembershipGeneration
              (checkedSystemId fixture.homeGenesis)
              (NonEmpty.fromList [fixture.h1, fixture.h2, fixture.h3])
          )
      unrelatedGeneration = heraldMembershipGenerationId unrelatedMembership
      unrelatedDigest = heraldMembershipGenerationActiveMemberSetDigest unrelatedMembership
      coordinateIsAdmitted generation digest =
        StructuralSettlement.environmentStructuralCoordinateLineageEvidence
          manifest
          generation
          digest
          afterRetirement
      stagesIn generation =
        filter ((== generation) . stampedStageGeneration) stampedAfterRetirement
      predecessorStages = stagesIn capturedGeneration
      successorStages = stagesIn successorGeneration
  assertBool
    "the exact admission membership coordinate is valid"
    (coordinateIsAdmitted capturedGeneration capturedDigest)
  assertBool
    "the admission generation with a different digest is invalid"
    (not (coordinateIsAdmitted capturedGeneration successorDigest))
  assertBool
    "an unrelated generation cannot borrow the admission digest"
    (not (coordinateIsAdmitted unrelatedGeneration capturedDigest))
  assertBool
    "the exact retained direct-successor coordinate is valid"
    (coordinateIsAdmitted successorGeneration successorDigest)
  assertBool
    "the direct-successor generation with a different digest is invalid"
    (not (coordinateIsAdmitted successorGeneration capturedDigest))
  assertBool
    "an unrelated historical generation is not valid for the captured manifest"
    (not (coordinateIsAdmitted unrelatedGeneration unrelatedDigest))
  assertEqual
    "retirement preserves exactly the already-stamped predecessor root"
    [predecessorOccurrence]
    (stampedOccurrence <$> predecessorStages)
  assertEqual
    "retirement preserves the predecessor root's H4 outbox evidence unchanged"
    [predecessorPeers]
    (Publication.stampedStructuralPeerPublications <$> predecessorStages)
  assertEqual
    "the other eleven roots stamp against the exact successor"
    11
    (length successorStages)
  assertBool
    "successor-stamped roots project receiver work to H2/H3"
    ( all
        ((== Set.fromList [fixture.h2, fixture.h3]) . Map.keysSet . Publication.stampedStructuralPeerPublications)
        successorStages
    )
  assertBool
    "the immutable frozen manifest still records H4 while only successor work drops it"
    ( all (Set.member fixture.h4 . routeMembers) (manifestRoots manifest)
        && all
          (Set.notMember fixture.h4 . Map.keysSet . Publication.stampedStructuralPeerPublications)
          successorStages
    )
  assertEqual
    "the successor-base cut cannot settle a manifest whose later roots are not in that cut"
    [manifest]
    (pendingManifests afterRetirement)
  let progressAfterRetirement = startupStructuralProgressState afterRetirement
      successorBaseCut = GraphProgress.structuralLastInstalledCutId progressAfterRetirement
  assertBool
    "the successor-base cut does not yet cover the complete mixed manifest"
    ( all
        ( \stage ->
            not
              ( GraphProgress.installedCutCoversCause
                  successorBaseCut
                  (structuralOccurrenceCause (stampedOccurrence stage))
                  progressAfterRetirement
              )
        )
        stampedAfterRetirement
    )

  (withWiring, cut) <-
    establishHomeCut [fixture.h2, fixture.h3] afterRetirement
  let settledProgress = startupStructuralProgressState withWiring
      exactOccurrences = Set.fromList (stampedOccurrence <$> stampedAfterRetirement)
      exactOccurrencesCoveredByCut =
        Set.filter
          (\occurrence -> GraphProgress.installedCutCoversCause cut (structuralOccurrenceCause occurrence) settledProgress)
          exactOccurrences
  assertEqual
    "the direct-successor common cut covers the twelve exact manifest occurrences"
    exactOccurrences
    exactOccurrencesCoveredByCut
  assertEqual
    "the common cut newly covers exactly the twelve manifest occurrences"
    exactOccurrences
    (Set.fromList (GraphProgress.topologyCutNewlyCoveredOccurrences cut settledProgress))
  (settled, _) <- establishHomeCut [fixture.h2, fixture.h3] withWiring
  completion <- soleCompletedEnvironment settled
  assertBool
    "successor settlement preserves every accepted identity and positioned root"
    (Environment.environmentManifestExtends (Environment.completedEnvironmentManifest completion) manifest)
  assertOwnDescriptions (Environment.completedEnvironmentManifest completion) settled
  assertBool
    "the surviving process receives one canonical six-role access bundle"
    ( maybe
        False
        ((== 6) . length . environmentAccessPredefined)
        (Environment.completedEnvironmentAccess completion)
    )
  assertEqual
    "successor settlement consumed the pending relation"
    []
    (pendingManifests settled)
  assertEqual
    "settled Herald remains on the exact direct successor"
    (heraldMembershipGenerationId successorMembership)
    (heraldMembershipGenerationId (currentMembership settled))
  assertValid "direct-successor environment settlement" settled
  where
    stampedOccurrence =
      structuralOccurrenceStampOccurrence . Publication.stampedStructuralStamp
    stampedStageGeneration =
      structuralVersionVectorMembershipGenerationId
        . structuralOccurrenceStampPredecessor
        . Publication.stampedStructuralStamp

caseProcessEndAfterPartialEnvironmentProgress :: Assertion
caseProcessEndAfterPartialEnvironmentProgress = do
  fixture <- fourHeraldFixture
  (accepted, manifest) <- acceptEnvironment fixture.opened fixture.opened.state
  firstStep <-
    checkedIO
      "stamp one environment root before process End"
      (StructuralProgress.advanceNextReadyApplicationStructuralStage accepted)
  partiallyApplied <- case firstStep of
    Just (successor, _) -> pure successor
    Nothing -> assertFailure "first pre-End environment root was held"
  assertEqual
    "the End schedule starts with exactly one stamped root"
    1
    (length (environmentStampedStages partiallyApplied))

  ended <- applyFixtureProcessEnd fixture partiallyApplied
  assertBool
    "the real End transition retires the source in Controlled"
    (Controlled.controlledProcessEnded fixture.opened.process (startupControlledState ended))
  assertEqual
    "the pre-End structural fence drains all twelve accepted roots"
    12
    (length (environmentStampedStages ended))
  assertEqual
    "process End preserves the globally pending immutable manifest"
    [manifest]
    (pendingManifests ended)
  assertEqual
    "process End removes every session-owned environment reply"
    []
    ( Application.applicationEnvironmentRequestEntries
        fixture.opened.session
        (startupApplicationState ended)
    )

  (settled, _) <-
    establishHomeCut [fixture.h2, fixture.h3, fixture.h4] ended
  completion <- soleCompletedEnvironment settled
  assertEqual
    "settlement after End preserves the exact global manifest"
    manifest
    (Environment.completedEnvironmentManifest completion)
  assertEqual
    "settlement after End cannot recreate process-local aliases"
    Nothing
    (Environment.completedEnvironmentAccess completion)
  assertEqual
    "settlement after End cannot recreate a session reply"
    Nothing
    (Environment.completedEnvironmentReplyKey completion)
  assertEqual
    "settlement consumes the globally pending relation"
    []
    (pendingManifests settled)
  assertBool
    "the terminal prefix has no wiring suffix"
    (not (Environment.environmentManifestComplete (Environment.completedEnvironmentManifest completion)))
  assertEqual
    "settlement after forced End does not publish the nineteen wiring objects"
    12
    (length (environmentStampedStages settled))
  (retried, _) <-
    checkedIO
      "redrive terminal forced-End construction"
      (StructuralCoordinator.advanceLocalStructuralWork settled)
  assertBool "redriving terminal construction leaves the frozen prefix unchanged" (retried == settled)
  assertValid "environment settlement after real process End" settled

caseProcessEndAfterWiringPositioned :: Assertion
caseProcessEndAfterWiringPositioned = do
  fixture <- fourHeraldFixture
  (accepted, prefix) <- acceptEnvironment fixture.opened fixture.opened.state
  (sourceApplied, _) <-
    checkedIO
      "advance endpoint prefix before wiring End"
      (StructuralCoordinator.advanceLocalStructuralWork accepted)
  (withWiring, _) <- establishHomeCut [fixture.h2, fixture.h3, fixture.h4] sourceApplied
  manifest <- case pendingManifests withWiring of
    [retained] -> pure retained
    _ -> assertFailure "expected one pending complete environment" >> fail "missing environment"
  assertBool
    "the first cut preserves the accepted prefix and positions all wiring"
    (Environment.environmentManifestExtends manifest prefix && Environment.environmentManifestComplete manifest)
  assertEqual "the wiring is stamped before forced End" 31 (length (environmentStampedStages withWiring))
  let endpointRoots = NonEmpty.toList (Environment.environmentManifestRoots prefix)
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
      wiringRoots = drop (length endpointRoots) roots
      process = Environment.environmentManifestProcess manifest
      groups = Publication.publicationGroups (startupPublicationState withWiring)
      expectedWiringSources =
        Set.fromList
          [ nablaIdFromGlobalObjectId (Environment.environmentRootPlanTargetObject plan)
          | root <- endpointRoots,
            let plan = Environment.positionedEnvironmentRootPlan root,
            EnvironmentWriterRootClaimView role _ <- [environmentRootClaimView (environmentRootSlotClaim (Environment.environmentRootPlanSlot plan))],
            role `elem` [NeutralVertexRole, EdgeRole]
          ]
  assertEqual "the second phase tracks nineteen distinct wiring publications" 19 (length wiringRoots)
  assertEqual
    "wiring uses the two writers installed by the endpoint prefix"
    expectedWiringSources
    (Set.fromList (Environment.environmentRootPlanSourceNabla . Environment.positionedEnvironmentRootPlan <$> wiringRoots))
  assertEqual
    "stamping the suffix leaves no duplicate endpoint or wiring token"
    0
    (PublicationGroups.retainedPublicationCount groups)
  forM_ roots $ \root -> do
    let plan = Environment.positionedEnvironmentRootPlan root
        key = PublicationGroups.groupKey process (Environment.environmentRootPlanTargetObject plan)
    assertEqual "every positioned root has finished local publication processing" 0 (PublicationGroups.groupLocalPendingCount key groups)
    assertEqual "no already stamped endpoint or wiring root remains unstamped" 0 (PublicationGroups.groupUnstampedCount key groups)
  forM_ wiringRoots $ \root -> do
    let plan = Environment.positionedEnvironmentRootPlan root
        key = PublicationGroups.groupKey process (Environment.environmentRootPlanTargetObject plan)
    assertEqual "dynamic wiring sources explicitly capture their unsequenced definition" Nothing (Environment.environmentRootPlanSourceSequencingObject plan)
    assertEqual "every wiring self-group retains its three real remote obligations" 3 (PublicationGroups.groupPendingPeerCount key groups)
  ended <- applyFixtureProcessEnd fixture withWiring
  assertEqual
    "forced End preserves all already-positioned publications"
    [manifest]
    (pendingManifests ended)
  (settled, _) <- establishHomeCut [fixture.h2, fixture.h3, fixture.h4] ended
  completion <- soleCompletedEnvironment settled
  assertEqual "the full published manifest settles unchanged" manifest (Environment.completedEnvironmentManifest completion)
  assertEqual "ended construction returns no aliases" Nothing (Environment.completedEnvironmentAccess completion)
  assertEqual "ended construction returns no reply" Nothing (Environment.completedEnvironmentReplyKey completion)
  assertEqual "ended construction consumes pending work" [] (pendingManifests settled)
  assertValid "full environment settlement after forced End" settled

caseCompletedRecoveryExpiry :: Assertion
caseCompletedRecoveryExpiry = do
  fixture <- fourHeraldFixture
  (accepted, _) <- acceptEnvironment fixture.opened fixture.opened.state
  (sourceApplied, _) <-
    checkedIO
      "advance recovery-expiry environment roots"
      (StructuralCoordinator.advanceLocalStructuralWork accepted)
  (settled, _, _) <-
    establishCompleteEnvironmentWithEffects [fixture.h2, fixture.h3, fixture.h4] sourceApplied
  initialCompletion <- soleCompletedEnvironment settled
  initialAccess <-
    maybe
      (assertFailure "settled recovery fixture has no localized access")
      pure
      (Environment.completedEnvironmentAccess initialCompletion)
  let settledApplication = startupApplicationState settled
  siblingPreparation <-
    checkedIO
      "open sibling session for completed environment"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch fixture.homeGenesis)
          fixture.opened.attachment
          (clientNonce 2)
          settledApplication
      )
  let (withSiblingApplication, siblingAcceptance) =
        Application.commitApplicationSessionAcceptance siblingPreparation
      siblingSession = case sessionAcceptanceReply siblingAcceptance of
        SessionOpened session _ _ -> session
        reply -> error ("expected completed-environment sibling session, got " <> show reply)
      withSibling = replaceStartupApplicationState withSiblingApplication settled
      lossPreparation =
        Application.prepareApplicationBindingLoss
          fixtureApplicationRecoveryConfiguration
          (monotonicInstant 10)
          fixture.opened.binding
          withSiblingApplication
      (recoveryAttempt, recoveryDeadline) =
        case Application.preparedApplicationBindingLossDisposition lossPreparation of
          Application.ApplicationRecoveryStarted attempt spec ->
            (attempt, timerSpecAbsoluteDeadline spec)
          disposition ->
            error
              ( "expected completed-environment session recovery, got "
                  <> show disposition
              )
      afterLossApplication =
        Application.commitApplicationBindingLoss lossPreparation
      afterLoss = replaceStartupApplicationState afterLossApplication withSibling
      firedAt = monotonicInstant (monotonicInstantWord64 recoveryDeadline + 1)
  assertValid "completed environment with a live sibling" withSibling
  assertValid "completed environment after binding loss" afterLoss
  afterLossCompletion <- soleCompletedEnvironment afterLoss
  assertEqual
    "binding loss preserves completed access"
    (Just initialAccess)
    (Environment.completedEnvironmentAccess afterLossCompletion)
  assertEqual
    "binding loss preserves the completed reply key during recovery"
    (Environment.completedEnvironmentReplyKey initialCompletion)
    (Environment.completedEnvironmentReplyKey afterLossCompletion)
  timerPreparation <-
    checkedIO
      "expire completed-environment session recovery"
      ( Application.prepareApplicationRecoveryTimerObservation
          recoveryAttempt
          (TimerFired firedAt)
          afterLossApplication
      )
  case Application.preparedApplicationRecoveryTimerDisposition timerPreparation of
    Application.ApplicationRecoveryTimerExpired expiration -> do
      assertEqual
        "only the lost completed-environment session expires"
        [fixture.opened.session]
        (Application.applicationRecoveryExpiredSessionIds expiration)
      assertEqual
        "the live sibling prevents process sealing"
        Nothing
        (Application.applicationRecoverySealedProcess expiration)
    disposition ->
      assertFailure
        ("completed-environment recovery did not expire: " <> show disposition)
  let expiredApplication =
        Application.commitApplicationRecoveryTimerObservation timerPreparation
      expired = replaceStartupApplicationState expiredApplication afterLoss
  completion <- soleCompletedEnvironment expired
  assertEqual
    "recovery expiry preserves the exact global completion"
    (Environment.completedEnvironmentManifest initialCompletion)
    (Environment.completedEnvironmentManifest completion)
  assertEqual
    "recovery expiry preserves process-scoped aliases"
    (Just initialAccess)
    (Environment.completedEnvironmentAccess completion)
  assertEqual
    "recovery expiry detaches the lost session's reply key"
    Nothing
    (Environment.completedEnvironmentReplyKey completion)
  assertEqual
    "recovery expiry removes the lost request-stage owner"
    []
    ( Application.applicationEnvironmentRequestEntries
        fixture.opened.session
        expiredApplication
    )
  assertEqual
    "the expired session is absent"
    Nothing
    (Application.applicationSessionProcess fixture.opened.session expiredApplication)
  assertEqual
    "the sibling keeps the environment process reachable"
    (Just fixture.opened.process)
    (Application.applicationSessionProcess siblingSession expiredApplication)
  assertEqual
    "one exact completed manifest remains after recovery expiry"
    [Environment.completedEnvironmentManifest initialCompletion]
    (completedManifests expired)
  assertValid "completed environment after recovery expiry" expired

caseDeterministicMultiHeraldSchedule :: Assertion
caseDeterministicMultiHeraldSchedule = do
  first <- runMultiHeraldSchedule
  second <- runMultiHeraldSchedule
  assertBool
    "equal pure four-Herald schedules produce equal complete owner states"
    ( first.home == second.home
        && first.remotes == second.remotes
        && first.manifest == second.manifest
        && first.work == second.work
    )
  assertEqual
    "the deterministic schedule reaches one completed manifest"
    [first.manifest]
    (completedManifests first.home)
  assertValid "deterministic multi-Herald home" first.home
  mapM_ (assertValid "deterministic multi-Herald receiver") (Map.elems first.remotes)

caseKernelWorkFormula :: Assertion
caseKernelWorkFormula = do
  schedule <- runMultiHeraldSchedule
  let work = schedule.work
      roots = length (manifestRoots schedule.manifest)
      destinations =
        sum
          [ length
              [ destination
              | destination <-
                  routeDestinations
                    ( Environment.environmentRootPlanRoute
                        (Environment.positionedEnvironmentRootPlan root)
                    ),
                destinationHerald destination /= schedule.fixture.h1
              ]
          | root <- manifestRoots schedule.manifest
          ]
      receiverItems =
        sum
          ( Map.size . Publication.stampedStructuralPeerPublications
              <$> environmentStampedStages schedule.home
          )
      expected = 2 + roots + destinations
  assertEqual "R counts the twelve endpoints, one hub and eighteen edges" 31 roots
  assertEqual "E counts one frozen remote destination for every object/member pair" 93 destinations
  assertEqual
    "this fixture has one receiver-qualified peer item per frozen remote destination"
    destinations
    receiverItems
  assertEqual "the schedule records one atomic acceptance" 1 work.atomicAcceptances
  assertEqual "the schedule records one source-local application per root" roots work.rootApplications
  assertEqual "the schedule records one logical receiver delivery per frozen remote destination" destinations work.receiverDeliveries
  assertEqual "the schedule records one access-only settlement" 1 work.accessSettlements
  assertEqual
    "the selected ledger counts one acceptance, R applications, E deliveries and one final settlement"
    expected
    (newEnvironmentKernelWorkTotal work)
  (retried, retryOutcome) <-
    checkedIO
      "retry completed environment"
      ( runEnvironment
          schedule.fixture.opened
          environmentRequest
          schedule.home
      )
  case retryOutcome of
    NewEnvironment.NewEnvironmentRetried retained ->
      assertEqual "completed retry returns the original manifest" schedule.manifest retained
    other -> assertFailure ("completed retry returned " <> show other)
  assertBool "completed retry is state-identical" (retried == schedule.home)
  assertEqual
    "an exact retry adds zero logical acceptance/root/delivery/settlement work"
    zeroNewEnvironmentKernelWork
    (kernelWorkDelta schedule.home retried)

data NewEnvironmentKernelWork = NewEnvironmentKernelWork
  { atomicAcceptances :: Int,
    rootApplications :: Int,
    receiverDeliveries :: Int,
    accessSettlements :: Int
  }
  deriving stock (Eq, Show)

zeroNewEnvironmentKernelWork :: NewEnvironmentKernelWork
zeroNewEnvironmentKernelWork = NewEnvironmentKernelWork 0 0 0 0

newEnvironmentKernelWorkTotal :: NewEnvironmentKernelWork -> Int
newEnvironmentKernelWorkTotal work =
  work.atomicAcceptances
    + work.rootApplications
    + work.receiverDeliveries
    + work.accessSettlements

-- | Selected acceptance/application/delivery/final-settlement ledger. It excludes
-- phase expansion, context seeding, runtime dispatch and topology-control work.
data MultiHeraldSchedule = MultiHeraldSchedule
  { fixture :: FourHeraldFixture,
    manifest :: Environment.EnvironmentManifest,
    home :: HeraldState,
    remotes :: Map.Map HeraldEpoch HeraldState,
    work :: NewEnvironmentKernelWork
  }

runMultiHeraldSchedule :: IO MultiHeraldSchedule
runMultiHeraldSchedule = do
  fixture <- fourHeraldFixture
  (accepted, prefix) <- acceptEnvironment fixture.opened fixture.opened.state
  (sourceApplied, _) <- checkedIO "apply four-Herald endpoint prefix" (StructuralCoordinator.advanceLocalStructuralWork accepted)
  let remoteEpochs = [fixture.h2, fixture.h3, fixture.h4]
  assertEnvironmentStamped "four-Herald endpoint prefix" prefix (Set.fromList remoteEpochs) sourceApplied
  (firstReceivers, firstDeliveries) <- foldM (deliverToRemote fixture sourceApplied) (Map.empty, 0) remoteEpochs
  let reportsFor = fmap (uncurry structuralReportFor) . Map.toAscList
  (withWiring, firstCut) <- establishHomeCutWithReports (reportsFor firstReceivers) sourceApplied
  assertEqual "the endpoint cut exposes no completed environment" [] (completedManifests withWiring)
  installedReceivers <- traverse (installHomeCut fixture withWiring firstCut) firstReceivers
  (receivers, delivered) <- foldM (deliverToRemote fixture withWiring) (installedReceivers, firstDeliveries) remoteEpochs
  (settled, finalCut) <- establishHomeCutWithReports (reportsFor receivers) withWiring
  completed <- soleCompletedEnvironment settled
  let manifest = Environment.completedEnvironmentManifest completed
  finalReceivers <- traverse (installHomeCut fixture settled finalCut) receivers
  assertBool "completed construction preserves its original prefix" (Environment.environmentManifestExtends manifest prefix)
  assertEnvironmentStamped "complete four-Herald environment" manifest (Set.fromList remoteEpochs) settled
  assertOwnDescriptions manifest settled
  let totalWork = kernelWorkDelta fixture.opened.state settled
  pure
    MultiHeraldSchedule
      { fixture,
        manifest,
        home = settled,
        remotes = finalReceivers,
        work = totalWork {receiverDeliveries = delivered}
      }

installHomeCut :: FourHeraldFixture -> HeraldState -> TopologyCutId -> HeraldState -> IO HeraldState
installHomeCut fixture home cut receiver = do
  installed <-
    maybe
      (assertFailure "source has no established cut")
      pure
      (GraphProgress.lookupInstalledTopologyCut cut (startupStructuralProgressState home))
  binding <-
    maybe
      (assertFailure "receiver has no source binding")
      pure
      (DiscoveryState.currentPeerBinding fixture.h1 (Startup.startupDiscoveryState receiver))
  (successor, _) <-
    step
      (monotonicInstantWord64 (Startup.startupLastObservedTime receiver) + 1)
      (PeerInput (PeerControlReceived binding (PeerTopologyCutEstablished (GraphProgress.installedTopologyCutEstablished installed))))
      receiver
  assertBool
    "receiver installs the exact source cut before later writer use"
    (GraphProgress.lookupInstalledTopologyCut cut (startupStructuralProgressState successor) /= Nothing)
  pure successor

deliverToRemote ::
  FourHeraldFixture ->
  HeraldState ->
  (Map.Map HeraldEpoch HeraldState, Int) ->
  HeraldEpoch ->
  IO (Map.Map HeraldEpoch HeraldState, Int)
deliverToRemote fixture home (completed, count) remote = do
  initial <-
    maybe
      (assertFailure ("missing initial receiver " <> show remote))
      pure
      (case Map.lookup remote completed of Just retained -> Just retained; Nothing -> Map.lookup remote fixture.initialRemotes)
  member <-
    maybe
      (assertFailure ("missing receiver member " <> show remote))
      pure
      (findMember remote fixture.members)
  let genesis = checkedValue "receiver genesis" (checkHeraldGenesis (deploymentAt member))
  bootstraps <- bootstrapsFor genesis
  (connected, binding) <- case DiscoveryState.currentPeerBinding fixture.h1 (Startup.startupDiscoveryState initial) of
    Just retained -> pure (initial, retained)
    Nothing -> connectHomePeer fixture genesis bootstraps initial
  direction <- checkedIO "environment outgoing direction" (mkStreamDirection fixture.h1 remote)
  items <-
    checkedIO
      "environment outgoing items"
      (PeerStream.activeOutgoingItems direction (startupPeerStreamState home))
  receiver <-
    foldM
      ( \state item ->
          fst
            <$> step
              (monotonicInstantWord64 (Startup.startupLastObservedTime state) + 1)
              (PeerInput (peerPublicationReceived binding (peerLogicalItem item)))
              state
      )
      connected
      items
  assertEqual
    ("receiver applies all environment roots at " <> show remote)
    (length (environmentStampedStages home))
    (length (Publication.incomingPublicationEntries (startupPublicationState receiver)))
  pure (Map.insert remote receiver completed, count + length (Publication.incomingPublicationEntries (startupPublicationState receiver)) - length (Publication.incomingPublicationEntries (startupPublicationState initial)))
  where
    deploymentAt local =
      environmentDeployment
        { deploymentLocalHeraldId = heraldMemberId local,
          deploymentLocalHeraldEpoch = heraldMemberEpoch local
        }

connectHomePeer ::
  FourHeraldFixture ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  HeraldState ->
  IO (HeraldState, PeerBinding)
connectHomePeer fixture genesis bootstraps predecessor = do
  homeMember <-
    maybe
      (assertFailure "four-Herald fixture lost H1")
      pure
      (findMember fixture.h1 fixture.members)
  let nonce = connectionNonce 301
      candidate =
        peerCandidate
          (heraldMemberId homeMember)
          fixture.h1
          nonce
      hello =
        peerHello
          (checkedSystemId genesis)
          (heraldMemberId homeMember)
          fixture.h1
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest genesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      membership = currentMembership predecessor
  (connected, effects) <-
    step
      1
      ( PeerInput
          ( PeerHelloReceived
              candidate
              Set.empty
              hello
              (heraldMembershipGenerationId membership)
              (heraldMembershipGenerationActiveMemberSetDigest membership)
              Nothing
          )
      )
      predecessor
  case [ binding
       | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects
       ] of
    [binding] -> pure (connected, binding)
    observed ->
      assertFailure
        ("receiver admitted " <> show (length observed) <> " H1 peer bindings")

findMember :: HeraldEpoch -> [HeraldMember] -> Maybe HeraldMember
findMember wanted = go
  where
    go [] = Nothing
    go (member : remaining)
      | heraldMemberEpoch member == wanted = Just member
      | otherwise = go remaining

-- A live environment first establishes its endpoint writers, then its wiring.
establishCompleteEnvironmentWithEffects :: [HeraldEpoch] -> HeraldState -> IO (HeraldState, TopologyCutId, EffectBatch)
establishCompleteEnvironmentWithEffects reporters initial = do
  (withWiring, _, firstEffects) <- establishHomeCutWithEffects reporters initial
  assertEqual "the endpoint cut does not complete newenv" [] (completedManifests withWiring)
  (settled, cut, finalEffects) <- establishHomeCutWithEffects reporters withWiring
  pure (settled, cut, firstEffects <> finalEffects)

establishHomeCut ::
  [HeraldEpoch] ->
  HeraldState ->
  IO (HeraldState, TopologyCutId)
establishHomeCut reporters state = do
  (settled, cut, _) <- establishHomeCutWithEffects reporters state
  pure (settled, cut)

establishHomeCutWithEffects ::
  [HeraldEpoch] ->
  HeraldState ->
  IO (HeraldState, TopologyCutId, EffectBatch)
establishHomeCutWithEffects reporters state =
  establishHomeCutWithReportsAndEffects
    [ structuralReportFor reporter state
    | reporter <- reporters
    ]
    state

establishHomeCutWithReports ::
  [StructuralAppliedReport] ->
  HeraldState ->
  IO (HeraldState, TopologyCutId)
establishHomeCutWithReports reports initial = do
  (settled, cut, _) <- establishHomeCutWithReportsAndEffects reports initial
  pure (settled, cut)

establishHomeCutWithReportsAndEffects ::
  [StructuralAppliedReport] ->
  HeraldState ->
  IO (HeraldState, TopologyCutId, EffectBatch)
establishHomeCutWithReportsAndEffects reports initial = do
  reported <- foldM retainReport initial reports
  (announced, _, prematurelyInstalled) <-
    checkedIO
      "open environment topology cut"
      (PeerControl.advanceLocalTopologyCutIfReady reported)
  assertEqual
    "a multi-Herald environment cut waits for remote acceptance"
    []
    prematurelyInstalled
  announce <-
    maybe
      (assertFailure "complete environment report matrix did not open a topology cut")
      pure
      (GraphProgress.structuralOpenCut (startupStructuralProgressState announced))
  let cut = topologyCutAnnounceId announce
  acceptedProgress <-
    foldM
      (retainAcceptance cut)
      (startupStructuralProgressState announced)
      reports
  establishment <-
    checkedIO
      "establish environment topology cut"
      (GraphProgress.prepareTopologyCutEstablishment acceptedProgress)
  acknowledged <-
    foldM
      ( \progress reporter ->
          GraphProgress.commitTopologyCutEstablishedAck
            <$> checkedIO
              "acknowledge established environment cut"
              ( GraphProgress.prepareTopologyCutEstablishedAck
                  (topologyCutEstablishedAck cut reporter (heraldMembershipGenerationId (currentMembership announced)))
                  progress
              )
      )
      (GraphProgress.commitTopologyCutEstablishment establishment)
      [reporter | reporter <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (currentMembership announced)), reporter /= checkedLocalHeraldEpoch (Startup.startupGenesis announced)]
  let installed = replaceStartupStructuralProgressState acknowledged announced
  (settled, effects) <-
    checkedIO
      "settle environment topology cut"
      (StructuralCoordinator.settleInstalledStructuralCuts [cut] installed)
  pure (settled, cut, effects)
  where
    retainReport state report = do
      prepared <-
        checkedIO
          "retain environment structural report"
          ( GraphProgress.prepareStructuralReport
              report
              (startupStructuralProgressState state)
          )
      let (progress, _) = GraphProgress.commitStructuralReport prepared
      pure (replaceStartupStructuralProgressState progress state)

    retainAcceptance cut progress report = do
      prepared <-
        checkedIO
          "retain environment topology acceptance"
          ( GraphProgress.prepareTopologyCutAcceptance
              (topologyCutAcceptance cut report)
              progress
          )
      pure (GraphProgress.commitTopologyCutAcceptance prepared)

structuralReportFor :: HeraldEpoch -> HeraldState -> StructuralAppliedReport
structuralReportFor reporter state =
  structuralAppliedReport
    reporter
    (GraphProgress.structuralAppliedVector progress)
    (GraphProgress.structuralAppliedControlPrefix progress)
  where
    progress = startupStructuralProgressState state

manifestRoots :: Environment.EnvironmentManifest -> [Environment.PositionedEnvironmentRoot]
manifestRoots = NonEmpty.toList . Environment.environmentManifestRoots

routeMembers :: Environment.PositionedEnvironmentRoot -> Set.Set HeraldEpoch
routeMembers =
  Set.fromList
    . fmap destinationHerald
    . routeDestinations
    . Environment.environmentRootPlanRoute
    . Environment.positionedEnvironmentRootPlan

environmentStampedStages :: HeraldState -> [Publication.StampedStructuralStage]
environmentStampedStages state =
  [ stage
  | (_, stage) <-
      Publication.stampedStructuralStageEntries (startupPublicationState state),
    Publication.stampedStructuralEnvironmentRoot stage /= Nothing
  ]

assertEnvironmentStamped ::
  String ->
  Environment.EnvironmentManifest ->
  Set.Set HeraldEpoch ->
  HeraldState ->
  Assertion
assertEnvironmentStamped context manifest expectedPeers state = do
  let roots = manifestRoots manifest
      stages = environmentStampedStages state
      unstamped =
        [ ( Publication.structuralSourceMembershipGenerationId source,
            Publication.structuralSourceTopologyPrerequisite source,
            Publication.structuralSourceControlPrerequisite source
          )
        | source <-
            Publication.unstampedStructuralSourceStageEntries
              (startupPublicationState state),
          Publication.structuralSourceEnvironmentRoot source /= Nothing
        ]
      progress = startupStructuralProgressState state
      coordinator = Startup.startupStructuralBaseCoordinator state
      coordinatorCoordinate =
        ( \coordinatorState ->
            ( heraldMembershipGenerationId
                (StructuralBase.structuralBasePredecessorMembership coordinatorState),
              heraldMembershipGenerationId
                (StructuralBase.structuralBaseSuccessorMembership coordinatorState),
              StructuralBase.structuralBaseEvidence coordinatorState
                >>= \base ->
                  Just (GraphProgress.structuralSuccessorBaseInstalled base progress)
            )
        )
          <$> coordinator
      retained =
        [ (retainedManifest, root, stage)
        | stage <- stages,
          Just (retainedManifest, root) <- [Publication.stampedStructuralEnvironmentRoot stage]
        ]
  assertEqual
    ( context
        <> " stamps every exact root; unstamped coordinates="
        <> show unstamped
        <> "; current/progress membership="
        <> show
          ( heraldMembershipGenerationId (currentMembership state),
            GraphProgress.structuralProgressMembershipGenerationId progress
          )
        <> "; last cut/control="
        <> show
          ( GraphProgress.structuralLastInstalledCutId progress,
            GraphProgress.structuralAppliedControlPrefix progress
          )
        <> "; coordinator="
        <> show coordinatorCoordinate
    )
    (length roots)
    (length stages)
  assertEqual
    (context <> " retains the immutable manifest/root pairs")
    roots
    [root | (_, root, _) <- retained]
  assertBool
    (context <> " retains the exact accepted manifest in every stage")
    (all (Environment.environmentManifestExtends manifest . firstOfThree) retained)
  assertBool
    (context <> " emits exactly one receiver-qualified item per active remote")
    (all ((== expectedPeers) . Map.keysSet . Publication.stampedStructuralPeerPublications . thirdOfThree) retained)
  where
    firstOfThree (first, _, _) = first
    thirdOfThree (_, _, third) = third

pendingManifests :: HeraldState -> [Environment.EnvironmentManifest]
pendingManifests =
  fmap (Environment.pendingEnvironmentManifest . snd)
    . Application.applicationPendingEnvironmentEntries
    . startupApplicationState

completedManifests :: HeraldState -> [Environment.EnvironmentManifest]
completedManifests =
  fmap (Environment.completedEnvironmentManifest . snd)
    . Application.applicationCompletedEnvironmentEntries
    . startupApplicationState

soleCompletedEnvironment :: HeraldState -> IO Environment.CompletedEnvironment
soleCompletedEnvironment state =
  case Application.applicationCompletedEnvironmentEntries (startupApplicationState state) of
    [(_, completed)] -> pure completed
    observed ->
      assertFailure
        ("expected one completed environment, got " <> show (length observed))

runEnvironment ::
  Opened ->
  RequestId ->
  HeraldState ->
  Either
    NewEnvironment.NewEnvironmentFailure
    (HeraldState, NewEnvironment.NewEnvironmentOutcome)
runEnvironment opened request =
  NewEnvironment.planNewEnvironment
    opened.session
    opened.binding
    request
    Application.newEnvironmentCoordinatorInput

kernelWorkDelta :: HeraldState -> HeraldState -> NewEnvironmentKernelWork
kernelWorkDelta predecessor successor =
  NewEnvironmentKernelWork
    { atomicAcceptances =
        length (Publication.environmentManifestEntries afterPublication)
          - length (Publication.environmentManifestEntries beforePublication),
      rootApplications =
        length (environmentStampedStages successor)
          - length (environmentStampedStages predecessor),
      receiverDeliveries =
        retainedRemoteItems successor - retainedRemoteItems predecessor,
      accessSettlements =
        length (Application.applicationCompletedEnvironmentEntries afterApplication)
          - length (Application.applicationCompletedEnvironmentEntries beforeApplication)
    }
  where
    beforePublication = startupPublicationState predecessor
    afterPublication = startupPublicationState successor
    beforeApplication = startupApplicationState predecessor
    afterApplication = startupApplicationState successor
    retainedRemoteItems =
      sum
        . fmap (Map.size . Publication.stampedStructuralPeerPublications)
        . environmentStampedStages

data Opened = Opened
  { state :: HeraldState,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    process :: ProcessEpochId,
    attachment :: ApplicationAttachment
  }

data FourHeraldFixture = FourHeraldFixture
  { homeGenesis :: CheckedHeraldGenesis,
    homeBootstraps :: CheckedInitialBootstraps,
    oracleBinding :: OracleBinding,
    opened :: Opened,
    initialRemotes :: Map.Map HeraldEpoch HeraldState,
    members :: [HeraldMember],
    h1 :: HeraldEpoch,
    h2 :: HeraldEpoch,
    h3 :: HeraldEpoch,
    h4 :: HeraldEpoch
  }

fourHeraldFixture :: IO FourHeraldFixture
fourHeraldFixture = do
  let members = fourMembers
      h1Member = fixtureLocalMember
      h2Member = fixtureRemoteMember
      h3Member = environmentH3
      h4Member = environmentH4
      homeGenesis = checkedValue "H1 environment genesis" (checkHeraldGenesis (deploymentAt h1Member))
  homeBootstraps <- bootstrapsFor homeGenesis
  (homeInitial, initialEffects) <- initialize fixtureGeneratorSeedH1 homeGenesis homeBootstraps
  (homeBound, oracleBinding) <- bindOracle homeInitial initialEffects
  opened <- openEnvironmentProcess homeGenesis homeBootstraps homeBound
  remotes <-
    traverse
      ( \(member, seed) -> do
          let genesis = checkedValue "remote environment genesis" (checkHeraldGenesis (deploymentAt member))
          bootstraps <- bootstrapsFor genesis
          fst <$> initialize seed genesis bootstraps
      )
      [ (h2Member, fixtureGeneratorSeedH2),
        (h3Member, fixtureGeneratorSeedH3),
        (h4Member, environmentH4Seed)
      ]
  pure
    FourHeraldFixture
      { homeGenesis,
        homeBootstraps,
        oracleBinding,
        opened,
        initialRemotes = Map.fromList (zip (heraldMemberEpoch <$> drop 1 members) remotes),
        members,
        h1 = heraldMemberEpoch h1Member,
        h2 = heraldMemberEpoch h2Member,
        h3 = heraldMemberEpoch h3Member,
        h4 = heraldMemberEpoch h4Member
      }
  where
    deploymentAt local =
      environmentDeployment
        { deploymentLocalHeraldId = heraldMemberId local,
          deploymentLocalHeraldEpoch = heraldMemberEpoch local
        }

environmentDeployment :: DeploymentManifest
environmentDeployment =
  fixtureDeploymentManifest
    { deploymentActiveHeralds = fourMembers,
      deploymentOracleGenesis =
        (deploymentOracleGenesis fixtureDeploymentManifest)
          { oracleGenesisActiveHeralds = fourMembers
          }
    }

fourMembers :: [HeraldMember]
fourMembers = fixtureMembers <> [environmentH3, environmentH4]

environmentH3 :: HeraldMember
environmentH3 =
  HeraldMember
    (checkedValue "environment H3 id" (mkHeraldId (fixtureIdentifierBytes 0x81)))
    (checkedValue "environment H3 epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x82)))

environmentH4 :: HeraldMember
environmentH4 =
  HeraldMember
    (checkedValue "environment H4 id" (mkHeraldId (fixtureIdentifierBytes 0x83)))
    (checkedValue "environment H4 epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x84)))

environmentH4Seed :: IdGenerator.GeneratorSeed
environmentH4Seed =
  checkedValue
    "environment H4 generator seed"
    (IdGenerator.mkGeneratorSeed (ByteString.replicate 32 0x44))

bootstrapsFor :: CheckedHeraldGenesis -> IO CheckedInitialBootstraps
bootstrapsFor genesis = do
  bootstrap <- localBootstrap
  checkedIO
    "environment initial bootstraps"
    (checkInitialBootstraps genesis (PrimordialProcessManifest [bootstrap]))

initialize ::
  IdGenerator.GeneratorSeed ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  IO (HeraldState, EffectBatch)
initialize seed genesis bootstraps =
  checkedIO
    "initialize environment Herald"
    ( initialHerald
        (monotonicInstant 0)
        genesis
        bootstraps
        fixtureOracleContacts
        seed
        fixtureApplicationRecoveryConfiguration
        fixturePeerRecoveryConfiguration
    )

bindOracle :: HeraldState -> EffectBatch -> IO (HeraldState, OracleBinding)
bindOracle initial effects = do
  attempt <- initialOracleAttempt effects
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (controlIndex 2)
          (Just node)
          True
  (bound, bindEffects) <-
    step 1 (OracleInput (OracleHelloReceived attempt acceptance)) initial
  case effectBatchMembers bindEffects of
    [ RunOracleClientAction (BindOracleConnection observed binding),
      RunOracleClientAction (WatchOracle watched cursor)
      ]
        | observed == attempt,
          watched == binding,
          cursor == controlIndex 0 ->
            pure (bound, binding)
    observed ->
      assertFailure
        ("environment Oracle binding emitted " <> show observed)

initialOracleAttempt :: EffectBatch -> IO OracleConnectAttempt
initialOracleAttempt effects =
  case [ attempt
       | RunOracleClientAction (ConnectAndHelloOracle attempt _) <- effectBatchMembers effects
       ] of
    [attempt] -> pure attempt
    observed ->
      assertFailure
        ("environment initialization emitted " <> show (length observed) <> " Oracle attempts")

openEnvironmentProcess ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  HeraldState ->
  IO Opened
openEnvironmentProcess genesis bootstraps predecessor = do
  bootstrap <- localBootstrap
  attachment <-
    maybe
      (assertFailure "environment attachment is absent")
      pure
      (primordialApplicationAttachment genesis bootstraps bootstrap)
  process <-
    maybe
      (assertFailure "environment attachment has no process")
      pure
      ( Application.applicationAttachmentProcess
          attachment
          (startupApplicationState predecessor)
      )
  prepared <-
    checkedIO
      "open environment application session"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch genesis)
          attachment
          (clientNonce 1)
          (startupApplicationState predecessor)
      )
  let (application, acceptance) =
        Application.commitApplicationSessionAcceptance prepared
      successor = replaceStartupApplicationState application predecessor
      binding = sessionAcceptanceBinding acceptance
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ _ ->
      pure Opened {state = successor, session, binding, process, attachment}
    reply -> assertFailure ("environment session was not opened: " <> show reply)

localBootstrap :: IO BootstrapManifestId
localBootstrap =
  case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "environment fixture has no H1 bootstrap"

acceptEnvironment ::
  Opened ->
  HeraldState ->
  IO (HeraldState, Environment.EnvironmentManifest)
acceptEnvironment opened predecessor =
  case NewEnvironment.planNewEnvironment
    opened.session
    opened.binding
    environmentRequest
    Application.newEnvironmentCoordinatorInput
    predecessor of
    Right (successor, NewEnvironment.NewEnvironmentAccepted manifest) ->
      pure (successor, manifest)
    Right (_, outcome) -> assertFailure ("environment was not accepted: " <> show outcome)
    Left problem -> assertFailure ("environment acceptance failed: " <> show problem)

applyFixtureProcessEnd :: FourHeraldFixture -> HeraldState -> IO HeraldState
applyFixtureProcessEnd fixture predecessor = do
  oracle <-
    checkedIO
      "initialize process-End Oracle"
      (initialOracle (matchingOracleGenesis fixture))
  let envelope =
        oracleEnvelope
          (oracleClientRequestId fixture.h1 701)
          Nothing
          fixture.h1
          ( checkedValue
              "construct environment process-End command"
              (endProcessEpochCommand fixture.opened.process ExplicitAdministrativeEnd)
          )
  canonical <- commitOracleEntry oracle envelope
  (successor, _) <-
    step
      2
      (OracleInput (OracleEntriesReceived fixture.oracleBinding (NonEmpty.fromList [canonical])))
      predecessor
  pure successor

commitOracleEntry :: OracleState -> OracleEnvelope -> IO CanonicalAppliedOracleEntry
commitOracleEntry predecessor envelope =
  case stepOracle envelope predecessor of
    Left problem -> assertFailure ("commit environment process End: " <> show problem)
    Right (_, OracleCommitted _, effects) ->
      case oracleEffects effects of
        [EmitAppliedOracleEntry entry] -> pure (canonicalizeAppliedOracleEntry entry)
        observed ->
          assertFailure
            ("environment process-End Oracle emitted " <> show observed)
    Right (_, outcome, _) ->
      assertFailure ("environment process-End Oracle returned " <> show outcome)

matchingOracleGenesis :: FourHeraldFixture -> CheckedOracleGenesis
matchingOracleGenesis fixture =
  checkedValue
    "check matching environment Oracle genesis"
    ( checkOracleGenesis
        ( oracleGenesis
            (checkedSystemId fixture.homeGenesis)
            (checkedActiveHeralds fixture.homeGenesis)
            (checkedCatalogueDigest fixture.homeGenesis)
            (checkedOraclePredefinedDescriptors fixtureCheckedOracleGenesis)
            (checkedInitialBootstraps fixture.homeBootstraps)
            (checkedConfigurationDigest fixture.homeGenesis)
            (checkedInitialTopologyProjection fixture.homeBootstraps)
            (checkedInitialProjectionDigest fixture.homeBootstraps)
            bindings
            nativeConfiguration
            ( deriveRaftConfigurationDigest
                (checkedSystemId fixture.homeGenesis)
                bindings
                nativeConfiguration
            )
        )
    )
  where
    bindings =
      zipWith
        (\binding member -> raftVoterBinding (raftVoterBindingNode binding) (heraldMemberEpoch member))
        (checkedOracleRaftVoterBindings fixtureCheckedOracleGenesis)
        (checkedActiveHeralds fixture.homeGenesis)
    nativeConfiguration =
      checkedOracleRaftNativeConfiguration fixtureCheckedOracleGenesis

environmentRequest :: RequestId
environmentRequest = requestId 1

retireH4 :: FourHeraldFixture -> HeraldState -> IO (HeraldState, HeraldMembershipGeneration)
retireH4 fixture supplied = do
  let predecessor
        | OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (Startup.startupOracleProjectionState supplied)) == controlIndex 0 = fixtureHeraldAfterProbePrefix supplied
        | otherwise = supplied
      predecessorMembership = currentMembership predecessor
      successorMembership =
        checkedValue
          "environment H4 retirement membership"
          ( retireHeraldMembershipGeneration
              (controlIndex 2)
              (fixtureRetirementResolution (controlIndex 2))
              fixture.h4
              predecessorMembership
          )
  membershipAdvance <-
    checkedIO
      "prepare environment H4 membership advance"
      ( MembershipAdvance.prepareMembershipAdvance
          (controlIndex 2)
          successorMembership
          []
          predecessor
      )
  let afterMembership = MembershipAdvance.commitMembershipAdvance membershipAdvance
  base <-
    checkedIO
      "begin environment successor structural base"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          predecessor
          afterMembership
      )
  established <-
    establishEmptyStructuralBase predecessor membershipAdvance base
  successorCut <-
    checkedIO
      "derive environment successor structural cut"
      (StructuralBase.expectedSuccessorStructuralCut established)
  installedCoordinator <-
    checkedIO
      "record environment successor structural cut"
      ( StructuralBase.recordInstalledSuccessorStructuralCut
          successorCut
          established
      )
  let withCoordinator =
        replaceStartupStructuralBaseCoordinator
          (Just installedCoordinator)
          afterMembership
  preparedClosure <-
    checkedIO
      "prepare environment retirement closure"
      ( RetirementClosure.prepareRetirementClosure
          predecessor
          withCoordinator
          membershipAdvance
          installedCoordinator
      )
  let successor = RetirementClosure.commitRetirementClosure preparedClosure
  assertValid "environment retirement closure" successor
  pure (successor, successorMembership)

currentMembership :: HeraldState -> HeraldMembershipGeneration
currentMembership =
  OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . Startup.startupOracleProjectionState

step :: Word64 -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step observed body state =
  checkedIO
    "environment Herald transition"
    ( verifiedStepHerald
        (heraldInput (monotonicInstant observed) body)
        state
    )

assertValid :: String -> HeraldState -> Assertion
assertValid context =
  either
    (assertFailure . ((context <> " violates the Herald invariant: ") <>) . show)
    pure
    . validateHeraldState

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
