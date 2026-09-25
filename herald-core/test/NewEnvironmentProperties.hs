{-# LANGUAGE OverloadedStrings #-}

module NewEnvironmentProperties
  ( tests,
    Fixture (..),
    fixture,
  )
where

import GenesisFixtures (fixtureGenesisRetirementLineage, fixtureRetirementResolution)

import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Identity (mkPrivateUniqueId)
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewEnvironmentApplication, NewIdApplication),
  )
import Eclips.Application.Types.Rejection
  ( ApplicationRejection (ApplicationOperateNotPermitted),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (EnvironmentStabilizationPending),
    RegularCallResult (NewIdCompleted),
  )
import Eclips.Domain.Identity
  ( ProcessEpochId,
    controlIndex,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
  )
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    ApplicationRequestReply (RequestConflict, RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (OperationAccepted),
    processAcceptancePositionOrdinal,
    requestId,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened, SessionResumed),
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (SendApplicationReply),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( HeraldMember (..),
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.TerminalSource
  ( terminalSourceGenesisPredecessorBase,
  )
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Initialization
  ( HeraldState,
    initialHerald,
    primordialApplicationAttachment,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupOracleProjectionState,
    replaceStartupStructuralBaseCoordinator,
    startupApplicationState,
    startupControlledState,
    startupGraphState,
    startupIdGeneratorState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStructuralProgressState,
  )
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
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
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
  ( Positive (Positive),
    Property,
    counterexample,
    testProperty,
    (.&&.),
  )

tests :: TestTree
tests =
  testGroup
    "private newenv acceptance composition"
    [ testCase
        "public EAPP newenv emits and retains its pending reply atomically"
        casePublicApplicationCall,
      testCase
        "first acceptance advances all four owners atomically and defers structural stamping"
        caseAtomicFirstAcceptance,
      testCase
        "exact retry returns the retained manifest and is whole-state identical"
        caseExactRetry,
      testCase
        "an ordinary request occupying the same key rejects without a successor"
        caseConflictingRequest,
      testCase
        "binding loss and resume preserve an accepted manifest and exact retry"
        caseBindingLossAndResume,
      testCase
        "recovery expiry detaches an accepted reply while a sibling keeps the process live"
        caseRecoveryExpiryDetachesReply,
      testCase
        "session End detaches only reply reachability"
        caseSessionEnd,
      testCase
        "process End gates fresh acceptance and preserves accepted global work"
        caseProcessEnd,
      testCase
        "unavailable canonical source authority rejects without changing an owner"
        caseUnavailableSourceAuthority,
      testCase
        "the exact Step-15 membership gap defers without consuming acceptance state"
        caseExactMembershipGapDefers,
      testCase
        "uncoordinated or unrelated membership gaps are invariant faults"
        caseInvalidMembershipGapFaults,
      testProperty
        "repeating a finite acceptance/retry schedule is deterministic"
        propDeterministicSchedule
    ]

casePublicApplicationCall :: Assertion
casePublicApplicationCall = do
  let predecessor = fixtureState fixture
      binding = fixtureBinding fixture
      request = fixtureRequest
      ingress call =
        CallApplicationRequest
          binding
          (fixtureSession fixture)
          request
          call
  (accepted, acceptedEffects) <-
    checkedIO
      "accept public newenv call"
      (ApplicationCall.applyApplicationRequest (ingress NewEnvironmentApplication) predecessor)
  let expectedReplyBody =
        OperationAccepted request EnvironmentStabilizationPending
  acceptedReply <- soleApplicationReply "public newenv acceptance" binding acceptedEffects
  case acceptedReply of
    RetainedRequestReply _ body ->
      assertEqual "public newenv pending body" expectedReplyBody body
    reply -> assertFailure ("public newenv did not retain a reply: " <> show reply)
  assertEqual
    "public newenv retains one pending manifest"
    1
    (length (Application.applicationPendingEnvironmentEntries (startupApplicationState accepted)))
  assertValidHerald "public newenv acceptance" accepted

  (retried, retryEffects) <-
    checkedIO
      "retry public newenv call"
      (ApplicationCall.applyApplicationRequest (ingress NewEnvironmentApplication) accepted)
  retryReply <- soleApplicationReply "public newenv retry" binding retryEffects
  assertEqual "exact public retry reoffers the retained reply" acceptedReply retryReply
  assertBool "exact public retry is state-identical" (retried == accepted)

  (conflicted, conflictEffects) <-
    checkedIO
      "conflict public newenv request"
      (ApplicationCall.applyApplicationRequest (ingress (NewIdApplication BareNewId)) accepted)
  conflictReply <- soleApplicationReply "public newenv conflict" binding conflictEffects
  assertEqual "same request with another call conflicts" (RequestConflict request) conflictReply
  assertBool "public request conflict is state-identical" (conflicted == accepted)

soleApplicationReply ::
  String ->
  ApplicationSessionBinding ->
  EffectBatch ->
  IO ApplicationRequestReply
soleApplicationReply context expectedBinding effects =
  case [ reply
       | SendApplicationReply binding reply <- effectBatchMembers effects,
         binding == expectedBinding
       ] of
    [reply] -> pure reply
    replies ->
      assertFailure
        (context <> " emitted " <> show (length replies) <> " matching replies")
        >> fail "missing sole application reply"

caseAtomicFirstAcceptance :: Assertion
caseAtomicFirstAcceptance = do
  let predecessor = fixtureState fixture
      (successor, manifest) = acceptEnvironment fixtureRequest fixture predecessor
      beforeApplication = startupApplicationState predecessor
      afterApplication = startupApplicationState successor
      beforePublication = startupPublicationState predecessor
      afterPublication = startupPublicationState successor
      beforeWitness = Publication.publicationStateWitness beforePublication
      afterWitness = Publication.publicationStateWitness afterPublication
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
      beforePosition = publicationNextPosition predecessor
      afterPosition = publicationNextPosition successor
  assertValidHerald "first accepted environment" successor
  assertEqual
    "generated identity range reserves all 31 objects"
    (generatorCounter predecessor + 31)
    (generatorCounter successor)
  assertEqual
    "process acceptance supply advances by one"
    (fmap (+ 1) (nextProcessPosition (fixtureProcess fixture) beforeApplication))
    (nextProcessPosition (fixtureProcess fixture) afterApplication)
  assertEqual
    "manifest has the canonical root count"
    12
    (length roots)
  assertEqual
    "manifest owns the consumed process position"
    (nextProcessPosition (fixtureProcess fixture) beforeApplication)
    ( Just
        ( processAcceptancePositionOrdinal
            (Environment.environmentManifestPosition manifest)
        )
    )
  assertEqual
    "Herald positions advance by twelve"
    (beforePosition + 12)
    afterPosition
  assertEqual
    "the manifest occupies the exact consecutive Herald-position range"
    [beforePosition .. beforePosition + 11]
    ( fmap
        ( Publication.heraldPublicationPositionWord64
            . Environment.positionedEnvironmentRootHeraldPosition
        )
        roots
    )
  assertSourceTenuresAdvance beforePublication afterPublication roots
  assertEqual
    "one complete manifest is retained by Publication"
    [manifest]
    (Publication.publicationWitnessEnvironmentManifests afterWitness)
  assertEqual
    "all roots are retained by Publication"
    12
    (length (Publication.publicationWitnessEnvironmentRootStages afterWitness))
  assertEqual
    "Application retains one reply-linked pending manifest"
    [manifest]
    (pendingManifests afterApplication)
  assertEqual
    "Controlled gains exactly twelve dynamic records"
    (length (Controlled.controlledLocalRecords (startupControlledState predecessor)) + 12)
    (length (Controlled.controlledLocalRecords (startupControlledState successor)))
  assertEqual
    "newenv allocates no Controlled reservations"
    (Controlled.controlledReservationWitnesses (startupControlledState predecessor))
    (Controlled.controlledReservationWitnesses (startupControlledState successor))
  assertEqual
    "no structural sequence is allocated"
    (Publication.publicationWitnessNextStructuralSequence beforeWitness)
    (Publication.publicationWitnessNextStructuralSequence afterWitness)
  assertEqual
    "no structural stage is stamped"
    (Publication.publicationWitnessStampedStructuralStages beforeWitness)
    (Publication.publicationWitnessStampedStructuralStages afterWitness)
  assertEqual
    "all accepted roots enter the third structural source arm"
    [ (manifest, root)
    | root <- roots
    ]
    [ retained
    | source <- Publication.unstampedStructuralSourceStageEntries afterPublication,
      Just retained <- [Publication.structuralSourceEnvironmentRoot source]
    ]
  assertEqual
    "no ordinary outgoing peer publication is fabricated"
    (Publication.publicationWitnessOutgoing beforeWitness)
    (Publication.publicationWitnessOutgoing afterWitness)
  assertBool
    "peer-stream owner is unchanged"
    (startupPeerStreamState successor == startupPeerStreamState predecessor)
  assertBool
    "Graph is unchanged before structural reconciliation"
    (startupGraphState successor == startupGraphState predecessor)
  assertBool
    "Placement is unchanged before structural reconciliation"
    (startupPlacementState successor == startupPlacementState predecessor)

caseExactRetry :: Assertion
caseExactRetry = do
  let (accepted, manifest) =
        acceptEnvironment fixtureRequest fixture (fixtureState fixture)
      (retried, outcome) =
        checkedPure
          "retry private environment"
          (runEnvironment fixtureRequest (fixtureBinding fixture) accepted)
  assertEqual
    "retry classification"
    (NewEnvironment.NewEnvironmentRetried manifest)
    outcome
  assertBool "exact retry is whole-state identical" (retried == accepted)
  assertValidHerald "exact environment retry" retried

caseConflictingRequest :: Assertion
caseConflictingRequest = do
  let predecessor = fixtureState fixture
      application = startupApplicationState predecessor
      occupiedApplication = occupyOrdinaryRequest fixture application
      occupied = replaceStartupApplicationState occupiedApplication predecessor
      attempt = runEnvironment fixtureRequest (fixtureBinding fixture) occupied
      rolledBack = either (const occupied) fst attempt
  case attempt of
    Left (NewEnvironment.NewEnvironmentRequestConflict conflicting) ->
      assertEqual "conflicting request identity" fixtureRequest conflicting
    Left problem -> assertFailure ("unexpected request failure: " <> show problem)
    Right _ -> assertFailure "ordinary request namespace was accepted as newenv"
  assertBool "failed coordination exposes no successor" (rolledBack == occupied)
  assertEqual
    "request conflict does not generate identities"
    (generatorCounter occupied)
    (generatorCounter rolledBack)
  assertEqual
    "request conflict retains no environment publication"
    []
    (Publication.environmentManifestEntries (startupPublicationState rolledBack))

caseBindingLossAndResume :: Assertion
caseBindingLossAndResume = do
  let (accepted, manifest) =
        acceptEnvironment fixtureRequest fixture (fixtureState fixture)
      acceptedApplication = startupApplicationState accepted
      afterLossApplication =
        Application.commitApplicationBindingLoss
          ( Application.prepareApplicationBindingLoss
              fixtureApplicationRecoveryConfiguration
              (monotonicInstant 10)
              (fixtureBinding fixture)
              acceptedApplication
          )
      afterLoss = replaceStartupApplicationState afterLossApplication accepted
  assertValidHerald "binding loss with retained environment" afterLoss
  assertEqual
    "binding loss preserves the accepted manifest and reply key"
    (Application.applicationPendingEnvironmentEntries acceptedApplication)
    (Application.applicationPendingEnvironmentEntries afterLossApplication)
  resumePreparation <-
    checkedIO
      "resume newenv fixture session"
      ( Application.prepareApplicationSessionResume
          (fixtureSession fixture)
          (fixtureResumeToken fixture)
          (fixtureCursor fixture)
          afterLossApplication
      )
  let (resumedApplication, acceptance) =
        Application.commitApplicationSessionAcceptance resumePreparation
      resumed = replaceStartupApplicationState resumedApplication afterLoss
      resumedBinding = sessionAcceptanceBinding acceptance
  case sessionAcceptanceReply acceptance of
    SessionResumed resumedSession _ ->
      assertEqual "resumed session identity" (fixtureSession fixture) resumedSession
    reply -> assertFailure ("expected resumed session, got " <> show reply)
  let (retried, outcome) =
        checkedPure
          "retry environment after resume"
          (runEnvironment fixtureRequest resumedBinding resumed)
  assertEqual
    "resumed exact retry returns the retained manifest"
    (NewEnvironment.NewEnvironmentRetried manifest)
    outcome
  assertBool "resumed exact retry is state-identical" (retried == resumed)
  (publicRetried, retryEffects) <-
    checkedIO
      "retry resumed environment through public application call"
      ( ApplicationCall.applyApplicationRequest
          ( CallApplicationRequest
              resumedBinding
              (fixtureSession fixture)
              fixtureRequest
              NewEnvironmentApplication
          )
          resumed
      )
  publicReply <- soleApplicationReply "resumed public newenv retry" resumedBinding retryEffects
  case publicReply of
    RetainedRequestReply _ (OperationAccepted retained EnvironmentStabilizationPending) ->
      assertEqual "resumed public retry request" fixtureRequest retained
    reply -> assertFailure ("unexpected resumed public reply: " <> show reply)
  assertBool "resumed public retry is state-identical" (publicRetried == resumed)
  assertValidHerald "resumed environment retry" retried

caseRecoveryExpiryDetachesReply :: Assertion
caseRecoveryExpiryDetachesReply = do
  let (accepted, manifest) =
        acceptEnvironment fixtureRequest fixture (fixtureState fixture)
      acceptedApplication = startupApplicationState accepted
  siblingPreparation <-
    checkedIO
      "open sibling newenv fixture session"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          (fixtureAttachment fixture)
          (clientNonce 2)
          acceptedApplication
      )
  let (withSiblingApplication, siblingAcceptance) =
        Application.commitApplicationSessionAcceptance siblingPreparation
      siblingSession = case sessionAcceptanceReply siblingAcceptance of
        SessionOpened session _ _ -> session
        reply -> error ("expected opened sibling session, got " <> show reply)
      withSibling =
        replaceStartupApplicationState withSiblingApplication accepted
      lossPreparation =
        Application.prepareApplicationBindingLoss
          fixtureApplicationRecoveryConfiguration
          (monotonicInstant 10)
          (fixtureBinding fixture)
          withSiblingApplication
      (recoveryAttempt, recoveryDeadline) =
        case Application.preparedApplicationBindingLossDisposition lossPreparation of
          Application.ApplicationRecoveryStarted attempt spec ->
            (attempt, timerSpecAbsoluteDeadline spec)
          disposition ->
            error
              ( "expected current environment-session recovery, got "
                  <> show disposition
              )
      afterLossApplication =
        Application.commitApplicationBindingLoss lossPreparation
      afterLoss = replaceStartupApplicationState afterLossApplication withSibling
      firedAt = monotonicInstant (monotonicInstantWord64 recoveryDeadline + 1)
  assertEqual
    "the accepted environment starts with the request session's reply key"
    [Just (Environment.environmentReplyKey (fixtureSession fixture) fixtureRequest)]
    ( pendingEnvironmentReplyKeys
        (Application.applicationPendingEnvironmentEntries acceptedApplication)
    )
  assertEqual
    "binding loss alone preserves the accepted environment reply key"
    (Application.applicationPendingEnvironmentEntries withSiblingApplication)
    (Application.applicationPendingEnvironmentEntries afterLossApplication)
  assertValidHerald "accepted environment with live sibling" withSibling
  assertValidHerald "lost environment session with live sibling" afterLoss
  timerPreparation <-
    checkedIO
      "expire current environment-session recovery"
      ( Application.prepareApplicationRecoveryTimerObservation
          recoveryAttempt
          (TimerFired firedAt)
          afterLossApplication
      )
  case Application.preparedApplicationRecoveryTimerDisposition timerPreparation of
    Application.ApplicationRecoveryTimerExpired expiration -> do
      assertEqual
        "the current recovery expires only the lost reply-owning session"
        [fixtureSession fixture]
        (Application.applicationRecoveryExpiredSessionIds expiration)
      assertEqual
        "the attached sibling prevents process sealing"
        Nothing
        (Application.applicationRecoverySealedProcess expiration)
    disposition ->
      assertFailure
        ("current recovery timer did not expire: " <> show disposition)
  let expiredApplication =
        Application.commitApplicationRecoveryTimerObservation timerPreparation
      expired = replaceStartupApplicationState expiredApplication afterLoss
  assertDetachedManifest "session recovery expiry" manifest expired
  assertEqual
    "the expired reply-owning session is gone"
    Nothing
    (Application.applicationSessionProcess (fixtureSession fixture) expiredApplication)
  assertEqual
    "the sibling keeps the accepted environment process live"
    (Just (fixtureProcess fixture))
    (Application.applicationSessionProcess siblingSession expiredApplication)
  assertValidHerald "expired environment session with live sibling" expired

caseSessionEnd :: Assertion
caseSessionEnd = do
  let (accepted, manifest) =
        acceptEnvironment fixtureRequest fixture (fixtureState fixture)
  endPreparation <-
    checkedIO
      "end newenv fixture session"
      ( Application.prepareApplicationSessionEnd
          (fixtureSession fixture)
          (fixtureBinding fixture)
          (startupApplicationState accepted)
      )
  let endedApplication = Application.commitApplicationSessionEnd endPreparation
      ended = replaceStartupApplicationState endedApplication accepted
  assertDetachedManifest "session End" manifest ended
  assertEqual
    "session End removes request reply ownership"
    []
    ( Application.applicationEnvironmentRequestEntries
        (fixtureSession fixture)
        endedApplication
    )
  assertEqual
    "session End preserves Publication's immutable manifest"
    (Publication.environmentManifestEntries (startupPublicationState accepted))
    (Publication.environmentManifestEntries (startupPublicationState ended))
  assertBool
    "session End preserves Controlled roots"
    (startupControlledState ended == startupControlledState accepted)
  assertValidHerald "session End with detached environment reply" ended

caseProcessEnd :: Assertion
caseProcessEnd = do
  let predecessor = fixtureState fixture
      endedBefore = retireProcess (fixtureProcess fixture) predecessor
      beforeAttempt = runEnvironment fixtureRequest (fixtureBinding fixture) endedBefore
      rolledBackBefore = either (const endedBefore) fst beforeAttempt
  case beforeAttempt of
    Left NewEnvironment.NewEnvironmentSessionFailure {} -> pure ()
    Left problem -> assertFailure ("unexpected pre-acceptance End failure: " <> show problem)
    Right _ -> assertFailure "newenv was accepted after process End"
  assertBool
    "pre-acceptance End rejection exposes no successor"
    (rolledBackBefore == endedBefore)
  assertEqual
    "pre-acceptance End consumes no generated identity"
    (generatorCounter predecessor)
    (generatorCounter endedBefore)
  assertEqual
    "pre-acceptance End retains no manifest"
    []
    (Publication.environmentManifestEntries (startupPublicationState endedBefore))

  let (accepted, manifest) = acceptEnvironment fixtureRequest fixture predecessor
      endedAfter = retireProcess (fixtureProcess fixture) accepted
      targets =
        fmap
          ( Environment.environmentRootPlanTargetObject
              . Environment.positionedEnvironmentRootPlan
          )
          (NonEmpty.toList (Environment.environmentManifestRoots manifest))
  assertDetachedManifest "process End" manifest endedAfter
  assertEqual
    "process End preserves the publication-owned global manifest"
    (Publication.environmentManifestEntries (startupPublicationState accepted))
    (Publication.environmentManifestEntries (startupPublicationState endedAfter))
  assertEqual
    "process End preserves all immutable Controlled root records"
    (Controlled.controlledLocalRecords (startupControlledState accepted))
    (Controlled.controlledLocalRecords (startupControlledState endedAfter))
  assertBool
    "process End revokes every environment-root possession"
    ( all
        ( \target ->
            not
              ( Controlled.controlledHasNormalPossession
                  (fixtureProcess fixture)
                  target
                  (startupControlledState endedAfter)
              )
        )
        targets
    )
  let postEndAttempt =
        runEnvironment fixtureRequest (fixtureBinding fixture) endedAfter
      postEndRollback = either (const endedAfter) fst postEndAttempt
  case postEndAttempt of
    Left NewEnvironment.NewEnvironmentSessionFailure {} -> pure ()
    Left problem -> assertFailure ("unexpected post-acceptance End failure: " <> show problem)
    Right _ -> assertFailure "ended process retained a callable reply link"
  assertBool
    "post-acceptance End retry cannot rewrite retained work"
    (postEndRollback == endedAfter)

caseUnavailableSourceAuthority :: Assertion
caseUnavailableSourceAuthority = do
  let predecessor = fixtureState fixture
      process = fixtureProcess fixture
      unavailableControlled =
        Controlled.commitControlledProcessRetirement
          ( Controlled.prepareControlledProcessRetirement
              process
              (startupControlledState predecessor)
          )
      unavailable =
        replaceStartupControlledState unavailableControlled predecessor
      attempt = runEnvironment fixtureRequest (fixtureBinding fixture) unavailable
      rolledBack = either (const unavailable) fst attempt
  case attempt of
    Left
      ( NewEnvironment.NewEnvironmentRejected
          ApplicationOperateNotPermitted
        ) -> pure ()
    Left problem ->
      assertFailure ("unexpected unavailable-source failure: " <> show problem)
    Right _ -> assertFailure "newenv accepted a process without source authority"
  assertBool "source rejection exposes no successor state" (rolledBack == unavailable)
  assertEqual
    "source rejection consumes no generated identity"
    (generatorCounter unavailable)
    (generatorCounter rolledBack)
  assertEqual
    "source rejection consumes no process position"
    (nextProcessPosition process (startupApplicationState unavailable))
    (nextProcessPosition process (startupApplicationState rolledBack))
  assertBool
    "source rejection changes no Application owner state"
    (startupApplicationState rolledBack == startupApplicationState unavailable)
  assertBool
    "source rejection changes no Publication owner state"
    (startupPublicationState rolledBack == startupPublicationState unavailable)
  assertBool
    "source rejection changes no Controlled owner state"
    (startupControlledState rolledBack == startupControlledState unavailable)

caseExactMembershipGapDefers :: Assertion
caseExactMembershipGapDefers = do
  let gap = exactMembershipGapState 7
      beforeApplication = startupApplicationState gap
      attempt = runEnvironment fixtureRequest (fixtureBinding fixture) gap
  (deferred, outcome) <- checkedIO "defer exact membership gap" attempt
  assertEqual "exact membership gap outcome" NewEnvironment.NewEnvironmentDeferred outcome
  assertEqual
    "membership deferral consumes no generated identity"
    (generatorCounter gap)
    (generatorCounter deferred)
  assertEqual
    "membership deferral consumes no process position"
    (nextProcessPosition (fixtureProcess fixture) beforeApplication)
    (nextProcessPosition (fixtureProcess fixture) (startupApplicationState deferred))
  assertBool
    "membership deferral changes no Controlled owner state"
    (startupControlledState deferred == startupControlledState gap)
  assertBool
    "membership deferral changes no Publication owner state"
    (startupPublicationState deferred == startupPublicationState gap)
  assertEqual
    "membership deferral retains one pre-acceptance request candidate"
    1
    ( length
        ( Application.applicationEnvironmentRequestEntries
            (fixtureSession fixture)
            (startupApplicationState deferred)
        )
    )
  let redrive = runEnvironment fixtureRequest (fixtureBinding fixture) deferred
  (redriven, redriveOutcome) <- checkedIO "redrive exact membership gap" redrive
  assertEqual
    "exact membership gap remains deferred"
    NewEnvironment.NewEnvironmentDeferred
    redriveOutcome
  assertBool "repeated membership deferral is state-identical" (redriven == deferred)

caseInvalidMembershipGapFaults :: Assertion
caseInvalidMembershipGapFaults = do
  let exactGap = exactMembershipGapState 7
      withoutCoordinator =
        replaceStartupStructuralBaseCoordinator Nothing exactGap
      unrelated =
        replaceOracleMembership
          (membershipSuccessor 8)
          exactGap
  assertMembershipGapFault "missing coordinator" withoutCoordinator
  assertMembershipGapFault "unrelated coordinator successor" unrelated

assertMembershipGapFault :: String -> HeraldState -> Assertion
assertMembershipGapFault context state = do
  let attempt = runEnvironment fixtureRequest (fixtureBinding fixture) state
      rolledBack = either (const state) fst attempt
  case attempt of
    Left NewEnvironment.NewEnvironmentInvariantFault -> pure ()
    Left problem ->
      assertFailure (context <> " produced the wrong failure: " <> show problem)
    Right _ -> assertFailure (context <> " was treated as a transient membership gap")
  assertBool (context <> " exposes no successor state") (rolledBack == state)
  assertEqual
    (context <> " consumes no generated identity")
    (generatorCounter state)
    (generatorCounter rolledBack)
  assertEqual
    (context <> " consumes no process position")
    (nextProcessPosition (fixtureProcess fixture) (startupApplicationState state))
    (nextProcessPosition (fixtureProcess fixture) (startupApplicationState rolledBack))

propDeterministicSchedule :: Positive Int -> Property
propDeterministicSchedule (Positive suppliedCount) =
  let count = 1 + suppliedCount `mod` 6
      predecessor = fixtureState fixture
      first = runSchedule count predecessor
      second = runSchedule count predecessor
   in case first of
        Left problem -> counterexample problem False
        Right (successor, manifests) ->
          counterexample "same pure schedule produced a different result" (first == second)
            .&&. counterexample
              "schedule retained the wrong manifest count"
              (length manifests == count)
            .&&. counterexample
              "schedule advanced the generator by the wrong amount"
              ( generatorCounter successor
                  == generatorCounter predecessor + 31 * fromIntegral count
              )
            .&&. counterexample
              "schedule advanced the process supply by the wrong amount"
              ( nextProcessPosition (fixtureProcess fixture) (startupApplicationState successor)
                  == fmap
                    (+ fromIntegral count)
                    ( nextProcessPosition
                        (fixtureProcess fixture)
                        (startupApplicationState predecessor)
                    )
              )

runSchedule ::
  Int ->
  HeraldState ->
  Either String (HeraldState, [Environment.EnvironmentManifest])
runSchedule count = go 1 []
  where
    go ordinal reverseManifests state
      | ordinal > count = Right (state, reverse reverseManifests)
      | otherwise = do
          let currentRequest = requestId (fromIntegral ordinal)
          (accepted, outcome) <-
            mapLeft
              show
              (runEnvironment currentRequest (fixtureBinding fixture) state)
          manifest <- case outcome of
            NewEnvironment.NewEnvironmentAccepted retained -> Right retained
            other -> Left ("fresh schedule step was not accepted: " <> show other)
          (retried, retryOutcome) <-
            mapLeft
              show
              (runEnvironment currentRequest (fixtureBinding fixture) accepted)
          if retried /= accepted
            then Left "schedule exact retry changed state"
            else case retryOutcome of
              NewEnvironment.NewEnvironmentRetried retained
                | retained == manifest ->
                    go (ordinal + 1) (manifest : reverseManifests) retried
              other -> Left ("schedule retry did not retain its manifest: " <> show other)

data Fixture = Fixture
  { fixtureState :: HeraldState,
    fixtureProcess :: ProcessEpochId,
    fixtureAttachment :: ApplicationAttachment,
    fixtureSession :: ApplicationSessionId,
    fixtureBinding :: ApplicationSessionBinding,
    fixtureResumeToken :: ApplicationResumeToken,
    fixtureCursor :: ApplicationReplyCursor
  }

fixture :: Fixture
fixture =
  Fixture
    { fixtureState = opened,
      fixtureProcess = process,
      fixtureAttachment = attachment,
      fixtureSession = session,
      fixtureBinding = binding,
      fixtureResumeToken = resumeToken,
      fixtureCursor = cursor
    }
  where
    bootstrap = case fixtureLocalBootstrapIds of
      first : _ -> first
      [] -> error "newenv fixture has no local bootstrap"
    bootstraps =
      checkedPure
        "check newenv fixture bootstrap"
        ( checkInitialBootstraps
            fixtureCheckedGenesis
            (PrimordialProcessManifest [bootstrap])
        )
    (initial, _) =
      checkedPure
        "initialize newenv fixture Herald"
        ( initialHerald
            (monotonicInstant 0)
            fixtureCheckedGenesis
            bootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    attachment =
      case primordialApplicationAttachment fixtureCheckedGenesis bootstraps bootstrap of
        Just admitted -> admitted
        Nothing -> error "newenv fixture attachment is unavailable"
    initialApplication = startupApplicationState initial
    process =
      case Application.applicationAttachmentProcess attachment initialApplication of
        Just admitted -> admitted
        Nothing -> error "newenv fixture attachment has no process"
    (openedApplication, acceptance) =
      Application.commitApplicationSessionAcceptance
        ( checkedPure
            "open newenv fixture session"
            ( Application.prepareApplicationSessionOpen
                (checkedLocalHeraldEpoch fixtureCheckedGenesis)
                attachment
                (clientNonce 1)
                initialApplication
            )
        )
    opened = replaceStartupApplicationState openedApplication initial
    binding = sessionAcceptanceBinding acceptance
    cursor = sessionAcceptanceCursor acceptance
    (session, resumeToken) = case sessionAcceptanceReply acceptance of
      SessionOpened openedSession token _ -> (openedSession, token)
      reply -> error ("expected opened newenv fixture session, got " <> show reply)

fixtureRequest :: RequestId
fixtureRequest = requestId 1

exactMembershipGapState :: Word64 -> HeraldState
exactMembershipGapState retirementIndex =
  replaceStartupStructuralBaseCoordinator (Just coordinator)
    . replaceOracleMembership successor
    $ fixtureState fixture
  where
    successor = membershipSuccessor retirementIndex
    progress = startupStructuralProgressState (fixtureState fixture)
    predecessorBase =
      checkedPure
        "derive newenv membership-gap predecessor base"
        ( terminalSourceGenesisPredecessorBase
            fixtureHeraldMembershipGeneration
            (GraphProgress.structuralLastInstalledCutId progress)
        )
    coordinator =
      checkedPure
        "begin newenv membership-gap coordinator"
        ( StructuralBase.beginMembershipBaseClosure
            (fixtureGenesisRetirementLineage fixtureHeraldMembershipGeneration successor)
            (fixtureGenesisRetirementLineage fixtureHeraldMembershipGeneration successor)
            (checkedLocalHeraldEpoch fixtureCheckedGenesis)
            predecessorBase
            []
            []
            []
        )

membershipSuccessor :: Word64 -> HeraldMembershipGeneration
membershipSuccessor retirementIndex =
  checkedPure
    "derive newenv fixture membership successor"
    ( retireHeraldMembershipGeneration
        (controlIndex retirementIndex)
        (fixtureRetirementResolution (controlIndex retirementIndex))
        (heraldMemberEpoch fixtureRemoteMember)
        fixtureHeraldMembershipGeneration
    )

replaceOracleMembership :: HeraldMembershipGeneration -> HeraldState -> HeraldState
replaceOracleMembership membership state =
  replaceStartupOracleProjectionState
    ( OracleProjection.replaceCurrentHeraldMembershipForInvariantTest
        membership
        (startupOracleProjectionState state)
    )
    state

runEnvironment ::
  RequestId ->
  ApplicationSessionBinding ->
  HeraldState ->
  Either
    NewEnvironment.NewEnvironmentFailure
    (HeraldState, NewEnvironment.NewEnvironmentOutcome)
runEnvironment request binding =
  NewEnvironment.planNewEnvironment
    (fixtureSession fixture)
    binding
    request
    Application.newEnvironmentCoordinatorInput

acceptEnvironment ::
  RequestId ->
  Fixture ->
  HeraldState ->
  (HeraldState, Environment.EnvironmentManifest)
acceptEnvironment request environmentFixture predecessor =
  case NewEnvironment.planNewEnvironment
    (fixtureSession environmentFixture)
    (fixtureBinding environmentFixture)
    request
    Application.newEnvironmentCoordinatorInput
    predecessor of
    Right (successor, NewEnvironment.NewEnvironmentAccepted manifest) ->
      (successor, manifest)
    Right (_, outcome) -> error ("expected accepted newenv, got " <> show outcome)
    Left problem -> error ("accept newenv: " <> show problem)

occupyOrdinaryRequest :: Fixture -> Application.State -> Application.State
occupyOrdinaryRequest environmentFixture state =
  case Application.classifyApplicationRequest
    (fixtureSession environmentFixture)
    (fixtureBinding environmentFixture)
    fixtureRequest
    (NewIdApplication BareNewId)
    state of
    Right (Application.FirstApplicationRequest candidate) ->
      fst
        ( Application.commitApplicationRequest
            ( Application.prepareApplicationRequestCompletion
                candidate
                ( NewIdCompleted
                    (checkedPure "conflict private identity" (mkPrivateUniqueId 777))
                )
            )
        )
    Right _ -> error "ordinary conflict fixture request was not first"
    Left problem -> error ("ordinary conflict fixture: " <> show problem)

retireProcess :: ProcessEpochId -> HeraldState -> HeraldState
retireProcess process state =
  replaceStartupControlledState
    retiredControlled
    (replaceStartupApplicationState retiredApplication state)
  where
    (retiredApplication, _) =
      Application.commitApplicationProcessRetirement
        ( Application.prepareApplicationProcessRetirement
            process
            (startupApplicationState state)
        )
    retiredControlled =
      Controlled.commitControlledProcessRetirement
        ( Controlled.prepareControlledProcessRetirement
            process
            (startupControlledState state)
        )

assertDetachedManifest ::
  String ->
  Environment.EnvironmentManifest ->
  HeraldState ->
  Assertion
assertDetachedManifest context expected state =
  case Application.applicationPendingEnvironmentEntries (startupApplicationState state) of
    [(_, pending)] -> do
      assertEqual
        (context <> " retains the exact manifest")
        expected
        (Environment.pendingEnvironmentManifest pending)
      assertEqual
        (context <> " detaches the reply key")
        Nothing
        (Environment.pendingEnvironmentReplyKey pending)
    entries ->
      assertFailure
        (context <> " retained " <> show (length entries) <> " pending manifests")

pendingManifests :: Application.State -> [Environment.EnvironmentManifest]
pendingManifests =
  fmap
    (Environment.pendingEnvironmentManifest . snd)
    . Application.applicationPendingEnvironmentEntries

pendingEnvironmentReplyKeys ::
  [(position, Environment.PendingEnvironment)] ->
  [Maybe Environment.EnvironmentReplyKey]
pendingEnvironmentReplyKeys =
  fmap (Environment.pendingEnvironmentReplyKey . snd)

nextProcessPosition :: ProcessEpochId -> Application.State -> Maybe Word64
nextProcessPosition process =
  lookup process
    . Application.applicationRequestNextAcceptancePositions
    . Application.applicationRequestStateWitness

generatorCounter :: HeraldState -> Word64
generatorCounter state =
  case IdGenerator.idGeneratorStateWitness (startupIdGeneratorState state) of
    IdGenerator.IdGeneratorStateWitness counter -> counter

publicationNextPosition :: HeraldState -> Word64
publicationNextPosition =
  Publication.heraldPublicationPositionWord64
    . Publication.publicationWitnessNextHeraldPosition
    . Publication.publicationStateWitness
    . startupPublicationState

assertSourceTenuresAdvance ::
  Publication.State ->
  Publication.State ->
  [Environment.PositionedEnvironmentRoot] ->
  Assertion
assertSourceTenuresAdvance before after roots = do
  assertEqual "environment uses exactly two source tenures" 2 (Set.size tenures)
  mapM_ assertTenure (Set.toAscList tenures)
  where
    beforeNext =
      Map.fromList
        ( Publication.publicationWitnessNextSequences
            (Publication.publicationStateWitness before)
        )
    afterNext =
      Map.fromList
        ( Publication.publicationWitnessNextSequences
            (Publication.publicationStateWitness after)
        )
    identifiers = Environment.positionedEnvironmentRootPublicationId <$> roots
    tenureOf identifier =
      (publicationNabla identifier, publicationAuthorityEpoch identifier)
    tenures = Set.fromList (tenureOf <$> identifiers)
    assertTenure tenure = do
      let initial = maybe 1 nablaSequenceWord64 (Map.lookup tenure beforeNext)
          retained =
            sort
              [ nablaSequenceWord64 (publicationNablaSequence identifier)
              | identifier <- identifiers,
                tenureOf identifier == tenure
              ]
      assertEqual
        ("six consecutive source publications for " <> show tenure)
        [initial .. initial + 5]
        retained
      assertEqual
        ("source successor advances by six for " <> show tenure)
        (Just (initial + 6))
        (nablaSequenceWord64 <$> Map.lookup tenure afterNext)

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

assertValidHerald :: String -> HeraldState -> Assertion
assertValidHerald context =
  either
    (assertFailure . ((context <> " violates the whole-state invariant: ") <>) . show)
    pure
    . validateHeraldState

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft convert = either (Left . convert) Right
