module AdministrationRpcProperties
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.Either (isRight)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    HeraldEpoch,
    ProcessEpochId,
    ProcessId,
    SystemId,
    controlIndex,
    heraldEpochBytes,
    mkProcessEpochId,
    mkProcessId,
    processEpochIdBytes,
    processIdBytes,
  )
import Eclips.Domain.Membership (mkHeraldMembershipGenerationId)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.Administration.Internal (drainId)
import Eclips.Herald.Administration.RPC
  ( AdministrationIngressContext (..),
    AdministrationOutbound (..),
    AdministrationRpcAdmissionError (AdministrationRpcWrongIngressPhase),
    admitAdministrationClientDto,
    projectAdministrationEffect,
    projectFinalAdministrationReply,
  )
import Eclips.Herald.Administration.RPC.Internal qualified as Rpc
import Eclips.Herald.Administration.State qualified as AdministrationState
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    HeraldEffect (..),
  )
import Eclips.Herald.Genesis.Internal (checkedSystemId)
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    HeraldInputBody (AdministrationInput),
    candidateAdministrationLane,
  )
import Eclips.Oracle.Receipt qualified as Oracle
import Eclips.Protocol.Admin.Types qualified as Protocol
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureRemoteMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Start/End administration RPC adapter"
    [ testCase "deployment, manifest, and process claims round-trip nominally" caseClaimRoundTrip,
      testCase "AdminHello and established calls enforce the complete phase matrix" caseIngressMatrix,
      testCase "every retained result and disposition projects exactly" caseEffectProjection,
      testCase "operator status, preparation phases, and final drain use exact typed DTOs" caseOperatorProjection,
      testCase "preparation cancellation retains one admin correlation across command kinds" caseCancellation
    ]

caseClaimRoundTrip :: Assertion
caseClaimRoundTrip = do
  assertRightEqual
    (Rpc.deploymentToClaim fixtureSystem >>= Rpc.deploymentFromClaim)
    fixtureSystem
  assertRightEqual
    (Rpc.processEpochToClaim fixtureProcessEpoch >>= Rpc.processEpochFromClaim)
    fixtureProcessEpoch
  assertEqual
    "correlation representation is exact"
    fixtureCorrelation
    (Administration.adminCorrelationIdForBinding fixtureBinding (Administration.adminCorrelationIdWord64 (Rpc.correlationFromClaim (Rpc.correlationToClaim fixtureCorrelation))))

caseIngressMatrix :: Assertion
caseIngressMatrix = do
  let lane = candidateAdministrationLane 7
      candidate = CandidateAdministrationIngress lane
      established = EstablishedAdministrationIngress fixtureBinding
  assertAccepted candidate helloDto
  assertRejected candidate startDto
  assertRejected candidate endDto
  assertRejected candidate getDto
  assertRejected established helloDto
  assertAccepted established startDto
  assertAccepted established endDto
  assertAccepted established getDto
  forM_ [Protocol.GetHeraldStatus fixtureCorrelationClaim, Protocol.ListChildPreparations fixtureCorrelationClaim, Protocol.DrainHerald fixtureCorrelationClaim] $ \dto -> do
    assertRejected candidate dto
    assertAccepted established dto
  forM_
    [ (Protocol.GetHeraldStatus fixtureCorrelationClaim, GetHeraldStatus fixtureBinding fixtureCorrelation),
      (Protocol.ListChildPreparations fixtureCorrelationClaim, ListChildPreparations fixtureBinding fixtureCorrelation),
      (Protocol.DrainHerald fixtureCorrelationClaim, DrainConfiguredHerald fixtureBinding fixtureCorrelation)
    ]
    $ \(dto, ingress) ->
      assertEqual "operator correlation and established binding" (Right (AdministrationInput ingress)) (admitAdministrationClientDto established dto)
  assertEqual
    "AdminHello carries the checked deployment into typed candidate ingress"
    ( Right
        ( AdministrationInput
            (OpenAdministrationConnection lane fixtureSystem)
        )
    )
    (admitAdministrationClientDto candidate helloDto)
  assertEqual
    "Start carries the exact binding, correlation, and manifest"
    ( Right
        ( AdministrationInput
            ( StartProcessEpoch
                fixtureBinding
                ( Administration.startProcessRequest
                    fixtureCorrelation
                )
            )
        )
    )
    (admitAdministrationClientDto established startDto)
  assertEqual
    "End carries the exact binding, correlation, process epoch, and explicit reason"
    ( Right
        ( AdministrationInput
            ( EndProcessEpoch
                fixtureBinding
                ( Administration.endProcessEpochRequest
                    fixtureCorrelation
                    fixtureProcessEpoch
                    ExplicitAdministrativeEnd
                )
            )
        )
    )
    (admitAdministrationClientDto established endDto)
  assertEqual
    "Get carries the exact binding and correlation"
    ( Right
        ( AdministrationInput
            (GetAdministrationResult fixtureBinding fixtureCorrelation)
        )
    )
    (admitAdministrationClientDto established getDto)
  where
    assertAccepted context dto =
      assertBool
        "expected phase admission"
        (isRight (admitAdministrationClientDto context dto))
    assertRejected context dto =
      assertEqual
        "wrong phase rejects before kernel ingress"
        (Left AdministrationRpcWrongIngressPhase)
        (admitAdministrationClientDto context dto)

caseEffectProjection :: Assertion
caseEffectProjection = do
  let lane = candidateAdministrationLane 7
  assertEqual
    "candidate acceptance carries its binding outside the DTO vocabulary"
    (Right (Just (AcceptAdministrationCandidate lane fixtureBinding)))
    ( projectAdministrationEffect
        (SetAdministrationConnectionDisposition lane fixtureBinding)
    )
  assertEqual
    "candidate rejection retains only runtime lane metadata"
    (Right (Just (RejectAdministrationCandidate lane)))
    ( projectAdministrationEffect
        ( RejectAdministrationConnection
            (CandidateAdministrationDisposition lane)
        )
    )
  assertEqual
    "established rejection retains only the owner binding"
    (Right (Just (RejectEstablishedAdministration fixtureBinding)))
    ( projectAdministrationEffect
        ( RejectAdministrationConnection
            (EstablishedAdministrationDisposition fixtureBinding)
        )
    )
  mapM_
    ( \(reply, dto) ->
        assertEqual
          "retained administration reply projection"
          ( Right
              ( Just
                  (SendEstablishedAdministration fixtureBinding dto)
              )
          )
          ( projectAdministrationEffect
              (SendAdministrationReply fixtureBinding reply)
          )
    )
    replyCases
  assertEqual
    "non-administration effects are outside the adapter"
    (Right Nothing)
    (projectAdministrationEffect (BeginDrain (drainId 1)))

caseOperatorProjection :: Assertion
caseOperatorProjection = do
  let generation = checked (mkHeraldMembershipGenerationId (fixtureIdentifierBytes 83))
      generationClaim = checked (Protocol.adminMembershipGenerationClaim (fixtureIdentifierBytes 83))
      phases =
        [ (Administration.AdminServing, Protocol.AdminServingDto),
          (Administration.AdminDraining, Protocol.AdminDrainingDto),
          (Administration.AdminStopped, Protocol.AdminStoppedDto),
          (Administration.AdminIsolationDrain, Protocol.AdminIsolationDrainDto),
          (Administration.AdminIsolated, Protocol.AdminIsolatedDto)
        ]
  forM_ phases $ \(phase, dtoPhase) -> do
    let snapshot = Administration.AdminHeraldStatus fixtureSystem fixtureLocalEpoch [fixtureLocalEpoch, fixtureRemoteEpoch] [fixtureRemoteEpoch] generation (controlIndex 7) phase Nothing Nothing
        dto = Protocol.AdminHeraldStatusDto (checked (Rpc.deploymentToClaim fixtureSystem)) fixtureLocalEpochClaim [fixtureLocalEpochClaim, fixtureRemoteEpochClaim] [fixtureRemoteEpochClaim] generationClaim (Protocol.adminControlIndexDto 7) dtoPhase Nothing Nothing
    assertProjected (Administration.AdminHeraldStatusReply fixtureCorrelation snapshot) (Protocol.HeraldStatus fixtureCorrelationClaim dto)
  let reference = checked (Lifecycle.childPreparation (fixtureIdentifierBytes 84) 2 9)
      preparationPhases =
        [ (Administration.AdminPreparationPreparing, Protocol.AdminPreparationPreparingDto),
          (Administration.AdminPreparationPrepared, Protocol.AdminPreparationPreparedDto),
          (Administration.AdminPreparationAttached, Protocol.AdminPreparationAttachedDto),
          (Administration.AdminPreparationCancelling, Protocol.AdminPreparationCancellingDto),
          (Administration.AdminPreparationTerminal, Protocol.AdminPreparationTerminalDto)
        ]
  forM_ preparationPhases $ \(phase, dtoPhase) ->
    assertProjected
      (Administration.AdminPreparationsReply fixtureCorrelation [Administration.AdminPreparationInspection reference (Just fixtureProcessEpoch) phase (Just Lifecycle.StartupNotLive)])
      (Protocol.ChildPreparations fixtureCorrelationClaim [Protocol.AdminPreparationDto reference (Just fixtureProcessEpochClaim) dtoPhase (Just Lifecycle.StartupNotLive)])
  assertProjected (Administration.AdminDrainAccepted fixtureCorrelation) (Protocol.HeraldDrainAccepted fixtureCorrelationClaim)
  assertEqual
    "public final drain uses exact binding and correlation"
    (Just (SendEstablishedAdministration fixtureBinding (Protocol.HeraldDrained fixtureCorrelationClaim)))
    (projectFinalAdministrationReply (Administration.ConfiguredHeraldShutdownCompleted fixtureBinding fixtureCorrelation))
  assertEqual
    "private drain lease has no EADM projection"
    Nothing
    (projectFinalAdministrationReply (Administration.OrderlyHeraldShutdownCompleted fixtureCorrelation))
  where
    assertProjected reply dto = assertEqual "operator reply projection" (Right (Just (SendEstablishedAdministration fixtureBinding dto))) (projectAdministrationEffect (SendAdministrationReply fixtureBinding reply))

replyCases :: [(Administration.AdminReply, Protocol.AdminServerDto)]
replyCases =
  [ retained Administration.AdminRequestAccepted Protocol.AdminAccepted,
    retained
      (Administration.AdminStartRequestCompleted fixtureProcessEpoch fixtureAttachment)
      ( Protocol.AdminCompleted
          (Protocol.StartProcessReadyDto fixtureProcessEpochClaim fixtureAttachmentClaim)
      ),
    retained
      (Administration.AdminStartRequestEndedBeforeAttachment fixtureProcessEpoch)
      ( Protocol.AdminCompleted
          (Protocol.StartProcessEndedBeforeAttachmentDto fixtureProcessEpochClaim)
      ),
    retained
      (Administration.AdminEndRequestCompleted fixtureProcessEpoch)
      ( Protocol.AdminCompleted
          (Protocol.ProcessEpochEndedDto fixtureProcessEpochClaim)
      ),
    oracleRejected
      (Oracle.RequestHomeEpochMismatch fixtureRemoteEpoch fixtureLocalEpoch)
      ( Protocol.AdminStartRequestHomeEpochMismatchDto
          fixtureRemoteEpochClaim
          fixtureLocalEpochClaim
      ),
    oracleRejected
      (Oracle.InactiveHomeHerald fixtureRemoteEpoch)
      (Protocol.AdminStartInactiveHomeHeraldDto fixtureRemoteEpochClaim),
    oracleRejected
      (Oracle.StaleExpectedControlIndex (controlIndex 4) (controlIndex 5))
      ( Protocol.AdminStaleExpectedControlIndexDto
          (Protocol.adminControlIndexDto 4)
          (Protocol.adminControlIndexDto 5)
      ),
    oracleRejected
      ( Oracle.StartProcessResidenceMismatch
          fixtureProcessEpoch
          fixtureRemoteEpoch
          fixtureLocalEpoch
      )
      ( Protocol.AdminStartProcessResidenceMismatchDto
          fixtureProcessEpochClaim
          fixtureRemoteEpochClaim
          fixtureLocalEpochClaim
      ),
    oracleRejected
      (Oracle.ProcessEpochAlreadyStarted fixtureProcessEpoch)
      (Protocol.AdminProcessEpochAlreadyStartedDto fixtureProcessEpochClaim),
    oracleRejected
      (Oracle.ProcessAlreadyStarted fixtureProcess)
      (Protocol.AdminProcessAlreadyStartedDto fixtureProcessClaim),
    endOracleRejected
      (Oracle.RequestHomeEpochMismatch fixtureRemoteEpoch fixtureLocalEpoch)
      ( Protocol.AdminEndRequestHomeEpochMismatchDto
          fixtureRemoteEpochClaim
          fixtureLocalEpochClaim
      ),
    endOracleRejected
      (Oracle.InactiveHomeHerald fixtureRemoteEpoch)
      (Protocol.AdminEndInactiveHomeHeraldDto fixtureRemoteEpochClaim),
    endOracleRejected
      (Oracle.EndProcessUnknown fixtureProcessEpoch)
      (Protocol.AdminEndProcessUnknownDto fixtureProcessEpochClaim),
    endOracleRejected
      ( Oracle.EndProcessResidenceMismatch
          fixtureProcessEpoch
          fixtureRemoteEpoch
          fixtureLocalEpoch
      )
      ( Protocol.AdminEndProcessResidenceMismatchDto
          fixtureProcessEpochClaim
          fixtureRemoteEpochClaim
          fixtureLocalEpochClaim
      ),
    endOracleRejected
      (Oracle.EndProcessAlreadyEnded fixtureProcessEpoch)
      (Protocol.AdminEndProcessAlreadyEndedDto fixtureProcessEpochClaim),
    ( Administration.AbsentAdminResult fixtureCorrelation,
      Protocol.AdminAbsent fixtureCorrelationClaim
    ),
    ( Administration.ConflictingAdminResult fixtureCorrelation,
      Protocol.AdminConflict fixtureCorrelationClaim
    )
  ]
  where
    retained status dto =
      ( Administration.RetainedAdminResult fixtureCorrelation status,
        Protocol.AdminResult fixtureCorrelationClaim dto
      )
    retainedRejected owner dto =
      retained
        (Administration.AdminRequestRejected owner)
        (Protocol.AdminRejected dto)
    oracleRejected rejection dto =
      retainedRejected
        ( Administration.AdminStartProcessNotApplied
            (controlIndex 99)
            rejection
        )
        (Protocol.StartProcessNotApplied dto)
    endOracleRejected rejection dto =
      retainedRejected
        ( Administration.AdminEndProcessNotApplied
            (controlIndex 99)
            rejection
        )
        (Protocol.EndProcessNotApplied dto)

fixtureBinding :: Administration.AdministrationBinding
fixtureBinding =
  snd
    ( AdministrationState.commitConfiguredAdministrationOpen
        ( checked
            ( AdministrationState.prepareConfiguredAdministrationOpen
                (AdministrationState.initialState fixtureLocalEpoch)
            )
        )
    )

fixtureCorrelation :: Administration.AdminCorrelationId
fixtureCorrelation = Administration.adminCorrelationIdForBinding fixtureBinding 91

fixtureCorrelationClaim :: Protocol.AdminCorrelationIdClaim
fixtureCorrelationClaim = Protocol.adminCorrelationIdClaim 91

fixtureSystem :: SystemId
fixtureSystem = checkedSystemId fixtureCheckedGenesis

fixtureManifest :: BootstrapManifestId
fixtureManifest = case fixtureLocalBootstrapIds of
  manifest : _ -> manifest
  [] -> error "fixture local bootstrap list is empty"

fixtureLocalEpoch :: HeraldEpoch
fixtureLocalEpoch = heraldMemberEpoch fixtureLocalMember

fixtureRemoteEpoch :: HeraldEpoch
fixtureRemoteEpoch = heraldMemberEpoch fixtureRemoteMember

fixtureProcessEpoch :: ProcessEpochId
fixtureProcessEpoch = checked (mkProcessEpochId (fixtureIdentifierBytes 30))

fixtureProcess :: ProcessId
fixtureProcess = checked (mkProcessId (fixtureIdentifierBytes 20))

fixtureAttachment :: Session.ApplicationAttachment
fixtureAttachment = Session.applicationAttachmentForBootstrap fixtureManifest

helloDto :: Protocol.AdminClientDto
helloDto =
  Protocol.AdminHello
    (checked (Rpc.deploymentToClaim fixtureSystem))
    Protocol.ProcessAdministrator

startDto :: Protocol.AdminClientDto
startDto =
  Protocol.StartProcessEpoch fixtureCorrelationClaim

endDto :: Protocol.AdminClientDto
endDto =
  Protocol.EndProcessEpoch
    fixtureCorrelationClaim
    fixtureProcessEpochClaim
    Protocol.AdminExplicitAdministrativeEnd

getDto :: Protocol.AdminClientDto
getDto = Protocol.GetAdminResult fixtureCorrelationClaim

fixtureProcessEpochClaim :: Protocol.AdminProcessEpochIdClaim
fixtureProcessEpochClaim =
  checked
    (Protocol.adminProcessEpochIdClaim (processEpochIdBytes fixtureProcessEpoch))

fixtureProcessClaim :: Protocol.AdminProcessIdClaim
fixtureProcessClaim =
  checked (Protocol.adminProcessIdClaim (processIdBytes fixtureProcess))

fixtureLocalEpochClaim :: Protocol.AdminHeraldEpochClaim
fixtureLocalEpochClaim =
  checked (Protocol.adminHeraldEpochClaim (heraldEpochBytes fixtureLocalEpoch))

fixtureRemoteEpochClaim :: Protocol.AdminHeraldEpochClaim
fixtureRemoteEpochClaim =
  checked (Protocol.adminHeraldEpochClaim (heraldEpochBytes fixtureRemoteEpoch))

fixtureAttachmentClaim :: Protocol.AdminApplicationAttachmentClaim
fixtureAttachmentClaim =
  checked
    ( Protocol.adminApplicationAttachmentClaim
        (Session.applicationAttachmentBytes fixtureAttachment)
    )

assertRightEqual :: (Eq value, Show value) => Either problem value -> value -> Assertion
assertRightEqual result expected = case result of
  Left _ -> error "expected successful claim round-trip"
  Right actual -> assertEqual "claim round-trip" expected actual

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

caseCancellation :: Assertion
caseCancellation = do
  let reference = checked (Lifecycle.childPreparation (fixtureIdentifierBytes 49) 3 1)
      other = checked (Lifecycle.childPreparation (fixtureIdentifierBytes 49) 3 2)
      dto = Protocol.CancelChildPreparation (Rpc.correlationToClaim fixtureCorrelation) reference
      status = Lifecycle.LifecyclePending []
      result = Administration.AdminPreparationCancellation status
      empty = AdministrationState.initialState fixtureLocalEpoch
      (retained, first) = AdministrationState.commitPreparationCancellation (checked (AdministrationState.preparePreparationCancellation fixtureCorrelation reference status empty))
      (retried, repeated) = AdministrationState.commitPreparationCancellation (checked (AdministrationState.preparePreparationCancellation fixtureCorrelation reference (Lifecycle.LifecycleCompleted Lifecycle.ChildCancelled) retained))
      (_, conflict) = AdministrationState.commitPreparationCancellation (checked (AdministrationState.preparePreparationCancellation fixtureCorrelation other status retained))
      settled = checked (AdministrationState.settlePreparationCancellation fixtureCorrelation (Lifecycle.LifecycleCompleted Lifecycle.ChildCancelled) retained)
  assertEqual "candidate cannot cancel before admin role admission" (Left AdministrationRpcWrongIngressPhase) (admitAdministrationClientDto (CandidateAdministrationIngress (candidateAdministrationLane 33)) dto)
  assertEqual
    "authorized cancellation carries exact opaque reference"
    (Right (AdministrationInput (CancelChildPreparation fixtureBinding fixtureCorrelation reference)))
    (admitAdministrationClientDto (EstablishedAdministrationIngress fixtureBinding) dto)
  assertEqual "first cancellation result" (Administration.RetainedAdminResult fixtureCorrelation result) first
  assertEqual "exact retry cannot supply its own terminal settlement" first repeated
  assertBool "exact retry retains unchanged owner" (retained == retried)
  assertEqual "different preparation conflicts at same correlation" (Administration.ConflictingAdminResult fixtureCorrelation) conflict
  assertEqual "generic admin query sees ordered cancellation completion" (Just (Administration.AdminPreparationCancellation (Lifecycle.LifecycleCompleted Lifecycle.ChildCancelled))) (AdministrationState.lookupAdministrationResultStatus fixtureCorrelation settled)
  assertEqual
    "cancellation result projects unchanged under EADM"
    (Right (Just (SendEstablishedAdministration fixtureBinding (Protocol.AdminResult (Rpc.correlationToClaim fixtureCorrelation) (Protocol.AdminPreparationCancellation status)))))
    (projectAdministrationEffect (SendAdministrationReply fixtureBinding first))
  assertEqual
    "existing cancellation correlation cannot allocate Start"
    (Right False)
    (AdministrationState.configuredStartRequiresOracle fixtureCheckedGenesis (Administration.startProcessRequest fixtureCorrelation) retained)
