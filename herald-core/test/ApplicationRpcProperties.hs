module ApplicationRpcProperties
  ( tests,
  )
where

import Data.Either (isRight)
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
  )
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement)
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewEnvironmentApplication),
  )
import Eclips.Application.Types.Rejection
  ( ApplicationRejection (ApplicationQueryPredicateMismatch),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason
      ( EnvironmentStabilizationPending,
        StructuralStabilizationPending
      ),
    RegularCallResult (NewEnvironmentCompleted, WaitCompleted),
    WaitResult (WaitReady),
  )
import Eclips.Domain.Identity (BootstrapManifestId, HeraldEpoch)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Administration.Internal (drainId)
import Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (..),
    ApplicationOutbound (..),
    ApplicationRpcAdmissionError (ApplicationRpcWrongIngressPhase),
    admitApplicationClientDto,
    projectApplicationEffect,
  )
import Eclips.Herald.Application.RPC.Internal qualified as Rpc
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (..),
    HeraldEffect (..),
  )
import Eclips.Herald.Input
  ( ApplicationLifecycleIngress (..),
    ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    HeraldInputBody (..),
    candidateApplicationLane,
  )
import Eclips.Protocol.Application.Types qualified as Protocol
import GenesisFixtures
  ( fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
  )
import PrimordialTestAccess (conventionalStartupEnvironment)
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
    "application RPC adapter"
    [ testCase "owner and protocol claims round-trip without granting authority" caseClaimRoundTrip,
      testCase "candidate and established ingress enforce the complete phase matrix" caseIngressMatrix,
      testCase "every application effect projects with runtime-only targeting" caseEffectProjection,
      testCase "initial claims and lifecycle requests retain target and reply scope" caseLifecycle
    ]

caseClaimRoundTrip :: Assertion
caseClaimRoundTrip = do
  let attachment = Session.applicationAttachmentForBootstrap fixtureManifest
      (session, token, _) = fixtureSession
      request = Request.requestId 9
      wait = Request.waitIdForSessionParts fixtureEpoch 7 request
      cursor = Request.firstApplicationReplyCursor
      nonce = Session.clientNonce 42
  assertRightEqual
    (Rpc.attachmentToClaim attachment >>= Rpc.attachmentFromClaim)
    attachment
  assertRightEqual
    (Rpc.sessionToClaim session >>= Rpc.sessionFromClaim)
    session
  assertRightEqual
    (Rpc.resumeTokenToClaim token >>= Rpc.resumeTokenFromClaim)
    token
  assertEqual "request representation is exact" request (Rpc.requestFromClaim (Rpc.requestToClaim request))
  assertRightEqual
    (Rpc.waitToClaim wait >>= Rpc.waitFromClaim)
    wait
  assertRightEqual
    (Rpc.cursorToClaim cursor >>= Rpc.cursorFromClaim)
    cursor
  assertEqual "nonce representation is exact" nonce (Rpc.nonceFromClaim (Rpc.nonceToClaim nonce))

caseIngressMatrix :: Assertion
caseIngressMatrix = do
  let lane = candidateApplicationLane 5
      (_, _, binding) = fixtureSession
      candidate = CandidateApplicationIngress lane
      established = EstablishedApplicationIngress binding
  assertAccepted candidate openDto
  assertAccepted candidate resumeDto
  mapM_ (assertRejected candidate) establishedDtos
  mapM_ (assertAccepted established) establishedDtos
  assertRejected established openDto
  assertRejected established resumeDto
  assertEqual
    "decoded operation reaches the exact typed kernel ingress"
    ( Right
        ( ApplicationRequestInput
            (CallApplicationRequest binding fixtureSessionId fixtureRequest fixtureOperation)
        )
    )
    (admitApplicationClientDto established callDto)
  where
    assertAccepted context dto =
      assertBool "expected phase admission" (isRight (admitApplicationClientDto context dto))
    assertRejected context dto =
      assertEqual
        "wrong phase rejects before kernel ingress"
        (Left ApplicationRpcWrongIngressPhase)
        (admitApplicationClientDto context dto)

caseEffectProjection :: Assertion
caseEffectProjection = do
  let lane = candidateApplicationLane 5
      (session, token, binding) = fixtureSession
      resumedAcceptance =
        Session.applicationSessionAcceptance
          binding
          Request.firstApplicationReplyCursor
          (Session.SessionResumed session fixtureSummary)
      openedAcceptance =
        Session.applicationSessionAcceptance
          binding
          Request.firstApplicationReplyCursor
          (Session.SessionOpened session token fixtureStartupAccess)
      wakeDto =
        checked
          ( Rpc.waitWakeToDto
              binding
              Request.firstApplicationReplyCursor
              fixtureRequest
              fixtureWait
              WaitReady
          )
  mapM_
    ( \acceptance ->
        let dto = checked (Rpc.sessionAcceptanceToDto acceptance)
         in assertEqual
              "candidate acceptance carries binding outside every DTO arm"
              (Right (Just (AcceptApplicationCandidate lane binding dto)))
              (projectApplicationEffect (SetApplicationConnectionDisposition lane acceptance))
    )
    [openedAcceptance, resumedAcceptance]
  mapM_
    ( \(rejection, rejectionDto) -> do
        let dto = Protocol.SessionRejected rejectionDto
        assertEqual
          "candidate rejection remains candidate-targeted"
          (Right (Just (RejectApplicationCandidate lane dto)))
          ( projectApplicationEffect
              (RejectApplicationConnection (CandidateApplicationDisposition lane) rejection)
          )
        assertEqual
          "established rejection remains binding-targeted"
          (Right (Just (RejectEstablishedApplication binding dto)))
          ( projectApplicationEffect
              (RejectApplicationConnection (EstablishedApplicationDisposition binding) rejection)
          )
    )
    sessionRejectionCases
  mapM_
    ( \(reply, dto) ->
        assertEqual
          "every request-reply arm projects exactly"
          (Right (Just (SendEstablishedApplication binding dto)))
          (projectApplicationEffect (SendApplicationReply binding reply))
    )
    fixtureReplyCases
  assertEqual
    "wake projections preserve all correlations"
    (Right (Just (SendEstablishedApplication binding wakeDto)))
    ( projectApplicationEffect
        ( SendApplicationWaitWake
            binding
            Request.firstApplicationReplyCursor
            fixtureRequest
            fixtureWait
            WaitReady
        )
    )
  let terminalDto =
        Protocol.HeraldPermanentlyUnavailable
          (checked (Rpc.sessionToClaim session))
          Protocol.ApplicationSessionNoLongerLiveDto
  assertEqual
    "terminal session disposition is session-targeted, not generation-targeted"
    (Right (Just (DisposeEstablishedApplicationSession session terminalDto)))
    ( projectApplicationEffect
        (DisposeApplicationSession session Session.ApplicationSessionNoLongerLive)
    )
  assertEqual
    "non-application effects are outside the adapter"
    (Right Nothing)
    (projectApplicationEffect (BeginDrain (drainId 1)))

fixtureEpoch :: HeraldEpoch
fixtureEpoch = heraldMemberEpoch fixtureLocalMember

fixtureManifest :: BootstrapManifestId
fixtureManifest = case fixtureLocalBootstrapIds of
  manifest : _ -> manifest
  [] -> error "fixture local bootstrap list is empty"

fixtureSession ::
  ( Session.ApplicationSessionId,
    Session.ApplicationResumeToken,
    Session.ApplicationSessionBinding
  )
fixtureSession = Session.initialSessionAllocation fixtureEpoch 7

fixtureSessionId :: Session.ApplicationSessionId
fixtureSessionId =
  let (session, _, _) = fixtureSession
   in session

fixtureRequest :: Request.RequestId
fixtureRequest = Request.requestId 9

fixtureWait :: Request.WaitId
fixtureWait = Request.waitIdForSessionParts fixtureEpoch 7 fixtureRequest

fixtureSummary :: Request.ApplicationRequestSummary
fixtureSummary =
  Request.applicationRequestSummary
    [ Request.applicationRequestSummaryEntry
        (Request.requestId 1)
        (Request.ApplicationRequestPending (waitFor 1)),
      Request.applicationRequestSummaryEntry
        (Request.requestId 2)
        (Request.ApplicationRequestCompleted (WaitCompleted WaitReady)),
      Request.applicationRequestSummaryEntry
        (Request.requestId 3)
        (Request.ApplicationRequestRejected ApplicationQueryPredicateMismatch),
      Request.applicationRequestSummaryEntry
        (Request.requestId 4)
        Request.ApplicationRequestCancelled,
      Request.applicationRequestSummaryEntry
        (Request.requestId 5)
        (Request.ApplicationOperationAccepted StructuralStabilizationPending),
      Request.applicationRequestSummaryEntry
        (Request.requestId 6)
        (Request.ApplicationOperationAccepted EnvironmentStabilizationPending)
    ]

fixtureStartupAccess :: Access.ApplicationStartupAccess
fixtureStartupAccess =
  Access.applicationStartupAccess
    (asPrivateProcessId (privateId 1))
    ( Access.primordialAccessFromSelection
        ( Access.selectEnvironment
            ( checked
                ( Access.environmentAccess
                    [ Access.predefinedAccess
                        role
                        (asPrivateNablaId (privateId writer))
                        (asPrivateDeltaId (privateId reader))
                    | (role, writer, reader) <-
                        zip3 Access.allApplicationPredefinedSortRoles [2 ..] [20 ..]
                    ]
                    (asPrivateObjectId (privateId 1000))
                    [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
                )
            )
        )
    )

privateId :: Word64 -> PrivateUniqueId
privateId = checked . mkPrivateUniqueId

waitFor :: Word64 -> Request.WaitId
waitFor request =
  Request.waitIdForSessionParts fixtureEpoch 7 (Request.requestId request)

sessionRejectionCases ::
  [(Session.ApplicationSessionRejection, Protocol.ApplicationSessionErrorDto)]
sessionRejectionCases =
  [ (Session.ApplicationAttachmentNotAdmitted, Protocol.ApplicationAttachmentNotAdmittedDto),
    (Session.ApplicationSessionNotLive, Protocol.ApplicationSessionNotLiveDto),
    (Session.ApplicationResumeTokenMismatch, Protocol.ApplicationResumeTokenMismatchDto),
    (Session.ApplicationReplyCursorNotIssued, Protocol.ApplicationReplyCursorNotIssuedDto),
    (Session.ApplicationRequestSessionMismatch, Protocol.ApplicationRequestSessionMismatchDto),
    (Session.ApplicationRequestBindingMismatch, Protocol.ApplicationRequestBindingMismatchDto),
    (Session.ApplicationRequestBindingNotLive, Protocol.ApplicationRequestBindingNotLiveDto),
    (Session.ApplicationEndSessionBindingMismatch, Protocol.ApplicationEndSessionBindingMismatchDto),
    (Session.ApplicationReceiptRetirementNotReady, Protocol.ApplicationReceiptRetirementNotReadyDto),
    (Session.ApplicationRequestAlreadyRetired, Protocol.ApplicationRequestAlreadyRetiredDto)
  ]

fixtureReplyCases :: [(Request.ApplicationRequestReply, Protocol.ApplicationServerDto)]
fixtureReplyCases =
  [ retained (Request.WaitAccepted fixtureRequest fixtureWait) (Protocol.WaitAccepted fixtureWaitClaim),
    retained
      (Request.OperationAccepted fixtureRequest StructuralStabilizationPending)
      (Protocol.OperationAccepted StructuralStabilizationPending),
    retained
      (Request.OperationAccepted fixtureRequest EnvironmentStabilizationPending)
      (Protocol.OperationAccepted EnvironmentStabilizationPending),
    retained
      (Request.Completed fixtureRequest (WaitCompleted WaitReady))
      (Protocol.Completed (WaitCompleted WaitReady)),
    retained
      (Request.Completed fixtureRequest (NewEnvironmentCompleted fixtureEnvironmentAccess))
      (Protocol.Completed (NewEnvironmentCompleted fixtureEnvironmentAccess)),
    retained
      (Request.Rejected fixtureRequest ApplicationQueryPredicateMismatch)
      (Protocol.Rejected ApplicationQueryPredicateMismatch),
    retained
      (Request.Cancelled fixtureRequest fixtureWait)
      (Protocol.Cancelled fixtureWaitClaim),
    (Request.RequestAbsent fixtureRequest, Protocol.RequestAbsent fixtureSessionClaim fixtureRequestClaim),
    (Request.RequestConflict fixtureRequest, Protocol.RequestConflict fixtureSessionClaim fixtureRequestClaim),
    (Request.RequestRetired fixtureRequest 9, Protocol.RequestRetired fixtureSessionClaim fixtureRequestClaim 9),
    (Request.ApplicationReceiptsRetired progress, Protocol.ReceiptsRetired fixtureSessionClaim progress)
  ]
  where
    progress = applicationReceiptRetirement (Just 0) (Just 3)
    retained body dtoBody =
      ( Request.RetainedRequestReply Request.firstApplicationReplyCursor body,
        Protocol.RequestRetained
          fixtureSessionClaim
          fixtureCursorClaim
          fixtureRequestClaim
          dtoBody
      )

fixtureOperation :: ApplicationOperation
fixtureOperation = NewEnvironmentApplication

fixtureEnvironmentAccess :: Access.EnvironmentAccess
fixtureEnvironmentAccess =
  conventionalStartupEnvironment fixtureStartupAccess

fixtureAttachmentClaim :: Protocol.ApplicationAttachmentClaim
fixtureAttachmentClaim =
  checked (Protocol.applicationAttachmentClaim (fixtureIdentifierBytes 10))

fixtureSessionClaim :: Protocol.ApplicationSessionClaim
fixtureSessionClaim = checked (Rpc.sessionToClaim fixtureSessionId)

fixtureTokenClaim :: Protocol.ApplicationResumeTokenClaim
fixtureTokenClaim =
  let (_, token, _) = fixtureSession
   in checked (Rpc.resumeTokenToClaim token)

fixtureCursorClaim :: Protocol.ApplicationReplyCursorClaim
fixtureCursorClaim = checked (Rpc.cursorToClaim Request.firstApplicationReplyCursor)

fixtureRequestClaim :: Protocol.ApplicationRequestIdClaim
fixtureRequestClaim = Rpc.requestToClaim fixtureRequest

fixtureWaitClaim :: Protocol.ApplicationWaitClaim
fixtureWaitClaim = checked (Rpc.waitToClaim fixtureWait)

openDto :: Protocol.ApplicationClientDto
openDto = Protocol.OpenSession fixtureAttachmentClaim (Protocol.applicationClientNonce 42)

resumeDto :: Protocol.ApplicationClientDto
resumeDto = Protocol.ResumeSession fixtureSessionClaim fixtureTokenClaim fixtureCursorClaim

callDto :: Protocol.ApplicationClientDto
callDto = Protocol.Call fixtureSessionClaim fixtureRequestClaim fixtureOperation

establishedDtos :: [Protocol.ApplicationClientDto]
establishedDtos =
  [ Protocol.EndSession fixtureSessionClaim,
    callDto,
    Protocol.GetRequestResult fixtureSessionClaim fixtureRequestClaim,
    Protocol.CancelPendingWait fixtureSessionClaim fixtureRequestClaim fixtureWaitClaim
  ]

assertRightEqual :: (Eq value, Show value) => Either problem value -> value -> Assertion
assertRightEqual actual expected = case actual of
  Left _ -> assertBool "expected successful structural conversion" False
  Right value -> assertEqual "structural round-trip" expected value

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

caseLifecycle :: Assertion
caseLifecycle = do
  let lane = candidateApplicationLane 93
      (_, _, binding) = fixtureSession
      locator = checked (Lifecycle.heraldLocator (Text.pack "127.0.0.1") 64000)
      descriptor = checked (Lifecycle.connectionDescriptor locator (fixtureIdentifierBytes 41) (fixtureIdentifierBytes 42) (fixtureIdentifierBytes 43))
      claim = checked (Lifecycle.initialClaimId (fixtureIdentifierBytes 44))
      request = checked (Lifecycle.lifecycleRequestId (fixtureIdentifierBytes 45) 7 1)
      context = EstablishedApplicationLifecycleIngress binding locator
      command = Lifecycle.EndOwnProcess
      reply = Lifecycle.LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)
      dto = Protocol.LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)
  assertEqual
    "candidate retains full descriptor and stable initial correlation"
    (Right (ApplicationSessionInput (ClaimInitialApplicationSession lane descriptor claim Nothing)))
    (admitApplicationClientDto (CandidateApplicationIngress lane) (Protocol.ClaimInitial descriptor claim))
  assertEqual
    "lifecycle admission uses actual runtime locator"
    (Right (ApplicationLifecycleInput (CallApplicationLifecycle binding fixtureSessionId request locator command)))
    (admitApplicationClientDto context (Protocol.LifecycleCall fixtureSessionClaim request command))
  assertEqual
    "missing bound listener cannot accept lifecycle admission"
    (Left ApplicationRpcWrongIngressPhase)
    (admitApplicationClientDto (EstablishedApplicationIngress binding) (Protocol.LifecycleCall fixtureSessionClaim request command))
  assertEqual
    "pending initial response leaves the candidate usable"
    (Right (Just (SendPendingApplicationCandidate lane (Protocol.InitialClaimPending claim []))))
    (projectApplicationEffect (SendInitialClaimPending lane claim []))
  assertEqual
    "initial terminal error closes the candidate"
    (Right (Just (RejectApplicationCandidate lane (Protocol.InitialClaimRejected claim Lifecycle.StartupCancelled))))
    (projectApplicationEffect (RejectInitialClaim lane claim Lifecycle.StartupCancelled))
  assertEqual
    "restricted recovery result closes after final response"
    (Right (Just (RejectApplicationCandidate lane dto)))
    (projectApplicationEffect (SendApplicationLifecycleReply (CandidateApplicationDisposition lane) reply))
  assertEqual
    "established lifecycle reply stays on its binding"
    (Right (Just (SendEstablishedApplication binding dto)))
    (projectApplicationEffect (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) reply))
