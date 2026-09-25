-- | Pure runtime-safe admission and projection for the application RPC lane.
--
-- Protocol decoding establishes only structural shape.  This adapter translates
-- nominal claims into typed kernel ingress and translates complete application
-- effects back to DTOs; all stateful admission remains in @stepHerald@.
module Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (..),
    ApplicationRpcAdmissionError (..),
    admitApplicationClientDto,
    ApplicationOutbound (..),
    ApplicationRpcProjectionFault (..),
    projectApplicationEffect,
  )
where

import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Herald.Application.RPC.Internal qualified as Internal
import Eclips.Herald.Application.Session qualified as Session
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (..),
    HeraldEffect (..),
  )
import Eclips.Herald.Input
  ( ApplicationLifecycleIngress (..),
    ApplicationReceiptRetirementIngress (..),
    ApplicationRequestIngress (..),
    ApplicationRetirementWork (..),
    ApplicationSessionIngress (..),
    CandidateApplicationLane,
    HeraldInputBody (..),
  )
import Eclips.Protocol.Application.Types qualified as Protocol

-- | Runtime-owned logical context in which a decoded DTO arrived.
data ApplicationIngressContext
  = CandidateApplicationIngress CandidateApplicationLane
  | CandidateApplicationLifecycleIngress CandidateApplicationLane HeraldLocator
  | EstablishedApplicationIngress Session.ApplicationSessionBinding
  | EstablishedApplicationLifecycleIngress Session.ApplicationSessionBinding HeraldLocator
  deriving stock (Eq, Show)

-- | A decoded DTO cannot be admitted in the supplied logical lane.
data ApplicationRpcAdmissionError
  = ApplicationRpcWrongIngressPhase
  | ApplicationRpcClaimContradiction
  deriving stock (Eq, Show)

-- | Translate one structurally checked client DTO into typed kernel ingress.
-- State-dependent mismatches deliberately remain inputs for @stepHerald@.
admitApplicationClientDto ::
  ApplicationIngressContext ->
  Protocol.ApplicationClientDto ->
  Either ApplicationRpcAdmissionError HeraldInputBody
admitApplicationClientDto (CandidateApplicationLifecycleIngress lane locator) (Protocol.ClaimInitial descriptor claim) =
  Right (ApplicationSessionInput (ClaimInitialApplicationSession lane descriptor claim (Just locator)))
admitApplicationClientDto (CandidateApplicationLifecycleIngress lane _) dto = admitApplicationClientDto (CandidateApplicationIngress lane) dto
admitApplicationClientDto (EstablishedApplicationLifecycleIngress binding locator) (Protocol.LifecycleCall session request command) = do
  checkedSession <- convertClaim (Internal.sessionFromClaim session)
  Right (ApplicationLifecycleInput (CallApplicationLifecycle binding checkedSession request locator command))
admitApplicationClientDto (EstablishedApplicationLifecycleIngress binding locator) (Protocol.LifecycleCallWithRetirement session request command progress) = do
  checkedSession <- convertClaim (Internal.sessionFromClaim session)
  Right (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding checkedSession progress (Just (RetiringApplicationLifecycle request locator command))))
admitApplicationClientDto (EstablishedApplicationLifecycleIngress binding _) dto = admitApplicationClientDto (EstablishedApplicationIngress binding) dto
admitApplicationClientDto context dto = case (context, dto) of
  (CandidateApplicationIngress lane, Protocol.ClaimInitial descriptor claim) ->
    Right (ApplicationSessionInput (ClaimInitialApplicationSession lane descriptor claim Nothing))
  (CandidateApplicationIngress lane, Protocol.RecoverLifecycleResult attachment request) -> do
    checkedAttachment <- convertClaim (Internal.attachmentFromClaim attachment)
    Right (ApplicationLifecycleInput (RecoverApplicationLifecycleResult lane checkedAttachment request))
  (EstablishedApplicationIngress binding, Protocol.GetLifecycleResult session request) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right (ApplicationLifecycleInput (GetApplicationLifecycleResult binding checkedSession request))
  (CandidateApplicationIngress lane, Protocol.OpenSession attachment nonce) -> do
    checkedAttachment <- convertClaim (Internal.attachmentFromClaim attachment)
    Right
      ( ApplicationSessionInput
          (OpenApplicationSession lane checkedAttachment (Internal.nonceFromClaim nonce))
      )
  (CandidateApplicationIngress lane, Protocol.ResumeSession session token cursor) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    checkedToken <- convertClaim (Internal.resumeTokenFromClaim token)
    checkedCursor <- convertClaim (Internal.cursorFromClaim cursor)
    Right
      ( ApplicationSessionInput
          (ResumeApplicationSession lane checkedSession checkedToken checkedCursor)
      )
  (EstablishedApplicationIngress binding, Protocol.EndSession session) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right (ApplicationSessionInput (EndApplicationSession binding checkedSession))
  (EstablishedApplicationIngress binding, Protocol.Call session request operation) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right
      ( ApplicationRequestInput
          ( CallApplicationRequest
              binding
              checkedSession
              (Internal.requestFromClaim request)
              operation
          )
      )
  (EstablishedApplicationIngress binding, Protocol.CallWithRetirement session request operation progress) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding checkedSession progress (Just (RetiringApplicationRequest (Internal.requestFromClaim request) operation))))
  (EstablishedApplicationIngress binding, Protocol.RetireReceipts session progress) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding checkedSession progress Nothing))
  (EstablishedApplicationIngress binding, Protocol.GetRequestResult session request) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    Right
      ( ApplicationRequestInput
          (GetApplicationRequestResult binding checkedSession (Internal.requestFromClaim request))
      )
  (EstablishedApplicationIngress binding, Protocol.CancelPendingWait session request wait) -> do
    checkedSession <- convertClaim (Internal.sessionFromClaim session)
    checkedWait <- convertClaim (Internal.waitFromClaim wait)
    Right
      ( ApplicationRequestInput
          ( CancelApplicationPendingWait
              binding
              checkedSession
              (Internal.requestFromClaim request)
              checkedWait
          )
      )
  _ -> Left ApplicationRpcWrongIngressPhase

-- | Complete application output plus runtime-only lane or binding metadata.
data ApplicationOutbound
  = AcceptApplicationCandidate
      CandidateApplicationLane
      Session.ApplicationSessionBinding
      Protocol.ApplicationServerDto
  | SendPendingApplicationCandidate CandidateApplicationLane Protocol.ApplicationServerDto
  | RejectApplicationCandidate
      CandidateApplicationLane
      Protocol.ApplicationServerDto
  | RejectEstablishedApplication
      Session.ApplicationSessionBinding
      Protocol.ApplicationServerDto
  | SendEstablishedApplication
      Session.ApplicationSessionBinding
      Protocol.ApplicationServerDto
  | DisposeEstablishedApplicationSession
      Session.ApplicationSessionId
      Protocol.ApplicationServerDto
  deriving stock (Eq, Show)

-- | A checked Herald-owned value contradicted the structurally checked
-- application protocol representation during effect projection.
data ApplicationRpcProjectionFault
  = ApplicationRpcOwnerClaimContradiction
  | ApplicationRpcOwnerSummaryContradiction
  deriving stock (Eq, Show)

-- | Project one already complete kernel effect.  Non-application effects are
-- outside this adapter and return 'Right Nothing'.
projectApplicationEffect ::
  HeraldEffect ->
  Either ApplicationRpcProjectionFault (Maybe ApplicationOutbound)
projectApplicationEffect = \case
  SendInitialClaimPending lane claim keys ->
    pure (Just (SendPendingApplicationCandidate lane (Protocol.InitialClaimPending claim keys)))
  RejectInitialClaim lane claim reason ->
    pure (Just (RejectApplicationCandidate lane (Protocol.InitialClaimRejected claim reason)))
  SendApplicationLifecycleReply target reply ->
    let dto = case reply of
          Lifecycle.LifecycleReply request status -> Protocol.LifecycleReply request status
          Lifecycle.LifecycleAbsent request -> Protocol.LifecycleAbsent request
          Lifecycle.LifecycleConflict request -> Protocol.LifecycleConflict request
          Lifecycle.LifecycleRetired request through -> Protocol.LifecycleRetired request through
     in pure
          ( Just
              ( case target of
                  CandidateApplicationDisposition lane -> RejectApplicationCandidate lane dto
                  EstablishedApplicationDisposition binding -> SendEstablishedApplication binding dto
              )
          )
  SetApplicationConnectionDisposition lane acceptance -> do
    dto <- projectOwnerClaim (Internal.sessionAcceptanceToDto acceptance)
    pure
      ( Just
          ( AcceptApplicationCandidate
              lane
              (Session.sessionAcceptanceBinding acceptance)
              dto
          )
      )
  RejectApplicationConnection target rejection ->
    let dto = Protocol.SessionRejected (Internal.sessionErrorToDto rejection)
     in pure
          ( Just
              ( case target of
                  CandidateApplicationDisposition lane ->
                    RejectApplicationCandidate lane dto
                  EstablishedApplicationDisposition binding ->
                    RejectEstablishedApplication binding dto
              )
          )
  SendApplicationReply binding reply -> do
    dto <- projectOwnerClaim (Internal.requestReplyToDto binding reply)
    pure (Just (SendEstablishedApplication binding dto))
  SendApplicationWaitWake binding cursor request wait result -> do
    dto <- projectOwnerClaim (Internal.waitWakeToDto binding cursor request wait result)
    pure (Just (SendEstablishedApplication binding dto))
  SendApplicationIsolationBegun binding cursor session reason -> do
    cursorClaim <- projectOwnerClaim (Internal.cursorToClaim cursor)
    sessionClaim <- projectOwnerClaim (Internal.sessionToClaim session)
    pure
      ( Just
          ( SendEstablishedApplication
              binding
              ( Protocol.HeraldIsolationBegun
                  cursorClaim
                  sessionClaim
                  Protocol.ResidentProcessesZombie
                  (Internal.sessionUnavailableReasonToDto reason)
              )
          )
      )
  DisposeApplicationSession session reason -> do
    claim <- projectOwnerClaim (Internal.sessionToClaim session)
    pure
      ( Just
          ( DisposeEstablishedApplicationSession
              session
              ( Protocol.HeraldPermanentlyUnavailable
                  claim
                  (Internal.sessionUnavailableReasonToDto reason)
              )
          )
      )
  _ -> Right Nothing

projectOwnerClaim ::
  Either Internal.ApplicationRpcClaimError value ->
  Either ApplicationRpcProjectionFault value
projectOwnerClaim = \case
  Left Internal.ApplicationRpcClaimShapeContradiction ->
    Left ApplicationRpcOwnerClaimContradiction
  Left Internal.ApplicationRpcSummaryOrderContradiction ->
    Left ApplicationRpcOwnerSummaryContradiction
  Right value -> Right value

convertClaim ::
  Either Internal.ApplicationRpcClaimError value ->
  Either ApplicationRpcAdmissionError value
convertClaim = either (const (Left ApplicationRpcClaimContradiction)) Right
