{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( attachmentClaim,
    sessionClaim,
    resumeTokenClaim,
    requestClaim,
    secondRequestClaim,
    waitClaim,
    replyCursorClaim,
    clientNonce,
    heartbeatNonce,
    checkedSummary,
    clientEnvelopes,
    serverEnvelopes,
    currentClientEnvelope,
    currentServerEnvelope,
    newEnvironmentClientEnvelope,
    newEnvironmentServerEnvelope,
    labelCasOperation,
    allEnvelopes,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    ApplicationStartupAccess,
    EnvironmentAccess,
    allApplicationPredefinedSortRoles,
    applicationStartupAccess,
    environmentAccess,
    predefinedAccess,
    primordialAccessFromSelection,
    selectEnvironment,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (..),
    LabelResult (..),
  )
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement, applicationReceiptRetirementWithExceptions)
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryLiteral (QueryEnum),
    ApplicationQueryPredicate (..),
  )
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (CompareField, NeverPredicate),
    ApplicationProjection (..),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue, RankField),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationScalarLiteral (LiteralEnum),
    ApplicationSortDefinition (..),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (EnumSchema, RecordSchema),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientDto (..),
    ApplicationClientNonce,
    ApplicationEnvelope (..),
    ApplicationHeartbeatNonce,
    ApplicationIsolationOverlayDto (..),
    ApplicationReplyCursorClaim,
    ApplicationRequestIdClaim,
    ApplicationRequestReplyBodyDto (..),
    ApplicationRequestSummaryDto,
    ApplicationRequestSummaryEntryDto (..),
    ApplicationRequestSummaryStatusDto (..),
    ApplicationResumeTokenClaim,
    ApplicationServerDto (..),
    ApplicationSessionClaim,
    ApplicationSessionErrorDto (..),
    ApplicationSessionUnavailableReasonDto (..),
    ApplicationWaitClaim,
    applicationAttachmentClaim,
    applicationClientNonce,
    applicationHeartbeatNonce,
    applicationReplyCursorClaim,
    applicationRequestIdClaim,
    applicationRequestSummaryDto,
    applicationResumeTokenClaim,
    applicationSessionClaim,
    applicationWaitClaim,
  )
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement

attachmentClaim :: ApplicationAttachmentClaim
attachmentClaim = mustAdmit (applicationAttachmentClaim scopeBytes)

sessionClaim :: ApplicationSessionClaim
sessionClaim = mustAdmit (applicationSessionClaim scopeBytes 7)

resumeTokenClaim :: ApplicationResumeTokenClaim
resumeTokenClaim = mustAdmit (applicationResumeTokenClaim scopeBytes 7)

requestClaim :: ApplicationRequestIdClaim
requestClaim = applicationRequestIdClaim 0

secondRequestClaim :: ApplicationRequestIdClaim
secondRequestClaim = applicationRequestIdClaim 1

waitClaim :: ApplicationWaitClaim
waitClaim = mustAdmit (applicationWaitClaim scopeBytes 7 requestClaim)

replyCursorClaim :: ApplicationReplyCursorClaim
replyCursorClaim = mustAdmit (applicationReplyCursorClaim 1)

clientNonce :: ApplicationClientNonce
clientNonce = applicationClientNonce 0

heartbeatNonce :: ApplicationHeartbeatNonce
heartbeatNonce = applicationHeartbeatNonce 0x0102030405060708

checkedSummary :: ApplicationRequestSummaryDto
checkedSummary =
  mustAdmit
    ( applicationRequestSummaryDto
        [ ApplicationRequestSummaryEntryDto requestClaim (PendingDto waitClaim),
          ApplicationRequestSummaryEntryDto secondRequestClaim (OperationAcceptedDto StructuralStabilizationPending),
          ApplicationRequestSummaryEntryDto (applicationRequestIdClaim 2) (CompletedDto (WaitCompleted WaitReady)),
          ApplicationRequestSummaryEntryDto (applicationRequestIdClaim 3) (RejectedDto ApplicationOperateNotPermitted),
          ApplicationRequestSummaryEntryDto (applicationRequestIdClaim 4) CancelledDto,
          ApplicationRequestSummaryEntryDto (applicationRequestIdClaim 5) (OperationAcceptedDto EnvironmentStabilizationPending),
          ApplicationRequestSummaryEntryDto (applicationRequestIdClaim 6) (CompletedDto (NewEnvironmentCompleted environmentAccessFixture))
        ]
    )

clientEnvelopes :: [ApplicationEnvelope]
clientEnvelopes =
  fmap
    ApplicationClientEnvelope
    ( [ OpenSession attachmentClaim clientNonce,
        ResumeSession sessionClaim resumeTokenClaim replyCursorClaim,
        RetireReceipts sessionClaim (applicationReceiptRetirement (Just 0) (Just 4)),
        RetireReceipts sessionClaim (applicationReceiptRetirementWithExceptions (mustAdmit (Retirement.receiptRetirement (Just 4) (Set.fromList [0, 3]))) (mustAdmit (Retirement.receiptRetirement (Just 5) (Set.singleton 2)))),
        CallWithRetirement sessionClaim requestClaim NewEnvironmentApplication (applicationReceiptRetirement (Just 0) Nothing),
        LifecycleCallWithRetirement sessionClaim lifecycleRequest Lifecycle.EndOwnProcess (applicationReceiptRetirement Nothing (Just 4)),
        EndSession sessionClaim
      ]
        <> fmap (Call sessionClaim requestClaim) operations
        <> fmap (LifecycleCall sessionClaim lifecycleRequest) lifecycleCommands
        <> [ GetRequestResult sessionClaim requestClaim,
             CancelPendingWait sessionClaim requestClaim waitClaim,
             Ping heartbeatNonce,
             ClaimInitial lifecycleDescriptor initialClaim,
             GetLifecycleResult sessionClaim lifecycleRequest,
             RecoverLifecycleResult attachmentClaim lifecycleRequest
           ]
    )

serverEnvelopes :: [ApplicationEnvelope]
serverEnvelopes =
  fmap
    ApplicationServerEnvelope
    ( [ SessionOpened replyCursorClaim sessionClaim resumeTokenClaim startupAccess,
        SessionResumed replyCursorClaim sessionClaim checkedSummary
      ]
        <> fmap SessionRejected sessionErrors
        <> fmap (InitialClaimRejected initialClaim) startupErrors
        <> fmap (LifecycleReply lifecycleRequest) lifecycleStatuses
        <> fmap (RequestRetained sessionClaim replyCursorClaim requestClaim) retainedBodies
        <> [ RequestAbsent sessionClaim requestClaim,
             RequestConflict sessionClaim requestClaim,
             WaitWake sessionClaim replyCursorClaim requestClaim waitClaim WaitReady,
             Pong heartbeatNonce,
             HeraldIsolationBegun
               replyCursorClaim
               sessionClaim
               ResidentProcessesZombie
               HeraldIsolatedDto,
             HeraldPermanentlyUnavailable
               sessionClaim
               ApplicationSessionNoLongerLiveDto,
             HeraldPermanentlyUnavailable sessionClaim HeraldIsolatedDto,
             HeraldPermanentlyUnavailable sessionClaim HeraldRetiredDto,
             InitialClaimPending initialClaim ["reader"],
             LifecycleAbsent lifecycleRequest,
             LifecycleConflict lifecycleRequest,
             ReceiptsRetired sessionClaim (applicationReceiptRetirement (Just 0) (Just 4)),
             RequestRetired sessionClaim requestClaim 4,
             LifecycleRetired lifecycleRequest 4
           ]
    )

currentClientEnvelope :: ApplicationEnvelope
currentClientEnvelope = newEnvironmentClientEnvelope

newEnvironmentClientEnvelope :: ApplicationEnvelope
newEnvironmentClientEnvelope =
  ApplicationClientEnvelope
    (Call sessionClaim requestClaim NewEnvironmentApplication)

currentServerEnvelope :: ApplicationEnvelope
currentServerEnvelope = newEnvironmentServerEnvelope

newEnvironmentServerEnvelope :: ApplicationEnvelope
newEnvironmentServerEnvelope =
  ApplicationServerEnvelope
    ( RequestRetained
        sessionClaim
        replyCursorClaim
        requestClaim
        (Completed (NewEnvironmentCompleted environmentAccessFixture))
    )

allEnvelopes :: [ApplicationEnvelope]
allEnvelopes = clientEnvelopes <> serverEnvelopes

scopeBytes :: ByteString.ByteString
scopeBytes = ByteString.pack [0 .. 31]

privateUnique :: Word64 -> PrivateUniqueId
privateUnique = mustAdmit . mkPrivateUniqueId

privateNabla :: Word64 -> PrivateNablaId
privateNabla = asPrivateNablaId . privateUnique

privateDelta :: Word64 -> PrivateDeltaId
privateDelta = asPrivateDeltaId . privateUnique

privateObject :: Word64 -> PrivateObjectId
privateObject = asPrivateObjectId . privateUnique

privateProcess :: Word64 -> PrivateProcessId
privateProcess = asPrivateProcessId . privateUnique

sortIdentity :: SortId
sortIdentity = mustAdmit (mkSortId (ByteString.pack [1 .. 32]))

otherSortIdentity :: SortId
otherSortIdentity = mustAdmit (mkSortId (ByteString.pack [2 .. 33]))

projection :: ApplicationProjection
projection = ApplicationProjection ("field" :| [])

query :: ApplicationQuery
query = ApplicationQuery Set.empty QueryAlways

enumQuery :: ApplicationQuery
enumQuery =
  ApplicationQuery
    Set.empty
    (QueryCompare projection ScalarEqual (QueryEnum "alpha"))

enumDescriptor :: ApplicationSortDescriptor
enumDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema =
        RecordSchema
          (Map.singleton "field" (EnumSchema ("zeta" :| ["alpha"]))),
      keyProjections = [projection],
      validityPredicate =
        CompareField projection ScalarEqual (LiteralEnum "alpha"),
      obsolescencePredicate = NeverPredicate,
      rankTerms =
        RankField projection Ascending
          :| [RankApplicationValue Ascending],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

operations :: [ApplicationOperation]
operations =
  [ NewIdApplication BareNewId,
    NewIdApplication (ControlledNewId (privateNabla 1)),
    WriteApplication
      (privateNabla 1)
      (PublishValue (SortDefinitionValue (PredefinedSortDefinition SortDefinitionRole sortIdentity))),
    WriteApplication
      (privateNabla 1)
      (PublishValue (SortDefinitionValue (DeclaredSortDefinition enumDescriptor Nothing))),
    WriteApplication (privateNabla 1) (PublishValue (BoolValue True)),
    WriteApplication (privateNabla 1) (PublishValue (EnumValue "preserve")),
    WriteApplication (privateNabla 1) (DeleteReserved (privateObject 2)),
    ForwardApplication (privateNabla 1) (privateObject 2),
    ReadApplication query,
    ReadApplication enumQuery,
    LocalTakeApplication query,
    WaitApplication [query],
    LabelApplication (privateObject 2) (VoidLabel, 0) (LabelToProcess (privateProcess 3)),
    LabelApplication (privateObject 2) (ProcessLabel (privateProcess 3), 0) LabelToVoid,
    labelCasOperation,
    NewEnvironmentApplication
  ]

labelCasOperation :: ApplicationOperation
labelCasOperation =
  LabelApplication
    (privateObject 2)
    (ZombieLabel (privateProcess 3), 7)
    LabelToDelete

sessionErrors :: [ApplicationSessionErrorDto]
sessionErrors =
  [ ApplicationAttachmentNotAdmittedDto,
    ApplicationSessionNotLiveDto,
    ApplicationResumeTokenMismatchDto,
    ApplicationReplyCursorNotIssuedDto,
    ApplicationRequestSessionMismatchDto,
    ApplicationRequestBindingMismatchDto,
    ApplicationRequestBindingNotLiveDto,
    ApplicationEndSessionBindingMismatchDto,
    ApplicationReceiptRetirementNotReadyDto,
    ApplicationRequestAlreadyRetiredDto
  ]

retainedBodies :: [ApplicationRequestReplyBodyDto]
retainedBodies =
  [ WaitAccepted waitClaim,
    OperationAccepted StructuralStabilizationPending,
    OperationAccepted LabelSettlementPending,
    OperationAccepted EnvironmentStabilizationPending,
    Cancelled waitClaim
  ]
    <> fmap Completed regularResults
    <> fmap Rejected rejections

regularResults :: [RegularCallResult]
regularResults =
  [ NewIdCompleted (privateUnique 4),
    WriteCompleted (SortDefinitionWritten sortIdentity),
    WriteCompleted WriteAccepted,
    ForwardCompleted ForwardAccepted,
    ReadCompleted [EnumValue "preserve"],
    LocalTakeCompleted [],
    WaitCompleted WaitReady,
    LabelCompleted LabelApplied,
    LabelCompleted LabelNotApplied,
    NewEnvironmentCompleted environmentAccessFixture
  ]

rejections :: [ApplicationRejection]
rejections =
  [ ApplicationUnknownPrivateIdentity (privateUnique 1),
    ApplicationInvalidFieldName "field",
    ApplicationInvalidQueryProjection projection,
    ApplicationNablaRoleMismatch (privateNabla 1),
    ApplicationDeltaRoleMismatch (privateDelta 2),
    ApplicationProcessRoleMismatch (privateProcess 3),
    ApplicationDeltaNotLocal (privateDelta 2),
    ApplicationDeltaStoreUnavailable (privateDelta 2),
    ApplicationOperateNotPermitted,
    ApplicationQueryPredicateMismatch,
    ApplicationSortDefinitionProjectionUnsupported projection,
    ApplicationSortDescriptorRejected,
    ApplicationSortIdClaimMismatch sortIdentity otherSortIdentity,
    ApplicationNotCurrentSortDefinitionWriter (privateNabla 1),
    ApplicationNablaSortNotControlled (privateNabla 1),
    ApplicationReservationUnavailable (privateNabla 1) (privateObject 2),
    ApplicationObjectNotLabelable (privateObject 2),
    ApplicationLabelTargetNotNameable (privateProcess 3),
    ApplicationLabelTransitionNotPermitted,
    EnvironmentSourcesUnavailable
  ]

startupAccess :: ApplicationStartupAccess
startupAccess =
  applicationStartupAccess
    (privateProcess 1)
    ( primordialAccessFromSelection
        ( selectEnvironment
            ( mustAdmit
                ( environmentAccess
                    ( zipWith
                        (\role offset -> predefinedAccess role (privateNabla offset) (privateDelta (offset + 20)))
                        allApplicationPredefinedSortRoles
                        [2 ..]
                    )
                    (privateObject 1000)
                    [privateObject value | value <- [1001 .. 1018]]
                )
            )
        )
    )

environmentAccessFixture :: EnvironmentAccess
environmentAccessFixture =
  mustAdmit
    ( environmentAccess
        ( zipWith
            (\role offset -> predefinedAccess role (privateNabla offset) (privateDelta (offset + 40)))
            allApplicationPredefinedSortRoles
            [30 ..]
        )
        (privateObject 1000)
        [privateObject value | value <- [1001 .. 1018]]
    )

mustAdmit :: (Show error) => Either error value -> value
mustAdmit = either (error . ("invalid protocol test fixture: " <>) . show) id

lifecycleRequest :: Lifecycle.LifecycleRequestId
lifecycleRequest = mustAdmit (Lifecycle.lifecycleRequestId scopeBytes 7 2)
lifecyclePreparation :: Lifecycle.ChildPreparation
lifecyclePreparation = mustAdmit (Lifecycle.childPreparation scopeBytes 7 2)
lifecycleDescriptor :: Lifecycle.ConnectionDescriptor
lifecycleDescriptor = mustAdmit (Lifecycle.connectionDescriptor (mustAdmit (Lifecycle.heraldLocator "127.0.0.1" 7000)) scopeBytes scopeBytes scopeBytes)
initialClaim :: Lifecycle.InitialClaimId
initialClaim = mustAdmit (Lifecycle.initialClaimId scopeBytes)
lifecycleCommands :: [Lifecycle.LifecycleCommand]
lifecycleCommands =
  [ Lifecycle.BeginChild (Lifecycle.connectionDescriptorLocator lifecycleDescriptor) (mustAdmit (Access.primordialSelection [] Set.empty Nothing)),
    Lifecycle.AwaitPreparedChild lifecyclePreparation,
    Lifecycle.AwaitChildReady lifecyclePreparation,
    Lifecycle.CancelChild lifecyclePreparation,
    Lifecycle.EndOwnProcess
  ]
lifecycleStatuses :: [Lifecycle.LifecycleStatus]
lifecycleStatuses =
  Lifecycle.LifecyclePending ["writer", "reader"]
    : [Lifecycle.LifecycleCompleted result | result <- [Lifecycle.ChildPreparationAccepted lifecyclePreparation, Lifecycle.ChildPrepared (Lifecycle.preparedChild lifecyclePreparation (privateProcess 3) lifecycleDescriptor), Lifecycle.ChildReady, Lifecycle.ChildCancelled, Lifecycle.ChildAlreadyAttached, Lifecycle.ProcessEnded]]
      <> [Lifecycle.LifecycleRejected problem | problem <- [Lifecycle.LifecycleUnknownTarget, Lifecycle.LifecycleTargetUnavailable, Lifecycle.LifecycleSelectionNotAdmitted, Lifecycle.LifecycleUnknownPreparation, Lifecycle.LifecycleNotPreparingParent, Lifecycle.LifecycleAlreadyTerminal, Lifecycle.LifecycleRequestNotAdmitted, Lifecycle.LifecycleOwnProcessNotLive, Lifecycle.LifecycleOracleRejected] <> fmap Lifecycle.LifecycleStartupFailed startupErrors]
startupErrors :: [Lifecycle.StartupError]
startupErrors = [Lifecycle.StartupWrongTarget, Lifecycle.StartupWrongLineage, Lifecycle.StartupUnknownAttachment, Lifecycle.StartupNotLive, Lifecycle.StartupAlreadyClaimed, Lifecycle.StartupCancelled, Lifecycle.StartupRequiredObjectUnavailable "reader", Lifecycle.StartupSessionExpired]
