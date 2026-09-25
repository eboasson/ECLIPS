module PublicStep11Facade
  ( operationArm,
    newIdTargetArm,
    writeValueArm,
    writeResultArm,
    forwardResultArm,
    regularResultArm,
    pendingReasonArm,
    environmentAccessSize,
    rejectionArm,
    admittedGeneratorSeed,
    seedErrorArm,
    emptySelection,
    selectCompleteEnvironment,
    startupWithSelectedAccess,
    primordialEntryArm,
    lifecycleCommandArm,
    lifecycleResultArm,
    lifecycleStatusArm,
    lifecycleReplyArm,
    checkedDescriptor,
  )
where

import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    EnvironmentAccess,
    PrimordialAccess,
    PrimordialEntry (..),
    PrimordialSelection,
    StartupAccessError,
    applicationStartupAccess,
    environmentAccessPredefined,
    primordialSelection,
    selectEnvironment,
  )
import Eclips.Application.Types.Forward (ForwardResult (..))
import Eclips.Application.Types.Identity (PrivateProcessId)
import Eclips.Application.Types.Lifecycle
  ( ConnectionDescriptor,
    HeraldLocator,
    LifecycleCommand (..),
    LifecycleReply (..),
    LifecycleResult (..),
    LifecycleShapeError,
    LifecycleStatus (..),
    connectionDescriptor,
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Herald.IdGenerator
  ( GeneratorSeed,
    StartupSeedError (..),
    mkGeneratorSeed,
  )

-- These deliberately exhaustive observers make the current public unions a
-- compile-time client contract.  The fixture is built with
-- -Werror=incomplete-patterns, so adding or removing a public branch requires
-- an explicit boundary review.
operationArm :: ApplicationOperation -> Int
operationArm operation = case operation of
  NewIdApplication _ -> 0
  WriteApplication _ _ -> 1
  ForwardApplication _ _ -> 2
  ReadApplication _ -> 3
  LocalTakeApplication _ -> 4
  WaitApplication _ -> 5
  LabelApplication _ _ _ -> 6
  NewEnvironmentApplication -> 7

newIdTargetArm :: NewIdTarget -> Int
newIdTargetArm target = case target of
  BareNewId -> 0
  ControlledNewId _ -> 1

writeValueArm :: ApplicationWriteValue -> Int
writeValueArm value = case value of
  PublishValue _ -> 0
  DeleteReserved _ -> 1

writeResultArm :: WriteResult -> Int
writeResultArm result = case result of
  SortDefinitionWritten _ -> 0
  WriteAccepted -> 1

forwardResultArm :: ForwardResult -> Int
forwardResultArm result = case result of
  ForwardAccepted -> 0

regularResultArm :: RegularCallResult -> Int
regularResultArm result = case result of
  NewIdCompleted _ -> 0
  WriteCompleted _ -> 1
  ForwardCompleted _ -> 2
  ReadCompleted _ -> 3
  LocalTakeCompleted _ -> 4
  WaitCompleted _ -> 5
  LabelCompleted _ -> 6
  NewEnvironmentCompleted _ -> 7

pendingReasonArm :: OperationPendingReason -> Int
pendingReasonArm reason = case reason of
  StructuralStabilizationPending -> 0
  LabelSettlementPending -> 1
  EnvironmentStabilizationPending -> 2

environmentAccessSize :: EnvironmentAccess -> Int
environmentAccessSize = length . environmentAccessPredefined

rejectionArm :: ApplicationRejection -> Int
rejectionArm rejection = case rejection of
  ApplicationUnknownPrivateIdentity _ -> 0
  ApplicationInvalidFieldName _ -> 1
  ApplicationInvalidQueryProjection _ -> 2
  ApplicationNablaRoleMismatch _ -> 3
  ApplicationDeltaRoleMismatch _ -> 4
  ApplicationProcessRoleMismatch _ -> 5
  ApplicationDeltaNotLocal _ -> 6
  ApplicationDeltaStoreUnavailable _ -> 7
  ApplicationOperateNotPermitted -> 8
  ApplicationQueryPredicateMismatch -> 9
  ApplicationSortDefinitionProjectionUnsupported _ -> 10
  ApplicationSortDescriptorRejected -> 11
  ApplicationSortIdClaimMismatch _ _ -> 12
  ApplicationNotCurrentSortDefinitionWriter _ -> 13
  ApplicationNablaSortNotControlled _ -> 14
  ApplicationReservationUnavailable _ _ -> 15
  ApplicationObjectNotLabelable _ -> 16
  ApplicationLabelTargetNotNameable _ -> 17
  ApplicationLabelTransitionNotPermitted -> 18
  EnvironmentSourcesUnavailable -> 19

admittedGeneratorSeed :: ByteString -> Either StartupSeedError GeneratorSeed
admittedGeneratorSeed = mkGeneratorSeed

seedErrorArm :: StartupSeedError -> Int
seedErrorArm seedError = case seedError of
  WrongGeneratorSeedByteCount {} -> 0

emptySelection :: Either StartupAccessError PrimordialSelection
emptySelection = primordialSelection [] Set.empty Nothing

selectCompleteEnvironment :: EnvironmentAccess -> PrimordialSelection
selectCompleteEnvironment = selectEnvironment

startupWithSelectedAccess :: PrivateProcessId -> PrimordialAccess -> ApplicationStartupAccess
startupWithSelectedAccess = applicationStartupAccess

primordialEntryArm :: PrimordialEntry -> Int
primordialEntryArm = \case
  Identity _ -> 0
  Object _ -> 1
  Writer _ -> 2
  Reader _ -> 3
  Process _ -> 4

lifecycleCommandArm :: LifecycleCommand -> Int
lifecycleCommandArm = \case
  BeginChild _ _ -> 0
  AwaitPreparedChild _ -> 1
  AwaitChildReady _ -> 2
  CancelChild _ -> 3
  EndOwnProcess -> 4

lifecycleResultArm :: LifecycleResult -> Int
lifecycleResultArm = \case
  ChildPreparationAccepted _ -> 0
  ChildPrepared _ -> 1
  ChildReady -> 2
  ChildCancelled -> 3
  ChildAlreadyAttached -> 4
  ProcessEnded -> 5

lifecycleStatusArm :: LifecycleStatus -> Int
lifecycleStatusArm = \case
  LifecyclePending _ -> 0
  LifecycleCompleted _ -> 1
  LifecycleRejected _ -> 2

lifecycleReplyArm :: LifecycleReply -> Int
lifecycleReplyArm = \case
  LifecycleReply _ _ -> 0
  LifecycleAbsent _ -> 1
  LifecycleConflict _ -> 2
  LifecycleRetired _ _ -> 3

checkedDescriptor :: HeraldLocator -> ByteString -> ByteString -> ByteString -> Either LifecycleShapeError ConnectionDescriptor
checkedDescriptor = connectionDescriptor
