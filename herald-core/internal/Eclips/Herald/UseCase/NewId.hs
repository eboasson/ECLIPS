-- | Atomic bare and controlled generated-identity coordination.
module Eclips.Herald.UseCase.NewId
  ( NewIdFailure (..),
    planNewId,
    OperableNabla,
    operableNablaId,
    operableNablaSortId,
    operableNablaAuthority,
    resolveOperableNabla,
  )
where

import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    privateNablaUniqueId,
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    NablaId,
    SortId,
    globalObjectIdFromGlobalUniqueId,
    nablaIdFromGlobalObjectId,
  )
import Eclips.Domain.Sort.Canonical (canonicalCheckedDescriptor)
import Eclips.Domain.Sort.Descriptor (SortKind (ControlledSort), descriptorKind)
import Eclips.Herald.Application.PrivateIdentity
  ( preparePrivateUniqueIdAllocation,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (SendApplicationReply),
    singletonEffectBatch,
  )
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupIdGeneratorState,
    startupApplicationState,
    startupControlledState,
    startupIdGeneratorState,
    startupSortRegistryState,
    startupStructuralProgressState,
  )

data NewIdFailure
  = NewIdRejected ApplicationRejection
  | NewIdContradiction

data OperableNabla
  = OperableNabla
      NablaId
      SortId
      AuthorityEpoch

operableNablaId :: OperableNabla -> NablaId
operableNablaId (OperableNabla nabla _ _) = nabla

operableNablaSortId :: OperableNabla -> SortId
operableNablaSortId (OperableNabla _ sortId _) = sortId

operableNablaAuthority :: OperableNabla -> AuthorityEpoch
operableNablaAuthority (OperableNabla _ _ authority) = authority

-- | Resolve the current controlled writer, effective controlled sort, caller
-- possession, and captured authority before generator preparation.
resolveOperableNabla ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  HeraldState ->
  Either NewIdFailure OperableNabla
resolveOperableNabla candidate privateNabla predecessor = do
  global <-
    case Application.resolveApplicationPrivateUniqueId
      process
      (privateNablaUniqueId privateNabla)
      (startupApplicationState predecessor) of
      Left (Application.ApplicationValueRejected _) ->
        Left
          ( NewIdRejected
              (ApplicationUnknownPrivateIdentity (privateNablaUniqueId privateNabla))
          )
      Left (Application.ApplicationValueGlobalizationInvariant _) ->
        Left NewIdContradiction
      Right identity -> Right identity
  let object = globalObjectIdFromGlobalUniqueId global
      nabla = nablaIdFromGlobalObjectId object
      controlled = startupControlledState predecessor
  operate <-
    either
      (operateFailure privateNabla)
      Right
      ( ControlledOperate.checkControlledOperate
          process
          nabla
          controlled
          (startupStructuralProgressState predecessor)
      )
  entry <-
    maybe
      (Left NewIdContradiction)
      Right
      ( SortRegistry.lookupEffectiveSort
          (ControlledOperate.controlledOperateSortId operate)
          (startupSortRegistryState predecessor)
      )
  if descriptorKind (canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor entry))
    /= ControlledSort
    then Left (NewIdRejected (ApplicationNablaSortNotControlled privateNabla))
    else
      if SortRegistry.registryEntrySortId entry
        /= ControlledOperate.controlledOperateSortId operate
        || SortRegistry.registryEntryOccurrenceId entry
          /= ControlledOperate.controlledOperateOccurrenceId operate
        then Left NewIdContradiction
        else
          Right
            ( OperableNabla
                nabla
                (ControlledOperate.controlledOperateSortId operate)
                (ControlledOperate.controlledOperateAuthority operate)
            )
  where
    process = Application.applicationRequestCandidateProcess candidate

operateFailure ::
  PrivateNablaId ->
  ControlledOperate.ControlledOperateError ->
  Either NewIdFailure value
operateFailure privateNabla = \case
  ControlledOperate.ControlledOperateBootstrapError problem ->
    case problem of
      Controlled.ControlledBootstrapOperateWriterUnavailable _ ->
        Left (NewIdRejected (ApplicationNablaRoleMismatch privateNabla))
      Controlled.ControlledBootstrapOperateWriterProcessMismatch {} ->
        Left (NewIdRejected ApplicationOperateNotPermitted)
      Controlled.ControlledBootstrapOperateProcessNotPossessed _ ->
        Left (NewIdRejected ApplicationOperateNotPermitted)
      Controlled.ControlledBootstrapOperateWriterNotPossessed {} ->
        Left (NewIdRejected ApplicationOperateNotPermitted)
      _ -> Left NewIdContradiction
  ControlledOperate.ControlledOperateDynamicWriterUnavailable _ ->
    Left (NewIdRejected (ApplicationNablaRoleMismatch privateNabla))
  ControlledOperate.ControlledOperateDynamicControllerMismatch {} ->
    Left (NewIdRejected ApplicationOperateNotPermitted)
  ControlledOperate.ControlledOperateDynamicResidenceMismatch ->
    Left (NewIdRejected ApplicationOperateNotPermitted)
  ControlledOperate.ControlledOperateDynamicWriterNotPossessed {} ->
    Left (NewIdRejected ApplicationOperateNotPermitted)
  _ -> Left NewIdContradiction

planNewId ::
  Application.ApplicationRequestCandidate ->
  NewIdTarget ->
  HeraldState ->
  Either NewIdFailure (HeraldState, EffectBatch)
planNewId candidate target predecessor = do
  operable <- case target of
    BareNewId -> Right Nothing
    ControlledNewId privateNabla ->
      Just <$> resolveOperableNabla candidate privateNabla predecessor
  privateAllocation <-
    either
      (const (Left NewIdContradiction))
      Right
      ( preparePrivateUniqueIdAllocation
          process
          (Application.applicationPrivateIdentity applicationPredecessor)
      )
  generatedPreparation <-
    either
      (const (Left NewIdContradiction))
      Right
      (IdGenerator.prepareGeneratedId (startupIdGeneratorState predecessor))
  let generated = IdGenerator.preparedGlobalUniqueId generatedPreparation
  preparedRequest <-
    either
      (const (Left NewIdContradiction))
      Right
      ( Application.prepareApplicationGeneratedIdCompletion
          candidate
          privateAllocation
          generated
      )
  let (applicationSuccessor, reply) =
        Application.commitApplicationRequest preparedRequest
      controlledSuccessor = case operable of
        Nothing -> startupControlledState predecessor
        Just admitted ->
          Controlled.commitControlledReservation
            ( Controlled.prepareControlledReservation
                process
                (operableNablaId admitted)
                (operableNablaSortId admitted)
                (operableNablaAuthority admitted)
                generated
                (startupControlledState predecessor)
            )
      successor =
        replaceStartupIdGeneratorState
          (IdGenerator.commitGeneratedId generatedPreparation)
          . replaceStartupControlledState controlledSuccessor
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
  Right
    ( successor,
      if Application.applicationRequestCandidateReplyDeliverable candidate
        then
          singletonEffectBatch
            ( SendApplicationReply
                (Application.applicationRequestCandidateBinding candidate)
                reply
            )
        else mempty
    )
  where
    process = Application.applicationRequestCandidateProcess candidate
    applicationPredecessor = startupApplicationState predecessor
