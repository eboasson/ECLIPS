-- | Exhaustive structural conversion between Start/End administration DTO claims
-- and Herald-owned values.
--
-- Claim conversion establishes nominal byte shape only.  Deployment
-- authentication, binding freshness, configured residence, and retained
-- correlation semantics remain in the Herald administration transition.
module Eclips.Herald.Administration.RPC.Internal
  ( AdministrationRpcClaimError (..),
    deploymentFromClaim,
    deploymentToClaim,
    processEpochFromClaim,
    processEpochToClaim,
    correlationFromClaim,
    correlationToClaim,
    adminReplyToDto,
  )
where

import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership (heraldMembershipGenerationIdBytes)
import Eclips.Herald.Administration.Internal qualified as Administration
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.OracleClient (oracleNodeClaimBytes)
import Eclips.Oracle.Receipt qualified as Oracle
import Eclips.Protocol.Admin.Types qualified as Protocol

-- | A structurally checked protocol claim contradicted an owner nominal type.
-- All current identities share the same checked width, so this denotes an
-- adapter invariant fault rather than a stateful request rejection.
data AdministrationRpcClaimError
  = AdministrationRpcClaimShapeContradiction
  deriving stock (Eq, Show)

deploymentFromClaim ::
  Protocol.AdminDeploymentIdClaim ->
  Either AdministrationRpcClaimError Identity.SystemId
deploymentFromClaim =
  ownerIdentity
    . Identity.mkSystemId
    . Protocol.adminDeploymentIdClaimBytes

deploymentToClaim ::
  Identity.SystemId ->
  Either AdministrationRpcClaimError Protocol.AdminDeploymentIdClaim
deploymentToClaim =
  protocolIdentity
    . Protocol.adminDeploymentIdClaim
    . Identity.systemIdBytes

correlationFromClaim ::
  Protocol.AdminCorrelationIdClaim ->
  Administration.AdminCorrelationId
correlationFromClaim =
  Administration.adminCorrelationId
    . Protocol.adminCorrelationIdClaimWord64

correlationToClaim ::
  Administration.AdminCorrelationId ->
  Protocol.AdminCorrelationIdClaim
correlationToClaim =
  Protocol.adminCorrelationIdClaim
    . Administration.adminCorrelationIdWord64

adminReplyToDto ::
  Administration.AdminReply ->
  Either AdministrationRpcClaimError Protocol.AdminServerDto
adminReplyToDto = \case
  Administration.RetiredAdminResult correlation progress -> pure (Protocol.AdminResultRetired (correlationToClaim correlation) progress)
  Administration.AdministrationReceiptsRetired progress -> pure (Protocol.AdminReceiptsRetired progress)
  Administration.AdministrationReceiptRetirementNotReady -> pure Protocol.AdminRetirementNotReady
  Administration.AdminOracleConfigurationReply correlation index configuration replicas pending ->
    pure (Protocol.OracleConfiguration (correlationToClaim correlation) (Protocol.AdminOracleConfigurationDto (controlIndexToDto index) configuration replicas pending))
  Administration.AdminVoterChangeStatusReply correlation index change ->
    pure (Protocol.VoterChangeStatus (correlationToClaim correlation) (Protocol.AdminVoterChangeStatusDto (controlIndexToDto index) change))
  Administration.AdminHeraldStatusReply correlation status ->
    Protocol.HeraldStatus (correlationToClaim correlation) <$> heraldStatusToDto status
  Administration.AdminPreparationsReply correlation preparations ->
    Protocol.ChildPreparations (correlationToClaim correlation) <$> traverse preparationToDto preparations
  Administration.AdminDrainAccepted correlation ->
    pure (Protocol.HeraldDrainAccepted (correlationToClaim correlation))
  Administration.RetainedAdminResult correlation status ->
    Protocol.AdminResult
      (correlationToClaim correlation)
      <$> resultStatusToDto status
  Administration.AbsentAdminResult correlation ->
    pure (Protocol.AdminAbsent (correlationToClaim correlation))
  Administration.ConflictingAdminResult correlation ->
    pure (Protocol.AdminConflict (correlationToClaim correlation))

heraldStatusToDto :: Administration.AdminHeraldStatus -> Either AdministrationRpcClaimError Protocol.AdminHeraldStatusDto
heraldStatusToDto (Administration.AdminHeraldStatus system local members voters generation index phase connected hint) =
  Protocol.AdminHeraldStatusDto
    <$> deploymentToClaim system
    <*> heraldEpochToClaim local
    <*> traverse heraldEpochToClaim members
    <*> traverse heraldEpochToClaim voters
    <*> protocolIdentity (Protocol.adminMembershipGenerationClaim (heraldMembershipGenerationIdBytes generation))
    <*> pure (controlIndexToDto index)
    <*> pure (case phase of Administration.AdminServing -> Protocol.AdminServingDto; Administration.AdminDraining -> Protocol.AdminDrainingDto; Administration.AdminStopped -> Protocol.AdminStoppedDto; Administration.AdminIsolationDrain -> Protocol.AdminIsolationDrainDto; Administration.AdminIsolated -> Protocol.AdminIsolatedDto)
    <*> traverse (protocolIdentity . Protocol.adminOracleNodeClaim . oracleNodeClaimBytes) connected
    <*> traverse (protocolIdentity . Protocol.adminOracleNodeClaim . oracleNodeClaimBytes) hint

preparationToDto :: Administration.AdminPreparationInspection -> Either AdministrationRpcClaimError Protocol.AdminPreparationDto
preparationToDto (Administration.AdminPreparationInspection reference process phase failure) =
  Protocol.AdminPreparationDto reference
    <$> traverse processEpochToClaim process
    <*> pure (case phase of Administration.AdminPreparationPreparing -> Protocol.AdminPreparationPreparingDto; Administration.AdminPreparationPrepared -> Protocol.AdminPreparationPreparedDto; Administration.AdminPreparationAttached -> Protocol.AdminPreparationAttachedDto; Administration.AdminPreparationCancelling -> Protocol.AdminPreparationCancellingDto; Administration.AdminPreparationTerminal -> Protocol.AdminPreparationTerminalDto)
    <*> pure failure

resultStatusToDto ::
  Administration.AdminResultStatus ->
  Either AdministrationRpcClaimError Protocol.AdminResultStatusDto
resultStatusToDto = \case
  Administration.AdminRequestAccepted -> pure Protocol.AdminAccepted
  Administration.AdminOracleVoterResult receipt -> pure (Protocol.AdminOracleVoterResult receipt)
  Administration.AdminStartRequestCompleted process attachment ->
    Protocol.AdminCompleted
      <$> ( Protocol.StartProcessReadyDto
              <$> processEpochToClaim process
              <*> attachmentToClaim attachment
          )
  Administration.AdminStartRequestEndedBeforeAttachment process ->
    Protocol.AdminCompleted
      . Protocol.StartProcessEndedBeforeAttachmentDto
      <$> processEpochToClaim process
  Administration.AdminEndRequestCompleted process ->
    Protocol.AdminCompleted
      . Protocol.ProcessEpochEndedDto
      <$> processEpochToClaim process
  Administration.AdminPreparationCancellation status -> pure (Protocol.AdminPreparationCancellation status)
  Administration.AdminRequestRejected rejection ->
    Protocol.AdminRejected <$> startErrorToDto rejection

startErrorToDto ::
  Administration.AdminError ->
  Either AdministrationRpcClaimError Protocol.AdminRejectedErrorDto
startErrorToDto = \case
  Administration.AdminOracleReplicaEndpointsNotConfigured -> pure Protocol.AdminOracleReplicaEndpointsNotConfiguredDto
  Administration.AdminStartProcessNotApplied _ rejection ->
    Protocol.StartProcessNotApplied <$> startOracleRejectionToDto rejection
  Administration.AdminEndProcessNotApplied _ rejection ->
    Protocol.EndProcessNotApplied <$> endOracleRejectionToDto rejection

startOracleRejectionToDto ::
  Oracle.OracleRejection ->
  Either AdministrationRpcClaimError Protocol.AdminStartOracleRejectionDto
startOracleRejectionToDto = \case
  Oracle.RequestHomeEpochMismatch requestHome envelopeHome ->
    Protocol.AdminStartRequestHomeEpochMismatchDto
      <$> heraldEpochToClaim requestHome
      <*> heraldEpochToClaim envelopeHome
  Oracle.InactiveHomeHerald home ->
    Protocol.AdminStartInactiveHomeHeraldDto <$> heraldEpochToClaim home
  Oracle.StaleExpectedControlIndex expected actual ->
    pure
      ( Protocol.AdminStaleExpectedControlIndexDto
          (controlIndexToDto expected)
          (controlIndexToDto actual)
      )
  Oracle.StartProcessResidenceMismatch process expected actual ->
    Protocol.AdminStartProcessResidenceMismatchDto
      <$> processEpochToClaim process
      <*> heraldEpochToClaim expected
      <*> heraldEpochToClaim actual
  Oracle.ProcessEpochAlreadyStarted process ->
    Protocol.AdminProcessEpochAlreadyStartedDto <$> processEpochToClaim process
  Oracle.ProcessAlreadyStarted process ->
    Protocol.AdminProcessAlreadyStartedDto <$> processToClaim process
  Oracle.EndProcessUnknown {} -> Left AdministrationRpcClaimShapeContradiction
  Oracle.EndProcessResidenceMismatch {} -> Left AdministrationRpcClaimShapeContradiction
  Oracle.EndProcessAlreadyEnded {} -> Left AdministrationRpcClaimShapeContradiction
  _ -> Left AdministrationRpcClaimShapeContradiction

endOracleRejectionToDto ::
  Oracle.OracleRejection ->
  Either AdministrationRpcClaimError Protocol.AdminEndOracleRejectionDto
endOracleRejectionToDto = \case
  Oracle.RequestHomeEpochMismatch requestHome envelopeHome ->
    Protocol.AdminEndRequestHomeEpochMismatchDto
      <$> heraldEpochToClaim requestHome
      <*> heraldEpochToClaim envelopeHome
  Oracle.InactiveHomeHerald home ->
    Protocol.AdminEndInactiveHomeHeraldDto <$> heraldEpochToClaim home
  Oracle.EndProcessUnknown process ->
    Protocol.AdminEndProcessUnknownDto <$> processEpochToClaim process
  Oracle.EndProcessResidenceMismatch process supplied expected ->
    Protocol.AdminEndProcessResidenceMismatchDto
      <$> processEpochToClaim process
      <*> heraldEpochToClaim supplied
      <*> heraldEpochToClaim expected
  Oracle.EndProcessAlreadyEnded process ->
    Protocol.AdminEndProcessAlreadyEndedDto <$> processEpochToClaim process
  Oracle.StaleExpectedControlIndex {} -> Left AdministrationRpcClaimShapeContradiction
  Oracle.StartProcessResidenceMismatch {} -> Left AdministrationRpcClaimShapeContradiction
  Oracle.ProcessEpochAlreadyStarted {} -> Left AdministrationRpcClaimShapeContradiction
  Oracle.ProcessAlreadyStarted {} -> Left AdministrationRpcClaimShapeContradiction
  _ -> Left AdministrationRpcClaimShapeContradiction

processEpochFromClaim ::
  Protocol.AdminProcessEpochIdClaim ->
  Either AdministrationRpcClaimError Identity.ProcessEpochId
processEpochFromClaim =
  ownerIdentity
    . Identity.mkProcessEpochId
    . Protocol.adminProcessEpochIdClaimBytes

processEpochToClaim ::
  Identity.ProcessEpochId ->
  Either AdministrationRpcClaimError Protocol.AdminProcessEpochIdClaim
processEpochToClaim =
  protocolIdentity
    . Protocol.adminProcessEpochIdClaim
    . Identity.processEpochIdBytes

processToClaim ::
  Identity.ProcessId ->
  Either AdministrationRpcClaimError Protocol.AdminProcessIdClaim
processToClaim =
  protocolIdentity
    . Protocol.adminProcessIdClaim
    . Identity.processIdBytes

heraldEpochToClaim ::
  Identity.HeraldEpoch ->
  Either AdministrationRpcClaimError Protocol.AdminHeraldEpochClaim
heraldEpochToClaim =
  protocolIdentity
    . Protocol.adminHeraldEpochClaim
    . Identity.heraldEpochBytes

attachmentToClaim ::
  Session.ApplicationAttachment ->
  Either AdministrationRpcClaimError Protocol.AdminApplicationAttachmentClaim
attachmentToClaim =
  protocolIdentity
    . Protocol.adminApplicationAttachmentClaim
    . Session.applicationAttachmentBytes

controlIndexToDto :: Identity.ControlIndex -> Protocol.AdminControlIndexDto
controlIndexToDto =
  Protocol.adminControlIndexDto . Identity.controlIndexWord64

ownerIdentity ::
  Either error value ->
  Either AdministrationRpcClaimError value
ownerIdentity =
  either
    (const (Left AdministrationRpcClaimShapeContradiction))
    Right

protocolIdentity ::
  Either Protocol.AdminClaimError value ->
  Either AdministrationRpcClaimError value
protocolIdentity =
  either
    (const (Left AdministrationRpcClaimShapeContradiction))
    Right
