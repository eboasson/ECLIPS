module PublicAdminProtocol
  ( clientArm,
    serverArm,
    resultArm,
    completedArm,
    endReasonArm,
    rejectionArm,
    startOracleRejectionArm,
    endOracleRejectionArm,
    roleArm,
  )
where

import Eclips.Protocol.Admin.Types

clientArm :: AdminClientDto -> Int
clientArm dto = case dto of
  AdminHello _ _ -> 0
  StartProcessEpoch _ -> 1
  EndProcessEpoch _ _ _ -> 2
  GetAdminResult _ -> 3
  CancelChildPreparation _ _ -> 4
  GetHeraldStatus _ -> 5
  ListChildPreparations _ -> 6
  DrainHerald _ -> 7
  PrepareOracleReplica _ -> 8
  BeginVoterChange _ _ _ _ -> 9
  CancelVoterChange _ _ -> 10
  GetOracleConfiguration _ -> 11
  GetVoterChangeStatus _ _ -> 12
  AdminWithRetirement _ _ -> 13
  RetireAdminReceipts _ -> 14

serverArm :: AdminServerDto -> Int
serverArm dto = case dto of
  AdminResult _ _ -> 0
  AdminAbsent _ -> 1
  AdminConflict _ -> 2
  HeraldStatus _ _ -> 3
  ChildPreparations _ _ -> 4
  HeraldDrainAccepted _ -> 5
  HeraldDrained _ -> 6
  OracleConfiguration _ _ -> 7
  VoterChangeStatus _ _ -> 8
  AdminResultRetired _ _ -> 9
  AdminReceiptsRetired _ -> 10
  AdminRetirementNotReady -> 11

resultArm :: AdminResultStatusDto -> Int
resultArm status = case status of
  AdminAccepted -> 0
  AdminCompleted _ -> 1
  AdminRejected _ -> 2
  AdminPreparationCancellation _ -> 3
  AdminOracleVoterResult _ -> 4

completedArm :: AdminCompletedResultDto -> Int
completedArm completed = case completed of
  StartProcessReadyDto _ _ -> 0
  StartProcessEndedBeforeAttachmentDto _ -> 1
  ProcessEpochEndedDto _ -> 2

endReasonArm :: AdminProcessEndReason -> Int
endReasonArm reason = case reason of
  AdminExplicitAdministrativeEnd -> 0

rejectionArm :: AdminRejectedErrorDto -> Int
rejectionArm rejection = case rejection of
  StartProcessNotApplied _ -> 0
  EndProcessNotApplied _ -> 1
  AdminOracleReplicaEndpointsNotConfiguredDto -> 2

startOracleRejectionArm :: AdminStartOracleRejectionDto -> Int
startOracleRejectionArm rejection = case rejection of
  AdminStartRequestHomeEpochMismatchDto _ _ -> 0
  AdminStartInactiveHomeHeraldDto _ -> 1
  AdminStaleExpectedControlIndexDto _ _ -> 2
  AdminStartProcessResidenceMismatchDto _ _ _ -> 3
  AdminProcessEpochAlreadyStartedDto _ -> 4
  AdminProcessAlreadyStartedDto _ -> 5

endOracleRejectionArm :: AdminEndOracleRejectionDto -> Int
endOracleRejectionArm rejection = case rejection of
  AdminEndRequestHomeEpochMismatchDto _ _ -> 0
  AdminEndInactiveHomeHeraldDto _ -> 1
  AdminEndProcessUnknownDto _ -> 2
  AdminEndProcessResidenceMismatchDto _ _ _ -> 3
  AdminEndProcessAlreadyEndedDto _ -> 4

roleArm :: AdminRoleClaim -> Int
roleArm role = case role of
  ProcessAdministrator -> 0
