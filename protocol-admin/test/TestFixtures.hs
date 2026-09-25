{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( deploymentClaim,
    processEpochClaim,
    processClaim,
    heraldClaim,
    otherHeraldClaim,
    attachmentClaim,
    correlationClaim,
    voterConfigurationClaim,
    voterChangeClaim,
    oracleNode,
    clientEnvelopes,
    serverEnvelopes,
    allEnvelopes,
  )
where

import Data.ByteString qualified as ByteString
import Data.Serialize qualified as S
import Data.Serialize.Put qualified as SP
import Data.Word (Word16, Word64, Word8)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Oracle.Canonical (CanonicalOracleReceipt, decodeCanonicalOracleReceipt)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Admin.Types
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)

deploymentClaim :: AdminDeploymentIdClaim
deploymentClaim = mustAdmit (adminDeploymentIdClaim (scope 1))

processEpochClaim :: AdminProcessEpochIdClaim
processEpochClaim = mustAdmit (adminProcessEpochIdClaim (scope 3))

processClaim :: AdminProcessIdClaim
processClaim = mustAdmit (adminProcessIdClaim (scope 4))

heraldClaim :: AdminHeraldEpochClaim
heraldClaim = mustAdmit (adminHeraldEpochClaim (scope 5))

otherHeraldClaim :: AdminHeraldEpochClaim
otherHeraldClaim = mustAdmit (adminHeraldEpochClaim (scope 6))

attachmentClaim :: AdminApplicationAttachmentClaim
attachmentClaim = mustAdmit (adminApplicationAttachmentClaim (scope 7))

correlationClaim :: AdminCorrelationIdClaim
correlationClaim = adminCorrelationIdClaim 0

clientEnvelopes :: [AdminEnvelope]
clientEnvelopes =
  fmap
    AdminClientEnvelope
    [ AdminHello deploymentClaim ProcessAdministrator,
      StartProcessEpoch correlationClaim,
      EndProcessEpoch correlationClaim processEpochClaim AdminExplicitAdministrativeEnd,
      GetAdminResult correlationClaim,
      CancelChildPreparation correlationClaim preparationReference,
      GetHeraldStatus correlationClaim,
      ListChildPreparations correlationClaim,
      DrainHerald correlationClaim,
      PrepareOracleReplica correlationClaim,
      BeginVoterChange correlationClaim voterConfigurationClaim AdminCommissionVotersDto [AdminVoterBindingDto oracleNode heraldClaim],
      BeginVoterChange correlationClaim voterConfigurationClaim AdminDecommissionVotersDto [],
      CancelVoterChange correlationClaim voterChangeClaim,
      GetOracleConfiguration correlationClaim,
      GetVoterChangeStatus correlationClaim voterChangeClaim,
      AdminWithRetirement (receiptRetirementPrefix (Just 0)) (GetHeraldStatus (adminCorrelationIdClaim 1)),
      RetireAdminReceipts (receiptRetirementPrefix (Just 0)),
      RetireAdminReceipts mempty
    ]

serverEnvelopes :: [AdminEnvelope]
serverEnvelopes =
  fmap
    AdminServerEnvelope
    ( fmap (AdminResult correlationClaim) resultStatuses
        <> [ AdminAbsent correlationClaim,
             AdminResultRetired correlationClaim (receiptRetirementPrefix (Just 0)),
             AdminReceiptsRetired (receiptRetirementPrefix (Just 0)),
             AdminRetirementNotReady,
             AdminConflict correlationClaim,
             HeraldDrainAccepted correlationClaim,
             HeraldDrained correlationClaim,
             ChildPreparations correlationClaim [],
             ChildPreparations correlationClaim preparationSnapshots,
             OracleConfiguration correlationClaim (AdminOracleConfigurationDto (adminControlIndexDto 0) Nothing [] Nothing),
             OracleConfiguration correlationClaim (AdminOracleConfigurationDto (adminControlIndexDto 1) (Just voterConfiguration) [replicaRegistration] (Just voterChange)),
             VoterChangeStatus correlationClaim (AdminVoterChangeStatusDto (adminControlIndexDto 0) Nothing),
             VoterChangeStatus correlationClaim (AdminVoterChangeStatusDto (adminControlIndexDto 1) (Just voterChange))
           ]
        <> [ HeraldStatus correlationClaim (AdminHeraldStatusDto deploymentClaim heraldClaim [heraldClaim, otherHeraldClaim] [heraldClaim] (mustAdmit (adminMembershipGenerationClaim (scope 10))) (adminControlIndexDto 9) phase connected leader)
           | phase <- [AdminServingDto, AdminDrainingDto, AdminStoppedDto, AdminIsolationDrainDto, AdminIsolatedDto],
             connected <- [Nothing, Just oracleNode],
             leader <- [Nothing, Just oracleNode]
           ]
    )

preparationReference :: Lifecycle.ChildPreparation
preparationReference = mustAdmit (Lifecycle.childPreparation (scope 8) 1 2)

oracleNode :: AdminOracleNodeClaim
oracleNode = mustAdmit (adminOracleNodeClaim (scope 9))

preparationSnapshots :: [AdminPreparationDto]
preparationSnapshots =
  [ AdminPreparationDto preparationReference process phase failure
  | process <- [Nothing, Just processEpochClaim],
    phase <- [AdminPreparationPreparingDto, AdminPreparationPreparedDto, AdminPreparationAttachedDto, AdminPreparationCancellingDto, AdminPreparationTerminalDto],
    failure <- [Nothing, Just Lifecycle.StartupNotLive, Just (Lifecycle.StartupRequiredObjectUnavailable "reader")]
  ]

allEnvelopes :: [AdminEnvelope]
allEnvelopes = clientEnvelopes <> serverEnvelopes

resultStatuses :: [AdminResultStatusDto]
resultStatuses =
  [ AdminOracleVoterResult voterReceipt,
    AdminRejected AdminOracleReplicaEndpointsNotConfiguredDto,
    AdminPreparationCancellation (Lifecycle.LifecyclePending []),
    AdminPreparationCancellation (Lifecycle.LifecycleCompleted Lifecycle.ChildCancelled),
    AdminPreparationCancellation (Lifecycle.LifecycleCompleted Lifecycle.ChildAlreadyAttached),
    AdminPreparationCancellation (Lifecycle.LifecycleRejected Lifecycle.LifecycleUnknownPreparation),
    AdminAccepted,
    AdminCompleted (StartProcessReadyDto processEpochClaim attachmentClaim),
    AdminCompleted (StartProcessEndedBeforeAttachmentDto processEpochClaim),
    AdminCompleted (ProcessEpochEndedDto processEpochClaim)
  ]
    <> fmap (AdminRejected . StartProcessNotApplied) startOracleRejections
    <> fmap (AdminRejected . EndProcessNotApplied) endOracleRejections

startOracleRejections :: [AdminStartOracleRejectionDto]
startOracleRejections =
  [ AdminStartRequestHomeEpochMismatchDto heraldClaim otherHeraldClaim,
    AdminStartInactiveHomeHeraldDto heraldClaim,
    AdminStaleExpectedControlIndexDto (adminControlIndexDto 0) (adminControlIndexDto 1),
    AdminStartProcessResidenceMismatchDto processEpochClaim heraldClaim otherHeraldClaim,
    AdminProcessEpochAlreadyStartedDto processEpochClaim,
    AdminProcessAlreadyStartedDto processClaim
  ]

endOracleRejections :: [AdminEndOracleRejectionDto]
endOracleRejections =
  [ AdminEndRequestHomeEpochMismatchDto heraldClaim otherHeraldClaim,
    AdminEndInactiveHomeHeraldDto heraldClaim,
    AdminEndProcessUnknownDto processEpochClaim,
    AdminEndProcessResidenceMismatchDto processEpochClaim heraldClaim otherHeraldClaim,
    AdminEndProcessAlreadyEndedDto processEpochClaim
  ]

scope :: Word8 -> ByteString.ByteString
scope value = ByteString.replicate 32 value

mustAdmit :: (Show error) => Either error value -> value
mustAdmit = either (error . ("invalid admin protocol fixture: " <>) . show) id

voterConfigurationClaim :: AdminVoterConfigurationClaim
voterConfigurationClaim = mustAdmit (adminVoterConfigurationClaim (Voter.voterConfigurationIdBytes (Voter.voterConfigurationId voterConfiguration)))

voterChangeClaim :: AdminVoterChangeIdClaim
voterChangeClaim = mustAdmit (adminVoterChangeIdClaim 1)

-- Canonical core values are decoded from small explicit transcripts. EADM must
-- preserve them exactly, and recheck their canonical shape when receiving them.
voterConfiguration :: Voter.VoterConfiguration
voterConfiguration =
  mustAdmit
    ( Voter.decodeVoterConfiguration
        ( S.encode
            (Nothing :: Maybe (Word64, Word64), ([(scope 9, scope 5)], Nothing :: Maybe [(ByteString.ByteString, ByteString.ByteString)]), Nothing :: Maybe ByteString.ByteString, 0 :: Word64)
        )
    )

replicaRegistration :: Voter.OracleReplicaRegistration
replicaRegistration =
  mustAdmit
    ( Voter.decodeReplicaRegistration
        ( S.encode
            (scope 9, scope 5, Nothing :: Maybe ((ByteString.ByteString, Word16), (ByteString.ByteString, Word16), (ByteString.ByteString, Word16)), 0 :: Word64)
        )
    )

voterChange :: Voter.VoterChange
voterChange =
  mustAdmit
    ( Voter.decodeVoterChange
        ( S.encode
            -- The reason tag distinguishes explicit administration from an
            -- accepted voter-host failure before the explicit reason payload.
            (1 :: Word64, Voter.voterConfigurationIdBytes (Voter.voterConfigurationId voterConfiguration), [(scope 9, scope 5)], [(scope 9, scope 5), (scope 10, scope 6)], (0 :: Word8, Voter.ExplicitCommission), Voter.VoterChangePreparing, 1 :: Word64)
        )
    )

voterReceipt :: CanonicalOracleReceipt
voterReceipt =
  mustAdmit
    ( decodeCanonicalOracleReceipt
        ( SP.runPut $ do
            framed "ECLIPS-ORACLE-RECEIPT"
            SP.putByteString (scope 5)
            SP.putWord64be 1
            SP.putByteString (scope 12)
            SP.putWord64be 1
            SP.putWord8 1
            SP.putWord8 43
            framed (S.encode Voter.VoterUnknownReplica)
        )
    )
  where
    framed bytes = SP.putWord64be (fromIntegral (ByteString.length bytes)) >> SP.putByteString bytes
