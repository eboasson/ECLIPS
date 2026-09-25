{-# LANGUAGE DeriveAnyClass #-}

-- | Restricted onboarding messages. A decoded request is a claim; the Herald's
-- pure join owner checks current membership and installed data before acting.
module Eclips.Herald.Join
  ( JoinRequest (..),
    JoinRequestId,
    joinRequestId,
    joinRequestOwner,
    joinRequestOrdinal,
    JoinReply (..),
    JoinStatus (..),
    HeraldOracleStatus (..),
    encodeJoinRequest,
    decodeJoinRequest,
    encodeJoinReply,
    decodeJoinReply,
    prepareJoinSeal,
    JoinHistory,
    JoinSourceHistory,
    decodeJoinSourceHistory,
    encodeJoinSourceHistory,
    sourceJoinHistory,
    joinSourceMemberCut,
    joinHistorySystem,
    joinHistoryAdmission,
    joinHistoryAttempt,
    joinHistorySource,
    joinHistoryTopologyCut,
    joinHistoryControlPrefix,
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Domain.Topology
import Eclips.Herald.Join.History
  ( JoinHistory,
    JoinSourceHistory,
    decodeJoinSourceHistory,
    encodeJoinSourceHistory,
    joinHistoryAdmission,
    joinHistoryAttempt,
    joinHistoryControlPrefix,
    joinHistorySource,
    joinHistorySystem,
    joinHistoryTopologyCut,
    joinSourceMemberCut,
    sourceJoinHistory,
  )
import Eclips.Herald.Join.Seal (prepareJoinSeal)
import Eclips.Herald.Join.State (JoinRequestId, joinRequestId, joinRequestOrdinal, joinRequestOwner)
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Voter
import Eclips.Public.Types.ReceiptRetirement
import GHC.Generics (Generic)

data JoinRequest
  = ReadJoinStatus
  | SubmitJoinCommand JoinRequestId HeraldAdmissionCommand
  | SubmitJoinCommandWithRetirement JoinRequestId HeraldAdmissionCommand ReceiptRetirement
  | ReadJoinCommand JoinRequestId
  | ReadJoinCommandWithRetirement JoinRequestId ReceiptRetirement
  | RetireJoinReceipts HeraldEpoch ReceiptRetirement
  | CaptureJoinHistory HeraldAdmissionId
  | InstallJoinHistory ByteString
  | ReadJoinControlHistory ControlIndex
  | ReadJoinReady HeraldAdmissionId
  | InstallJoinControlHistory [CanonicalAppliedOracleEntry]
  | ReadOracleStatus
  | ReadOracleVoterChange VoterChangeId
  | ReadOracleVoterHostFailure FailureProbeResolutionId
  deriving stock (Eq, Show)

data JoinStatus = JoinStatus
  { joinStatusSystem :: SystemId,
    joinStatusMember :: HeraldMember,
    joinStatusMembership :: HeraldMembershipGeneration,
    -- | The control projection, which can lead frozen semantic work.
    joinStatusControl :: ControlIndex,
    -- | The fully applied semantic projection used for onboarding suffixes.
    joinStatusAppliedControl :: ControlIndex,
    joinStatusAdmissions :: [HeraldAdmissionRecord],
    joinStatusCut :: Maybe TopologyCut,
    joinStatusServing :: Bool
  }
  deriving stock (Eq, Show)

-- | The selected owner's current applied projection. This read is not an
-- Oracle quorum-health observation and does not imply freshness elsewhere.
data HeraldOracleStatus = HeraldOracleStatus
  { heraldOracleStatusControlIndex :: ControlIndex,
    heraldOracleStatusConfiguration :: Maybe VoterConfiguration,
    heraldOracleStatusReplicas :: [OracleReplicaRegistration],
    heraldOracleStatusPendingChange :: Maybe VoterChange
  }
  deriving stock (Eq, Show)

data JoinReply
  = JoinStatusReply JoinStatus
  | JoinCommandPending
  | JoinCommandComplete [HeraldAdmissionRecord]
  | JoinHistoryReply ByteString
  | JoinHistoryInstalled
  | JoinControlHistoryReply [CanonicalAppliedOracleEntry]
  | JoinReadyReply HeraldJoinReadyReport
  | JoinRetry
  | JoinRejected
  | JoinRequestConflict
  | JoinRequestRetired JoinRequestId ReceiptRetirement
  | JoinReceiptsRetired HeraldEpoch ReceiptRetirement
  | JoinRetirementNotReady
  | JoinTransferRetired HeraldAdmissionId Word64
  | OracleStatusReply HeraldOracleStatus
  | OracleVoterChangeReply (Maybe VoterChange)
  | OracleVoterHostFailureReply (Maybe Failure.AcceptedVoterHostFailure)
  deriving stock (Eq, Show)

data RawRequest = RawRequest Word64 [ByteString]
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize)
data RawStatus = RawStatus ByteString ByteString ByteString ByteString Word64 Word64 [ByteString] (Maybe ByteString) Bool
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize)
data RawReply = RawReply Word64 (Maybe RawStatus) [ByteString]
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize)
data RawOracleStatus = RawOracleStatus Word64 (Maybe ByteString) [ByteString] (Maybe ByteString)
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize)

encodeJoinRequest :: JoinRequest -> ByteString
encodeJoinRequest = S.encode . raw
  where
    raw ReadJoinStatus = RawRequest 0 []
    raw (SubmitJoinCommand token command) = RawRequest 1 [encodeRequestId token, encodeAdmissionCommand command]
    raw (ReadJoinCommand token) = RawRequest 2 [encodeRequestId token]
    raw (SubmitJoinCommandWithRetirement token command progress) = RawRequest 10 [encodeRequestId token, encodeAdmissionCommand command, encodeProgress progress]
    raw (ReadJoinCommandWithRetirement token progress) = RawRequest 11 [encodeRequestId token, encodeProgress progress]
    raw (RetireJoinReceipts owner progress) = RawRequest 12 [heraldEpochBytes owner, encodeProgress progress]
    raw (CaptureJoinHistory admission) = RawRequest 3 [heraldAdmissionIdCanonicalBytes admission]
    raw (InstallJoinHistory bundle) = RawRequest 4 [bundle]
    raw (ReadJoinControlHistory after) = RawRequest 5 [S.encode (controlIndexWord64 after)]
    raw (ReadJoinReady admission) = RawRequest 7 [heraldAdmissionIdCanonicalBytes admission]
    raw (InstallJoinControlHistory entries) = RawRequest 6 (map canonicalAppliedOracleEntryBytes entries)
    raw ReadOracleStatus = RawRequest 8 []
    raw (ReadOracleVoterChange identifier) = RawRequest 9 [S.encode (controlIndexWord64 (voterChangeIdControlIndex identifier))]
    raw (ReadOracleVoterHostFailure resolution) = RawRequest 13 [failureProbeResolutionIdCanonicalBytes resolution]

decodeJoinRequest :: ByteString -> Either String JoinRequest
decodeJoinRequest bytes = do
  value <- S.decode bytes
  request <- case value of
    RawRequest 0 [] -> Right ReadJoinStatus
    RawRequest 1 [token, command] -> SubmitJoinCommand <$> decodeRequestId token <*> checked (decodeAdmissionCommand command)
    RawRequest 2 [token] -> ReadJoinCommand <$> decodeRequestId token
    RawRequest 10 [token, command, progress] -> SubmitJoinCommandWithRetirement <$> decodeRequestId token <*> checked (decodeAdmissionCommand command) <*> decodeProgress progress
    RawRequest 11 [token, progress] -> ReadJoinCommandWithRetirement <$> decodeRequestId token <*> decodeProgress progress
    RawRequest 12 [owner, progress] -> RetireJoinReceipts <$> checked (mkHeraldEpoch owner) <*> decodeProgress progress
    RawRequest 3 [admission] -> CaptureJoinHistory <$> checked (decodeHeraldAdmissionIdCanonicalBytes admission)
    RawRequest 4 [bundle] -> Right (InstallJoinHistory bundle)
    RawRequest 5 [after] -> ReadJoinControlHistory . controlIndex <$> S.decode after
    RawRequest 7 [admission] -> ReadJoinReady <$> checked (decodeHeraldAdmissionIdCanonicalBytes admission)
    RawRequest 6 entries -> InstallJoinControlHistory <$> traverse (checked . decodeCanonicalAppliedOracleEntry) entries
    RawRequest 8 [] -> Right ReadOracleStatus
    RawRequest 9 [identifier] -> ReadOracleVoterChange <$> (S.decode identifier >>= checked . mkVoterChangeId . controlIndex)
    RawRequest 13 [resolution] -> ReadOracleVoterHostFailure <$> checked (decodeFailureProbeResolutionIdCanonicalBytes resolution)
    _ -> Left "invalid join request"
  unless (encodeJoinRequest request == bytes) (Left "noncanonical join request")
  pure request

encodeRequestId :: JoinRequestId -> ByteString
encodeRequestId request = S.encode (heraldEpochBytes (joinRequestOwner request), joinRequestOrdinal request)
decodeRequestId :: ByteString -> Either String JoinRequestId
decodeRequestId bytes = do
  (owner, ordinal) <- S.decode bytes
  joinRequestId <$> checked (mkHeraldEpoch owner) <*> pure ordinal
encodeProgress :: ReceiptRetirement -> ByteString
encodeProgress progress = S.encode (receiptRetirementHighWater progress, Set.toAscList (receiptRetirementExceptions progress))
decodeProgress :: ByteString -> Either String ReceiptRetirement
decodeProgress bytes = do
  (high, exceptions) <- S.decode bytes
  checked (receiptRetirement high (Set.fromList exceptions))

encodeJoinReply :: JoinReply -> ByteString
encodeJoinReply = S.encode . raw
  where
    raw (JoinStatusReply status) = RawReply 0 (Just (rawStatus status)) []
    raw JoinCommandPending = RawReply 1 Nothing []
    raw (JoinCommandComplete records) = RawReply 2 Nothing (map encodeAdmissionRecord records)
    raw (JoinHistoryReply bundle) = RawReply 3 Nothing [bundle]
    raw JoinHistoryInstalled = RawReply 4 Nothing []
    raw JoinRetry = RawReply 5 Nothing []
    raw JoinRejected = RawReply 6 Nothing []
    raw JoinRequestConflict = RawReply 7 Nothing []
    raw (JoinRequestRetired token progress) = RawReply 12 Nothing [encodeRequestId token, encodeProgress progress]
    raw (JoinReceiptsRetired owner progress) = RawReply 13 Nothing [heraldEpochBytes owner, encodeProgress progress]
    raw JoinRetirementNotReady = RawReply 14 Nothing []
    raw (JoinTransferRetired admission attempt) = RawReply 15 Nothing [heraldAdmissionIdCanonicalBytes admission, S.encode attempt]
    raw (JoinControlHistoryReply entries) = RawReply 8 Nothing (map canonicalAppliedOracleEntryBytes entries)
    raw (JoinReadyReply report) = RawReply 9 Nothing [encodeAdmissionCommand (HeraldJoinReady report)]
    raw (OracleStatusReply status) =
      RawReply
        10
        Nothing
        [ S.encode
            ( RawOracleStatus
                (controlIndexWord64 (heraldOracleStatusControlIndex status))
                (encodeVoterConfiguration <$> heraldOracleStatusConfiguration status)
                (map encodeReplicaRegistration (heraldOracleStatusReplicas status))
                (encodeVoterChange <$> heraldOracleStatusPendingChange status)
            )
        ]
    raw (OracleVoterChangeReply change) = RawReply 11 Nothing (maybe [] ((: []) . encodeVoterChange) change)
    raw (OracleVoterHostFailureReply certificate) = RawReply 16 Nothing (maybe [] ((: []) . Failure.encodeAcceptedVoterHostFailure) certificate)
    rawStatus status =
      RawStatus
        (systemIdBytes (joinStatusSystem status))
        (heraldIdBytes (heraldMemberId (joinStatusMember status)))
        (heraldEpochBytes (heraldMemberEpoch (joinStatusMember status)))
        (heraldMembershipGenerationCanonicalBytes (joinStatusMembership status))
        (controlIndexWord64 (joinStatusControl status))
        (controlIndexWord64 (joinStatusAppliedControl status))
        (map encodeAdmissionRecord (joinStatusAdmissions status))
        (topologyCutCanonicalBytes <$> joinStatusCut status)
        (joinStatusServing status)

decodeJoinReply :: ByteString -> Either String JoinReply
decodeJoinReply bytes = do
  value <- S.decode bytes
  reply <- case value of
    RawReply 0 (Just status) [] -> JoinStatusReply <$> statusFromRaw status
    RawReply 1 Nothing [] -> Right JoinCommandPending
    RawReply 2 Nothing records -> JoinCommandComplete <$> traverse (checked . decodeAdmissionRecord) records
    RawReply 3 Nothing [bundle] -> Right (JoinHistoryReply bundle)
    RawReply 4 Nothing [] -> Right JoinHistoryInstalled
    RawReply 5 Nothing [] -> Right JoinRetry
    RawReply 6 Nothing [] -> Right JoinRejected
    RawReply 7 Nothing [] -> Right JoinRequestConflict
    RawReply 12 Nothing [token, progress] -> JoinRequestRetired <$> decodeRequestId token <*> decodeProgress progress
    RawReply 13 Nothing [owner, progress] -> JoinReceiptsRetired <$> checked (mkHeraldEpoch owner) <*> decodeProgress progress
    RawReply 14 Nothing [] -> Right JoinRetirementNotReady
    RawReply 15 Nothing [admission, attempt] -> JoinTransferRetired <$> checked (decodeHeraldAdmissionIdCanonicalBytes admission) <*> S.decode attempt
    RawReply 8 Nothing entries -> JoinControlHistoryReply <$> traverse (checked . decodeCanonicalAppliedOracleEntry) entries
    RawReply 9 Nothing [command] -> case decodeAdmissionCommand command of
      Right (HeraldJoinReady report) -> Right (JoinReadyReply report)
      _ -> Left "invalid join ready reply"
    RawReply 10 Nothing [encoded] -> do
      RawOracleStatus prefix configuration replicas pending <- S.decode encoded
      OracleStatusReply
        <$> ( HeraldOracleStatus (controlIndex prefix)
                <$> traverse (checked . decodeVoterConfiguration) configuration
                <*> traverse (checked . decodeReplicaRegistration) replicas
                <*> traverse (checked . decodeVoterChange) pending
            )
    RawReply 11 Nothing [] -> Right (OracleVoterChangeReply Nothing)
    RawReply 11 Nothing [change] -> OracleVoterChangeReply . Just <$> checked (decodeVoterChange change)
    RawReply 16 Nothing [] -> Right (OracleVoterHostFailureReply Nothing)
    RawReply 16 Nothing [certificate] -> OracleVoterHostFailureReply . Just <$> checked (Failure.decodeAcceptedVoterHostFailure certificate)
    _ -> Left "invalid join reply"
  unless (encodeJoinReply reply == bytes) (Left "noncanonical join reply")
  pure reply
  where
    statusFromRaw (RawStatus system ident epoch membership prefix applied records cut serving) = do
      unless (applied <= prefix) (Left "applied Join control exceeds observed control")
      JoinStatus
        <$> checked (mkSystemId system)
        <*> (HeraldMember <$> checked (mkHeraldId ident) <*> checked (mkHeraldEpoch epoch))
        <*> checked (decodeHeraldMembershipGenerationCanonicalBytes membership)
        <*> pure (controlIndex prefix)
        <*> pure (controlIndex applied)
        <*> traverse (checked . decodeAdmissionRecord) records
        <*> traverse (checked . decodeTopologyCutCanonicalBytes) cut
        <*> pure serving

checked :: (Show problem) => Either problem value -> Either String value
checked = either (Left . show) Right
