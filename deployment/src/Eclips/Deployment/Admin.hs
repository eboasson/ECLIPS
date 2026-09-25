{-# LANGUAGE OverloadedStrings #-}

-- | Small operator client over the public EADM framing boundary.
module Eclips.Deployment.Admin
  ( AdministrationCommand (..),
    runAdministration,
    renderAdministrationReply,
    encodePreparation,
    decodePreparation,
    encodeIdentity,
    decodeIdentity,
    decodeVoterConfiguration,
    decodeOracleNode,
    decodeVoterChange,
  )
where

import Control.Exception (bracket, onException)
import Data.Binary (decodeOrFail, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Lifecycle (ChildPreparation)
import Eclips.Deployment.Admin.Exchange (receiveAdministrationReplies)
import Eclips.Deployment.Configuration
import Eclips.Domain.Identity (controlIndexWord64, heraldEpochBytes)
import Eclips.Oracle.Canonical (canonicalOracleReceiptValue)
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Receipt (oracleReceiptControlIndex, oracleReceiptResult, oracleReceiptVoterResult)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Admin.Frame
import Eclips.Protocol.Admin.Types
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)
import Eclips.Raft.Identity (raftNodeIdBytes)
import Network.Socket (AddrInfo (..), SocketType (Stream), close, connect, defaultHints, getAddrInfo, socket)
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

data AdministrationCommand
  = InspectStatus
  | InspectPreparations
  | InspectResult
  | CancelPreparation ChildPreparation
  | Drain
  | PrepareReplica
  | CommissionVoters AdminVoterConfigurationClaim [AdminOracleNodeClaim]
  | DecommissionVoter AdminVoterConfigurationClaim AdminOracleNodeClaim
  | CancelVoterConfiguration AdminVoterChangeIdClaim
  | InspectOracleConfiguration
  | InspectVoterChange AdminVoterChangeIdClaim
  deriving stock (Eq, Show)

-- | Correlations belong to this one TCP lifetime. Wait for the semantic result,
-- transfer it to the operator, and release its RPC receipt before closing.
runAdministration :: Endpoint -> ByteString -> Word64 -> AdministrationCommand -> IO [Text]
runAdministration target system correlation command = do
  deployment <- either (ioError . userError . show) pure (adminDeploymentIdClaim system)
  addresses <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just (Text.unpack (endpointHost target))) (Just (show (endpointPort target)))
  address <- case addresses of [] -> ioError (userError "operator endpoint did not resolve"); first : _ -> pure first
  bracket (open address) close $ \connection -> do
    sendAll connection (encodeAdminFrame (AdminClientEnvelope (AdminHello deployment ProcessAdministrator)))
    request <- case command of
      InspectStatus -> pure (GetHeraldStatus claimed)
      InspectPreparations -> pure (ListChildPreparations claimed)
      InspectResult -> pure (GetAdminResult claimed)
      CancelPreparation preparation -> pure (CancelChildPreparation claimed preparation)
      Drain -> pure (DrainHerald claimed)
      PrepareReplica -> pure (PrepareOracleReplica claimed)
      CancelVoterConfiguration change -> pure (CancelVoterChange claimed change)
      InspectOracleConfiguration -> pure (GetOracleConfiguration claimed)
      InspectVoterChange change -> pure (GetVoterChangeStatus claimed change)
      CommissionVoters expected nodes -> do
        snapshot <- currentConfiguration connection
        requireExpected expected snapshot
        bindings <- commissionBindings snapshot nodes
        pure (BeginVoterChange claimed expected AdminCommissionVotersDto bindings)
      DecommissionVoter expected node -> do
        snapshot <- currentConfiguration connection
        requireExpected expected snapshot
        bindings <- currentBindings snapshot
        let desired = [binding | binding@(AdminVoterBindingDto current _) <- bindings, current /= node]
        if null desired
          then ioError (userError "cannot remove the last Oracle voter")
          else pure (BeginVoterChange claimed expected AdminDecommissionVotersDto desired)
    replies <- exchange connection request
    if any (\reply -> case reply of HeraldDrained _ -> True; _ -> False) replies
      then pure ()
      else do
        let progress = receiptRetirementPrefix (Just correlation)
        acknowledgement <- exchange connection (RetireAdminReceipts progress)
        if acknowledgement == [AdminReceiptsRetired progress]
          then pure ()
          else ioError (userError "administration receipt retirement was not acknowledged")
    pure (map renderAdministrationReply replies)
  where
    claimed = adminCorrelationIdClaim correlation
    exchange connection request = do
      sendAll connection (encodeAdminFrame (AdminClientEnvelope request))
      receiveAdministrationReplies (recv connection 4096)
    currentConfiguration connection = do
      replies <- exchange connection (GetOracleConfiguration claimed)
      case replies of
        [OracleConfiguration _ snapshot] -> pure snapshot
        _ -> ioError (userError "Oracle configuration is unavailable at the selected Herald")
    currentBindings (AdminOracleConfigurationDto _ configuration _ _) =
      case Voter.voterConfigurationView <$> configuration of
        Just (Voter.StableVoterConfigurationView bindings) -> traverse bindingDto (Voter.oracleVoterBindingList bindings)
        Just Voter.JointVoterConfigurationView {} -> ioError (userError "a joint voter change is still in progress")
        Nothing -> ioError (userError "Oracle configuration has not been applied")
    requireExpected expected (AdminOracleConfigurationDto _ configuration _ _) =
      case configuration of
        Just current | Voter.voterConfigurationIdBytes (Voter.voterConfigurationId current) == adminVoterConfigurationClaimBytes expected -> pure ()
        _ -> ioError (userError "expected configuration is not current; inspect the current voter configuration before submitting a new change")
    commissionBindings snapshot@(AdminOracleConfigurationDto _ _ registrations _) nodes = do
      current <- currentBindings snapshot
      requested <-
        traverse
          ( \node -> case [registration | registration <- registrations, raftNodeIdBytes (Voter.replicaRegistrationNode registration) == adminOracleNodeClaimBytes node] of
              [registration] -> AdminVoterBindingDto node <$> admit (adminHeraldEpochClaim (heraldEpochBytes (Voter.replicaRegistrationHost registration)))
              _ -> ioError (userError "commission names an unknown replica; prepare it first")
          )
          nodes
      pure (current <> [binding | binding@(AdminVoterBindingDto node _) <- requested, all (\(AdminVoterBindingDto retained _) -> retained /= node) current])
    bindingDto binding =
      AdminVoterBindingDto
        <$> admit (adminOracleNodeClaim (raftNodeIdBytes (raftVoterBindingNode binding)))
        <*> admit (adminHeraldEpochClaim (heraldEpochBytes (raftVoterBindingHeraldEpoch binding)))
    admit = either (ioError . userError . show) pure
    open address = do
      connection <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      connect connection (addrAddress address) `onException` close connection
      pure connection

renderAdministrationReply :: AdminServerDto -> Text
renderAdministrationReply = \case
  AdminResult _ (AdminRejected AdminOracleReplicaEndpointsNotConfiguredDto) ->
    "configure concrete Oracle and Raft advertised/listen ports before prepare-oracle"
  AdminResult _ (AdminOracleVoterResult canonical) ->
    let receipt = canonicalOracleReceiptValue canonical
     in Text.unlines
          [ "receipt-control-index " <> number (controlIndexWord64 (oracleReceiptControlIndex receipt)),
            "receipt-result " <> Text.pack (show (oracleReceiptResult receipt)),
            "receipt-voter-result " <> maybe "none" renderVoterResult (oracleReceiptVoterResult receipt)
          ]
  OracleConfiguration _ (AdminOracleConfigurationDto prefix configuration replicas pending) ->
    Text.unlines
      ( ["control-index " <> number (adminControlIndexDtoWord64 prefix)]
          <> maybe ["configuration unavailable"] renderConfiguration configuration
          <> map (("replica " <>) . renderReplica) replicas
          <> ["pending-change " <> maybe "none" renderChange pending]
      )
  VoterChangeStatus _ (AdminVoterChangeStatusDto prefix change) ->
    Text.unlines
      [ "control-index " <> number (adminControlIndexDtoWord64 prefix),
        "change " <> maybe "not-applied" renderChange change
      ]
  HeraldStatus _ (AdminHeraldStatusDto system local members voters membership control phase connected leader) ->
    Text.unlines
      [ "system " <> encodeIdentity (adminDeploymentIdClaimBytes system),
        "local-herald " <> encodeIdentity (adminHeraldEpochClaimBytes local),
        "phase " <> Text.pack (show phase),
        "control-index " <> Text.pack (show (adminControlIndexDtoWord64 control)),
        "membership-generation " <> encodeIdentity (adminMembershipGenerationClaimBytes membership),
        "active-heralds " <> Text.intercalate " " (fmap (encodeIdentity . adminHeraldEpochClaimBytes) members),
        "voter-hosts " <> Text.intercalate " " (fmap (encodeIdentity . adminHeraldEpochClaimBytes) voters),
        "connected-oracle " <> maybe "unavailable" (encodeIdentity . adminOracleNodeClaimBytes) connected,
        "oracle-leader-hint " <> maybe "unknown" (encodeIdentity . adminOracleNodeClaimBytes) leader
      ]
  ChildPreparations _ preparations -> Text.unlines (fmap showPreparation preparations)
  other -> Text.pack (show other)
  where
    number = Text.pack . show
    renderReplica registration =
      encodeIdentity (raftNodeIdBytes (Voter.replicaRegistrationNode registration))
        <> " host="
        <> encodeIdentity (heraldEpochBytes (Voter.replicaRegistrationHost registration))
    renderChange change =
      number (controlIndexWord64 (Voter.voterChangeIdControlIndex (Voter.voterChangeId change)))
        <> " "
        <> Text.pack (show (Voter.voterChangePhase change))
        <> " expected="
        <> encodeIdentity (Voter.voterConfigurationIdBytes (Voter.voterChangeExpectedConfiguration change))
    renderConfiguration configuration =
      ["configuration-id " <> encodeIdentity (Voter.voterConfigurationIdBytes (Voter.voterConfigurationId configuration))]
        <> case Voter.voterConfigurationView configuration of
          Voter.StableVoterConfigurationView bindings -> ["voters " <> renderBindings bindings]
          Voter.JointVoterConfigurationView old new -> ["old-voters " <> renderBindings old, "new-voters " <> renderBindings new]
    renderBindings = Text.intercalate " " . map (\binding -> encodeIdentity (raftNodeIdBytes (raftVoterBindingNode binding)) <> "@" <> encodeIdentity (heraldEpochBytes (raftVoterBindingHeraldEpoch binding))) . Voter.oracleVoterBindingList
    renderVoterResult = \case
      Voter.ReplicaRegistered registration -> "Registered " <> renderReplica registration
      Voter.ReplicaAlreadyVoter registration -> "AlreadyVoter " <> renderReplica registration
      Voter.ReplicaAlreadyLearner registration -> "AlreadyLearner " <> renderReplica registration
      Voter.VoterChangeAccepted change -> "Accepted " <> renderChange change
      Voter.VoterChangeCancelledResult change -> "Cancelled " <> renderChange change
    showPreparation (AdminPreparationDto reference process phase failure) =
      encodePreparation reference <> " " <> Text.pack (show phase) <> " process=" <> maybe "pending" (encodeIdentity . adminProcessEpochIdClaimBytes) process <> maybe "" ((" failure=" <>) . Text.pack . show) failure

encodePreparation :: ChildPreparation -> Text
encodePreparation = encodeIdentity . Lazy.toStrict . encode
decodePreparation :: Text -> Either String ChildPreparation
decodePreparation text = do
  bytes <- decodeIdentity text
  case decodeOrFail (Lazy.fromStrict bytes) of
    Right (remaining, _, reference) | Lazy.null remaining && encodePreparation reference == Text.strip text -> Right reference
    _ -> Left "invalid preparation reference"

decodeVoterConfiguration :: Text -> Either String AdminVoterConfigurationClaim
decodeVoterConfiguration text = decodeIdentity text >>= either (Left . show) Right . adminVoterConfigurationClaim
decodeOracleNode :: Text -> Either String AdminOracleNodeClaim
decodeOracleNode text = decodeIdentity text >>= either (Left . show) Right . adminOracleNodeClaim
decodeVoterChange :: Text -> Either String AdminVoterChangeIdClaim
decodeVoterChange text = do
  value <- maybe (Left "expected a positive voter-change integer") Right (readMaybe (Text.unpack text))
  either (Left . show) Right (adminVoterChangeIdClaim value)
encodeIdentity :: ByteString -> Text
encodeIdentity = Text.pack . concatMap hexByte . Bytes.unpack
  where
    hexByte byte = [digit (byte `div` 16), digit (byte `mod` 16)]
    digit nibble = toEnum (fromEnum (if nibble < 10 then '0' else 'a') + fromIntegral (if nibble < 10 then nibble else nibble - 10))
decodeIdentity :: Text -> Either String ByteString
decodeIdentity = fmap Bytes.pack . parse . Text.unpack . Text.strip
  where
    parse [] = Right []
    parse (high : low : rest) = do
      upper <- nibble high
      lower <- nibble low
      ((upper * 16 + lower) :) <$> parse rest
    parse [_] = Left "expected canonical hexadecimal bytes"
    nibble :: Char -> Either String Word8
    nibble character
      | character >= '0' && character <= '9' = Right (fromIntegral (ord character - ord '0'))
      | character >= 'a' && character <= 'f' = Right (fromIntegral (ord character - ord 'a' + 10))
      | otherwise = Left "expected canonical hexadecimal bytes"
