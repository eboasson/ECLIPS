{-# LANGUAGE OverloadedStrings #-}

-- | Checked physical endpoint descriptions. Ports are expanded only by the
-- command-line shorthand; runtime/protocol users receive each plane explicitly.
module Eclips.Deployment.Configuration
  ( Endpoint,
    endpoint,
    endpointHost,
    endpointPort,
    parseEndpoint,
    renderEndpoint,
    DeploymentEndpoints,
    deploymentEndpoints,
    deploymentEndpointsFromPlanes,
    peerListen,
    applicationListen,
    administrationListen,
    oracleListen,
    raftListen,
    applicationAdvertised,
    peerAdvertised,
    administrationAdvertised,
    oracleAdvertised,
    raftAdvertised,
  )
where

import Data.Binary (Binary (..))
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16)
import Text.Read (readMaybe)

data Endpoint = Endpoint Text Word16
  deriving stock (Eq, Ord, Show)

endpoint :: Text -> Word16 -> Either String Endpoint
endpoint host port
  | Text.null host = Left "TCP endpoint host is empty"
  | otherwise = Right (Endpoint host port)
endpointHost :: Endpoint -> Text
endpointHost (Endpoint host _) = host
endpointPort :: Endpoint -> Word16
endpointPort (Endpoint _ port) = port

instance Binary Endpoint where
  put (Endpoint host port) = put host >> put port
  get = do
    host <- get
    port <- get
    either fail pure (endpoint host port)

parseEndpoint :: Text -> Either String Endpoint
parseEndpoint value = do
  let (host, portText) = Text.breakOnEnd ":" value
      stripBrackets raw = case (Text.stripPrefix "[" raw >>= Text.stripSuffix "]") of Just inner -> inner; Nothing -> raw
  if Text.null host then Left "expected HOST:PORT" else pure ()
  port <- maybe (Left "TCP port must be between 0 and 65535") Right (readMaybe (Text.unpack portText) :: Maybe Integer)
  if port < 0 || port > 65535 then Left "TCP port must be between 0 and 65535" else endpoint (stripBrackets (Text.dropEnd 1 host)) (fromInteger port)

renderEndpoint :: Endpoint -> Text
renderEndpoint (Endpoint host port) = shownHost <> ":" <> Text.pack (show port)
  where
    shownHost = if Text.any (== ':') host then "[" <> host <> "]" else host

data DeploymentEndpoints = DeploymentEndpoints Endpoint Endpoint Endpoint Endpoint Endpoint (Maybe Endpoint) (Maybe Endpoint) (Maybe Endpoint) (Maybe Endpoint) (Maybe Endpoint)
  deriving stock (Eq, Show)

-- | Base zero requests five independently assigned ephemeral ports. Explicit
-- advertisement requires fixed ports and never guesses an ephemeral assignment.
deploymentEndpoints :: Text -> Word16 -> Maybe Text -> Either String DeploymentEndpoints
deploymentEndpoints host base advertised = do
  if base /= 0 && base > 65531 then Left "five-plane base port must be at most 65531" else pure ()
  if base == 0 && advertised /= Nothing then Left "explicit advertisement requires fixed ports" else pure ()
  let port offset = if base == 0 then 0 else base + offset
  peer <- endpoint host (port 0)
  application <- endpoint host (port 1)
  administration <- endpoint host (port 2)
  oracle <- endpoint host (port 3)
  raft <- endpoint host (port 4)
  advertisedApplication <- traverse (\name -> endpoint name (port 1)) advertised
  advertisedPeer <- traverse (\name -> endpoint name (port 0)) advertised
  advertisedAdministration <- traverse (\name -> endpoint name (port 2)) advertised
  advertisedOracle <- traverse (\name -> endpoint name (port 3)) advertised
  advertisedRaft <- traverse (\name -> endpoint name (port 4)) advertised
  deploymentEndpointsFromPlanes peer application administration oracle raft advertisedApplication advertisedPeer advertisedAdministration advertisedOracle advertisedRaft

deploymentEndpointsFromPlanes :: Endpoint -> Endpoint -> Endpoint -> Endpoint -> Endpoint -> Maybe Endpoint -> Maybe Endpoint -> Maybe Endpoint -> Maybe Endpoint -> Maybe Endpoint -> Either String DeploymentEndpoints
deploymentEndpointsFromPlanes peer application administration oracle raft advertisedApplication advertisedPeer advertisedAdministration advertisedOracle advertisedRaft = do
  let fixed = filter ((/= 0) . endpointPort) [peer, application, administration, oracle, raft]
  if length fixed /= length (nub fixed) then Left "local protocol listeners conflict" else pure ()
  if any ((== 0) . endpointPort) (concatMap (maybe [] pure) [advertisedApplication, advertisedPeer, advertisedAdministration, advertisedOracle, advertisedRaft])
    then Left "advertised TCP endpoint must have a nonzero port"
    else pure (DeploymentEndpoints peer application administration oracle raft advertisedApplication advertisedPeer advertisedAdministration advertisedOracle advertisedRaft)

instance Binary DeploymentEndpoints where
  put (DeploymentEndpoints peer application administration oracle raft advertisedApplication advertisedPeer advertisedAdministration advertisedOracle advertisedRaft) = put peer >> put application >> put administration >> put oracle >> put raft >> put advertisedApplication >> put advertisedPeer >> put advertisedAdministration >> put advertisedOracle >> put advertisedRaft
  get = do
    peer <- get
    application <- get
    administration <- get
    oracle <- get
    raft <- get
    advertisedApplication <- get
    advertisedPeer <- get
    advertisedAdministration <- get
    advertisedOracle <- get
    advertisedRaft <- get
    either fail pure (deploymentEndpointsFromPlanes peer application administration oracle raft advertisedApplication advertisedPeer advertisedAdministration advertisedOracle advertisedRaft)

peerListen, applicationListen, administrationListen, oracleListen, raftListen :: DeploymentEndpoints -> Endpoint
peerListen (DeploymentEndpoints value _ _ _ _ _ _ _ _ _) = value
applicationListen (DeploymentEndpoints _ value _ _ _ _ _ _ _ _) = value
administrationListen (DeploymentEndpoints _ _ value _ _ _ _ _ _ _) = value
oracleListen (DeploymentEndpoints _ _ _ value _ _ _ _ _ _) = value
raftListen (DeploymentEndpoints _ _ _ _ value _ _ _ _ _) = value
applicationAdvertised, peerAdvertised, administrationAdvertised, oracleAdvertised, raftAdvertised :: DeploymentEndpoints -> Maybe Endpoint
applicationAdvertised (DeploymentEndpoints _ _ _ _ _ value _ _ _ _) = value
peerAdvertised (DeploymentEndpoints _ _ _ _ _ _ value _ _ _) = value
administrationAdvertised (DeploymentEndpoints _ _ _ _ _ _ _ value _ _) = value
oracleAdvertised (DeploymentEndpoints _ _ _ _ _ _ _ _ value _) = value
raftAdvertised (DeploymentEndpoints _ _ _ _ _ _ _ _ _ value) = value
