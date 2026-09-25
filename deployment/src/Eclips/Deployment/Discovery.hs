{-# LANGUAGE OverloadedStrings #-}

-- | Restricted discovery is a cache of checked shapes and address hints. Only
-- the Herald and Oracle owners may turn an onboarding claim into authority.
module Eclips.Deployment.Discovery
  ( DiscoveryContact,
    discoveryContact,
    discoveryContactMember,
    discoveryContactLocators,
    discoveryPeerSeedLocators,
    DiscoveryOracleContact,
    discoveryOracleContact,
    discoveryOracleNode,
    discoveryOracleLocator,
    DiscoverySnapshot,
    discoverySnapshot,
    discoveryBootstrap,
    discoveryMembership,
    discoveryControlPrefix,
    discoveryContacts,
    discoveryOracleContacts,
    DiscoveryRequest (..),
    DiscoveryResponse (..),
    encodeDiscoveryRequest,
    decodeDiscoveryRequest,
    encodeDiscoveryResponse,
    decodeDiscoveryResponse,
    DiscoveryCache,
    initialDiscoveryCache,
    mergeDiscoverySnapshot,
    discoveryCacheSnapshot,
    discoveryCacheContacts,
    discoveryCacheLocators,
    discoverSystem,
    discoverSystemWith,
    refreshDiscovery,
    exchangeDiscoveryRequest,
    retryOnboarding,
    serveDeploymentDiscovery,
    lookupReplicaRegistration,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Deployment.Configuration (Endpoint, endpointHost, endpointPort)
import Eclips.Deployment.Manifest (CheckedDeployment, decodeDeployment, encodeDeployment)
import Eclips.Domain.Identity
  ( ControlIndex,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    heraldIdBytes,
    mkHeraldEpoch,
    mkHeraldId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Runtime.TCP (exchangeHeraldDiscovery, resolvedTcpEndpoint)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId, raftNodeIdBytes)

-- | An observed epoch and locators; there is deliberately no active flag.
data DiscoveryContact = DiscoveryContact HeraldMember (Set Endpoint)
  deriving stock (Eq, Ord, Show)

discoveryContact :: HeraldMember -> NonEmpty Endpoint -> Either String DiscoveryContact
discoveryContact member locators
  | any ((== 0) . endpointPort) locators = Left "discovery contact port is zero"
  | otherwise = Right (DiscoveryContact member (Set.fromList (NonEmpty.toList locators)))
discoveryContactMember :: DiscoveryContact -> HeraldMember
discoveryContactMember (DiscoveryContact member _) = member
discoveryContactLocators :: DiscoveryContact -> Set Endpoint
discoveryContactLocators (DiscoveryContact _ locators) = locators

-- | Select routes for a resident's identity-free peer dialers. Match the peer
-- Hello self-identity check: the local Herald's historical epochs remain
-- useful discovery information, but none of them is a peer seed.
discoveryPeerSeedLocators :: HeraldMember -> [DiscoveryContact] -> Set Endpoint
discoveryPeerSeedLocators local contacts =
  Set.unions
    [ discoveryContactLocators contact
    | contact <- contacts,
      let member = discoveryContactMember contact,
      heraldMemberId member /= heraldMemberId local,
      heraldMemberEpoch member /= heraldMemberEpoch local
    ]

instance Binary DiscoveryContact where
  put (DiscoveryContact (HeraldMember identifier epoch) locators) = put (heraldIdBytes identifier) >> put (heraldEpochBytes epoch) >> put (Set.toAscList locators)
  get = do
    identifier <- get >>= either (fail . show) pure . mkHeraldId
    epoch <- get >>= either (fail . show) pure . mkHeraldEpoch
    endpoints <- get
    locators <- maybe (fail "empty discovery contact") pure (NonEmpty.nonEmpty endpoints)
    either fail pure (discoveryContact (HeraldMember identifier epoch) locators)

data DiscoveryOracleContact = DiscoveryOracleContact RaftNodeId Endpoint
  deriving stock (Eq, Ord, Show)

discoveryOracleContact :: RaftNodeId -> Endpoint -> Either String DiscoveryOracleContact
discoveryOracleContact node locator
  | endpointPort locator == 0 = Left "Oracle contact port is zero"
  | otherwise = Right (DiscoveryOracleContact node locator)
discoveryOracleNode :: DiscoveryOracleContact -> RaftNodeId
discoveryOracleNode (DiscoveryOracleContact node _) = node
discoveryOracleLocator :: DiscoveryOracleContact -> Endpoint
discoveryOracleLocator (DiscoveryOracleContact _ locator) = locator

instance Binary DiscoveryOracleContact where
  put (DiscoveryOracleContact node locator) = put (raftNodeIdBytes node) >> put locator
  get = do
    node <- get >>= either (fail . show) pure . mkRaftNodeId
    locator <- get
    either fail pure (discoveryOracleContact node locator)

-- | The initial bootstrap remains byte-identical across every control prefix.
-- Membership and contacts describe observations; callers still replay control.
data DiscoverySnapshot = DiscoverySnapshot ByteString HeraldMembershipGenerationId ControlIndex (Set DiscoveryContact) (Set DiscoveryOracleContact)
  deriving stock (Eq, Show)

discoverySnapshot :: CheckedDeployment -> HeraldMembershipGenerationId -> ControlIndex -> [DiscoveryContact] -> NonEmpty DiscoveryOracleContact -> DiscoverySnapshot
discoverySnapshot bootstrap membership prefix contacts oracle = DiscoverySnapshot (encodeDeployment bootstrap) membership prefix (Set.fromList contacts) (Set.fromList (NonEmpty.toList oracle))
discoveryBootstrap :: DiscoverySnapshot -> CheckedDeployment
discoveryBootstrap (DiscoverySnapshot bytes _ _ _ _) = either error id (decodeDeployment bytes)
discoveryMembership :: DiscoverySnapshot -> HeraldMembershipGenerationId
discoveryMembership (DiscoverySnapshot _ membership _ _ _) = membership
discoveryControlPrefix :: DiscoverySnapshot -> ControlIndex
discoveryControlPrefix (DiscoverySnapshot _ _ prefix _ _) = prefix
discoveryContacts :: DiscoverySnapshot -> Set DiscoveryContact
discoveryContacts (DiscoverySnapshot _ _ _ contacts _) = contacts
discoveryOracleContacts :: DiscoverySnapshot -> Set DiscoveryOracleContact
discoveryOracleContacts (DiscoverySnapshot _ _ _ _ oracle) = oracle

instance Binary DiscoverySnapshot where
  put (DiscoverySnapshot bootstrap membership prefix contacts oracle) = put bootstrap >> put (heraldMembershipGenerationIdBytes membership) >> put (controlIndexWord64 prefix) >> put (Set.toAscList contacts) >> put (Set.toAscList oracle)
  get = do
    bytes <- get
    _ <- either fail pure (decodeDeployment bytes)
    membership <- get >>= either (fail . show) pure . mkHeraldMembershipGenerationId
    prefix <- controlIndex <$> get
    contacts <- get
    oracle <- get
    if null oracle then fail "no Oracle contacts" else pure (DiscoverySnapshot bytes membership prefix (Set.fromList contacts) (Set.fromList oracle))

-- | Onboard payloads carry exact pure-owner commands or finite transfer
-- records. This family never accepts ordinary peer/application envelopes.
data DiscoveryRequest = DiscoverSystem Word64 [DiscoveryContact] | Onboard Word64 ByteString | LookupOracleReplica Word64 RaftNodeId
  deriving stock (Eq, Show)

data DiscoveryResponse = SystemDiscovered Word64 DiscoverySnapshot | OnboardReply Word64 ByteString | OracleReplicaFound Word64 (Maybe Voter.OracleReplicaRegistration)
  deriving stock (Eq, Show)

instance Binary DiscoveryRequest where
  put (DiscoverSystem nonce contacts) = putWord8 0 >> put nonce >> put (Set.toAscList (Set.fromList contacts))
  put (Onboard nonce payload) = putWord8 1 >> put nonce >> put payload
  put (LookupOracleReplica nonce node) = putWord8 2 >> put nonce >> put (raftNodeIdBytes node)
  get =
    getWord8 >>= \case
      0 -> DiscoverSystem <$> get <*> get
      1 -> Onboard <$> get <*> get
      2 -> LookupOracleReplica <$> get <*> (get >>= either (fail . show) pure . mkRaftNodeId)
      _ -> fail "unknown restricted discovery request"
instance Binary DiscoveryResponse where
  put (SystemDiscovered nonce snapshot) = putWord8 0 >> put nonce >> put snapshot
  put (OnboardReply nonce payload) = putWord8 1 >> put nonce >> put payload
  put (OracleReplicaFound nonce registration) = putWord8 2 >> put nonce >> put (Voter.encodeReplicaRegistration <$> registration)
  get =
    getWord8 >>= \case
      0 -> SystemDiscovered <$> get <*> get
      1 -> OnboardReply <$> get <*> get
      2 -> OracleReplicaFound <$> get <*> (get >>= traverse (either (fail . show) pure . Voter.decodeReplicaRegistration))
      _ -> fail "unknown restricted discovery response"

encodeDiscoveryRequest :: DiscoveryRequest -> ByteString
encodeDiscoveryRequest = Lazy.toStrict . encode
decodeDiscoveryRequest :: ByteString -> Either String DiscoveryRequest
decodeDiscoveryRequest = decodeCanonical
encodeDiscoveryResponse :: DiscoveryResponse -> ByteString
encodeDiscoveryResponse = Lazy.toStrict . encode
decodeDiscoveryResponse :: ByteString -> Either String DiscoveryResponse
decodeDiscoveryResponse = decodeCanonical

decodeCanonical :: (Binary value) => ByteString -> Either String value
decodeCanonical bytes = case decodeOrFail (Lazy.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, value)
    | not (Lazy.null remaining) -> Left "trailing discovery payload bytes"
    | Lazy.toStrict (encode value) /= bytes -> Left "noncanonical discovery payload"
    | otherwise -> Right value

data DiscoveryCache = DiscoveryCache (Set Endpoint) (Maybe DiscoverySnapshot) (Set DiscoveryContact)
  deriving stock (Eq, Show)

initialDiscoveryCache :: NonEmpty Endpoint -> Either String DiscoveryCache
initialDiscoveryCache seeds
  | any ((== 0) . endpointPort) seeds = Left "discovery seed port is zero"
  | otherwise = Right (DiscoveryCache (Set.fromList (NonEmpty.toList seeds)) Nothing Set.empty)

discoveryCacheSnapshot :: DiscoveryCache -> Maybe DiscoverySnapshot
discoveryCacheSnapshot (DiscoveryCache _ snapshot _) = snapshot
discoveryCacheContacts :: DiscoveryCache -> Set DiscoveryContact
discoveryCacheContacts (DiscoveryCache _ _ contacts) = contacts
discoveryCacheLocators :: DiscoveryCache -> Set Endpoint
discoveryCacheLocators (DiscoveryCache seeds _ contacts) = Set.unions (seeds : map discoveryContactLocators (Set.toList contacts))

mergeDiscoverySnapshot :: DiscoverySnapshot -> DiscoveryCache -> Either String DiscoveryCache
mergeDiscoverySnapshot incoming@(DiscoverySnapshot bootstrap membership prefix contacts _) (DiscoveryCache seeds retained observed) = do
  latest <- case retained of
    Nothing -> pure incoming
    Just current@(DiscoverySnapshot oldBootstrap oldMembership oldPrefix _ _)
      | oldBootstrap /= bootstrap -> Left "seed replies disagree on the immutable system bootstrap"
      | oldPrefix == prefix && oldMembership /= membership -> Left "seed replies disagree on membership at the same control prefix"
      | oldPrefix > prefix -> pure current
      | otherwise -> pure incoming
  pure (DiscoveryCache seeds (Just latest) (Set.union observed contacts))

exchangeDiscoveryRequest :: Endpoint -> DiscoveryRequest -> IO (Either String DiscoveryResponse)
exchangeDiscoveryRequest locator request = case resolvedTcpEndpoint (endpointHost locator) (endpointPort locator) of
  Left problem -> pure (Left (show problem))
  Right resolved -> do
    bytes <- exchangeHeraldDiscovery resolved (encodeDiscoveryRequest request)
    pure (bytes >>= decodeDiscoveryResponse)

-- | Try every seed and newly learned locator before selecting the bootstrap.
-- A failed round waits and retries exactly the same identity-free request; it
-- never manufactures a new run. Cancellation interrupts both IO and pacing.
discoverSystem :: Word64 -> [DiscoveryContact] -> NonEmpty Endpoint -> IO DiscoveryCache
discoverSystem = discoverSystemWith exchangeDiscoveryRequest (threadDelay 100_000)

discoverSystemWith :: (Endpoint -> DiscoveryRequest -> IO (Either String DiscoveryResponse)) -> IO () -> Word64 -> [DiscoveryContact] -> NonEmpty Endpoint -> IO DiscoveryCache
discoverSystemWith exchange pace nonce advertised seeds = do
  initial <- either (ioError . userError) pure (initialDiscoveryCache seeds)
  let await current = do
        refreshed <- refreshDiscoveryWith exchange nonce advertised current
        case discoveryCacheSnapshot refreshed of
          Just _ -> pure refreshed
          Nothing -> pace >> await refreshed
  await initial

refreshDiscovery :: Word64 -> [DiscoveryContact] -> DiscoveryCache -> IO DiscoveryCache
refreshDiscovery = refreshDiscoveryWith exchangeDiscoveryRequest

refreshDiscoveryWith :: (Endpoint -> DiscoveryRequest -> IO (Either String DiscoveryResponse)) -> Word64 -> [DiscoveryContact] -> DiscoveryCache -> IO DiscoveryCache
refreshDiscoveryWith exchange nonce advertised = visit Set.empty
  where
    visit attempted cache = do
      let pending = Set.difference (discoveryCacheLocators cache) attempted
      if Set.null pending
        then pure cache
        else do
          next <- foldM observe cache (Set.toAscList pending)
          visit (Set.union attempted pending) next
    observe cache locator = do
      response <- exchange locator (DiscoverSystem nonce advertised)
      case response of
        Right (SystemDiscovered echoed snapshot) | echoed == nonce -> either (ioError . userError) pure (mergeDiscoverySnapshot snapshot cache)
        _ -> pure cache

-- | Preserve one exact request while following all learned routes. The reply
-- is still a claim for admission by the appropriate pure owner.
retryOnboarding :: DiscoveryCache -> Word64 -> ByteString -> IO (Endpoint, ByteString)
retryOnboarding cache nonce payload = loop cache
  where
    loop current = attempt current (Set.toAscList (discoveryCacheLocators current))
    attempt current [] = do
      refreshed <- refreshDiscovery nonce [] current
      threadDelay 100_000
      loop refreshed
    attempt current (locator : remaining) = do
      response <- exchangeDiscoveryRequest locator (Onboard nonce payload)
      case response of
        Right (OnboardReply echoed reply) | echoed == nonce -> pure (locator, reply)
        _ -> attempt current remaining

-- | Decoding and nonce reflection are shell duties. Contact observations and
-- onboarding commands are delivered to their separate supplied owners.
serveDeploymentDiscovery :: ([DiscoveryContact] -> IO DiscoverySnapshot) -> (ByteString -> IO (Maybe ByteString)) -> (RaftNodeId -> IO (Maybe Voter.OracleReplicaRegistration)) -> ByteString -> IO (Maybe ByteString)
serveDeploymentDiscovery snapshot onboard registration bytes = case decodeDiscoveryRequest bytes of
  Left _ -> pure Nothing
  Right (DiscoverSystem nonce advertised) -> Just . encodeDiscoveryResponse . SystemDiscovered nonce <$> snapshot advertised
  Right (Onboard nonce payload) -> fmap (encodeDiscoveryResponse . OnboardReply nonce) <$> onboard payload
  Right (LookupOracleReplica nonce node) -> Just . encodeDiscoveryResponse . OracleReplicaFound nonce <$> registration node

-- | One bounded physical attempt per known route. A checked registration can
-- open an ERFT lane; the local adapter still determines committed authority.
lookupReplicaRegistration :: [Endpoint] -> RaftNodeId -> IO (Maybe Voter.OracleReplicaRegistration)
lookupReplicaRegistration [] _ = pure Nothing
lookupReplicaRegistration (locator : remaining) node = do
  response <- exchangeDiscoveryRequest locator (LookupOracleReplica 1 node)
  case response of
    Right (OracleReplicaFound 1 (Just registration)) | Voter.replicaRegistrationNode registration == node -> pure (Just registration)
    _ -> lookupReplicaRegistration remaining node
