{-# LANGUAGE OverloadedStrings #-}

module DiscoveryProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Deployment.Configuration
import Eclips.Deployment.Discovery
import Eclips.Deployment.Manifest
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Membership (genesisHeraldMembershipGeneration, heraldMembershipGenerationId)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Genesis (checkedOracleRaftVoterBindings, checkedOracleSystemId, raftVoterBindingNode)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "restricted discovery"
    [ testProperty "canonical discovery and onboarding round trips" propCanonical,
      testProperty "duplicate gossip is idempotent and arrival order preserves newest prefix" propMerge,
      testProperty "self observations do not change remote peer seeds" propPeerSeeds,
      testCase "peer seed filtering excludes historical epochs of the local Herald" casePeerSeedEpoch,
      testCase "transitive discovery retains echoed self observations without self peer seeds" caseSeedChain,
      testCase "unavailable seeds retry the exact request without creating genesis" caseUnavailable,
      testCase "successful seeds must agree on immutable bootstrap" caseConflictingBootstrap,
      testCase "replica lookup returns only the selected canonical registration" caseReplicaLookup,
      testCase "canonical decoding rejects duplicate contacts and trailing bytes" caseCanonicalRejections
    ]

fixture :: Word8 -> CheckedDeployment
fixture tag = checked (checkDeployment (Bytes.replicate 32 tag) (NonEmpty.singleton (checked (deploymentEndpoints "127.0.0.1" 47000 Nothing))))

locator :: Int -> Endpoint
locator value = checked (endpoint "127.0.0.1" (fromIntegral (47000 + value)))

contact :: Word8 -> DiscoveryContact
contact tag = checked (discoveryContact (residentMember (NonEmpty.head (deploymentResidents (fixture tag)))) (locator (fromIntegral tag) :| []))

snapshot :: Word8 -> Word64 -> [DiscoveryContact] -> DiscoverySnapshot
snapshot tag prefix contacts =
  let plan = fixture tag
      oracle = deploymentCheckedOracleGenesis plan
      member = residentMember (NonEmpty.head (deploymentResidents plan))
      generation = checkedShow (genesisHeraldMembershipGeneration (checkedOracleSystemId oracle) (heraldMemberEpoch member :| []))
      node = case checkedOracleRaftVoterBindings oracle of
        [binding] -> raftVoterBindingNode binding
        _ -> error "discovery fixture requires one Oracle voter"
      oracleContact = checked (discoveryOracleContact node (locator 3))
   in discoverySnapshot plan (heraldMembershipGenerationId generation) (controlIndex prefix) contacts (oracleContact :| [])

propCanonical :: Word64 -> [Word8] -> Property
propCanonical nonce bytes =
  let request = Onboard nonce (Bytes.pack bytes)
      response = OnboardReply nonce (Bytes.pack bytes)
      discovery = SystemDiscovered nonce (snapshot 1 10 [contact 1, contact 2])
      registration = onlyRegistration (Voter.oracleReplicaRegistrations (checkedShow (initialOracle (deploymentCheckedOracleGenesis (fixture 1)))))
   in conjoin
        [ decodeDiscoveryRequest (encodeDiscoveryRequest request) === Right request,
          decodeDiscoveryResponse (encodeDiscoveryResponse response) === Right response,
          decodeDiscoveryResponse (encodeDiscoveryResponse discovery) === Right discovery,
          decodeDiscoveryRequest (encodeDiscoveryRequest (DiscoverSystem nonce [contact 1])) === Right (DiscoverSystem nonce [contact 1]),
          decodeDiscoveryRequest (encodeDiscoveryRequest (LookupOracleReplica nonce (Voter.replicaRegistrationNode registration))) === Right (LookupOracleReplica nonce (Voter.replicaRegistrationNode registration)),
          decodeDiscoveryResponse (encodeDiscoveryResponse (OracleReplicaFound nonce (Just registration))) === Right (OracleReplicaFound nonce (Just registration)),
          decodeDiscoveryResponse (encodeDiscoveryResponse (OracleReplicaFound nonce Nothing)) === Right (OracleReplicaFound nonce Nothing)
        ]

propMerge :: Word8 -> Word8 -> Property
propMerge first second =
  let initial = checked (initialDiscoveryCache (locator 1 :| [locator 1]))
      a = snapshot 1 (fromIntegral first) [contact 1]
      b = snapshot 1 (fromIntegral second) [contact 2]
      merge entries = foldl (\cache next -> checked (mergeDiscoverySnapshot next cache)) initial entries
      forward = merge [a, b]
      reverseOrder = merge [b, a]
   in conjoin
        [ discoveryCacheContacts forward === discoveryCacheContacts reverseOrder,
          discoveryCacheLocators forward === Set.fromList [locator 1, locator 2],
          fmap discoveryControlPrefix (discoveryCacheSnapshot forward) === Just (controlIndex (fromIntegral (max first second))),
          checked (mergeDiscoverySnapshot b forward) === forward
        ]

propPeerSeeds :: NonEmptyList Word8 -> NonEmptyList Word8 -> NonEmptyList Word8 -> Property
propPeerSeeds (NonEmpty localRoutes) (NonEmpty firstRoutes) (NonEmpty secondRoutes) =
  let observed tag routes = checked (discoveryContact (discoveryContactMember (contact tag)) (NonEmpty.fromList (map (locator . fromIntegral) routes)))
      local = observed 1 localRoutes
      first = observed 2 firstRoutes
      second = observed 3 secondRoutes
      seeds = discoveryPeerSeedLocators (discoveryContactMember local)
      remoteRoutes = Set.fromList (map (locator . fromIntegral) (firstRoutes <> secondRoutes))
   in conjoin
        [ seeds [] === Set.empty,
          seeds [local, local] === Set.empty,
          seeds [first, second] === remoteRoutes,
          seeds [local, first, local, second, first] === remoteRoutes,
          seeds [second, local, first, second, local] === remoteRoutes
        ]

casePeerSeedEpoch :: Assertion
casePeerSeedEpoch = do
  let local = discoveryContactMember (contact 1)
      previousEpoch = heraldMemberEpoch (discoveryContactMember (contact 2))
      previousMember = HeraldMember (heraldMemberId local) previousEpoch
      historical = checked (discoveryContact previousMember (locator 2 :| []))
      observations = [contact 1, historical, contact 3]
      initial = checked (initialDiscoveryCache (locator 3 :| []))
      cache = checked (mergeDiscoverySnapshot (snapshot 1 10 observations) initial)
  assertEqual
    "current and historical self observations do not become peer seeds"
    (Set.singleton (locator 3))
    (discoveryPeerSeedLocators local (Set.toAscList (discoveryCacheContacts cache)))
  assertEqual
    "historical self observations remain available to discovery"
    (Set.fromList observations)
    (discoveryCacheContacts cache)

caseSeedChain :: Assertion
caseSeedChain = do
  requests <- newIORef []
  let exchange target request@(DiscoverSystem nonce advertised) = do
        modifyIORef' requests (<> [(target, request)])
        pure
          ( if target == locator 9
              then Left "applicant has not started its listener"
              else Right (SystemDiscovered nonce (snapshot 1 10 (contact (if target == locator 1 then 2 else 3) : advertised)))
          )
      exchange _ _ = assertFailure "unexpected onboarding request"
  cache <- discoverSystemWith exchange (assertFailure "reachable seed should not sleep") 42 [contact 9] (locator 1 :| [locator 1])
  seen <- readIORef requests
  assertEqual "each identity-free seed/contact route gets one discovery attempt" [locator 1, locator 2, locator 9, locator 3] (map fst seen)
  assertEqual "the same nonce and applicant contact survive every hop" (replicate 4 (DiscoverSystem 42 [contact 9])) (map snd seen)
  assertEqual "all observations, including the applicant, remain cached" (Set.fromList [contact 2, contact 3, contact 9]) (discoveryCacheContacts cache)
  assertEqual
    "the echoed applicant does not become a repeating self peer dial"
    (Set.fromList [locator 2, locator 3])
    (discoveryPeerSeedLocators (discoveryContactMember (contact 9)) (Set.toAscList (discoveryCacheContacts cache)))
  assertEqual "discovery retains the founder bootstrap, without enlarging it" (Just (encodeDeployment (fixture 1))) (encodeDeployment . discoveryBootstrap <$> discoveryCacheSnapshot cache)

caseUnavailable :: Assertion
caseUnavailable = do
  attempts <- newIORef []
  rounds <- newIORef (0 :: Int)
  let exchange target request = do
        modifyIORef' attempts (<> [(target, request)])
        count <- readIORef rounds
        pure (if count == 0 then Left "offline" else Right (SystemDiscovered 72 (snapshot 1 0 [])))
      pace = modifyIORef' rounds (+ 1)
  _ <- discoverSystemWith exchange pace 72 [contact 8] (locator 1 :| [])
  seen <- readIORef attempts
  assertEqual "one paced retry occurs after a fully unavailable round" 1 =<< readIORef rounds
  assertEqual "retry preserves exact nonce and advertised applicant" [(locator 1, DiscoverSystem 72 [contact 8]), (locator 1, DiscoverSystem 72 [contact 8])] seen

caseConflictingBootstrap :: Assertion
caseConflictingBootstrap = do
  let initial = checked (initialDiscoveryCache (locator 1 :| []))
      first = checked (mergeDiscoverySnapshot (snapshot 1 0 []) initial)
  assertBool "different successful system replies are not merged" (case mergeDiscoverySnapshot (snapshot 2 0 []) first of Left _ -> True; Right _ -> False)

caseCanonicalRejections :: Assertion
caseCanonicalRejections = do
  let canonical = encodeDiscoveryRequest (DiscoverSystem 1 [contact 1, contact 1])
  assertEqual "encoder collapses duplicate contact observations" (Right (DiscoverSystem 1 [contact 1])) (decodeDiscoveryRequest canonical)
  assertBool "trailing bytes rejected" (case decodeDiscoveryRequest (canonical <> Bytes.singleton 0) of Left _ -> True; Right _ -> False)

checked :: Either String value -> value
checked = either error id
checkedShow :: (Show problem) => Either problem value -> value
checkedShow = either (error . show) id

caseReplicaLookup :: Assertion
caseReplicaLookup = do
  let registrations = Voter.oracleReplicaRegistrations (checkedShow (initialOracle (deploymentCheckedOracleGenesis (fixture 1))))
      registration = onlyRegistration registrations
      node = Voter.replicaRegistrationNode registration
      serve =
        serveDeploymentDiscovery
          (const (assertFailure "lookup must not invoke contact gossip"))
          (const (assertFailure "lookup must not invoke onboarding"))
          (\requested -> pure (if requested == node then Just registration else Nothing))
  encoded <- serve (encodeDiscoveryRequest (LookupOracleReplica 194 node))
  assertEqual
    "exact identity and nonce select one canonical committed registration"
    (Just (Right (OracleReplicaFound 194 (Just registration))))
    (decodeDiscoveryResponse <$> encoded)

onlyRegistration :: [Voter.OracleReplicaRegistration] -> Voter.OracleReplicaRegistration
onlyRegistration [registration] = registration
onlyRegistration _ = error "discovery fixture requires one registered genesis replica"
