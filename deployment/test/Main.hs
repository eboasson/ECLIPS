{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AdminProperties qualified
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word8)
import DiscoveryProperties qualified
import Eclips.Deployment.Configuration
import Eclips.Deployment.Manifest
import Eclips.Domain.Graph (EdgeStrength (Preserve), VertexId (..), edgeDestination, edgePayload)
import Eclips.Domain.Startup (AppliedRootRole (WriterRoot), HeraldMember (..), allPredefinedSortRoles, appliedProcessResidence, appliedProcessRoots, appliedRootCatalogueRole, appliedRootObjectId, appliedRootRole, checkedInitialTopologyEdges, deriveSystemViewDeltaId)
import Eclips.Herald.Genesis (checkedConfigurationDigest, checkedInitialProjectionDigest)
import Eclips.Oracle.Genesis
  ( checkedOracleActiveHeralds,
    checkedOracleAppliedBootstraps,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOracleInitialTopologyProjection,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    raftVoterBindingHeraldEpoch,
    raftVoterBindingNode,
  )
import Eclips.Raft.Genesis (raftNativeVoters)
import JoiningProperties qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TimingProperties qualified

main :: IO ()
main =
  defaultMain
    $ testGroup
      "deployment"
      [ TimingProperties.tests,
        AdminProperties.tests,
        DiscoveryProperties.tests,
        JoiningProperties.tests,
        testProperty "fixed membership retains one founder, one voter, and one full root environment" propFixedManifest,
        testCase "base-port shorthand preserves five distinct protocol planes" caseEndpoints,
        testCase "endpoint parsing handles bracketed IPv6 and rejects conflicting listeners" caseEndpointShapes,
        testCase "fixed manifest rejects ambiguous contacts and noncanonical files" caseFixedAdmission
      ]

propFixedManifest :: NonEmptyList Word8 -> Positive Int -> Property
propFixedManifest (NonEmpty source) (Positive size) =
  let seed = Bytes.pack (take 32 (cycle source))
      count = 1 + size `mod` 5
      planes = [checked (deploymentEndpoints "127.0.0.1" (fromIntegral (41000 + offset * 10)) Nothing) | offset <- [0 .. count - 1]]
      deployment = checked (checkDeployment seed (NonEmpty.fromList planes))
      oracle = deploymentCheckedOracleGenesis deployment
      residents = NonEmpty.toList (deploymentResidents deployment)
      founder = NonEmpty.head (deploymentResidents deployment)
      coherent resident =
        checkedConfigurationDigest (residentHeraldGenesis resident) == checkedOracleConfigurationDigest oracle
          && checkedInitialProjectionDigest (residentBootstraps resident) == checkedOracleInitialProjectionDigest oracle
   in conjoin
        [ length residents === count,
          Set.fromList (checkedOracleActiveHeralds oracle) === Set.fromList (map residentMember residents),
          Set.size (Set.fromList (map (heraldMemberEpoch . residentMember) residents)) === count,
          property (all coherent residents),
          case (checkedOracleAppliedBootstraps oracle, checkedOracleRaftVoterBindings oracle) of
            ([launcher], [voter]) ->
              conjoin
                [ appliedProcessResidence launcher === heraldMemberEpoch (residentMember founder),
                  raftVoterBindingHeraldEpoch voter === heraldMemberEpoch (residentMember founder),
                  raftNativeVoters (checkedOracleRaftNativeConfiguration oracle) === [raftVoterBindingNode voter],
                  let system = checkedOracleSystemId oracle
                      views = Set.fromList [DeltaVertex (deriveSystemViewDeltaId system (heraldMemberEpoch member) role) | member <- map residentMember residents, role <- allPredefinedSortRoles]
                      expected = Set.fromList [edgePayload (NablaVertex writer) (DeltaVertex (deriveSystemViewDeltaId system (heraldMemberEpoch member) (appliedRootCatalogueRole root))) Preserve | root <- appliedProcessRoots launcher, WriterRoot writer _ <- [appliedRootRole root], member <- map residentMember residents]
                      actual = Set.fromList [edge | edge <- checkedInitialTopologyEdges (checkedOracleInitialTopologyProjection oracle), edgeDestination edge `Set.member` views]
                   in counterexample "every founder writer reaches every member system view before dynamic preparation" (actual === expected),
                  length (appliedProcessRoots launcher) === 12,
                  Set.size (Set.fromList (map appliedRootObjectId (appliedProcessRoots launcher))) === 12
                ]
            _ -> counterexample "fixed run minted an extra launcher or voter" False,
          case decodeDeployment (encodeDeployment deployment) of
            Right decoded -> encodeDeployment decoded === encodeDeployment deployment
            Left failure -> counterexample failure False
        ]

caseEndpoints :: Assertion
caseEndpoints = do
  let planes = checked (deploymentEndpoints "0.0.0.0" 7100 (Just "demo.example"))
  assertEqual "five listener ports" [7100 .. 7104] (map (endpointPort . ($ planes)) [peerListen, applicationListen, administrationListen, oracleListen, raftListen])
  assertEqual "application is advertised independently of bind host" (Just (checked (endpoint "demo.example" 7101))) (applicationAdvertised planes)
  assertEqual "peer is advertised independently of bind host" (Just (checked (endpoint "demo.example" 7100))) (peerAdvertised planes)
  assertBool "base expansion cannot cross the TCP port range" (case deploymentEndpoints "localhost" 65532 Nothing of Left _ -> True; Right _ -> False)

caseEndpointShapes :: Assertion
caseEndpointShapes = do
  let ipv6 = checked (parseEndpoint "[::1]:7101")
  assertEqual "IPv6 host survives parsing" "::1" (endpointHost ipv6)
  assertEqual "IPv6 rendering stays unambiguous" "[::1]:7101" (renderEndpoint ipv6)
  let same = checked (endpoint "localhost" 7100)
  assertBool "conflicting nonzero listeners reject before startup" (case deploymentEndpointsFromPlanes same same same same same Nothing Nothing Nothing Nothing Nothing of Left _ -> True; Right _ -> False)
  assertBool "an advertised endpoint cannot be ephemeral" (case deploymentEndpoints "localhost" 0 (Just "localhost") of Left _ -> True; Right _ -> False)
  let ephemeral = checked (endpoint "127.0.0.1" 0)
      advertised = checked (endpoint "application.example" 48101)
      partial = checked (deploymentEndpointsFromPlanes ephemeral ephemeral ephemeral ephemeral ephemeral (Just advertised) Nothing Nothing Nothing Nothing)
  assertEqual "one advertised plane leaves the other plane independently ephemeral" (Nothing, 0) (peerAdvertised partial, endpointPort (peerListen partial))
  assertEqual "the partial application advertisement is retained" (Just advertised) (applicationAdvertised partial)

caseFixedAdmission :: Assertion
caseFixedAdmission = do
  let seed = Bytes.pack [0 .. 31]
      fixed = checked (deploymentEndpoints "127.0.0.1" 43000 Nothing)
      dynamic = checked (deploymentEndpoints "127.0.0.1" 0 Nothing)
      rejected result = case result of Left _ -> True; Right _ -> False
      manifest = checked (checkDeployment seed (NonEmpty.singleton fixed))
  assertBool "two residents cannot bind the same protocol endpoints" (rejected (checkDeployment seed (NonEmpty.fromList [fixed, fixed])))
  assertBool "a remote resident cannot resolve an ephemeral static contact" (rejected (checkDeployment seed (NonEmpty.fromList [fixed, dynamic])))
  assertBool "fresh run seed has exact identity width" (rejected (checkDeployment (Bytes.take 31 seed) (NonEmpty.singleton fixed)))
  assertBool "manifest decoder consumes the entire canonical file" (rejected (decodeDeployment (encodeDeployment manifest <> Bytes.singleton 0)))

checked :: Either String value -> value
checked = either error id
