{-# LANGUAGE OverloadedStrings #-}

module GenesisProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (sortOn)
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( controlIndex,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    ConfiguredProcessMaterializationError (..),
    HeraldMember (..),
    InitialProjectionDigest,
    InitialTopologyManifest (..),
    appliedBootstrapManifestId,
    checkInitialTopologyProjection,
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
    materializeConfiguredProcessBootstrap,
    mkCatalogueDigest,
    mkInitialProjectionDigest,
    profileCatalogueDigest,
  )
import Eclips.Oracle.Genesis
  ( OracleGenesis,
    OracleGenesisFault (..),
    RaftVoterBinding,
    checkOracleGenesis,
    checkedOracleActiveHeralds,
    checkedOracleAppliedBootstraps,
    checkedOracleConfigurationDigest,
    checkedOraclePredefinedDescriptors,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
    raftVoterBindingNode,
  )
import Eclips.Oracle.Identity
  ( RaftConfigurationDigest,
    mkRaftConfigurationDigest,
    raftConfigurationDigestBytes,
  )
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
    raftNativeVoters,
  )
import Eclips.Raft.Identity (mkRaftDurationMicros)
import Numeric (showHex)
import OracleFixtures
  ( checked,
    fixtureBindings,
    fixtureBootstraps,
    fixtureCheckedGenesis,
    fixtureConfigurationDigest,
    fixtureDescriptors,
    fixtureGenesisTemplate,
    fixtureHeraldEpoch,
    fixtureHeraldId,
    fixtureLocalBootstrap,
    fixtureMembers,
    fixtureRaftConfiguration,
    fixtureRaftLocalNode,
    fixtureRaftNodes,
    fixtureRemoteHeraldEpoch,
    fixtureSystemId,
    fixtureThirdHeraldEpoch,
    fixtureTopology,
    heraldEpoch,
    heraldId,
    raftConfigurationForVoters,
    raftNodeId,
    systemId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck (Property, elements, forAll, shuffle, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "genesis"
    [ testCase "valid exact genesis is admitted" caseValidGenesis,
      testProperty "one-, two-, and three-voter composition preserves exact normalized bindings" propSmallVoterComposition,
      testCase "set-like genesis inputs normalize independently of presentation" caseNormalization,
      testCase "configured process materialization is genesis-only" caseGenesisOnlyMaterialization,
      testCase "Raft configuration digest is binding-order invariant" caseRaftDigestNormalization,
      testCase "Raft configuration digest commits every transcript field" caseRaftDigestSensitivity,
      testGroup "checked faults" faultCases
    ]

propSmallVoterComposition :: Property
propSmallVoterComposition =
  forAll (elements [1, 2, 3]) $ \count ->
    forAll (shuffle (take count fixtureBindings)) $ \bindings ->
      let voters = take count fixtureRaftNodes
          native = raftConfigurationForVoters voters 100000 300000 500000
          supplied = makeGenesis fixtureMembers fixtureDescriptors fixtureBootstraps bindings native validProjectionDigest (deriveRaftConfigurationDigest fixtureSystemId bindings native)
       in fmap
            (\genesis -> (checkedOracleRaftVoterBindings genesis, raftNativeVoters (checkedOracleRaftNativeConfiguration genesis)))
            (checkOracleGenesis supplied)
            === Right (take count fixtureBindings, voters)

caseValidGenesis :: IO ()
caseValidGenesis = do
  assertEqual "active membership" fixtureMembers (checkedOracleActiveHeralds fixtureCheckedGenesis)
  assertEqual "predefined catalogue" (sortOn descriptorSortId fixtureDescriptors) (checkedOraclePredefinedDescriptors fixtureCheckedGenesis)
  assertEqual "process bootstraps" fixtureBootstraps (checkedOracleAppliedBootstraps fixtureCheckedGenesis)
  assertEqual "deployment configuration digest" fixtureConfigurationDigest (checkedOracleConfigurationDigest fixtureCheckedGenesis)
  assertEqual "co-host bindings" fixtureBindings (checkedOracleRaftVoterBindings fixtureCheckedGenesis)

caseNormalization :: IO ()
caseNormalization = do
  let reordered =
        makeGenesis
          (reverse fixtureMembers)
          (reverse fixtureDescriptors)
          (reverse fixtureBootstraps)
          (reverse fixtureBindings)
          fixtureRaftConfiguration
          (deriveInitialProjectionDigest (reverse fixtureBootstraps) fixtureTopology)
          (deriveRaftConfigurationDigest fixtureSystemId (reverse fixtureBindings) fixtureRaftConfiguration)
  assertEqual "checked normalized genesis" (Right fixtureCheckedGenesis) (checkOracleGenesis reordered)

caseGenesisOnlyMaterialization :: IO ()
caseGenesisOnlyMaterialization =
  assertEqual
    "dynamic Start must not manufacture an applied genesis bootstrap"
    (Left (ConfiguredProcessMaterializationRequiresGenesis (controlIndex 1)))
    ( materializeConfiguredProcessBootstrap
        (controlIndex 1)
        (genesisPredefinedOccurrenceSet fixtureSystemId)
        fixtureGenesisTemplate
    )

caseRaftDigestNormalization :: IO ()
caseRaftDigestNormalization = do
  assertEqual
    "digest sorts bindings"
    (deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureRaftConfiguration)
    (deriveRaftConfigurationDigest fixtureSystemId (reverse fixtureBindings) fixtureRaftConfiguration)
  assertEqual
    "fixed Raft configuration digest"
    raftConfigurationDigestVector
    (renderHex (raftConfigurationDigestBytes validRaftDigest))

caseRaftDigestSensitivity :: IO ()
caseRaftDigestSensitivity =
  mapM_
    ( \(label, changed) ->
        assertBool
          (label <> " participates in the Raft configuration digest")
          (validRaftDigest /= changed)
    )
    [ ( "system identity",
        deriveRaftConfigurationDigest (systemId 80) fixtureBindings fixtureRaftConfiguration
      ),
      ( "voter identity",
        deriveRaftConfigurationDigest
          fixtureSystemId
          (raftVoterBinding (raftNodeId 5) fixtureHeraldEpoch : drop 1 fixtureBindings)
          fixtureRaftConfiguration
      ),
      ( "co-host Herald epoch",
        deriveRaftConfigurationDigest
          fixtureSystemId
          (raftVoterBinding fixtureRaftLocalNode (heraldEpoch 81) : drop 1 fixtureBindings)
          fixtureRaftConfiguration
      ),
      ( "heartbeat interval",
        deriveRaftConfigurationDigest fixtureSystemId fixtureBindings (nativeConfiguration 100001 300000 500000)
      ),
      ( "election lower bound",
        deriveRaftConfigurationDigest fixtureSystemId fixtureBindings (nativeConfiguration 100000 300001 500000)
      ),
      ( "election upper bound",
        deriveRaftConfigurationDigest fixtureSystemId fixtureBindings (nativeConfiguration 100000 300000 500001)
      )
    ]

faultCases :: [TestTree]
faultCases =
  [ genesisFault "active membership must be non-empty" OracleGenesisHasNoActiveHeralds (makeGenesis [] fixtureDescriptors fixtureBootstraps fixtureBindings fixtureRaftConfiguration validProjectionDigest validRaftDigest),
    let first = HeraldMember fixtureHeraldId fixtureHeraldEpoch
     in genesisFault
          "Herald identities are unique"
          (DuplicateActiveHeraldId (heraldMemberId first))
          (makeGenesis (first : fixtureMembers) fixtureDescriptors fixtureBootstraps fixtureBindings fixtureRaftConfiguration validProjectionDigest validRaftDigest),
    let first = HeraldMember fixtureHeraldId fixtureHeraldEpoch
        duplicateEpoch = HeraldMember (heraldId 80) (heraldMemberEpoch first)
     in genesisFault
          "Herald epochs are unique"
          (DuplicateActiveHeraldEpoch (heraldMemberEpoch first))
          (makeGenesis (duplicateEpoch : fixtureMembers) fixtureDescriptors fixtureBootstraps fixtureBindings fixtureRaftConfiguration validProjectionDigest validRaftDigest),
    genesisFault
      "catalogue digest must match the closed profile"
      OracleCatalogueDigestMismatch
      ( oracleGenesis
          fixtureSystemId
          fixtureMembers
          (checked "wrong catalogue digest" (mkCatalogueDigest (ByteString.replicate 32 0)))
          fixtureDescriptors
          fixtureBootstraps
          fixtureConfigurationDigest
          fixtureTopology
          validProjectionDigest
          fixtureBindings
          fixtureRaftConfiguration
          validRaftDigest
      ),
    genesisFault
      "predefined descriptors must be exact"
      OraclePredefinedCatalogueMismatch
      (makeGenesis fixtureMembers (drop 1 fixtureDescriptors) fixtureBootstraps fixtureBindings fixtureRaftConfiguration validProjectionDigest validRaftDigest),
    let first = fixtureLocalBootstrap
     in genesisFault
          "bootstrap manifest identities are unique"
          (DuplicateBootstrapManifestId (appliedBootstrapManifestId first))
          (makeGenesis fixtureMembers (fixtureDescriptors) (first : fixtureBootstraps) fixtureBindings fixtureRaftConfiguration (deriveInitialProjectionDigest (first : fixtureBootstraps) fixtureTopology) validRaftDigest),
    let unrelatedTopology =
          checked
            "unrelated checked topology"
            (checkInitialTopologyProjection fixtureSystemId fixtureMembers [] (InitialTopologyManifest []))
     in genesisFault
          "checked topology must project the exact supplied bootstraps"
          InitialTopologyVertexSetMismatch
          ( oracleGenesis
              fixtureSystemId
              fixtureMembers
              profileCatalogueDigest
              fixtureDescriptors
              fixtureBootstraps
              fixtureConfigurationDigest
              unrelatedTopology
              (deriveInitialProjectionDigest fixtureBootstraps unrelatedTopology)
              fixtureBindings
              fixtureRaftConfiguration
              validRaftDigest
          ),
    genesisFault
      "initial projection digest is checked"
      InitialProjectionDigestMismatch
      ( makeGenesis
          fixtureMembers
          fixtureDescriptors
          fixtureBootstraps
          fixtureBindings
          fixtureRaftConfiguration
          (checked "wrong projection digest" (mkInitialProjectionDigest (ByteString.replicate 32 0)))
          validRaftDigest
      ),
    let first = raftVoterBinding fixtureRaftLocalNode fixtureHeraldEpoch
     in genesisFault
          "Raft node bindings are unique"
          (DuplicateRaftVoterBinding (raftVoterBindingNode first))
          (makeGenesis fixtureMembers fixtureDescriptors fixtureBootstraps (first : fixtureBindings) fixtureRaftConfiguration validProjectionDigest (deriveRaftConfigurationDigest fixtureSystemId (first : fixtureBindings) fixtureRaftConfiguration)),
    let duplicateEpochBindings =
          [ raftVoterBinding (raftNodeId 2) fixtureHeraldEpoch,
            raftVoterBinding (raftNodeId 3) fixtureHeraldEpoch,
            raftVoterBinding (raftNodeId 4) fixtureThirdHeraldEpoch
          ]
     in genesisFault
          "co-host Herald epochs are unique"
          (DuplicateRaftCohostHeraldEpoch fixtureHeraldEpoch)
          (makeGenesis fixtureMembers fixtureDescriptors fixtureBootstraps duplicateEpochBindings fixtureRaftConfiguration validProjectionDigest (deriveRaftConfigurationDigest fixtureSystemId duplicateEpochBindings fixtureRaftConfiguration)),
    let inactive = heraldEpoch 81
        bindings =
          [ raftVoterBinding (raftNodeId 2) fixtureHeraldEpoch,
            raftVoterBinding (raftNodeId 3) fixtureRemoteHeraldEpoch,
            raftVoterBinding (raftNodeId 4) inactive
          ]
     in genesisFault
          "each co-host Herald is active"
          (RaftCohostHeraldNotActive inactive)
          (makeGenesis fixtureMembers fixtureDescriptors fixtureBootstraps bindings fixtureRaftConfiguration validProjectionDigest (deriveRaftConfigurationDigest fixtureSystemId bindings fixtureRaftConfiguration)),
    let alternateNodes = fmap raftNodeId [5, 6, 7]
        bindings = zipWith raftVoterBinding alternateNodes (fmap heraldMemberEpoch fixtureMembers)
     in genesisFault
          "binding voters equal the native voter set"
          RaftVoterSetMismatch
          (makeGenesis fixtureMembers fixtureDescriptors fixtureBootstraps bindings fixtureRaftConfiguration validProjectionDigest (deriveRaftConfigurationDigest fixtureSystemId bindings fixtureRaftConfiguration)),
    genesisFault
      "supplied Raft configuration digest is verified"
      RaftConfigurationDigestMismatch
      ( makeGenesis
          fixtureMembers
          fixtureDescriptors
          fixtureBootstraps
          fixtureBindings
          fixtureRaftConfiguration
          validProjectionDigest
          (checked "wrong Raft digest" (mkRaftConfigurationDigest (ByteString.replicate 32 0)))
      )
  ]

genesisFault :: String -> OracleGenesisFault -> OracleGenesis -> TestTree
genesisFault label expected supplied =
  testCase label (assertEqual "genesis fault" (Left expected) (checkOracleGenesis supplied))

makeGenesis ::
  [HeraldMember] ->
  [CanonicalDescriptor] ->
  [AppliedProcessBootstrap] ->
  [RaftVoterBinding] ->
  RaftNativeConfiguration ->
  InitialProjectionDigest ->
  RaftConfigurationDigest ->
  OracleGenesis
makeGenesis members descriptors bootstraps bindings native projectionDigest raftDigest =
  oracleGenesis
    fixtureSystemId
    members
    profileCatalogueDigest
    descriptors
    bootstraps
    fixtureConfigurationDigest
    fixtureTopology
    projectionDigest
    bindings
    native
    raftDigest

validProjectionDigest :: InitialProjectionDigest
validProjectionDigest = deriveInitialProjectionDigest fixtureBootstraps fixtureTopology

validRaftDigest :: RaftConfigurationDigest
validRaftDigest = deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureRaftConfiguration

nativeConfiguration :: Word64 -> Word64 -> Word64 -> RaftNativeConfiguration
nativeConfiguration heartbeat lower upper =
  checkedRaftNativeConfiguration
    ( checked
        "alternative Raft configuration"
        ( checkRaftGenesis
            ( raftGenesis
                fixtureRaftLocalNode
                fixtureRaftNodes
                (checked "heartbeat" (mkRaftDurationMicros heartbeat))
                (checked "election lower" (mkRaftDurationMicros lower))
                (checked "election upper" (mkRaftDurationMicros upper))
            )
        )
    )

renderHex :: ByteString.ByteString -> String
renderHex = concatMap byteHex . ByteString.unpack
  where
    byteHex byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

raftConfigurationDigestVector :: String
raftConfigurationDigestVector = "8ffdd302267fb56067ea9771eadde9acd6e8716cd35aaca07aaf24999984ec43"
