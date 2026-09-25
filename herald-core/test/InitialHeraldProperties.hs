module InitialHeraldProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.List (sort, sortOn)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Identity
  ( privateDeltaUniqueId,
    privateNablaUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Domain.Graph (VertexId (NablaVertex))
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    GlobalUniqueId,
    NablaSequencing (UnsequencedNabla),
    SortId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    mkBootstrapManifestId,
    mkDeltaId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Startup
  ( AppliedRoot,
    AppliedRootRole (..),
    PredefinedSortRole (..),
    PrimordialDefinitionReplica,
    appliedProcessEnvironmentObjects,
    appliedProcessEpochId,
    appliedProcessRoots,
    appliedRootCatalogueRole,
    appliedRootRole,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaPublicationId,
    primordialReplicaRole,
    primordialReplicaSortId,
    primordialReplicaSourceAuthority,
  )
import Eclips.Domain.Store (visibleInstances)
import Eclips.Herald.Application.PrivateIdentity
  ( ProcessIdentityWitness,
    witnessedBindings,
    witnessedNextPrivateUniqueId,
    witnessedProcessEpoch,
    witnessedReverseBindings,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    effectBatchMemberCount,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    ConfiguredRootManifest (..),
    DeploymentManifest (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedInitialBootstraps,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemBootstrapWriters,
    checkedSystemId,
    systemBootstrapWriterNablaId,
  )
import Eclips.Herald.Initialization
  ( HeraldInvariantFault (HeraldStartupInvariant),
    HeraldState,
    StartupInvariantSubject (StartupStatic),
    StartupInvariantViolation (IsolationOwnerInvariant),
    initialHerald,
    initialHeraldWithIsolation,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.State
  ( StartupStateWitness (..),
    startupApplicationRecoveryConfiguration,
    startupPlacementState,
    startupStateWitness,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
  )
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureDeploymentAt,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedH1,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    initialEffectsAreOracleWatchAndGrace,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAllShrink,
    property,
    shrink,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "complete Herald initialization"
    [ testCase "empty startup retains static owners and starts Oracle watch and isolation grace" caseEmpty,
      testCase "global selection is filtered into local owners at each Herald" caseGlobalProjectionLocalOwners,
      testCase "multiple resident processes install independently" caseMultipleResidentProcesses,
      testCase "configured but unapplied processes are absent" caseConfiguredButUnapplied,
      testCase "the supplied time and deterministic witness are retained" caseTimeAndDeterminism,
      testCase "isolation voter hosts are a non-empty subset of genesis membership" caseIsolationVoterHostsChecked,
      testCase "canonical process and environment aliases are exactly 1 through 32" caseCanonicalAliases,
      testCase "primordial publication provenance is retained field by field" casePrimordialProvenance,
      testProperty "coherent small deployments initialize from every Herald perspective" propCoherentDeployments
    ]

caseEmpty :: Assertion
caseEmpty = do
  (state, effects) <- initialize fixtureCheckedGenesis [] (monotonicInstant 41)
  let witness = startupStateWitness state
      hiddenWriters = checkedSystemBootstrapWriters fixtureCheckedGenesis
      publicationWitness = startupWitnessPublication witness
  assertEqual "no global process is applied" [] (startupWitnessProjectedBootstraps witness)
  assertEqual "no local application identity exists" [] (startupWitnessApplicationIdentities witness)
  assertEqual "no live configured-process scaffold exists" [] (startupWitnessConfiguredScaffolds witness)
  assertEqual "no local controlled process exists" [] (startupWitnessControlledProcesses witness)
  assertEqual "no local controlled root exists" [] (startupWitnessControlledRoots witness)
  assertEqual "no peer binding exists" [] (startupWitnessPeerBindings witness)
  assertEqual "no peer contact hint exists" [] (startupWitnessKnownHeralds witness)
  case startupWitnessPeerStream witness of
    Left problem -> assertFailure ("empty peer stream rejected: " <> show problem)
    Right peerWitness -> do
      assertEqual "no outgoing peer direction exists" [] (PeerStream.peerStreamWitnessOutgoingDirections peerWitness)
      assertEqual "no incoming peer direction exists" [] (PeerStream.peerStreamWitnessIncomingDirections peerWitness)
  assertEqual
    "all active Herald system-view and hidden bootstrap-writer vertices exist"
    22
    (length (startupWitnessGraphVertices witness))
  assertBool
    "every checked hidden bootstrap writer is an isolated Graph nabla vertex"
    ( all
        ( (`elem` startupWitnessGraphVertices witness)
            . NablaVertex
            . systemBootstrapWriterNablaId
        )
        hiddenWriters
    )
  assertEqual "no graph edge exists" [] (startupWitnessGraphEdges witness)
  assertEqual "no application placement exists" [] (startupWitnessPlacements witness)
  assertEqual "six system-view placements exist" 6 (length (startupWitnessSystemViewPlacements witness))
  let placement = startupPlacementState state
      owner = checkedLocalHeraldEpoch fixtureCheckedGenesis
  assertEqual
    "startup commits physical-placement revision one"
    (Just PlacementMessage.firstPlacementSequence)
    (Placement.localPlacementSequence placement)
  assertEqual
    "startup retains the exact revision-one projection"
    (Just 6)
    (length <$> Placement.placementRoutesAtRevision owner PlacementMessage.firstPlacementSequence placement)
  assertSystemViewStores fixtureCheckedGenesis witness
  assertEqual "the closed catalogue remains installed" 6 (length (startupWitnessSortRegistry witness))
  assertEqual "the primordial publication vector remains installed" 6 (length (startupWitnessPrimordialPublications witness))
  assertEqual
    "no outgoing publication exists"
    []
    (Publication.publicationWitnessOutgoing publicationWitness)
  assertEqual
    "immutable hidden topology coordinates have no live publication tenure"
    []
    (Publication.publicationWitnessNextSequences publicationWitness)
  assertEqual "no wait exists" [] (startupWitnessWaits witness)
  assertInitialEffects effects

caseGlobalProjectionLocalOwners :: Assertion
caseGlobalProjectionLocalOwners = do
  localResult <- initializeAt fixtureLocalMember selected observed
  remoteResult <- initializeAt fixtureRemoteMember selected observed
  let localWitness = startupStateWitness (fst localResult)
      remoteWitness = startupStateWitness (fst remoteResult)
      projected witness = sort (fmap snd (startupWitnessProjectedBootstraps witness))
  assertEqual "the local Herald retains both global applications" (sort selected) (projected localWitness)
  assertEqual "the remote Herald retains the same global applications" (sort selected) (projected remoteWitness)
  assertEqual "the local Herald owns only its resident process" 1 (length (startupWitnessApplicationIdentities localWitness))
  assertEqual "the remote Herald owns only its resident process" 1 (length (startupWitnessApplicationIdentities remoteWitness))
  assertEqual
    "both selected processes plus common infrastructure produce forty-eight vertices"
    48
    (length (startupWitnessGraphVertices localWitness))
  assertEqual
    "the remote perspective retains the same forty-eight vertices"
    48
    (length (startupWitnessGraphVertices remoteWitness))
  assertEqual "both selected processes produce forty-eight common routing edges" 48 (length (startupWitnessGraphEdges localWitness))
  assertEqual "the remote perspective retains the same forty-eight edges" 48 (length (startupWitnessGraphEdges remoteWitness))
  assertEqual
    "both perspectives retain the identical common graph"
    (startupWitnessGraphVertices localWitness, startupWitnessGraphEdges localWitness)
    (startupWitnessGraphVertices remoteWitness, startupWitnessGraphEdges remoteWitness)
  assertEqual "one local process produces six placements" 6 (length (startupWitnessPlacements localWitness))
  assertEqual "one remote process produces six placements" 6 (length (startupWitnessPlacements remoteWitness))
  assertEqual "the local Herald has six system-view placements" 6 (length (startupWitnessSystemViewPlacements localWitness))
  assertEqual "the remote Herald has six system-view placements" 6 (length (startupWitnessSystemViewPlacements remoteWitness))
  assertEqual
    "both perspectives bind the same global initial projection"
    (startupWitnessInitialProjectionDigest localWitness)
    (startupWitnessInitialProjectionDigest remoteWitness)
  assertBool
    "each Herald owns distinct private system-view deltas"
    ( null
        ( systemViewDeltas localWitness
            `intersectEqual` systemViewDeltas remoteWitness
        )
    )
  assertInitialEffects (snd localResult)
  assertInitialEffects (snd remoteResult)
  where
    selected = take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
    observed = monotonicInstant 42

caseMultipleResidentProcesses :: Assertion
caseMultipleResidentProcesses = do
  (state, _) <- initialize fixtureCheckedGenesis fixtureLocalBootstrapIds (monotonicInstant 43)
  let witness = startupStateWitness state
      identities = startupWitnessApplicationIdentities witness
  assertEqual "two resident process namespaces" 2 (length (startupWitnessApplicationIdentities witness))
  assertEqual "two resident process control facts" 2 (length (startupWitnessControlledProcesses witness))
  assertEqual "two complete root sets" 24 (length (startupWitnessControlledRoots witness))
  assertEqual
    "two process graphs plus all active infrastructure vertices"
    48
    (length (startupWitnessGraphVertices witness))
  assertEqual "application and system-view edges for both processes" 48 (length (startupWitnessGraphEdges witness))
  assertEqual "one placement per resident reader" 12 (length (startupWitnessPlacements witness))
  assertEqual "one placement per private system view" 6 (length (startupWitnessSystemViewPlacements witness))
  assertEqual "six system views plus one store per resident reader" 18 (length (startupWitnessStoreSlots witness))
  mapM_ (assertIndependentProcessAlias witness) identities
  assertEqual
    "alias one resolves to a distinct process object in each namespace"
    2
    ( length
        ( unique
            [ globalIdentity
            | identity <- identities,
              (privateIdentity, globalIdentity) <- witnessedReverseBindings identity,
              privateUniqueIdWord64 privateIdentity == 1
            ]
        )
    )

caseConfiguredButUnapplied :: Assertion
caseConfiguredButUnapplied = do
  let selected = take 1 fixtureLocalBootstrapIds
      absent = drop 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]
  (state, _) <- initialize fixtureCheckedGenesis selected (monotonicInstant 44)
  let witness = startupStateWitness state
      projected = fmap snd (startupWitnessProjectedBootstraps witness)
  assertEqual "only the selected template is globally applied" (sort selected) (sort projected)
  mapM_
    (\manifest -> assertBool "unapplied configured template is absent" (manifest `notElem` projected))
    absent
  assertEqual "only the selected resident template has an owner" 1 (length (startupWitnessApplicationIdentities witness))

caseTimeAndDeterminism :: Assertion
caseTimeAndDeterminism = do
  checkedBootstraps <- checkedInitial fixtureCheckedGenesis fixtureLocalBootstrapIds
  let observed = monotonicInstant 987654321
      first = initialHerald observed fixtureCheckedGenesis checkedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration
      second = initialHerald observed fixtureCheckedGenesis checkedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration
  (state, effects) <- checkedIO "initialize deterministic fixture" first
  (secondState, secondEffects) <- checkedIO "repeat deterministic fixture" second
  (differentSeedState, _) <-
    checkedIO
      "initialize different-seed fixture"
      ( initialHerald
          observed
          fixtureCheckedGenesis
          checkedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedH1
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  assertBool "same checked inputs produce equal opaque states" (state == secondState)
  assertBool "changing only the seed changes retained state" (state /= differentSeedState)
  assertEqual "same checked inputs produce the same witness" (startupStateWitness state) (startupStateWitness secondState)
  assertEqual "same checked inputs produce the same effects" effects secondEffects
  assertEqual "the runtime-supplied observation is retained exactly" observed (startupWitnessLastObservedTime (startupStateWitness state))
  assertEqual "the checked application recovery configuration is retained exactly" fixtureApplicationRecoveryConfiguration (startupApplicationRecoveryConfiguration state)
  assertEqual "the generator begins at counter zero" 0 (startupWitnessNextGeneratorCounter (startupStateWitness state))
  assertInitialEffects effects

caseIsolationVoterHostsChecked :: Assertion
caseIsolationVoterHostsChecked = do
  let configuration = checked "isolation configuration" (checkIsolationConfiguration 10 10)
      initializeWith voterHosts =
        initialHeraldWithIsolation
          (monotonicInstant 1)
          fixtureCheckedGenesis
          fixtureCheckedInitialBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
          voterHosts
          configuration
      expectedFault = HeraldStartupInvariant StartupStatic IsolationOwnerInvariant
      local = heraldMemberEpoch fixtureLocalMember
      assertRejected context result = case result of
        Left fault -> assertEqual context expectedFault fault
        Right _ -> assertFailure (context <> ": initialization unexpectedly succeeded")
  assertRejected
    "an empty placement is rejected before owner construction"
    (initializeWith Set.empty)
  assertRejected
    "a placement outside immutable genesis membership is rejected"
    ( initializeWith
        (Set.singleton (checked "foreign voter epoch" (mkHeraldEpoch (fixtureIdentifierBytes 251))))
    )
  case initializeWith (Set.singleton local) of
    Left problem -> assertFailure ("active singleton voter placement was rejected: " <> show problem)
    Right _ -> pure ()

caseCanonicalAliases :: Assertion
caseCanonicalAliases = case fixtureLocalBootstrapIds of
  selected : _ -> do
    checkedBootstraps <- checkedInitial fixtureCheckedGenesis [selected]
    bootstrap <- case checkedInitialBootstraps checkedBootstraps of
      [one] -> pure one
      others -> assertFailure ("expected one applied bootstrap, got " <> show (length others))
    (state, _) <-
      checkedIO
        "initialize canonical alias fixture"
        ( initialHerald
            (monotonicInstant 45)
            fixtureCheckedGenesis
            checkedBootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    let witness = startupStateWitness state
        process = appliedProcessEpochId bootstrap
    identity <- exactlyOne "process identity witness" (startupWitnessApplicationIdentities witness)
    access <- case lookup process (startupWitnessApplicationAccess witness) of
      Just (Just one) -> pure one
      _ -> assertFailure "resident process bootstrap access is missing"
    assertEqual "identity witness belongs to the applied process" process (witnessedProcessEpoch identity)
    assertAliasEvidence
      identity
      (globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process))
      1
    assertEqual
      "process handle is alias one"
      1
      (privateUniqueIdWord64 (privateProcessUniqueId (Application.bootstrapAccessProcess access)))
    assertEqual "twelve canonical applied roots" 12 (length (appliedProcessRoots bootstrap))
    assertEqual "twelve canonical application handles" 12 (length (Application.bootstrapAccessRoots access))
    mapM_
      (uncurry3 (assertGoldenRoot identity))
      (zip3 goldenRootAliases (appliedProcessRoots bootstrap) (Application.bootstrapAccessRoots access))
    mapM_
      (\(object, alias) -> assertAliasEvidence identity (globalUniqueIdFromGlobalObjectId object) alias)
      (zip (appliedProcessEnvironmentObjects bootstrap) [2 .. 32])
    assertEqual
      "the next unassigned alias follows all thirty-one environment objects"
      33
      (privateUniqueIdWord64 (witnessedNextPrivateUniqueId identity))
  [] -> assertFailure "fixture has no local configured process"

casePrimordialProvenance :: Assertion
casePrimordialProvenance = do
  (state, _) <- initialize fixtureCheckedGenesis [] (monotonicInstant 46)
  let witness = startupStateWitness state
      publications = startupWitnessPrimordialPublications witness
      replicas = checkedPrimordialReplicas fixtureCheckedGenesis
      rawDefinitions = sortOn primordialManifestRole (deploymentPrimordialDefinitions fixtureDeploymentManifest)
  assertEqual "one publication per predefined role" 6 (length publications)
  assertEqual "replica and raw vectors have equal length" (length replicas) (length rawDefinitions)
  assertEqual "state and replica vectors have equal length" (length replicas) (length publications)
  sortDefinitionCarrier <-
    exactlyOne
      "SortDefinition carrier sort"
      [ primordialReplicaSortId replica
      | replica <- replicas,
        primordialReplicaRole replica == SortDefinitionRole
      ]
  mapM_ (assertReplicaProvenance sortDefinitionCarrier) (zip3 publications replicas rawDefinitions)

assertSystemViewStores :: CheckedHeraldGenesis -> StartupStateWitness -> Assertion
assertSystemViewStores genesis witness = do
  let slots = startupWitnessStoreSlots witness
      placements = startupWitnessSystemViewPlacements witness
      replicas = checkedPrimordialReplicas genesis
      system = checkedSystemId genesis
      herald = checkedLocalHeraldEpoch genesis
  assertEqual "one private system-view store per predefined role" 6 (length slots)
  assertEqual "one private system-view placement per predefined role" 6 (length placements)
  mapM_
    ( \replica -> do
        let role = primordialReplicaRole replica
            expectedDelta = deriveSystemViewDeltaId system herald role
            matching =
              [ slot
              | slot <- slots,
                Store.storeSlotDelta slot == expectedDelta
              ]
            matchingPlacements =
              [ placement
              | placement <- placements,
                Placement.systemViewPlacementDelta placement == expectedDelta
              ]
        slot <- exactlyOne ("system-view store " <> show role) matching
        placement <- exactlyOne ("system-view placement " <> show role) matchingPlacements
        assertEqual
          "system-view provenance"
          (Store.HeraldSystemView herald role)
          (Store.storeSlotProvenance slot)
        assertEqual "system-view sort" (primordialReplicaSortId replica) (Store.storeSlotSortId slot)
        assertEqual
          "system-view occurrence"
          (primordialReplicaOccurrenceId replica)
          (Store.storeSlotOccurrenceId slot)
        assertEqual
          "system-view incarnation"
          (deriveSystemViewStoreIncarnationId system herald role)
          (Store.storeSlotIncarnation slot)
        assertEqual "system-view placement role" role (Placement.systemViewPlacementRole placement)
        assertEqual "system-view placement sort" (primordialReplicaSortId replica) (Placement.systemViewPlacementSortId placement)
        assertEqual
          "system-view placement occurrence"
          (primordialReplicaOccurrenceId replica)
          (Placement.systemViewPlacementOccurrenceId placement)
        assertEqual "system-view placement Herald" herald (Placement.systemViewPlacementHeraldEpoch placement)
        assertEqual
          "system-view placement incarnation"
          (deriveSystemViewStoreIncarnationId system herald role)
          (Placement.systemViewPlacementStoreIncarnation placement)
        assertEqual
          "only the sort-definition system view is primordially populated"
          (if role == SortDefinitionRole then 6 else 0)
          (length (visibleInstances (Store.storeSlotContents slot)))
    )
    replicas

data CoherentShape = CoherentShape
  { shapeHeraldCount :: Int,
    shapeAppliedCount :: Int,
    shapeUnappliedCount :: Int
  }
  deriving stock (Show)

coherentShape :: Gen CoherentShape
coherentShape =
  CoherentShape
    <$> chooseInt (1, 4)
    <*> chooseInt (0, 3)
    <*> chooseInt (0, 3)

shrinkCoherentShape :: CoherentShape -> [CoherentShape]
shrinkCoherentShape (CoherentShape heraldCount appliedCount unappliedCount) =
  [ CoherentShape smallerHeralds smallerApplied smallerUnapplied
  | (smallerHeralds, smallerApplied, smallerUnapplied) <- shrink (heraldCount, appliedCount, unappliedCount),
    smallerHeralds >= 1,
    smallerHeralds <= 4,
    smallerApplied >= 0,
    smallerApplied <= 3,
    smallerUnapplied >= 0,
    smallerUnapplied <= 3
  ]

propCoherentDeployments :: Property
propCoherentDeployments =
  forAllShrink coherentShape shrinkCoherentShape $ \shape ->
    let members = generatedMembers (shapeHeraldCount shape)
        configured = generatedConfiguredProcesses members (shapeAppliedCount shape + shapeUnappliedCount shape)
        selected = fmap configuredBootstrapManifestId (take (shapeAppliedCount shape) configured)
     in conjoin
          [ counterexample
              ("perspective " <> show member <> " for " <> show shape)
              (perspectiveInitializes member members configured selected)
          | member <- members
          ]

perspectiveInitializes ::
  HeraldMember ->
  [HeraldMember] ->
  [ConfiguredProcessManifest] ->
  [BootstrapManifestId] ->
  Property
perspectiveInitializes local members configured selected =
  case checkHeraldGenesis (generatedDeployment local members configured) of
    Left problem -> counterexample ("genesis rejected: " <> show problem) False
    Right genesis -> case checkInitialBootstraps genesis (PrimordialProcessManifest selected) of
      Left problem -> counterexample ("initial fixture rejected: " <> show problem) False
      Right bootstraps -> case initialHerald generatedTime genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration of
        Left problem -> counterexample ("initial Herald rejected: " <> show problem) False
        Right (state, effects) ->
          let witness = startupStateWitness state
              applied = take (length selected) configured
              residentCount = length (filter ((== heraldMemberEpoch local) . configuredProcessResidence) applied)
              activeHeraldCount = length members
              selectedCount = length applied
              projected = sort (fmap snd (startupWitnessProjectedBootstraps witness))
           in counterexample ("unexpected startup owner counts: " <> show (projected, length (startupWitnessApplicationIdentities witness), length (startupWitnessControlledProcesses witness), length (startupWitnessControlledRoots witness), length (startupWitnessGraphVertices witness), length (startupWitnessGraphEdges witness), length (startupWitnessPlacements witness), length (startupWitnessSystemViewPlacements witness), length (startupWitnessStoreSlots witness)))
                $ property
                  ( projected == sort selected
                      && length (startupWitnessApplicationIdentities witness) == residentCount
                      && length (startupWitnessControlledProcesses witness) == residentCount
                      && length (startupWitnessControlledRoots witness) == residentCount * 12
                      && length (startupWitnessGraphVertices witness) == activeHeraldCount * 11 + selectedCount * 13
                      && length (startupWitnessGraphEdges witness) == selectedCount * 24
                      && length (startupWitnessPlacements witness) == residentCount * 6
                      && length (startupWitnessSystemViewPlacements witness) == 6
                      && length (startupWitnessStoreSlots witness) == 6 + residentCount * 6
                      && startupWitnessLastObservedTime witness == generatedTime
                      && initialEffectsAreOracleWatchAndGrace effects
                  )
  where
    generatedTime = monotonicInstant 47

generatedDeployment ::
  HeraldMember ->
  [HeraldMember] ->
  [ConfiguredProcessManifest] ->
  DeploymentManifest
generatedDeployment local members configured =
  base
    { deploymentLocalHeraldId = heraldMemberId local,
      deploymentLocalHeraldEpoch = heraldMemberEpoch local,
      deploymentActiveHeralds = members,
      deploymentPrimordialWriterAuthority =
        writer {primordialWriterSourceHeraldEpoch = heraldMemberEpoch firstMember},
      deploymentConfiguredProcesses = configured,
      deploymentOracleGenesis = oracle {oracleGenesisActiveHeralds = members}
    }
  where
    base = fixtureDeploymentManifest
    writer = deploymentPrimordialWriterAuthority base
    oracle = deploymentOracleGenesis base
    firstMember = case members of
      first : _ -> first
      [] -> local

generatedMembers :: Int -> [HeraldMember]
generatedMembers count =
  [ HeraldMember
      (checked "generated HeraldId" (mkHeraldId (fixtureIdentifierBytes (fromIntegral (200 + ordinal)))))
      (checked "generated HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes (fromIntegral (210 + ordinal)))))
  | ordinal <- [0 .. count - 1]
  ]

generatedConfiguredProcesses :: [HeraldMember] -> Int -> [ConfiguredProcessManifest]
generatedConfiguredProcesses members count =
  [ generatedConfiguredProcess members ordinal
  | ordinal <- [0 .. count - 1]
  ]

generatedConfiguredProcess :: [HeraldMember] -> Int -> ConfiguredProcessManifest
generatedConfiguredProcess members ordinal =
  ConfiguredProcessManifest
    { configuredBootstrapManifestId = fixtureBootstrapId (20 + ordinal),
      configuredProcessId = checked "generated ProcessId" (mkProcessId (bytes (30 + ordinal))),
      configuredProcessEpochId = checked "generated ProcessEpochId" (mkProcessEpochId (bytes (40 + ordinal))),
      configuredProcessResidence = heraldMemberEpoch (members !! (ordinal `mod` length members)),
      configuredProcessRoots =
        [ ConfiguredRootManifest
            { configuredRootRole = role,
              configuredWriterNabla = checked "generated NablaId" (mkNablaId (bytes (rootBase + 2 * roleOrdinal))),
              configuredWriterSequencing = UnsequencedNabla,
              configuredReaderDelta = checked "generated DeltaId" (mkDeltaId (bytes (rootBase + 2 * roleOrdinal + 1))),
              configuredReaderStoreIncarnation =
                checked
                  "generated StoreIncarnationId"
                  (mkStoreIncarnationId (bytes (140 + 6 * ordinal + roleOrdinal)))
            }
        | (roleOrdinal, role) <- zip [0 ..] goldenRoles
        ]
    }
  where
    rootBase = 60 + 12 * ordinal

fixtureBootstrapId :: Int -> BootstrapManifestId
fixtureBootstrapId seed =
  checked "generated BootstrapManifestId" (mkBootstrapManifestId (bytes seed))

bytes :: Int -> ByteString
bytes = fixtureIdentifierBytes . fromIntegral

goldenRoles :: [PredefinedSortRole]
goldenRoles =
  [ SortDefinitionRole,
    NeutralVertexRole,
    EdgeRole,
    NablaRole,
    DeltaRole,
    ProcessEpochRole
  ]

data GoldenRootKind = GoldenWriter | GoldenReader
  deriving stock (Eq, Show)

goldenRootAliases :: [(PredefinedSortRole, GoldenRootKind, Word64)]
goldenRootAliases =
  [ (SortDefinitionRole, GoldenWriter, 2),
    (SortDefinitionRole, GoldenReader, 3),
    (NeutralVertexRole, GoldenWriter, 4),
    (NeutralVertexRole, GoldenReader, 5),
    (EdgeRole, GoldenWriter, 6),
    (EdgeRole, GoldenReader, 7),
    (NablaRole, GoldenWriter, 8),
    (NablaRole, GoldenReader, 9),
    (DeltaRole, GoldenWriter, 10),
    (DeltaRole, GoldenReader, 11),
    (ProcessEpochRole, GoldenWriter, 12),
    (ProcessEpochRole, GoldenReader, 13)
  ]

assertGoldenRoot ::
  ProcessIdentityWitness ->
  (PredefinedSortRole, GoldenRootKind, Word64) ->
  AppliedRoot ->
  Application.RootAccess ->
  Assertion
assertGoldenRoot identity (expectedRole, expectedKind, expectedAlias) root access = do
  assertEqual "applied root catalogue role" expectedRole (appliedRootCatalogueRole root)
  case (expectedKind, appliedRootRole root, access) of
    (GoldenWriter, WriterRoot nabla sequencing, Application.WriterAccess actualRole privateNabla actualSequencing) -> do
      assertEqual "writer access role" expectedRole actualRole
      assertEqual "writer sequencing" sequencing actualSequencing
      assertEqual "writer access alias" expectedAlias (privateUniqueIdWord64 (privateNablaUniqueId privateNabla))
      assertAliasEvidence identity (globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId nabla)) expectedAlias
    (GoldenReader, ReaderRoot delta, Application.ReaderAccess actualRole privateDelta) -> do
      assertEqual "reader access role" expectedRole actualRole
      assertEqual "reader access alias" expectedAlias (privateUniqueIdWord64 (privateDeltaUniqueId privateDelta))
      assertAliasEvidence identity (globalUniqueIdFromGlobalObjectId (globalObjectIdFromDeltaId delta)) expectedAlias
    _ -> assertFailure ("root/access kind mismatch for " <> show expectedRole)

assertAliasEvidence :: ProcessIdentityWitness -> GlobalUniqueId -> Word64 -> Assertion
assertAliasEvidence identity globalIdentity expectedAlias =
  case lookup globalIdentity (witnessedBindings identity) of
    Nothing -> assertFailure "forward private-identity binding is absent"
    Just privateIdentity -> do
      assertEqual "forward binding has the golden alias" expectedAlias (privateUniqueIdWord64 privateIdentity)
      assertEqual
        "reverse binding returns the same global identity"
        (Just globalIdentity)
        (lookup privateIdentity (witnessedReverseBindings identity))

assertIndependentProcessAlias :: StartupStateWitness -> ProcessIdentityWitness -> Assertion
assertIndependentProcessAlias startupWitness identity = do
  let process = witnessedProcessEpoch identity
      globalProcess = globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process)
  access <- case lookup process (startupWitnessApplicationAccess startupWitness) of
    Just (Just one) -> pure one
    _ -> assertFailure "multiple-process bootstrap access is missing"
  assertEqual
    "each process independently receives process alias one"
    1
    (privateUniqueIdWord64 (privateProcessUniqueId (Application.bootstrapAccessProcess access)))
  assertAliasEvidence identity globalProcess 1
  assertEqual
    "each process independently advances past its thirty-one environment objects"
    33
    (privateUniqueIdWord64 (witnessedNextPrivateUniqueId identity))

assertReplicaProvenance ::
  SortId ->
  ( CheckedPublication,
    PrimordialDefinitionReplica,
    PrimordialDefinitionManifest
  ) ->
  Assertion
assertReplicaProvenance sortDefinitionCarrier (publication, replica, rawDefinition) = do
  let provenance = checkedPublicationId publication
      authority = primordialReplicaSourceAuthority replica
  assertEqual "replica role follows the raw manifest" (primordialManifestRole rawDefinition) (primordialReplicaRole replica)
  assertEqual "stored publication is the checked replica publication" (primordialReplicaPublication replica) publication
  assertEqual "publication provenance is the checked replica provenance" (primordialReplicaPublicationId replica) provenance
  assertEqual "every definition publication uses the SortDefinition carrier" sortDefinitionCarrier (checkedPublicationSort publication)
  assertEqual "provenance nabla" (primordialWriterNabla authority) (publicationNabla provenance)
  assertEqual "provenance authority epoch" (primordialWriterAuthorityEpoch authority) (publicationAuthorityEpoch provenance)
  assertEqual "provenance source Herald" (primordialWriterSourceHeraldEpoch authority) (publicationSourceHeraldEpoch provenance)
  assertEqual "provenance nabla sequence" (primordialManifestPublicationSequence rawDefinition) (publicationNablaSequence provenance)

initializeAt ::
  HeraldMember ->
  [BootstrapManifestId] ->
  MonotonicInstant ->
  IO (HeraldState, EffectBatch)
initializeAt member selected observed = do
  genesis <- checkedIO "check perspective genesis" (checkHeraldGenesis (fixtureDeploymentAt member))
  initialize genesis selected observed

initialize ::
  CheckedHeraldGenesis ->
  [BootstrapManifestId] ->
  MonotonicInstant ->
  IO (HeraldState, EffectBatch)
initialize genesis selected observed = do
  bootstraps <- checkedInitial genesis selected
  checkedIO "initialize Herald" (initialHerald observed genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)

checkedInitial ::
  CheckedHeraldGenesis ->
  [BootstrapManifestId] ->
  IO CheckedInitialBootstraps
checkedInitial genesis selected =
  checkedIO
    "check initial process fixture"
    (checkInitialBootstraps genesis (PrimordialProcessManifest selected))

assertInitialEffects :: EffectBatch -> Assertion
assertInitialEffects effects = do
  assertBool "startup emits exactly one Oracle connect-and-Hello and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace effects)
  assertEqual "two startup actions" 2 (effectBatchMemberCount effects)

exactlyOne :: String -> [value] -> IO value
exactlyOne _ [value] = pure value
exactlyOne context values = assertFailure (context <> ": expected one, got " <> show (length values))

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

uncurry3 :: (a -> b -> c -> value) -> ((a, b, c)) -> value
uncurry3 function (first, second, third) = function first second third

unique :: (Eq value) => [value] -> [value]
unique = foldr (\value seen -> if value `elem` seen then seen else value : seen) []

systemViewDeltas :: StartupStateWitness -> [DeltaId]
systemViewDeltas witness =
  [ Store.storeSlotDelta slot
  | slot <- startupWitnessStoreSlots witness,
    Store.HeraldSystemView {} <- [Store.storeSlotProvenance slot]
  ]

intersectEqual :: (Eq value) => [value] -> [value] -> [value]
intersectEqual left right = filter (`elem` right) left
