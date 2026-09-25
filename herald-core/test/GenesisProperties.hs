module GenesisProperties
  ( tests,
  )
where

import Data.List (sortOn)
import Eclips.Domain.Identity
  ( controlIndex,
    deltaIdBytes,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    mkDeltaId,
    mkHeraldEpoch,
    mkNablaId,
    mkStructuralSequence,
    mkSystemId,
    mkTopologyCutId,
    nablaIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Sort.Canonical (CanonicalDescriptorError (..))
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Startup
  ( HeraldMember (..),
    PredefinedSortRole (..),
    allStructuralCarrierRoles,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    mkCatalogueDigest,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    ConfiguredProcessManifest (..),
    ConfiguredRootManifest (..),
    DeploymentManifest (..),
    GenesisAuthority (..),
    GenesisError (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkedConfigurationDigest,
    checkedConfiguredProcessBootstraps,
    checkedLocalSystemBootstrapWriter,
    checkedPrimordialReplicas,
    checkedSystemBootstrapWriters,
    checkedSystemId,
    lookupSystemBootstrapWriter,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
    systemBootstrapWriterAuthority,
    systemBootstrapWriterNablaId,
    systemBootstrapWriterResidence,
    systemBootstrapWriterRole,
  )
import GenesisFixtures
  ( fixtureConfiguredProcesses,
    fixtureDeploymentAt,
    fixtureDeploymentManifest,
    fixtureIdentifierBytes,
    fixtureLocalMember,
    fixtureRemoteMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "checked genesis"
    [ testCase "the coherent profile fixture is admitted" caseValidFixture,
      testProperty "presentation order cannot change checked genesis" propPresentationOrder,
      testCase "the configuration digest is independent of the local perspective" casePerspectiveDigest,
      testProperty "unused process templates cannot change immutable configuration" propFutureTemplatesExcluded,
      testCase "hidden bootstrap writers are the canonical checked member/carrier product" caseSystemBootstrapWriters,
      testCase "duplicate membership is rejected" caseDuplicateMembership,
      testCase "duplicate Herald epochs are rejected distinctly" caseDuplicateHeraldEpoch,
      testCase "missing local membership is rejected" caseMissingLocalMembership,
      testCase "a changed catalogue descriptor is rejected" caseChangedCatalogueDescriptor,
      testCase "a changed catalogue hash claim is rejected" caseChangedCatalogueHash,
      testCase "a missing catalogue role is rejected" caseMissingCatalogueRole,
      testCase "a duplicate catalogue role is rejected distinctly" caseDuplicateCatalogueRole,
      testCase "a changed primordial descriptor is rejected" caseChangedPrimordialDescriptor,
      testCase "a missing primordial role is rejected" caseMissingPrimordialRole,
      testCase "a duplicate primordial role is rejected distinctly" caseDuplicatePrimordialRole,
      testCase "duplicate primordial publication sequence is rejected" caseDuplicatePublicationSequence,
      testCase "an inactive primordial source is rejected" caseInactivePrimordialSource,
      testCase "a configured residence outside membership is rejected" caseUnknownResidence,
      testCase "an incomplete configured root set is rejected" caseIncompleteRootSet,
      testCase "a duplicate configured root role is rejected distinctly" caseDuplicateRootRole,
      testCase "duplicate configured manifest ids are rejected" caseDuplicateConfiguredManifest,
      testCase "duplicate configured process epochs are rejected" caseDuplicateConfiguredProcessEpoch,
      testCase "globally convertible configured identities are disjoint" caseDuplicateGlobalIdentity,
      testCase "configured identities are disjoint from the primordial writer" casePrimordialWriterCollision,
      testCase "configured identities are disjoint from hidden bootstrap writers" caseBootstrapWriterCollision,
      testCase "configured reader identities cannot reuse hidden-writer bytes" caseBootstrapWriterCrossNominalCollision,
      testCase "configured identities are disjoint from private system views" caseSystemViewCollision,
      testCase "configured writer identities cannot reuse system-view bytes" caseSystemViewCrossNominalCollision,
      testCase "store incarnations are globally distinct" caseDuplicateStoreIncarnation,
      testCase "configured stores are disjoint from private system views" caseSystemViewStoreIncarnationCollision,
      testCase "primordial authority must be the Genesis variant" caseNonGenesisAuthority,
      testCase "the Oracle control index is independently zero" caseOracleControlIndex,
      testCase "the index-zero Oracle projection must agree" caseOracleMismatch,
      testCase "the Oracle membership projection must agree" caseOracleMembershipMismatch,
      testCase "the Oracle catalogue binding must agree" caseOracleCatalogueMismatch,
      testCase "derived primordial occurrences are stable and role ordered" caseDerivedOccurrences
    ]

caseValidFixture :: IO ()
caseValidFixture =
  assertBool "fixture checks" (either (const False) (const True) (checkHeraldGenesis fixtureDeploymentManifest))

propPresentationOrder :: NonNegative Int -> Bool -> Bool -> Property
propPresentationOrder (NonNegative amount) flipMembers flipCatalogue =
  checkHeraldGenesis permuted === checkHeraldGenesis fixtureDeploymentManifest
  where
    base = fixtureDeploymentManifest
    reorder :: Bool -> [a] -> [a]
    reorder shouldReverse values =
      rotate amount (if shouldReverse then reverse values else values)
    configured =
      [ process {configuredProcessRoots = reorder flipCatalogue (configuredProcessRoots process)}
      | process <- reorder flipMembers (deploymentConfiguredProcesses base)
      ]
    oracle = deploymentOracleGenesis base
    permuted =
      base
        { deploymentActiveHeralds = reorder flipMembers (deploymentActiveHeralds base),
          deploymentPredefinedCatalogue = reorder flipCatalogue (deploymentPredefinedCatalogue base),
          deploymentPrimordialDefinitions = reorder flipMembers (deploymentPrimordialDefinitions base),
          deploymentConfiguredProcesses = configured,
          deploymentOracleGenesis =
            oracle
              { oracleGenesisActiveHeralds =
                  reorder flipCatalogue (oracleGenesisActiveHeralds oracle)
              }
        }

casePerspectiveDigest :: IO ()
casePerspectiveDigest = do
  local <- checkedGenesis (fixtureDeploymentAt fixtureLocalMember)
  remote <- checkedGenesis (fixtureDeploymentAt fixtureRemoteMember)
  assertEqual
    "all Heralds retain the same configuration binding"
    (checkedConfigurationDigest local)
    (checkedConfigurationDigest remote)

propFutureTemplatesExcluded :: NonNegative Int -> Property
propFutureTemplatesExcluded (NonNegative count) =
  fmap checkedConfigurationDigest (checkHeraldGenesis altered)
    === fmap checkedConfigurationDigest (checkHeraldGenesis fixtureDeploymentManifest)
  where
    altered =
      fixtureDeploymentManifest
        { deploymentConfiguredProcesses = take (count `mod` (length fixtureConfiguredProcesses + 1)) fixtureConfiguredProcesses
        }

caseSystemBootstrapWriters :: IO ()
caseSystemBootstrapWriters = do
  local <- checkedGenesis fixtureDeploymentManifest
  remote <- checkedGenesis (fixtureDeploymentAt fixtureRemoteMember)
  let writers = checkedSystemBootstrapWriters local
      expectedKeys =
        [ (heraldMemberEpoch member, role)
        | member <- [fixtureLocalMember, fixtureRemoteMember],
          role <- allStructuralCarrierRoles
        ]
  assertEqual
    "canonical member/carrier order"
    expectedKeys
    [ (systemBootstrapWriterResidence writer, systemBootstrapWriterRole writer)
    | writer <- writers
    ]
  assertEqual
    "every hidden source has the closed genesis authority"
    (replicate (length expectedKeys) GenesisAuthority)
    (fmap systemBootstrapWriterAuthority writers)
  assertEqual
    "all Herald perspectives derive the same hidden sources"
    writers
    (checkedSystemBootstrapWriters remote)
  mapM_
    ( \writer ->
        assertEqual
          "narrow lookup resolves the exact checked source"
          (Just writer)
          ( lookupSystemBootstrapWriter
              (systemBootstrapWriterResidence writer)
              (systemBootstrapWriterRole writer)
              local
          )
    )
    writers
  assertEqual
    "local lookup is derived from the checked local residence"
    (lookupSystemBootstrapWriter (heraldMemberEpoch fixtureLocalMember) NablaCarrier local)
    (Just (checkedLocalSystemBootstrapWriter NablaCarrier local))

caseDuplicateMembership :: IO ()
caseDuplicateMembership =
  assertGenesisError
    (GenesisDuplicateHeraldId (heraldMemberId fixtureLocalMember))
    fixtureDeploymentManifest
      { deploymentActiveHeralds = fixtureLocalMember : deploymentActiveHeralds fixtureDeploymentManifest
      }

caseDuplicateHeraldEpoch :: IO ()
caseDuplicateHeraldEpoch =
  assertGenesisError
    (GenesisDuplicateHeraldEpoch (heraldMemberEpoch fixtureLocalMember))
    fixtureDeploymentManifest
      { deploymentActiveHeralds =
          [ fixtureLocalMember,
            fixtureRemoteMember {heraldMemberEpoch = heraldMemberEpoch fixtureLocalMember}
          ]
      }

caseMissingLocalMembership :: IO ()
caseMissingLocalMembership =
  assertGenesisError
    ( GenesisLocalMembershipMissing
        (heraldMemberId fixtureLocalMember)
        (heraldMemberEpoch fixtureLocalMember)
    )
    fixtureDeploymentManifest
      { deploymentActiveHeralds = [fixtureRemoteMember]
      }

caseChangedCatalogueDescriptor :: IO ()
caseChangedCatalogueDescriptor = case deploymentPredefinedCatalogue fixtureDeploymentManifest of
  first : second : remaining ->
    assertGenesisError
      ( GenesisCatalogueDescriptorRejected
          (catalogueManifestRole first)
          ( DescriptorClaimedSortIdMismatch
              (catalogueManifestSortId first)
              (catalogueManifestSortId second)
          )
      )
      fixtureDeploymentManifest
        { deploymentPredefinedCatalogue =
            first
              { catalogueManifestDescriptorBytes = catalogueManifestDescriptorBytes second
              }
              : second
              : remaining
        }
  _ -> fail "profile catalogue unexpectedly has fewer than two entries"

caseChangedCatalogueHash :: IO ()
caseChangedCatalogueHash = case deploymentPredefinedCatalogue fixtureDeploymentManifest of
  first : second : remaining ->
    assertGenesisError
      ( GenesisCatalogueDescriptorRejected
          (catalogueManifestRole first)
          ( DescriptorClaimedSortIdMismatch
              (catalogueManifestSortId second)
              (catalogueManifestSortId first)
          )
      )
      fixtureDeploymentManifest
        { deploymentPredefinedCatalogue =
            first {catalogueManifestSortId = catalogueManifestSortId second}
              : second
              : remaining
        }
  _ -> fail "profile catalogue unexpectedly has fewer than two entries"

caseMissingCatalogueRole :: IO ()
caseMissingCatalogueRole = case deploymentPredefinedCatalogue fixtureDeploymentManifest of
  first : remaining ->
    assertGenesisError
      (GenesisCatalogueRoleMissing (catalogueManifestRole first))
      fixtureDeploymentManifest
        { deploymentPredefinedCatalogue = remaining
        }
  [] -> fail "profile catalogue unexpectedly empty"

caseDuplicateCatalogueRole :: IO ()
caseDuplicateCatalogueRole = case deploymentPredefinedCatalogue fixtureDeploymentManifest of
  first : second : remaining ->
    assertGenesisError
      (GenesisCatalogueRoleDuplicate (catalogueManifestRole first))
      fixtureDeploymentManifest
        { deploymentPredefinedCatalogue =
            first : second {catalogueManifestRole = catalogueManifestRole first} : remaining
        }
  _ -> fail "profile catalogue unexpectedly has fewer than two entries"

caseChangedPrimordialDescriptor :: IO ()
caseChangedPrimordialDescriptor =
  case (deploymentPrimordialDefinitions fixtureDeploymentManifest, deploymentPredefinedCatalogue fixtureDeploymentManifest) of
    (first : second : remaining, firstCatalogue : secondCatalogue : _) ->
      assertGenesisError
        ( GenesisPrimordialDescriptorRejected
            (primordialManifestRole first)
            ( DescriptorClaimedSortIdMismatch
                (catalogueManifestSortId firstCatalogue)
                (catalogueManifestSortId secondCatalogue)
            )
        )
        fixtureDeploymentManifest
          { deploymentPrimordialDefinitions =
              first
                { primordialManifestDescriptorBytes = primordialManifestDescriptorBytes second
                }
                : second
                : remaining
          }
    _ -> fail "profile primordial/catalogue set unexpectedly has fewer than two entries"

caseMissingPrimordialRole :: IO ()
caseMissingPrimordialRole = case deploymentPrimordialDefinitions fixtureDeploymentManifest of
  first : remaining ->
    assertGenesisError
      (GenesisPrimordialRoleMissing (primordialManifestRole first))
      fixtureDeploymentManifest
        { deploymentPrimordialDefinitions = remaining
        }
  [] -> fail "profile primordial set unexpectedly empty"

caseDuplicatePrimordialRole :: IO ()
caseDuplicatePrimordialRole = case deploymentPrimordialDefinitions fixtureDeploymentManifest of
  first : second : remaining ->
    assertGenesisError
      (GenesisPrimordialRoleDuplicate (primordialManifestRole first))
      fixtureDeploymentManifest
        { deploymentPrimordialDefinitions =
            first : second {primordialManifestRole = primordialManifestRole first} : remaining
        }
  _ -> fail "profile primordial set unexpectedly has fewer than two entries"

caseDuplicatePublicationSequence :: IO ()
caseDuplicatePublicationSequence = case deploymentPrimordialDefinitions fixtureDeploymentManifest of
  first : second : remaining ->
    assertGenesisError
      GenesisPrimordialPublicationSequenceDuplicate
      fixtureDeploymentManifest
        { deploymentPrimordialDefinitions =
            first
              : second
                { primordialManifestPublicationSequence =
                    primordialManifestPublicationSequence first
                }
              : remaining
        }
  _ -> fail "profile primordial set unexpectedly has fewer than two entries"

caseInactivePrimordialSource :: IO ()
caseInactivePrimordialSource = do
  source <- checked "inactive HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes 203))
  let writer = deploymentPrimordialWriterAuthority fixtureDeploymentManifest
  assertGenesisError
    (GenesisPrimordialSourceNotActive source)
    fixtureDeploymentManifest
      { deploymentPrimordialWriterAuthority =
          writer {primordialWriterSourceHeraldEpoch = source}
      }

caseUnknownResidence :: IO ()
caseUnknownResidence = case fixtureConfiguredProcesses of
  first : remaining -> do
    unknown <- checked "Nabla-shaped fixture bytes" (mkNablaId (fixtureIdentifierBytes 200))
    let impossibleResidenceBytes = nablaIdBytes unknown
    residence <- checked "HeraldEpoch" (mkHeraldEpoch impossibleResidenceBytes)
    assertGenesisError
      (GenesisConfiguredResidenceNotActive (configuredBootstrapManifestId first) residence)
      fixtureDeploymentManifest
        { deploymentConfiguredProcesses =
            first {configuredProcessResidence = residence} : remaining
        }
  [] -> fail "fixture has no configured process"

caseIncompleteRootSet :: IO ()
caseIncompleteRootSet = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : otherRoots ->
      assertGenesisError
        (GenesisConfiguredRootMissing (configuredBootstrapManifestId first) (configuredRootRole firstRoot))
        fixtureDeploymentManifest
          { deploymentConfiguredProcesses =
              first {configuredProcessRoots = otherRoots} : remaining
          }
    [] -> fail "fixture process has no roots"
  [] -> fail "fixture has no configured process"

caseDuplicateRootRole :: IO ()
caseDuplicateRootRole = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : secondRoot : otherRoots ->
      assertGenesisError
        (GenesisConfiguredRootDuplicate (configuredBootstrapManifestId first) (configuredRootRole firstRoot))
        fixtureDeploymentManifest
          { deploymentConfiguredProcesses =
              first
                { configuredProcessRoots =
                    firstRoot : secondRoot {configuredRootRole = configuredRootRole firstRoot} : otherRoots
                }
                : remaining
          }
    _ -> fail "fixture process has fewer than two roots"
  [] -> fail "fixture has no configured process"

caseDuplicateConfiguredManifest :: IO ()
caseDuplicateConfiguredManifest = case fixtureConfiguredProcesses of
  first : second : remaining ->
    assertGenesisError
      (GenesisConfiguredManifestIdDuplicate (configuredBootstrapManifestId first))
      fixtureDeploymentManifest
        { deploymentConfiguredProcesses =
            first
              : second {configuredBootstrapManifestId = configuredBootstrapManifestId first}
              : remaining
        }
  _ -> fail "fixture has fewer than two configured processes"

caseDuplicateConfiguredProcessEpoch :: IO ()
caseDuplicateConfiguredProcessEpoch = case fixtureConfiguredProcesses of
  first : second : remaining ->
    assertGenesisError
      (GenesisConfiguredProcessEpochDuplicate (configuredProcessEpochId first))
      fixtureDeploymentManifest
        { deploymentConfiguredProcesses =
            first
              : second {configuredProcessEpochId = configuredProcessEpochId first}
              : remaining
        }
  _ -> fail "fixture has fewer than two configured processes"

caseDuplicateGlobalIdentity :: IO ()
caseDuplicateGlobalIdentity = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : otherRoots -> do
      duplicate <- checked "NablaId" (mkNablaId (fixtureIdentifierBytes 30))
      let changedRoot = firstRoot {configuredWriterNabla = duplicate}
      assertGenesisError
        (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromNablaId duplicate))
        fixtureDeploymentManifest
          { deploymentConfiguredProcesses =
              first {configuredProcessRoots = changedRoot : otherRoots} : remaining
          }
    [] -> fail "fixture process has no roots"
  [] -> fail "fixture has no configured process"

casePrimordialWriterCollision :: IO ()
casePrimordialWriterCollision = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : otherRoots ->
      let writer =
            primordialWriterNabla
              (deploymentPrimordialWriterAuthority fixtureDeploymentManifest)
          changedRoot = firstRoot {configuredWriterNabla = writer}
       in assertGenesisError
            (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromNablaId writer))
            fixtureDeploymentManifest
              { deploymentConfiguredProcesses =
                  first {configuredProcessRoots = changedRoot : otherRoots} : remaining
              }
    [] -> fail "fixture process has no roots"
  [] -> fail "fixture has no configured process"

caseBootstrapWriterCollision :: IO ()
caseBootstrapWriterCollision = do
  genesis <- checkedGenesis fixtureDeploymentManifest
  let hidden =
        systemBootstrapWriterNablaId
          (checkedLocalSystemBootstrapWriter NablaCarrier genesis)
  case fixtureConfiguredProcesses of
    first : remaining -> case configuredProcessRoots first of
      firstRoot : otherRoots ->
        assertGenesisError
          (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromNablaId hidden))
          fixtureDeploymentManifest
            { deploymentConfiguredProcesses =
                first
                  { configuredProcessRoots =
                      firstRoot {configuredWriterNabla = hidden} : otherRoots
                  }
                  : remaining
            }
      [] -> fail "fixture process has no roots"
    [] -> fail "fixture has no configured process"

caseBootstrapWriterCrossNominalCollision :: IO ()
caseBootstrapWriterCrossNominalCollision = do
  genesis <- checkedGenesis fixtureDeploymentManifest
  let hidden =
        systemBootstrapWriterNablaId
          (checkedLocalSystemBootstrapWriter NablaCarrier genesis)
  reader <- checked "cross-nominal hidden-writer DeltaId" (mkDeltaId (nablaIdBytes hidden))
  case fixtureConfiguredProcesses of
    first : remaining -> case configuredProcessRoots first of
      firstRoot : otherRoots ->
        assertGenesisError
          (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromDeltaId reader))
          fixtureDeploymentManifest
            { deploymentConfiguredProcesses =
                first
                  { configuredProcessRoots =
                      firstRoot {configuredReaderDelta = reader} : otherRoots
                  }
                  : remaining
            }
      [] -> fail "fixture process has no roots"
    [] -> fail "fixture has no configured process"

caseSystemViewCollision :: IO ()
caseSystemViewCollision = do
  genesis <- checkedGenesis fixtureDeploymentManifest
  let systemView =
        deriveSystemViewDeltaId
          (checkedSystemId genesis)
          (heraldMemberEpoch fixtureLocalMember)
          NablaRole
  case fixtureConfiguredProcesses of
    first : remaining -> case configuredProcessRoots first of
      firstRoot : otherRoots ->
        assertGenesisError
          (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromDeltaId systemView))
          fixtureDeploymentManifest
            { deploymentConfiguredProcesses =
                first
                  { configuredProcessRoots =
                      firstRoot {configuredReaderDelta = systemView} : otherRoots
                  }
                  : remaining
            }
      [] -> fail "fixture process has no roots"
    [] -> fail "fixture has no configured process"

caseSystemViewCrossNominalCollision :: IO ()
caseSystemViewCrossNominalCollision = do
  genesis <- checkedGenesis fixtureDeploymentManifest
  let systemView =
        deriveSystemViewDeltaId
          (checkedSystemId genesis)
          (heraldMemberEpoch fixtureLocalMember)
          NablaRole
  writer <- checked "cross-nominal system-view NablaId" (mkNablaId (deltaIdBytes systemView))
  case fixtureConfiguredProcesses of
    first : remaining -> case configuredProcessRoots first of
      firstRoot : otherRoots ->
        assertGenesisError
          (GenesisConfiguredGlobalIdentityDuplicate (globalObjectIdFromNablaId writer))
          fixtureDeploymentManifest
            { deploymentConfiguredProcesses =
                first
                  { configuredProcessRoots =
                      firstRoot {configuredWriterNabla = writer} : otherRoots
                  }
                  : remaining
            }
      [] -> fail "fixture process has no roots"
    [] -> fail "fixture has no configured process"

caseDuplicateStoreIncarnation :: IO ()
caseDuplicateStoreIncarnation = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : secondRoot : otherRoots ->
      let changedRoot =
            secondRoot
              { configuredReaderStoreIncarnation =
                  configuredReaderStoreIncarnation firstRoot
              }
       in assertGenesisError
            (GenesisStoreIncarnationDuplicate (configuredReaderStoreIncarnation firstRoot))
            fixtureDeploymentManifest
              { deploymentConfiguredProcesses =
                  first
                    { configuredProcessRoots =
                        firstRoot : changedRoot : otherRoots
                    }
                    : remaining
              }
    _ -> fail "fixture process has fewer than two roots"
  [] -> fail "fixture has no configured process"

caseSystemViewStoreIncarnationCollision :: IO ()
caseSystemViewStoreIncarnationCollision = case fixtureConfiguredProcesses of
  first : remaining -> case configuredProcessRoots first of
    firstRoot : otherRoots ->
      let systemViewIncarnation =
            deriveSystemViewStoreIncarnationId
              (deploymentSystemId fixtureDeploymentManifest)
              (heraldMemberEpoch fixtureLocalMember)
              NablaRole
          changedRoot =
            firstRoot
              { configuredReaderStoreIncarnation = systemViewIncarnation
              }
       in assertGenesisError
            (GenesisStoreIncarnationDuplicate systemViewIncarnation)
            fixtureDeploymentManifest
              { deploymentConfiguredProcesses =
                  first
                    { configuredProcessRoots = changedRoot : otherRoots
                    }
                    : remaining
              }
    [] -> fail "fixture process has no roots"
  [] -> fail "fixture has no configured process"

caseNonGenesisAuthority :: IO ()
caseNonGenesisAuthority = do
  sequenceNumber <- checked "StructuralSequence" (mkStructuralSequence 1)
  cut <- checked "TopologyCutId" (mkTopologyCutId (fixtureIdentifierBytes 204))
  let writer = deploymentPrimordialWriterAuthority fixtureDeploymentManifest
      structuralAuthority =
        structuralAuthorityEpoch
          (structuralOccurrenceId (heraldMemberEpoch fixtureLocalMember) sequenceNumber)
          cut
  assertGenesisError
    GenesisAuthorityNotAtControlIndexZero
    fixtureDeploymentManifest
      { deploymentPrimordialWriterAuthority =
          writer {primordialWriterAuthorityEpoch = structuralAuthority}
      }

caseOracleControlIndex :: IO ()
caseOracleControlIndex =
  let oracle = deploymentOracleGenesis fixtureDeploymentManifest
   in assertEqual
        "typed nonzero control index"
        (Left GenesisOracleControlIndexNotZero)
        ( checkHeraldGenesis
            fixtureDeploymentManifest
              { deploymentOracleGenesis =
                  oracle {oracleGenesisControlIndex = controlIndex 1}
              }
        )

caseOracleMismatch :: IO ()
caseOracleMismatch = do
  otherSystem <- checked "SystemId" (mkSystemId (fixtureIdentifierBytes 201))
  let oracle = deploymentOracleGenesis fixtureDeploymentManifest
  assertGenesisError
    GenesisOracleSystemMismatch
    fixtureDeploymentManifest
      { deploymentOracleGenesis =
          oracle {oracleGenesisSystemId = otherSystem}
      }

caseOracleMembershipMismatch :: IO ()
caseOracleMembershipMismatch =
  let oracle = deploymentOracleGenesis fixtureDeploymentManifest
   in assertGenesisError
        GenesisOracleMembershipMismatch
        fixtureDeploymentManifest
          { deploymentOracleGenesis =
              oracle {oracleGenesisActiveHeralds = [fixtureLocalMember]}
          }

caseOracleCatalogueMismatch :: IO ()
caseOracleCatalogueMismatch = do
  otherDigest <- checked "CatalogueDigest" (mkCatalogueDigest (fixtureIdentifierBytes 202))
  let oracle = deploymentOracleGenesis fixtureDeploymentManifest
  assertEqual
    "typed independent-role mismatch"
    (Left GenesisOracleCatalogueMismatch)
    ( checkHeraldGenesis
        fixtureDeploymentManifest
          { deploymentOracleGenesis =
              oracle {oracleGenesisCatalogueDigest = otherDigest}
          }
    )

caseDerivedOccurrences :: IO ()
caseDerivedOccurrences = do
  genesis <- checkedGenesis fixtureDeploymentManifest
  let replicas = checkedPrimordialReplicas genesis
  assertEqual
    "role order"
    (sortOn id (fmap primordialReplicaRole replicas))
    (fmap primordialReplicaRole replicas)
  assertEqual
    "one occurrence per role"
    (length replicas)
    (length (unique (fmap primordialReplicaOccurrenceId replicas)))
  assertEqual
    "every configured process survives checking"
    (length fixtureConfiguredProcesses)
    (length (checkedConfiguredProcessBootstraps genesis))

rotate :: Int -> [value] -> [value]
rotate _ [] = []
rotate amount values = drop offset values <> take offset values
  where
    offset = amount `mod` length values

unique :: (Eq value) => [value] -> [value]
unique [] = []
unique (value : remaining) = value : unique (filter (/= value) remaining)

assertGenesisError :: GenesisError -> DeploymentManifest -> IO ()
assertGenesisError expected manifest =
  assertEqual "typed genesis rejection" (Left expected) (checkHeraldGenesis manifest)

checkedGenesis :: DeploymentManifest -> IO CheckedHeraldGenesis
checkedGenesis = either (fail . show) pure . checkHeraldGenesis

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (fail . ((context <> ": ") <>) . show) pure
