module Eclips.ModuleBoundaries
  ( Violation,
    checkProject,
    renderViolations,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import Data.List (intercalate, isPrefixOf, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Compat.NonEmptySet qualified as NonEmptySet
import Distribution.Fields qualified as Fields
import Distribution.Package (packageName)
import Distribution.PackageDescription
  ( Benchmark (benchmarkBuildInfo),
    BuildInfo (buildToolDepends, defaultLanguage, hsSourceDirs, otherModules, targetBuildDepends),
    CondTree (condTreeComponents, condTreeData),
    Dependency (Dependency),
    Executable (buildInfo),
    ForeignLib (foreignLibBuildInfo),
    GenericPackageDescription
      ( condBenchmarks,
        condExecutables,
        condForeignLibs,
        condLibrary,
        condSubLibraries,
        condTestSuites
      ),
    Library (exposedModules, libBuildInfo, libVisibility, reexportedModules),
    LibraryName (LMainLibName, LSubLibName),
    LibraryVisibility (LibraryVisibilityPrivate, LibraryVisibilityPublic),
    TestSuite (testBuildInfo),
  )
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Utils.Path (getSymbolicPath)
import Eclips.ModuleBoundaries.MembershipLedger
  ( MembershipSource,
    auditMembershipLedger,
    membershipSource,
  )
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    pathIsSymbolicLink,
  )
import System.FilePath
  ( makeRelative,
    normalise,
    pathSeparator,
    takeExtension,
    (</>),
  )

newtype Violation = Violation String
  deriving stock (Eq, Show)

renderViolations :: [Violation] -> String
renderViolations = unlines . fmap (\(Violation message) -> "module boundary violation: " <> message)

data PackageKey
  = PublicTypes
  | ApplicationTypes
  | Domain
  | ProtocolFrame
  | ProtocolApplication
  | ProtocolAdmin
  | ProtocolPeer
  | RaftCore
  | OracleCore
  | ProtocolRaft
  | ProtocolOracle
  | HeraldCore
  | ApplicationClient
  | ApplicationApi
  | OracleRuntime
  | HeraldRuntime
  | Deployment
  | HelloWorldExample
  | HelloContextExample
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data PackageClass
  = ArchitecturePackage
  | AuxiliaryExample
  deriving stock (Eq, Show)

data RuntimeImportPolicy
  = RuntimeFree
  | RuntimeBoundary
  deriving stock (Eq, Show)

data PackageSpec = PackageSpec
  { specKey :: PackageKey,
    specClass :: PackageClass,
    specRelativeRoot :: FilePath,
    specCabalFile :: FilePath,
    specExpectedPackageName :: String,
    specExpectedComponents :: Map String ComponentExpectation,
    specProductionSourceRoots :: [FilePath],
    specAllowedEclipsRoots :: [String],
    specAllowedEclipsModules :: [String],
    specRuntimeImportPolicy :: RuntimeImportPolicy,
    specDiagnosticLabel :: String
  }

data PackageView = PackageView
  { packageSpec :: PackageSpec,
    packageRoot :: FilePath,
    declaredPackageName :: String,
    components :: Map String ComponentView,
    conditionalComponents :: [String]
  }

data ComponentView = ComponentView
  { sourceDirectories :: [FilePath],
    dependencies :: Set String,
    executableDependencies :: Set String,
    languageEdition :: Maybe String,
    visibility :: Maybe String,
    exposedModuleNames :: [String],
    otherModuleNames :: [String],
    reexportedModuleNames :: [String]
  }
  deriving stock (Eq, Show)

data ComponentExpectation = ComponentExpectation
  { expectedSources :: [FilePath],
    expectedDependencies :: Set String,
    expectedExecutableDependencies :: Set String,
    expectedLanguageEdition :: String,
    expectedVisibility :: Maybe String,
    expectedExposedModules :: Maybe [String],
    expectedOtherModules :: Maybe [String],
    expectedReexportedModules :: Maybe [String]
  }

checkProject :: FilePath -> IO [Violation]
checkProject rawProjectRoot = do
  let projectRoot = normalise rawProjectRoot
  projectViolations <- checkProjectFile (projectRoot </> "cabal.project")
  packageResults <- traverse (loadPackage projectRoot) packageSpecs
  let parseViolations = [violation | Left violation <- packageResults]
      parsedPackages = [packageView | Right packageView <- packageResults]
      packageViolations = concatMap checkPackage parsedPackages
  sourceResults <- traverse checkPackageSources parsedPackages
  let importViolations = foldMap fst sourceResults
      membershipViolations =
        fmap
          Violation
          (auditMembershipLedger (foldMap snd sourceResults))
  pure
    ( projectViolations
        <> parseViolations
        <> packageViolations
        <> importViolations
        <> membershipViolations
    )

packageSpecs :: [PackageSpec]
packageSpecs = specForPackage <$> [minBound .. maxBound]

specForPackage :: PackageKey -> PackageSpec
specForPackage = \case
  PublicTypes ->
    architectureSpec PublicTypes "public-types" "eclips-public-types.cabal" "eclips-public-types" ["src"] ["Eclips.Public.Types"] RuntimeFree "public shared types"
  ApplicationTypes ->
    architectureSpec ApplicationTypes "application-types" "eclips-application-types.cabal" "eclips-application-types" ["src"] ["Eclips.Application.Types", "Eclips.Public.Types"] RuntimeFree "application-facing types"
  Domain ->
    architectureSpec Domain "domain" "eclips-domain.cabal" "eclips-domain" ["src"] ["Eclips.Domain", "Eclips.Public.Types"] RuntimeFree "semantic Domain"
  ProtocolFrame ->
    architectureSpec ProtocolFrame "protocol-frame" "eclips-protocol-frame.cabal" "eclips-protocol-frame" ["src"] ["Eclips.Protocol.Frame"] RuntimeFree "family-neutral frame protocol"
  ProtocolApplication ->
    architectureSpec ProtocolApplication "protocol-application" "eclips-protocol-application.cabal" "eclips-protocol-application" ["src"] ["Eclips.Application.Types", "Eclips.Protocol.Application", "Eclips.Protocol.Frame", "Eclips.Public.Types"] RuntimeFree "application protocol"
  ProtocolAdmin ->
    (architectureSpec ProtocolAdmin "protocol-admin" "eclips-protocol-admin.cabal" "eclips-protocol-admin" ["src"] ["Eclips.Protocol.Admin", "Eclips.Protocol.Frame"] RuntimeFree "Start/End administration protocol")
      { specAllowedEclipsModules =
          protocolAdminExternalModules <> diagnosticRenderingModule <> receiptRetirementModule
      }
  ProtocolPeer ->
    architectureSpec ProtocolPeer "protocol-peer" "eclips-protocol-peer.cabal" "eclips-protocol-peer" ["src"] ["Eclips.Application.Types.Lifecycle", "Eclips.Protocol.Frame", "Eclips.Protocol.Peer", "Eclips.Public.Types"] RuntimeFree "peer protocol"
  RaftCore ->
    (architectureSpec RaftCore "raft-core" "eclips-raft-core.cabal" "eclips-raft-core" ["src"] ["Eclips.Raft"] RuntimeFree "pure Raft core")
      { specAllowedEclipsModules = diagnosticRenderingModule
      }
  OracleCore ->
    (architectureSpec OracleCore "oracle-core" "eclips-oracle-core.cabal" "eclips-oracle-core" ["src"] [] RuntimeFree "pure Oracle core")
      { specAllowedEclipsModules =
          oracleCoreModules
            <> oracleCoreInternalModules
            <> oracleCoreExternalModules
            <> receiptRetirementModule
            <> diagnosticRenderingModule
      }
  ProtocolRaft ->
    (architectureSpec ProtocolRaft "protocol-raft" "eclips-protocol-raft.cabal" "eclips-protocol-raft" ["src"] [] RuntimeFree "Raft protocol")
      { specAllowedEclipsModules =
          protocolRaftModules
            <> protocolRaftExternalModules
            <> diagnosticRenderingModule
      }
  ProtocolOracle ->
    (architectureSpec ProtocolOracle "protocol-oracle" "eclips-protocol-oracle.cabal" "eclips-protocol-oracle" ["src"] [] RuntimeFree "Oracle protocol")
      { specAllowedEclipsModules =
          protocolOracleModules
            <> protocolOracleExternalModules
            <> receiptRetirementModule
            <> diagnosticRenderingModule
      }
  HeraldCore ->
    (architectureSpec HeraldCore "herald-core" "eclips-herald-core.cabal" "eclips-herald-core" ["src", "internal"] ["Eclips.Application.Types", "Eclips.Domain", "Eclips.Herald", "Eclips.Protocol.Application", "Eclips.Protocol.Peer", "Eclips.Public.Types"] RuntimeFree "Herald core")
      { specAllowedEclipsModules = heraldOracleExternalModules <> protocolAdminModules
      }
  ApplicationClient ->
    (architectureSpec ApplicationClient "application-client" "eclips-application-client.cabal" "eclips-application-client" ["src"] ["Eclips.Application.Client", "Eclips.Application.Types", "Eclips.Protocol.Application"] RuntimeFree "pure application client")
      { specAllowedEclipsModules = receiptRetirementModule
      }
  ApplicationApi ->
    (architectureSpec ApplicationApi "application-api" "eclips-application-api.cabal" "eclips-application-api" ["src", "internal"] ["Eclips.Application", "Eclips.Protocol.Application"] RuntimeBoundary "application TCP facade")
      { specAllowedEclipsModules = ["Eclips.Public.Types.Timing", "Eclips.Public.Types.SortCatalogue"]
      }
  OracleRuntime ->
    (architectureSpec OracleRuntime "oracle-runtime" "eclips-oracle-runtime.cabal" "eclips-oracle-runtime" ["src", "internal", "socket-internal"] [] RuntimeBoundary "Oracle/Raft runtime")
      { specAllowedEclipsModules =
          oracleRuntimeModules
            <> oracleRuntimeInternalModules
            <> oracleRuntimeSocketInternalModules
            <> oracleRuntimeExternalModules
            <> receiptRetirementModule
            <> ["Eclips.Public.Types.Timing"]
      }
  HeraldRuntime ->
    (architectureSpec HeraldRuntime "herald-runtime" "eclips-herald-runtime.cabal" "eclips-herald-runtime" ["src", "internal", "tcp", "tcp-internal"] ["Eclips.Domain", "Eclips.Herald.Runtime", "Eclips.Protocol.Admin", "Eclips.Protocol.Application", "Eclips.Protocol.Peer"] RuntimeBoundary "typed Herald runtime")
      { specAllowedEclipsModules =
          heraldFacadeModules <> protocolAdminModules <> heraldRuntimeOracleExternalModules <> receiptRetirementModule <> ["Eclips.Application.Types.Lifecycle", "Eclips.Protocol.Frame", "Eclips.Public.Types.Timing"]
      }
  Deployment ->
    (architectureSpec Deployment "deployment" "eclips-deployment.cabal" "eclips-deployment" ["src", "internal", "app/herald", "app/admin"] ["Eclips.Deployment", "Eclips.Application.Types", "Eclips.Domain"] RuntimeBoundary "deployment runtime")
      { specAllowedEclipsModules = deploymentFacadeModules <> receiptRetirementModule <> ["Eclips.Public.Types.Timing"]
      }
  HelloWorldExample ->
    (auxiliaryExampleSpec HelloWorldExample "examples/hello-world" "eclips-hello-world.cabal" "eclips-hello-world" [".", "applications", "launcher", "publisher", "reader"] ["Eclips.Application.Types", "Eclips.Domain"] "hello-world example")
      { specAllowedEclipsModules = helloWorldFacadeModules <> ["Eclips.Public.Types.Timing"]
      }
  HelloContextExample ->
    (auxiliaryExampleSpec HelloContextExample "examples/hello-context" "eclips-hello-context.cabal" "eclips-hello-context" ["applications", "app"] ["Eclips.Application.Types"] "hello-context example")
      { specAllowedEclipsModules = ["Eclips.Application", "Eclips.Application.Advanced", "Eclips.Application.Connection", "Eclips.Application.Typed", "Eclips.Application.Typed.Advanced"]
      }

architectureSpec ::
  PackageKey ->
  FilePath ->
  FilePath ->
  String ->
  [FilePath] ->
  [String] ->
  RuntimeImportPolicy ->
  String ->
  PackageSpec
architectureSpec key relativeRoot cabalFile expectedName sourceRoots allowedRoots runtimeImportPolicy diagnosticLabel =
  PackageSpec
    { specKey = key,
      specClass = ArchitecturePackage,
      specRelativeRoot = relativeRoot,
      specCabalFile = cabalFile,
      specExpectedPackageName = expectedName,
      specExpectedComponents = expectedComponents key,
      specProductionSourceRoots = sourceRoots,
      specAllowedEclipsRoots = allowedRoots,
      specAllowedEclipsModules = [],
      specRuntimeImportPolicy = runtimeImportPolicy,
      specDiagnosticLabel = diagnosticLabel
    }

auxiliaryExampleSpec ::
  PackageKey ->
  FilePath ->
  FilePath ->
  String ->
  [FilePath] ->
  [String] ->
  String ->
  PackageSpec
auxiliaryExampleSpec key relativeRoot cabalFile expectedName sourceRoots allowedRoots diagnosticLabel =
  (architectureSpec key relativeRoot cabalFile expectedName sourceRoots allowedRoots RuntimeBoundary diagnosticLabel)
    { specClass = AuxiliaryExample
    }

projectPackagePath :: PackageSpec -> FilePath
projectPackagePath PackageSpec {specRelativeRoot, specCabalFile} =
  "./" <> specRelativeRoot <> "/" <> specCabalFile

expectedProjectFields :: [(String, [String])]
expectedProjectFields =
  [ ( "packages",
      fmap projectPackagePath packageSpecs
    ),
    ("tests", ["True"])
  ]

checkProjectFile :: FilePath -> IO [Violation]
checkProjectFile path = do
  contentsResult <- readBytes path
  pure $ case contentsResult of
    Left message -> [Violation message]
    Right contents -> case Fields.readFields contents of
      Left parseError -> [Violation (path <> ": " <> show parseError)]
      Right fields ->
        let actual = traverse projectField fields
         in case actual of
              Left message -> [Violation (path <> ": " <> message)]
              Right parsedFields
                | parsedFields == expectedProjectFields -> []
                | otherwise ->
                    [ Violation
                        ( path
                            <> ": the default project must contain exactly the reviewed architecture and auxiliary package plan; expected "
                            <> show expectedProjectFields
                            <> ", found "
                            <> show parsedFields
                        )
                    ]

projectField :: Fields.Field annotation -> Either String (String, [String])
projectField = \case
  Fields.Field (Fields.Name _ rawName) lines' ->
    Right
      ( fmap toLower (BS8.unpack rawName),
        concatMap (words . fieldLineText) lines'
      )
  Fields.Section (Fields.Name _ rawName) _ _ ->
    Left ("project sections are not part of the reviewed plan: " <> BS8.unpack rawName)

fieldLineText :: Fields.FieldLine annotation -> String
fieldLineText (Fields.FieldLine _ contents) = BS8.unpack contents

loadPackage :: FilePath -> PackageSpec -> IO (Either Violation PackageView)
loadPackage projectRoot spec@PackageSpec {specRelativeRoot, specCabalFile} = do
  let packageRoot = projectRoot </> specRelativeRoot
      cabalPath = packageRoot </> specCabalFile
  exists <- doesFileExist cabalPath
  if not exists
    then pure (Left (Violation ("missing package description: " <> cabalPath)))
    else do
      contentsResult <- readBytes cabalPath
      pure $ do
        contents <- either (Left . Violation) Right contentsResult
        genericDescription <- parsePackage cabalPath contents
        let (componentPairs, conditionals) = packageComponents genericDescription
        pure
          PackageView
            { packageSpec = spec,
              packageRoot,
              declaredPackageName = prettyShow (packageName genericDescription),
              components = Map.fromList componentPairs,
              conditionalComponents = conditionals
            }

parsePackage :: FilePath -> BS.ByteString -> Either Violation GenericPackageDescription
parsePackage path contents =
  let (warnings, parseResult) = Cabal.runParseResult (Cabal.parseGenericPackageDescription contents)
   in case parseResult of
        Right description
          | null warnings -> Right description
          | otherwise ->
              Left
                ( Violation
                    (intercalate "\n" (fmap (Fields.showPWarning path) warnings))
                )
        Left (_, errors) ->
          Left
            ( Violation
                (intercalate "\n" (fmap (Fields.showPError path) (NonEmpty.toList errors)))
            )

packageComponents :: GenericPackageDescription -> ([(String, ComponentView)], [String])
packageComponents description =
  let mainLibrary = case condLibrary description of
        Nothing -> []
        Just tree -> [("library", libraryView tree)]
      subLibraries =
        [ ("library:" <> prettyShow name, libraryView tree)
        | (name, tree) <- condSubLibraries description
        ]
      foreignLibraries =
        [ ("foreign-library:" <> prettyShow name, buildInfoView (foreignLibBuildInfo (condTreeData tree)))
        | (name, tree) <- condForeignLibs description
        ]
      executables =
        [ ("executable:" <> prettyShow name, buildInfoView (buildInfo (condTreeData tree)))
        | (name, tree) <- condExecutables description
        ]
      tests =
        [ ("test-suite:" <> prettyShow name, buildInfoView (testBuildInfo (condTreeData tree)))
        | (name, tree) <- condTestSuites description
        ]
      benchmarks =
        [ ("benchmark:" <> prettyShow name, buildInfoView (benchmarkBuildInfo (condTreeData tree)))
        | (name, tree) <- condBenchmarks description
        ]
      conditionals =
        maybe [] (conditional "library") (condLibrary description)
          <> conditionalComponentsNamed "library:" (condSubLibraries description)
          <> conditionalComponentsNamed "foreign-library:" (condForeignLibs description)
          <> conditionalComponentsNamed "executable:" (condExecutables description)
          <> conditionalComponentsNamed "test-suite:" (condTestSuites description)
          <> conditionalComponentsNamed "benchmark:" (condBenchmarks description)
   in (mainLibrary <> subLibraries <> foreignLibraries <> executables <> tests <> benchmarks, conditionals)

conditionalComponentsNamed :: (Pretty name) => String -> [(name, CondTree variable constraints component)] -> [String]
conditionalComponentsNamed prefix =
  concatMap (\(name, tree) -> conditional (prefix <> prettyShow name) tree)

conditional :: String -> CondTree variable constraints component -> [String]
conditional name tree = [name | not (null (condTreeComponents tree))]

libraryView :: CondTree variable constraints Library -> ComponentView
libraryView tree =
  let library = condTreeData tree
      baseView = buildInfoView (libBuildInfo library)
   in baseView
        { visibility = Just (renderVisibility (libVisibility library)),
          exposedModuleNames = fmap prettyShow (exposedModules library),
          reexportedModuleNames = fmap prettyShow (reexportedModules library)
        }

buildInfoView :: BuildInfo -> ComponentView
buildInfoView info =
  ComponentView
    { sourceDirectories = fmap getSymbolicPath (hsSourceDirs info),
      dependencies = Set.fromList (concatMap renderDependency (targetBuildDepends info)),
      executableDependencies = Set.fromList (fmap renderExecutableDependency (buildToolDepends info)),
      languageEdition = prettyShow <$> defaultLanguage info,
      visibility = Nothing,
      exposedModuleNames = [],
      otherModuleNames = fmap prettyShow (otherModules info),
      reexportedModuleNames = []
    }

renderVisibility :: LibraryVisibility -> String
renderVisibility LibraryVisibilityPublic = "public"
renderVisibility LibraryVisibilityPrivate = "private"

renderDependency :: Dependency -> [String]
renderDependency (Dependency dependencyPackageName _ libraryNames) =
  let package = prettyShow dependencyPackageName
   in fmap
        ( \case
            LMainLibName -> package
            LSubLibName libraryName -> package <> ":" <> prettyShow libraryName
        )
        (NonEmptySet.toList libraryNames)

renderExecutableDependency :: ExeDependency -> String
renderExecutableDependency (ExeDependency dependencyPackageName executableName _) =
  prettyShow dependencyPackageName <> ":" <> prettyShow executableName

checkPackage :: PackageView -> [Violation]
checkPackage PackageView {packageSpec, packageRoot, declaredPackageName, components, conditionalComponents} =
  let PackageSpec
        { specClass,
          specExpectedPackageName,
          specExpectedComponents,
          specDiagnosticLabel
        } = packageSpec
      expectations = specExpectedComponents
      packageNameViolations =
        [ Violation
            ( packageRoot
                <> ": "
                <> specDiagnosticLabel
                <> " "
                <> packageClassLabel specClass
                <> " name differs from the reviewed plan; expected "
                <> specExpectedPackageName
                <> ", found "
                <> declaredPackageName
            )
        | declaredPackageName /= specExpectedPackageName
        ]
      actualNames = Map.keysSet components
      expectedNames = Map.keysSet expectations
      componentSetViolations =
        [ Violation
            ( packageRoot
                <> ": components differ from the reviewed package wall; expected "
                <> show (Set.toAscList expectedNames)
                <> ", found "
                <> show (Set.toAscList actualNames)
            )
        | actualNames /= expectedNames
        ]
      conditionalViolations =
        [ Violation (packageRoot <> ": conditional component configuration is not reviewed: " <> name)
        | name <- conditionalComponents
        ]
      detailViolations =
        concat
          [ checkComponent packageRoot name expectation actual
          | (name, expectation) <- Map.toList expectations,
            Just actual <- [Map.lookup name components]
          ]
   in packageNameViolations <> componentSetViolations <> conditionalViolations <> detailViolations

packageClassLabel :: PackageClass -> String
packageClassLabel ArchitecturePackage = "architecture package"
packageClassLabel AuxiliaryExample = "auxiliary example package"

checkComponent :: FilePath -> String -> ComponentExpectation -> ComponentView -> [Violation]
checkComponent packageRoot name expectation actual =
  concat
    [ mismatch "source directories" expectedSources sourceDirectories,
      mismatch "dependencies" expectedDependencies dependencies,
      mismatch "executable dependencies" expectedExecutableDependencies executableDependencies,
      mismatch "default language" (Just expectedLanguageEdition) languageEdition,
      optionalMismatch "visibility" expectedVisibility visibility,
      optionalMismatch "exposed modules" expectedExposedModules (Just exposedModuleNames),
      optionalMismatch "other modules" expectedOtherModules (Just otherModuleNames),
      optionalMismatch "re-exported modules" expectedReexportedModules (Just reexportedModuleNames)
    ]
  where
    ComponentExpectation
      { expectedSources,
        expectedDependencies,
        expectedExecutableDependencies,
        expectedLanguageEdition,
        expectedVisibility,
        expectedExposedModules,
        expectedOtherModules,
        expectedReexportedModules
      } = expectation
    ComponentView
      { sourceDirectories,
        dependencies,
        executableDependencies,
        languageEdition,
        visibility,
        exposedModuleNames,
        otherModuleNames,
        reexportedModuleNames
      } = actual
    mismatch :: (Eq value, Show value) => String -> value -> value -> [Violation]
    mismatch label expected found =
      [ Violation
          ( packageRoot
              <> ": "
              <> name
              <> " "
              <> label
              <> " differ from the reviewed wall; expected "
              <> show expected
              <> ", found "
              <> show found
          )
      | expected /= found
      ]
    optionalMismatch :: (Eq value, Show value) => String -> Maybe value -> Maybe value -> [Violation]
    optionalMismatch _ Nothing _ = []
    optionalMismatch label (Just expected) found = mismatch label (Just expected) found

expectedComponents :: PackageKey -> Map String ComponentExpectation
expectedComponents = \case
  PublicTypes ->
    Map.fromList
      [ ( "library",
          (component ["src"] ["base", "binary", "bytestring", "cereal", "containers", "cryptohash-sha256"])
            { expectedExposedModules = Just ["Eclips.Public.Types.Diagnostic", "Eclips.Public.Types.ReceiptRetirement", "Eclips.Public.Types.SortCanonical", "Eclips.Public.Types.SortCatalogue", "Eclips.Public.Types.SortId", "Eclips.Public.Types.Timing"],
              expectedOtherModules = Just [],
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:public-types-properties",
          ( component
              ["test"]
              [ "base",
                "binary",
                "bytestring",
                "containers",
                "eclips-public-types",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules = Just ["ReceiptRetirementProperties", "SortIdProperties", "TimingProperties"]
            }
        )
      ]
  ApplicationTypes ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "containers",
                "eclips-public-types",
                "text"
              ]
          )
            { expectedExposedModules = Just applicationTypeModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:application-types-properties",
          component
            ["test"]
            [ "base",
              "binary",
              "bytestring",
              "containers",
              "eclips-application-types",
              "eclips-public-types",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck",
              "text"
            ]
        )
      ]
  ProtocolFrame ->
    Map.fromList
      [ ( "library",
          (component ["src"] ["base", "binary", "bytestring"])
            { expectedExposedModules = Just protocolFrameModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-frame-properties",
          component
            ["test"]
            [ "base",
              "binary",
              "bytestring",
              "eclips-protocol-frame",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        )
      ]
  ProtocolApplication ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-application-types",
                "eclips-protocol-frame",
                "eclips-public-types",
                "text"
              ]
          )
            { expectedExposedModules = Just protocolApplicationModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-application-properties",
          component
            ["test"]
            [ "base",
              "binary",
              "bytestring",
              "containers",
              "eclips-application-types",
              "eclips-protocol-application",
              "eclips-public-types",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        )
      ]
  ProtocolAdmin ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-application-types",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-protocol-frame",
                "eclips-public-types"
              ]
          )
            { expectedExposedModules = Just protocolAdminModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-admin-properties",
          ( component
              ["test"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-application-types",
                "cereal",
                "eclips-oracle-core",
                "eclips-protocol-admin",
                "eclips-public-types",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "CodecProperties",
                    "FrameProperties",
                    "TestFixtures",
                    "TypesProperties"
                  ]
            }
        )
      ]
  ProtocolPeer ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-application-types",
                "containers",
                "eclips-protocol-frame",
                "eclips-public-types",
                "text"
              ]
          )
            { expectedExposedModules = Just protocolPeerModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-peer-properties",
          component
            ["test"]
            [ "base",
              "binary",
              "bytestring",
              "containers",
              "eclips-application-types",
              "eclips-protocol-peer",
              "eclips-public-types",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        )
      ]
  RaftCore ->
    Map.fromList
      [ ( "library",
          (component ["src"] ["base", "bytestring", "containers", "eclips-public-types"])
            { expectedVisibility = Just "public",
              expectedExposedModules = Just raftCoreModules,
              expectedOtherModules =
                Just
                  [ "Eclips.Raft.Internal.Configuration",
                    "Eclips.Raft.Internal.Log",
                    "Eclips.Raft.Internal.Prepared",
                    "Eclips.Raft.Internal.State"
                  ],
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:raft-core-properties",
          ( component
              ["test"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-raft-core",
                "process",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "GenesisProperties",
                    "ConfigurationProperties",
                    "CheckpointProperties",
                    "LogWorkProperties",
                    "RaftModelProperties",
                    "ReconfigurationModelProperties",
                    "SmallClusterProperties",
                    "TransitionProperties"
                  ]
            }
        )
      ]
  OracleCore ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "bytestring",
                "cereal",
                "containers",
                "cryptohash-sha256",
                "eclips-domain",
                "eclips-public-types",
                "eclips-raft-core",
                "text"
              ]
          )
            { expectedVisibility = Just "public",
              expectedExposedModules = Just oracleCoreModules,
              expectedOtherModules = Just oracleCoreInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:oracle-core-properties",
          ( component
              ["test"]
              [ "base",
                "bytestring",
                "cereal",
                "containers",
                "cryptohash-sha256",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-public-types",
                "eclips-raft-core",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "AdmissionProperties",
                    "CanonicalProperties",
                    "DynamicProcessProperties",
                    "GenesisProperties",
                    "LabelProperties",
                    "LabelRetirementProperties",
                    "OracleLabelProperties",
                    "OracleFixtures",
                    "OracleModelProperties",
                    "ReceiptRetirementProperties",
                    "Step15ReferenceProperties",
                    "Step15WorkflowProperties",
                    "Step16DisappearanceProperties",
                    "Eclips.Oracle.Step16.DisappearanceReference",
                    "TransitionProperties",
                    "VoterProperties",
                    "VoterFailureFixtures"
                  ]
            }
        )
      ]
  ProtocolRaft ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-protocol-frame",
                "eclips-public-types",
                "eclips-raft-core"
              ]
          )
            { expectedVisibility = Just "public",
              expectedExposedModules = Just protocolRaftModules,
              expectedOtherModules = Just [],
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-raft-properties",
          ( component
              ["test"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-protocol-frame",
                "eclips-protocol-raft",
                "eclips-raft-core",
                "containers",
                "network",
                "stm",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "CodecProperties",
                    "FrameProperties",
                    "NativeTcpProperties",
                    "TestFixtures",
                    "TypesProperties"
                  ]
            }
        )
      ]
  ProtocolOracle ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-protocol-frame",
                "eclips-public-types",
                "eclips-raft-core"
              ]
          )
            { expectedVisibility = Just "public",
              expectedExposedModules = Just protocolOracleModules,
              expectedOtherModules = Just [],
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:protocol-oracle-properties",
          ( component
              ["test"]
              [ "base",
                "binary",
                "bytestring",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-protocol-application",
                "eclips-protocol-frame",
                "eclips-protocol-oracle",
                "eclips-public-types",
                "eclips-protocol-peer",
                "eclips-protocol-raft",
                "eclips-raft-core",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "AdmissionProperties",
                    "CodecProperties",
                    "DisappearanceProperties",
                    "OracleFixtures",
                    "FrameProperties",
                    "TestFixtures",
                    "TypesProperties",
                    "VoterProperties"
                  ]
            }
        )
      ]
  Domain ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "bytestring",
                "cereal",
                "containers",
                "cryptohash-sha256",
                "eclips-public-types",
                "text"
              ]
          )
            { expectedExposedModules = Just domainModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:domain-properties",
          ( component
              ["test"]
              [ "base",
                "bytestring",
                "cereal",
                "containers",
                "cryptohash-sha256",
                "eclips-domain",
                "eclips-public-types",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck",
                "text"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "AlignmentProperties",
                    "CanonicalGoldenProperties",
                    "ContextProperties",
                    "DescriptorProperties",
                    "DisappearanceProperties",
                    "EnvironmentProperties",
                    "FoundationProperties",
                    "LabelProperties",
                    "MembershipProperties",
                    "ProcessLifecycleProperties",
                    "QueryProperties",
                    "SortDefinitionValueProperties",
                    "SortOccurrenceProperties",
                    "StartupProperties",
                    "StoreProperties",
                    "StructuralProperties",
                    "TopologyProperties",
                    "ValueProperties"
                  ]
            }
        )
      ]
  HeraldCore ->
    Map.fromList
      [ ( "library:kernel-internal",
          ( component
              ["src", "internal"]
              [ "base",
                "binary",
                "bytestring",
                "cereal",
                "containers",
                "crypton",
                "cryptohash-sha256",
                "eclips-application-types",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-protocol-admin",
                "eclips-protocol-application",
                "eclips-protocol-peer",
                "eclips-public-types",
                "eclips-raft-core",
                "text"
              ]
          )
            { expectedVisibility = Just "private",
              expectedExposedModules = Just heraldInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "library",
          (component [] ["eclips-herald-core:kernel-internal"])
            { expectedExposedModules = Just [],
              expectedReexportedModules = Just heraldFacadeModules
            }
        ),
        ( "test-suite:herald-core-properties",
          component
            ["test"]
            [ "base",
              "bytestring",
              "cereal",
              "containers",
              "eclips-application-client",
              "eclips-application-types",
              "eclips-domain",
              "eclips-herald-core:kernel-internal",
              "eclips-public-types",
              "eclips-oracle-core",
              "eclips-protocol-admin",
              "eclips-protocol-application",
              "eclips-protocol-peer",
              "eclips-raft-core",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck",
              "text"
            ]
        ),
        ( "test-suite:herald-step16-prospective-properties",
          ( component
              ["test-step16-prospective"]
              [ "base",
                "bytestring",
                "containers",
                "cryptohash-sha256",
                "eclips-domain",
                "eclips-herald-core:kernel-internal",
                "eclips-oracle-core",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "DisappearanceReferenceDriver",
                    "Eclips.Oracle.Step16.DisappearanceReference",
                    "Step16ProspectiveFixtures",
                    "Step16ProspectiveHarness",
                    "Step16ProspectiveProjection",
                    "Step16DisappearanceProperties"
                  ]
            }
        )
      ]
  ApplicationClient ->
    Map.fromList
      [ ( "library",
          ( component
              ["src"]
              [ "base",
                "containers",
                "eclips-application-types",
                "eclips-protocol-application",
                "eclips-public-types"
              ]
          )
            { expectedExposedModules = Just applicationClientModules,
              expectedOtherModules = Just applicationClientInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:application-client-properties",
          component
            ["test"]
            [ "base",
              "bytestring",
              "containers",
              "eclips-application-client",
              "eclips-application-types",
              "eclips-protocol-application",
              "eclips-public-types",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        )
      ]
  ApplicationApi ->
    Map.fromList
      [ ( "library:physical-internal",
          (component ["internal"] ["base"])
            { expectedVisibility = Just "private",
              expectedExposedModules = Just applicationPhysicalInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "library",
          ( component
              ["src"]
              [ "base",
                "binary",
                "bytestring",
                "containers",
                "eclips-application-api:physical-internal",
                "eclips-application-client",
                "eclips-application-types",
                "eclips-protocol-application",
                "eclips-public-types",
                "entropy",
                "network",
                "stm",
                "text"
              ]
          )
            { expectedExposedModules = Just applicationApiModules,
              expectedOtherModules = Just applicationApiInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:application-api-properties",
          component
            ["test"]
            [ "base",
              "bytestring",
              "containers",
              "eclips-application-api",
              "eclips-application-api:physical-internal",
              "eclips-application-types",
              "eclips-protocol-application",
              "eclips-public-types",
              "network",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck",
              "text"
            ]
        )
      ]
  OracleRuntime ->
    Map.fromList
      [ ( "library:socket-internal",
          ( component
              ["socket-internal"]
              [ "base",
                "bytestring",
                "network",
                "stm"
              ]
          )
            { expectedVisibility = Just "private",
              expectedExposedModules = Just oracleRuntimeSocketInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "library",
          ( component
              ["src", "internal"]
              [ "base",
                "binary",
                "bytestring",
                "containers",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-oracle-runtime:socket-internal",
                "eclips-protocol-oracle",
                "eclips-protocol-raft",
                "eclips-public-types",
                "eclips-raft-core",
                "entropy",
                "network",
                "stm",
                "text"
              ]
          )
            { expectedVisibility = Just "public",
              expectedExposedModules = Just oracleRuntimeModules,
              expectedOtherModules = Just oracleRuntimeInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:oracle-runtime-properties",
          ( component
              ["test", "internal"]
              [ "base",
                "binary",
                "bytestring",
                "containers",
                "eclips-domain",
                "eclips-oracle-core",
                "eclips-oracle-runtime",
                "eclips-oracle-runtime:socket-internal",
                "eclips-protocol-oracle",
                "eclips-protocol-raft",
                "eclips-public-types",
                "eclips-raft-core",
                "network",
                "stm",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "AdapterRetentionProperties",
                    "CheckpointCacheProperties",
                    "ClusterProperties",
                    "ConformanceClientProperties",
                    "ConnectedWaitProperties",
                    "CoordinationProperties",
                    "HealthRuntimeProperties",
                    "Eclips.Oracle.Runtime.Internal.Adapter",
                    "Eclips.Oracle.Runtime.Internal.Checkpoint",
                    "Eclips.Oracle.Runtime.Internal.CheckpointCache",
                    "Eclips.Oracle.Runtime.Internal.Coordination",
                    "Eclips.Oracle.Runtime.Internal.Scope",
                    "Eclips.Oracle.Runtime.Internal.Types",
                    "Eclips.Oracle.Runtime.Internal.ConnectedWait",
                    "Eclips.Oracle.Runtime.Internal.Hello",
                    "Eclips.Oracle.Runtime.Internal.LeaseQueue",
                    "Eclips.Oracle.Runtime.Internal.RetainedRequest",
                    "Eclips.Oracle.Runtime.Internal.ReplicaRegistration",
                    "Eclips.Oracle.Runtime.Internal.StatusProjection",
                    "Eclips.Oracle.Runtime.Internal.Submission",
                    "Eclips.Oracle.Runtime.Internal.TCP.ManagedWorkers",
                    "Eclips.Oracle.Runtime.Internal.TCP.Retirement",
                    "Eclips.Oracle.Runtime.Internal.TCP.Retry",
                    "Eclips.Oracle.Runtime.Internal.WatchAvailability",
                    "Eclips.Oracle.Runtime.Internal.WatchServe",
                    "Eclips.Oracle.Runtime.Internal.WatchWorker",
                    "Eclips.Oracle.Runtime.Internal.WorkCounts",
                    "LeaseQueueProperties",
                    "RaftAdmissionProperties",
                    "RaftRetryProperties",
                    "RecordingProperties",
                    "RetainedRequestProperties",
                    "RuntimeProperties",
                    "SocketProgressProperties",
                    "SocketRetirementProperties",
                    "TestFixtures",
                    "WatchServeProperties",
                    "WatchWorkerProperties",
                    "WorkCountsProperties",
                    "VoterRuntimeProperties",
                    "FailureRuntimeProperties"
                  ]
            }
        )
      ]
  HeraldRuntime ->
    Map.fromList
      [ ( "library:runtime-internal",
          ( component
              ["src", "internal"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-application-types",
                "eclips-domain",
                "eclips-public-types",
                "eclips-herald-core",
                "eclips-oracle-core",
                "eclips-protocol-admin",
                "eclips-protocol-application",
                "eclips-protocol-oracle",
                "eclips-protocol-peer",
                "eclips-raft-core",
                "entropy",
                "stm"
              ]
          )
            { expectedVisibility = Just "private",
              expectedExposedModules = Just heraldRuntimeInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "library",
          (component [] ["eclips-herald-runtime:runtime-internal"])
            { expectedExposedModules = Just [],
              expectedReexportedModules = Just heraldRuntimeFacadeModules
            }
        ),
        ( "library:tcp-internal",
          ( component
              ["tcp-internal"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-application-types",
                "eclips-domain",
                "eclips-public-types",
                "eclips-herald-core",
                "eclips-herald-runtime",
                "eclips-oracle-core",
                "eclips-protocol-admin",
                "eclips-protocol-application",
                "eclips-protocol-frame",
                "eclips-protocol-oracle",
                "eclips-protocol-peer",
                "network",
                "stm",
                "text"
              ]
          )
            { expectedVisibility = Just "private",
              expectedExposedModules = Just heraldTcpInternalModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "library:tcp",
          ( component
              ["tcp"]
              [ "base",
                "bytestring",
                "eclips-herald-core",
                "eclips-herald-runtime",
                "eclips-herald-runtime:tcp-internal",
                "eclips-public-types",
                "text"
              ]
          )
            { expectedVisibility = Just "public",
              expectedExposedModules = Just heraldTcpFacadeModules,
              expectedReexportedModules = Just []
            }
        ),
        ( "test-suite:herald-runtime-properties",
          component
            ["test", "test-support"]
            [ "base",
              "bytestring",
              "containers",
              "eclips-application-client",
              "eclips-application-types",
              "eclips-domain",
              "eclips-public-types",
              "eclips-herald-core",
              "eclips-herald-runtime:runtime-internal",
              "eclips-protocol-admin",
              "eclips-protocol-application",
              "eclips-protocol-peer",
              "stm",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        ),
        ( "test-suite:herald-slice1-tcp-properties",
          component
            ["test-tcp", "test", "test-support"]
            [ "base",
              "bytestring",
              "containers",
              "cryptohash-sha256",
              "eclips-application-api",
              "eclips-application-types",
              "eclips-domain",
              "eclips-herald-core",
              "eclips-herald-runtime:runtime-internal",
              "eclips-herald-runtime:tcp",
              "eclips-herald-runtime:tcp-internal",
              "eclips-protocol-admin",
              "eclips-protocol-application",
              "eclips-protocol-peer",
              "eclips-public-types",
              "network",
              "stm",
              "tasty",
              "tasty-hunit",
              "tasty-quickcheck"
            ]
        ),
        ( "test-suite:herald-step12-control-properties",
          ( component
              ["test-step12", "test", "test-support"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-domain",
                "eclips-public-types",
                "eclips-herald-core",
                "eclips-herald-runtime:runtime-internal",
                "eclips-herald-runtime:tcp",
                "eclips-herald-runtime:tcp-internal",
                "eclips-oracle-core",
                "eclips-oracle-runtime",
                "eclips-protocol-admin",
                "eclips-protocol-application",
                "eclips-raft-core",
                "network",
                "stm",
                "tasty",
                "tasty-hunit",
                "tasty-quickcheck"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "PeerEvidence",
                    "OracleSubmissionTrackerProperties",
                    "OracleHealthTcpProperties",
                    "OracleReplicaWaitProperties",
                    "RuntimeFixtures",
                    "Step12ControlProperties",
                    "Step12Fixtures"
                  ]
            }
        ),
        ( "test-suite:herald-step14-label-properties",
          ( component
              ["test-step14", "test-step12", "test-support"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-application-api",
                "eclips-application-types",
                "eclips-domain",
                "eclips-herald-core",
                "eclips-herald-runtime:runtime-internal",
                "eclips-herald-runtime:tcp",
                "eclips-herald-runtime:tcp-internal",
                "eclips-oracle-core",
                "eclips-oracle-runtime",
                "eclips-protocol-admin",
                "eclips-protocol-application",
                "eclips-protocol-peer",
                "eclips-public-types",
                "eclips-raft-core",
                "network",
                "stm",
                "tasty",
                "tasty-hunit",
                "text"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "PeerEvidence",
                    "ApplicationTerminalTcpProperties",
                    "Step12Fixtures",
                    "Step14AdministrationClient",
                    "Step14AdministrationProperties",
                    "Step14AlignmentLossProperties",
                    "Step14ApplicationReplyLossProperties",
                    "Step14ApplicationWorkflow",
                    "Step14DeferredEndProperties",
                    "Step14Fixtures",
                    "Step14LabelProperties",
                    "Step14LifecycleProperties",
                    "Step14PreparedReleaseProperties",
                    "Step14PreparationReadinessProperties",
                    "Step14TargetEndProperties"
                  ]
            }
        ),
        ( "test-suite:herald-step15-crash-properties",
          ( component
              ["test-step15", "test-step12", "test-support"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-application-api",
                "eclips-application-types",
                "eclips-domain",
                "eclips-herald-core",
                "eclips-herald-runtime:runtime-internal",
                "eclips-herald-runtime:tcp",
                "eclips-herald-runtime:tcp-internal",
                "eclips-oracle-core",
                "eclips-oracle-runtime",
                "eclips-protocol-application",
                "eclips-protocol-peer",
                "eclips-raft-core",
                "network",
                "stm",
                "tasty",
                "tasty-hunit"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "PeerEvidence",
                    "Step12Fixtures",
                    "Step15Applications",
                    "Step15ApplicationLossProperties",
                    "Step15Configuration",
                    "Step15CrashProperties",
                    "Step15Fixtures",
                    "Step15FoundationProperties",
                    "Step15Genesis",
                    "Step15TerminalRepairProperties",
                    "RepeatedRetirementProperties",
                    "Step15WorkCounts"
                  ]
            }
        ),
        ( "test-suite:herald-step16-failure-properties",
          ( component
              ["test-step16", "test-step15", "test-step14", "test-step12", "test-support"]
              [ "base",
                "bytestring",
                "containers",
                "eclips-application-api",
                "eclips-application-types",
                "eclips-domain",
                "eclips-herald-core",
                "eclips-herald-runtime:runtime-internal",
                "eclips-herald-runtime:tcp",
                "eclips-herald-runtime:tcp-internal",
                "eclips-oracle-core",
                "eclips-oracle-runtime",
                "eclips-protocol-application",
                "eclips-protocol-peer",
                "eclips-raft-core",
                "network",
                "stm",
                "tasty",
                "tasty-hunit",
                "text"
              ]
          )
            { expectedOtherModules =
                Just
                  [ "PeerEvidence",
                    "Step12Fixtures",
                    "Step14AlignmentLossProperties",
                    "Step14Fixtures",
                    "Step15Applications",
                    "Step15Configuration",
                    "Step15CrashProperties",
                    "Step15Fixtures",
                    "Step15Genesis",
                    "Step15WorkCounts",
                    "Step16MarkerRepairProperties",
                    "Step16PeerWave",
                    "Step16TourFixtures",
                    "Step16TourProperties"
                  ]
            }
        ),
        ( "executable:eclips-herald-slice1-node",
          component
            ["node", "test-tcp"]
            [ "base",
              "bytestring",
              "eclips-domain",
              "eclips-herald-core",
              "eclips-herald-runtime",
              "eclips-herald-runtime:tcp",
              "eclips-herald-runtime:tcp-internal",
              "stm",
              "text"
            ]
        ),
        ( "test-suite:herald-slice1-process-properties",
          ( component
              ["test-process"]
              [ "base",
                "directory",
                "process",
                "tasty",
                "tasty-hunit"
              ]
          )
            { expectedExecutableDependencies =
                Set.singleton "eclips-herald-runtime:eclips-herald-slice1-node"
            }
        )
      ]
  Deployment ->
    Map.fromList
      [ ("library", (component ["src", "internal"] ["base", "binary", "bytestring", "containers", "cryptohash-sha256", "eclips-application-api", "eclips-application-types", "eclips-domain", "eclips-public-types", "eclips-herald-core", "eclips-herald-runtime", "eclips-herald-runtime:tcp", "eclips-oracle-core", "eclips-oracle-runtime", "eclips-protocol-admin", "eclips-raft-core", "network", "stm", "text"]) {expectedVisibility = Just "public", expectedExposedModules = Just ["Eclips.Deployment.Timing", "Eclips.Deployment.Configuration", "Eclips.Deployment.Discovery", "Eclips.Deployment.Joining", "Eclips.Deployment.Manifest", "Eclips.Deployment.Runtime", "Eclips.Deployment.Admin"], expectedOtherModules = Just ["Eclips.Deployment.Admin.Exchange", "Eclips.Deployment.Joining.HistoryTransfer"], expectedReexportedModules = Just []}),
        ("executable:eclips-herald", component ["app/herald"] ["base", "bytestring", "eclips-deployment", "entropy", "text"]),
        ("executable:eclips-admin", component ["app/admin"] ["base", "eclips-deployment", "text"]),
        ("test-suite:deployment-properties", (component ["test", "internal"] ["base", "bytestring", "containers", "eclips-application-types", "eclips-deployment", "eclips-domain", "eclips-public-types", "eclips-herald-core", "eclips-oracle-core", "eclips-protocol-admin", "eclips-raft-core", "tasty", "tasty-hunit", "tasty-quickcheck"]) {expectedOtherModules = Just ["TimingProperties", "AdminProperties", "DiscoveryProperties", "JoiningProperties", "Eclips.Deployment.Admin.Exchange", "Eclips.Deployment.Joining.HistoryTransfer"]}),
        ( "test-suite:deployment-os-integration",
          (component ["test-os"] ["base", "bytestring", "directory", "eclips-application-api", "eclips-application-types", "eclips-deployment", "eclips-domain", "eclips-public-types", "eclips-herald-core", "eclips-herald-runtime", "eclips-oracle-core", "eclips-oracle-runtime", "eclips-protocol-application", "eclips-protocol-oracle", "eclips-protocol-frame", "eclips-protocol-peer", "eclips-protocol-raft", "eclips-raft-core", "filepath", "network", "process", "stm", "tasty", "tasty-hunit", "text"])
            { expectedOtherModules = Just ["Proxy", "ParentExit", "VoterApplication", "VoterCheckpoint", "NativeHealth", "Partition", "Timing"],
              expectedExecutableDependencies = Set.fromList ["eclips-deployment:eclips-herald", "eclips-deployment:eclips-admin", "eclips-hello-world:eclips-hello-launcher", "eclips-hello-world:eclips-hello-publisher", "eclips-hello-world:eclips-hello-reader"]
            }
        )
      ]
  HelloContextExample ->
    Map.fromList
      [ ("library:applications", (component ["applications"] ["base", "containers", "eclips-application-api", "eclips-application-types", "process", "text"]) {expectedVisibility = Just "private", expectedExposedModules = Just ["ContextVocabulary", "ContextApplication", "ContextLauncher", "ContextArguments"], expectedOtherModules = Just [], expectedReexportedModules = Just []}),
        ("executable:eclips-hello-context", component ["app"] ["base", "eclips-application-types", "eclips-hello-context:applications"]),
        ("test-suite:hello-context-properties", (component ["test"] ["base", "containers", "eclips-hello-context:applications", "tasty", "tasty-quickcheck"]) {expectedOtherModules = Just []}),
        ( "test-suite:hello-context-integration",
          (component ["test-os"] ["base", "directory", "filepath", "network", "process", "tasty", "tasty-hunit", "text"])
            { expectedExecutableDependencies = Set.fromList ["eclips-deployment:eclips-herald", "eclips-deployment:eclips-admin", "eclips-hello-context:eclips-hello-context"],
              expectedOtherModules = Just []
            }
        )
      ]
  HelloWorldExample ->
    Map.fromList
      [ ("library:applications", (component ["applications"] ["base", "containers", "eclips-application-api", "eclips-application-types", "process", "text"]) {expectedVisibility = Just "private", expectedExposedModules = Just ["HelloArguments", "HelloCommon", "HelloLauncher", "HelloPublisher", "HelloReader"], expectedOtherModules = Just [], expectedReexportedModules = Just []}),
        ("executable:eclips-hello-launcher", component ["launcher"] ["base", "eclips-hello-world:applications", "filepath", "text"]),
        ("executable:eclips-hello-publisher", component ["publisher"] ["base", "eclips-hello-world:applications", "text"]),
        ("executable:eclips-hello-reader", component ["reader"] ["base", "eclips-hello-world:applications", "text"]),
        helloWorldComponent "executable:eclips-hello-world",
        helloWorldComponent "test-suite:hello-world-integration"
      ]
    where
      helloWorldComponent name =
        ( name,
          ( component
              [".", "applications"]
              ( [ "base",
                  "bytestring",
                  "containers",
                  "cryptohash-sha256",
                  "eclips-application-api",
                  "eclips-application-types",
                  "eclips-domain",
                  "eclips-public-types",
                  "eclips-herald-core",
                  "eclips-herald-runtime",
                  "eclips-herald-runtime:tcp",
                  "eclips-oracle-core",
                  "eclips-oracle-runtime",
                  "eclips-raft-core",
                  "entropy",
                  "stm",
                  "text"
                ]
                  <> [dependency | name == "test-suite:hello-world-integration", dependency <- ["QuickCheck", "binary", "directory", "process"]]
              )
          )
            { expectedOtherModules =
                Just
                  ( [ "FounderConfiguration",
                      "FounderDeployment",
                      "HelloApplication",
                      "HelloCommon",
                      "HelloDeployment"
                    ]
                      <> [name' | name == "test-suite:hello-world-integration", name' <- ["FounderProperties", "PreparedChildProperties"]]
                  )
            }
        )

component :: [FilePath] -> [String] -> ComponentExpectation
component expectedSources dependencies =
  ComponentExpectation
    { expectedSources,
      expectedDependencies = Set.fromList dependencies,
      expectedExecutableDependencies = Set.empty,
      expectedLanguageEdition = "GHC2024",
      expectedVisibility = Nothing,
      expectedExposedModules = Nothing,
      expectedOtherModules = Nothing,
      expectedReexportedModules = Nothing
    }

applicationTypeModules :: [String]
applicationTypeModules =
  [ "Eclips.Application.Types.Access",
    "Eclips.Application.Types.Forward",
    "Eclips.Application.Types.Identity",
    "Eclips.Application.Types.Label",
    "Eclips.Application.Types.Lifecycle",
    "Eclips.Application.Types.Lifetime",
    "Eclips.Application.Types.NewId",
    "Eclips.Application.Types.Operation",
    "Eclips.Application.Types.Query",
    "Eclips.Application.Types.Rejection",
    "Eclips.Application.Types.Result",
    "Eclips.Application.Types.SortDescriptor",
    "Eclips.Application.Types.Typed",
    "Eclips.Application.Types.Value",
    "Eclips.Application.Types.Write"
  ]

diagnosticRenderingModule :: [String]
diagnosticRenderingModule =
  ["Eclips.Public.Types.Diagnostic"]

protocolFrameModules :: [String]
protocolFrameModules =
  ["Eclips.Protocol.Frame"]

protocolApplicationModules :: [String]
protocolApplicationModules =
  [ "Eclips.Protocol.Application.Codec",
    "Eclips.Protocol.Application.Frame",
    "Eclips.Protocol.Application.Types"
  ]

protocolAdminModules :: [String]
protocolAdminModules =
  [ "Eclips.Protocol.Admin.Codec",
    "Eclips.Protocol.Admin.Frame",
    "Eclips.Protocol.Admin.Types"
  ]

receiptRetirementModule :: [String]
receiptRetirementModule = ["Eclips.Public.Types.ReceiptRetirement"]

protocolAdminExternalModules :: [String]
protocolAdminExternalModules =
  ["Eclips.Domain.ProcessLifecycle", "Eclips.Application.Types.Lifecycle", "Eclips.Oracle.Canonical", "Eclips.Oracle.Voter"]

protocolPeerModules :: [String]
protocolPeerModules =
  [ "Eclips.Protocol.Peer.Codec",
    "Eclips.Protocol.Peer.Frame",
    "Eclips.Protocol.Peer.Types"
  ]

raftCoreModules :: [String]
raftCoreModules =
  [ "Eclips.Raft.Identity",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Checkpoint",
    "Eclips.Raft.Genesis",
    "Eclips.Raft.Input",
    "Eclips.Raft.Effect",
    "Eclips.Raft.State",
    "Eclips.Raft.Transition"
  ]

oracleCoreModules :: [String]
oracleCoreModules =
  [ "Eclips.Oracle.Admission",
    "Eclips.Oracle.Failure",
    "Eclips.Oracle.Voter",
    "Eclips.Oracle.Identity",
    "Eclips.Oracle.Genesis",
    "Eclips.Oracle.Command",
    "Eclips.Oracle.Disappearance",
    "Eclips.Oracle.Canonical",
    "Eclips.Oracle.Receipt",
    "Eclips.Oracle.Projection",
    "Eclips.Oracle.Progress",
    "Eclips.Oracle.Input",
    "Eclips.Oracle.Effect",
    "Eclips.Oracle.State",
    "Eclips.Oracle.Label",
    "Eclips.Oracle.Step15.Reference",
    "Eclips.Oracle.Step15.WorkflowReference",
    "Eclips.Oracle.Transition"
  ]

oracleCoreInternalModules :: [String]
oracleCoreInternalModules =
  [ "Eclips.Oracle.Internal.Admission",
    "Eclips.Oracle.Internal.Voter",
    "Eclips.Oracle.Internal.CanonicalState",
    "Eclips.Oracle.Internal.ConfiguredBootstrapCanonical",
    "Eclips.Oracle.Internal.Digest",
    "Eclips.Oracle.Internal.Disappearance",
    "Eclips.Oracle.Internal.DisappearanceCanonical",
    "Eclips.Oracle.Internal.Failure",
    "Eclips.Oracle.Internal.Label",
    "Eclips.Oracle.Internal.LabelCanonical"
  ]

oracleCoreExternalModules :: [String]
oracleCoreExternalModules =
  [ "Eclips.Domain.Structural",
    "Eclips.Domain.Disappearance",
    "Eclips.Domain.Environment",
    "Eclips.Domain.Graph",
    "Eclips.Domain.Identity",
    "Eclips.Domain.Label",
    "Eclips.Domain.MemberSet",
    "Eclips.Domain.Membership",
    "Eclips.Domain.ProcessLifecycle",
    "Eclips.Domain.ProcessStart",
    "Eclips.Domain.Sort.Canonical",
    "Eclips.Domain.Sort.Descriptor",
    "Eclips.Domain.Sort.Profile",
    "Eclips.Domain.SortOccurrence",
    "Eclips.Domain.Startup",
    "Eclips.Domain.Topology",
    "Eclips.Domain.Value",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Effect",
    "Eclips.Raft.Input",
    "Eclips.Raft.Genesis",
    "Eclips.Raft.Identity"
  ]

protocolRaftModules :: [String]
protocolRaftModules =
  [ "Eclips.Protocol.Raft.Types",
    "Eclips.Protocol.Raft.Codec",
    "Eclips.Protocol.Raft.Frame"
  ]

protocolRaftExternalModules :: [String]
protocolRaftExternalModules =
  [ "Eclips.Protocol.Frame",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Checkpoint",
    "Eclips.Raft.Identity",
    "Eclips.Raft.Input"
  ]

protocolOracleModules :: [String]
protocolOracleModules =
  [ "Eclips.Protocol.Oracle.Types",
    "Eclips.Protocol.Oracle.Codec",
    "Eclips.Protocol.Oracle.Frame"
  ]

protocolOracleExternalModules :: [String]
protocolOracleExternalModules =
  [ "Eclips.Domain.Identity",
    "Eclips.Domain.Membership",
    "Eclips.Domain.Startup",
    "Eclips.Oracle.Canonical",
    "Eclips.Oracle.Command",
    "Eclips.Oracle.Identity",
    "Eclips.Oracle.Progress",
    "Eclips.Oracle.Projection",
    "Eclips.Oracle.Receipt",
    "Eclips.Protocol.Frame",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Identity"
  ]

applicationClientModules :: [String]
applicationClientModules =
  [ "Eclips.Application.Client",
    "Eclips.Application.Client.Effect",
    "Eclips.Application.Client.Input",
    "Eclips.Application.Client.Recovery"
  ]

applicationClientInternalModules :: [String]
applicationClientInternalModules =
  [ "Eclips.Application.Client.Effect.Internal",
    "Eclips.Application.Client.Internal",
    "Eclips.Application.Client.Recovery.Internal"
  ]

applicationApiModules :: [String]
applicationApiModules =
  [ "Eclips.Application",
    "Eclips.Application.Advanced",
    "Eclips.Application.Connection",
    "Eclips.Application.Runtime",
    "Eclips.Application.Typed",
    "Eclips.Application.Typed.Advanced"
  ]

applicationApiInternalModules :: [String]
applicationApiInternalModules =
  [ "Eclips.Application.Internal",
    "Eclips.Application.Typed.Internal",
    "Eclips.Application.Runtime.Internal.Connection",
    "Eclips.Application.Runtime.Internal.Owner",
    "Eclips.Application.Runtime.Internal.Socket",
    "Eclips.Application.Runtime.Internal.Types"
  ]

applicationPhysicalInternalModules :: [String]
applicationPhysicalInternalModules =
  ["Eclips.Application.Runtime.Internal.Physical"]

oracleRuntimeModules :: [String]
oracleRuntimeModules =
  [ "Eclips.Oracle.Runtime",
    "Eclips.Oracle.Runtime.TCP",
    "Eclips.Oracle.Runtime.ConformanceClient"
  ]

oracleRuntimeInternalModules :: [String]
oracleRuntimeInternalModules =
  [ "Eclips.Oracle.Runtime.Internal.Adapter",
    "Eclips.Oracle.Runtime.Internal.Connection",
    "Eclips.Oracle.Runtime.Internal.Checkpoint",
    "Eclips.Oracle.Runtime.Internal.CheckpointCache",
    "Eclips.Oracle.Runtime.Internal.ConnectedWait",
    "Eclips.Oracle.Runtime.Internal.Coordination",
    "Eclips.Oracle.Runtime.Internal.Dispatcher",
    "Eclips.Oracle.Runtime.Internal.Entropy",
    "Eclips.Oracle.Runtime.Internal.Hello",
    "Eclips.Oracle.Runtime.Internal.LeaseQueue",
    "Eclips.Oracle.Runtime.Internal.OracleOwner",
    "Eclips.Oracle.Runtime.Internal.RaftOwner",
    "Eclips.Oracle.Runtime.Internal.Recording",
    "Eclips.Oracle.Runtime.Internal.ReplicaRegistration",
    "Eclips.Oracle.Runtime.Internal.RetainedRequest",
    "Eclips.Oracle.Runtime.Internal.Scope",
    "Eclips.Oracle.Runtime.Internal.Submission",
    "Eclips.Oracle.Runtime.Internal.StatusProjection",
    "Eclips.Oracle.Runtime.Internal.Timer",
    "Eclips.Oracle.Runtime.Internal.Types",
    "Eclips.Oracle.Runtime.Internal.WatchAvailability",
    "Eclips.Oracle.Runtime.Internal.WatchLedgerOwner",
    "Eclips.Oracle.Runtime.Internal.WatchServe",
    "Eclips.Oracle.Runtime.Internal.WatchWorker",
    "Eclips.Oracle.Runtime.Internal.WorkCounts",
    "Eclips.Oracle.Runtime.Internal.TCP.Oracle",
    "Eclips.Oracle.Runtime.Internal.TCP.ManagedWorkers",
    "Eclips.Oracle.Runtime.Internal.TCP.Raft",
    "Eclips.Oracle.Runtime.Internal.TCP.Retirement",
    "Eclips.Oracle.Runtime.Internal.TCP.Retry",
    "Eclips.Oracle.Runtime.Internal.TCP.Server"
  ]

oracleRuntimeSocketInternalModules :: [String]
oracleRuntimeSocketInternalModules =
  ["Eclips.Oracle.Runtime.Internal.TCP.Socket"]

oracleRuntimeExternalModules :: [String]
oracleRuntimeExternalModules =
  [ "Eclips.Oracle.Admission",
    "Eclips.Oracle.Voter",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Checkpoint",
    "Eclips.Domain.Identity",
    "Eclips.Domain.Membership",
    "Eclips.Domain.Startup",
    "Eclips.Oracle.Canonical",
    "Eclips.Oracle.Command",
    "Eclips.Oracle.Effect",
    "Eclips.Oracle.Genesis",
    "Eclips.Oracle.Identity",
    "Eclips.Oracle.Progress",
    "Eclips.Oracle.Projection",
    "Eclips.Oracle.Receipt",
    "Eclips.Oracle.State",
    "Eclips.Oracle.Transition",
    "Eclips.Protocol.Oracle.Codec",
    "Eclips.Protocol.Oracle.Frame",
    "Eclips.Protocol.Oracle.Types",
    "Eclips.Protocol.Raft.Codec",
    "Eclips.Protocol.Raft.Frame",
    "Eclips.Protocol.Raft.Types",
    "Eclips.Raft.Effect",
    "Eclips.Raft.Genesis",
    "Eclips.Raft.Identity",
    "Eclips.Raft.Input",
    "Eclips.Raft.State",
    "Eclips.Raft.Transition"
  ]

heraldOracleExternalModules :: [String]
heraldOracleExternalModules =
  [ "Eclips.Oracle.Admission",
    "Eclips.Oracle.Failure",
    "Eclips.Oracle.Voter",
    "Eclips.Raft.Configuration",
    "Eclips.Raft.Identity",
    "Eclips.Oracle.Disappearance",
    "Eclips.Oracle.Canonical",
    "Eclips.Oracle.Command",
    "Eclips.Oracle.Genesis",
    "Eclips.Oracle.Identity",
    "Eclips.Oracle.Progress",
    "Eclips.Oracle.Label",
    "Eclips.Oracle.Projection",
    "Eclips.Oracle.Receipt",
    "Eclips.Oracle.Step15.Reference",
    "Eclips.Oracle.Step15.WorkflowReference"
  ]

heraldRuntimeOracleExternalModules :: [String]
heraldRuntimeOracleExternalModules =
  [ "Eclips.Herald.OracleClient",
    "Eclips.Herald.OracleHealth",
    "Eclips.Oracle.Voter",
    "Eclips.Raft.Identity",
    "Eclips.Oracle.Canonical",
    "Eclips.Oracle.Command",
    "Eclips.Oracle.Identity",
    "Eclips.Oracle.Progress",
    "Eclips.Oracle.Receipt",
    "Eclips.Protocol.Oracle.Codec",
    "Eclips.Protocol.Oracle.Frame",
    "Eclips.Protocol.Oracle.Types"
  ]

domainModules :: [String]
domainModules =
  [ "Eclips.Domain.Alignment",
    "Eclips.Domain.Context",
    "Eclips.Domain.Disappearance",
    "Eclips.Domain.Environment",
    "Eclips.Domain.Graph",
    "Eclips.Domain.Identity",
    "Eclips.Domain.Label",
    "Eclips.Domain.MemberSet",
    "Eclips.Domain.Membership",
    "Eclips.Domain.ProcessLifecycle",
    "Eclips.Domain.ProcessStart",
    "Eclips.Domain.Publication",
    "Eclips.Domain.Query",
    "Eclips.Domain.Route",
    "Eclips.Domain.Startup",
    "Eclips.Domain.Sort.Canonical",
    "Eclips.Domain.Sort.Descriptor",
    "Eclips.Domain.Sort.Profile",
    "Eclips.Domain.SortOccurrence",
    "Eclips.Domain.Store",
    "Eclips.Domain.Structural",
    "Eclips.Domain.StructuralConsequence",
    "Eclips.Domain.Topology",
    "Eclips.Domain.Value"
  ]

heraldInternalModules :: [String]
heraldInternalModules =
  [ "Eclips.Herald.Administration",
    "Eclips.Herald.Administration.Internal",
    "Eclips.Herald.Administration.RPC",
    "Eclips.Herald.Administration.RPC.Internal",
    "Eclips.Herald.Administration.State",
    "Eclips.Herald.Application.Environment",
    "Eclips.Herald.Application.Disappearance",
    "Eclips.Herald.Application.PrivateIdentity",
    "Eclips.Herald.Application.Forward",
    "Eclips.Herald.Application.Publication",
    "Eclips.Herald.Application.PublicationEvidence",
    "Eclips.Herald.Application.Query",
    "Eclips.Herald.Application.Recovery",
    "Eclips.Herald.Application.Recovery.Internal",
    "Eclips.Herald.Application.RPC",
    "Eclips.Herald.Application.RPC.Internal",
    "Eclips.Herald.Application.Request",
    "Eclips.Herald.Application.Request.Internal",
    "Eclips.Herald.Application.Session",
    "Eclips.Herald.Application.Session.Internal",
    "Eclips.Herald.Application.SortDefinition",
    "Eclips.Herald.Application.Primordial",
    "Eclips.Herald.Application.Primordial.Transfer",
    "Eclips.Herald.Application.State",
    "Eclips.Herald.Authority",
    "Eclips.Herald.Bootstrap",
    "Eclips.Herald.ConfiguredProcess.Start",
    "Eclips.Herald.ConfiguredProcess.State",
    "Eclips.Herald.ProcessPreparation.State",
    "Eclips.Herald.ProcessPreparation.Readiness",
    "Eclips.Herald.ProcessPreparation.Protocol",
    "Eclips.Herald.Controlled.Operate",
    "Eclips.Herald.Controlled.Disappearance",
    "Eclips.Herald.Controlled.State",
    "Eclips.Herald.Disappearance.Evidence",
    "Eclips.Herald.Disappearance.Gate",
    "Eclips.Herald.Disappearance.Evidence.Internal",
    "Eclips.Herald.Disappearance.OwnerEvidence",
    "Eclips.Herald.Disappearance.Protocol",
    "Eclips.Herald.Disappearance.State",
    "Eclips.Herald.DiagnosticChecks",
    "Eclips.Herald.Diagnostics",
    "Eclips.Herald.Discovery",
    "Eclips.Herald.Discovery.Internal",
    "Eclips.Herald.Discovery.State",
    "Eclips.Herald.EffectBatch",
    "Eclips.Herald.EffectivePublication",
    "Eclips.Herald.FailureDetection.State",
    "Eclips.Herald.Genesis",
    "Eclips.Herald.Genesis.Internal",
    "Eclips.Herald.Graph.Progress",
    "Eclips.Herald.Graph.Disappearance",
    "Eclips.Herald.Graph.DisappearanceReadiness",
    "Eclips.Herald.Graph.Protocol",
    "Eclips.Herald.Graph.State",
    "Eclips.Herald.Graph.TerminalSource",
    "Eclips.Herald.IdGenerator",
    "Eclips.Herald.IdGenerator.Internal",
    "Eclips.Herald.IdGenerator.State",
    "Eclips.Herald.Initialization",
    "Eclips.Herald.Input",
    "Eclips.Herald.Internal.Derived",
    "Eclips.Herald.Internal.Diagnostics",
    "Eclips.Herald.Internal.Prepared",
    "Eclips.Herald.Internal.WorkIndex",
    "Eclips.Herald.Isolation",
    "Eclips.Herald.Isolation.Internal",
    "Eclips.Herald.Isolation.State",
    "Eclips.Herald.Join",
    "Eclips.Herald.Join.Bootstrap",
    "Eclips.Herald.Join.Base",
    "Eclips.Herald.Join.State",
    "Eclips.Herald.Join.Replay",
    "Eclips.Herald.Join.Readiness",
    "Eclips.Herald.Join.History",
    "Eclips.Herald.Join.Seal",
    "Eclips.Herald.Join.SourceBundle",
    "Eclips.Herald.Label.Collection",
    "Eclips.Herald.LabelBarrier.State",
    "Eclips.Herald.OracleClient",
    "Eclips.Herald.OracleClient.Internal",
    "Eclips.Herald.OracleClient.Request",
    "Eclips.Herald.OracleClient.State",
    "Eclips.Herald.OracleHealth",
    "Eclips.Herald.OracleHealth.State",
    "Eclips.Herald.OracleProjection",
    "Eclips.Herald.OracleProjection.State",
    "Eclips.Herald.OracleProjection.Step15",
    "Eclips.Herald.Peer.RPC",
    "Eclips.Herald.Peer.RPC.Internal",
    "Eclips.Herald.Peer.Step15",
    "Eclips.Herald.PeerDispatch",
    "Eclips.Herald.PeerDispatch.Internal",
    "Eclips.Herald.PeerLiveness",
    "Eclips.Herald.PeerLiveness.Internal",
    "Eclips.Herald.PeerLiveness.State",
    "Eclips.Herald.PeerPayload",
    "Eclips.Herald.PeerStream",
    "Eclips.Herald.PeerStream.Disappearance",
    "Eclips.Herald.PeerStream.State",
    "Eclips.Herald.PeerPublication",
    "Eclips.Herald.Placement",
    "Eclips.Herald.Placement.Disappearance",
    "Eclips.Herald.Placement.State",
    "Eclips.Herald.Publication.Route",
    "Eclips.Herald.Publication.Disappearance",
    "Eclips.Herald.Publication.Groups",
    "Eclips.Herald.Publication.State",
    "Eclips.Herald.Query",
    "Eclips.Herald.SortRegistry.State",
    "Eclips.Herald.Startup.Invariant",
    "Eclips.Herald.Startup.Semantic",
    "Eclips.Herald.Startup.State",
    "Eclips.Herald.Store.Observation",
    "Eclips.Herald.Store.Disappearance",
    "Eclips.Herald.Store.State",
    "Eclips.Herald.TerminalSourceHold.State",
    "Eclips.Herald.Alignment.CutQueries",
    "Eclips.Herald.Alignment.Generation",
    "Eclips.Herald.Alignment.Plan",
    "Eclips.Herald.Alignment.Plan.Identity",
    "Eclips.Herald.Alignment.History",
    "Eclips.Herald.Alignment.Disappearance",
    "Eclips.Herald.Alignment.Loss",
    "Eclips.Herald.Alignment.Protocol",
    "Eclips.Herald.Alignment.State",
    "Eclips.Herald.PeerDelivery",
    "Eclips.Herald.PeerDelivery.State",
    "Eclips.Herald.UseCase.PeerDelivery",
    "Eclips.Herald.Alignment.Transfer",
    "Eclips.Herald.Structural.Debt",
    "Eclips.Herald.Structural.Reconciliation",
    "Eclips.Herald.Time",
    "Eclips.Herald.Timer",
    "Eclips.Herald.Timer.Internal",
    "Eclips.Herald.Transition",
    "Eclips.Herald.UseCase.Join",
    "Eclips.Herald.UseCase.JoinControlTails",
    "Eclips.Herald.UseCase.ApplicationCall",
    "Eclips.Herald.UseCase.ProcessPreparation",
    "Eclips.Herald.UseCase.ApplicationLiveness",
    "Eclips.Herald.UseCase.ApplicationRetirement",
    "Eclips.Herald.UseCase.ControlledRemoval",
    "Eclips.Herald.UseCase.Disappearance",
    "Eclips.Herald.UseCase.DisappearanceLive",
    "Eclips.Herald.UseCase.FailureDetection",
    "Eclips.Herald.UseCase.LabelPatch",
    "Eclips.Herald.UseCase.Administration",
    "Eclips.Herald.UseCase.Alignment",
    "Eclips.Herald.UseCase.AlignmentHistory",
    "Eclips.Herald.UseCase.AlignmentTransfer",
    "Eclips.Herald.UseCase.NewId",
    "Eclips.Herald.UseCase.NewEnvironment",
    "Eclips.Herald.UseCase.JoinHistory",
    "Eclips.Herald.UseCase.ControlBase",
    "Eclips.Herald.UseCase.ControlReclamation",
    "Eclips.Herald.UseCase.LabelCollection",
    "Eclips.Herald.UseCase.OracleAdvance",
    "Eclips.Herald.UseCase.PeerControl",
    "Eclips.Herald.UseCase.PeerInput",
    "Eclips.Herald.UseCase.PeerPlacement",
    "Eclips.Herald.UseCase.RegularRetirement",
    "Eclips.Herald.UseCase.StructuralCoordinator",
    "Eclips.Herald.UseCase.StructuralProgress",
    "Eclips.Herald.UseCase.StructuralSettlement",
    "Eclips.Herald.UseCase.Step15FailureVertical",
    "Eclips.Herald.UseCase.Step15MembershipAdvance",
    "Eclips.Herald.UseCase.Step15RetirementClosure",
    "Eclips.Herald.UseCase.Step15StructuralBase",
    "Eclips.Herald.UseCase.TerminalStructuralArchive",
    "Eclips.Herald.UseCase.TerminalStructuralStart",
    "Eclips.Herald.Visibility.State",
    "Eclips.Herald.Wait.State"
  ]

heraldFacadeModules :: [String]
heraldFacadeModules =
  [ "Eclips.Herald.Administration",
    "Eclips.Herald.Administration.RPC",
    "Eclips.Herald.Application.RPC",
    "Eclips.Herald.Application.Recovery",
    "Eclips.Herald.Application.Request",
    "Eclips.Herald.Application.Session",
    "Eclips.Herald.DiagnosticChecks",
    "Eclips.Herald.Diagnostics",
    "Eclips.Herald.Discovery",
    "Eclips.Herald.EffectBatch",
    "Eclips.Herald.Genesis",
    "Eclips.Herald.IdGenerator",
    "Eclips.Herald.Initialization",
    "Eclips.Herald.Input",
    "Eclips.Herald.Isolation",
    "Eclips.Herald.Join",
    "Eclips.Herald.OracleClient",
    "Eclips.Herald.OracleHealth",
    "Eclips.Herald.OracleProjection",
    "Eclips.Herald.Peer.RPC",
    "Eclips.Herald.PeerDispatch",
    "Eclips.Herald.PeerLiveness",
    "Eclips.Herald.Placement",
    "Eclips.Herald.Transition",
    "Eclips.Herald.Time",
    "Eclips.Herald.Timer"
  ]

heraldRuntimeInternalModules :: [String]
heraldRuntimeInternalModules =
  [ "Eclips.Herald.Runtime",
    "Eclips.Herald.Runtime.Connection",
    "Eclips.Herald.Runtime.Handler",
    "Eclips.Herald.Runtime.Ingress",
    "Eclips.Herald.Runtime.Oracle",
    "Eclips.Herald.Runtime.Trace",
    "Eclips.Herald.Runtime.Internal.Arbiter",
    "Eclips.Herald.Runtime.Internal.Connection",
    "Eclips.Herald.Runtime.Internal.Conformance",
    "Eclips.Herald.Runtime.Internal.Coordination",
    "Eclips.Herald.Runtime.Internal.GeneratorSeed",
    "Eclips.Herald.Runtime.Internal.Owner",
    "Eclips.Herald.Runtime.Internal.Recording",
    "Eclips.Herald.Runtime.Internal.Timer",
    "Eclips.Herald.Runtime.Internal.Trace",
    "Eclips.Herald.Runtime.Internal.Types"
  ]

heraldRuntimeFacadeModules :: [String]
heraldRuntimeFacadeModules =
  [ "Eclips.Herald.Runtime",
    "Eclips.Herald.Runtime.Connection",
    "Eclips.Herald.Runtime.Handler",
    "Eclips.Herald.Runtime.Ingress",
    "Eclips.Herald.Runtime.Oracle",
    "Eclips.Herald.Runtime.Trace"
  ]

heraldTcpInternalModules :: [String]
heraldTcpInternalModules =
  [ "Eclips.Herald.Runtime.TCP.Internal.Administration",
    "Eclips.Herald.Runtime.TCP.Internal.Application",
    "Eclips.Herald.Runtime.TCP.Internal.Discovery",
    "Eclips.Herald.Runtime.TCP.Internal.Facade",
    "Eclips.Herald.Runtime.TCP.Internal.Heartbeat",
    "Eclips.Herald.Runtime.TCP.Internal.Oracle",
    "Eclips.Herald.Runtime.TCP.Internal.OracleHealth",
    "Eclips.Herald.Runtime.TCP.Internal.Peer",
    "Eclips.Herald.Runtime.TCP.Internal.Scope",
    "Eclips.Herald.Runtime.TCP.Internal.Socket",
    "Eclips.Herald.Runtime.TCP.Internal.Types"
  ]

heraldTcpFacadeModules :: [String]
heraldTcpFacadeModules =
  ["Eclips.Herald.Runtime.TCP"]

deploymentFacadeModules :: [String]
deploymentFacadeModules = helloWorldFacadeModules <> protocolAdminModules <> ["Eclips.Oracle.Admission", "Eclips.Oracle.Canonical", "Eclips.Oracle.Receipt", "Eclips.Oracle.Transition", "Eclips.Oracle.Voter"]

helloWorldFacadeModules :: [String]
helloWorldFacadeModules =
  applicationApiModules
    <> ["Eclips.Protocol.Application.Types"]
    <> heraldFacadeModules
    <> heraldRuntimeFacadeModules
    <> heraldTcpFacadeModules
    <> [ "Eclips.Oracle.Genesis",
         "Eclips.Oracle.Projection",
         "Eclips.Oracle.Runtime",
         "Eclips.Oracle.Runtime.TCP",
         "Eclips.Oracle.Transition",
         "Eclips.Oracle.Voter",
         "Eclips.Raft.Genesis",
         "Eclips.Raft.Identity"
       ]

checkPackageSources :: PackageView -> IO ([Violation], [MembershipSource])
checkPackageSources package@PackageView {packageSpec = PackageSpec {specProductionSourceRoots}, packageRoot} = do
  let productionRoots = fmap (packageRoot </>) specProductionSourceRoots
  listings <- traverse listHaskellFiles productionRoots
  let listingViolations = concatMap fst listings
      files = concatMap snd listings
  sourceResults <- traverse (checkSourceFile package) files
  pure (listingViolations <> foldMap fst sourceResults, foldMap snd sourceResults)

listHaskellFiles :: FilePath -> IO ([Violation], [FilePath])
listHaskellFiles root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure ([Violation ("missing reviewed source directory: " <> root)], [])
    else walk root
  where
    walk directory = do
      entries <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents directory
      results <- forM entries $ \entry -> do
        let path = directory </> entry
        symbolic <- pathIsSymbolicLink path
        if symbolic
          then pure ([Violation ("symbolic links are not reviewed source entries: " <> path)], [])
          else do
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then walk path
              else pure ([], [path | takeExtension path `Set.member` haskellSourceExtensions])
      pure (foldMap fst results, foldMap snd results)

haskellSourceExtensions :: Set String
haskellSourceExtensions =
  Set.fromList
    [ ".hs",
      ".lhs",
      ".hs-boot",
      ".lhs-boot",
      ".hsig",
      ".lhsig",
      ".hsc",
      ".chs",
      ".x",
      ".y",
      ".ly",
      ".cpphs"
    ]

checkSourceFile :: PackageView -> FilePath -> IO ([Violation], [MembershipSource])
checkSourceFile package@PackageView {packageSpec = PackageSpec {specRelativeRoot}, packageRoot} path = do
  contentsResult <- try (readFile path) :: IO (Either IOException String)
  pure $ case contentsResult of
    Left exception -> ([Violation (path <> ": unable to read source: " <> show exception)], [])
    Right contents ->
      let tokens = lexSource 1 contents
          imports = extractImportsFromTokens tokens
          relativePath = slashPath (makeRelative packageRoot path)
          projectRelativePath = slashPath (normalise (specRelativeRoot </> relativePath))
          identifiers =
            [ (line, finalIdentifier word)
            | LocatedToken line (WordToken word) <- tokens
            ]
       in ( concatMap (checkImport package relativePath path) imports
              <> checkPrivilegedIdentifiers package relativePath path tokens
              <> checkOracleRuntimeOwnerIdentifiers package relativePath path tokens
              <> checkProtocolOracleAuthorityIdentifiers package relativePath path tokens
              <> checkRuntimeProfileIdentifiers package path tokens,
            [membershipSource projectRelativePath identifiers]
          )

-- The Herald and Oracle/Raft runtimes are intentionally unbounded,
-- current-build typed shells. Keep the small set of explicitly deferred policy
-- mechanisms mechanically absent from their production sources, in addition to
-- checking dependencies and imports above. Tokenizing first avoids matches in
-- comments and diagnostics.
checkRuntimeProfileIdentifiers :: PackageView -> FilePath -> [LocatedToken] -> [Violation]
checkRuntimeProfileIdentifiers PackageView {packageSpec = PackageSpec {specKey = packageKey}} absolutePath tokens
  | packageKey `notElem` [OracleRuntime, HeraldRuntime] = []
  | otherwise =
      [ Violation
          ( absolutePath
              <> ":"
              <> show line
              <> ": deferred runtime policy/scaffolding identifier is not part of profile 0.1: "
              <> identifier
          )
      | LocatedToken line (WordToken word) <- tokens,
        let identifier = finalIdentifier word,
        identifier `Set.member` runtimeProfileForbiddenIdentifiers
      ]

runtimeProfileForbiddenIdentifiers :: Set String
runtimeProfileForbiddenIdentifiers =
  Set.fromList
    [ "TBQueue",
      "newTBQueue",
      "newTBQueueIO",
      "readTBQueue",
      "tryReadTBQueue",
      "peekTBQueue",
      "tryPeekTBQueue",
      "writeTBQueue",
      "isFullTBQueue",
      "lengthTBQueue",
      "flushTBQueue",
      "maxBound",
      "RuntimeCapacity",
      "RuntimeOverloaded",
      "ProtocolVersion",
      "SchemaVersion",
      "RuntimeAuthentication",
      "RuntimeRateLimit",
      "GenericRuntimeTimer",
      "RuntimeDialRequest"
    ]

data Import = Import
  { importLine :: Int,
    importedModule :: String
  }
  deriving stock (Eq, Show)

data TokenValue
  = WordToken String
  | StringToken String
  deriving stock (Eq, Show)

data LocatedToken = LocatedToken
  { tokenLine :: Int,
    tokenValue :: TokenValue
  }
  deriving stock (Eq, Show)

extractImportsFromTokens :: [LocatedToken] -> [Import]
extractImportsFromTokens = go
  where
    go [] = []
    go (LocatedToken line (WordToken "import") : rest) =
      case importedName rest of
        Nothing -> go rest
        Just (name, remaining) -> Import line name : go remaining
    go (_ : rest) = go rest

importedName :: [LocatedToken] -> Maybe (String, [LocatedToken])
importedName tokens =
  let withoutSafe = dropOptionalWord "safe" tokens
      withoutLeadingQualified = dropOptionalWord "qualified" withoutSafe
      withoutPackage = case withoutLeadingQualified of
        LocatedToken _ (StringToken _) : rest -> rest
        rest -> rest
      withoutTrailingQualified = dropOptionalWord "qualified" withoutPackage
   in case withoutTrailingQualified of
        LocatedToken _ (WordToken name) : rest
          | isModuleName name -> Just (name, rest)
        _ -> Nothing

dropOptionalWord :: String -> [LocatedToken] -> [LocatedToken]
dropOptionalWord wanted (LocatedToken _ (WordToken actual) : rest)
  | actual == wanted = rest
dropOptionalWord _ tokens = tokens

isModuleName :: String -> Bool
isModuleName name = case name of
  first : _ -> first >= 'A' && first <= 'Z'
  [] -> False

lexSource :: Int -> String -> [LocatedToken]
lexSource _ [] = []
lexSource line ('-' : '-' : rest) = lexSource (line + newlineCount comment) remaining
  where
    (comment, remaining) = break (== '\n') rest
lexSource line ('{' : '-' : rest) =
  let (consumedLines, remaining) = skipBlockComment 1 0 rest
   in lexSource (line + consumedLines) remaining
lexSource line ('"' : rest) =
  let (value, consumedLines, remaining) = scanQuoted '"' rest
   in LocatedToken line (StringToken value) : lexSource (line + consumedLines) remaining
lexSource line (character : rest)
  | character == '\n' = lexSource (line + 1) rest
  | isSpace character = lexSource line rest
  | isWordStart character =
      let (suffix, remaining) = span isWordCharacter rest
       in LocatedToken line (WordToken (character : suffix)) : lexSource line remaining
  | otherwise = lexSource line rest

isWordStart :: Char -> Bool
isWordStart character = isAlpha character || character == '_'

isWordCharacter :: Char -> Bool
isWordCharacter character = isAlphaNum character || character `elem` ("_'." :: String)

scanQuoted :: Char -> String -> (String, Int, String)
scanQuoted delimiter = go [] 0
  where
    go accumulator linesSeen [] = (reverse accumulator, linesSeen, [])
    go accumulator linesSeen ('\\' : escaped : rest) =
      go (escaped : accumulator) (linesSeen + fromEnum (escaped == '\n')) rest
    go accumulator linesSeen (character : rest)
      | character == delimiter = (reverse accumulator, linesSeen, rest)
      | otherwise = go (character : accumulator) (linesSeen + fromEnum (character == '\n')) rest

skipBlockComment :: Int -> Int -> String -> (Int, String)
skipBlockComment _ linesSeen [] = (linesSeen, [])
skipBlockComment depth linesSeen ('{' : '-' : rest) = skipBlockComment (depth + 1) linesSeen rest
skipBlockComment depth linesSeen ('-' : '}' : rest)
  | depth == 1 = (linesSeen, rest)
  | otherwise = skipBlockComment (depth - 1) linesSeen rest
skipBlockComment depth linesSeen (character : rest) =
  skipBlockComment depth (linesSeen + fromEnum (character == '\n')) rest

newlineCount :: String -> Int
newlineCount = length . filter (== '\n')

-- Owner-minting functions live beside otherwise shared nominal vocabulary.
-- Pinning their exact source consumers prevents an authorised vocabulary import
-- from silently becoming authority to mint a Discovery generation or dispatch
-- correlation.
checkPrivilegedIdentifiers ::
  PackageView ->
  FilePath ->
  FilePath ->
  [LocatedToken] ->
  [Violation]
checkPrivilegedIdentifiers PackageView {packageSpec = PackageSpec {specKey = packageKey}} relativePath absolutePath tokens
  | packageKey `notElem` [HeraldCore, HeraldRuntime] = []
  | otherwise =
      [ Violation
          ( absolutePath
              <> ": owner-only constructor function used outside its reviewed allowlist: "
              <> identifier
          )
      | identifier <- Set.toAscList sourceIdentifiers,
        Just authorisedPaths <- [Map.lookup identifier identifierAllowlist],
        relativePath `Set.notMember` authorisedPaths
      ]
  where
    identifierAllowlist = case packageKey of
      HeraldRuntime -> runtimeDisappearanceAbortIdentifierAllowlist
      _ -> privilegedIdentifierAllowlist
    sourceIdentifiers =
      Set.fromList
        [ finalIdentifier word
        | LocatedToken _ (WordToken word) <- tokens
        ]

checkOracleRuntimeOwnerIdentifiers ::
  PackageView ->
  FilePath ->
  FilePath ->
  [LocatedToken] ->
  [Violation]
checkOracleRuntimeOwnerIdentifiers PackageView {packageSpec = PackageSpec {specKey = packageKey}} relativePath absolutePath tokens
  | packageKey /= OracleRuntime = []
  | otherwise =
      [ Violation
          ( absolutePath
              <> ": kernel state/transition authority used outside its serialized runtime owner: "
              <> identifier
          )
      | LocatedToken _ (WordToken word) <- tokens,
        let identifier = finalIdentifier word,
        Just ownerPath <- [Map.lookup identifier oracleRuntimeOwnerIdentifiers],
        relativePath /= ownerPath,
        not
          ( identifier == "RaftState"
              && relativePath
                == "internal/Eclips/Oracle/Runtime/Internal/StatusProjection.hs"
          )
      ]

oracleRuntimeOwnerIdentifiers :: Map String FilePath
oracleRuntimeOwnerIdentifiers =
  Map.fromList
    [ ("OracleState", "src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs"),
      ("RaftState", "src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs"),
      ("stepOracle", "src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs"),
      ("stepRaft", "src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs")
    ]

checkProtocolOracleAuthorityIdentifiers ::
  PackageView ->
  FilePath ->
  FilePath ->
  [LocatedToken] ->
  [Violation]
checkProtocolOracleAuthorityIdentifiers PackageView {packageSpec = PackageSpec {specKey = packageKey}} relativePath absolutePath tokens
  | packageKey /= ProtocolOracle = []
  | relativePath `Set.member` protocolOracleAuthorityOwnerPaths = []
  | otherwise =
      [ Violation
          ( absolutePath
              <> ":"
              <> show line
              <> ": Oracle request-absence authority vocabulary used outside its Types/Frame owners: "
              <> identifier
          )
      | LocatedToken line (WordToken word) <- tokens,
        let identifier = finalIdentifier word,
        identifier `Set.member` protocolOracleAuthorityIdentifiers
      ]

protocolOracleAuthorityOwnerPaths :: Set FilePath
protocolOracleAuthorityOwnerPaths =
  Set.fromList
    [ "src/Eclips/Protocol/Oracle/Types.hs",
      "src/Eclips/Protocol/Oracle/Frame.hs"
    ]

protocolOracleAuthorityIdentifiers :: Set String
protocolOracleAuthorityIdentifiers =
  Set.fromList
    [ "OracleAuthorityError",
      "OracleIngressAuthorityUpdateError",
      "OracleIngressAuthorityUpdateResult",
      "OracleRequestAbsenceAuthority",
      "OracleServiceReadyLeaderContext",
      "admitOracleEnvelopeAuthority",
      "admitOracleHelloAuthority",
      "invalidateOracleClientIngressAuthority",
      "noOracleRequestAbsenceAuthority",
      "oracleServiceReadyLeaderContext",
      "updateOracleClientIngressAuthority"
    ]

finalIdentifier :: String -> String
finalIdentifier = reverse . takeWhile (/= '.') . reverse

-- Only explicit deployment/test ingress reaches the serialized input lane.
-- Clock, retry, peer and failure workers have no Abort construction authority.
runtimeDisappearanceAbortIdentifierAllowlist :: Map String (Set FilePath)
runtimeDisappearanceAbortIdentifierAllowlist =
  Map.fromList
    [ ("AbortDisappearanceProbe", Set.singleton "internal/Eclips/Herald/Runtime/Internal/Owner.hs"),
      ( "DisappearanceInput",
        Set.fromList
          ["internal/Eclips/Herald/Runtime/Internal/Owner.hs", "internal/Eclips/Herald/Runtime/Internal/Coordination.hs"]
      ),
      ( "DisappearanceAbortEnvelope",
        Set.fromList
          ["internal/Eclips/Herald/Runtime/Internal/Owner.hs", "internal/Eclips/Herald/Runtime/Internal/Coordination.hs"]
      ),
      ( "runtimeSubmitDisappearanceAbort",
        Set.fromList
          ["src/Eclips/Herald/Runtime/Ingress.hs", "internal/Eclips/Herald/Runtime/Internal/Owner.hs", "internal/Eclips/Herald/Runtime/Internal/Types.hs"]
      ),
      ("submitDisappearanceProbeAbort", Set.singleton "src/Eclips/Herald/Runtime/Ingress.hs")
    ]

privilegedIdentifierAllowlist :: Map String (Set FilePath)
privilegedIdentifierAllowlist =
  Map.fromList
    [ ( "DisappearanceInput",
        Set.fromList ["src/Eclips/Herald/Input.hs", "src/Eclips/Herald/Transition.hs"]
      ),
      ( "AbortDisappearanceProbe",
        Set.fromList ["src/Eclips/Herald/Input.hs", "src/Eclips/Herald/Transition.hs"]
      ),
      ( "prepareAbortDisappearanceProbeRequest",
        Set.fromList ["internal/Eclips/Herald/OracleClient/State.hs", "internal/Eclips/Herald/UseCase/DisappearanceLive.hs"]
      ),
      ( "authorizedDisappearanceAbortReason",
        -- Projection only reconstructs an already accepted terminal fact from
        -- a portable base; it cannot issue an Abort request.
        Set.fromList
          [ "internal/Eclips/Herald/UseCase/DisappearanceLive.hs",
            "internal/Eclips/Herald/OracleProjection/State.hs"
          ]
      ),
      ( "applicationAttachmentFromClaimBytes",
        Set.fromList ["internal/Eclips/Herald/UseCase/ApplicationLiveness.hs", "internal/Eclips/Herald/UseCase/ProcessPreparation.hs"]
          <> applicationClaimReconstructionOwners "internal/Eclips/Herald/Application/Session/Internal.hs"
      ),
      ( "applicationSessionIdFromClaimParts",
        applicationClaimReconstructionOwners
          "internal/Eclips/Herald/Application/Session/Internal.hs"
      ),
      ( "applicationResumeTokenFromClaimParts",
        applicationClaimReconstructionOwners
          "internal/Eclips/Herald/Application/Session/Internal.hs"
      ),
      ( "applicationWaitIdFromClaimParts",
        applicationClaimReconstructionOwners
          "internal/Eclips/Herald/Application/Request/Internal.hs"
      ),
      ( "applicationReplyCursorFromClaimWord64",
        applicationClaimReconstructionOwners
          "internal/Eclips/Herald/Application/Request/Internal.hs"
      ),
      ( "applicationAttachmentForBootstrap",
        Set.fromList
          [ "internal/Eclips/Herald/Application/Session/Internal.hs",
            "internal/Eclips/Herald/Bootstrap.hs",
            "internal/Eclips/Herald/Startup/Invariant.hs",
            "internal/Eclips/Herald/UseCase/StructuralSettlement.hs",
            "internal/Eclips/Herald/UseCase/ProcessPreparation.hs"
          ]
      ),
      ( "waitIdForSessionParts",
        requestAllocationOwners
      ),
      ( "firstApplicationReplyCursor",
        Set.insert "internal/Eclips/Herald/Startup/Invariant.hs" requestAllocationOwners
      ),
      ( "nextApplicationReplyCursor",
        requestAllocationOwners
      ),
      ( "initialSessionAllocation",
        Set.insert "internal/Eclips/Herald/Startup/Invariant.hs" sessionAllocationOwners
      ),
      ( "advanceSessionBinding",
        sessionAllocationOwners
      ),
      ( "assembleDisappearanceEvidenceSnapshot",
        Set.fromList
          [ "internal/Eclips/Herald/Disappearance/Evidence.hs",
            "internal/Eclips/Herald/Disappearance/Evidence/Internal.hs"
          ]
      ),
      ( "completedIncomingAlignmentEvidenceForOwner",
        disappearanceStateOwners
      ),
      ( "disappearanceAbsenceAttestation",
        Set.fromList
          [ "internal/Eclips/Herald/Disappearance/Evidence.hs",
            disappearanceProtocolPath
          ]
      ),
      ( "disappearanceBlockerWitness",
        Set.fromList
          [ "internal/Eclips/Herald/Disappearance/OwnerEvidence.hs",
            disappearanceProtocolPath
          ]
      ),
      ( "disappearanceOpenContext",
        Set.fromList
          [ disappearanceProtocolPath,
            "internal/Eclips/Herald/Graph/DisappearanceReadiness.hs"
          ]
      ),
      ( "localAbsenceReportForOwner",
        disappearanceStateOwners
      ),
      ( "matchingPublicationObservation",
        Set.fromList
          [ disappearanceProtocolPath,
            "internal/Eclips/Herald/Publication/Disappearance.hs"
          ]
      ),
      ( "ownerEvidenceFact",
        disappearanceEvidenceAdapterPaths
      ),
      ( "projectedDisappearanceProbe",
        Set.fromList [disappearanceProtocolPath, "internal/Eclips/Herald/UseCase/OracleAdvance.hs"]
      ),
      ( "projectedLabelTerminal",
        Set.fromList [disappearanceProtocolPath, "internal/Eclips/Herald/UseCase/OracleAdvance.hs"]
      ),
      ( "discoveryStatic",
        Set.fromList
          [ "internal/Eclips/Herald/Discovery/Internal.hs",
            "src/Eclips/Herald/Initialization.hs"
          ]
      ),
      ( "firstPeerBindingGeneration",
        discoveryGenerationOwners
      ),
      ( "nextPeerBindingGeneration",
        discoveryGenerationOwners
      ),
      ( "peerDispatchAttempt",
        peerDispatchOwners
      ),
      ( "peerDispatchAttemptGenerationForOwner",
        peerDispatchOwners
      ),
      ( "peerDispatchTicket",
        peerDispatchOwners
      )
    ]
  where
    discoveryGenerationOwners =
      Set.fromList
        [ "internal/Eclips/Herald/Discovery/Internal.hs",
          "internal/Eclips/Herald/Discovery/State.hs"
        ]
    peerDispatchOwners =
      Set.fromList
        [ "internal/Eclips/Herald/PeerStream.hs",
          "internal/Eclips/Herald/PeerStream/State.hs"
        ]
    applicationClaimReconstructionOwners definitionPath =
      Set.fromList
        [ definitionPath,
          applicationRpcInternalPath
        ]
    requestAllocationOwners =
      Set.fromList
        [ "internal/Eclips/Herald/Application/Request/Internal.hs",
          "internal/Eclips/Herald/Application/State.hs"
        ]
    sessionAllocationOwners =
      Set.fromList
        [ "internal/Eclips/Herald/Application/Session/Internal.hs",
          "internal/Eclips/Herald/Application/State.hs"
        ]
    disappearanceProtocolPath =
      "internal/Eclips/Herald/Disappearance/Protocol.hs"
    disappearanceStateOwners =
      Set.fromList
        [ disappearanceProtocolPath,
          "internal/Eclips/Herald/Disappearance/State.hs"
        ]
    disappearanceEvidenceAdapterPaths =
      Set.fromList
        [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
          "internal/Eclips/Herald/Application/Disappearance.hs",
          "internal/Eclips/Herald/Controlled/Disappearance.hs",
          "internal/Eclips/Herald/Disappearance/OwnerEvidence.hs",
          "internal/Eclips/Herald/Graph/Disappearance.hs",
          "internal/Eclips/Herald/PeerStream/Disappearance.hs",
          "internal/Eclips/Herald/Placement/Disappearance.hs",
          "internal/Eclips/Herald/Publication/Disappearance.hs",
          "internal/Eclips/Herald/Store/Disappearance.hs"
        ]

peerDispatchInternalPath :: FilePath
peerDispatchInternalPath =
  "internal/Eclips/Herald/PeerDispatch/Internal.hs"

oracleRaftImportAllowlist :: Set (FilePath, String)
oracleRaftImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Oracle/Internal/Label.hs", "Eclips.Raft.Configuration"),
      ("src/Eclips/Oracle/Internal/Label.hs", "Eclips.Raft.Effect"),
      ("src/Eclips/Oracle/Internal/Label.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Oracle/Internal/Voter.hs", "Eclips.Raft.Configuration"),
      ("src/Eclips/Oracle/Internal/Voter.hs", "Eclips.Raft.Effect"),
      ("src/Eclips/Oracle/Internal/Voter.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Oracle/Internal/Voter.hs", "Eclips.Raft.Input"),
      ("src/Eclips/Oracle/Transition.hs", "Eclips.Raft.Effect"),
      ("src/Eclips/Oracle/Genesis.hs", "Eclips.Raft.Genesis"),
      ("src/Eclips/Oracle/Genesis.hs", "Eclips.Raft.Identity")
    ]

oracleCerealImportAllowlist :: Set (FilePath, String)
oracleCerealImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Oracle/Internal/Voter.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/Admission.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/Disappearance.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Genesis.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/ConfiguredBootstrapCanonical.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/ConfiguredBootstrapCanonical.hs", "Data.Serialize.Get"),
      ("src/Eclips/Oracle/Internal/Digest.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/Failure.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/Failure.hs", "Data.Serialize.Put"),
      ("src/Eclips/Oracle/Internal/Label.hs", "Data.Serialize.Get"),
      ("src/Eclips/Oracle/Internal/Label.hs", "Data.Serialize.Put"),
      ("src/Eclips/Oracle/Internal/LabelCanonical.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/DisappearanceCanonical.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Internal/DisappearanceCanonical.hs", "Data.Serialize.Get"),
      ("src/Eclips/Oracle/Internal/DisappearanceCanonical.hs", "Data.Serialize.Put"),
      ("src/Eclips/Oracle/Step15/Reference.hs", "Data.Serialize"),
      ("src/Eclips/Oracle/Step15/Reference.hs", "Data.Serialize.Put"),
      ("src/Eclips/Oracle/Step15/WorkflowReference.hs", "Data.Serialize")
    ]

oracleCryptoImportAllowlist :: Set (FilePath, String)
oracleCryptoImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Oracle/Internal/Voter.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Internal/Admission.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Internal/Digest.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Internal/Label.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Step15/Reference.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Step15/WorkflowReference.hs", "Crypto.Hash.SHA256"),
      ("src/Eclips/Oracle/Internal/Disappearance.hs", "Crypto.Hash.SHA256")
    ]

protocolRaftCoreImportAllowlist :: Set (FilePath, String)
protocolRaftCoreImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Raft/Types.hs", "Eclips.Raft.Configuration"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Eclips.Raft.Checkpoint"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Eclips.Raft.Input")
    ]

protocolRaftFrameImportAllowlist :: Set (FilePath, String)
protocolRaftFrameImportAllowlist =
  Set.singleton
    ("src/Eclips/Protocol/Raft/Frame.hs", "Eclips.Protocol.Frame")

protocolRaftBinaryImportAllowlist :: Set (FilePath, String)
protocolRaftBinaryImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Raft/Codec.hs", "Data.Binary"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Data.Binary"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Data.Binary.Get"),
      ("src/Eclips/Protocol/Raft/Types.hs", "Data.Binary.Put")
    ]

protocolOracleSemanticImportAllowlist :: Set (FilePath, String)
protocolOracleSemanticImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Domain.Identity"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Oracle.Canonical"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Oracle.Projection"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Raft.Configuration"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Domain.Identity"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Domain.Membership"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Domain.Startup"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Canonical"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Command"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Identity"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Progress"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Projection"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Receipt"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Raft.Identity")
    ]

protocolOracleCanonicalImportAllowlist :: Set (FilePath, String)
protocolOracleCanonicalImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Oracle/Types.hs", "Eclips.Oracle.Canonical"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Oracle.Canonical")
    ]

protocolOracleFrameImportAllowlist :: Set (FilePath, String)
protocolOracleFrameImportAllowlist =
  Set.singleton
    ("src/Eclips/Protocol/Oracle/Frame.hs", "Eclips.Protocol.Frame")

protocolOracleBinaryImportAllowlist :: Set (FilePath, String)
protocolOracleBinaryImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Oracle/Types.hs", "Data.Binary"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Data.Binary.Get"),
      ("src/Eclips/Protocol/Oracle/Types.hs", "Data.Binary.Put"),
      ("src/Eclips/Protocol/Oracle/Codec.hs", "Data.Binary")
    ]

protocolOracleInternalImportAllowlist :: Set (FilePath, String)
protocolOracleInternalImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Protocol/Oracle/Codec.hs", "Eclips.Protocol.Oracle.Types"),
      ("src/Eclips/Protocol/Oracle/Frame.hs", "Eclips.Protocol.Oracle.Codec"),
      ("src/Eclips/Protocol/Oracle/Frame.hs", "Eclips.Protocol.Oracle.Types")
    ]

checkImport :: PackageView -> FilePath -> FilePath -> Import -> [Violation]
checkImport PackageView {packageSpec = PackageSpec {specKey = packageKey, specAllowedEclipsRoots, specAllowedEclipsModules, specRuntimeImportPolicy, specDiagnosticLabel}} relativePath absolutePath Import {importLine, importedModule} =
  fmap (Violation . diagnostic) reasons
  where
    diagnostic reason =
      absolutePath <> ":" <> show importLine <> ": " <> reason <> ": " <> importedModule
    reasons =
      packagePolicy
        <> preparedPolicy
        <> privateIdentityPolicy
        <> peerStreamPolicy
        <> step7OwnerCodecPolicy
        <> heraldCanonicalCodecPolicy
        <> idGeneratorCryptoPolicy
        <> idGeneratorInternalPolicy
        <> oracleCryptoPolicy
        <> oracleCanonicalCodecPolicy
        <> oracleRaftSeamPolicy
        <> protocolAdminOraclePolicy
        <> protocolRaftBinaryPolicy
        <> protocolRaftCorePolicy
        <> protocolRaftFramePolicy
        <> protocolOracleBinaryPolicy
        <> protocolOracleCanonicalPolicy
        <> protocolOracleSemanticPolicy
        <> protocolOracleFramePolicy
        <> protocolOracleInternalPolicy
        <> applicationClientRecoveryConstructionPolicy
        <> oracleRuntimeEntropyPolicy
        <> oracleRuntimeNetworkPolicy
        <> oracleRuntimeInternalSeamPolicy
        <> oracleRuntimeProtocolPolicy
        <> oracleRuntimeTransitionPolicy
        <> step15ReferencePolicy
        <> disappearanceReferencePolicy
        <> heraldOracleCorePolicy
        <> heraldRuntimeOraclePolicy
        <> heraldRuntimeAdminPolicy
        <> heraldProtocolPolicy
        <> heraldAdminProtocolPolicy
        <> heraldPeerProtocolPolicy
        <> ownerConstructionPolicy
        <> kernelSeamPolicy
        <> crossLeafPolicy
        <> applicationPhysicalPurityPolicy
        <> applicationApiSocketPolicy
        <> runtimeInternalImportPolicy
        <> tcpRuntimeInternalImportPolicy
        <> runtimeKernelOwnerPolicy
        <> runtimeRecoveryVocabularyPolicy
        <> runtimeTimerInternalPolicy
        <> tcpHeartbeatInternalPolicy
    packagePolicy =
      [ specDiagnosticLabel <> " must not import runtime, effect, concurrency, network, clock, entropy, or IO modules"
      | specRuntimeImportPolicy == RuntimeFree,
        isRuntimeImport importedModule
      ]
        <> [ specDiagnosticLabel <> " may import only its reviewed ECLIPS namespace roots"
           | isEclipsImport importedModule,
             not
               ( any (`moduleWithin` importedModule) specAllowedEclipsRoots
                   || importedModule `elem` specAllowedEclipsModules
               )
           ]
        <> packageSpecificPolicy
    packageSpecificPolicy = case packageKey of
      HelloWorldExample ->
        [ "ordinary application executables may import only the direct facade, advanced typed calls, connection descriptors, and application types"
        | any (`isPrefixOf` relativePath) ["applications/", "launcher/", "publisher/", "reader/"],
          isEclipsImport importedModule,
          not
            ( moduleWithin "Eclips.Application.Types" importedModule
                || importedModule `elem` ["Eclips.Application", "Eclips.Application.Advanced", "Eclips.Application.Connection", "Eclips.Application.Typed", "Eclips.Application.Typed.Advanced"]
            )
        ]
      Domain ->
        [ "semantic-domain modules must not import protocol/codec or higher-level components"
        | any (`moduleWithin` importedModule) domainForbiddenImports
        ]
      RaftCore ->
        [ "the pure Raft core must not import codecs, cryptography, persistence, or higher-level components"
        | any (`moduleWithin` importedModule) raftCoreForbiddenImports
        ]
      OracleCore ->
        [ "the pure Oracle core must not import generic wire codecs, persistence, or higher-level components"
        | any (`moduleWithin` importedModule) oracleCoreForbiddenImports
        ]
      ProtocolRaft ->
        [ "the pure Raft protocol must not import canonical codecs, cryptography, persistence, or higher-level components"
        | any (`moduleWithin` importedModule) protocolRaftForbiddenImports
        ]
      ProtocolOracle ->
        [ "the pure Oracle protocol must not import cryptography, persistence, runtime owners, or unrelated protocol families"
        | any (`moduleWithin` importedModule) protocolOracleForbiddenImports
        ]
      OracleRuntime ->
        [ "the Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules"
        | any (`moduleWithin` importedModule) oracleRuntimeForbiddenImports
            && not
              ( relativePath
                  == "socket-internal/Eclips/Oracle/Runtime/Internal/TCP/Socket.hs"
                  && importedModule == "System.Timeout"
                  || relativePath == "internal/Eclips/Oracle/Runtime/Internal/Checkpoint.hs" && importedModule == "Data.Binary"
                  || isOracleOwnerDiagnosticImport
              )
        ]
      HeraldRuntime ->
        [ "the typed Herald runtime must not import sockets, codecs, entropy, persistence, process, or compatibility modules"
        | not isTcpSource,
          isRuntimeForbiddenImport importedModule,
          not
            ( relativePath
                == "internal/Eclips/Herald/Runtime/Internal/GeneratorSeed.hs"
                && importedModule == "System.Entropy"
            )
        ]
          <> [ "the typed Herald runtime may import only checked DTO vocabulary, not codecs or framing"
             | not isTcpSource,
               moduleWithin "Eclips.Protocol.Application" importedModule,
               importedModule /= "Eclips.Protocol.Application.Types"
             ]
          <> [ "the typed Herald runtime may import only checked peer DTO vocabulary, not codecs or framing"
             | not isTcpSource,
               moduleWithin "Eclips.Protocol.Peer" importedModule,
               importedModule /= "Eclips.Protocol.Peer.Types"
             ]
          <> [ "Herald TCP modules may import only public Herald/runtime facades and the reviewed application, administration, peer, Oracle protocol, or shared timing seams"
             | isTcpSource,
               isEclipsImport importedModule,
               not
                 ( moduleWithin "Eclips.Herald.Runtime" importedModule
                     || importedModule `elem` heraldFacadeModules
                     || moduleWithin "Eclips.Protocol.Application" importedModule
                     || importedModule == "Eclips.Application.Types.Lifecycle"
                     || moduleWithin "Eclips.Protocol.Peer" importedModule
                     || (relativePath == "tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs" && importedModule `elem` receiptRetirementModule)
                     || (relativePath, importedModule)
                       `Set.member` heraldRuntimeTcpDomainImportAllowlist
                     || (relativePath, importedModule)
                       `Set.member` heraldRuntimeAdminImportAllowlist
                     || (relativePath, importedModule)
                       `Set.member` heraldRuntimeOracleImportAllowlist
                     || (relativePath, importedModule)
                       `Set.member` heraldRuntimeTimingImportAllowlist
                 )
             ]
      _ -> []
    preparedPolicy =
      [ "Eclips.Herald.Internal.Prepared and Derived must remain import-free"
      | packageKey == HeraldCore,
        relativePath `elem` ["internal/Eclips/Herald/Internal/Prepared.hs", "internal/Eclips/Herald/Internal/Derived.hs"]
      ]
    privateIdentityPolicy =
      [ "the private-identity owner may import only its application/domain identity vocabulary and Prepared below the Herald leaves"
      | packageKey == HeraldCore,
        relativePath == "src/Eclips/Herald/Application/PrivateIdentity.hs",
        isEclipsImport importedModule,
        importedModule
          `notElem` [ "Eclips.Application.Types.Identity",
                      "Eclips.Domain.Identity",
                      "Eclips.Herald.Internal.Prepared"
                    ]
      ]
    peerStreamPolicy =
      [ "the standalone PeerStream owner may import only its vocabulary, epoch identity, Prepared, and exact compact scheduling dependencies"
      | packageKey == HeraldCore,
        relativePath
          `elem` [ "internal/Eclips/Herald/PeerStream.hs",
                   "internal/Eclips/Herald/PeerStream/State.hs"
                 ],
        isEclipsImport importedModule,
        importedModule
          `notElem` [ "Eclips.Domain.Identity",
                      "Eclips.Herald.Internal.Prepared",
                      "Eclips.Herald.PeerStream",
                      "Eclips.Public.Types.Diagnostic",
                      "Eclips.Public.Types.ReceiptRetirement"
                    ],
        not
          ( relativePath == "internal/Eclips/Herald/PeerStream/State.hs"
              && importedModule
                `elem` [ "Eclips.Domain.Alignment",
                         "Eclips.Domain.Disappearance",
                         "Eclips.Herald.Internal.WorkIndex"
                       ]
          )
      ]
    step7OwnerCodecPolicy =
      [ "Herald kernel owners must not import wire or generic codec modules"
      | packageKey == HeraldCore,
        relativePath `Set.member` step7OwnerSourcePaths,
        any (`moduleWithin` importedModule) codecImportRoots,
        (relativePath, importedModule)
          `Set.notMember` heraldCerealImportAllowlist
      ]
    heraldCanonicalCodecPolicy =
      [ "Herald canonical cereal imports must remain in the exact reviewed digest/transcript owners"
      | packageKey == HeraldCore,
        moduleWithin "Data.Serialize" importedModule,
        (relativePath, importedModule)
          `Set.notMember` heraldCerealImportAllowlist
      ]
    idGeneratorCryptoPolicy =
      [ "Herald-core cryptography imports must remain at their exact reviewed owners"
      | packageKey == HeraldCore,
        moduleWithin "Crypto" importedModule,
        not
          ( ( relativePath == "internal/Eclips/Herald/IdGenerator/State.hs"
                && importedModule
                  `elem` [ "Crypto.Cipher.AES",
                           "Crypto.Cipher.Types",
                           "Crypto.Error"
                         ]
            )
              || ( relativePath == "internal/Eclips/Herald/PeerPublication.hs"
                     && importedModule == "Crypto.Hash.SHA256"
                 )
              || ( relativePath == "internal/Eclips/Herald/Disappearance/Protocol.hs"
                     && importedModule == "Crypto.Hash.SHA256"
                 )
              || ( relativePath == "internal/Eclips/Herald/Administration/Internal.hs"
                     && importedModule == "Crypto.Hash.SHA256"
                 )
              || ( relativePath == "internal/Eclips/Herald/Graph/TerminalSource.hs"
                     && importedModule == "Crypto.Hash.SHA256"
                 )
          )
      ]
    idGeneratorInternalPolicy =
      [ "the exact generator key bytes may be imported only by the public opaque facade and private generator state"
      | packageKey == HeraldCore,
        importedModule == "Eclips.Herald.IdGenerator.Internal",
        relativePath
          `notElem` [ "src/Eclips/Herald/IdGenerator.hs",
                      "internal/Eclips/Herald/IdGenerator/State.hs"
                    ]
      ]
    oracleCryptoPolicy =
      [ "Oracle-core cryptographic imports must remain in the reviewed digest owners"
      | packageKey == OracleCore,
        moduleWithin "Crypto" importedModule,
        (relativePath, importedModule)
          `Set.notMember` oracleCryptoImportAllowlist
      ]
    oracleCanonicalCodecPolicy =
      [ "Oracle-core cereal imports must remain in the reviewed canonical and normalized-transcript owners"
      | packageKey == OracleCore,
        moduleWithin "Data.Serialize" importedModule,
        (relativePath, importedModule)
          `Set.notMember` oracleCerealImportAllowlist
      ]
    oracleRaftSeamPolicy =
      [ "only reviewed Oracle genesis and committed-configuration owners may import the checked Raft vocabulary"
      | packageKey == OracleCore,
        moduleWithin "Eclips.Raft" importedModule,
        (relativePath, importedModule)
          `Set.notMember` oracleRaftImportAllowlist
      ]
    protocolAdminOraclePolicy =
      [ "canonical Oracle voter values in EADM belong only to the reviewed DTO owner"
      | packageKey == ProtocolAdmin,
        moduleWithin "Eclips.Oracle" importedModule,
        relativePath /= "src/Eclips/Protocol/Admin/Types.hs"
      ]
    protocolRaftBinaryPolicy =
      [ "Raft-protocol Binary imports must remain in the reviewed DTO and codec owners"
      | packageKey == ProtocolRaft,
        moduleWithin "Data.Binary" importedModule,
        (relativePath, importedModule)
          `Set.notMember` protocolRaftBinaryImportAllowlist
      ]
    protocolRaftCorePolicy =
      [ "only Raft-protocol DTO adapters may import the reviewed Raft-core configuration, identity and input vocabulary"
      | packageKey == ProtocolRaft,
        moduleWithin "Eclips.Raft" importedModule,
        (relativePath, importedModule)
          `Set.notMember` protocolRaftCoreImportAllowlist
      ]
    protocolRaftFramePolicy =
      [ "only the Raft-protocol frame owner may import the common frame protocol"
      | packageKey == ProtocolRaft,
        importedModule == "Eclips.Protocol.Frame",
        (relativePath, importedModule)
          `Set.notMember` protocolRaftFrameImportAllowlist
      ]
    protocolOracleBinaryPolicy =
      [ "Oracle-protocol Binary imports must remain in the reviewed DTO and codec owners"
      | packageKey == ProtocolOracle,
        moduleWithin "Data.Binary" importedModule,
        (relativePath, importedModule)
          `Set.notMember` protocolOracleBinaryImportAllowlist
      ]
    protocolOracleCanonicalPolicy =
      [ "Oracle-protocol canonical semantic bytes must use Oracle.Canonical only in the reviewed DTO and adapter owners"
      | packageKey == ProtocolOracle,
        importedModule == "Eclips.Oracle.Canonical",
        (relativePath, importedModule)
          `Set.notMember` protocolOracleCanonicalImportAllowlist
      ]
        <> [ "Oracle-protocol wire DTOs must not define an alternate cereal canonical encoding"
           | packageKey == ProtocolOracle,
             moduleWithin "Data.Serialize" importedModule
           ]
    protocolOracleSemanticPolicy =
      [ "Oracle-protocol semantic/core imports must remain in the reviewed DTO and adapter owners"
      | packageKey == ProtocolOracle,
        any (`moduleWithin` importedModule) ["Eclips.Domain", "Eclips.Oracle", "Eclips.Raft"],
        (relativePath, importedModule)
          `Set.notMember` protocolOracleSemanticImportAllowlist
      ]
    protocolOracleFramePolicy =
      [ "only the Oracle-protocol frame owner may import the common frame protocol"
      | packageKey == ProtocolOracle,
        importedModule == "Eclips.Protocol.Frame",
        (relativePath, importedModule)
          `Set.notMember` protocolOracleFrameImportAllowlist
      ]
    protocolOracleInternalPolicy =
      [ "Oracle-protocol internal imports may flow only from Frame to Codec/Types and from Codec to Types"
      | packageKey == ProtocolOracle,
        moduleWithin "Eclips.Protocol.Oracle" importedModule,
        (relativePath, importedModule)
          `Set.notMember` protocolOracleInternalImportAllowlist
      ]
    oracleRuntimeEntropyPolicy =
      [ "Oracle/Raft runtime entropy acquisition must remain in the private election-timeout source"
      | packageKey == OracleRuntime,
        importedModule == "System.Entropy",
        relativePath /= "src/Eclips/Oracle/Runtime/Internal/Entropy.hs"
      ]
    oracleRuntimeNetworkPolicy =
      [ "Oracle/Raft socket imports must remain in the reviewed socket owners"
      | packageKey == OracleRuntime,
        moduleWithin "Network" importedModule,
        not isOracleRuntimeSocketSource
      ]
    oracleRuntimeInternalSeamPolicy =
      [ "the provisional replica-registration comparison may be imported only by the Raft transport binding owner"
      | packageKey == OracleRuntime,
        importedModule == "Eclips.Oracle.Runtime.Internal.ReplicaRegistration",
        relativePath /= "src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs"
      ]
        <> [ "the Oracle/Raft runtime status projection may be imported only by its serialized owners"
           | packageKey == OracleRuntime,
             importedModule == "Eclips.Oracle.Runtime.Internal.StatusProjection",
             relativePath
               `notElem` [ "src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs",
                           "src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs"
                         ]
           ]
        <> [ "the Oracle/Raft socket-retirement seam may be imported only by its reviewed stream owners"
           | packageKey == OracleRuntime,
             importedModule == "Eclips.Oracle.Runtime.Internal.TCP.Retirement",
             relativePath
               `notElem` [ "src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs",
                           "src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs",
                           "internal/Eclips/Oracle/Runtime/Internal/WatchServe.hs"
                         ]
           ]
        <> [ "the private Oracle/Raft socket sublibrary may be imported only by its reviewed TCP consumers"
           | packageKey == OracleRuntime,
             importedModule == "Eclips.Oracle.Runtime.Internal.TCP.Socket",
             relativePath
               `notElem` [ "src/Eclips/Oracle/Runtime/TCP.hs",
                           "src/Eclips/Oracle/Runtime/ConformanceClient.hs",
                           "src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs",
                           "src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs",
                           "src/Eclips/Oracle/Runtime/Internal/TCP/Server.hs",
                           "internal/Eclips/Oracle/Runtime/Internal/ConnectedWait.hs"
                         ]
           ]
    oracleRuntimeProtocolPolicy =
      [ "Oracle wire protocol modules may be imported only by the Oracle TCP worker and authorized conformance client"
      | packageKey == OracleRuntime,
        moduleWithin "Eclips.Protocol.Oracle" importedModule,
        relativePath
          `notElem` [ "src/Eclips/Oracle/Runtime/ConformanceClient.hs",
                      "src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs",
                      "internal/Eclips/Oracle/Runtime/Internal/Types.hs",
                      "internal/Eclips/Oracle/Runtime/Internal/WatchServe.hs"
                    ]
      ]
        <> [ "Raft wire protocol modules may be imported only by the Raft TCP worker"
           | packageKey == OracleRuntime,
             moduleWithin "Eclips.Protocol.Raft" importedModule,
             relativePath /= "src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs"
           ]
    oracleRuntimeTransitionPolicy =
      [ "only the serialized Oracle owner and shared fault vocabulary may import the Oracle transition kernel"
      | packageKey == OracleRuntime,
        importedModule == "Eclips.Oracle.Transition",
        relativePath
          `notElem` [ "src/Eclips/Oracle/Runtime.hs",
                      "src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs",
                      "internal/Eclips/Oracle/Runtime/Internal/Types.hs"
                    ]
      ]
        <> [ "only the serialized Raft owner and shared fault vocabulary may import the Raft transition kernel"
           | packageKey == OracleRuntime,
             importedModule == "Eclips.Raft.Transition",
             relativePath
               `notElem` [ "src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs",
                           "internal/Eclips/Oracle/Runtime/Internal/Types.hs"
                         ]
           ]
    step15ReferencePolicy =
      [ "the private Step-15 Oracle reference may be imported only by its two retained Herald reference-model consumers"
      | importedModule == "Eclips.Oracle.Step15.Reference",
        not
          ( packageKey == HeraldCore
              && relativePath `Set.member` step15ReferenceModelHeraldImporters
          )
      ]
    disappearanceReferencePolicy =
      [ "the test-only disappearance reference must not enter a production unit"
      | importedModule == "Eclips.Oracle.Step16.DisappearanceReference"
      ]
    heraldOracleCorePolicy =
      [ "Oracle semantic imports in Herald core must remain in the reviewed client, projection, invariant, and coordinator seams"
      | packageKey == HeraldCore,
        any (`moduleWithin` importedModule) ["Eclips.Oracle", "Eclips.Raft"],
        (relativePath, importedModule)
          `Set.notMember` heraldOracleCoreImportAllowlist
      ]
    heraldRuntimeOraclePolicy =
      [ "Herald Oracle-client imports must remain in the reviewed typed adapter, owner, trace, and private TCP seams"
      | packageKey == HeraldRuntime,
        any
          (`moduleWithin` importedModule)
          [ "Eclips.Herald.OracleClient",
            "Eclips.Herald.OracleHealth",
            "Eclips.Herald.OracleProjection",
            "Eclips.Oracle",
            "Eclips.Protocol.Oracle"
          ],
        (relativePath, importedModule)
          `Set.notMember` heraldRuntimeOracleImportAllowlist
      ]
        <> [ "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner"
           | packageKey == HeraldRuntime,
             any
               (`moduleWithin` importedModule)
               [ "Eclips.Raft",
                 "Eclips.Oracle.Runtime"
               ],
             (relativePath, importedModule) `Set.notMember` heraldRuntimeOracleImportAllowlist
           ]
    heraldRuntimeAdminPolicy =
      [ "Herald administration protocol imports must remain in the reviewed typed owner and private TCP seam"
      | packageKey == HeraldRuntime,
        moduleWithin "Eclips.Protocol.Admin" importedModule,
        (relativePath, importedModule)
          `Set.notMember` heraldRuntimeAdminImportAllowlist
      ]
    heraldProtocolPolicy =
      [ "only the reviewed Herald application RPC adapters may import application-protocol modules"
      | packageKey == HeraldCore,
        moduleWithin "Eclips.Protocol.Application" importedModule,
        (relativePath, importedModule)
          `Set.notMember` heraldProtocolImportAllowlist
      ]
    heraldAdminProtocolPolicy =
      [ "only the reviewed Herald administration RPC adapters may import administration-protocol modules"
      | packageKey == HeraldCore,
        moduleWithin "Eclips.Protocol.Admin" importedModule,
        (relativePath, importedModule)
          `Set.notMember` heraldAdminProtocolImportAllowlist
      ]
    heraldPeerProtocolPolicy =
      [ "only the reviewed Herald peer RPC adapters may import peer-protocol modules"
      | packageKey == HeraldCore,
        moduleWithin "Eclips.Protocol.Peer" importedModule,
        (relativePath, importedModule)
          `Set.notMember` heraldPeerProtocolImportAllowlist
      ]
    applicationClientRecoveryConstructionPolicy =
      [ "application recovery constructors may be imported only by their public facade and state owner"
      | packageKey == ApplicationClient,
        importedModule == "Eclips.Application.Client.Recovery.Internal",
        relativePath
          `notElem` [ "src/Eclips/Application/Client/Recovery.hs",
                      "src/Eclips/Application/Client/Internal.hs"
                    ]
      ]
    ownerConstructionPolicy =
      [ "owner-only Herald correlation constructors may be imported only by their facade, owner, bootstrap, or invariant"
      | packageKey == HeraldCore,
        importedModule `Set.member` heraldOwnerConstructionModules,
        not (authorisedOwnerConstructionImport relativePath importedModule)
      ]
    kernelSeamPolicy =
      [ "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers"
      | packageKey == HeraldCore,
        importedModule `Set.member` heraldKernelSeamModules,
        not (authorisedKernelSeamImport relativePath importedModule)
      ]
    crossLeafPolicy =
      [ "cross-leaf imports must use the reviewed owner/coordinator allowlist"
      | packageKey == HeraldCore,
        importedModule `Set.member` heraldLeafModules,
        not (authorisedLeafImport relativePath importedModule)
      ]
    applicationPhysicalPurityPolicy =
      [ "the private application physical state machine must remain pure and runtime-free"
      | packageKey == ApplicationApi,
        relativePath == "internal/Eclips/Application/Runtime/Internal/Physical.hs",
        isRuntimeImport importedModule
      ]
    applicationApiSocketPolicy =
      [ "application socket imports must remain in the private connection and socket owners"
      | packageKey == ApplicationApi,
        moduleWithin "Network" importedModule,
        relativePath
          `notElem` [ "src/Eclips/Application/Runtime/Internal/Connection.hs",
                      "src/Eclips/Application/Runtime/Internal/Owner.hs",
                      "src/Eclips/Application/Runtime/Internal/Socket.hs",
                      "src/Eclips/Application/Runtime/Internal/Types.hs"
                    ]
      ]
    runtimeInternalImportPolicy =
      [ "public runtime facade modules may import only their reviewed private implementation seams"
      | packageKey == HeraldRuntime,
        "src/" `isPrefixOf` relativePath,
        moduleWithin "Eclips.Herald.Runtime.Internal" importedModule,
        importedModule `notElem` allowedPublicRuntimeInternalImports relativePath
      ]
    tcpRuntimeInternalImportPolicy =
      [ "Herald TCP modules must use the public runtime facade rather than runtime-internal modules"
      | packageKey == HeraldRuntime,
        isTcpSource,
        moduleWithin "Eclips.Herald.Runtime.Internal" importedModule
      ]
    runtimeKernelOwnerPolicy =
      [ "only the serialized runtime owner and post-terminal replay may import Herald transitions"
      | packageKey == HeraldRuntime,
        ( importedModule == "Eclips.Herald.Diagnostics"
            && relativePath /= "internal/Eclips/Herald/Runtime/Internal/Owner.hs"
        )
          || ( importedModule == "Eclips.Herald.Transition"
                 && relativePath
                   `notElem` [ "internal/Eclips/Herald/Runtime/Internal/Owner.hs",
                               "internal/Eclips/Herald/Runtime/Internal/Recording.hs"
                             ]
             )
          || ( importedModule == "Eclips.Herald.Initialization"
                 && relativePath
                   `notElem` [ "internal/Eclips/Herald/Runtime/Internal/Owner.hs",
                               "internal/Eclips/Herald/Runtime/Internal/Recording.hs",
                               "internal/Eclips/Herald/Runtime/Internal/Trace.hs"
                             ]
             )
      ]
    runtimeRecoveryVocabularyPolicy =
      [ "peer-recovery vocabulary may be imported only by the reviewed runtime configuration, owner, conformance, recording, and timer seams"
      | packageKey == HeraldRuntime,
        importedModule == "Eclips.Herald.PeerLiveness",
        relativePath
          `notElem` [ "src/Eclips/Herald/Runtime.hs",
                      "internal/Eclips/Herald/Runtime/Internal/Conformance.hs",
                      "internal/Eclips/Herald/Runtime/Internal/Owner.hs",
                      "internal/Eclips/Herald/Runtime/Internal/Recording.hs",
                      "internal/Eclips/Herald/Runtime/Internal/Types.hs"
                    ]
      ]
        <> [ "logical timer vocabulary may be imported only by the reviewed runtime owner, scheduler, and trace seams"
           | packageKey == HeraldRuntime,
             importedModule == "Eclips.Herald.Timer",
             relativePath
               `notElem` [ "internal/Eclips/Herald/Runtime/Internal/Owner.hs",
                           "internal/Eclips/Herald/Runtime/Internal/Timer.hs",
                           "internal/Eclips/Herald/Runtime/Internal/Trace.hs"
                         ]
           ]
    runtimeTimerInternalPolicy =
      [ "the runtime timer scheduler may be imported only by the serialized runtime owner"
      | packageKey == HeraldRuntime,
        importedModule == "Eclips.Herald.Runtime.Internal.Timer",
        relativePath /= "internal/Eclips/Herald/Runtime/Internal/Owner.hs"
      ]
    tcpHeartbeatInternalPolicy =
      [ "the TCP heartbeat supervisor may be imported only by the application and peer connection owners"
      | packageKey == HeraldRuntime,
        importedModule == "Eclips.Herald.Runtime.TCP.Internal.Heartbeat",
        relativePath
          `notElem` [ "tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Application.hs",
                      "tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs"
                    ]
      ]
    isTcpSource =
      "tcp/" `isPrefixOf` relativePath
        || "tcp-internal/" `isPrefixOf` relativePath
    -- Opt-in diagnostic destination and unique snapshot timestamp only. This
    -- explicit serialized runtime owner remains the sole state holder; neither
    -- the pure kernels nor the categorical WorkCounts helper acquire ambient IO.
    isOracleOwnerDiagnosticImport =
      relativePath == "src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs"
        && importedModule `elem` ["GHC.Clock", "System.Environment"]
    isOracleRuntimeSocketSource =
      "src/Eclips/Oracle/Runtime/Internal/TCP/" `isPrefixOf` relativePath
        || relativePath
          `elem` [ "socket-internal/Eclips/Oracle/Runtime/Internal/TCP/Socket.hs",
                   "internal/Eclips/Oracle/Runtime/Internal/ConnectedWait.hs",
                   "internal/Eclips/Oracle/Runtime/Internal/TCP/Retirement.hs",
                   "internal/Eclips/Oracle/Runtime/Internal/WatchServe.hs"
                 ]

runtimeImportRoots :: [String]
runtimeImportRoots =
  [ "Effectful",
    "Control.Exception",
    "Control.Concurrent",
    "Control.Monad.IO.Class",
    "Crypto.Random",
    "Data.Time",
    "GHC.Clock",
    "GHC.Conc",
    "GHC.Event",
    "GHC.IO",
    "Network",
    "System.Clock",
    "System.Directory",
    "System.Entropy",
    "System.Environment",
    "System.IO",
    "System.Process",
    "System.Random",
    "System.Timeout",
    "UnliftIO",
    "Eclips.Oracle.Runtime",
    "Eclips.Herald.Runtime"
  ]

raftCoreForbiddenImports :: [String]
raftCoreForbiddenImports =
  [ "Codec",
    "Crypto",
    "Data.Binary",
    "Data.Serialize",
    "Database"
  ]

oracleCoreForbiddenImports :: [String]
oracleCoreForbiddenImports =
  [ "Codec",
    "Data.Binary",
    "Database",
    "Eclips.Application",
    "Eclips.Herald",
    "Eclips.Oracle.Runtime",
    "Eclips.Protocol"
  ]

protocolRaftForbiddenImports :: [String]
protocolRaftForbiddenImports =
  [ "Crypto",
    "Data.Serialize",
    "Database",
    "Eclips.Application",
    "Eclips.Domain",
    "Eclips.Herald",
    "Eclips.Oracle",
    "Eclips.Protocol.Application",
    "Eclips.Protocol.Oracle",
    "Eclips.Protocol.Peer"
  ]

protocolOracleForbiddenImports :: [String]
protocolOracleForbiddenImports =
  [ "Crypto",
    "Database",
    "Eclips.Application",
    "Eclips.Herald",
    "Eclips.Oracle.Runtime",
    "Eclips.Protocol.Application",
    "Eclips.Protocol.Peer",
    "Eclips.Protocol.Raft"
  ]

oracleRuntimeForbiddenImports :: [String]
oracleRuntimeForbiddenImports =
  [ "Codec.Serialise",
    "Crypto",
    "Data.Binary",
    "Data.Serialize",
    "Data.Time",
    "Database",
    "Effectful",
    "GHC.Clock",
    "System.Clock",
    "System.Directory",
    "System.Environment",
    "System.Process",
    "System.Random",
    "System.Timeout",
    "UnliftIO",
    "Eclips.Application",
    "Eclips.Herald",
    "Eclips.Protocol.Application",
    "Eclips.Protocol.Peer"
  ]

runtimeForbiddenImportRoots :: [String]
runtimeForbiddenImportRoots =
  [ "Codec",
    "Data.Binary",
    "Data.Serialize",
    "Effectful",
    "Network",
    "System.Directory",
    "System.Entropy",
    "System.Environment",
    "System.Process",
    "System.Random",
    "Eclips.Application.Client",
    "Eclips.Protocol.Application.Codec",
    "Eclips.Protocol.Application.Frame"
  ]

isRuntimeForbiddenImport :: String -> Bool
isRuntimeForbiddenImport importedModule =
  any (`moduleWithin` importedModule) runtimeForbiddenImportRoots

allowedPublicRuntimeInternalImports :: FilePath -> [String]
allowedPublicRuntimeInternalImports = \case
  "src/Eclips/Herald/Runtime.hs" ->
    [ "Eclips.Herald.Runtime.Internal.GeneratorSeed",
      "Eclips.Herald.Runtime.Internal.Owner",
      "Eclips.Herald.Runtime.Internal.Types"
    ]
  "src/Eclips/Herald/Runtime/Connection.hs" ->
    ["Eclips.Herald.Runtime.Internal.Types"]
  "src/Eclips/Herald/Runtime/Handler.hs" ->
    ["Eclips.Herald.Runtime.Internal.Types"]
  "src/Eclips/Herald/Runtime/Ingress.hs" ->
    ["Eclips.Herald.Runtime.Internal.Types"]
  "src/Eclips/Herald/Runtime/Trace.hs" ->
    ["Eclips.Herald.Runtime.Internal.Types"]
  _ -> []

domainForbiddenImports :: [String]
domainForbiddenImports =
  [ "Codec",
    "Data.Binary",
    "Herald",
    "Application",
    "Oracle",
    "Raft",
    "Protocol",
    "Runtime",
    "Eclips.Herald",
    "Eclips.Application",
    "Eclips.Oracle",
    "Eclips.Raft",
    "Eclips.Protocol",
    "Eclips.Runtime"
  ]

codecImportRoots :: [String]
codecImportRoots =
  [ "Codec",
    "Data.Binary",
    "Data.Serialize"
  ]

applicationRpcPath :: FilePath
applicationRpcPath = "src/Eclips/Herald/Application/RPC.hs"

applicationRpcInternalPath :: FilePath
applicationRpcInternalPath =
  "internal/Eclips/Herald/Application/RPC/Internal.hs"

administrationRpcPath :: FilePath
administrationRpcPath = "src/Eclips/Herald/Administration/RPC.hs"

administrationRpcInternalPath :: FilePath
administrationRpcInternalPath =
  "internal/Eclips/Herald/Administration/RPC/Internal.hs"

peerRpcPath :: FilePath
peerRpcPath = "src/Eclips/Herald/Peer/RPC.hs"

peerRpcInternalPath :: FilePath
peerRpcInternalPath =
  "internal/Eclips/Herald/Peer/RPC/Internal.hs"

heraldProtocolImportAllowlist :: Set (FilePath, String)
heraldProtocolImportAllowlist =
  Set.fromList
    [ (applicationRpcPath, "Eclips.Protocol.Application.Types"),
      (applicationRpcInternalPath, "Eclips.Protocol.Application.Types")
    ]

heraldAdminProtocolImportAllowlist :: Set (FilePath, String)
heraldAdminProtocolImportAllowlist =
  Set.fromList
    [ (administrationRpcPath, "Eclips.Protocol.Admin.Types"),
      (administrationRpcInternalPath, "Eclips.Protocol.Admin.Types")
    ]

heraldPeerProtocolImportAllowlist :: Set (FilePath, String)
heraldPeerProtocolImportAllowlist =
  Set.fromList
    [ ("internal/Eclips/Herald/Alignment/History.hs", "Eclips.Protocol.Peer.Types"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Protocol.Peer.Types"),
      (peerRpcPath, "Eclips.Protocol.Peer.Types"),
      (peerRpcInternalPath, "Eclips.Protocol.Peer.Types")
    ]

heraldCerealImportAllowlist :: Set (FilePath, String)
heraldCerealImportAllowlist =
  Set.fromList
    [ ("internal/Eclips/Herald/Join/Bootstrap.hs", "Data.Serialize.Get"),
      ("internal/Eclips/Herald/Join/Bootstrap.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Join/History.hs", "Data.Serialize"),
      -- Only canonical framing; individual owners admit the embedded claims.
      ("internal/Eclips/Herald/Join/SourceBundle.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Alignment/History.hs", "Data.Serialize"),
      ("src/Eclips/Herald/Join.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Administration/Internal.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Alignment/Disappearance.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Alignment/Protocol.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Alignment/Plan/Identity.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Alignment/State.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Application/State.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Graph/Progress.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Graph/Progress.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/Graph/Protocol.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Graph/TerminalSource.hs", "Data.Serialize.Get"),
      ("internal/Eclips/Herald/Graph/TerminalSource.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Data.Serialize.Put"),
      -- Explicit nominal Projection base leaves, never private State instances.
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/PeerPublication.hs", "Data.Serialize"),
      -- Portable registry facts only; the private owner has no codec instance.
      ("internal/Eclips/Herald/SortRegistry/State.hs", "Data.Serialize"),
      -- Explicit portable carrier facts; no private applied-owner codec.
      ("internal/Eclips/Herald/Structural/Reconciliation.hs", "Data.Serialize"),
      ("internal/Eclips/Herald/Structural/Reconciliation.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/UseCase/ApplicationLiveness.hs", "Data.Serialize.Put"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Data.Serialize.Put")
    ]

heraldOracleCoreImportAllowlist :: Set (FilePath, String)
heraldOracleCoreImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/OracleHealth.hs", "Eclips.Raft.Configuration"),
      ("src/Eclips/Herald/OracleHealth.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Herald/Transition.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/Controlled/State.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Failure"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Failure"),
      ("internal/Eclips/Herald/Peer/Step15.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Isolation/State.hs", "Eclips.Raft.Configuration"),
      ("internal/Eclips/Herald/Isolation/State.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/OracleHealth/State.hs", "Eclips.Raft.Configuration"),
      ("internal/Eclips/Herald/OracleHealth/State.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/FailureDetection/State.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/UseCase/FailureDetection.hs", "Eclips.Oracle.Failure"),
      ("internal/Eclips/Herald/UseCase/FailureDetection.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/UseCase/FailureDetection.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Administration/RPC/Internal.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/Peer/RPC/Internal.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Administration/RPC.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Initialization.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Input.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Join.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Transition.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Administration/Internal.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Administration/RPC.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Herald/Initialization.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Herald/Administration/RPC.hs", "Eclips.Oracle.Genesis"),
      ("src/Eclips/Herald/Initialization.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/Administration/Internal.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/Administration/State.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/Discovery/State.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Genesis/Internal.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Graph/Progress.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Join/Readiness.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/Join/Seal.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Join/State.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/JoinControlTails.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/UseCase/Alignment.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Oracle.Failure"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/StructuralProgress.hs", "Eclips.Oracle.Admission"),
      ("internal/Eclips/Herald/UseCase/ControlledRemoval.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/UseCase/RegularRetirement.hs", "Eclips.Oracle.Disappearance"),
      ("src/Eclips/Herald/Join.hs", "Eclips.Oracle.Admission"),
      ("src/Eclips/Herald/Join.hs", "Eclips.Oracle.Canonical"),
      ("src/Eclips/Herald/Join.hs", "Eclips.Oracle.Failure"),
      ("internal/Eclips/Herald/Administration/State.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/Authority.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/ConfiguredProcess/Start.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/ConfiguredProcess/Start.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/ConfiguredProcess/State.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/FailureDetection/State.hs", "Eclips.Oracle.Command"),
      ("internal/Eclips/Herald/Administration/Internal.hs", "Eclips.Oracle.Receipt"),
      (administrationRpcInternalPath, "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Oracle.Identity"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Oracle.Progress"),
      ("internal/Eclips/Herald/OracleClient/Internal.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Command"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Identity"),
      ("internal/Eclips/Herald/OracleClient/Request.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Command"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Identity"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Progress"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/OracleClient/State.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Command"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Genesis"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Identity"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/OracleProjection/State.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Oracle.Receipt"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Identity"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/UseCase/FailureDetection.hs", "Eclips.Oracle.Canonical"),
      ("internal/Eclips/Herald/UseCase/FailureDetection.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Oracle.Disappearance"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/UseCase/LabelCollection.hs", "Eclips.Oracle.Label"),
      ("internal/Eclips/Herald/UseCase/PeerInput.hs", "Eclips.Oracle.Projection"),
      ("internal/Eclips/Herald/OracleProjection/Step15.hs", "Eclips.Oracle.Step15.Reference"),
      ("internal/Eclips/Herald/UseCase/Step15FailureVertical.hs", "Eclips.Oracle.Step15.Reference"),
      ("internal/Eclips/Herald/UseCase/Step15FailureVertical.hs", "Eclips.Oracle.Step15.WorkflowReference")
    ]

step15ReferenceModelHeraldImporters :: Set FilePath
step15ReferenceModelHeraldImporters =
  Set.fromList
    [ "internal/Eclips/Herald/OracleProjection/Step15.hs",
      "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs"
    ]

heraldRuntimeAdminImportAllowlist :: Set (FilePath, String)
heraldRuntimeAdminImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/Runtime/Ingress.hs", "Eclips.Protocol.Admin.Types"),
      ("internal/Eclips/Herald/Runtime/Internal/Coordination.hs", "Eclips.Protocol.Admin.Types"),
      ("internal/Eclips/Herald/Runtime/Internal/Owner.hs", "Eclips.Protocol.Admin.Types"),
      ("internal/Eclips/Herald/Runtime/Internal/Types.hs", "Eclips.Protocol.Admin.Types"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Administration.hs", "Eclips.Protocol.Admin.Frame"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Administration.hs", "Eclips.Protocol.Admin.Types")
    ]

heraldRuntimeTcpDomainImportAllowlist :: Set (FilePath, String)
heraldRuntimeTcpDomainImportAllowlist =
  Set.fromList
    [ ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Discovery.hs", "Eclips.Protocol.Frame"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Domain.Identity"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs", "Eclips.Domain.Identity"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Types.hs", "Eclips.Domain.Identity")
    ]

heraldRuntimeOracleImportAllowlist :: Set (FilePath, String)
heraldRuntimeOracleImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/Runtime/Oracle.hs", "Eclips.Herald.OracleHealth"),
      ("src/Eclips/Herald/Runtime/Ingress.hs", "Eclips.Herald.OracleHealth"),
      ("internal/Eclips/Herald/Runtime/Internal/Types.hs", "Eclips.Herald.OracleHealth"),
      ("internal/Eclips/Herald/Runtime/Internal/Owner.hs", "Eclips.Herald.OracleHealth"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs", "Eclips.Herald.OracleClient"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs", "Eclips.Herald.OracleHealth"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs", "Eclips.Protocol.Oracle.Frame"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs", "Eclips.Protocol.Oracle.Types"),
      ("src/Eclips/Herald/Runtime.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Runtime.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Herald/Runtime/Ingress.hs", "Eclips.Oracle.Voter"),
      ("src/Eclips/Herald/Runtime/Ingress.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/Runtime/Internal/Types.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Runtime/Internal/Types.hs", "Eclips.Raft.Identity"),
      ("internal/Eclips/Herald/Runtime/Internal/Owner.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Runtime/Internal/Owner.hs", "Eclips.Raft.Identity"),
      ("src/Eclips/Herald/Runtime.hs", "Eclips.Herald.OracleClient"),
      ("src/Eclips/Herald/Runtime/Ingress.hs", "Eclips.Herald.OracleClient"),
      ("src/Eclips/Herald/Runtime/Oracle.hs", "Eclips.Herald.OracleClient"),
      ("src/Eclips/Herald/Runtime/Oracle.hs", "Eclips.Oracle.Canonical"),
      ("src/Eclips/Herald/Runtime/Oracle.hs", "Eclips.Protocol.Oracle.Codec"),
      ("src/Eclips/Herald/Runtime/Oracle.hs", "Eclips.Protocol.Oracle.Types"),
      ("internal/Eclips/Herald/Runtime/Internal/Conformance.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Runtime/Internal/Conformance.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Runtime/Internal/Coordination.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Runtime/Internal/Owner.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Runtime/Internal/Recording.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Runtime/Internal/Recording.hs", "Eclips.Oracle.Voter"),
      ("internal/Eclips/Herald/Runtime/Internal/Types.hs", "Eclips.Herald.OracleClient"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Facade.hs", "Eclips.Herald.OracleClient"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Herald.OracleClient"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Canonical"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Command"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Identity"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Progress"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Projection"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Oracle.Receipt"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Protocol.Oracle.Codec"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Protocol.Oracle.Frame"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Protocol.Oracle.Types"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Types.hs", "Eclips.Herald.OracleClient"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Types.hs", "Eclips.Oracle.Identity")
    ]

heraldRuntimeTimingImportAllowlist :: Set (FilePath, String)
heraldRuntimeTimingImportAllowlist =
  Set.fromList
    [ ("tcp/Eclips/Herald/Runtime/TCP.hs", "Eclips.Public.Types.Timing"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Facade.hs", "Eclips.Public.Types.Timing"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs", "Eclips.Public.Types.Timing"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs", "Eclips.Public.Types.Timing"),
      ("tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Types.hs", "Eclips.Public.Types.Timing")
    ]

step7OwnerSourcePaths :: Set FilePath
step7OwnerSourcePaths =
  Set.fromList
    [ "internal/Eclips/Herald/Administration/State.hs",
      "internal/Eclips/Herald/Alignment/Disappearance.hs",
      "internal/Eclips/Herald/Alignment/State.hs",
      "internal/Eclips/Herald/Alignment/Loss.hs",
      "internal/Eclips/Herald/Application/Disappearance.hs",
      "internal/Eclips/Herald/Application/State.hs",
      "src/Eclips/Herald/Application/PrivateIdentity.hs",
      "internal/Eclips/Herald/Controlled/Disappearance.hs",
      "internal/Eclips/Herald/Controlled/State.hs",
      "internal/Eclips/Herald/ConfiguredProcess/State.hs",
      "internal/Eclips/Herald/Disappearance/Evidence.hs",
      "internal/Eclips/Herald/Disappearance/Evidence/Internal.hs",
      "internal/Eclips/Herald/Disappearance/OwnerEvidence.hs",
      "internal/Eclips/Herald/Disappearance/Protocol.hs",
      "internal/Eclips/Herald/Disappearance/State.hs",
      "internal/Eclips/Herald/Discovery/State.hs",
      "internal/Eclips/Herald/FailureDetection/State.hs",
      "internal/Eclips/Herald/Graph/Disappearance.hs",
      "internal/Eclips/Herald/Graph/DisappearanceReadiness.hs",
      "internal/Eclips/Herald/Graph/State.hs",
      "internal/Eclips/Herald/Graph/Progress.hs",
      "internal/Eclips/Herald/IdGenerator/State.hs",
      "internal/Eclips/Herald/Isolation/State.hs",
      "internal/Eclips/Herald/OracleHealth/State.hs",
      "internal/Eclips/Herald/OracleProjection/State.hs",
      "internal/Eclips/Herald/PeerDelivery.hs",
      "internal/Eclips/Herald/PeerDelivery/State.hs",
      "internal/Eclips/Herald/PeerLiveness/State.hs",
      "internal/Eclips/Herald/PeerStream.hs",
      "internal/Eclips/Herald/PeerStream/Disappearance.hs",
      "internal/Eclips/Herald/PeerStream/State.hs",
      "internal/Eclips/Herald/Placement/Disappearance.hs",
      "internal/Eclips/Herald/Placement/State.hs",
      "internal/Eclips/Herald/Publication/Disappearance.hs",
      "internal/Eclips/Herald/Publication/Groups.hs",
      "internal/Eclips/Herald/Publication/State.hs",
      "internal/Eclips/Herald/SortRegistry/State.hs",
      "internal/Eclips/Herald/Store/State.hs",
      "internal/Eclips/Herald/Store/Observation.hs",
      "internal/Eclips/Herald/Store/Disappearance.hs",
      "internal/Eclips/Herald/TerminalSourceHold/State.hs",
      "internal/Eclips/Herald/Label/Collection.hs",
      "internal/Eclips/Herald/LabelBarrier/State.hs",
      "internal/Eclips/Herald/Visibility/State.hs",
      "internal/Eclips/Herald/UseCase/Disappearance.hs",
      "internal/Eclips/Herald/UseCase/DisappearanceLive.hs",
      "internal/Eclips/Herald/UseCase/ControlledRemoval.hs",
      "internal/Eclips/Herald/UseCase/RegularRetirement.hs",
      "internal/Eclips/Herald/Wait/State.hs"
    ]

heraldLeafModules :: Set String
heraldLeafModules =
  Set.fromList
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Alignment.Transfer",
      "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Disappearance.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.FailureDetection.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.Isolation.State",
      "Eclips.Herald.OracleHealth.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerDelivery",
      "Eclips.Herald.PeerDelivery.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.Groups",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Store.Observation",
      "Eclips.Herald.TerminalSourceHold.State",
      "Eclips.Herald.Label.Collection",
      "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.Visibility.State",
      "Eclips.Herald.Wait.State"
    ]

liveHeraldLeafModules :: Set String
liveHeraldLeafModules =
  heraldLeafModules

heraldOwnerConstructionModules :: Set String
heraldOwnerConstructionModules =
  Set.fromList
    [ "Eclips.Herald.Administration.Internal",
      "Eclips.Herald.Application.Request.Internal",
      "Eclips.Herald.Application.Session.Internal",
      "Eclips.Herald.Discovery.Internal",
      "Eclips.Herald.Isolation.Internal",
      "Eclips.Herald.OracleClient.Internal",
      "Eclips.Herald.OracleClient.Request",
      "Eclips.Herald.PeerLiveness.Internal",
      "Eclips.Herald.Timer.Internal"
    ]

-- | P06/P10 composers import only the exact owners and semantic vocabulary
-- they inspect or prepare and commit. The alignment catalogue uses checked peer
-- codecs; replay derives historical plans against Graph and Placement. Join
-- captures/adopts that evidence and registers its intended recipients. Alignment
-- transfer reads current placement only to reconcile exact historical Store loss.
-- Canonical history transfer remains pure; runtime shells see the Join facade.
heraldJoinImportAllowlist :: Set (FilePath, String)
heraldJoinImportAllowlist =
  Set.fromList
    [ -- The paired base installs checked current owners and internal system
      -- views into a restricted observer. Historical alignment remains passive;
      -- OracleAdvance supplies only current routing and reconciliation views.
      -- Shared immutable bootstrap assembly for ordinary startup and a private
      -- joining replay origin. It owns no live protocol or request state.
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Graph.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.SortRegistry.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Store.State"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/Startup/Semantic.hs", "Eclips.Herald.Structural.Reconciliation"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.PeerLiveness.State"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.PeerStream.State"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.Alignment.History"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.Graph.TerminalSource"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.UseCase.OracleAdvance"),
      ("internal/Eclips/Herald/UseCase/ControlBase.hs", "Eclips.Herald.Join.Readiness"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Join.Readiness"),
      ("internal/Eclips/Herald/Join/Readiness.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/Join/Readiness.hs", "Eclips.Herald.UseCase.OracleAdvance"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.Application.State"),
      ("internal/Eclips/Herald/Alignment/History.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/Alignment/History.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/Alignment/History.hs", "Eclips.Herald.Peer.RPC.Internal"),
      ("internal/Eclips/Herald/Alignment/History.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Alignment.History"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Placement"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.UseCase.AlignmentHistory"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Alignment.Generation"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Alignment.History"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/UseCase/AlignmentHistory.hs", "Eclips.Herald.UseCase.Alignment"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Placement"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Alignment.History"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Herald.Graph.State"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Herald.Placement"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/Join/Base.hs", "Eclips.Herald.Store.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Graph.Protocol"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Graph.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Graph.TerminalSource"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Peer.RPC.Internal"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.PeerPublication"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Publication.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.SortRegistry.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Store.State"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.Structural.Reconciliation"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.UseCase.PeerInput"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.UseCase.Step15StructuralBase"),
      ("internal/Eclips/Herald/Join/History.hs", "Eclips.Herald.UseCase.StructuralProgress"),
      ("internal/Eclips/Herald/Join/State.hs", "Eclips.Herald.Graph.TerminalSource"),
      ("internal/Eclips/Herald/Join/Replay.hs", "Eclips.Herald.Graph.TerminalSource"),
      ("internal/Eclips/Herald/Join/Replay.hs", "Eclips.Herald.UseCase.PeerInput"),
      ("internal/Eclips/Herald/Join/Seal.hs", "Eclips.Herald.Graph.Protocol"),
      ("internal/Eclips/Herald/Join/State.hs", "Eclips.Herald.OracleClient.Request"),
      ("internal/Eclips/Herald/Join/State.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Herald.Join.State"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.Isolation.State"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/JoinControlTails.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/ControlReclamation.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/UseCase/ControlReclamation.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.UseCase.OracleAdvance"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.UseCase.StructuralCoordinator"),
      ("internal/Eclips/Herald/UseCase/Join.hs", "Eclips.Herald.UseCase.Step15StructuralBase"),
      ("internal/Eclips/Herald/UseCase/StructuralProgress.hs", "Eclips.Herald.Join.State"),
      ("internal/Eclips/Herald/UseCase/StructuralProgress.hs", "Eclips.Herald.Publication.Route"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Herald.Publication.Route"),
      ("internal/Eclips/Herald/UseCase/Disappearance.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Herald.UseCase.OracleAdvance"),
      ("internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs", "Eclips.Herald.UseCase.OracleAdvance"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/UseCase/JoinHistory.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.Isolation.State"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.UseCase.Disappearance"),
      ("src/Eclips/Herald/Initialization.hs", "Eclips.Herald.Application.State"),
      ("src/Eclips/Herald/Transition.hs", "Eclips.Herald.ProcessPreparation.State")
    ]

authorisedOwnerConstructionImport :: FilePath -> String -> Bool
authorisedOwnerConstructionImport relativePath importedModule =
  (relativePath, importedModule) `Set.member` heraldJoinImportAllowlist
    || case importedModule of
      "Eclips.Herald.Administration.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/Administration.hs",
                   administrationRpcInternalPath,
                   "internal/Eclips/Herald/Administration/State.hs",
                   "internal/Eclips/Herald/ConfiguredProcess/Start.hs",
                   "internal/Eclips/Herald/Startup/Invariant.hs",
                   "internal/Eclips/Herald/UseCase/Administration.hs"
                 ]
      "Eclips.Herald.Application.Session.Internal" ->
        relativePath
          `elem` [ "internal/Eclips/Herald/ProcessPreparation/State.hs",
                   "internal/Eclips/Herald/UseCase/ApplicationLiveness.hs",
                   "internal/Eclips/Herald/UseCase/ProcessPreparation.hs",
                   "internal/Eclips/Herald/Application/Session.hs",
                   administrationRpcInternalPath,
                   applicationRpcInternalPath,
                   "internal/Eclips/Herald/Application/Environment.hs",
                   "internal/Eclips/Herald/Application/State.hs",
                   "internal/Eclips/Herald/Bootstrap.hs",
                   "internal/Eclips/Herald/Startup/Invariant.hs",
                   "internal/Eclips/Herald/Timer/Internal.hs",
                   "internal/Eclips/Herald/UseCase/StructuralSettlement.hs"
                 ]
      "Eclips.Herald.Application.Request.Internal" ->
        relativePath
          `elem` [ "internal/Eclips/Herald/Application/Request.hs",
                   "internal/Eclips/Herald/Application/Forward.hs",
                   "internal/Eclips/Herald/Application/Environment.hs",
                   "internal/Eclips/Herald/Application/Publication.hs",
                   applicationRpcInternalPath,
                   "internal/Eclips/Herald/Application/Session/Internal.hs",
                   "internal/Eclips/Herald/Application/State.hs",
                   "internal/Eclips/Herald/Publication/State.hs",
                   "internal/Eclips/Herald/Startup/Invariant.hs",
                   "internal/Eclips/Herald/Startup/State.hs",
                   "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                   "internal/Eclips/Herald/Wait/State.hs"
                 ]
      "Eclips.Herald.Discovery.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/Discovery.hs",
                   "internal/Eclips/Herald/Discovery/State.hs",
                   "src/Eclips/Herald/Initialization.hs",
                   "internal/Eclips/Herald/Startup/Invariant.hs"
                 ]
      "Eclips.Herald.Isolation.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/Isolation.hs",
                   "internal/Eclips/Herald/Isolation/State.hs",
                   "internal/Eclips/Herald/Timer/Internal.hs"
                 ]
      "Eclips.Herald.OracleClient.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/OracleClient.hs",
                   "internal/Eclips/Herald/OracleClient/State.hs"
                 ]
      "Eclips.Herald.OracleClient.Request" ->
        relativePath
          `elem` [ "src/Eclips/Herald/OracleClient.hs",
                   "internal/Eclips/Herald/Administration/State.hs",
                   "internal/Eclips/Herald/LabelBarrier/State.hs",
                   "internal/Eclips/Herald/OracleClient/Internal.hs",
                   "internal/Eclips/Herald/OracleClient/State.hs"
                 ]
      "Eclips.Herald.PeerLiveness.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/PeerLiveness.hs",
                   "internal/Eclips/Herald/FailureDetection/State.hs",
                   "internal/Eclips/Herald/OracleClient/Request.hs",
                   "internal/Eclips/Herald/OracleClient/State.hs",
                   "internal/Eclips/Herald/PeerLiveness/State.hs",
                   "internal/Eclips/Herald/Timer/Internal.hs",
                   "internal/Eclips/Herald/UseCase/FailureDetection.hs",
                   "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs"
                 ]
      "Eclips.Herald.Timer.Internal" ->
        relativePath
          `elem` [ "src/Eclips/Herald/Timer.hs",
                   "src/Eclips/Herald/Transition.hs",
                   "internal/Eclips/Herald/Application/State.hs",
                   "internal/Eclips/Herald/FailureDetection/State.hs",
                   "internal/Eclips/Herald/Isolation/State.hs",
                   "internal/Eclips/Herald/PeerLiveness/State.hs",
                   "internal/Eclips/Herald/PeerDelivery.hs",
                   "internal/Eclips/Herald/UseCase/PeerDelivery.hs",
                   "internal/Eclips/Herald/UseCase/FailureDetection.hs",
                   "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs"
                 ]
      _ -> False

heraldKernelSeamModules :: Set String
heraldKernelSeamModules =
  Set.fromList
    [ "Eclips.Herald.ProcessPreparation.State",
      "Eclips.Herald.ProcessPreparation.Readiness",
      "Eclips.Herald.UseCase.ProcessPreparation",
      "Eclips.Herald.Application.Primordial",
      "Eclips.Herald.Administration.RPC",
      "Eclips.Herald.Administration.RPC.Internal",
      "Eclips.Herald.Alignment.Disappearance",
      "Eclips.Herald.Alignment.CutQueries",
      "Eclips.Herald.Alignment.Generation",
      "Eclips.Herald.Alignment.Plan",
      "Eclips.Herald.Alignment.Plan.Identity",
      "Eclips.Herald.Alignment.History",
      "Eclips.Herald.Alignment.Protocol",
      "Eclips.Herald.Alignment.Transfer",
      "Eclips.Herald.Application.Disappearance",
      "Eclips.Herald.Application.Environment",
      "Eclips.Herald.Application.Forward",
      "Eclips.Herald.Application.Query",
      "Eclips.Herald.Application.RPC",
      "Eclips.Herald.Application.RPC.Internal",
      "Eclips.Herald.Authority",
      "Eclips.Herald.Controlled.Disappearance",
      "Eclips.Herald.Controlled.Operate",
      "Eclips.Herald.Disappearance.Evidence",
      "Eclips.Herald.Disappearance.Gate",
      "Eclips.Herald.Disappearance.Gate",
      "Eclips.Herald.Disappearance.Evidence.Internal",
      "Eclips.Herald.Disappearance.OwnerEvidence",
      "Eclips.Herald.Disappearance.Protocol",
      "Eclips.Herald.Discovery",
      "Eclips.Herald.EffectivePublication",
      "Eclips.Herald.Graph.Disappearance",
      "Eclips.Herald.Graph.DisappearanceReadiness",
      "Eclips.Herald.Join.Readiness",
      "Eclips.Herald.UseCase.FailureDetection",
      "Eclips.Herald.OracleClient",
      "Eclips.Herald.OracleProjection.Step15",
      "Eclips.Herald.Peer.RPC",
      "Eclips.Herald.Peer.RPC.Internal",
      "Eclips.Herald.Peer.Step15",
      "Eclips.Herald.PeerDispatch",
      "Eclips.Herald.PeerDispatch.Internal",
      "Eclips.Herald.PeerDelivery",
      "Eclips.Herald.PeerDelivery.State",
      "Eclips.Herald.UseCase.PeerDelivery",
      "Eclips.Herald.PeerLiveness",
      "Eclips.Herald.PeerPayload",
      "Eclips.Herald.PeerPublication",
      "Eclips.Herald.PeerStream",
      "Eclips.Herald.PeerStream.Disappearance",
      "Eclips.Herald.Placement",
      "Eclips.Herald.Placement.Disappearance",
      "Eclips.Herald.Publication.Disappearance",
      "Eclips.Herald.Query",
      "Eclips.Herald.Store.Disappearance",
      "Eclips.Herald.Structural.Debt",
      "Eclips.Herald.Graph.Protocol",
      "Eclips.Herald.Graph.TerminalSource",
      "Eclips.Herald.Structural.Reconciliation",
      "Eclips.Herald.Timer",
      "Eclips.Herald.UseCase.ApplicationCall",
      "Eclips.Herald.UseCase.ApplicationLiveness",
      "Eclips.Herald.UseCase.ApplicationRetirement",
      "Eclips.Herald.UseCase.ControlBase",
      "Eclips.Herald.UseCase.ControlledRemoval",
      "Eclips.Herald.UseCase.Disappearance",
      "Eclips.Herald.UseCase.DisappearanceLive",
      "Eclips.Herald.UseCase.LabelPatch",
      "Eclips.Herald.UseCase.Administration",
      "Eclips.Herald.UseCase.Alignment",
      "Eclips.Herald.UseCase.AlignmentHistory",
      "Eclips.Herald.UseCase.AlignmentTransfer",
      "Eclips.Herald.UseCase.NewId",
      "Eclips.Herald.UseCase.NewEnvironment",
      "Eclips.Herald.UseCase.LabelCollection",
      "Eclips.Herald.UseCase.JoinControlTails",
      "Eclips.Herald.UseCase.ControlReclamation",
      "Eclips.Herald.UseCase.OracleAdvance",
      "Eclips.Herald.UseCase.PeerControl",
      "Eclips.Herald.UseCase.PeerInput",
      "Eclips.Herald.UseCase.PeerPlacement",
      "Eclips.Herald.UseCase.RegularRetirement",
      "Eclips.Herald.UseCase.StructuralCoordinator",
      "Eclips.Herald.UseCase.StructuralProgress",
      "Eclips.Herald.UseCase.StructuralSettlement",
      "Eclips.Herald.UseCase.Step15FailureVertical",
      "Eclips.Herald.UseCase.Step15MembershipAdvance",
      "Eclips.Herald.UseCase.Step15RetirementClosure",
      "Eclips.Herald.UseCase.Step15StructuralBase",
      "Eclips.Herald.UseCase.TerminalStructuralArchive",
      "Eclips.Herald.UseCase.TerminalStructuralStart"
    ]

-- Increment 9 promotes the retained leaf through these exact owner and
-- coordinator consumers. This does not grant owner-construction authority.
disappearanceLiveImportAllowlist :: Set (FilePath, String)
disappearanceLiveImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/Transition.hs", "Eclips.Herald.UseCase.DisappearanceLive"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Herald.Disappearance.Gate"),
      ("internal/Eclips/Herald/Disappearance/Gate.hs", "Eclips.Herald.Disappearance.OwnerEvidence"),
      ("internal/Eclips/Herald/Disappearance/Gate.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/Disappearance/Gate.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/Disappearance/Gate.hs", "Eclips.Herald.PeerPublication"),
      ("internal/Eclips/Herald/Disappearance/Gate.hs", "Eclips.Herald.PeerStream"),
      ("internal/Eclips/Herald/Disappearance/Evidence.hs", "Eclips.Herald.Publication.State"),
      ("internal/Eclips/Herald/Disappearance/Evidence.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/Peer/RPC/Internal.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/PeerPayload.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/UseCase/ApplicationCall.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/UseCase/ApplicationCall.hs", "Eclips.Herald.Disappearance.Gate"),
      ("internal/Eclips/Herald/UseCase/ApplicationCall.hs", "Eclips.Herald.UseCase.DisappearanceLive"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Alignment.Transfer"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Discovery.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Graph.DisappearanceReadiness"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.PeerPayload"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.PeerStream.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.SortRegistry.State"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.Structural.Reconciliation"),
      ("internal/Eclips/Herald/UseCase/DisappearanceLive.hs", "Eclips.Herald.UseCase.Disappearance"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.UseCase.DisappearanceLive"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Herald.UseCase.DisappearanceLive"),
      ("internal/Eclips/Herald/UseCase/PeerInput.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/UseCase/PeerInput.hs", "Eclips.Herald.Disappearance.State"),
      ("internal/Eclips/Herald/UseCase/PeerInput.hs", "Eclips.Herald.Disappearance.Gate"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Disappearance.Gate"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Disappearance.OwnerEvidence"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Disappearance.Protocol"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.Disappearance.State")
    ]

-- P03/P04 admit only these pure preparation, transfer and readiness compositions.
processPreparationImportAllowlist :: Set (FilePath, String)
processPreparationImportAllowlist =
  Set.fromList
    [ ("internal/Eclips/Herald/Administration/Internal.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Administration/RPC/Internal.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.Application.Primordial"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.Bootstrap"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/Application/Primordial/Transfer.hs", "Eclips.Herald.SortRegistry.State"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.Discovery"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.Isolation.State"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Herald.UseCase.ProcessPreparation"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Discovery"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Discovery.State"),
      ("internal/Eclips/Herald/UseCase/OracleAdvance.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/Administration.hs", "Eclips.Herald.UseCase.ProcessPreparation"),
      ("internal/Eclips/Herald/UseCase/ApplicationLiveness.hs", "Eclips.Herald.UseCase.ProcessPreparation"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Alignment.Generation"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Controlled.Operate"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Store.State"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.UseCase.Alignment"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.Application.Primordial"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.Application.State"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.Structural.Debt"),
      ("internal/Eclips/Herald/ProcessPreparation/State.hs", "Eclips.Herald.OracleClient"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Alignment.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Graph.Progress"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Placement.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Store.State"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Application.Primordial"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/ProcessPreparation/Readiness.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Application.Primordial"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Application.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Application.Environment"),
      ("internal/Eclips/Herald/UseCase/StructuralSettlement.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/ApplicationRetirement.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Controlled.Operate"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Controlled.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.IdGenerator.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.Isolation.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.OracleClient.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.ProcessPreparation.Readiness"),
      ("internal/Eclips/Herald/UseCase/ProcessPreparation.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Herald.Application.Primordial"),
      ("internal/Eclips/Herald/Startup/Invariant.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("internal/Eclips/Herald/Startup/State.hs", "Eclips.Herald.ProcessPreparation.State"),
      ("src/Eclips/Herald/Transition.hs", "Eclips.Herald.UseCase.ProcessPreparation")
    ]

-- The cumulative delivery owner remains independent of alignment semantics.
-- Only its owning product and named coordinators may inspect retained work.
peerDeliveryImportAllowlist :: Set (FilePath, String)
peerDeliveryImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/EffectBatch.hs", "Eclips.Herald.Alignment.Protocol"),
      ("src/Eclips/Herald/Transition.hs", "Eclips.Herald.UseCase.PeerDelivery"),
      ("internal/Eclips/Herald/PeerDelivery.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/PeerDelivery.hs", "Eclips.Herald.PeerDelivery.State"),
      ("internal/Eclips/Herald/Startup/State.hs", "Eclips.Herald.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/Alignment.hs", "Eclips.Herald.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/AlignmentTransfer.hs", "Eclips.Herald.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Herald.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/PeerControl.hs", "Eclips.Herald.UseCase.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.Alignment.Protocol"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.Discovery"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.Discovery.State"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.Isolation.State"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.OracleProjection.State"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.PeerDelivery"),
      ("internal/Eclips/Herald/UseCase/PeerDelivery.hs", "Eclips.Herald.PeerDelivery.State")
    ]

authorisedKernelSeamImport :: FilePath -> String -> Bool
authorisedKernelSeamImport relativePath importedModule =
  (relativePath, importedModule) `Set.member` diagnosticReadImportAllowlist
    || (relativePath, importedModule) `Set.member` peerDeliveryImportAllowlist
    || (relativePath, importedModule) `Set.member` disappearanceLiveImportAllowlist
    || (relativePath, importedModule) `Set.member` processPreparationImportAllowlist
    || (relativePath, importedModule) `Set.member` heraldJoinImportAllowlist
    || authorisedSharedKernelSeamImport relativePath importedModule

authorisedSharedKernelSeamImport :: FilePath -> String -> Bool
authorisedSharedKernelSeamImport relativePath importedModule =
  case importedModule of
    "Eclips.Herald.Administration.RPC" -> False
    "Eclips.Herald.Administration.RPC.Internal" ->
      relativePath == administrationRpcPath
    "Eclips.Herald.Alignment.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Alignment.CutQueries" ->
      relativePath `elem` ["internal/Eclips/Herald/Startup/State.hs", "internal/Eclips/Herald/UseCase/Alignment.hs"]
    "Eclips.Herald.Alignment.Plan" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/History.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Join/History.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentHistory.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs"
               ]
    "Eclips.Herald.Alignment.Plan.Identity" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/Plan.hs",
                 "internal/Eclips/Herald/Alignment/Protocol.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Join/State.hs",
                 "internal/Eclips/Herald/Peer/RPC/Internal.hs",
                 "internal/Eclips/Herald/PeerDelivery.hs"
               ]
    "Eclips.Herald.Alignment.Generation" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Plan.hs",
                 "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Alignment/Loss.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs"
               ]
    "Eclips.Herald.Alignment.Protocol" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Input.hs",
                 "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/Generation.hs",
                 "internal/Eclips/Herald/Alignment/Plan.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Alignment/Loss.hs",
                 "internal/Eclips/Herald/Alignment/Transfer.hs",
                 "internal/Eclips/Herald/Disappearance/Protocol.hs",
                 "internal/Eclips/Herald/Disappearance/State.hs",
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/PeerPayload.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs"
               ]
    "Eclips.Herald.Alignment.Transfer" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Alignment/Loss.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs"
               ]
    "Eclips.Herald.Application.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Application.Environment" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Application/State.hs",
                 "internal/Eclips/Herald/Publication/State.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/NewEnvironment.hs",
                 "internal/Eclips/Herald/UseCase/StructuralSettlement.hs"
               ]
    "Eclips.Herald.Application.Primordial" ->
      relativePath == "internal/Eclips/Herald/Application/State.hs"
    "Eclips.Herald.Application.Forward" ->
      relativePath == "internal/Eclips/Herald/UseCase/ApplicationCall.hs"
    "Eclips.Herald.Application.Query" ->
      relativePath == "internal/Eclips/Herald/UseCase/ApplicationCall.hs"
    "Eclips.Herald.Application.RPC" -> False
    "Eclips.Herald.Application.RPC.Internal" ->
      relativePath == applicationRpcPath
    "Eclips.Herald.Authority" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Controlled/Operate.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs"
               ]
    "Eclips.Herald.Controlled.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Controlled.Operate" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Application/Forward.hs",
                 "internal/Eclips/Herald/Application/Publication.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/NewId.hs",
                 "internal/Eclips/Herald/UseCase/NewEnvironment.hs"
               ]
    "Eclips.Herald.Disappearance.Evidence" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Disappearance/State.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Disappearance.hs",
                 "internal/Eclips/Herald/UseCase/DisappearanceLive.hs",
                 "internal/Eclips/Herald/UseCase/RegularRetirement.hs"
               ]
    "Eclips.Herald.Disappearance.Evidence.Internal" ->
      relativePath `elem` ["internal/Eclips/Herald/Disappearance/Evidence.hs", "internal/Eclips/Herald/Disappearance/State.hs"]
    "Eclips.Herald.Disappearance.OwnerEvidence" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Application/Disappearance.hs",
                 "internal/Eclips/Herald/Controlled/Disappearance.hs",
                 "internal/Eclips/Herald/Disappearance/Evidence.hs",
                 "internal/Eclips/Herald/Graph/Disappearance.hs",
                 "internal/Eclips/Herald/PeerStream/Disappearance.hs",
                 "internal/Eclips/Herald/Placement/Disappearance.hs",
                 "internal/Eclips/Herald/Publication/Disappearance.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Store/Disappearance.hs",
                 "internal/Eclips/Herald/Store/State.hs"
               ]
    "Eclips.Herald.Disappearance.Protocol" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Application/Disappearance.hs",
                 "internal/Eclips/Herald/Controlled/Disappearance.hs",
                 "internal/Eclips/Herald/Disappearance/Evidence.hs",
                 "internal/Eclips/Herald/Disappearance/Evidence/Internal.hs",
                 "internal/Eclips/Herald/Disappearance/OwnerEvidence.hs",
                 "internal/Eclips/Herald/Disappearance/State.hs",
                 "internal/Eclips/Herald/Graph/Disappearance.hs",
                 "internal/Eclips/Herald/Graph/DisappearanceReadiness.hs",
                 "internal/Eclips/Herald/PeerStream/Disappearance.hs",
                 "internal/Eclips/Herald/Placement/Disappearance.hs",
                 "internal/Eclips/Herald/Publication/Disappearance.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Store/Disappearance.hs",
                 "internal/Eclips/Herald/UseCase/Disappearance.hs",
                 "internal/Eclips/Herald/UseCase/DisappearanceLive.hs",
                 "internal/Eclips/Herald/UseCase/RegularRetirement.hs"
               ]
    "Eclips.Herald.Discovery" ->
      relativePath
        `elem` [ "src/Eclips/Herald/EffectBatch.hs",
                 "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/Transition.hs",
                 peerRpcPath,
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Peer/Step15.hs",
                 "internal/Eclips/Herald/FailureDetection/State.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Startup/State.hs",
                 "internal/Eclips/Herald/TerminalSourceHold/State.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/FailureDetection.hs",
                 "internal/Eclips/Herald/UseCase/LabelCollection.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.EffectivePublication" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Store/Observation.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs"
               ]
    "Eclips.Herald.Graph.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Graph.DisappearanceReadiness" -> False
    "Eclips.Herald.OracleClient" ->
      relativePath
        `elem` [ "src/Eclips/Herald/EffectBatch.hs",
                 "src/Eclips/Herald/Initialization.hs",
                 "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/FailureDetection.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs"
               ]
    "Eclips.Herald.OracleProjection.Step15" ->
      relativePath == "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs"
    "Eclips.Herald.PeerDispatch" ->
      relativePath
        `elem` [ "src/Eclips/Herald/EffectBatch.hs",
                 "src/Eclips/Herald/Input.hs",
                 peerRpcPath,
                 peerRpcInternalPath
               ]
    "Eclips.Herald.PeerDispatch.Internal" ->
      relativePath
        `elem` [ "src/Eclips/Herald/PeerDispatch.hs",
                 peerRpcInternalPath,
                 "src/Eclips/Herald/Transition.hs"
               ]
    "Eclips.Herald.PeerLiveness" ->
      relativePath == "src/Eclips/Herald/Initialization.hs"
    "Eclips.Herald.Peer.RPC" -> False
    "Eclips.Herald.Peer.RPC.Internal" ->
      relativePath == peerRpcPath
    "Eclips.Herald.Peer.Step15" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Input.hs",
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/FailureDetection/State.hs",
                 "internal/Eclips/Herald/UseCase/FailureDetection.hs",
                 "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs"
               ]
    "Eclips.Herald.PeerPayload" ->
      relativePath
        `elem` [ "src/Eclips/Herald/PeerDispatch.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Alignment/Transfer.hs",
                 "internal/Eclips/Herald/Application/Forward.hs",
                 "internal/Eclips/Herald/Application/Publication.hs",
                 "internal/Eclips/Herald/PeerStream/Disappearance.hs",
                 peerDispatchInternalPath,
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Startup/State.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.PeerPublication" ->
      relativePath
        `elem` [ "src/Eclips/Herald/EffectBatch.hs",
                 "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/PeerDispatch.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Publication/State.hs",
                 "internal/Eclips/Herald/Graph/Progress.hs",
                 "internal/Eclips/Herald/Graph/TerminalSource.hs",
                 "internal/Eclips/Herald/Alignment/Protocol.hs",
                 "internal/Eclips/Herald/Disappearance/OwnerEvidence.hs",
                 "internal/Eclips/Herald/PeerPayload.hs",
                 "internal/Eclips/Herald/PeerStream/Disappearance.hs",
                 "internal/Eclips/Herald/Publication/Disappearance.hs",
                 "internal/Eclips/Herald/Application/Forward.hs",
                 "internal/Eclips/Herald/Application/Publication.hs",
                 peerDispatchInternalPath,
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Startup/State.hs",
                 "internal/Eclips/Herald/Store/State.hs",
                 "internal/Eclips/Herald/Structural/Reconciliation.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs",
                 "internal/Eclips/Herald/UseCase/StructuralSettlement.hs",
                 "internal/Eclips/Herald/UseCase/TerminalStructuralArchive.hs"
               ]
    "Eclips.Herald.PeerStream" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/PeerDispatch.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Disappearance/Protocol.hs",
                 "internal/Eclips/Herald/Disappearance/State.hs",
                 "internal/Eclips/Herald/PeerPublication.hs",
                 peerDispatchInternalPath,
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Alignment/Transfer.hs",
                 "internal/Eclips/Herald/PeerPayload.hs",
                 "internal/Eclips/Herald/PeerStream/Disappearance.hs",
                 "internal/Eclips/Herald/PeerStream/State.hs",
                 "internal/Eclips/Herald/Publication/Disappearance.hs",
                 "internal/Eclips/Herald/Publication/Groups.hs",
                 "internal/Eclips/Herald/Publication/State.hs",
                 "internal/Eclips/Herald/Application/Publication.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/Disappearance.hs",
                 "internal/Eclips/Herald/UseCase/DisappearanceLive.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs"
               ]
    "Eclips.Herald.PeerStream.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Placement.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Publication.Disappearance" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Disappearance/Evidence.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs"
               ]
    "Eclips.Herald.Structural.Debt" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/Plan.hs",
                 "internal/Eclips/Herald/Alignment/Plan/Identity.hs",
                 "internal/Eclips/Herald/Alignment/Protocol.hs",
                 "internal/Eclips/Herald/Alignment/CutQueries.hs",
                 "internal/Eclips/Herald/Application/Primordial.hs",
                 "internal/Eclips/Herald/Alignment/Disappearance.hs",
                 "internal/Eclips/Herald/Alignment/State.hs",
                 "internal/Eclips/Herald/Alignment/Loss.hs",
                 "internal/Eclips/Herald/Alignment/Generation.hs",
                 "internal/Eclips/Herald/Authority.hs",
                 "internal/Eclips/Herald/Controlled/Operate.hs",
                 "internal/Eclips/Herald/Graph/Disappearance.hs",
                 "internal/Eclips/Herald/Graph/Progress.hs",
                 "internal/Eclips/Herald/Placement/State.hs",
                 "internal/Eclips/Herald/Peer/RPC/Internal.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Store/State.hs",
                 "internal/Eclips/Herald/Structural/Reconciliation.hs",
                 "src/Eclips/Herald/Initialization.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/PeerPlacement.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs",
                 "internal/Eclips/Herald/UseCase/LabelPatch.hs",
                 "internal/Eclips/Herald/UseCase/ControlledRemoval.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs"
               ]
    "Eclips.Herald.Graph.Protocol" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Input.hs",
                 "internal/Eclips/Herald/Graph/DisappearanceReadiness.hs",
                 "internal/Eclips/Herald/Graph/Progress.hs",
                 "internal/Eclips/Herald/Graph/TerminalSource.hs",
                 "internal/Eclips/Herald/UseCase/Step15StructuralBase.hs",
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/TerminalSourceHold/State.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.Graph.TerminalSource" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Input.hs",
                 "internal/Eclips/Herald/Graph/Progress.hs",
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Startup/State.hs",
                 "internal/Eclips/Herald/Structural/Reconciliation.hs",
                 "internal/Eclips/Herald/TerminalSourceHold/State.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/Step15StructuralBase.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralSettlement.hs",
                 "internal/Eclips/Herald/UseCase/TerminalStructuralArchive.hs",
                 "internal/Eclips/Herald/UseCase/TerminalStructuralStart.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.Structural.Reconciliation" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/CutQueries.hs",
                 "internal/Eclips/Herald/Application/Primordial.hs",
                 "src/Eclips/Herald/Initialization.hs",
                 "internal/Eclips/Herald/Authority.hs",
                 "internal/Eclips/Herald/Controlled/Operate.hs",
                 "internal/Eclips/Herald/Graph/Disappearance.hs",
                 "internal/Eclips/Herald/Graph/Progress.hs",
                 "internal/Eclips/Herald/Graph/State.hs",
                 "internal/Eclips/Herald/Placement/State.hs",
                 "internal/Eclips/Herald/Publication/State.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Store/State.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/ControlBase.hs",
                 "internal/Eclips/Herald/UseCase/LabelPatch.hs",
                 "internal/Eclips/Herald/UseCase/ControlledRemoval.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/PeerPlacement.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.Timer" ->
      relativePath
        `elem` [ "src/Eclips/Herald/EffectBatch.hs",
                 "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationLiveness.hs",
                 "internal/Eclips/Herald/UseCase/FailureDetection.hs"
               ]
    "Eclips.Herald.Placement" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Alignment/CutQueries.hs",
                 "src/Eclips/Herald/Input.hs",
                 "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Placement/State.hs",
                 "internal/Eclips/Herald/Placement/Disappearance.hs",
                 peerRpcInternalPath,
                 "internal/Eclips/Herald/Publication/Route.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/Alignment.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/PeerPlacement.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralProgress.hs"
               ]
    "Eclips.Herald.Store.Disappearance" ->
      relativePath == "internal/Eclips/Herald/Disappearance/Evidence.hs"
    "Eclips.Herald.Query" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Store/Observation.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/Wait/State.hs"
               ]
    "Eclips.Herald.UseCase.ApplicationCall" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs"
               ]
    "Eclips.Herald.UseCase.ApplicationRetirement" ->
      relativePath == "src/Eclips/Herald/Transition.hs"
    "Eclips.Herald.UseCase.ApplicationLiveness" ->
      relativePath == "src/Eclips/Herald/Transition.hs"
    "Eclips.Herald.UseCase.FailureDetection" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs"
               ]
    "Eclips.Herald.UseCase.LabelPatch" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/Startup/State.hs",
                 "internal/Eclips/Herald/UseCase/ControlBase.hs",
                 "internal/Eclips/Herald/UseCase/ControlledRemoval.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs"
               ]
    "Eclips.Herald.UseCase.Administration" ->
      relativePath == "src/Eclips/Herald/Transition.hs"
    "Eclips.Herald.UseCase.Alignment" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.AlignmentTransfer" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/ControlledRemoval.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.NewId" ->
      relativePath == "internal/Eclips/Herald/UseCase/ApplicationCall.hs"
    "Eclips.Herald.UseCase.NewEnvironment" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/StructuralSettlement.hs"
               ]
    "Eclips.Herald.UseCase.ControlledRemoval" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/Disappearance.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs"
               ]
    -- Join captures one frozen aggregate source bundle. Live receivers still
    -- replay its nested History; this seam does not adopt an owner snapshot.
    "Eclips.Herald.UseCase.ControlBase" ->
      relativePath == "internal/Eclips/Herald/UseCase/Join.hs"
    "Eclips.Herald.UseCase.RegularRetirement" ->
      relativePath == "internal/Eclips/Herald/UseCase/Disappearance.hs"
    "Eclips.Herald.UseCase.LabelCollection" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs"
               ]
    "Eclips.Herald.UseCase.JoinControlTails" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs"
               ]
    "Eclips.Herald.UseCase.ControlReclamation" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/ControlBase.hs",
                 "internal/Eclips/Herald/UseCase/Join.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs"
               ]
    "Eclips.Herald.UseCase.OracleAdvance" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs"
               ]
    "Eclips.Herald.UseCase.PeerControl" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.PeerInput" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.PeerPlacement" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs"
               ]
    "Eclips.Herald.UseCase.StructuralProgress" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.StructuralCoordinator" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/PeerInput.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs"
               ]
    "Eclips.Herald.UseCase.StructuralSettlement" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/ApplicationCall.hs",
                 "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs"
               ]
    "Eclips.Herald.UseCase.Step15FailureVertical" -> False
    "Eclips.Herald.UseCase.Disappearance" -> False
    "Eclips.Herald.UseCase.Step15MembershipAdvance" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/Step15StructuralBase.hs"
               ]
    "Eclips.Herald.UseCase.Step15RetirementClosure" -> False
    "Eclips.Herald.UseCase.Step15StructuralBase" ->
      relativePath
        `elem` [ "src/Eclips/Herald/Transition.hs",
                 "internal/Eclips/Herald/Startup/Invariant.hs",
                 "internal/Eclips/Herald/UseCase/NewEnvironment.hs",
                 "internal/Eclips/Herald/UseCase/PeerControl.hs",
                 "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs",
                 "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs",
                 "internal/Eclips/Herald/UseCase/StructuralSettlement.hs"
               ]
    "Eclips.Herald.UseCase.TerminalStructuralArchive" ->
      relativePath == "internal/Eclips/Herald/UseCase/OracleAdvance.hs"
    "Eclips.Herald.UseCase.TerminalStructuralStart" ->
      relativePath
        `elem` [ "internal/Eclips/Herald/UseCase/OracleAdvance.hs",
                 "internal/Eclips/Herald/UseCase/Step15StructuralBase.hs"
               ]
    _ -> False

authorisedLeafImport :: FilePath -> String -> Bool
authorisedLeafImport relativePath importedModule =
  (relativePath, importedModule) `Set.member` diagnosticReadImportAllowlist
    || (relativePath, importedModule) `Set.member` peerDeliveryImportAllowlist
    || (relativePath, importedModule) `Set.member` disappearanceLiveImportAllowlist
    || (relativePath, importedModule) `Set.member` processPreparationImportAllowlist
    || (relativePath, importedModule) `Set.member` heraldJoinImportAllowlist
    || importedModule `elem` allowedLeafImports relativePath

-- The diagnostic facade only reads an owner-local snapshot; its public API
-- exports finite counters, never leaf state or mutation operations.
diagnosticReadImportAllowlist :: Set.Set (FilePath, String)
diagnosticReadImportAllowlist =
  Set.fromList
    [ ("src/Eclips/Herald/Diagnostics.hs", imported)
    | imported <- ["Eclips.Herald.Alignment.Plan", "Eclips.Herald.Alignment.State", "Eclips.Herald.Alignment.Transfer", "Eclips.Herald.Discovery.State", "Eclips.Herald.EffectBatch", "Eclips.Herald.Graph.Progress", "Eclips.Herald.Internal.Diagnostics", "Eclips.Herald.Publication.Groups", "Eclips.Herald.Publication.State", "Eclips.Herald.Startup.State", "Eclips.Herald.UseCase.LabelPatch"]
    ]
    <> Set.fromList
      [ ("internal/Eclips/Herald/Internal/Diagnostics.hs", imported)
      | imported <- ["Eclips.Herald.Alignment.Generation", "Eclips.Herald.Alignment.Plan", "Eclips.Herald.Discovery", "Eclips.Herald.EffectBatch", "Eclips.Herald.Graph.Protocol", "Eclips.Herald.Input"]
      ]

allowedLeafImports :: FilePath -> [String]
allowedLeafImports = \case
  -- Capture immutable observed definitions, including retained retirements,
  -- for portable carrier admission; no Registry mutation or coordination.
  "internal/Eclips/Herald/Structural/Reconciliation.hs" ->
    ["Eclips.Herald.SortRegistry.State"]
  -- The client reads a checked shared base and its immutable exact archive;
  -- this grants neither projection mutation nor transfer of donor ownership.
  "internal/Eclips/Herald/OracleClient/State.hs" ->
    ["Eclips.Herald.OracleProjection.State"]
  -- Reconstruct current portable control facts from checked semantic evidence;
  -- this grants no access to donor-local possessions or projection mutation.
  "internal/Eclips/Herald/Controlled/State.hs" ->
    ["Eclips.Herald.OracleProjection.State"]
  "internal/Eclips/Herald/UseCase/ControlBase.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.FailureDetection.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.Isolation.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/Alignment/CutQueries.hs" ->
    ["Eclips.Herald.Graph.Progress", "Eclips.Herald.Placement.State"]
  "internal/Eclips/Herald/Isolation/State.hs" -> ["Eclips.Herald.OracleHealth.State"]
  "internal/Eclips/Herald/Alignment/Disappearance.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Alignment.Transfer"
    ]
  "internal/Eclips/Herald/Application/Disappearance.hs" ->
    ["Eclips.Herald.Application.State"]
  "internal/Eclips/Herald/Controlled/Disappearance.hs" ->
    ["Eclips.Herald.Controlled.State"]
  "internal/Eclips/Herald/Disappearance/Protocol.hs" ->
    ["Eclips.Herald.PeerStream.State"]
  "internal/Eclips/Herald/Disappearance/State.hs" ->
    ["Eclips.Herald.PeerStream.State"]
  "internal/Eclips/Herald/Disappearance/Evidence.hs" ->
    [ "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Startup.State"
    ]
  "internal/Eclips/Herald/Graph/Disappearance.hs" ->
    ["Eclips.Herald.Graph.State"]
  "internal/Eclips/Herald/Graph/DisappearanceReadiness.hs" ->
    [ "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State"
    ]
  "internal/Eclips/Herald/PeerStream/Disappearance.hs" ->
    ["Eclips.Herald.PeerStream.State"]
  "internal/Eclips/Herald/Placement/Disappearance.hs" ->
    ["Eclips.Herald.Placement.State"]
  "internal/Eclips/Herald/Publication/State.hs" ->
    ["Eclips.Herald.Publication.Groups"]
  "internal/Eclips/Herald/Publication/Disappearance.hs" ->
    ["Eclips.Herald.Publication.State"]
  "internal/Eclips/Herald/Store/Disappearance.hs" ->
    ["Eclips.Herald.Store.State"]
  "internal/Eclips/Herald/Alignment/State.hs" ->
    ["Eclips.Herald.Alignment.Transfer"]
  "internal/Eclips/Herald/Alignment/Loss.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Alignment.Transfer"
    ]
  "internal/Eclips/Herald/EffectivePublication.hs" ->
    ["Eclips.Herald.Controlled.State"]
  "internal/Eclips/Herald/Graph/Progress.hs" ->
    ["Eclips.Herald.Graph.State"]
  "internal/Eclips/Herald/Administration/State.hs" ->
    ["Eclips.Herald.ConfiguredProcess.State"]
  "internal/Eclips/Herald/LabelBarrier/State.hs" ->
    ["Eclips.Herald.Label.Collection"]
  "internal/Eclips/Herald/Application/Query.hs" ->
    ["Eclips.Herald.Application.State"]
  "internal/Eclips/Herald/Application/Primordial.hs" ->
    [ "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress"
    ]
  "internal/Eclips/Herald/Application/State.hs" ->
    [ "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Publication.Groups"
    ]
  "internal/Eclips/Herald/Authority.hs" ->
    [ "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State"
    ]
  "internal/Eclips/Herald/Application/Forward.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/Application/Publication.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/Controlled/Operate.hs" ->
    [ "Eclips.Herald.Graph.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress"
    ]
  "internal/Eclips/Herald/Bootstrap.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/ConfiguredProcess/Start.hs" ->
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.IdGenerator.State"
    ]
  "internal/Eclips/Herald/Publication/Route.hs" ->
    [ "Eclips.Herald.Graph.State",
      "Eclips.Herald.Placement",
      "Eclips.Herald.Placement.State"
    ]
  "src/Eclips/Herald/Initialization.hs" ->
    [ "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.FailureDetection.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.Isolation.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/Startup/Invariant.hs" -> Set.toList liveHeraldLeafModules
  "internal/Eclips/Herald/Startup/State.hs" -> Set.toList liveHeraldLeafModules
  "src/Eclips/Herald/Transition.hs" ->
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Isolation.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.TerminalSourceHold.State",
      "Eclips.Herald.Wait.State"
    ]
  "src/Eclips/Herald/OracleProjection.hs" ->
    ["Eclips.Herald.OracleProjection.State"]
  "internal/Eclips/Herald/PeerPayload.hs" ->
    [ "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.PeerStream.State"
    ]
  "internal/Eclips/Herald/Peer/RPC/Internal.hs" ->
    ["Eclips.Herald.LabelBarrier.State"]
  "internal/Eclips/Herald/UseCase/ApplicationCall.hs" ->
    [ "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.Isolation.State",
      "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.Groups",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.Observation",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/ApplicationRetirement.hs" ->
    ["Eclips.Herald.Application.State", "Eclips.Herald.ProcessPreparation.State"]
  "internal/Eclips/Herald/UseCase/ApplicationLiveness.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/Disappearance.hs" ->
    [ "Eclips.Herald.Disappearance.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Startup.State"
    ]
  "internal/Eclips/Herald/UseCase/ControlledRemoval.hs" ->
    [ "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Disappearance.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Startup.State",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/RegularRetirement.hs" ->
    [ "Eclips.Herald.Disappearance.State",
      "Eclips.Herald.Genesis.Internal",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Startup.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/UseCase/FailureDetection.hs" ->
    [ "Eclips.Herald.Discovery.State",
      "Eclips.Herald.FailureDetection.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State"
    ]
  "internal/Eclips/Herald/Store/Observation.hs" ->
    ["Eclips.Herald.Store.State"]
  "internal/Eclips/Herald/UseCase/LabelPatch.hs" ->
    [ "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/UseCase/Administration.hs" ->
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.OracleClient.State"
    ]
  "internal/Eclips/Herald/UseCase/Alignment.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Alignment.Transfer",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State"
    ]
  "internal/Eclips/Herald/UseCase/AlignmentTransfer.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Alignment.Transfer",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.Observation",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/NewId.hs" ->
    [ "Eclips.Herald.Application.PrivateIdentity",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.SortRegistry.State"
    ]
  "internal/Eclips/Herald/UseCase/NewEnvironment.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State"
    ]
  "internal/Eclips/Herald/UseCase/LabelCollection.hs" ->
    [ "Eclips.Herald.Label.Collection",
      "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State"
    ]
  "internal/Eclips/Herald/UseCase/OracleAdvance.hs" ->
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.Groups",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Visibility.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/PeerControl.hs" ->
    [ "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.TerminalSourceHold.State"
    ]
  "internal/Eclips/Herald/UseCase/PeerInput.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.LabelBarrier.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Store.Observation",
      "Eclips.Herald.Store.State",
      "Eclips.Herald.Wait.State"
    ]
  "internal/Eclips/Herald/UseCase/PeerPlacement.hs" ->
    [ "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State"
    ]
  "internal/Eclips/Herald/UseCase/Step15FailureVertical.hs" ->
    [ "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State"
    ]
  "internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs" ->
    [ "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleClient.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerLiveness.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State"
    ]
  "internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs" ->
    [ "Eclips.Herald.Alignment.Loss",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.Placement.State"
    ]
  "internal/Eclips/Herald/UseCase/Step15StructuralBase.hs" ->
    [ "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State"
    ]
  "internal/Eclips/Herald/UseCase/TerminalStructuralArchive.hs" ->
    ["Eclips.Herald.Publication.State"]
  "internal/Eclips/Herald/UseCase/TerminalStructuralStart.hs" ->
    [ "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.OracleProjection.State"
    ]
  "internal/Eclips/Herald/UseCase/StructuralProgress.hs" ->
    [ "Eclips.Herald.Alignment.State",
      "Eclips.Herald.Application.Request.Internal",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.IdGenerator.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Startup.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/UseCase/StructuralCoordinator.hs" ->
    [ "Eclips.Herald.Application.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Discovery.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Graph.State",
      "Eclips.Herald.OracleProjection.State",
      "Eclips.Herald.PeerStream.State",
      "Eclips.Herald.Placement.State",
      "Eclips.Herald.SortRegistry.State",
      "Eclips.Herald.Startup.State",
      "Eclips.Herald.Store.State"
    ]
  "internal/Eclips/Herald/UseCase/StructuralSettlement.hs" ->
    [ "Eclips.Herald.Administration.State",
      "Eclips.Herald.Application.Environment",
      "Eclips.Herald.Application.State",
      "Eclips.Herald.ConfiguredProcess.State",
      "Eclips.Herald.Controlled.State",
      "Eclips.Herald.Graph.Progress",
      "Eclips.Herald.Publication.State",
      "Eclips.Herald.Startup.State",
      "Eclips.Herald.Store.State"
    ]
  _ -> []

isRuntimeImport :: String -> Bool
isRuntimeImport importedModule = any (`moduleWithin` importedModule) runtimeImportRoots

isEclipsImport :: String -> Bool
isEclipsImport = moduleWithin "Eclips"

moduleWithin :: String -> String -> Bool
moduleWithin root candidate = candidate == root || (root <> ".") `isPrefixOf` candidate

slashPath :: FilePath -> FilePath
slashPath = fmap (\character -> if character == pathSeparator then '/' else character)

readBytes :: FilePath -> IO (Either String BS.ByteString)
readBytes path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure (either (Left . ((path <> ": ") <>) . show) Right result)
