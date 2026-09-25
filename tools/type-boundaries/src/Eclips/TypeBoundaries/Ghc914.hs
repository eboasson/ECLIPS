module Eclips.TypeBoundaries.Ghc914
  ( CheckerConfig (..),
    CheckerMode (..),
    diagnosticSupportsRejection,
    extractCppSelectors,
    prepareProbeSource,
    runGhcChecks,
  )
where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as ByteString
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (group, isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe)
import GHC
  ( Ghc,
    LoadHowMuch (LoadAllTargets),
    SuccessFlag (Succeeded),
    TyThing (ACoAxiom, AConLike, ATyCon, AnId),
    getModuleInfo,
    getSession,
    getSessionDynFlags,
    guessTarget,
    load,
    lookupName,
    parseDynamicFlags,
    runGhc,
    setSessionDynFlags,
    setTargets,
  )
import GHC.Core.ConLike (ConLike (PatSynCon, RealDataCon))
import GHC.Core.TyCon (isClassTyCon)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Env (hsc_unit_env)
import GHC.Driver.Monad (pushLogHookM)
import GHC.Driver.Session (DynFlags)
import GHC.Driver.Session.Inspect (minf_exports)
import GHC.Types.Avail
  ( AvailInfo (Avail, AvailTC),
    availNames,
  )
import GHC.Types.Error
  ( MessageClass (MCDiagnostic, MCFatal),
    Severity (SevError),
  )
import GHC.Types.Id (isClassOpId_maybe, isRecordSelector)
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence
  ( isDataConNameSpace,
    isFieldNameSpace,
    isTcClsNameSpace,
    isTvNameSpace,
    isVarNameSpace,
    occNameSpace,
    occNameString,
  )
import GHC.Types.SrcLoc (noLoc, unLoc)
import GHC.Unit.Database
  ( DbUnitInfo,
    readPackageDbForGhc,
    unitComponentName,
    unitExposedModules,
    unitId,
    unitPackageName,
  )
import GHC.Unit.Env (ue_homeUnitState)
import GHC.Unit.Info (UnitInfo, mkUnit, unPackageName, unitPackageNameString)
import GHC.Unit.State (UnitState, listUnitInfo, lookupUnit)
import GHC.Unit.Types
  ( Module,
    mkModule,
    moduleName,
    moduleUnit,
    unitIdString,
    unitString,
  )
import GHC.Utils.Logger (getLogger, log_default_user_context)
import GHC.Utils.Outputable (ppr, renderWithContext, showSDocUnsafe)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))

import Eclips.TypeBoundaries.Spec
  ( LogicalUnit (..),
    ProbeExpectation (..),
    ProbeSpec (..),
    RejectionClass (..),
    cabalPrivateProbeSelectors,
    expectedNegativeProbeCount,
    expectedPositiveProbeCount,
    fixtureRelativeRoot,
    negativeProbes,
    positiveProbes,
    publicUnits,
  )
import Eclips.TypeBoundaries.Surface (SurfaceEntry (..))

data CheckerMode
  = CheckSurface
  | WriteSurface FilePath
  deriving stock (Eq, Show)

data CheckerConfig = CheckerConfig
  { projectRoot :: FilePath,
    ghcLibdir :: FilePath,
    packageDatabase :: FilePath,
    dependencyPackageDatabases :: [FilePath],
    scratchDirectory :: FilePath,
    expectedSurfacePath :: FilePath,
    checkerMode :: CheckerMode
  }
  deriving stock (Eq, Show)

data ResolvedUnit = ResolvedUnit
  { logicalUnit :: LogicalUnit,
    unitInfo :: UnitInfo
  }

data DiscoveredUnit = DiscoveredUnit
  { discoveredLogicalUnit :: LogicalUnit,
    discoveredUnitId :: String
  }

data ProbeResult = ProbeResult
  { resultSpec :: ProbeSpec,
    resultSuccess :: Bool,
    resultDiagnostics :: [String]
  }

runGhcChecks :: CheckerConfig -> IO (Either [String] ([String], [SurfaceEntry]))
runGhcChecks config@CheckerConfig {ghcLibdir, packageDatabase} = do
  diagnosticsRef <- newIORef []
  selectorViolations <- validateFixtureSelectorCoverage config
  if not (null selectorViolations)
    then pure (Left selectorViolations)
    else do
      databaseUnits <- readPackageDbForGhc (packageDatabase </> "package.cache")
      case traverse (discoverUnit databaseUnits) publicUnits of
        Left violations -> pure (Left violations)
        Right discoveredUnits ->
          runGhc (Just ghcLibdir) $ do
            installDiagnosticCapture diagnosticsRef
            setupResult <- configurePackageDatabase config discoveredUnits
            case setupResult of
              Left violations -> pure (Left violations)
              Right resolvedUnits -> do
                surfaceResult <- extractSurface resolvedUnits
                probeViolations <- runAllProbes config diagnosticsRef
                pure $ case surfaceResult of
                  Left surfaceViolations -> Left (surfaceViolations <> probeViolations)
                  Right surface -> Right (probeViolations, surface)

validateFixtureSelectorCoverage :: CheckerConfig -> IO [String]
validateFixtureSelectorCoverage config = do
  let fixtureDirectory = projectRoot config </> fixtureRelativeRoot
  directoryExists <- doesDirectoryExist fixtureDirectory
  if not directoryExists
    then pure ["missing boundary fixture directory: " <> fixtureDirectory]
    else do
      fixtureSources <- sort . filter ((== ".hs") . takeExtension) <$> listDirectory fixtureDirectory
      selectorViolations <-
        fmap concat . forM fixtureNames $ \fixture -> do
          let path = fixtureDirectory </> fixture
              expected = sort [selector | (candidate, selector) <- reviewedSelectors, candidate == fixture]
          exists <- doesFileExist path
          if not exists
            then pure ["missing boundary fixture source: " <> path]
            else do
              actual <- extractCppSelectors <$> readFile path
              pure
                [ "CPP selector ledger differs for "
                    <> fixture
                    <> ": expected "
                    <> show expected
                    <> ", found "
                    <> show actual
                | actual /= expected
                ]
      pure
        ( [ "boundary fixture ledger differs: expected "
              <> show fixtureNames
              <> ", found "
              <> show fixtureSources
          | fixtureSources /= fixtureNames
          ]
            <> selectorViolations
        )
  where
    reviewedSelectors =
      [ (fixtureName, selector)
      | ProbeSpec {fixtureName, cppSelector = Just selector} <- negativeProbes
      ]
        <> cabalPrivateProbeSelectors
    fixtureNames =
      sort . nub
        $ fmap fixtureName (negativeProbes <> positiveProbes)
          <> fmap fst cabalPrivateProbeSelectors

extractCppSelectors :: String -> [String]
extractCppSelectors = sort . nub . concatMap selectorsFromDirective . lines

selectorsFromDirective :: String -> [String]
selectorsFromDirective line = case words (dropWhile isSpace line) of
  "#ifdef" : selector : _ -> [takeWhile isMacroCharacter selector]
  "#ifndef" : selector : _ -> [takeWhile isMacroCharacter selector]
  directive : condition
    | directive `elem` ["#if", "#elif"] -> conditionSelectors (unwords condition)
  "#" : directive : condition
    | directive `elem` ["if", "elif"] -> conditionSelectors (unwords condition)
  "#" : directive : selector : _
    | directive `elem` ["ifdef", "ifndef"] -> [takeWhile isMacroCharacter selector]
  _ -> []

conditionSelectors :: String -> [String]
conditionSelectors condition =
  case filter isSelectorToken tokens of
    [] -> ["<conditional-without-selector>"]
    selectors -> selectors
  where
    tokens = words [if isMacroCharacter character then character else ' ' | character <- condition]
    isSelectorToken token = token /= "defined" && any (not . (`elem` ['0' .. '9'])) token

isMacroCharacter :: Char -> Bool
isMacroCharacter character = isAlphaNum character || character == '_'

discoverUnit :: [DbUnitInfo] -> LogicalUnit -> Either [String] DiscoveredUnit
discoverUnit databaseUnits wanted = case filter (matchesDatabaseUnit wanted) databaseUnits of
  [found] ->
    Right
      DiscoveredUnit
        { discoveredLogicalUnit = wanted,
          discoveredUnitId = ByteString.unpack (unitId found)
        }
  [] -> Left ["missing reviewed public unit in package database: " <> renderLogicalUnit wanted]
  found ->
    Left
      [ "ambiguous reviewed public unit in package database "
          <> renderLogicalUnit wanted
          <> ": "
          <> show (fmap (ByteString.unpack . unitId) found)
      ]

matchesDatabaseUnit :: LogicalUnit -> DbUnitInfo -> Bool
matchesDatabaseUnit LogicalUnit {packageName, componentName} info =
  ByteString.unpack (unitPackageName info) == packageName
    && fmap ByteString.unpack (unitComponentName info) == componentName

configurePackageDatabase :: CheckerConfig -> [DiscoveredUnit] -> Ghc (Either [String] [ResolvedUnit])
configurePackageDatabase CheckerConfig {packageDatabase, dependencyPackageDatabases, scratchDirectory} discoveredUnits = do
  initialFlags <- getSessionDynFlags
  let publicUnitIds = fmap discoveredUnitId discoveredUnits
      ordinaryPackages = ["base", "binary", "bytestring", "containers", "text"]
      flags =
        ["-clear-package-db", "-global-package-db"]
          <> concatMap (\database -> ["-package-db", database]) dependencyPackageDatabases
          <> [ "-package-db",
               packageDatabase,
               "-hide-all-packages",
               "-fno-code",
               "-fno-diagnostics-show-caret",
               "-fforce-recomp",
               "-XGHC2024",
               "-XCPP",
               "-Wall",
               "-Werror=incomplete-patterns",
               "-Werror=missing-fields",
               "-Wno-missing-signatures",
               "-Wno-unused-imports",
               "-Wno-unused-top-binds",
               "-i",
               "-i" <> scratchDirectory
             ]
          <> concatMap (\package -> ["-package", package]) ordinaryPackages
          <> concatMap (\identifier -> ["-package-id", identifier]) publicUnitIds
  flagsResult <- parseFlags initialFlags flags
  case flagsResult of
    Left violations -> pure (Left violations)
    Right configuredFlags -> do
      _ <- setSessionDynFlags configuredFlags
      state <- currentUnitState
      pure (traverse (resolveDiscoveredUnit state) discoveredUnits)

resolveDiscoveredUnit :: UnitState -> DiscoveredUnit -> Either [String] ResolvedUnit
resolveDiscoveredUnit state DiscoveredUnit {discoveredLogicalUnit, discoveredUnitId} =
  case filter ((== discoveredUnitId) . unitIdString . unitId) (listUnitInfo state) of
    [found] -> Right ResolvedUnit {logicalUnit = discoveredLogicalUnit, unitInfo = found}
    [] -> Left ["configured public unit is unavailable: " <> discoveredUnitId]
    _ -> Left ["configured public unit is ambiguous: " <> discoveredUnitId]

parseFlags :: DynFlags -> [String] -> Ghc (Either [String] DynFlags)
parseFlags flags arguments = do
  logger <- getLogger
  (parsed, leftovers, warnings) <- parseDynamicFlags logger flags (fmap noLoc arguments)
  let leftoverArguments = fmap unLoc leftovers
      warningText = showSDocUnsafe (ppr warnings)
      violations =
        ["unrecognized GHC checker flags: " <> show leftoverArguments | not (null leftoverArguments)]
          <> ["GHC checker flag warnings: " <> warningText | not (null warnings)]
  pure (if null violations then Right parsed else Left violations)

currentUnitState :: Ghc UnitState
currentUnitState = ue_homeUnitState . hsc_unit_env <$> getSession

normalizedComponent :: UnitInfo -> Maybe String
normalizedComponent info
  | "-inplace" `isSuffixOf` identifier = Nothing
  | otherwise = case unitComponentName info of
      Nothing -> componentFromUnitId info
      Just component ->
        let raw = unpackFS (unPackageName component)
            package = unitPackageNameString info
            cabalPrefix = "z-" <> package <> "-z-"
         in Just (fromMaybe raw (stripPrefix cabalPrefix raw))
  where
    identifier = unitIdString (unitId info)

componentFromUnitId :: UnitInfo -> Maybe String
componentFromUnitId info =
  stripPrepared . snd <$> breakOn "-inplace-" (unitIdString (unitId info))
  where
    stripPrepared component = fromMaybe component (stripSuffix "-prepared" component)

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix value
  | suffix `isSuffixOf` value = Just (take (length value - length suffix) value)
  | otherwise = Nothing

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
  | prefix `isPrefixOf` value = Just (drop (length prefix) value)
  | otherwise = Nothing

extractSurface :: [ResolvedUnit] -> Ghc (Either [String] [SurfaceEntry])
extractSurface resolvedUnits = do
  state <- currentUnitState
  results <- concat <$> traverse (surfaceForUnit state) resolvedUnits
  pure $ case partitionEithers results of
    ([], entries) -> Right entries
    (violations, _) -> Left violations

surfaceForUnit :: UnitState -> ResolvedUnit -> Ghc [Either String SurfaceEntry]
surfaceForUnit state ResolvedUnit {logicalUnit, unitInfo} =
  fmap concat . forM (unitExposedModules unitInfo) $ \(publicName, reexportedModule) -> do
    let publicNameText = moduleNameString publicName
        definingModule = fromMaybe (mkModule (mkUnit unitInfo) publicName) reexportedModule
        moduleMarker =
          SurfaceEntry
            { publicUnit = renderLogicalUnit logicalUnit,
              publicModule = publicNameText,
              exportNamespace = "module",
              exportOccurrence = "<module>",
              exportParent = "<module>",
              definingUnit = logicalNameForModule state definingModule,
              definingModule = moduleNameString (moduleName definingModule)
            }
    moduleInfo <- getModuleInfo definingModule
    case moduleInfo of
      Nothing ->
        pure
          [ Left
              ( "unable to load semantic exports for "
                  <> renderLogicalUnit logicalUnit
                  <> ":"
                  <> publicNameText
              )
          ]
      Just info -> do
        exports <- fmap concat . forM (minf_exports info) $ \availability -> do
          parent <- exportParentFor availability
          forM (availNames availability) $ \name -> do
            thing <- lookupName name
            pure
              ( Right
                  SurfaceEntry
                    { publicUnit = renderLogicalUnit logicalUnit,
                      publicModule = publicNameText,
                      exportNamespace = classifyName thing name,
                      exportOccurrence = occNameString (nameOccName name),
                      exportParent = parent,
                      definingUnit = maybe "<internal>" (logicalNameForModule state) (nameModule_maybe name),
                      definingModule = maybe "<internal>" (moduleNameString . moduleName) (nameModule_maybe name)
                    }
              )
        pure (Right moduleMarker : exports)

exportParentFor :: AvailInfo -> Ghc String
exportParentFor (Avail _) = pure "<none>"
exportParentFor (AvailTC parent _) = do
  thing <- lookupName parent
  pure (classifyName thing parent <> ":" <> occNameString (nameOccName parent))

logicalNameForModule :: UnitState -> Module -> String
logicalNameForModule state module' = case lookupUnit state (moduleUnit module') of
  Nothing -> unitString (moduleUnit module')
  Just info ->
    renderLogicalUnit
      LogicalUnit
        { packageName = unitPackageNameString info,
          componentName = normalizedComponent info
        }

classifyName :: Maybe TyThing -> Name -> String
classifyName thing name = case thing of
  Just (AConLike (RealDataCon _)) -> "constructor"
  Just (AConLike (PatSynCon _)) -> "pattern"
  Just (AnId identifier)
    | isRecordSelector identifier -> "field"
    | Just _ <- isClassOpId_maybe identifier -> "method"
    | otherwise -> "value"
  Just (ATyCon tyCon)
    | isClassTyCon tyCon -> "class"
    | otherwise -> "type"
  Just (ACoAxiom _) -> "axiom"
  Nothing
    | isFieldNameSpace namespace -> "field"
    | isDataConNameSpace namespace -> "constructor"
    | isTcClsNameSpace namespace -> "type"
    | isTvNameSpace namespace -> "type-variable"
    | isVarNameSpace namespace -> "value"
    | otherwise -> "unknown"
  where
    namespace = occNameSpace (nameOccName name)

runAllProbes :: CheckerConfig -> IORef [String] -> Ghc [String]
runAllProbes config diagnosticsRef = do
  let probeIds = fmap probeId (negativeProbes <> positiveProbes)
      countViolations =
        [ "negative probe ledger count differs: expected "
            <> show expectedNegativeProbeCount
            <> ", found "
            <> show (length negativeProbes)
        | length negativeProbes /= expectedNegativeProbeCount
        ]
          <> [ "positive probe ledger count differs: expected "
                 <> show expectedPositiveProbeCount
                 <> ", found "
                 <> show (length positiveProbes)
             | length positiveProbes /= expectedPositiveProbeCount
             ]
          <> ["probe IDs must not be empty" | any null probeIds]
          <> ["duplicate probe IDs: " <> show (duplicates probeIds) | not (null (duplicates probeIds))]
          <> ["duplicate reviewed public units: " <> show (duplicates publicUnits) | not (null (duplicates publicUnits))]
  if not (null countViolations)
    then pure countViolations
    else do
      liftIO (createDirectoryIfMissing True (scratchDirectory config))
      results <-
        forM (zip [1 :: Int ..] (negativeProbes <> positiveProbes)) $ \(index, spec) ->
          runProbe config diagnosticsRef index spec
      pure (concatMap validateProbeResult results)

runProbe :: CheckerConfig -> IORef [String] -> Int -> ProbeSpec -> Ghc ProbeResult
runProbe config diagnosticsRef index spec@ProbeSpec {fixtureName, cppSelector} = do
  let fixturePath = projectRoot config </> fixtureRelativeRoot </> fixtureName
      generatedModule = "EclipsBoundaryProbe" <> show index
      generatedPath = scratchDirectory config </> generatedModule <> ".hs"
  exists <- liftIO (doesFileExist fixturePath)
  if not exists
    then
      pure
        ProbeResult
          { resultSpec = spec,
            resultSuccess = False,
            resultDiagnostics = ["missing fixture source: " <> fixturePath]
          }
    else do
      original <- liftIO (readFile fixturePath)
      case prepareProbeSource generatedModule fixtureName cppSelector original of
        Left message ->
          pure
            ProbeResult
              { resultSpec = spec,
                resultSuccess = False,
                resultDiagnostics = [message]
              }
        Right generated -> do
          liftIO (writeFile generatedPath generated)
          liftIO (writeIORef diagnosticsRef [])
          target <- guessTarget generatedPath Nothing Nothing
          setTargets [target]
          outcome <- load LoadAllTargets
          diagnostics <- liftIO (readIORef diagnosticsRef)
          setTargets []
          pure
            ProbeResult
              { resultSpec = spec,
                resultSuccess = case outcome of
                  Succeeded -> True
                  _ -> False,
                resultDiagnostics = diagnostics
              }

duplicates :: (Ord value) => [value] -> [value]
duplicates values = [value | value : _ : _ <- group (sort values)]

validateProbeResult :: ProbeResult -> [String]
validateProbeResult ProbeResult {resultSpec = ProbeSpec {probeId, expectation}, resultSuccess, resultDiagnostics} =
  case expectation of
    MustSucceed
      | resultSuccess -> []
      | otherwise ->
          [ "allowed client failed ["
              <> probeId
              <> "]:\n"
              <> indentDiagnostics resultDiagnostics
          ]
    MustReject rejectionClass needles
      | resultSuccess -> ["forbidden client typechecked successfully [" <> probeId <> "]"]
      | null resultDiagnostics -> ["forbidden client failed without a captured GHC diagnostic [" <> probeId <> "]"]
      | any (diagnosticSupportsRejection rejectionClass needles) resultDiagnostics -> []
      | otherwise ->
          [ "forbidden client failed for an unexpected reason ["
              <> probeId
              <> "]; no single "
              <> show rejectionClass
              <> " error contained required evidence "
              <> show needles
              <> ":\n"
              <> indentDiagnostics resultDiagnostics
          ]

diagnosticSupportsRejection :: RejectionClass -> [String] -> String -> Bool
diagnosticSupportsRejection rejectionClass requiredEvidence diagnostic =
  all (`isInfixOf` diagnostic) requiredEvidence
    && any (`isInfixOf` lowercaseDiagnostic) (rejectionClassEvidence rejectionClass)
  where
    lowercaseDiagnostic = fmap toLower diagnostic

rejectionClassEvidence :: RejectionClass -> [String]
rejectionClassEvidence CoercionFailure = ["arising from a use of", "couldn't match representation"]
rejectionClassEvidence HiddenImport = ["could not load module", "hidden module", "hidden package"]
rejectionClassEvidence MissingInstance = ["no instance for", "could not deduce"]
rejectionClassEvidence TypeMismatch = ["couldn't match type", "couldn't match expected type"]
rejectionClassEvidence UnavailableName =
  [ "does not export",
    "illegal term-level use",
    "no constructor has",
    "not a record selector",
    "not in scope",
    "out of scope",
    "unknown field"
  ]

indentDiagnostics :: [String] -> String
indentDiagnostics = unlines . fmap ("  " <>)

installDiagnosticCapture :: IORef [String] -> Ghc ()
installDiagnosticCapture diagnosticsRef =
  pushLogHookM $ \_ flags messageClass _ document ->
    when (isErrorMessage messageClass)
      $ modifyIORef'
        diagnosticsRef
        (<> [renderWithContext (log_default_user_context flags) document])

isErrorMessage :: MessageClass -> Bool
isErrorMessage MCFatal = True
isErrorMessage (MCDiagnostic SevError _ _) = True
isErrorMessage _ = False

prepareProbeSource :: String -> FilePath -> Maybe String -> String -> Either String String
prepareProbeSource generatedModule fixtureName selector contents = do
  let originalModule = dropExtension (takeFileName fixtureName)
      oldDeclaration = "module " <> originalModule
      newDeclaration = "module " <> generatedModule
  rewritten <- replaceOnce oldDeclaration newDeclaration contents
  pure (maybe "" (\name -> "#define " <> name <> "\n") selector <> rewritten)

replaceOnce :: String -> String -> String -> Either String String
replaceOnce wanted replacement source = case breakOn wanted source of
  Nothing -> Left ("fixture does not contain expected module declaration: " <> wanted)
  Just (prefix, suffix) -> Right (prefix <> replacement <> suffix)

breakOn :: String -> String -> Maybe (String, String)
breakOn wanted = go []
  where
    go _ [] = Nothing
    go prefix remaining
      | wanted `isPrefixOf` remaining = Just (reverse prefix, drop (length wanted) remaining)
      | character : rest <- remaining = go (character : prefix) rest

renderLogicalUnit :: LogicalUnit -> String
renderLogicalUnit LogicalUnit {packageName, componentName} =
  packageName <> ":" <> fromMaybe "main" componentName

partitionEithers :: [Either left right] -> ([left], [right])
partitionEithers = foldr step ([], [])
  where
    step (Left left) (lefts, rights) = (left : lefts, rights)
    step (Right right) (lefts, rights) = (lefts, right : rights)
