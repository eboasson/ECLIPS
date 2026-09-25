module Eclips.TypeBoundaries
  ( mainWithArguments,
    diagnosticSupportsRejection,
    extractCppSelectors,
    prepareProbeSource,
    runConfiguredChecker,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath ((</>))

import Eclips.TypeBoundaries.Ghc914
  ( CheckerConfig (..),
    CheckerMode (..),
    diagnosticSupportsRejection,
    extractCppSelectors,
    prepareProbeSource,
    runGhcChecks,
  )
import Eclips.TypeBoundaries.Spec
  ( expectedNegativeProbeCount,
    expectedPositiveProbeCount,
  )
import Eclips.TypeBoundaries.Surface
  ( SurfaceEntry (..),
    parseSurface,
    renderSurface,
    surfaceDifference,
  )

mainWithArguments :: [String] -> IO ()
mainWithArguments arguments = case parseArguments arguments of
  Left message -> die (message <> "\n" <> usage)
  Right config -> do
    violations <- runConfiguredChecker config
    unless (null violations) (die (unlines (fmap ("type boundary violation: " <>) violations)))
    case checkerMode config of
      CheckSurface ->
        putStrLn
          ( "semantic type boundaries passed: "
              <> show expectedNegativeProbeCount
              <> " in-process negative probes, "
              <> show expectedPositiveProbeCount
              <> " positive client modules, and the exact public export surface"
          )
      WriteSurface output -> putStrLn ("wrote semantic public export candidate: " <> output)

runConfiguredChecker :: CheckerConfig -> IO [String]
runConfiguredChecker config@CheckerConfig {projectRoot, ghcLibdir, packageDatabase, dependencyPackageDatabases, expectedSurfacePath, checkerMode} = do
  projectRootExists <- doesDirectoryExist projectRoot
  ghcLibdirExists <- doesDirectoryExist ghcLibdir
  packageDbExists <- doesDirectoryExist packageDatabase
  packageCacheExists <- doesFileExist (packageDatabase </> "package.cache")
  dependencyPackageDbExists <- traverse doesDirectoryExist dependencyPackageDatabases
  dependencyPackageCacheExists <- traverse (doesFileExist . (</> "package.cache")) dependencyPackageDatabases
  surfaceExists <- doesFileExist expectedSurfacePath
  let inputViolations =
        ["missing project root: " <> projectRoot | not projectRootExists]
          <> ["missing GHC libdir: " <> ghcLibdir | not ghcLibdirExists]
          <> ["missing project package database: " <> packageDatabase | not packageDbExists]
          <> ["missing project package cache: " <> (packageDatabase </> "package.cache") | packageDbExists && not packageCacheExists]
          <> [ "missing dependency package database: " <> database
             | (database, exists) <- zip dependencyPackageDatabases dependencyPackageDbExists,
               not exists
             ]
          <> [ "missing dependency package cache: " <> (database </> "package.cache")
             | (database, databaseExists, cacheExists) <- zip3 dependencyPackageDatabases dependencyPackageDbExists dependencyPackageCacheExists,
               databaseExists && not cacheExists
             ]
          <> ["missing expected semantic surface: " <> expectedSurfacePath | checkerMode == CheckSurface && not surfaceExists]
  if not (null inputViolations)
    then pure inputViolations
    else do
      result <- runGhcChecks config
      case result of
        Left violations -> pure violations
        Right (probeViolations, actualSurface) -> case checkerMode of
          WriteSurface output
            | null probeViolations -> writeFile output (renderSurface actualSurface) >> pure []
            | otherwise -> pure probeViolations
          CheckSurface -> (probeViolations <>) <$> compareSurface expectedSurfacePath actualSurface

compareSurface :: FilePath -> [SurfaceEntry] -> IO [String]
compareSurface path actual = do
  contents <- readFile path
  pure $ case parseSurface contents of
    Left message -> [path <> ": " <> message]
    Right expected ->
      let (added, removed) = surfaceDifference expected actual
       in [ renderSurfaceDrift added removed
          | not (null added) || not (null removed)
          ]

renderSurfaceDrift :: [SurfaceEntry] -> [SurfaceEntry] -> String
renderSurfaceDrift added removed =
  "semantic public export surface differs from its reviewed manifest:\n"
    <> intercalate "\n" (fmap (("  + " <>) . renderOne) added <> fmap (("  - " <>) . renderOne) removed)

renderOne :: SurfaceEntry -> String
renderOne SurfaceEntry {publicUnit, publicModule, exportNamespace, exportOccurrence, exportParent, definingUnit, definingModule} =
  publicUnit
    <> " "
    <> publicModule
    <> " "
    <> exportNamespace
    <> " "
    <> exportOccurrence
    <> " {parent "
    <> exportParent
    <> "}"
    <> " <- "
    <> definingUnit
    <> "/"
    <> definingModule

parseArguments :: [String] -> Either String CheckerConfig
parseArguments = go emptyArguments
  where
    go partial [] = finish partial
    go partial ("--project-root" : value : rest) = go partial {argProjectRoot = Just value} rest
    go partial ("--ghc-libdir" : value : rest) = go partial {argGhcLibdir = Just value} rest
    go partial ("--package-db" : value : rest) = go partial {argPackageDatabase = Just value} rest
    go partial ("--dependency-package-db" : value : rest) =
      go partial {argDependencyPackageDatabases = argDependencyPackageDatabases partial <> [value]} rest
    go partial ("--scratch" : value : rest) = go partial {argScratchDirectory = Just value} rest
    go partial ("--surface" : value : rest) = go partial {argSurfacePath = Just value} rest
    go partial ("--write-surface" : value : rest) = go partial {argWriteSurface = Just value} rest
    go _ (unknown : _) = Left ("unknown or incomplete argument: " <> unknown)

data PartialArguments = PartialArguments
  { argProjectRoot :: Maybe FilePath,
    argGhcLibdir :: Maybe FilePath,
    argPackageDatabase :: Maybe FilePath,
    argDependencyPackageDatabases :: [FilePath],
    argScratchDirectory :: Maybe FilePath,
    argSurfacePath :: Maybe FilePath,
    argWriteSurface :: Maybe FilePath
  }

emptyArguments :: PartialArguments
emptyArguments = PartialArguments Nothing Nothing Nothing [] Nothing Nothing Nothing

finish :: PartialArguments -> Either String CheckerConfig
finish PartialArguments {argProjectRoot, argGhcLibdir, argPackageDatabase, argDependencyPackageDatabases, argScratchDirectory, argSurfacePath, argWriteSurface} = do
  projectRoot <- required "--project-root" argProjectRoot
  ghcLibdir <- required "--ghc-libdir" argGhcLibdir
  packageDatabase <- required "--package-db" argPackageDatabase
  scratchDirectory <- required "--scratch" argScratchDirectory
  expectedSurfacePath <- required "--surface" argSurfacePath
  pure
    CheckerConfig
      { projectRoot,
        ghcLibdir,
        packageDatabase,
        dependencyPackageDatabases = argDependencyPackageDatabases,
        scratchDirectory,
        expectedSurfacePath,
        checkerMode = maybe CheckSurface WriteSurface argWriteSurface
      }

required :: String -> Maybe value -> Either String value
required label = maybe (Left ("missing required argument: " <> label)) Right

usage :: String
usage =
  unwords
    [ "usage: eclips-type-boundaries",
      "--project-root ROOT",
      "--ghc-libdir LIBDIR",
      "--package-db PACKAGE_DB",
      "[--dependency-package-db PACKAGE_DB]...",
      "--scratch DIRECTORY",
      "--surface FILE",
      "[--write-surface FILE]"
    ]
