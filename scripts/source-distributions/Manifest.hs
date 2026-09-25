{-# LANGUAGE ImportQualifiedPost #-}

-- Cabal owns source enumeration, including common stanzas, conditional
-- components, preprocessed modules, main-is, documentation and licence files.
module Main (main) where

import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Distribution.Fields qualified as Fields
import Distribution.PackageDescription (PackageDescription (extraDocFiles, licenseFiles, package))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.SrcDist (listPackageSources)
import Distribution.Utils.Json (Json (..), renderJson)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Verbosity (silent)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (isRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))

main :: IO ()
main = do
  arguments <- getArgs
  root <- case arguments of
    [path] -> pure path
    _ -> die "usage: Manifest.hs PROJECT_ROOT"
  fields <- either (die . show) pure . Fields.readFields =<< BS.readFile (root </> "cabal.project")
  parsed <- traverse projectField fields
  let packageFields = [values | ("packages", values) <- parsed]
  packageFiles <- case packageFields of
    [paths] | not (null paths) -> pure (fmap normalise paths)
    _ -> die "cabal.project must declare one nonempty explicit packages field"
  unless (all (\(name, _) -> name `elem` ["packages", "tests"]) parsed)
    $ die "source certification requires review of new cabal.project fields"
  unless ([values | ("tests", values) <- parsed] == [["True"]])
    $ die "source certification expects the root tests: True plan"
  unless (length packageFiles == length (nub packageFiles))
    $ die "cabal.project lists a package more than once"
  descriptions <- traverse (packageManifest root) packageFiles
  LBS.putStr (renderJson (JsonArray descriptions))

projectField :: Fields.Field annotation -> IO (String, [String])
projectField field = case field of
  Fields.Field (Fields.Name _ name) values ->
    pure (BS8.unpack name, concatMap (words . fieldLine) values)
  Fields.Section (Fields.Name _ name) _ _ ->
    die ("project sections are not part of the exact local package plan: " <> BS8.unpack name)
  where
    fieldLine (Fields.FieldLine _ contents) = BS8.unpack contents

packageManifest :: FilePath -> FilePath -> IO Json
packageManifest root path = do
  unless (isRelative path && ".." `notElem` splitDirectories path && takeExtension path == ".cabal")
    $ die ("expected an explicit in-project .cabal package path: " <> path)
  exists <- doesFileExist (root </> path)
  unless exists (die ("missing project package: " <> path))
  contents <- BS.readFile (root </> path)
  let (warnings, parsed) = Cabal.runParseResult (Cabal.parseGenericPackageDescription contents)
  unless (null warnings) (die (unlines (fmap (Fields.showPWarning path) warnings)))
  generic <- case parsed of
    Right value -> pure value
    Left (_, errors) -> die (unlines (fmap (Fields.showPError path) (NonEmpty.toList errors)))
  let description = flattenPackageDescription generic
      packageRoot = takeDirectory path
  when (null (licenseFiles description)) (die (path <> ": no declared licence file"))
  sources <- listPackageSources silent (Just (makeSymbolicPath (root </> packageRoot))) description knownSuffixHandlers
  let inventory = sort (nub (takeFileName path : fmap (normalise . getSymbolicPath) sources))
  when (null (extraDocFiles description) && "README.md" `notElem` inventory)
    $ die (path <> ": no declared documentation or source-distributed README")
  pure
    $ JsonObject
      [ ("id", JsonString (prettyShow (package description))),
        ("package_file", JsonString path),
        ("source_root", JsonString packageRoot),
        ("files", JsonArray (fmap JsonString inventory))
      ]
