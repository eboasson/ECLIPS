module Main (main) where

import Data.Text.IO qualified as Text
import HelloArguments (readConnectionArguments, readLocator)
import HelloCommon (failure)
import HelloLauncher
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  (descriptor, remaining) <- getArgs >>= readConnectionArguments
  case remaining of
    "--publisher-herald" : publisher : "--reader-herald" : reader : options -> do
      publisherLocator <- readLocator publisher
      readerLocator <- readLocator reader
      directory <- takeDirectory <$> getExecutablePath
      (order, executables) <- parseOptions options PublisherFirst (directory </> "eclips-hello-publisher") (directory </> "eclips-hello-reader")
      result <- runHelloLauncher executables order descriptor publisherLocator readerLocator
      Text.putStrLn result
    _ -> failure "expected --publisher-herald HOST:PORT --reader-herald HOST:PORT [--reader-first]"

parseOptions :: [String] -> ChildOrder -> FilePath -> FilePath -> IO (ChildOrder, ChildExecutables)
parseOptions [] order publisher reader = pure (order, ChildExecutables publisher reader)
parseOptions ("--reader-first" : rest) _ publisher reader = parseOptions rest ReaderFirst publisher reader
parseOptions ("--publisher-executable" : executable : rest) order _ reader = parseOptions rest order executable reader
parseOptions ("--reader-executable" : executable : rest) order publisher _ = parseOptions rest order publisher executable
parseOptions _ _ _ _ = failure "unexpected launcher options; use --reader-first, --publisher-executable PATH, or --reader-executable PATH"
