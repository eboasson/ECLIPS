module Main (main) where

import ContextApplication
import ContextArguments
import ContextLauncher
import Eclips.Application.Types.Lifecycle (connectionDescriptorLocator)
import System.Environment (getArgs, getExecutablePath)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  arguments <- getArgs
  case arguments of
    ["--help"] -> putStrLn usage
    "publisher" : rest -> child runPublisher rest
    "reader" : rest -> child runReader rest
    "holder" : rest -> child runHolder rest
    _ -> do
      (descriptor, options) <- readConnectionArguments arguments
      (locators, once) <- launcherOptions (connectionDescriptorLocator descriptor) options
      executable <- getExecutablePath
      runLauncher executable descriptor locators once
  where
    child action arguments = do
      (descriptor, remaining) <- readConnectionArguments arguments
      if null remaining then action descriptor else fail "unexpected child arguments"

usage :: String
usage =
  unlines
    [ "eclips-hello-context --connection-file PATH [--context-herald HOST:PORT ...] [--once]",
      "Locators name Herald application listeners; by default use the founder.",
      "The publisher exits before readers are created. Context holders stay alive.",
      "Interactive commands: read HOST:PORT | quit",
      "--once creates one reader on the first context Herald, then stops.",
      "The publisher, reader and holder subcommands are launched automatically."
    ]
