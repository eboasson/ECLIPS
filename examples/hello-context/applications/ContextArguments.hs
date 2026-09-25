{-# LANGUAGE OverloadedStrings #-}

module ContextArguments
  ( readConnectionArguments,
    readLocator,
    locatorText,
    launcherOptions,
  ) where

import ContextApplication (expect)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Application.Connection
import Eclips.Application.Types.Lifecycle
import Text.Read (readMaybe)

readConnectionArguments :: [String] -> IO (ConnectionDescriptor, [String])
readConnectionArguments ("--connection" : encoded : remaining) = do
  descriptor <- expect (decodeConnectionDescriptor (Text.pack encoded))
  pure (descriptor, remaining)
readConnectionArguments ("--connection-file" : path : remaining) = do
  descriptor <- Text.readFile path >>= expect . decodeConnectionDescriptor
  pure (descriptor, remaining)
readConnectionArguments _ = fail "expected --connection-file PATH (or --connection TEXT)"

readLocator :: String -> IO HeraldLocator
readLocator value = case Text.breakOnEnd ":" (Text.pack value) of
  (hostWithColon, portText) | not (Text.null hostWithColon) -> case readMaybe (Text.unpack portText) :: Maybe Integer of
    Just port | port > 0 && port <= 65535 -> expect (heraldLocator (Text.dropEnd 1 hostWithColon) (fromIntegral port))
    _ -> fail "invalid Herald TCP port"
  _ -> fail "expected a Herald application locator HOST:PORT"

locatorText :: HeraldLocator -> String
locatorText locator = Text.unpack (heraldLocatorHost locator) <> ":" <> show (heraldLocatorPort locator)

launcherOptions :: HeraldLocator -> [String] -> IO (NonEmpty HeraldLocator, Bool)
launcherOptions fallback = go [] False
  where
    go locators once [] = pure (case reverse locators of [] -> fallback :| []; first : rest -> first :| rest, once)
    go locators once ("--context-herald" : value : rest) = do
      locator <- readLocator value
      go (locator : locators) once rest
    go locators False ("--once" : rest) = go locators True rest
    go _ _ _ = fail "expected --context-herald HOST:PORT (repeatable) or --once"
