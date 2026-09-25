-- | Ordinary executable input: opaque descriptors and endpoint locators only.
module HelloArguments (readConnectionArguments, readLocator) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Application.Connection
import Eclips.Application.Types.Lifecycle
import HelloCommon (expect, failure)
import Text.Read (readMaybe)

readConnectionArguments :: [String] -> IO (ConnectionDescriptor, [String])
readConnectionArguments ("--connection" : encoded : remaining) = do
  descriptor <- expect (decodeConnectionDescriptor (Text.pack encoded))
  pure (descriptor, remaining)
readConnectionArguments ("--connection-file" : path : remaining) = do
  encoded <- Text.readFile path
  descriptor <- expect (decodeConnectionDescriptor encoded)
  pure (descriptor, remaining)
readConnectionArguments _ = failure "expected --connection TEXT or --connection-file PATH"

readLocator :: String -> IO HeraldLocator
readLocator value = case Text.breakOnEnd (Text.pack ":") (Text.pack value) of
  (hostWithColon, portText) | not (Text.null hostWithColon) -> case readMaybe (Text.unpack portText) :: Maybe Integer of
    Just port | port > 0 && port <= 65535 -> expect (heraldLocator (Text.dropEnd 1 hostWithColon) (fromIntegral port))
    _ -> failure "invalid Herald TCP port"
  _ -> failure "expected a Herald locator HOST:PORT"
