{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Deployment.Admin
import Eclips.Deployment.Configuration (parseEndpoint)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  arguments <- getArgs
  if arguments == ["--help"]
    then putStrLn usage
    else do
      (target, system, correlation, remaining) <- parse arguments Nothing Nothing 1
      endpoint <- maybe (fail usage) (checked . parseEndpoint . Text.pack) target
      identity <- maybe (fail usage) (checked . decodeIdentity . Text.pack) system
      command <- case remaining of
        ["status"] -> pure InspectStatus
        ["preparations"] -> pure InspectPreparations
        ["cancel-child", reference] -> CancelPreparation <$> checked (decodePreparation (Text.pack reference))
        ["drain"] -> pure Drain
        ["prepare-oracle"] -> pure PrepareReplica
        ["oracle-configuration"] -> pure InspectOracleConfiguration
        "commission-voters" : expected : nodes@(_ : _) ->
          CommissionVoters
            <$> checked (decodeVoterConfiguration (Text.pack expected))
            <*> traverse (checked . decodeOracleNode . Text.pack) nodes
        ["decommission-voter", expected, node] ->
          DecommissionVoter
            <$> checked (decodeVoterConfiguration (Text.pack expected))
            <*> checked (decodeOracleNode (Text.pack node))
        ["cancel-voter-change", change] -> CancelVoterConfiguration <$> checked (decodeVoterChange (Text.pack change))
        ["voter-change", change] -> InspectVoterChange <$> checked (decodeVoterChange (Text.pack change))
        _ -> fail usage
      runAdministration endpoint identity correlation command >>= mapM_ Text.putStrLn
  where
    parse ("--endpoint" : value : rest) _ system correlation = parse rest (Just value) system correlation
    parse ("--system" : value : rest) target _ correlation = parse rest target (Just value) correlation
    parse ("--correlation" : value : rest) target system _ = do
      correlation <- maybe (fail "invalid correlation") pure (readMaybe value)
      parse rest target system correlation
    parse remaining target system correlation = pure (target, system, correlation, remaining)
    checked = either (fail . show) pure
usage :: String
usage =
  unlines
    [ "eclips-admin --endpoint HOST:PORT --system HEX [--correlation INTEGER] COMMAND",
      "  status | preparations | cancel-child REFERENCE | drain",
      "  prepare-oracle | oracle-configuration",
      "  commission-voters EXPECTED_CONFIGURATION_HEX NODE_HEX...",
      "  decommission-voter EXPECTED_CONFIGURATION_HEX NODE_HEX",
      "  cancel-voter-change CHANGE_INTEGER | voter-change CHANGE_INTEGER",
      "Each invocation waits for its result within one connection lifetime."
    ]
