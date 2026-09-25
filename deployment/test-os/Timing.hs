{-# LANGUAGE OverloadedStrings #-}

-- | An ordinary application process using a nondefault deployment target.
module Timing (runTimingApplications) where

import Control.Monad (void)
import Data.Text.IO qualified as Text
import Eclips.Application qualified as App
import Eclips.Application.Runtime qualified as Runtime
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Public.Types.Timing qualified as Policy
import Test.Tasty.HUnit (assertEqual)

-- The founder CLI chooses eight seconds. Only its startup artifact crosses into
-- this OS process; neither connect call supplies an explicit liveness override.
runTimingApplications :: FilePath -> IO ()
runTimingApplications descriptorPath = do
  descriptor <- Text.readFile descriptorPath >>= checked . App.decodeConnectionDescriptor
  checkTiming "launcher" descriptor
  App.withHerald descriptor (prepareAndUseChild descriptor) >>= checked
  putStrLn "nondefault timing reached launcher and prepared child"
  where
    prepareAndUseChild descriptor parent = do
      void (App.newid parent BareNewId >>= checked)
      -- An empty primordial selection isolates startup policy propagation from
      -- graph construction and label-transfer work measured by other fixtures.
      selection <- checked (Access.primordialSelection [] mempty Nothing)
      child <- App.prepareChild parent (Lifecycle.connectionDescriptorLocator descriptor) selection >>= checked
      App.awaitChildReady parent child >>= checked
      let childDescriptor = Lifecycle.preparedChildConnection child
      checkTiming "prepared child" childDescriptor
      assertEqual
        "prepared child startup text preserves the complete descriptor"
        (Right childDescriptor)
        (App.decodeConnectionDescriptor (App.encodeConnectionDescriptor childDescriptor))
      App.withHerald childDescriptor operateAndEnd >>= checked
      App.endProcess parent >>= checked
    operateAndEnd child = do
      void (App.newid child BareNewId >>= checked)
      App.endProcess child >>= checked

checkTiming :: String -> Lifecycle.ConnectionDescriptor -> IO ()
checkTiming description descriptor = do
  let target = Lifecycle.connectionDescriptorTakeoverTarget descriptor
      liveness = Runtime.applicationLivenessFromTakeoverTarget target
  assertEqual (description <> " inherits eight seconds") 8_000_000 (Policy.takeoverTargetMicroseconds target)
  assertEqual
    (description <> " resolves the carried application timing")
    [16_000, 3_200_000, 400_000, 10_000_000]
    [ Runtime.applicationRetryDelayMicroseconds liveness,
      Runtime.applicationRecoveryGraceMicroseconds liveness,
      Runtime.applicationHeartbeatIdleMicroseconds liveness,
      Runtime.applicationHeartbeatReplyTimeoutMicroseconds liveness
    ]

checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
