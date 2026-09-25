{-# LANGUAGE OverloadedStrings #-}

-- | The three small ordinary applications. Each runs in its own OS process
-- and receives only an opaque connection descriptor and selected endpoints.
module ContextApplication
  ( runPublisher,
    runReader,
    runHolder,
    expect,
    selectedWriter,
    selectedReader,
  ) where

import ContextVocabulary
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import Eclips.Application.Types.Result (WaitResult (WaitReady))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import System.IO (hIsEOF, stdin)

runPublisher :: App.ConnectionDescriptor -> IO ()
runPublisher descriptor = App.withHerald descriptor publish >>= expect
  where
    publish herald = do
      writer <- selectedWriter herald "messages.writer"
      result <- App.write writer greeting >>= expect
      unless (result == WriteAccepted) (fail ("greeting publication: " <> show result))
      App.endProcess herald >>= expect
      putStrLn "published"

runReader :: App.ConnectionDescriptor -> IO ()
runReader descriptor = App.withHerald descriptor readGreeting >>= expect
  where
    readGreeting herald = do
      reader <- selectedReader herald "messages.reader"
      awaitGreeting reader
      putStrLn "Hello, world!"
      App.endProcess herald >>= expect

-- | Keep this delta active independently of the publisher and every reader.
-- Its stdout acknowledgement is an observation of this store, not a claim
-- that WriteAccepted itself waited for replication.
runHolder :: App.ConnectionDescriptor -> IO ()
runHolder descriptor = App.withHerald descriptor hold >>= expect
  where
    hold herald = do
      reader <- selectedReader herald "messages.reader"
      awaitGreeting reader
      putStrLn "stored"
      eof <- hIsEOF stdin
      unless eof $ do
        command <- getLine
        unless (command == "stop") (fail "context holder expected stop")
      -- A deliberate Herald crash may already have ended this child's session.
      -- Cleanup also works for that holder; its surviving peers retain context.
      App.endProcess herald >>= \case
        Right () -> pure ()
        Left App.LifecycleClosed -> pure ()
        Left (App.LifecycleUnavailable _) -> pure ()
        Left problem -> fail ("context holder End: " <> show problem)

awaitGreeting :: App.Delta Message -> IO ()
awaitGreeting reader = do
  result <- App.wait (App.SomeQuery (messageQuery reader) :| []) >>= expect
  unless (result == WaitReady) (fail ("greeting wait: " <> show result))
  values <- App.read (messageQuery reader) >>= expect
  -- Wait may wake spuriously. Recheck the store rather than treating the wake
  -- itself as evidence that the requested value is present.
  unless (greeting `elem` values) (awaitGreeting reader)

expect :: (Show problem) => Either problem value -> IO value
expect = either (fail . show) pure

selectedWriter :: App.Herald -> Text -> IO (App.Nabla Message)
selectedWriter herald key = expect (App.bindNabla @Message herald key)

selectedReader :: App.Herald -> Text -> IO (App.Delta Message)
selectedReader herald key = expect (App.bindDelta @Message herald key)
