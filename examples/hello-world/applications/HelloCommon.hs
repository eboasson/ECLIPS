{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Application data and its sort policy shared by the ordinary applications.
module HelloCommon
  ( Message (..),
    expect,
    failure,
    selectedWriter,
    selectedReader,
    messageSort,
    messageQuery,
    awaitMessage,
    publishMessage,
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import Eclips.Application.Types.Result (WaitResult (WaitReady))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import GHC.Generics (Generic)

data Message = Message {message :: Text}
  deriving stock (Eq, Show, Generic)

instance App.ValueType Message

instance App.ApplicationSort Message where
  sortPolicy = App.regularPolicy [App.key (App.field @"message")]

messageSort :: Either App.SortError (App.Sort Message)
messageSort = App.compileSort @Message

expect :: (Show problem) => Either problem value -> IO value
expect = either (failure . show) pure

failure :: String -> IO value
failure = ioError . userError

selectedWriter :: App.Herald -> Text -> IO (App.Nabla Message)
selectedWriter herald = expect . App.bindNabla @Message herald

selectedReader :: App.Herald -> Text -> IO (App.Delta Message)
selectedReader herald = expect . App.bindDelta @Message herald

messageQuery :: App.Delta Message -> Text -> App.Query Message
messageQuery reader text =
  App.query reader (App.queryEqual (App.field @"message" @Message) text)

awaitMessage :: App.Delta Message -> Text -> IO ()
awaitMessage reader text = do
  ready <- App.wait (App.SomeQuery (messageQuery reader text) :| []) >>= expect
  case ready of { WaitReady -> pure () }
  values <- App.read (messageQuery reader text) >>= expect
  if Message text `elem` values then pure () else failure "ready message was not readable"

publishMessage :: App.Nabla Message -> Text -> IO ()
publishMessage writer text = do
  result <- App.write writer (Message text) >>= expect
  case result of
    WriteAccepted -> pure ()
    _ -> failure ("message publication: " <> show result)
