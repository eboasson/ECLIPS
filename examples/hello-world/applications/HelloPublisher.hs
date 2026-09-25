{-# LANGUAGE OverloadedStrings #-}

-- | Independent publisher: its only OS input is an opaque connection descriptor.
module HelloPublisher (runHelloPublisher) where

import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import HelloCommon

runHelloPublisher :: App.ConnectionDescriptor -> IO Text
runHelloPublisher descriptor = App.withHerald descriptor workflow >>= expect
  where
    workflow herald = do
      writer <- selectedWriter herald "messages.writer"
      acknowledgements <- selectedReader herald "acks.reader"
      awaitMessage acknowledgements "ready"
      publishMessage writer "Hello, world!"
      awaitMessage acknowledgements "received"
      publishMessage writer "finished"
      awaitMessage acknowledgements "finished"
      App.endProcess herald >>= expect
      pure "publisher completed"
