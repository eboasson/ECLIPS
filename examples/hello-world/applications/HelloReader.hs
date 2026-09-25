{-# LANGUAGE OverloadedStrings #-}

-- | Independent reader: readiness and acknowledgement travel through ECLIPS.
module HelloReader (runHelloReader) where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import Eclips.Application.Typed.Advanced qualified as Advanced
import HelloCommon

runHelloReader :: App.ConnectionDescriptor -> IO Text
runHelloReader descriptor = App.withHerald descriptor workflow >>= expect
  where
    workflow herald = do
      reader <- selectedReader herald "messages.reader"
      acknowledgements <- selectedWriter herald "acks.writer"
      commands <- selectedReader herald "commands.reader"
      unused <- Advanced.submit (Advanced.Wait (App.SomeQuery (messageQuery reader "unused") :| []))
      Advanced.cancel unused
      cancelled <- Advanced.await unused
      unless (cancelled == Left (App.UnderlyingCall App.CallCancelled)) (failure "unused wait did not settle cancellation")
      publishMessage acknowledgements "ready"
      awaitMessage reader "Hello, world!"
      taken <- App.localTake (messageQuery reader "Hello, world!") >>= expect
      unless (taken == [Message "Hello, world!"]) (failure "reader local take did not return the greeting")
      publishMessage acknowledgements "received"
      awaitMessage reader "finished"
      publishMessage acknowledgements "finished"
      awaitMessage commands "exit"
      App.endProcess herald >>= expect
      pure "Hello, world!"
