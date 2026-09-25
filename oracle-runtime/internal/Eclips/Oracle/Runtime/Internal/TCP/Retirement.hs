-- | Foreign-thread retirement for an owned stream socket.
--
-- A worker which does not own the descriptor may wake its blocked reader, but
-- must not release the descriptor while that reader is still registered with
-- the runtime I/O manager.  The reader's enclosing owner remains responsible
-- for the eventual physical close after its child workers have joined.
module Eclips.Oracle.Runtime.Internal.TCP.Retirement
  ( retireSocketQuietly,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Network.Socket
  ( ShutdownCmd (ShutdownBoth),
    Socket,
    shutdown,
  )

-- | Idempotently make a connected stream unusable and wake its reader without
-- releasing the descriptor from a foreign thread.
retireSocketQuietly :: Socket -> IO ()
retireSocketQuietly connection =
  void (try @IOException (shutdown connection ShutdownBoth))
