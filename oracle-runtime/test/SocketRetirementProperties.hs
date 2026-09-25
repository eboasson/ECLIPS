module SocketRetirementProperties (tests) where

import Control.Concurrent
  ( ThreadId,
    forkIO,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Exception
  ( IOException,
    bracket,
    try,
  )
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Eclips.Oracle.Runtime.Internal.TCP.Retirement
  ( retireSocketQuietly,
  )
import GHC.Conc
  ( ThreadStatus (..),
    threadStatus,
    yield,
  )
import Network.Socket
  ( Family (AF_UNIX),
    Socket,
    SocketType (Stream),
    defaultProtocol,
    socketPair,
    withFdSocket,
  )
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "socket retirement"
    [ testCase
        "foreign retirement wakes the reader but leaves close to its owner"
        caseForeignRetirementPreservesOwnership
    ]

caseForeignRetirementPreservesOwnership :: Assertion
caseForeignRetirementPreservesOwnership =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    closePair
    $ \(owned, _peer) -> do
      descriptor <- socketDescriptor owned
      readerReleased <- newEmptyMVar
      allowOwnerClose <- newEmptyMVar
      ownerClosed <- newEmptyMVar
      owner <-
        forkIO $ do
          received <- try @IOException (SocketByteString.recv owned 1)
          putMVar readerReleased received
          takeMVar allowOwnerClose
          Socket.close owned
          putMVar ownerClosed ()
      awaitThreadBlocked owner

      retireSocketQuietly owned
      received <- awaitMVar "retirement did not wake the descriptor owner" readerReleased
      case received of
        Left failure -> assertFailure ("retired reader failed: " <> show failure)
        Right bytes -> assertEqual "retirement presents EOF to the reader" ByteString.empty bytes
      assertEqual
        "retirement retains the exact descriptor until owner close"
        descriptor
        =<< socketDescriptor owned

      putMVar allowOwnerClose ()
      _ <- awaitMVar "the descriptor owner did not complete its close" ownerClosed
      pure ()

awaitThreadBlocked :: ThreadId -> Assertion
awaitThreadBlocked thread = do
  observed <- timeout 1_000_000 loop
  case observed of
    Just () -> pure ()
    Nothing -> assertFailure "the descriptor owner did not block in recv"
  where
    loop =
      threadStatus thread >>= \case
        ThreadBlocked _ -> pure ()
        ThreadRunning -> yield >> loop
        ThreadFinished -> assertFailure "the descriptor owner finished before retirement"
        ThreadDied -> assertFailure "the descriptor owner died before retirement"

socketDescriptor :: Socket -> IO Int
socketDescriptor connection =
  withFdSocket connection (pure . fromIntegral)

awaitMVar :: String -> MVar value -> IO value
awaitMVar failure result = do
  observed <- timeout 1_000_000 (takeMVar result)
  maybe (assertFailure failure) pure observed

closePair :: (Socket, Socket) -> IO ()
closePair (left, right) = closeQuietly left >> closeQuietly right

closeQuietly :: Socket -> IO ()
closeQuietly connection = void (try @IOException (Socket.close connection))
