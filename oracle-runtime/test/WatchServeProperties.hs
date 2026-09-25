module WatchServeProperties (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar,
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
import Data.IORef
  ( modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (controlIndex)
import Eclips.Oracle.Command (oracleEnvelope)
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Oracle.Runtime.Internal.WatchServe
  ( OracleWatchTermination (..),
    prepareOracleWatchEntries,
    publishedOracleWatchRange,
    terminateOracleWatchLane,
  )
import Eclips.Oracle.Transition
  ( initialOracle,
    stepOracle,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleShapeError (CommittedOracleEntriesDoNotFollowCursor),
    controlIndexDto,
  )
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC
import TestFixtures
  ( checked,
    fixtureCheckedGenesis,
    fixtureHeraldEpoch,
    validOracleCommand,
  )

tests :: TestTree
tests =
  testGroup
    "watch serving"
    [ testCase "a ledger suffix encoding contradiction is an explicit invariant outcome" caseEncodingInvariant,
      QC.testProperty "watch ranges are exactly the exclusive cursor through inclusive published prefix" propPublishedRanges,
      testCase "unavailable and invariant watch outcomes retire their live lane" caseTerminalOutcomesRetire,
      testCase "watch lane termination wakes a reader blocked in recv" caseTerminalOutcomeWakesReader
    ]

propPublishedRanges :: QC.Property
propPublishedRanges =
  QC.forAll (QC.choose (0, 128)) $ \retained ->
    QC.forAll (QC.choose (0, retained)) $ \published ->
      QC.forAll (QC.choose (0, retained + 1)) $ \cursor ->
        let entries = Map.fromList [(controlIndex index, index) | index <- [1 .. retained]]
            expected = [index | index <- [1 .. retained], index > cursor, index <= published]
            range = publishedOracleWatchRange (controlIndex cursor) (controlIndex published) entries
         in QC.conjoin
              [ QC.counterexample "range differs from independent finite prefix selection" (range QC.=== expected),
                QC.counterexample "the published cursor must have no suffix" (publishedOracleWatchRange (controlIndex published) (controlIndex published) entries QC.=== []),
                QC.counterexample "unpublished retention changed the visible range" (range QC.=== publishedOracleWatchRange (controlIndex cursor) (controlIndex published) (Map.filterWithKey (\index _ -> index <= controlIndex published) entries))
              ]

caseEncodingInvariant :: Assertion
caseEncodingInvariant = do
  entry <- fixtureAppliedEntry
  assertEqual
    "an index-one suffix cannot follow cursor one"
    ( Left
        ( OracleWatchEncodingInvariant
            (CommittedOracleEntriesDoNotFollowCursor (controlIndexDto 2) (controlIndexDto 1))
        )
    )
    (prepareOracleWatchEntries (controlIndex 1) (entry :| []))

caseTerminalOutcomesRetire :: Assertion
caseTerminalOutcomesRetire = do
  let invariant =
        CommittedOracleEntriesDoNotFollowCursor
          (controlIndexDto 2)
          (controlIndexDto 1)
  assertTermination [] OracleWatchRuntimeUnavailable
  assertTermination [invariant] (OracleWatchEncodingInvariant invariant)
  where
    assertTermination expected termination =
      bracket socketPair closePair $ \(server, client) -> do
        reported <- newIORef []
        descriptor <- socketDescriptor server
        terminateOracleWatchLane
          (\problem -> modifyIORef' reported (<> [problem]))
          server
          termination
        assertEqual
          "the watch worker leaves physical close to the connection owner"
          descriptor
          =<< socketDescriptor server
        observed <- SocketByteString.recv client 1
        assertEqual "the peer observes EOF instead of a silent watch" ByteString.empty observed
        assertEqual "only an encoding contradiction is reported" expected =<< readIORef reported

caseTerminalOutcomeWakesReader :: Assertion
caseTerminalOutcomeWakesReader =
  bracket socketPair closePair $ \(server, client) -> do
    entered <- newEmptyMVar
    received <- newEmptyMVar
    _ <-
      forkIO $ do
        putMVar entered ()
        result <- try @IOException (SocketByteString.recv server 1)
        putMVar received result
    takeMVar entered
    terminateOracleWatchLane
      (const (pure ()))
      server
      OracleWatchRuntimeUnavailable
    result <- timeout 1_000_000 (takeMVar received)
    assertBool
      "descriptor retirement wakes the connection owner's blocked recv"
      (maybe False (const True) result)
    observed <- SocketByteString.recv client 1
    assertEqual "the peer observes EOF after lane termination" ByteString.empty observed

fixtureAppliedEntry :: IO AppliedOracleEntry
fixtureAppliedEntry = do
  let initial = checked "initial Oracle" (initialOracle fixtureCheckedGenesis)
      envelope =
        oracleEnvelope
          (oracleClientRequestId fixtureHeraldEpoch 1)
          Nothing
          fixtureHeraldEpoch
          validOracleCommand
  case checked "applied Oracle entry" (stepOracle envelope initial) of
    (_, OracleCommitted _, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] -> pure entry
      other -> assertFailure ("expected one applied entry, got " <> show other)
    (_, other, _) -> assertFailure ("expected a committed entry, got " <> show other)

socketPair :: IO (Socket.Socket, Socket.Socket)
socketPair = Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

closePair :: (Socket.Socket, Socket.Socket) -> IO ()
closePair (left, right) = closeQuietly left >> closeQuietly right

closeQuietly :: Socket.Socket -> IO ()
closeQuietly socket = void (try @IOException (Socket.close socket))

socketDescriptor :: Socket.Socket -> IO Int
socketDescriptor socket =
  Socket.withFdSocket socket (pure . fromIntegral)
