module AdministrationRuntimeProperties
  ( tests,
  )
where

import Control.Concurrent
  ( Chan,
    MVar,
    newChan,
    newEmptyMVar,
    readChan,
    readMVar,
    takeMVar,
    tryPutMVar,
    writeChan,
  )
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_, void)
import Eclips.Domain.Identity
  ( systemIdBytes,
  )
import Eclips.Herald.Administration.RPC (AdministrationOutbound (..))
import Eclips.Herald.EffectBatch (HeraldEffect (FinishDrain))
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    awaitHeraldRuntimeExit,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( ConfiguredAdministrationPlane,
    ConnectionRef,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeConfiguredAdministrationConnectionHandlers,
    RuntimeLaneOffer (..),
    heraldRuntimeHandlers,
    runtimeConfiguredAdministrationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerConfiguredAdministrationConnection,
    submitConfiguredAdministrationDto,
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    RuntimeRetentionCounts (retainedPublicCloses),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( RuntimeTraceEvent (ShellEvent),
    ShellTraceEvent (ShellEffectRouted),
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained, HeraldRuntimeScopeClosed),
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (..),
    AdminCorrelationIdClaim,
    AdminProcessEndReason (AdminExplicitAdministrativeEnd),
    AdminResultStatusDto (AdminAccepted),
    AdminRoleClaim (ProcessAdministrator),
    AdminServerDto (..),
    adminCorrelationIdClaim,
    adminDeploymentIdClaim,
    adminProcessEpochIdClaim,
  )
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureSystemId,
  )
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
    "process administration runtime"
    [ testCase
        "authorization, exact same-binding retry, conflict, and terminal loss cross the owner lane"
        caseReconnectableAdministration,
      testCase
        "a wrong deployment Hello closes only its reconnectable candidate"
        caseAuthorizationRejection,
      testCase
        "a superseded final writer releases its publicly closed marker"
        caseSupersededFinalWriterRetention,
      testCase
        "an earlier failed write settles the queued final drain acknowledgement"
        caseFailureBeforeFinalDrainWrite
    ]

caseReconnectableAdministration :: Assertion
caseReconnectableAdministration = do
  result <-
    withHeraldRuntime fixtureConfiguration $ \runtime -> do
      firstOutput <- newChan
      firstClosed <- newEmptyMVar
      first <- registerLane runtime firstOutput firstClosed
      assertEqual "authorized Hello enters the first candidate FIFO" Queued
        =<< submitConfiguredAdministrationDto runtime first authorizedHello
      assertEqual "the first Start enters behind Hello" Queued
        =<< submitConfiguredAdministrationDto runtime first firstStart
      expectReply "first retained acceptance" expectedAccepted firstOutput
      assertEqual "same-binding query is queued" Queued
        =<< submitConfiguredAdministrationDto runtime first firstQuery
      expectReply "same-binding query retains its accepted result" expectedAccepted firstOutput

      assertEqual "closing the established physical lease is exact" ConnectionClosed
        =<< closeRuntimeConnection runtime first
      awaitSignal "first process administration close" firstClosed
      assertEqual "the old physical generation cannot query" StalePhysicalGeneration
        =<< submitConfiguredAdministrationDto runtime first firstQuery

      secondOutput <- newChan
      secondClosed <- newEmptyMVar
      second <- registerLane runtime secondOutput secondClosed
      assertEqual "reconnect Hello enters a fresh candidate FIFO" Queued
        =<< submitConfiguredAdministrationDto runtime second authorizedHello
      assertEqual "Get enters behind reconnect Hello" Queued
        =<< submitConfiguredAdministrationDto runtime second firstQuery
      expectReply "the new binding cannot retrieve the old binding's result" (AdminAbsent firstCorrelation) secondOutput

      assertEqual "a fresh binding may reuse its own correlation" Queued
        =<< submitConfiguredAdministrationDto runtime second firstStart
      expectReply "new-scope Start is accepted independently" expectedAccepted secondOutput
      assertEqual "same-binding retry is queued" Queued
        =<< submitConfiguredAdministrationDto runtime second firstStart
      expectReply "same-binding retry reoffers its result" expectedAccepted secondOutput
      assertEqual "correlation reuse with a different request enters for semantic classification" Queued
        =<< submitConfiguredAdministrationDto runtime second conflictingStart
      expectReply "different request is a conflict" expectedConflict secondOutput
      assertEqual "the second reconnectable lane closes independently" ConnectionClosed
        =<< closeRuntimeConnection runtime second
      awaitSignal "second process administration close" secondClosed
  case result of
    Left failure -> assertFailure ("process administration runtime failed: " <> show failure)
    Right ((), HeraldRuntimeScopeClosed) -> pure ()
    Right (_, other) -> assertFailure ("unexpected process administration exit: " <> show other)

caseAuthorizationRejection :: Assertion
caseAuthorizationRejection = do
  result <-
    withHeraldRuntime fixtureConfiguration $ \runtime -> do
      output <- newChan
      closed <- newEmptyMVar
      reference <- registerLane runtime output closed
      assertEqual "wrong deployment Hello enters for owner authentication" Queued
        =<< submitConfiguredAdministrationDto runtime reference unauthorizedHello
      awaitSignal "unauthorized candidate close" closed
      rejected <- timeout 2_000_000 (readChan output)
      case rejected of
        Just RejectAdministrationCandidate {} -> pure ()
        Just other -> assertFailure ("unexpected authorization disposition: " <> show other)
        Nothing -> assertFailure "authorization disposition timed out"
      assertEqual "closed unauthorized generation cannot submit Start" StalePhysicalGeneration
        =<< submitConfiguredAdministrationDto runtime reference firstStart
      noReply <- timeout 50_000 (readChan output)
      assertEqual "authorization rejection emits no Start-family server DTO" Nothing noReply
  case result of
    Left failure -> assertFailure ("authorization rejection failed the runtime: " <> show failure)
    Right ((), HeraldRuntimeScopeClosed) -> pure ()
    Right (_, other) -> assertFailure ("unexpected authorization exit: " <> show other)

caseSupersededFinalWriterRetention :: Assertion
caseSupersededFinalWriterRetention = do
  countsCell <- newEmptyMVar
  rejecting <- newEmptyMVar
  releaseRejection <- newEmptyMVar
  firstClosed <- newEmptyMVar
  let hooks = defaultRuntimeHooks {hookRuntimeRetentionReader = void . tryPutMVar countsCell}
      firstHandlers =
        runtimeConfiguredAdministrationConnectionHandlers
          ( \outbound -> case outbound of
              RejectAdministrationCandidate {} -> do
                void (tryPutMVar rejecting ())
                readMVar releaseRejection
                pure LaneOffered
              other -> assertFailure ("unexpected first candidate outbound: " <> show other)
          )
          (void (tryPutMVar firstClosed ()))
  result <- withHeraldRuntimeWithHooks hooks fixtureConfiguration $ \runtime -> do
    first <- registerHandlers runtime firstHandlers
    assertEqual "rejected Hello is admitted" Queued
      =<< submitConfiguredAdministrationDto runtime first unauthorizedHello
    awaitSignal "final rejection writer" rejecting
    readCounts <- readMVar countsCell
    assertEqual "public close leaves the final writer owning its disposition" ConnectionClosed
      =<< closeRuntimeConnection runtime first
    assertEqual "the final writer retains one physical close marker" 1 . retainedPublicCloses =<< readCounts
    output <- newChan
    secondClosed <- newEmptyMVar
    second <- registerLane runtime output secondClosed
    assertEqual "replacement Hello is admitted" Queued
      =<< submitConfiguredAdministrationDto runtime second authorizedHello
    acceptance <- timeout 2_000_000 (readChan output)
    case acceptance of
      Just AcceptAdministrationCandidate {} -> pure ()
      other -> assertFailure ("replacement candidate was not accepted: " <> show other)
    assertEqual "rehome releases the old physical close marker before its writer finishes" 0 . retainedPublicCloses =<< readCounts
    assertEqual "the detached generation stays stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime first
    void (tryPutMVar releaseRejection ())
    awaitSignal "superseded final writer close" firstClosed
    assertEqual "late finalization does not recreate the marker" 0 . retainedPublicCloses =<< readCounts
  assertEqual "superseded final writer scope closes normally" (Right ((), HeraldRuntimeScopeClosed)) result

caseFailureBeforeFinalDrainWrite :: Assertion
caseFailureBeforeFinalDrainWrite =
  forM_
    [ ("LaneLost", pure LaneLost),
      ("exception", ioError (userError "intentional earlier administration write failure"))
    ]
    $ \(label, offerResult) -> do
      ledgerCell <- newEmptyMVar
      firstOffer <- newEmptyMVar
      releaseOffer <- newEmptyMVar
      closed <- newEmptyMVar
      output <- newChan
      let hooks = defaultRuntimeHooks {hookRuntimeInitialized = \_ _ _ _ _ _ -> void . tryPutMVar ledgerCell}
          handlers =
            runtimeConfiguredAdministrationConnectionHandlers
              ( \outbound -> do
                  writeChan output outbound
                  void (tryPutMVar firstOffer ())
                  readMVar releaseOffer
                  offerResult
              )
              (void (tryPutMVar closed ()))
      result <- withHeraldRuntimeWithHooks hooks fixtureConfiguration $ \runtime -> do
        reference <- registerHandlers runtime handlers
        assertEqual (label <> " Hello is admitted") Queued
          =<< submitConfiguredAdministrationDto runtime reference authorizedHello
        awaitSignal (label <> " first writer offer") firstOffer
        acceptance <- readChan output
        case acceptance of
          AcceptAdministrationCandidate {} -> pure ()
          other -> assertFailure ("expected first acceptance: " <> show other)
        assertEqual (label <> " drain is admitted while the earlier offer is blocked") Queued
          =<< submitConfiguredAdministrationDto runtime reference (DrainHerald firstCorrelation)
        ledger <- readMVar ledgerCell
        routed <- timeout 2_000_000 $ atomically $ do
          events <- snapshotTraceLedger ledger
          check (any (\case ShellEvent _ (ShellEffectRouted _ _ FinishDrain {}) -> True; _ -> False) events)
        assertEqual (label <> " final drain acknowledgement is queued behind the earlier offer") (Just ()) routed
        void (tryPutMVar releaseOffer ())
        awaitSignal (label <> " failed writer physically closes") closed
        assertEqual (label <> " abandoned final acknowledgement cannot hang drain") (Just (Right HeraldRuntimeDrained))
          =<< timeout 2_000_000 (awaitHeraldRuntimeExit runtime)
        assertEqual (label <> " abandoned commands never reach the closed lane") Nothing
          =<< timeout 50_000 (readChan output)
      assertEqual (label <> " preserves semantic drain completion") (Right ((), HeraldRuntimeDrained)) result

registerLane ::
  HeraldRuntime ->
  Chan AdministrationOutbound ->
  MVar () ->
  IO (ConnectionRef ConfiguredAdministrationPlane)
registerLane runtime output closed = do
  registerHandlers
    runtime
    ( runtimeConfiguredAdministrationConnectionHandlers
        (\outbound -> writeChan output outbound >> pure LaneOffered)
        (void (tryPutMVar closed ()))
    )

registerHandlers ::
  HeraldRuntime ->
  RuntimeConfiguredAdministrationConnectionHandlers ->
  IO (ConnectionRef ConfiguredAdministrationPlane)
registerHandlers runtime handlers = do
  registered <-
    registerConfiguredAdministrationConnection
      runtime
      handlers
  case registered of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "process administration registration stopped"

expectReply ::
  String ->
  AdminServerDto ->
  Chan AdministrationOutbound ->
  IO ()
expectReply context expected output = do
  observed <- timeout 2_000_000 (readChan output)
  case observed of
    Just (SendEstablishedAdministration _ actual) ->
      assertEqual context expected actual
    Just AcceptAdministrationCandidate {} -> expectReply context expected output
    Just other -> assertFailure (context <> ": unexpected outbound " <> show other)
    Nothing -> assertFailure (context <> ": timed out")

awaitSignal :: String -> MVar () -> IO ()
awaitSignal context signal = do
  observed <- timeout 2_000_000 (takeMVar signal)
  case observed of
    Just () -> pure ()
    Nothing -> assertFailure (context <> ": timed out")

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

authorizedHello :: AdminClientDto
authorizedHello =
  AdminHello
    (checked "deployment claim" (adminDeploymentIdClaim (systemIdBytes fixtureSystemId)))
    ProcessAdministrator

unauthorizedHello :: AdminClientDto
unauthorizedHello =
  AdminHello
    (checked "foreign deployment claim" (adminDeploymentIdClaim (fixtureIdentifierBytes 0xee)))
    ProcessAdministrator

firstStart :: AdminClientDto
firstStart = StartProcessEpoch firstCorrelation

firstQuery :: AdminClientDto
firstQuery = GetAdminResult firstCorrelation

conflictingStart :: AdminClientDto
conflictingStart =
  EndProcessEpoch
    firstCorrelation
    (checked "process claim" (adminProcessEpochIdClaim (fixtureIdentifierBytes 0xef)))
    AdminExplicitAdministrativeEnd

firstCorrelation :: AdminCorrelationIdClaim
firstCorrelation = adminCorrelationIdClaim 901

expectedAccepted :: AdminServerDto
expectedAccepted = AdminResult firstCorrelation AdminAccepted

expectedConflict :: AdminServerDto
expectedConflict = AdminConflict firstCorrelation

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
