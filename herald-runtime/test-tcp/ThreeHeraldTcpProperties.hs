{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ThreeHeraldTcpProperties
  ( tests,
  )
where

import Control.Concurrent
  ( newEmptyMVar,
    readMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
  )
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Runtime qualified as Application
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (SortDefinitionRole),
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedReader,
    predefinedWriter,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    bootstrapManifestIdBytes,
  )
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
    peerBindingSelectedCandidate,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (PeerBindingLost, PeerDispatchObserved),
    inputBody,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (PeerDispatchDeferred),
    PeerLogicalAttempt,
    peerDispatchAttemptItem,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (ConnectionClosed))
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (ShellPeerOutcomeSuppressed, ShellPeerOutcomeWon),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeraldTcpConfiguration,
    HeraldTcpFailure,
    ResolvedTcpEndpoint,
    configuredPeerSeed,
    heartbeatConfigurationMicroseconds,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    peerDialRetryDelayMicroseconds,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (HeraldRuntimeScope),
    withHeraldTcpRuntimeInScopeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( closeAcceptedPeerBinding,
    setPeerPublicationProbeForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeraldTcp (..),
    TcpContext (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit)
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientNonce,
    applicationAttachmentClaim,
    applicationClientNonce,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import ThreeHeraldFixtures
  ( threeH1BootstrapId,
    threeH1Bootstraps,
    threeH1GeneratorSeedSource,
    threeH1Genesis,
    threeH1HeraldEpoch,
    threeH2BootstrapId,
    threeH2Bootstraps,
    threeH2GeneratorSeedSource,
    threeH2Genesis,
    threeH2HeraldEpoch,
    threeH3Bootstraps,
    threeH3GeneratorSeedSource,
    threeH3Genesis,
    threeH3HeraldEpoch,
    threeOracleContacts,
  )

threeH1ApplicationAttachment :: ApplicationAttachmentClaim
threeH1ApplicationAttachment =
  checked (applicationAttachmentClaim (bootstrapManifestIdBytes threeH1BootstrapId))

threeH2ApplicationAttachment :: ApplicationAttachmentClaim
threeH2ApplicationAttachment =
  checked (applicationAttachmentClaim (bootstrapManifestIdBytes threeH2BootstrapId))

threeH1ApplicationNonce :: ApplicationClientNonce
threeH1ApplicationNonce = applicationClientNonce 1001

threeH2ApplicationNonce :: ApplicationClientNonce
threeH2ApplicationNonce = applicationClientNonce 1002

tests :: TestTree
tests =
  testGroup
    "three-Herald TCP"
    [ testCase "H1 learns H3 through H2 and opens a direct EPRP binding" caseLearnedDirectBinding,
      testCase
        "a handed publication survives its TCP slot entering ClosingPeer"
        caseClosingPeerPublicationReoffered
    ]

caseLearnedDirectBinding :: Assertion
caseLearnedDirectBinding = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      heartbeatConfiguration = checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000)
  observed <-
    timeout 10000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH3Genesis threeH3Bootstraps threeH3GeneratorSeedSource) listener listener listener [] retryDelay heartbeatConfiguration)
        ( \h3Tcp -> do
            let h3PeerEndpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h3Tcp)
                h3Seed = seedFor h3PeerEndpoint
            h2Result <-
              withHeraldTcpRuntime
                (heraldTcpConfiguration (runtimeConfiguration threeH2Genesis threeH2Bootstraps threeH2GeneratorSeedSource) listener listener listener [h3Seed] retryDelay heartbeatConfiguration)
                ( \h2Tcp -> do
                    _ <- awaitMatchingCurrentBindings h2Tcp threeH3HeraldEpoch h3Tcp threeH2HeraldEpoch
                    let h2PeerEndpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp)
                        h2Seed = seedFor h2PeerEndpoint
                    h1Result <-
                      withHeraldTcpRuntime
                        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [h2Seed] retryDelay heartbeatConfiguration)
                        ( \h1Tcp -> do
                            (h1ToH2, h2FromH1) <-
                              awaitMatchingCurrentBindings h1Tcp threeH2HeraldEpoch h2Tcp threeH1HeraldEpoch
                            (h1ToH3, h3FromH1) <-
                              awaitMatchingCurrentBindings h1Tcp threeH3HeraldEpoch h3Tcp threeH1HeraldEpoch
                            assertEqual
                              "both ends selected the same direct learned candidate"
                              (peerBindingSelectedCandidate h1ToH3)
                              (peerBindingSelectedCandidate h3FromH1)
                            h1H2Closed <- closeAcceptedPeerBinding h1Tcp h1ToH2
                            assertEqual
                              "the package-private seam closes the exact current H1/H2 lease"
                              (Just ConnectionClosed)
                              h1H2Closed
                            (h1ToH2Replacement, h2FromH1Replacement) <-
                              awaitMatchingReplacementBindings
                                h1Tcp
                                h1ToH2
                                threeH2HeraldEpoch
                                h2Tcp
                                h2FromH1
                                threeH1HeraldEpoch
                            assertEqual
                              "configured/learned ownership converges on one replacement H1/H2 candidate"
                              (peerBindingSelectedCandidate h1ToH2Replacement)
                              (peerBindingSelectedCandidate h2FromH1Replacement)
                            h1H3Closed <- closeAcceptedPeerBinding h1Tcp h1ToH3
                            assertEqual
                              "the package-private seam closes the exact current H1/H3 lease"
                              (Just ConnectionClosed)
                              h1H3Closed
                            (replacement, replacementAtH3) <-
                              awaitMatchingReplacementBindings
                                h1Tcp
                                h1ToH3
                                threeH3HeraldEpoch
                                h3Tcp
                                h3FromH1
                                threeH1HeraldEpoch
                            assertEqual
                              "the retained learned contact reconnects with one new direct candidate"
                              (peerBindingSelectedCandidate replacement)
                              (peerBindingSelectedCandidate replacementAtH3)
                            runApplicationPublication h1Tcp h2Tcp
                        )
                    assertTcpResult "H1" h1Result
                )
            assertTcpResult "H2" h2Result
        )
  case observed of
    Nothing -> assertFailure "the three-Herald learned-direct-dial gate timed out"
    Just result -> assertTcpResult "H3" result

caseClosingPeerPublicationReoffered :: Assertion
caseClosingPeerPublicationReoffered = do
  ledgerCell <- newEmptyMVar
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      heartbeatConfiguration = checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000)
      h1Hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ ledger ->
              void (tryPutMVar ledgerCell ledger)
          }
  observed <-
    timeout 10000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH3Genesis threeH3Bootstraps threeH3GeneratorSeedSource) listener listener listener [] retryDelay heartbeatConfiguration)
        ( \h3Tcp -> do
            let h3Seed = seedFor (heraldTcpPeerEndpoint (heraldTcpEndpoints h3Tcp))
            h2Result <-
              withHeraldTcpRuntime
                (heraldTcpConfiguration (runtimeConfiguration threeH2Genesis threeH2Bootstraps threeH2GeneratorSeedSource) listener listener listener [h3Seed] retryDelay heartbeatConfiguration)
                ( \h2Tcp -> do
                    _ <- awaitMatchingCurrentBindings h2Tcp threeH3HeraldEpoch h3Tcp threeH2HeraldEpoch
                    let h2Seed = seedFor (heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp))
                    h1Result <-
                      withHookedHeraldTcp
                        h1Hooks
                        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [h2Seed] retryDelay heartbeatConfiguration)
                        ( \h1Tcp -> do
                            (h1ToH2, _) <-
                              awaitMatchingCurrentBindings h1Tcp threeH2HeraldEpoch h2Tcp threeH1HeraldEpoch
                            _ <-
                              awaitMatchingCurrentBindings h1Tcp threeH3HeraldEpoch h3Tcp threeH1HeraldEpoch
                            ledger <- readMVar ledgerCell
                            runClosingPeerPublication h1Tcp h2Tcp h1ToH2 ledger
                        )
                    assertTcpResult "H1 ClosingPeer publication" h1Result
                )
            assertTcpResult "H2 ClosingPeer publication" h2Result
        )
  case observed of
    Nothing -> assertFailure "the handed ClosingPeer publication test timed out"
    Just result -> assertTcpResult "H3 ClosingPeer publication" result

withHookedHeraldTcp ::
  RuntimeHooks ->
  HeraldTcpConfiguration ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withHookedHeraldTcp hooks =
  withHeraldTcpRuntimeInScopeInternal
    (HeraldRuntimeScope (withHeraldRuntimeWithHooks hooks))

runtimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
runtimeConfiguration genesis bootstraps seedSource =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    threeOracleContacts
    seedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    (checked (checkApplicationRecoveryConfiguration 30_000_000))
    (checked (checkPeerRecoveryConfiguration 30_000_000))

seedFor :: ResolvedTcpEndpoint -> ConfiguredPeerSeed
seedFor endpoint =
  checked
    (configuredPeerSeed (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))

awaitMatchingCurrentBindings ::
  HeraldTcp ->
  HeraldEpoch ->
  HeraldTcp ->
  HeraldEpoch ->
  IO (PeerBinding, PeerBinding)
awaitMatchingCurrentBindings left leftRemote right rightRemote =
  awaitMatchingBindings left Nothing leftRemote right Nothing rightRemote

awaitMatchingReplacementBindings ::
  HeraldTcp ->
  PeerBinding ->
  HeraldEpoch ->
  HeraldTcp ->
  PeerBinding ->
  HeraldEpoch ->
  IO (PeerBinding, PeerBinding)
awaitMatchingReplacementBindings left leftPredecessor leftRemote right rightPredecessor rightRemote =
  awaitMatchingBindings left (Just leftPredecessor) leftRemote right (Just rightPredecessor) rightRemote

awaitMatchingBindings ::
  HeraldTcp ->
  Maybe PeerBinding ->
  HeraldEpoch ->
  HeraldTcp ->
  Maybe PeerBinding ->
  HeraldEpoch ->
  IO (PeerBinding, PeerBinding)
awaitMatchingBindings
  (HeraldTcp _ _ _ leftContext)
  leftPredecessor
  leftRemote
  (HeraldTcp _ _ _ rightContext)
  rightPredecessor
  rightRemote =
    atomically $ do
      leftBindings <- fmap fst <$> readTVar leftContext.tcpCurrentPeerConnections
      rightBindings <- fmap fst <$> readTVar rightContext.tcpCurrentPeerConnections
      case [ (leftBinding, rightBinding)
           | leftBinding <- leftBindings,
             rightBinding <- rightBindings,
             matches leftPredecessor leftRemote leftBinding,
             matches rightPredecessor rightRemote rightBinding,
             peerBindingSelectedCandidate leftBinding == peerBindingSelectedCandidate rightBinding
           ] of
        matched : _ -> pure matched
        [] -> retry
    where
      matches predecessor expectedRemote binding =
        maybe True (/= binding) predecessor
          && peerBindingRemoteHeraldEpoch binding == expectedRemote

runApplicationPublication :: HeraldTcp -> HeraldTcp -> IO ()
runApplicationPublication h1Tcp h2Tcp = do
  pResult <-
    Application.withApplication
      (applicationConfigurationFor h1Tcp threeH1ApplicationAttachment threeH1ApplicationNonce)
      ( \p pStartup -> do
          qResult <-
            Application.withApplication
              (applicationConfigurationFor h2Tcp threeH2ApplicationAttachment threeH2ApplicationNonce)
              ( \q qStartup -> do
                  pAccess <- sortDefinitionAccess "P" pStartup
                  qAccess <- sortDefinitionAccess "Q" qStartup
                  publication <-
                    Application.write
                      p
                      (predefinedWriter pAccess)
                      ( PublishValue
                          (SortDefinitionValue (DeclaredSortDefinition declaredDescriptor Nothing))
                      )
                  published <- Application.awaitApplicationCall publication
                  case published of
                    Application.ApplicationCallSucceeded
                      (WriteCompleted (SortDefinitionWritten _)) -> pure ()
                    other -> assertFailure ("unexpected H1 publication result: " <> show other)
                  let query =
                        ApplicationQuery
                          { applicationQueryDeltas = Set.singleton (predefinedReader qAccess),
                            applicationQueryPredicate = QueryAlways
                          }
                  awaitRemoteDefinition q query declaredDescriptor
              )
          assertApplicationResult "Q" qResult
      )
  assertApplicationResult "P" pResult

runClosingPeerPublication ::
  HeraldTcp ->
  HeraldTcp ->
  PeerBinding ->
  TraceLedger ->
  IO ()
runClosingPeerPublication h1Tcp h2Tcp originalBinding ledger = do
  firstHandoff <- newEmptyMVar
  releaseFirst <- newEmptyMVar
  replacementHandoff <- newEmptyMVar
  observedHandoffs <- newTVarIO []
  let observe binding attempt =
        when (peerBindingRemoteHeraldEpoch binding == threeH2HeraldEpoch) $ do
          first <- tryPutMVar firstHandoff (binding, attempt)
          if first
            then do
              atomically (modifyTVar' observedHandoffs (<> [(binding, attempt)]))
              readMVar releaseFirst
            else do
              (failedBinding, failedAttempt) <- readMVar firstHandoff
              when
                ( binding /= failedBinding
                    && peerDispatchAttemptItem attempt
                      == peerDispatchAttemptItem failedAttempt
                )
                $ do
                  atomically (modifyTVar' observedHandoffs (<> [(binding, attempt)]))
                  void (tryPutMVar replacementHandoff (binding, attempt))
      cleanup = do
        void (tryPutMVar releaseFirst ())
        setPeerPublicationProbeForTest h1Tcp Nothing
  setPeerPublicationProbeForTest h1Tcp (Just observe)
  exercise firstHandoff releaseFirst replacementHandoff observedHandoffs
    `finally` cleanup
  where
    exercise firstHandoff releaseFirst replacementHandoff observedHandoffs = do
      pResult <-
        Application.withApplication
          (applicationConfigurationFor h1Tcp threeH1ApplicationAttachment threeH1ApplicationNonce)
          ( \p pStartup -> do
              qResult <-
                Application.withApplication
                  (applicationConfigurationFor h2Tcp threeH2ApplicationAttachment threeH2ApplicationNonce)
                  ( \q qStartup -> do
                      pAccess <- sortDefinitionAccess "P ClosingPeer" pStartup
                      qAccess <- sortDefinitionAccess "Q ClosingPeer" qStartup
                      publication <-
                        Application.write
                          p
                          (predefinedWriter pAccess)
                          ( PublishValue
                              (SortDefinitionValue (DeclaredSortDefinition declaredDescriptor Nothing))
                          )
                      (failedBinding, failedAttempt) <- readMVar firstHandoff
                      assertEqual
                        "the handed publication belongs to the exact current H1/H2 TCP lease"
                        originalBinding
                        failedBinding
                      closed <- closeAcceptedPeerBinding h1Tcp failedBinding
                      assertEqual
                        "the exact handed TCP slot starts closing"
                        (Just ConnectionClosed)
                        closed
                      void (tryPutMVar releaseFirst ())
                      (replacementBinding, replacementAttempt) <- readMVar replacementHandoff
                      setPeerPublicationProbeForTest h1Tcp Nothing
                      assertBool
                        "repair selects a fresh physical peer binding"
                        (replacementBinding /= failedBinding)
                      assertBool
                        "repair mints a fresh physical dispatch attempt"
                        (replacementAttempt /= failedAttempt)
                      assertEqual
                        "repair reoffers the retained logical publication item"
                        (peerDispatchAttemptItem failedAttempt)
                        (peerDispatchAttemptItem replacementAttempt)

                      published <- Application.awaitApplicationCall publication
                      case published of
                        Application.ApplicationCallSucceeded
                          (WriteCompleted (SortDefinitionWritten _)) -> pure ()
                        other -> assertFailure ("unexpected repaired H1 publication result: " <> show other)
                      let query =
                            ApplicationQuery
                              { applicationQueryDeltas = Set.singleton (predefinedReader qAccess),
                                applicationQueryPredicate = QueryAlways
                              }
                      awaitRemoteDefinition q query declaredDescriptor

                      handoffs <- atomically (readTVar observedHandoffs)
                      case handoffs of
                        first : replacements@(_ : _) -> do
                          assertEqual
                            "the retained logical item is first handed to the failed slot"
                            (failedBinding, failedAttempt)
                            first
                          assertBool
                            "every observed repair handoff uses the replacement binding and immutable item"
                            ( all
                                ( \(binding, attempt) ->
                                    binding == replacementBinding
                                      && peerDispatchAttemptItem attempt
                                        == peerDispatchAttemptItem failedAttempt
                                )
                                replacements
                            )
                          assertBool
                            "production retries on the replacement binding mint pairwise-fresh attempts"
                            (pairwiseDistinct (fmap snd replacements))
                        _ -> assertFailure "the retained logical item did not reach a replacement binding"
                      trace <- atomically (snapshotTraceLedger ledger)
                      assertClosingPublicationTrace failedBinding failedAttempt trace
                  )
              assertApplicationResult "Q ClosingPeer" qResult
          )
      assertApplicationResult "P ClosingPeer" pResult

pairwiseDistinct :: (Eq value) => [value] -> Bool
pairwiseDistinct [] = True
pairwiseDistinct (value : retained) =
  value `notElem` retained && pairwiseDistinct retained

assertClosingPublicationTrace ::
  PeerBinding ->
  PeerLogicalAttempt ->
  [RuntimeTraceEvent] ->
  Assertion
assertClosingPublicationTrace failedBinding failedAttempt events = do
  assertEqual
    "the failed handed attempt produces one Deferred kernel observation"
    [failedAttempt]
    [ observed
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      RuntimeObserved (PeerDispatchObserved observed PeerDispatchDeferred) <- [inputBody input],
      observed == failedAttempt
    ]
  assertEqual
    "the closing physical lease produces one exact binding-loss input"
    [failedBinding]
    [ observed
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      RuntimeObserved (PeerBindingLost observed) <- [inputBody input],
      observed == failedBinding
    ]
  assertEqual
    "the close path wins the failed attempt outcome exactly once"
    [failedAttempt]
    [ observed
    | ShellEvent _ (ShellPeerOutcomeWon _ observed PeerDispatchDeferred) <- events,
      observed == failedAttempt
    ]
  assertEqual
    "the throwing ClosingPeer callback cannot claim a second outcome"
    []
    [ observed
    | ShellEvent _ (ShellPeerOutcomeSuppressed _ observed _) <- events,
      observed == failedAttempt
    ]

applicationConfigurationFor ::
  HeraldTcp ->
  ApplicationAttachmentClaim ->
  ApplicationClientNonce ->
  Application.ApplicationConfiguration
applicationConfigurationFor tcp attachment nonce =
  Application.applicationConfiguration
    ( checked
        ( Application.applicationEndpoint
            (resolvedTcpHost endpoint)
            (resolvedTcpPort endpoint)
        )
    )
    attachment
    nonce
    (checked (Application.applicationLivenessConfigurationMicroseconds 1_000 2_000_000 500_000 500_000))
  where
    endpoint = heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp)

sortDefinitionAccess :: String -> ApplicationStartupAccess -> IO PredefinedAccess
sortDefinitionAccess label startup =
  case (Map.lookup (Access.environmentWriterKey SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess SortDefinitionRole writer reader)
    unexpected -> assertFailure (label <> " received unexpected sort-definition access: " <> show unexpected)

isWrittenDefinition :: ApplicationSortDescriptor -> ApplicationValue -> Bool
isWrittenDefinition descriptor value = case value of
  SortDefinitionValue (DeclaredSortDefinition actual (Just _)) -> actual == descriptor
  _ -> False

awaitRemoteDefinition :: Application.Application -> ApplicationQuery -> ApplicationSortDescriptor -> IO ()
awaitRemoteDefinition application query descriptor = do
  observed <- Application.read application query >>= Application.awaitApplicationCall
  case observed of
    Application.ApplicationCallSucceeded (ReadCompleted values)
      | any (isWrittenDefinition descriptor) values -> pure ()
      | otherwise -> awaitRemoteDefinition application query descriptor
    other -> assertFailure ("H2 read failed before observing the H1 sort definition: " <> show other)

assertApplicationResult :: String -> Either Application.ApplicationRuntimeFailure result -> Assertion
assertApplicationResult label result = case result of
  Left failure -> assertFailure (label <> " application failed: " <> show failure)
  Right _ -> pure ()

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

assertTcpResult :: (Show failure) => String -> Either failure result -> Assertion
assertTcpResult label result = case result of
  Left failure -> assertFailure (label <> " Herald TCP failed: " <> show failure)
  Right _ -> pure ()

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
