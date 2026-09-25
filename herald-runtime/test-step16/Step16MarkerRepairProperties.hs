{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Exact marker repair over a real EPRP connection with a destination-owned
-- live alignment subscription. All semantic work enters through EAPP or the
-- ordinary Oracle watch; the only injected event is one physical socket loss.
module Step16MarkerRepairProperties (tests, caseLiveMarkerRepair) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (finally)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access (ApplicationPredefinedSortRole (NeutralVertexRole), PredefinedAccess, predefinedReader)
import Eclips.Application.Types.Identity (PrivateUniqueId)
import Eclips.Application.Types.Query (ApplicationProjection (ApplicationProjection), ApplicationQuery (..), ApplicationQueryLiteral (QueryUniqueId), ApplicationQueryPredicate (QueryCompare), ApplicationScalarComparison (ScalarEqual))
import Eclips.Application.Types.Result (RegularCallResult (LocalTakeCompleted, ReadCompleted, WriteCompleted))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import Eclips.Domain.Disappearance qualified as D
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Discovery (PeerBinding, peerBindingRemoteHeraldEpoch, peerBindingSelectedCandidate)
import Eclips.Herald.EffectBatch (HeraldEffect (RunOracleClientAction), effectBatchMembers)
import Eclips.Herald.Input (HeraldInputBody (OracleInput, PeerInput, RuntimeObserved), PeerControl (PeerStreamResumeOffered), PeerIngress (PeerHelloReceived), RuntimeObservation (PeerDispatchObserved), inputBody)
import Eclips.Herald.OracleClient (OracleClientAction (SubmitOracleRequest), OracleClientIngress (OracleEntriesReceived), oracleRequestDispatchEnvelope)
import Eclips.Herald.Peer.RPC (projectPeerLogicalAttempt)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (ConnectionClosed))
import Eclips.Herald.Runtime.Internal.Trace (KernelTraceStep (KernelTraceStepped), RuntimeTraceEvent (KernelEvent, ShellEvent), ShellTraceEvent (ShellEffectRouted))
import Eclips.Herald.Runtime.TCP.Internal.Peer (closeAcceptedPeerBinding, setPeerEnvelopeProbeForTest)
import Eclips.Herald.Runtime.TCP.Internal.Types (PeerEnvelopeProbePhase (..))
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue, canonicalOracleEnvelopeValue)
import Eclips.Oracle.Command (oracleEnvelopeCommand, oracleEnvelopeHomeHeraldEpoch, oracleEnvelopeRequestId, reportPredefinedAbsenceCommand, resolveDisappearanceProbeCommand)
import Eclips.Oracle.Disappearance (completeDisappearanceEvidenceDigest)
import Eclips.Oracle.Projection (OracleProjectionEventView (..), appliedEntryProjectionEvents, oracleProjectionEventView)
import Eclips.Protocol.Peer.Codec (encodePeerEnvelope)
import Eclips.Protocol.Peer.Codec qualified as PeerCodec
import Eclips.Protocol.Peer.Types qualified as Peer
import PeerEvidence (data SemanticPeerControlReceived)
import Step12Fixtures (step12H1HeraldEpoch, step12H2HeraldEpoch, step12H3HeraldEpoch, step12H4HeraldEpoch)
import Step14AlignmentLossProperties (AlignmentLossFixture (..), establishAlignmentLossTopology, scenarioTimeoutMicroseconds, withStep14Applications)
import Step14Fixtures (Step14Deployment, Step14Herald, Step14HeraldName (..), awaitStep14PeerMesh, causalWaitTimeoutMicroseconds, snapshotStep14HeraldTrace, step14H1, step14H2, step14H3, step14H4, step14HeraldName, step14HeraldTcp, step14HeraldTraceLedger, withStep14Deployment)
import Step15Applications (awaitStep15ApplicationCallWithin, publishStep15NeutralCarrier, readStep15PredefinedStore, step15PredefinedAccess)
import Step16PeerWave (PeerWaveCapture, awaitPeerWave, capturePeerWave, snapshotPendingPeerWave)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Step-16 real marker repair"
    [testCase "seed-162001: one peer loss repairs exact publication and live alignment markers once" caseLiveMarkerRepair]

caseLiveMarkerRepair :: Assertion
caseLiveMarkerRepair = do
  baseline <- runMarkerSchedule False
  repaired <- runMarkerSchedule True
  assertEqual "loss preserves logical publication markers P" baseline.publications repaired.publications
  assertEqual "loss preserves logical remote alignment markers A_remote" baseline.remoteAlignments repaired.remoteAlignments
  assertEqual "loss-free physical marker writes equal P + A_remote" (baseline.publications + baseline.remoteAlignments) baseline.physical
  assertEqual "one loss adds exactly the affected retained marker writes" (repaired.publications + repaired.remoteAlignments + repaired.repairs) repaired.physical
  assertEqual "repair work L is exactly the three markers held on the duplex socket" 3 repaired.repairs
  assertBool "repair work L is bounded by P + A_remote" (repaired.repairs <= repaired.publications + repaired.remoteAlignments)
  putStrLn ("Step-16 marker work after drain/join: baseline=" <> show baseline <> "; repaired=" <> show repaired)

data MarkerCounts = MarkerCounts
  { publications :: Int,
    remoteAlignments :: Int,
    physical :: Int,
    repairs :: Int
  }
  deriving stock (Eq, Show)

data MarkerKind = PublicationMarker | AlignmentMarker
  deriving stock (Eq, Ord, Show)

-- The full encoded envelope is the immutable item identity. Source and
-- destination distinguish otherwise equal alignment control frames.
data MarkerKey = MarkerKey Step14HeraldName HeraldEpoch MarkerKind ByteString
  deriving stock (Eq, Ord, Show)

data MarkerWrite = MarkerWrite MarkerKey Peer.DisappearanceProbeIdDto PeerBinding

data MarkerObservation = MarkerObservation
  { writes :: TVar [MarkerWrite],
    affected :: TVar (Set.Set MarkerKey),
    sourceBinding :: TVar (Maybe PeerBinding),
    sourceWriteHeld :: TVar Bool,
    expectedSubscription :: TVar (Maybe Peer.AlignmentSubscriptionIdDto),
    firstReceiveHeld :: TVar Bool,
    holdAvailable :: TVar Bool,
    reverseBinding :: TVar (Maybe PeerBinding),
    reverseWriteHeld :: TVar Bool,
    reverseReceiveHeld :: TVar Bool,
    reverseHoldAvailable :: TVar Bool,
    injectLoss :: Bool,
    releaseReceive :: MVar ()
  }

newObservation :: Bool -> IO MarkerObservation
newObservation injectLoss = do
  writes <- newTVarIO []
  affected <- newTVarIO Set.empty
  sourceBinding <- newTVarIO Nothing
  sourceWriteHeld <- newTVarIO False
  expectedSubscription <- newTVarIO Nothing
  firstReceiveHeld <- newTVarIO False
  holdAvailable <- newTVarIO injectLoss
  reverseBinding <- newTVarIO Nothing
  reverseWriteHeld <- newTVarIO False
  reverseReceiveHeld <- newTVarIO False
  reverseHoldAvailable <- newTVarIO injectLoss
  releaseReceive <- newEmptyMVar
  pure MarkerObservation {writes, affected, sourceBinding, sourceWriteHeld, expectedSubscription, firstReceiveHeld, holdAvailable, reverseBinding, reverseWriteHeld, reverseReceiveHeld, reverseHoldAvailable, injectLoss, releaseReceive}

runMarkerSchedule :: Bool -> IO MarkerCounts
runMarkerSchedule injectLoss = do
  observation <- newObservation injectLoss
  -- Returning the handles out of this bracket retains immutable ledgers only.
  -- Every deployment scope has semantically drained and joined its workers
  -- before the final sampler below reads counters or Oracle traces.
  finished <- timeout scenarioTimeoutMicroseconds $ withStep14Deployment $ \deployment -> do
    awaitStep14PeerMesh deployment
    let setupWave =
          capturePeerWave
            ( zip
                [step12H1HeraldEpoch, step12H2HeraldEpoch, step12H3HeraldEpoch, step12H4HeraldEpoch]
                (fmap step14HeraldTraceLedger (deploymentHeralds deployment))
            )
    withStep14Applications deployment $ \p pStartup q qStartup -> do
      pAccess <- step15PredefinedAccess "marker P" NeutralVertexRole pStartup
      primordialValues <- readStep15PredefinedStore "P primordial hub" p pAccess
      assertEqual "P initially retains its primordial hub" 1 (length primordialValues)
      (subject, publication) <- publishStep15NeutralCarrier "marker subject" p pStartup pAccess
      awaitStep15ApplicationCallWithin "marker subject publication" publication >>= \case
        EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
        other -> assertFailure ("marker subject publication failed: " <> show other)
      awaitExactlyOneCarrier "P" p pAccess subject
      -- Establish and apply the final historical alignment after publishing the
      -- subject, so its structural prefix precedes the live subscription witness.
      alignment <- establishAlignmentLossTopology deployment p pStartup q qStartup
      -- A destination's Live/history witness can precede other participants'
      -- setup-control receipts. Close that finite wave before taking the last
      -- carrier, so the marker schedule starts with a settled evidence set.
      awaitMarkerSetupWave setupWave
      let Peer.AlignmentSubscribeDto _ subscription _ _ = alignment.lossActiveSubscribe
      atomically (writeTVar observation.expectedSubscription (Just subscription))
      installObservations observation deployment
      ( do
          -- The inherited topology routes SortDefinition P-to-Q and Delta
          -- Q-to-P, but no NeutralVertex application edge. P owns the sole
          -- visible subject carrier; its primordial hub remains in the same
          -- reader. Every other member still owes its exact cut/report.
          takeCarrier "P" p pAccess subject
          remaining <- readStep15PredefinedStore "P preserved primordial hub" p pAccess
          assertEqual "taking the subject preserves the primordial hub" primordialValues remaining
          when injectLoss $ do
            (binding, markers) <- awaitBothMarkerWrites observation
            assertEqual "the affected connection carries both marker families" (Set.fromList [PublicationMarker, AlignmentMarker]) (Set.map (\(MarkerKey _ _ kind _) -> kind) markers)
            assertEqual "the exact loss cut holds two publication markers and one alignment marker" (Map.fromList [(PublicationMarker, 2 :: Int), (AlignmentMarker, 1)]) (Map.fromListWith (+) [(kind, 1) | MarkerKey _ _ kind _ <- Set.toList markers])
            assertBool "the alignment marker names the destination's actual live subscription" (any (keyHasSubscription subscription) (Set.toList markers))
            closed <- closeAcceptedPeerBinding (step14HeraldTcp (step14H1 deployment)) binding
            assertEqual "one exact accepted peer binding is closed" (Just ConnectionClosed) closed
            void (tryPutMVar observation.releaseReceive ())
          probe <- awaitResolvedAtEveryHerald deployment observation
          pure (deploymentHeralds deployment, probe)
        )
        `finally` void (tryPutMVar observation.releaseReceive ())
  (heralds, probe) <- case finished of
    Just result -> pure result
    Nothing -> assertFailure "Step-16 marker repair schedule exceeded its test failure bound"
  retained <- atomically (readTVar observation.writes)
  affectedItems <- atomically (readTVar observation.affected)
  traces <- traverse snapshotStep14HeraldTrace heralds
  let selected = [key | MarkerWrite key actual _ <- retained, sameProbe probe actual]
      histogram = Map.fromListWith (+) [(key, 1 :: Int) | key <- selected]
      countKind kind = length [() | MarkerKey _ _ actual _ <- Map.keys histogram, actual == kind]
      expectedMultiplicity key = if injectLoss && Set.member key affectedItems then 2 else 1
  assertEqual "every captured member assigns one marker to each other member" 12 (countKind PublicationMarker)
  assertBool "the disappearance cut includes at least one real alignment subscription" (countKind AlignmentMarker > 0)
  forM_ (Map.toList histogram) $ \(key, actual) ->
    assertEqual
      ("exact retained frame delivery count: " <> show key <> "; physical binding order=" <> show [binding | MarkerWrite observed _ binding <- reverse retained, observed == key] <> "; kernel outcome history=" <> show (concatMap (markerOutcomeHistory key) traces))
      (expectedMultiplicity key)
      actual
  assertExactlyOneReportAndResolve probe traces
  pure (MarkerCounts (countKind PublicationMarker) (countKind AlignmentMarker) (length selected) (if injectLoss then Set.size affectedItems else 0))

installObservations :: MarkerObservation -> Step14Deployment -> IO ()
installObservations observation deployment =
  forM_ (deploymentHeralds deployment) $ \herald ->
    setPeerEnvelopeProbeForTest (step14HeraldTcp herald) (Just (observeEnvelope observation (step14HeraldName herald)))

observeEnvelope :: MarkerObservation -> Step14HeraldName -> PeerEnvelopeProbePhase -> PeerBinding -> Peer.PeerEnvelope -> IO ()
observeEnvelope observation source phase binding envelope =
  case markerIdentity envelope of
    Nothing -> pure ()
    Just (kind, probe) -> do
      let destination = peerBindingRemoteHeraldEpoch binding
          key = MarkerKey source destination kind (encodePeerEnvelope envelope)
      case phase of
        PeerEnvelopeWriting -> pure ()
        PeerEnvelopeWritten -> do
          held <- atomically $ do
            modifyTVar' observation.writes (MarkerWrite key probe binding :)
            current <- readTVar observation.sourceBinding
            if source == Step14H1 && destination == step12H2HeraldEpoch && maybe True (== binding) current
              then do
                when (current == Nothing) (writeTVar observation.sourceBinding (Just binding))
                modifyTVar' observation.affected (Set.insert key)
                affected <- readTVar observation.affected
                expected <- readTVar observation.expectedSubscription
                let bothSent =
                      any (\(MarkerKey origin _ observed _) -> origin == Step14H1 && observed == PublicationMarker) affected
                        && maybe False (\subscription -> any (keyHasSubscription subscription) affected) expected
                when (observation.injectLoss && bothSent) (writeTVar observation.sourceWriteHeld True)
                pure (observation.injectLoss && bothSent)
              else do
                reverseCurrent <- readTVar observation.reverseBinding
                if source == Step14H2 && destination == step12H1HeraldEpoch && maybe True (== binding) reverseCurrent
                  then do
                    when (reverseCurrent == Nothing) (writeTVar observation.reverseBinding (Just binding))
                    modifyTVar' observation.affected (Set.insert key)
                    let holdReverse = observation.injectLoss && kind == PublicationMarker
                    when holdReverse (writeTVar observation.reverseWriteHeld True)
                    pure holdReverse
                  else pure False
          when held $ do
            readMVar observation.releaseReceive
            ioError (userError "Step-16 injected loss after exact marker writes")
        PeerEnvelopeReceiving -> do
          held <- atomically $ do
            available <- readTVar observation.holdAvailable
            if available && source == Step14H2 && destination == step12H1HeraldEpoch
              then do
                writeTVar observation.holdAvailable False
                writeTVar observation.firstReceiveHeld True
                pure True
              else do
                reverseAvailable <- readTVar observation.reverseHoldAvailable
                if reverseAvailable && source == Step14H1 && destination == step12H2HeraldEpoch
                  then do
                    writeTVar observation.reverseHoldAvailable False
                    writeTVar observation.reverseReceiveHeld True
                    pure True
                  else pure False
          when held $ do
            readMVar observation.releaseReceive
            -- The source closes this socket while this already-read frame is
            -- still outside admission. Drop the closed socket's buffered frame;
            -- the replacement connection must deliver the retained item.
            ioError (userError "Step-16 injected loss after both marker writes and before receive admission")

awaitBothMarkerWrites :: MarkerObservation -> IO (PeerBinding, Set.Set MarkerKey)
awaitBothMarkerWrites observation =
  within "both real marker writes before acknowledgement" $ atomically $ do
    held <- readTVar observation.firstReceiveHeld
    written <- readTVar observation.sourceWriteHeld
    markers <- readTVar observation.affected
    binding <- readTVar observation.sourceBinding
    reverseWritten <- readTVar observation.reverseWriteHeld
    reverseReceived <- readTVar observation.reverseReceiveHeld
    reverseBinding <- readTVar observation.reverseBinding
    check (held && written && reverseWritten && reverseReceived)
    case (binding, reverseBinding) of
      (Just forward, Just backward) -> do
        -- One TCP socket is duplex. Freeze its reverse marker as well so the
        -- affected set is known before the close, rather than inferred from
        -- whichever items happen to be repaired afterward.
        check (peerBindingSelectedCandidate forward == peerBindingSelectedCandidate backward)
        pure (forward, markers)
      _ -> retry

markerIdentity :: Peer.PeerEnvelope -> Maybe (MarkerKind, Peer.DisappearanceProbeIdDto)
markerIdentity = \case
  Peer.PeerPublicationEnvelope _ (Peer.PeerDisappearanceProbeMarkerDto _ _ _ (Peer.DisappearanceProbeMarkerDto probe _ _ _)) -> Just (PublicationMarker, probe)
  Peer.PeerControlEnvelope _ (Peer.AlignmentControlDto (Peer.AlignmentProbeMarkerDto probe _ _)) -> Just (AlignmentMarker, probe)
  _ -> Nothing

keyHasSubscription :: Peer.AlignmentSubscriptionIdDto -> MarkerKey -> Bool
keyHasSubscription subscription (MarkerKey _ _ kind bytes) =
  kind == AlignmentMarker && case PeerCodec.decodePeerEnvelope bytes of
    Right (Peer.PeerControlEnvelope _ (Peer.AlignmentControlDto (Peer.AlignmentProbeMarkerDto _ actual _))) -> actual == subscription
    _ -> False

sameProbe :: D.DisappearanceProbeId -> Peer.DisappearanceProbeIdDto -> Bool
sameProbe probe dto = D.disappearanceProbeIdBytes probe == Peer.disappearanceProbeDigestClaimBytes (Peer.disappearanceProbeIdDigest dto)

awaitExactlyOneCarrier :: String -> EAPP.Application -> PredefinedAccess -> PrivateUniqueId -> IO ()
awaitExactlyOneCarrier name application access object = do
  latestCount <- newTVarIO Nothing
  let loop = do
        call <- EAPP.read application (subjectQuery access object)
        values <-
          awaitStep15ApplicationCallWithin (name <> " marker subject read") call >>= \case
            EAPP.ApplicationCallSucceeded (ReadCompleted found) -> pure found
            other -> assertFailure (name <> " marker subject read failed: " <> show other)
        let count = length values
        atomically (writeTVar latestCount (Just count))
        if count == 1 then pure () else threadDelay 10_000 >> loop
  timeout causalWaitTimeoutMicroseconds loop >>= \case
    Just () -> pure ()
    Nothing -> do
      observed <- atomically (readTVar latestCount)
      assertFailure (name <> " subject visible exceeded its test failure bound; latest observed value count=" <> show observed)

takeCarrier :: String -> EAPP.Application -> PredefinedAccess -> PrivateUniqueId -> IO ()
takeCarrier name application access object = do
  call <- EAPP.localTake application (subjectQuery access object)
  awaitStep15ApplicationCallWithin (name <> " marker subject LocalTake") call >>= \case
    EAPP.ApplicationCallSucceeded (LocalTakeCompleted values) -> assertEqual (name <> " removes the sole visible subject") 1 (length values)
    other -> assertFailure (name <> " marker subject LocalTake failed: " <> show other)

subjectQuery :: PredefinedAccess -> PrivateUniqueId -> ApplicationQuery
subjectQuery access object =
  ApplicationQuery
    (Set.singleton (predefinedReader access))
    (QueryCompare (ApplicationProjection (NonEmpty.singleton "object_id")) ScalarEqual (QueryUniqueId object))

awaitResolvedAtEveryHerald :: Step14Deployment -> MarkerObservation -> IO D.DisappearanceProbeId
awaitResolvedAtEveryHerald deployment observation =
  timeout causalWaitTimeoutMicroseconds loop >>= \case
    Just probe -> pure probe
    Nothing -> do
      traces <- traverse snapshotStep14HeraldTrace (deploymentHeralds deployment)
      writes <- atomically (readTVar observation.writes)
      let markerCounts = Map.fromListWith (+) [((source, destination, kind), 1 :: Int) | MarkerWrite (MarkerKey source destination kind _) _ _ <- writes]
          h1Submissions = case traces of
            h1 : _ -> oracleSubmissionSteps h1
            [] -> []
      assertFailure ("every Herald applies the same Resolve exceeded its test failure bound; actual projections=" <> show (fmap projectionEvents traces) <> "; physical marker writes=" <> show markerCounts <> "; H1 Oracle-submission kernel steps=" <> show h1Submissions)
  where
    loop = do
      traces <- traverse snapshotStep14HeraldTrace (deploymentHeralds deployment)
      let resolved = fmap (nub . resolvedProbes) traces
      case resolved of
        [[probe], [second], [third], [fourth]] | all (== probe) [second, third, fourth] -> pure probe
        _ -> threadDelay 10_000 >> loop

projectionEvents :: [RuntimeTraceEvent] -> [OracleProjectionEventView]
projectionEvents events =
  [ oracleProjectionEventView event
  | entry <- entries,
    event <- appliedEntryProjectionEvents entry
  ]
  where
    entries =
      nub
        [ canonicalAppliedOracleEntryValue entry
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          OracleInput (OracleEntriesReceived _ received) <- [inputBody input],
          entry <- NonEmpty.toList received
        ]

resolvedProbes :: [RuntimeTraceEvent] -> [D.DisappearanceProbeId]
resolvedProbes events = [probe | DisappearanceProbeResolvedView probe _ <- projectionEvents events]

markerOutcomeHistory :: MarkerKey -> [RuntimeTraceEvent] -> [String]
markerOutcomeHistory (MarkerKey _ destination _ bytes) events =
  case outcomeOrdinals of
    [] -> []
    first : remaining ->
      [ show (ordinal, body)
      | (ordinal, body) <- inputs,
        ordinal >= first,
        ordinal <= foldr max first remaining,
        relevant body
      ]
  where
    inputs = [(ordinal, inputBody input) | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events]
    outcomeOrdinals = [ordinal | (ordinal, body) <- inputs, isMarkerOutcome body]
    isMarkerOutcome = \case
      RuntimeObserved (PeerDispatchObserved attempt _) ->
        case projectPeerLogicalAttempt attempt of
          Right envelope -> encodePeerEnvelope envelope == bytes
          Left _ -> False
      _ -> False
    relevant body =
      isMarkerOutcome body || case body of
        PeerInput (PeerHelloReceived {}) -> True
        PeerInput (SemanticPeerControlReceived binding (PeerStreamResumeOffered _)) -> peerBindingRemoteHeraldEpoch binding == destination
        _ -> False

assertExactlyOneReportAndResolve :: D.DisappearanceProbeId -> [[RuntimeTraceEvent]] -> Assertion
assertExactlyOneReportAndResolve probe traces = do
  reports <- case traces of
    first : _ -> pure [claim | PredefinedAbsenceReportedView claim <- projectionEvents first, D.disappearanceEvidenceClaimProbeId claim == probe]
    [] -> assertFailure "no final Herald traces"
  assertEqual "one committed absence Report per captured member" 4 (length reports)
  assertEqual "four distinct report owners" 4 (length (nub (fmap D.disappearanceEvidenceClaimReporter reports)))
  forM_ traces $ \events -> assertEqual "one committed Resolve at every Herald" [probe] (filter (== probe) (resolvedProbes events))
  let expectedReports = fmap (reportPredefinedAbsenceCommand probe) reports
      expectedResolve = resolveDisappearanceProbeCommand probe (completeDisappearanceEvidenceDigest reports)
      requests =
        nub
          [ canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)
          | events <- traces,
            ShellEvent _ (ShellEffectRouted _ _ (RunOracleClientAction (SubmitOracleRequest _ dispatch))) <- events
          ]
      intents command =
        nub
          [ (oracleEnvelopeHomeHeraldEpoch envelope, oracleEnvelopeRequestId envelope)
          | envelope <- requests,
            oracleEnvelopeCommand envelope == command
          ]
      resolveIntents = intents expectedResolve
  forM_ expectedReports $ \command -> assertEqual "repair allocates no duplicate Report intention" 1 (length (intents command))
  assertBool "the complete report set retains one to M Resolve intentions" (not (null resolveIntents) && length resolveIntents <= 4)
  assertEqual "each Herald retains at most one exact Resolve intention" (length resolveIntents) (length (nub (fmap fst resolveIntents)))

deploymentHeralds :: Step14Deployment -> [Step14Herald]
deploymentHeralds deployment = [step14H1 deployment, step14H2 deployment, step14H3 deployment, step14H4 deployment]

awaitMarkerSetupWave :: PeerWaveCapture -> IO ()
awaitMarkerSetupWave wave =
  timeout causalWaitTimeoutMicroseconds (awaitPeerWave wave) >>= \case
    Just () -> pure ()
    Nothing -> do
      pending <- snapshotPendingPeerWave wave
      assertFailure ("marker setup has outstanding finite peer work: " <> show pending)

-- Report the triggering immutable owner input together with its emitted Oracle
-- envelopes. Final projections alone cannot distinguish a legitimate late
-- evidence invalidation from an unrelated failure to complete marker repair.
oracleSubmissionSteps :: [RuntimeTraceEvent] -> [String]
oracleSubmissionSteps events =
  [ show (ordinal, inputBody input, submissions)
  | KernelEvent ordinal (KernelTraceStepped _ _ input (Right batch)) <- events,
    let submissions =
          [ canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)
          | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers batch
          ],
    not (null submissions)
  ]

within :: String -> IO a -> IO a
within name action = timeout causalWaitTimeoutMicroseconds action >>= maybe (assertFailure (name <> " exceeded its test failure bound")) pure
