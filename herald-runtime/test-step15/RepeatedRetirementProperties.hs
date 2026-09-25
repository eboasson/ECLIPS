{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | P05 schedules use ordinary EORC/EPRP and immutable runtime evidence. A
-- physical writer fence delays only the soon-to-be-retired H2 lane; Oracle and
-- the H1/H3 survivor lane continue independently.
module RepeatedRetirementProperties (tests) where

import Control.Concurrent (newEmptyMVar, readMVar, threadDelay, tryPutMVar)
import Control.Exception (SomeException, bracket, catch, displayException)
import Control.Monad (forM_, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access (ApplicationPredefinedSortRole (NeutralVertexRole), environmentAccessPredefined)
import Eclips.Application.Types.Result (RegularCallResult (NewEnvironmentCompleted, WriteCompleted))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import Eclips.Domain.Identity (heraldEpochBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
  )
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.EffectBatch (HeraldEffect (FinishIsolation, RunOracleClientAction), effectBatchMembers)
import Eclips.Herald.Input (HeraldInputBody (RuntimeObserved), RuntimeObservation (TimerObserved), inputBody)
import Eclips.Herald.OracleClient (OracleClientAction (WatchOracle))
import Eclips.Herald.Peer.RPC (admitTerminalSourceUnionOccurrenceClaim, projectPeerControl, projectPeerLogicalAttempt)
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (ShellEffectRouted),
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( setPeerEnvelopeProbeForTest,
    setPeerPublicationProbeForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types (PeerEnvelopeProbePhase (PeerEnvelopeReceiving, PeerEnvelopeWriting))
import Eclips.Oracle.Projection (OracleProjectionEventView (HeraldMembershipAdvancedView))
import Eclips.Oracle.Runtime (oracleRuntimeMembershipGeneration)
import Eclips.Oracle.Runtime.TCP (oracleTcpNodeStatuses)
import Eclips.Protocol.Peer.Types qualified as Protocol
import PeerEvidence (data SemanticSendPeerControl, data SemanticSendPeerItem)
import Step15Applications (awaitStep15ApplicationCallWithin, publishStep15NeutralCarrier, step15PredefinedAccess, withStep15Application, withStep15Applications)
import Step15Configuration (Step15HeraldName (..), step15HeraldEpoch)
import Step15Fixtures
  ( Step15Deployment,
    awaitStep15PeerMesh,
    awaitStep15PeerTopology,
    closeStep15HeraldPeerTransport,
    crashStep15H4,
    crashStep15Herald,
    openStep15HeraldPeerTransport,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldIsRunning,
    step15ManagedHeraldTcp,
    step15ManagedHeraldWorkLedger,
    withRepeatedRetirementDeployment,
    withRepeatedRetirementSelfFenceDeployment,
  )
import Step15WorkCounts
  ( Step15AttributableWork (step15IsolationFinishes),
    Step15WorkEvidence,
    awaitStep15WorkEvidenceSince,
    snapshotStep15WorkBaselines,
    snapshotStep15WorkEvidenceSince,
    step15KernelFaultEvidence,
    step15OracleProjectionViews,
    step15RuntimeEvidence,
    summarizeStep15WorkEvidence,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

data Fence = AfterEstablished | AtInventory | AtAnnouncement | AtAcceptance | AtEstablishedReceive
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "P05 repeated non-voter retirement"
    ( [ testCase (show fence <> ": retained newenv settles through two real retirements") (runSchedule fence)
      | fence <- [AfterEstablished, AtInventory, AtAnnouncement, AtAcceptance, AtEstablishedReceive]
      ]
        <> [testCase "two still-running removed epochs self-fence and cannot reopen" caseRepeatedSelfFencing]
    )

caseRepeatedSelfFencing :: Assertion
caseRepeatedSelfFencing = bounded "P05 repeated self-fencing" $ withRepeatedRetirementSelfFenceDeployment $ \deployment -> do
  awaitStep15PeerMesh deployment
  let heralds = [step15H1 deployment, step15H2 deployment, step15H3 deployment, step15H4 deployment]
      ledgers = fmap step15ManagedHeraldWorkLedger heralds
  (baseline, cursors) <- snapshotStep15WorkBaselines ledgers
  forM_ [(step15H4 deployment, [Step15H1, Step15H2, Step15H3]), (step15H2 deployment, [Step15H1, Step15H3])] $ \(removed, members) -> do
    assertEqual "the subject runtime is executing when transport is cut" True =<< step15ManagedHeraldIsRunning removed
    let removedIndex = if length members == 3 then 3 else 1
        diagnostic :: String -> IO value -> IO value
        diagnostic label action = action `catch` selfFenceFailure (show removedIndex <> ": " <> label) (fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers))
    _ <- diagnostic "removed Herald initially established an Oracle watch" (bounded "retirement subject Oracle watch" (awaitStep15WorkEvidenceSince cursors ledgers (\observed -> oracleWatchOpened (baseline !! removedIndex) || oracleWatchOpened (observed !! removedIndex))))
    diagnostic "close peer transport" (closeStep15HeraldPeerTransport removed)
    membership <- diagnostic "commit membership retirement" (awaitMembers deployment members)
    _ <-
      diagnostic "await autonomous semantic fence"
        $ bounded "retired Herald completes its application drain"
        $ awaitStep15WorkEvidenceSince
          cursors
          ledgers
          (\evidence -> step15IsolationFinishes (summarizeStep15WorkEvidence (evidence !! removedIndex)) == 1)
    observed <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
    assertBool
      "the executing epoch fences from its exact committed retirement or a fired isolation grace after losing Oracle authority"
      (membershipProjectedOn membership removedIndex observed || isolationFinishedFromTimer (observed !! removedIndex))
    openStep15HeraldPeerTransport removed
    assertEqual "retired epoch keeps its control shell alive after physical transport reopens" True =<< step15ManagedHeraldIsRunning removed
    let indexes = if length members == 3 then [0, 1, 2] else [0, 2]
    _ <- bounded "surviving established base after executing-host removal" (awaitStep15WorkEvidenceSince cursors ledgers (basesInstalled membership indexes))
    pure ()
  awaitStep15PeerTopology [(step15H1 deployment, [step15HeraldEpoch Step15H3]), (step15H3 deployment, [step15HeraldEpoch Step15H1])]
  withStep15Application Step15H1 (step15H1 deployment) 15_541 $ \application _ -> do
    result <- EAPP.newenv application >>= awaitStep15ApplicationCallWithin "P05 surviving application after self-fencing"
    case result of
      EAPP.ApplicationCallSucceeded (NewEnvironmentCompleted _) -> pure ()
      other -> assertFailure ("surviving application failed: " <> show other)
  evidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
  assertBool "all observed kernels preserve their invariants through both fences" (all (null . step15KernelFaultEvidence) evidence)

selfFenceFailure :: String -> IO [Step15WorkEvidence] -> SomeException -> IO result
selfFenceFailure stage snapshot exception = do
  evidence <- snapshot
  assertFailure
    ( "P05 self-fence stage "
        <> stage
        <> " failed: "
        <> displayException exception
        <> "; work="
        <> show (fmap summarizeStep15WorkEvidence evidence)
        <> "; projected memberships="
        <> show [[membership | HeraldMembershipAdvancedView membership <- step15OracleProjectionViews observed] | observed <- evidence]
        <> "; kernel faults="
        <> show (fmap step15KernelFaultEvidence evidence)
    )

-- The self-fence fixture has no application sessions to drain, so an applied
-- timer input producing FinishIsolation identifies the quorum-loss grace path.
-- This is immutable owner trace evidence, not a read of runtime kernel state.
isolationFinishedFromTimer :: Step15WorkEvidence -> Bool
isolationFinishedFromTimer evidence =
  not
    ( null
        [ ()
        | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- step15RuntimeEvidence evidence,
          RuntimeObserved (TimerObserved _ _) <- [inputBody input],
          FinishIsolation _ <- effectBatchMembers effects
        ]
    )

oracleWatchOpened :: Step15WorkEvidence -> Bool
oracleWatchOpened evidence =
  not
    ( null
        [ ()
        | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- step15RuntimeEvidence evidence,
          RunOracleClientAction (WatchOracle _ _) <- effectBatchMembers effects
        ]
    )

runSchedule :: Fence -> Assertion
runSchedule fence = bounded "P05 repeated-retirement schedule" $ do
  (first, second, evidence, retainedSourceClaims) <- withRepeatedRetirementDeployment $ \deployment -> do
    let h1 = step15H1 deployment
        h2 = step15H2 deployment
        h3 = step15H3 deployment
        heralds = [h1, h2, h3, step15H4 deployment]
        ledgers = fmap step15ManagedHeraldWorkLedger heralds
    (_, diagnosticCursors) <- snapshotStep15WorkBaselines ledgers
    stage <- newIORef "peer mesh"
    let atStage :: String -> IO value -> IO value
        atStage label action = writeIORef stage label >> action
        snapshot = fmap (fmap snd) (snapshotStep15WorkEvidenceSince diagnosticCursors ledgers)
        diagnostic action =
          action `catch` \exception -> do
            label <- readIORef stage
            repeatedRetirementFailure fence label snapshot exception
    -- Keep the failure handler inside the live deployment bracket. The outer
    -- deadline can then name the active phase without dumping complete traces.
    diagnostic $ do
      atStage "peer mesh" (awaitStep15PeerMesh deployment)
      (_, cursors) <- snapshotStep15WorkBaselines ledgers
      atStage "application session startup" $ withStep15Applications deployment $ \application _startup h2Application h2Startup h4Application h4Startup -> do
        -- Both soon-to-be-retired sources publish genuine G0-stamped work. H2's
        -- immutable occurrence must remain usable when its retirement is closed
        -- from either a G0 or an already-established G1 Graph anchor.
        forM_ [("H2", h2Application, h2Startup), ("H4", h4Application, h4Startup)] $ \(name, sourceApplication, startup) -> do
          atStage (name <> " source publication") (pure ())
          access <- step15PredefinedAccess name NeutralVertexRole startup
          (_, publication) <- publishStep15NeutralCarrier (name <> " retained source") sourceApplication startup access
          assertEqual
            (name <> " structural publication is accepted before either retirement")
            (EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted))
            =<< awaitStep15ApplicationCallWithin (name <> " retained source write") publication
        prehistory <-
          atStage "both retired-source occurrence claims"
            $ bounded
              "both retired-source occurrence claims"
              (awaitStep15WorkEvidenceSince cursors ledgers (\observed -> all (not . null . neutralClaims . (observed !!)) [1, 3]))
        sourceClaims <- traverse (sourceClaim prehistory) [(Step15H2, 1), (Step15H4, 3)]
        -- The source accepts all environment roots while one original delivery
        -- destination remains pending. The same call must survive both changes.
        withHeldEnvironment deployment $ \awaitEnvironment releaseEnvironment -> do
          pending <- atStage "submit retained newenv" (EAPP.newenv application)
          atStage "held environment physical handoff" awaitEnvironment
          (first, second) <- withFence fence deployment $ \awaitFence releaseFence -> do
            atStage "crash H4" (crashStep15H4 deployment)
            releaseEnvironment
            first <- atStage "first Oracle retirement" (awaitMembers deployment [Step15H1, Step15H2, Step15H3])
            case fence of
              AfterEstablished -> void (atStage "first established base" $ bounded "first established base" (awaitStep15WorkEvidenceSince cursors ledgers (basesInstalled first [0, 1, 2])))
              AtEstablishedReceive -> do
                atStage ("first closure fence " <> show fence) awaitFence
                beforeSecond <- atStage "H1/H2 first established base while H3 is fenced" $ bounded "H1/H2 installed G1 while H3 receive is fenced" (awaitStep15WorkEvidenceSince cursors ledgers (basesInstalled first [0, 1]))
                assertBool "H3 remains at its old Graph anchor before the second retirement" (not (basesInstalled first [2] beforeSecond))
              _ -> do
                atStage ("first closure fence " <> show fence) awaitFence
                beforeSecond <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
                assertEqual
                  "the fenced first closure has no G1 Established certificate before the second crash"
                  []
                  [ ()
                  | observed <- beforeSecond,
                    Protocol.TerminalSourceUnionEstablishedControlDto (Protocol.TerminalSourceUnionEstablishedDto (Protocol.TerminalSourceUnionDto _ target _ _ _) _) <- controls observed,
                    Protocol.heraldMembershipGenerationClaimBytes target == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId first)
                  ]
            atStage "crash H2" (crashStep15Herald h2)
            second <-
              if fence == AtEstablishedReceive
                then do
                  current <- atStage "second Oracle retirement while H3 is fenced" (awaitMembers deployment [Step15H1, Step15H3])
                  projected <- atStage "H3 projects second retirement while first control is fenced" $ bounded "H3 projects G2 before releasing its delayed G1 control" (awaitStep15WorkEvidenceSince cursors ledgers (membershipProjectedOn current 2))
                  assertBool "H3 emitted no G1 structural report before projecting G2" (not (basesInstalled first [2] projected))
                  releaseFence
                  releaseEnvironment
                  pure current
                else do
                  -- The delayed writer children are joined before release, so no
                  -- held H2 frame can acquire a replacement identity.
                  releaseFence
                  releaseEnvironment
                  atStage "second Oracle retirement" (awaitMembers deployment [Step15H1, Step15H3])
            pure (first, second)
          atStage "surviving peer topology"
            $ awaitStep15PeerTopology
              [(h1, [step15HeraldEpoch Step15H3]), (h3, [step15HeraldEpoch Step15H1])]
          environment <-
            atStage "original newenv completion" (awaitStep15ApplicationCallWithin "P05 original newenv" pending) >>= \case
              EAPP.ApplicationCallSucceeded (NewEnvironmentCompleted value) -> pure value
              other -> assertFailure ("P05 newenv did not settle: " <> show other)
          assertEqual "all six environment roles survive" 6 (length (environmentAccessPredefined environment))
          _ <- atStage "second established base" $ bounded "second established base" (awaitStep15WorkEvidenceSince cursors ledgers (basesInstalled second [0, 2]))
          snapshots <- snapshotStep15WorkEvidenceSince cursors ledgers
          atStage "application session cleanup" (pure (first, second, fmap snd snapshots, sourceClaims))
  assertEqual "second retirement retains the exact first generation" (Just (heraldMembershipGenerationId first)) (heraldMembershipGenerationPredecessor second)
  assertEqual "surviving epochs are exact" (sort (fmap step15HeraldEpoch [Step15H1, Step15H3])) (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs second)))
  assertBool "no kernel invariant fault in the complete observed interval" (all (null . step15KernelFaultEvidence) evidence)
  let finalUnions = [union | observed <- evidence, Protocol.TerminalSourceUnionEstablishedControlDto (Protocol.TerminalSourceUnionEstablishedDto union@(Protocol.TerminalSourceUnionDto _ target _ _ _) _) <- controls observed, Protocol.heraldMembershipGenerationClaimBytes target == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId second)]
      retiredNames = if fence `elem` [AfterEstablished, AtEstablishedReceive] then [Step15H2] else [Step15H2, Step15H4]
      expected = sort (fmap (heraldEpochBytes . step15HeraldEpoch) retiredNames)
  assertBool "the final wire union is observed" (not (null finalUnions))
  forM_ finalUnions $ \union@(Protocol.TerminalSourceUnionDto _ _ retired _ _) -> do
    assertEqual "the final union seals the exact unfinished retired origins" expected (fmap Protocol.heraldEpochClaimBytes (NonEmpty.toList (Protocol.retiredHeraldEntries retired)))
    forM_ [(name, occurrence, digest) | (name, occurrence, digest) <- retainedSourceClaims, name `elem` retiredNames] $ \(name, occurrence, digest) ->
      case admitTerminalSourceUnionOccurrenceClaim union occurrence digest of
        Left problem -> assertFailure ("final union occurrence authentication failed: " <> show problem)
        Right present -> assertBool (show name <> " retains its exact original occurrence and publication digest in the final union") present

repeatedRetirementFailure :: Fence -> String -> IO [Step15WorkEvidence] -> SomeException -> IO result
repeatedRetirementFailure fence stage snapshot exception = do
  evidence <- snapshot
  assertFailure
    ( "P05 repeated-retirement "
        <> show fence
        <> " stage "
        <> stage
        <> " failed: "
        <> displayException exception
        <> "; work="
        <> show (fmap summarizeStep15WorkEvidence evidence)
        <> "; latest projected memberships (up to four per Herald)="
        <> show [take 4 (reverse [membership | HeraldMembershipAdvancedView membership <- step15OracleProjectionViews observed]) | observed <- evidence]
        <> "; kernel faults (up to two per Herald)="
        <> show (fmap (take 2 . step15KernelFaultEvidence) evidence)
    )

sourceClaim ::
  [Step15WorkEvidence] ->
  (Step15HeraldName, Int) ->
  IO (Step15HeraldName, Protocol.StructuralOccurrenceIdDto, Protocol.StructuralPublicationDigestClaim)
sourceClaim evidence (name, index) =
  case nub (neutralClaims (evidence !! index)) of
    [(occurrence@(Protocol.StructuralOccurrenceIdDto source _), digest)] -> do
      assertEqual "the retained claim names the original source epoch" (heraldEpochBytes (step15HeraldEpoch name)) (Protocol.heraldEpochClaimBytes source)
      pure (name, occurrence, digest)
    claims -> assertFailure ("expected exactly one original " <> show name <> " neutral occurrence, got " <> show claims)

neutralClaims :: Step15WorkEvidence -> [(Protocol.StructuralOccurrenceIdDto, Protocol.StructuralPublicationDigestClaim)]
neutralClaims evidence =
  [ (occurrence, digest)
  | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <- step15RuntimeEvidence evidence,
    Right
      (Protocol.PeerPublicationEnvelope _ (Protocol.PeerPublicationDto _ _ _ (Protocol.StructuralPublicationDto (Protocol.StructuralOccurrenceStampDto occurrence _ _ digest Protocol.NeutralVertexCarrierDto) _))) <-
      [projectPeerLogicalAttempt attempt]
  ]

withHeldEnvironment :: Step15Deployment -> (IO () -> IO () -> IO result) -> IO result
withHeldEnvironment deployment use = do
  reached <- newEmptyMVar
  release <- newEmptyMVar
  let tcp = step15ManagedHeraldTcp (step15H1 deployment)
      unhold = void (tryPutMVar release ())
      probe binding _ = when (peerBindingRemoteHeraldEpoch binding == step15HeraldEpoch Step15H4) (void (tryPutMVar reached ()) >> readMVar release)
  bracket
    (setPeerPublicationProbeForTest tcp (Just probe))
    (const (unhold >> setPeerPublicationProbeForTest tcp Nothing))
    (const (use (bounded "accepted environment physical handoff" (readMVar reached)) unhold))

withFence :: Fence -> Step15Deployment -> (IO () -> IO () -> IO result) -> IO result
withFence AfterEstablished _ use = use (pure ()) (pure ())
withFence fence deployment use = do
  reached <- newEmptyMVar
  release <- newEmptyMVar
  let (local, remote, wantedPhase) = case fence of
        AtAnnouncement -> (step15H1 deployment, Step15H2, PeerEnvelopeWriting)
        AtEstablishedReceive -> (step15H3 deployment, Step15H1, PeerEnvelopeReceiving)
        _ -> (step15H2 deployment, Step15H1, PeerEnvelopeWriting)
      tcp = step15ManagedHeraldTcp local
      unhold = void (tryPutMVar release ())
      probe phase binding envelope =
        when
          (phase == wantedPhase && peerBindingRemoteHeraldEpoch binding == step15HeraldEpoch remote && matches envelope)
          (void (tryPutMVar reached ()) >> readMVar release)
      matches = \case
        Protocol.PeerControlEnvelope _ Protocol.TerminalSourceInventoryAdvertisedDto {} -> fence == AtInventory
        Protocol.PeerControlEnvelope _ Protocol.TerminalSourceUnionAnnouncedDto {} -> fence == AtAnnouncement
        Protocol.PeerControlEnvelope _ Protocol.TerminalSourceUnionAcceptedDto {} -> fence == AtAcceptance
        Protocol.PeerControlEnvelope _ Protocol.TerminalSourceUnionEstablishedControlDto {} -> fence == AtEstablishedReceive
        _ -> False
  bracket
    (setPeerEnvelopeProbeForTest tcp (Just probe))
    (const (unhold >> setPeerEnvelopeProbeForTest tcp Nothing))
    (const (use (bounded ("first closure fence " <> show fence) (readMVar reached)) unhold))

awaitMembers :: Step15Deployment -> [Step15HeraldName] -> IO HeraldMembershipGeneration
awaitMembers deployment names = bounded ("Oracle membership " <> show names) loop
  where
    expected = sort (fmap step15HeraldEpoch names)
    loop = do
      statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
      case nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses) of
        [membership] | sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)) == expected -> pure membership
        _ -> threadDelay 10_000 >> loop

membershipProjectedOn :: HeraldMembershipGeneration -> Int -> [Step15WorkEvidence] -> Bool
membershipProjectedOn membership index evidence =
  any (\case HeraldMembershipAdvancedView observed -> observed == membership; _ -> False) (step15OracleProjectionViews (evidence !! index))

-- Non-announcers offer their cumulative report only after the successor base
-- installs. The announcer retains its own report locally, so its witness is the
-- exact Established certificate emitted by a successful kernel transition. That
-- certificate includes its checked local terminal-vector/control acceptance;
-- completeTerminalSourceUnionIfReady installs the base before returning effects.
-- Waiting for a later topology announcement would incorrectly require H3 to
-- unfreeze in the AtEstablishedReceive schedule.
basesInstalled :: HeraldMembershipGeneration -> [Int] -> [Step15WorkEvidence] -> Bool
basesInstalled membership indexes evidence = all installed indexes
  where
    generation = heraldMembershipGenerationIdBytes (heraldMembershipGenerationId membership)
    announcer = NonEmpty.head (heraldMembershipGenerationActiveHeraldEpochs membership)
    local index = step15HeraldEpoch ([Step15H1, Step15H2, Step15H3, Step15H4] !! index)
    installed index =
      any reportMatches (controls (evidence !! index))
        || (local index == announcer && any establishmentMatches (kernelControls (evidence !! index)))
    reportMatches = \case
      Protocol.StructuralAppliedReportedDto (Protocol.StructuralAppliedReportDto _ vector _) ->
        Protocol.heraldMembershipGenerationClaimBytes (Protocol.structuralVersionVectorMembershipGeneration vector) == generation
      _ -> False
    establishmentMatches = \case
      Protocol.TerminalSourceUnionEstablishedControlDto (Protocol.TerminalSourceUnionEstablishedDto (Protocol.TerminalSourceUnionDto _ target _ _ _) _) ->
        Protocol.heraldMembershipGenerationClaimBytes target == generation
      _ -> False

kernelControls :: Step15WorkEvidence -> [Protocol.PeerControlDto]
kernelControls evidence =
  [ control
  | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- step15RuntimeEvidence evidence,
    SemanticSendPeerControl _ value <- effectBatchMembers effects,
    Right (Protocol.PeerControlEnvelope _ control) <- [projectPeerControl value]
  ]

controls :: Step15WorkEvidence -> [Protocol.PeerControlDto]
controls evidence =
  [ control
  | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl _ value)) <- step15RuntimeEvidence evidence,
    Right (Protocol.PeerControlEnvelope _ control) <- [projectPeerControl value]
  ]

bounded :: String -> IO value -> IO value
bounded label action = timeout 30_000_000 action >>= maybe (assertFailure (label <> " timed out")) pure
