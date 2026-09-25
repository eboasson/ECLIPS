{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A failure-sensitive, serial real-network tour. Its observations are EAPP
-- results, immutable Oracle watch entries and routed/physical work evidence.
module Step16TourProperties (tests) where

import Control.Monad (forM_)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (EdgeRole, NeutralVertexRole, SortDefinitionRole),
    EnvironmentAccess,
    allApplicationPredefinedSortRoles,
    environmentAccessPredefined,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity qualified as Application
import Eclips.Application.Types.Result (RegularCallResult (WriteCompleted))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import Eclips.Domain.Disappearance
  ( DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubjectView (..),
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceProbeIdBytes,
    disappearanceResolutionOutcomeView,
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity (HeraldEpoch, controlIndexWord64, heraldEpochBytes, nablaIdBytes, sortIdBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
  )
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.SortOccurrence (deriveSortDefinitionOccurrenceId, resolvedRetirementOccurrenceBase)
import Eclips.Domain.Startup
  ( AppliedRootRole (WriterRoot),
    appliedProcessEpochId,
    appliedProcessRoots,
    appliedRootCatalogueRole,
    appliedRootRole,
  )
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.EffectBatch (HeraldEffect (RunOracleClientAction), effectBatchMembers)
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput, PeerInput),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (AssociateOracleSubmissionRetry, CancelOracleRetry, ReleaseDeferredOracleSubmission, ScheduleOracleRetry, SubmitOracleRequest),
    OracleClientIngress (OracleEntriesReceived, OracleRetryElapsed, OracleSubmissionNotReadyReceived),
    oracleRequestDispatchEnvelope,
  )
import Eclips.Herald.Peer.RPC (projectPeerControl, projectPeerLogicalAttempt)
import Eclips.Herald.PeerDispatch (peerLogicalDispatchAttemptItem)
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (ShellEffectRouted),
  )
import Eclips.Herald.Runtime.TCP.Internal.Types (OracleManagerProbeEvent (OraclePhysicalConnectAttempt, OraclePhysicalRedirect, OraclePhysicalSubmitAttempt, OracleRetryPaced))
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue)
import Eclips.Oracle.Command
  ( invalidateDisappearanceProbeCommand,
    openDisappearanceProbeCommand,
    oracleEnvelopeCommand,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
    reportPredefinedAbsenceCommand,
    resolveDisappearanceProbeCommand,
  )
import Eclips.Oracle.Disappearance
  ( DisappearanceProbeInvalidationView (CommandDisappearanceInvalidation),
    completeDisappearanceEvidenceDigest,
    projectedDisappearanceProbeHeaderCoordinate,
    projectedDisappearanceProbeHeaderId,
    projectedDisappearanceProbeHeaderMembership,
    projectedDisappearanceProbeHeaderSubject,
  )
import Eclips.Oracle.Genesis (checkedOracleAppliedBootstraps, checkedOracleSystemId)
import Eclips.Oracle.Projection (OracleProjectionEventView (..))
import Eclips.Protocol.Peer.Types qualified as Protocol
import PeerEvidence (data SemanticPeerControlReceived, data SemanticPeerPublicationReceived, data SemanticSendPeerControl, data SemanticSendPeerItem)
import Step15Applications
  ( awaitStep15ApplicationCallWithin,
    publishStep15NeutralCarrier,
    step15PredefinedAccess,
    withStep15Applications,
  )
import Step15Configuration (Step15HeraldName (..), step15HeraldEpoch)
import Step15CrashProperties (awaitH4Retirement, currentMembership)
import Step15Fixtures
  ( awaitStep15PeerMesh,
    awaitStep15SurvivorMesh,
    crashStep15H4,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldWorkLedger,
    withStep15CrashDeployment,
  )
import Step15Genesis (step15CheckedOracleGenesis, step15H1ProcessEpochId)
import Step15WorkCounts
  ( Step15AttributableWork (..),
    Step15WorkCursor,
    Step15WorkEvidence,
    Step15WorkLedger,
    awaitStep15WorkEvidenceSince,
    snapshotStep15WorkBaselines,
    snapshotStep15WorkEvidenceSince,
    step15OracleActionEvidence,
    step15OracleManagerEvidence,
    step15OracleProjectionViews,
    step15RuntimeEvidence,
    summarizeStep15WorkEvidence,
  )
import Step16PeerWave (pendingFinitePeerWave)
import Step16TourFixtures
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Step-16 four-Herald disappearance tour"
    [ testCase
        "seed-162002: accepted newenv survives retirement, both disappearance families resolve, and successor newenv stays private"
        caseFourHeraldDisappearanceTour
    ]

caseFourHeraldDisappearanceTour :: Assertion
caseFourHeraldDisappearanceTour = do
  completed <- timeout 25_000_000 $ withStep15CrashDeployment $ \deployment -> do
    awaitStep15PeerMesh deployment
    withStep15Applications deployment $ \appP startupP appQ startupQ _appR _startupR -> do
      let ledgers =
            fmap
              step15ManagedHeraldWorkLedger
              [step15H1 deployment, step15H2 deployment, step15H3 deployment, step15H4 deployment]
      (_, cursors) <- snapshotStep15WorkBaselines ledgers
      pNeutral <- step15PredefinedAccess "P" NeutralVertexRole startupP
      (pObject, publish) <- publishStep15NeutralCarrier "neutral prehistory" appP startupP pNeutral
      awaitStep15ApplicationCallWithin "neutral prehistory publication" publish >>= \case
        EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
        other -> assertFailure ("neutral prehistory publication: " <> show other)
      -- The later completed environment/successor wave is the causal setup
      -- boundary before this subject's final LocalTake, without a time delay.
      (_, manifestCursors) <- snapshotStep15WorkBaselines ledgers
      predecessor <- currentMembership deployment
      (acceptedEnvironment, successor, acceptedPrefix) <-
        withHeldH1EnvironmentDestination deployment $ \gate -> do
          accepted <- EAPP.newenv appP
          -- The callback is below admission, manifest allocation and stream
          -- retention. H4 cannot receive the held root or ACK its destination.
          heldStamp <- awaitHeldH1EnvironmentDestination gate
          acceptedPrefix <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince manifestCursors ledgers)
          acceptedH1 <- h1Evidence acceptedPrefix
          assertBool
            "the exact held H4 frame belongs to the accepted manifest's source prefix"
            (heldStamp `elem` structuralStamps acceptedH1)
          crashStep15H4 deployment
          releaseHeldH1EnvironmentDestination gate
          awaitStep15SurvivorMesh deployment
          successor <- awaitH4Retirement deployment predecessor
          environment <- awaitTourEnvironment "accepted predecessor newenv completes after H4 retirement" accepted
          pure (environment, successor, acceptedH1)
      assertEqual
        "retirement has exactly the original predecessor"
        (Just (heraldMembershipGenerationId predecessor))
        (heraldMembershipGenerationPredecessor successor)
      assertEnvironment "accepted newenv" acceptedEnvironment
      newenvCompletion <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince manifestCursors ledgers)
      completedH1 <- h1Evidence newenvCompletion
      assertAcceptedLineage predecessor successor acceptedPrefix completedH1 newenvCompletion
      awaitSuccessorWave cursors ledgers

      -- P has the sole normal destination for its neutral writer in this
      -- genesis. H2/H3 retain the actual system-view structural copies, which
      -- the disappearance barrier must classify and purge on Resolve.
      pTaken <- takeTourObject "P final neutral LocalTake" appP pNeutral pObject
      assertEqual "P removes one exact controlled carrier" 1 (length pTaken)
      (controlledProbe, controlledOutcome) <- awaitResolution cursors ledgers isControlled
      assertResolvedMembership successor controlledProbe controlledOutcome
        =<< fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)

      pSort <- step15PredefinedAccess "P" SortDefinitionRole startupP
      qSort <- step15PredefinedAccess "Q" SortDefinitionRole startupQ
      let definition = tourDefinition "step16_retired"
      regularSort <- publishTourDefinition "first regular definition" appP pSort definition
      awaitSuccessorWave cursors ledgers
      awaitTourDefinition "Q receives regular definition" appQ qSort regularSort
      qDefinition <- takeTourDefinition "Q final definition LocalTake" appQ qSort regularSort
      pDefinition <- takeTourDefinition "P final definition LocalTake" appP pSort regularSort
      assertEqual "Q removes exact regular definition" 1 (length qDefinition)
      assertEqual "P removes exact regular definition" 1 (length pDefinition)
      (regularProbe, regularOutcome) <- awaitResolution cursors ledgers (isRegular regularSort)
      assertResolvedMembership successor regularProbe regularOutcome
        =<< fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      equalSort <- publishTourDefinition "equal descriptor after retirement" appP pSort definition
      assertEqual "equal redefinition preserves the content-addressed SortId" regularSort equalSort
      awaitSuccessorWave cursors ledgers
      awaitTourDefinition "Q receives the fresh equal definition" appQ qSort equalSort
      afterRedefinition <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      assertFreshOccurrence regularOutcome afterRedefinition

      (_, privateCursors) <- snapshotStep15WorkBaselines ledgers
      privateEnvironment <- EAPP.newenv appP >>= awaitTourEnvironment "fresh successor newenv"
      privateEvidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince privateCursors ledgers)
      assertEqual
        "newenv requires no physical Oracle submission"
        []
        [attempt | evidence <- privateEvidence, attempt@OraclePhysicalSubmitAttempt {} <- step15OracleManagerEvidence evidence]
      assertEnvironment "fresh successor newenv" privateEnvironment
      assertDistinctEnvironments acceptedEnvironment privateEnvironment
      privateSort <- tourEnvironmentRole SortDefinitionRole privateEnvironment
      privateEdge <- tourEnvironmentRole EdgeRole privateEnvironment
      connectTourPrivatePair appP (startupAccessProcess startupP) privateEdge privateSort
      awaitSuccessorWave cursors ledgers
      secretSort <- publishTourDefinition "private successor definition" appP privateSort (tourDefinition "step16_private")
      awaitSuccessorWave cursors ledgers
      awaitTourDefinition "private writer reaches its private reader" appP privateSort secretSort
      assertEqual "another process has no path to the private environment" []
        =<< readTourDefinition "Q cannot read P's successor environment" appQ qSort secretSort
      assertEqual "newenv adds no bridge to P's original environment" []
        =<< readTourDefinition "P original environment remains separate" appP pSort secretSort
      pure (cursors, ledgers, controlledProbe, regularProbe)

  -- withStep15CrashDeployment has closed and joined every application, Herald,
  -- TCP writer and Oracle scope before these final interval ledgers are read.
  (cursors, ledgers, controlledProbe, regularProbe) <-
    maybe (assertFailure "four-Herald disappearance tour exceeded the existing terminal-tour guard") pure completed
  joinedEvidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
  assertClosedWorkBounds joinedEvidence
  assertDisappearanceWork [controlledProbe, regularProbe] joinedEvidence
  let allViews = concatMap step15OracleProjectionViews joinedEvidence
      openedProbes = nub [projectedDisappearanceProbeHeaderId header | DisappearanceProbeOpenedView header _ <- allViews]
  assertEqual
    "the two disappearance subjects allocate exactly two live probes"
    (sort [controlledProbe, regularProbe])
    (sort openedProbes)
  assertEqual
    "the loss-free successor probes need neither abort nor invalidation"
    []
    [view | view <- allViews, isUnexpectedTerminal view]
  forM_ (take 3 joinedEvidence) $ \evidence -> do
    let resolved = [probe | DisappearanceProbeResolvedView probe _ <- step15OracleProjectionViews evidence]
    assertBool
      "each surviving Herald projects both exact terminal probes before join"
      (controlledProbe `elem` resolved && regularProbe `elem` resolved)
  where
    isUnexpectedTerminal DisappearanceProbeInvalidatedView {} = True
    isUnexpectedTerminal DisappearanceProbeAbortedView {} = True
    isUnexpectedTerminal _ = False

assertEnvironment :: String -> EnvironmentAccess -> Assertion
assertEnvironment label environment = do
  let accesses = environmentAccessPredefined environment
  assertEqual
    (label <> " has the exact ordered six-role catalogue")
    allApplicationPredefinedSortRoles
    (fmap predefinedAccessRole accesses)
  assertEqual (label <> " writers are private distinct handles") 6 (length (nub (fmap predefinedWriter accesses)))
  assertEqual (label <> " readers are private distinct handles") 6 (length (nub (fmap predefinedReader accesses)))

h1Evidence :: [Step15WorkEvidence] -> IO Step15WorkEvidence
h1Evidence [h1, _, _, _] = pure h1
h1Evidence evidence = assertFailure ("expected exactly four ordered Herald ledgers, got " <> show (length evidence))

assertDistinctEnvironments :: EnvironmentAccess -> EnvironmentAccess -> Assertion
assertDistinctEnvironments first second = do
  let left = environmentAccessPredefined first
      right = environmentAccessPredefined second
  assertBool
    "successor newenv allocates fresh private writers"
    (all (`notElem` fmap predefinedWriter left) (fmap predefinedWriter right))
  assertBool
    "successor newenv allocates fresh private readers"
    (all (`notElem` fmap predefinedReader left) (fmap predefinedReader right))

isControlled :: DisappearanceResolutionOutcome -> Bool
isControlled outcome = case disappearanceResolutionOutcomeView outcome of
  ControlledDisappearanceResolved {} -> True
  _ -> False

isRegular :: Application.SortId -> DisappearanceResolutionOutcome -> Bool
isRegular expected outcome = case disappearanceResolutionOutcomeView outcome of
  RegularSortDefinitionRetired actual _ _ _ _ -> sortIdBytes actual == Application.sortIdBytes expected
  _ -> False

awaitResolution ::
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  (DisappearanceResolutionOutcome -> Bool) ->
  IO (DisappearanceProbeId, DisappearanceResolutionOutcome)
awaitResolution cursors ledgers matches = do
  observed <- timeout 10_000_000 (awaitStep15WorkEvidenceSince (take 3 cursors) (take 3 ledgers) (all (not . null . resolutions)))
  evidence <- case observed of
    Just values -> pure values
    Nothing -> do
      snapshots <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      assertFailure
        ( "successor disappearance did not resolve; per-Herald immutable projections="
            <> show [nub (filter isDisappearanceView (step15OracleProjectionViews item)) | item <- snapshots]
            <> "; work="
            <> show (fmap summarizeStep15WorkEvidence snapshots)
            <> "; first invalidating input and preceding peer controls="
            <> show (fmap (invalidatingInputs snapshots) snapshots)
        )
  case nub (concatMap resolutions evidence) of
    [resolved] -> pure resolved
    values -> assertFailure ("expected one exact disappearance outcome: " <> show values)
  where
    resolutions evidence =
      [(probe, outcome) | DisappearanceProbeResolvedView probe outcome <- step15OracleProjectionViews evidence, matches outcome]
    isDisappearanceView DisappearanceProbeOpenedView {} = True
    isDisappearanceView PredefinedAbsenceReportedView {} = True
    isDisappearanceView DisappearanceProbeResolvedView {} = True
    isDisappearanceView DisappearanceProbeInvalidatedView {} = True
    isDisappearanceView DisappearanceProbeAbortedView {} = True
    isDisappearanceView _ = False
    invalidatingInputs snapshots snapshot =
      take
        1
        [ (input, matchingItems input, reverse (take 6 (reverse prior)))
        | (index, KernelEvent _ (KernelTraceStepped _ _ input (Right effects))) <- zip [0 :: Int ..] (step15RuntimeEvidence snapshot),
          RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers effects,
          let command = oracleEnvelopeCommand (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)),
          command
            `elem` [ invalidateDisappearanceProbeCommand probe reporter reason witness
                   | DisappearanceProbeInvalidatedView probe (CommandDisappearanceInvalidation reporter reason witness) <- step15OracleProjectionViews snapshot
                   ],
          let prior =
                [ earlier
                | KernelEvent _ (KernelTraceStepped _ _ earlier _) <- take index (step15RuntimeEvidence snapshot),
                  PeerInput SemanticPeerControlReceived {} <- [inputBody earlier]
                ]
        ]
      where
        matchingItems input =
          [ (source, projectPeerLogicalAttempt attempt)
          | PeerInput (SemanticPeerPublicationReceived binding item) <- [inputBody input],
            (source, evidence) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] snapshots,
            step15HeraldEpoch source == peerBindingRemoteHeraldEpoch binding,
            KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- step15RuntimeEvidence evidence,
            SemanticSendPeerItem _ attempt <- effectBatchMembers effects,
            peerLogicalDispatchAttemptItem attempt == item
          ]

-- The public newenv result covers its 31 controlled objects. Each applied
-- root can also generate alignment subscriptions and ordered cutover items.
-- Close that finite causal wave using exact opaque runtime correlations before
-- taking a carrier: every announced dispatch has selected, every sent item has
-- both its source outcome and its destination input, and each distinct finite
-- control fact has its exact destination input. With stable survivor bindings,
-- equal control replays are already covered by that receipt: they cannot add a
-- new subscription fact. This is semantic readiness, not physical multiplicity.
-- Reading kernel effect batches includes all follow-up work in the same
-- observed receiving step, before the later shell outbox routes those effects.
-- Retired H4 and time-driven failure probes are outside this survivor wave.
awaitSuccessorWave :: [Step15WorkCursor] -> [Step15WorkLedger] -> IO ()
awaitSuccessorWave cursors ledgers = do
  observed <- timeout 10_000_000 (awaitStep15WorkEvidenceSince (take 3 cursors) (take 3 ledgers) (null . pendingSuccessorWork))
  case observed of
    Just _ -> pure ()
    Nothing -> do
      snapshots <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince (take 3 cursors) (take 3 ledgers))
      assertFailure ("successor alignment wave has outstanding causal work: " <> show (pendingSuccessorWork snapshots))

pendingSuccessorWork :: [Step15WorkEvidence] -> [String]
pendingSuccessorWork snapshots =
  pendingFinitePeerWave
    (zip (fmap step15HeraldEpoch [Step15H1, Step15H2, Step15H3]) (fmap step15RuntimeEvidence snapshots))

assertResolvedMembership ::
  HeraldMembershipGeneration -> DisappearanceProbeId -> DisappearanceResolutionOutcome -> [Step15WorkEvidence] -> Assertion
assertResolvedMembership successor probe outcome evidence = do
  forM_ (take 3 evidence) $ \herald -> do
    let views = step15OracleProjectionViews herald
        headers = nub [header | DisappearanceProbeOpenedView header _ <- views, projectedDisappearanceProbeHeaderId header == probe]
        reporters = nub [disappearanceEvidenceClaimReporter claim | PredefinedAbsenceReportedView claim <- views, disappearanceEvidenceClaimProbeId claim == probe]
    case headers of
      [header] -> do
        assertEqual "probe freezes the exact successor membership" successor (projectedDisappearanceProbeHeaderMembership header)
        assertBool "terminal matches the retained Open subject" $ case (disappearanceSubjectView (projectedDisappearanceProbeHeaderSubject header), disappearanceResolutionOutcomeView outcome) of
          (ControlledPredefinedSubjectView object occurrence revision, ControlledDisappearanceResolved actual actualOccurrence actualRevision _) ->
            (object, occurrence, revision) == (actual, actualOccurrence, actualRevision)
          (RegularSortDefinitionSubjectView sortId digest occurrence, RegularSortDefinitionRetired actual actualDigest actualOccurrence _ _) ->
            (sortId, digest, occurrence) == (actual, actualDigest, actualOccurrence)
          _ -> False
      other -> assertFailure ("expected one immutable Open header: " <> show other)
    assertEqual
      "only H1/H2/H3 supply complete absence evidence"
      (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)))
      (sort reporters)

structuralStamps :: Step15WorkEvidence -> [Protocol.StructuralOccurrenceStampDto]
structuralStamps evidence =
  nub
    [ stamp
    | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <- step15RuntimeEvidence evidence,
      Right (Protocol.PeerPublicationEnvelope _ (Protocol.PeerPublicationDto _ _ _ (Protocol.StructuralPublicationDto stamp _))) <- [projectPeerLogicalAttempt attempt]
    ]

projectedControls :: Step15WorkEvidence -> [(HeraldEpoch, Protocol.PeerControlDto)]
projectedControls evidence =
  [ (peerBindingRemoteHeraldEpoch binding, dto)
  | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl binding control)) <- step15RuntimeEvidence evidence,
    Right (Protocol.PeerControlEnvelope _ dto) <- [projectPeerControl control]
  ]

assertAcceptedLineage :: HeraldMembershipGeneration -> HeraldMembershipGeneration -> Step15WorkEvidence -> Step15WorkEvidence -> [Step15WorkEvidence] -> Assertion
assertAcceptedLineage predecessor successor before after allEvidence = do
  let oldStamps = structuralStamps before
      allStamps = structuralStamps after
      accepted = [(occurrence, stamp) | stamp@(Protocol.StructuralOccurrenceStampDto occurrence _ _ _ _) <- oldStamps]
      controls = concatMap projectedControls (take 3 allEvidence)
  assertEqual "the accepted manifest has exactly 31 controlled objects" 31 (length allStamps)
  forM_ [Protocol.NablaCarrierDto, Protocol.DeltaCarrierDto] $ \role -> do
    let sources =
          [ (nabla, Protocol.nablaSequenceDtoWord64 sequenceNumber)
          | Protocol.StructuralOccurrenceStampDto _ _ (Protocol.PublicationIdDto nabla _ sequenceNumber source) _ actualRole <- allStamps,
            actualRole == role,
            Protocol.heraldEpochClaimBytes source == heraldEpochBytes (step15HeraldEpoch Step15H1)
          ]
    assertEqual "each carrier role contributes the exact six roots from H1" 6 (length sources)
    assertEqual "each role retains one canonical source Nabla" 1 (length (nub (fmap fst sources)))
    assertEqual
      "root source is the accepted process's canonical startup carrier writer"
      (canonicalSources role)
      (nub (fmap (Protocol.nablaClaimBytes . fst) sources))
    assertEqual "manifest roots retain distinct source publication sequences" 6 (length (nub (fmap snd sources)))
    assertBool "every environment publication has a positive source sequence" (all ((> 0) . snd) sources)
  forM_ [(Protocol.NeutralVertexCarrierDto, 1), (Protocol.EdgeCarrierDto, 18)] $ \(role, count) -> do
    let stamps = [stamp | stamp@(Protocol.StructuralOccurrenceStampDto _ _ _ _ actualRole) <- allStamps, actualRole == role]
        sources = [(nabla, sequenceNumber) | Protocol.StructuralOccurrenceStampDto _ _ (Protocol.PublicationIdDto nabla _ sequenceNumber _) _ _ <- stamps]
    assertEqual "the wiring phase retains its exact hub and edge counts" count (length stamps)
    assertEqual "each wiring carrier has one fresh source writer" 1 (length (nub (fmap fst sources)))
    assertEqual "each wiring publication has a distinct source sequence" count (length (nub sources))
    forM_ stamps $ \(Protocol.StructuralOccurrenceStampDto _ vector _ _ _) ->
      assertEqual
        "wiring starts only after the endpoint cut in the surviving generation"
        (heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor))
        (Protocol.heraldMembershipGenerationClaimBytes (Protocol.structuralVersionVectorMembershipGeneration vector))
  forM_ accepted $ \(occurrence, stamp) -> do
    assertEqual
      "accepted manifest occurrence is never restamped across retirement"
      [stamp]
      [later | later@(Protocol.StructuralOccurrenceStampDto actual _ _ _ _) <- allStamps, actual == occurrence]
    case stamp of
      Protocol.StructuralOccurrenceStampDto (Protocol.StructuralOccurrenceIdDto source _) vector _ _ _ -> do
        assertEqual "accepted manifest keeps H1's canonical structural source" (heraldEpochBytes (step15HeraldEpoch Step15H1)) (Protocol.heraldEpochClaimBytes source)
        assertEqual
          "already-stamped roots retain their predecessor generation"
          (heraldMembershipGenerationIdBytes (heraldMembershipGenerationId predecessor))
          (Protocol.heraldMembershipGenerationClaimBytes (Protocol.structuralVersionVectorMembershipGeneration vector))
  assertBool
    "same accepted newenv crosses the exact successor topology lineage"
    (any successorCut controls || any successorUnion controls)
  assertBool
    "completion has an established successor base and all three reports cover every manifest root"
    (any (coversManifest allStamps) controls || (any successorUnion controls && completeSuccessorReports allStamps controls))
  where
    canonicalSources role =
      [ nablaIdBytes writer
      | bootstrap <- checkedOracleAppliedBootstraps step15CheckedOracleGenesis,
        appliedProcessEpochId bootstrap == step15H1ProcessEpochId,
        root <- appliedProcessRoots bootstrap,
        WriterRoot writer _ <- [appliedRootRole root],
        appliedRootCatalogueRole root == if role == Protocol.NablaCarrierDto then Profile.NablaRole else Profile.DeltaRole
      ]
    successorCut
      ( _,
        Protocol.TopologyCutEstablishedControlDto
          ( Protocol.TopologyCutEstablishedDto
              _
              (Protocol.TopologyCutDto (Protocol.MembershipSuccessorPredecessorDto _ old new _ _ _) _ _)
              _
            )
        ) =
        Protocol.heraldMembershipGenerationClaimBytes old == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId predecessor)
          && Protocol.heraldMembershipGenerationClaimBytes new == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
    successorCut _ = False
    successorUnion
      ( _,
        Protocol.TerminalSourceUnionEstablishedControlDto
          (Protocol.TerminalSourceUnionEstablishedDto (Protocol.TerminalSourceUnionDto old new retired _ _) acceptances)
        ) =
        Protocol.heraldMembershipGenerationClaimBytes old == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId predecessor)
          && Protocol.heraldMembershipGenerationClaimBytes new == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
          && fmap Protocol.heraldEpochClaimBytes (NonEmpty.toList (Protocol.retiredHeraldEntries retired)) == [heraldEpochBytes (step15HeraldEpoch Step15H4)]
          && sort (fmap (Protocol.heraldEpochClaimBytes . Protocol.terminalSourceUnionAcceptanceReporter) (NonEmpty.toList (Protocol.terminalSourceUnionAcceptanceEntries acceptances)))
            == survivorBytes
    successorUnion _ = False
    survivorBytes = sort (fmap heraldEpochBytes (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)))
    completeSuccessorReports stamps controls =
      sort
        ( nub
            [ Protocol.heraldEpochClaimBytes reporter
            | (_, Protocol.StructuralAppliedReportedDto (Protocol.StructuralAppliedReportDto reporter vector _)) <- controls,
              Protocol.heraldMembershipGenerationClaimBytes (Protocol.structuralVersionVectorMembershipGeneration vector)
                == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor),
              all (coveredBy vector) stamps
            ]
        )
        == survivorBytes
    coversManifest
      stamps
      ( _,
        Protocol.TopologyCutEstablishedControlDto
          (Protocol.TopologyCutEstablishedDto _ (Protocol.TopologyCutDto _ (Protocol.TopologyFrontierDto vector _) _) acceptances)
        ) =
        Protocol.heraldMembershipGenerationClaimBytes (Protocol.structuralVersionVectorMembershipGeneration vector)
          == heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
          && sort (fmap (Protocol.heraldEpochClaimBytes . Protocol.topologyCutAcceptanceReporter) (NonEmpty.toList (Protocol.topologyCutAcceptanceEntries acceptances)))
            == sort (fmap heraldEpochBytes (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)))
          && all (coveredBy vector) stamps
    coversManifest _ _ = False
    coveredBy vector (Protocol.StructuralOccurrenceStampDto (Protocol.StructuralOccurrenceIdDto source sequenceNumber) _ _ _ _) =
      any (coversSource source sequenceNumber) (NonEmpty.toList (Protocol.structuralVersionVectorEntries vector))
    coversSource source sequenceNumber (Protocol.StructuralVectorEntryDto reporter (Protocol.StructuralPrefixThroughDto prefix)) =
      reporter == source && Protocol.positiveStructuralSequenceDtoWord64 prefix >= Protocol.positiveStructuralSequenceDtoWord64 sequenceNumber
    coversSource _ _ _ = False

assertFreshOccurrence :: DisappearanceResolutionOutcome -> [Step15WorkEvidence] -> Assertion
assertFreshOccurrence outcome evidence = case disappearanceResolutionOutcomeView outcome of
  RegularSortDefinitionRetired expected _ old resolvedAt next -> do
    assertBool "retirement names a distinct successor occurrence" (old /= next)
    base <- either (assertFailure . show) pure (resolvedRetirementOccurrenceBase resolvedAt)
    assertEqual
      "the canonical Resolve floor derives the exact next occurrence"
      (deriveSortDefinitionOccurrenceId (checkedOracleSystemId step15CheckedOracleGenesis) expected base)
      next
    let definitions =
          [ (bytes, occurrence, Protocol.controlIndexDtoWord64 requiredControl)
          | herald <- evidence,
            ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <- step15RuntimeEvidence herald,
            Right (Protocol.PeerPublicationEnvelope _ (Protocol.PeerPublicationDto _ _ _ item)) <- [projectPeerLogicalAttempt attempt],
            Protocol.PublicationBatchDto _ _ _ occurrence bytes _ _ requiredControl _ <- [batchOf item],
            Right value <- [Value.decodeCanonicalValue bytes],
            Value.RecordValue fields <- [Value.viewValue value],
            (field, sortValue) <- Map.toList fields,
            Value.fieldNameText field == "sort_id",
            Value.BytesValue sortBytes <- [Value.viewValue sortValue],
            sortBytes == sortIdBytes expected
          ]
    assertEqual "equal descriptor preserves exact canonical publication bytes" 1 (length (nub [bytes | (bytes, _, _) <- definitions]))
    assertEqual "the outer primordial sort-sort carrier keeps its occurrence" 1 (length (nub [occurrence | (_, occurrence, _) <- definitions]))
    assertBool
      "equal redefinition carries the Resolve floor that selects the next occurrence"
      (any (\(_, _, floorIndex) -> floorIndex >= controlIndexWord64 resolvedAt) definitions)
  _ -> assertFailure "regular retirement expected"
  where
    batchOf (Protocol.OrdinaryPublicationDto batch) = batch
    batchOf (Protocol.StructuralPublicationDto _ batch) = batch

-- The inherited clock, heartbeat and dial guards remain in Step15. This
-- runtime tour measures the added protocol families: exact source roots,
-- bounded directed deliveries and their actual receiving kernel inputs.
-- Owner-local A/W/F algebra stays in the focused pure-kernel properties.
assertClosedWorkBounds :: [Step15WorkEvidence] -> Assertion
assertClosedWorkBounds evidence = do
  forM_ (zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence) $ \(name, herald) ->
    assertEqual (show name <> " has no kernel fault") 0 (step15KernelFaults (summarizeStep15WorkEvidence herald))
  let roots = nub (concatMap structuralStamps evidence)
      deliveries =
        [ (name, peerBindingRemoteHeraldEpoch binding, occurrence, peerLogicalDispatchAttemptItem attempt)
        | (name, herald) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence,
          ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem binding attempt)) <- step15RuntimeEvidence herald,
          Right
            ( Protocol.PeerPublicationEnvelope
                _
                ( Protocol.PeerPublicationDto
                    _
                    _
                    _
                    (Protocol.StructuralPublicationDto (Protocol.StructuralOccurrenceStampDto occurrence _ _ _ _) _)
                  )
              ) <-
            [projectPeerLogicalAttempt attempt]
        ]
      directed = [(source, remote, occurrence) | (source, remote, occurrence, _) <- deliveries]
      -- The neutral and first endpoint phase freeze four members. Its wiring
      -- phase, the second complete environment and explicit private edge freeze
      -- three: E <= (1+12)*3 + (19+31+1)*2 = 141.
      maximumDeliveries = (1 + 12) * (4 - 1) + (19 + 31 + 1) * (3 - 1)
  assertEqual "two 31-object environments, one neutral and one private edge allocate exactly 64 source occurrences" 64 (length roots)
  assertEqual "each source occurrence has at most one logical delivery per destination" (length (nub directed)) (length directed)
  assertBool "structural handoffs are bounded by the frozen destination counts E" (length deliveries <= maximumDeliveries)
  forM_ deliveries $ \(_, remote, _, item) -> do
    let receives =
          [ ()
          | (name, herald) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence,
            step15HeraldEpoch name == remote,
            KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence herald,
            PeerInput (SemanticPeerPublicationReceived _ received) <- [inputBody input],
            received == item
          ]
    if remote == step15HeraldEpoch Step15H4
      then assertBool "retired destination consumes an allocated root at most once" (length receives <= 1)
      else assertEqual "each surviving destination consumes the exact root in one kernel input" 1 (length receives)
  let offers =
        [ canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)
        | herald <- evidence,
          SubmitOracleRequest _ dispatch <- step15OracleActionEvidence herald
        ]
      intentions = nub (fmap oracleEnvelopeRequestId offers)
      manager = concatMap step15OracleManagerEvidence evidence
      submits = [request | OraclePhysicalSubmitAttempt _ request <- manager]
      contacts = length [() | OraclePhysicalConnectAttempt _ <- manager]
      redirects = length [() | OraclePhysicalRedirect _ <- manager]
      -- The existing Increment-9 physical law is O + O*V + 4*X. H4 hosts
      -- no Oracle voter and this tour injects no Oracle connection loss: X=0.
      bound = length intentions + length intentions * 3
  assertBool "all physical Oracle submits belong to owner-retained intentions" (all (`elem` intentions) submits)
  assertBool
    "physical Oracle contact work satisfies O + O*V with V=3 and X=0"
    (contacts + redirects + length submits <= bound)

-- The logical budget is fixed by the two observed canonical probes, not by
-- how many owner offers were emitted. The physical manager probe is below
-- connection-local deduplication; repeated watch offers do not count as sends.
-- No disappearance intention is minted by a retry.
assertDisappearanceWork :: [DisappearanceProbeId] -> [Step15WorkEvidence] -> Assertion
assertDisappearanceWork probes evidence = forM_ probes $ \probe -> do
  let views = concatMap step15OracleProjectionViews evidence
      headers = nub [header | DisappearanceProbeOpenedView header _ <- views, projectedDisappearanceProbeHeaderId header == probe]
      claims = nub [claim | PredefinedAbsenceReportedView claim <- views, disappearanceEvidenceClaimProbeId claim == probe]
      envelopes =
        [ canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)
        | herald <- evidence,
          SubmitOracleRequest _ dispatch <- step15OracleActionEvidence herald
        ]
      physicalRequests = [request | herald <- evidence, OraclePhysicalSubmitAttempt _ request <- step15OracleManagerEvidence herald]
      checkCommand label maximumIntentions command = do
        let offers = [envelope | envelope <- envelopes, oracleEnvelopeCommand envelope == command]
            intentions = nub (fmap oracleEnvelopeRequestId offers)
            physical = [request | request <- physicalRequests, request `elem` intentions]
            homes = nub (fmap oracleEnvelopeHomeHeraldEpoch offers)
        assertBool (label <> " has at least one actual physical submission") (not (null physical))
        assertBool (label <> " keeps one stable intention per permitted home") (length intentions <= maximumIntentions)
        forM_ homes $ \home ->
          assertEqual
            (label <> " never allocates a second request identity at one home")
            1
            (length (nub [oracleEnvelopeRequestId envelope | envelope <- offers, oracleEnvelopeHomeHeraldEpoch envelope == home]))
        let namedEvidence = zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence
            associations =
              [ (name, binding, request, retry)
              | (name, herald) <- namedEvidence,
                AssociateOracleSubmissionRetry binding request retry <- step15OracleActionEvidence herald,
                request `elem` intentions
              ]
            relevantRetry name retry = any (\(owner, _, _, token) -> owner == name && token == retry) associations
            retryAction name = \case
              AssociateOracleSubmissionRetry _ request _ -> request `elem` intentions
              ReleaseDeferredOracleSubmission _ request -> request `elem` intentions
              ScheduleOracleRetry _ retry -> relevantRetry name retry
              CancelOracleRetry retry -> relevantRetry name retry
              _ -> False
            releaseTrigger input = case inputBody input of
              OracleInput (OracleEntriesReceived _ entries) -> "committed entry batch, count=" <> show (length entries)
              OracleInput (OracleRetryElapsed retry) -> "retry elapsed: " <> show retry
              OracleInput ingress -> "other Oracle input: " <> show ingress
              _ -> "non-Oracle input"
            detail =
              "\nprobe="
                <> show probe
                <> "; command="
                <> show command
                <> "; request homes="
                <> show (nub [(oracleEnvelopeRequestId offer, oracleEnvelopeHomeHeraldEpoch offer) | offer <- offers])
                <> "; physical count="
                <> show (length physical)
                <> "; strict bound="
                <> show (length intentions * (1 + 3))
                <> "\nphysical submits="
                <> show
                  [ (name, ordinal, binding, request)
                  | (name, herald) <- namedEvidence,
                    (ordinal, OraclePhysicalSubmitAttempt binding request) <- zip [0 :: Int ..] (step15OracleManagerEvidence herald),
                    request `elem` intentions
                  ]
                <> "\nNotReady kernel inputs="
                <> show
                  [ (name, ordinal, binding, request, term)
                  | (name, herald) <- namedEvidence,
                    KernelEvent ordinal (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence herald,
                    OracleInput (OracleSubmissionNotReadyReceived binding request term) <- [inputBody input],
                    request `elem` intentions
                  ]
                <> "\nmanager retry/release actions="
                <> show
                  [ (name, ordinal, action)
                  | (name, herald) <- namedEvidence,
                    (ordinal, action) <- zip [0 :: Int ..] (step15OracleActionEvidence herald),
                    retryAction name action
                  ]
                <> "\nadmitted retry-release triggers="
                <> show
                  [ (name, ordinal, retry, releaseTrigger input)
                  | (name, herald) <- namedEvidence,
                    KernelEvent ordinal (KernelTraceStepped _ _ input (Right effects)) <- step15RuntimeEvidence herald,
                    RunOracleClientAction (CancelOracleRetry retry) <- effectBatchMembers effects,
                    relevantRetry name retry
                  ]
                <> "\nretry pacing="
                <> show
                  [ (name, ordinal, event)
                  | (name, herald) <- namedEvidence,
                    (ordinal, event@(OracleRetryPaced _ retry _ _)) <- zip [0 :: Int ..] (step15OracleManagerEvidence herald),
                    relevantRetry name retry
                  ]
        assertBool
          (label <> " physical attempts exceed O + O*V, with V=3 and X=0" <> detail)
          (length physical <= length intentions * (1 + 3))
  assertEqual "one report claim per captured successor member" 3 (length claims)
  forM_ claims $ \claim -> checkCommand "Report" 1 (reportPredefinedAbsenceCommand probe claim)
  case headers of
    [header] ->
      checkCommand
        "Open"
        3
        (openDisappearanceProbeCommand (projectedDisappearanceProbeHeaderSubject header) (projectedDisappearanceProbeHeaderCoordinate header))
    _ -> assertFailure "work accounting requires the one immutable Open header"
  checkCommand "Resolve" 3 (resolveDisappearanceProbeCommand probe (completeDisappearanceEvidenceDigest claims))
  let publicationMarkers =
        [ (direction, sequenceNumber, digest)
        | herald <- evidence,
          ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <- step15RuntimeEvidence herald,
          Right
            ( Protocol.PeerPublicationEnvelope
                _
                (Protocol.PeerDisappearanceProbeMarkerDto direction sequenceNumber digest (Protocol.DisappearanceProbeMarkerDto actual _ _ _))
              ) <-
            [projectPeerLogicalAttempt attempt],
          Protocol.disappearanceProbeDigestClaimBytes (Protocol.disappearanceProbeIdDigest actual) == disappearanceProbeIdBytes probe
        ]
  assertEqual "P=M*(M-1)=6 retained publication markers per successor probe" 6 (length (nub publicationMarkers))
  assertEqual "the loss-free successor sends each exact publication marker once" (length (nub publicationMarkers)) (length publicationMarkers)
  let markerItems =
        [ peerLogicalDispatchAttemptItem attempt
        | herald <- evidence,
          ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <- step15RuntimeEvidence herald,
          Right
            ( Protocol.PeerPublicationEnvelope
                _
                (Protocol.PeerDisappearanceProbeMarkerDto _ _ _ (Protocol.DisappearanceProbeMarkerDto actual _ _ _))
              ) <-
            [projectPeerLogicalAttempt attempt],
          Protocol.disappearanceProbeDigestClaimBytes (Protocol.disappearanceProbeIdDigest actual) == disappearanceProbeIdBytes probe
        ]
      markerInputs =
        [ item
        | herald <- evidence,
          KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence herald,
          PeerInput (SemanticPeerPublicationReceived _ item) <- [inputBody input],
          item `elem` markerItems
        ]
  assertEqual "P=6 actual kernel publication-marker admissions" 6 (length markerInputs)
  let alignmentMarkers =
        [ (step15HeraldEpoch name, remote, subscription, control)
        | (name, herald) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence,
          (remote, control@(Protocol.AlignmentControlDto (Protocol.AlignmentProbeMarkerDto actual subscription _))) <- projectedControls herald,
          Protocol.disappearanceProbeDigestClaimBytes (Protocol.disappearanceProbeIdDigest actual) == disappearanceProbeIdBytes probe
        ]
      alignmentKeys = [(source, remote, subscription) | (source, remote, subscription, _) <- alignmentMarkers]
  assertEqual "one remote alignment marker per exact captured subscription" (length (nub alignmentKeys)) (length alignmentKeys)
  forM_ alignmentMarkers $ \(source, remote, _, control) -> do
    let admissions =
          [ ()
          | (name, herald) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence,
            step15HeraldEpoch name == remote,
            KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence herald,
            PeerInput (SemanticPeerControlReceived binding admitted) <- [inputBody input],
            peerBindingRemoteHeraldEpoch binding == source,
            Right (Protocol.PeerControlEnvelope _ actual) <- [projectPeerControl admitted],
            actual == control
          ]
    assertEqual "each exact remote alignment marker reaches its destination in one kernel input" 1 (length admissions)
