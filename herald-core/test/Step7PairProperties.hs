{-# LANGUAGE OverloadedStrings #-}

module Step7PairProperties
  ( tests,
  )
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16, Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result (RegularCallResult (..))
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
import Eclips.Application.Types.Value
  ( ApplicationValue (SortDefinitionValue),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    SortId,
  )
import Eclips.Herald.Application.Request
  ( ApplicationOperation (..),
    ApplicationRequestReply (..),
    RetainedRequestReplyBody (..),
    requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (..),
    clientNonce,
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortId,
  )
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerBinding,
    PeerCandidate,
    PeerHello,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerAddress,
    peerBindingRemoteHeraldEpoch,
    peerCandidateOpened,
    peerDialIntentHeraldEpoch,
    peerHelloAdvertisedAddresses,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    PredefinedSortRole (SortDefinitionRole),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstrapsWithTopology,
    checkedInitialProjectionDigest,
    heraldMemberEpoch,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (PeerBindingLost, PeerDispatchObserved),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.PeerDispatch qualified as PeerDispatch
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerStream
  ( PeerDispatchAttempt,
    PeerDispatchOutcome (PeerDispatchWritten),
    PeerDispatchTicket,
    firstStreamSequence,
    resumeOfferNextSourceSequence,
  )
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (HeraldState)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    initialEffectsAreOracleWatchAndGrace,
  )
import PrimordialTestAccess (conventionalStartupPairs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-7 two-Herald message-only gate"
    [ testCase "publication survives reconnect and is applied exactly once" casePairTrace,
      testProperty "the complete pair trace replays deterministically" propDeterministicPairTrace,
      testProperty "full placement snapshots can start late and skip revisions across withdrawal and replay" propPlacementSnapshotGaps
    ]

-- Exercise the composed peer transition, including alignment loss observation,
-- through real Hello bindings and wire controls. No intermediate snapshot was
-- received, so no intermediate revision can be required by that transition.
propPlacementSnapshotGaps :: Word16 -> Property
propPlacementSnapshotGaps seed =
  counterexample (show result) (result == Right ())
  where
    result = do
      remoteGenesis <- checked "remote genesis" (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
      localBootstraps <- checkedTopology fixtureCheckedGenesis
      remoteBootstraps <- checkedTopology remoteGenesis
      local <- initialize "local" fixtureCheckedGenesis localBootstraps
      remote <- initialize "remote" remoteGenesis remoteBootstraps
      Connected connected _ binding _ _ _ _ remoteHelloEffects <-
        connect "placement connection" (nonce seed 1) 1 1 2 local remote
      control <- exactlyOne "initial placement" (placementControls remoteHelloEffects)
      snapshot <- case control of
        PeerPlacementUpdate (Placement.FullPlacementSnapshot value) -> Right value
        _ -> Left "initial control was not a full placement snapshot"
      first <- checked "late first revision" (Placement.mkPlacementSequence (2 + fromIntegral seed))
      later <- checked "later non-adjacent revision" (Placement.mkPlacementSequence (4 + 2 * fromIntegral seed))
      let owner = Placement.placementSnapshotOwner snapshot
          routes = Placement.placementSnapshotRoutes snapshot
          acknowledgement revision = PeerPlacementAcknowledged (Placement.placementAcknowledgement owner revision)
      require "the initial snapshot must exercise Store withdrawal" (not (null routes))
      lateSnapshot <- checked "late snapshot" (Placement.placementSnapshot owner first routes)
      withdrawnSnapshot <- checked "withdrawn snapshot" (Placement.placementSnapshot owner later [])
      let lateControl = PeerPlacementUpdate (Placement.FullPlacementSnapshot lateSnapshot)
          withdrawnControl = PeerPlacementUpdate (Placement.FullPlacementSnapshot withdrawnSnapshot)
      (placed, placedEffects) <- peerControlStep "first snapshot starts after revision one" 3 binding lateControl connected
      require "late first snapshot is acknowledged" (acknowledgement first `elem` sentControls placedEffects)
      (withdrawn, withdrawnEffects) <- peerControlStep "newer full snapshot skips revisions and withdraws Stores" 4 binding withdrawnControl placed
      require "skipped revision snapshot is acknowledged" (acknowledgement later `elem` sentControls withdrawnEffects)
      (replayed, replayEffects) <- peerControlStep "duplicate withdrawn snapshot" 5 binding withdrawnControl withdrawn
      require "duplicate snapshot only acknowledges current revision" (sentControls replayEffects == [acknowledgement later])
      (_, staleEffects) <- peerControlStep "old snapshot cannot restore withdrawn Stores" 6 binding lateControl replayed
      require "stale snapshot only acknowledges current revision" (sentControls staleEffects == [acknowledgement later])

casePairTrace :: Assertion
casePairTrace = case pairTrace 1 of
  Left problem -> assertFailure problem
  Right _ -> pure ()

propDeterministicPairTrace :: Word16 -> Property
propDeterministicPairTrace seed =
  counterexample diagnostic (first == second && isSuccessful first)
  where
    first = pairTrace seed
    second = pairTrace seed
    diagnostic =
      "equal pure pair traces diverged or failed\nfirst: "
        <> show first
        <> "\nsecond: "
        <> show second

    isSuccessful (Right _) = True
    isSuccessful (Left _) = False

data TraceSummary = TraceSummary
  { traceInitialAttempt :: PeerDispatchAttempt PeerLogicalPayload,
    traceRetransmittedAttempt :: PeerDispatchAttempt PeerLogicalPayload,
    traceFirstRead :: [ApplicationValue],
    traceDuplicateRead :: [ApplicationValue],
    traceAcknowledgements :: [PeerControl],
    tracePostReleaseRetryEffects :: [HeraldEffect],
    tracePostReleaseEffects :: [HeraldEffect]
  }
  deriving stock (Eq, Show)

-- The two states below are deliberately opaque transition tokens.  The harness
-- learns correlations and payloads only from effects, then routes those exact
-- typed values back through 'stepHerald'; the remote definition is observed only
-- through its application session.
pairTrace :: Word16 -> Either String TraceSummary
pairTrace seed = do
  remoteGenesis <-
    checked
      "remote Herald genesis"
      (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  localBootstraps <- checkedTopology fixtureCheckedGenesis
  remoteBootstraps <- checkedTopology remoteGenesis
  require
    "both Heralds must start from the same checked topology projection"
    ( checkedInitialProjectionDigest localBootstraps
        == checkedInitialProjectionDigest remoteBootstraps
    )
  localInitial <- initialize "local Herald" fixtureCheckedGenesis localBootstraps
  remoteInitial <- initialize "remote Herald" remoteGenesis remoteBootstraps

  Connected localConnected remoteConnected localBinding remoteBinding _ _ localHelloEffects remoteHelloEffects <-
    connect
      "initial connection"
      (nonce seed 1)
      1
      1
      2
      localInitial
      remoteInitial
  require
    "Hello establishes only discovery, placement, and stream-resume control"
    (all helloControlsAreHandshakeOnly [localHelloEffects, remoteHelloEffects])

  remotePlacement <-
    exactlyOne
      "remote initial placement snapshot"
      (placementControls remoteHelloEffects)
  (localPlaced, _) <-
    peerControlFromEffectsStep
      remoteHelloEffects
      "apply remote placement"
      3
      localBinding
      remotePlacement
      localConnected

  initialLocalResumeOffer <-
    exactlyOne "initial local resume offer" (resumeOfferControls localHelloEffects)
  initialRemoteResumeOffer <-
    exactlyOne "initial remote resume offer" (resumeOfferControls remoteHelloEffects)
  require
    "initial empty streams must advertise exclusive frontier one"
    ( case (initialLocalResumeOffer, initialRemoteResumeOffer) of
        (PeerStreamResumeOffered localOffer, PeerStreamResumeOffered remoteOffer) ->
          resumeOfferNextSourceSequence localOffer == firstStreamSequence
            && resumeOfferNextSourceSequence remoteOffer == firstStreamSequence
        _ -> False
    )
  (localAfterInitialOffer, localInitialResumeResponseEffects) <-
    peerControlFromEffectsStep
      remoteHelloEffects
      "apply initial remote resume offer"
      4
      localBinding
      initialRemoteResumeOffer
      localPlaced
  (localAfterRepeatedInitialOffer, localRepeatedResumeResponseEffects) <-
    peerControlFromEffectsStep
      remoteHelloEffects
      "repeat initial remote resume offer"
      4
      localBinding
      initialRemoteResumeOffer
      localAfterInitialOffer
  require
    "a repeated same-binding ResumeOffer emits only its ResumeAccepted response"
    ( case sentControls localRepeatedResumeResponseEffects of
        [PeerStreamResumeAccepted _] ->
          null (dispatchEffects localRepeatedResumeResponseEffects)
        _ -> False
    )
  (remoteAfterInitialOffer, remoteInitialResumeResponseEffects) <-
    peerControlFromEffectsStep
      localHelloEffects
      "apply initial local resume offer"
      2
      remoteBinding
      initialLocalResumeOffer
      remoteConnected
  require
    "ResumeAccepted leads every initial reconnect-repair control on its source FIFO"
    ( all
        resumeAcceptedLeadsRepair
        [localInitialResumeResponseEffects, remoteInitialResumeResponseEffects]
    )
  initialLocalResumeResponse <-
    exactlyOne
      "initial local resume response"
      (resumeResponseControls localInitialResumeResponseEffects)
  initialRemoteResumeResponse <-
    exactlyOne
      "initial remote resume response"
      (resumeResponseControls remoteInitialResumeResponseEffects)
  (remoteInitialResumed, remoteAcceptedInitialResumeEffects) <-
    peerControlFromEffectsStep
      localInitialResumeResponseEffects
      "apply initial local resume response"
      3
      remoteBinding
      initialLocalResumeResponse
      remoteAfterInitialOffer
  (localInitialResumed, localAcceptedInitialResumeEffects) <-
    peerControlFromEffectsStep
      remoteInitialResumeResponseEffects
      "apply initial remote resume response"
      5
      localBinding
      initialRemoteResumeResponse
      localAfterRepeatedInitialOffer
  require
    "the empty initial resume exchange must not create dispatch work"
    ( all
        (null . dispatchEffects)
        [remoteAcceptedInitialResumeEffects, localAcceptedInitialResumeEffects]
    )

  OpenApplication localOpened localSession localApplicationBinding localAccess <-
    openApplication
      "local application"
      6
      (lane seed 1)
      (applicationNonce seed 1)
      fixtureCheckedGenesis
      localBootstraps
      firstLocalBootstrapId
      localInitialResumed
  OpenApplication remoteOpened remoteSession remoteApplicationBinding remoteAccess <-
    openApplication
      "remote application"
      4
      (lane seed 2)
      (applicationNonce seed 2)
      remoteGenesis
      remoteBootstraps
      fixtureRemoteBootstrapId
      remoteInitialResumed

  localSortAccess <- sortDefinitionAccess "local sort-definition access" localAccess
  remoteSortAccess <- sortDefinitionAccess "remote sort-definition access" remoteAccess
  let writer = Access.predefinedWriter localSortAccess
      reader = Access.predefinedReader remoteSortAccess
      definition = DeclaredSortDefinition declaredDescriptor Nothing
      writeCall = WriteApplication writer (PublishValue (SortDefinitionValue definition))

  (localWritten, writeEffects) <-
    applicationCallStep
      "local declared sort write"
      7
      localSession
      localApplicationBinding
      1
      writeCall
      localOpened
  (writeReply, writtenSort, initialTicket) <- writeResult writeEffects
  writeStreamFrontier <-
    exactlyOne
      "ordinary sort-definition stream-frontier advance"
      (frontierAdvanceControls writeEffects)
  require
    "an ordinary sort-definition write replies, advances its stream frontier, and schedules"
    ( effectBatchMembers writeEffects
        == [ writeReply,
             SendPeerControl localBinding writeStreamFrontier,
             SchedulePeerDispatch initialTicket
           ]
    )
  require
    "the ordinary stream frontier advances through the newly allocated item"
    ( case writeStreamFrontier of
        PeerStreamFrontierAdvanced _ nextSourceSequence ->
          nextSourceSequence > firstStreamSequence
        _ -> False
    )
  (remoteFrontierAdvanced, frontierAdvanceEffects) <-
    peerControlFromEffectsStep
      writeEffects
      "apply ordinary one-way stream frontier"
      5
      remoteBinding
      writeStreamFrontier
      remoteOpened
  require
    "ordinary frontier advancement is one-way and creates no reverse control or dispatch work"
    (effectBatchIsEmpty frontierAdvanceEffects)

  (localRetried, retryEffects) <-
    applicationCallStep
      "exact application retry"
      9
      localSession
      localApplicationBinding
      1
      writeCall
      localWritten
  require
    "an exact application retry must reoffer only the retained reply"
    (effectBatchMembers retryEffects == [writeReply])
  require
    "an exact application retry must not enqueue or schedule new peer work"
    (null (dispatchEffects retryEffects))

  (localSelected, initialDispatchEffects) <-
    peerStep
      "select the initial dispatch"
      10
      (PeerDispatchSelected initialTicket localBinding)
      localRetried
  initialAttempt <- exactlyOne "initial peer dispatch attempt" (sentAttempts initialDispatchEffects)

  (localLost, localLossEffects) <-
    runtimeStep "lose the local binding" 11 (PeerBindingLost localBinding) localSelected
  (remoteLost, remoteLossEffects) <-
    runtimeStep "lose the remote binding" 6 (PeerBindingLost remoteBinding) remoteFrontierAdvanced
  require
    "local binding loss arms recovery before its retained direct-dial intention"
    (isRecoveryArmAndSoleDialFor localBinding localLossEffects)
  require
    "remote binding loss arms recovery before its retained direct-dial intention"
    (isRecoveryArmAndSoleDialFor remoteBinding remoteLossEffects)

  Connected localReconnected remoteReconnected localBinding2 remoteBinding2 localReofferCandidate localReofferHello localReconnectEffects remoteReconnectEffects <-
    connect
      "reconnection"
      (nonce seed 2)
      12
      7
      13
      localLost
      remoteLost
  require
    "reconnection Hello does not enqueue the bulk repair catalogue"
    (all helloControlsAreHandshakeOnly [localReconnectEffects, remoteReconnectEffects])

  localResumeOffer <-
    exactlyOne "local resume offer" (resumeOfferControls localReconnectEffects)
  remoteResumeOffer <-
    exactlyOne "remote resume offer" (resumeOfferControls remoteReconnectEffects)
  (localAfterOffer, localResumeResponseEffects) <-
    peerControlFromEffectsStep
      remoteReconnectEffects
      "apply remote resume offer"
      14
      localBinding2
      remoteResumeOffer
      localReconnected
  (remoteAfterOffer, remoteResumeResponseEffects) <-
    peerControlFromEffectsStep
      localReconnectEffects
      "apply local resume offer"
      8
      remoteBinding2
      localResumeOffer
      remoteReconnected
  require
    "ResumeAccepted leads every reconnection-repair control on its source FIFO"
    ( all
        resumeAcceptedLeadsRepair
        [localResumeResponseEffects, remoteResumeResponseEffects]
    )
  localResumeResponse <-
    exactlyOne "local resume response" (resumeResponseControls localResumeResponseEffects)
  remoteResumeResponse <-
    exactlyOne "remote resume response" (resumeResponseControls remoteResumeResponseEffects)
  (remoteResumed, _) <-
    peerControlFromEffectsStep
      localResumeResponseEffects
      "apply local resume response"
      9
      remoteBinding2
      localResumeResponse
      remoteAfterOffer
  (localResumed, localAcceptedResumeEffects) <-
    peerControlFromEffectsStep
      remoteResumeResponseEffects
      "apply remote resume response"
      15
      localBinding2
      remoteResumeResponse
      localAfterOffer

  retransmissionTicket <-
    firstOne
      "retransmission dispatch ticket"
      ( concatMap
          scheduledTickets
          [localReconnectEffects, localResumeResponseEffects, localAcceptedResumeEffects]
      )
  (localReselected, retransmissionEffects) <-
    peerStep
      "select the retransmission"
      16
      (PeerDispatchSelected retransmissionTicket localBinding2)
      localResumed
  retransmittedAttempt <-
    exactlyOne "retransmitted peer item" (sentAttempts retransmissionEffects)
  require
    "reconnection must retransmit the exact immutable stream assignment"
    ( PeerDispatch.peerDispatchAttemptItem retransmittedAttempt
        == PeerDispatch.peerDispatchAttemptItem initialAttempt
    )

  (localAwaitingProgress, writtenEffects) <-
    runtimeStep
      "observe the retransmission write"
      17
      (PeerDispatchObserved retransmittedAttempt PeerDispatchWritten)
      localReselected
  require
    "a successful physical write waits silently for peer progress"
    (effectBatchIsEmpty writtenEffects)
  (localAfterRepeatedOffer, repeatedOfferEffects) <-
    peerControlFromEffectsStep
      remoteReconnectEffects
      "repeat the same-binding resume claim after write"
      18
      localBinding2
      remoteResumeOffer
      localAwaitingProgress
  require
    "an exact same-binding ResumeOffer preserves successful-write suppression"
    ( case sentControls repeatedOfferEffects of
        [PeerStreamResumeAccepted _] -> null (dispatchEffects repeatedOfferEffects)
        _ -> False
    )
  (localReoffered, localReofferEffects) <-
    peerStep
      "reoffer the accepted Hello"
      19
      ( currentPeerHelloReceived
          localAfterRepeatedOffer
          localReofferCandidate
          (Set.singleton (peerAddress "pair-local.example:4040"))
          localReofferHello
      )
      localAfterRepeatedOffer
  require
    "a same-binding Hello reoffer remains a handshake-only effect"
    (helloControlsAreHandshakeOnly localReofferEffects)
  (localReactivated, reactivatedOfferEffects) <-
    peerControlFromEffectsStep
      remoteReconnectEffects
      "apply the first ResumeOffer after Hello reoffer"
      20
      localBinding2
      remoteResumeOffer
      localReoffered
  require
    "the Hello reoffer restores exactly one retained dispatch activation"
    (length (dispatchEffects reactivatedOfferEffects) == 1)

  retransmittedProgress <- exactlyOne "retransmitted publication receipt header" [progress | (progress, attempt) <- sentAttemptMessages retransmissionEffects, attempt == retransmittedAttempt]
  let retransmittedItem = PeerDispatch.peerDispatchAttemptItem retransmittedAttempt
  (remoteApplied, firstAcknowledgementEffects) <-
    peerStep
      "deliver the retransmitted publication"
      10
      (PeerPublicationReceivedWithProgress remoteBinding2 retransmittedProgress retransmittedItem)
      remoteResumed
  firstAcknowledgement <-
    completionAcknowledgement "first publication acknowledgement" firstAcknowledgementEffects

  let remoteQuery =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton reader,
            applicationQueryPredicate = QueryAlways
          }
  (remoteAfterFirstRead, firstReadEffects) <-
    applicationCallStep
      "read the local Store after remote publication delivery"
      11
      remoteSession
      remoteApplicationBinding
      1
      (ReadApplication remoteQuery)
      remoteApplied
  firstValues <- readResult 1 firstReadEffects
  let expectedDefinition =
        SortDefinitionValue
          (DeclaredSortDefinition declaredDescriptor (Just writtenSort))
  require
    "the H2 local read must observe the remotely published definition"
    (expectedDefinition `elem` firstValues)
  require
    "the H2 reader's local cut must contain the six primordial definitions and the new one"
    (length firstValues == 7)

  (remoteDuplicate, duplicateAcknowledgementEffects) <-
    peerStep
      "deliver the exact publication duplicate"
      12
      (PeerPublicationReceivedWithProgress remoteBinding2 retransmittedProgress retransmittedItem)
      remoteAfterFirstRead
  duplicateAcknowledgement <-
    completionAcknowledgement "duplicate publication acknowledgement" duplicateAcknowledgementEffects
  require
    "a duplicate delivery must replay the exact cumulative acknowledgement"
    (duplicateAcknowledgement == firstAcknowledgement)

  (remoteAfterDuplicateRead, duplicateReadEffects) <-
    applicationCallStep
      "read the local Store after duplicate publication delivery"
      13
      remoteSession
      remoteApplicationBinding
      2
      (ReadApplication remoteQuery)
      remoteDuplicate
  duplicateValues <- readResult 2 duplicateReadEffects
  require
    "a duplicate delivery must not reapply the semantic publication"
    (duplicateValues == firstValues)

  (localAfterCompleted, completedEffects) <-
    peerControlFromEffectsStep
      duplicateAcknowledgementEffects
      "apply replayed completed acknowledgement"
      21
      localBinding2
      duplicateAcknowledgement
      localReactivated
  (localReleased, completedReplayEffects) <-
    peerControlFromEffectsStep
      duplicateAcknowledgementEffects
      "replay completed acknowledgement"
      22
      localBinding2
      duplicateAcknowledgement
      localAfterCompleted
  require
    "completion acknowledgement application and replay are silent"
    ( all
        effectBatchIsEmpty
        [completedEffects, completedReplayEffects]
    )

  (localAfterReleaseRetry, postReleaseRetryEffects) <-
    applicationCallStep
      "exact application retry after completed release"
      23
      localSession
      localApplicationBinding
      1
      writeCall
      localReleased
  require
    "a post-release retry must reoffer only the retained application reply"
    (effectBatchMembers postReleaseRetryEffects == [writeReply])
  require
    "a post-release retry must not recreate peer work"
    (null (dispatchEffects postReleaseRetryEffects))

  -- No remote step occurs for the application retry. Keeping the same token in
  -- the subsequent trace makes that one-sided ownership explicit.
  let remoteAfterReleaseRetry = remoteAfterDuplicateRead
  require
    "a post-release application retry must not step or change H2"
    (remoteAfterReleaseRetry == remoteAfterDuplicateRead)
  (localLostAfterCompletion, _) <-
    runtimeStep
      "lose the completed local binding"
      24
      (PeerBindingLost localBinding2)
      localAfterReleaseRetry
  (remoteLostAfterCompletion, _) <-
    runtimeStep
      "lose the completed remote binding"
      14
      (PeerBindingLost remoteBinding2)
      remoteAfterReleaseRetry
  Connected localFinal remoteFinal localBinding3 remoteBinding3 _ _ localFinalHelloEffects remoteFinalHelloEffects <-
    connect
      "post-completion reconnection"
      (nonce seed 3)
      25
      15
      26
      localLostAfterCompletion
      remoteLostAfterCompletion
  require
    "post-completion Hello remains free of bulk repair control"
    (all helloControlsAreHandshakeOnly [localFinalHelloEffects, remoteFinalHelloEffects])
  finalRemoteResume <-
    exactlyOne "post-completion remote resume offer" (resumeOfferControls remoteFinalHelloEffects)
  finalLocalResume <-
    exactlyOne "post-completion local resume offer" (resumeOfferControls localFinalHelloEffects)
  (_, finalLocalEffects) <-
    peerControlFromEffectsStep
      remoteFinalHelloEffects
      "apply post-completion remote resume offer"
      27
      localBinding3
      finalRemoteResume
      localFinal
  (_, finalRemoteEffects) <-
    peerControlFromEffectsStep
      localFinalHelloEffects
      "apply post-completion local resume offer"
      16
      remoteBinding3
      finalLocalResume
      remoteFinal
  let postReleaseEffects =
        effectBatchMembers localFinalHelloEffects
          <> effectBatchMembers finalLocalEffects
          <> effectBatchMembers finalRemoteEffects
  require
    "completed work must remain released across another Hello/resume cycle"
    (not (any isDispatchEffect postReleaseEffects))

  pure
    TraceSummary
      { traceInitialAttempt = initialAttempt,
        traceRetransmittedAttempt = retransmittedAttempt,
        traceFirstRead = firstValues,
        traceDuplicateRead = duplicateValues,
        traceAcknowledgements = [firstAcknowledgement],
        tracePostReleaseRetryEffects = effectBatchMembers postReleaseRetryEffects,
        tracePostReleaseEffects = postReleaseEffects
      }

data Connected
  = Connected
      HeraldState
      HeraldState
      PeerBinding
      PeerBinding
      PeerCandidate
      PeerHello
      EffectBatch
      EffectBatch

connect ::
  String ->
  ConnectionNonce ->
  Word64 ->
  Word64 ->
  Word64 ->
  HeraldState ->
  HeraldState ->
  Either String Connected
connect context openedNonce localOfferAt remoteAcceptAt localAcceptAt local remote = do
  let localAddresses = Set.singleton (peerAddress "pair-local.example:4040")
      remoteAddresses = Set.singleton (peerAddress "pair-remote.example:4040")
  (localOffered, offerEffects) <-
    peerStep
      (context <> ": open candidate")
      localOfferAt
      (PeerCandidateOpened (peerCandidateOpened openedNonce localAddresses Nothing))
      local
  (candidate, localHello) <-
    exactlyOne (context <> ": local Hello") (sentCandidates offerEffects)
  require
    (context <> ": local Discovery did not preserve its address hints")
    (peerHelloAdvertisedAddresses localHello == localAddresses)
  (remoteAccepted, remoteEffects) <-
    peerStep
      (context <> ": accept local Hello")
      remoteAcceptAt
      (currentPeerHelloReceived remote candidate remoteAddresses localHello)
      remote
  remoteBinding <- acceptedBinding (context <> ": remote binding") remoteEffects
  (responseCandidate, remoteHello) <-
    exactlyOne (context <> ": reciprocal Hello") (sentCandidates remoteEffects)
  require
    (context <> ": reciprocal Hello changed candidate correlation")
    (responseCandidate == candidate)
  require
    (context <> ": remote Discovery did not author its own address hints")
    (peerHelloAdvertisedAddresses remoteHello == remoteAddresses)
  (localAccepted, localEffects) <-
    peerStep
      (context <> ": accept reciprocal Hello")
      localAcceptAt
      (currentPeerHelloReceived localOffered responseCandidate localAddresses remoteHello)
      localOffered
  localBinding <- acceptedBinding (context <> ": local binding") localEffects
  pure
    ( Connected
        localAccepted
        remoteAccepted
        localBinding
        remoteBinding
        responseCandidate
        remoteHello
        localEffects
        remoteEffects
    )

data OpenApplication
  = OpenApplication
      HeraldState
      ApplicationSessionId
      ApplicationSessionBinding
      Access.ApplicationStartupAccess

openApplication ::
  String ->
  Word64 ->
  Word64 ->
  Word64 ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  BootstrapManifestId ->
  HeraldState ->
  Either String OpenApplication
openApplication context observed laneNumber nonceNumber genesis bootstraps bootstrap state = do
  attachment <-
    maybe
      (Left (context <> ": missing primordial application attachment"))
      Right
      (primordialApplicationAttachment genesis bootstraps bootstrap)
  (successor, effects) <-
    stepAt
      context
      observed
      ( ApplicationSessionInput
          ( OpenApplicationSession
              (candidateApplicationLane laneNumber)
              attachment
              (clientNonce nonceNumber)
          )
      )
      state
  acceptance <- exactlyOne (context <> ": session acceptance") (sessionAcceptances effects)
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ access ->
      pure
        ( OpenApplication
            successor
            session
            (sessionAcceptanceBinding acceptance)
            access
        )
    other -> Left (context <> ": expected SessionOpened, got " <> show other)

applicationCallStep ::
  String ->
  Word64 ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Word64 ->
  ApplicationOperation ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
applicationCallStep context observed session binding identifier call =
  stepAt
    context
    observed
    ( ApplicationRequestInput
        ( CallApplicationRequest
            binding
            session
            (requestId identifier)
            call
        )
    )

peerStep ::
  String ->
  Word64 ->
  PeerIngress ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
peerStep context observed ingress =
  stepAt context observed (PeerInput ingress)

peerControlStep ::
  String ->
  Word64 ->
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
peerControlStep context observed binding control =
  peerStep context observed (PeerControlReceived binding control)

peerControlFromEffectsStep :: EffectBatch -> String -> Word64 -> PeerBinding -> PeerControl -> HeraldState -> Either String (HeraldState, EffectBatch)
peerControlFromEffectsStep effects context observed binding control state = do
  progress <- controlProgress effects control
  peerStep context observed (PeerControlReceivedWithProgress binding progress control) state

controlProgress :: EffectBatch -> PeerControl -> Either String ReceiptRetirement
controlProgress effects control = exactlyOne "peer control receipt header" [progress | (progress, emitted) <- sentControlMessages effects, emitted == control]

runtimeStep ::
  String ->
  Word64 ->
  RuntimeObservation ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
runtimeStep context observed observation =
  stepAt context observed (RuntimeObserved observation)

stepAt ::
  String ->
  Word64 ->
  HeraldInputBody ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
stepAt context observed body state =
  checked
    context
    (verifiedStepHerald (heraldInput (monotonicInstant observed) body) state)

initialize ::
  String ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  Either String HeraldState
initialize context genesis bootstraps = do
  (state, effects) <-
    checked
      context
      (initialHerald (monotonicInstant 0) genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  require (context <> ": initialization must emit one Oracle watch and one isolation grace timer") (initialEffectsAreOracleWatchAndGrace effects)
  pure state

checkedTopology ::
  CheckedHeraldGenesis ->
  Either String CheckedInitialBootstraps
checkedTopology genesis =
  checked
    "common checked Step-7 topology"
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest [firstLocalBootstrapId, fixtureRemoteBootstrapId])
        topologyManifest
    )

topologyManifest :: InitialTopologyManifest
topologyManifest =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        firstLocalBootstrapId
        fixtureRemoteBootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        firstLocalBootstrapId
        (heraldMemberEpoch fixtureRemoteMember)
        SortDefinitionRole
    ]

writeResult :: EffectBatch -> Either String (HeraldEffect, SortId, PeerDispatchTicket)
writeResult effects = do
  sortId <-
    admittedApplicationSortId
      <$> checked
        "declared sort definition"
        ( admitApplicationSortDefinition
            (DeclaredSortDefinition declaredDescriptor Nothing)
        )
  reply <- exactlyOne "write application reply" (applicationReplyEffects effects)
  case reply of
    SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed _ (WriteCompleted (SortDefinitionWritten actualSort)))
        )
        | actualSort == sortId ->
            pure ()
    other -> Left ("unexpected write reply: " <> show other)
  ticket <- exactlyOne "write dispatch ticket" (scheduledTickets effects)
  pure (reply, sortId, ticket)

readResult :: Word64 -> EffectBatch -> Either String [ApplicationValue]
readResult expectedRequest effects = case applicationReplyEffects effects of
  [ SendApplicationReply
      _
      (RetainedRequestReply _ (Completed actualRequest (ReadCompleted values)))
    ]
      | actualRequest == requestId expectedRequest -> Right values
  actual -> Left ("unexpected application read result: " <> show actual)

completionAcknowledgement :: String -> EffectBatch -> Either String PeerControl
completionAcknowledgement context effects = case streamAcknowledgements effects of
  [completed@PeerStreamCompleted {}] -> Right completed
  actual -> Left (context <> ": unexpected control batch " <> show actual)

streamAcknowledgements :: EffectBatch -> [PeerControl]
streamAcknowledgements effects =
  [ control
  | control <- sentControls effects,
    case control of
      PeerStreamReceived {} -> True
      PeerStreamCompleted {} -> True
      _ -> False
  ]

sortDefinitionAccess ::
  String ->
  Access.ApplicationStartupAccess ->
  Either String Access.PredefinedAccess
sortDefinitionAccess context startup =
  maybe
    (Left (context <> ": missing"))
    Right
    ( find
        ((== Access.SortDefinitionRole) . Access.predefinedAccessRole)
        (conventionalStartupPairs startup)
    )

acceptedBinding :: String -> EffectBatch -> Either String PeerBinding
acceptedBinding context effects =
  exactlyOne
    context
    [ binding
    | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects
    ]

sentCandidates :: EffectBatch -> [(PeerCandidate, PeerHello)]
sentCandidates effects =
  [ (candidate, hello)
  | SendPeerCandidate candidate _ _ hello <- effectBatchMembers effects
  ]

sessionAcceptances :: EffectBatch -> [ApplicationSessionAcceptance]
sessionAcceptances effects =
  [ acceptance
  | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers effects
  ]

applicationReplyEffects :: EffectBatch -> [HeraldEffect]
applicationReplyEffects effects =
  [ effect
  | effect@SendApplicationReply {} <- effectBatchMembers effects
  ]

placementControls :: EffectBatch -> [PeerControl]
placementControls effects =
  [ control
  | control@PeerPlacementUpdate {} <- sentControls effects
  ]

frontierAdvanceControls :: EffectBatch -> [PeerControl]
frontierAdvanceControls effects =
  [ control
  | control@PeerStreamFrontierAdvanced {} <- sentControls effects
  ]

resumeOfferControls :: EffectBatch -> [PeerControl]
resumeOfferControls effects =
  [ control
  | control@PeerStreamResumeOffered {} <- sentControls effects
  ]

resumeResponseControls :: EffectBatch -> [PeerControl]
resumeResponseControls effects =
  [ control
  | control@PeerStreamResumeAccepted {} <- sentControls effects
  ]

sentControls :: EffectBatch -> [PeerControl]
sentControls = map snd . sentControlMessages

sentControlMessages :: EffectBatch -> [(ReceiptRetirement, PeerControl)]
sentControlMessages = concatMap message . effectBatchMembers
  where
    message (SendPeerControl _ control) = [(mempty, control)]
    message (SendPeerControlWithProgress _ progress control) = [(progress, control)]
    message _ = []

helloControlsAreHandshakeOnly :: EffectBatch -> Bool
helloControlsAreHandshakeOnly effects = case sentControls effects of
  [PeerKnownHeralds _, PeerPlacementUpdate _, PeerStreamResumeOffered _] -> True
  _ -> False

resumeAcceptedLeadsRepair :: EffectBatch -> Bool
resumeAcceptedLeadsRepair effects = case sentControls effects of
  PeerStreamResumeAccepted _ : repairs ->
    all isRepairControl repairs
  _ -> False

isRepairControl :: PeerControl -> Bool
isRepairControl PeerStructuralAppliedReported {} = True
isRepairControl PeerTopologyCutAnnounced {} = True
isRepairControl PeerTopologyCutAccepted {} = True
isRepairControl PeerTopologyCutEstablished {} = True
isRepairControl PeerTopologyCutEstablishedAcknowledged {} = True
isRepairControl PeerAlignmentControl {} = True
isRepairControl PeerAlignmentEvidenceDelivered {} = True
isRepairControl PeerAlignmentDeliveryProgress = True
isRepairControl _ = False

scheduledTickets :: EffectBatch -> [PeerDispatchTicket]
scheduledTickets effects =
  [ ticket
  | SchedulePeerDispatch ticket <- effectBatchMembers effects
  ]

sentAttempts :: EffectBatch -> [PeerDispatchAttempt PeerLogicalPayload]
sentAttempts = map snd . sentAttemptMessages

sentAttemptMessages :: EffectBatch -> [(ReceiptRetirement, PeerDispatchAttempt PeerLogicalPayload)]
sentAttemptMessages = concatMap message . effectBatchMembers
  where
    message (SendPeerItem _ attempt) = [(mempty, attempt)]
    message (SendPeerItemWithProgress _ progress attempt) = [(progress, attempt)]
    message _ = []

dispatchEffects :: EffectBatch -> [HeraldEffect]
dispatchEffects = filter isDispatchEffect . effectBatchMembers

isDispatchEffect :: HeraldEffect -> Bool
isDispatchEffect SchedulePeerDispatch {} = True
isDispatchEffect SendPeerItem {} = True
isDispatchEffect SendPeerItemWithProgress {} = True
isDispatchEffect _ = False

isRecoveryArmAndSoleDialFor :: PeerBinding -> EffectBatch -> Bool
isRecoveryArmAndSoleDialFor binding effects = case effectBatchMembers effects of
  [ArmTimer _ _, DialPeer intent] ->
    peerDialIntentHeraldEpoch intent == peerBindingRemoteHeraldEpoch binding
  _ -> False

exactlyOne :: String -> [value] -> Either String value
exactlyOne _ [value] = Right value
exactlyOne context values =
  Left
    ( context
        <> ": expected exactly one value, observed "
        <> show (length values)
    )

firstOne :: String -> [value] -> Either String value
firstOne _ (value : _) = Right value
firstOne context [] = Left (context <> ": expected at least one value")

require :: String -> Bool -> Either String ()
require _ True = Right ()
require problem False = Left problem

checked :: (Show problem) => String -> Either problem value -> Either String value
checked context = either (Left . ((context <> ": ") <>) . show) Right

nonce :: Word16 -> Word64 -> ConnectionNonce
nonce seed offset = connectionNonce (seedBase seed + 1000 + offset)

lane :: Word16 -> Word64 -> Word64
lane seed offset = seedBase seed + 2000 + offset

applicationNonce :: Word16 -> Word64 -> Word64
applicationNonce seed offset = seedBase seed + 3000 + offset

seedBase :: Word16 -> Word64
seedBase seed = fromIntegral seed * 16

firstLocalBootstrapId :: BootstrapManifestId
firstLocalBootstrapId = case fixtureLocalBootstrapIds of
  first : _ -> first
  [] -> error "fixture has no local bootstrap"

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections =
        [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }
