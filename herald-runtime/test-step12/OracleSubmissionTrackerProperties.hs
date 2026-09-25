module OracleSubmissionTrackerProperties (tests) where

import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryReadMVar,
  )
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (HeraldEpoch, controlIndex)
import Eclips.Domain.ProcessStart (processStartResidence)
import Eclips.Herald.OracleClient
  ( OracleRetryPurpose (..),
    oracleObservedTerm,
  )
import Eclips.Herald.Runtime.TCP
  ( HeraldTcpConfigurationError (PeerDialRetryDelayMustBePositive),
    PeerDialRetryDelay,
    peerDialRetryDelayMicroseconds,
  )
import Eclips.Herald.Runtime.TCP.Internal.Oracle
  ( OracleRetryPacing,
    OracleSubmissionProgressEvent (..),
    OracleSubmissionTracker,
    advanceOracleRetryPacing,
    advanceOracleRetryPacingWithTiming,
    associateOracleSubmissionRetry,
    cancelOracleRetryWorker,
    claimOracleProgress,
    claimOracleSubmission,
    emptyOracleSubmissionTracker,
    initialOracleRetryPacing,
    initialOracleSubmissionProgress,
    newOracleRetryWorkersIO,
    observeOracleSubmissionProgress,
    oracleSubmissionProgressRetainedCounts,
    oracleSubmissionTrackerRetainedCounts,
    releaseDeferredOracleSubmission,
    releaseOracleProgress,
    releaseOracleSubmissionRetries,
    resetOracleRetryPacing,
    retireOracleSubmissionProgress,
    retireOracleSubmissions,
    scheduleOracleRetryWorker,
  )
import Eclips.Oracle.Canonical (CanonicalOracleEnvelope, canonicalizeOracleEnvelope)
import Eclips.Oracle.Command
  ( oracleEnvelope,
    oracleEnvelopeWithReceiptRetirement,
    retireOracleReceiptsCommand,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
  )
import Eclips.Oracle.Progress qualified as Progress
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Public.Types.Timing
import Step12Fixtures (step12H1HeraldEpoch, step12StartedProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, conjoin, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup
    "physical Oracle submission coalescing"
    [ testCase "same receipt high-water NotReady releases only the exact full offer" caseProgressNotReady,
      testProperty "derived retry pacing respects scaled class bases and cap" propDerivedRetryPacing,
      testProperty
        "a request is sent at most once per lane"
        propClaimOnce,
      testProperty
        "one exact retry releases only its NotReady request"
        propReleaseOnlyNotReady,
      testProperty
        "reordered retirement carriers exclude late sends, retries and settlements per home"
        propRetirementReordering,
      testProperty
        "flowing piggyback retirement bounds tracking by the unsettled window and idle flush drains it"
        propRetirementCycles,
      testProperty
        "connection retry pacing starts at its configured delay and grows monotonically to its bound"
        propConnectionRetryPacingBounded,
      testProperty
        "connection and submission retry pacing advance independently"
        propRetryPacingClassesIndependent,
      testProperty
        "submission retry pacing grows monotonically to the connection bound"
        propSubmissionRetryPacingBounded,
      testProperty
        "a successful-binding reset restores the first connection delay"
        propConnectionRetryPacingReset,
      testProperty
        "a useful-progress reset restores only the submission delay"
        propSubmissionRetryPacingReset,
      testProperty
        "progress admits only strict prefixes, first settlements, and newer usable leaders"
        propUsefulSubmissionProgressIsStrict,
      testProperty
        "all current NotReady requests share one exact pending retry"
        propNotReadyRequestsShareExactRetry,
      testProperty
        "serialized NotReady association closes both elapsed-input orderings"
        propNotReadyAssociationOrderIsRaceProof,
      testProperty
        "a zero configured delay is rejected before Oracle pacing"
        propZeroRetryDelayRejected,
      testCase
        "a deferred request stays suppressed until release, then reoffers exactly once"
        caseDeferredRelease,
      testCase
        "a fired retry unregisters before exposing its elapsed action"
        caseFiredRetryUnregistersBeforeElapsed,
      testCase
        "cancelling a sleeping retry promptly stops its registered worker"
        caseSleepingRetryCancellation
    ]

propDerivedRetryPacing :: Word8 -> Bool
propDerivedRetryPacing scale =
  let target = either (error . show) id (takeoverTarget (500_000 * (1 + fromIntegral scale)))
      policy = deriveTimingPolicy target
      delays purpose = take 24 (go initialOracleRetryPacing)
        where
          go previous = let (next, delay) = advanceOracleRetryPacingWithTiming policy purpose previous in delay : go next
      connections = delays OracleConnectionRetry
      submissions = delays OracleSubmissionRetry
      valid base samples = case (samples, reverse samples) of
        (first : _, final : _) -> first == base && final == timingRetryCapMicroseconds policy && and (zipWith (<=) samples (drop 1 samples))
        _ -> False
   in valid (timingReconnectInitialMicroseconds policy) connections
        && valid (timingSubmissionInitialMicroseconds policy) submissions

propClaimOnce :: Bool -> Bool
propClaimOnce chooseSecond =
  let request = if chooseSecond then secondRequest else firstRequest
      (claimed, first) = claimOracleSubmission request emptyOracleSubmissionTracker
      (_, duplicate) = claimOracleSubmission request claimed
   in first && not duplicate

propReleaseOnlyNotReady :: Bool -> Bool
propReleaseOnlyNotReady chooseSecond =
  let blockedRequest = if chooseSecond then secondRequest else firstRequest
      acceptedRequest = if chooseSecond then firstRequest else secondRequest
      claimed = claimBoth
      associated =
        associateOracleSubmissionRetry
          blockedRequest
          (1 :: Word64)
          claimed
      wrongRetry = releaseOracleSubmissionRetries (2 :: Word64) associated
      correctlyReleased = releaseOracleSubmissionRetries (1 :: Word64) associated
   in not (claimWouldSend blockedRequest wrongRetry)
        && not (claimWouldSend acceptedRequest wrongRetry)
        && claimWouldSend blockedRequest correctlyReleased
        && not (claimWouldSend acceptedRequest correctlyReleased)

-- The reference keeps the original request population throughout and tracks
-- only the greatest independently supplied frontier for each home. Every
-- intermediate compacted state is checked, including regressed carriers and
-- late retry releases after the corresponding individual records are gone.
propRetirementReordering :: Word8 -> [(Bool, Word8, Bool)] -> Property
propRetirementReordering populationSeed schedule =
  conjoin (map checkSnapshot snapshots)
  where
    population = 1 + fromIntegral (populationSeed `mod` 32)
    homes = [firstHome, step12H1HeraldEpoch]
    requests = [oracleClientRequestId home sequenceNumber | home <- homes, sequenceNumber <- [1 .. population]]
    initialTracker = foldl' claimAndAssociate emptyOracleSubmissionTracker requests
    claimAndAssociate tracker request =
      associateOracleSubmissionRetry request (oracleClientRequestSequence request `mod` 3) (fst (claimOracleSubmission request tracker))
    initialProgress =
      foldl'
        (\progress event -> fst (observeOracleSubmissionProgress event progress))
        initialOracleSubmissionProgress
        ( OracleCommittedPrefixProgress (controlIndex 37)
            : OracleServiceReadyLeaderProgress (oracleObservedTerm 5)
            : map OracleTerminalRequestProgress requests
        )
    snapshots = scanl advance (initialTracker, initialProgress, (0, 0)) schedule
    advance (tracker, progress, (firstFloor, secondFloor)) (otherHome, frontierSeed, standalone) =
      let home = if otherHome then step12H1HeraldEpoch else firstHome
          through = 1 + fromIntegral frontierSeed `mod` population
          canonical = retirementCarrier home (population + 1) through standalone
          frontiers = if otherHome then (firstFloor, max secondFloor through) else (max firstFloor through, secondFloor)
       in (retireOracleSubmissions canonical tracker, retireOracleSubmissionProgress canonical progress, frontiers)
    checkSnapshot (tracker, progress, frontiers@(firstFloor, secondFloor)) =
      counterexample ("retirement frontiers " <> show frontiers)
        $ and
          [ oracleSubmissionTrackerRetainedCounts tracker == (remaining, remaining, floorCount),
            oracleSubmissionProgressRetainedCounts progress == (remaining, floorCount),
            all (\request -> claimWouldSend request released == beyondFloor request) requests,
            all staleRequestIsInert (filter (not . beyondFloor) requests),
            all (\request -> observeOracleSubmissionProgress (OracleTerminalRequestProgress request) progress == (progress, False)) requests,
            not (snd (observeOracleSubmissionProgress (OracleCommittedPrefixProgress (controlIndex 37)) progress)),
            snd (observeOracleSubmissionProgress (OracleCommittedPrefixProgress (controlIndex 38)) progress),
            not (snd (observeOracleSubmissionProgress (OracleServiceReadyLeaderProgress (oracleObservedTerm 5)) progress)),
            snd (observeOracleSubmissionProgress (OracleServiceReadyLeaderProgress (oracleObservedTerm 6)) progress),
            all carrierFormsAgree homes,
            retireOracleSubmissions (ordinaryCarrier firstHome (population + 1)) tracker == tracker,
            retireOracleSubmissionProgress (ordinaryCarrier firstHome (population + 1)) progress == progress
          ]
      where
        remaining = fromIntegral (2 * population - firstFloor - secondFloor)
        floorCount = length (filter (> 0) [firstFloor, secondFloor])
        released = foldr releaseOracleSubmissionRetries tracker [0, 1, 2]
        beyondFloor request =
          oracleClientRequestSequence request
            > if oracleClientRequestHome request == firstHome then firstFloor else secondFloor
        staleRequestIsInert request =
          claimOracleSubmission request (releaseDeferredOracleSubmission request (associateOracleSubmissionRetry request 7 released)) == (released, False)
        carrierFormsAgree home =
          let piggyback = retirementCarrier home (population + 1) population False
              standalone = retirementCarrier home (population + 1) population True
           in retireOracleSubmissions piggyback tracker == retireOracleSubmissions standalone tracker
                && retireOracleSubmissionProgress piggyback progress == retireOracleSubmissionProgress standalone progress

propRetirementCycles :: Word8 -> Word8 -> Property
propRetirementCycles cycleSeed windowSeed =
  conjoin
    ( [ counterexample ("cycle " <> show sequenceNumber <> ", window " <> show window)
          $ and
            [ sent,
              settled,
              oracleSubmissionTrackerRetainedCounts tracker == (retained, retained, floorCount),
              oracleSubmissionProgressRetainedCounts progress == (retained, floorCount)
            ]
      | (sequenceNumber, (tracker, progress, sent, settled)) <- zip [1 .. cycles] (drop 1 snapshots),
        let retained = fromIntegral (min sequenceNumber window),
        let floorCount = if sequenceNumber > window then 1 else 0
      ]
        <> [counterexample "standalone idle flush releases all individual tracking" drained]
    )
  where
    cycles = 1 + fromIntegral cycleSeed
    window = 1 + fromIntegral (windowSeed `mod` 16)
    snapshots = scanl advance (emptyOracleSubmissionTracker, initialOracleSubmissionProgress, False, False) [1 .. cycles]
    advance (tracker, progress, _, _) sequenceNumber =
      let request = fixtureRequest sequenceNumber
          canonical =
            if sequenceNumber > window
              then retirementCarrier firstHome sequenceNumber (sequenceNumber - window) False
              else ordinaryCarrier firstHome sequenceNumber
          (claimed, sent) = claimOracleSubmission request (retireOracleSubmissions canonical tracker)
          associated = associateOracleSubmissionRetry request (sequenceNumber :: Word64) claimed
          (observed, settled) = observeOracleSubmissionProgress (OracleTerminalRequestProgress request) (retireOracleSubmissionProgress canonical progress)
       in (associated, observed, sent, settled)
    drained = case reverse snapshots of
      (tracker, progress, _, _) : _ ->
        let canonical = retirementCarrier firstHome (cycles + 1) cycles True
            retiredTracker = retireOracleSubmissions canonical tracker
            retiredProgress = retireOracleSubmissionProgress canonical progress
         in oracleSubmissionTrackerRetainedCounts retiredTracker == (0, 0, 1)
              && oracleSubmissionProgressRetainedCounts retiredProgress == (0, 1)
              && and
                [ claimOracleSubmission request (releaseOracleSubmissionRetries sequenceNumber (associateOracleSubmissionRetry request sequenceNumber retiredTracker)) == (retiredTracker, False)
                    && observeOracleSubmissionProgress (OracleTerminalRequestProgress request) retiredProgress == (retiredProgress, False)
                | sequenceNumber <- [1 .. cycles],
                  let request = fixtureRequest sequenceNumber
                ]
      [] -> False

ordinaryCarrier :: HeraldEpoch -> Word64 -> CanonicalOracleEnvelope
ordinaryCarrier home sequenceNumber =
  canonicalizeOracleEnvelope
    (oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home (startProcessEpochCommand step12StartedProcess))

retirementCarrier :: HeraldEpoch -> Word64 -> Word64 -> Bool -> CanonicalOracleEnvelope
retirementCarrier home sequenceNumber through standalone =
  canonicalizeOracleEnvelope
    ( if standalone
        then oracleEnvelope (oracleClientRequestId home through) Nothing home (retireOracleReceiptsCommand through)
        else
          oracleEnvelopeWithReceiptRetirement
            through
            (oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home (startProcessEpochCommand step12StartedProcess))
    )

firstHome :: HeraldEpoch
firstHome = processStartResidence step12StartedProcess

propConnectionRetryPacingBounded :: Word8 -> Word64 -> Bool
propConnectionRetryPacingBounded countSeed configured =
  let retryDelay = checkedRetryDelay configured
      count = 1 + fromIntegral countSeed
      delays = connectionDelays count retryDelay initialOracleRetryPacing
      effectiveConfigured = max 1 configured
      maximumDelay = max effectiveConfigured 1_000_000
   in case delays of
        [] -> False
        first : remaining ->
          first == effectiveConfigured
            && and (zipWith (<=) delays remaining)
            && all (<= maximumDelay) delays

propRetryPacingClassesIndependent :: Word64 -> Bool
propRetryPacingClassesIndependent configured =
  let retryDelay = checkedRetryDelay configured
      effectiveConfigured = max 1 configured
      submissionBase = max 100_000 effectiveConfigured
      (afterConnection, firstConnectionDelay) =
        advanceOracleRetryPacing
          OracleConnectionRetry
          retryDelay
          initialOracleRetryPacing
      (connectionThenSubmission, firstSubmissionDelay) =
        advanceOracleRetryPacing
          OracleSubmissionRetry
          retryDelay
          afterConnection
      (afterSubmission, alternateSubmissionDelay) =
        advanceOracleRetryPacing
          OracleSubmissionRetry
          retryDelay
          initialOracleRetryPacing
      (submissionThenConnection, alternateConnectionDelay) =
        advanceOracleRetryPacing
          OracleConnectionRetry
          retryDelay
          afterSubmission
      (_, secondConnectionDelay) =
        advanceOracleRetryPacing
          OracleConnectionRetry
          retryDelay
          connectionThenSubmission
      (_, secondSubmissionDelay) =
        advanceOracleRetryPacing
          OracleSubmissionRetry
          retryDelay
          connectionThenSubmission
      expectedSecondConnectionDelay =
        fromInteger
          ( min
              (toInteger (max effectiveConfigured 1_000_000))
              (2 * toInteger effectiveConfigured)
          )
      expectedSecondSubmissionDelay =
        fromInteger
          ( min
              (toInteger (max submissionBase 1_000_000))
              (2 * toInteger submissionBase)
          )
   in connectionThenSubmission == submissionThenConnection
        && firstConnectionDelay == effectiveConfigured
        && firstSubmissionDelay == submissionBase
        && alternateConnectionDelay == effectiveConfigured
        && alternateSubmissionDelay == submissionBase
        && secondConnectionDelay == expectedSecondConnectionDelay
        && secondSubmissionDelay == expectedSecondSubmissionDelay

propSubmissionRetryPacingBounded :: Word8 -> Word64 -> Bool
propSubmissionRetryPacingBounded countSeed configured =
  let retryDelay = checkedRetryDelay configured
      count = 1 + fromIntegral countSeed
      delays = retryDelays OracleSubmissionRetry count retryDelay initialOracleRetryPacing
      effectiveConfigured = max 1 configured
      submissionBase = max 100_000 effectiveConfigured
      maximumDelay = max submissionBase 1_000_000
   in case delays of
        [] -> False
        first : remaining ->
          first == submissionBase
            && and (zipWith (<=) delays remaining)
            && all (<= maximumDelay) delays

propConnectionRetryPacingReset :: Word8 -> Word64 -> Bool
propConnectionRetryPacingReset countSeed configured =
  let retryDelay = checkedRetryDelay configured
      effectiveConfigured = max 1 configured
      advanced =
        advanceConnections
          (1 + fromIntegral countSeed)
          retryDelay
          initialOracleRetryPacing
      (afterResetRetry, resetDelay) =
        advanceOracleRetryPacing
          OracleConnectionRetry
          retryDelay
          (resetOracleRetryPacing OracleConnectionRetry advanced)
      (expectedState, expectedDelay) =
        advanceOracleRetryPacing
          OracleConnectionRetry
          retryDelay
          initialOracleRetryPacing
   in afterResetRetry == expectedState
        && resetDelay == expectedDelay
        && resetDelay == effectiveConfigured

propSubmissionRetryPacingReset :: Word8 -> Word64 -> Bool
propSubmissionRetryPacingReset countSeed configured =
  let retryDelay = checkedRetryDelay configured
      count = 1 + fromIntegral countSeed
      afterConnections = advanceRetries OracleConnectionRetry count retryDelay initialOracleRetryPacing
      advanced = advanceRetries OracleSubmissionRetry count retryDelay afterConnections
      (_, expectedConnectionDelay) =
        advanceOracleRetryPacing OracleConnectionRetry retryDelay advanced
      reset = resetOracleRetryPacing OracleSubmissionRetry advanced
      (_, observedConnectionDelay) =
        advanceOracleRetryPacing OracleConnectionRetry retryDelay reset
      (_, observedSubmissionDelay) =
        advanceOracleRetryPacing OracleSubmissionRetry retryDelay reset
   in observedConnectionDelay == expectedConnectionDelay
        && observedSubmissionDelay == max 100_000 (max 1 configured)

propUsefulSubmissionProgressIsStrict :: Bool
propUsefulSubmissionProgressIsStrict =
  let (afterInitialPrefix, initialPrefix) =
        observeOracleSubmissionProgress
          (OracleCommittedPrefixProgress (controlIndex 0))
          initialOracleSubmissionProgress
      (afterPrefix, firstPrefix) =
        observeOracleSubmissionProgress
          (OracleCommittedPrefixProgress (controlIndex 2))
          afterInitialPrefix
      (afterSamePrefix, samePrefix) =
        observeOracleSubmissionProgress
          (OracleCommittedPrefixProgress (controlIndex 2))
          afterPrefix
      (afterStalePrefix, stalePrefix) =
        observeOracleSubmissionProgress
          (OracleCommittedPrefixProgress (controlIndex 1))
          afterSamePrefix
      (afterAdvancedPrefix, advancedPrefix) =
        observeOracleSubmissionProgress
          (OracleCommittedPrefixProgress (controlIndex 3))
          afterStalePrefix
      (afterSettlement, firstSettlement) =
        observeOracleSubmissionProgress
          (OracleTerminalRequestProgress firstRequest)
          afterAdvancedPrefix
      (afterDuplicateSettlement, duplicateSettlement) =
        observeOracleSubmissionProgress
          (OracleTerminalRequestProgress firstRequest)
          afterSettlement
      (afterOtherSettlement, otherSettlement) =
        observeOracleSubmissionProgress
          (OracleTerminalRequestProgress secondRequest)
          afterDuplicateSettlement
      (afterLeader, firstLeader) =
        observeOracleSubmissionProgress
          (OracleServiceReadyLeaderProgress (oracleObservedTerm 5))
          afterOtherSettlement
      (afterSameLeader, sameLeader) =
        observeOracleSubmissionProgress
          (OracleServiceReadyLeaderProgress (oracleObservedTerm 5))
          afterLeader
      (afterStaleLeader, staleLeader) =
        observeOracleSubmissionProgress
          (OracleServiceReadyLeaderProgress (oracleObservedTerm 4))
          afterSameLeader
      (_, newerLeader) =
        observeOracleSubmissionProgress
          (OracleServiceReadyLeaderProgress (oracleObservedTerm 6))
          afterStaleLeader
   in and
        [ not initialPrefix,
          firstPrefix,
          not samePrefix,
          not stalePrefix,
          advancedPrefix,
          firstSettlement,
          not duplicateSettlement,
          otherSettlement,
          firstLeader,
          not sameLeader,
          not staleLeader,
          newerLeader
        ]

propNotReadyRequestsShareExactRetry :: Bool
propNotReadyRequestsShareExactRetry =
  let bothNotReady =
        associateOracleSubmissionRetry
          secondRequest
          (1 :: Word64)
          ( associateOracleSubmissionRetry
              firstRequest
              (1 :: Word64)
              claimBoth
          )
      duplicateAssociation =
        associateOracleSubmissionRetry firstRequest (1 :: Word64) bothNotReady
      wrongRelease = releaseOracleSubmissionRetries (2 :: Word64) duplicateAssociation
      exactRelease = releaseOracleSubmissionRetries (1 :: Word64) duplicateAssociation
   in not (claimWouldSend firstRequest wrongRelease)
        && not (claimWouldSend secondRequest wrongRelease)
        && claimWouldSend firstRequest exactRelease
        && claimWouldSend secondRequest exactRelease

propNotReadyAssociationOrderIsRaceProof :: Bool -> Bool
propNotReadyAssociationOrderIsRaceProof secondArrivesBeforeElapsed =
  let first = if secondArrivesBeforeElapsed then firstRequest else secondRequest
      second = if secondArrivesBeforeElapsed then secondRequest else firstRequest
      initialRetry = 1 :: Word64
      successorRetry = 2 :: Word64
      firstAssociated =
        associateOracleSubmissionRetry first initialRetry claimBoth
   in if secondArrivesBeforeElapsed
        then
          let released =
                releaseOracleSubmissionRetries
                  initialRetry
                  (associateOracleSubmissionRetry second initialRetry firstAssociated)
              (afterFirstSend, firstSent) = claimOracleSubmission first released
              (_, secondSent) = claimOracleSubmission second afterFirstSend
           in firstSent && secondSent
        else
          let afterFirstCancel =
                releaseOracleSubmissionRetries initialRetry firstAssociated
              (afterFirstReoffer, firstSent) =
                claimOracleSubmission first afterFirstCancel
              (afterSuppressedSecond, secondSentTooSoon) =
                claimOracleSubmission second afterFirstReoffer
              secondAssociated =
                associateOracleSubmissionRetry
                  second
                  successorRetry
                  afterSuppressedSecond
              afterSecondCancel =
                releaseOracleSubmissionRetries successorRetry secondAssociated
              (afterSecondReoffer, secondSent) =
                claimOracleSubmission second afterSecondCancel
              (_, firstSentTwice) =
                claimOracleSubmission first afterSecondReoffer
           in firstSent
                && not secondSentTooSoon
                && secondSent
                && not firstSentTwice

propZeroRetryDelayRejected :: Bool
propZeroRetryDelayRejected =
  peerDialRetryDelayMicroseconds 0
    == Left PeerDialRetryDelayMustBePositive

connectionDelays :: Int -> PeerDialRetryDelay -> OracleRetryPacing -> [Word64]
connectionDelays = retryDelays OracleConnectionRetry

retryDelays ::
  OracleRetryPurpose ->
  Int ->
  PeerDialRetryDelay ->
  OracleRetryPacing ->
  [Word64]
retryDelays purpose count retryDelay pacing
  | count <= 0 = []
  | otherwise =
      let (successor, delay) =
            advanceOracleRetryPacing purpose retryDelay pacing
       in delay : retryDelays purpose (count - 1) retryDelay successor

advanceConnections :: Int -> PeerDialRetryDelay -> OracleRetryPacing -> OracleRetryPacing
advanceConnections = advanceRetries OracleConnectionRetry

advanceRetries ::
  OracleRetryPurpose ->
  Int ->
  PeerDialRetryDelay ->
  OracleRetryPacing ->
  OracleRetryPacing
advanceRetries purpose count retryDelay pacing
  | count <= 0 = pacing
  | otherwise =
      let (successor, _) =
            advanceOracleRetryPacing purpose retryDelay pacing
       in advanceRetries purpose (count - 1) retryDelay successor

caseDeferredRelease :: IO ()
caseDeferredRelease = do
  assertBool
    "a stale duplicate emitted before owner release remains suppressed"
    (not (claimWouldSend firstRequest claimBoth))
  let released = releaseDeferredOracleSubmission firstRequest claimBoth
      (afterReoffer, firstReoffer) = claimOracleSubmission firstRequest released
      (_, duplicateReoffer) = claimOracleSubmission firstRequest afterReoffer
  assertBool "owner release admits the exact deferred request once" firstReoffer
  assertBool "the exact reoffer immediately closes its lane-local slot again" (not duplicateReoffer)
  assertBool
    "owner release does not reopen another submitted request"
    (not (claimWouldSend secondRequest released))

caseFiredRetryUnregistersBeforeElapsed :: IO ()
caseFiredRetryUnregistersBeforeElapsed = do
  workers <- newOracleRetryWorkersIO
  delayEntered <- newEmptyMVar
  releaseDelay <- newEmptyMVar
  fireEntered <- newEmptyMVar
  releaseFire <- newEmptyMVar
  completionCell <- newEmptyMVar
  scheduleOracleRetryWorker
    (spawnObserved completionCell)
    (putMVar delayEntered () >> takeMVar releaseDelay)
    (putMVar fireEntered () >> takeMVar releaseFire)
    workers
    firstRetry
  completion <- takeMVar completionCell
  takeMVar delayEntered
  putMVar releaseDelay ()
  fired <- timeout 1_000_000 (takeMVar fireEntered)
  assertBool "the registered timer reaches its elapsed action" (fired == Just ())

  cancelled <- timeout 1_000_000 (cancelOracleRetryWorker workers firstRetry)
  assertBool
    "cancelling the elapsed token cannot synchronously kill its producer"
    (cancelled == Just ())
  assertEqual
    "the elapsed action remains live until its own release"
    Nothing
    =<< tryReadMVar completion

  putMVar releaseFire ()
  joined <- timeout 1_000_000 (readMVar completion)
  assertBool "the elapsed worker completes after its action" (joined == Just ())

caseSleepingRetryCancellation :: IO ()
caseSleepingRetryCancellation = do
  workers <- newOracleRetryWorkersIO
  delayEntered <- newEmptyMVar
  releaseDelay <- newEmptyMVar
  fired <- newEmptyMVar
  completionCell <- newEmptyMVar
  scheduleOracleRetryWorker
    (spawnObserved completionCell)
    (putMVar delayEntered () >> takeMVar releaseDelay)
    (putMVar fired ())
    workers
    firstRetry
  completion <- takeMVar completionCell
  takeMVar delayEntered

  cancelled <- timeout 1_000_000 (cancelOracleRetryWorker workers firstRetry)
  assertBool "sleeping-timer cancellation remains prompt" (cancelled == Just ())
  joined <- timeout 1_000_000 (readMVar completion)
  assertBool "the cancelled timer worker is joined by its test scope" (joined == Just ())
  assertEqual "a cancelled sleeping timer emits no elapsed action" Nothing =<< tryReadMVar fired

spawnObserved :: MVar (MVar ()) -> IO () -> IO (Maybe ThreadId)
spawnObserved completionCell action = do
  completion <- newEmptyMVar
  thread <- forkFinally action (const (putMVar completion ()))
  putMVar completionCell completion
  pure (Just thread)

firstRetry :: Word64
firstRetry = 1

claimBoth :: OracleSubmissionTracker retry
claimBoth =
  let (withFirst, _) = claimOracleSubmission firstRequest emptyOracleSubmissionTracker
      (withSecond, _) = claimOracleSubmission secondRequest withFirst
   in withSecond

claimWouldSend :: OracleClientRequestId -> OracleSubmissionTracker retry -> Bool
claimWouldSend request = snd . claimOracleSubmission request

firstRequest :: OracleClientRequestId
firstRequest = fixtureRequest 1

secondRequest :: OracleClientRequestId
secondRequest = fixtureRequest 2

fixtureRequest :: Word64 -> OracleClientRequestId
fixtureRequest =
  oracleClientRequestId
    (processStartResidence step12StartedProcess)

checkedRetryDelay :: Word64 -> PeerDialRetryDelay
checkedRetryDelay configured =
  either (error . show) id (peerDialRetryDelayMicroseconds (max 1 configured))

caseProgressNotReady :: IO ()
caseProgressNotReady = do
  let receipts = Lifetime.receiptRetirementPrefix (Just 7)
      sparse = either (error . show) id (Lifetime.receiptRetirement (Just 7) (Set.singleton 5))
      pairs =
        [ (Progress.oracleProgress receipts (controlIndex 11), Progress.oracleProgress receipts (controlIndex 12)),
          (Progress.oracleProgress sparse (controlIndex 11), Progress.oracleProgress receipts (controlIndex 11))
        ]
  mapM_ check pairs
  where
    check (old, newer) = do
      let (first, sentFirst) = claimOracleProgress old mempty
          (second, sentSecond) = claimOracleProgress newer first
          staleReleased = releaseOracleProgress old second
          (unchanged, repeated) = claimOracleProgress newer staleReleased
          released = releaseOracleProgress newer second
          (retried, retrySent) = claimOracleProgress newer released
      assertBool "initial progress and same-high-water advancement are sent" (sentFirst && sentSecond)
      assertEqual "old same-high-water NotReady cannot reopen newer offer" second staleReleased
      assertEqual "newer offer remains coalesced" (second, False) (unchanged, repeated)
      assertEqual "matching full offer can be retried once" (second, True) (retried, retrySent)
