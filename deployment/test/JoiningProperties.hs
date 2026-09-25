{-# LANGUAGE OverloadedStrings #-}

module JoiningProperties (tests) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Eclips.Deployment.Configuration
import Eclips.Deployment.Joining.HistoryTransfer
import Eclips.Deployment.Manifest
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Startup (HeraldMember (..), appliedProcessEpochId, appliedProcessResidence)
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Herald.Join
import Eclips.Oracle.Admission
import Eclips.Oracle.Command
import Eclips.Oracle.Effect (OracleStepOutcome (OracleCommitted))
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "join history transfer retries"
    [ testCase "End between precheck and staging reply restarts with a fresh capture attempt" (caseInvalidation 1 JoinRejected),
      testCase "End while awaiting installed history restarts instead of waiting on frozen bytes" (caseInvalidation (length transfers + 1) JoinRetry),
      testCase "the complete donor union is staged before exact installation retries" caseStableRetry,
      testCase "same-attempt malformed and conflicting histories remain fatal" caseFatalReplies,
      testCase "a rejecting receiver's newer projection overrides a lagging status observer" caseLaggingObserver,
      testCase "missing receiver status after rejection waits for canonical attempt evidence" caseMissingReceiverStatus,
      testCase "an unavailable old source does not hide canonical cancellation from a survivor" caseUnavailableCancellation,
      QC.testProperty "retired transfer waits only for semantic status through unavailable observations" propRetiredStatusWait,
      testCase "control synchronization stops before installing on a serving applicant" caseControlAlreadyServing,
      testCase "activation inside a control batch ends replay despite an unconsumed suffix" caseControlActivation,
      testCase "installed control history needs no extra remote authority poll" caseControlInstalled,
      testCase "local cancellation ends control replay before installation" caseControlAlreadyCancelled,
      testCase "remote cancellation ends a control wait with unavailable local status" caseControlCancellation,
      testCase "same-admission rejected control history remains fatal" caseControlRejected,
      QC.testProperty "control retries stop at activation or cancellation without another install" propControlRetryTerminal
    ]

data Fixture = Fixture HeraldAdmissionRecord HeraldAdmissionRecord HeraldAdmissionRecord

fixture :: Fixture
fixture =
  let planes = [checked (deploymentEndpoints "127.0.0.1" port Nothing) | port <- [47000, 47010]]
      deployment = checked (checkDeployment (Bytes.replicate 32 81) (NE.fromList planes))
      genesis = deploymentCheckedOracleGenesis deployment
      initial = checked (initialOracle genesis)
      generation = oracleCurrentMembership initial
      home = case checkedOracleAppliedBootstraps genesis of [bootstrap] -> appliedProcessResidence bootstrap; _ -> error "expected one founder process"
      candidate = checked (mkHeraldEpoch (Bytes.replicate 32 91))
      manifest = heraldAdmissionManifest (checkedOracleSystemId genesis) (checked (mkHeraldId (Bytes.replicate 32 92))) candidate
      cut = checked (topologyCut (sameGenerationPredecessor (checked (mkTopologyCutId (Bytes.replicate 32 93)))) (topologyFrontier (emptyStructuralVersionVector generation) (controlIndex 0)) (checked (mkTopologyOccurrenceDigest (Bytes.replicate 32 94))))
      commit command state =
        let request = oracleClientRequestId home (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
         in case checked (stepOracle (oracleEnvelope request Nothing home command) state) of
              (successor, OracleCommitted receipt, _) | oracleReceiptResult receipt == OracleAccepted -> successor
              (_, outcome, _) -> error (show outcome)
      begun = commit (beginHeraldAdmissionCommand manifest cut) initial
      record state = case oraclePendingHeraldAdmission state of Just value -> value; Nothing -> error "missing pending admission"
      before = record begun
      parent = case checkedOracleAppliedBootstraps genesis of [bootstrap] -> appliedProcessEpochId bootstrap; _ -> error "expected one founder process"
      after = record (commit (checked (endProcessEpochCommand parent ExplicitAdministrativeEnd)) begun)
      cancelled = case oracleHeraldAdmission (admissionRecordId before) (commit (cancelHeraldAdmissionCommand (admissionRecordId before)) begun) of Just value -> value; Nothing -> error "missing cancelled admission"
   in Fixture before after cancelled

bundles :: [ByteString]
bundles = ["first source immutable history", "second source immutable history"]

transfers :: [(Maybe HeraldEpoch, JoinRequest)]
transfers =
  let Fixture before _ _ = fixture
      members = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor before))
   in [(Just member, InstallJoinHistory bundle) | member <- members, bundle <- bundles] <> [(Nothing, InstallJoinHistory bundle) | bundle <- bundles]

caseInvalidation :: Int -> JoinReply -> Assertion
caseInvalidation boundary staleReply = do
  let Fixture before after _ = fixture
  current <- newIORef before
  sent <- newIORef []
  let exchange destination request = do
        modifyIORef' sent (<> [(destination, request)])
        count <- length <$> readIORef sent
        when (count == boundary) (writeIORef current after)
        pure (if count == boundary then staleReply else JoinRetry)
  result <-
    transferJoinHistories
      (\_ -> matchingObservation <$> readIORef current)
      (\member request -> Just <$> exchange (Just member) request)
      (exchange Nothing)
      (assertFailure "a changed attempt must not pace or resend stale history")
      before
      bundles
  result @?= HistoryTransferRestart after
  admissionRecordAttempt after @?= admissionRecordAttempt before + 1
  seen <- readIORef sent
  length seen @?= boundary
  take (length transfers) seen @?= take boundary transfers

caseStableRetry :: Assertion
caseStableRetry = do
  let Fixture before _ _ = fixture
  sent <- newIORef []
  pauses <- newIORef (0 :: Int)
  let exchange destination request = do
        modifyIORef' sent (<> [(destination, request)])
        count <- length <$> readIORef sent
        pure (if count <= length transfers + 1 then JoinRetry else JoinHistoryInstalled)
  result <- transferJoinHistories (\_ -> pure (matchingObservation before)) (\member request -> Just <$> exchange (Just member) request) (exchange Nothing) (modifyIORef' pauses (+ 1)) before bundles
  result @?= HistoriesTransferred
  seen <- readIORef sent
  seen @?= transfers <> take 1 transfers <> transfers
  readIORef pauses >>= (@?= 1)

caseFatalReplies :: Assertion
caseFatalReplies = do
  let Fixture before _ _ = fixture
      run reply = transferJoinHistories (\_ -> pure (matchingObservation before)) (\_ _ -> pure (Just reply)) (\_ -> assertFailure "fatal source reply stops the transfer") (assertFailure "fatal source reply cannot retry") before bundles
  run JoinRejected >>= (@?= HistoryTransferRejected)
  run JoinRequestConflict >>= (@?= HistoryTransferConflict)

caseLaggingObserver :: Assertion
caseLaggingObserver = do
  let Fixture before after _ = fixture
  receiver <- newIORef before
  let observe _ = HistoryAdmissionObservation (Just before) . Just <$> readIORef receiver
      exchange _ _ = writeIORef receiver after >> pure (Just JoinRejected)
  result <- transferJoinHistories observe exchange (\_ -> assertFailure "recapture precedes local installation") (assertFailure "new receiver authority immediately restarts") before bundles
  result @?= HistoryTransferRestart after

caseMissingReceiverStatus :: Assertion
caseMissingReceiverStatus = do
  let Fixture before after _ = fixture
  current <- newIORef before
  sent <- newIORef (0 :: Int)
  pauses <- newIORef (0 :: Int)
  let observe _ = do
        record <- readIORef current
        pure (HistoryAdmissionObservation (Just record) Nothing)
      exchange _ _ = modifyIORef' sent (+ 1) >> pure (Just JoinRejected)
      pace = modifyIORef' pauses (+ 1) >> writeIORef current after
  result <- transferJoinHistories observe exchange (\_ -> assertFailure "no local install before source authority refresh") pace before bundles
  result @?= HistoryTransferRestart after
  readIORef sent >>= (@?= 1)
  readIORef pauses >>= (@?= 1)

caseUnavailableCancellation :: Assertion
caseUnavailableCancellation = do
  let Fixture before _ cancelled = fixture
  current <- newIORef before
  contacted <- newIORef (0 :: Int)
  let unavailable _ _ = do
        modifyIORef' contacted (+ 1)
        writeIORef current cancelled
        pure Nothing
  result <- transferJoinHistories (\_ -> (\record -> HistoryAdmissionObservation (Just record) Nothing) <$> readIORef current) unavailable (\_ -> assertFailure "cancelled applicant cannot install local history") (assertFailure "checked cancellation stops an unavailable-source wait") before bundles
  result @?= HistoryTransferCancelled
  readIORef contacted >>= (@?= 1)

propRetiredStatusWait :: QC.NonNegative Int -> Bool -> QC.Property
propRetiredStatusWait (QC.NonNegative supplied) cancel = QC.ioProperty $ do
  let Fixture before after cancelled = fixture
      missingRounds = 1 + supplied `mod` 30
      terminal = if cancel then cancelled else after
      expectedResult = if cancel then HistoryTransferCancelled else HistoryTransferRestart after
  sent <- newIORef (0 :: Int)
  paused <- newIORef (0 :: Int)
  let observe _ = do
        rounds <- readIORef paused
        pure (HistoryAdmissionObservation (Just (if rounds >= missingRounds then terminal else before)) Nothing)
      exchange _ _ = do
        modifyIORef' sent (+ 1)
        count <- readIORef sent
        when (count /= 1) (assertFailure "a retired envelope was sent again")
        pure (Just (JoinTransferRetired (admissionRecordId before) (admissionRecordAttempt before)))
  result <- transferJoinHistories observe exchange (\_ -> assertFailure "retired source must refresh authority before local installation") (modifyIORef' paused (+ 1)) before bundles
  result @?= expectedResult
  readIORef sent >>= (@?= 1)
  readIORef paused >>= (@?= missingRounds)
  pure True

controlStatus :: HeraldAdmissionRecord -> Bool -> JoinReply
controlStatus record serving =
  let manifest = admissionRecordManifest record
   in JoinStatusReply
        ( JoinStatus
            (admissionManifestSystem manifest)
            (HeraldMember (admissionManifestHeraldId manifest) (admissionManifestHeraldEpoch manifest))
            (admissionRecordPredecessor record)
            (admissionRecordChangedIndex record)
            (admissionRecordChangedIndex record)
            [record]
            Nothing
            serving
        )

caseControlAlreadyServing :: Assertion
caseControlAlreadyServing = do
  let Fixture before _ _ = fixture
      local ReadJoinStatus = pure (controlStatus before True)
      local _ = assertFailure "a serving applicant must not receive an onboarding install"
  result <- awaitJoinControlHistory (assertFailure "local service needs no remote poll") local (assertFailure "local service must not wait") (admissionRecordId before) []
  result @?= ControlHistorySynchronized

caseControlActivation :: Assertion
caseControlActivation = do
  let Fixture before _ _ = fixture
  installed <- newIORef False
  let local ReadJoinStatus = controlStatus before <$> readIORef installed
      local (InstallJoinControlHistory []) = do
        repeated <- readIORef installed
        when repeated (assertFailure "activation must prevent a second control install")
        writeIORef installed True
        -- The kernel can activate and then leave a later unknown suffix for
        -- its ordinary Oracle watch, returning JoinRetry for this batch.
        pure JoinRetry
      local _ = assertFailure "unexpected control synchronization request"
  result <- awaitJoinControlHistory (assertFailure "local activation needs no remote poll") local (assertFailure "activation must not retry the batch") (admissionRecordId before) []
  result @?= ControlHistorySynchronized

caseControlInstalled :: Assertion
caseControlInstalled = do
  let Fixture before _ _ = fixture
      local ReadJoinStatus = pure (controlStatus before False)
      local (InstallJoinControlHistory []) = pure JoinHistoryInstalled
      local _ = assertFailure "unexpected control synchronization request"
  result <- awaitJoinControlHistory (assertFailure "installed history needs no remote poll") local (assertFailure "installed history must not wait") (admissionRecordId before) []
  result @?= ControlHistorySynchronized

caseControlAlreadyCancelled :: Assertion
caseControlAlreadyCancelled = do
  let Fixture before _ cancelled = fixture
      local ReadJoinStatus = pure (controlStatus cancelled False)
      local _ = assertFailure "a cancelled applicant must not receive another install"
  result <- awaitJoinControlHistory (assertFailure "local cancellation needs no remote poll") local (assertFailure "canonical cancellation must not wait") (admissionRecordId before) []
  result @?= ControlHistoryCancelled

caseControlCancellation :: Assertion
caseControlCancellation = do
  let Fixture before _ cancelled = fixture
  installed <- newIORef False
  let local ReadJoinStatus = do
        after <- readIORef installed
        pure (if after then JoinRetry else controlStatus before False)
      local (InstallJoinControlHistory []) = writeIORef installed True >> pure JoinRejected
      local _ = assertFailure "unexpected control synchronization request"
  result <- awaitJoinControlHistory (pure (Just cancelled)) local (assertFailure "a survivor's canonical cancellation must stop the local wait") (admissionRecordId before) []
  result @?= ControlHistoryCancelled

caseControlRejected :: Assertion
caseControlRejected = do
  let Fixture before _ _ = fixture
      local ReadJoinStatus = pure (controlStatus before False)
      local (InstallJoinControlHistory []) = pure JoinRejected
      local _ = assertFailure "unexpected control synchronization request"
  result <- awaitJoinControlHistory (pure (Just before)) local (assertFailure "unchanged rejection must not retry") (admissionRecordId before) []
  result @?= ControlHistoryRejected

propControlRetryTerminal :: QC.NonNegative Int -> Bool -> QC.Property
propControlRetryTerminal (QC.NonNegative supplied) activates = QC.ioProperty $ do
  let Fixture before after cancelled = fixture
      retries = supplied `mod` 20
  installed <- newIORef (0 :: Int)
  paused <- newIORef (0 :: Int)
  let local ReadJoinStatus = do
        count <- readIORef installed
        pure (controlStatus before (activates && count > retries))
      local (InstallJoinControlHistory []) = do
        count <- readIORef installed
        when (count > retries) (assertFailure "terminal status must prevent another control install")
        modifyIORef' installed (+ 1)
        pure JoinRetry
      local _ = assertFailure "unexpected control synchronization request"
      latest = do
        count <- readIORef installed
        -- An intermediate attempt change remains the caller's decision; this
        -- helper must still synchronize control instead of treating it as done.
        pure (Just (if count > retries then cancelled else after))
  result <- awaitJoinControlHistory latest local (modifyIORef' paused (+ 1)) (admissionRecordId before) []
  result @?= if activates then ControlHistorySynchronized else ControlHistoryCancelled
  readIORef installed >>= (@?= retries + 1)
  readIORef paused >>= (@?= retries)
  pure True

matchingObservation :: HeraldAdmissionRecord -> HistoryAdmissionObservation
matchingObservation record = HistoryAdmissionObservation (Just record) (Just record)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
