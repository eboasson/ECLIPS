{-# LANGUAGE OverloadedStrings #-}

-- | Seed-only orchestration. The shell transports canonical claims; readiness,
-- history admission, and membership remain decisions of the serialized owners.
module Eclips.Deployment.Joining (withJoiningDeployment) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (find, maximumBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Deployment.Configuration
import Eclips.Deployment.Discovery
import Eclips.Deployment.Joining.HistoryTransfer
import Eclips.Deployment.Manifest
import Eclips.Deployment.Runtime
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Join
import Eclips.Herald.OracleClient qualified as Oracle
import Eclips.Oracle.Admission
import Eclips.Public.Types.ReceiptRetirement
import Eclips.Raft.Identity (raftNodeIdBytes)

data JoinSession = JoinSession ByteString DiscoveryContact (IORef DiscoveryCache) (IORef (Map HeraldEpoch (Word64, ReceiptRetirement)))

-- | Entropy and locators identify this process attempt only. No local bootstrap
-- is created when seeds are unreachable, and no Oracle voter starts here.
withJoiningDeployment :: ByteString -> DeploymentEndpoints -> NonEmpty Endpoint -> (DeploymentRuntime -> IO value) -> IO value
withJoiningDeployment entropy planes seeds use = do
  _ <- checked (mkHeraldEpoch entropy)
  identifier <- checked (mkHeraldId (SHA256.hash ("ECLIPS-JOIN-HERALD" <> entropy)))
  epoch <- checked (mkHeraldEpoch (SHA256.hash ("ECLIPS-JOIN-EPOCH" <> entropy)))
  applicant <-
    checked
      ( discoveryContact
          (HeraldMember identifier epoch)
          (NE.singleton (fromMaybe (peerListen planes) (peerAdvertised planes)))
      )
  discovered <- discoverSystem 1 [applicant] seeds
  cache <- newIORef discovered
  requestProgress <- newIORef Map.empty
  let session = JoinSession entropy applicant cache requestProgress
  snapshot <- snapshotOf discovered
  let deployment = discoveryBootstrap snapshot
  system <- checked (mkSystemId (deploymentSystemBytes deployment))
  let manifest = heraldAdmissionManifest system identifier epoch
  pending <- beginAdmission session manifest
  resident <- checked (joiningResidentDeployment deployment planes pending)
  contacts <- oracleContactHints snapshot
  withJoiningResident deployment resident contacts (Set.toAscList (discoveryCacheContacts discovered)) $ \running -> do
    completeAdmission session running pending
    use running

beginAdmission :: JoinSession -> HeraldAdmissionManifest -> IO HeraldAdmissionRecord
beginAdmission session manifest = do
  status <- activeStatus session
  case find ((== manifest) . admissionRecordManifest) (joinStatusAdmissions status) of
    Just record -> pendingRecord record
    Nothing -> case joinStatusCut status of
      Nothing -> pause >> beginAdmission session manifest
      Just cut -> do
        let coordinator = NE.head (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status))
        result <- submit session coordinator (BeginHeraldAdmission manifest cut)
        case result of
          Just record -> pendingRecord record
          Nothing -> pause >> beginAdmission session manifest
  where
    pendingRecord record = case admissionRecordPhase record of
      AdmissionCancelled {} -> fail "this Herald admission was cancelled; restart with a fresh epoch"
      AdmissionActivated {} -> fail "this process epoch was already activated"
      _ -> pure record

completeAdmission :: JoinSession -> DeploymentRuntime -> HeraldAdmissionRecord -> IO ()
completeAdmission session running initial = attempt initial
  where
    ident = admissionRecordId initial
    epoch = admissionManifestHeraldEpoch (admissionRecordManifest initial)
    attempt record = do
      let members = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record))
          coordinator = headMember record
      case admissionRecordPhase record of
        AdmissionActivated _ generation -> awaitServing coordinator generation
        AdmissionCancelled {} -> cancelled
        _ -> prepareAttempt record members coordinator
    prepareAttempt record members coordinator = do
      captures <- forM members $ \member -> capture session member ident
      case sequence captures of
        Nothing -> restart coordinator
        Just bundles -> prepareCaptured record members coordinator bundles
    prepareCaptured record members coordinator bundles = do
      histories <- traverse (checked . decodeJoinSourceHistory) bundles
      latest <- currentRecord session coordinator ident
      if admissionRecordAttempt latest /= admissionRecordAttempt record
        then attempt latest
        else case prepareJoinSeal latest histories of
          Left _ -> pause >> attempt latest
          Right seal -> do
            transferred <-
              transferJoinHistories
                (observeHistoryAdmission session running ident)
                (remoteOnce session)
                (exchangeDeploymentJoin running)
                (refresh session >> pause)
                latest
                bundles
            case transferred of
              HistoryTransferRestart next -> attempt next
              HistoryTransferCancelled -> cancelled
              HistoryTransferRejected -> fail "current admission history was rejected"
              HistoryTransferConflict -> fail "current admission history conflicts with retained source bytes"
              HistoriesTransferred -> finishAttempt coordinator members seal
    finishAttempt coordinator members seal = do
      syncControl session coordinator running ident
      sealed <- submit session coordinator (SealHeraldAdmission seal)
      case sealed of
        Nothing -> restart coordinator
        Just _ -> do
          syncControl session coordinator running ident
          accepted <- allReports members $ \member ->
            submit
              session
              member
              (AcceptHeraldJoinSeal ident (joinSealAttempt seal) (joinSealDigest seal))
          if not accepted
            then restart coordinator
            else do
              oldReady <- allReports members $ \member ->
                submit
                  session
                  member
                  (HeraldJoinBaseReady (readyReport seal member))
              if not oldReady
                then restart coordinator
                else do
                  syncControl session coordinator running ident
                  readiness <- newcomerReady session coordinator running ident (joinSealAttempt seal)
                  case readiness of
                    Nothing -> restart coordinator
                    Just report -> do
                      newcomer <- submit session coordinator (HeraldJoinReady report)
                      case newcomer of
                        Nothing -> restart coordinator
                        Just _ -> do
                          activated <- submit session coordinator (ActivateHerald ident)
                          case activated of
                            Nothing -> restart coordinator
                            Just active -> case admissionRecordPhase active of
                              AdmissionActivated _ generation -> awaitServing coordinator generation
                              AdmissionCancelled {} -> cancelled
                              _ -> restart coordinator
    restart coordinator = pause >> currentRecord session coordinator ident >>= attempt
    readyReport seal member =
      heraldJoinReadyReport
        ident
        (joinSealAttempt seal)
        member
        (joinSealDigest seal)
        (joinSealControlPrefix seal)
        (joinSealRecipeDigest seal)
    awaitServing coordinator generation = do
      syncControl session coordinator running ident
      reply <- exchangeDeploymentJoin running ReadJoinStatus
      case reply of
        JoinStatusReply status
          | joinStatusServing status,
            joinStatusMembership status == generation,
            heraldMemberEpoch (joinStatusMember status) == epoch ->
              pure ()
        _ -> pause >> awaitServing coordinator generation
    allReports members action = and <$> traverse (fmap maybeSucceeded . action) members
    maybeSucceeded = maybe False (const True)

-- A ready report is produced by the newcomer owner only after it has checked
-- its complete local replay and admission base.
newcomerReady :: JoinSession -> HeraldEpoch -> DeploymentRuntime -> HeraldAdmissionId -> Word64 -> IO (Maybe HeraldJoinReadyReport)
newcomerReady session source running admission attempt = do
  reply <- exchangeDeploymentJoin running (ReadJoinReady admission)
  case reply of
    JoinReadyReply report -> pure (if joinReadyAttempt report == attempt then Just report else Nothing)
    JoinRejected -> fail "newcomer rejected its readiness request"
    JoinTransferRetired {} -> pure Nothing
    _ -> do
      record <- currentRecord session source admission
      if admissionRecordAttempt record /= attempt
        then pure Nothing
        else do
          syncControl session source running admission
          pause
          newcomerReady session source running admission attempt

syncControl :: JoinSession -> HeraldEpoch -> DeploymentRuntime -> HeraldAdmissionId -> IO ()
syncControl session target running admission = do
  local <- exchangeDeploymentJoin running ReadJoinStatus
  case controlHistoryLocalResult admission local of
    Just result -> finish result
    Nothing -> case local of
      JoinStatusReply receiver -> do
        status <- activeStatus session
        case find ((== admission) . admissionRecordId) (joinStatusAdmissions status) of
          Just record | AdmissionCancelled {} <- admissionRecordPhase record -> cancelled
          _ -> pure ()
        let source = heraldMemberEpoch (joinStatusMember status)
        reply <- remoteOnce session source (ReadJoinControlHistory (joinStatusAppliedControl receiver))
        case reply of
          Just (JoinControlHistoryReply entries) ->
            awaitJoinControlHistory (availableAdmission session admission) (exchangeDeploymentJoin running) pause admission entries >>= finish
          _ -> retry source
      _ -> retry target
  where
    finish ControlHistorySynchronized = pure ()
    finish ControlHistoryCancelled = cancelled
    finish ControlHistoryRejected = fail "newcomer rejected onboarding history"
    retry source = do
      record <- availableAdmission session admission
      case record of
        Just observed | AdmissionCancelled {} <- admissionRecordPhase observed -> cancelled
        _ -> pause >> syncControl session source running admission

capture :: JoinSession -> HeraldEpoch -> HeraldAdmissionId -> IO (Maybe ByteString)
capture session source admission = do
  reply <- remote session source (CaptureJoinHistory admission)
  case reply of
    JoinHistoryReply bytes -> pure (Just bytes)
    JoinRejected -> fail "source rejected the current admission history request"
    JoinTransferRetired {} -> pure Nothing
    _ -> pause >> capture session source admission

-- Each applicant epoch owns a monotonic request namespace per destination.
-- Exact transport retries keep their identity. A consumed terminal reply is
-- released before this sequential workflow issues another command.
submit :: JoinSession -> HeraldEpoch -> HeraldAdmissionCommand -> IO (Maybe HeraldAdmissionRecord)
submit session@(JoinSession _ applicant _ progressRef) source command = do
  (ordinal, ready) <- atomicModifyIORef' progressRef $ \progress ->
    let current@(next, released) = Map.findWithDefault (0, mempty) source progress
     in (Map.insert source (next + 1, released) progress, current)
  let owner = heraldMemberEpoch (discoveryContactMember applicant)
      token = joinRequestId owner ordinal
      request = SubmitJoinCommandWithRetirement token command ready
      finish result = do
        let released = ready <> receiptRetirementPrefix (Just ordinal)
        modifyIORef' progressRef (Map.adjust (\(next, _) -> (next, released)) source)
        -- A destination may disappear after delivering the terminal result.
        -- Keep ready progress for the next piggyback, but do not turn cleanup
        -- into a new dependency of already completed semantic work.
        acknowledged <- remoteOnce session source (RetireJoinReceipts owner released)
        case acknowledged of
          Just (JoinReceiptsRetired actual accepted)
            | actual == owner, released <> accepted == accepted -> pure result
          Nothing -> pure result
          _ -> fail "join receipt retirement was not acknowledged"
      submitted reply = case reply of
        JoinCommandPending -> pause >> remote session source (ReadJoinCommandWithRetirement token ready) >>= submitted
        JoinCommandComplete records -> finish (case records of record : _ -> Just record; [] -> Nothing)
        JoinRetry -> do
          stillCurrent <- commandStillCurrent session source command
          if stillCurrent then pause >> remote session source request >>= submitted else finish Nothing
        JoinRejected -> finish Nothing
        JoinRequestRetired actual _ | actual == token -> pure Nothing
        JoinRetirementNotReady -> fail "join receipt retirement covered unresolved work"
        JoinRequestConflict -> fail "join request identity was reused with different content"
        _ -> fail "unexpected admission command response"
  remote session source request >>= submitted

commandStillCurrent :: JoinSession -> HeraldEpoch -> HeraldAdmissionCommand -> IO Bool
commandStillCurrent session source command = do
  reply <- remote session source ReadJoinStatus
  pure $ case reply of
    JoinStatusReply status -> case command of
      BeginHeraldAdmission manifest cut ->
        joinStatusCut status == Just cut
          && not (any ((== manifest) . admissionRecordManifest) (joinStatusAdmissions status))
      SealHeraldAdmission seal -> current status (joinSealAdmissionId seal) (Just (joinSealAttempt seal))
      AcceptHeraldJoinSeal ident attempt _ -> current status ident (Just attempt)
      HeraldJoinBaseReady report -> current status (joinReadyAdmissionId report) (Just (joinReadyAttempt report))
      HeraldJoinReady report -> current status (joinReadyAdmissionId report) (Just (joinReadyAttempt report))
      ActivateHerald ident -> current status ident Nothing
      CancelHeraldAdmission ident -> current status ident Nothing
    _ -> True
  where
    current status ident attempt = case find ((== ident) . admissionRecordId) (joinStatusAdmissions status) of
      Nothing -> True
      Just record ->
        maybe True (== admissionRecordAttempt record) attempt && case admissionRecordPhase record of
          AdmissionActivated {} -> False
          AdmissionCancelled {} -> False
          _ -> True

currentRecord :: JoinSession -> HeraldEpoch -> HeraldAdmissionId -> IO HeraldAdmissionRecord
currentRecord session source admission = do
  observed <- availableAdmission session admission
  case observed of
    Just record -> case admissionRecordPhase record of
      AdmissionCancelled {} -> cancelled
      _ -> pure record
    Nothing -> refresh session >> pause >> currentRecord session source admission

availableAdmission :: JoinSession -> HeraldAdmissionId -> IO (Maybe HeraldAdmissionRecord)
availableAdmission session admission = do
  records <- availableAdmissions session
  pure (newest [record | record <- records, admissionRecordId record == admission])

observeHistoryAdmission :: JoinSession -> DeploymentRuntime -> HeraldAdmissionId -> Maybe HeraldEpoch -> IO HistoryAdmissionObservation
observeHistoryAdmission session running admission receiver = do
  remoteRecords <- availableAdmissions session
  localReply <- exchangeDeploymentJoin running ReadJoinStatus
  receiverReply <- case receiver of
    Nothing -> pure (Just localReply)
    Just member -> remoteOnce session member ReadJoinStatus
  let recordsFrom (Just (JoinStatusReply status)) = [record | record <- joinStatusAdmissions status, admissionRecordId record == admission]
      recordsFrom _ = []
      available = newest (filter ((== admission) . admissionRecordId) remoteRecords <> recordsFrom (Just localReply))
  pure (HistoryAdmissionObservation available (newest (recordsFrom receiverReply)))

-- Read any available active owner, since cancellation can retire the original
-- reporter/coordinator. A missing route never substitutes for Oracle evidence.
availableAdmissions :: JoinSession -> IO [HeraldAdmissionRecord]
availableAdmissions (JoinSession _ _ cacheRef _) = do
  cache <- readIORef cacheRef
  replies <- traverse (`exchangeAt` ReadJoinStatus) (Set.toAscList (discoveryCacheLocators cache))
  pure [record | Just (JoinStatusReply status) <- replies, joinStatusServing status, record <- joinStatusAdmissions status]

newest :: [HeraldAdmissionRecord] -> Maybe HeraldAdmissionRecord
newest [] = Nothing
newest records = Just (maximumBy (comparing admissionRecordChangedIndex) records)

activeStatus :: JoinSession -> IO JoinStatus
activeStatus session@(JoinSession _ _ cacheRef _) = do
  cache <- readIORef cacheRef
  let seek [] = refresh session >> pause >> activeStatus session
      seek (locator : rest) =
        exchangeAt locator ReadJoinStatus >>= \case
          Just (JoinStatusReply status) | joinStatusServing status -> pure status
          _ -> seek rest
  seek (Set.toAscList (discoveryCacheLocators cache))

-- Contacts supply routing only. Check the answering identity before sending a
-- source-specific command so stale location hints do not choose a reporter.
remote :: JoinSession -> HeraldEpoch -> JoinRequest -> IO JoinReply
remote session@(JoinSession _ applicant _ _) target request = do
  reply <- remoteOnce session target request
  case reply of
    Just value -> pure value
    Nothing -> do
      refresh session
      records <- availableAdmissions session
      case newest [record | record <- records, admissionManifestHeraldEpoch (admissionRecordManifest record) == heraldMemberEpoch (discoveryContactMember applicant)] of
        Just record | AdmissionCancelled {} <- admissionRecordPhase record -> cancelled
        _ -> pause >> remote session target request

remoteOnce :: JoinSession -> HeraldEpoch -> JoinRequest -> IO (Maybe JoinReply)
remoteOnce (JoinSession _ _ cacheRef _) target request = do
  cache <- readIORef cacheRef
  let routes = Set.unions [discoveryContactLocators contact | contact <- Set.toList (discoveryCacheContacts cache), heraldMemberEpoch (discoveryContactMember contact) == target]
      tryRoutes [] = pure Nothing
      tryRoutes (locator : rest) =
        exchangeAt locator ReadJoinStatus >>= \case
          Just (JoinStatusReply status)
            | heraldMemberEpoch (joinStatusMember status) == target ->
                exchangeAt locator request >>= maybe (tryRoutes rest) (pure . Just)
          _ -> tryRoutes rest
  tryRoutes (Set.toAscList routes)

exchangeAt :: Endpoint -> JoinRequest -> IO (Maybe JoinReply)
exchangeAt locator request = do
  result <- exchangeDiscoveryRequest locator (Onboard 2 (encodeJoinRequest request))
  pure $ case result of
    Right (OnboardReply 2 bytes) -> either (const Nothing) Just (decodeJoinReply bytes)
    _ -> Nothing

refresh :: JoinSession -> IO ()
refresh (JoinSession _ contact cacheRef _) = do
  cache <- readIORef cacheRef
  refreshed <- refreshDiscovery 1 [contact] cache
  writeIORef cacheRef refreshed

headMember :: HeraldAdmissionRecord -> HeraldEpoch
headMember = NE.head . heraldMembershipGenerationActiveHeraldEpochs . admissionRecordPredecessor

snapshotOf :: DiscoveryCache -> IO DiscoverySnapshot
snapshotOf = maybe (fail "discovery returned no bootstrap") pure . discoveryCacheSnapshot

oracleContactHints :: DiscoverySnapshot -> IO Oracle.OracleContactSet
oracleContactHints snapshot = do
  hints <- traverse contact (Set.toAscList (discoveryOracleContacts snapshot))
  nonempty <- maybe (fail "discovery returned no Oracle contacts") pure (NE.nonEmpty hints)
  checked (Oracle.oracleContactSet nonempty)
  where
    contact hint = do
      node <- checked (Oracle.oracleNodeClaim (raftNodeIdBytes (discoveryOracleNode hint)))
      let locator = discoveryOracleLocator hint
      checked (Oracle.oracleContact node (endpointHost locator) (endpointPort locator))

pause :: IO ()
pause = threadDelay 100_000
cancelled :: IO value
cancelled = fail "Herald admission was cancelled before activation"
checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
