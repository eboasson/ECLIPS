{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private failure-probe and membership substate of the live Oracle owner.
--
-- This module deliberately owns no request ledger and no control-index cursor.
-- The enclosing Oracle state machine supplies the index of the one command it
-- is applying, so failure decisions and label/process decisions inhabit one
-- canonical Raft-applied history.
module Eclips.Oracle.Internal.Failure
  ( ProbeResult (..),
    FailureProbeTerminalView (..),
    AcceptedVoterHostFailure,
    acceptedVoterHostFailureResolution,
    acceptedVoterHostFailureTarget,
    acceptedVoterHostFailureMembership,
    acceptedVoterHostFailureConfiguration,
    acceptedVoterHostFailureReports,
    acceptedVoterHostFailureControlIndex,
    acceptedVoterHostFailureChangeId,
    encodeAcceptedVoterHostFailure,
    decodeAcceptedVoterHostFailure,
    acceptVoterHostFailure,
    FailureProbeView (..),
    FailureResolutionReady (..),
    FailureCommandResult (..),
    FailureRejection (..),
    FailureEvent (..),
    FailureState,
    initialFailureState,
    installAdmissionMembership,
    failureHasPendingRetirement,
    supersedeFailureProbesForVoters,
    failureCurrentMembership,
    failureMembershipHistory,
    failureCheckedMembershipHistory,
    failureRetiredHeralds,
    failureActiveHeralds,
    failureVoterHosts,
    failureProbe,
    failureActiveProbeFor,
    failureStateIsGenesisIdle,
    openFailureProbe,
    reportFailureProbe,
    dismissFailureProbe,
    retireHeraldEpoch,
    failureStateCanonicalBytes,
    encodeFailureCheckpoint,
    decodeFailureCheckpoint,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as S
import Data.Serialize.Put qualified as SerializePut
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word32, Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    mkHeraldEpoch,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (..),
    FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    appendHeraldMembershipGeneration,
    decodeFailureProbeResolutionIdCanonicalBytes,
    decodeHeraldMembershipHistoryCanonicalBytes,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    failureProbeResolutionDisposition,
    failureProbeResolutionIdCanonicalBytes,
    failureProbeResolutionProbeId,
    genesisHeraldMembershipGeneration,
    heraldFailureProbeControlIndex,
    heraldFailureProbeIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementId,
    heraldMembershipHistory,
    heraldMembershipHistoryCanonicalBytes,
    heraldMembershipHistoryCurrent,
    heraldMembershipHistoryGenerations,
    mkHeraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch))
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleActiveHeralds,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    raftVoterBindingHeraldEpoch,
  )

import Eclips.Oracle.Internal.Voter qualified as V
import GHC.Generics (Generic)

data ProbeResult
  = ProbeReachable
  | ProbeUnreachable
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- Opaque retained proof: admission checks a stable captured denominator, exact
-- ordered reporter facts and a positive acceptance after the authorizing Open.
data AcceptedVoterHostFailure = AcceptedVoterHostFailure FailureProbeResolutionId HeraldEpoch HeraldMembershipGenerationId V.VoterConfiguration [(HeraldEpoch, ProbeResult)] ControlIndex
  deriving stock (Eq, Show)
acceptedVoterHostFailureResolution :: AcceptedVoterHostFailure -> FailureProbeResolutionId
acceptedVoterHostFailureResolution (AcceptedVoterHostFailure value _ _ _ _ _) = value
acceptedVoterHostFailureTarget :: AcceptedVoterHostFailure -> HeraldEpoch
acceptedVoterHostFailureTarget (AcceptedVoterHostFailure _ value _ _ _ _) = value
acceptedVoterHostFailureMembership :: AcceptedVoterHostFailure -> HeraldMembershipGenerationId
acceptedVoterHostFailureMembership (AcceptedVoterHostFailure _ _ value _ _ _) = value
acceptedVoterHostFailureConfiguration :: AcceptedVoterHostFailure -> V.VoterConfiguration
acceptedVoterHostFailureConfiguration (AcceptedVoterHostFailure _ _ _ value _ _) = value
acceptedVoterHostFailureReports :: AcceptedVoterHostFailure -> [(HeraldEpoch, ProbeResult)]
acceptedVoterHostFailureReports (AcceptedVoterHostFailure _ _ _ _ value _) = value
acceptedVoterHostFailureControlIndex :: AcceptedVoterHostFailure -> ControlIndex
acceptedVoterHostFailureControlIndex (AcceptedVoterHostFailure _ _ _ _ _ value) = value
acceptedVoterHostFailureChangeId :: AcceptedVoterHostFailure -> V.VoterChangeId
acceptedVoterHostFailureChangeId = either (error . show) id . V.mkVoterChangeId . acceptedVoterHostFailureControlIndex
encodeAcceptedVoterHostFailure :: AcceptedVoterHostFailure -> ByteString
encodeAcceptedVoterHostFailure (AcceptedVoterHostFailure resolution target generation configuration reports index) =
  S.encode
    ( failureProbeResolutionIdCanonicalBytes resolution,
      heraldEpochBytes target,
      heraldMembershipGenerationIdBytes generation,
      V.encodeVoterConfiguration configuration,
      fmap (\(host, result) -> (heraldEpochBytes host, (fromIntegral (fromEnum result) :: Word8))) reports,
      controlIndexWord64 index
    )
decodeAcceptedVoterHostFailure :: ByteString -> Either String AcceptedVoterHostFailure
decodeAcceptedVoterHostFailure bytes = do
  (rawResolution, rawTarget, rawGeneration, rawConfiguration, rawReports, rawIndex) <- S.decode bytes
  resolution <- admit (decodeFailureProbeResolutionIdCanonicalBytes rawResolution)
  target <- admit (mkHeraldEpoch rawTarget)
  generation <- admit (mkHeraldMembershipGenerationId rawGeneration)
  configuration <- admit (V.decodeVoterConfiguration rawConfiguration)
  case V.voterConfigurationView configuration of
    V.StableVoterConfigurationView _ -> pure ()
    V.JointVoterConfigurationView {} -> Left "accepted failure requires stable configuration"
  reports <- mapM (\(host, result) -> (,) <$> admit (mkHeraldEpoch host) <*> case (result :: Word8) of 0 -> Right ProbeReachable; 1 -> Right ProbeUnreachable; _ -> Left "invalid accepted report") rawReports
  let eligible = configurationHosts configuration
      index = controlIndex rawIndex
      probeIndex = heraldFailureProbeControlIndex (failureProbeResolutionProbeId resolution)
      certificate = AcceptedVoterHostFailure resolution target generation configuration reports index
  unless
    ( failureProbeResolutionDisposition resolution == RetireFailureProbeTarget
        && Set.member target eligible
        && reports == Map.toAscList (Map.fromList reports)
        && all ((`Set.member` eligible) . fst) reports
        && 2 * length (filter ((== ProbeUnreachable) . snd) reports) > Set.size eligible
        && Set.size eligible > 1
        && V.voterConfigurationControlIndex configuration < probeIndex
        && probeIndex < index
        && encodeAcceptedVoterHostFailure certificate == bytes
    )
    (Left "invalid accepted voter-host certificate")
  pure certificate
  where
    admit :: (Show problem) => Either problem value -> Either String value
    admit = either (Left . show) Right

data FailureProbeTerminal
  = ProbeVoterHostFailureAccepted AcceptedVoterHostFailure
  | ProbeDismissed FailureProbeResolutionId ControlIndex
  | ProbeRetired
      FailureProbeResolutionId
      HeraldMembershipGenerationId
      ControlIndex
  | ProbeMembershipSuperseded
      HeraldMembershipGenerationId
      ControlIndex
  deriving stock (Eq, Show)

data FailureProbeTerminalView
  = ProbeVoterHostFailureAcceptedView AcceptedVoterHostFailure
  | ProbeDismissedView FailureProbeResolutionId ControlIndex
  | ProbeRetiredView
      FailureProbeResolutionId
      HeraldMembershipGenerationId
      ControlIndex
  | ProbeMembershipSupersededView
      HeraldMembershipGenerationId
      ControlIndex
  deriving stock (Eq, Show)

data FailureProbe
  = FailureProbe
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfiguration
      ControlIndex
      (Map HeraldEpoch ProbeResult)
      (Maybe FailureProbeTerminal)
  deriving stock (Eq, Show)

data FailureProbeView
  = FailureProbeView
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfiguration
      ControlIndex
      [(HeraldEpoch, ProbeResult)]
      (Maybe FailureProbeTerminalView)
  deriving stock (Eq, Show)

data FailureResolutionReady
  = FailureResolutionReady
      FailureProbeResolutionId
      FailureProbeResolution
      HeraldEpoch
  deriving stock (Eq, Show)

data FailureCommandResult
  = FailureProbeOpened HeraldFailureProbeId
  | FailureProbeReportRecorded
      HeraldFailureProbeId
      (Maybe FailureResolutionReady)
  | FailureProbeDismissedResult FailureProbeResolutionId
  | VoterHostFailureAcceptedResult AcceptedVoterHostFailure
  | HeraldRetiredResult
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data FailureRejection
  = StaleMembershipGeneration
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | FailureProbeTargetNotActive HeraldEpoch
  | FailureProbeTargetHostsVoter HeraldEpoch
  | UnknownFailureProbe HeraldFailureProbeId
  | FailureProbeTerminal
      HeraldFailureProbeId
      FailureProbeTerminalView
  | IneligibleFailureReporter HeraldEpoch
  | ConflictingFailureReport
      HeraldFailureProbeId
      HeraldEpoch
      ProbeResult
      ProbeResult
  | FailureProbeThresholdNotReached
      HeraldFailureProbeId
      FailureProbeResolution
  | RetirementTargetMismatch HeraldEpoch HeraldEpoch
  | StaleFailureVoterConfiguration V.VoterConfigurationId V.VoterConfigurationId
  | FailureProbeTargetNotVoter HeraldEpoch
  | FailureVoterExclusionNotApplied HeraldEpoch
  deriving stock (Eq, Show)

data FailureEvent
  = FailureProbeOpenedEvent
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfiguration
  | VoterHostFailureAcceptedEvent AcceptedVoterHostFailure
  | FailureProbeReportRecordedEvent
      HeraldFailureProbeId
      HeraldEpoch
      ProbeResult
  | FailureProbeDismissedEvent
      HeraldFailureProbeId
      FailureProbeResolutionId
  | HeraldMembershipAdvancedEvent HeraldMembershipGeneration
  | FailureProbeRetiredEvent
      HeraldFailureProbeId
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | FailureProbeSupersededEvent
      HeraldFailureProbeId
      HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data FailureState
  = FailureState
      (Set HeraldEpoch)
      HeraldMembershipHistory
      (Map HeraldFailureProbeId FailureProbe)
  deriving stock (Eq, Show)

initialFailureState :: CheckedOracleGenesis -> FailureState
initialFailureState genesis =
  FailureState
    voters
    (either (error . ("checked Oracle genesis history invariant: " <>) . show) id (heraldMembershipHistory (NonEmpty.singleton generation)))
    Map.empty
  where
    members = fmap heraldMemberEpoch (checkedOracleActiveHeralds genesis)
    generation = case NonEmpty.nonEmpty members of
      Just active ->
        either
          (error . ("checked Oracle genesis membership invariant: " <>) . show)
          id
          (genesisHeraldMembershipGeneration (checkedOracleSystemId genesis) active)
      Nothing -> error "checked Oracle genesis unexpectedly has no active Herald"
    voters =
      Set.fromList
        (fmap raftVoterBindingHeraldEpoch (checkedOracleRaftVoterBindings genesis))

failureCurrentMembership :: FailureState -> HeraldMembershipGeneration
failureCurrentMembership = heraldMembershipHistoryCurrent . stateMembershipHistory

failureCheckedMembershipHistory :: FailureState -> HeraldMembershipHistory
failureCheckedMembershipHistory = stateMembershipHistory

failureMembershipHistory :: FailureState -> [HeraldMembershipGeneration]
failureMembershipHistory = NonEmpty.toList . heraldMembershipHistoryGenerations . stateMembershipHistory

failureRetiredHeralds :: FailureState -> [HeraldEpoch]
failureRetiredHeralds state =
  [ retired
  | generation <- failureMembershipHistory state,
    Just retired <- [heraldMembershipGenerationRetiredHeraldEpoch generation]
  ]

failureActiveHeralds :: FailureState -> Set HeraldEpoch
failureActiveHeralds =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . failureCurrentMembership

failureVoterHosts :: FailureState -> Set HeraldEpoch
failureVoterHosts (FailureState voters _ _) = voters

failureProbe :: HeraldFailureProbeId -> FailureState -> Maybe FailureProbeView
failureProbe identifier state =
  probeView <$> Map.lookup identifier (stateProbes state)

failureActiveProbeFor ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  FailureState ->
  Maybe HeraldFailureProbeId
failureActiveProbeFor target generation state =
  probeIdentifier <$> find matches (Map.elems (stateProbes state))
  where
    matches probe =
      probeTarget probe == target
        && probeGeneration probe == generation
        && probeTerminal probe == Nothing

failureStateIsGenesisIdle :: FailureState -> Bool
failureStateIsGenesisIdle state =
  length (failureMembershipHistory state) == 1
    && heraldMembershipGenerationPredecessor (failureCurrentMembership state) == Nothing
    && Map.null (stateProbes state)

openFailureProbe :: ControlIndex -> HeraldEpoch -> HeraldMembershipGenerationId -> V.VoterConfiguration -> FailureState -> Either FailureRejection (FailureState, FailureCommandResult, [FailureEvent])
openFailureProbe nextIndex target suppliedGeneration configuration state = do
  let currentGenerationId = heraldMembershipGenerationId (failureCurrentMembership state)
  unless (suppliedGeneration == currentGenerationId) (Left (StaleMembershipGeneration suppliedGeneration currentGenerationId))
  unless (Set.member target (failureActiveHeralds state)) (Left (FailureProbeTargetNotActive target))
  case failureActiveProbeFor target suppliedGeneration state of
    Just existing -> Right (state, FailureProbeOpened existing, [])
    Nothing ->
      let identifier = either (error . show) id (deriveHeraldFailureProbeId nextIndex)
          record = FailureProbe identifier target suppliedGeneration configuration nextIndex Map.empty Nothing
       in Right
            ( setStateProbes (Map.insert identifier record (stateProbes state)) state,
              FailureProbeOpened identifier,
              [FailureProbeOpenedEvent identifier target suppliedGeneration configuration]
            )

reportFailureProbe :: HeraldFailureProbeId -> V.VoterConfigurationId -> HeraldEpoch -> ProbeResult -> FailureState -> Either FailureRejection (FailureState, FailureCommandResult, [FailureEvent])
reportFailureProbe identifier configuration reporter suppliedResult state = do
  record <- lookupProbe identifier state
  unless (configuration == V.voterConfigurationId (probeConfiguration record)) (Left (StaleFailureVoterConfiguration configuration (V.voterConfigurationId (probeConfiguration record))))
  case probeTerminal record of
    Just terminal -> Left (FailureProbeTerminal identifier (terminalView terminal))
    Nothing -> Right ()
  requireEligibleReporter reporter record
  case Map.lookup reporter (probeReports record) of
    Just retained
      | retained == suppliedResult -> Right (state, FailureProbeReportRecorded identifier (terminalIntention record), [])
      | otherwise -> Left (ConflictingFailureReport identifier reporter retained suppliedResult)
    Nothing ->
      let updated = setProbeReports (Map.insert reporter suppliedResult (probeReports record)) record
          successor = setStateProbes (Map.insert identifier updated (stateProbes state)) state
       in Right (successor, FailureProbeReportRecorded identifier (terminalIntention updated), [FailureProbeReportRecordedEvent identifier reporter suppliedResult])

dismissFailureProbe :: ControlIndex -> HeraldEpoch -> FailureProbeResolutionId -> FailureState -> Either FailureRejection (FailureState, FailureCommandResult, [FailureEvent])
dismissFailureProbe nextIndex reporter resolution state = do
  let identifier = failureProbeResolutionProbeId resolution
  record <- lookupProbe identifier state
  requireEligibleReporter reporter record
  case probeTerminal record of
    Just (ProbeDismissed retained _) | retained == resolution -> Right (state, FailureProbeDismissedResult retained, [])
    Just terminal -> Left (FailureProbeTerminal identifier (terminalView terminal))
    Nothing -> do
      requireThreshold resolution DismissFailureProbe record
      let updated = setProbeTerminal (Just (ProbeDismissed resolution nextIndex)) record
      Right (setStateProbes (Map.insert identifier updated (stateProbes state)) state, FailureProbeDismissedResult resolution, [FailureProbeDismissedEvent identifier resolution])

acceptVoterHostFailure :: ControlIndex -> HeraldEpoch -> FailureProbeResolutionId -> V.VoterConfiguration -> FailureState -> Either FailureRejection (FailureState, FailureCommandResult, [FailureEvent])
acceptVoterHostFailure nextIndex reporter resolution configuration state = do
  let identifier = failureProbeResolutionProbeId resolution
  record <- lookupProbe identifier state
  requireEligibleReporter reporter record
  case probeTerminal record of
    Just (ProbeVoterHostFailureAccepted certificate)
      | acceptedVoterHostFailureResolution certificate == resolution -> Right (state, VoterHostFailureAcceptedResult certificate, [])
    Just terminal -> Left (FailureProbeTerminal identifier (terminalView terminal))
    Nothing -> do
      let captured = probeConfiguration record
          target = probeTarget record
          currentId = heraldMembershipGenerationId (failureCurrentMembership state)
      unless (Set.member target (configurationHosts captured)) (Left (FailureProbeTargetNotVoter target))
      unless (V.voterConfigurationId configuration == V.voterConfigurationId captured) (Left (StaleFailureVoterConfiguration (V.voterConfigurationId captured) (V.voterConfigurationId configuration)))
      unless (probeGeneration record == currentId) (Left (StaleMembershipGeneration (probeGeneration record) currentId))
      requireThreshold resolution RetireFailureProbeTarget record
      let certificate = AcceptedVoterHostFailure resolution target (probeGeneration record) captured (Map.toAscList (probeReports record)) nextIndex
          updated = setProbeTerminal (Just (ProbeVoterHostFailureAccepted certificate)) record
      Right (setStateProbes (Map.insert identifier updated (stateProbes state)) state, VoterHostFailureAcceptedResult certificate, [VoterHostFailureAcceptedEvent certificate])

-- The enclosing voter owner supplies exclusionApplied only for this retained
-- certificate's final applied exclusion. It never supplies an observed timeout.
retireHeraldEpoch :: ControlIndex -> HeraldEpoch -> FailureProbeResolutionId -> HeraldEpoch -> Bool -> FailureState -> Either FailureRejection (FailureState, FailureCommandResult, [FailureEvent])
retireHeraldEpoch nextIndex reporter resolution suppliedTarget exclusionApplied state = do
  let identifier = failureProbeResolutionProbeId resolution
  record <- lookupProbe identifier state
  requireEligibleReporter reporter record
  unless (probeTarget record == suppliedTarget) (Left (RetirementTargetMismatch suppliedTarget (probeTarget record)))
  case find ((== Just resolution) . heraldMembershipGenerationRetirementId) (failureMembershipHistory state) of
    Just retained -> Right (state, HeraldRetiredResult resolution (heraldMembershipGenerationId retained), [])
    Nothing -> do
      case probeTerminal record of
        Just (ProbeVoterHostFailureAccepted certificate)
          | acceptedVoterHostFailureResolution certificate == resolution ->
              unless exclusionApplied (Left (FailureVoterExclusionNotApplied suppliedTarget))
        Just terminal -> Left (FailureProbeTerminal identifier (terminalView terminal))
        Nothing -> do
          unless (not (Set.member suppliedTarget (configurationHosts (probeConfiguration record)))) (Left (FailureProbeTargetHostsVoter suppliedTarget))
          requireThreshold resolution RetireFailureProbeTarget record
      let current = failureCurrentMembership state
          currentId = heraldMembershipGenerationId current
      unless (probeGeneration record == currentId) (Left (StaleMembershipGeneration (probeGeneration record) currentId))
      let successorGeneration = either (error . ("admitted membership retirement invariant: " <>) . show) id (retireHeraldMembershipGeneration nextIndex resolution suppliedTarget current)
          successorId = heraldMembershipGenerationId successorGeneration
          (updatedProbes, terminalEvents) = terminalizeProbes nextIndex identifier resolution (probeGeneration record) successorId (stateProbes state)
          successor = setStateProbes updatedProbes . setStateMembership successorGeneration $ state
      Right (successor, HeraldRetiredResult resolution successorId, HeraldMembershipAdvancedEvent successorGeneration : terminalEvents)

terminalizeProbes ::
  ControlIndex ->
  HeraldFailureProbeId ->
  FailureProbeResolutionId ->
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  Map HeraldFailureProbeId FailureProbe ->
  (Map HeraldFailureProbeId FailureProbe, [FailureEvent])
terminalizeProbes index authorizer resolution predecessor successor =
  \probes -> Map.foldrWithKey terminalize (probes, []) probes
  where
    terminalize identifier record (updated, events)
      | identifier == authorizer,
        Just (ProbeVoterHostFailureAccepted _) <- probeTerminal record =
          (updated, FailureProbeRetiredEvent identifier resolution successor : events)
      | probeTerminal record /= Nothing = (updated, events)
      | probeGeneration record /= predecessor = (updated, events)
      | identifier == authorizer =
          ( Map.insert
              identifier
              (setProbeTerminal (Just (ProbeRetired resolution successor index)) record)
              updated,
            FailureProbeRetiredEvent identifier resolution successor : events
          )
      | otherwise =
          ( Map.insert
              identifier
              (setProbeTerminal (Just (ProbeMembershipSuperseded successor index)) record)
              updated,
            FailureProbeSupersededEvent identifier successor : events
          )

lookupProbe ::
  HeraldFailureProbeId ->
  FailureState ->
  Either FailureRejection FailureProbe
lookupProbe identifier state =
  maybe
    (Left (UnknownFailureProbe identifier))
    Right
    (Map.lookup identifier (stateProbes state))

requireEligibleReporter :: HeraldEpoch -> FailureProbe -> Either FailureRejection ()
requireEligibleReporter reporter record
  | Set.member reporter (configurationHosts (probeConfiguration record)) = Right ()
  | otherwise = Left (IneligibleFailureReporter reporter)

requireThreshold :: FailureProbeResolutionId -> FailureProbeResolution -> FailureProbe -> Either FailureRejection ()
requireThreshold resolution disposition record
  | failureProbeResolutionDisposition resolution == disposition && hasStrictMajority disposition record = Right ()
  | otherwise = Left (FailureProbeThresholdNotReached (probeIdentifier record) disposition)

terminalIntention :: FailureProbe -> Maybe FailureResolutionReady
terminalIntention record
  | probeTerminal record /= Nothing = Nothing
  | hasStrictMajority DismissFailureProbe record = Just (mkResolutionReady record DismissFailureProbe)
  | hasStrictMajority RetireFailureProbeTarget record = Just (mkResolutionReady record RetireFailureProbeTarget)
  | otherwise = Nothing
mkResolutionReady :: FailureProbe -> FailureProbeResolution -> FailureResolutionReady
mkResolutionReady record disposition = FailureResolutionReady (deriveFailureProbeResolutionId (probeIdentifier record) disposition) disposition (probeTarget record)
hasStrictMajority :: FailureProbeResolution -> FailureProbe -> Bool
hasStrictMajority disposition record = 2 * matching > Set.size eligible
  where
    eligible = configurationHosts (probeConfiguration record)
    expected = case disposition of DismissFailureProbe -> ProbeReachable; RetireFailureProbeTarget -> ProbeUnreachable
    matching = length [() | (reporter, result) <- Map.toList (probeReports record), Set.member reporter eligible, result == expected]
configurationHosts :: V.VoterConfiguration -> Set HeraldEpoch
configurationHosts = Set.fromList . fmap raftVoterBindingHeraldEpoch . V.voterConfigurationBindings

probeIdentifier :: FailureProbe -> HeraldFailureProbeId
probeIdentifier (FailureProbe identifier _ _ _ _ _ _) = identifier

probeTarget :: FailureProbe -> HeraldEpoch
probeTarget (FailureProbe _ target _ _ _ _ _) = target

probeGeneration :: FailureProbe -> HeraldMembershipGenerationId
probeGeneration (FailureProbe _ _ generation _ _ _ _) = generation

probeConfiguration :: FailureProbe -> V.VoterConfiguration
probeConfiguration (FailureProbe _ _ _ configuration _ _ _) = configuration

probeReports :: FailureProbe -> Map HeraldEpoch ProbeResult
probeReports (FailureProbe _ _ _ _ _ reports _) = reports

probeTerminal :: FailureProbe -> Maybe FailureProbeTerminal
probeTerminal (FailureProbe _ _ _ _ _ _ terminal) = terminal

setProbeReports :: Map HeraldEpoch ProbeResult -> FailureProbe -> FailureProbe
setProbeReports reports (FailureProbe identifier target generation configuration opened _ terminal) =
  FailureProbe identifier target generation configuration opened reports terminal

setProbeTerminal :: Maybe FailureProbeTerminal -> FailureProbe -> FailureProbe
setProbeTerminal terminal (FailureProbe identifier target generation configuration opened reports _) =
  FailureProbe identifier target generation configuration opened reports terminal

probeView :: FailureProbe -> FailureProbeView
probeView (FailureProbe identifier target generation configuration opened reports terminal) =
  FailureProbeView
    identifier
    target
    generation
    configuration
    opened
    (Map.toAscList reports)
    (terminalView <$> terminal)

terminalView :: FailureProbeTerminal -> FailureProbeTerminalView
terminalView terminal = case terminal of
  ProbeVoterHostFailureAccepted certificate -> ProbeVoterHostFailureAcceptedView certificate
  ProbeDismissed resolution index -> ProbeDismissedView resolution index
  ProbeRetired resolution successor index ->
    ProbeRetiredView resolution successor index
  ProbeMembershipSuperseded successor index ->
    ProbeMembershipSupersededView successor index

stateMembershipHistory ::
  FailureState ->
  HeraldMembershipHistory
stateMembershipHistory (FailureState _ history _) = history

stateProbes :: FailureState -> Map HeraldFailureProbeId FailureProbe
stateProbes (FailureState _ _ probes) = probes

setStateMembership :: HeraldMembershipGeneration -> FailureState -> FailureState
setStateMembership generation (FailureState voters history probes) =
  FailureState
    voters
    (either (error . ("admitted Oracle membership history invariant: " <>) . show) id (appendHeraldMembershipGeneration generation history))
    probes

setStateProbes ::
  Map HeraldFailureProbeId FailureProbe ->
  FailureState ->
  FailureState
setStateProbes probes (FailureState voters history _) =
  FailureState voters history probes

failureStateCanonicalBytes :: FailureState -> ByteString
failureStateCanonicalBytes state = SerializePut.runPut $ do
  putFramedBytes "ECLIPS-ORACLE-FAILURE-STATE"
  putCountedList
    (putFramedBytes . heraldMembershipGenerationCanonicalBytes)
    (failureMembershipHistory state)
  putFramedBytes
    (heraldMembershipGenerationCanonicalBytes (failureCurrentMembership state))
  putCountedList putProbe (Map.toAscList (stateProbes state))

-- The checkpoint also owns the current voter hosts. They cannot be recovered
-- from the historical probe denominators after native voter changes.
data CheckpointFailureProbe = CheckpointFailureProbe ByteString ByteString ByteString Word64 [(ByteString, Word8)] (Maybe CheckpointFailureTerminal)
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

data CheckpointFailureTerminal
  = CheckpointFailureAccepted ByteString
  | CheckpointFailureDismissed ByteString Word64
  | CheckpointFailureRetired ByteString ByteString Word64
  | CheckpointFailureSuperseded ByteString Word64
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

encodeFailureCheckpoint :: FailureState -> ByteString
encodeFailureCheckpoint (FailureState voters history probes) =
  S.encode
    ( fmap heraldEpochBytes (Set.toAscList voters),
      heraldMembershipHistoryCanonicalBytes history,
      fmap encodeProbe (Map.elems probes)
    )
  where
    encodeProbe (FailureProbe _ target generation configuration opened reports terminal) =
      CheckpointFailureProbe
        (heraldEpochBytes target)
        (heraldMembershipGenerationIdBytes generation)
        (V.encodeVoterConfiguration configuration)
        (controlIndexWord64 opened)
        (fmap (\(host, result) -> (heraldEpochBytes host, fromIntegral (fromEnum result))) (Map.toAscList reports))
        (encodeTerminal <$> terminal)
    encodeTerminal terminal = case terminal of
      ProbeVoterHostFailureAccepted certificate -> CheckpointFailureAccepted (encodeAcceptedVoterHostFailure certificate)
      ProbeDismissed resolution index -> CheckpointFailureDismissed (failureProbeResolutionIdCanonicalBytes resolution) (controlIndexWord64 index)
      ProbeRetired resolution successor index -> CheckpointFailureRetired (failureProbeResolutionIdCanonicalBytes resolution) (heraldMembershipGenerationIdBytes successor) (controlIndexWord64 index)
      ProbeMembershipSuperseded successor index -> CheckpointFailureSuperseded (heraldMembershipGenerationIdBytes successor) (controlIndexWord64 index)

decodeFailureCheckpoint :: CheckedOracleGenesis -> ByteString -> Either String FailureState
decodeFailureCheckpoint genesis bytes = do
  (rawVoters, rawHistory, rawProbes) <- S.decode bytes
  voters <- traverse (admit . mkHeraldEpoch) rawVoters
  history <- admit (decodeHeraldMembershipHistoryCanonicalBytes rawHistory)
  let generations = NonEmpty.toList (heraldMembershipHistoryGenerations history)
      byGeneration = Map.fromList [(heraldMembershipGenerationId generation, generation) | generation <- generations]
  probes <- traverse (decodeProbe byGeneration (heraldMembershipGenerationId (heraldMembershipHistoryCurrent history))) rawProbes
  let state = FailureState (Set.fromList voters) history (Map.fromList [(probeIdentifier probe, probe) | probe <- probes])
      live = [(probeTarget probe, probeGeneration probe) | probe <- probes, probeTerminal probe == Nothing]
  unless
    ( take 1 generations == take 1 (failureMembershipHistory (initialFailureState genesis))
        && Set.size (Set.fromList live) == length live
        && encodeFailureCheckpoint state == bytes
    )
    (Left "invalid failure checkpoint")
  pure state
  where
    admit :: (Show problem) => Either problem value -> Either String value
    admit = either (Left . show) Right
    decodeProbe generations current (CheckpointFailureProbe rawTarget rawGeneration rawConfiguration rawOpened rawReports rawTerminal) = do
      let opened = controlIndex rawOpened
      identifier <- admit (deriveHeraldFailureProbeId opened)
      target <- admit (mkHeraldEpoch rawTarget)
      generation <- admit (mkHeraldMembershipGenerationId rawGeneration)
      captured <- maybe (Left "unknown failure checkpoint membership") Right (Map.lookup generation generations)
      configuration <- admit (V.decodeVoterConfiguration rawConfiguration)
      reports <- traverse (\(host, result) -> (,) <$> admit (mkHeraldEpoch host) <*> decodeResult result) rawReports
      terminal <- traverse decodeTerminal rawTerminal
      let record = FailureProbe identifier target generation configuration opened (Map.fromList reports) terminal
          terminalValid = case terminal of
            Nothing -> generation == current
            Just (ProbeVoterHostFailureAccepted certificate) ->
              acceptedVoterHostFailureResolution certificate == deriveFailureProbeResolutionId identifier RetireFailureProbeTarget
                && acceptedVoterHostFailureTarget certificate == target
                && acceptedVoterHostFailureMembership certificate == generation
                && acceptedVoterHostFailureConfiguration certificate == configuration
                && acceptedVoterHostFailureReports certificate == reports
            Just (ProbeDismissed resolution index) ->
              resolution == deriveFailureProbeResolutionId identifier DismissFailureProbe
                && opened < index
                && hasStrictMajority DismissFailureProbe record
            Just (ProbeRetired resolution successor index) ->
              resolution == deriveFailureProbeResolutionId identifier RetireFailureProbeTarget
                && opened < index
                && hasStrictMajority RetireFailureProbeTarget record
                && maybe False (\next -> heraldMembershipGenerationRetirementId next == Just resolution && heraldMembershipGenerationRetiredHeraldEpoch next == Just target) (Map.lookup successor generations)
            Just (ProbeMembershipSuperseded successor index) -> opened < index && Map.member successor generations
      unless
        ( target `elem` heraldMembershipGenerationActiveHeraldEpochs captured
            && V.voterConfigurationControlIndex configuration < opened
            && all ((`Set.member` configurationHosts configuration) . fst) reports
            && terminalValid
        )
        (Left "invalid failure checkpoint probe")
      pure record
    decodeResult result = case result of
      0 -> Right ProbeReachable
      1 -> Right ProbeUnreachable
      _ -> Left "invalid failure checkpoint report"
    decodeTerminal terminal = case terminal of
      CheckpointFailureAccepted certificate -> ProbeVoterHostFailureAccepted <$> decodeAcceptedVoterHostFailure certificate
      CheckpointFailureDismissed resolution index -> ProbeDismissed <$> admit (decodeFailureProbeResolutionIdCanonicalBytes resolution) <*> pure (controlIndex index)
      CheckpointFailureRetired resolution successor index -> ProbeRetired <$> admit (decodeFailureProbeResolutionIdCanonicalBytes resolution) <*> admit (mkHeraldMembershipGenerationId successor) <*> pure (controlIndex index)
      CheckpointFailureSuperseded successor index -> ProbeMembershipSuperseded <$> admit (mkHeraldMembershipGenerationId successor) <*> pure (controlIndex index)

putProbe :: (HeraldFailureProbeId, FailureProbe) -> SerializePut.Put
putProbe (key, FailureProbe identifier target generation configuration opened reports terminal) = do
  SerializePut.putByteString (heraldFailureProbeIdCanonicalBytes key)
  SerializePut.putByteString (heraldFailureProbeIdCanonicalBytes identifier)
  SerializePut.putByteString (heraldEpochBytes target)
  SerializePut.putByteString (heraldMembershipGenerationIdBytes generation)
  putFramedBytes (V.encodeVoterConfiguration configuration)
  SerializePut.putWord64be (controlIndexWord64 opened)
  putCountedList putReport (Map.toAscList reports)
  case terminal of
    Nothing -> SerializePut.putWord8 0
    Just value -> do
      SerializePut.putWord8 1
      putTerminal value

putReport :: (HeraldEpoch, ProbeResult) -> SerializePut.Put
putReport (reporter, result) = do
  SerializePut.putByteString (heraldEpochBytes reporter)
  SerializePut.putWord8 $ case result of
    ProbeReachable -> 0
    ProbeUnreachable -> 1

putTerminal :: FailureProbeTerminal -> SerializePut.Put
putTerminal terminal = case terminal of
  ProbeVoterHostFailureAccepted certificate -> SerializePut.putWord8 3 >> putFramedBytes (encodeAcceptedVoterHostFailure certificate)
  ProbeDismissed resolution index -> do
    SerializePut.putWord8 0
    SerializePut.putByteString (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putWord64be (controlIndexWord64 index)
  ProbeRetired resolution successor index -> do
    SerializePut.putWord8 1
    SerializePut.putByteString (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
    SerializePut.putWord64be (controlIndexWord64 index)
  ProbeMembershipSuperseded successor index -> do
    SerializePut.putWord8 2
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
    SerializePut.putWord64be (controlIndexWord64 index)

putCountedList :: (value -> SerializePut.Put) -> [value] -> SerializePut.Put
putCountedList putValue values = do
  SerializePut.putWord32be (fromIntegral (length values) :: Word32)
  mapM_ putValue values

putFramedBytes :: ByteString -> SerializePut.Put
putFramedBytes bytes = do
  SerializePut.putWord32be (fromIntegral (ByteString.length bytes) :: Word32)
  SerializePut.putByteString bytes

-- | Install an admitted exact successor and supersede every old-generation
-- probe. The Herald admission owner already proved the insertion and history.
installAdmissionMembership :: ControlIndex -> HeraldMembershipGeneration -> FailureState -> (FailureState, [FailureEvent])
installAdmissionMembership index generation state =
  (setStateProbes probes (setStateMembership generation state), HeraldMembershipAdvancedEvent generation : events)
  where
    successor = heraldMembershipGenerationId generation
    predecessor = heraldMembershipGenerationId (failureCurrentMembership state)
    (probes, events) = Map.foldrWithKey supersede (stateProbes state, []) (stateProbes state)
    supersede ident probe (updated, retained)
      | probeTerminal probe == Nothing && probeGeneration probe == predecessor =
          (Map.insert ident (setProbeTerminal (Just (ProbeMembershipSuperseded successor index)) probe) updated, FailureProbeSupersededEvent ident successor : retained)
      | otherwise = (updated, retained)

-- A sufficient retirement certificate retains the membership slot until its
-- semantic retirement commits. A voter change may not rebase that certificate.
failureHasPendingRetirement :: FailureState -> Bool
failureHasPendingRetirement state = any accepted (Map.elems (stateProbes state))
  where
    accepted probe = probeTerminal probe == Nothing && hasStrictMajority RetireFailureProbeTarget probe

-- Supersession retains each probe and its original reports. The semantic
-- generation remains the same when only voter authority changes; no report is
-- transferred into a fresh probe under the successor voters.
supersedeFailureProbesForVoters :: ControlIndex -> Set HeraldEpoch -> FailureState -> (FailureState, [FailureEvent])
supersedeFailureProbesForVoters index voters state@(FailureState _ history _) =
  (FailureState voters history probes, events)
  where
    generation = heraldMembershipGenerationId (failureCurrentMembership state)
    (probes, events) = Map.foldrWithKey supersede (stateProbes state, []) (stateProbes state)
    supersede ident probe (updated, retained)
      | probeTerminal probe == Nothing =
          ( Map.insert ident (setProbeTerminal (Just (ProbeMembershipSuperseded generation index)) probe) updated,
            FailureProbeSupersededEvent ident generation : retained
          )
      | otherwise = (updated, retained)
