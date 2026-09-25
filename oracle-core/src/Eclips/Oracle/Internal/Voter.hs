{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Finite, deterministic ownership of Oracle replica and configuration history.
module Eclips.Oracle.Internal.Voter
  ( OracleReplicaEndpoint,
    oracleReplicaEndpoint,
    replicaEndpointHost,
    replicaEndpointPort,
    OracleReplicaContact,
    oracleReplicaContact,
    replicaContactOracle,
    replicaContactRaft,
    replicaContactDiscovery,
    OracleReplicaRegistration,
    replicaRegistrationNode,
    replicaRegistrationHost,
    replicaRegistrationContact,
    replicaRegistrationControlIndex,
    OracleVoterBindings,
    oracleVoterBindings,
    oracleVoterBindingList,
    VoterConfigurationId,
    voterConfigurationIdBytes,
    mkVoterConfigurationId,
    VoterConfiguration,
    voterConfigurationId,
    voterConfigurationNativeRef,
    VoterConfigurationView (..),
    voterConfigurationView,
    voterConfigurationBindings,
    voterConfigurationPredecessor,
    voterConfigurationControlIndex,
    voterConfigurationNative,
    VoterChangeId,
    voterChangeIdControlIndex,
    mkVoterChangeId,
    ExplicitVoterChangeReason (..),
    VoterChangeReason (..),
    VoterChangePhase (..),
    ConfigurationStage (..),
    VoterChange,
    voterChangeId,
    voterChangeExpectedConfiguration,
    voterChangeOldBindings,
    voterChangeNewBindings,
    voterChangeReason,
    voterChangePhase,
    voterChangeControlIndex,
    voterChangeLearners,
    VoterChangeProblem (..),
    VoterAdministrationResult (..),
    VoterCommand (..),
    VoterState,
    initialVoterState,
    voterRegistrations,
    voterCurrentConfiguration,
    voterConfigurationHistory,
    voterLookupChange,
    voterPendingChange,
    applyVoterCommand,
    beginAcceptedHostFailure,
    completeAcceptedHostFailure,
    prepareVoterConfiguration,
    applyVoterConfiguration,
    voterStateCanonicalBytes,
    encodeVoterCheckpoint,
    decodeVoterCheckpoint,
    voterStateIsGenesisIdle,
    encodeReplicaRegistration,
    decodeReplicaRegistration,
    encodeVoterConfiguration,
    decodeVoterConfiguration,
    encodeVoterChange,
    decodeVoterChange,
    encodeVoterCommand,
    decodeVoterCommand,
    encodeVoterProblem,
    decodeVoterProblem,
    encodeVoterResult,
    decodeVoterResult,
  ) where

import Control.Monad (unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Data.Word (Word16, Word64, Word8)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, controlIndex, controlIndexWord64, heraldEpochBytes, mkHeraldEpoch)
import Eclips.Domain.Membership (FailureProbeResolution (..), FailureProbeResolutionId, decodeFailureProbeResolutionIdCanonicalBytes, failureProbeResolutionDisposition, failureProbeResolutionIdCanonicalBytes)
import Eclips.Oracle.Genesis (CheckedOracleGenesis, RaftVoterBinding, checkedOracleRaftVoterBindings, raftVoterBinding, raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Raft.Configuration
import Eclips.Raft.Effect (RaftCommittedEntry, committedEntryIndex, committedEntryPayload, committedEntryTerm)
import Eclips.Raft.Identity
import Eclips.Raft.Input (RaftEntry (..))
import GHC.Generics (Generic)

data OracleReplicaEndpoint = OracleReplicaEndpoint ByteString Word16 deriving stock (Eq, Ord, Show)
oracleReplicaEndpoint :: ByteString -> Word16 -> Either VoterChangeProblem OracleReplicaEndpoint
oracleReplicaEndpoint host port
  | BS.null host || BS.elem 0 host || port == 0 = Left VoterInvalidEndpoint
  | Left _ <- Text.decodeUtf8' host = Left VoterInvalidEndpoint
  | otherwise = Right (OracleReplicaEndpoint host port)
replicaEndpointHost :: OracleReplicaEndpoint -> ByteString
replicaEndpointHost (OracleReplicaEndpoint host _) = host
replicaEndpointPort :: OracleReplicaEndpoint -> Word16
replicaEndpointPort (OracleReplicaEndpoint _ port) = port

data OracleReplicaContact = OracleReplicaContact OracleReplicaEndpoint OracleReplicaEndpoint OracleReplicaEndpoint deriving stock (Eq, Ord, Show)
oracleReplicaContact :: OracleReplicaEndpoint -> OracleReplicaEndpoint -> OracleReplicaEndpoint -> OracleReplicaContact
oracleReplicaContact = OracleReplicaContact
replicaContactOracle, replicaContactRaft, replicaContactDiscovery :: OracleReplicaContact -> OracleReplicaEndpoint
replicaContactOracle (OracleReplicaContact value _ _) = value
replicaContactRaft (OracleReplicaContact _ value _) = value
replicaContactDiscovery (OracleReplicaContact _ _ value) = value

data OracleReplicaRegistration = OracleReplicaRegistration RaftNodeId HeraldEpoch (Maybe OracleReplicaContact) ControlIndex deriving stock (Eq, Show)
replicaRegistrationNode :: OracleReplicaRegistration -> RaftNodeId
replicaRegistrationNode (OracleReplicaRegistration node _ _ _) = node
replicaRegistrationHost :: OracleReplicaRegistration -> HeraldEpoch
replicaRegistrationHost (OracleReplicaRegistration _ host _ _) = host
replicaRegistrationContact :: OracleReplicaRegistration -> Maybe OracleReplicaContact
replicaRegistrationContact (OracleReplicaRegistration _ _ contact _) = contact
replicaRegistrationControlIndex :: OracleReplicaRegistration -> ControlIndex
replicaRegistrationControlIndex (OracleReplicaRegistration _ _ _ index) = index

newtype OracleVoterBindings = OracleVoterBindings [RaftVoterBinding] deriving stock (Eq, Ord, Show)
oracleVoterBindings :: [RaftVoterBinding] -> Either VoterChangeProblem OracleVoterBindings
oracleVoterBindings bindings
  | null bindings = Left VoterWouldRemoveLastVoter
  | not (unique (fmap raftVoterBindingNode bindings)) || not (unique (fmap raftVoterBindingHeraldEpoch bindings)) = Left VoterBindingConflict
  | otherwise = Right (OracleVoterBindings (sortOn raftVoterBindingNode bindings))
  where
    unique values = Set.size (Set.fromList values) == length values
oracleVoterBindingList :: OracleVoterBindings -> [RaftVoterBinding]
oracleVoterBindingList (OracleVoterBindings bindings) = bindings

newtype VoterConfigurationId = VoterConfigurationId ByteString deriving stock (Eq, Ord, Show)
voterConfigurationIdBytes :: VoterConfigurationId -> ByteString
voterConfigurationIdBytes (VoterConfigurationId value) = value
mkVoterConfigurationId :: ByteString -> Either VoterChangeProblem VoterConfigurationId
mkVoterConfigurationId bytes
  | BS.length bytes == 32 = Right (VoterConfigurationId bytes)
  | otherwise = Left VoterMalformedClaim

data VoterConfigurationView = StableVoterConfigurationView OracleVoterBindings | JointVoterConfigurationView OracleVoterBindings OracleVoterBindings deriving stock (Eq, Show)
data VoterConfiguration = VoterConfiguration VoterConfigurationId RaftConfigurationRef VoterConfigurationView (Maybe VoterConfigurationId) ControlIndex deriving stock (Eq, Show)
voterConfigurationId :: VoterConfiguration -> VoterConfigurationId
voterConfigurationId (VoterConfiguration value _ _ _ _) = value
voterConfigurationNativeRef :: VoterConfiguration -> RaftConfigurationRef
voterConfigurationNativeRef (VoterConfiguration _ value _ _ _) = value
voterConfigurationView :: VoterConfiguration -> VoterConfigurationView
voterConfigurationView (VoterConfiguration _ _ value _ _) = value
voterConfigurationPredecessor :: VoterConfiguration -> Maybe VoterConfigurationId
voterConfigurationPredecessor (VoterConfiguration _ _ _ value _) = value
voterConfigurationControlIndex :: VoterConfiguration -> ControlIndex
voterConfigurationControlIndex (VoterConfiguration _ _ _ _ value) = value
voterConfigurationBindings :: VoterConfiguration -> [RaftVoterBinding]
voterConfigurationBindings configuration =
  Map.elems
    $ Map.fromList
      [ (raftVoterBindingNode binding, binding)
      | binding <- case voterConfigurationView configuration of
          StableVoterConfigurationView bindings -> oracleVoterBindingList bindings
          JointVoterConfigurationView old new -> oracleVoterBindingList old <> oracleVoterBindingList new
      ]
voterConfigurationNative :: VoterConfiguration -> RaftVotingConfiguration
voterConfigurationNative = nativeView . voterConfigurationView
nativeView :: VoterConfigurationView -> RaftVotingConfiguration
nativeView (StableVoterConfigurationView bindings) = stableRaftConfiguration (nativeSet bindings)
nativeView (JointVoterConfigurationView old new) = jointRaftConfiguration (nativeSet old) (nativeSet new)
nativeSet :: OracleVoterBindings -> RaftVoterSet
nativeSet = either (error . show) id . raftVoterSet . fmap raftVoterBindingNode . oracleVoterBindingList

newtype VoterChangeId = VoterChangeId ControlIndex deriving stock (Eq, Ord, Show)
voterChangeIdControlIndex :: VoterChangeId -> ControlIndex
voterChangeIdControlIndex (VoterChangeId index) = index
mkVoterChangeId :: ControlIndex -> Either VoterChangeProblem VoterChangeId
mkVoterChangeId index
  | controlIndexWord64 index == 0 = Left VoterMalformedClaim
  | otherwise = Right (VoterChangeId index)
data ExplicitVoterChangeReason = ExplicitCommission | ExplicitDemotion
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)
data VoterChangeReason = ExplicitVoterReason ExplicitVoterChangeReason | AcceptedHostFailureReason FailureProbeResolutionId
  deriving stock (Eq, Ord, Show)
data VoterChangePhase = VoterChangePreparing | VoterChangeJointCommitted | VoterChangeCompleted | VoterChangeCancelled | VoterExcludedAwaitingHeraldRetirement
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)
data ConfigurationStage = JointConfigurationStage | FinalConfigurationStage
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)
data VoterChange = VoterChange VoterChangeId VoterConfigurationId OracleVoterBindings OracleVoterBindings VoterChangeReason VoterChangePhase ControlIndex deriving stock (Eq, Show)
voterChangeId :: VoterChange -> VoterChangeId
voterChangeId (VoterChange value _ _ _ _ _ _) = value
voterChangeExpectedConfiguration :: VoterChange -> VoterConfigurationId
voterChangeExpectedConfiguration (VoterChange _ value _ _ _ _ _) = value
voterChangeOldBindings, voterChangeNewBindings :: VoterChange -> OracleVoterBindings
voterChangeOldBindings (VoterChange _ _ value _ _ _ _) = value
voterChangeNewBindings (VoterChange _ _ _ value _ _ _) = value
voterChangeReason :: VoterChange -> VoterChangeReason
voterChangeReason (VoterChange _ _ _ _ value _ _) = value
voterChangePhase :: VoterChange -> VoterChangePhase
voterChangePhase (VoterChange _ _ _ _ _ value _) = value
voterChangeControlIndex :: VoterChange -> ControlIndex
voterChangeControlIndex (VoterChange _ _ _ _ _ _ value) = value
voterChangeLearners :: VoterChange -> [RaftNodeId]
voterChangeLearners change = Set.toAscList (nodes (voterChangeNewBindings change) Set.\\ nodes (voterChangeOldBindings change))
nodes :: OracleVoterBindings -> Set RaftNodeId
nodes = Set.fromList . fmap raftVoterBindingNode . oracleVoterBindingList

data VoterChangeProblem
  = VoterMalformedClaim
  | VoterInvalidEndpoint
  | VoterBindingConflict
  | VoterHostNotActive
  | VoterUnknownReplica
  | VoterStaleConfiguration
  | VoterChangeInProgress
  | VoterWouldRemoveLastVoter
  | VoterAlreadyVoter
  | VoterAlreadyLearner
  | VoterUnknownChange
  | VoterJointInProgress
  | VoterChangeTerminal
  | VoterReasonMismatch
  | VoterMembershipCoordinationBusy
  | VoterCommittedConfigurationContradiction
  | VoterAcceptedFailureNotCancellable
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)
data VoterAdministrationResult = ReplicaRegistered OracleReplicaRegistration | ReplicaAlreadyVoter OracleReplicaRegistration | ReplicaAlreadyLearner OracleReplicaRegistration | VoterChangeAccepted VoterChange | VoterChangeCancelledResult VoterChange deriving stock (Eq, Show)
data VoterCommand = RegisterOracleReplica RaftNodeId HeraldEpoch OracleReplicaContact | BeginVoterChange VoterConfigurationId ExplicitVoterChangeReason OracleVoterBindings | CancelVoterChange VoterChangeId deriving stock (Eq, Show)
data VoterState = VoterState (Map RaftNodeId OracleReplicaRegistration) [VoterConfiguration] (Map VoterChangeId VoterChange) (Maybe VoterChangeId) deriving stock (Eq, Show)
initialVoterState :: CheckedOracleGenesis -> VoterState
initialVoterState genesis = VoterState registrations [configuration] Map.empty Nothing
  where
    bindings = either (error . show) id (oracleVoterBindings (checkedOracleRaftVoterBindings genesis))
    configuration = makeConfiguration genesisRaftConfigurationRef (StableVoterConfigurationView bindings) Nothing (controlIndex 0)
    registrations = Map.fromList [(raftVoterBindingNode b, OracleReplicaRegistration (raftVoterBindingNode b) (raftVoterBindingHeraldEpoch b) Nothing (controlIndex 0)) | b <- oracleVoterBindingList bindings]
voterRegistrations :: VoterState -> [OracleReplicaRegistration]
voterRegistrations (VoterState registrations _ _ _) = Map.elems registrations
voterCurrentConfiguration :: VoterState -> VoterConfiguration
voterCurrentConfiguration (VoterState _ (current : _) _ _) = current
voterCurrentConfiguration _ = error "admitted voter state has no genesis configuration"
voterConfigurationHistory :: VoterState -> [VoterConfiguration]
voterConfigurationHistory (VoterState _ history _ _) = reverse history
voterLookupChange :: VoterChangeId -> VoterState -> Maybe VoterChange
voterLookupChange ident (VoterState _ _ changes _) = Map.lookup ident changes
voterPendingChange :: VoterState -> Maybe VoterChange
voterPendingChange state@(VoterState _ _ _ pending) = pending >>= (`voterLookupChange` state)

applyVoterCommand :: Set HeraldEpoch -> Bool -> ControlIndex -> VoterCommand -> VoterState -> Either VoterChangeProblem (VoterState, VoterAdministrationResult)
applyVoterCommand active membershipBusy index command state@(VoterState registrations history changes pending) = case command of
  RegisterOracleReplica node host contact -> do
    unless (Set.member host active) (Left VoterHostNotActive)
    case Map.lookup node registrations of
      Just registered -> do
        unless (replicaRegistrationHost registered == host) (Left VoterBindingConflict)
        case replicaRegistrationContact registered of
          Just retained | retained /= contact -> Left VoterBindingConflict
          _ -> pure ()
        let registration = case replicaRegistrationContact registered of
              Nothing -> OracleReplicaRegistration node host (Just contact) index
              Just _ -> registered
            successor = VoterState (Map.insert node registration registrations) history changes pending
            voter = node `elem` fmap raftVoterBindingNode (voterConfigurationBindings current)
        pure (successor, if voter then ReplicaAlreadyVoter registration else ReplicaAlreadyLearner registration)
      Nothing -> do
        unless (all ((/= host) . replicaRegistrationHost) (Map.elems registrations)) (Left VoterBindingConflict)
        let registration = OracleReplicaRegistration node host (Just contact) index
        pure (VoterState (Map.insert node registration registrations) history changes pending, ReplicaRegistered registration)
  BeginVoterChange expected reason new -> do
    unless (pending == Nothing) (Left VoterChangeInProgress)
    unless (not membershipBusy) (Left VoterMembershipCoordinationBusy)
    unless (voterConfigurationId current == expected) (Left VoterStaleConfiguration)
    old <- case voterConfigurationView current of
      StableVoterConfigurationView bindings -> Right bindings
      JointVoterConfigurationView {} -> Left VoterJointInProgress
    unless (old /= new) (Left (if reason == ExplicitCommission then VoterAlreadyVoter else VoterAlreadyLearner))
    mapM_ checkBinding (oracleVoterBindingList new)
    unless
      ( case reason of
          ExplicitCommission -> nodes old `Set.isProperSubsetOf` nodes new
          ExplicitDemotion -> nodes new `Set.isProperSubsetOf` nodes old
      )
      (Left VoterReasonMismatch)
    let change = VoterChange (VoterChangeId index) expected old new (ExplicitVoterReason reason) VoterChangePreparing index
    pure (VoterState registrations history (Map.insert (voterChangeId change) change changes) (Just (voterChangeId change)), VoterChangeAccepted change)
  CancelVoterChange ident -> do
    change <- maybe (Left VoterUnknownChange) Right (Map.lookup ident changes)
    case voterChangeReason change of
      AcceptedHostFailureReason _ -> Left VoterAcceptedFailureNotCancellable
      ExplicitVoterReason _ -> pure ()
    case voterChangePhase change of
      VoterChangeCancelled -> Right (state, VoterChangeCancelledResult change)
      VoterChangePreparing -> do
        unless (pending == Just ident) (Left VoterCommittedConfigurationContradiction)
        let cancelled = setChangePhase index VoterChangeCancelled change
        pure (VoterState registrations history (Map.insert ident cancelled changes) Nothing, VoterChangeCancelledResult cancelled)
      VoterChangeJointCommitted -> Left VoterJointInProgress
      VoterChangeCompleted -> Left VoterChangeTerminal
      VoterExcludedAwaitingHeraldRetirement -> Left VoterAcceptedFailureNotCancellable
  where
    current = voterCurrentConfiguration state
    checkBinding binding = do
      unless (Set.member (raftVoterBindingHeraldEpoch binding) active) (Left VoterHostNotActive)
      registration <- maybe (Left VoterUnknownReplica) Right (Map.lookup (raftVoterBindingNode binding) registrations)
      unless (replicaRegistrationHost registration == raftVoterBindingHeraldEpoch binding) (Left VoterBindingConflict)

-- The enclosing failure owner has admitted the immutable old-configuration
-- certificate. Only this private operation creates an automatic change reason.
beginAcceptedHostFailure :: ControlIndex -> FailureProbeResolutionId -> HeraldEpoch -> VoterState -> Either VoterChangeProblem (VoterState, VoterChange)
beginAcceptedHostFailure index resolution target state@(VoterState registrations history changes pending) = do
  unless (pending == Nothing) (Left VoterChangeInProgress)
  old <- case voterConfigurationView current of
    StableVoterConfigurationView bindings -> Right bindings
    JointVoterConfigurationView {} -> Left VoterJointInProgress
  let retained = filter ((/= target) . raftVoterBindingHeraldEpoch) (oracleVoterBindingList old)
  unless (length retained < length (oracleVoterBindingList old)) (Left VoterReasonMismatch)
  new <- oracleVoterBindings retained
  let change = VoterChange (VoterChangeId index) (voterConfigurationId current) old new (AcceptedHostFailureReason resolution) VoterChangePreparing index
  pure (VoterState registrations history (Map.insert (voterChangeId change) change changes) (Just (voterChangeId change)), change)
  where
    current = voterCurrentConfiguration state

completeAcceptedHostFailure :: ControlIndex -> FailureProbeResolutionId -> VoterChangeId -> VoterState -> Either VoterChangeProblem (VoterState, VoterChange)
completeAcceptedHostFailure index resolution ident state@(VoterState registrations history changes pending) = do
  change <- maybe (Left VoterUnknownChange) Right (Map.lookup ident changes)
  unless (voterChangeReason change == AcceptedHostFailureReason resolution) (Left VoterReasonMismatch)
  case voterChangePhase change of
    VoterChangeCompleted -> pure (state, change)
    VoterExcludedAwaitingHeraldRetirement -> do
      unless (pending == Just ident) (Left VoterCommittedConfigurationContradiction)
      let completed = setChangePhase index VoterChangeCompleted change
      pure (VoterState registrations history (Map.insert ident completed changes) Nothing, completed)
    _ -> Left VoterJointInProgress

prepareVoterConfiguration :: VoterChangeId -> ConfigurationStage -> VoterState -> Either VoterChangeProblem (RaftVotingConfiguration, ByteString)
prepareVoterConfiguration ident stage state = do
  change <- maybe (Left VoterUnknownChange) Right (voterLookupChange ident state)
  unless (fmap voterChangeId (voterPendingChange state) == Just ident) (Left VoterChangeTerminal)
  let current = voterCurrentConfiguration state
  view <- case (stage, voterChangePhase change, voterConfigurationView current) of
    (JointConfigurationStage, VoterChangePreparing, StableVoterConfigurationView old)
      | voterConfigurationId current == voterChangeExpectedConfiguration change && old == voterChangeOldBindings change ->
          Right (JointVoterConfigurationView old (voterChangeNewBindings change))
    (FinalConfigurationStage, VoterChangeJointCommitted, JointVoterConfigurationView old new)
      | old == voterChangeOldBindings change && new == voterChangeNewBindings change -> Right (StableVoterConfigurationView new)
    _ -> Left VoterJointInProgress
  pure (nativeView view, S.encode (RawMetadata (controlIndexWord64 (voterChangeIdControlIndex ident)) stage (voterConfigurationIdBytes (voterChangeExpectedConfiguration change)) (rawBindings (voterChangeOldBindings change)) (rawBindings (voterChangeNewBindings change))))

-- Only a sealed native committed entry can drive configuration application.
applyVoterConfiguration :: ControlIndex -> RaftCommittedEntry ByteString -> VoterState -> Either VoterChangeProblem (VoterState, VoterConfiguration, VoterChange)
applyVoterConfiguration index entry state@(VoterState registrations history changes pending) = do
  (native, bytes) <- case committedEntryPayload entry of
    Configuration configuration metadata -> Right (configuration, metadata)
    _ -> Left VoterCommittedConfigurationContradiction
  RawMetadata rawIdent stage _ _ _ <- decodeChecked bytes
  ident <- mkVoterChangeId (controlIndex rawIdent)
  (expectedNative, expectedBytes) <- prepareVoterConfiguration ident stage state
  unless (native == expectedNative && bytes == expectedBytes) (Left VoterCommittedConfigurationContradiction)
  ref <- either (const (Left VoterCommittedConfigurationContradiction)) Right (raftConfigurationEntryRef (committedEntryIndex entry) (committedEntryTerm entry))
  let current = voterCurrentConfiguration state
  unless (controlIndexWord64 index > controlIndexWord64 (voterConfigurationControlIndex current) && refIndex ref > refIndex (voterConfigurationNativeRef current)) (Left VoterCommittedConfigurationContradiction)
  change <- maybe (Left VoterUnknownChange) Right (Map.lookup ident changes)
  let view = case stage of
        JointConfigurationStage -> JointVoterConfigurationView (voterChangeOldBindings change) (voterChangeNewBindings change)
        FinalConfigurationStage -> StableVoterConfigurationView (voterChangeNewBindings change)
      configuration = makeConfiguration ref view (Just (voterConfigurationId current)) index
      finalPhase = case voterChangeReason change of ExplicitVoterReason _ -> VoterChangeCompleted; AcceptedHostFailureReason _ -> VoterExcludedAwaitingHeraldRetirement
      changed = setChangePhase index (case stage of JointConfigurationStage -> VoterChangeJointCommitted; FinalConfigurationStage -> finalPhase) change
      remaining = if voterChangePhase changed == VoterChangeCompleted then Nothing else pending
  pure (VoterState registrations (configuration : history) (Map.insert ident changed changes) remaining, configuration, changed)

setChangePhase :: ControlIndex -> VoterChangePhase -> VoterChange -> VoterChange
setChangePhase index phase (VoterChange ident expected old new reason _ _) = VoterChange ident expected old new reason phase index
makeConfiguration :: RaftConfigurationRef -> VoterConfigurationView -> Maybe VoterConfigurationId -> ControlIndex -> VoterConfiguration
makeConfiguration ref view predecessor index = VoterConfiguration ident ref view predecessor index
  where
    ident = VoterConfigurationId (SHA256.hash ("ECLIPS-ORACLE-VOTER-CONFIGURATION" <> S.encode (rawRef ref, rawView view)))
refIndex :: RaftConfigurationRef -> Word64
refIndex ref = case raftConfigurationRefView ref of GenesisRaftConfigurationRefView -> 0; RaftConfigurationEntryRefView index _ -> raftLogIndexWord64 index

type RawBindings = [(ByteString, ByteString)]
type RawEndpoint = (ByteString, Word16)
type RawContact = (RawEndpoint, RawEndpoint, RawEndpoint)
data RawRegistration = RawRegistration ByteString ByteString (Maybe RawContact) Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawConfiguration = RawConfiguration (Maybe (Word64, Word64)) (RawBindings, Maybe RawBindings) (Maybe ByteString) Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawReason = RawExplicit ExplicitVoterChangeReason | RawAcceptedFailure ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawChange = RawChange Word64 ByteString RawBindings RawBindings RawReason VoterChangePhase Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawCommand = RawRegister ByteString ByteString RawContact | RawBegin ByteString ExplicitVoterChangeReason RawBindings | RawCancel Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawResult = RawRegistered Word8 ByteString | RawChanged Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RawMetadata = RawMetadata Word64 ConfigurationStage ByteString RawBindings RawBindings
  deriving stock (Generic)
  deriving anyclass (Serialize)
rawBindings :: OracleVoterBindings -> RawBindings
rawBindings = fmap (\b -> (raftNodeIdBytes (raftVoterBindingNode b), heraldEpochBytes (raftVoterBindingHeraldEpoch b))) . oracleVoterBindingList
admitBindings :: RawBindings -> Either VoterChangeProblem OracleVoterBindings
admitBindings raw = do
  bindings <- mapM (\(node, host) -> raftVoterBinding <$> admit (mkRaftNodeId node) <*> admit (mkHeraldEpoch host)) raw
  checked <- oracleVoterBindings bindings
  unless (rawBindings checked == raw) (Left VoterMalformedClaim)
  pure checked
rawEndpoint :: OracleReplicaEndpoint -> RawEndpoint
rawEndpoint endpoint = (replicaEndpointHost endpoint, replicaEndpointPort endpoint)
rawContact :: OracleReplicaContact -> RawContact
rawContact contact = (rawEndpoint (replicaContactOracle contact), rawEndpoint (replicaContactRaft contact), rawEndpoint (replicaContactDiscovery contact))
admitContact :: RawContact -> Either VoterChangeProblem OracleReplicaContact
admitContact (a, b, c) = oracleReplicaContact <$> uncurry oracleReplicaEndpoint a <*> uncurry oracleReplicaEndpoint b <*> uncurry oracleReplicaEndpoint c
rawRef :: RaftConfigurationRef -> Maybe (Word64, Word64)
rawRef ref = case raftConfigurationRefView ref of GenesisRaftConfigurationRefView -> Nothing; RaftConfigurationEntryRefView index term -> Just (raftLogIndexWord64 index, raftTermWord64 term)
admitRef :: Maybe (Word64, Word64) -> Either VoterChangeProblem RaftConfigurationRef
admitRef Nothing = Right genesisRaftConfigurationRef
admitRef (Just (index, term)) = admit (raftConfigurationEntryRef (raftLogIndex index) (raftTerm term))
rawView :: VoterConfigurationView -> (RawBindings, Maybe RawBindings)
rawView (StableVoterConfigurationView bindings) = (rawBindings bindings, Nothing)
rawView (JointVoterConfigurationView old new) = (rawBindings old, Just (rawBindings new))
admitView :: (RawBindings, Maybe RawBindings) -> Either VoterChangeProblem VoterConfigurationView
admitView (rawOld, rawNew) = do
  old <- admitBindings rawOld
  case rawNew of
    Nothing -> Right (StableVoterConfigurationView old)
    Just bytes -> do
      new <- admitBindings bytes
      let allBindings = oracleVoterBindingList old <> oracleVoterBindingList new
          byNode = Map.fromListWith Set.union [(raftVoterBindingNode b, Set.singleton (raftVoterBindingHeraldEpoch b)) | b <- allBindings]
          byHost = Map.fromListWith Set.union [(raftVoterBindingHeraldEpoch b, Set.singleton (raftVoterBindingNode b)) | b <- allBindings]
      unless (old /= new && all ((== 1) . Set.size) (Map.elems byNode) && all ((== 1) . Set.size) (Map.elems byHost)) (Left VoterBindingConflict)
      Right (JointVoterConfigurationView old new)
encodeReplicaRegistration :: OracleReplicaRegistration -> ByteString
encodeReplicaRegistration r = S.encode (RawRegistration (raftNodeIdBytes (replicaRegistrationNode r)) (heraldEpochBytes (replicaRegistrationHost r)) (rawContact <$> replicaRegistrationContact r) (controlIndexWord64 (replicaRegistrationControlIndex r)))
decodeReplicaRegistration :: ByteString -> Either VoterChangeProblem OracleReplicaRegistration
decodeReplicaRegistration bytes = do
  RawRegistration node host contact index <- decodeChecked bytes
  unless (case contact of Nothing -> index == 0; Just _ -> index > 0) (Left VoterMalformedClaim)
  OracleReplicaRegistration <$> admit (mkRaftNodeId node) <*> admit (mkHeraldEpoch host) <*> traverse admitContact contact <*> pure (controlIndex index)
encodeVoterConfiguration :: VoterConfiguration -> ByteString
encodeVoterConfiguration c = S.encode (RawConfiguration (rawRef (voterConfigurationNativeRef c)) (rawView (voterConfigurationView c)) (voterConfigurationIdBytes <$> voterConfigurationPredecessor c) (controlIndexWord64 (voterConfigurationControlIndex c)))
decodeVoterConfiguration :: ByteString -> Either VoterChangeProblem VoterConfiguration
decodeVoterConfiguration bytes = do
  RawConfiguration rawReference rawBindingsView rawPredecessor rawIndex <- decodeChecked bytes
  ref <- admitRef rawReference
  view <- admitView rawBindingsView
  predecessor <- traverse mkVoterConfigurationId rawPredecessor
  unless
    ( case (rawReference, view, predecessor, rawIndex) of
        (Nothing, StableVoterConfigurationView {}, Nothing, 0) -> True
        (Just _, _, Just _, i) -> i > 0
        _ -> False
    )
    (Left VoterMalformedClaim)
  pure (makeConfiguration ref view predecessor (controlIndex rawIndex))
encodeVoterChange :: VoterChange -> ByteString
encodeVoterChange c = S.encode (RawChange (controlIndexWord64 (voterChangeIdControlIndex (voterChangeId c))) (voterConfigurationIdBytes (voterChangeExpectedConfiguration c)) (rawBindings (voterChangeOldBindings c)) (rawBindings (voterChangeNewBindings c)) (rawReason (voterChangeReason c)) (voterChangePhase c) (controlIndexWord64 (voterChangeControlIndex c)))
decodeVoterChange :: ByteString -> Either VoterChangeProblem VoterChange
decodeVoterChange bytes = do
  RawChange ident expected old new rawChangeReason phase index <- decodeChecked bytes
  reason <- admitReason rawChangeReason
  ident' <- mkVoterChangeId (controlIndex ident)
  expected' <- mkVoterConfigurationId expected
  old' <- admitBindings old
  new' <- admitBindings new
  _ <- admitView (old, Just new)
  unless
    ( (if phase == VoterChangePreparing then index == ident else index > ident) && case reason of
        ExplicitVoterReason ExplicitCommission -> phase /= VoterExcludedAwaitingHeraldRetirement && nodes old' `Set.isProperSubsetOf` nodes new'
        ExplicitVoterReason ExplicitDemotion -> phase /= VoterExcludedAwaitingHeraldRetirement && nodes new' `Set.isProperSubsetOf` nodes old'
        AcceptedHostFailureReason _ -> phase /= VoterChangeCancelled && nodes new' `Set.isProperSubsetOf` nodes old' && Set.size (nodes old') == Set.size (nodes new') + 1
    )
    (Left VoterMalformedClaim)
  pure (VoterChange ident' expected' old' new' reason phase (controlIndex index))
rawReason :: VoterChangeReason -> RawReason
rawReason (ExplicitVoterReason reason) = RawExplicit reason
rawReason (AcceptedHostFailureReason resolution) = RawAcceptedFailure (failureProbeResolutionIdCanonicalBytes resolution)
admitReason :: RawReason -> Either VoterChangeProblem VoterChangeReason
admitReason (RawExplicit reason) = Right (ExplicitVoterReason reason)
admitReason (RawAcceptedFailure bytes) = do
  resolution <- admit (decodeFailureProbeResolutionIdCanonicalBytes bytes)
  unless (failureProbeResolutionDisposition resolution == RetireFailureProbeTarget) (Left VoterMalformedClaim)
  pure (AcceptedHostFailureReason resolution)
encodeVoterCommand :: VoterCommand -> ByteString
encodeVoterCommand command = S.encode $ case command of
  RegisterOracleReplica node host contact -> RawRegister (raftNodeIdBytes node) (heraldEpochBytes host) (rawContact contact)
  BeginVoterChange expected reason bindings -> RawBegin (voterConfigurationIdBytes expected) reason (rawBindings bindings)
  CancelVoterChange ident -> RawCancel (controlIndexWord64 (voterChangeIdControlIndex ident))
decodeVoterCommand :: ByteString -> Either VoterChangeProblem VoterCommand
decodeVoterCommand bytes = do
  raw <- decodeChecked bytes
  case raw of
    RawRegister node host contact -> RegisterOracleReplica <$> admit (mkRaftNodeId node) <*> admit (mkHeraldEpoch host) <*> admitContact contact
    RawBegin expected reason bindings -> BeginVoterChange <$> mkVoterConfigurationId expected <*> pure reason <*> admitBindings bindings
    RawCancel ident -> CancelVoterChange <$> mkVoterChangeId (controlIndex ident)
encodeVoterProblem :: VoterChangeProblem -> ByteString
encodeVoterProblem = S.encode
decodeVoterProblem :: ByteString -> Either VoterChangeProblem VoterChangeProblem
decodeVoterProblem = decodeChecked
encodeVoterResult :: VoterAdministrationResult -> ByteString
encodeVoterResult result = S.encode $ case result of
  ReplicaRegistered r -> RawRegistered 0 (encodeReplicaRegistration r)
  ReplicaAlreadyVoter r -> RawRegistered 1 (encodeReplicaRegistration r)
  ReplicaAlreadyLearner r -> RawRegistered 2 (encodeReplicaRegistration r)
  VoterChangeAccepted c -> RawChanged 0 (encodeVoterChange c)
  VoterChangeCancelledResult c -> RawChanged 1 (encodeVoterChange c)
decodeVoterResult :: ByteString -> Either VoterChangeProblem VoterAdministrationResult
decodeVoterResult bytes = do
  raw <- decodeChecked bytes
  case raw of
    RawRegistered 0 b -> ReplicaRegistered <$> decodeReplicaRegistration b
    RawRegistered 1 b -> ReplicaAlreadyVoter <$> decodeReplicaRegistration b
    RawRegistered 2 b -> ReplicaAlreadyLearner <$> decodeReplicaRegistration b
    RawChanged 0 b -> VoterChangeAccepted <$> decodeVoterChange b
    RawChanged 1 b -> VoterChangeCancelledResult <$> decodeVoterChange b
    _ -> Left VoterMalformedClaim
voterStateCanonicalBytes :: VoterState -> ByteString
voterStateCanonicalBytes state@(VoterState _ _ changes pending) = S.encode (fmap encodeReplicaRegistration (voterRegistrations state), fmap encodeVoterConfiguration (voterConfigurationHistory state), fmap encodeVoterChange (Map.elems changes), fmap (controlIndexWord64 . voterChangeIdControlIndex) pending)

encodeVoterCheckpoint :: VoterState -> ByteString
encodeVoterCheckpoint = voterStateCanonicalBytes

decodeVoterCheckpoint :: CheckedOracleGenesis -> ByteString -> Either String VoterState
decodeVoterCheckpoint genesis bytes = do
  (rawRegistrations, rawHistory, rawChanges, rawPending) <- S.decode bytes
  registrations <- traverse (checkpointClaim . decodeReplicaRegistration) rawRegistrations
  history <- traverse (checkpointClaim . decodeVoterConfiguration) rawHistory
  changes <- traverse (checkpointClaim . decodeVoterChange) rawChanges
  pending <- traverse (checkpointClaim . mkVoterChangeId . controlIndex) rawPending
  let byNode = Map.fromList [(replicaRegistrationNode registration, registration) | registration <- registrations]
      byChange = Map.fromList [(voterChangeId change, change) | change <- changes]
      byConfiguration = Map.fromList [(voterConfigurationId configuration, configuration) | configuration <- history]
      state = VoterState byNode (reverse history) byChange pending
      live = [voterChangeId change | change <- changes, voterChangePhase change `elem` [VoterChangePreparing, VoterChangeJointCommitted, VoterExcludedAwaitingHeraldRetirement]]
      initial = initialVoterState genesis
      genesisPresent registration = maybe False ((== replicaRegistrationHost registration) . replicaRegistrationHost) (Map.lookup (replicaRegistrationNode registration) byNode)
      bindingKnown binding = maybe False ((== raftVoterBindingHeraldEpoch binding) . replicaRegistrationHost) (Map.lookup (raftVoterBindingNode binding) byNode)
      linked (previous, next) = voterConfigurationPredecessor next == Just (voterConfigurationId previous) && voterConfigurationControlIndex previous < voterConfigurationControlIndex next
      changeKnown change = case Map.lookup (voterChangeExpectedConfiguration change) byConfiguration of
        Just configuration ->
          voterConfigurationView configuration == StableVoterConfigurationView (voterChangeOldBindings change)
            && voterConfigurationControlIndex configuration < voterChangeIdControlIndex (voterChangeId change)
            && all bindingKnown (oracleVoterBindingList (voterChangeNewBindings change))
        Nothing -> False
  unless
    ( take 1 history == [voterCurrentConfiguration initial]
        && all linked (zip history (drop 1 history))
        && all genesisPresent (voterRegistrations initial)
        && all (all bindingKnown . voterConfigurationBindings) history
        && all changeKnown changes
        && live == maybe [] pure pending
        && encodeVoterCheckpoint state == bytes
    )
    (Left "inconsistent voter checkpoint")
  pure state
  where
    checkpointClaim :: (Show problem) => Either problem value -> Either String value
    checkpointClaim = either (Left . show) Right
admit :: Either problem value -> Either VoterChangeProblem value
admit = either (const (Left VoterMalformedClaim)) Right
decodeChecked :: (Serialize value) => ByteString -> Either VoterChangeProblem value
decodeChecked bytes = do
  value <- admit (S.decode bytes)
  unless (S.encode value == bytes) (Left VoterMalformedClaim)
  pure value

voterStateIsGenesisIdle :: VoterState -> Bool
voterStateIsGenesisIdle (VoterState registrations history changes pending) =
  length history == 1
    && Map.null changes
    && pending == Nothing
    && all ((== Nothing) . replicaRegistrationContact) (Map.elems registrations)
