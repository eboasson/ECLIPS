{-# LANGUAGE RoleAnnotations #-}

-- | Protocol-owned claims, the closed current Raft DTO vocabulary, and pure
-- connection-role admission.
--
-- Decoding establishes structural wire shape only. Native voter membership,
-- current term, log agreement, and state transition admission remain owned by
-- the Raft runtime adapter and pure Raft kernel.
module Eclips.Protocol.Raft.Types
  ( RaftClaimError (..),
    RaftGenesisDigestDto,
    raftGenesisDigestDto,
    raftGenesisDigestDtoBytes,
    RaftNodeIdDto,
    raftNodeIdDto,
    raftNodeIdDtoBytes,
    RaftTermDto,
    raftTermDto,
    raftTermDtoWord64,
    RaftLogIndexDto,
    raftLogIndexDto,
    raftLogIndexDtoWord64,
    raftNodeIdDtoFromCore,
    raftNodeIdDtoToCore,
    raftTermDtoFromCore,
    raftTermDtoToCore,
    raftLogIndexDtoFromCore,
    raftLogIndexDtoToCore,
    RaftShapeError (..),
    RaftConfigurationSequenceProblem (..),
    RaftHelloClaim,
    raftHelloClaim,
    raftHelloGenesisDigest,
    raftHelloSource,
    raftHelloTarget,
    RaftVotingConfigurationDto,
    stableRaftConfigurationDto,
    jointRaftConfigurationDto,
    RaftVotingConfigurationDtoView (..),
    raftVotingConfigurationDtoView,
    raftVotingConfigurationDtoFromCore,
    raftVotingConfigurationDtoToCore,
    RaftLogEntryPayloadDto (..),
    RaftLogEntryDto,
    raftLogEntryDto,
    raftLogEntryIndex,
    raftLogEntryTerm,
    raftLogEntryPayload,
    RaftConflictHintDto (..),
    RaftRpcKind (..),
    RaftRpcDto,
    requestVoteDto,
    requestVoteResponseDto,
    appendEntriesDto,
    appendEntriesResponseDto,
    RaftCheckpointDto,
    raftCheckpointDto,
    raftCheckpointDtoFields,
    installSnapshotDto,
    installSnapshotResponseDto,
    raftRpcKind,
    raftRpcTerm,
    raftRpcSource,
    raftRequestVoteFields,
    raftRequestVoteResponseFields,
    raftAppendEntriesFields,
    raftAppendEntriesResponseFields,
    raftInstallSnapshotFields,
    raftInstallSnapshotResponseFields,
    RaftProtocolEnvelope (..),
    RaftConnectionContext,
    raftConnectionContext,
    raftConnectionGenesisDigest,
    raftConnectionLocalNode,
    raftConnectionRemoteNode,
    RaftConnectionPhase (..),
    RaftAdmissionError (..),
    admitRaftHello,
    admitRaftRpc,
    admitRaftEnvelope,
    RaftAdapterError (..),
    raftRpcDtoFromCore,
    raftRpcDtoToCore,
    raftRequestDtoToCoreInput,
    raftResponseDtoToCoreInput,
  )
where

import Control.Monad (unless)
import Data.Binary (Binary (..))
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Maybe (isJust)
import Data.Word (Word64)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Raft.Checkpoint qualified as CoreCheckpoint
import Eclips.Raft.Configuration qualified as CoreConfiguration
import Eclips.Raft.Identity qualified as CoreIdentity
import Eclips.Raft.Input qualified as CoreInput

claimByteCount :: Int
claimByteCount = 32

-- | A malformed exact-width ERFT claim.
data RaftClaimError
  = RaftClaimWrongByteCount
  { expectedRaftClaimByteCount :: Int,
    actualRaftClaimByteCount :: Int
  }
  deriving stock (Eq, Show)

-- A nominal role prevents identical byte carriers from being coerced between
-- immutable genesis-digest and node-identity namespaces.
newtype Claim domain = Claim ByteString
  deriving stock (Eq, Ord)

instance Show (Claim domain) where
  show = renderGroupedHex . claimBytes

type role Claim nominal

data RaftGenesisDigestDtoDomain

type RaftGenesisDigestDto = Claim RaftGenesisDigestDtoDomain

data RaftNodeIdDtoDomain

type RaftNodeIdDto = Claim RaftNodeIdDtoDomain

checkedClaim :: ByteString -> Either RaftClaimError (Claim domain)
checkedClaim bytes
  | ByteString.length bytes == claimByteCount = Right (Claim bytes)
  | otherwise =
      Left
        RaftClaimWrongByteCount
          { expectedRaftClaimByteCount = claimByteCount,
            actualRaftClaimByteCount = ByteString.length bytes
          }

claimBytes :: Claim domain -> ByteString
claimBytes (Claim bytes) = bytes

instance Binary (Claim domain) where
  put = put . claimBytes
  get = get >>= either (fail . show) pure . checkedClaim

-- | Check the immutable run/genesis digest's ERFT representation. It binds the
-- system, initial native node bindings, and timer policy. Mutable configuration
-- progress is deliberately absent from connection admission.
raftGenesisDigestDto :: ByteString -> Either RaftClaimError RaftGenesisDigestDto
raftGenesisDigestDto = checkedClaim

-- | Observe the exact digest bytes without assigning them semantic ownership.
raftGenesisDigestDtoBytes :: RaftGenesisDigestDto -> ByteString
raftGenesisDigestDtoBytes = claimBytes

-- | Check one opaque replica identity claim; it grants no native voting role.
raftNodeIdDto :: ByteString -> Either RaftClaimError RaftNodeIdDto
raftNodeIdDto = checkedClaim

-- | Observe one node claim's exact bytes for checked Raft-core adaptation.
raftNodeIdDtoBytes :: RaftNodeIdDto -> ByteString
raftNodeIdDtoBytes = claimBytes

-- | Protocol representation of a Raft term.  Zero is the genesis term.
newtype RaftTermDto = RaftTermDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

raftTermDto :: Word64 -> RaftTermDto
raftTermDto = RaftTermDto

raftTermDtoWord64 :: RaftTermDto -> Word64
raftTermDtoWord64 (RaftTermDto value) = value

-- | Protocol representation of a log position.  Zero is the sentinel/base.
newtype RaftLogIndexDto = RaftLogIndexDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

raftLogIndexDto :: Word64 -> RaftLogIndexDto
raftLogIndexDto = RaftLogIndexDto

raftLogIndexDtoWord64 :: RaftLogIndexDto -> Word64
raftLogIndexDtoWord64 (RaftLogIndexDto value) = value

-- | Project a checked core identity without assigning it a wire instance.
raftNodeIdDtoFromCore :: CoreIdentity.RaftNodeId -> Either RaftClaimError RaftNodeIdDto
raftNodeIdDtoFromCore = raftNodeIdDto . CoreIdentity.raftNodeIdBytes

-- | Admit an exact-width node claim into the core's nominal identity.
raftNodeIdDtoToCore :: RaftNodeIdDto -> Either CoreIdentity.RaftIdentityFault CoreIdentity.RaftNodeId
raftNodeIdDtoToCore = CoreIdentity.mkRaftNodeId . raftNodeIdDtoBytes

raftTermDtoFromCore :: CoreIdentity.RaftTerm -> RaftTermDto
raftTermDtoFromCore = raftTermDto . CoreIdentity.raftTermWord64

raftTermDtoToCore :: RaftTermDto -> CoreIdentity.RaftTerm
raftTermDtoToCore = CoreIdentity.raftTerm . raftTermDtoWord64

raftLogIndexDtoFromCore :: CoreIdentity.RaftLogIndex -> RaftLogIndexDto
raftLogIndexDtoFromCore = raftLogIndexDto . CoreIdentity.raftLogIndexWord64

raftLogIndexDtoToCore :: RaftLogIndexDto -> CoreIdentity.RaftLogIndex
raftLogIndexDtoToCore = CoreIdentity.raftLogIndex . raftLogIndexDtoWord64

-- | A structurally incoherent current ERFT value.
data RaftShapeError
  = RaftHelloEndpointsEqual RaftNodeIdDto
  | RaftVotingConfigurationEmpty
  | RaftVotersNotStrictlyAscending RaftNodeIdDto RaftNodeIdDto
  | RaftAppendConfigurationSequenceInvalid RaftLogIndexDto RaftConfigurationSequenceProblem
  | RaftLogEntryUsesSentinelIndex
  | RaftLogEntryUsesGenesisTerm RaftLogIndexDto
  | RaftLogEntriesNotStrictlyAscending RaftLogIndexDto RaftLogIndexDto
  | RaftAppendTermIsZero
  | RaftAppendPreviousIndexTermShapeMismatch RaftLogIndexDto RaftTermDto
  | RaftAppendPreviousTermAfterRequestTerm RaftTermDto RaftTermDto
  | RaftAppendEntriesNotContiguous RaftLogIndexDto RaftLogIndexDto
  | RaftAppendEntryTermAfterRequestTerm RaftLogIndexDto RaftTermDto RaftTermDto
  | RaftAppendEntryTermRegression RaftLogIndexDto RaftTermDto RaftTermDto
  | RaftAppendLeaderCommitAfterSentPrefix RaftLogIndexDto RaftLogIndexDto
  | RaftRequestVoteTermIsZero
  | RaftRequestVoteLastIndexTermShapeMismatch RaftLogIndexDto RaftTermDto
  | RaftRequestVoteLastTermAfterRequestTerm RaftTermDto RaftTermDto
  | RaftRequestVoteResponseTermIsZero
  | RaftAppendResponseAppliedAfterMatch RaftLogIndexDto RaftLogIndexDto
  | RaftRejectedAppendHasAppliedPrefix
  | RaftAppendResponseTermIsZero
  | RaftConflictFirstIndexIsZero
  | RaftConflictTermIsZero
  | RaftConflictTermAfterResponseTerm RaftTermDto RaftTermDto
  | RaftSuccessfulAppendHasConflictHint
  | RaftRejectedAppendMissingConflictHint
  | RaftRejectedAppendMatchIndexMismatch RaftLogIndexDto RaftLogIndexDto
  | RaftConflictTermMissingFirstIndex
  | RaftCheckpointIndexIsZero
  | RaftCheckpointTermIsZero
  | RaftCheckpointConfigurationReferenceInvalid
  | RaftSnapshotTermIsZero
  | RaftSnapshotTermBeforeCheckpoint RaftTermDto RaftTermDto
  deriving stock (Eq, Show)

-- | A self-contained native configuration succession contradiction.
data RaftConfigurationSequenceProblem
  = RaftExpectedJointConfiguration
  | RaftExpectedStableConfiguration
  | RaftJointOldSetMismatch
  | RaftFinalVoterSetMismatch
  | RaftUnchangedConfiguration
  deriving stock (Eq, Show)

-- | Candidate-lane identity and immutable genesis claim exchanged symmetrically.
data RaftHelloClaim = RaftHelloClaim
  { helloGenesisDigest :: RaftGenesisDigestDto,
    helloSource :: RaftNodeIdDto,
    helloTarget :: RaftNodeIdDto
  }
  deriving stock (Eq, Show)

raftHelloClaim ::
  RaftGenesisDigestDto ->
  RaftNodeIdDto ->
  RaftNodeIdDto ->
  Either RaftShapeError RaftHelloClaim
raftHelloClaim genesisDigest source target
  | source == target = Left (RaftHelloEndpointsEqual source)
  | otherwise = Right (RaftHelloClaim genesisDigest source target)

raftHelloGenesisDigest :: RaftHelloClaim -> RaftGenesisDigestDto
raftHelloGenesisDigest (RaftHelloClaim genesisDigest _ _) =
  genesisDigest

raftHelloSource :: RaftHelloClaim -> RaftNodeIdDto
raftHelloSource (RaftHelloClaim _ source _) = source

raftHelloTarget :: RaftHelloClaim -> RaftNodeIdDto
raftHelloTarget (RaftHelloClaim _ _ target) = target

instance Binary RaftHelloClaim where
  put hello = do
    put (raftHelloGenesisDigest hello)
    put (raftHelloSource hello)
    put (raftHelloTarget hello)
  get = do
    genesisDigest <- get
    source <- get
    target <- get
    either (fail . show) pure (raftHelloClaim genesisDigest source target)

-- | Canonical configuration shape. Both voter lists are nonempty and strictly
-- ordered; the two sets remain separate even where their membership overlaps.
data RaftVotingConfigurationDto
  = StableRaftConfigurationDto [RaftNodeIdDto]
  | JointRaftConfigurationDto [RaftNodeIdDto] [RaftNodeIdDto]
  deriving stock (Eq, Show)

data RaftVotingConfigurationDtoView
  = StableRaftConfigurationDtoView [RaftNodeIdDto]
  | JointRaftConfigurationDtoView [RaftNodeIdDto] [RaftNodeIdDto]
  deriving stock (Eq, Show)

stableRaftConfigurationDto :: [RaftNodeIdDto] -> Either RaftShapeError RaftVotingConfigurationDto
stableRaftConfigurationDto voters = checkVoters voters >> pure (StableRaftConfigurationDto voters)

jointRaftConfigurationDto :: [RaftNodeIdDto] -> [RaftNodeIdDto] -> Either RaftShapeError RaftVotingConfigurationDto
jointRaftConfigurationDto old new = checkVoters old >> checkVoters new >> pure (JointRaftConfigurationDto old new)

raftVotingConfigurationDtoView :: RaftVotingConfigurationDto -> RaftVotingConfigurationDtoView
raftVotingConfigurationDtoView = \case
  StableRaftConfigurationDto voters -> StableRaftConfigurationDtoView voters
  JointRaftConfigurationDto old new -> JointRaftConfigurationDtoView old new

checkVoters :: [RaftNodeIdDto] -> Either RaftShapeError ()
checkVoters [] = Left RaftVotingConfigurationEmpty
checkVoters voters = mapM_ checkPair (zip voters (drop 1 voters))
  where
    checkPair (left, right) = unless (left < right) (Left (RaftVotersNotStrictlyAscending left right))

instance Binary RaftVotingConfigurationDto where
  put = \case
    StableRaftConfigurationDto voters -> putWord8 0 >> put voters
    JointRaftConfigurationDto old new -> putWord8 1 >> put old >> put new
  get =
    getWord8 >>= \case
      0 -> get >>= either (fail . show) pure . stableRaftConfigurationDto
      1 -> do
        old <- get
        new <- get
        either (fail . show) pure (jointRaftConfigurationDto old new)
      _ -> fail "unknown Raft voting configuration tag"

-- | The complete current Raft log-entry payload vocabulary.
data RaftLogEntryPayloadDto
  = LeaderNoOpDto
  | ApplicationBytesDto ByteString
  | ConfigurationDto RaftVotingConfigurationDto ByteString
  deriving stock (Eq, Show)

instance Binary RaftLogEntryPayloadDto where
  put LeaderNoOpDto = putWord8 0
  put (ApplicationBytesDto bytes) = putWord8 1 >> put bytes
  put (ConfigurationDto configuration metadata) = putWord8 2 >> put configuration >> put metadata
  get =
    getWord8 >>= \case
      0 -> pure LeaderNoOpDto
      1 -> ApplicationBytesDto <$> get
      2 -> ConfigurationDto <$> get <*> get
      _ -> fail "unknown Raft log-entry payload tag"

-- | One non-sentinel transmitted log entry.
data RaftLogEntryDto = RaftLogEntryDto
  { logEntryIndex :: RaftLogIndexDto,
    logEntryTerm :: RaftTermDto,
    logEntryPayload :: RaftLogEntryPayloadDto
  }
  deriving stock (Eq, Show)

raftLogEntryDto ::
  RaftLogIndexDto ->
  RaftTermDto ->
  RaftLogEntryPayloadDto ->
  Either RaftShapeError RaftLogEntryDto
raftLogEntryDto index term payload
  | raftLogIndexDtoWord64 index == 0 = Left RaftLogEntryUsesSentinelIndex
  | raftTermDtoWord64 term == 0 = Left (RaftLogEntryUsesGenesisTerm index)
  | otherwise = Right (RaftLogEntryDto index term payload)

raftLogEntryIndex :: RaftLogEntryDto -> RaftLogIndexDto
raftLogEntryIndex (RaftLogEntryDto index _ _) = index

raftLogEntryTerm :: RaftLogEntryDto -> RaftTermDto
raftLogEntryTerm (RaftLogEntryDto _ term _) = term

raftLogEntryPayload :: RaftLogEntryDto -> RaftLogEntryPayloadDto
raftLogEntryPayload (RaftLogEntryDto _ _ payload) = payload

instance Binary RaftLogEntryDto where
  put entry = do
    put (raftLogEntryIndex entry)
    put (raftLogEntryTerm entry)
    put (raftLogEntryPayload entry)
  get = do
    index <- get
    term <- get
    payload <- get
    either (fail . show) pure (raftLogEntryDto index term payload)

-- | Coherent failed-append repair information.
--
-- A missing suffix has no conflicting term.  A term conflict always names its
-- first index.  Success carries no hint at all.
data RaftConflictHintDto
  = MissingSuffixFromDto RaftLogIndexDto
  | ConflictingTermFromDto RaftTermDto RaftLogIndexDto
  deriving stock (Eq, Show)

-- | The configuration reference is absent only for immutable genesis. The
-- payload is the actual application checkpoint, never a replay of old entries.
data RaftCheckpointDto = RaftCheckpointDto RaftLogIndexDto RaftTermDto (Maybe (RaftLogIndexDto, RaftTermDto)) RaftVotingConfigurationDto ByteString
  deriving stock (Eq, Show)

raftCheckpointDto :: RaftLogIndexDto -> RaftTermDto -> Maybe (RaftLogIndexDto, RaftTermDto) -> RaftVotingConfigurationDto -> ByteString -> Either RaftShapeError RaftCheckpointDto
raftCheckpointDto index term reference configuration payload
  | raftLogIndexDtoWord64 index == 0 = Left RaftCheckpointIndexIsZero
  | raftTermDtoWord64 term == 0 = Left RaftCheckpointTermIsZero
  | Just (configurationIndex, configurationTerm) <- reference,
    raftLogIndexDtoWord64 configurationIndex == 0 || raftTermDtoWord64 configurationTerm == 0 || configurationIndex > index || configurationTerm > term =
      Left RaftCheckpointConfigurationReferenceInvalid
  | otherwise = Right (RaftCheckpointDto index term reference configuration payload)

raftCheckpointDtoFields :: RaftCheckpointDto -> (RaftLogIndexDto, RaftTermDto, Maybe (RaftLogIndexDto, RaftTermDto), RaftVotingConfigurationDto, ByteString)
raftCheckpointDtoFields (RaftCheckpointDto index term reference configuration payload) = (index, term, reference, configuration, payload)

instance Binary RaftCheckpointDto where
  put (RaftCheckpointDto index term reference configuration payload) = put index >> put term >> put reference >> put configuration >> put payload
  get = do
    index <- get
    term <- get
    reference <- get
    configuration <- get
    payload <- get
    either (fail . show) pure (raftCheckpointDto index term reference configuration payload)

data RaftRpcDto
  = RequestVoteDto
      RaftTermDto
      RaftNodeIdDto
      RaftLogIndexDto
      RaftTermDto
  | RequestVoteResponseDto
      RaftTermDto
      RaftNodeIdDto
      Bool
  | AppendEntriesDto
      RaftTermDto
      RaftNodeIdDto
      RaftLogIndexDto
      RaftTermDto
      [RaftLogEntryDto]
      RaftLogIndexDto
  | AppendEntriesResponseDto
      RaftTermDto
      RaftNodeIdDto
      Bool
      RaftLogIndexDto
      RaftLogIndexDto
      (Maybe RaftConflictHintDto)
  | InstallSnapshotDto RaftTermDto RaftNodeIdDto RaftCheckpointDto
  | InstallSnapshotResponseDto RaftTermDto RaftNodeIdDto RaftLogIndexDto Bool
  deriving stock (Eq, Show)

-- | Exhaustive discriminator for the current RPC constructors.
data RaftRpcKind
  = RequestVoteKind
  | RequestVoteResponseKind
  | AppendEntriesKind
  | AppendEntriesResponseKind
  | InstallSnapshotKind
  | InstallSnapshotResponseKind
  deriving stock (Bounded, Enum, Eq, Show)

requestVoteDto ::
  RaftTermDto ->
  RaftNodeIdDto ->
  RaftLogIndexDto ->
  RaftTermDto ->
  Either RaftShapeError RaftRpcDto
requestVoteDto term candidate lastIndex lastTerm
  | raftTermDtoWord64 term == 0 = Left RaftRequestVoteTermIsZero
  | not (coherentIndexTerm lastIndex lastTerm) =
      Left (RaftRequestVoteLastIndexTermShapeMismatch lastIndex lastTerm)
  | lastTerm > term = Left (RaftRequestVoteLastTermAfterRequestTerm lastTerm term)
  | otherwise = Right (RequestVoteDto term candidate lastIndex lastTerm)

requestVoteResponseDto ::
  RaftTermDto ->
  RaftNodeIdDto ->
  Bool ->
  Either RaftShapeError RaftRpcDto
requestVoteResponseDto term voter granted
  | raftTermDtoWord64 term == 0 = Left RaftRequestVoteResponseTermIsZero
  | otherwise = Right (RequestVoteResponseDto term voter granted)

appendEntriesDto ::
  RaftTermDto ->
  RaftNodeIdDto ->
  RaftLogIndexDto ->
  RaftTermDto ->
  [RaftLogEntryDto] ->
  RaftLogIndexDto ->
  Either RaftShapeError RaftRpcDto
appendEntriesDto term leader previousIndex previousTerm entries leaderCommit = do
  checkAscendingEntries entries
  checkAppendEntriesShape term previousIndex previousTerm entries leaderCommit
  pure
    ( AppendEntriesDto
        term
        leader
        previousIndex
        previousTerm
        entries
        leaderCommit
    )

appendEntriesResponseDto ::
  RaftTermDto ->
  RaftNodeIdDto ->
  Bool ->
  RaftLogIndexDto ->
  RaftLogIndexDto ->
  Maybe RaftConflictHintDto ->
  Either RaftShapeError RaftRpcDto
appendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint
  | raftTermDtoWord64 term == 0 = Left RaftAppendResponseTermIsZero
  | success && appliedIndex > matchIndex = Left (RaftAppendResponseAppliedAfterMatch appliedIndex matchIndex)
  | not success && raftLogIndexDtoWord64 appliedIndex /= 0 = Left RaftRejectedAppendHasAppliedPrefix
  | success && isJust conflictHint = Left RaftSuccessfulAppendHasConflictHint
  | not success && not (isJust conflictHint) =
      Left RaftRejectedAppendMissingConflictHint
  | not success,
    Just hint <- conflictHint,
    raftLogIndexDtoWord64 (conflictFirstIndex hint) == 0 =
      Left RaftConflictFirstIndexIsZero
  | not success,
    Just (ConflictingTermFromDto conflictTerm _) <- conflictHint,
    raftTermDtoWord64 conflictTerm == 0 =
      Left RaftConflictTermIsZero
  | not success,
    Just (ConflictingTermFromDto conflictTerm _) <- conflictHint,
    conflictTerm > term =
      Left (RaftConflictTermAfterResponseTerm conflictTerm term)
  | not success,
    Just hint <- conflictHint,
    matchIndex /= rejectedMatchIndex hint =
      Left
        ( RaftRejectedAppendMatchIndexMismatch
            (rejectedMatchIndex hint)
            matchIndex
        )
  | otherwise =
      Right
        (AppendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint)

installSnapshotDto :: RaftTermDto -> RaftNodeIdDto -> RaftCheckpointDto -> Either RaftShapeError RaftRpcDto
installSnapshotDto term leader checkpoint@(RaftCheckpointDto _ checkpointTerm _ _ _)
  | raftTermDtoWord64 term == 0 = Left RaftSnapshotTermIsZero
  | checkpointTerm > term = Left (RaftSnapshotTermBeforeCheckpoint term checkpointTerm)
  | otherwise = Right (InstallSnapshotDto term leader checkpoint)

installSnapshotResponseDto :: RaftTermDto -> RaftNodeIdDto -> RaftLogIndexDto -> Bool -> Either RaftShapeError RaftRpcDto
installSnapshotResponseDto term voter index installed
  | raftTermDtoWord64 term == 0 = Left RaftSnapshotTermIsZero
  | raftLogIndexDtoWord64 index == 0 = Left RaftCheckpointIndexIsZero
  | otherwise = Right (InstallSnapshotResponseDto term voter index installed)

rejectedMatchIndex :: RaftConflictHintDto -> RaftLogIndexDto
rejectedMatchIndex = \case
  MissingSuffixFromDto firstIndex -> predecessor firstIndex
  ConflictingTermFromDto _ firstIndex -> predecessor firstIndex
  where
    predecessor firstIndex =
      raftLogIndexDto
        ( if raftLogIndexDtoWord64 firstIndex == 0
            then 0
            else raftLogIndexDtoWord64 firstIndex - 1
        )

conflictFirstIndex :: RaftConflictHintDto -> RaftLogIndexDto
conflictFirstIndex = \case
  MissingSuffixFromDto firstIndex -> firstIndex
  ConflictingTermFromDto _ firstIndex -> firstIndex

raftRpcKind :: RaftRpcDto -> RaftRpcKind
raftRpcKind = \case
  RequestVoteDto {} -> RequestVoteKind
  RequestVoteResponseDto {} -> RequestVoteResponseKind
  AppendEntriesDto {} -> AppendEntriesKind
  AppendEntriesResponseDto {} -> AppendEntriesResponseKind
  InstallSnapshotDto {} -> InstallSnapshotKind
  InstallSnapshotResponseDto {} -> InstallSnapshotResponseKind

raftRpcTerm :: RaftRpcDto -> RaftTermDto
raftRpcTerm = \case
  RequestVoteDto term _ _ _ -> term
  RequestVoteResponseDto term _ _ -> term
  AppendEntriesDto term _ _ _ _ _ -> term
  AppendEntriesResponseDto term _ _ _ _ _ -> term
  InstallSnapshotDto term _ _ -> term
  InstallSnapshotResponseDto term _ _ _ -> term

-- | The candidate, voter, or leader identity claimed as the RPC source.
raftRpcSource :: RaftRpcDto -> RaftNodeIdDto
raftRpcSource = \case
  RequestVoteDto _ candidate _ _ -> candidate
  RequestVoteResponseDto _ voter _ -> voter
  AppendEntriesDto _ leader _ _ _ _ -> leader
  AppendEntriesResponseDto _ voter _ _ _ _ -> voter
  InstallSnapshotDto _ leader _ -> leader
  InstallSnapshotResponseDto _ voter _ _ -> voter

raftRequestVoteFields ::
  RaftRpcDto ->
  Maybe (RaftTermDto, RaftNodeIdDto, RaftLogIndexDto, RaftTermDto)
raftRequestVoteFields = \case
  RequestVoteDto term candidate lastIndex lastTerm ->
    Just (term, candidate, lastIndex, lastTerm)
  _ -> Nothing

raftRequestVoteResponseFields ::
  RaftRpcDto ->
  Maybe (RaftTermDto, RaftNodeIdDto, Bool)
raftRequestVoteResponseFields = \case
  RequestVoteResponseDto term voter granted -> Just (term, voter, granted)
  _ -> Nothing

raftAppendEntriesFields ::
  RaftRpcDto ->
  Maybe
    ( RaftTermDto,
      RaftNodeIdDto,
      RaftLogIndexDto,
      RaftTermDto,
      [RaftLogEntryDto],
      RaftLogIndexDto
    )
raftAppendEntriesFields = \case
  AppendEntriesDto term leader previousIndex previousTerm entries leaderCommit ->
    Just
      ( term,
        leader,
        previousIndex,
        previousTerm,
        entries,
        leaderCommit
      )
  _ -> Nothing

raftAppendEntriesResponseFields ::
  RaftRpcDto ->
  Maybe
    ( RaftTermDto,
      RaftNodeIdDto,
      Bool,
      RaftLogIndexDto,
      RaftLogIndexDto,
      Maybe RaftConflictHintDto
    )
raftAppendEntriesResponseFields = \case
  AppendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint ->
    Just (term, voter, success, matchIndex, appliedIndex, conflictHint)
  _ -> Nothing

raftInstallSnapshotFields :: RaftRpcDto -> Maybe (RaftTermDto, RaftNodeIdDto, RaftCheckpointDto)
raftInstallSnapshotFields = \case
  InstallSnapshotDto term leader checkpoint -> Just (term, leader, checkpoint)
  _ -> Nothing

raftInstallSnapshotResponseFields :: RaftRpcDto -> Maybe (RaftTermDto, RaftNodeIdDto, RaftLogIndexDto, Bool)
raftInstallSnapshotResponseFields = \case
  InstallSnapshotResponseDto term voter index installed -> Just (term, voter, index, installed)
  _ -> Nothing

instance Binary RaftRpcDto where
  put = \case
    RequestVoteDto term candidate lastIndex lastTerm -> do
      putWord8 0
      put term
      put candidate
      put lastIndex
      put lastTerm
    RequestVoteResponseDto term voter granted -> do
      putWord8 1
      put term
      put voter
      put granted
    AppendEntriesDto term leader previousIndex previousTerm entries leaderCommit -> do
      putWord8 2
      put term
      put leader
      put previousIndex
      put previousTerm
      put entries
      put leaderCommit
    AppendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint -> do
      putWord8 3
      put term
      put voter
      put success
      put matchIndex
      put appliedIndex
      putConflictHint conflictHint
    InstallSnapshotDto term leader checkpoint -> putWord8 4 >> put term >> put leader >> put checkpoint
    InstallSnapshotResponseDto term voter index installed -> putWord8 5 >> put term >> put voter >> put index >> put installed
  get =
    getWord8 >>= \case
      0 -> do
        term <- get
        candidate <- get
        lastIndex <- get
        lastTerm <- get
        either
          (fail . show)
          pure
          (requestVoteDto term candidate lastIndex lastTerm)
      1 -> do
        term <- get
        voter <- get
        granted <- get
        either
          (fail . show)
          pure
          (requestVoteResponseDto term voter granted)
      2 -> do
        term <- get
        leader <- get
        previousIndex <- get
        previousTerm <- get
        entries <- get
        leaderCommit <- get
        either
          (fail . show)
          pure
          ( appendEntriesDto
              term
              leader
              previousIndex
              previousTerm
              entries
              leaderCommit
          )
      3 -> do
        term <- get
        voter <- get
        success <- get
        matchIndex <- get
        appliedIndex <- get
        conflictHint <- getConflictHint
        either
          (fail . show)
          pure
          (appendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint)
      4 -> do
        term <- get
        leader <- get
        checkpoint <- get
        either (fail . show) pure (installSnapshotDto term leader checkpoint)
      5 -> do
        term <- get
        voter <- get
        index <- get
        installed <- get
        either (fail . show) pure (installSnapshotResponseDto term voter index installed)
      _ -> fail "unknown Raft RPC tag"

putConflictHint :: Maybe RaftConflictHintDto -> Put
putConflictHint Nothing =
  put (Nothing :: Maybe RaftTermDto)
    >> put (Nothing :: Maybe RaftLogIndexDto)
putConflictHint (Just (MissingSuffixFromDto firstIndex)) =
  put (Nothing :: Maybe RaftTermDto) >> put (Just firstIndex)
putConflictHint (Just (ConflictingTermFromDto conflictTerm firstIndex)) =
  put (Just conflictTerm) >> put (Just firstIndex)

getConflictHint :: Get (Maybe RaftConflictHintDto)
getConflictHint = do
  conflictTerm <- get
  firstIndexClaim <- get
  case (conflictTerm, firstIndexClaim) of
    (Nothing, Nothing) -> pure Nothing
    (Nothing, Just firstIndex) -> pure (Just (MissingSuffixFromDto firstIndex))
    (Just term, Just firstIndex) ->
      pure (Just (ConflictingTermFromDto term firstIndex))
    (Just _, Nothing) -> fail (show RaftConflictTermMissingFirstIndex)

checkAscendingEntries :: [RaftLogEntryDto] -> Either RaftShapeError ()
checkAscendingEntries entries =
  mapM_ checkPair (zip entries (drop 1 entries))
  where
    checkPair (left, right) =
      unless
        (raftLogEntryIndex left < raftLogEntryIndex right)
        ( Left
            ( RaftLogEntriesNotStrictlyAscending
                (raftLogEntryIndex left)
                (raftLogEntryIndex right)
            )
        )

checkAppendEntriesShape ::
  RaftTermDto ->
  RaftLogIndexDto ->
  RaftTermDto ->
  [RaftLogEntryDto] ->
  RaftLogIndexDto ->
  Either RaftShapeError ()
checkAppendEntriesShape requestTerm previousIndex previousTerm entries leaderCommit = do
  if raftTermDtoWord64 requestTerm == 0
    then Left RaftAppendTermIsZero
    else Right ()
  if coherentIndexTerm previousIndex previousTerm
    then Right ()
    else Left (RaftAppendPreviousIndexTermShapeMismatch previousIndex previousTerm)
  if previousTerm <= requestTerm
    then Right ()
    else Left (RaftAppendPreviousTermAfterRequestTerm previousTerm requestTerm)
  checkEntries
    requestTerm
    (nextIndex previousIndex)
    previousTerm
    entries
  checkConfigurationSequence previousIndex entries
  let lastSentIndex = case reverse entries of
        [] -> previousIndex
        entry : _ -> raftLogEntryIndex entry
  if leaderCommit <= lastSentIndex
    then Right ()
    else Left (RaftAppendLeaderCommitAfterSentPrefix leaderCommit lastSentIndex)

-- The omitted prefix may contain an earlier configuration. Validate only what
-- this request establishes itself; matching that omitted prefix is native work.
checkConfigurationSequence :: RaftLogIndexDto -> [RaftLogEntryDto] -> Either RaftShapeError ()
checkConfigurationSequence previousIndex = go Nothing
  where
    go _ [] = Right ()
    go preceding (entry : remaining) = case raftLogEntryPayload entry of
      ConfigurationDto configuration _ -> do
        let reject = Left . RaftAppendConfigurationSequenceInvalid (raftLogEntryIndex entry)
        case (preceding, configuration) of
          (Nothing, StableRaftConfigurationDto _) | previousIndex == raftLogIndexDto 0 -> reject RaftExpectedJointConfiguration
          (Nothing, JointRaftConfigurationDto old new) | old == new -> reject RaftUnchangedConfiguration
          (Just (StableRaftConfigurationDto old), JointRaftConfigurationDto expected new)
            | old /= expected -> reject RaftJointOldSetMismatch
            | old == new -> reject RaftUnchangedConfiguration
          (Just (JointRaftConfigurationDto _ new), StableRaftConfigurationDto final)
            | new /= final -> reject RaftFinalVoterSetMismatch
          (Just StableRaftConfigurationDto {}, StableRaftConfigurationDto {}) -> reject RaftExpectedJointConfiguration
          (Just JointRaftConfigurationDto {}, JointRaftConfigurationDto {}) -> reject RaftExpectedStableConfiguration
          _ -> Right ()
        go (Just configuration) remaining
      _ -> go preceding remaining

checkEntries ::
  RaftTermDto ->
  RaftLogIndexDto ->
  RaftTermDto ->
  [RaftLogEntryDto] ->
  Either RaftShapeError ()
checkEntries _ _ _ [] = Right ()
checkEntries requestTerm expectedIndex precedingTerm (entry : entries)
  | raftLogEntryIndex entry /= expectedIndex =
      Left
        ( RaftAppendEntriesNotContiguous
            expectedIndex
            (raftLogEntryIndex entry)
        )
  | raftLogEntryTerm entry > requestTerm =
      Left
        ( RaftAppendEntryTermAfterRequestTerm
            (raftLogEntryIndex entry)
            (raftLogEntryTerm entry)
            requestTerm
        )
  | raftLogEntryTerm entry < precedingTerm =
      Left
        ( RaftAppendEntryTermRegression
            (raftLogEntryIndex entry)
            precedingTerm
            (raftLogEntryTerm entry)
        )
  | otherwise =
      checkEntries
        requestTerm
        (nextIndex expectedIndex)
        (raftLogEntryTerm entry)
        entries

nextIndex :: RaftLogIndexDto -> RaftLogIndexDto
nextIndex = raftLogIndexDto . (+ 1) . raftLogIndexDtoWord64

coherentIndexTerm :: RaftLogIndexDto -> RaftTermDto -> Bool
coherentIndexTerm index term =
  (raftLogIndexDtoWord64 index == 0)
    == (raftTermDtoWord64 term == 0)

-- | The complete symmetric current-build ERFT envelope.
data RaftProtocolEnvelope
  = RaftHello RaftHelloClaim
  | RaftRpc RaftRpcDto
  deriving stock (Eq, Show)

instance Binary RaftProtocolEnvelope where
  put = \case
    RaftHello hello -> putWord8 0 >> put hello
    RaftRpc rpc -> putWord8 1 >> put rpc
  get =
    getWord8 >>= \case
      0 -> RaftHello <$> get
      1 -> RaftRpc <$> get
      _ -> fail "unknown Raft envelope tag"

-- | Immutable out-of-band facts for one registered replica connection. The
-- source may be a learner, lagging voter, or historical transition participant.
-- This context never supplies native voting eligibility.
data RaftConnectionContext = RaftConnectionContext
  { connectionGenesisDigest :: RaftGenesisDigestDto,
    connectionLocalNode :: RaftNodeIdDto,
    connectionRemoteNode :: RaftNodeIdDto
  }
  deriving stock (Eq, Show)

raftConnectionContext ::
  RaftGenesisDigestDto ->
  RaftNodeIdDto ->
  RaftNodeIdDto ->
  Either RaftShapeError RaftConnectionContext
raftConnectionContext expectedDigest localNode remoteNode
  | localNode == remoteNode = Left (RaftHelloEndpointsEqual localNode)
  | otherwise =
      Right
        RaftConnectionContext
          { connectionGenesisDigest = expectedDigest,
            connectionLocalNode = localNode,
            connectionRemoteNode = remoteNode
          }

raftConnectionGenesisDigest :: RaftConnectionContext -> RaftGenesisDigestDto
raftConnectionGenesisDigest (RaftConnectionContext expectedDigest _ _) =
  expectedDigest

raftConnectionLocalNode :: RaftConnectionContext -> RaftNodeIdDto
raftConnectionLocalNode (RaftConnectionContext _ localNode _) = localNode

raftConnectionRemoteNode :: RaftConnectionContext -> RaftNodeIdDto
raftConnectionRemoteNode (RaftConnectionContext _ _ remoteNode) = remoteNode

-- | The only two legal admission phases on a symmetric connection.
data RaftConnectionPhase
  = AwaitingRaftHello
  | EstablishedRaftBinding
  deriving stock (Eq, Show)

-- | A structurally decoded ERFT envelope contradicts its physical role.
data RaftAdmissionError
  = RaftHelloExpected
  | RaftRpcExpected
  | RaftHelloGenesisDigestMismatch
  | RaftHelloSourceMismatch RaftNodeIdDto RaftNodeIdDto
  | RaftHelloTargetMismatch RaftNodeIdDto RaftNodeIdDto
  | RaftRpcSourceMismatch RaftNodeIdDto RaftNodeIdDto
  deriving stock (Eq, Show)

-- | Validate a candidate-lane hello against immutable registered context.
admitRaftHello ::
  RaftConnectionContext ->
  RaftHelloClaim ->
  Either RaftAdmissionError RaftHelloClaim
admitRaftHello context hello
  | raftHelloGenesisDigest hello /= raftConnectionGenesisDigest context =
      Left RaftHelloGenesisDigestMismatch
  | raftHelloSource hello /= raftConnectionRemoteNode context =
      Left
        ( RaftHelloSourceMismatch
            (raftConnectionRemoteNode context)
            (raftHelloSource hello)
        )
  | raftHelloTarget hello /= raftConnectionLocalNode context =
      Left
        ( RaftHelloTargetMismatch
            (raftConnectionLocalNode context)
            (raftHelloTarget hello)
        )
  | otherwise = Right hello

-- | Validate the candidate/leader/voter role claim on an established binding.
admitRaftRpc ::
  RaftConnectionContext ->
  RaftRpcDto ->
  Either RaftAdmissionError RaftRpcDto
admitRaftRpc context rpc
  | raftRpcSource rpc == raftConnectionRemoteNode context = Right rpc
  | otherwise =
      Left
        ( RaftRpcSourceMismatch
            (raftConnectionRemoteNode context)
            (raftRpcSource rpc)
        )

-- | Enforce hello-before-RPC phase and out-of-band source/target context.
admitRaftEnvelope ::
  RaftConnectionPhase ->
  RaftConnectionContext ->
  RaftProtocolEnvelope ->
  Either RaftAdmissionError RaftProtocolEnvelope
admitRaftEnvelope AwaitingRaftHello context (RaftHello hello) =
  RaftHello <$> admitRaftHello context hello
admitRaftEnvelope AwaitingRaftHello _ (RaftRpc _) = Left RaftHelloExpected
admitRaftEnvelope EstablishedRaftBinding _ (RaftHello _) = Left RaftRpcExpected
admitRaftEnvelope EstablishedRaftBinding context (RaftRpc rpc) =
  RaftRpc <$> admitRaftRpc context rpc

-- | A DTO was structurally admitted but could not be converted through the
-- public checked Raft-core boundary, or an allegedly checked core RPC could not
-- be projected back into the current DTO shape.
data RaftAdapterError
  = RaftAdapterIdentityFault CoreIdentity.RaftIdentityFault
  | RaftAdapterConfigurationFault CoreConfiguration.RaftConfigurationFault
  | RaftAdapterInputFault CoreInput.RaftInputFault
  | RaftAdapterClaimFault RaftClaimError
  | RaftAdapterShapeFault RaftShapeError
  | RaftAdapterCheckpointFault CoreCheckpoint.RaftCheckpointFault
  deriving stock (Eq, Show)

-- | Project one checked core RPC into protocol-owned DTOs.  This defines no
-- wire instance for the core value.
raftRpcDtoFromCore ::
  CoreInput.RaftRpc ByteString ->
  Either RaftAdapterError RaftRpcDto
raftRpcDtoFromCore rpc = case CoreInput.raftRpcView rpc of
  CoreInput.RequestVoteView term candidate lastIndex lastTerm -> do
    candidateDto <- coreNodeDto candidate
    mapShape
      ( requestVoteDto
          (raftTermDtoFromCore term)
          candidateDto
          (raftLogIndexDtoFromCore lastIndex)
          (raftTermDtoFromCore lastTerm)
      )
  CoreInput.RequestVoteResponseView term voter granted -> do
    voterDto <- coreNodeDto voter
    mapShape
      ( requestVoteResponseDto
          (raftTermDtoFromCore term)
          voterDto
          granted
      )
  CoreInput.AppendEntriesView term leader previousIndex previousTerm entries leaderCommit -> do
    leaderDto <- coreNodeDto leader
    entryDtos <- traverse coreLogEntryDto entries
    either
      (Left . RaftAdapterShapeFault)
      Right
      ( appendEntriesDto
          (raftTermDtoFromCore term)
          leaderDto
          (raftLogIndexDtoFromCore previousIndex)
          (raftTermDtoFromCore previousTerm)
          entryDtos
          (raftLogIndexDtoFromCore leaderCommit)
      )
  CoreInput.AppendEntriesResponseView term voter result -> do
    voterDto <- coreNodeDto voter
    case result of
      CoreInput.AppendAccepted matchIndex appliedIndex ->
        mapShape
          ( appendEntriesResponseDto
              (raftTermDtoFromCore term)
              voterDto
              True
              (raftLogIndexDtoFromCore matchIndex)
              (raftLogIndexDtoFromCore appliedIndex)
              Nothing
          )
      CoreInput.AppendRejected hint -> do
        hintDto <- coreConflictHintDto hint
        mapShape
          ( appendEntriesResponseDto
              (raftTermDtoFromCore term)
              voterDto
              False
              (rejectedMatchIndex hintDto)
              (raftLogIndexDto 0)
              (Just hintDto)
          )
  CoreInput.InstallSnapshotView term leader checkpoint -> do
    leaderDto <- coreNodeDto leader
    configuration <- raftVotingConfigurationDtoFromCore (CoreCheckpoint.raftCheckpointConfiguration checkpoint)
    let reference = case CoreConfiguration.raftConfigurationRefView (CoreCheckpoint.raftCheckpointConfigurationRef checkpoint) of
          CoreConfiguration.GenesisRaftConfigurationRefView -> Nothing
          CoreConfiguration.RaftConfigurationEntryRefView index configurationTerm -> Just (raftLogIndexDtoFromCore index, raftTermDtoFromCore configurationTerm)
    snapshot <- mapShape (raftCheckpointDto (raftLogIndexDtoFromCore (CoreCheckpoint.raftCheckpointIndex checkpoint)) (raftTermDtoFromCore (CoreCheckpoint.raftCheckpointTerm checkpoint)) reference configuration (CoreCheckpoint.raftCheckpointPayload checkpoint))
    mapShape (installSnapshotDto (raftTermDtoFromCore term) leaderDto snapshot)
  CoreInput.InstallSnapshotResponseView term voter index installed -> do
    voterDto <- coreNodeDto voter
    mapShape (installSnapshotResponseDto (raftTermDtoFromCore term) voterDto (raftLogIndexDtoFromCore index) installed)

-- | Validate one protocol RPC through the core's public smart constructors.
raftRpcDtoToCore ::
  RaftRpcDto ->
  Either RaftAdapterError (CoreInput.RaftRpc ByteString)
raftRpcDtoToCore dto = case dto of
  RequestVoteDto term candidate lastIndex lastTerm -> do
    candidateCore <- dtoNodeCore candidate
    mapInput
      ( CoreInput.requestVote
          (raftTermDtoToCore term)
          candidateCore
          (raftLogIndexDtoToCore lastIndex)
          (raftTermDtoToCore lastTerm)
      )
  RequestVoteResponseDto term voter granted -> do
    voterCore <- dtoNodeCore voter
    mapInput
      ( CoreInput.requestVoteResponse
          (raftTermDtoToCore term)
          voterCore
          granted
      )
  AppendEntriesDto term leader previousIndex previousTerm entries leaderCommit -> do
    leaderCore <- dtoNodeCore leader
    coreEntries <- traverse dtoLogEntryCore entries
    mapInput
      ( CoreInput.appendEntries
          (raftTermDtoToCore term)
          leaderCore
          (raftLogIndexDtoToCore previousIndex)
          (raftTermDtoToCore previousTerm)
          coreEntries
          (raftLogIndexDtoToCore leaderCommit)
      )
  AppendEntriesResponseDto term voter success matchIndex appliedIndex conflictHint -> do
    voterCore <- dtoNodeCore voter
    result <-
      if success
        then pure (CoreInput.AppendAccepted (raftLogIndexDtoToCore matchIndex) (raftLogIndexDtoToCore appliedIndex))
        else case conflictHint of
          Nothing -> Left (RaftAdapterShapeFault RaftRejectedAppendMissingConflictHint)
          Just hint -> CoreInput.AppendRejected <$> dtoConflictHintCore hint
    mapInput
      ( CoreInput.appendEntriesResponse
          (raftTermDtoToCore term)
          voterCore
          result
      )
  InstallSnapshotDto term leader (RaftCheckpointDto index checkpointTerm reference configuration payload) -> do
    leaderCore <- dtoNodeCore leader
    configurationCore <- raftVotingConfigurationDtoToCore configuration
    referenceCore <- case reference of
      Nothing -> Right CoreConfiguration.genesisRaftConfigurationRef
      Just (configurationIndex, configurationTerm) -> either (Left . RaftAdapterConfigurationFault) Right (CoreConfiguration.raftConfigurationEntryRef (raftLogIndexDtoToCore configurationIndex) (raftTermDtoToCore configurationTerm))
    snapshot <- either (Left . RaftAdapterCheckpointFault) Right (CoreCheckpoint.raftCheckpoint (raftLogIndexDtoToCore index) (raftTermDtoToCore checkpointTerm) referenceCore configurationCore payload)
    mapInput (CoreInput.installSnapshot (raftTermDtoToCore term) leaderCore snapshot)
  InstallSnapshotResponseDto term voter index installed -> do
    voterCore <- dtoNodeCore voter
    mapInput (CoreInput.installSnapshotResponse (raftTermDtoToCore term) voterCore (raftLogIndexDtoToCore index) installed)

-- | Attach the established physical source and require a request constructor.
raftRequestDtoToCoreInput ::
  RaftNodeIdDto ->
  RaftRpcDto ->
  Either RaftAdapterError (CoreInput.RaftInput ByteString)
raftRequestDtoToCoreInput observedSource dto = do
  source <- dtoNodeCore observedSource
  rpc <- raftRpcDtoToCore dto
  mapInput (CoreInput.observeRaftRequest source rpc)

-- | Attach the established physical source and runtime-owned dispatch
-- generation and require a response constructor.
raftResponseDtoToCoreInput ::
  RaftNodeIdDto ->
  CoreIdentity.RaftDispatchGeneration ->
  RaftRpcDto ->
  Either RaftAdapterError (CoreInput.RaftInput ByteString)
raftResponseDtoToCoreInput observedSource generation dto = do
  source <- dtoNodeCore observedSource
  rpc <- raftRpcDtoToCore dto
  mapInput (CoreInput.observeRaftResponse source generation rpc)

coreNodeDto :: CoreIdentity.RaftNodeId -> Either RaftAdapterError RaftNodeIdDto
coreNodeDto = either (Left . RaftAdapterClaimFault) Right . raftNodeIdDtoFromCore

dtoNodeCore :: RaftNodeIdDto -> Either RaftAdapterError CoreIdentity.RaftNodeId
dtoNodeCore = either (Left . RaftAdapterIdentityFault) Right . raftNodeIdDtoToCore

raftVotingConfigurationDtoFromCore :: CoreConfiguration.RaftVotingConfiguration -> Either RaftAdapterError RaftVotingConfigurationDto
raftVotingConfigurationDtoFromCore configuration = case CoreConfiguration.raftVotingConfigurationView configuration of
  CoreConfiguration.StableRaftConfigurationView voters -> traverse coreNodeDto voters >>= mapShape . stableRaftConfigurationDto
  CoreConfiguration.JointRaftConfigurationView old new -> do
    oldDto <- traverse coreNodeDto old
    newDto <- traverse coreNodeDto new
    mapShape (jointRaftConfigurationDto oldDto newDto)

raftVotingConfigurationDtoToCore :: RaftVotingConfigurationDto -> Either RaftAdapterError CoreConfiguration.RaftVotingConfiguration
raftVotingConfigurationDtoToCore = \case
  StableRaftConfigurationDto voters -> CoreConfiguration.stableRaftConfiguration <$> voterSet voters
  JointRaftConfigurationDto old new -> CoreConfiguration.jointRaftConfiguration <$> voterSet old <*> voterSet new
  where
    voterSet voters = do
      nodes <- traverse dtoNodeCore voters
      either (Left . RaftAdapterConfigurationFault) Right (CoreConfiguration.raftVoterSet nodes)

coreLogEntryDto ::
  CoreInput.RaftLogEntry ByteString ->
  Either RaftAdapterError RaftLogEntryDto
coreLogEntryDto entry = do
  payload <- case CoreInput.raftLogEntryPayload entry of
    CoreInput.LeaderNoOp -> pure LeaderNoOpDto
    CoreInput.Application bytes -> pure (ApplicationBytesDto bytes)
    CoreInput.Configuration configuration metadata -> ConfigurationDto <$> raftVotingConfigurationDtoFromCore configuration <*> pure metadata
  mapShape
    ( raftLogEntryDto
        (raftLogIndexDtoFromCore (CoreInput.raftLogEntryIndex entry))
        (raftTermDtoFromCore (CoreInput.raftLogEntryTerm entry))
        payload
    )

dtoLogEntryCore :: RaftLogEntryDto -> Either RaftAdapterError (CoreInput.RaftLogEntry ByteString)
dtoLogEntryCore entry = do
  payload <- case raftLogEntryPayload entry of
    LeaderNoOpDto -> pure CoreInput.LeaderNoOp
    ApplicationBytesDto bytes -> pure (CoreInput.Application bytes)
    ConfigurationDto configuration metadata -> CoreInput.Configuration <$> raftVotingConfigurationDtoToCore configuration <*> pure metadata
  pure (CoreInput.raftLogEntry (raftLogIndexDtoToCore (raftLogEntryIndex entry)) (raftTermDtoToCore (raftLogEntryTerm entry)) payload)

coreConflictHintDto ::
  CoreInput.RaftConflictHint ->
  Either RaftAdapterError RaftConflictHintDto
coreConflictHintDto hint = case CoreInput.raftConflictHintView hint of
  CoreInput.MissingPrefixView firstIndex ->
    Right (MissingSuffixFromDto (raftLogIndexDtoFromCore firstIndex))
  CoreInput.ConflictingTermView term firstIndex ->
    Right
      ( ConflictingTermFromDto
          (raftTermDtoFromCore term)
          (raftLogIndexDtoFromCore firstIndex)
      )

dtoConflictHintCore ::
  RaftConflictHintDto ->
  Either RaftAdapterError CoreInput.RaftConflictHint
dtoConflictHintCore = \case
  MissingSuffixFromDto firstIndex ->
    mapInput (CoreInput.missingPrefixHint (raftLogIndexDtoToCore firstIndex))
  ConflictingTermFromDto term firstIndex ->
    mapInput
      ( CoreInput.conflictingTermHint
          (raftTermDtoToCore term)
          (raftLogIndexDtoToCore firstIndex)
      )

mapInput :: Either CoreInput.RaftInputFault value -> Either RaftAdapterError value
mapInput = either (Left . RaftAdapterInputFault) Right

mapShape :: Either RaftShapeError value -> Either RaftAdapterError value
mapShape = either (Left . RaftAdapterShapeFault) Right
