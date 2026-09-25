{-# LANGUAGE DeriveAnyClass #-}

-- | Process preparation and connection claims, separate from the eight data
-- operations. Public construction checks representation only, never authority.
module Eclips.Application.Types.Lifecycle
  ( LifecycleShapeError (..),
    HeraldLocator,
    heraldLocator,
    heraldLocatorHost,
    heraldLocatorPort,
    ConnectionDescriptor,
    connectionDescriptor,
    connectionDescriptorWithTiming,
    connectionDescriptorTakeoverTarget,
    connectionDescriptorLocator,
    connectionDescriptorLineageBytes,
    connectionDescriptorEpochBytes,
    connectionDescriptorAttachmentBytes,
    ChildPreparation,
    childPreparation,
    childPreparationScopeBytes,
    childPreparationSessionOrdinal,
    childPreparationOrdinal,
    PreparedChild,
    preparedChild,
    preparedChildPreparation,
    preparedChildProcess,
    preparedChildConnection,
    InitialClaimId,
    initialClaimId,
    initialClaimIdBytes,
    LifecycleRequestId,
    lifecycleRequestId,
    lifecycleRequestIdScopeBytes,
    lifecycleRequestIdSessionOrdinal,
    lifecycleRequestIdWord64,
    LifecycleCommand (..),
    LifecycleResult (..),
    LifecycleStatus (..),
    LifecycleReply (..),
    LifecycleError (..),
    StartupError (..),
    lifecycleResultMatches,
  ) where

import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import Eclips.Application.Types.Access (PrimordialSelection)
import Eclips.Application.Types.Identity (PrivateProcessId)
import Eclips.Public.Types.Timing (TakeoverTarget, defaultTakeoverTarget)
import GHC.Generics (Generic)

data LifecycleShapeError
  = LifecycleEmptyHost
  | LifecycleZeroPort
  | LifecycleWrongByteCount Int
  deriving stock (Eq, Show)

data HeraldLocator = HeraldLocator Text Word16 deriving stock (Eq, Ord, Show)
heraldLocator :: Text -> Word16 -> Either LifecycleShapeError HeraldLocator
heraldLocator host port
  | Text.null host = Left LifecycleEmptyHost
  | port == 0 = Left LifecycleZeroPort
  | otherwise = Right (HeraldLocator host port)
heraldLocatorHost :: HeraldLocator -> Text
heraldLocatorHost (HeraldLocator host _) = host
heraldLocatorPort :: HeraldLocator -> Word16
heraldLocatorPort (HeraldLocator _ port) = port
instance Binary HeraldLocator where
  put (HeraldLocator host port) = put host >> put port
  get = do
    host <- get
    port <- get
    either (fail . show) pure (heraldLocator host port)

data ConnectionDescriptor = ConnectionDescriptor TakeoverTarget HeraldLocator ByteString ByteString ByteString deriving stock (Eq, Ord, Show)

-- | Construct portable startup material with the default five-second target.
connectionDescriptor :: HeraldLocator -> ByteString -> ByteString -> ByteString -> Either LifecycleShapeError ConnectionDescriptor
connectionDescriptor = connectionDescriptorWithTiming defaultTakeoverTarget

-- | Carry the deployment's checked timing target to a starting application.
connectionDescriptorWithTiming :: TakeoverTarget -> HeraldLocator -> ByteString -> ByteString -> ByteString -> Either LifecycleShapeError ConnectionDescriptor
connectionDescriptorWithTiming target locator lineage epoch attachment = ConnectionDescriptor target locator <$> checkedBytes lineage <*> checkedBytes epoch <*> checkedBytes attachment

connectionDescriptorTakeoverTarget :: ConnectionDescriptor -> TakeoverTarget
connectionDescriptorTakeoverTarget (ConnectionDescriptor target _ _ _ _) = target
connectionDescriptorLocator :: ConnectionDescriptor -> HeraldLocator
connectionDescriptorLocator (ConnectionDescriptor _ locator _ _ _) = locator
connectionDescriptorLineageBytes, connectionDescriptorEpochBytes, connectionDescriptorAttachmentBytes :: ConnectionDescriptor -> ByteString
connectionDescriptorLineageBytes (ConnectionDescriptor _ _ lineage _ _) = lineage
connectionDescriptorEpochBytes (ConnectionDescriptor _ _ _ epoch _) = epoch
connectionDescriptorAttachmentBytes (ConnectionDescriptor _ _ _ _ attachment) = attachment
instance Binary ConnectionDescriptor where
  put (ConnectionDescriptor target locator lineage epoch attachment) = put target >> put locator >> put lineage >> put epoch >> put attachment
  get = do
    target <- get
    locator <- get
    lineage <- get
    epoch <- get
    attachment <- get
    either (fail . show) pure (connectionDescriptorWithTiming target locator lineage epoch attachment)

data ChildPreparation = ChildPreparation ByteString Word64 Word64 deriving stock (Eq, Ord, Show)
childPreparation :: ByteString -> Word64 -> Word64 -> Either LifecycleShapeError ChildPreparation
childPreparation scope session ordinal = (\bytes -> ChildPreparation bytes session ordinal) <$> checkedBytes scope
childPreparationScopeBytes :: ChildPreparation -> ByteString
childPreparationScopeBytes (ChildPreparation scope _ _) = scope
childPreparationSessionOrdinal, childPreparationOrdinal :: ChildPreparation -> Word64
childPreparationSessionOrdinal (ChildPreparation _ session _) = session
childPreparationOrdinal (ChildPreparation _ _ ordinal) = ordinal
instance Binary ChildPreparation where
  put (ChildPreparation scope session ordinal) = put scope >> put session >> put ordinal
  get = do
    scope <- get
    session <- get
    ordinal <- get
    either (fail . show) pure (childPreparation scope session ordinal)

data PreparedChild = PreparedChild ChildPreparation PrivateProcessId ConnectionDescriptor deriving stock (Eq, Show)
preparedChild :: ChildPreparation -> PrivateProcessId -> ConnectionDescriptor -> PreparedChild
preparedChild = PreparedChild
preparedChildPreparation :: PreparedChild -> ChildPreparation
preparedChildPreparation (PreparedChild preparation _ _) = preparation
preparedChildProcess :: PreparedChild -> PrivateProcessId
preparedChildProcess (PreparedChild _ process _) = process
preparedChildConnection :: PreparedChild -> ConnectionDescriptor
preparedChildConnection (PreparedChild _ _ connection) = connection
instance Binary PreparedChild where
  put (PreparedChild preparation process connection) = put preparation >> put process >> put connection
  get = PreparedChild <$> get <*> get <*> get

newtype InitialClaimId = InitialClaimId ByteString deriving stock (Eq, Ord, Show)
initialClaimId :: ByteString -> Either LifecycleShapeError InitialClaimId
initialClaimId bytes = InitialClaimId <$> checkedBytes bytes
initialClaimIdBytes :: InitialClaimId -> ByteString
initialClaimIdBytes (InitialClaimId bytes) = bytes
instance Binary InitialClaimId where
  put = put . initialClaimIdBytes
  get = get >>= either (fail . show) pure . initialClaimId

data LifecycleRequestId = LifecycleRequestId ByteString Word64 Word64 deriving stock (Eq, Ord, Show)
lifecycleRequestId :: ByteString -> Word64 -> Word64 -> Either LifecycleShapeError LifecycleRequestId
lifecycleRequestId scope session ordinal = (\bytes -> LifecycleRequestId bytes session ordinal) <$> checkedBytes scope
lifecycleRequestIdScopeBytes :: LifecycleRequestId -> ByteString
lifecycleRequestIdScopeBytes (LifecycleRequestId scope _ _) = scope
lifecycleRequestIdSessionOrdinal, lifecycleRequestIdWord64 :: LifecycleRequestId -> Word64
lifecycleRequestIdSessionOrdinal (LifecycleRequestId _ session _) = session
lifecycleRequestIdWord64 (LifecycleRequestId _ _ value) = value
instance Binary LifecycleRequestId where
  put (LifecycleRequestId scope session ordinal) = put scope >> put session >> put ordinal
  get = do
    scope <- get
    session <- get
    ordinal <- get
    either (fail . show) pure (lifecycleRequestId scope session ordinal)

data LifecycleCommand
  = BeginChild HeraldLocator PrimordialSelection
  | AwaitPreparedChild ChildPreparation
  | AwaitChildReady ChildPreparation
  | CancelChild ChildPreparation
  | EndOwnProcess
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data LifecycleResult
  = ChildPreparationAccepted ChildPreparation
  | ChildPrepared PreparedChild
  | ChildReady
  | ChildCancelled
  | ChildAlreadyAttached
  | ProcessEnded
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data LifecycleStatus
  = LifecyclePending [Text]
  | LifecycleCompleted LifecycleResult
  | LifecycleRejected LifecycleError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data LifecycleReply
  = LifecycleReply LifecycleRequestId LifecycleStatus
  | LifecycleAbsent LifecycleRequestId
  | LifecycleConflict LifecycleRequestId
  | LifecycleRetired LifecycleRequestId Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data LifecycleError
  = LifecycleUnknownTarget
  | LifecycleTargetUnavailable
  | LifecycleSelectionNotAdmitted
  | LifecycleUnknownPreparation
  | LifecycleNotPreparingParent
  | LifecycleAlreadyTerminal
  | LifecycleRequestNotAdmitted
  | LifecycleOwnProcessNotLive
  | LifecycleOracleRejected
  | LifecycleStartupFailed StartupError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data StartupError
  = StartupWrongTarget
  | StartupWrongLineage
  | StartupUnknownAttachment
  | StartupNotLive
  | StartupAlreadyClaimed
  | StartupCancelled
  | StartupRequiredObjectUnavailable Text
  | StartupSessionExpired
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The single result-kind witness used by client and transport admission.
lifecycleResultMatches :: LifecycleCommand -> LifecycleResult -> Bool
lifecycleResultMatches command result = case (command, result) of
  (BeginChild _ _, ChildPreparationAccepted _) -> True
  (AwaitPreparedChild expected, ChildPrepared child) -> expected == preparedChildPreparation child
  (AwaitChildReady _, ChildReady) -> True
  (CancelChild _, ChildCancelled) -> True
  (CancelChild _, ChildAlreadyAttached) -> True
  (EndOwnProcess, ProcessEnded) -> True
  _ -> False

checkedBytes :: ByteString -> Either LifecycleShapeError ByteString
checkedBytes bytes
  | Bytes.length bytes == 32 = Right bytes
  | otherwise = Left (LifecycleWrongByteCount (Bytes.length bytes))
