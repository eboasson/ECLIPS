-- | Authority classification and the replaceable bounded-delay seam for an
-- empty EORC watch at the local applied tip.
module Eclips.Oracle.Runtime.Internal.WatchAvailability
  ( OracleWatchAvailability (..),
    RuntimeDelay,
    runtimeDelay,
    awaitWatchAuthorityGrace,
    classifyOracleWatchAvailability,
  )
where

import Control.Concurrent.STM
  ( STM,
    atomically,
    orElse,
  )
import Eclips.Raft.Identity (RaftDurationMicros)

-- | Replaceable physical-delay registration. The returned STM action becomes
-- enabled after the supplied checked Raft duration. Keeping the constructor
-- private makes every caller cross the explicit builder seam.
newtype RuntimeDelay = RuntimeDelay
  { prepareRuntimeDelay :: RaftDurationMicros -> IO (STM ())
  }

runtimeDelay :: (RaftDurationMicros -> IO (STM ())) -> RuntimeDelay
runtimeDelay = RuntimeDelay

-- | Arm one authority-loss episode and left-bias already available watch or
-- status evidence over simultaneous physical expiry.
awaitWatchAuthorityGrace ::
  RuntimeDelay ->
  RaftDurationMicros ->
  STM outcome ->
  STM outcome ->
  IO outcome
awaitWatchAuthorityGrace delay duration observe expire = do
  elapsed <- prepareRuntimeDelay delay duration
  atomically (observe `orElse` (elapsed >> expire))

-- | Current authority evidence for an empty watch at the local applied tip.
-- The caller gives unresolved authority a bounded grace only when this replica
-- had accepted the watch as leader; an initially contacted nonleader still
-- redirects into the client's paced fixed-contact rotation.
data OracleWatchAvailability
  = OracleWatchAuthorityStopped
  | OracleWatchAuthorityLocal
  | OracleWatchAuthorityDistinct
  | OracleWatchAuthorityUnresolved
  deriving stock (Eq, Show)

classifyOracleWatchAvailability ::
  (Eq node) =>
  Bool ->
  Bool ->
  node ->
  Maybe node ->
  OracleWatchAvailability
classifyOracleWatchAvailability accepting localLeader local leaderHint
  | not accepting = OracleWatchAuthorityStopped
  | localLeader = OracleWatchAuthorityLocal
  | maybe False (/= local) leaderHint = OracleWatchAuthorityDistinct
  | otherwise = OracleWatchAuthorityUnresolved
