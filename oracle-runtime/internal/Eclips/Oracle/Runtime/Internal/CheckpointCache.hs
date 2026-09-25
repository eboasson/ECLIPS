-- | One serialized local checkpoint, privately owned by the Oracle worker.
module Eclips.Oracle.Runtime.Internal.CheckpointCache
  ( OracleCheckpointCache,
    CapturedOracleCheckpoint,
    OracleCheckpointCaptureWork (..),
    emptyOracleCheckpointCache,
    advanceOracleCheckpointCache,
    captureOracleCheckpoint,
    capturedOracleCheckpointIndex,
    capturedOracleCheckpointBytes,
  )
where

import Data.ByteString (ByteString)
import Eclips.Domain.Identity (ControlIndex)

data CapturedOracleCheckpoint = CapturedOracleCheckpoint !ControlIndex !ByteString
  deriving stock (Eq, Show)

data OracleCheckpointCache
  = EmptyOracleCheckpointCache
  | CachedOracleCheckpoint !CapturedOracleCheckpoint
  deriving stock (Eq, Show)

data OracleCheckpointCaptureWork = OracleCheckpointEncoded | OracleCheckpointReused
  deriving stock (Eq, Show)

emptyOracleCheckpointCache :: OracleCheckpointCache
emptyOracleCheckpointCache = EmptyOracleCheckpointCache

-- Ordinary transitions preserve the entire state when the control index stays
-- fixed. Installation of a remote checkpoint must instead reset the cache,
-- including when the installed control index equals the previous one.
advanceOracleCheckpointCache :: ControlIndex -> OracleCheckpointCache -> OracleCheckpointCache
advanceOracleCheckpointCache current cached@(CachedOracleCheckpoint checkpoint)
  | capturedOracleCheckpointIndex checkpoint == current = cached
advanceOracleCheckpointCache _ _ = EmptyOracleCheckpointCache

-- The encoded argument remains unevaluated on a hit. On a miss the strict
-- checkpoint fields, once forced by the owner, retain bytes rather than an
-- encoding closure over an old Oracle state. Cache the final local recipe so
-- the adapter can hand the same immutable buffer directly to Raft.
captureOracleCheckpoint :: ControlIndex -> ByteString -> OracleCheckpointCache -> (OracleCheckpointCache, CapturedOracleCheckpoint, OracleCheckpointCaptureWork)
captureOracleCheckpoint current _ cached@(CachedOracleCheckpoint checkpoint)
  | capturedOracleCheckpointIndex checkpoint == current = (cached, checkpoint, OracleCheckpointReused)
captureOracleCheckpoint current encoded _ =
  let checkpoint = CapturedOracleCheckpoint current encoded
   in (CachedOracleCheckpoint checkpoint, checkpoint, OracleCheckpointEncoded)

capturedOracleCheckpointIndex :: CapturedOracleCheckpoint -> ControlIndex
capturedOracleCheckpointIndex (CapturedOracleCheckpoint index _) = index

capturedOracleCheckpointBytes :: CapturedOracleCheckpoint -> ByteString
capturedOracleCheckpointBytes (CapturedOracleCheckpoint _ bytes) = bytes
