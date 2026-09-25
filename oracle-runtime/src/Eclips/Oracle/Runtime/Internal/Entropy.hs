-- | Runtime election-timeout selection and the sole production entropy read.
module Eclips.Oracle.Runtime.Internal.Entropy
  ( systemRuntimeElectionTimeoutSource,
    runtimeElectionTimeoutSource,
    selectElectionTimeoutDuration,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Oracle.Runtime.Internal.Types
  ( RuntimeElectionTimeoutSource (..),
  )
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    raftNativeElectionTimeoutLower,
    raftNativeElectionTimeoutUpper,
  )
import Eclips.Raft.Identity
  ( RaftDurationMicros,
    mkRaftDurationMicros,
    raftDurationMicrosWord64,
  )
import System.Entropy (getEntropy)

systemRuntimeElectionTimeoutSource :: RuntimeElectionTimeoutSource
systemRuntimeElectionTimeoutSource =
  RuntimeElectionTimeoutSource (decodeWord64 <$> getEntropy 8)

runtimeElectionTimeoutSource :: IO Word64 -> RuntimeElectionTimeoutSource
runtimeElectionTimeoutSource = RuntimeElectionTimeoutSource

-- | Select inclusively using unbounded arithmetic. Checked genesis ensures the
-- range is non-empty and positive, so reconstructing the typed duration cannot
-- fail for one of its own endpoints.
selectElectionTimeoutDuration ::
  RaftNativeConfiguration ->
  Word64 ->
  RaftDurationMicros
selectElectionTimeoutDuration configuration sample =
  case mkRaftDurationMicros (fromInteger selected) of
    Right duration -> duration
    Left fault -> error ("checked election duration rejected: " <> show fault)
  where
    lower = toInteger (raftDurationMicrosWord64 (raftNativeElectionTimeoutLower configuration))
    upper = toInteger (raftDurationMicrosWord64 (raftNativeElectionTimeoutUpper configuration))
    selected = lower + toInteger sample `mod` (upper - lower + 1)

decodeWord64 :: ByteString.ByteString -> Word64
decodeWord64 = ByteString.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0
