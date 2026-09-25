-- | Replaceable logical physical-timer workers.
module Eclips.Oracle.Runtime.Internal.Timer
  ( runRuntimeTimer,
    systemRuntimeDelay,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    atomically,
    check,
    orElse,
    putTMVar,
    readTQueue,
    readTVar,
    registerDelay,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( RuntimeTimerCommand (..),
  )
import Eclips.Oracle.Runtime.Internal.WatchAvailability
  ( RuntimeDelay,
    runtimeDelay,
  )
import Eclips.Raft.Identity (raftDurationMicrosWord64)

systemRuntimeDelay :: RuntimeDelay
systemRuntimeDelay =
  runtimeDelay $ \duration -> do
    elapsed <- registerDelay (fromIntegral (raftDurationMicrosWord64 duration))
    pure (readTVar elapsed >>= check)

runRuntimeTimer ::
  TQueue (RuntimeTimerCommand generation) ->
  (generation -> IO ()) ->
  IO ()
runRuntimeTimer commands fire = idle
  where
    idle = do
      command <- atomically (readTQueue commands)
      case command of
        ArmRuntimeTimer generation duration -> armed generation duration
        SupersedeRuntimeTimer _ -> idle
        BarrierRuntimeTimer done -> atomically (putTMVar done ()) >> idle
        StopRuntimeTimer -> pure ()

    armed generation duration = do
      elapsed <- registerDelay (fromIntegral duration)
      waitArmed generation duration elapsed

    waitArmed generation duration elapsed = do
      next <-
        atomically
          ( (Left <$> readTQueue commands)
              `orElse` (readTVar elapsed >>= check >> pure (Right generation))
          )
      case next of
        Right fired -> fire fired >> idle
        Left command -> case command of
          ArmRuntimeTimer replacement replacementDuration ->
            armed replacement replacementDuration
          SupersedeRuntimeTimer _ -> idle
          BarrierRuntimeTimer done -> atomically (putTMVar done ()) >> waitArmed generation duration elapsed
          StopRuntimeTimer -> pure ()
