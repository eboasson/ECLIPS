-- | Framed administration replies for one operator exchange.
module Eclips.Deployment.Admin.Exchange (receiveAdministrationReplies) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.List (nub)
import Eclips.Application.Types.Lifecycle (LifecycleStatus (LifecyclePending))
import Eclips.Protocol.Admin.Frame
import Eclips.Protocol.Admin.Types

-- | Exact retry and result lookup can reoffer the same retained reply. Collapse
-- exact DTO duplicates in first-seen order so coalescing their frames does not
-- duplicate the operator result. Distinct replies and status changes stay
-- observable; drain acceptance still waits for its terminal receipt.
receiveAdministrationReplies :: IO ByteString -> IO [AdminServerDto]
receiveAdministrationReplies receive = go (initialAdminFrameDecoder AdminServerFrames) []
  where
    go decoder accumulated = do
      bytes <- receive
      if Bytes.null bytes
        then ioError (userError "operator connection closed before its reply")
        else case feedAdminFrame decoder bytes of
          AdminFrameFeedResult envelopes continuation -> do
            replies <- traverse server envelopes
            let observed = nub (accumulated <> replies)
            if any finalReply replies
              then pure observed
              else case continuation of
                NeedAdminFrameBytes next -> go next observed
                AdminFrameFailed failure -> ioError (userError (show failure))
    server (AdminServerEnvelope reply) = pure reply
    server _ = ioError (userError "operator server sent a client envelope")
    finalReply reply = case reply of
      HeraldDrainAccepted _ -> False
      AdminResult _ AdminAccepted -> False
      AdminResult _ (AdminPreparationCancellation (LifecyclePending _)) -> False
      _ -> True
