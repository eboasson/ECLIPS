{-# LANGUAGE OverloadedStrings #-}

-- | Canonical framing for one frozen transferable source bundle. The five
-- frames remain claims for their respective owners to admit; this envelope
-- carries no checked snapshot, donor state, or installation authority.
module Eclips.Herald.Join.SourceBundle
  ( JoinSourceBundle,
    joinSourceBundle,
    encodeJoinSourceBundle,
    decodeJoinSourceBundle,
    joinSourceBundleProjectionFrame,
    joinSourceBundleRegistryFrame,
    joinSourceBundleProgressFrame,
    joinSourceBundleCarrierFrame,
    joinSourceBundleHistoryFrame,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize qualified as Codec

-- Fields are private so a parsed envelope cannot be changed through record
-- update. Component frames own their bytes independently of the input buffer.
data JoinSourceBundle
  = JoinSourceBundle !ByteString !ByteString !ByteString !ByteString !ByteString
  deriving stock (Eq, Show)

-- | Assemble explicit owner frames. This checks no inner semantic claim;
-- owner admission remains separate from canonical envelope construction.
joinSourceBundle :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> JoinSourceBundle
joinSourceBundle projection registry progress carriers history =
  JoinSourceBundle
    (ByteString.copy projection)
    (ByteString.copy registry)
    (ByteString.copy progress)
    (ByteString.copy carriers)
    (ByteString.copy history)

-- | The complete source digest preimage. The digest is deliberately absent
-- from these bytes and is referenced by a later Oracle seal.
encodeJoinSourceBundle :: JoinSourceBundle -> ByteString
encodeJoinSourceBundle (JoinSourceBundle projection registry progress carriers history) =
  Codec.encode
    ( "ECLIPS-HERALD-JOINING-BASE" :: ByteString,
      projection,
      registry,
      progress,
      carriers,
      history
    )

decodeJoinSourceBundle :: ByteString -> Either String JoinSourceBundle
decodeJoinSourceBundle bytes = do
  transcript@(domain, projection, registry, progress, carriers, history) <- Codec.decode bytes
  unless
    (domain == ("ECLIPS-HERALD-JOINING-BASE" :: ByteString) && Codec.encode transcript == bytes)
    (Left "noncanonical joining source bundle")
  pure (joinSourceBundle projection registry progress carriers history)

joinSourceBundleProjectionFrame :: JoinSourceBundle -> ByteString
joinSourceBundleProjectionFrame (JoinSourceBundle projection _ _ _ _) = projection

joinSourceBundleRegistryFrame :: JoinSourceBundle -> ByteString
joinSourceBundleRegistryFrame (JoinSourceBundle _ registry _ _ _) = registry

joinSourceBundleProgressFrame :: JoinSourceBundle -> ByteString
joinSourceBundleProgressFrame (JoinSourceBundle _ _ progress _ _) = progress

joinSourceBundleCarrierFrame :: JoinSourceBundle -> ByteString
joinSourceBundleCarrierFrame (JoinSourceBundle _ _ _ carriers _) = carriers

joinSourceBundleHistoryFrame :: JoinSourceBundle -> ByteString
joinSourceBundleHistoryFrame (JoinSourceBundle _ _ _ _ history) = history
