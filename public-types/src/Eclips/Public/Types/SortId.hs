-- | The public content-addressed identity of a sort.
--
-- This module establishes only an opaque 32-byte representation. It neither
-- computes a descriptor hash nor proves that a corresponding descriptor has been
-- admitted.
module Eclips.Public.Types.SortId
  ( SortIdError (..),
    SortId,
    mkSortId,
    sortIdBytes,
  )
where

import Data.Binary (Binary (get, put))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

sortIdByteCount :: Int
sortIdByteCount = 32

-- | A structurally invalid public sort identity.
data SortIdError = WrongSortIdByteCount
  { -- | Required byte length for every current sort identity.
    expectedSortIdByteCount :: Int,
    -- | Byte length supplied to the checked constructor.
    actualSortIdByteCount :: Int
  }
  deriving stock (Eq, Show)

-- | An opaque 32-byte public sort content address.
newtype SortId = SortId ByteString
  deriving stock (Eq, Ord)

instance Show SortId where
  show = renderGroupedHex . sortIdBytes

-- | Current-build wire representation of a public sort identity.
--
-- Decoding repeats the exact-width structural admission performed by
-- 'mkSortId'. The representation is intentionally not a protocol compatibility
-- promise.
instance Binary SortId where
  put (SortId bytes) = put bytes
  get = do
    bytes <- get
    case mkSortId bytes of
      Left _ -> fail "SortId must contain exactly 32 bytes"
      Right sortId -> pure sortId

-- | Check the exact byte length of a public sort identity.
mkSortId :: ByteString -> Either SortIdError SortId
mkSortId bytes
  | ByteString.length bytes == sortIdByteCount = Right (SortId bytes)
  | otherwise =
      Left
        WrongSortIdByteCount
          { expectedSortIdByteCount = sortIdByteCount,
            actualSortIdByteCount = ByteString.length bytes
          }

-- | Recover the exact opaque bytes of a checked sort identity.
sortIdBytes :: SortId -> ByteString
sortIdBytes (SortId bytes) = bytes
