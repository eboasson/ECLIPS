-- | Domain-neutral diagnostic rendering for opaque fixed-width identifiers.
--
-- This rendering is deliberately separate from every canonical and wire
-- representation.  Callers retain ownership of byte-width admission.
module Eclips.Public.Types.Diagnostic
  ( renderGroupedHex,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (intercalate)
import Numeric (showHex)

-- | Render bytes as lowercase hexadecimal, separated into four-byte groups.
renderGroupedHex :: ByteString -> String
renderGroupedHex = intercalate ":" . groupsOfEight . concatMap renderByte . ByteString.unpack
  where
    renderByte byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

groupsOfEight :: String -> [String]
groupsOfEight [] = []
groupsOfEight digits =
  let (group, following) = splitAt 8 digits
   in group : groupsOfEight following
