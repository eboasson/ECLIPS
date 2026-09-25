-- | Explicit terminal outcomes for one EORC watch worker.
module Eclips.Oracle.Runtime.Internal.WatchServe
  ( OracleWatchTermination (..),
    prepareOracleWatchEntries,
    publishedOracleWatchRange,
    terminateOracleWatchLane,
  )
where

import Control.Exception (finally)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (ControlIndex)
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Oracle.Runtime.Internal.TCP.Retirement
  ( retireSocketQuietly,
  )
import Eclips.Protocol.Oracle.Codec (committedOracleEntriesDtoFromValues)
import Eclips.Protocol.Oracle.Types
  ( CommittedOracleEntriesDto,
    OracleShapeError,
  )
import Network.Socket (Socket)

-- | A watch that cannot produce a protocol reply must retire its physical
-- lane. Runtime unavailability is an ordinary shutdown outcome; failure to
-- encode an owner-produced contiguous suffix is an invariant fault which the
-- caller reports before the same lane retirement.
data OracleWatchTermination
  = OracleWatchRuntimeUnavailable
  | OracleWatchEncodingInvariant OracleShapeError
  deriving stock (Eq, Show)

prepareOracleWatchEntries ::
  ControlIndex ->
  NonEmpty AppliedOracleEntry ->
  Either OracleWatchTermination CommittedOracleEntriesDto
prepareOracleWatchEntries cursor =
  mapLeft OracleWatchEncodingInvariant
    . committedOracleEntriesDtoFromValues cursor
    . NonEmpty.toList

-- | The retained ledger can extend past the published prefix. Split both
-- ordered bounds so a small watch response does not walk old history or
-- expose an entry whose adapter acknowledgement is still pending.
publishedOracleWatchRange :: ControlIndex -> ControlIndex -> Map ControlIndex entry -> [entry]
publishedOracleWatchRange cursor published entries =
  Map.elems beforePublished <> maybe [] pure atPublished
  where
    (_, afterCursor) = Map.split cursor entries
    (beforePublished, atPublished, _) = Map.splitLookup published afterCursor

terminateOracleWatchLane ::
  (OracleShapeError -> IO ()) ->
  Socket ->
  OracleWatchTermination ->
  IO ()
terminateOracleWatchLane reportInvariant socket termination =
  case termination of
    OracleWatchRuntimeUnavailable -> retireSocketQuietly socket
    OracleWatchEncodingInvariant problem ->
      reportInvariant problem `finally` retireSocketQuietly socket

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft f = \case
  Left value -> Left (f value)
  Right value -> Right value
