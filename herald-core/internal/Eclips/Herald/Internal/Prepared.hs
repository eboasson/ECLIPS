-- | Package-private support for ephemeral pure Herald transition preparation.
--
-- A @Prepared@ value is not replay or protocol data. It packages the complete
-- successor and output after every semantic decision has succeeded, so commit has
-- no semantic failure branch. Herald leaves wrap this type in their own opaque,
-- use-case-specific plans; it is unavailable to other packages.
module Eclips.Herald.Internal.Prepared
  ( Prepared,
    prepareTransition,
    preparedOutput,
    commitPrepared,
  )
where

-- | Complete successor state and transition output. The constructor is private.
data Prepared state output = Prepared state output

-- | Run a fallible pure preparation without exposing a partial successor.
prepareTransition ::
  (state -> Either error (state, output)) ->
  state ->
  Either error (Prepared state output)
prepareTransition transition predecessor = do
  (successor, output) <- transition predecessor
  Right (Prepared successor output)

-- | Inspect only the checked output without exposing the successor state.
preparedOutput :: Prepared state output -> output
preparedOutput (Prepared _ output) = output

-- | Reveal the already complete transition result.
commitPrepared :: Prepared state output -> (state, output)
commitPrepared (Prepared state output) = (state, output)
