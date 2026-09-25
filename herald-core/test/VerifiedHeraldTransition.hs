-- | Test-only exhaustive verification around the production Herald transition.
module VerifiedHeraldTransition
  ( verifiedStepHerald,
  )
where

import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Input (HeraldInput)
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault,
    validateHeraldState,
  )
import Eclips.Herald.Startup.State (HeraldState)
import Eclips.Herald.Transition (stepHerald)

-- | Preserve the production result exactly while exhaustively checking every
-- successful successor exercised by the pure test suite.
verifiedStepHerald ::
  HeraldInput ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
verifiedStepHerald input predecessor = do
  result@(successor, _) <- stepHerald input predecessor
  validateHeraldState successor
  Right result
