-- | Reclaim an admitted control prefix after its composed consumers finish.
-- Client keeps exact request/label exceptions; Projection also keeps every
-- outstanding admission tail. No semantic replay or whole-Herald audit belongs
-- at this completed transaction boundary.
module Eclips.Herald.UseCase.ControlReclamation
  ( ControlReclamationProblem (..),
    reclaimControlPrefix,
    reclaimControlPrefixOrFault,
  )
where

import Control.Monad (unless)
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldOracleTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    startupJoinState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupSemanticControlIsFrozen,
  )

data ControlReclamationProblem
  = ControlReclamationClientProblem Client.OracleClientProblem
  | ControlReclamationProjectionProblem Projection.OracleProjectionInvariant
  deriving stock (Eq, Show)

-- Frozen semantics and its later control observations have different owners.
-- Only an unfrozen completed successor can advance both coverage coordinates.
reclaimControlPrefix :: HeraldState -> Either ControlReclamationProblem HeraldState
reclaimControlPrefix state
  | startupSemanticControlIsFrozen state = pure state
  | otherwise = do
      let initialProjection = startupOracleProjectionState state
          initialClient = startupOracleClientState state
          through = Projection.oracleViewControlIndex (Projection.oracleView initialProjection)
      unless
        ( Client.oracleClientAppliedCursor initialClient == through
            && Client.oracleClientCanonicalCoveredThrough initialClient == Projection.projectionCanonicalCoveredThrough initialProjection
        )
        (Left (ControlReclamationClientProblem Client.OracleClientStateContradiction))
      !client <- either (Left . ControlReclamationClientProblem) Right (Client.reclaimOracleClientCanonicalPrefix through initialClient)
      !projection <- either (Left . ControlReclamationProjectionProblem) Right (Projection.reclaimCurrentAppliedPrefix (Client.oracleClientProtectedPrefixEvidence client) (Join.admissionControlTailFloor (startupJoinState state)) initialProjection)
      let !successor = replaceStartupOracleClientState client . replaceStartupOracleProjectionState projection $ state
      pure successor

reclaimControlPrefixOrFault :: HeraldState -> Either HeraldInvariantFault HeraldState
reclaimControlPrefixOrFault = either (const (Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction))) Right . reclaimControlPrefix
