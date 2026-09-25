-- | Pure effects emitted by the Oracle transition owner.
module Eclips.Oracle.Effect
  ( OracleProtocolDisposition (..),
    OracleStepOutcome (..),
    OracleEffect (..),
    OracleEffectBatch,
    oracleEffects,
    emptyOracleEffectBatch,
    emitAppliedOracleEntry,
  )
where

import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, LabelDecisionId)
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Internal.Label
  ( AppliedOracleEntry,
    OracleProtocolDisposition (..),
    OracleReceipt,
    OracleRequestRetirement,
  )
import Eclips.Oracle.Progress (OracleProgress)

data OracleStepOutcome
  = OracleCommitted OracleReceipt
  | OracleDeferred OracleClientRequestId LabelDecisionId ControlIndex
  | OracleDuplicate OracleReceipt
  | OracleProtocolRejected OracleProtocolDisposition
  | OracleRequestRetired OracleRequestRetirement
  | OracleProgressRetired HeraldEpoch OracleProgress
  deriving stock (Eq, Show)

data OracleEffect
  = EmitAppliedOracleEntry AppliedOracleEntry
  deriving stock (Eq, Show)

newtype OracleEffectBatch = OracleEffectBatch [OracleEffect]
  deriving stock (Eq, Show)

oracleEffects :: OracleEffectBatch -> [OracleEffect]
oracleEffects (OracleEffectBatch effects) = effects

emptyOracleEffectBatch :: OracleEffectBatch
emptyOracleEffectBatch = OracleEffectBatch []

emitAppliedOracleEntry :: AppliedOracleEntry -> OracleEffectBatch
emitAppliedOracleEntry entry =
  OracleEffectBatch [EmitAppliedOracleEntry entry]
