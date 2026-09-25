-- | Redacted runtime observations and terminal classes.
module Eclips.Herald.Runtime.Trace
  ( RuntimeDiagnostic,
    RuntimeDiagnosticClass (..),
    runtimeDiagnosticClass,
    HeraldRuntimeFailure,
    HeraldRuntimeFailureClass (..),
    heraldRuntimeFailureClass,
    HeraldRuntimeExit (..),
  )
where

import Eclips.Herald.Runtime.Internal.Types
  ( HeraldRuntimeExit (..),
    HeraldRuntimeFailure (..),
    HeraldRuntimeFailureClass (..),
    RuntimeDiagnostic (..),
    RuntimeDiagnosticClass (..),
  )

-- | Observe only the redacted diagnostic class.
runtimeDiagnosticClass :: RuntimeDiagnostic -> RuntimeDiagnosticClass
runtimeDiagnosticClass (RuntimeDiagnostic diagnosticClass) = diagnosticClass

-- | Observe only the deterministic public failure class.
heraldRuntimeFailureClass :: HeraldRuntimeFailure -> HeraldRuntimeFailureClass
heraldRuntimeFailureClass (HeraldRuntimeFailure failureClass) = failureClass
