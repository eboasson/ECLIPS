-- | Local diagnostic policy. It never changes admission of external data or
-- becomes part of a portable state witness.
module Eclips.Herald.DiagnosticChecks
  ( DiagnosticChecks (..),
    runDiagnosticCheck,
  )
where

-- | Exhaustive audits of facts already established by checked owner transitions.
-- Ordinary pure entry points and runtime configurations enable these by default.
data DiagnosticChecks
  = DiagnosticChecksEnabled
  | DiagnosticChecksDisabled
  deriving stock (Eq, Show)

-- | The disabled branch deliberately does not evaluate the audit argument.
runDiagnosticCheck :: DiagnosticChecks -> Either problem () -> Either problem ()
runDiagnosticCheck DiagnosticChecksEnabled audit = audit
runDiagnosticCheck DiagnosticChecksDisabled _ = Right ()
