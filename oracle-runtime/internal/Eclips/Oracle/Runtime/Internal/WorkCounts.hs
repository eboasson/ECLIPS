-- | Pure categorical diagnostic counters. This module neither retains nor
-- inspects a kernel state; state inventory and IO remain with the sole owner.
module Eclips.Oracle.Runtime.Internal.WorkCounts
  ( OracleWorkCounts,
    emptyOracleWorkCounts,
    countOracleWork,
    oracleWorkRows,
    oracleCommandWorkTag,
    oracleOutcomeWorkTag,
    oracleWorkSnapshotComplete,
  )
where

import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex)
import Eclips.Oracle.Command (OracleCommand)
import Eclips.Oracle.Effect (OracleStepOutcome (..))
import Eclips.Oracle.Receipt (OracleReceiptResult (..), oracleReceiptResult)

-- Keys name closed constructor families, never requests, identities or payloads.
newtype OracleWorkCounts = OracleWorkCounts (Map (String, String) Word64)
  deriving stock (Eq, Show)

emptyOracleWorkCounts :: OracleWorkCounts
emptyOracleWorkCounts = OracleWorkCounts Map.empty

countOracleWork :: String -> String -> OracleWorkCounts -> OracleWorkCounts
countOracleWork family result (OracleWorkCounts counts) =
  forceTag family `seq` forceTag result `seq` OracleWorkCounts (Map.insertWith (+) (family, result) 1 counts)
  where
    -- A partly evaluated Show/takeWhile result can retain its source payload.
    -- Fully force both short names before they enter the persistent map.
    forceTag = foldl' (\() character -> character `seq` ()) ()

oracleWorkRows :: OracleWorkCounts -> [(String, String, Word64)]
oracleWorkRows (OracleWorkCounts counts) =
  [(family, result, count) | ((family, result), count) <- Map.toAscList counts]

-- Derived Show exposes the constructor before its payload. Taking only this
-- prefix neither traverses nor retains that payload, and emits ASCII JSON keys.
constructorTag :: (Show value) => value -> String
constructorTag = takeWhile (\character -> isAlphaNum character || character == '_') . show

oracleCommandWorkTag :: OracleCommand -> String
oracleCommandWorkTag = wrappedConstructorTag ["AdmissionCommand", "VoterCommand"]

wrappedConstructorTag :: (Show value) => [String] -> value -> String
wrappedConstructorTag wrappers value
  | name `elem` wrappers = name <> "_" <> takeWhile isName (dropWhile (not . isName) (drop (length name) rendered))
  | otherwise = name
  where
    rendered = show value
    isName character = isAlphaNum character || character == '_'
    name = takeWhile isName rendered

oracleOutcomeWorkTag :: OracleStepOutcome -> String
oracleOutcomeWorkTag outcome = case outcome of
  OracleCommitted receipt -> case oracleReceiptResult receipt of
    OracleAccepted -> "fresh_accepted"
    OracleRejected rejection -> "fresh_rejected_" <> rejectionTag rejection
  OracleDuplicate receipt -> case oracleReceiptResult receipt of
    OracleAccepted -> "duplicate_accepted"
    OracleRejected rejection -> "duplicate_rejected_" <> rejectionTag rejection
  OracleProtocolRejected problem -> "protocol_rejected_" <> constructorTag problem
  OracleRequestRetired retirement -> "request_retired_" <> constructorTag retirement
  OracleProgressRetired {} -> "progress_retired"
  OracleDeferred {} -> "deferred"
  where
    rejectionTag = wrappedConstructorTag ["FailureCommandRejected", "DisappearanceCommandRejected", "HeraldAdmissionCommandRejected", "VoterCommandRejected"]

-- This establishes a coherent installed-state snapshot, not completion of the
-- surrounding workload or publication of every non-advancing response.
oracleWorkSnapshotComplete :: ControlIndex -> ControlIndex -> OracleWorkCounts -> Bool
oracleWorkSnapshotComplete computed published counts =
  computed == published && any terminal (oracleWorkRows counts)
  where
    terminal (family, result, count) = family == "owner_exit" && result `elem` ["returned", "exception"] && count > 0
