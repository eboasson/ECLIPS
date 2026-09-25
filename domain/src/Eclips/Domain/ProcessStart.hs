-- | Minimal process admission facts. Identifier shape is checked by the domain
-- identifier constructors; freshness and active residence are admitted by Oracle.
-- The stable lifecycle request identity belongs to the Oracle request envelope.
module Eclips.Domain.ProcessStart
  ( ProcessStart,
    processStart,
    processStartProcessId,
    processStartProcessEpochId,
    processStartResidence,
  )
where

import Eclips.Domain.Identity (HeraldEpoch, ProcessEpochId, ProcessId)

data ProcessStart = ProcessStart ProcessId ProcessEpochId HeraldEpoch
  deriving stock (Eq, Ord, Show)

processStart :: ProcessId -> ProcessEpochId -> HeraldEpoch -> ProcessStart
processStart = ProcessStart

processStartProcessId :: ProcessStart -> ProcessId
processStartProcessId (ProcessStart process _ _) = process

processStartProcessEpochId :: ProcessStart -> ProcessEpochId
processStartProcessEpochId (ProcessStart _ epoch _) = epoch

processStartResidence :: ProcessStart -> HeraldEpoch
processStartResidence (ProcessStart _ _ residence) = residence
