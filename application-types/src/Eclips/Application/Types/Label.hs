{-# LANGUAGE DeriveAnyClass #-}

-- | Application-private label targets and terminal results.
--
-- The boundary deliberately carries only private process handles.  Zombie is
-- an observed semantic state, never a caller-selected target, and detailed
-- post-acceptance Oracle reasons remain internal to the Herald.
module Eclips.Application.Types.Label
  ( ApplicationLabelTarget (..),
    LabelResult (..),
  )
where

import Data.Binary (Binary)
import Eclips.Application.Types.Identity (PrivateProcessId)
import GHC.Generics (Generic)

data ApplicationLabelTarget
  = LabelToProcess PrivateProcessId
  | LabelToVoid
  | LabelToDelete
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data LabelResult
  = LabelApplied
  | LabelNotApplied
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
