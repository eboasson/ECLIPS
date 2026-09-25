{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Content-derived identities for one effective sort-definition occurrence.
--
-- A sort keeps its public 'SortId' across retirement and redefinition.  The
-- occurrence base distinguishes the immutable genesis occurrence from a later
-- occurrence released by one resolved retirement.  Genesis is directly
-- constructible; the retirement smart constructor admits only positive applied
-- Oracle indices.
module Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    SortOccurrenceBaseProblem (..),
    resolvedRetirementOccurrenceBase,
    deriveSortDefinitionOccurrenceId,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    controlIndexWord64,
    mkSortDefinitionOccurrenceId,
    sortIdBytes,
    systemIdBytes,
  )
import GHC.Generics (Generic)

-- | The exact event that establishes a sort-definition occurrence.
--
-- Repeated publications before a resolved retirement use the same base and
-- therefore the same occurrence identity.  A retirement resolution supplies
-- its applied Oracle index as the base for the next occurrence.
data SortOccurrenceBase
  = Genesis
  | ResolvedRetirement ControlIndex
  deriving stock (Eq, Ord, Show)

data SortOccurrenceBaseProblem
  = ResolvedRetirementControlIndexMustBePositive
  deriving stock (Eq, Show)

-- | Admit the positive Oracle index of a resolved retirement.
--
-- Index zero names the immutable genesis projection and therefore cannot be a
-- retirement resolution.
resolvedRetirementOccurrenceBase ::
  ControlIndex ->
  Either SortOccurrenceBaseProblem SortOccurrenceBase
resolvedRetirementOccurrenceBase retirementIndex
  | controlIndexWord64 retirementIndex == 0 =
      Left ResolvedRetirementControlIndexMustBePositive
  | otherwise = Right (ResolvedRetirement retirementIndex)

-- | Derive the unique occurrence for one system, public sort, and checked base.
--
-- The genesis transcript intentionally retains its established encoding so
-- existing genesis occurrences and their golden vectors do not change.  The
-- resolved-retirement transcript has a separate domain tag and shape.
deriveSortDefinitionOccurrenceId ::
  SystemId ->
  SortId ->
  SortOccurrenceBase ->
  SortDefinitionOccurrenceId
deriveSortDefinitionOccurrenceId systemId sortId base =
  occurrenceInvariant
    "sort-definition occurrence identity"
    (mkSortDefinitionOccurrenceId (SHA256.hash transcript))
  where
    transcript = case base of
      Genesis ->
        Serialize.encode
          ( GenesisOccurrenceTranscript
              "ECLIPS-OCCURRENCE-GENESIS"
              (systemIdBytes systemId)
              (sortIdBytes sortId)
          )
      ResolvedRetirement retirementIndex ->
        Serialize.encode
          ( ResolvedRetirementOccurrenceTranscript
              "ECLIPS-OCCURRENCE-RESOLVED-RETIREMENT"
              (systemIdBytes systemId)
              (sortIdBytes sortId)
              (controlIndexWord64 retirementIndex)
          )

data GenesisOccurrenceTranscript
  = GenesisOccurrenceTranscript
      ByteString
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ResolvedRetirementOccurrenceTranscript
  = ResolvedRetirementOccurrenceTranscript
      ByteString
      ByteString
      ByteString
      Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

occurrenceInvariant :: (Show problem) => String -> Either problem value -> value
occurrenceInvariant context =
  either
    (error . (("invalid closed profile " <> context <> ": ") <>) . show)
    id
