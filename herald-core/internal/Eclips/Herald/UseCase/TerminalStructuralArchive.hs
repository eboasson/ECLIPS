{-# LANGUAGE LambdaCase #-}

-- | Freeze the exact retired-source structural payloads already admitted by a
-- survivor's publication owner.
--
-- This seam deliberately reads immutable incoming-publication evidence rather
-- than Graph progress: an occurrence may have reached only one survivor or may
-- still be held behind a causal gap when the source disappears.  The terminal
-- union must contain the union of those retained payloads without fabricating
-- anything known only to the retired Herald.
module Eclips.Herald.UseCase.TerminalStructuralArchive
  ( TerminalStructuralArchiveProblem (..),
    retainedRetiredSourceOccurrences,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity
  ( HeraldEpoch,
    StructuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
  )
import Eclips.Herald.Graph.TerminalSource
  ( TerminalSourceProblem,
    TerminalStructuralOccurrence,
    terminalStructuralOccurrence,
    terminalStructuralOccurrenceId,
  )
import Eclips.Herald.PeerPublication
  ( peerPublicationStructuralCanonicalBytes,
    peerPublicationStructuralStamp,
    structuralOccurrenceStampOccurrence,
  )
import Eclips.Herald.Publication.State qualified as Publication

data TerminalStructuralArchiveProblem
  = TerminalStructuralArchiveOccurrenceProblem TerminalSourceProblem
  | TerminalStructuralArchiveOccurrenceConflict StructuralOccurrenceId
  deriving stock (Eq, Show)

-- | Return the canonical, occurrence-ordered inventory payloads this survivor
-- actually retained from the retired source. Exact semantic repeats collapse;
-- a conflicting repeat is an invariant fault rather than a union choice.
retainedRetiredSourceOccurrences ::
  HeraldEpoch ->
  Publication.State ->
  Either TerminalStructuralArchiveProblem [TerminalStructuralOccurrence]
retainedRetiredSourceOccurrences retired publication =
  Map.elems
    <$> foldM retain Map.empty (Publication.incomingPublicationEntries publication)
  where
    retain archive (_, record) =
      let peerPublication = Publication.incomingPeerPublication record
       in case ( peerPublicationStructuralStamp peerPublication,
                 peerPublicationStructuralCanonicalBytes peerPublication
               ) of
            (Just stamp, Just payloadBytes)
              | Publication.incomingPublicationSemanticallyAuthenticated record,
                structuralOccurrenceSourceHeraldEpoch
                  (structuralOccurrenceStampOccurrence stamp)
                  == retired -> do
                  occurrence <-
                    either
                      (Left . TerminalStructuralArchiveOccurrenceProblem)
                      Right
                      (terminalStructuralOccurrence stamp payloadBytes)
                  insertOccurrence occurrence archive
            _ -> Right archive

    insertOccurrence occurrence archive =
      case Map.lookup identifier archive of
        Nothing -> Right (Map.insert identifier occurrence archive)
        Just incumbent
          | incumbent == occurrence -> Right archive
          | otherwise ->
              Left (TerminalStructuralArchiveOccurrenceConflict identifier)
      where
        identifier = terminalStructuralOccurrenceId occurrence
