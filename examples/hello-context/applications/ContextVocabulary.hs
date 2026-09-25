{-# LANGUAGE OverloadedStrings #-}

-- | The greeting and its data-space graph. Context is a graph relationship;
-- there is no durability flag on the message sort.
module ContextVocabulary
  ( Message (..),
    contextEdges,
    greeting,
    messageQuery,
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Eclips.Application.Typed
import GHC.Generics (Generic)

-- | Schema, codec, fields and complete sort policy belong to this one type.
data Message = Message {message :: Text}
  deriving stock (Eq, Show, Generic)

instance ValueType Message

instance ApplicationSort Message where
  sortPolicy = regularPolicy [key (field @"message")]

-- | A preserving cycle makes every context delta reachable from every other
-- one. An ended holder leaves a neutral vertex in this parent-owned graph.
contextEdges :: NonEmpty vertex -> [(vertex, vertex)]
contextEdges vertices@(first :| rest) = zip (NonEmpty.toList vertices) (rest <> [first])

greeting :: Message
greeting = Message "Hello, world!"

messageQuery :: Delta Message -> Query Message
messageQuery reader = query reader (queryEqual (field @"message") "Hello, world!")
