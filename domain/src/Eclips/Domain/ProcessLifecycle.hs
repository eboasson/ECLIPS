{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared pure vocabulary for process termination.
--
-- A transient transport failure is deliberately absent from this closed reason
-- type; permanent application loss is a Herald-owned recovery conclusion.
-- Label normalization is a read projection: retained labels are never rewritten
-- merely because their named process has ended.
module Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (..),
    ProcessEndReasonCanonicalProblem (..),
    processEndReasonCanonicalBytes,
    decodeProcessEndReasonCanonicalBytes,
    processEndReasonMayBeHeraldSubmitted,
    processEndReasonIsHeraldRetirement,
    effectiveProcessLabel,
    normalizeLabelForProcessEnd,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word8)
import Eclips.Domain.Identity (ProcessEpochId)
import Eclips.Domain.Value (Label, LabelOwner (..))
import GHC.Generics (Generic)

-- | The complete profile-0.1 process-end reason vocabulary.
data ProcessEndReason
  = ExplicitAdministrativeEnd
  | ApplicationPermanentlyLost
  | HeraldRetired
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data ProcessEndReasonCanonicalProblem
  = ProcessEndReasonCanonicalDecodeFailed String
  | ProcessEndReasonCanonicalWrongDomain ByteString
  | ProcessEndReasonCanonicalUnknownTag Word8
  deriving stock (Eq, Show)

-- | Stable canonical reason bytes embedded by EADM and Oracle transcripts.
-- Existing tag assignments are stable as the closed vocabulary grows.
processEndReasonCanonicalBytes :: ProcessEndReason -> ByteString
processEndReasonCanonicalBytes reason =
  Serialize.encode
    ( ProcessEndReasonTranscript
        processEndReasonDomain
        (processEndReasonTag reason)
    )

-- | Checked inverse of 'processEndReasonCanonicalBytes'.  Domain and tag
-- validation stay with the Domain owner; Oracle and EADM must not duplicate a
-- looser decoder for the same closed semantic value.
decodeProcessEndReasonCanonicalBytes ::
  ByteString -> Either ProcessEndReasonCanonicalProblem ProcessEndReason
decodeProcessEndReasonCanonicalBytes bytes = do
  ProcessEndReasonTranscript domain tag <-
    case Serialize.decode bytes of
      Left problem -> Left (ProcessEndReasonCanonicalDecodeFailed problem)
      Right transcript
        | Serialize.encode transcript == bytes -> Right transcript
        | otherwise ->
            Left
              ( ProcessEndReasonCanonicalDecodeFailed
                  "non-canonical encoding or trailing bytes"
              )
  if domain == processEndReasonDomain
    then Right ()
    else Left (ProcessEndReasonCanonicalWrongDomain domain)
  case tag of
    0 -> Right ExplicitAdministrativeEnd
    1 -> Right ApplicationPermanentlyLost
    2 -> Right HeraldRetired
    _ -> Left (ProcessEndReasonCanonicalUnknownTag tag)

processEndReasonDomain :: ByteString
processEndReasonDomain = "ECLIPS-PROCESS-END-REASON"

-- Keep the wire-independent semantic tag assignment explicit: adding or
-- reordering constructors must not silently renumber an existing reason.
processEndReasonTag :: ProcessEndReason -> Word8
processEndReasonTag ExplicitAdministrativeEnd = 0
processEndReasonTag ApplicationPermanentlyLost = 1
processEndReasonTag HeraldRetired = 2

-- | Whether a Herald may originate this reason through the ordinary live
-- process-End command.  Membership retirement is instead derived by the
-- Oracle membership transition that owns the retirement control index.
processEndReasonMayBeHeraldSubmitted :: ProcessEndReason -> Bool
processEndReasonMayBeHeraldSubmitted ExplicitAdministrativeEnd = True
processEndReasonMayBeHeraldSubmitted ApplicationPermanentlyLost = True
processEndReasonMayBeHeraldSubmitted HeraldRetired = False

-- | Whether this reason is the Oracle-derived consequence of retiring the
-- process's resident Herald. Keeping this classification with the closed
-- Domain vocabulary lets consumers validate retirement evidence without
-- importing or spelling the constructor which ordinary Herald commands must
-- not originate.
processEndReasonIsHeraldRetirement :: ProcessEndReason -> Bool
processEndReasonIsHeraldRetirement HeraldRetired = True
processEndReasonIsHeraldRetirement ExplicitAdministrativeEnd = False
processEndReasonIsHeraldRetirement ApplicationPermanentlyLost = False

-- | Project a stored label through the owner's canonical process-liveness view.
--
-- The predicate returns 'True' exactly for ended process epochs.  Void and
-- already-zombie labels are unchanged, making this projection idempotent.
effectiveProcessLabel :: (ProcessEpochId -> Bool) -> Label -> Label
effectiveProcessLabel isEnded label = case label of
  (ProcessLabel process, generation)
    | isEnded process -> (ZombieLabel process, generation)
  _ -> label

-- | Apply the projection for one exact newly ended process epoch.
normalizeLabelForProcessEnd :: ProcessEpochId -> Label -> Label
normalizeLabelForProcessEnd ended = effectiveProcessLabel (== ended)

data ProcessEndReasonTranscript
  = ProcessEndReasonTranscript ByteString Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)
