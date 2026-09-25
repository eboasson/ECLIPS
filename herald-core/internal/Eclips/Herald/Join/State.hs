{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Join RPC receipts and independently retained semantic onboarding evidence.
module Eclips.Herald.Join.State
  ( State,
    emptyState,
    JoinRequestId,
    joinRequestId,
    joinRequestOwner,
    joinRequestOrdinal,
    lookupRequest,
    retainRequest,
    requestEntries,
    requestReceiptRetirement,
    requestIsRetired,
    retireRequestReceipts,
    retireClosedRequestReceipts,
    AdmissionControlTail,
    AdmissionControlTailProblem (..),
    admissionControlTailAdmission,
    admissionControlTailApplicant,
    admissionControlTailBegin,
    admissionControlTailExclusiveFloor,
    retainAdmissionControlTail,
    lookupAdmissionControlTail,
    admissionControlTails,
    admissionControlTailFloor,
    advanceAdmissionControlTailFloor,
    releaseAdmissionControlTail,
    lookupCapture,
    retainCapture,
    capturedHistories,
    retainInstalledHistory,
    installedHistories,
    replaceJoiningMaterial,
    releaseTransferHistories,
    retainImportedProvenance,
    importedPlanAcceptances,
    importedOccurrences,
    hasImportedObservation,
    retainImportedObservation,
    importedSystemViewObservations,
    importedSystemViewObservationCount,
  ) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, StructuralOccurrenceId)
import Eclips.Domain.Membership (HeraldAdmissionId)
import Eclips.Domain.Sort.Profile (PredefinedSortRole)
import Eclips.Herald.Alignment.Plan.Identity (AlignmentPlanId)
import Eclips.Herald.Alignment.Protocol (AlignmentPlanAccepted, RetainedPublicationEvidence, alignmentPlanAcceptedHerald, alignmentPlanAcceptedId, retainedPublicationEvidenceCanonicalBytes)
import Eclips.Herald.Graph.TerminalSource (TerminalStructuralOccurrence, terminalStructuralOccurrenceId)
import Eclips.Herald.OracleClient.Request (OracleRequestRef)
import Eclips.Oracle.Admission (HeraldAdmissionCommand, HeraldAdmissionRecord, admissionManifestHeraldEpoch, admissionRecordBeginIndex, admissionRecordId, admissionRecordManifest)
import Eclips.Public.Types.ReceiptRetirement

data JoinRequestId = JoinRequestId HeraldEpoch Word64 deriving stock (Eq, Ord, Show)
joinRequestId :: HeraldEpoch -> Word64 -> JoinRequestId
joinRequestId = JoinRequestId
joinRequestOwner :: JoinRequestId -> HeraldEpoch
joinRequestOwner (JoinRequestId owner _) = owner
joinRequestOrdinal :: JoinRequestId -> Word64
joinRequestOrdinal (JoinRequestId _ ordinal) = ordinal

-- | An outstanding local obligation to retain the contiguous control suffix
-- after this floor. The canonical Begin stays fixed when a joining receiver
-- advances its floor after a completed source import. No transfer bytes or
-- completed-admission ledger belong to this owner.
data AdmissionControlTail = AdmissionControlTail HeraldAdmissionId HeraldEpoch ControlIndex ControlIndex
  deriving stock (Eq, Show)

data AdmissionControlTailProblem
  = AdmissionControlTailIdentityMismatch HeraldEpoch
  | AdmissionControlTailMissing HeraldEpoch
  | AdmissionControlTailFloorRegressed HeraldEpoch ControlIndex ControlIndex
  deriving stock (Eq, Show)

admissionControlTailAdmission :: AdmissionControlTail -> HeraldAdmissionId
admissionControlTailAdmission (AdmissionControlTail admission _ _ _) = admission

admissionControlTailApplicant :: AdmissionControlTail -> HeraldEpoch
admissionControlTailApplicant (AdmissionControlTail _ applicant _ _) = applicant

admissionControlTailBegin :: AdmissionControlTail -> ControlIndex
admissionControlTailBegin (AdmissionControlTail _ _ begin _) = begin

admissionControlTailExclusiveFloor :: AdmissionControlTail -> ControlIndex
admissionControlTailExclusiveFloor (AdmissionControlTail _ _ _ after) = after

data State = State
  { requests :: Map JoinRequestId (HeraldAdmissionCommand, OracleRequestRef),
    captures :: Map (HeraldAdmissionId, Word64) ByteString,
    installed :: Map (HeraldAdmissionId, Word64, HeraldEpoch) ByteString,
    observations :: Map (PredefinedSortRole, ByteString) RetainedPublicationEvidence,
    retirement :: Map HeraldEpoch ReceiptRetirement,
    acceptances :: Map (AlignmentPlanId, HeraldEpoch) AlignmentPlanAccepted,
    occurrences :: Map StructuralOccurrenceId TerminalStructuralOccurrence,
    controlTails :: Map HeraldEpoch AdmissionControlTail
  }
  deriving stock (Eq, Show)

emptyState :: State
emptyState = State Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
lookupRequest :: JoinRequestId -> State -> Maybe (HeraldAdmissionCommand, OracleRequestRef)
lookupRequest key state = Map.lookup key state.requests
retainRequest :: JoinRequestId -> HeraldAdmissionCommand -> OracleRequestRef -> State -> State
retainRequest key command reference state = state {requests = Map.insert key (command, reference) state.requests}
requestEntries :: State -> [(JoinRequestId, HeraldAdmissionCommand, OracleRequestRef)]
requestEntries state = [(key, command, reference) | (key, (command, reference)) <- Map.toAscList state.requests]
requestReceiptRetirement :: HeraldEpoch -> State -> ReceiptRetirement
requestReceiptRetirement owner state = Map.findWithDefault mempty owner state.retirement
requestIsRetired :: JoinRequestId -> State -> Bool
requestIsRetired key state = receiptIsRetired (joinRequestOrdinal key) (requestReceiptRetirement (joinRequestOwner key) state)
retireRequestReceipts :: HeraldEpoch -> ReceiptRetirement -> (OracleRequestRef -> Bool) -> State -> Either () State
retireRequestReceipts owner supplied settled state
  | any (\(key, (_, reference)) -> selected key && not (settled reference)) (Map.toAscList state.requests) = Left ()
  | otherwise =
      Right
        state
          { requests = Map.filterWithKey (\key _ -> not (selected key)) state.requests,
            retirement = if combined == mempty then state.retirement else Map.insert owner combined state.retirement
          }
  where
    combined = requestReceiptRetirement owner state <> supplied
    selected key = joinRequestOwner key == owner && receiptIsRetired (joinRequestOrdinal key) combined

-- A committed terminal applicant lifetime releases its completed RPC receipts.
-- Pending Oracle work remains exceptional until its semantic result projects.
retireClosedRequestReceipts :: (HeraldEpoch -> Bool) -> (OracleRequestRef -> Bool) -> State -> Either () State
retireClosedRequestReceipts closed settled state = foldM release state owners
  where
    owners = Set.toAscList (Set.fromList [joinRequestOwner key | key <- Map.keys state.requests, closed (joinRequestOwner key)])
    release retained owner = do
      let owned = [(joinRequestOrdinal key, reference) | (key, (_, reference)) <- Map.toAscList retained.requests, joinRequestOwner key == owner]
          high = foldr (\(ordinal, _) prior -> max (Just ordinal) prior) (receiptRetirementHighWater (requestReceiptRetirement owner retained)) owned
          pending = Set.fromList [ordinal | (ordinal, reference) <- owned, not (settled reference)]
      progress <- either (const (Left ())) Right (receiptRetirement high pending)
      retireRequestReceipts owner progress settled retained

-- | Called for an admitted Begin, or for the receiver's own admitted base.
-- Later attempts keep the same obligation. Repeated registration cannot undo
-- a completed local rebase; after discharge the coordinator must not replay
-- an old Begin as fresh work.
retainAdmissionControlTail :: HeraldAdmissionRecord -> State -> Either AdmissionControlTailProblem State
retainAdmissionControlTail record state = case lookupAdmissionControlTail applicant state of
  Nothing -> Right state {controlTails = Map.insert applicant (AdmissionControlTail admission applicant begin begin) state.controlTails}
  Just previous
    | admissionControlTailAdmission previous == admission,
      admissionControlTailBegin previous == begin ->
        Right state
    | otherwise -> Left (AdmissionControlTailIdentityMismatch applicant)
  where
    admission = admissionRecordId record
    applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)
    begin = admissionRecordBeginIndex record

lookupAdmissionControlTail :: HeraldEpoch -> State -> Maybe AdmissionControlTail
lookupAdmissionControlTail applicant state = Map.lookup applicant state.controlTails

admissionControlTails :: State -> [AdmissionControlTail]
admissionControlTails state = Map.elems state.controlTails

admissionControlTailFloor :: State -> Maybe ControlIndex
admissionControlTailFloor = foldr minimumFloor Nothing . admissionControlTails
  where
    minimumFloor retained previous = Just (maybe after (min after) previous)
      where
        after = admissionControlTailExclusiveFloor retained

-- | The coordinator supplies the actual completed source-import cursor,
-- before replaying any newer local suffix. A lower cursor is a contradiction,
-- not an acknowledgement which may silently join by maximum.
advanceAdmissionControlTailFloor :: HeraldAdmissionId -> HeraldEpoch -> ControlIndex -> State -> Either AdmissionControlTailProblem State
advanceAdmissionControlTailFloor admission applicant after state = case lookupAdmissionControlTail applicant state of
  Nothing -> Left (AdmissionControlTailMissing applicant)
  Just (AdmissionControlTail identifier _ begin previous)
    | identifier /= admission -> Left (AdmissionControlTailIdentityMismatch applicant)
    | after < previous -> Left (AdmissionControlTailFloorRegressed applicant previous after)
    | after == previous -> Right state
    | otherwise -> Right state {controlTails = Map.insert applicant (AdmissionControlTail admission applicant begin after) state.controlTails}

-- | Completion, canonical cancellation or retirement discharges exactly this
-- applicant lifetime. Late or repeated release never removes another pin.
releaseAdmissionControlTail :: HeraldAdmissionId -> HeraldEpoch -> State -> State
releaseAdmissionControlTail admission applicant state = case lookupAdmissionControlTail applicant state of
  Just retained | admissionControlTailAdmission retained == admission -> state {controlTails = Map.delete applicant state.controlTails}
  _ -> state

lookupCapture :: HeraldAdmissionId -> Word64 -> State -> Maybe ByteString
lookupCapture admission attempt state = Map.lookup (admission, attempt) state.captures
retainCapture :: HeraldAdmissionId -> Word64 -> ByteString -> State -> State
retainCapture admission attempt bytes state = state {captures = Map.insert (admission, attempt) bytes state.captures}
capturedHistories :: State -> [((HeraldAdmissionId, Word64), ByteString)]
capturedHistories state = Map.toAscList state.captures
retainInstalledHistory :: HeraldAdmissionId -> Word64 -> HeraldEpoch -> ByteString -> State -> State
retainInstalledHistory admission attempt source bytes state = state {installed = Map.insert (admission, attempt, source) bytes state.installed}
installedHistories :: State -> [((HeraldAdmissionId, Word64, HeraldEpoch), ByteString)]
installedHistories state = Map.toAscList state.installed

-- | Adopt independently checked private staging material without rewinding
-- live RPC receipts, their retirement, local control-tail obligations or locally
-- frozen source captures. A donor's obligations never become the receiver's.
replaceJoiningMaterial :: State -> State -> State
replaceJoiningMaterial candidate current =
  current
    { installed = candidate.installed,
      observations = candidate.observations,
      acceptances = candidate.acceptances,
      occurrences = candidate.occurrences
    }

-- The predicate can close over the enclosing Herald state. Strict owner fields
-- evaluate the resulting map spines when the caller adopts the owner, so even
-- unobserved empty maps cannot retain old states through deferred filters.
-- Payloads remain shared; this does not recursively force the whole Herald.
releaseTransferHistories :: (HeraldAdmissionId -> Word64 -> Bool) -> State -> State
releaseTransferHistories released state =
  state
    { captures = Map.filterWithKey (\(admission, attempt) _ -> not (released admission attempt)) state.captures,
      installed = Map.filterWithKey (\(admission, attempt, _) _ -> not (released admission attempt)) state.installed
    }

-- These are semantic facts, stored once per identity. Their lifetime is distinct
-- from the potentially many transfer bundles which conveyed the same facts.
retainImportedProvenance :: [AlignmentPlanAccepted] -> [TerminalStructuralOccurrence] -> State -> Either String State
retainImportedProvenance accepted occurrences state = do
  retainedAcceptances <- foldM (insertChecked (\value -> (alignmentPlanAcceptedId value, alignmentPlanAcceptedHerald value))) state.acceptances accepted
  retainedOccurrences <- foldM (insertChecked terminalStructuralOccurrenceId) state.occurrences occurrences
  pure state {acceptances = retainedAcceptances, occurrences = retainedOccurrences}
  where
    insertChecked :: (Ord key, Eq value) => (value -> key) -> Map key value -> value -> Either String (Map key value)
    insertChecked key retained value = case Map.lookup (key value) retained of
      Nothing -> Right (Map.insert (key value) value retained)
      Just previous | previous == value -> Right retained
      _ -> Left "conflicting inherited join provenance"
importedPlanAcceptances :: State -> [AlignmentPlanAccepted]
importedPlanAcceptances state = Map.elems state.acceptances
importedOccurrences :: State -> [TerminalStructuralOccurrence]
importedOccurrences state = Map.elems state.occurrences

-- Canonical observations already re-admitted to the local private system view.
hasImportedObservation :: PredefinedSortRole -> RetainedPublicationEvidence -> State -> Bool
hasImportedObservation role evidence state = Map.member (role, retainedPublicationEvidenceCanonicalBytes evidence) state.observations
retainImportedObservation :: PredefinedSortRole -> RetainedPublicationEvidence -> State -> State
retainImportedObservation role evidence state = state {observations = Map.insert (role, retainedPublicationEvidenceCanonicalBytes evidence) evidence state.observations}
importedSystemViewObservations :: State -> [(PredefinedSortRole, RetainedPublicationEvidence)]
importedSystemViewObservations state = [(role, evidence) | ((role, _), evidence) <- Map.toAscList state.observations]

importedSystemViewObservationCount :: State -> Int
importedSystemViewObservationCount state = Map.size state.observations
