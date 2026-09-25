-- | Admission control tails belong to the local serving obligation, not to
-- transferred source material. Only fresh Begin registers an obligation;
-- canonical closure or an active applicant's semantic cursor discharges it.
module Eclips.Herald.UseCase.JoinControlTails
  ( observeAdmission,
    releaseAdmission,
    releaseOwnActivation,
    releaseRetired,
    observeAcceptedPeer,
  )
where

import Eclips.Domain.Identity (ControlIndex, HeraldEpoch)
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldOracleTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupJoinState,
    startupGenesis,
    startupJoinState,
    startupOracleProjectionState,
  )
import Eclips.Oracle.Admission
  ( HeraldAdmissionPhase (..),
    HeraldAdmissionRecord,
    admissionManifestHeraldEpoch,
    admissionRecordBeginIndex,
    admissionRecordChangedIndex,
    admissionRecordId,
    admissionRecordManifest,
    admissionRecordPhase,
    admissionRecordPredecessor,
  )

observeAdmission :: HeraldAdmissionRecord -> HeraldState -> Either HeraldInvariantFault HeraldState
observeAdmission record state
  | AdmissionPreparing <- admissionRecordPhase record,
    admissionRecordChangedIndex record == admissionRecordBeginIndex record,
    local == applicant || local `elem` heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record) = do
      !retained <- either (const admissionFault) Right (Join.retainAdmissionControlTail record (startupJoinState state))
      pure (replaceStartupJoinState retained state)
  | otherwise = pure (releaseAdmission record state)
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)

-- A fenced owner can observe cancellation without consuming semantic work.
-- Merely observing Activate does not complete the applicant's reconstruction.
releaseAdmission :: HeraldAdmissionRecord -> HeraldState -> HeraldState
releaseAdmission record state = case admissionRecordPhase record of
  AdmissionCancelled {} -> releaseRecord record state
  _ -> state

-- Called after the applicant installs its membership base and starts ordinary
-- participation in the same pure successor. Donors await its accepted Hello.
releaseOwnActivation :: HeraldAdmissionRecord -> HeraldState -> HeraldState
releaseOwnActivation record state
  | admissionManifestHeraldEpoch (admissionRecordManifest record) == checkedLocalHeraldEpoch (startupGenesis state) = releaseRecord record state
  | otherwise = state

releaseRecord :: HeraldAdmissionRecord -> HeraldState -> HeraldState
releaseRecord record state =
  let !released = Join.releaseAdmissionControlTail (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) (startupJoinState state)
   in replaceStartupJoinState released state

releaseRetired :: HeraldEpoch -> HeraldState -> HeraldState
releaseRetired retired state = replaceStartupJoinState released state
  where
    retained = startupJoinState state
    localRetired = retired == checkedLocalHeraldEpoch (startupGenesis state)
    selected
      | localRetired = Join.admissionControlTails retained
      | otherwise = maybe [] pure (Join.lookupAdmissionControlTail retired retained)
    !released = foldl' (\current tailOwner -> Join.releaseAdmissionControlTail (Join.admissionControlTailAdmission tailOwner) (Join.admissionControlTailApplicant tailOwner) current) retained selected

-- Discovery has already accepted this exact current peer, including a
-- reoffered binding. The cursor is the peer's semantic applied prefix.
observeAcceptedPeer :: HeraldEpoch -> ControlIndex -> HeraldState -> Either HeraldInvariantFault HeraldState
observeAcceptedPeer applicant cursor state = case Join.lookupAdmissionControlTail applicant (startupJoinState state) of
  Nothing -> pure state
  Just retained -> do
    record <- maybe admissionFault Right (Projection.oracleViewHeraldAdmission (Join.admissionControlTailAdmission retained) (Projection.oracleView (startupOracleProjectionState state)))
    pure $ case admissionRecordPhase record of
      AdmissionActivated activated _ | cursor >= activated -> releaseRecord record state
      _ -> state

admissionFault :: Either HeraldInvariantFault value
admissionFault = Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)
