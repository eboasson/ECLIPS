-- | Pure logical administration and drain correlations.
--
-- These values identify work inside one finite profile-0.1 run. They are not
-- credentials and carry no expiry, entropy, or rollover policy.
module Eclips.Herald.Administration
  ( AdministrationBinding,
    administrationBindingHeraldEpoch,
    administrationBindingGeneration,
    AdminBindingGeneration,
    adminBindingGenerationWord64,
    checkedInitialAdministrationBinding,
    AdminCorrelationId,
    adminCorrelationId,
    adminCorrelationIdForBinding,
    adminCorrelationBelongsTo,
    adminCorrelationIdWord64,
    StartProcessRequest,
    startProcessRequest,
    startProcessCorrelation,
    EndProcessEpochRequest,
    endProcessEpochRequest,
    endProcessEpochCorrelation,
    endProcessEpochProcess,
    endProcessEpochReason,
    AdminError (..),
    AdminResultStatus (..),
    AdminReply (..),
    AdminHeraldStatus (..),
    AdminServicePhase (..),
    AdminPreparationInspection (..),
    AdminPreparationPhase (..),
    DrainId,
    drainIdWord64,
    FinalAdminReply (..),
  )
where

import Eclips.Herald.Administration.Internal
  ( AdminBindingGeneration,
    AdminCorrelationId,
    AdminError (..),
    AdminHeraldStatus (..),
    AdminPreparationInspection (..),
    AdminPreparationPhase (..),
    AdminReply (..),
    AdminResultStatus (..),
    AdminServicePhase (..),
    AdministrationBinding,
    DrainId,
    EndProcessEpochRequest,
    FinalAdminReply (..),
    StartProcessRequest,
    adminBindingGenerationWord64,
    adminCorrelationBelongsTo,
    adminCorrelationId,
    adminCorrelationIdForBinding,
    adminCorrelationIdWord64,
    administrationBindingGeneration,
    administrationBindingHeraldEpoch,
    drainIdWord64,
    endProcessEpochCorrelation,
    endProcessEpochProcess,
    endProcessEpochReason,
    endProcessEpochRequest,
    initialAdministrationBinding,
    startProcessCorrelation,
    startProcessRequest,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
  )

-- | Obtain the same initial binding installed during checked initialization.
checkedInitialAdministrationBinding ::
  CheckedHeraldGenesis ->
  AdministrationBinding
checkedInitialAdministrationBinding =
  initialAdministrationBinding . checkedLocalHeraldEpoch
