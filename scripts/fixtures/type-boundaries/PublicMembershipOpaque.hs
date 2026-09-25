{-# LANGUAGE CPP #-}

module PublicMembershipOpaque where

#if defined(HERALD_ADMISSION_ID_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldAdmissionId (..))
forged :: HeraldAdmissionId
forged = HeraldAdmissionId undefined undefined
#elif defined(HERALD_ADMISSION_ID_COERCION)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Eclips.Domain.Membership (HeraldAdmissionId)
forged :: HeraldAdmissionId -> ByteString
forged = coerce
#elif defined(HERALD_JOIN_RECIPE_CONSTRUCTOR)
import Eclips.Domain.Topology (HeraldJoinBaseRecipe (..))
forged :: HeraldJoinBaseRecipe
forged = HeraldJoinBaseRecipe undefined undefined undefined undefined undefined
#elif defined(HERALD_JOIN_RECIPE_RECORD_UPDATE)
import Eclips.Domain.Topology (HeraldJoinBaseRecipe, heraldJoinBaseRecipeApplicant)
forged :: HeraldJoinBaseRecipe -> HeraldJoinBaseRecipe
forged recipe = recipe {heraldJoinBaseRecipeApplicant = undefined}
#elif defined(ORACLE_ADMISSION_RECORD_CONSTRUCTOR)
import Eclips.Oracle.Admission (HeraldAdmissionRecord (..))
forged :: HeraldAdmissionRecord
forged = HeraldAdmissionRecord undefined
#elif defined(ORACLE_ADMISSION_RECORD_UPDATE)
import Eclips.Oracle.Admission (HeraldAdmissionRecord, admissionRecordPhase)
forged :: HeraldAdmissionRecord -> HeraldAdmissionRecord
forged record = record {admissionRecordPhase = undefined}
#elif defined(ORACLE_ADMISSION_INTERNAL_IMPORT)
import Eclips.Oracle.Internal.Admission (HeraldAdmissionRecord)
forged :: Maybe HeraldAdmissionRecord
forged = Nothing
#elif defined(HERALD_JOIN_OWNER_IMPORT)
import Eclips.Herald.Join.State (State)
forged :: Maybe State
forged = Nothing
#elif defined(MEMBERSHIP_GENERATION_ID_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldMembershipGenerationId (..))

forgedGenerationId :: HeraldMembershipGenerationId
forgedGenerationId = HeraldMembershipGenerationId undefined
#elif defined(MEMBERSHIP_HISTORY_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldMembershipHistory (..))

forgedHistory :: HeraldMembershipHistory
forgedHistory = HeraldMembershipHistory undefined
#elif defined(MEMBERSHIP_LINEAGE_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldMembershipLineage (..))

forgedLineage :: HeraldMembershipLineage
forgedLineage = HeraldMembershipLineage undefined
#elif defined(MEMBERSHIP_HISTORY_RECORD_UPDATE)
import Data.List.NonEmpty (NonEmpty)
import Eclips.Domain.Membership (HeraldMembershipGeneration, HeraldMembershipHistory, heraldMembershipHistoryGenerations)

invalidHistoryUpdate :: NonEmpty HeraldMembershipGeneration -> HeraldMembershipHistory -> HeraldMembershipHistory
invalidHistoryUpdate replacement history = history {heraldMembershipHistoryGenerations = replacement}
#elif defined(MEMBERSHIP_LINEAGE_RECORD_UPDATE)
import Eclips.Domain.Membership (HeraldMembershipGeneration, HeraldMembershipLineage, heraldMembershipLineageTarget)

invalidLineageUpdate :: HeraldMembershipGeneration -> HeraldMembershipLineage -> HeraldMembershipLineage
invalidLineageUpdate replacement lineage = lineage {heraldMembershipLineageTarget = replacement}
#elif defined(FAILURE_PROBE_ID_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldFailureProbeId (..))

forgedProbeId :: HeraldFailureProbeId
forgedProbeId = HeraldFailureProbeId undefined undefined
#elif defined(FAILURE_RESOLUTION_ID_CONSTRUCTOR)
import Eclips.Domain.Membership (FailureProbeResolutionId (..))

forgedResolutionId :: FailureProbeResolutionId
forgedResolutionId = FailureProbeResolutionId undefined undefined undefined
#elif defined(MEMBERSHIP_GENERATION_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldMembershipGeneration (..))

forgedGeneration :: HeraldMembershipGeneration
forgedGeneration = HeraldMembershipGeneration undefined undefined
#elif defined(MEMBERSHIP_GENERATION_BODY_CONSTRUCTOR)
import Eclips.Domain.Membership (HeraldMembershipGenerationBody (..))

forgedGenerationBody :: HeraldMembershipGenerationBody
forgedGenerationBody = GenesisHeraldMembership undefined undefined undefined
#elif defined(MEMBERSHIP_GENERATION_ID_COERCION)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)

invalidGenerationIdCoercion :: HeraldMembershipGenerationId -> ByteString
invalidGenerationIdCoercion = coerce
#elif defined(FAILURE_PROBE_ID_COERCION)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Eclips.Domain.Membership (HeraldFailureProbeId)

invalidProbeIdCoercion :: HeraldFailureProbeId -> ByteString
invalidProbeIdCoercion = coerce
#elif defined(MEMBERSHIP_GENERATION_RECORD_UPDATE)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationId,
  )

invalidGenerationUpdate ::
  HeraldMembershipGenerationId ->
  HeraldMembershipGeneration ->
  HeraldMembershipGeneration
invalidGenerationUpdate replacement generation =
  generation {heraldMembershipGenerationId = replacement}
#else
#error "select one membership opacity fixture"
#endif
