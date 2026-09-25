{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The administration owner for shutdown and dynamic logical Start.
--
-- This module retains the complete orderly-shutdown request and result plus
-- retry-stable dynamic process intentions and settlements. Neither path is
-- exposed as a new public EAPP operation in this increment.
module Eclips.Herald.Administration.State
  ( State,
    initialState,
    retainVoterAdministration,
    retainVoterPreparationRejection,
    voterAdministrationEntries,
    voterPreparationRejectionEntries,
    lookupVoterAdministration,
    settleVoterAdministration,
    preparationCancellationEntries,
    preparationCancellationIsFresh,
    PreparedPreparationCancellation,
    preparePreparationCancellation,
    commitPreparationCancellation,
    settlePreparationCancellation,
    DrainRequestRef,
    drainRequestRefDrainId,
    AdministrationShutdownStatus (..),
    AdministrationStateWitness,
    administrationStateWitness,
    administrationLocalHeraldEpoch,
    administrationWitnessBinding,
    administrationWitnessDeliveryLive,
    administrationWitnessNextDrainOrdinal,
    administrationWitnessShutdown,
    administrationWitnessNextConfiguredAcceptanceOrdinal,
    administrationWitnessConfiguredStarts,
    administrationWitnessCommands,
    administrationWitnessConfiguredStartEndSettlements,
    administrationWitnessConfiguredBinding,
    administrationWitnessNextConfiguredBindingGeneration,
    replaceAdministrationBindingForInvariantTest,
    replaceAdministrationNextDrainOrdinalForInvariantTest,
    AdministrationInvariant (..),
    PreparedOrderlyShutdown,
    prepareOrderlyShutdown,
    prepareConfiguredOrderlyShutdown,
    preparedDrainRequestRef,
    commitOrderlyShutdown,
    PreparedDrainCompletion,
    prepareDrainCompletion,
    preparedFinalAdminReply,
    commitDrainCompletion,
    observeAdministrationBindingLoss,
    administrationReceiptRetirement,
    retireAdministrationReceipts,
    retireClosedAdministrationReceipts,
    administrationCorrelationRetired,
    configuredStartRetiredOrigins,
    configuredStartApplicationAliases,
    retiredAcceptanceCount,

    -- * External configured-process administration connection
    PreparedConfiguredAdministrationOpen,
    prepareConfiguredAdministrationOpen,
    preparedConfiguredAdministrationBinding,
    commitConfiguredAdministrationOpen,
    currentConfiguredAdministrationBinding,
    configuredAdministrationBindingIsCurrent,

    -- * Package-internal configured-process start
    StartProcessEpochIntention,
    startProcessIntentionAcceptancePosition,
    startProcessIntentionOracleRequest,
    ConfiguredStartStatus (..),
    ConfiguredStartOutcome (..),
    lookupConfiguredStartStatus,
    configuredStartEntries,
    configuredStartProcessOrigins,
    configuredStartNextAcceptanceOrdinal,
    replaceConfiguredStartReadyAttachmentForInvariantTest,
    configuredStartAdminReply,
    configuredStartStatusResult,
    lookupAdministrationResultStatus,
    ConfiguredStartEndSettlement,
    configuredStartEndSettlementReference,
    configuredStartEndSettlementControlIndex,
    configuredStartEndSettlementReason,
    configuredStartEndSettlementEntries,
    configuredStartRequiresOracle,
    PreparedConfiguredStart,
    prepareStartProcessEpoch,
    prepareGatedStartProcessEpoch,
    preparedConfiguredStartOutcome,
    commitStartProcessEpoch,
    ConfiguredStartSettlement (..),
    ConfiguredStartSettlementClassification (..),
    PreparedConfiguredStartSettlement,
    prepareConfiguredStartSettlement,
    preparedConfiguredStartSettlementClassification,
    commitConfiguredStartSettlement,
    ConfiguredStartCompletionClassification (..),
    PreparedConfiguredStartCompletion,
    prepareConfiguredStartCompletion,
    preparedConfiguredStartCompletionClassification,
    commitConfiguredStartCompletion,
    ConfiguredStartProcessEndClassification (..),
    PreparedConfiguredStartProcessEnd,
    prepareConfiguredStartProcessEnd,
    preparedConfiguredStartProcessEndClassification,
    commitConfiguredStartProcessEnd,

    -- * Explicit process End
    EndProcessEpochIntention,
    endProcessIntentionAcceptancePosition,
    endProcessIntentionOracleRequest,
    EndProcessEpochStatus (..),
    EndProcessEpochOutcome (..),
    endProcessEpochEntries,
    lookupEndProcessEpochStatus,
    endProcessEpochRequiresOracle,
    PreparedEndProcessEpoch,
    prepareEndProcessEpoch,
    preparedEndProcessEpochOutcome,
    commitEndProcessEpoch,
    EndProcessEpochSettlement (..),
    EndProcessSettlementClassification (..),
    PreparedEndProcessSettlement,
    prepareEndProcessSettlement,
    preparedEndProcessSettlementClassification,
    commitEndProcessSettlement,
    EndProcessCompletionClassification (..),
    PreparedEndProcessCompletion,
    prepareEndProcessCompletion,
    commitEndProcessCompletion,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (ChildPreparation, LifecycleStatus (..))
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    AdministrationBinding,
    DrainId,
    FinalAdminReply (ConfiguredHeraldShutdownCompleted, OrderlyHeraldShutdownCompleted),
    administrationBindingHeraldEpoch,
  )
import Eclips.Herald.Administration.Internal
  ( AdminBindingGeneration (..),
    AdminError (..),
    AdminReply (..),
    AdminResultStatus (..),
    AdministrationCommand (..),
    AdministrationCommandDigest,
    ConfiguredStartAcceptancePosition,
    EndProcessEpochRequest,
    ProcessStartRequestDigest,
    StartProcessRequest,
    adminCorrelationBelongsTo,
    adminCorrelationIdWord64,
    administrationBinding,
    administrationCommandDigest,
    configuredStartAcceptancePosition,
    drainId,
    endProcessEpochCorrelation,
    endProcessEpochProcess,
    endProcessEpochReason,
    initialAdministrationBinding,
    startProcessCorrelation,
    startProcessRequestDigest,
  )
import Eclips.Herald.Application.Session (ApplicationAttachment)
import Eclips.Herald.ConfiguredProcess.State
  ( LiveProcessScaffoldRef,
    liveProcessScaffoldRefProcessEpoch,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.OracleClient.Request (OracleRequestRef)
import Eclips.Oracle.Canonical (CanonicalOracleReceipt)
import Eclips.Oracle.Receipt
  ( OracleRejection,
  )
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, receiptIsRetired)

-- | Immutable reference to the administration-owned shutdown record.
newtype DrainRequestRef = DrainRequestRef DrainId
  deriving stock (Eq, Show)

drainRequestRefDrainId :: DrainRequestRef -> DrainId
drainRequestRefDrainId (DrainRequestRef drain) = drain

data AdministrationShutdownStatus
  = AdministrationShutdownPending
  | AdministrationShutdownCompleted
  deriving stock (Eq, Show)

data ShutdownRecord = ShutdownRecord
  { correlation :: AdminCorrelationId,
    drain :: DrainId,
    status :: AdministrationShutdownStatus,
    configuredTarget :: Maybe AdministrationBinding
  }
  deriving stock (Eq)

data RetainedAdministrationCommand = RetainedAdministrationCommand
  { command :: AdministrationCommand,
    digest :: AdministrationCommandDigest
  }
  deriving stock (Eq, Show)

-- | State owned solely by the administration leaf.
data State = State
  { binding :: AdministrationBinding,
    deliveryLive :: Bool,
    nextDrainOrdinal :: Word64,
    shutdown :: Maybe ShutdownRecord,
    nextConfiguredAcceptanceOrdinal :: Word64,
    commands :: Map AdminCorrelationId RetainedAdministrationCommand,
    voterRequests :: Map AdminCorrelationId (OracleRequestRef, Maybe CanonicalOracleReceipt),
    voterCorrelations :: Map OracleRequestRef AdminCorrelationId,
    voterPreparationRejections :: Map AdminCorrelationId AdminError,
    configuredStarts :: Map AdminCorrelationId ConfiguredStartRecord,
    processEnds :: Map AdminCorrelationId EndProcessEpochRecord,
    preparationCancellations :: Map AdminCorrelationId (ChildPreparation, LifecycleStatus),
    configuredStartEndSettlements ::
      Map AdminCorrelationId ConfiguredStartEndSettlement,
    retiredStartOrigins :: Map ProcessEpochId (ControlIndex, OracleRequestRef, LiveProcessScaffoldRef, Maybe ApplicationAttachment),
    retiredAcceptances :: Word64,
    receiptRetirement :: ReceiptRetirement,
    configuredBinding :: Maybe AdministrationBinding,
    nextConfiguredBindingGeneration :: Word64
  }
  deriving stock (Eq)

initialState :: HeraldEpoch -> State
initialState herald =
  State
    { binding = initialAdministrationBinding herald,
      deliveryLive = True,
      nextDrainOrdinal = 1,
      shutdown = Nothing,
      nextConfiguredAcceptanceOrdinal = 1,
      commands = Map.empty,
      voterRequests = Map.empty,
      voterCorrelations = Map.empty,
      voterPreparationRejections = Map.empty,
      configuredStarts = Map.empty,
      processEnds = Map.empty,
      preparationCancellations = Map.empty,
      configuredStartEndSettlements = Map.empty,
      retiredStartOrigins = Map.empty,
      retiredAcceptances = 0,
      receiptRetirement = mempty,
      configuredBinding = Nothing,
      nextConfiguredBindingGeneration = 1
    }

-- | The immutable Herald epoch named by this owner's generation-zero binding.
-- Configured reconnects never replace this local identity.
administrationLocalHeraldEpoch :: State -> HeraldEpoch
administrationLocalHeraldEpoch = administrationBindingHeraldEpoch . (.binding)

-- | Read-only evidence used by composed phase invariants and properties.
data AdministrationStateWitness = AdministrationStateWitness
  { administrationWitnessBinding :: AdministrationBinding,
    administrationWitnessDeliveryLive :: Bool,
    administrationWitnessNextDrainOrdinal :: Word64,
    administrationWitnessShutdown ::
      Maybe (AdminCorrelationId, DrainId, AdministrationShutdownStatus),
    administrationWitnessNextConfiguredAcceptanceOrdinal :: Word64,
    administrationWitnessCommands ::
      [(AdminCorrelationId, AdministrationCommand, AdministrationCommandDigest)],
    administrationWitnessConfiguredStarts ::
      [ ( AdminCorrelationId,
          StartProcessRequest,
          ProcessStartRequestDigest,
          ConfiguredStartStatus
        )
      ],
    administrationWitnessConfiguredStartEndSettlements ::
      [(AdminCorrelationId, ConfiguredStartEndSettlement)],
    administrationWitnessConfiguredBinding :: Maybe AdministrationBinding,
    administrationWitnessNextConfiguredBindingGeneration :: Word64
  }
  deriving stock (Eq, Show)

administrationStateWitness :: State -> AdministrationStateWitness
administrationStateWitness state =
  AdministrationStateWitness
    { administrationWitnessBinding = state.binding,
      administrationWitnessDeliveryLive = state.deliveryLive,
      administrationWitnessNextDrainOrdinal = state.nextDrainOrdinal,
      administrationWitnessShutdown =
        (\record -> (record.correlation, record.drain, record.status))
          <$> state.shutdown,
      administrationWitnessNextConfiguredAcceptanceOrdinal =
        state.nextConfiguredAcceptanceOrdinal,
      administrationWitnessCommands =
        [ (correlation, retained.command, retained.digest)
        | (correlation, retained) <- Map.toAscList state.commands
        ],
      administrationWitnessConfiguredStarts = configuredStartEntries state,
      administrationWitnessConfiguredStartEndSettlements =
        configuredStartEndSettlementEntries state,
      administrationWitnessConfiguredBinding = state.configuredBinding,
      administrationWitnessNextConfiguredBindingGeneration =
        state.nextConfiguredBindingGeneration
    }

administrationWitnessBinding ::
  AdministrationStateWitness -> AdministrationBinding
administrationWitnessBinding witness = witness.administrationWitnessBinding

administrationWitnessDeliveryLive :: AdministrationStateWitness -> Bool
administrationWitnessDeliveryLive witness = witness.administrationWitnessDeliveryLive

administrationWitnessNextDrainOrdinal :: AdministrationStateWitness -> Word64
administrationWitnessNextDrainOrdinal witness = witness.administrationWitnessNextDrainOrdinal

administrationWitnessShutdown ::
  AdministrationStateWitness ->
  Maybe (AdminCorrelationId, DrainId, AdministrationShutdownStatus)
administrationWitnessShutdown witness = witness.administrationWitnessShutdown

administrationWitnessNextConfiguredAcceptanceOrdinal ::
  AdministrationStateWitness ->
  Word64
administrationWitnessNextConfiguredAcceptanceOrdinal witness =
  witness.administrationWitnessNextConfiguredAcceptanceOrdinal

administrationWitnessCommands ::
  AdministrationStateWitness ->
  [(AdminCorrelationId, AdministrationCommand, AdministrationCommandDigest)]
administrationWitnessCommands witness = witness.administrationWitnessCommands

administrationWitnessConfiguredStarts ::
  AdministrationStateWitness ->
  [ ( AdminCorrelationId,
      StartProcessRequest,
      ProcessStartRequestDigest,
      ConfiguredStartStatus
    )
  ]
administrationWitnessConfiguredStarts witness =
  witness.administrationWitnessConfiguredStarts

administrationWitnessConfiguredStartEndSettlements ::
  AdministrationStateWitness ->
  [(AdminCorrelationId, ConfiguredStartEndSettlement)]
administrationWitnessConfiguredStartEndSettlements witness =
  witness.administrationWitnessConfiguredStartEndSettlements

administrationWitnessConfiguredBinding ::
  AdministrationStateWitness ->
  Maybe AdministrationBinding
administrationWitnessConfiguredBinding witness =
  witness.administrationWitnessConfiguredBinding

administrationWitnessNextConfiguredBindingGeneration ::
  AdministrationStateWitness ->
  Word64
administrationWitnessNextConfiguredBindingGeneration witness =
  witness.administrationWitnessNextConfiguredBindingGeneration

-- | Narrow contradiction fixture for composed invariant properties.
replaceAdministrationBindingForInvariantTest ::
  AdministrationBinding -> State -> State
replaceAdministrationBindingForInvariantTest replacement state =
  state {binding = replacement}

-- | Narrow contradiction fixture for composed invariant properties.
replaceAdministrationNextDrainOrdinalForInvariantTest :: Word64 -> State -> State
replaceAdministrationNextDrainOrdinalForInvariantTest replacement state =
  state {nextDrainOrdinal = replacement}

-- | Contradictions between an opaque phase witness and its sole owner.
data AdministrationInvariant
  = AdministrationShutdownAlreadyAdmitted
  | AdministrationShutdownRecordMissing
  | AdministrationDrainReferenceMismatch
  | AdministrationShutdownAlreadyCompleted
  | AdministrationConfiguredStartOwnerMismatch
  | AdministrationConfiguredStartOracleRequestMissing
  | AdministrationConfiguredStartOracleRequestUnexpected
  | AdministrationPreparationCancellationRecordMissing
  | AdministrationConfiguredStartRecordMissing
  | AdministrationConfiguredStartOracleRequestMismatch
  | AdministrationConfiguredStartOracleResultConflict
  | AdministrationConfiguredStartProcessNotInstalled
  | AdministrationConfiguredStartCompletionConflict
  | AdministrationConfiguredStartEndMultipleRecords ProcessEpochId
  | AdministrationConfiguredStartEndEvidenceConflict ProcessEpochId
  deriving stock (Eq, Show)

newtype PreparedOrderlyShutdown
  = PreparedOrderlyShutdown (Prepared State DrainRequestRef)

-- | Prepare the sole current administration command.
--
-- A stale or no-longer-deliverable logical binding is an ordinary no-op. Once a
-- request has been admitted, another admission in the serving phase would contradict
-- the Herald phase and is therefore an invariant fault.
prepareOrderlyShutdown ::
  AdministrationBinding ->
  AdminCorrelationId ->
  State ->
  Either AdministrationInvariant (Maybe PreparedOrderlyShutdown)
prepareOrderlyShutdown candidate correlation state
  | candidate /= state.binding || not state.deliveryLive = Right Nothing
  | otherwise = case state.shutdown of
      Just _ -> Left AdministrationShutdownAlreadyAdmitted
      Nothing ->
        Just . PreparedOrderlyShutdown
          <$> prepareTransition admit state
  where
    admit predecessor =
      let allocatedDrain = drainId predecessor.nextDrainOrdinal
          reference = DrainRequestRef allocatedDrain
          successor =
            predecessor
              { nextDrainOrdinal = predecessor.nextDrainOrdinal + 1,
                shutdown =
                  Just
                    ShutdownRecord
                      { correlation = correlation,
                        drain = allocatedDrain,
                        status = AdministrationShutdownPending,
                        configuredTarget = Nothing
                      }
              }
       in Right (successor, reference)

-- | The checked public operator requests the same sole drain. It keeps its
-- delivery target separate from the private generation-zero runtime lease.
prepareConfiguredOrderlyShutdown :: AdministrationBinding -> AdminCorrelationId -> State -> Either AdministrationInvariant (Maybe PreparedOrderlyShutdown)
prepareConfiguredOrderlyShutdown candidate correlation state
  | not (configuredAdministrationBindingIsCurrent candidate state) = Right Nothing
  | otherwise = case state.shutdown of
      Just record
        | record.correlation == correlation && record.configuredTarget == Just candidate -> Right Nothing
        | otherwise -> Left AdministrationShutdownAlreadyAdmitted
      Nothing -> do
        prepared <-
          prepareTransition
            ( \predecessor ->
                let drain = drainId predecessor.nextDrainOrdinal
                    successor =
                      predecessor
                        { nextDrainOrdinal = predecessor.nextDrainOrdinal + 1,
                          shutdown = Just (ShutdownRecord correlation drain AdministrationShutdownPending (Just candidate))
                        }
                 in Right (successor, DrainRequestRef drain)
            )
            state
        Right (Just (PreparedOrderlyShutdown prepared))

preparedDrainRequestRef :: PreparedOrderlyShutdown -> DrainRequestRef
preparedDrainRequestRef (PreparedOrderlyShutdown prepared) =
  preparedOutput prepared

commitOrderlyShutdown ::
  PreparedOrderlyShutdown ->
  (State, DrainRequestRef)
commitOrderlyShutdown (PreparedOrderlyShutdown prepared) =
  commitPrepared prepared

newtype PreparedDrainCompletion
  = PreparedDrainCompletion (Prepared State (Maybe FinalAdminReply))

-- | Prepare the final immutable result for the exact retained drain request.
prepareDrainCompletion ::
  DrainRequestRef ->
  State ->
  Either AdministrationInvariant PreparedDrainCompletion
prepareDrainCompletion reference state =
  PreparedDrainCompletion <$> prepareTransition complete state
  where
    complete predecessor = case predecessor.shutdown of
      Nothing -> Left AdministrationShutdownRecordMissing
      Just record
        | record.drain /= drainRequestRefDrainId reference ->
            Left AdministrationDrainReferenceMismatch
        | record.status == AdministrationShutdownCompleted ->
            Left AdministrationShutdownAlreadyCompleted
        | otherwise ->
            let successor =
                  predecessor
                    { shutdown =
                        Just
                          (completeShutdownRecord record)
                    }
                reply = case record.configuredTarget of
                  Just target
                    | configuredAdministrationBindingIsCurrent target predecessor -> Just (ConfiguredHeraldShutdownCompleted target record.correlation)
                    | otherwise -> Nothing
                  Nothing
                    | predecessor.deliveryLive -> Just (OrderlyHeraldShutdownCompleted record.correlation)
                    | otherwise -> Nothing
             in Right (successor, reply)

preparedFinalAdminReply :: PreparedDrainCompletion -> Maybe FinalAdminReply
preparedFinalAdminReply (PreparedDrainCompletion prepared) =
  preparedOutput prepared

commitDrainCompletion ::
  PreparedDrainCompletion ->
  (State, Maybe FinalAdminReply)
commitDrainCompletion (PreparedDrainCompletion prepared) =
  commitPrepared prepared

newtype PreparedConfiguredAdministrationOpen
  = PreparedConfiguredAdministrationOpen
      (Prepared State AdministrationBinding)

-- | Allocate a fresh logical administration binding.  Opening a new
-- connection supersedes the previous external binding but cannot affect the
-- private generation-zero orderly-shutdown delivery lane.
prepareConfiguredAdministrationOpen ::
  State ->
  Either AdministrationInvariant PreparedConfiguredAdministrationOpen
prepareConfiguredAdministrationOpen state =
  PreparedConfiguredAdministrationOpen <$> prepareTransition prepare state
  where
    prepare predecessor =
      let candidate =
            administrationBinding
              (administrationBindingHeraldEpoch predecessor.binding)
              (AdminBindingGeneration predecessor.nextConfiguredBindingGeneration)
          successor =
            predecessor
              { configuredBinding = Just candidate,
                receiptRetirement = mempty,
                nextConfiguredBindingGeneration =
                  predecessor.nextConfiguredBindingGeneration + 1
              }
       in Right (successor, candidate)

preparedConfiguredAdministrationBinding ::
  PreparedConfiguredAdministrationOpen ->
  AdministrationBinding
preparedConfiguredAdministrationBinding
  (PreparedConfiguredAdministrationOpen prepared) = preparedOutput prepared

commitConfiguredAdministrationOpen ::
  PreparedConfiguredAdministrationOpen ->
  (State, AdministrationBinding)
commitConfiguredAdministrationOpen
  (PreparedConfiguredAdministrationOpen prepared) = commitPrepared prepared

currentConfiguredAdministrationBinding ::
  State ->
  Maybe AdministrationBinding
currentConfiguredAdministrationBinding state = state.configuredBinding

configuredAdministrationBindingIsCurrent ::
  AdministrationBinding ->
  State ->
  Bool
configuredAdministrationBindingIsCurrent candidate state =
  state.configuredBinding == Just candidate

-- | Clear delivery only for the currently live logical binding.
--
-- Duplicate and stale losses are deterministic no-ops.
observeAdministrationBindingLoss :: AdministrationBinding -> State -> State
observeAdministrationBindingLoss candidate state
  | candidate == state.binding && state.deliveryLive =
      state {deliveryLive = False}
  | state.configuredBinding == Just candidate =
      state {configuredBinding = Nothing, receiptRetirement = mempty}
  | otherwise = state

data StartProcessEpochIntention = StartProcessEpochIntention
  { acceptancePosition :: ConfiguredStartAcceptancePosition,
    oracleRequest :: OracleRequestRef
  }
  deriving stock (Eq, Show)

startProcessIntentionAcceptancePosition ::
  StartProcessEpochIntention ->
  ConfiguredStartAcceptancePosition
startProcessIntentionAcceptancePosition intention = intention.acceptancePosition

startProcessIntentionOracleRequest ::
  StartProcessEpochIntention ->
  OracleRequestRef
startProcessIntentionOracleRequest intention = intention.oracleRequest

data ConfiguredStartStatus
  = ConfiguredStartGated
  | ConfiguredStartAwaitingOracle StartProcessEpochIntention
  | ConfiguredStartOracleRejected
      StartProcessEpochIntention
      ControlIndex
      OracleRejection
  | ConfiguredStartProcessInstalled
      StartProcessEpochIntention
      ControlIndex
      LiveProcessScaffoldRef
  | ConfiguredStartReady
      StartProcessEpochIntention
      ControlIndex
      LiveProcessScaffoldRef
      ApplicationAttachment
  deriving stock (Eq, Show)

data ConfiguredStartRecord = ConfiguredStartRecord
  { request :: StartProcessRequest,
    digest :: ProcessStartRequestDigest,
    status :: ConfiguredStartStatus
  }
  deriving stock (Eq, Show)

-- | Terminal settlement for a configured Start whose process ended before
-- application attachment material became ready. The live Start/End result sum
-- projects this retained fact as @StartProcessEndedBeforeAttachment@.
data ConfiguredStartEndSettlement
  = ConfiguredStartEndSettlement
      LiveProcessScaffoldRef
      ControlIndex
      ProcessEndReason
  deriving stock (Eq, Show)

configuredStartEndSettlementReference ::
  ConfiguredStartEndSettlement ->
  LiveProcessScaffoldRef
configuredStartEndSettlementReference
  (ConfiguredStartEndSettlement reference _ _) = reference

configuredStartEndSettlementControlIndex ::
  ConfiguredStartEndSettlement ->
  ControlIndex
configuredStartEndSettlementControlIndex
  (ConfiguredStartEndSettlement _ index _) = index

configuredStartEndSettlementReason ::
  ConfiguredStartEndSettlement ->
  ProcessEndReason
configuredStartEndSettlementReason
  (ConfiguredStartEndSettlement _ _ reason) = reason

configuredStartEndSettlementEntries ::
  State ->
  [(AdminCorrelationId, ConfiguredStartEndSettlement)]
configuredStartEndSettlementEntries =
  Map.toAscList . (.configuredStartEndSettlements)

-- | Narrow cross-owner contradiction fixture for Ready-result validation.
replaceConfiguredStartReadyAttachmentForInvariantTest ::
  AdminCorrelationId ->
  ApplicationAttachment ->
  State ->
  State
replaceConfiguredStartReadyAttachmentForInvariantTest correlation replacement state =
  state
    { configuredStarts =
        Map.adjust replaceAttachment correlation state.configuredStarts
    }
  where
    replaceAttachment record = case record.status of
      ConfiguredStartReady intention index reference _ ->
        replaceConfiguredStartStatus
          (ConfiguredStartReady intention index reference replacement)
          record
      _ -> record

data ConfiguredStartOutcome
  = ConfiguredStartFirst ConfiguredStartStatus
  | ConfiguredStartExactRetry ConfiguredStartStatus
  | ConfiguredStartConflict
      AdministrationCommandDigest
      AdministrationCommandDigest
  deriving stock (Eq, Show)

configuredStartEntries ::
  State ->
  [ ( AdminCorrelationId,
      StartProcessRequest,
      ProcessStartRequestDigest,
      ConfiguredStartStatus
    )
  ]
configuredStartEntries state =
  [ (correlation, record.request, record.digest, record.status)
  | (correlation, record) <- Map.toAscList state.configuredStarts
  ]

-- | Retained configured-Start provenance for one projected process.  The list
-- shape preserves a detectable Administration invariant fault if more than one
-- accepted record ever names the same process epoch.
configuredStartProcessOrigins ::
  ProcessEpochId ->
  State ->
  [(ControlIndex, OracleRequestRef)]
configuredStartProcessOrigins process state =
  [ (index, startProcessIntentionOracleRequest intention)
  | (_, _, _, status) <- configuredStartEntries state,
    Just (index, reference) <- [configuredStartStatusOrigin status],
    Just intention <- [configuredStartStatusIntention status],
    liveProcessScaffoldRefProcessEpoch reference == process
  ]
    <> [(index, request) | Just (index, request, _, _) <- [Map.lookup process state.retiredStartOrigins]]

-- Semantic origin and live attachment facts outlive the caller's RPC receipt.
-- There is one entry per actual configured process, never per query or retry.
configuredStartRetiredOrigins :: State -> [(ProcessEpochId, ControlIndex, OracleRequestRef, LiveProcessScaffoldRef, Maybe ApplicationAttachment)]
configuredStartRetiredOrigins state = [(process, index, request, reference, attachment) | (process, (index, request, reference, attachment)) <- Map.toAscList state.retiredStartOrigins]

configuredStartApplicationAliases :: State -> [(ApplicationAttachment, ProcessEpochId)]
configuredStartApplicationAliases state =
  [(attachment, liveProcessScaffoldRefProcessEpoch reference) | (_, _, _, ConfiguredStartReady _ _ reference attachment) <- configuredStartEntries state]
    <> [(attachment, process) | (process, _, _, _, Just attachment) <- configuredStartRetiredOrigins state]

retiredAcceptanceCount :: State -> Word64
retiredAcceptanceCount state = state.retiredAcceptances

administrationReceiptRetirement :: State -> ReceiptRetirement
administrationReceiptRetirement state = state.receiptRetirement

administrationCorrelationRetired :: AdministrationBinding -> AdminCorrelationId -> State -> Bool
administrationCorrelationRetired binding correlation state =
  configuredAdministrationBindingIsCurrent binding state
    && adminCorrelationBelongsTo binding correlation
    && receiptIsRetired (adminCorrelationIdWord64 correlation) state.receiptRetirement

-- Both the progress join and receipt removal commit only after every selected
-- active operation has produced a terminal result. Queries own no result cell.
retireAdministrationReceipts :: AdministrationBinding -> ReceiptRetirement -> State -> Either () State
retireAdministrationReceipts binding progress state
  | not (configuredAdministrationBindingIsCurrent binding state) = Left ()
  | any (\correlation -> selected correlation && not (terminalResult correlation state)) (Map.keys state.commands) = Left ()
  | otherwise = Right (pruneAdministrationReceipts selected state {receiptRetirement = combined})
  where
    combined = state.receiptRetirement <> progress
    selected correlation = adminCorrelationBelongsTo binding correlation && receiptIsRetired (adminCorrelationIdWord64 correlation) combined

-- Closing or superseding the delivery lifetime does not cancel semantic work.
-- Its pending records finish normally and are released after their effects have
-- been derived by the enclosing transition.
retireClosedAdministrationReceipts :: State -> State
retireClosedAdministrationReceipts state = pruneAdministrationReceipts closedTerminal state
  where
    closedTerminal correlation =
      terminalResult correlation state
        && if adminCorrelationBelongsTo state.binding correlation
          then not state.deliveryLive
          else maybe True (\binding -> not (adminCorrelationBelongsTo binding correlation)) state.configuredBinding

terminalResult :: AdminCorrelationId -> State -> Bool
terminalResult correlation state = case lookupAdministrationResultStatus correlation state of
  Nothing -> False
  Just AdminRequestAccepted -> False
  Just (AdminPreparationCancellation (LifecyclePending _)) -> False
  Just _ -> True

pruneAdministrationReceipts :: (AdminCorrelationId -> Bool) -> State -> State
pruneAdministrationReceipts selected state =
  if Set.null retired
    then state
    else
      state
        { commands = keep state.commands,
          voterRequests = keep state.voterRequests,
          voterCorrelations = Map.filter (`Set.notMember` retired) state.voterCorrelations,
          voterPreparationRejections = keep state.voterPreparationRejections,
          configuredStarts = keep state.configuredStarts,
          processEnds = keep state.processEnds,
          preparationCancellations = keep state.preparationCancellations,
          configuredStartEndSettlements = keep state.configuredStartEndSettlements,
          retiredStartOrigins = Map.union state.retiredStartOrigins origins,
          retiredAcceptances = state.retiredAcceptances + fromIntegral (length retiredStarts + length retiredEnds)
        }
  where
    -- Every result owner belongs to one retained command. Compute this set
    -- once, then discard the selecting closure before returning the successor.
    -- Strict owner fields evaluate changed map spines and counters on adoption;
    -- no-op cleanup preserves the predecessor without constructing them again.
    retired = Map.keysSet (Map.filterWithKey (\correlation _ -> selected correlation) state.commands)
    keep :: Map AdminCorrelationId value -> Map AdminCorrelationId value
    keep = (`Map.withoutKeys` retired)
    retiredStarts = [intention | (correlation, record) <- Map.toAscList state.configuredStarts, Set.member correlation retired, Just intention <- [configuredStartStatusIntention record.status]]
    retiredEnds = [() | (correlation, _) <- Map.toAscList state.processEnds, Set.member correlation retired]
    origins =
      Map.fromList
        [ (liveProcessScaffoldRefProcessEpoch reference, (index, startProcessIntentionOracleRequest intention, reference, attachment))
        | (correlation, record) <- Map.toAscList state.configuredStarts,
          Set.member correlation retired,
          Just intention <- [configuredStartStatusIntention record.status],
          Just (index, reference) <- [configuredStartStatusOrigin record.status],
          let attachment = case record.status of ConfiguredStartReady _ _ _ value -> Just value; _ -> Nothing
        ]

lookupConfiguredStartStatus ::
  AdminCorrelationId ->
  State ->
  Maybe ConfiguredStartStatus
lookupConfiguredStartStatus correlation state =
  (.status) <$> Map.lookup correlation state.configuredStarts

configuredStartAdminReply ::
  AdminCorrelationId ->
  ConfiguredStartOutcome ->
  State ->
  AdminReply
configuredStartAdminReply correlation outcome state = case outcome of
  ConfiguredStartFirst _ -> retained
  ConfiguredStartExactRetry _ -> retained
  ConfiguredStartConflict {} -> ConflictingAdminResult correlation
  where
    retained = case lookupAdministrationResultStatus correlation state of
      Nothing -> AbsentAdminResult correlation
      Just status -> RetainedAdminResult correlation status

configuredStartStatusResult :: ConfiguredStartStatus -> AdminResultStatus
configuredStartStatusResult = \case
  ConfiguredStartGated -> AdminRequestAccepted
  ConfiguredStartAwaitingOracle {} -> AdminRequestAccepted
  ConfiguredStartOracleRejected _ index rejection ->
    AdminRequestRejected (AdminStartProcessNotApplied index rejection)
  ConfiguredStartProcessInstalled {} -> AdminRequestAccepted
  ConfiguredStartReady _ _ reference attachment ->
    AdminStartRequestCompleted
      (liveProcessScaffoldRefProcessEpoch reference)
      attachment

lookupAdministrationResultStatus ::
  AdminCorrelationId ->
  State ->
  Maybe AdminResultStatus
lookupAdministrationResultStatus correlation state =
  case Map.lookup correlation state.commands of
    Nothing -> Nothing
    Just retained -> case retained.command of
      StartAdministrationCommand _ ->
        case Map.lookup correlation state.configuredStartEndSettlements of
          Just settlement ->
            Just
              ( AdminStartRequestEndedBeforeAttachment
                  ( liveProcessScaffoldRefProcessEpoch
                      (configuredStartEndSettlementReference settlement)
                  )
              )
          Nothing ->
            configuredStartStatusResult . (.status)
              <$> Map.lookup correlation state.configuredStarts
      EndAdministrationCommand _ ->
        endProcessEpochStatusResult . (.status)
          <$> Map.lookup correlation state.processEnds
      VoterAdministrationCommand _ -> case Map.lookup correlation state.voterPreparationRejections of
        Just failure -> Just (AdminRequestRejected failure)
        Nothing -> (maybe AdminRequestAccepted AdminOracleVoterResult . snd) <$> Map.lookup correlation state.voterRequests
      CancelPreparationAdministrationCommand _ _ ->
        AdminPreparationCancellation . snd <$> Map.lookup correlation state.preparationCancellations

configuredStartNextAcceptanceOrdinal :: State -> Word64
configuredStartNextAcceptanceOrdinal state = state.nextConfiguredAcceptanceOrdinal

-- | Only an unseen target-local correlation allocates an identity pair and
-- an Oracle request. Exact retries and conflicting commands retain all supplies.
configuredStartRequiresOracle ::
  CheckedHeraldGenesis ->
  StartProcessRequest ->
  State ->
  Either AdministrationInvariant Bool
configuredStartRequiresOracle genesis request state = do
  validateConfiguredStartOwner genesis state
  let correlation = startProcessCorrelation request
  Right $ case Map.lookup correlation state.commands of
    Nothing -> True
    Just retained ->
      retained.command == StartAdministrationCommand request
        && lookupConfiguredStartStatus correlation state == Just ConfiguredStartGated

newtype PreparedConfiguredStart
  = PreparedConfiguredStart (Prepared State ConfiguredStartOutcome)

-- | Reserve only the command correlation while semantic membership is closed.
-- AdminRequestAccepted is the existing pending reply, not a process acceptance
-- position. No intention or generated process identity exists at this stage.
prepareGatedStartProcessEpoch :: CheckedHeraldGenesis -> StartProcessRequest -> State -> Either AdministrationInvariant PreparedConfiguredStart
prepareGatedStartProcessEpoch genesis request state = do
  validateConfiguredStartOwner genesis state
  PreparedConfiguredStart <$> prepareTransition retain state
  where
    correlation = startProcessCorrelation request
    command = StartAdministrationCommand request
    digest = administrationCommandDigest command
    retain predecessor = case Map.lookup correlation predecessor.commands of
      Just incumbent | incumbent.command /= command -> Right (predecessor, ConfiguredStartConflict incumbent.digest digest)
      Just _ -> do
        record <- maybe (Left AdministrationConfiguredStartRecordMissing) Right (Map.lookup correlation predecessor.configuredStarts)
        Right (predecessor, ConfiguredStartExactRetry record.status)
      Nothing ->
        Right
          ( predecessor
              { commands = Map.insert correlation (RetainedAdministrationCommand command digest) predecessor.commands,
                configuredStarts = Map.insert correlation (ConfiguredStartRecord request (startProcessRequestDigest request) ConfiguredStartGated) predecessor.configuredStarts
              },
            ConfiguredStartFirst ConfiguredStartGated
          )

prepareStartProcessEpoch ::
  CheckedHeraldGenesis ->
  StartProcessRequest ->
  Maybe OracleRequestRef ->
  State ->
  Either AdministrationInvariant PreparedConfiguredStart
prepareStartProcessEpoch genesis request oracleReference state =
  PreparedConfiguredStart <$> prepareTransition prepare state
  where
    correlation = startProcessCorrelation request
    attemptedCommand = StartAdministrationCommand request
    attemptedDigest = administrationCommandDigest attemptedCommand
    prepare predecessor =
      case Map.lookup correlation predecessor.commands of
        Just incumbent
          | incumbent.command == attemptedCommand -> do
              record <-
                maybe
                  (Left AdministrationConfiguredStartRecordMissing)
                  Right
                  (Map.lookup correlation predecessor.configuredStarts)
              if record.status == ConfiguredStartGated
                then prepareFirst predecessor
                else Right (predecessor, ConfiguredStartExactRetry record.status)
          | otherwise ->
              Right
                ( predecessor,
                  ConfiguredStartConflict incumbent.digest attemptedDigest
                )
        Nothing -> prepareFirst predecessor
    prepareFirst predecessor = do
      validateConfiguredStartOwner genesis predecessor
      case oracleReference of
        Nothing -> Left AdministrationConfiguredStartOracleRequestMissing
        Just reference -> retainAccepted reference predecessor
    retainAccepted reference predecessor =
      let intention =
            StartProcessEpochIntention
              { acceptancePosition =
                  configuredStartAcceptancePosition
                    predecessor.nextConfiguredAcceptanceOrdinal,
                oracleRequest = reference
              }
          status = ConfiguredStartAwaitingOracle intention
          record =
            ConfiguredStartRecord
              { request,
                digest = startProcessRequestDigest request,
                status
              }
       in Right
            ( predecessor
                { nextConfiguredAcceptanceOrdinal =
                    predecessor.nextConfiguredAcceptanceOrdinal + 1,
                  commands =
                    Map.insert
                      correlation
                      (RetainedAdministrationCommand attemptedCommand attemptedDigest)
                      predecessor.commands,
                  configuredStarts =
                    Map.insert correlation record predecessor.configuredStarts
                },
              ConfiguredStartFirst status
            )

validateConfiguredStartOwner ::
  CheckedHeraldGenesis -> State -> Either AdministrationInvariant ()
validateConfiguredStartOwner genesis state
  | administrationBindingHeraldEpoch state.binding == checkedLocalHeraldEpoch genesis = Right ()
  | otherwise = Left AdministrationConfiguredStartOwnerMismatch

preparedConfiguredStartOutcome ::
  PreparedConfiguredStart ->
  ConfiguredStartOutcome
preparedConfiguredStartOutcome (PreparedConfiguredStart prepared) =
  preparedOutput prepared

commitStartProcessEpoch ::
  PreparedConfiguredStart ->
  (State, ConfiguredStartOutcome)
commitStartProcessEpoch (PreparedConfiguredStart prepared) =
  commitPrepared prepared

data EndProcessEpochIntention = EndProcessEpochIntention
  { acceptancePosition :: ConfiguredStartAcceptancePosition,
    oracleRequest :: OracleRequestRef
  }
  deriving stock (Eq, Show)

endProcessIntentionAcceptancePosition ::
  EndProcessEpochIntention -> ConfiguredStartAcceptancePosition
endProcessIntentionAcceptancePosition intention = intention.acceptancePosition

endProcessIntentionOracleRequest :: EndProcessEpochIntention -> OracleRequestRef
endProcessIntentionOracleRequest intention = intention.oracleRequest

data EndProcessEpochStatus
  = EndProcessAwaitingOracle EndProcessEpochIntention
  | EndProcessOracleRejected
      EndProcessEpochIntention
      ControlIndex
      OracleRejection
  | EndProcessAwaitingRetirement
      EndProcessEpochIntention
      ControlIndex
      ProcessEpochId
      ProcessEndReason
  | EndProcessCompleted
      EndProcessEpochIntention
      ControlIndex
      ProcessEpochId
      ProcessEndReason
  deriving stock (Eq, Show)

data EndProcessEpochRecord = EndProcessEpochRecord
  { request :: EndProcessEpochRequest,
    status :: EndProcessEpochStatus
  }
  deriving stock (Eq, Show)

data EndProcessEpochOutcome
  = EndProcessEpochFirst EndProcessEpochStatus
  | EndProcessEpochExactRetry EndProcessEpochStatus
  | EndProcessEpochConflict AdministrationCommandDigest AdministrationCommandDigest
  deriving stock (Eq, Show)

endProcessEpochEntries ::
  State ->
  [(AdminCorrelationId, EndProcessEpochRequest, EndProcessEpochStatus)]
endProcessEpochEntries state =
  [ (correlation, record.request, record.status)
  | (correlation, record) <- Map.toAscList state.processEnds
  ]

lookupEndProcessEpochStatus ::
  AdminCorrelationId -> State -> Maybe EndProcessEpochStatus
lookupEndProcessEpochStatus correlation state =
  (.status) <$> Map.lookup correlation state.processEnds

endProcessEpochRequiresOracle ::
  EndProcessEpochRequest -> State -> Bool
endProcessEpochRequiresOracle request state =
  Map.notMember (endProcessEpochCorrelation request) state.commands

newtype PreparedEndProcessEpoch
  = PreparedEndProcessEpoch (Prepared State EndProcessEpochOutcome)

prepareEndProcessEpoch ::
  EndProcessEpochRequest ->
  Maybe OracleRequestRef ->
  State ->
  Either AdministrationInvariant PreparedEndProcessEpoch
prepareEndProcessEpoch request oracleReference state =
  PreparedEndProcessEpoch <$> prepareTransition prepare state
  where
    correlation = endProcessEpochCorrelation request
    attemptedCommand = EndAdministrationCommand request
    attemptedDigest = administrationCommandDigest attemptedCommand
    prepare predecessor =
      case Map.lookup correlation predecessor.commands of
        Just retained
          | retained.command == attemptedCommand -> do
              record <-
                maybe
                  (Left AdministrationConfiguredStartRecordMissing)
                  Right
                  (Map.lookup correlation predecessor.processEnds)
              case oracleReference of
                Nothing -> Right ()
                Just _ -> Left AdministrationConfiguredStartOracleRequestUnexpected
              Right (predecessor, EndProcessEpochExactRetry record.status)
          | otherwise ->
              Right
                ( predecessor,
                  EndProcessEpochConflict retained.digest attemptedDigest
                )
        Nothing -> do
          reference <-
            maybe
              (Left AdministrationConfiguredStartOracleRequestMissing)
              Right
              oracleReference
          let intention =
                EndProcessEpochIntention
                  { acceptancePosition =
                      configuredStartAcceptancePosition
                        predecessor.nextConfiguredAcceptanceOrdinal,
                    oracleRequest = reference
                  }
              status = EndProcessAwaitingOracle intention
              successor =
                predecessor
                  { nextConfiguredAcceptanceOrdinal =
                      predecessor.nextConfiguredAcceptanceOrdinal + 1,
                    commands =
                      Map.insert
                        correlation
                        (RetainedAdministrationCommand attemptedCommand attemptedDigest)
                        predecessor.commands,
                    processEnds =
                      Map.insert
                        correlation
                        EndProcessEpochRecord {request, status}
                        predecessor.processEnds
                  }
          Right (successor, EndProcessEpochFirst status)

preparedEndProcessEpochOutcome :: PreparedEndProcessEpoch -> EndProcessEpochOutcome
preparedEndProcessEpochOutcome (PreparedEndProcessEpoch prepared) = preparedOutput prepared

commitEndProcessEpoch ::
  PreparedEndProcessEpoch -> (State, EndProcessEpochOutcome)
commitEndProcessEpoch (PreparedEndProcessEpoch prepared) = commitPrepared prepared

data EndProcessEpochSettlement
  = EndProcessOracleWasRejected OracleRequestRef ControlIndex OracleRejection
  | EndProcessOracleWasAccepted
      OracleRequestRef
      ControlIndex
      ProcessEpochId
      ProcessEndReason
  deriving stock (Eq, Show)

data EndProcessSettlementClassification
  = EndProcessFirstSettled EndProcessEpochStatus
  | EndProcessSettlementExactRetry EndProcessEpochStatus
  deriving stock (Eq, Show)

newtype PreparedEndProcessSettlement
  = PreparedEndProcessSettlement
      (Prepared State EndProcessSettlementClassification)

prepareEndProcessSettlement ::
  AdminCorrelationId ->
  EndProcessEpochSettlement ->
  State ->
  Either AdministrationInvariant PreparedEndProcessSettlement
prepareEndProcessSettlement correlation settlement state =
  PreparedEndProcessSettlement <$> prepareTransition prepare state
  where
    prepare predecessor = do
      record <-
        maybe
          (Left AdministrationConfiguredStartRecordMissing)
          Right
          (Map.lookup correlation predecessor.processEnds)
      case record.status of
        EndProcessAwaitingOracle intention -> settleFirst predecessor record intention
        retained@(EndProcessOracleRejected intention index rejection) ->
          exactSettled
            predecessor
            retained
            (EndProcessOracleWasRejected intention.oracleRequest index rejection)
        retained@(EndProcessAwaitingRetirement intention index process reason) ->
          exactSettled
            predecessor
            retained
            ( EndProcessOracleWasAccepted
                intention.oracleRequest
                index
                process
                reason
            )
        EndProcessCompleted {} ->
          Left AdministrationConfiguredStartOracleResultConflict
    exactSettled predecessor retained retainedSettlement
      | retainedSettlement == settlement =
          Right (predecessor, EndProcessSettlementExactRetry retained)
      | otherwise = Left AdministrationConfiguredStartOracleResultConflict
    settleFirst predecessor record intention = do
      status <- case settlement of
        EndProcessOracleWasRejected request index rejection
          | request == intention.oracleRequest ->
              Right (EndProcessOracleRejected intention index rejection)
          | otherwise -> Left AdministrationConfiguredStartOracleRequestMismatch
        EndProcessOracleWasAccepted request index process reason
          | request == intention.oracleRequest
              && process == endProcessEpochProcess record.request
              && reason == endProcessEpochReason record.request ->
              Right
                ( EndProcessAwaitingRetirement
                    intention
                    index
                    process
                    reason
                )
          | otherwise -> Left AdministrationConfiguredStartOracleRequestMismatch
      let successor =
            predecessor
              { processEnds =
                  Map.insert
                    correlation
                    (replaceEndProcessEpochStatus status record)
                    predecessor.processEnds
              }
      Right (successor, EndProcessFirstSettled status)

preparedEndProcessSettlementClassification ::
  PreparedEndProcessSettlement -> EndProcessSettlementClassification
preparedEndProcessSettlementClassification
  (PreparedEndProcessSettlement prepared) = preparedOutput prepared

commitEndProcessSettlement ::
  PreparedEndProcessSettlement -> (State, EndProcessSettlementClassification)
commitEndProcessSettlement (PreparedEndProcessSettlement prepared) =
  commitPrepared prepared

data EndProcessCompletionClassification
  = EndProcessCompletionNoAdministrationRecord
  | EndProcessFirstCompleted AdminCorrelationId EndProcessEpochStatus
  | EndProcessCompletionExactRetry AdminCorrelationId EndProcessEpochStatus
  deriving stock (Eq, Show)

newtype PreparedEndProcessCompletion
  = PreparedEndProcessCompletion
      (Prepared State EndProcessCompletionClassification)

prepareEndProcessCompletion ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  State ->
  Either AdministrationInvariant PreparedEndProcessCompletion
prepareEndProcessCompletion process index reason state =
  PreparedEndProcessCompletion <$> prepareTransition prepare state
  where
    prepare predecessor = case matchingRecords predecessor of
      [] -> Right (predecessor, EndProcessCompletionNoAdministrationRecord)
      [(correlation, record)] -> case record.status of
        EndProcessAwaitingRetirement intention retainedIndex retainedProcess retainedReason
          | retainedIndex == index
              && retainedProcess == process
              && retainedReason == reason ->
              let status = EndProcessCompleted intention index process reason
                  successor =
                    predecessor
                      { processEnds =
                          Map.insert
                            correlation
                            (replaceEndProcessEpochStatus status record)
                            predecessor.processEnds
                      }
               in Right
                    ( successor,
                      EndProcessFirstCompleted correlation status
                    )
          | otherwise ->
              Left (AdministrationConfiguredStartEndEvidenceConflict process)
        completed@(EndProcessCompleted _ retainedIndex retainedProcess retainedReason)
          | retainedIndex == index
              && retainedProcess == process
              && retainedReason == reason ->
              Right
                ( predecessor,
                  EndProcessCompletionExactRetry correlation completed
                )
          | otherwise ->
              Left (AdministrationConfiguredStartEndEvidenceConflict process)
        _ -> Left AdministrationConfiguredStartProcessNotInstalled
      _ -> Left (AdministrationConfiguredStartEndMultipleRecords process)
    matchingRecords predecessor =
      [ (correlation, record)
      | (correlation, record) <- Map.toAscList predecessor.processEnds,
        endProcessEpochProcess record.request == process,
        case record.status of
          EndProcessAwaitingRetirement _ retainedIndex _ _ -> retainedIndex == index
          EndProcessCompleted _ retainedIndex _ _ -> retainedIndex == index
          _ -> False
      ]

commitEndProcessCompletion ::
  PreparedEndProcessCompletion ->
  (State, EndProcessCompletionClassification)
commitEndProcessCompletion (PreparedEndProcessCompletion prepared) =
  commitPrepared prepared

endProcessEpochStatusResult :: EndProcessEpochStatus -> AdminResultStatus
endProcessEpochStatusResult status = case status of
  EndProcessAwaitingOracle _ -> AdminRequestAccepted
  EndProcessOracleRejected _ index rejection ->
    AdminRequestRejected (AdminEndProcessNotApplied index rejection)
  EndProcessAwaitingRetirement {} -> AdminRequestAccepted
  EndProcessCompleted _ _ process _ -> AdminEndRequestCompleted process

replaceEndProcessEpochStatus ::
  EndProcessEpochStatus -> EndProcessEpochRecord -> EndProcessEpochRecord
replaceEndProcessEpochStatus replacement record =
  EndProcessEpochRecord record.request replacement

data ConfiguredStartSettlementClassification
  = ConfiguredStartFirstSettled ConfiguredStartStatus
  | ConfiguredStartSettlementExactRetry ConfiguredStartStatus
  deriving stock (Eq, Show)

data ConfiguredStartSettlement
  = ConfiguredStartOracleWasRejected
      OracleRequestRef
      ControlIndex
      OracleRejection
  | ConfiguredStartOracleWasAccepted
      OracleRequestRef
      ControlIndex
      LiveProcessScaffoldRef
  deriving stock (Eq, Show)

newtype PreparedConfiguredStartSettlement
  = PreparedConfiguredStartSettlement
      (Prepared State ConfiguredStartSettlementClassification)

prepareConfiguredStartSettlement ::
  AdminCorrelationId ->
  ConfiguredStartSettlement ->
  State ->
  Either AdministrationInvariant PreparedConfiguredStartSettlement
prepareConfiguredStartSettlement correlation settlement state =
  PreparedConfiguredStartSettlement <$> prepareTransition prepare state
  where
    prepare predecessor =
      case Map.lookup correlation predecessor.configuredStarts of
        Nothing -> Left AdministrationConfiguredStartRecordMissing
        Just record -> case record.status of
          ConfiguredStartGated -> Left AdministrationConfiguredStartOracleRequestMissing
          ConfiguredStartAwaitingOracle intention ->
            settleFirst predecessor record intention
          retained@(ConfiguredStartOracleRejected intention index rejection) ->
            exactSettled
              predecessor
              retained
              ( ConfiguredStartOracleWasRejected
                  intention.oracleRequest
                  index
                  rejection
              )
          retained@(ConfiguredStartProcessInstalled intention index reference) ->
            exactSettled
              predecessor
              retained
              ( ConfiguredStartOracleWasAccepted
                  intention.oracleRequest
                  index
                  reference
              )
          ConfiguredStartReady {} ->
            Left AdministrationConfiguredStartOracleResultConflict
    exactSettled predecessor retained retainedSettlement
      | retainedSettlement == settlement =
          Right
            ( predecessor,
              ConfiguredStartSettlementExactRetry retained
            )
      | otherwise = Left AdministrationConfiguredStartOracleResultConflict
    settleFirst predecessor record intention = do
      status <- case settlement of
        ConfiguredStartOracleWasRejected request index rejection
          | request == intention.oracleRequest ->
              Right (ConfiguredStartOracleRejected intention index rejection)
          | otherwise -> Left AdministrationConfiguredStartOracleRequestMismatch
        ConfiguredStartOracleWasAccepted request index reference
          | request == intention.oracleRequest ->
              Right (ConfiguredStartProcessInstalled intention index reference)
          | otherwise -> Left AdministrationConfiguredStartOracleRequestMismatch
      let successor =
            predecessor
              { configuredStarts =
                  Map.insert
                    correlation
                    (replaceConfiguredStartStatus status record)
                    predecessor.configuredStarts
              }
      Right (successor, ConfiguredStartFirstSettled status)

preparedConfiguredStartSettlementClassification ::
  PreparedConfiguredStartSettlement ->
  ConfiguredStartSettlementClassification
preparedConfiguredStartSettlementClassification
  (PreparedConfiguredStartSettlement prepared) = preparedOutput prepared

commitConfiguredStartSettlement ::
  PreparedConfiguredStartSettlement ->
  (State, ConfiguredStartSettlementClassification)
commitConfiguredStartSettlement (PreparedConfiguredStartSettlement prepared) =
  commitPrepared prepared

data ConfiguredStartCompletionClassification
  = ConfiguredStartFirstCompleted ConfiguredStartStatus
  | ConfiguredStartCompletionExactRetry ConfiguredStartStatus
  | ConfiguredStartCompletionSuppressedByEnd ConfiguredStartEndSettlement
  deriving stock (Eq, Show)

newtype PreparedConfiguredStartCompletion
  = PreparedConfiguredStartCompletion
      (Prepared State ConfiguredStartCompletionClassification)

-- | Advance an Oracle-accepted Start to usable application access after the
-- atomic local application installation. This leaf retains the exact linkage.
prepareConfiguredStartCompletion ::
  AdminCorrelationId ->
  LiveProcessScaffoldRef ->
  ApplicationAttachment ->
  State ->
  Either AdministrationInvariant PreparedConfiguredStartCompletion
prepareConfiguredStartCompletion correlation reference attachment state =
  PreparedConfiguredStartCompletion <$> prepareTransition prepare state
  where
    prepare predecessor =
      case Map.lookup correlation predecessor.configuredStartEndSettlements of
        Just settlement
          | configuredStartEndSettlementReference settlement == reference ->
              Right
                ( predecessor,
                  ConfiguredStartCompletionSuppressedByEnd settlement
                )
          | otherwise -> Left AdministrationConfiguredStartCompletionConflict
        Nothing ->
          case Map.lookup correlation predecessor.configuredStarts of
            Nothing -> Left AdministrationConfiguredStartRecordMissing
            Just record -> case record.status of
              ConfiguredStartProcessInstalled intention index retainedReference
                | retainedReference == reference ->
                    let status = ConfiguredStartReady intention index reference attachment
                        successor =
                          predecessor
                            { configuredStarts =
                                Map.insert
                                  correlation
                                  (replaceConfiguredStartStatus status record)
                                  predecessor.configuredStarts
                            }
                     in Right (successor, ConfiguredStartFirstCompleted status)
                | otherwise -> Left AdministrationConfiguredStartCompletionConflict
              retained@(ConfiguredStartReady _ _ retainedReference retainedAttachment)
                | retainedReference == reference && retainedAttachment == attachment ->
                    Right
                      ( predecessor,
                        ConfiguredStartCompletionExactRetry retained
                      )
                | otherwise -> Left AdministrationConfiguredStartCompletionConflict
              _ -> Left AdministrationConfiguredStartProcessNotInstalled

preparedConfiguredStartCompletionClassification ::
  PreparedConfiguredStartCompletion ->
  ConfiguredStartCompletionClassification
preparedConfiguredStartCompletionClassification
  (PreparedConfiguredStartCompletion prepared) = preparedOutput prepared

commitConfiguredStartCompletion ::
  PreparedConfiguredStartCompletion ->
  (State, ConfiguredStartCompletionClassification)
commitConfiguredStartCompletion (PreparedConfiguredStartCompletion prepared) =
  commitPrepared prepared

data ConfiguredStartProcessEndClassification
  = ConfiguredStartProcessEndNoRecord
  | ConfiguredStartFirstEndedBeforeAttachment
      AdminCorrelationId
      ConfiguredStartEndSettlement
  | ConfiguredStartProcessEndExactRetry
      AdminCorrelationId
      ConfiguredStartEndSettlement
  | ConfiguredStartProcessEndAfterAttachment AdminCorrelationId ConfiguredStartStatus
  | ConfiguredStartProcessEndRetiredBeforeAttachment
  | ConfiguredStartProcessEndRetiredAfterAttachment
  deriving stock (Eq, Show)

newtype PreparedConfiguredStartProcessEnd
  = PreparedConfiguredStartProcessEnd
      (Prepared State ConfiguredStartProcessEndClassification)

-- | Retain the private terminal Start result caused by a projected resident
-- End. No current public administration result is constructed here.
-- Retired RPC results retain their semantic attachment classification, while
-- the configured-process owner keeps the exact End-before-attachment evidence.
-- A process with no configured Start origin (for example a primordial resident)
-- is an ordinary no-op in this relation.
prepareConfiguredStartProcessEnd ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  State ->
  Either AdministrationInvariant PreparedConfiguredStartProcessEnd
prepareConfiguredStartProcessEnd process endIndex reason state =
  PreparedConfiguredStartProcessEnd <$> prepareTransition prepare state
  where
    prepare predecessor =
      case configuredStartsForProcess process predecessor of
        [] ->
          Right
            ( predecessor,
              case Map.lookup process predecessor.retiredStartOrigins of
                Nothing -> ConfiguredStartProcessEndNoRecord
                Just (_, _, _, Nothing) -> ConfiguredStartProcessEndRetiredBeforeAttachment
                Just (_, _, _, Just _) -> ConfiguredStartProcessEndRetiredAfterAttachment
            )
        [(correlation, status, reference)] ->
          settleOne predecessor correlation status reference
        _ -> Left (AdministrationConfiguredStartEndMultipleRecords process)

    settleOne predecessor correlation status reference =
      case status of
        ConfiguredStartProcessInstalled {} ->
          let attempted =
                ConfiguredStartEndSettlement reference endIndex reason
           in case Map.lookup correlation predecessor.configuredStartEndSettlements of
                Nothing ->
                  Right
                    ( predecessor
                        { configuredStartEndSettlements =
                            Map.insert
                              correlation
                              attempted
                              predecessor.configuredStartEndSettlements
                        },
                      ConfiguredStartFirstEndedBeforeAttachment
                        correlation
                        attempted
                    )
                Just retained
                  | retained == attempted ->
                      Right
                        ( predecessor,
                          ConfiguredStartProcessEndExactRetry
                            correlation
                            retained
                        )
                  | otherwise ->
                      Left (AdministrationConfiguredStartEndEvidenceConflict process)
        ready@ConfiguredStartReady {} ->
          case Map.lookup correlation predecessor.configuredStartEndSettlements of
            Nothing ->
              Right
                ( predecessor,
                  ConfiguredStartProcessEndAfterAttachment correlation ready
                )
            Just _ ->
              Left (AdministrationConfiguredStartEndEvidenceConflict process)
        _ -> Left AdministrationConfiguredStartProcessNotInstalled

preparedConfiguredStartProcessEndClassification ::
  PreparedConfiguredStartProcessEnd ->
  ConfiguredStartProcessEndClassification
preparedConfiguredStartProcessEndClassification
  (PreparedConfiguredStartProcessEnd prepared) = preparedOutput prepared

commitConfiguredStartProcessEnd ::
  PreparedConfiguredStartProcessEnd ->
  (State, ConfiguredStartProcessEndClassification)
commitConfiguredStartProcessEnd (PreparedConfiguredStartProcessEnd prepared) =
  commitPrepared prepared

configuredStartsForProcess ::
  ProcessEpochId ->
  State ->
  [(AdminCorrelationId, ConfiguredStartStatus, LiveProcessScaffoldRef)]
configuredStartsForProcess process state =
  [ (correlation, status, reference)
  | (correlation, record) <- Map.toAscList state.configuredStarts,
    let status = record.status,
    Just reference <- [configuredStartScaffoldReference status],
    liveProcessScaffoldRefProcessEpoch reference == process
  ]

configuredStartScaffoldReference ::
  ConfiguredStartStatus ->
  Maybe LiveProcessScaffoldRef
configuredStartScaffoldReference = \case
  ConfiguredStartProcessInstalled _ _ reference -> Just reference
  ConfiguredStartReady _ _ reference _ -> Just reference
  _ -> Nothing

configuredStartStatusOrigin ::
  ConfiguredStartStatus ->
  Maybe (ControlIndex, LiveProcessScaffoldRef)
configuredStartStatusOrigin status = case status of
  ConfiguredStartProcessInstalled _ index reference -> Just (index, reference)
  ConfiguredStartReady _ index reference _ -> Just (index, reference)
  _ -> Nothing

completeShutdownRecord :: ShutdownRecord -> ShutdownRecord
completeShutdownRecord (ShutdownRecord correlation drain _ target) = ShutdownRecord correlation drain AdministrationShutdownCompleted target

replaceConfiguredStartStatus ::
  ConfiguredStartStatus ->
  ConfiguredStartRecord ->
  ConfiguredStartRecord
replaceConfiguredStartStatus replacement (ConfiguredStartRecord request digest _) =
  ConfiguredStartRecord request digest replacement

configuredStartStatusIntention :: ConfiguredStartStatus -> Maybe StartProcessEpochIntention
configuredStartStatusIntention = \case
  ConfiguredStartGated -> Nothing
  ConfiguredStartAwaitingOracle intention -> Just intention
  ConfiguredStartOracleRejected intention _ _ -> Just intention
  ConfiguredStartProcessInstalled intention _ _ -> Just intention
  ConfiguredStartReady intention _ _ _ -> Just intention

preparationCancellationEntries :: State -> [(AdminCorrelationId, ChildPreparation, LifecycleStatus)]
preparationCancellationEntries state = [(correlation, reference, status) | (correlation, (reference, status)) <- Map.toAscList state.preparationCancellations]

preparationCancellationIsFresh :: AdminCorrelationId -> State -> Bool
preparationCancellationIsFresh correlation state = Map.notMember correlation state.commands

newtype PreparedPreparationCancellation = PreparedPreparationCancellation (Prepared State AdminReply)

-- | Reserve the ordinary administration correlation before publishing its
-- cancellation result. A retry can never change command kind or preparation.
preparePreparationCancellation :: AdminCorrelationId -> ChildPreparation -> LifecycleStatus -> State -> Either AdministrationInvariant PreparedPreparationCancellation
preparePreparationCancellation correlation reference status state =
  PreparedPreparationCancellation <$> prepareTransition transition state
  where
    command = CancelPreparationAdministrationCommand correlation reference
    digest = administrationCommandDigest command
    transition predecessor = case Map.lookup correlation predecessor.commands of
      Nothing -> Right (predecessor {commands = Map.insert correlation (RetainedAdministrationCommand command digest) predecessor.commands, preparationCancellations = Map.insert correlation (reference, status) predecessor.preparationCancellations}, RetainedAdminResult correlation (AdminPreparationCancellation status))
      Just retained
        | retained.command /= command -> Right (predecessor, ConflictingAdminResult correlation)
        | otherwise -> case Map.lookup correlation predecessor.preparationCancellations of
            Nothing -> Left AdministrationPreparationCancellationRecordMissing
            Just (_, retainedStatus) -> Right (predecessor, RetainedAdminResult correlation (AdminPreparationCancellation retainedStatus))

commitPreparationCancellation :: PreparedPreparationCancellation -> (State, AdminReply)
commitPreparationCancellation (PreparedPreparationCancellation prepared) = commitPrepared prepared

-- | Only pending cancellation results follow the common preparation owner;
-- an already settled winner remains the exact retained administration result.
settlePreparationCancellation :: AdminCorrelationId -> LifecycleStatus -> State -> Either AdministrationInvariant State
settlePreparationCancellation correlation status state = case Map.lookup correlation state.preparationCancellations of
  Nothing -> Left AdministrationPreparationCancellationRecordMissing
  Just (reference, LifecyclePending _) -> Right state {preparationCancellations = Map.insert correlation (reference, status) state.preparationCancellations}
  Just _ -> Right state

-- | Reserve the ordinary admin correlation together with its exact Oracle
-- request. Different command families cannot reuse an accepted correlation.
retainVoterAdministration :: AdminCorrelationId -> OracleRequestRef -> State -> Either AdministrationInvariant State
retainVoterAdministration correlation reference state = case Map.lookup correlation state.commands of
  Nothing ->
    Right
      state
        { commands = Map.insert correlation (RetainedAdministrationCommand command (administrationCommandDigest command)) state.commands,
          voterRequests = Map.insert correlation (reference, Nothing) state.voterRequests,
          voterCorrelations = Map.insert reference correlation state.voterCorrelations
        }
  Just retained | retained.command == command, Just (prior, _) <- Map.lookup correlation state.voterRequests, prior == reference -> Right state
  Just _ -> Left AdministrationConfiguredStartOracleRequestMismatch
  where
    command = VoterAdministrationCommand correlation

lookupVoterAdministration :: AdminCorrelationId -> State -> Maybe (OracleRequestRef, Maybe CanonicalOracleReceipt)
lookupVoterAdministration correlation = Map.lookup correlation . (.voterRequests)

settleVoterAdministration :: OracleRequestRef -> CanonicalOracleReceipt -> State -> Either AdministrationInvariant State
settleVoterAdministration reference receipt state = case Map.lookup reference state.voterCorrelations of
  Nothing -> Left AdministrationConfiguredStartOracleRequestMissing
  Just correlation -> case Map.lookup correlation state.voterRequests of
    Just (retained, Nothing) | retained == reference -> Right state {voterRequests = Map.insert correlation (reference, Just receipt) state.voterRequests}
    Just (retained, Just previous) | retained == reference && previous == receipt -> Right state
    _ -> Left AdministrationConfiguredStartOracleResultConflict

voterAdministrationEntries :: State -> [(AdminCorrelationId, OracleRequestRef, Maybe CanonicalOracleReceipt)]
voterAdministrationEntries state = [(correlation, reference, receipt) | (correlation, (reference, receipt)) <- Map.toAscList state.voterRequests]

voterPreparationRejectionEntries :: State -> [(AdminCorrelationId, AdminError)]
voterPreparationRejectionEntries = Map.toAscList . (.voterPreparationRejections)

-- | Local endpoint refusal is still an exact retained administration result;
-- it allocates no Oracle identity or native log entry.
retainVoterPreparationRejection :: AdminCorrelationId -> State -> (State, AdminReply)
retainVoterPreparationRejection correlation state = case Map.lookup correlation state.commands of
  Nothing ->
    (state {commands = Map.insert correlation (RetainedAdministrationCommand command (administrationCommandDigest command)) state.commands, voterPreparationRejections = Map.insert correlation failure state.voterPreparationRejections}, reply)
  Just retained | retained.command == command, Map.lookup correlation state.voterPreparationRejections == Just failure -> (state, reply)
  Just _ -> (state, ConflictingAdminResult correlation)
  where
    command = VoterAdministrationCommand correlation
    failure = AdminOracleReplicaEndpointsNotConfigured
    reply = RetainedAdminResult correlation (AdminRequestRejected failure)
