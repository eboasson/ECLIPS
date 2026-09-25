{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure Start/End administration coordination.
--
-- The externally authenticated administration binding is deliberately
-- separate from the private generation-zero orderly-shutdown lane.  Every
-- accepted Start is retained before any Oracle submission effect is exposed,
-- and query/retry/reconnect can therefore reoffer only owner-derived facts.
module Eclips.Herald.UseCase.Administration
  ( applyConfiguredAdministrationIngress,
    advancePreparationCancellations,
    advanceGatedStarts,
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Application.Types.Lifecycle (LifecycleStatus (..))
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs, heraldMembershipGenerationId)
import Eclips.Domain.ProcessStart (processStartProcessEpochId)
import Eclips.Herald.Administration
  ( AdminReply (..),
    startProcessCorrelation,
  )
import Eclips.Herald.Administration qualified as AdministrationValue
import Eclips.Herald.Administration.Internal qualified as AdministrationInternal
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.ConfiguredProcess.Start qualified as ConfiguredStart
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    EffectBatch,
    HeraldEffect (..),
    emptyEffectBatch,
    orderedEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedSystemId)
import Eclips.Herald.Input (AdministrationIngress (..))
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient (oracleBindingContact, oracleContactNode)
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.ProcessPreparation.Protocol qualified as Remote
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldAdministrationTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldPhase (..),
    HeraldState,
    heraldPhase,
    replaceStartupAdministrationState,
    replaceStartupConfiguredProcessState,
    replaceStartupIdGeneratorState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    startupAdministrationState,
    startupApplicationState,
    startupConfiguredProcessState,
    startupControlOracleProjectionState,
    startupGenesis,
    startupIdGeneratorState,
    startupIsolationState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupProcessPreparationState,
  )
import Eclips.Herald.UseCase.ProcessPreparation qualified as ProcessPreparation
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch)
import Eclips.Oracle.Voter qualified as Voter

-- | Apply one executable Start/End administration input.  The private shutdown arm
-- remains owned by 'Eclips.Herald.Transition' and is an invariant fault here.
applyConfiguredAdministrationIngress ::
  AdministrationIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyConfiguredAdministrationIngress ingress predecessor = case ingress of
  RetireAdministrationReceipts {} -> administrationFault
  GetOracleConfiguration binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise ->
        let current = Projection.oracleView (startupControlOracleProjectionState predecessor)
         in checkedSuccess predecessor (singletonEffectBatch (SendAdministrationReply binding (AdminOracleConfigurationReply correlation (Projection.oracleViewControlIndex current) (Projection.oracleViewVoterConfiguration current) (Projection.oracleViewOracleReplicas current) (Projection.oracleViewPendingVoterChange current))))
  GetVoterChangeStatus binding correlation change
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise ->
        let current = Projection.oracleView (startupControlOracleProjectionState predecessor)
         in checkedSuccess predecessor (singletonEffectBatch (SendAdministrationReply binding (AdminVoterChangeStatusReply correlation (Projection.oracleViewControlIndex current) (Projection.oracleViewVoterChange change current))))
  PrepareOracleReplica binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise -> case OracleClient.oracleClientLocalReplica (startupOracleClientState predecessor) of
        Nothing ->
          let (retained, reply) = Administration.retainVoterPreparationRejection correlation administration
           in checkedSuccess (replaceStartupAdministrationState retained predecessor) (singletonEffectBatch (SendAdministrationReply binding reply))
        Just (node, contact) -> voterOperation binding correlation (OracleClient.RegisterOracleReplica node (checkedLocalHeraldEpoch (startupGenesis predecessor)) contact)
  BeginVoterChange binding correlation expected reason bindings ->
    voterOperation binding correlation (OracleClient.BeginOracleVoterChange expected reason bindings)
  CancelVoterChange binding correlation change ->
    voterOperation binding correlation (OracleClient.CancelOracleVoterChange change)
  GetHeraldStatus binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise -> checkedSuccess predecessor (singletonEffectBatch (SendAdministrationReply binding (AdministrationValue.AdminHeraldStatusReply correlation (heraldStatus predecessor))))
  ListChildPreparations binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise -> checkedSuccess predecessor (singletonEffectBatch (SendAdministrationReply binding (AdministrationValue.AdminPreparationsReply correlation (inspectPreparations predecessor))))
  DrainConfiguredHerald {} -> administrationFault
  OpenAdministrationConnection candidate deployment
    | deployment /= checkedSystemId (startupGenesis predecessor) ->
        checkedSuccess
          predecessor
          ( singletonEffectBatch
              ( RejectAdministrationConnection
                  (CandidateAdministrationDisposition candidate)
              )
          )
    | otherwise ->
        case Administration.prepareConfiguredAdministrationOpen administration of
          Left _ -> administrationFault
          Right prepared ->
            let (successorAdministration, binding) =
                  Administration.commitConfiguredAdministrationOpen prepared
             in checkedSuccess
                  ( replaceStartupAdministrationState
                      successorAdministration
                      predecessor
                  )
                  ( singletonEffectBatch
                      (SetAdministrationConnectionDisposition candidate binding)
                  )
  StartProcessEpoch binding request
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) ->
        rejectEstablished binding
    | Application.applicationMembershipGateClosed (startupApplicationState predecessor) ->
        case Administration.prepareGatedStartProcessEpoch (startupGenesis predecessor) request administration of
          Left _ -> administrationFault
          Right prepared ->
            let (owner, outcome) = Administration.commitStartProcessEpoch prepared
                reply = Administration.configuredStartAdminReply (startProcessCorrelation request) outcome owner
             in checkedSuccess (replaceStartupAdministrationState owner predecessor) (singletonEffectBatch (SendAdministrationReply binding reply))
    | otherwise ->
        case ConfiguredStart.prepareConfiguredStartRequest
          (startupGenesis predecessor)
          request
          configuredOwners of
          Left _ -> administrationFault
          Right prepared ->
            let (successorOwners, outcome) =
                  ConfiguredStart.commitConfiguredStartRequest prepared
                successor = installConfiguredOwners successorOwners predecessor
                reply =
                  Administration.configuredStartAdminReply
                    (startProcessCorrelation request)
                    outcome
                    (ConfiguredStart.configuredStartAdministration successorOwners)
                oracleActions
                  | configuredStartOutcomeMayRequireDispatch outcome =
                      OracleClient.oracleClientRequestActions
                        (ConfiguredStart.configuredStartOracleClient successorOwners)
                  | otherwise = []
             in checkedSuccess
                  successor
                  ( orderedEffectBatch
                      ( SendAdministrationReply binding reply
                          : fmap RunOracleClientAction oracleActions
                      )
                  )
  EndProcessEpoch binding request
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) ->
        rejectEstablished binding
    | otherwise ->
        case prepareEnd request predecessor of
          Left _ -> administrationFault
          Right (successor, outcome) ->
            let correlation = AdministrationValue.endProcessEpochCorrelation request
                reply = case outcome of
                  Administration.EndProcessEpochConflict {} ->
                    ConflictingAdminResult correlation
                  _ -> case Administration.lookupAdministrationResultStatus
                    correlation
                    (startupAdministrationState successor) of
                    Nothing -> AbsentAdminResult correlation
                    Just status -> RetainedAdminResult correlation status
                oracleActions
                  | endProcessOutcomeMayRequireDispatch outcome =
                      OracleClient.oracleClientRequestActions
                        (startupOracleClientState successor)
                  | otherwise = []
             in checkedSuccess
                  successor
                  ( orderedEffectBatch
                      ( SendAdministrationReply binding reply
                          : fmap RunOracleClientAction oracleActions
                      )
                  )
  CancelChildPreparation binding correlation reference
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) -> rejectEstablished binding
    | otherwise -> do
        (sealed, status) <-
          if Administration.preparationCancellationIsFresh correlation administration
            then ProcessPreparation.requestPreparationCancellation reference predecessor
            else Right (predecessor, LifecyclePending [])
        prepared <- either (const administrationFault) Right (Administration.preparePreparationCancellation correlation reference status (startupAdministrationState sealed))
        let (retained, reply) = Administration.commitPreparationCancellation prepared
        checkedSuccess (replaceStartupAdministrationState retained sealed) (singletonEffectBatch (SendAdministrationReply binding reply))
  GetAdministrationResult binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding administration) ->
        rejectEstablished binding
    | otherwise ->
        let reply = case Administration.lookupAdministrationResultStatus correlation administration of
              Nothing -> AbsentAdminResult correlation
              Just status -> RetainedAdminResult correlation status
         in checkedSuccess
              predecessor
              (singletonEffectBatch (SendAdministrationReply binding reply))
  OrderlyHeraldShutdown {} -> administrationFault
  where
    administration = startupAdministrationState predecessor
    voterOperation binding correlation operation
      | not (Administration.configuredAdministrationBindingIsCurrent binding administration) = rejectEstablished binding
      | not (Projection.oracleViewLocalHeraldIsCurrent (Projection.oracleView (startupOracleProjectionState predecessor))) = rejectEstablished binding
      | Administration.lookupAdministrationResultStatus correlation administration /= Nothing,
        Administration.lookupVoterAdministration correlation administration == Nothing =
          conflict
      | otherwise = case OracleClient.prepareVoterAdministrationRequest key operation (startupOracleClientState predecessor) of
          Left (OracleClient.OracleRequestConflict _) -> conflict
          Left _ -> administrationFault
          Right prepared -> do
            let (client, _, reference) = OracleClient.commitOracleRequest prepared
            retained <- either (const administrationFault) Right (Administration.retainVoterAdministration correlation reference administration)
            let successor = replaceStartupOracleClientState client (replaceStartupAdministrationState retained predecessor)
                result = maybe AdministrationValue.AdminRequestAccepted id (Administration.lookupAdministrationResultStatus correlation retained)
            checkedSuccess successor (orderedEffectBatch (SendAdministrationReply binding (RetainedAdminResult correlation result) : map RunOracleClientAction (OracleClient.oracleClientRequestActions client)))
      where
        key = AdministrationInternal.administrationCommandDigestBytes (AdministrationInternal.administrationCommandDigest (AdministrationInternal.VoterAdministrationCommand correlation))
        conflict = checkedSuccess predecessor (singletonEffectBatch (SendAdministrationReply binding (ConflictingAdminResult correlation)))
    configuredOwners =
      ConfiguredStart.configuredStartOwners
        administration
        (startupOracleClientState predecessor)
        (startupOracleProjectionState predecessor)
        (startupConfiguredProcessState predecessor)
        (startupIdGeneratorState predecessor)
    rejectEstablished binding =
      checkedSuccess
        predecessor
        ( singletonEffectBatch
            ( RejectAdministrationConnection
                (EstablishedAdministrationDisposition binding)
            )
        )

heraldStatus :: HeraldState -> AdministrationValue.AdminHeraldStatus
heraldStatus state =
  AdministrationValue.AdminHeraldStatus
    (checkedSystemId (startupGenesis state))
    (checkedLocalHeraldEpoch (startupGenesis state))
    (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
    (maybe (Set.toAscList (Isolation.isolationWitnessVoterHosts isolation)) (map raftVoterBindingHeraldEpoch . Voter.voterConfigurationBindings) (Projection.oracleViewVoterConfiguration view))
    (heraldMembershipGenerationId membership)
    (Projection.oracleViewControlIndex view)
    phase
    (oracleContactNode . oracleBindingContact <$> OracleClient.oracleClientCurrentBinding client)
    (OracleClient.oracleClientLeaderHint client)
  where
    view = Projection.oracleView (startupControlOracleProjectionState state)
    membership = Projection.oracleViewCurrentHeraldMembership view
    client = startupOracleClientState state
    isolation = Isolation.stateWitness (startupIsolationState state)
    phase = case Isolation.isolationWitnessPhase isolation of
      Isolation.IsolationReadOnlyDrainView -> AdministrationValue.AdminIsolationDrain
      Isolation.IsolationTerminalView -> AdministrationValue.AdminIsolated
      _ -> case heraldPhase state of
        HeraldServing -> AdministrationValue.AdminServing
        HeraldDraining -> AdministrationValue.AdminDraining
        HeraldStopped -> AdministrationValue.AdminStopped

inspectPreparations :: HeraldState -> [AdministrationValue.AdminPreparationInspection]
inspectPreparations state = local <> outgoing <> incoming
  where
    owner = startupProcessPreparationState state
    local =
      [ AdministrationValue.AdminPreparationInspection
          reference
          (Just (processStartProcessEpochId (Preparation.preparationStart preparation)))
          ( case Preparation.preparationPhase preparation of
              Preparation.Preparing -> AdministrationValue.AdminPreparationPreparing
              Preparation.Prepared -> AdministrationValue.AdminPreparationPrepared
              Preparation.Attached -> AdministrationValue.AdminPreparationAttached
              Preparation.Cancelling -> AdministrationValue.AdminPreparationCancelling
              Preparation.Terminal -> AdministrationValue.AdminPreparationTerminal
          )
          (Preparation.preparationFailure preparation)
      | (reference, preparation) <- Preparation.preparationEntries owner
      ]
    localReferences = Set.fromList (map fst (Preparation.preparationEntries owner))
    outgoing =
      [ let (phase, failure) = remotePhase preparation.outgoingStatus
         in AdministrationValue.AdminPreparationInspection
              reference
              (remoteProcess <$> preparation.outgoingChild)
              (if preparation.outgoingCancelled && phase `elem` [AdministrationValue.AdminPreparationPreparing, AdministrationValue.AdminPreparationPrepared] then AdministrationValue.AdminPreparationCancelling else phase)
              failure
      | (reference, preparation) <- Preparation.outgoingEntries owner
      ]
    incoming =
      [ AdministrationValue.AdminPreparationInspection
          reference
          Nothing
          (if preparation.incomingCancelled || preparation.incomingFailure /= Nothing then AdministrationValue.AdminPreparationTerminal else AdministrationValue.AdminPreparationPreparing)
          preparation.incomingFailure
      | (reference, preparation) <- Preparation.incomingEntries owner,
        reference `Set.notMember` localReferences
      ]
    remoteProcess (Remote.RemoteChild process _ _) = process
    remotePhase = \case
      Remote.RemotePreparationPending _ -> (AdministrationValue.AdminPreparationPreparing, Nothing)
      Remote.RemotePreparationAvailable _ _ -> (AdministrationValue.AdminPreparationPrepared, Nothing)
      Remote.RemotePreparationClaimed _ -> (AdministrationValue.AdminPreparationAttached, Nothing)
      Remote.RemotePreparationCancelled -> (AdministrationValue.AdminPreparationTerminal, Nothing)
      Remote.RemotePreparationFailed failure -> (AdministrationValue.AdminPreparationTerminal, Just failure)

configuredStartOutcomeMayRequireDispatch ::
  Administration.ConfiguredStartOutcome ->
  Bool
configuredStartOutcomeMayRequireDispatch = \case
  Administration.ConfiguredStartFirst
    (Administration.ConfiguredStartAwaitingOracle _) -> True
  Administration.ConfiguredStartExactRetry
    (Administration.ConfiguredStartAwaitingOracle _) -> True
  _ -> False

prepareEnd ::
  AdministrationValue.EndProcessEpochRequest ->
  HeraldState ->
  Either Administration.AdministrationInvariant (HeraldState, Administration.EndProcessEpochOutcome)
prepareEnd request predecessor = do
  let administration = startupAdministrationState predecessor
      oracleClient = startupOracleClientState predecessor
  (successorClient, oracleReference) <-
    if Administration.endProcessEpochRequiresOracle request administration
      then do
        preparedOracle <-
          either
            (const (Left Administration.AdministrationConfiguredStartOracleRequestMissing))
            Right
            ( OracleClient.prepareEndProcessEpochRequest
                ( AdministrationInternal.endProcessRequestDigestBytes
                    (AdministrationInternal.endProcessEpochRequestDigest request)
                )
                (AdministrationValue.endProcessEpochProcess request)
                (AdministrationValue.endProcessEpochReason request)
                oracleClient
            )
        if OracleClient.preparedOracleRequestClassification preparedOracle
          == OracleClient.OracleRequestFirstPrepared
          then Right ()
          else Left Administration.AdministrationConfiguredStartOracleRequestUnexpected
        let (successor, _, reference) =
              OracleClient.commitOracleRequest preparedOracle
        Right (successor, Just reference)
      else Right (oracleClient, Nothing)
  preparedAdministration <-
    Administration.prepareEndProcessEpoch
      request
      oracleReference
      administration
  let (successorAdministration, outcome) =
        Administration.commitEndProcessEpoch preparedAdministration
      successor =
        replaceStartupOracleClientState successorClient
          . replaceStartupAdministrationState successorAdministration
          $ predecessor
  Right (successor, outcome)

endProcessOutcomeMayRequireDispatch ::
  Administration.EndProcessEpochOutcome -> Bool
endProcessOutcomeMayRequireDispatch = \case
  Administration.EndProcessEpochFirst
    (Administration.EndProcessAwaitingOracle _) -> True
  Administration.EndProcessEpochExactRetry
    (Administration.EndProcessAwaitingOracle _) -> True
  _ -> False

installConfiguredOwners ::
  ConfiguredStart.Owners ->
  HeraldState ->
  HeraldState
installConfiguredOwners owners =
  replaceStartupIdGeneratorState
    (ConfiguredStart.configuredStartIdGenerator owners)
    . replaceStartupConfiguredProcessState
      (ConfiguredStart.configuredStartConfiguredProcesses owners)
    . replaceStartupOracleProjectionState
      (ConfiguredStart.configuredStartOracleProjection owners)
    . replaceStartupOracleClientState
      (ConfiguredStart.configuredStartOracleClient owners)
    . replaceStartupAdministrationState
      (ConfiguredStart.configuredStartAdministration owners)

checkedSuccess ::
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
checkedSuccess state effects = Right (state, effects)

administrationFault :: Either HeraldInvariantFault value
administrationFault =
  Left
    ( HeraldTransitionInvariant
        HeraldAdministrationTransitionContradiction
    )

-- | Observe ordered cancellation completion after the common lifecycle driver.
advancePreparationCancellations :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advancePreparationCancellations state = foldM advance (state, emptyEffectBatch) pending
  where
    pending = [(correlation, reference, status) | (correlation, reference, status@LifecyclePending {}) <- Administration.preparationCancellationEntries (startupAdministrationState state)]
    advance (predecessor, effects) (correlation, reference, previous) = do
      let status = ProcessPreparation.preparationCancellationStatus reference predecessor
      if status == previous
        then Right (predecessor, effects)
        else do
          administration <- either (const administrationFault) Right (Administration.settlePreparationCancellation correlation status (startupAdministrationState predecessor))
          let successor = replaceStartupAdministrationState administration predecessor
              reply = RetainedAdminResult correlation (AdministrationInternal.AdminPreparationCancellation status)
              emitted = maybe emptyEffectBatch (\binding -> if AdministrationValue.adminCorrelationBelongsTo binding correlation then singletonEffectBatch (SendAdministrationReply binding reply) else emptyEffectBatch) (Administration.currentConfiguredAdministrationBinding administration)
          Right (successor, effects <> emitted)

-- | Recheck the same deferred command after the membership barrier opens.
-- The generated Start composer allocates its pair and Oracle request once here.
advanceGatedStarts :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceGatedStarts initial
  | Application.applicationMembershipGateClosed (startupApplicationState initial) = Right (initial, emptyEffectBatch)
  | otherwise = foldM advance (initial, emptyEffectBatch) requests
  where
    requests = [request | (_, request, _, Administration.ConfiguredStartGated) <- Administration.configuredStartEntries (startupAdministrationState initial)]
    advance (predecessor, effects) request = do
      let owners = ConfiguredStart.configuredStartOwners (startupAdministrationState predecessor) (startupOracleClientState predecessor) (startupOracleProjectionState predecessor) (startupConfiguredProcessState predecessor) (startupIdGeneratorState predecessor)
      prepared <- either (const administrationFault) Right (ConfiguredStart.prepareConfiguredStartRequest (startupGenesis predecessor) request owners)
      let (successorOwners, outcome) = ConfiguredStart.commitConfiguredStartRequest prepared
          successor = installConfiguredOwners successorOwners predecessor
          administration = ConfiguredStart.configuredStartAdministration successorOwners
          reply = Administration.configuredStartAdminReply (startProcessCorrelation request) outcome administration
          replyEffects = maybe [] (\binding -> [SendAdministrationReply binding reply | AdministrationValue.adminCorrelationBelongsTo binding (startProcessCorrelation request)]) (Administration.currentConfiguredAdministrationBinding administration)
          actions = OracleClient.oracleClientRequestActions (ConfiguredStart.configuredStartOracleClient successorOwners)
      Right (successor, effects <> orderedEffectBatch (replyEffects <> map RunOracleClientAction actions))
