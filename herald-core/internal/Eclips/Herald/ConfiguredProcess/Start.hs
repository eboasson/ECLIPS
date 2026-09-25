{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Atomic package-internal dynamic process administration workflow.
--
-- The resident generator and Oracle client retain one fresh identity pair and
-- canonical Start intention before any submission effect is exposed. Applying
-- its Oracle result installs the exact local process scaffold.
module Eclips.Herald.ConfiguredProcess.Start
  ( Owners,
    configuredStartOwners,
    configuredStartAdministration,
    configuredStartOracleClient,
    configuredStartOracleProjection,
    configuredStartConfiguredProcesses,
    configuredStartIdGenerator,
    ConfiguredStartProblem (..),
    PreparedConfiguredStartRequest,
    prepareConfiguredStartRequest,
    preparedConfiguredStartRequestOutcome,
    commitConfiguredStartRequest,
    ConfiguredStartOracleOutcome (..),
    PreparedConfiguredStartOracleResult,
    prepareConfiguredStartOracleResult,
    preparedConfiguredStartOracleOutcome,
    commitConfiguredStartOracleResult,
    ProcessEndOwners,
    configuredProcessEndOwners,
    configuredProcessEndAdministration,
    configuredProcessEndConfiguredProcesses,
    ConfiguredStartProcessEndOutcome (..),
    PreparedConfiguredStartProcessEnd,
    prepareConfiguredStartProcessEnd,
    preparedConfiguredStartProcessEndOutcome,
    commitConfiguredStartProcessEnd,
  )
where

import Eclips.Domain.Identity (ControlIndex, ProcessEpochId, globalUniqueIdBytes, mkProcessEpochId, mkProcessId)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Herald.Administration.Internal
  ( AdminCorrelationId,
    StartProcessRequest,
    processStartRequestDigestBytes,
    startProcessRequestDigest,
  )
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Genesis.Internal (CheckedHeraldGenesis, checkedLocalHeraldEpoch)
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Projection (appliedEntryControlIndex)

data Owners = Owners
  { administration :: Administration.State,
    oracleClient :: OracleClient.State,
    oracleProjection :: OracleProjection.State,
    configuredProcesses :: Configured.State,
    idGenerator :: IdGenerator.State
  }
  deriving stock (Eq)

configuredStartOwners ::
  Administration.State ->
  OracleClient.State ->
  OracleProjection.State ->
  Configured.State ->
  IdGenerator.State ->
  Owners
configuredStartOwners = Owners

configuredStartAdministration :: Owners -> Administration.State
configuredStartAdministration owners = owners.administration

configuredStartOracleClient :: Owners -> OracleClient.State
configuredStartOracleClient owners = owners.oracleClient

configuredStartOracleProjection :: Owners -> OracleProjection.State
configuredStartOracleProjection owners = owners.oracleProjection

configuredStartConfiguredProcesses :: Owners -> Configured.State
configuredStartConfiguredProcesses owners = owners.configuredProcesses

configuredStartIdGenerator :: Owners -> IdGenerator.State
configuredStartIdGenerator owners = owners.idGenerator

data ConfiguredStartProblem
  = ConfiguredStartAdministrationProblem Administration.AdministrationInvariant
  | ConfiguredStartOracleClientProblem OracleClient.OracleClientProblem
  | ConfiguredStartScaffoldProblem Configured.ConfiguredScaffoldProblem
  | ConfiguredStartGeneratorProblem IdGenerator.IdGeneratorInvariant
  | ConfiguredStartRecordMissing
  | ConfiguredStartFirstCommitWasAlreadyRetained
  | ConfiguredStartProjectedEntryMissing
  | ConfiguredStartEndClassificationMismatch
  deriving stock (Eq, Show)

newtype PreparedConfiguredStartRequest
  = PreparedConfiguredStartRequest
      (Prepared Owners Administration.ConfiguredStartOutcome)

prepareConfiguredStartRequest ::
  CheckedHeraldGenesis ->
  StartProcessRequest ->
  Owners ->
  Either ConfiguredStartProblem PreparedConfiguredStartRequest
prepareConfiguredStartRequest genesis request owners = do
  requiredBootstrap <-
    either
      (Left . ConfiguredStartAdministrationProblem)
      Right
      ( Administration.configuredStartRequiresOracle
          genesis
          request
          owners.administration
      )
  (successorOracleClient, successorGenerator, oracleReference) <-
    case requiredBootstrap of
      False -> Right (owners.oracleClient, owners.idGenerator, Nothing)
      True -> do
        first <- either (Left . ConfiguredStartGeneratorProblem) Right (IdGenerator.prepareGeneratedId owners.idGenerator)
        second <- either (Left . ConfiguredStartGeneratorProblem) Right (IdGenerator.prepareGeneratedId (IdGenerator.commitGeneratedId first))
        process <- either (const (Left (ConfiguredStartGeneratorProblem IdGenerator.IdGeneratorIdentityRefinementInvariant))) Right (mkProcessId (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId first)))
        epoch <- either (const (Left (ConfiguredStartGeneratorProblem IdGenerator.IdGeneratorIdentityRefinementInvariant))) Right (mkProcessEpochId (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId second)))
        let bootstrap = processStart process epoch (checkedLocalHeraldEpoch genesis)
        preparedOracle <-
          either
            (Left . ConfiguredStartOracleClientProblem)
            Right
            ( OracleClient.prepareStartProcessEpochRequest
                ( processStartRequestDigestBytes
                    (startProcessRequestDigest request)
                )
                bootstrap
                owners.oracleClient
            )
        if OracleClient.preparedOracleRequestClassification preparedOracle
          /= OracleClient.OracleRequestFirstPrepared
          then Left ConfiguredStartFirstCommitWasAlreadyRetained
          else Right ()
        let (successor, _, reference) =
              OracleClient.commitOracleRequest preparedOracle
        Right (successor, IdGenerator.commitGeneratedId second, Just reference)
  preparedAdministration <-
    either
      (Left . ConfiguredStartAdministrationProblem)
      Right
      ( Administration.prepareStartProcessEpoch
          genesis
          request
          oracleReference
          owners.administration
      )
  let (successorAdministration, outcome) =
        Administration.commitStartProcessEpoch preparedAdministration
      successor =
        owners
          { administration = successorAdministration,
            oracleClient = successorOracleClient,
            idGenerator = successorGenerator
          }
  PreparedConfiguredStartRequest
    <$> prepareTransition (\_ -> Right (successor, outcome)) owners

preparedConfiguredStartRequestOutcome ::
  PreparedConfiguredStartRequest ->
  Administration.ConfiguredStartOutcome
preparedConfiguredStartRequestOutcome (PreparedConfiguredStartRequest prepared) =
  preparedOutput prepared

commitConfiguredStartRequest ::
  PreparedConfiguredStartRequest ->
  (Owners, Administration.ConfiguredStartOutcome)
commitConfiguredStartRequest (PreparedConfiguredStartRequest prepared) =
  commitPrepared prepared

data ConfiguredStartOracleOutcome
  = ConfiguredStartOracleRejected Administration.ConfiguredStartStatus
  | ConfiguredStartProcessInstalled Configured.LiveProcessScaffoldRef
  | ConfiguredStartOracleExactRetry Administration.ConfiguredStartStatus
  deriving stock (Eq, Show)

newtype PreparedConfiguredStartOracleResult
  = PreparedConfiguredStartOracleResult
      (Prepared Owners ConfiguredStartOracleOutcome)

prepareConfiguredStartOracleResult ::
  CheckedHeraldGenesis ->
  AdminCorrelationId ->
  Owners ->
  Either ConfiguredStartProblem PreparedConfiguredStartOracleResult
prepareConfiguredStartOracleResult genesis correlation owners = do
  status <-
    maybe
      (Left ConfiguredStartRecordMissing)
      Right
      (Administration.lookupConfiguredStartStatus correlation owners.administration)
  intention <- case status of
    Administration.ConfiguredStartGated -> Left ConfiguredStartRecordMissing
    Administration.ConfiguredStartAwaitingOracle retained -> Right retained
    Administration.ConfiguredStartOracleRejected retained _ _ -> Right retained
    Administration.ConfiguredStartProcessInstalled retained _ _ -> Right retained
    Administration.ConfiguredStartReady retained _ _ _ -> Right retained
  let request = Administration.startProcessIntentionOracleRequest intention
  entry <-
    maybe
      (Left ConfiguredStartProjectedEntryMissing)
      Right
      (OracleClient.oracleClientRequestEvidence request owners.oracleClient)
  preparedOracle <-
    either
      (Left . ConfiguredStartOracleClientProblem)
      Right
      (OracleClient.prepareOracleResult request entry owners.oracleClient)
  let (successorOracleClient, resultClassification, result) =
        OracleClient.commitOracleResult preparedOracle
      index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
      withOracle = owners {oracleClient = successorOracleClient}
  (successor, outcome) <-
    case status of
      Administration.ConfiguredStartGated -> Left ConfiguredStartRecordMissing
      Administration.ConfiguredStartAwaitingOracle _ -> do
        if resultClassification == OracleClient.OracleResultFirstRecorded
          then Right ()
          else Left ConfiguredStartFirstCommitWasAlreadyRetained
        case result of
          OracleClient.OracleRequestRejected rejection ->
            settleRejected request index rejection withOracle
          OracleClient.OracleRequestStarted bootstrap startIndex ->
            settleAccepted request index bootstrap startIndex withOracle
          _ ->
            Left ConfiguredStartFirstCommitWasAlreadyRetained
      retained@(Administration.ConfiguredStartOracleRejected _ _ _) -> do
        if resultClassification == OracleClient.OracleResultExactRetry
          then settleRetainedRejected retained request index result withOracle
          else Left ConfiguredStartFirstCommitWasAlreadyRetained
      retained@(Administration.ConfiguredStartProcessInstalled _ _ reference) -> do
        if resultClassification == OracleClient.OracleResultExactRetry
          then settleRetainedAccepted retained request index result reference withOracle
          else Left ConfiguredStartFirstCommitWasAlreadyRetained
      retained@(Administration.ConfiguredStartReady _ _ reference _) -> do
        if resultClassification == OracleClient.OracleResultExactRetry
          then settleRetainedAccepted retained request index result reference withOracle
          else Left ConfiguredStartFirstCommitWasAlreadyRetained
  PreparedConfiguredStartOracleResult
    <$> prepareTransition (\_ -> Right (successor, outcome)) owners
  where
    settleRejected request index rejection predecessor = do
      prepared <-
        either
          (Left . ConfiguredStartAdministrationProblem)
          Right
          ( Administration.prepareConfiguredStartSettlement
              correlation
              ( Administration.ConfiguredStartOracleWasRejected
                  request
                  index
                  rejection
              )
              predecessor.administration
          )
      let (successorAdministration, classification) =
            Administration.commitConfiguredStartSettlement prepared
      case classification of
        Administration.ConfiguredStartFirstSettled status ->
          Right
            ( predecessor {administration = successorAdministration},
              ConfiguredStartOracleRejected status
            )
        Administration.ConfiguredStartSettlementExactRetry _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained

    settleAccepted request index bootstrap startIndex predecessor = do
      preparedScaffold <-
        either
          (Left . ConfiguredStartScaffoldProblem)
          Right
          ( Configured.prepareLiveProcessScaffold
              genesis
              bootstrap
              startIndex
              predecessor.configuredProcesses
          )
      if Configured.preparedLiveProcessScaffoldClassification preparedScaffold
        /= Configured.ConfiguredScaffoldFirstPrepared
        then Left ConfiguredStartFirstCommitWasAlreadyRetained
        else Right ()
      let (successorConfigured, (_, reference)) =
            Configured.commitLiveProcessScaffold preparedScaffold
      preparedSettlement <-
        either
          (Left . ConfiguredStartAdministrationProblem)
          Right
          ( Administration.prepareConfiguredStartSettlement
              correlation
              ( Administration.ConfiguredStartOracleWasAccepted
                  request
                  index
                  reference
              )
              predecessor.administration
          )
      let (successorAdministration, classification) =
            Administration.commitConfiguredStartSettlement preparedSettlement
      case classification of
        Administration.ConfiguredStartFirstSettled _ ->
          Right
            ( predecessor
                { administration = successorAdministration,
                  oracleClient = predecessor.oracleClient,
                  configuredProcesses = successorConfigured
                },
              ConfiguredStartProcessInstalled reference
            )
        Administration.ConfiguredStartSettlementExactRetry _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained

    settleRetainedRejected retained request index result predecessor = do
      rejection <- case result of
        OracleClient.OracleRequestRejected value -> Right value
        _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained
      prepared <-
        either
          (Left . ConfiguredStartAdministrationProblem)
          Right
          ( Administration.prepareConfiguredStartSettlement
              correlation
              ( Administration.ConfiguredStartOracleWasRejected
                  request
                  index
                  rejection
              )
              predecessor.administration
          )
      let (successorAdministration, classification) =
            Administration.commitConfiguredStartSettlement prepared
      case classification of
        Administration.ConfiguredStartSettlementExactRetry _ ->
          Right
            ( predecessor {administration = successorAdministration},
              ConfiguredStartOracleExactRetry retained
            )
        Administration.ConfiguredStartFirstSettled _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained

    settleRetainedAccepted retained request index result reference predecessor = do
      case result of
        OracleClient.OracleRequestStarted {} -> Right ()
        _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained
      prepared <-
        either
          (Left . ConfiguredStartAdministrationProblem)
          Right
          ( Administration.prepareConfiguredStartSettlement
              correlation
              ( Administration.ConfiguredStartOracleWasAccepted
                  request
                  index
                  reference
              )
              predecessor.administration
          )
      let (successorAdministration, classification) =
            Administration.commitConfiguredStartSettlement prepared
      case classification of
        Administration.ConfiguredStartSettlementExactRetry _ ->
          Right
            ( predecessor {administration = successorAdministration},
              ConfiguredStartOracleExactRetry retained
            )
        Administration.ConfiguredStartFirstSettled _ ->
          Left ConfiguredStartFirstCommitWasAlreadyRetained

preparedConfiguredStartOracleOutcome ::
  PreparedConfiguredStartOracleResult ->
  ConfiguredStartOracleOutcome
preparedConfiguredStartOracleOutcome
  (PreparedConfiguredStartOracleResult prepared) = preparedOutput prepared

commitConfiguredStartOracleResult ::
  PreparedConfiguredStartOracleResult ->
  (Owners, ConfiguredStartOracleOutcome)
commitConfiguredStartOracleResult
  (PreparedConfiguredStartOracleResult prepared) = commitPrepared prepared

data ConfiguredStartProcessEndOutcome
  = ConfiguredStartProcessEndNoRecord
  | ConfiguredStartEndedBeforeAttachment
  | ConfiguredStartProcessEndExactRetry
  | ConfiguredStartProcessEndAfterAttachment
  deriving stock (Eq, Show)

-- | The two resident owners changed by Start-before-attachment settlement.
-- OracleClient, OracleProjection, and the generator are intentionally absent:
-- End consumes an already projected lifecycle fact and must preserve their
-- accepted history byte-for-byte.
data ProcessEndOwners
  = ProcessEndOwners Administration.State Configured.State
  deriving stock (Eq)

configuredProcessEndOwners ::
  Administration.State ->
  Configured.State ->
  ProcessEndOwners
configuredProcessEndOwners = ProcessEndOwners

configuredProcessEndAdministration :: ProcessEndOwners -> Administration.State
configuredProcessEndAdministration (ProcessEndOwners administration _) =
  administration

configuredProcessEndConfiguredProcesses :: ProcessEndOwners -> Configured.State
configuredProcessEndConfiguredProcesses (ProcessEndOwners _ configuredProcesses) =
  configuredProcesses

newtype PreparedConfiguredStartProcessEnd
  = PreparedConfiguredStartProcessEnd
      (Prepared ProcessEndOwners ConfiguredStartProcessEndOutcome)

-- | Atomically retain the configured-owner and Administration-owner halves of
-- the private Start/End race.  It creates no live End request, reply, Oracle
-- command, or wire value.
prepareConfiguredStartProcessEnd ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  ProcessEndOwners ->
  Either ConfiguredStartProblem PreparedConfiguredStartProcessEnd
prepareConfiguredStartProcessEnd
  process
  endIndex
  reason
  owners@(ProcessEndOwners predecessorAdministration predecessorConfigured) = do
    preparedConfigured <-
      either
        (Left . ConfiguredStartScaffoldProblem)
        Right
        ( Configured.prepareConfiguredProcessEnd
            process
            endIndex
            reason
            predecessorConfigured
        )
    preparedAdministration <-
      either
        (Left . ConfiguredStartAdministrationProblem)
        Right
        ( Administration.prepareConfiguredStartProcessEnd
            process
            endIndex
            reason
            predecessorAdministration
        )
    let (configuredProcesses, configuredClassification) =
          Configured.commitConfiguredProcessEnd preparedConfigured
        (administration, administrationClassification) =
          Administration.commitConfiguredStartProcessEnd preparedAdministration
    outcome <-
      classifyProcessEnd configuredClassification administrationClassification
    let successor = ProcessEndOwners administration configuredProcesses
    PreparedConfiguredStartProcessEnd
      <$> prepareTransition (\_ -> Right (successor, outcome)) owners

preparedConfiguredStartProcessEndOutcome ::
  PreparedConfiguredStartProcessEnd ->
  ConfiguredStartProcessEndOutcome
preparedConfiguredStartProcessEndOutcome
  (PreparedConfiguredStartProcessEnd prepared) = preparedOutput prepared

commitConfiguredStartProcessEnd ::
  PreparedConfiguredStartProcessEnd ->
  (ProcessEndOwners, ConfiguredStartProcessEndOutcome)
commitConfiguredStartProcessEnd (PreparedConfiguredStartProcessEnd prepared) =
  commitPrepared prepared

classifyProcessEnd ::
  Configured.ConfiguredProcessEndClassification ->
  Administration.ConfiguredStartProcessEndClassification ->
  Either ConfiguredStartProblem ConfiguredStartProcessEndOutcome
classifyProcessEnd configured administration =
  case (configured, administration) of
    ( Configured.ConfiguredProcessEndNoScaffold,
      Administration.ConfiguredStartProcessEndNoRecord
      ) -> Right ConfiguredStartProcessEndNoRecord
    ( Configured.ConfiguredProcessFirstEndedBeforeAttachment,
      Administration.ConfiguredStartFirstEndedBeforeAttachment {}
      ) -> Right ConfiguredStartEndedBeforeAttachment
    ( Configured.ConfiguredProcessEndExactRetry,
      Administration.ConfiguredStartProcessEndExactRetry {}
      ) -> Right ConfiguredStartProcessEndExactRetry
    ( Configured.ConfiguredProcessEndExactRetry,
      Administration.ConfiguredStartProcessEndRetiredBeforeAttachment
      ) -> Right ConfiguredStartProcessEndExactRetry
    ( Configured.ConfiguredProcessEndAfterAttachment,
      Administration.ConfiguredStartProcessEndAfterAttachment {}
      ) -> Right ConfiguredStartProcessEndAfterAttachment
    ( Configured.ConfiguredProcessEndAfterAttachment,
      Administration.ConfiguredStartProcessEndRetiredAfterAttachment
      ) -> Right ConfiguredStartProcessEndAfterAttachment
    _ -> Left ConfiguredStartEndClassificationMismatch
