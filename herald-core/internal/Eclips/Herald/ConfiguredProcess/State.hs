{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Package-internal owner of applied dynamic process identity and local
-- application installation settlement. No ordinary roots or publication
-- authority are granted by this owner.
module Eclips.Herald.ConfiguredProcess.State
  ( State,
    initialState,
    LiveProcessScaffold,
    liveProcessScaffoldBootstrap,
    liveProcessScaffoldControlIndex,
    LiveProcessScaffoldRef,
    liveProcessScaffoldRefProcessEpoch,
    lookupLiveProcessScaffold,
    configuredProcessScaffoldEntries,
    ConfiguredAttachmentSettlement (..),
    configuredAttachmentSettlement,
    configuredAttachmentSettlementEntries,
    ConfiguredScaffoldClassification (..),
    ConfiguredScaffoldProblem (..),
    PreparedLiveProcessScaffold,
    prepareLiveProcessScaffold,
    preparedLiveProcessScaffoldClassification,
    preparedLiveProcessScaffold,
    commitLiveProcessScaffold,
    ConfiguredAttachmentReadyClassification (..),
    PreparedConfiguredAttachmentReady,
    prepareConfiguredAttachmentReady,
    preparedConfiguredAttachmentReadyClassification,
    commitConfiguredAttachmentReady,
    ConfiguredProcessEndClassification (..),
    PreparedConfiguredProcessEnd,
    prepareConfiguredProcessEnd,
    preparedConfiguredProcessEndClassification,
    commitConfiguredProcessEnd,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, ProcessEpochId)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Domain.ProcessStart (ProcessStart, processStartProcessEpochId, processStartResidence)
import Eclips.Herald.Genesis.Internal (CheckedHeraldGenesis, checkedLocalHeraldEpoch)
import Eclips.Herald.Internal.Prepared (Prepared, commitPrepared, prepareTransition, preparedOutput)

-- | One applied minimal Start. All ordinary root authority is installed through
-- explicit selected grants; a dynamic process has no privileged root stages.
data LiveProcessScaffold = LiveProcessScaffold
  { bootstrap :: ProcessStart,
    control :: ControlIndex
  }
  deriving stock (Eq, Show)

liveProcessScaffoldBootstrap :: LiveProcessScaffold -> ProcessStart
liveProcessScaffoldBootstrap scaffold = scaffold.bootstrap

liveProcessScaffoldControlIndex :: LiveProcessScaffold -> ControlIndex
liveProcessScaffoldControlIndex scaffold = scaffold.control

newtype LiveProcessScaffoldRef = LiveProcessScaffoldRef ProcessEpochId
  deriving stock (Eq, Ord, Show)

liveProcessScaffoldRefProcessEpoch :: LiveProcessScaffoldRef -> ProcessEpochId
liveProcessScaffoldRefProcessEpoch (LiveProcessScaffoldRef process) = process

data State = State
  { scaffolds :: Map ProcessEpochId LiveProcessScaffold,
    attachmentSettlements :: Map ProcessEpochId ConfiguredAttachmentSettlement
  }
  deriving stock (Eq, Show)

initialState :: State
initialState =
  State
    { scaffolds = Map.empty,
      attachmentSettlements = Map.empty
    }

lookupLiveProcessScaffold ::
  ProcessEpochId ->
  State ->
  Maybe LiveProcessScaffold
lookupLiveProcessScaffold process = Map.lookup process . (.scaffolds)

configuredProcessScaffoldEntries ::
  State ->
  [(ProcessEpochId, LiveProcessScaffold)]
configuredProcessScaffoldEntries = Map.toAscList . (.scaffolds)

-- | Monotone settlement of the application attachment associated with one
-- dynamic Start. An End before installation prevents readiness.
data ConfiguredAttachmentSettlement
  = ConfiguredAwaitingAttachment
  | ConfiguredAttachmentReady
  | ConfiguredEndedBeforeAttachment ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

configuredAttachmentSettlement ::
  ProcessEpochId ->
  State ->
  Maybe ConfiguredAttachmentSettlement
configuredAttachmentSettlement process =
  Map.lookup process . (.attachmentSettlements)

configuredAttachmentSettlementEntries ::
  State ->
  [(ProcessEpochId, ConfiguredAttachmentSettlement)]
configuredAttachmentSettlementEntries =
  Map.toAscList . (.attachmentSettlements)

data ConfiguredScaffoldClassification
  = ConfiguredScaffoldFirstPrepared
  | ConfiguredScaffoldExactRetry
  deriving stock (Eq, Ord, Show)

data ConfiguredScaffoldProblem
  = ConfiguredScaffoldWrongResidence HeraldEpoch HeraldEpoch
  | ConfiguredScaffoldProcessConflict ProcessEpochId
  | ConfiguredScaffoldSettlementMissing ProcessEpochId
  | ConfiguredScaffoldEndEvidenceConflict
      ProcessEpochId
      ControlIndex
      ProcessEndReason
      ControlIndex
      ProcessEndReason
  deriving stock (Eq, Show)

newtype PreparedLiveProcessScaffold
  = PreparedLiveProcessScaffold
      ( Prepared
          State
          (ConfiguredScaffoldClassification, LiveProcessScaffold)
      )

-- | Install an applied dynamic process only at its checked resident Herald.
prepareLiveProcessScaffold ::
  CheckedHeraldGenesis ->
  ProcessStart ->
  ControlIndex ->
  State ->
  Either ConfiguredScaffoldProblem PreparedLiveProcessScaffold
prepareLiveProcessScaffold genesis bootstrap startIndex state = do
  let process = processStartProcessEpochId bootstrap
  if processStartResidence bootstrap /= checkedLocalHeraldEpoch genesis
    then
      Left
        ( ConfiguredScaffoldWrongResidence
            (checkedLocalHeraldEpoch genesis)
            (processStartResidence bootstrap)
        )
    else Right ()
  let scaffold = LiveProcessScaffold bootstrap startIndex
  case Map.lookup process state.scaffolds of
    Just incumbent
      | incumbent == scaffold,
        Map.member process state.attachmentSettlements ->
          PreparedLiveProcessScaffold
            <$> prepareTransition
              (\predecessor -> Right (predecessor, (ConfiguredScaffoldExactRetry, incumbent)))
              state
      | incumbent == scaffold ->
          Left (ConfiguredScaffoldSettlementMissing process)
      | otherwise -> Left (ConfiguredScaffoldProcessConflict process)
    Nothing -> do
      let successor =
            state
              { scaffolds = Map.insert process scaffold state.scaffolds,
                attachmentSettlements =
                  Map.insert
                    process
                    ConfiguredAwaitingAttachment
                    state.attachmentSettlements
              }
      PreparedLiveProcessScaffold
        <$> prepareTransition
          (\_ -> Right (successor, (ConfiguredScaffoldFirstPrepared, scaffold)))
          state

preparedLiveProcessScaffoldClassification ::
  PreparedLiveProcessScaffold ->
  ConfiguredScaffoldClassification
preparedLiveProcessScaffoldClassification (PreparedLiveProcessScaffold prepared) =
  fst (preparedOutput prepared)

preparedLiveProcessScaffold ::
  PreparedLiveProcessScaffold ->
  LiveProcessScaffold
preparedLiveProcessScaffold (PreparedLiveProcessScaffold prepared) =
  snd (preparedOutput prepared)

commitLiveProcessScaffold ::
  PreparedLiveProcessScaffold ->
  (State, (ConfiguredScaffoldClassification, LiveProcessScaffoldRef))
commitLiveProcessScaffold (PreparedLiveProcessScaffold prepared) =
  let (successor, (classification, scaffold)) = commitPrepared prepared
   in ( successor,
        ( classification,
          LiveProcessScaffoldRef
            (processStartProcessEpochId scaffold.bootstrap)
        )
      )

data ConfiguredAttachmentReadyClassification
  = ConfiguredAttachmentFirstReady
  | ConfiguredAttachmentReadyExactRetry
  | ConfiguredAttachmentReadySuppressedByEnd ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

newtype PreparedConfiguredAttachmentReady
  = PreparedConfiguredAttachmentReady
      (Prepared State ConfiguredAttachmentReadyClassification)

-- | Mark application attachment material ready only while the configured
-- process is still awaiting it. A late installation callback after End is a
-- checked no-op carrying the retained End evidence.
prepareConfiguredAttachmentReady ::
  ProcessEpochId ->
  State ->
  Either ConfiguredScaffoldProblem PreparedConfiguredAttachmentReady
prepareConfiguredAttachmentReady process state =
  PreparedConfiguredAttachmentReady <$> prepareTransition prepare state
  where
    prepare predecessor = do
      _ <- requireScaffold process predecessor
      settlement <- requireSettlement process predecessor
      case settlement of
        ConfiguredAwaitingAttachment ->
          Right
            ( predecessor
                { attachmentSettlements =
                    Map.insert
                      process
                      ConfiguredAttachmentReady
                      predecessor.attachmentSettlements
                },
              ConfiguredAttachmentFirstReady
            )
        ConfiguredAttachmentReady ->
          Right (predecessor, ConfiguredAttachmentReadyExactRetry)
        ConfiguredEndedBeforeAttachment index reason ->
          Right
            ( predecessor,
              ConfiguredAttachmentReadySuppressedByEnd index reason
            )

preparedConfiguredAttachmentReadyClassification ::
  PreparedConfiguredAttachmentReady ->
  ConfiguredAttachmentReadyClassification
preparedConfiguredAttachmentReadyClassification
  (PreparedConfiguredAttachmentReady prepared) = preparedOutput prepared

commitConfiguredAttachmentReady ::
  PreparedConfiguredAttachmentReady ->
  (State, ConfiguredAttachmentReadyClassification)
commitConfiguredAttachmentReady (PreparedConfiguredAttachmentReady prepared) =
  commitPrepared prepared

data ConfiguredProcessEndClassification
  = ConfiguredProcessEndNoScaffold
  | ConfiguredProcessFirstEndedBeforeAttachment
  | ConfiguredProcessEndExactRetry
  | ConfiguredProcessEndAfterAttachment
  deriving stock (Eq, Show)

newtype PreparedConfiguredProcessEnd
  = PreparedConfiguredProcessEnd
      (Prepared State ConfiguredProcessEndClassification)

-- | Retain the terminal Start-before-attachment race. Processes whose
-- attachment was already ready keep that immutable settlement; index-zero
-- residents have no dynamic scaffold and are a no-op in this owner.
prepareConfiguredProcessEnd ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  State ->
  Either ConfiguredScaffoldProblem PreparedConfiguredProcessEnd
prepareConfiguredProcessEnd process endIndex reason state =
  PreparedConfiguredProcessEnd <$> prepareTransition prepare state
  where
    prepare predecessor =
      case Map.lookup process predecessor.scaffolds of
        Nothing -> Right (predecessor, ConfiguredProcessEndNoScaffold)
        Just _ -> do
          settlement <- requireSettlement process predecessor
          case settlement of
            ConfiguredAwaitingAttachment ->
              Right
                ( predecessor
                    { attachmentSettlements =
                        Map.insert
                          process
                          (ConfiguredEndedBeforeAttachment endIndex reason)
                          predecessor.attachmentSettlements
                    },
                  ConfiguredProcessFirstEndedBeforeAttachment
                )
            ConfiguredAttachmentReady ->
              Right (predecessor, ConfiguredProcessEndAfterAttachment)
            ConfiguredEndedBeforeAttachment retainedIndex retainedReason
              | retainedIndex == endIndex && retainedReason == reason ->
                  Right (predecessor, ConfiguredProcessEndExactRetry)
              | otherwise ->
                  Left
                    ( ConfiguredScaffoldEndEvidenceConflict
                        process
                        retainedIndex
                        retainedReason
                        endIndex
                        reason
                    )

preparedConfiguredProcessEndClassification ::
  PreparedConfiguredProcessEnd ->
  ConfiguredProcessEndClassification
preparedConfiguredProcessEndClassification
  (PreparedConfiguredProcessEnd prepared) = preparedOutput prepared

commitConfiguredProcessEnd ::
  PreparedConfiguredProcessEnd ->
  (State, ConfiguredProcessEndClassification)
commitConfiguredProcessEnd (PreparedConfiguredProcessEnd prepared) =
  commitPrepared prepared

requireScaffold ::
  ProcessEpochId ->
  State ->
  Either ConfiguredScaffoldProblem LiveProcessScaffold
requireScaffold process state =
  maybe
    (Left (ConfiguredScaffoldProcessConflict process))
    Right
    (Map.lookup process state.scaffolds)

requireSettlement ::
  ProcessEpochId ->
  State ->
  Either ConfiguredScaffoldProblem ConfiguredAttachmentSettlement
requireSettlement process state =
  maybe
    (Left (ConfiguredScaffoldSettlementMissing process))
    Right
    (Map.lookup process state.attachmentSettlements)
