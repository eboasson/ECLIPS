{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Herald-private process identity localization.
--
-- The state is pure and is owned per Herald kernel. Every live process epoch has
-- one stable bidirectional private/global map and a never-reused local counter.
-- Application sessions deliberately do not occur in this leaf.
module Eclips.Herald.Application.PrivateIdentity
  ( PrivateIdentity,
    PrivateIdentityError (..),
    emptyPrivateIdentity,
    PreparedProcessRegistration,
    prepareProcessRegistration,
    commitProcessRegistration,
    PreparedLocalization,
    prepareLocalization,
    preparedPrivateUniqueId,
    commitLocalization,
    PreparedPrivateUniqueIdAllocation,
    preparePrivateUniqueIdAllocation,
    preparedAllocatedPrivateUniqueId,
    commitPrivateUniqueIdAllocation,
    lookupPrivateUniqueId,
    resolvePrivateUniqueId,
    ProcessIdentityWitness,
    processIdentityWitnesses,
    witnessedProcessEpoch,
    witnessedBindings,
    witnessedReverseBindings,
    witnessedNextPrivateUniqueId,
    PreparedProcessRetirement,
    prepareProcessRetirement,
    commitProcessRetirement,
    OracleProcessRetirementClassification (..),
    PreparedOracleProcessRetirement,
    prepareOracleProcessRetirement,
    preparedOracleProcessRetirementClassification,
    commitOracleProcessRetirement,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    PrivateUniqueIdSupply,
    initialPrivateUniqueIdSupply,
    takePrivateUniqueId,
  )
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )

-- | A rejected private-identity transition.
data PrivateIdentityError
  = ProcessEpochAlreadyRegistered
  | ProcessEpochIsUnknown
  | ProcessEpochIsRetired
  deriving stock (Eq, Show)

-- | All live process-local registries and retired process-epoch tombstones.
--
-- Tombstones prevent an accidentally repeated process epoch from restarting its
-- local counter and reusing a stale private name.
data PrivateIdentity
  = PrivateIdentity
      (Map ProcessEpochId ProcessPrivateIdentity)
      (Set ProcessEpochId)

instance Eq PrivateIdentity where
  left == right =
    processIdentityWitnesses left == processIdentityWitnesses right
      && retiredProcesses left == retiredProcesses right
    where
      retiredProcesses (PrivateIdentity _ retired) = retired

-- | One process epoch's inverse maps and next unassigned positive local value.
data ProcessPrivateIdentity
  = ProcessPrivateIdentity
      (Map GlobalUniqueId PrivateUniqueId)
      (Map PrivateUniqueId GlobalUniqueId)
      PrivateUniqueIdSupply

-- | Read-only evidence used by the composed Herald invariant validator.
--
-- The witness deliberately exposes neither a state constructor nor a replacement
-- operation.  Its binding list is taken from the forward map; the owning
-- invariant check can resolve every pair through the public reverse lookup and
-- compare it with the independently retained bootstrap access.
data ProcessIdentityWitness = ProcessIdentityWitness
  { processEpoch :: ProcessEpochId,
    bindings :: [(GlobalUniqueId, PrivateUniqueId)],
    reverseBindings :: [(PrivateUniqueId, GlobalUniqueId)],
    nextPrivateUniqueId :: PrivateUniqueId
  }
  deriving stock (Eq, Show)

witnessedProcessEpoch :: ProcessIdentityWitness -> ProcessEpochId
witnessedProcessEpoch witness = witness.processEpoch

witnessedBindings :: ProcessIdentityWitness -> [(GlobalUniqueId, PrivateUniqueId)]
witnessedBindings witness = witness.bindings

witnessedReverseBindings ::
  ProcessIdentityWitness ->
  [(PrivateUniqueId, GlobalUniqueId)]
witnessedReverseBindings witness = witness.reverseBindings

witnessedNextPrivateUniqueId :: ProcessIdentityWitness -> PrivateUniqueId
witnessedNextPrivateUniqueId witness = witness.nextPrivateUniqueId

-- | The registry with no live or retired process epochs.
emptyPrivateIdentity :: PrivateIdentity
emptyPrivateIdentity = PrivateIdentity Map.empty Set.empty

emptyProcessPrivateIdentity :: ProcessPrivateIdentity
emptyProcessPrivateIdentity =
  ProcessPrivateIdentity Map.empty Map.empty initialPrivateUniqueIdSupply

-- | A complete process-registration transition whose successor remains opaque.
newtype PreparedProcessRegistration
  = PreparedProcessRegistration
      (Prepared PrivateIdentity ())

-- | Prepare an empty private namespace for a newly accepted process epoch.
--
-- Registration belongs to process bootstrap rather than localization so a live
-- process with no IDs still has explicit state. The caller must already have
-- admitted the nominal 'ProcessEpochId' through checked bootstrap or applied
-- Oracle state; this leaf validates only its own registry lifecycle. A retired
-- epoch can never reopen.
prepareProcessRegistration ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError PreparedProcessRegistration
prepareProcessRegistration process state =
  PreparedProcessRegistration
    <$> prepareTransition (registerTransition process) state

-- | Reveal a completely checked process-registration successor.
commitProcessRegistration :: PreparedProcessRegistration -> PrivateIdentity
commitProcessRegistration (PreparedProcessRegistration prepared) =
  fst (commitPrepared prepared)

registerTransition ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError (PrivateIdentity, ())
registerTransition process (PrivateIdentity processes retired)
  | Set.member process retired = Left ProcessEpochIsRetired
  | Map.member process processes = Left ProcessEpochAlreadyRegistered
  | otherwise =
      Right
        ( PrivateIdentity
            (Map.insert process emptyProcessPrivateIdentity processes)
            retired,
          ()
        )

-- | A complete localization transition whose successor cannot be observed until
-- committed.
newtype PreparedLocalization
  = PreparedLocalization
      (Prepared PrivateIdentity PrivateUniqueId)

-- | Reuse the process epoch's existing alias or prepare one bidirectional binding.
-- A retired process epoch cannot be recreated.
prepareLocalization ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  Either PrivateIdentityError PreparedLocalization
prepareLocalization process globalIdentity state =
  PreparedLocalization
    <$> prepareTransition
      (localizeTransition process globalIdentity)
      state

-- | Inspect only the checked output carried by an ordinary localization plan.
preparedPrivateUniqueId :: PreparedLocalization -> PrivateUniqueId
preparedPrivateUniqueId (PreparedLocalization prepared) =
  preparedOutput prepared

-- | Reveal the complete successor and localized identity without further checks.
commitLocalization ::
  PreparedLocalization ->
  (PrivateIdentity, PrivateUniqueId)
commitLocalization (PreparedLocalization prepared) =
  commitPrepared prepared

-- | A fresh private slot prepared independently of the global identity that
-- will occupy it. Unlike ordinary localization, this path never reuses an
-- existing global alias.
data PreparedPrivateUniqueIdAllocation
  = PreparedPrivateUniqueIdAllocation
      ProcessEpochId
      PrivateUniqueId
      PrivateUniqueIdSupply
      (Map GlobalUniqueId PrivateUniqueId)
      (Map PrivateUniqueId GlobalUniqueId)
      (Map ProcessEpochId ProcessPrivateIdentity)
      (Set ProcessEpochId)

preparePrivateUniqueIdAllocation ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError PreparedPrivateUniqueIdAllocation
preparePrivateUniqueIdAllocation process state@(PrivateIdentity processes retired) = do
  ProcessPrivateIdentity privateByGlobal globalByPrivate supply <-
    lookupLiveProcess process state
  let (privateIdentity, successorSupply) = takePrivateUniqueId supply
  Right
    ( PreparedPrivateUniqueIdAllocation
        process
        privateIdentity
        successorSupply
        privateByGlobal
        globalByPrivate
        processes
        retired
    )

preparedAllocatedPrivateUniqueId ::
  PreparedPrivateUniqueIdAllocation ->
  PrivateUniqueId
preparedAllocatedPrivateUniqueId
  (PreparedPrivateUniqueIdAllocation _ privateIdentity _ _ _ _ _) =
    privateIdentity

-- | Install the generated global bits into both inverse maps and reveal the
-- already prepared private result.
commitPrivateUniqueIdAllocation ::
  GlobalUniqueId ->
  PreparedPrivateUniqueIdAllocation ->
  (PrivateIdentity, PrivateUniqueId)
commitPrivateUniqueIdAllocation
  globalIdentity
  ( PreparedPrivateUniqueIdAllocation
      process
      privateIdentity
      successorSupply
      privateByGlobal
      globalByPrivate
      processes
      retired
    ) =
    ( PrivateIdentity
        ( Map.insert
            process
            ( ProcessPrivateIdentity
                (Map.insert globalIdentity privateIdentity privateByGlobal)
                (Map.insert privateIdentity globalIdentity globalByPrivate)
                successorSupply
            )
            processes
        )
        retired,
      privateIdentity
    )

-- | Look up the private name already assigned to a global identity in one live
-- process epoch.
lookupPrivateUniqueId ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  Either PrivateIdentityError (Maybe PrivateUniqueId)
lookupPrivateUniqueId process globalIdentity state = do
  ProcessPrivateIdentity privateByGlobal _ _ <-
    lookupLiveProcess process state
  Right (Map.lookup globalIdentity privateByGlobal)

-- | Resolve an application private ID in the namespace of one live process
-- epoch.
resolvePrivateUniqueId ::
  ProcessEpochId ->
  PrivateUniqueId ->
  PrivateIdentity ->
  Either PrivateIdentityError (Maybe GlobalUniqueId)
resolvePrivateUniqueId process privateIdentity state = do
  ProcessPrivateIdentity _ globalByPrivate _ <-
    lookupLiveProcess process state
  Right (Map.lookup privateIdentity globalByPrivate)

-- | Enumerate every live process namespace in stable process-epoch order.
--
-- Taking the next candidate from the opaque supply is observational only: the
-- successor supply is discarded, so validation cannot advance allocation.
processIdentityWitnesses :: PrivateIdentity -> [ProcessIdentityWitness]
processIdentityWitnesses (PrivateIdentity processes _) =
  fmap witness (Map.toAscList processes)
  where
    witness (process, ProcessPrivateIdentity privateByGlobal globalByPrivate supply) =
      ProcessIdentityWitness
        { processEpoch = process,
          bindings = Map.toAscList privateByGlobal,
          reverseBindings = Map.toAscList globalByPrivate,
          nextPrivateUniqueId = fst (takePrivateUniqueId supply)
        }

-- | A complete process-retirement transition whose successor remains opaque.
newtype PreparedProcessRetirement
  = PreparedProcessRetirement
      (Prepared PrivateIdentity ())

-- | Prepare removal of every live binding for a process epoch and permanent
-- tombstoning of that epoch in this Herald state.
prepareProcessRetirement ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError PreparedProcessRetirement
prepareProcessRetirement process state =
  PreparedProcessRetirement
    <$> prepareTransition (retireTransition process) state

-- | Reveal a completely checked process-retirement successor.
commitProcessRetirement :: PreparedProcessRetirement -> PrivateIdentity
commitProcessRetirement (PreparedProcessRetirement prepared) =
  fst (commitPrepared prepared)

retireTransition ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError (PrivateIdentity, ())
retireTransition process state@(PrivateIdentity processes retired) = do
  _ <- lookupLiveProcess process state
  Right
    ( PrivateIdentity
        (Map.delete process processes)
        (Set.insert process retired),
      ()
    )

-- | Whether an Oracle-authorized retirement first installed the tombstone or
-- exactly replayed one that this owner had already retained.
--
-- Unlike ordinary application retirement, Oracle application is authoritative
-- for the epoch's lifecycle.  It may therefore tombstone an epoch whose local
-- namespace was never installed, as happens when End follows Start before the
-- resident attachment becomes ready.
data OracleProcessRetirementClassification
  = OracleProcessRetirementApplied
  | OracleProcessRetirementExactRetry
  deriving stock (Eq, Ord, Show)

-- | A complete Oracle-authorized process-retirement successor.
data PreparedOracleProcessRetirement
  = PreparedOracleProcessRetirement
      PrivateIdentity
      OracleProcessRetirementClassification

-- | Prepare authoritative retirement of a process epoch.
--
-- A live local namespace is removed, an epoch not yet installed locally gains
-- its tombstone, and an already retained tombstone is an exact unchanged retry.
-- This deliberately does not relax 'prepareProcessRetirement', whose ordinary
-- caller must still prove that the local namespace is live.
prepareOracleProcessRetirement ::
  ProcessEpochId ->
  PrivateIdentity ->
  PreparedOracleProcessRetirement
prepareOracleProcessRetirement
  process
  state@(PrivateIdentity processes retired)
    | Set.member process retired =
        PreparedOracleProcessRetirement
          state
          OracleProcessRetirementExactRetry
    | otherwise =
        PreparedOracleProcessRetirement
          ( PrivateIdentity
              (Map.delete process processes)
              (Set.insert process retired)
          )
          OracleProcessRetirementApplied

preparedOracleProcessRetirementClassification ::
  PreparedOracleProcessRetirement ->
  OracleProcessRetirementClassification
preparedOracleProcessRetirementClassification
  (PreparedOracleProcessRetirement _ classification) =
    classification

-- | Reveal the authoritative successor and its exact-retry classification.
commitOracleProcessRetirement ::
  PreparedOracleProcessRetirement ->
  (PrivateIdentity, OracleProcessRetirementClassification)
commitOracleProcessRetirement
  (PreparedOracleProcessRetirement successor classification) =
    (successor, classification)

localizeTransition ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  Either PrivateIdentityError (PrivateIdentity, PrivateUniqueId)
localizeTransition
  process
  globalIdentity
  state@(PrivateIdentity processes retired) =
    do
      processState <- lookupLiveProcess process state
      case lookupGlobal globalIdentity processState of
        Just privateIdentity -> Right (state, privateIdentity)
        Nothing ->
          allocateInProcess process globalIdentity processState processes retired

allocateInProcess ::
  ProcessEpochId ->
  GlobalUniqueId ->
  ProcessPrivateIdentity ->
  Map ProcessEpochId ProcessPrivateIdentity ->
  Set ProcessEpochId ->
  Either PrivateIdentityError (PrivateIdentity, PrivateUniqueId)
allocateInProcess process globalIdentity processState processes retired =
  Right
    ( PrivateIdentity (Map.insert process successor processes) retired,
      privateIdentity
    )
  where
    (successor, privateIdentity) = allocateGlobal globalIdentity processState

lookupLiveProcess ::
  ProcessEpochId ->
  PrivateIdentity ->
  Either PrivateIdentityError ProcessPrivateIdentity
lookupLiveProcess process (PrivateIdentity processes retired) =
  case Map.lookup process processes of
    Just processState -> Right processState
    Nothing
      | Set.member process retired -> Left ProcessEpochIsRetired
      | otherwise -> Left ProcessEpochIsUnknown

lookupGlobal :: GlobalUniqueId -> ProcessPrivateIdentity -> Maybe PrivateUniqueId
lookupGlobal globalIdentity (ProcessPrivateIdentity privateByGlobal _ _) =
  Map.lookup globalIdentity privateByGlobal

allocateGlobal ::
  GlobalUniqueId ->
  ProcessPrivateIdentity ->
  (ProcessPrivateIdentity, PrivateUniqueId)
allocateGlobal
  globalIdentity
  (ProcessPrivateIdentity privateByGlobal globalByPrivate supply) =
    ( successor,
      privateIdentity
    )
    where
      (privateIdentity, successorSupply) = takePrivateUniqueId supply
      successor =
        ProcessPrivateIdentity
          (Map.insert globalIdentity privateIdentity privateByGlobal)
          (Map.insert privateIdentity globalIdentity globalByPrivate)
          successorSupply
