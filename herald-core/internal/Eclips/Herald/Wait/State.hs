-- | Exact level-triggered application wait registrations.
--
-- This owner retains resolved queries but does not inspect Store state. The
-- application-call coordinator evaluates registrations against an explicit
-- predecessor or successor Store projection, then prepares the corresponding
-- removals here.
module Eclips.Herald.Wait.State
  ( State,
    emptyState,
    WaitRegistration,
    waitRegistration,
    waitRegistrationId,
    waitRegistrationSessionId,
    waitRegistrationRequestId,
    waitRegistrationProcessEpoch,
    waitRegistrationProcessPosition,
    waitRegistrationQueries,
    waitRegistrations,
    lookupWaitRegistration,
    WaitPreparationError (..),
    PreparedWaitRegistration,
    prepareWaitRegistration,
    commitWaitRegistration,
    PreparedWaitRemoval,
    prepareWaitRemoval,
    prepareSessionWaitRemoval,
    prepareProcessWaitRemoval,
    preparedRemovedWaits,
    commitWaitRemoval,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (ProcessEpochId)
import Eclips.Herald.Application.Request.Internal
  ( ProcessAcceptancePosition,
    RequestId,
    WaitId,
  )
import Eclips.Herald.Application.Session (ApplicationSessionId)
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.Query (ResolvedQuery)

-- | One pending wait occurrence and its immutable admitted query disjunction.
data WaitRegistration
  = WaitRegistration
      WaitId
      ApplicationSessionId
      RequestId
      ProcessEpochId
      ProcessAcceptancePosition
      [ResolvedQuery]
  deriving stock (Eq, Show)

waitRegistration ::
  WaitId ->
  ApplicationSessionId ->
  RequestId ->
  ProcessEpochId ->
  ProcessAcceptancePosition ->
  [ResolvedQuery] ->
  WaitRegistration
waitRegistration = WaitRegistration

waitRegistrationId :: WaitRegistration -> WaitId
waitRegistrationId (WaitRegistration wait _ _ _ _ _) = wait

waitRegistrationSessionId :: WaitRegistration -> ApplicationSessionId
waitRegistrationSessionId (WaitRegistration _ session _ _ _ _) = session

waitRegistrationRequestId :: WaitRegistration -> RequestId
waitRegistrationRequestId (WaitRegistration _ _ request _ _ _) = request

waitRegistrationProcessEpoch :: WaitRegistration -> ProcessEpochId
waitRegistrationProcessEpoch (WaitRegistration _ _ _ process _ _) = process

waitRegistrationProcessPosition ::
  WaitRegistration -> ProcessAcceptancePosition
waitRegistrationProcessPosition (WaitRegistration _ _ _ _ position _) = position

waitRegistrationQueries :: WaitRegistration -> [ResolvedQuery]
waitRegistrationQueries (WaitRegistration _ _ _ _ _ queries) = queries

newtype State = State (Map WaitId WaitRegistration)
  deriving stock (Eq)

emptyState :: State
emptyState = State Map.empty

waitRegistrations :: State -> [WaitRegistration]
waitRegistrations (State registrations) = Map.elems registrations

lookupWaitRegistration :: WaitId -> State -> Maybe WaitRegistration
lookupWaitRegistration wait (State registrations) = Map.lookup wait registrations

data WaitPreparationError
  = WaitAlreadyRegistered WaitId
  | WaitRegistrationMissing WaitId
  deriving stock (Eq, Show)

newtype PreparedWaitRegistration
  = PreparedWaitRegistration (Prepared State ())

prepareWaitRegistration ::
  WaitRegistration ->
  State ->
  Either WaitPreparationError PreparedWaitRegistration
prepareWaitRegistration registration state =
  PreparedWaitRegistration
    <$> prepareTransition install state
  where
    install (State registrations)
      | Map.member wait registrations = Left (WaitAlreadyRegistered wait)
      | otherwise =
          Right
            ( State (Map.insert wait registration registrations),
              ()
            )
    wait = waitRegistrationId registration

commitWaitRegistration :: PreparedWaitRegistration -> State
commitWaitRegistration (PreparedWaitRegistration prepared) =
  fst (commitPrepared prepared)

newtype PreparedWaitRemoval
  = PreparedWaitRemoval (Prepared State [WaitRegistration])

-- | Prepare removal of an exact canonical set. Missing registrations are an
-- invariant contradiction after the coordinator has selected them.
prepareWaitRemoval ::
  [WaitId] ->
  State ->
  Either WaitPreparationError PreparedWaitRemoval
prepareWaitRemoval waits state =
  PreparedWaitRemoval
    <$> prepareTransition removeEvery state
  where
    removeEvery predecessor =
      foldM removeOne (predecessor, []) waits
    removeOne (State registrations, removed) wait =
      case Map.lookup wait registrations of
        Nothing -> Left (WaitRegistrationMissing wait)
        Just registration ->
          Right
            ( State (Map.delete wait registrations),
              removed <> [registration]
            )

-- | End-session removal is total: an empty session registration set is valid.
prepareSessionWaitRemoval ::
  ApplicationSessionId ->
  State ->
  Either WaitPreparationError PreparedWaitRemoval
prepareSessionWaitRemoval session state =
  PreparedWaitRemoval
    <$> prepareTransition totalRemoval state
  where
    totalRemoval ::
      State -> Either WaitPreparationError (State, [WaitRegistration])
    totalRemoval (State registrations) =
      Right
        ( State retained,
          Map.elems removed
        )
      where
        (removed, retained) =
          Map.partition
            ((== session) . waitRegistrationSessionId)
            registrations

-- | Process retirement removes every registration owned by the epoch in one
-- canonical, total preparation.  No registrations is a valid exact retry.
prepareProcessWaitRemoval ::
  ProcessEpochId ->
  State ->
  Either WaitPreparationError PreparedWaitRemoval
prepareProcessWaitRemoval process state =
  PreparedWaitRemoval
    <$> prepareTransition totalRemoval state
  where
    totalRemoval ::
      State -> Either WaitPreparationError (State, [WaitRegistration])
    totalRemoval (State registrations) =
      Right
        ( State retained,
          Map.elems removed
        )
      where
        (removed, retained) =
          Map.partition
            ((== process) . waitRegistrationProcessEpoch)
            registrations

preparedRemovedWaits :: PreparedWaitRemoval -> [WaitRegistration]
preparedRemovedWaits (PreparedWaitRemoval prepared) = preparedOutput prepared

commitWaitRemoval :: PreparedWaitRemoval -> (State, [WaitRegistration])
commitWaitRemoval (PreparedWaitRemoval prepared) = commitPrepared prepared
