{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of logical peer bindings and non-authoritative contact hints.
module Eclips.Herald.Discovery.State
  ( State,
    initialState,
    initialJoiningState,
    JoiningControlBaseProblem (..),
    PreparedJoiningControlBase,
    prepareJoiningControlBase,
    commitJoiningControlBase,
    adoptJoiningReplay,
    liveHeraldCatalogue,
    PreparedHeraldAdmission,
    prepareHeraldAdmission,
    commitHeraldAdmission,
    PreparedCandidateHello,
    prepareCandidateHello,
    preparedCandidateHello,
    commitCandidateHello,
    PreparedPeerHello,
    preparePeerHello,
    preparedPeerHelloDisposition,
    commitPeerHello,
    PreparedBindingLoss,
    prepareBindingLoss,
    preparedBindingWasCurrent,
    commitBindingLoss,
    PreparedReconnectCatalogue,
    prepareReconnectCatalogue,
    preparedReconnectCatalogueWasPending,
    commitReconnectCatalogue,
    PreparedKnownContacts,
    prepareKnownContacts,
    preparedKnownContactsDisposition,
    commitKnownContacts,
    PreparedMembershipAdvance,
    MembershipAdvance,
    prepareMembershipAdvance,
    preparedMembershipRetiredBindings,
    preparedMembershipLocalRetired,
    commitMembershipAdvance,
    currentMembership,
    currentPeerBinding,
    takePreparationBindingChanges,
    clearPreparationBindingChanges,
    currentPeerBindings,
    knownHeralds,
    peerDialIntents,
    peerDialIntentFor,
    discoveryStaticView,
    applicationLocatorFor,
    resolveApplicationLocator,
    rememberLocalApplicationLocator,
    validateDiscoveryState,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, HeraldId)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipHistory,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationAdmissionId,
    heraldMembershipGenerationAdmittedHeraldEpoch,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipHistoryCurrent,
    heraldMembershipHistoryGenesis,
  )
import Eclips.Herald.Discovery.Internal
  ( BindingAdmission (..),
    CandidateHello (..),
    DiscoveryInvariantFault (..),
    DiscoveryStatic (..),
    HelloRejection (..),
    KnownContactsDisposition (..),
    KnownHerald (..),
    PeerAddress,
    PeerBinding (..),
    PeerBindingGeneration (..),
    PeerCandidate (..),
    PeerCandidateOpened (..),
    PeerDialClass (..),
    PeerDialGeneration (..),
    PeerDialIntent (..),
    PeerHello (..),
    PeerHelloDisposition (..),
    firstPeerBindingGeneration,
    nextPeerBindingGeneration,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Oracle.Admission
  ( HeraldAdmissionPhase (..),
    HeraldAdmissionRecord,
    admissionManifestHeraldEpoch,
    admissionManifestHeraldId,
    admissionManifestSystem,
    admissionRecordAttempt,
    admissionRecordBeginIndex,
    admissionRecordChangedIndex,
    admissionRecordId,
    admissionRecordManifest,
    admissionRecordPhase,
    admissionRecordPredecessor,
  )

data PeerRecord = PeerRecord
  { lastGeneration :: Maybe PeerBindingGeneration,
    currentBinding :: Maybe PeerBinding,
    reconnectCataloguePending :: Bool
  }
  deriving stock (Eq, Show)

data ContactKey = ContactKey HeraldId HeraldEpoch
  deriving stock (Eq, Ord, Show)

data State = State
  { staticView :: DiscoveryStatic,
    currentMembershipGeneration :: HeraldMembershipGeneration,
    heraldCatalogue :: Map HeraldEpoch HeraldId,
    heraldAdmissions :: Map HeraldEpoch HeraldAdmissionRecord,
    joiningAdmission :: Maybe HeraldAdmissionRecord,
    peers :: Map HeraldEpoch PeerRecord,
    contacts :: Map ContactKey (Set PeerAddress),
    applicationContacts :: Map HeraldEpoch HeraldLocator,
    preparationBindingChanges :: !(Set HeraldEpoch)
  }
  deriving stock (Eq, Show)

initialState :: DiscoveryStatic -> State
initialState staticView =
  State
    { staticView,
      currentMembershipGeneration = staticView.initialMembership,
      heraldCatalogue = staticView.heraldCatalogue,
      heraldAdmissions = Map.empty,
      joiningAdmission = Nothing,
      peers = Map.empty,
      contacts = Map.empty,
      applicationContacts = Map.empty,
      preparationBindingChanges = Set.empty
    }

-- | A newcomer starts with the immutable bootstrap membership and its exact
-- Oracle admission record. It is catalogued but has no ordinary peer authority.
initialJoiningState :: DiscoveryStatic -> HeraldAdmissionRecord -> Either DiscoveryInvariantFault State
initialJoiningState staticView record = do
  let manifest = admissionRecordManifest record
  if admissionManifestSystem manifest == staticView.systemId
    && admissionManifestHeraldId manifest == staticView.localHeraldId
    && admissionManifestHeraldEpoch manifest == staticView.localHeraldEpoch
    && Map.notMember staticView.localHeraldEpoch staticView.heraldCatalogue
    && (case admissionRecordPhase record of AdmissionActivated {} -> False; AdmissionCancelled {} -> False; _ -> True)
    then pure ()
    else Left DiscoveryLocalMembershipContradiction
  -- Retain only local onboarding provenance. The global catalogue starts at
  -- original genesis and is populated by the same contiguous replay as every
  -- old member; a later applicant's predecessor may already include other joins.
  let initial = (initialState staticView) {joiningAdmission = Just record}
  validateDiscoveryState initial
  pure initial

data JoiningControlBaseProblem
  = JoiningControlBaseNotFreshObserver
  | JoiningControlBaseGenesisMismatch
  | JoiningControlBaseAdmissionMismatch
  | JoiningControlBaseDiscoveryInvariant DiscoveryInvariantFault
  deriving stock (Eq, Show)

newtype PreparedJoiningControlBase = PreparedJoiningControlBase (Prepared State ())

-- | Install the current global discovery facts from a checked control base.
-- The caller supplies one canonical projected admission catalogue and its
-- checked membership history. This admission checks the receiver's genesis
-- and pending attempt; it does not replay earlier admission transitions.
-- Receiver-local joining provenance, contacts and transport facts are retained.
prepareJoiningControlBase ::
  HeraldMembershipHistory ->
  [HeraldAdmissionRecord] ->
  State ->
  Either JoiningControlBaseProblem PreparedJoiningControlBase
prepareJoiningControlBase history admissions state = do
  checkedDiscovery (validateDiscoveryState state)
  joining <- maybe (Left JoiningControlBaseNotFreshObserver) Right state.joiningAdmission
  if state.currentMembershipGeneration == state.staticView.initialMembership
    && state.heraldCatalogue == state.staticView.heraldCatalogue
    && Map.null state.heraldAdmissions
    && null (currentPeerBindings state)
    then pure ()
    else Left JoiningControlBaseNotFreshObserver
  if heraldMembershipHistoryGenesis history == state.staticView.initialMembership
    then pure ()
    else Left JoiningControlBaseGenesisMismatch
  let membership = heraldMembershipHistoryCurrent history
      records = Map.fromList [(admissionManifestHeraldEpoch (admissionRecordManifest record), record) | record <- admissions]
      local = state.staticView.localHeraldEpoch
  projected <- maybe (Left JoiningControlBaseAdmissionMismatch) Right (Map.lookup local records)
  -- A fresh receiver may start with the later Seal record while its immutable
  -- source capture precedes Seal. Match the same attempt's identity here; the
  -- aggregate importer binds the exact captured bytes to any supplied seal.
  -- The fresh-owner check above still excludes rewinding observed discovery.
  if admissionRecordId projected == admissionRecordId joining
    && admissionRecordAttempt projected == admissionRecordAttempt joining
    && admissionRecordManifest projected == admissionRecordManifest joining
    && admissionRecordPredecessor projected == admissionRecordPredecessor joining
    && admissionRecordPredecessor projected == membership
    && local `notElem` heraldMembershipGenerationActiveHeraldEpochs membership
    && (case admissionRecordPhase projected of AdmissionActivated {} -> False; AdmissionCancelled {} -> False; _ -> True)
    then pure ()
    else Left JoiningControlBaseAdmissionMismatch
  let catalogue = Map.map (admissionManifestHeraldId . admissionRecordManifest) records
  if Map.null (Map.intersection catalogue state.staticView.heraldCatalogue)
    then pure ()
    else Left JoiningControlBaseGenesisMismatch
  let successor =
        state
          { currentMembershipGeneration = membership,
            heraldCatalogue = Map.union catalogue state.staticView.heraldCatalogue,
            heraldAdmissions = records
          }
  checkedDiscovery (validateDiscoveryState successor)
  PreparedJoiningControlBase <$> prepareTransition (\_ -> Right (successor, ())) state
  where
    checkedDiscovery = either (Left . JoiningControlBaseDiscoveryInvariant) Right

commitJoiningControlBase :: PreparedJoiningControlBase -> State
commitJoiningControlBase (PreparedJoiningControlBase prepared) = fst (commitPrepared prepared)

-- | Replace the canonical catalogue of a passive observer without replacing
-- its local joining provenance, contact hints or transport generations.
adoptJoiningReplay :: State -> State -> Either DiscoveryInvariantFault State
adoptJoiningReplay candidate current = do
  validateDiscoveryState candidate
  validateDiscoveryState current
  if candidate.staticView == current.staticView
    && candidate.joiningAdmission /= Nothing
    && current.joiningAdmission /= Nothing
    && not (localIsActive candidate)
    && not (localIsActive current)
    then pure ()
    else Left DiscoveryLocalMembershipContradiction
  let !successor =
        current
          { currentMembershipGeneration = candidate.currentMembershipGeneration,
            heraldCatalogue = candidate.heraldCatalogue,
            heraldAdmissions = candidate.heraldAdmissions
          }
  validateDiscoveryState successor
  pure successor

-- | Transport rebinding wakes delivery only; it does not invalidate cached
-- endpoint readiness. Reoffering the same logical binding is inert here.
takePreparationBindingChanges :: State -> (Set HeraldEpoch, State)
takePreparationBindingChanges state =
  let changes = state.preparationBindingChanges
   in changes `seq` (changes, clearPreparationBindingChanges state)

clearPreparationBindingChanges :: State -> State
clearPreparationBindingChanges state
  | Set.null state.preparationBindingChanges = state
  | otherwise = state {preparationBindingChanges = Set.empty}

liveHeraldCatalogue :: State -> Map HeraldEpoch HeraldId
liveHeraldCatalogue state = state.heraldCatalogue

newtype PreparedHeraldAdmission = PreparedHeraldAdmission (Prepared State ())

-- | Only a contiguous Oracle admission observation reaches this seam. Retain
-- manifest and admission provenance independently of the immutable genesis map.
prepareHeraldAdmission :: HeraldAdmissionRecord -> State -> Either DiscoveryInvariantFault PreparedHeraldAdmission
prepareHeraldAdmission record state = do
  validateDiscoveryState state
  PreparedHeraldAdmission
    <$> prepareTransition
      (\before -> (\after -> (after, ())) <$> admitHeraldRecord record before)
      state

commitHeraldAdmission :: PreparedHeraldAdmission -> State
commitHeraldAdmission (PreparedHeraldAdmission prepared) = fst (commitPrepared prepared)

admitHeraldRecord :: HeraldAdmissionRecord -> State -> Either DiscoveryInvariantFault State
admitHeraldRecord record state = do
  let manifest = admissionRecordManifest record
      epoch = admissionManifestHeraldEpoch manifest
      identifier = admissionManifestHeraldId manifest
      reject = Left (DiscoveryAdmissionCatalogueContradiction epoch)
  if admissionManifestSystem manifest /= state.staticView.systemId
    || Map.member epoch state.staticView.heraldCatalogue
    then reject
    else pure ()
  case Map.lookup epoch state.heraldAdmissions of
    Nothing ->
      if Map.member epoch state.heraldCatalogue
        || admissionRecordPredecessor record /= state.currentMembershipGeneration
        then reject
        else pure ()
    Just previous ->
      if admissionRecordId previous /= admissionRecordId record
        || admissionRecordManifest previous /= manifest
        || admissionRecordPredecessor previous /= admissionRecordPredecessor record
        || admissionRecordBeginIndex previous /= admissionRecordBeginIndex record
        || admissionRecordChangedIndex previous > admissionRecordChangedIndex record
        || (admissionRecordChangedIndex previous == admissionRecordChangedIndex record && previous /= record)
        || (terminal previous && previous /= record)
        then reject
        else pure ()
  let successor =
        state
          { heraldCatalogue = Map.insert epoch identifier state.heraldCatalogue,
            heraldAdmissions = Map.insert epoch record state.heraldAdmissions
          }
  validateDiscoveryState successor
  pure successor
  where
    terminal previous = case admissionRecordPhase previous of
      AdmissionActivated {} -> True
      AdmissionCancelled {} -> True
      _ -> False

currentMembership :: State -> HeraldMembershipGeneration
currentMembership state = state.currentMembershipGeneration

discoveryStaticView :: State -> DiscoveryStatic
discoveryStaticView state = state.staticView

-- | Retain an explicit local listener observation from the runtime boundary.
rememberLocalApplicationLocator :: Maybe HeraldLocator -> State -> State
rememberLocalApplicationLocator locator state =
  state
    { applicationContacts = case locator of
        Nothing -> state.applicationContacts
        Just contact -> Map.insert state.staticView.localHeraldEpoch contact state.applicationContacts
    }

-- | Only a directly admitted peer Hello supplies an application address.
-- The checked active catalogue, not the address, establishes its authority.
applicationLocatorFor :: HeraldEpoch -> State -> Maybe HeraldLocator
applicationLocatorFor epoch state = do
  _ <- activeHeraldId epoch state
  Map.lookup epoch state.applicationContacts

resolveApplicationLocator :: HeraldLocator -> State -> Maybe HeraldEpoch
resolveApplicationLocator locator state = case [ epoch
                                               | (epoch, candidate) <- Map.toAscList state.applicationContacts,
                                                 candidate == locator,
                                                 activeHeraldId epoch state /= Nothing
                                               ] of
  [epoch] -> Just epoch
  _ -> Nothing

newtype PreparedCandidateHello
  = PreparedCandidateHello (Prepared State CandidateHello)

-- | Derive every outbound local Hello from checked Discovery facts.
prepareCandidateHello ::
  ControlIndex ->
  PeerCandidateOpened ->
  State ->
  Either DiscoveryInvariantFault PreparedCandidateHello
prepareCandidateHello appliedControlIndex opened state = do
  validateDiscoveryState state
  PreparedCandidateHello
    <$> prepareTransition (offerCandidateHello appliedControlIndex opened) state

preparedCandidateHello :: PreparedCandidateHello -> CandidateHello
preparedCandidateHello (PreparedCandidateHello prepared) = preparedOutput prepared

commitCandidateHello :: PreparedCandidateHello -> (State, CandidateHello)
commitCandidateHello (PreparedCandidateHello prepared) = commitPrepared prepared

offerCandidateHello ::
  ControlIndex ->
  PeerCandidateOpened ->
  State ->
  Either DiscoveryInvariantFault (State, CandidateHello)
offerCandidateHello appliedControlIndex opened state =
  if localIsActive state
    then
      Right
        ( rememberLocalApplicationLocator opened.applicationLocator state,
          CandidateHello
            ( PeerCandidate
                state.staticView.localHeraldId
                state.staticView.localHeraldEpoch
                opened.localConnectionNonce
            )
            ( PeerHello
                state.staticView.systemId
                state.staticView.localHeraldId
                state.staticView.localHeraldEpoch
                opened.localConnectionNonce
                opened.advertisedAddresses
                appliedControlIndex
                state.staticView.catalogueDigest
                state.staticView.initialProjectionDigest
                opened.applicationLocator
            )
        )
    else Left DiscoveryLocalHeraldRetired

newtype PreparedPeerHello
  = PreparedPeerHello (Prepared State PeerHelloDisposition)

preparePeerHello ::
  PeerCandidate ->
  PeerHello ->
  State ->
  Either DiscoveryInvariantFault PreparedPeerHello
preparePeerHello candidate hello state = do
  validateDiscoveryState state
  if hello.initialProjectionDigest /= state.staticView.initialProjectionDigest
    && staticPreambleAdmitted state hello
    then
      Left
        ( DiscoveryInitialProjectionMismatch
            state.staticView.initialProjectionDigest
            hello.initialProjectionDigest
        )
    else
      PreparedPeerHello <$> prepareTransition (admitHello candidate hello) state

preparedPeerHelloDisposition :: PreparedPeerHello -> PeerHelloDisposition
preparedPeerHelloDisposition (PreparedPeerHello prepared) = preparedOutput prepared

commitPeerHello :: PreparedPeerHello -> (State, PeerHelloDisposition)
commitPeerHello (PreparedPeerHello prepared) = commitPrepared prepared

admitHello ::
  PeerCandidate ->
  PeerHello ->
  State ->
  Either DiscoveryInvariantFault (State, PeerHelloDisposition)
admitHello candidate hello state = case rejectHello candidate hello state of
  Just rejection -> Right (state, PeerHelloRejected rejection)
  Nothing -> admitCandidate candidate hello state

rejectHello :: PeerCandidate -> PeerHello -> State -> Maybe HelloRejection
rejectHello candidate hello state
  | not (localIsActive state) = Just HelloLocalHeraldRetired
  | hello.systemId /= state.staticView.systemId = Just HelloSystemMismatch
  | hello.catalogueDigest /= state.staticView.catalogueDigest =
      Just HelloCatalogueMismatch
  | hello.heraldId == state.staticView.localHeraldId
      || hello.heraldEpoch == state.staticView.localHeraldEpoch =
      Just HelloSelfConnection
  | activeHeraldId hello.heraldEpoch state
      /= Just hello.heraldId =
      Just
        ( if terminalHeraldId hello.heraldEpoch state == Just hello.heraldId
            then HelloRetiredHerald
            else HelloInactiveHerald
        )
  | hello.initialProjectionDigest /= state.staticView.initialProjectionDigest =
      Nothing
  | not (candidateMatches candidate hello state.staticView) =
      Just HelloCandidateTupleInvalid
  | otherwise = Nothing

staticPreambleAdmitted :: State -> PeerHello -> Bool
staticPreambleAdmitted state hello =
  localIsActive state
    && hello.systemId == state.staticView.systemId
    && hello.catalogueDigest == state.staticView.catalogueDigest
    && hello.heraldId /= state.staticView.localHeraldId
    && hello.heraldEpoch /= state.staticView.localHeraldEpoch
    && activeHeraldId hello.heraldEpoch state
      == Just hello.heraldId

candidateMatches :: PeerCandidate -> PeerHello -> DiscoveryStatic -> Bool
candidateMatches candidate hello staticView =
  candidate.connectionNonce == hello.connectionNonce
    && ( initiator == localInitiator
           || initiator == remoteInitiator
       )
  where
    initiator = (candidate.initiatorHeraldId, candidate.initiatorHeraldEpoch)
    localInitiator = (staticView.localHeraldId, staticView.localHeraldEpoch)
    remoteInitiator = (hello.heraldId, hello.heraldEpoch)

admitCandidate ::
  PeerCandidate ->
  PeerHello ->
  State ->
  Either DiscoveryInvariantFault (State, PeerHelloDisposition)
admitCandidate candidate hello state = case current of
  Just binding
    | binding.selectedCandidate == candidate ->
        let successor =
              mergeHelloAddresses
                hello
                state
                  { peers =
                      Map.insert
                        peer
                        record {reconnectCataloguePending = True}
                        state.peers
                  }
         in Right
              ( successor,
                PeerHelloAccepted ReofferedBinding binding
              )
    | candidate > binding.selectedCandidate ->
        Right
          ( state,
            PeerHelloRejected
              (HelloCandidateNotSelected binding.selectedCandidate)
          )
    | otherwise -> install ReplacedBinding record
  Nothing ->
    install
      (case record.lastGeneration of Nothing -> FirstBinding; Just _ -> ReplacedBinding)
      record
  where
    peer = hello.heraldEpoch
    record = Map.findWithDefault (PeerRecord Nothing Nothing False) peer state.peers
    current = record.currentBinding

    install admission predecessorRecord = do
      let generation = case predecessorRecord.lastGeneration of
            Nothing -> firstPeerBindingGeneration
            Just previous -> nextPeerBindingGeneration previous
          binding =
            PeerBinding
              { remoteHeraldId = hello.heraldId,
                remoteHeraldEpoch = hello.heraldEpoch,
                generation,
                selectedCandidate = candidate
              }
          successorRecord =
            PeerRecord
              { lastGeneration = Just generation,
                currentBinding = Just binding,
                reconnectCataloguePending = True
              }
          successor =
            mergeHelloAddresses
              hello
              state {peers = Map.insert peer successorRecord state.peers, preparationBindingChanges = Set.insert peer state.preparationBindingChanges}
      validateDiscoveryState successor
      Right (successor, PeerHelloAccepted admission binding)

mergeHelloAddresses :: PeerHello -> State -> State
mergeHelloAddresses hello state =
  state
    { contacts =
        Map.insertWith
          Set.union
          (ContactKey hello.heraldId hello.heraldEpoch)
          hello.advertisedAddresses
          state.contacts,
      applicationContacts = case hello.applicationLocator of
        Nothing -> Map.delete hello.heraldEpoch state.applicationContacts
        Just locator -> Map.insert hello.heraldEpoch locator state.applicationContacts
    }

newtype PreparedBindingLoss
  = PreparedBindingLoss (Prepared State Bool)

prepareBindingLoss ::
  PeerBinding ->
  State ->
  Either DiscoveryInvariantFault PreparedBindingLoss
prepareBindingLoss binding state = do
  validateDiscoveryState state
  PreparedBindingLoss <$> prepareTransition (loseBinding binding) state

preparedBindingWasCurrent :: PreparedBindingLoss -> Bool
preparedBindingWasCurrent (PreparedBindingLoss prepared) = preparedOutput prepared

commitBindingLoss :: PreparedBindingLoss -> (State, Bool)
commitBindingLoss (PreparedBindingLoss prepared) = commitPrepared prepared

loseBinding ::
  PeerBinding ->
  State ->
  Either DiscoveryInvariantFault (State, Bool)
loseBinding binding state = case Map.lookup binding.remoteHeraldEpoch state.peers of
  Just record
    | record.currentBinding == Just binding -> do
        let successorRecord =
              record
                { currentBinding = Nothing,
                  reconnectCataloguePending = False
                }
            successor =
              state
                { peers =
                    Map.insert
                      binding.remoteHeraldEpoch
                      successorRecord
                      state.peers,
                  preparationBindingChanges = Set.insert binding.remoteHeraldEpoch state.preparationBindingChanges
                }
        validateDiscoveryState successor
        Right (successor, True)
  _ -> Right (state, False)

newtype PreparedReconnectCatalogue
  = PreparedReconnectCatalogue (Prepared State Bool)

-- | Consume the one reconnect-catalogue permission installed by the latest
-- accepted (including reoffered) Hello for this exact logical binding.
-- Ordinary stream/gap repair offers on the same binding observe 'False' and
-- therefore cannot repeatedly replay the structural and alignment catalogues.
prepareReconnectCatalogue ::
  PeerBinding ->
  State ->
  Either DiscoveryInvariantFault PreparedReconnectCatalogue
prepareReconnectCatalogue binding state = do
  validateDiscoveryState state
  PreparedReconnectCatalogue
    <$> prepareTransition (consumeReconnectCatalogue binding) state

preparedReconnectCatalogueWasPending :: PreparedReconnectCatalogue -> Bool
preparedReconnectCatalogueWasPending (PreparedReconnectCatalogue prepared) =
  preparedOutput prepared

commitReconnectCatalogue :: PreparedReconnectCatalogue -> (State, Bool)
commitReconnectCatalogue (PreparedReconnectCatalogue prepared) =
  commitPrepared prepared

consumeReconnectCatalogue ::
  PeerBinding ->
  State ->
  Either DiscoveryInvariantFault (State, Bool)
consumeReconnectCatalogue binding state =
  case Map.lookup binding.remoteHeraldEpoch state.peers of
    Just record
      | record.currentBinding == Just binding,
        record.reconnectCataloguePending -> do
          let successorRecord = record {reconnectCataloguePending = False}
              successor =
                state
                  { peers =
                      Map.insert
                        binding.remoteHeraldEpoch
                        successorRecord
                        state.peers
                  }
          validateDiscoveryState successor
          Right (successor, True)
    _ -> Right (state, False)

newtype PreparedKnownContacts
  = PreparedKnownContacts (Prepared State KnownContactsDisposition)

prepareKnownContacts ::
  PeerBinding ->
  [KnownHerald] ->
  State ->
  Either DiscoveryInvariantFault PreparedKnownContacts
prepareKnownContacts binding observations state = do
  validateDiscoveryState state
  PreparedKnownContacts
    <$> prepareTransition (mergeKnownContacts binding observations) state

preparedKnownContactsDisposition ::
  PreparedKnownContacts ->
  KnownContactsDisposition
preparedKnownContactsDisposition (PreparedKnownContacts prepared) =
  preparedOutput prepared

commitKnownContacts ::
  PreparedKnownContacts ->
  (State, KnownContactsDisposition)
commitKnownContacts (PreparedKnownContacts prepared) = commitPrepared prepared

mergeKnownContacts ::
  PeerBinding ->
  [KnownHerald] ->
  State ->
  Either DiscoveryInvariantFault (State, KnownContactsDisposition)
mergeKnownContacts binding observations state
  | currentPeerBinding binding.remoteHeraldEpoch state /= Just binding =
      Right (state, KnownContactsStaleBinding)
  | otherwise = do
      let merged = foldl insertObservation state.contacts observations
          disposition =
            if merged == state.contacts
              then KnownContactsUnchanged
              else KnownContactsChanged
          successor = state {contacts = merged}
      validateDiscoveryState successor
      Right (successor, disposition)

insertObservation ::
  Map ContactKey (Set PeerAddress) ->
  KnownHerald ->
  Map ContactKey (Set PeerAddress)
insertObservation contacts observation =
  Map.insertWith
    Set.union
    (ContactKey observation.heraldId observation.observedHeraldEpoch)
    observation.addresses
    contacts

currentPeerBinding :: HeraldEpoch -> State -> Maybe PeerBinding
currentPeerBinding peer state = do
  record <- Map.lookup peer state.peers
  record.currentBinding

currentPeerBindings :: State -> [PeerBinding]
currentPeerBindings state =
  mapMaybe (\record -> record.currentBinding) (Map.elems state.peers)

knownHeralds :: State -> [KnownHerald]
knownHeralds state =
  [ KnownHerald identifier epoch addresses
  | (ContactKey identifier epoch, addresses) <- Map.toAscList state.contacts
  ]

-- | Derive the complete current direct-contact desire in canonical target
-- order.  This is a level predicate over already admitted Discovery state, not
-- an attempt allocator.
peerDialIntents :: State -> [PeerDialIntent]
peerDialIntents state =
  mapMaybe
    (\(ContactKey identifier epoch, _) -> peerDialIntentFor identifier epoch state)
    (Map.toAscList state.contacts)

-- | Re-evaluate one exact retained contact, as required after loss of its
-- current binding.
peerDialIntentFor ::
  HeraldId ->
  HeraldEpoch ->
  State ->
  Maybe PeerDialIntent
peerDialIntentFor identifier epoch state
  | not (localIsActive state) = Nothing
  | activeHeraldId epoch state /= Just identifier = Nothing
  | identifier == state.staticView.localHeraldId = Nothing
  | epoch == state.staticView.localHeraldEpoch = Nothing
  | currentPeerBinding epoch state /= Nothing = Nothing
  | otherwise = do
      addresses <- Map.lookup (ContactKey identifier epoch) state.contacts
      if Set.null addresses
        then Nothing
        else
          Just
            ( PeerDialIntent
                identifier
                epoch
                addresses
                (dialClassFor identifier epoch state.staticView)
                (dialGenerationFor epoch state)
            )

dialClassFor :: HeraldId -> HeraldEpoch -> DiscoveryStatic -> PeerDialClass
dialClassFor identifier epoch staticView
  | (staticView.localHeraldId, staticView.localHeraldEpoch) < (identifier, epoch) =
      ImmediatePeerDial
  | otherwise = FallbackPeerDial

-- The next logical binding generation is also a stable permission generation:
-- duplicate/address-growth gossip retains it, while every accepted binding or
-- replacement advances the generation observed after a later current loss.
dialGenerationFor :: HeraldEpoch -> State -> PeerDialGeneration
dialGenerationFor epoch state =
  case Map.lookup epoch state.peers >>= (.lastGeneration) of
    Nothing -> PeerDialGeneration 1
    Just (PeerBindingGeneration generation) ->
      PeerDialGeneration (generation + 1)

validateDiscoveryState :: State -> Either DiscoveryInvariantFault ()
validateDiscoveryState state = do
  mapM_ validateMember (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs state.currentMembershipGeneration))
  if heraldMembershipGenerationPredecessor state.currentMembershipGeneration == Nothing
    && not (localIsActive state)
    && state.joiningAdmission == Nothing
    then Left DiscoveryLocalMembershipContradiction
    else Right ()
  if Map.isSubmapOf state.staticView.heraldCatalogue state.heraldCatalogue
    then pure ()
    else Left DiscoveryLocalMembershipContradiction
  mapM_ validateAdmission (Map.toAscList state.heraldAdmissions)
  mapM_ validatePeer (Map.toAscList state.peers)
  where
    validateMember epoch = case Map.lookup epoch state.heraldCatalogue of
      Nothing -> Left (DiscoveryMembershipOutsideCatalogue epoch)
      Just _ -> Right ()

    validateAdmission (epoch, record)
      | let manifest = admissionRecordManifest record,
        admissionManifestSystem manifest == state.staticView.systemId,
        admissionManifestHeraldEpoch manifest == epoch,
        Map.lookup epoch state.heraldCatalogue == Just (admissionManifestHeraldId manifest) =
          Right ()
      | otherwise = Left (DiscoveryAdmissionCatalogueContradiction epoch)

    validatePeer (peer, record) = case record.currentBinding of
      Nothing
        | record.reconnectCataloguePending ->
            Left (DiscoveryReconnectCatalogueContradiction peer)
        | otherwise -> Right ()
      Just binding
        | binding.remoteHeraldEpoch /= peer
            || activeHeraldId peer state
              /= Just binding.remoteHeraldId ->
            Left (DiscoveryBindingMembershipContradiction peer)
        | record.lastGeneration /= Just binding.generation ->
            Left (DiscoveryBindingGenerationContradiction peer)
        | not (bindingCandidateValid binding state.staticView) ->
            Left (DiscoveryBindingCandidateContradiction peer)
        | otherwise -> Right ()

bindingCandidateValid :: PeerBinding -> DiscoveryStatic -> Bool
bindingCandidateValid binding staticView =
  initiator == localInitiator || initiator == remoteInitiator
  where
    candidate = binding.selectedCandidate
    initiator = (candidate.initiatorHeraldId, candidate.initiatorHeraldEpoch)
    localInitiator = (staticView.localHeraldId, staticView.localHeraldEpoch)
    remoteInitiator = (binding.remoteHeraldId, binding.remoteHeraldEpoch)

activeHeraldId :: HeraldEpoch -> State -> Maybe HeraldId
activeHeraldId epoch state
  | epoch `elem` heraldMembershipGenerationActiveHeraldEpochs state.currentMembershipGeneration =
      Map.lookup epoch state.heraldCatalogue
  | otherwise = Nothing

terminalHeraldId :: HeraldEpoch -> State -> Maybe HeraldId
terminalHeraldId epoch state
  | Map.member epoch state.staticView.heraldCatalogue = Map.lookup epoch state.heraldCatalogue
  | otherwise = do
      record <- Map.lookup epoch state.heraldAdmissions
      case admissionRecordPhase record of
        AdmissionActivated {} -> Map.lookup epoch state.heraldCatalogue
        AdmissionCancelled {} -> Map.lookup epoch state.heraldCatalogue
        _ -> Nothing

localIsActive :: State -> Bool
localIsActive state =
  activeHeraldId state.staticView.localHeraldEpoch state
    == Just state.staticView.localHeraldId

data MembershipAdvance = MembershipAdvance [PeerBinding] Bool
  deriving stock (Eq, Show)

newtype PreparedMembershipAdvance
  = PreparedMembershipAdvance (Prepared State MembershipAdvance)

prepareMembershipAdvance ::
  HeraldMembershipGeneration ->
  State ->
  Either DiscoveryInvariantFault PreparedMembershipAdvance
prepareMembershipAdvance successor state = do
  validateDiscoveryState state
  PreparedMembershipAdvance <$> prepareTransition (advanceMembership successor) state

preparedMembershipRetiredBindings :: PreparedMembershipAdvance -> [PeerBinding]
preparedMembershipRetiredBindings (PreparedMembershipAdvance prepared) =
  case preparedOutput prepared of
    MembershipAdvance bindings _ -> bindings

preparedMembershipLocalRetired :: PreparedMembershipAdvance -> Bool
preparedMembershipLocalRetired (PreparedMembershipAdvance prepared) =
  case preparedOutput prepared of
    MembershipAdvance _ localRetired -> localRetired

commitMembershipAdvance ::
  PreparedMembershipAdvance ->
  (State, MembershipAdvance)
commitMembershipAdvance (PreparedMembershipAdvance prepared) = commitPrepared prepared

advanceMembership ::
  HeraldMembershipGeneration ->
  State ->
  Either DiscoveryInvariantFault (State, MembershipAdvance)
advanceMembership successor state = do
  if heraldMembershipGenerationPredecessor successor
    == Just (heraldMembershipGenerationId state.currentMembershipGeneration)
    then Right ()
    else Left DiscoveryMembershipNotExactSuccessor
  let predecessorMembers = Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs state.currentMembershipGeneration))
      successorMembers = Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))
  case heraldMembershipGenerationAdmittedHeraldEpoch successor of
    Just applicant -> do
      record <- maybe (Left (DiscoveryMembershipOutsideCatalogue applicant)) Right (Map.lookup applicant state.heraldAdmissions)
      if heraldMembershipGenerationAdmissionId successor == Just (admissionRecordId record)
        && Set.notMember applicant predecessorMembers
        && Set.insert applicant predecessorMembers == successorMembers
        && admissionRecordPredecessor record == state.currentMembershipGeneration
        && (case admissionRecordPhase record of AdmissionActivated _ activated -> activated == successor; _ -> False)
        then pure ()
        else Left DiscoveryMembershipNotExactSuccessor
      let next = state {currentMembershipGeneration = successor}
      validateDiscoveryState next
      pure (next, MembershipAdvance [] False)
    Nothing -> do
      retired <- maybe (Left DiscoveryMembershipNotExactSuccessor) Right (heraldMembershipGenerationRetiredHeraldEpoch successor)
      if Set.member retired predecessorMembers && Set.delete retired predecessorMembers == successorMembers
        then retireMember retired
        else Left DiscoveryMembershipNotExactSuccessor
  where
    retireMember retired =
      if retired == state.staticView.localHeraldEpoch
        then
          let retiredBindings = foldMap (maybe [] pure . (.currentBinding)) state.peers
              clearBinding record = record {currentBinding = Nothing, reconnectCataloguePending = False}
              next =
                state
                  { currentMembershipGeneration = successor,
                    peers = Map.map clearBinding state.peers
                  }
           in finish retired retiredBindings next
        else case Map.lookup retired state.peers of
          Just record ->
            let retiredBindings = maybe [] pure record.currentBinding
                successorRecord = record {currentBinding = Nothing, reconnectCataloguePending = False}
                next =
                  state
                    { currentMembershipGeneration = successor,
                      peers = Map.insert retired successorRecord state.peers
                    }
             in finish retired retiredBindings next
          Nothing ->
            finish retired [] state {currentMembershipGeneration = successor}
    finish retired retiredBindings next = do
      let notified = next {preparationBindingChanges = foldr (Set.insert . (.remoteHeraldEpoch)) next.preparationBindingChanges retiredBindings}
      validateDiscoveryState notified
      Right
        ( notified,
          MembershipAdvance
            retiredBindings
            (retired == state.staticView.localHeraldEpoch)
        )
