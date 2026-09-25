{-# LANGUAGE OverloadedStrings #-}

module PrimordialGrantProperties (tests) where

import ApplicationLabelProperties qualified as Labels
import ConfiguredProcessProperties (step)
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Private
import Eclips.Application.Types.Rejection (ApplicationRejection (ApplicationOperateNotPermitted, EnvironmentSourcesUnavailable))
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Domain.Publication qualified as Publication
import Eclips.Domain.Route (destinationDelta, routeDestinations)
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup (deriveSystemViewDeltaId, mkInitialProjectionDigest, profileCatalogueDigest)
import Eclips.Domain.StructuralConsequence (predefinedDisappearanceCause)
import Eclips.Domain.Topology (deriveMemberSetDigest)
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Primordial.Transfer qualified as Transfer
import Eclips.Herald.Application.PrivateIdentity qualified as Identity
import Eclips.Herald.Application.Request.Internal (requestId)
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedSystemId)
import Eclips.Herald.Input (HeraldInputBody (ApplicationRequestInput))
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Publication.Route qualified as Route
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.NewEnvironment qualified as NewEnvironment
import GenesisFixtures (fixtureCheckedGenesis)
import NewEnvironmentProperties (Fixture (..), fixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), Property, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup
    "checked primordial grants"
    [ testProperty "arbitrary partial selections copy only normal grants and localize aliases once" propExactSelection,
      testProperty "retained transfer roundtrips exact arbitrary partial grants and admits against local facts" propTransferSelection,
      testProperty "unknown private aliases reject before grant admission" propUnknownAlias,
      testCase "transfer reinstalls a selected dynamic controlled record through registry and observation admission" caseTransferObservation,
      testCase "transfer retains immutable first observation separately from a later forward" caseTransferForwardedObservation,
      testCase "transfer preserves a receiver's later first observation and released label" caseTransferReceiverHistory,
      testCase "private type refinements cannot invent a writer role" caseForgedRole,
      testProperty "removing either retained source rejects without borrowing another writer" propRemovedSource,
      testCase "a child process name needs a normal grant, never only a private alias" caseNameGrant,
      testCase "two selected sources privately construct the endpoint prefix without leaking descriptions" casePartialNewEnvironment,
      testCase "missing startup sources reject newenv without synthesizing roots" caseNoSources,
      testCase "selected passive writers confer possession without current operation" casePassiveSources,
      testCase "source sort admission checks both writer roles" caseSourceSorts,
      testCase "conventional startup records the canonical carried sort of all endpoints" caseConventionalEndpointSorts
    ]

base :: HeraldState
base = fixtureState fixture
parent :: ProcessEpochId
parent = fixtureProcess fixture
child :: ProcessEpochId
child = checked (mkProcessEpochId (Bytes.replicate 32 0xe8))

sourceAccess :: Access.PrimordialAccess
sourceAccess = Application.bootstrapAccessPrimordial (maybe (error "missing parent access") id (Application.applicationBootstrapAccess parent (startupApplicationState base)))

admit :: Access.PrimordialSelection -> Primordial.CheckedPrimordialGrant
admit selected = checked (Primordial.checkPrimordialSelection parent selected (Application.applicationPrivateIdentity (startupApplicationState base)) (startupControlledState base) (startupStructuralProgressState base))

withChild :: Controlled.State
withChild = Controlled.commitControlledBootstrap (checked (Controlled.prepareControlledBootstrap fact [] (startupControlledState base)))
  where
    fact = Controlled.processFact (checked (mkProcessId (Bytes.replicate 32 0xe9))) child (checkedLocalHeraldEpoch fixtureCheckedGenesis) genesisAuthorityEpoch

caseTransferObservation :: IO ()
caseTransferObservation = do
  (sourceState, object, owner, _, _) <- Labels.settledDeltaControlledFixtureWithPendingWait
  checkTransferObservation sourceState object owner (Access.Reader . Private.asPrivateDeltaId)

caseTransferForwardedObservation :: IO ()
caseTransferForwardedObservation = do
  (initial, object, owner, _, forwarding) <- Labels.settledNeutralControlledFixtureWithForwardRequest
  (forwarded, _) <- step (ApplicationRequestInput forwarding) initial
  sourceState <- Labels.settleStructuralPublication forwarded
  let record = maybe (error "missing forwarded neutral") id (Controlled.controlledLocalRecord object (startupControlledState sourceState))
  assertBool
    "fixture has two distinct accepted publications"
    (Controlled.controlledRecordFirstPublication record /= Controlled.controlledRecordLatestPublication record)
  assertEqual
    "forwarding preserves the immutable definition"
    (Publication.checkedPublicationValue (Controlled.controlledRecordFirstPublication record))
    (Publication.checkedPublicationValue (Controlled.controlledRecordLatestPublication record))
  checkTransferObservation sourceState object owner (Access.Object . Private.asPrivateObjectId)

caseTransferReceiverHistory :: IO ()
caseTransferReceiverHistory = do
  (initial, object, owner, _, forwarding) <- Labels.settledNeutralControlledFixtureWithForwardRequest
  (forwarded, _) <- step (ApplicationRequestInput forwarding) initial
  sourceState <- Labels.settleStructuralPublication forwarded
  let sourceControlled = startupControlledState sourceState
      sourceRecord = present (Controlled.controlledLocalRecord object sourceControlled)
      sourceFirst = Controlled.controlledRecordFirstPublication sourceRecord
      sourceLatest = Controlled.controlledRecordLatestPublication sourceRecord
      occurrence = Controlled.controlledRecordOccurrenceId sourceRecord
      descriptor = Registry.registryEntryDescriptor (present (Registry.lookupEffectiveSort (Controlled.controlledRecordSortId sourceRecord) (startupSortRegistryState sourceState)))
      oldId = Publication.checkedPublicationId sourceFirst
      receiverId = publicationId (publicationNabla oldId) (publicationAuthorityEpoch oldId) (publicationSourceHeraldEpoch oldId) (nablaSequence 100)
      receiverValue = case Value.viewValue (Publication.checkedPublicationValue sourceFirst) of
        Value.RecordValue fields -> Value.recordValueMap (Map.insert (checked (Value.mkFieldName "label")) (Value.labelValue ((Value.ProcessLabel child, 0))) fields)
        _ -> error "neutral carrier is not a record"
      receiverFirst = checked (Publication.mkCheckedPublication descriptor receiverId receiverValue)
      receiverObservation = checked (Controlled.checkControlledObservation descriptor occurrence receiverFirst)
      receiverObserved = fst (Controlled.commitControlledPeerObservation (checked (Controlled.prepareControlledPeerObservation receiverObservation withChild)))
      digest = checked (mkInitialProjectionDigest (Bytes.replicate 32 0xa1))
      prior = checked (Controlled.checkControlledLabelPrior digest object receiverObserved)
      proposed = Label.releasedLabel (Value.VoidLabel, 1)
      authority = Controlled.controlledLabelPriorAuthority prior
      source = checkedLocalHeraldEpoch (startupGenesis sourceState)
      facts = checked (Label.mkPreparedLabelFacts (checked (mkLabelDecisionId (Bytes.replicate 32 0xa2))) (controlIndex 2) object proposed (Controlled.controlledLabelPriorEffectiveState prior) (Controlled.controlledLabelPriorRevision prior) profileCatalogueDigest authority (Controlled.controlledLabelPriorAuthorityJustification prior) (Label.preparedAuthorityDisposition authority ((Value.ProcessLabel child, 0)) proposed) (deriveMemberSetDigest (source :| [])))
      receiverReleased = Controlled.commitControlledLabelRelease (checked (Controlled.prepareControlledLabelRelease digest facts (controlIndex 2) receiverObserved))
      identities = Application.applicationPrivateIdentity (startupApplicationState sourceState)
      alias = present (checked (Identity.lookupPrivateUniqueId owner (globalUniqueIdFromGlobalObjectId object) identities))
      selection = checked (Access.primordialSelection [("object", Access.Object (Private.asPrivateObjectId alias))] Set.empty Nothing)
      grant = checked (Primordial.checkPrimordialSelection owner selection identities sourceControlled (startupStructuralProgressState sourceState))
      transfer = checked (Transfer.captureStartupTransfer owner source grant sourceState)
      recipient = replaceStartupControlledState receiverReleased sourceState
      expectedWinner = if Publication.checkedPublicationWinnerKey sourceLatest > Publication.checkedPublicationWinnerKey receiverFirst then sourceLatest else receiverFirst
  assertBool "the receiver first saw a different label" (Controlled.controlledRecordObservedLabel sourceRecord /= Controlled.controlledRecordObservedLabel (present (Controlled.controlledLocalRecord object receiverReleased)))
  case Transfer.admitStartupTransfer source transfer recipient of
    Right (Just (received, successor)) -> do
      let final = startupControlledState successor
          record = present (Controlled.controlledLocalRecord object final)
      assertEqual "normal grant stays exact" grant received
      assertEqual "receiver retains its own first observation" receiverFirst (Controlled.controlledRecordFirstPublication record)
      assertEqual "ordinary winner comparison merges retained latest" expectedWinner (Controlled.controlledRecordLatestPublication record)
      assertEqual "source latest remains exact evidence" (Just sourceLatest) (Controlled.controlledRecordObservation (Publication.checkedPublicationId sourceLatest) record)
      assertEqual "current released label is unchanged" (Controlled.controlledReleasedLabelRecord object receiverReleased) (Controlled.controlledReleasedLabelRecord object final)
      assertBool "source possession is not copied" (not (Controlled.controlledHasNormalPossession owner object final))
    Left problem -> assertFailure ("existing receiver history rejected retained transfer: " <> show problem)
    Right Nothing -> assertFailure "current receiver object unexpectedly waited"
  where
    present = maybe (error "missing receiver history fixture fact") id

checkTransferObservation :: HeraldState -> GlobalObjectId -> ProcessEpochId -> (Private.PrivateUniqueId -> Access.PrimordialEntry) -> IO ()
checkTransferObservation sourceState object owner selectedEntry = do
  let identities = Application.applicationPrivateIdentity (startupApplicationState sourceState)
      alias =
        maybe
          (error "dynamic delta has no private name")
          id
          (checked (Identity.lookupPrivateUniqueId owner (globalUniqueIdFromGlobalObjectId object) identities))
      selection = checked (Access.primordialSelection [("selected.reader", selectedEntry alias)] Set.empty Nothing)
      grant = checked (Primordial.checkPrimordialSelection owner selection identities (startupControlledState sourceState) (startupStructuralProgressState sourceState))
      source = checkedLocalHeraldEpoch (startupGenesis sourceState)
      transfer = checked (Transfer.captureStartupTransfer owner source grant sourceState)
      -- A focused receiver boundary retains the already applied structure and
      -- Oracle history but has not yet observed this controlled publication.
      recipient = replaceStartupControlledState (startupControlledState base) sourceState
  assertEqual
    "recipient starts without the selected controlled record"
    Nothing
    (Controlled.controlledLocalRecord object (startupControlledState recipient))
  case Transfer.admitStartupTransfer source transfer recipient of
    Right (Just (received, successor)) -> do
      assertEqual "checked transfer grants remain exact" grant received
      assertEqual
        "publication evidence reinstalls the exact semantic observation"
        (observation <$> Controlled.controlledLocalRecord object (startupControlledState sourceState))
        (observation <$> Controlled.controlledLocalRecord object (startupControlledState successor))
      assertBool
        "transfer does not copy the source parent possession table"
        (not (Controlled.controlledHasNormalPossession owner object (startupControlledState successor)))
      assertEqual
        "grant transfer never synthesizes bootstrap roots"
        (Controlled.controlledRootFacts (startupControlledState recipient))
        (Controlled.controlledRootFacts (startupControlledState successor))
    Left problem -> assertFailure ("transfer admission failed: " <> show problem)
    Right Nothing -> assertFailure "retained applied prerequisites unexpectedly waited"
  where
    observation record =
      ( Controlled.controlledRecordSortId record,
        Controlled.controlledRecordOccurrenceId record,
        Controlled.controlledRecordFirstPublication record,
        Controlled.controlledRecordLatestPublication record
      )

propTransferSelection :: NonNegative Int -> Property
propTransferSelection (NonNegative count) =
  counterexample "transfer changed the selected grant or admitted the wrong source"
    $ and
      [ Transfer.decodeStartupTransfer encoded == Right retained,
        Transfer.decodeStartupTransfer (encoded <> Bytes.singleton 0) == Left Transfer.StartupTransferMalformed,
        case Transfer.admitStartupTransfer other retained base of
          Left Transfer.StartupTransferSourceMismatch -> True
          _ -> False,
        case Transfer.admitStartupTransfer source retained base of
          Right (Just (received, successor)) ->
            received == grant
              && startupControlledState successor == startupControlledState base
              && startupSortRegistryState successor == startupSortRegistryState base
          _ -> False
      ]
  where
    candidates = take (count `mod` (Map.size (Access.accessEntries sourceAccess) + 1)) (Map.toAscList (Access.accessEntries sourceAccess))
    selected = checked (Access.primordialSelection candidates Set.empty Nothing)
    grant = admit selected
    source = checkedLocalHeraldEpoch fixtureCheckedGenesis
    other = checked (mkHeraldEpoch (Bytes.replicate 32 0xea))
    retained = checked (Transfer.captureStartupTransfer parent source grant base)
    encoded = Transfer.encodeStartupTransfer retained

propExactSelection :: NonNegative Int -> Property
propExactSelection (NonNegative count) =
  counterexample "selected aliases or exact possession changed"
    $ and
      [ Map.keysSet localized == Map.keysSet selectedEntries,
        Access.accessEndpointSorts (Application.bootstrapAccessPrimordial access) == Map.mapMaybe endpointSort (Primordial.primordialGrantEntries grant),
        Access.accessObjectSorts (Application.bootstrapAccessPrimordial access) == Map.mapMaybe (\case Primordial.GlobalObject _ ownSort -> Just ownSort; _ -> Nothing) (Primordial.primordialGrantEntries grant),
        Controlled.controlledNormalPossessionEntries installed == Set.toAscList expected,
        all aliasesMatch (Map.toList selectedEntries),
        all (\possession -> Controlled.controlledHasNormalPossession child (Controlled.controlledGrantObject possession) afterSourceEnd) (Primordial.primordialGrantPossessions grant),
        all ((/= child) . fst) (Controlled.controlledNormalPossessionEntries afterChildEnd),
        Map.lookup "identity" localized == fmap (Access.Identity . Access.primordialEntryIdentity) (Map.lookup "alias" localized)
      ]
  where
    candidates = take (count `mod` (Map.size (Access.accessEntries sourceAccess) + 1)) (Map.toAscList (Access.accessEntries sourceAccess))
    self = Private.privateProcessUniqueId (Application.bootstrapAccessProcess (maybe (error "missing") id (Application.applicationBootstrapAccess parent (startupApplicationState base))))
    selectedEntries = Map.fromList ([("identity", Access.Identity self), ("alias", Access.Identity self)] <> [("selected." <> key, entry) | (key, entry) <- candidates])
    grant = admit (checked (Access.primordialSelection (Map.toList selectedEntries) Set.empty Nothing))
    installed = Controlled.commitControlledGrants (checked (Controlled.prepareControlledGrants child (Primordial.primordialGrantPossessions grant) withChild))
    afterSourceEnd = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement parent installed)
    afterChildEnd = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement child installed)
    (application, access) = Application.commitApplicationBootstrap (checked (Application.prepareApplicationPrimordialBootstrap (Session.applicationAttachmentForProcess child) child grant (startupApplicationState base)))
    localized = Access.accessEntries (Application.bootstrapAccessPrimordial access)
    expected = Set.fromList (Controlled.controlledNormalPossessionEntries withChild <> [(child, Controlled.controlledGrantObject possessed) | possessed <- Primordial.primordialGrantPossessions grant])
    aliasesMatch (key, entry) = doResolve parent entry (startupApplicationState base) == (Map.lookup key localized >>= \local -> doResolve child local application)
    doResolve process entry ownerState = either (const Nothing) Just (Application.resolveApplicationPrivateUniqueId process (Access.primordialEntryIdentity entry) ownerState)
    endpointSort = \case
      Primordial.GlobalWriter _ carried -> Just carried
      Primordial.GlobalReader _ carried -> Just carried
      _ -> Nothing

caseConventionalEndpointSorts :: IO ()
caseConventionalEndpointSorts = do
  let rolePairs = zip Access.allApplicationPredefinedSortRoles SortProfile.allPredefinedSortRoles
      expected =
        Map.fromList
          [ (key role, SortProfile.profileSortFor domainRole)
          | (role, domainRole) <- rolePairs,
            key <- [Access.environmentWriterKey, Access.environmentReaderKey]
          ]
  assertEqual "all twelve startup endpoints retain their complete canonical sort IDs" expected (Access.accessEndpointSorts sourceAccess)
  let objectSorts = Map.fromList ((Access.environmentHubKey, SortProfile.profileSortFor SortProfile.NeutralVertexRole) : [(Access.environmentEdgeKey role direction, SortProfile.profileSortFor SortProfile.EdgeRole) | role <- Access.allApplicationPredefinedSortRoles, direction <- Access.allEnvironmentEdgeRoles])
  assertEqual "hub and supporting edges retain their verified own sorts" objectSorts (Access.accessObjectSorts sourceAccess)

caseNameGrant :: IO ()
caseNameGrant = do
  let childObject = globalObjectIdFromProcessEpochId child
      grant = checked (Controlled.checkControlledGrant child childObject withChild)
      installed = Controlled.commitControlledGrants (checked (Controlled.prepareControlledGrants parent [grant] withChild))
      (aliases, privateChild) = Identity.commitLocalization (checked (Identity.prepareLocalization parent (globalUniqueIdFromGlobalObjectId childObject) (Application.applicationPrivateIdentity (startupApplicationState base))))
      claim = checked (Access.primordialSelection [("child", Access.Process (Private.asPrivateProcessId privateChild))] Set.empty Nothing)
  case Primordial.checkPrimordialSelection parent claim aliases withChild (startupStructuralProgressState base) of
    Left (Primordial.PrimordialGrantPossession (Controlled.ControlledGrantSourceNotPossessed owner object)) -> assertEqual "alias cannot stand in for capability" (parent, childObject) (owner, object)
    other -> assertFailure ("alias-only process claim did not reject: " <> show other)
  assertBool "the child's alias alone is not a parent capability" (not (Controlled.controlledHasNormalPossession parent childObject withChild))
  assertEqual
    "a source without possession cannot grant a name"
    (Left (Controlled.ControlledGrantSourceNotPossessed parent childObject))
    (Controlled.checkControlledGrant parent childObject withChild)
  assertBool "an admitted direct process-name grant supplies ordinary possession" (Controlled.controlledHasNormalPossession parent childObject installed)
  case Primordial.checkPrimordialSelection parent claim aliases installed (startupStructuralProgressState base) of
    Right _ -> pure ()
    Left problem -> assertFailure ("normal live process name was rejected: " <> show problem)
  let ended = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement child installed)
  assertBool "child End revokes the parent's live process-name possession" (not (Controlled.controlledHasNormalPossession parent childObject ended))
  assertBool "child End removes the direct process-name grant" (not (Controlled.controlledHasDirectPossession parent childObject ended))
  case Primordial.checkPrimordialSelection parent claim aliases ended (startupStructuralProgressState base) of
    Left (Primordial.PrimordialGrantPossession (Controlled.ControlledGrantObjectNotCurrent object)) -> assertEqual "ended process no longer grants a live process handle" childObject object
    other -> assertFailure ("ended process name claim was admitted: " <> show other)

sourceSelection :: Maybe (Text.Text, Text.Text) -> Access.PrimordialSelection
sourceSelection sources = checked (Access.primordialSelection [("n", writer Access.NablaRole), ("d", writer Access.DeltaRole)] Set.empty sources)
  where
    writer role = maybe (error "missing source") id (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries sourceAccess))

newEnvironmentWith :: ProcessEpochId -> Access.PrimordialSelection -> Controlled.State -> Either NewEnvironment.NewEnvironmentFailure (HeraldState, NewEnvironment.NewEnvironmentOutcome)
newEnvironmentWith process selection controlled =
  NewEnvironment.planNewEnvironment session binding (requestId 99) Application.newEnvironmentCoordinatorInput ready
  where
    grant = admit selection
    attachment = Session.applicationAttachmentForProcess process
    (application, _) = Application.commitApplicationBootstrap (checked (Application.prepareApplicationPrimordialBootstrap attachment process grant Application.emptyState))
    (opened, accepted) = Application.commitApplicationSessionAcceptance (checked (Application.prepareApplicationSessionOpen (checkedLocalHeraldEpoch fixtureCheckedGenesis) attachment (Session.clientNonce 80) application))
    binding = Session.sessionAcceptanceBinding accepted
    session = case Session.sessionAcceptanceReply accepted of
      Session.SessionOpened value _ _ -> value
      _ -> error "session was not opened"
    ready = replaceStartupApplicationState opened (replaceStartupControlledState controlled base)

casePartialNewEnvironment :: IO ()
casePartialNewEnvironment = case newEnvironmentWith parent (sourceSelection (Just ("n", "d"))) (startupControlledState base) of
  Right (successor, NewEnvironment.NewEnvironmentAccepted manifest) -> do
    assertEqual "construction retains the first twelve endpoint descriptions" 12 (length (Environment.environmentManifestRoots manifest))
    assertEqual "all thirty-one object identities are reserved at acceptance" 31 (length (Environment.environmentManifestGeneratedIds manifest))
    mapM_ privateRoute (NonEmpty.toList (Environment.environmentManifestRoots manifest))
    let retained = maybe (error "lost selected access") Application.bootstrapAccessPrimordial (Application.applicationBootstrapAccess parent (startupApplicationState successor))
    assertEqual "newenv keeps the fixed selected source keys" (Just ("n", "d")) (Access.accessEnvironmentSources retained)
    assertEqual "newenv does not append generated roots to primordial access" (Set.fromList ["n", "d"]) (Map.keysSet (Access.accessEntries retained))
  outcome -> assertFailure ("partial source pair did not accept newenv: " <> summarize outcome)
  where
    privateRoute root = do
      let plan = Environment.positionedEnvironmentRootPlan root
          sort = Publication.checkedPublicationSort (Environment.positionedEnvironmentRootChecked root)
          role = case [candidate | candidate <- SortProfile.allPredefinedSortRoles, SortProfile.profileSortFor candidate == sort] of
            [candidate] -> candidate
            _ -> error "environment endpoint description has no predefined sort"
          system = checkedSystemId (startupGenesis base)
          membership = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView (startupOracleProjectionState base))
          required = Set.fromList [deriveSystemViewDeltaId system herald role | herald <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)]
          actual = Set.fromList (fmap destinationDelta (routeDestinations (Environment.environmentRootPlanRoute plan)))
          ordinary = checked (Route.freezeWriterRoute (Environment.environmentRootPlanSourceNabla plan) sort (startupGraphState base) (startupPlacementState base))
          applicationReaders = Set.fromList (fmap destinationDelta (routeDestinations ordinary)) `Set.difference` required
      assertBool "the parent source has an ordinary reader that must not observe construction" (not (Set.null applicationReaders))
      assertEqual "private construction targets only mandatory Herald system views" required actual
      assertBool "no parent application store receives a private endpoint description" (Set.disjoint applicationReaders actual)

caseNoSources :: IO ()
caseNoSources = case newEnvironmentWith parent (sourceSelection Nothing) (startupControlledState base) of
  Left (NewEnvironment.NewEnvironmentRejected EnvironmentSourcesUnavailable) -> pure ()
  outcome -> assertFailure ("missing source pair did not reject: " <> summarize outcome)

casePassiveSources :: IO ()
casePassiveSources = do
  let selection = sourceSelection (Just ("n", "d"))
      grants = Primordial.primordialGrantPossessions (admit selection)
      installed = Controlled.commitControlledGrants (checked (Controlled.prepareControlledGrants child grants withChild))
  assertBool "the child possesses both writers" (all (\grant -> Controlled.controlledHasNormalPossession child (Controlled.controlledGrantObject grant) installed) grants)
  case newEnvironmentWith child selection installed of
    Left (NewEnvironment.NewEnvironmentRejected ApplicationOperateNotPermitted) -> pure ()
    outcome -> assertFailure ("passive selection unexpectedly operated: " <> summarize outcome)

caseSourceSorts :: IO ()
caseSourceSorts = do
  let selection = sourceSelection (Just ("d", "n"))
  case Primordial.checkPrimordialSelection parent selection (Application.applicationPrivateIdentity (startupApplicationState base)) (startupControlledState base) (startupStructuralProgressState base) of
    Left (Primordial.PrimordialGrantEnvironmentSourceSortMismatch "d") -> pure ()
    outcome -> assertFailure ("swapped source sorts admitted: " <> show outcome)

summarize :: Either NewEnvironment.NewEnvironmentFailure (HeraldState, NewEnvironment.NewEnvironmentOutcome) -> String
summarize = either show (show . snd)
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

propRemovedSource :: Bool -> Property
propRemovedSource removeNabla = counterexample "a removed selected writer acquired substitute authority"
  $ case newEnvironmentWith parent selection removed of
    Left (NewEnvironment.NewEnvironmentRejected ApplicationOperateNotPermitted) -> True
    _ -> False
  where
    selection = sourceSelection (Just ("n", "d"))
    grant = admit selection
    source = maybe (error "source missing") id (Map.lookup (if removeNabla then "n" else "d") (Primordial.primordialGrantEntries grant))
    object = globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity source)
    cause = checked (predefinedDisappearanceCause (checked (deriveDisappearanceProbeId (controlIndex 1))) (controlIndex 2))
    removed = Controlled.commitControlledRemoval (checked (Controlled.prepareControlledRemoval cause object (startupControlledState base)))

propUnknownAlias :: NonNegative Int -> Bool
propUnknownAlias (NonNegative ordinal) =
  Primordial.checkPrimordialSelection parent selection (Application.applicationPrivateIdentity (startupApplicationState base)) (startupControlledState base) (startupStructuralProgressState base)
    == Left (Primordial.PrimordialGrantUnknownAlias unknown)
  where
    unknown = checked (Private.mkPrivateUniqueId (100000 + fromIntegral (ordinal `mod` 100000)))
    selection = checked (Access.primordialSelection [("bare", Access.Identity unknown)] Set.empty Nothing)

caseForgedRole :: IO ()
caseForgedRole = do
  let reader = maybe (error "missing reader") id (Map.lookup (Access.environmentReaderKey Access.NablaRole) (Access.accessEntries sourceAccess))
      identity = Access.primordialEntryIdentity reader
      forged = checked (Access.primordialSelection [("writer", Access.Writer (Private.asPrivateNablaId identity))] Set.empty Nothing)
  assertEqual
    "typed syntax cannot change an admitted current endpoint role"
    (Left (Primordial.PrimordialGrantRoleMismatch identity))
    (Primordial.checkPrimordialSelection parent forged (Application.applicationPrivateIdentity (startupApplicationState base)) (startupControlledState base) (startupStructuralProgressState base))
