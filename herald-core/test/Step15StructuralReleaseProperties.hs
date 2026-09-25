{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step15StructuralReleaseProperties
  ( tests,
  )
where

import ApplicationLabelProperties qualified
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    PublicationId,
    controlIndex,
    controlIndexWord64,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipHistoryGenesis,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Topology
  ( TopologyOccurrenceDigest,
    deriveTopologyCutId,
    mkTopologyOccurrenceDigest,
    topologyCut,
    topologyFrontier,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch (effectBatchIsEmpty)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAnnounceId,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( SuccessorStructuralBase,
    deriveTerminalSourceUnion,
    emptyTerminalSourcePayloadArchive,
    establishedSuccessorStructuralBase,
    successorStructuralBaseCutId,
    successorStructuralBaseEstablishedUnion,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInstalledPredecessorBase,
    terminalSourceInventory,
    terminalSourceUnionAcceptance,
    terminalSourceUnionEstablished,
    terminalSourceUnionReady,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTopologyPredecessor,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    StructuralOccurrenceStamp,
    peerPublicationBatch,
    peerPublicationStructuralStamp,
    publicationBatchId,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
  )
import Eclips.Herald.PeerStream
  ( sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamPrefixThrough,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupLabelBarrierState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    replaceStartupWaitState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupLabelBarrierState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import GenesisFixtures qualified as MembershipFixtures
import HeraldTransitionProperties
  ( SuccessorStructuralHoldFixture (..),
    successorStructuralHeldFixture,
  )
import OracleAdvanceProperties (heldControlIndexTwoForMembershipAdvance)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Step-15 structural-base release"
    [ testCase
        "peer survivor work crosses after a later same-generation cut with its old stamp intact"
        casePeerCarriedRelease,
      testCase
        "pre-contraction survivor work applies before the base and is not repeated after it"
        casePreContractionPeerCarriedRelease,
      testCase
        "local application stages release after a later same-generation cut"
        caseLocalStageRelease,
      testCase
        "the successor-base gate accepts immutable predecessor application admission"
        casePredecessorAdmittedApplicationAdmission
    ]

casePeerCarriedRelease :: Assertion
casePeerCarriedRelease = do
  fixture <- successorStructuralHeldFixture
  let held = successorStructuralHeldState fixture
      successorMembership = successorStructuralHeldMembership fixture
      item = successorStructuralHeldItem fixture
      peerPublication = sequencedItemPayload item
      identifier = publicationBatchId (peerPublicationBatch peerPublication)
      dependency =
        Publication.SuccessorStructuralBaseDependency
          (heraldMembershipGenerationId successorMembership)
      heldContext = peerContext held
      heldOwners = peerOwners held
  (_, heldResult) <-
    checked
      "ordinary dependency redrive before base"
      (PeerInput.releasePeerDependency heldContext dependency heldOwners)
  assertEqual
    "the generic dependency seam cannot infer terminal lineage"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition heldResult)
  retainedStamp <- requireStructuralStamp peerPublication
  installed <- installEmptySuccessorBase held successorMembership
  afterLaterCut <- installLaterSameGenerationCut installed.state
  assertBool
    "the membership-successor base remains historical rather than current"
    ( GraphProgress.structuralLastInstalledCutId
        (startupStructuralProgressState afterLaterCut)
        /= successorStructuralBaseCutId installed.base
    )
  let installedOwners =
        replacePeerStructuralProgress
          (startupStructuralProgressState afterLaterCut)
          heldOwners
  (releasedOwners, releasedResult) <-
    checked
      "release exact successor base"
      ( PeerInput.releasePeerSuccessorStructuralBase
          installed.base
          (peerContext afterLaterCut)
          installedOwners
      )
  assertEqual
    "the proof-aware seam releases the held publication"
    PeerInput.PeerDependencyReleased
    (PeerInput.peerDependencyReleaseDisposition releasedResult)
  assertEqual
    "the released publication is reported once"
    [identifier]
    (PeerInput.peerDependencyReleasedPublications releasedResult)
  let released = installPeerOwners releasedOwners afterLaterCut
  record <- requireIncoming identifier released
  assertEqual
    "the peer publication applies after base establishment"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  retained <-
    checked
      "read released peer stream"
      ( PeerStream.incomingRetainedItems
          (sequencedItemDirection item)
          (startupPeerStreamState released)
      )
  assertEqual
    "proof-aware application releases its completed payload receipt"
    []
    (fmap snd retained)
  assertBool
    "compact progress remembers proof-aware semantic completion"
    (PeerStream.incomingSequenceCompleted (sequencedItemDirection item, sequencedItemSequence item) (startupPeerStreamState released))
  watermarks <- checked "read proof-aware completion barrier" (PeerStream.incomingWatermarks (sequencedItemDirection item) (startupPeerStreamState released))
  assertEqual
    "proof-aware semantic completion advances the exact stream barrier"
    (streamPrefixThrough (sequencedItemSequence item))
    (streamCompletedPrefix watermarks)
  let occurrence = structuralOccurrenceStampOccurrence retainedStamp
  applied <-
    maybe
      (assertFailure "released survivor occurrence missing from Graph progress")
      pure
      ( GraphProgress.lookupAppliedStructuralOccurrence
          occurrence
          (startupStructuralProgressState released)
      )
  assertEqual
    "terminal carry never rewrites the predecessor-generation stamp"
    retainedStamp
    (GraphProgress.appliedStructuralOccurrenceStamp applied)
  assertEqual
    "the retained stamp still names the predecessor generation"
    ( structuralVersionVectorMembershipGenerationId
        (structuralOccurrenceStampPredecessor retainedStamp)
    )
    ( maybe
        (error "successor fixture has no predecessor")
        id
        (heraldMembershipGenerationPredecessor successorMembership)
    )
  assertEqual
    "the applied owner advances in successor coordinates"
    (heraldMembershipGenerationId successorMembership)
    ( structuralVersionVectorMembershipGenerationId
        (GraphProgress.structuralAppliedVector (startupStructuralProgressState released))
    )
  assertEqual "released whole state remains valid" (Right ()) (validateHeraldState released)

casePreContractionPeerCarriedRelease :: Assertion
casePreContractionPeerCarriedRelease = do
  (held, identifier, _, retired) <- heldControlIndexTwoForMembershipAdvance
  let predecessorMembership = currentMembership held
      predecessorId = heraldMembershipGenerationId predecessorMembership
      retirementIndex = controlIndex 2
  original <- requireIncoming identifier held
  retainedStamp <-
    requireStructuralStamp (Publication.incomingPeerPublication original)
  assertEqual
    "the held record was admitted before contraction"
    predecessorId
    (Publication.incomingPublicationMembershipGenerationId original)
  assertEqual
    "the legitimate pre-contraction hold names the pending retirement index"
    (Set.singleton (Publication.ControlIndexDependency retirementIndex))
    (Publication.incomingPublicationDependencies original)
  let successorMembership =
        checkedValue
          "pre-contraction survivor successor membership"
          (retireHeraldMembershipGeneration retirementIndex (MembershipFixtures.fixtureRetirementResolution retirementIndex) retired predecessorMembership)
      successorId = heraldMembershipGenerationId successorMembership
  preparedAdvance <-
    checked
      "advance pre-contraction survivor membership"
      ( MembershipAdvance.prepareMembershipAdvance
          retirementIndex
          successorMembership
          []
          held
      )
  let contracted = MembershipAdvance.commitMembershipAdvance preparedAdvance
  contractedRecord <- requireIncoming identifier contracted
  assertEqual
    "old stamped work applies in its still-installed Graph generation"
    Set.empty
    (Publication.incomingPublicationDependencies contractedRecord)
  assertEqual
    "dependency redrive preserves predecessor admission authority"
    predecessorId
    (Publication.incomingPublicationMembershipGenerationId contractedRecord)
  assertEqual
    "the contracted predecessor-admitted state remains valid"
    (Right ())
    (validateHeraldState contracted)
  installed <- installEmptySuccessorBase contracted successorMembership
  (releasedOwners, releasedResult) <-
    checked
      "release pre-contraction survivor through exact successor base"
      ( PeerInput.releasePeerSuccessorStructuralBase
          installed.base
          (peerContext installed.state)
          (peerOwners installed.state)
      )
  assertEqual
    "the exact base notification is admitted"
    PeerInput.PeerDependencyReleased
    (PeerInput.peerDependencyReleaseDisposition releasedResult)
  assertEqual
    "base installation does not duplicate the earlier release"
    []
    (PeerInput.peerDependencyReleasedPublications releasedResult)
  let released = installPeerOwners releasedOwners installed.state
  releasedRecord <- requireIncoming identifier released
  assertEqual
    "the earlier applied publication remains applied after base establishment"
    Publication.Applied
    (Publication.incomingPublicationDisposition releasedRecord)
  assertEqual
    "application does not rewrite the record's admission generation"
    predecessorId
    (Publication.incomingPublicationMembershipGenerationId releasedRecord)
  let occurrence = structuralOccurrenceStampOccurrence retainedStamp
  applied <-
    maybe
      (assertFailure "pre-contraction survivor occurrence missing from Graph progress")
      pure
      ( GraphProgress.lookupAppliedStructuralOccurrence
          occurrence
          (startupStructuralProgressState released)
      )
  assertEqual
    "terminal carry preserves the complete pre-contraction stamp"
    retainedStamp
    (GraphProgress.appliedStructuralOccurrenceStamp applied)
  assertEqual
    "the carried application advances only the live cursor to successor coordinates"
    successorId
    ( structuralVersionVectorMembershipGenerationId
        (GraphProgress.structuralAppliedVector (startupStructuralProgressState released))
    )
  assertEqual
    "released pre-contraction survivor whole state remains valid"
    (Right ())
    (validateHeraldState released)

caseLocalStageRelease :: Assertion
caseLocalStageRelease = do
  positioned <- ApplicationLabelProperties.predecessorAdmittedApplicationPublicationFixture
  let predecessorMembership = currentMembership positioned
      local = checkedLocalHeraldEpoch (startupGenesis positioned)
  retired <-
    case filter (\herald -> herald /= local && not (hostsLiveProcess herald positioned)) (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs predecessorMembership)) of
      herald : _ -> pure herald
      [] -> assertFailure "application-stage fixture needs an unoccupied remote Herald to retire"
  let retirementIndex =
        controlIndex
          ( controlIndexWord64
              ( OracleProjection.oracleViewControlIndex
                  (OracleProjection.oracleView (startupOracleProjectionState positioned))
              )
              + 1
          )
      successorMembership =
        checkedValue
          "application-stage successor membership"
          (retireHeraldMembershipGeneration retirementIndex (MembershipFixtures.fixtureRetirementResolution retirementIndex) retired predecessorMembership)
      successorId = heraldMembershipGenerationId successorMembership
  preparedAdvance <-
    checked
      "advance application-stage membership"
      ( MembershipAdvance.prepareMembershipAdvance
          retirementIndex
          successorMembership
          []
          positioned
      )
  let contracted = MembershipAdvance.commitMembershipAdvance preparedAdvance
      unstampedBefore =
        Publication.unstampedStructuralSourceStageEntries
          (startupPublicationState contracted)
  assertBool "the fixture retains local unsequenced stages" (not (null unstampedBefore))
  (stillHeld, heldEffects) <-
    checked
      "scan local stages before successor base"
      (StructuralProgress.advanceReadyApplicationStructuralStages contracted)
  assertBool
    "an ordinary scan cannot sequence post-contraction stages"
    (startupPublicationState contracted == startupPublicationState stillHeld)
  assertBool "a held scan emits no effects" (effectBatchIsEmpty heldEffects)
  installed <- installEmptySuccessorBase stillHeld successorMembership
  afterLaterCut <- installLaterSameGenerationCut installed.state
  assertBool
    "the membership-successor base remains historical rather than current"
    ( GraphProgress.structuralLastInstalledCutId
        (startupStructuralProgressState afterLaterCut)
        /= successorStructuralBaseCutId installed.base
    )
  (released, _) <-
    checked
      "release local stages after a later same-generation cut"
      ( StructuralProgress.advanceReadyApplicationStructuralStagesAfterSuccessorBase
          installed.base
          afterLaterCut
      )
  let publication = startupPublicationState released
      stamped = Publication.stampedStructuralStageEntries publication
      survivorRemotes =
        Set.delete
          local
          ( Set.fromList
              (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successorMembership))
          )
  assertEqual
    "every held source stage is consumed"
    []
    (Publication.unstampedStructuralSourceStageEntries publication)
  assertBool
    "the released stages become real retained stamped stages"
    (length stamped >= length unstampedBefore)
  assertBool
    "every newly allocated predecessor is successor-qualified"
    ( all
        ( (== successorId)
            . structuralVersionVectorMembershipGenerationId
            . structuralOccurrenceStampPredecessor
            . Publication.stampedStructuralStamp
            . snd
        )
        stamped
    )
  assertBool
    "remote batches preserve every survivor and drop only the retired destination"
    ( all
        ( (== survivorRemotes)
            . Map.keysSet
            . Publication.stampedStructuralPeerPublications
            . snd
        )
        stamped
    )
  assertEqual
    "local applied progress remains in successor coordinates"
    successorId
    ( structuralVersionVectorMembershipGenerationId
        (GraphProgress.structuralAppliedVector (startupStructuralProgressState released))
    )
  assertEqual "released local whole state remains valid" (Right ()) (validateHeraldState released)

casePredecessorAdmittedApplicationAdmission :: Assertion
casePredecessorAdmittedApplicationAdmission = do
  admitted <- ApplicationLabelProperties.predecessorAdmittedApplicationPublicationFixture
  let predecessorMembership = currentMembership admitted
      predecessorId = heraldMembershipGenerationId predecessorMembership
      local = checkedLocalHeraldEpoch (startupGenesis admitted)
  application <- case Publication.applicationPublicationEntries (startupPublicationState admitted) of
    [(_, one)] -> pure one
    applications ->
      assertFailure
        ("expected one predecessor-admitted application publication, got " <> show (length applications))
  assertEqual
    "application admission retains the predecessor generation"
    predecessorId
    (Publication.applicationPublicationMembershipGenerationId application)
  retired <-
    case filter
      (\herald -> herald /= local && not (hostsLiveProcess herald admitted))
      (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs predecessorMembership)) of
      herald : _ -> pure herald
      [] -> assertFailure "application-stage fixture needs an unoccupied remote Herald to retire"
  let retirementIndex = nextControlIndex admitted
      successorMembership =
        checkedValue
          "application-stage successor membership"
          (retireHeraldMembershipGeneration retirementIndex (MembershipFixtures.fixtureRetirementResolution retirementIndex) retired predecessorMembership)
  preparedAdvance <-
    checked
      "advance application-stage membership"
      ( MembershipAdvance.prepareMembershipAdvance
          retirementIndex
          successorMembership
          []
          admitted
      )
  let contracted = MembershipAdvance.commitMembershipAdvance preparedAdvance
  installed <- installEmptySuccessorBase contracted successorMembership
  assertBool
    "the exact successor base admits the retained predecessor authority"
    ( StructuralProgress.installedLineageAdmitsApplicationStage
        (startupStructuralProgressState installed.state)
        (heraldMembershipGenerationId successorMembership)
        application
    )
  assertEqual
    "base installation does not rewrite admission-time membership authority"
    predecessorId
    (Publication.applicationPublicationMembershipGenerationId application)
  assertEqual
    "installed predecessor-admitted whole state remains valid"
    (Right ())
    (validateHeraldState installed.state)
  secondRetired <- case filter
    (/= local)
    (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successorMembership)) of
    herald : _ -> pure herald
    [] -> assertFailure "application-stage fixture needs a second nonlocal survivor"
  let secondIndex = nextControlIndex installed.state
      finalMembership = checkedValue "second application-stage retirement" (retireHeraldMembershipGeneration secondIndex (MembershipFixtures.fixtureRetirementResolution secondIndex) secondRetired successorMembership)
      finalId = heraldMembershipGenerationId finalMembership
  let secondProjection = startupOracleProjectionState installed.state
      secondView = OracleProjection.oracleView secondProjection
      residentEnds = [OracleProjection.membershipAdvanceProcessEnd process secondIndex MembershipFixtures.fixtureHeraldRetirementReason | process <- OracleProjection.projectedProcessEpochs secondProjection, OracleProjection.oracleViewProcessResidence process secondView == Just secondRetired, OracleProjection.oracleViewProcessIsLive process secondView]
  secondAdvance <- checked "advance past the installed first base" (MembershipAdvance.prepareMembershipAdvance secondIndex finalMembership residentEnds installed.state)
  let awaitingBase = MembershipAdvance.commitMembershipAdvance secondAdvance
  (waiting, _) <- checked "ordinary work waits while its installed closure belongs to G1" (StructuralCoordinator.advanceLocalStructuralWork awaitingBase)
  assertEqual "ordinary work preserves the actually installed G1 Graph coordinate" (heraldMembershipGenerationId successorMembership) (GraphProgress.structuralProgressMembershipGenerationId (startupStructuralProgressState waiting))
  nextClosure <- checked "the next retirement extends from the retained installed closure" (StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance [] installed.state waiting)
  assertEqual "next closure targets the newly ordered membership" finalMembership (StructuralBase.structuralBaseSuccessorMembership nextClosure)
  assertBool
    "membership ancestry alone cannot admit the old stage while the second base is missing"
    (not (StructuralProgress.installedLineageAdmitsApplicationStage (startupStructuralProgressState awaitingBase) finalId application))
  final <- installEmptySuccessorBase awaitingBase finalMembership
  assertEqual "both established membership bases remain installed" 2 (length (GraphProgress.structuralInstalledSuccessorBases (startupStructuralProgressState final.state)))
  assertBool
    "G0 application admission remains usable after actual G1 and G2 base installation"
    (StructuralProgress.installedLineageAdmitsApplicationStage (startupStructuralProgressState final.state) finalId application)
  assertEqual "the second base still preserves the original application admission" predecessorId (Publication.applicationPublicationMembershipGenerationId application)
  assertEqual "two-retirement staged state remains valid" (Right ()) (validateHeraldState final.state)

data InstalledBaseFixture = InstalledBaseFixture
  { base :: SuccessorStructuralBase,
    state :: HeraldState
  }

installEmptySuccessorBase ::
  HeraldState ->
  HeraldMembershipGeneration ->
  IO InstalledBaseFixture
installEmptySuccessorBase predecessor successorMembership = do
  let view = OracleProjection.oracleView (startupOracleProjectionState predecessor)
      progress = startupStructuralProgressState predecessor
  predecessorId <-
    maybe
      (assertFailure "successor membership has no predecessor")
      pure
      (heraldMembershipGenerationPredecessor successorMembership)
  predecessorMembership <-
    maybe
      (assertFailure "Oracle history lost predecessor membership")
      pure
      (OracleProjection.oracleViewHeraldMembershipById predecessorId view)
  retired <-
    maybe
      (assertFailure "successor membership has no retired Herald")
      pure
      (heraldMembershipGenerationRetiredHeraldEpoch successorMembership)
  let targetId = heraldMembershipGenerationId successorMembership
      lineage = checkedValue "current retirement lineage" $ maybe (Left ("missing lineage" :: String)) Right (OracleProjection.oracleViewHeraldMembershipLineage predecessorId targetId view)
      genesisId = heraldMembershipGenerationId (heraldMembershipHistoryGenesis (OracleProjection.oracleViewHeraldMembershipHistoryChecked view))
      fullLineage = checkedValue "full retirement history" $ maybe (Left ("missing history" :: String)) Right (OracleProjection.oracleViewHeraldMembershipLineage genesisId targetId view)
      reporters = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successorMembership)
      anchorId = GraphProgress.structuralLastInstalledCutId progress
      predecessorBase =
        if anchorId == GraphProgress.structuralGenesisCutId progress
          then checkedValue "genesis predecessor base" (terminalSourceGenesisPredecessorBase predecessorMembership anchorId)
          else case GraphProgress.lookupInstalledTopologyCut anchorId progress of
            Nothing -> error "fixture lost installed anchor"
            Just installed -> checkedValue "installed predecessor base" (terminalSourceInstalledPredecessorBase predecessorMembership (GraphProgress.installedTopologyCutCut installed))
      chain = GraphProgress.structuralInstalledEstablishedChain progress
      bases = fmap successorStructuralBaseEstablishedUnion (GraphProgress.structuralInstalledSuccessorBases progress)
      inventories = [checkedValue "empty terminal-source inventory" (terminalSourceInventory lineage retired reporter chain bases []) | reporter <- reporters]
      terminalDigest =
        checkedValue
          "terminal topology digest"
          (mkTopologyOccurrenceDigest (identifierBytes 0x6d))
      union =
        checkedValue
          "empty terminal-source union"
          ( deriveTerminalSourceUnion
              fullLineage
              lineage
              predecessorBase
              inventories
              emptyTerminalSourcePayloadArchive
              (constantDigest terminalDigest)
          )
      ready =
        checkedValue
          "terminal-source readiness"
          ( terminalSourceUnionReady
              (GraphProgress.structuralAppliedVector progress)
              (GraphProgress.structuralAppliedControlPrefix progress)
              union
          )
      acceptances =
        [ checkedValue
            "terminal-source acceptance"
            (terminalSourceUnionAcceptance successorMembership reporter ready)
        | reporter <- reporters
        ]
      established =
        checkedValue
          "terminal-source establishment"
          (terminalSourceUnionEstablished successorMembership union acceptances)
      topologyPredecessor =
        checkedValue
          "membership-successor topology predecessor"
          ( terminalSourceUnionTopologyPredecessor
              lineage
              union
          )
      successorCut =
        checkedValue
          "membership-successor topology cut"
          ( topologyCut
              topologyPredecessor
              ( topologyFrontier
                  (terminalSourceUnionSuccessorInitialVector union)
                  (terminalSourceUnionTerminalControlPrefix union)
              )
              terminalDigest
          )
      base =
        checkedValue
          "successor structural base"
          ( establishedSuccessorStructuralBase
              lineage
              established
              successorCut
          )
  prepared <-
    checked
      "install successor structural base"
      ( GraphProgress.prepareMembershipSuccessorBaseInstallation
          successorMembership
          base
          successorCut
          progress
      )
  initialClosure <- checked "begin retained fixture closure" (StructuralBase.beginMembershipBaseClosure fullLineage lineage (checkedLocalHeraldEpoch (startupGenesis predecessor)) predecessorBase chain bases [])
  withInventories <- foldM (flip (\inventory closure -> checked "retain exact fixture inventory" (StructuralBase.receiveStructuralBaseInventory inventory closure))) initialClosure inventories
  withEstablished <- checked "retain exact fixture Established union" (StructuralBase.receiveStructuralBaseUnionEstablished (\vector control -> Right (constantDigest terminalDigest vector control)) (GraphProgress.structuralAppliedVector progress) (GraphProgress.structuralAppliedControlPrefix progress) established withInventories)
  installedClosure <- checked "record exact installed fixture closure" (StructuralBase.recordInstalledSuccessorStructuralCut successorCut withEstablished)
  assertEqual
    "the opaque base and installed cut agree"
    (deriveTopologyCutId successorCut)
    (successorStructuralBaseCutId base)
  pure
    InstalledBaseFixture
      { base,
        state =
          replaceStartupStructuralBaseCoordinator
            (Just installedClosure)
            (replaceStartupStructuralProgressState (GraphProgress.commitMembershipSuccessorBaseInstallation prepared) predecessor)
      }

-- Advance the Graph owner beyond the first membership-successor cut while
-- retaining that cut and its exact base in installed history.  The unrelated
-- control step stands in for ordinary post-contraction progress which may race
-- with a late peer publication or a still-unsequenced local stage.
installLaterSameGenerationCut :: HeraldState -> IO HeraldState
installLaterSameGenerationCut predecessor = do
  let initial = startupStructuralProgressState predecessor
      laterControl =
        controlIndex
          ( controlIndexWord64 (GraphProgress.structuralAppliedControlPrefix initial)
              + 1
          )
      membership = currentMembership predecessor
      local = checkedLocalHeraldEpoch (startupGenesis predecessor)
      remotes =
        filter
          (/= local)
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
  preparedControl <-
    checked
      "advance Graph control before later same-generation cut"
      (GraphProgress.prepareStructuralControlProgress laterControl initial)
  let (withControl, _) = GraphProgress.commitStructuralControlProgress preparedControl
      vector = GraphProgress.structuralAppliedVector withControl
      report reporter = structuralAppliedReport reporter vector laterControl
      (affectsTopology, checkpointBytes) =
        Reconciliation.structuralControlCheckpointAt
          laterControl
          (GraphProgress.structuralProgressReconciliation withControl)
  withReports <-
    foldM
      ( \state reporter -> do
          prepared <-
            checked
              "record later-cut structural report"
              (GraphProgress.prepareStructuralReport (report reporter) state)
          pure (fst (GraphProgress.commitStructuralReport prepared))
      )
      withControl
      remotes
  proposal <-
    checked
      "prepare later same-generation cut"
      ( GraphProgress.prepareTopologyCutProposal
          (reconciliationViewsFor predecessor)
          ( GraphProgress.topologyControlCheckpointWithConsequence
              laterControl
              affectsTopology
              checkpointBytes
          )
          withReports
      )
  preparedProposal <- case proposal of
    GraphProgress.TopologyCutProposalReady prepared -> pure prepared
    GraphProgress.TopologyCutProposalUnavailable ->
      assertFailure "later same-generation cut was unexpectedly unavailable"
    GraphProgress.TopologyCutProposalHeld dependencies ->
      assertFailure ("later same-generation cut held: " <> show dependencies)
  let announce = GraphProgress.preparedTopologyCutProposalAnnounce preparedProposal
      cutId = topologyCutAnnounceId announce
      proposed = GraphProgress.commitTopologyCutProposal preparedProposal
  withAcceptances <-
    foldM
      ( \state reporter -> do
          prepared <-
            checked
              "record later-cut acceptance"
              ( GraphProgress.prepareTopologyCutAcceptance
                  (topologyCutAcceptance cutId (report reporter))
                  state
              )
          pure (GraphProgress.commitTopologyCutAcceptance prepared)
      )
      proposed
      remotes
  establishment <-
    checked
      "establish later same-generation cut"
      (GraphProgress.prepareTopologyCutEstablishment withAcceptances)
  let installed = GraphProgress.commitTopologyCutEstablishment establishment
  assertEqual
    "later same-generation Graph state remains valid"
    (Right ())
    (GraphProgress.validateStructuralProgressState installed)
  pure (replaceStartupStructuralProgressState installed predecessor)

reconciliationViewsFor :: HeraldState -> Reconciliation.ReconciliationViews
reconciliationViewsFor state =
  Reconciliation.reconciliationViews
    (checkedLocalHeraldEpoch (startupGenesis state))
    ( Map.fromList
        [ ( SortRegistry.registryEntrySortId entry,
            sortOccurrence
              (SortRegistry.registryEntrySortId entry)
              (SortRegistry.registryEntryOccurrenceId entry)
          )
        | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
        ]
    )
    (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
    ( Map.fromList
        [ (process, residence)
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleViewProcessIsLive process view,
          Just residence <- [OracleProjection.oracleViewProcessResidence process view]
        ]
    )
    ( Set.fromList
        (Controlled.controlledNormalPossessionEntries (startupControlledState state))
    )
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection

peerContext :: HeraldState -> PeerInput.PeerInputContext
peerContext state =
  PeerInput.peerInputContext
    (startupGenesis state)
    (startupOracleProjectionState state)
    (startupDiscoveryState state)
    (startupPlacementState state)

peerOwners :: HeraldState -> PeerInput.PeerInputState
peerOwners state =
  PeerInput.peerInputState
    (startupPublicationState state)
    (startupPeerStreamState state)
    (startupStoreState state)
    (startupGraphState state)
    (startupIdGeneratorState state)
    (startupStructuralProgressState state)
    (startupAlignmentState state)
    (startupPlacementState state)
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupApplicationState state)
    (startupWaitState state)
    (startupLabelBarrierState state)
    (startupDisappearanceState state)

replacePeerStructuralProgress ::
  GraphProgress.StructuralProgressState ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerStructuralProgress progress state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    progress
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    (PeerInput.peerInputControlledState state)
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

installPeerOwners :: PeerInput.PeerInputState -> HeraldState -> HeraldState
installPeerOwners owners =
  replaceStartupLabelBarrierState (PeerInput.peerInputLabelBarrierState owners)
    . replaceStartupWaitState (PeerInput.peerInputWaitState owners)
    . replaceStartupApplicationState (PeerInput.peerInputApplicationState owners)
    . replaceStartupSortRegistryState (PeerInput.peerInputSortRegistryState owners)
    . replaceStartupControlledState (PeerInput.peerInputControlledState owners)
    . replaceStartupPlacementState (PeerInput.peerInputPlacementState owners)
    . replaceStartupAlignmentState (PeerInput.peerInputAlignmentState owners)
    . replaceStartupStructuralProgressState (PeerInput.peerInputStructuralProgressState owners)
    . replaceStartupIdGeneratorState (PeerInput.peerInputIdGeneratorState owners)
    . replaceStartupGraphState (PeerInput.peerInputGraphState owners)
    . replaceStartupStoreState (PeerInput.peerInputStoreState owners)
    . replaceStartupPeerStreamState (PeerInput.peerInputPeerStreamState owners)
    . replaceStartupPublicationState (PeerInput.peerInputPublicationState owners)

requireStructuralStamp :: PeerPublication -> IO StructuralOccurrenceStamp
requireStructuralStamp peerPublication =
  maybe
    (assertFailure "fixture peer publication has no structural stamp")
    pure
    (peerPublicationStructuralStamp peerPublication)

requireIncoming :: PublicationId -> HeraldState -> IO Publication.IncomingPublicationRecord
requireIncoming identifier state =
  maybe
    (assertFailure "incoming publication missing")
    pure
    (Publication.lookupIncomingPublication identifier (startupPublicationState state))

currentMembership :: HeraldState -> HeraldMembershipGeneration
currentMembership =
  OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . startupOracleProjectionState

nextControlIndex :: HeraldState -> ControlIndex
nextControlIndex state =
  controlIndex
    ( controlIndexWord64
        ( OracleProjection.oracleViewControlIndex
            (OracleProjection.oracleView (startupOracleProjectionState state))
        )
        + 1
    )

hostsLiveProcess :: HeraldEpoch -> HeraldState -> Bool
hostsLiveProcess herald state =
  any residentAndLive (OracleProjection.projectedProcessEpochs projection)
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection
    index = OracleProjection.oracleViewControlIndex view
    residentAndLive process =
      OracleProjection.oracleProjectionProcessResidenceAt index process projection
        == Just herald
        && OracleProjection.oracleProjectionProcessIsLiveAt index process projection

constantDigest ::
  TopologyOccurrenceDigest ->
  StructuralVersionVector ->
  ControlIndex ->
  TopologyOccurrenceDigest
constantDigest digest _ _ = digest

identifierBytes :: Word -> ByteString
identifierBytes = ByteString.replicate 32 . fromIntegral

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
