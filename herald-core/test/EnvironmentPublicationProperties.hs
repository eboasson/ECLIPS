{-# LANGUAGE OverloadedStrings #-}

module EnvironmentPublicationProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (heraldPublicationPositionWord64)
import Eclips.Domain.Environment
  ( EnvironmentRootClaimView (..),
    EnvironmentRootSlot,
    environmentManifestShapeRoots,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    TopologyCutId,
    controlIndex,
    genesisAuthorityEpoch,
    mkDeltaId,
    mkGlobalUniqueId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    nablaSequenceWord64,
    publicationNablaSequence,
    sortIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (Normal, Weak),
    RouteDestination,
    destinationDelta,
    destinationHerald,
    destinationStoreIncarnation,
    destinationStrength,
    freezeRoute,
    routeDestination,
    routeDestinations,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, NablaCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Startup
  ( deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel),
    Value,
    bytesValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Request.Internal (processAcceptancePosition, processAcceptancePositionOrdinal)
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    StructuralOccurrenceStamp,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    publicationDestination,
    structuralPublicationDigestForSemantics,
  )
import Eclips.Herald.Publication.State qualified as Publication
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    counterexample,
    property,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "publication private-environment ranges"
    [ testCase
        "one atomic range owns twelve consecutive positions and two positive six-sequence tenures"
        caseCanonicalRange,
      testCase
        "an exact retained retry is state-identical while a second first acceptance conflicts"
        caseRetainedRetry,
      testCase
        "the third structural source arm retains every manifest-qualified root in position order"
        caseStructuralSourceArm,
      testCase
        "an environment source stamps exactly once through the shared structural owner"
        caseStructuralSourceStamp,
      testCase
        "a later environment continues both source tenures without publication reuse"
        caseLaterRangeContinuesTenures,
      testCase
        "a Weak configured reader is accepted in addition to the mandatory route"
        caseCurrentMembershipRoute,
      testCase
        "a route missing one current member rejects before any range can commit"
        caseRouteMembershipMismatch,
      testCase
        "weakening a mandatory system-view destination leaves it missing"
        caseMandatoryRouteWeakening,
      testProperty
        "a duplicate generated target at any later ordinal rejects the complete manifest"
        propDuplicateTargetRejects
    ]

caseCanonicalRange :: Assertion
caseCanonicalRange = do
  prepared <-
    checkedIO
      "prepare canonical environment range"
      ( prepareRootRange
          manifestKey
          fixtureHeraldMembershipGeneration
          rootPlans
          publicationBase
      )
  assertEqual
    "prepared classification"
    Publication.EnvironmentRootRangeFirstPositioned
    (Publication.preparedEnvironmentRootRangeClassification prepared)
  let (successor, classification, manifest) =
        Publication.commitEnvironmentRootRange prepared
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
      witness = Publication.publicationStateWitness successor
  assertEqual
    "committed classification"
    Publication.EnvironmentRootRangeFirstPositioned
    classification
  assertEqual "canonical root count" 12 (length roots)
  assertEqual
    "consecutive Herald positions"
    [1 .. 12]
    ( heraldPublicationPositionWord64
        . Environment.positionedEnvironmentRootHeraldPosition
        <$> roots
    )
  assertEqual "six positive writer-source sequences" [1 .. 6] (sourceSequences nablaSource roots)
  assertEqual "six positive reader-source sequences" [1 .. 6] (sourceSequences deltaSource roots)
  assertEqual
    "writer tenure successor"
    (Just (nablaSequence 7))
    (lookup (nablaSource, authority) (Publication.publicationWitnessNextSequences witness))
  assertEqual
    "reader tenure successor"
    (Just (nablaSequence 7))
    (lookup (deltaSource, authority) (Publication.publicationWitnessNextSequences witness))
  assertEqual
    "next Herald position"
    13
    ( heraldPublicationPositionWord64
        (Publication.publicationWitnessNextHeraldPosition witness)
    )
  assertEqual
    "frozen membership generation"
    (heraldMembershipGenerationId fixtureHeraldMembershipGeneration)
    (Environment.environmentManifestMembershipGenerationId manifest)
  assertEqual
    "frozen active-member digest"
    (heraldMembershipGenerationActiveMemberSetDigest fixtureHeraldMembershipGeneration)
    (Environment.environmentManifestActiveMemberSetDigest manifest)
  assertEqual
    "one distinct manifest index entry"
    [(manifestKey, manifest)]
    (Publication.environmentManifestEntries successor)
  assertEqual
    "manifest witness"
    [manifest]
    (Publication.publicationWitnessEnvironmentManifests witness)
  assertEqual
    "twelve distinct root-index entries"
    12
    (length (Publication.environmentRootStageEntries successor))
  assertEqual
    "twelve root witnesses"
    12
    (length (Publication.publicationWitnessEnvironmentRootStages witness))
  mapM_
    ( \root ->
        assertEqual
          "root lookup"
          (Just root)
          ( Publication.lookupPositionedEnvironmentRoot
              (Environment.positionedEnvironmentRootPublicationId root)
              successor
          )
    )
    roots
  assertBool
    "manifest and root indices are exact"
    (Publication.publicationEnvironmentRangesWellFormed successor)
  assertEqual
    "environment roots are not ordinary outgoing publications"
    []
    (Publication.publicationWitnessOutgoing witness)
  assertEqual
    "environment roots remain unstamped in Increment 2"
    []
    (Publication.publicationWitnessStampedStructuralStages witness)

caseRetainedRetry :: Assertion
caseRetainedRetry = do
  let (installed, _, manifest) = commitCanonical publicationBase rootPlans
  retry <-
    checkedIO
      "prepare retained exact retry"
      (Publication.prepareRetainedEnvironmentRootRange manifestKey installed)
  assertEqual
    "retry classification"
    Publication.EnvironmentRootRangeExactRetry
    (Publication.preparedEnvironmentRootRangeClassification retry)
  assertEqual
    "retry returns the exact retained manifest"
    manifest
    (Publication.preparedEnvironmentManifest retry)
  let (replayed, classification, replayedManifest) =
        Publication.commitEnvironmentRootRange retry
  assertEqual "committed retry classification" Publication.EnvironmentRootRangeExactRetry classification
  assertEqual "committed retry manifest" manifest replayedManifest
  assertBool "exact retry is state-identical" (replayed == installed)
  case prepareRootRange
    manifestKey
    fixtureHeraldMembershipGeneration
    rootPlans
    installed of
    Left (Publication.PublicationEnvironmentAcceptanceConflict observed) ->
      assertEqual "conflicting first-acceptance key" manifestKey observed
    other -> assertFailure ("expected acceptance conflict, got " <> resultShape other)

caseStructuralSourceArm :: Assertion
caseStructuralSourceArm = do
  let (installed, _, manifest) = commitCanonical publicationBase rootPlans
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
      sources = Publication.unstampedStructuralSourceStageEntries installed
  assertEqual "one source stage per manifest root" 12 (length sources)
  assertEqual
    "source stages retain canonical Herald-position order"
    (Environment.positionedEnvironmentRootHeraldPosition <$> roots)
    (Publication.structuralSourceHeraldPosition <$> sources)
  mapM_ (assertRootSource manifest) (zip roots sources)
  where
    assertRootSource manifest (root, source) = do
      let plan = Environment.positionedEnvironmentRootPlan root
      assertEqual
        "manifest-qualified source arm"
        (Just (manifest, root))
        (Publication.structuralSourceEnvironmentRoot source)
      assertEqual "no application-source alias" Nothing (Publication.structuralSourceApplicationRecord source)
      assertEqual
        "captured membership generation"
        (Just (heraldMembershipGenerationId fixtureHeraldMembershipGeneration))
        (Publication.structuralSourceMembershipGenerationId source)
      assertEqual "caller process" process (Publication.structuralSourceProcess source)
      assertEqual
        "checked root publication"
        (Environment.positionedEnvironmentRootChecked root)
        (Publication.structuralSourceChecked source)
      assertEqual
        "carrier sort occurrence"
        (Environment.environmentRootPlanOccurrenceId plan)
        (Publication.structuralSourceSortOccurrenceId source)
      assertEqual "frozen route" (Environment.environmentRootPlanRoute plan) (Publication.structuralSourceRoute source)
      assertEqual
        "source topology prerequisite"
        (Just (Environment.environmentRootPlanSourceTopologyPrerequisite plan))
        (Publication.structuralSourceTopologyPrerequisite source)
      assertEqual
        "carrier role"
        (environmentRootSlotStructuralCarrierRole (Environment.environmentRootPlanSlot plan))
        (Publication.structuralSourceRole source)

caseStructuralSourceStamp :: Assertion
caseStructuralSourceStamp = do
  let (installed, _, manifest) = commitCanonical publicationBase rootPlans
      root = NonEmpty.head (Environment.environmentManifestRoots manifest)
      plan = Environment.positionedEnvironmentRootPlan root
      checkedRoot = Environment.positionedEnvironmentRootChecked root
      predecessor = emptyStructuralVersionVector fixtureHeraldMembershipGeneration
      candidate = Publication.proposeStructuralOccurrence predecessor installed
  source <- case Publication.unstampedStructuralSourceStageEntries installed of
    first : _ -> pure first
    [] -> assertFailure "environment manifest exposed no structural source"
  let remoteHeralds = filter (/= localHerald) activeHeralds
  remote <- case remoteHeralds of
    first : _ -> pure first
    [] -> assertFailure "environment fixture exposed no remote Herald"
  assertBool
    "an unstamped environment root holds a covering publication prefix"
    ( not
        ( Publication.environmentRootPrefixSettledFor
            (Environment.positionedEnvironmentRootHeraldPosition root)
            remote
            installed
        )
    )
  digest <-
    checkedIO
      "environment structural digest"
      ( structuralPublicationDigestForSemantics
          (Environment.environmentRootPlanDescriptor plan)
          (Environment.environmentRootPlanOccurrenceId plan)
          checkedRoot
          process
          (Environment.environmentRootPlanSourceStrength plan)
          (Environment.environmentRootPlanSourceTopologyPrerequisite plan)
          (Environment.environmentRootPlanControlPrerequisite plan)
      )
  stamp <-
    checkedIO
      "environment structural stamp"
      ( mkStructuralOccurrenceStamp
          (Publication.structuralCandidateOccurrence candidate)
          predecessor
          (checkedPublicationId checkedRoot)
          digest
          (environmentRootSlotStructuralCarrierRole (Environment.environmentRootPlanSlot plan))
      )
  peerPublications <-
    Map.fromList
      <$> traverse
        (makePeerPublication plan checkedRoot stamp)
        remoteHeralds
  prepared <-
    checkedIO
      "prepare stamped environment source"
      ( Publication.prepareStampedStructuralSourceStage
          source
          (Environment.environmentRootPlanSourceTopologyPrerequisite plan)
          stamp
          peerPublications
          installed
      )
  assertEqual
    "first stamp classification"
    Publication.StampedStructuralFirstRetained
    (Publication.preparedStampedStructuralClassification prepared)
  let (stamped, classification, retained) =
        Publication.commitStampedStructuralStage prepared
  assertEqual
    "committed stamp classification"
    Publication.StampedStructuralFirstRetained
    classification
  assertEqual
    "stamped root retains its exact manifest qualification"
    (Just (manifest, root))
    (Publication.stampedStructuralEnvironmentRoot retained)
  assertEqual
    "one root left the unsequenced source order"
    11
    (length (Publication.unstampedStructuralSourceStageEntries stamped))
  assertBool
    "the exact stamped root releases its publication prefix"
    ( Publication.environmentRootPrefixSettledFor
        (Environment.positionedEnvironmentRootHeraldPosition root)
        remote
        stamped
    )
  assertEqual
    "retained occurrence lookup"
    (Just retained)
    ( Publication.lookupStampedStructuralStage
        (Publication.structuralCandidateOccurrence candidate)
        stamped
    )
  replay <-
    checkedIO
      "prepare exact environment stamp retry"
      ( Publication.prepareStampedStructuralSourceStage
          source
          (Environment.environmentRootPlanSourceTopologyPrerequisite plan)
          stamp
          peerPublications
          stamped
      )
  assertEqual
    "exact retry classification"
    Publication.StampedStructuralExactRetry
    (Publication.preparedStampedStructuralClassification replay)
  let (replayed, _, replayedStage) = Publication.commitStampedStructuralStage replay
  assertBool "exact retry is state-identical" (replayed == stamped)
  assertEqual "exact retry returns retained stamp" retained replayedStage

makePeerPublication ::
  Environment.EnvironmentRootPlan ->
  CheckedPublication ->
  StructuralOccurrenceStamp ->
  HeraldEpoch ->
  IO (HeraldEpoch, PeerPublication)
makePeerPublication plan checkedRoot stamp peer = do
  let destinations =
        [ publicationDestination
            (destinationDelta destination)
            (destinationStoreIncarnation destination)
            (destinationStrength destination)
        | destination <- routeDestinations (Environment.environmentRootPlanRoute plan),
          destinationHerald destination == peer
        ]
  nonEmptyDestinations <-
    maybe
      (assertFailure "environment peer route unexpectedly empty")
      pure
      (NonEmpty.nonEmpty destinations)
  batch <-
    checkedIO
      "environment peer batch"
      ( mkPublicationBatch
          (checkedPublicationId checkedRoot)
          process
          (checkedPublicationSort checkedRoot)
          (Environment.environmentRootPlanOccurrenceId plan)
          (checkedPublicationCanonicalValue checkedRoot)
          (Environment.environmentRootPlanSourceStrength plan)
          (Environment.environmentRootPlanSourceTopologyPrerequisite plan)
          (Environment.environmentRootPlanControlPrerequisite plan)
          nonEmptyDestinations
      )
  publication <-
    checkedIO
      "environment peer publication"
      ( mkStructuralPeerPublication
          (Environment.environmentRootPlanDescriptor plan)
          (Environment.environmentRootPlanOccurrenceId plan)
          checkedRoot
          stamp
          batch
      )
  pure (peer, publication)

caseLaterRangeContinuesTenures :: Assertion
caseLaterRangeContinuesTenures = do
  let (afterFirst, _, _) = commitCanonical publicationBase rootPlans
      laterPlans = rootPlansAt 12
      laterKey = manifestKeyAt 2
  prepared <-
    checkedIO
      "prepare later environment range"
      ( prepareRootRange
          laterKey
          fixtureHeraldMembershipGeneration
          laterPlans
          afterFirst
      )
  let (successor, _, manifest) = Publication.commitEnvironmentRootRange prepared
      roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
  assertEqual "later writer sequences" [7 .. 12] (sourceSequences nablaSource roots)
  assertEqual "later reader sequences" [7 .. 12] (sourceSequences deltaSource roots)
  assertEqual
    "later Herald positions"
    [13 .. 24]
    ( heraldPublicationPositionWord64
        . Environment.positionedEnvironmentRootHeraldPosition
        <$> roots
    )
  assertEqual "both manifests remain indexed" 2 (length (Publication.environmentManifestEntries successor))
  assertEqual "all roots remain indexed" 24 (length (Publication.environmentRootStageEntries successor))
  assertBool
    "both manifest/root indices remain exact"
    (Publication.publicationEnvironmentRangesWellFormed successor)

caseCurrentMembershipRoute :: Assertion
caseCurrentMembershipRoute = do
  let first = NonEmpty.head rootPlans
      remaining = NonEmpty.tail rootPlans
      withExtra =
        NonEmpty.fromList
          (replacePlanRoute first (routeWithExtraLocalDestination first) : remaining)
  prepared <-
    checkedIO
      "prepare route with an additional Weak same-Herald destination"
      ( prepareRootRange
          manifestKey
          fixtureHeraldMembershipGeneration
          withExtra
          publicationBase
      )
  let (successor, _, _) = Publication.commitEnvironmentRootRange prepared
  assertBool
    "an additional Weak configured reader destination preserves mandatory membership coverage"
    (Publication.publicationEnvironmentRangesWellFormed successor)

caseRouteMembershipMismatch :: Assertion
caseRouteMembershipMismatch = do
  let first = NonEmpty.head rootPlans
      remaining = NonEmpty.tail rootPlans
      missingRemote =
        NonEmpty.fromList
          (replacePlanRoute first (routeForMembers first [localHerald]) : remaining)
  case prepareRootRange
    manifestKey
    fixtureHeraldMembershipGeneration
    missingRemote
    publicationBase of
    Left (Publication.PublicationEnvironmentRouteMembershipMismatch expected observed) -> do
      assertEqual "expected current members" activeHeralds expected
      assertEqual "only local member was routed" [localHerald] observed
    other -> assertFailure ("expected route-membership rejection, got " <> resultShape other)

caseMandatoryRouteWeakening :: Assertion
caseMandatoryRouteWeakening = do
  let first = NonEmpty.head rootPlans
      remaining = NonEmpty.tail rootPlans
      carrierRole = planCarrierCatalogueRole first
      weakened =
        checked
          "route with weak local destination"
          ( freezeRoute
              [ routeDestination
                  (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) herald carrierRole)
                  (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) herald carrierRole)
                  herald
                  (if herald == localHerald then Weak else Normal)
              | herald <- activeHeralds
              ]
          )
      weakenedPlans =
        NonEmpty.fromList (replacePlanRoute first weakened : remaining)
  case prepareRootRange
    manifestKey
    fixtureHeraldMembershipGeneration
    weakenedPlans
    publicationBase of
    Left (Publication.PublicationEnvironmentMandatoryRouteMissing observed) ->
      assertEqual
        "the weakened local mandatory destination is reported missing"
        [localHerald]
        observed
    other -> assertFailure ("expected missing-mandatory-route rejection, got " <> resultShape other)

propDuplicateTargetRejects :: NonNegative Int -> Property
propDuplicateTargetRejects (NonNegative choice) =
  counterexample ("duplicate ordinal=" <> show ordinal)
    $ case prepareRootRange
      manifestKey
      fixtureHeraldMembershipGeneration
      duplicated
      publicationBase of
      Left
        ( Publication.PublicationEnvironmentManifestProblem
            Environment.EnvironmentManifestShapeMismatch
          ) ->
          property
            ( Publication.publicationStateWitness publicationBase
                == Publication.publicationStateWitness
                  (Publication.initialState localHerald)
            )
      other -> counterexample ("unexpected result: " <> resultShape other) False
  where
    ordinal = 1 + choice `mod` 11
    plans = NonEmpty.toList rootPlans
    selected = plans !! ordinal
    duplicated =
      NonEmpty.fromList
        ( take ordinal plans
            <> [replacePlanTarget selected (generatedId 0)]
            <> drop (ordinal + 1) plans
        )

-- The initial positioned prefix owns all identities reserved for later wiring.
prepareRootRange ::
  Environment.EnvironmentManifestKey ->
  HeraldMembershipGeneration ->
  NonEmpty Environment.EnvironmentRootPlan ->
  Publication.State ->
  Either Publication.PublicationPreparationError Publication.PreparedEnvironmentRootRange
prepareRootRange key membership plans =
  Publication.prepareEnvironmentRootRange key membership generated plans
  where
    generated =
      fmap Environment.environmentRootPlanGeneratedId plans
        <> NonEmpty.fromList [generatedId ordinal | ordinal <- [wiringOffset .. wiringOffset + 18]]
    wiringOffset = 100 + 19 * fromIntegral (processAcceptancePositionOrdinal (Environment.environmentManifestKeyPosition key))

commitCanonical ::
  Publication.State ->
  NonEmpty Environment.EnvironmentRootPlan ->
  (Publication.State, Publication.EnvironmentRootRangeClassification, Environment.EnvironmentManifest)
commitCanonical state plans =
  Publication.commitEnvironmentRootRange
    ( checked
        "prepare environment range"
        ( prepareRootRange
            manifestKey
            fixtureHeraldMembershipGeneration
            plans
            state
        )
    )

sourceSequences ::
  NablaId ->
  [Environment.PositionedEnvironmentRoot] ->
  [Word64]
sourceSequences source roots =
  fmap
    ( nablaSequenceWord64
        . publicationNablaSequence
        . Environment.positionedEnvironmentRootPublicationId
    )
    ( filter
        ( (== source)
            . Environment.environmentRootPlanSourceNabla
            . Environment.positionedEnvironmentRootPlan
        )
        roots
    )

rootPlans :: NonEmpty Environment.EnvironmentRootPlan
rootPlans = rootPlansAt 0

rootPlansAt :: Int -> NonEmpty Environment.EnvironmentRootPlan
rootPlansAt offset =
  NonEmpty.fromList
    [ planFor slot (generatedId (offset + ordinal))
    | (ordinal, slot) <- zip [0 ..] (NonEmpty.toList environmentSlots)
    ]

planFor :: EnvironmentRootSlot -> GlobalUniqueId -> Environment.EnvironmentRootPlan
planFor slot generated =
  Environment.environmentRootPlan
    slot
    generated
    source
    Nothing
    authority
    (predefinedCatalogueDescriptor (profileEntryFor carrierCatalogueRole))
    (rootValue slot generated dataRole)
    occurrence
    Normal
    topologyPrerequisite
    (controlIndex 1)
    (routeForMembersForRole carrierCatalogueRole activeHeralds)
  where
    (carrierCatalogueRole, dataRole, source, occurrence) = rootFacts slot

replacePlanTarget ::
  Environment.EnvironmentRootPlan ->
  GlobalUniqueId ->
  Environment.EnvironmentRootPlan
replacePlanTarget original replacement =
  Environment.environmentRootPlan
    (Environment.environmentRootPlanSlot original)
    replacement
    (Environment.environmentRootPlanSourceNabla original)
    (Environment.environmentRootPlanSourceSequencingObject original)
    (Environment.environmentRootPlanSourceAuthority original)
    (Environment.environmentRootPlanDescriptor original)
    (rootValue (Environment.environmentRootPlanSlot original) replacement dataRole)
    (Environment.environmentRootPlanOccurrenceId original)
    (Environment.environmentRootPlanSourceStrength original)
    (Environment.environmentRootPlanSourceTopologyPrerequisite original)
    (Environment.environmentRootPlanControlPrerequisite original)
    (Environment.environmentRootPlanRoute original)
  where
    (_, dataRole, _, _) = rootFacts (Environment.environmentRootPlanSlot original)

replacePlanRoute ::
  Environment.EnvironmentRootPlan ->
  FrozenRoute ->
  Environment.EnvironmentRootPlan
replacePlanRoute original replacement =
  Environment.environmentRootPlan
    (Environment.environmentRootPlanSlot original)
    (Environment.environmentRootPlanGeneratedId original)
    (Environment.environmentRootPlanSourceNabla original)
    (Environment.environmentRootPlanSourceSequencingObject original)
    (Environment.environmentRootPlanSourceAuthority original)
    (Environment.environmentRootPlanDescriptor original)
    (Environment.environmentRootPlanValue original)
    (Environment.environmentRootPlanOccurrenceId original)
    (Environment.environmentRootPlanSourceStrength original)
    (Environment.environmentRootPlanSourceTopologyPrerequisite original)
    (Environment.environmentRootPlanControlPrerequisite original)
    replacement

routeWithExtraLocalDestination :: Environment.EnvironmentRootPlan -> FrozenRoute
routeWithExtraLocalDestination plan =
  checked
    "route with extra local configured destination"
    ( freezeRoute
        ( routeDestinationsForRole carrierCatalogueRole activeHeralds
            <> [ routeDestination
                   (checked "extra Delta" (mkDeltaId (fixtureIdentifierBytes 0xf1)))
                   (checked "extra Store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xf2)))
                   localHerald
                   Weak
               ]
        )
    )
  where
    carrierCatalogueRole = planCarrierCatalogueRole plan

planCarrierCatalogueRole :: Environment.EnvironmentRootPlan -> PredefinedSortRole
planCarrierCatalogueRole plan = carrierCatalogueRole
  where
    (carrierCatalogueRole, _, _, _) =
      rootFacts (Environment.environmentRootPlanSlot plan)

routeForMembers :: Environment.EnvironmentRootPlan -> [HeraldEpoch] -> FrozenRoute
routeForMembers plan = routeForMembersForRole carrierCatalogueRole
  where
    (carrierCatalogueRole, _, _, _) = rootFacts (Environment.environmentRootPlanSlot plan)

routeForMembersForRole :: PredefinedSortRole -> [HeraldEpoch] -> FrozenRoute
routeForMembersForRole role members =
  checked "environment route" (freezeRoute (routeDestinationsForRole role members))

routeDestinationsForRole ::
  PredefinedSortRole -> [HeraldEpoch] -> [RouteDestination]
routeDestinationsForRole role members =
  [ routeDestination
      (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) herald role)
      (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) herald role)
      herald
      Normal
  | herald <- members
  ]

rootValue :: EnvironmentRootSlot -> GlobalUniqueId -> PredefinedSortRole -> Value
rootValue slot generated carriedRole =
  checked "closed environment root value" (recordValue fields)
  where
    fields =
      [ (field "object_id", globalUniqueIdValue generated),
        (field "label", labelValue ((ProcessLabel process, 0))),
        (field "sort_id", bytesValue (sortIdBytes (profileSortFor carriedRole)))
      ]
        <> case environmentRootClaimView (environmentRootSlotClaim slot) of
          EnvironmentWriterRootClaimView {} ->
            [(field "sequencing_object", optionalGlobalUniqueIdValue Nothing)]
          EnvironmentReaderRootClaimView {} -> []
          EnvironmentHubRootClaimView -> error "endpoint fixture received a hub slot"
          EnvironmentEdgeRootClaimView {} -> error "endpoint fixture received an edge slot"

rootFacts ::
  EnvironmentRootSlot ->
  (PredefinedSortRole, PredefinedSortRole, NablaId, SortDefinitionOccurrenceId)
rootFacts slot =
  case environmentRootClaimView (environmentRootSlotClaim slot) of
    EnvironmentWriterRootClaimView role _ ->
      checkedCarrier NablaCarrier (NablaRole, role, nablaSource, nablaOccurrence)
    EnvironmentReaderRootClaimView role ->
      checkedCarrier DeltaCarrier (DeltaRole, role, deltaSource, deltaOccurrence)
    EnvironmentHubRootClaimView -> error "endpoint fixture received a hub slot"
    EnvironmentEdgeRootClaimView {} -> error "endpoint fixture received an edge slot"
  where
    checkedCarrier expected facts
      | environmentRootSlotStructuralCarrierRole slot == expected = facts
      | otherwise = error "closed environment carrier role changed"

environmentSlots :: NonEmpty EnvironmentRootSlot
environmentSlots = environmentManifestShapeRoots profileEnvironmentManifestShape

publicationBase :: Publication.State
publicationBase = Publication.initialState localHerald

manifestKey :: Environment.EnvironmentManifestKey
manifestKey = manifestKeyAt 1

manifestKeyAt :: Word64 -> Environment.EnvironmentManifestKey
manifestKeyAt position =
  checked
    "environment manifest key"
    (Environment.environmentManifestKey process (processAcceptancePosition process position))

process :: ProcessEpochId
process = checked "environment process" (mkProcessEpochId (fixtureIdentifierBytes 0xc1))

localHerald :: HeraldEpoch
localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis

activeHeralds :: [HeraldEpoch]
activeHeralds =
  sort
    (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs fixtureHeraldMembershipGeneration))

authority :: AuthorityEpoch
authority = genesisAuthorityEpoch

nablaSource, deltaSource :: NablaId
nablaSource = checked "Nabla carrier source" (mkNablaId (fixtureIdentifierBytes 0xe1))
deltaSource = checked "Delta carrier source" (mkNablaId (fixtureIdentifierBytes 0xe2))

nablaOccurrence, deltaOccurrence :: SortDefinitionOccurrenceId
nablaOccurrence =
  checked "Nabla carrier occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xd1))
deltaOccurrence =
  checked "Delta carrier occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xd2))

topologyPrerequisite :: TopologyCutId
topologyPrerequisite =
  checked "environment topology prerequisite" (mkTopologyCutId (fixtureIdentifierBytes 0xd3))

generatedId :: Int -> GlobalUniqueId
generatedId ordinal =
  checked
    "generated environment identity"
    (mkGlobalUniqueId (identityBytes (fromIntegral ordinal + 0x20)))

identityBytes :: Word8 -> ByteString.ByteString
identityBytes byte = ByteString.replicate 31 0 <> ByteString.singleton byte

field :: Text -> FieldName
field name = checked ("field " <> show name) (mkFieldName name)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

resultShape :: Either problem value -> String
resultShape = either (const "Left") (const "Right")
