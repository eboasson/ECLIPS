{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module StructuralPublicationProperties
  ( tests,
  )
where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word8)
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    edgeStrengthSymbol,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndex,
    firstStructuralSequence,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    nextStructuralSequence,
    publicationId,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    canonicalizeDescriptor,
  )
import Eclips.Domain.Sort.Descriptor
  ( DescriptorAdmission (PrimordialDescriptor),
    DescriptorSpec (..),
    StructuralCarrierRole (..),
    checkedDescriptorSpec,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profileEntryFor,
    sortDefinitionValue,
  )
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.Startup (deriveSystemViewDeltaId, deriveSystemViewStoreIncarnationId)
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (VoidLabel),
    Value,
    boolValue,
    bytesValue,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Genesis.Internal (checkedSystemId)
import Eclips.Herald.Graph.TerminalSource
  ( deriveTerminalStructuralPayloadDigest,
    terminalStructuralOccurrence,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PeerPublicationKind (..),
    PeerPublicationProblem (..),
    PublicationBatch,
    PublicationBatchProblem,
    PublicationDestination,
    StructuralOccurrenceStamp,
    StructuralOccurrenceStampProblem (..),
    StructuralPublicationDigest,
    decodeStructuralPublicationCanonicalBytes,
    mkOrdinaryPeerPublication,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    peerPublicationBatch,
    peerPublicationDigest,
    peerPublicationItem,
    peerPublicationKind,
    peerPublicationStructuralCanonicalBytes,
    peerPublicationStructuralStamp,
    presentedOrdinaryPeerPublication,
    publicationDestination,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralPublicationCanonicalBytesForSemantics,
    structuralPublicationDigestBytes,
    structuralPublicationDigestFor,
    structuralPublicationDigestForSemantics,
    structuralPublicationSemanticsCanonicalValue,
    structuralPublicationSemanticsCarrierRole,
    structuralPublicationSemanticsControlPrerequisite,
    structuralPublicationSemanticsOccurrenceId,
    structuralPublicationSemanticsPublicationId,
    structuralPublicationSemanticsSortId,
    structuralPublicationSemanticsSourceProcess,
    structuralPublicationSemanticsSourceStrength,
    structuralPublicationSemanticsSourceTopologyPrerequisite,
  )
import Eclips.Herald.PeerStream
  ( firstStreamSequence,
    mkStreamDirection,
    nextStreamSequence,
    peerItemDigest,
    peerItemDigestBytes,
  )
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State
  ( DestinationOutcome (DestinationApplied, DestinationPending),
    IncomingDisposition (ProtocolRejected),
    PublicationDependency (..),
    commitIncomingPublication,
    commitIncomingPublicationRejection,
    finalizeIncomingPublication,
    incomingPublicationDisposition,
    incomingPublicationResolution,
    incomingPublicationSemanticallyAuthenticated,
    initialState,
    prepareIncomingPublicationCandidate,
    prepareIncomingPublicationRejection,
  )
import Eclips.Herald.UseCase.TerminalStructuralArchive
  ( retainedRetiredSourceOccurrences,
  )
import GenesisFixtures (fixtureCheckedGenesis, fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "structural publication draft"
    [ testCase "ordinary construction preserves the live Step-7 digest" caseOrdinaryPreserved,
      testCase "the structural semantic and peer transcripts are golden" caseStructuralGolden,
      testCase "the structural digest is field-sensitive" caseStructuralDigestSensitivity,
      testCase "the complete item binds occurrence and predecessor" caseStampSensitivity,
      testCase "receiver destinations qualify only the peer digest" caseReceiverQualifiedDigest,
      testCase "canonical semantic bytes are terminal-source compatible" caseCanonicalSemanticPayload,
      testCase "terminal archive retains valid structural work and omits protocol rejection" caseRetainedTerminalArchive,
      testProperty "destination and vector presentations normalize" propPresentationNormalization,
      testProperty "retire then admit extends mandatory delivery and preserves frozen ordinary destinations" propMembershipRouteExtension,
      testCase "the closed ordinary and structural roles are enforced" caseClosedRoles,
      testCase "stamp source and predecessor claims are checked" caseStampMismatch,
      testCase "stamp publication, role, and digest claims are checked" caseMismatch,
      testCase "only the exact predefined structural descriptor is admitted" caseExactPredefinedDescriptor,
      testCase "descriptor, checked value, and batch facts must agree" caseBatchMismatch
    ]

propMembershipRouteExtension :: Bool -> Property
propMembershipRouteExtension weak =
  case fixture of
    Left problem -> counterexample problem False
    Right (frozen, extended, again, ordinary, retired, newcomer) ->
      let destinations = Route.routeDestinations extended
          newcomerDestination =
            Route.routeDestination
              (deriveSystemViewDeltaId system newcomer Profile.EdgeRole)
              (deriveSystemViewStoreIncarnationId system newcomer Profile.EdgeRole)
              newcomer
              Normal
       in conjoin
            [ counterexample "new member receives the exact mandatory Edge system view" (newcomerDestination `elem` destinations),
              counterexample "frozen ordinary destination retains exact strength and incarnation" (ordinary `elem` destinations),
              counterexample "old route remains immutable audit evidence" (all (`elem` destinations) (Route.routeDestinations frozen)),
              counterexample "retired peer remains only in historical route evidence" (any ((== retired) . Route.destinationHerald) (Route.routeDestinations frozen)),
              again === extended
            ]
  where
    system = checkedSystemId fixtureCheckedGenesis
    fixture = do
      old <- identityEither "old" mkHeraldEpoch 0x41
      retired <- identityEither "retired" mkHeraldEpoch 0x42
      newcomer <- identityEither "newcomer" mkHeraldEpoch 0x43
      ordinaryDelta <- identityEither "ordinary delta" mkDeltaId 0x44
      ordinaryStore <- identityEither "ordinary store" mkStoreIncarnationId 0x45
      generation <- fixtureMembership (old :| [retired])
      probe <- firstShow "probe" (Membership.deriveHeraldFailureProbeId (controlIndex 1))
      let resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      survivors <- firstShow "retire" (Membership.retireHeraldMembershipGeneration (controlIndex 2) resolution retired generation)
      admission <- firstShow "admission" (Membership.deriveHeraldAdmissionId (controlIndex 3))
      successor <- firstShow "activate" (Membership.admitHeraldMembershipGeneration (controlIndex 4) admission newcomer survivors)
      let ordinary = Route.routeDestination ordinaryDelta ordinaryStore old (if weak then Weak else Normal)
      original <- firstShow "ordinary route" (Route.freezeRoute [ordinary])
      frozen <- firstShow "original mandatory route" (PublicationRoute.extendStructuralSystemViews system generation EdgeCarrier original)
      extended <- firstShow "successor mandatory route" (PublicationRoute.extendStructuralSystemViews system successor EdgeCarrier frozen)
      again <- firstShow "exact route retry" (PublicationRoute.extendStructuralSystemViews system successor EdgeCarrier extended)
      pure (frozen, extended, again, ordinary, retired, newcomer)

caseOrdinaryPreserved :: Assertion
caseOrdinaryPreserved = do
  fixture <- checked "ordinary fixture" ordinaryFixture
  draft <-
    checked
      "ordinary draft"
      ( mkOrdinaryPeerPublication
          fixture.descriptor
          fixture.definitionOccurrence
          fixture.publication
          fixture.batch
      )
  assertEqual "ordinary kind" OrdinaryPublicationKind (peerPublicationKind draft)
  assertEqual "ordinary batch" fixture.batch (peerPublicationBatch draft)
  assertEqual "ordinary arm has no stamp" Nothing (peerPublicationStructuralStamp draft)
  assertEqual
    "the draft reuses the exact live digest"
    (peerPublicationDigest (presentedOrdinaryPeerPublication fixture.batch))
    (peerPublicationDigest draft)
  assertEqual
    "the pre-existing live item still carries those exact bytes"
    (peerPublicationDigest draft)
    (peerItemDigest (peerPublicationItem draft))

caseStructuralGolden :: Assertion
caseStructuralGolden = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  assertEqual
    "destination-independent structural transcript"
    [55, 36, 226, 145, 15, 112, 218, 115, 81, 114, 200, 134, 116, 75, 161, 191, 79, 219, 137, 127, 161, 19, 167, 22, 56, 99, 242, 141, 247, 241, 61, 46]
    (ByteString.unpack (structuralPublicationDigestBytes fixture.structuralDigest))
  assertEqual
    "receiver-qualified structural item transcript"
    [16, 186, 31, 111, 58, 64, 55, 34, 230, 49, 7, 57, 55, 6, 225, 218, 213, 203, 72, 41, 199, 20, 141, 87, 190, 88, 26, 27, 197, 181, 50, 244]
    (ByteString.unpack (peerItemDigestBytes (peerPublicationDigest fixture.draft)))
  assertEqual "structural kind" StructuralPublicationKind (peerPublicationKind fixture.draft)
  assertEqual "structural batch" fixture.batch (peerPublicationBatch fixture.draft)
  assertEqual "checked stamp retained" (Just fixture.stamp) (peerPublicationStructuralStamp fixture.draft)
  assertEqual "stamp occurrence" fixture.occurrence (structuralOccurrenceStampOccurrence fixture.stamp)
  assertEqual "stamp predecessor" fixture.predecessor (structuralOccurrenceStampPredecessor fixture.stamp)
  assertEqual "stamp publication" (checkedPublicationId fixture.publication) (structuralOccurrenceStampPublication fixture.stamp)
  assertEqual "stamp digest" fixture.structuralDigest (structuralOccurrenceStampPublicationDigest fixture.stamp)
  assertEqual "stamp role" NeutralVertexCarrier (structuralOccurrenceStampCarrierRole fixture.stamp)

caseStructuralDigestSensitivity :: Assertion
caseStructuralDigestSensitivity = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  otherProcess <- identity "other process" mkProcessEpochId 0x31
  otherOccurrence <- identity "other occurrence" mkSortDefinitionOccurrenceId 0x32
  otherNabla <- identity "other nabla" mkNablaId 0x33
  alternateValue <- checked "alternate neutral value" (valueForRoleWithObjectByte NeutralVertexCarrier 0x34)
  let otherIdentifier =
        publicationId
          otherNabla
          (alternateAuthority fixture.source)
          fixture.source
          (nablaSequence 3)
  otherPublication <-
    checked
      "other-identity publication"
      (mkCheckedPublication fixture.descriptor otherIdentifier fixture.value)
  alternatePublication <-
    checked
      "alternate-value publication"
      (mkCheckedPublication fixture.descriptor (checkedPublicationId fixture.publication) alternateValue)
  variants <-
    traverse
      (checked "variant structural digest")
      [ digestForRawBatch fixture fixture.publication otherProcess fixture.definitionOccurrence fixture.control fixture.destinations,
        digestForRawBatch fixture fixture.publication fixture.process otherOccurrence fixture.control fixture.destinations,
        digestForRawBatch fixture fixture.publication fixture.process fixture.definitionOccurrence (controlIndex 12) fixture.destinations,
        digestForRawBatch fixture otherPublication fixture.process fixture.definitionOccurrence fixture.control fixture.destinations,
        digestForRawBatch fixture alternatePublication fixture.process fixture.definitionOccurrence fixture.control fixture.destinations
      ]
  mapM_
    (assertNotEqual "a source semantic field changes the structural digest" fixture.structuralDigest)
    variants

caseStampSensitivity :: Assertion
caseStampSensitivity = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  let secondSequence = nextStructuralSequence firstStructuralSequence
  secondPredecessor <-
    checked
      "second predecessor"
      ( vector
          fixture.membership
          [ (fixture.source, structuralPrefixThrough firstStructuralSequence),
            (fixture.peer, emptyStructuralPrefix)
          ]
      )
  secondDraft <-
    checkedFor
      fixture
      (structuralOccurrenceId fixture.source secondSequence)
      secondPredecessor
  foreignAdvanced <-
    checked
      "foreign-advanced predecessor"
      ( vector
          fixture.membership
          [ (fixture.source, emptyStructuralPrefix),
            (fixture.peer, structuralPrefixThrough firstStructuralSequence)
          ]
      )
  foreignDraft <- checkedFor fixture fixture.occurrence foreignAdvanced
  assertNotEqual
    "the occurrence dot is digest-bound"
    (peerPublicationDigest fixture.draft)
    (peerPublicationDigest secondDraft)
  assertNotEqual
    "the complete predecessor vector is digest-bound"
    (peerPublicationDigest fixture.draft)
    (peerPublicationDigest foreignDraft)

caseReceiverQualifiedDigest :: Assertion
caseReceiverQualifiedDigest = do
  first <- checked "first receiver fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  alternateDestination <- destination 0x61 0x62 Weak
  second <-
    checked
      "second receiver fixture"
      (structuralFixture NeutralVertexCarrier (alternateDestination :| []))
  assertEqual
    "one occurrence has one destination-independent structural digest"
    first.structuralDigest
    second.structuralDigest
  semanticDigest <-
    checked
      "destination-independent semantic digest"
      ( structuralPublicationDigestForSemantics
          first.descriptor
          first.definitionOccurrence
          first.publication
          first.process
          Normal
          fixtureTopologyCut
          first.control
      )
  assertEqual
    "source semantics equal the digest derived through the first receiver batch"
    semanticDigest
    first.structuralDigest
  assertEqual
    "source semantics equal the digest derived through a differently qualified receiver batch"
    semanticDigest
    second.structuralDigest
  assertNotEqual
    "receiver-specific destination cuts have distinct peer digests"
    (peerPublicationDigest first.draft)
    (peerPublicationDigest second.draft)

caseCanonicalSemanticPayload :: Assertion
caseCanonicalSemanticPayload = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  payload <-
    checked
      "destination-independent semantic bytes"
      ( structuralPublicationCanonicalBytesForSemantics
          fixture.descriptor
          fixture.definitionOccurrence
          fixture.publication
          fixture.process
          Normal
          fixtureTopologyCut
          fixture.control
      )
  semanticDigest <-
    checked
      "destination-independent semantic digest"
      ( structuralPublicationDigestForSemantics
          fixture.descriptor
          fixture.definitionOccurrence
          fixture.publication
          fixture.process
          Normal
          fixtureTopologyCut
          fixture.control
      )
  assertEqual
    "terminal-source hashing preserves the peer-publication semantic digest"
    semanticDigest
    (deriveTerminalStructuralPayloadDigest payload)
  assertEqual
    "a retained peer publication recovers the exact destination-free transcript"
    (Just payload)
    (peerPublicationStructuralCanonicalBytes fixture.draft)
  decoded <-
    checked
      "checked destination-independent semantic transcript"
      (decodeStructuralPublicationCanonicalBytes payload)
  assertEqual
    "the transcript retains the publication"
    (checkedPublicationId fixture.publication)
    (structuralPublicationSemanticsPublicationId decoded)
  assertEqual
    "the transcript retains the source process"
    fixture.process
    (structuralPublicationSemanticsSourceProcess decoded)
  assertEqual
    "the transcript retains the carrier sort"
    (checkedPublicationSort fixture.publication)
    (structuralPublicationSemanticsSortId decoded)
  assertEqual
    "the transcript retains the effective definition"
    fixture.definitionOccurrence
    (structuralPublicationSemanticsOccurrenceId decoded)
  assertEqual
    "the transcript retains the exact canonical value"
    (checkedPublicationCanonicalValue fixture.publication)
    (structuralPublicationSemanticsCanonicalValue decoded)
  assertEqual
    "the transcript retains source strength"
    Normal
    (structuralPublicationSemanticsSourceStrength decoded)
  assertEqual
    "the transcript retains the source topology prerequisite"
    fixtureTopologyCut
    (structuralPublicationSemanticsSourceTopologyPrerequisite decoded)
  assertEqual
    "the transcript retains the control prerequisite"
    fixture.control
    (structuralPublicationSemanticsControlPrerequisite decoded)
  assertEqual
    "the transcript retains the structural carrier role"
    NeutralVertexCarrier
    (structuralPublicationSemanticsCarrierRole decoded)
  assertProblem
    "trailing bytes cannot alias one canonical semantic transcript"
    (const True)
    (decodeStructuralPublicationCanonicalBytes (ByteString.snoc payload 0))
  _ <-
    checked
      "terminal structural occurrence"
      (terminalStructuralOccurrence fixture.stamp payload)
  pure ()

caseRetainedTerminalArchive :: Assertion
caseRetainedTerminalArchive = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  direction <- checked "source-to-survivor direction" (mkStreamDirection fixture.source fixture.peer)
  let membershipId = heraldMembershipGenerationId fixture.membership
      appliedOutcomes =
        Map.fromList
          [ (retainedDestination, DestinationApplied)
          | retainedDestination <-
              let first :| rest = fixture.destinations in first : rest
          ]
      pendingOutcomes = Map.map (const DestinationPending) appliedOutcomes
      retain context sequenceNumber draft resolution predecessor = do
        candidate <-
          checked
            (context <> " candidate")
            ( prepareIncomingPublicationCandidate
                membershipId
                direction
                sequenceNumber
                (peerPublicationDigest draft)
                draft
                predecessor
            )
        prepared <-
          checked
            (context <> " publication")
            (finalizeIncomingPublication (Just resolution) candidate)
        pure (commitIncomingPublication prepared)
      makeVariant context nablaByte occurrence predecessor = do
        variantNabla <- identity (context <> " nabla") mkNablaId nablaByte
        let identifier =
              publicationId
                variantNabla
                (fixtureAuthority fixture.source)
                fixture.source
                (nablaSequence 4)
        publication <-
          checked
            (context <> " checked publication")
            (mkCheckedPublication fixture.descriptor identifier fixture.value)
        batch <-
          checked
            (context <> " publication batch")
            ( batchFor
                publication
                fixture.process
                fixture.definitionOccurrence
                fixture.control
                fixture.destinations
            )
        digest <-
          checked
            (context <> " structural digest")
            ( structuralPublicationDigestFor
                fixture.descriptor
                fixture.definitionOccurrence
                publication
                batch
            )
        stamp <-
          checked
            (context <> " structural stamp")
            ( mkStructuralOccurrenceStamp
                occurrence
                predecessor
                identifier
                digest
                NeutralVertexCarrier
            )
        draft <-
          checked
            (context <> " structural draft")
            ( mkStructuralPeerPublication
                fixture.descriptor
                fixture.definitionOccurrence
                publication
                stamp
                batch
            )
        pure (identifier, stamp, draft)
      secondStructuralSequence =
        nextStructuralSequence firstStructuralSequence
      thirdStructuralSequence =
        nextStructuralSequence secondStructuralSequence
      fourthStructuralSequence =
        nextStructuralSequence thirdStructuralSequence
      secondStreamSequence = nextStreamSequence firstStreamSequence
      thirdStreamSequence = nextStreamSequence secondStreamSequence
      fourthStreamSequence = nextStreamSequence thirdStreamSequence
  appliedResolution <-
    checked
      "applied incoming resolution"
      (incomingPublicationResolution Set.empty appliedOutcomes)
  (appliedState, appliedRecord) <-
    retain
      "applied incoming structural"
      firstStreamSequence
      fixture.draft
      appliedResolution
      (initialState fixture.peer)
  assertBool
    "applied structural evidence is authenticated for archival"
    (incomingPublicationSemanticallyAuthenticated appliedRecord)
  postAuthenticationPredecessor <-
    checked
      "post-authentication structural predecessor"
      ( vector
          fixture.membership
          [ (fixture.source, structuralPrefixThrough firstStructuralSequence),
            (fixture.peer, emptyStructuralPrefix)
          ]
      )
  (_, postAuthenticationStamp, postAuthenticationDraft) <-
    makeVariant
      "post-authentication held"
      0x31
      (structuralOccurrenceId fixture.source secondStructuralSequence)
      postAuthenticationPredecessor
  postAuthenticationResolution <-
    checked
      "post-authentication held resolution"
      ( incomingPublicationResolution
          (Set.singleton (SuccessorStructuralBaseDependency membershipId))
          pendingOutcomes
      )
  (postAuthenticationState, postAuthenticationRecord) <-
    retain
      "post-authentication held structural"
      secondStreamSequence
      postAuthenticationDraft
      postAuthenticationResolution
      appliedState
  assertBool
    "an execution-only structural hold remains authenticated for archival"
    (incomingPublicationSemanticallyAuthenticated postAuthenticationRecord)
  rejectedPredecessor <-
    checked
      "rejected structural predecessor"
      ( vector
          fixture.membership
          [ (fixture.source, structuralPrefixThrough secondStructuralSequence),
            (fixture.peer, emptyStructuralPrefix)
          ]
      )
  (rejectedIdentifier, _, rejectedDraft) <-
    makeVariant
      "rejected held"
      0x32
      (structuralOccurrenceId fixture.source thirdStructuralSequence)
      rejectedPredecessor
  rejectedResolution <-
    checked
      "rejected held resolution"
      ( incomingPublicationResolution
          (Set.singleton (ControlIndexDependency (controlIndex 12)))
          pendingOutcomes
      )
  (rejectionPredecessor, _) <-
    retain
      "rejected held structural"
      thirdStreamSequence
      rejectedDraft
      rejectedResolution
      postAuthenticationState
  rejection <-
    checked
      "protocol-reject held structural publication"
      (prepareIncomingPublicationRejection rejectedIdentifier rejectionPredecessor)
  let (rejectedState, rejectedRecord) =
        commitIncomingPublicationRejection rejection
  assertEqual
    "the malformed retained item is terminally protocol-rejected"
    ProtocolRejected
    (incomingPublicationDisposition rejectedRecord)
  assertBool
    "protocol-rejected structural evidence is not authenticated for archival"
    (not (incomingPublicationSemanticallyAuthenticated rejectedRecord))
  preAuthenticationPredecessor <-
    checked
      "pre-authentication structural predecessor"
      ( vector
          fixture.membership
          [ (fixture.source, structuralPrefixThrough thirdStructuralSequence),
            (fixture.peer, emptyStructuralPrefix)
          ]
      )
  (_, _, preAuthenticationDraft) <-
    makeVariant
      "pre-authentication held"
      0x33
      (structuralOccurrenceId fixture.source fourthStructuralSequence)
      preAuthenticationPredecessor
  preAuthenticationResolution <-
    checked
      "pre-authentication held resolution"
      ( incomingPublicationResolution
          ( Set.fromList
              [ EffectiveSortDependency
                  (checkedPublicationSort fixture.publication)
                  fixture.definitionOccurrence,
                ControlIndexDependency (controlIndex 12),
                SourceTopologyDependency fixtureTopologyCut
              ]
          )
          pendingOutcomes
      )
  (archiveState, preAuthenticationRecord) <-
    retain
      "pre-authentication held structural"
      fourthStreamSequence
      preAuthenticationDraft
      preAuthenticationResolution
      rejectedState
  assertBool
    "sort, control, and topology holds are not authenticated for archival"
    (not (incomingPublicationSemanticallyAuthenticated preAuthenticationRecord))
  expectedPayload <-
    checked
      "destination-independent semantic bytes"
      ( structuralPublicationCanonicalBytesForSemantics
          fixture.descriptor
          fixture.definitionOccurrence
          fixture.publication
          fixture.process
          Normal
          fixtureTopologyCut
          fixture.control
      )
  expected <-
    checked
      "expected terminal occurrence"
      (terminalStructuralOccurrence fixture.stamp expectedPayload)
  postAuthenticationPayload <-
    maybe
      (assertFailure "post-authentication structural draft has no canonical payload")
      pure
      (peerPublicationStructuralCanonicalBytes postAuthenticationDraft)
  postAuthenticationExpected <-
    checked
      "expected post-authentication terminal occurrence"
      ( terminalStructuralOccurrence
          postAuthenticationStamp
          postAuthenticationPayload
      )
  actual <-
    checked
      "retired-source survivor inventory"
      (retainedRetiredSourceOccurrences fixture.source archiveState)
  assertEqual
    "the survivor seals applied and post-authentication work but omits rejected and pre-authentication payloads"
    [expected, postAuthenticationExpected]
    actual
  assertEqual
    "a different source cannot borrow the retained occurrence"
    (Right [])
    (retainedRetiredSourceOccurrences fixture.peer archiveState)

propPresentationNormalization :: Bool -> Property
propPresentationNormalization reversed =
  case normalizedFixturePair reversed of
    Left problem -> counterexample problem False
    Right (left, right) ->
      counterexample
        (show (left.draft, right.draft))
        ( ( left.batch,
            left.predecessor,
            left.structuralDigest,
            peerPublicationDigest left.draft
          )
            === ( right.batch,
                  right.predecessor,
                  right.structuralDigest,
                  peerPublicationDigest right.draft
                )
        )

caseClosedRoles :: Assertion
caseClosedRoles = do
  ordinary <- checked "ordinary fixture" ordinaryFixture
  _ <-
    checked
      "ordinary draft"
      ( mkOrdinaryPeerPublication
          ordinary.descriptor
          ordinary.definitionOccurrence
          ordinary.publication
          ordinary.batch
      )
  assertProblem
    "an ordinary value cannot be wrapped in the structural arm"
    (== StructuralCarrierMissing)
    ( structuralPublicationDigestFor
        ordinary.descriptor
        ordinary.definitionOccurrence
        ordinary.publication
        ordinary.batch
    )
  mapM_ assertOrdinaryRejectsStructural publishedStructuralCarrierRoles
  mapM_
    (\role -> checked ("permitted structural role " <> show role) (structuralFixture role defaultDestinations) >> pure ())
    publishedStructuralCarrierRoles
  processFixture <- checked "process fixture" (baseFixture ProcessEpochCarrier defaultDestinations)
  processDraft <-
    checked
      "ordinary ProcessEpoch draft"
      ( mkOrdinaryPeerPublication
          processFixture.descriptor
          processFixture.definitionOccurrence
          processFixture.publication
          processFixture.batch
      )
  assertEqual
    "Herald-managed ProcessEpoch remains ordinary and forwardable"
    OrdinaryPublicationKind
    (peerPublicationKind processDraft)
  assertProblem
    "ProcessEpoch is Herald-managed, not a structural publication draft"
    (== StructuralProcessEpochUnsupported)
    ( structuralPublicationDigestFor
        processFixture.descriptor
        processFixture.definitionOccurrence
        processFixture.publication
        processFixture.batch
    )

caseStampMismatch :: Assertion
caseStampMismatch = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  otherSource <- identity "other source" mkHeraldEpoch 0x71
  membershipWithOther <-
    checked
      "membership containing other source"
      (fixtureMembership (otherSource :| [fixture.source, fixture.peer]))
  vectorWithOther <-
    checked
      "vector containing other source"
      ( vector
          membershipWithOther
          [ (otherSource, emptyStructuralPrefix),
            (fixture.source, emptyStructuralPrefix),
            (fixture.peer, emptyStructuralPrefix)
          ]
      )
  assertStampProblem
    "occurrence source equals publication source"
    (\case StructuralStampSourceMismatch occurrenceSource publicationSource -> occurrenceSource == otherSource && publicationSource == fixture.source; _ -> False)
    ( mkStructuralOccurrenceStamp
        (structuralOccurrenceId otherSource firstStructuralSequence)
        vectorWithOther
        (checkedPublicationId fixture.publication)
        fixture.structuralDigest
        NeutralVertexCarrier
    )
  membershipWithoutSource <-
    checked
      "membership without source"
      (fixtureMembership (fixture.peer :| []))
  let vectorWithoutSource = emptyStructuralVersionVector membershipWithoutSource
  assertStampProblem
    "the source component must be present"
    (== StructuralStampPredecessorSourceMissing fixture.source)
    ( mkStructuralOccurrenceStamp
        fixture.occurrence
        vectorWithoutSource
        (checkedPublicationId fixture.publication)
        fixture.structuralDigest
        NeutralVertexCarrier
    )
  assertStampProblem
    "sequence two requires source prefix one"
    (\case StructuralStampPredecessorMismatch source expected actual -> source == fixture.source && expected == structuralPrefixThrough firstStructuralSequence && actual == emptyStructuralPrefix; _ -> False)
    ( mkStructuralOccurrenceStamp
        (structuralOccurrenceId fixture.source (nextStructuralSequence firstStructuralSequence))
        fixture.predecessor
        (checkedPublicationId fixture.publication)
        fixture.structuralDigest
        NeutralVertexCarrier
    )

caseMismatch :: Assertion
caseMismatch = do
  fixture <- checked "neutral fixture" (structuralFixture NeutralVertexCarrier defaultDestinations)
  otherNabla <- identity "other nabla" mkNablaId 0x72
  let otherIdentifier =
        publicationId
          otherNabla
          (alternateAuthority fixture.source)
          fixture.source
          (nablaSequence 3)
  publicationStamp <-
    checked
      "publication-mismatch stamp"
      ( mkStructuralOccurrenceStamp
          fixture.occurrence
          fixture.predecessor
          otherIdentifier
          fixture.structuralDigest
          NeutralVertexCarrier
      )
  assertProblem
    "stamp publication equals the batch publication"
    (\case StructuralStampPublicationMismatch expected actual -> expected == checkedPublicationId fixture.publication && actual == otherIdentifier; _ -> False)
    ( mkStructuralPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        publicationStamp
        fixture.batch
    )
  roleStamp <-
    checked
      "role-mismatch stamp"
      ( mkStructuralOccurrenceStamp
          fixture.occurrence
          fixture.predecessor
          (checkedPublicationId fixture.publication)
          fixture.structuralDigest
          EdgeCarrier
      )
  assertProblem
    "stamp role equals the descriptor-derived role"
    (== StructuralStampCarrierRoleMismatch NeutralVertexCarrier EdgeCarrier)
    ( mkStructuralPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        roleStamp
        fixture.batch
    )
  otherProcess <- identity "other process" mkProcessEpochId 0x73
  wrongDigest <-
    checked
      "different semantic digest"
      (digestForRawBatch fixture fixture.publication otherProcess fixture.definitionOccurrence fixture.control fixture.destinations)
  digestStamp <-
    checked
      "digest-mismatch stamp"
      ( mkStructuralOccurrenceStamp
          fixture.occurrence
          fixture.predecessor
          (checkedPublicationId fixture.publication)
          wrongDigest
          NeutralVertexCarrier
      )
  assertProblem
    "the stamp digest is recomputed"
    (== StructuralStampDigestMismatch fixture.structuralDigest wrongDigest)
    ( mkStructuralPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        digestStamp
        fixture.batch
    )

caseExactPredefinedDescriptor :: Assertion
caseExactPredefinedDescriptor = do
  fixture <- checked "neutral fixture" (baseFixture NeutralVertexCarrier defaultDestinations)
  let baseSpecification = checkedDescriptorSpec (canonicalCheckedDescriptor fixture.descriptor)
      changedSpecification =
        baseSpecification
          { descriptorSpecMinimumRetentionMicros = 1
          }
  changedDescriptor <-
    checked
      "changed primordial structural descriptor"
      (canonicalizeDescriptor PrimordialDescriptor changedSpecification)
  changedPublication <-
    checked
      "changed-descriptor publication"
      (mkCheckedPublication changedDescriptor (checkedPublicationId fixture.publication) fixture.value)
  changedBatch <-
    checked
      "changed-descriptor batch"
      ( batchFor
          changedPublication
          fixture.process
          fixture.definitionOccurrence
          fixture.control
          fixture.destinations
      )
  assertProblem
    "a same-role lookalike is not a predefined carrier"
    (== StructuralDescriptorNotExactPredefined NeutralVertexCarrier)
    ( structuralPublicationDigestFor
        changedDescriptor
        fixture.definitionOccurrence
        changedPublication
        changedBatch
    )

caseBatchMismatch :: Assertion
caseBatchMismatch = do
  fixture <- checked "neutral fixture" (baseFixture NeutralVertexCarrier defaultDestinations)
  edge <- checked "edge fixture" (baseFixture EdgeCarrier defaultDestinations)
  assertProblem
    "descriptor and checked publication sort agree"
    (\case DescriptorPublicationSortMismatch {} -> True; _ -> False)
    ( mkOrdinaryPeerPublication
        edge.descriptor
        fixture.definitionOccurrence
        fixture.publication
        fixture.batch
    )
  wrongSortBatch <-
    checked
      "wrong-sort batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.process
          (checkedPublicationSort edge.publication)
          fixture.definitionOccurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          fixtureTopologyCut
          fixture.control
          fixture.destinations
      )
  assertProblem
    "checked publication and batch sort agree"
    (\case BatchPublicationSortMismatch {} -> True; _ -> False)
    ( mkOrdinaryPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        wrongSortBatch
    )
  otherOccurrence <-
    identity "other expected occurrence" mkSortDefinitionOccurrenceId 0x75
  assertProblem
    "the batch occurrence equals the registry-qualified occurrence"
    ( ==
        BatchDefinitionOccurrenceMismatch
          otherOccurrence
          fixture.definitionOccurrence
    )
    ( mkOrdinaryPeerPublication
        fixture.descriptor
        otherOccurrence
        fixture.publication
        fixture.batch
    )
  otherNabla <- identity "other nabla" mkNablaId 0x74
  let otherIdentifier =
        publicationId otherNabla (alternateAuthority fixture.source) fixture.source (nablaSequence 3)
  wrongIdentityBatch <-
    checked
      "wrong-identity batch"
      ( mkPublicationBatch
          otherIdentifier
          fixture.process
          (checkedPublicationSort fixture.publication)
          fixture.definitionOccurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          fixtureTopologyCut
          fixture.control
          fixture.destinations
      )
  assertProblem
    "checked publication and batch identity agree"
    (\case BatchPublicationIdentityMismatch {} -> True; _ -> False)
    ( mkOrdinaryPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        wrongIdentityBatch
    )
  wrongCanonicalBatch <-
    checked
      "wrong-canonical batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.process
          (checkedPublicationSort fixture.publication)
          fixture.definitionOccurrence
          (checkedPublicationCanonicalValue edge.publication)
          Normal
          fixtureTopologyCut
          fixture.control
          fixture.destinations
      )
  assertProblem
    "checked publication and batch canonical value agree"
    (== BatchCanonicalValueMismatch)
    ( mkOrdinaryPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        wrongCanonicalBatch
    )

data BaseFixture = BaseFixture
  { descriptor :: CanonicalDescriptor,
    publication :: CheckedPublication,
    batch :: PublicationBatch,
    value :: Value,
    source :: HeraldEpoch,
    peer :: HeraldEpoch,
    process :: ProcessEpochId,
    definitionOccurrence :: SortDefinitionOccurrenceId,
    control :: ControlIndex,
    destinations :: NonEmpty PublicationDestination,
    membership :: HeraldMembershipGeneration
  }

data StructuralFixture = StructuralFixture
  { descriptor :: CanonicalDescriptor,
    publication :: CheckedPublication,
    batch :: PublicationBatch,
    value :: Value,
    source :: HeraldEpoch,
    peer :: HeraldEpoch,
    process :: ProcessEpochId,
    definitionOccurrence :: SortDefinitionOccurrenceId,
    control :: ControlIndex,
    destinations :: NonEmpty PublicationDestination,
    membership :: HeraldMembershipGeneration,
    occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector,
    structuralDigest :: StructuralPublicationDigest,
    stamp :: StructuralOccurrenceStamp,
    draft :: PeerPublication
  }

baseFixture ::
  StructuralCarrierRole ->
  NonEmpty PublicationDestination ->
  Either String BaseFixture
baseFixture role destinations = do
  source <- identityEither "source" mkHeraldEpoch 0x10
  peer <- identityEither "peer" mkHeraldEpoch 0x11
  nabla <- identityEither "nabla" mkNablaId 0x12
  process <- identityEither "process" mkProcessEpochId 0x13
  definitionOccurrence <-
    identityEither "definition occurrence" mkSortDefinitionOccurrenceId 0x14
  value <- valueForRoleWithObjectByte role 0x15
  membership <- fixtureMembership (source :| [peer])
  let descriptor =
        predefinedCatalogueDescriptor
          (profileEntryFor (predefinedRoleForCarrier role))
      identifier =
        publicationId
          nabla
          (fixtureAuthority source)
          source
          (nablaSequence 3)
      control = controlIndex 11
  publication <-
    firstShow
      "checked publication"
      (mkCheckedPublication descriptor identifier value)
  batch <-
    firstShow
      "publication batch"
      (batchFor publication process definitionOccurrence control destinations)
  Right
    BaseFixture
      { descriptor,
        publication,
        batch,
        value,
        source,
        peer,
        process,
        definitionOccurrence,
        control,
        destinations,
        membership
      }

structuralFixture ::
  StructuralCarrierRole ->
  NonEmpty PublicationDestination ->
  Either String StructuralFixture
structuralFixture role destinations = do
  base <- baseFixture role destinations
  let predecessor = emptyStructuralVersionVector base.membership
      occurrence = structuralOccurrenceId base.source firstStructuralSequence
  structuralDigest <-
    firstShow
      "structural publication digest"
      ( structuralPublicationDigestFor
          base.descriptor
          base.definitionOccurrence
          base.publication
          base.batch
      )
  stamp <-
    firstShow
      "structural occurrence stamp"
      ( mkStructuralOccurrenceStamp
          occurrence
          predecessor
          (checkedPublicationId base.publication)
          structuralDigest
          role
      )
  draft <-
    firstShow
      "structural publication draft"
      ( mkStructuralPeerPublication
          base.descriptor
          base.definitionOccurrence
          base.publication
          stamp
          base.batch
      )
  Right
    StructuralFixture
      { descriptor = base.descriptor,
        publication = base.publication,
        batch = base.batch,
        value = base.value,
        source = base.source,
        peer = base.peer,
        process = base.process,
        definitionOccurrence = base.definitionOccurrence,
        control = base.control,
        destinations = base.destinations,
        membership = base.membership,
        occurrence,
        predecessor,
        structuralDigest,
        stamp,
        draft
      }

ordinaryFixture :: Either String BaseFixture
ordinaryFixture = do
  source <- identityEither "source" mkHeraldEpoch 0x10
  peer <- identityEither "peer" mkHeraldEpoch 0x11
  nabla <- identityEither "nabla" mkNablaId 0x12
  process <- identityEither "process" mkProcessEpochId 0x13
  definitionOccurrence <-
    identityEither "definition occurrence" mkSortDefinitionOccurrenceId 0x14
  let descriptor =
        predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole)
      carriedDescriptor =
        predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
      value = sortDefinitionValue carriedDescriptor
      identifier =
        publicationId nabla (fixtureAuthority source) source (nablaSequence 3)
      control = controlIndex 11
  publication <-
    firstShow
      "ordinary checked publication"
      (mkCheckedPublication descriptor identifier value)
  destinations <- defaultDestinationsEither
  membership <- fixtureMembership (source :| [peer])
  batch <-
    firstShow
      "ordinary batch"
      (batchFor publication process definitionOccurrence control destinations)
  Right
    BaseFixture
      { descriptor,
        publication,
        batch,
        value,
        source,
        peer,
        process,
        definitionOccurrence,
        control,
        destinations,
        membership
      }

checkedFor ::
  StructuralFixture ->
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  IO PeerPublication
checkedFor fixture occurrence predecessor = do
  stamp <-
    checked
      "variant stamp"
      ( mkStructuralOccurrenceStamp
          occurrence
          predecessor
          (checkedPublicationId fixture.publication)
          fixture.structuralDigest
          (structuralOccurrenceStampCarrierRole fixture.stamp)
      )
  checked
    "variant draft"
    ( mkStructuralPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        stamp
        fixture.batch
    )

digestForRawBatch ::
  StructuralFixture ->
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  Either String StructuralPublicationDigest
digestForRawBatch fixture publication process occurrence prerequisite destinations = do
  batch <-
    firstShow
      "variant batch"
      (batchFor publication process occurrence prerequisite destinations)
  firstShow
    "variant structural digest"
    ( structuralPublicationDigestFor
        fixture.descriptor
        occurrence
        publication
        batch
    )

batchFor ::
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  NonEmpty PublicationDestination ->
  Either PublicationBatchProblem PublicationBatch
batchFor publication process occurrence prerequisite =
  mkPublicationBatch
    (checkedPublicationId publication)
    process
    (checkedPublicationSort publication)
    occurrence
    (checkedPublicationCanonicalValue publication)
    Normal
    fixtureTopologyCut
    prerequisite

normalizedFixturePair :: Bool -> Either String (StructuralFixture, StructuralFixture)
normalizedFixturePair reversed = do
  first <- destinationEither 0x21 0x22 Normal
  second <- destinationEither 0x23 0x24 Weak
  let canonicalDestinations = first :| [second]
      suppliedDestinations =
        if reversed
          then second :| [first, second]
          else first :| [second, first]
  canonical <- structuralFixture NeutralVertexCarrier canonicalDestinations
  suppliedBase <- baseFixture NeutralVertexCarrier suppliedDestinations
  suppliedPredecessor <-
    firstShow
      "presented predecessor"
      ( vector
          suppliedBase.membership
          [ (suppliedBase.peer, emptyStructuralPrefix),
            (suppliedBase.source, emptyStructuralPrefix)
          ]
      )
  suppliedDigest <-
    firstShow
      "presented digest"
      ( structuralPublicationDigestFor
          suppliedBase.descriptor
          suppliedBase.definitionOccurrence
          suppliedBase.publication
          suppliedBase.batch
      )
  let suppliedOccurrence =
        structuralOccurrenceId suppliedBase.source firstStructuralSequence
  suppliedStamp <-
    firstShow
      "presented stamp"
      ( mkStructuralOccurrenceStamp
          suppliedOccurrence
          suppliedPredecessor
          (checkedPublicationId suppliedBase.publication)
          suppliedDigest
          NeutralVertexCarrier
      )
  suppliedDraft <-
    firstShow
      "presented draft"
      ( mkStructuralPeerPublication
          suppliedBase.descriptor
          suppliedBase.definitionOccurrence
          suppliedBase.publication
          suppliedStamp
          suppliedBase.batch
      )
  Right
    ( canonical,
      StructuralFixture
        { descriptor = suppliedBase.descriptor,
          publication = suppliedBase.publication,
          batch = suppliedBase.batch,
          value = suppliedBase.value,
          source = suppliedBase.source,
          peer = suppliedBase.peer,
          process = suppliedBase.process,
          definitionOccurrence = suppliedBase.definitionOccurrence,
          control = suppliedBase.control,
          destinations = suppliedBase.destinations,
          membership = suppliedBase.membership,
          occurrence = suppliedOccurrence,
          predecessor = suppliedPredecessor,
          structuralDigest = suppliedDigest,
          stamp = suppliedStamp,
          draft = suppliedDraft
        }
    )

assertOrdinaryRejectsStructural :: StructuralCarrierRole -> Assertion
assertOrdinaryRejectsStructural role = do
  fixture <- checked ("fixture for " <> show role) (baseFixture role defaultDestinations)
  assertProblem
    ("ordinary arm rejects " <> show role)
    (== OrdinaryStructuralCarrier role)
    ( mkOrdinaryPeerPublication
        fixture.descriptor
        fixture.definitionOccurrence
        fixture.publication
        fixture.batch
    )

valueForRoleWithObjectByte ::
  StructuralCarrierRole ->
  Word8 ->
  Either String Value
valueForRoleWithObjectByte role objectByte = do
  object <- identityEither "object" mkGlobalUniqueId objectByte
  endpointA <- identityEither "source endpoint" mkGlobalUniqueId 0x41
  endpointB <- identityEither "destination endpoint" mkGlobalUniqueId 0x42
  objectField <- field "object_id"
  labelField <- field "label"
  sortField <- field "sort_id"
  sequencingObjectField <- field "sequencing_object"
  sourceField <- field "source_vertex"
  destinationField <- field "destination_vertex"
  strengthField <- field "strength"
  processField <- field "process_id"
  residenceField <- field "residence"
  liveField <- field "live"
  firstShow
    "structural value"
    ( recordValue
        ( case role of
            NeutralVertexCarrier ->
              [ (objectField, globalUniqueIdValue object),
                (labelField, labelValue (VoidLabel, 0))
              ]
            EdgeCarrier ->
              [ (objectField, globalUniqueIdValue object),
                (labelField, labelValue (VoidLabel, 0)),
                (sourceField, globalUniqueIdValue endpointA),
                (destinationField, globalUniqueIdValue endpointB),
                (strengthField, enumValue (edgeStrengthSymbol Preserve))
              ]
            NablaCarrier ->
              [ (objectField, globalUniqueIdValue object),
                (labelField, labelValue (VoidLabel, 0)),
                (sequencingObjectField, optionalGlobalUniqueIdValue Nothing),
                (sortField, bytesValue ordinarySortBytes)
              ]
            DeltaCarrier ->
              [ (objectField, globalUniqueIdValue object),
                (labelField, labelValue (VoidLabel, 0)),
                (sortField, bytesValue ordinarySortBytes)
              ]
            ProcessEpochCarrier ->
              [ (objectField, globalUniqueIdValue object),
                (labelField, labelValue (VoidLabel, 0)),
                (processField, bytesValue (fixtureIdentifierBytes 0x43)),
                (residenceField, bytesValue (fixtureIdentifierBytes 0x44)),
                (liveField, boolValue True)
              ]
        )
    )
  where
    ordinarySortBytes =
      sortIdBytes
        (predefinedCatalogueSortId (profileEntryFor SortDefinitionRole))

predefinedRoleForCarrier :: StructuralCarrierRole -> PredefinedSortRole
predefinedRoleForCarrier role = case role of
  NeutralVertexCarrier -> NeutralVertexRole
  EdgeCarrier -> EdgeRole
  NablaCarrier -> NablaRole
  DeltaCarrier -> DeltaRole
  ProcessEpochCarrier -> ProcessEpochRole

publishedStructuralCarrierRoles :: [StructuralCarrierRole]
publishedStructuralCarrierRoles =
  [NeutralVertexCarrier, EdgeCarrier, NablaCarrier, DeltaCarrier]

fixtureTopologyCut :: TopologyCutId
fixtureTopologyCut =
  either
    (error . ("invalid fixture topology cut: " <>) . show)
    id
    (mkTopologyCutId (fixtureIdentifierBytes 0x16))

fixtureAuthority :: HeraldEpoch -> AuthorityEpoch
fixtureAuthority source =
  structuralAuthorityEpoch
    (structuralOccurrenceId source firstStructuralSequence)
    fixtureTopologyCut

alternateAuthority :: HeraldEpoch -> AuthorityEpoch
alternateAuthority source =
  structuralAuthorityEpoch
    (structuralOccurrenceId source (nextStructuralSequence firstStructuralSequence))
    fixtureTopologyCut

defaultDestinations :: NonEmpty PublicationDestination
defaultDestinations =
  either (error . ("invalid default destinations: " <>)) id defaultDestinationsEither

defaultDestinationsEither :: Either String (NonEmpty PublicationDestination)
defaultDestinationsEither = do
  first <- destinationEither 0x21 0x22 Normal
  second <- destinationEither 0x23 0x24 Weak
  Right (first :| [second])

destination :: Word8 -> Word8 -> ReplicaStrength -> IO PublicationDestination
destination deltaByte incarnationByte strength =
  checked "destination" (destinationEither deltaByte incarnationByte strength)

destinationEither ::
  Word8 ->
  Word8 ->
  ReplicaStrength ->
  Either String PublicationDestination
destinationEither deltaByte incarnationByte strength = do
  delta <- identityEither "delta" mkDeltaId deltaByte
  incarnation <- identityEither "store incarnation" mkStoreIncarnationId incarnationByte
  Right (publicationDestination delta incarnation strength)

vector ::
  HeraldMembershipGeneration ->
  [(HeraldEpoch, StructuralPrefix)] ->
  Either String StructuralVersionVector
vector membership = firstShow "structural vector" . mkStructuralVersionVector membership

fixtureMembership ::
  NonEmpty HeraldEpoch ->
  Either String HeraldMembershipGeneration
fixtureMembership =
  firstShow "Herald membership"
    . genesisHeraldMembershipGeneration (checkedSystemId fixtureCheckedGenesis)

field :: Text -> Either String FieldName
field = firstShow "field" . mkFieldName

identity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem value) ->
  Word8 ->
  IO value
identity context constructor byte =
  checked context (constructor (fixtureIdentifierBytes byte))

identityEither ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem value) ->
  Word8 ->
  Either String value
identityEither context constructor byte =
  firstShow context (constructor (fixtureIdentifierBytes byte))

firstShow :: (Show problem) => String -> Either problem value -> Either String value
firstShow context = Bifunctor.first (((context <> ": ") <>) . show)

checked :: (Show problem) => String -> Either problem value -> IO value
checked context =
  either
    (assertFailure . ((context <> ": ") <>) . show)
    pure

assertNotEqual :: (Eq value, Show value) => String -> value -> value -> Assertion
assertNotEqual context left right =
  assertBool
    (context <> ": both were " <> show left)
    (left /= right)

assertProblem ::
  String ->
  (PeerPublicationProblem -> Bool) ->
  Either PeerPublicationProblem value ->
  Assertion
assertProblem context matches result = case result of
  Left problem ->
    assertBool
      (context <> ": unexpected problem " <> show problem)
      (matches problem)
  Right _ -> assertFailure (context <> ": construction unexpectedly succeeded")

assertStampProblem ::
  String ->
  (StructuralOccurrenceStampProblem -> Bool) ->
  Either StructuralOccurrenceStampProblem value ->
  Assertion
assertStampProblem context matches result = case result of
  Left problem ->
    assertBool
      (context <> ": unexpected problem " <> show problem)
      (matches problem)
  Right _ -> assertFailure (context <> ": construction unexpectedly succeeded")
