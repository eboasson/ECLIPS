{-# LANGUAGE OverloadedStrings #-}

module TopologyProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    SystemId,
    TopologyCutId,
    controlIndex,
    firstStructuralSequence,
    mkHeraldEpoch,
    mkStructuralSequence,
    mkSystemId,
    topologyCutIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    mkInitialProjectionDigest,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
    structuralVersionVectorEntries,
  )
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.Topology
  ( MemberSetDigest,
    TerminalSourceUnionDigest,
    TopologyPredecessor,
    TopologyPredecessorView (..),
    TopologyShapeProblem (..),
    decodeTopologyCutCanonicalBytes,
    deriveGenesisTopologyCutId,
    deriveMemberSetDigest,
    deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    memberSetDigestBytes,
    membershipSuccessorPredecessor,
    mkMemberSetDigest,
    mkTerminalSourceUnionDigest,
    mkTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyCutCanonicalBytes,
    topologyCutFrontier,
    topologyCutOccurrenceDigest,
    topologyCutPredecessor,
    topologyFrontier,
    topologyFrontierAppliedControlPrefix,
    topologyFrontierMemberSetDigest,
    topologyFrontierMembershipGenerationId,
    topologyFrontierStructuralVersionVector,
    topologyOccurrenceDigestBytes,
    topologyPredecessorFromClaimedCoordinates,
    topologyPredecessorView,
  )
import Eclips.Domain.Topology qualified as Topology
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "topology identity"
    [ testProperty "topology digest wrappers require exactly 32 bytes" propDigestWidths,
      testCase "member-set presentation is canonical" caseMemberPresentation,
      testCase "frontier and cut products retain exact nominal components" caseProducts,
      testCase "membership successor retains exact lineage and projection" caseMembershipSuccessor,
      testProperty "admission recipe activates at the committed coordinate with an empty newcomer component" propAdmissionRecipe,
      testCase "claimed successor checks coordinates while deferring lineage resolution" caseClaimedSuccessor,
      testCase "membership successor rejects a non-exact projection" caseSuccessorProjection,
      testCase "successor cut validates vectors while retirement control remains owner-checked" caseSuccessorFrontier,
      testProperty "descendant vector projection composes without renumbering" propCompoundProjection,
      testCase "successive and compound predecessor cuts use exact checked lineage" caseCompoundLineage,
      testCase "topology canonical decoder round-trips and rejects mutated shape" caseCanonicalTopology,
      testCase "genesis identity rejects a successor membership" caseGenesisMembership,
      testCase "occurrence, dynamic-cut, and genesis transcripts are domain-separated" caseDomainSeparation
    ]

propDigestWidths :: Property
propDigestWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let bytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (mkMemberSetDigest bytes) === malformed,
              isLeft (mkTopologyOccurrenceDigest bytes) === malformed,
              isLeft (mkTerminalSourceUnionDigest bytes) === malformed
            ]

propAdmissionRecipe :: Word16 -> Property
propAdmissionRecipe offset =
  let activation = controlIndex (fromIntegral offset + 20)
      admission = checked "admission" (Membership.deriveHeraldAdmissionId (controlIndex 2))
      applicant = checked "applicant" (mkHeraldEpoch (ByteString.replicate 32 0x77))
      oldCut = checked "sealed old cut" (topologyCut (sameGenerationPredecessor genesisRoot) (topologyFrontier terminalVector (controlIndex 10)) (deriveTopologyOccurrenceDigest "sealed predecessor projection"))
      contribution = deriveTopologyOccurrenceDigest "closed newcomer system-view contribution"
      recipe = checked "join recipe" (Topology.heraldJoinBaseRecipe admission applicant genesisMembership oldCut contribution)
      occurrence = deriveTopologyOccurrenceDigest "activation projection with admission provenance"
      (joined, cut) = checked "activate recipe" (Topology.activateHeraldJoinBase activation occurrence recipe)
      history = checked "admission history" (Membership.heraldMembershipHistory (genesisMembership :| [joined]))
      lineage = checked "admission lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId genesisMembership) (heraldMembershipGenerationId joined) history)
      initial = topologyFrontierStructuralVersionVector (topologyCutFrontier cut)
      wrongEntries = (applicant, structuralPrefixThrough firstStructuralSequence) : structuralVersionVectorEntries terminalVector
      wrongInitial = checked "shape-valid advanced newcomer" (mkStructuralVersionVector joined wrongEntries)
   in conjoin
        [ Topology.decodeHeraldJoinBaseRecipeCanonicalBytes (Topology.heraldJoinBaseRecipeCanonicalBytes recipe) === Right recipe,
          decodeTopologyCutCanonicalBytes (topologyCutCanonicalBytes cut) === Right cut,
          topologyFrontierAppliedControlPrefix (topologyCutFrontier cut) === activation,
          Membership.heraldMembershipGenerationChangeControlIndex joined === Just activation,
          Structural.projectStructuralVersionVector lineage terminalVector === Right initial,
          Structural.structuralVersionVectorComponent applicant initial === Just emptyStructuralPrefix,
          conjoin [Structural.structuralVersionVectorComponent herald initial === Just prefix | (herald, prefix) <- structuralVersionVectorEntries terminalVector],
          topologyPredecessorView (topologyCutPredecessor cut) === AdmissionTopologyPredecessorView (deriveTopologyCutId oldCut) (heraldMembershipGenerationId genesisMembership) (heraldMembershipGenerationId joined) terminalVector initial admission (Topology.heraldJoinBaseRecipeDigest recipe),
          counterexample "admission rejects an advanced newcomer claim" (isLeft (Topology.admissionTopologyPredecessorFromClaimedCoordinates (deriveTopologyCutId oldCut) (heraldMembershipGenerationId genesisMembership) (heraldMembershipGenerationId joined) terminalVector wrongInitial admission (Topology.heraldJoinBaseRecipeDigest recipe))),
          Topology.activateHeraldJoinBase (controlIndex 10) occurrence recipe === Left (Topology.TopologyAdmissionActivationNotAfterCut (controlIndex 10) (controlIndex 10)),
          counterexample "recipe digest binds contribution" (Topology.heraldJoinBaseRecipeDigest recipe /= Topology.heraldJoinBaseRecipeDigest (checked "other contribution" (Topology.heraldJoinBaseRecipe admission applicant genesisMembership oldCut (deriveTopologyOccurrenceDigest "other contribution"))))
        ]

caseMemberPresentation :: IO ()
caseMemberPresentation = do
  assertEqual
    "order and exact duplicates do not change the low-level set digest"
    (deriveMemberSetDigest (heraldA :| [heraldB]))
    (deriveMemberSetDigest (heraldB :| [heraldA, heraldA]))
  assertEqual
    "digest width"
    32
    (ByteString.length (memberSetDigestBytes memberSet))

caseProducts :: IO ()
caseProducts = do
  let frontier = topologyFrontier emptyVector prerequisite
      occurrenceDigest = deriveTopologyOccurrenceDigest "canonical projection"
      predecessor = sameGenerationPredecessor genesisRoot
      cut = checked "topology cut" (topologyCut predecessor frontier occurrenceDigest)
  assertEqual
    "frontier generation"
    (heraldMembershipGenerationId genesisMembership)
    (topologyFrontierMembershipGenerationId frontier)
  assertEqual "frontier member set" memberSet (topologyFrontierMemberSetDigest frontier)
  assertEqual "frontier vector" emptyVector (topologyFrontierStructuralVersionVector frontier)
  assertEqual "frontier control" prerequisite (topologyFrontierAppliedControlPrefix frontier)
  assertEqual "cut predecessor" predecessor (topologyCutPredecessor cut)
  assertEqual
    "same-generation predecessor view"
    (SameGenerationPredecessorView genesisRoot)
    (topologyPredecessorView (topologyCutPredecessor cut))
  assertEqual "cut frontier" frontier (topologyCutFrontier cut)
  assertEqual "cut occurrence digest" occurrenceDigest (topologyCutOccurrenceDigest cut)
  assertEqual "derived cut width" 32 (ByteString.length (topologyCutIdBytes (deriveTopologyCutId cut)))

caseMembershipSuccessor :: IO ()
caseMembershipSuccessor = do
  assertEqual
    "the successor predecessor view retains every normative field"
    ( MembershipSuccessorPredecessorView
        genesisRoot
        (heraldMembershipGenerationId genesisMembership)
        (heraldMembershipGenerationId successorMembership)
        terminalVector
        successorInitialVector
        terminalUnionDigest
    )
    (topologyPredecessorView successorPredecessor)
  let frontier = topologyFrontier successorInitialVector retirementIndex
      cut =
        checked
          "membership-successor topology cut"
          ( topologyCut
              successorPredecessor
              frontier
              (deriveTopologyOccurrenceDigest "successor projection")
          )
  assertEqual "the admitted successor frontier is retained" frontier (topologyCutFrontier cut)

caseClaimedSuccessor :: IO ()
caseClaimedSuccessor = do
  let unrelatedSuccessor =
        checked
          "unrelated one-member generation"
          (genesisHeraldMembershipGeneration otherSystem (heraldA :| []))
      unrelatedInitialVector =
        checked
          "unrelated initial vector"
          ( mkStructuralVersionVector
              unrelatedSuccessor
              [(heraldA, structuralPrefixThrough firstStructuralSequence)]
          )
      claimed =
        checked
          "claimed membership-successor predecessor"
          ( topologyPredecessorFromClaimedCoordinates
              genesisRoot
              (heraldMembershipGenerationId genesisMembership)
              (heraldMembershipGenerationId unrelatedSuccessor)
              terminalVector
              unrelatedInitialVector
              terminalUnionDigest
          )
  assertEqual
    "the boundary retains claims whose lineage the owner must still resolve"
    ( MembershipSuccessorPredecessorView
        genesisRoot
        (heraldMembershipGenerationId genesisMembership)
        (heraldMembershipGenerationId unrelatedSuccessor)
        terminalVector
        unrelatedInitialVector
        terminalUnionDigest
    )
    (topologyPredecessorView claimed)
  assertEqual
    "the boundary still rejects a rebased surviving component"
    ( Left
        ( TopologyClaimedSuccessorProjectionNotProperContraction
            (structuralVersionVectorEntries terminalVector)
            (structuralVersionVectorEntries lowerSuccessorVector)
        )
    )
    ( topologyPredecessorFromClaimedCoordinates
        genesisRoot
        (heraldMembershipGenerationId genesisMembership)
        (heraldMembershipGenerationId successorMembership)
        terminalVector
        lowerSuccessorVector
        terminalUnionDigest
    )

caseSuccessorProjection :: IO ()
caseSuccessorProjection = do
  let wrongProjection =
        checked
          "wrong successor projection"
          ( mkStructuralVersionVector
              successorMembership
              [(heraldA, emptyStructuralPrefix)]
          )
  assertEqual
    "survivor components must be copied without rebasing"
    ( Left
        ( TopologySuccessorVectorProjectionMismatch
            [(heraldA, structuralPrefixThrough firstStructuralSequence)]
            [(heraldA, emptyStructuralPrefix)]
        )
    )
    ( membershipSuccessorPredecessor
        successorLineage
        genesisRoot
        terminalVector
        wrongProjection
        terminalUnionDigest
    )

caseSuccessorFrontier :: IO ()
caseSuccessorFrontier = do
  assertEqual
    "a predecessor-generation frontier cannot establish the successor cut"
    ( Left
        ( TopologySuccessorFrontierGenerationMismatch
            (heraldMembershipGenerationId successorMembership)
            (heraldMembershipGenerationId genesisMembership)
        )
    )
    ( topologyCut
        successorPredecessor
        (topologyFrontier terminalVector retirementIndex)
        (deriveTopologyOccurrenceDigest "wrong generation")
    )
  assertEqual
    "a regressed successor vector does not cover its initial vector"
    (Left TopologySuccessorFrontierDoesNotCoverInitialVector)
    ( topologyCut
        successorPredecessor
        (topologyFrontier lowerSuccessorVector retirementIndex)
        (deriveTopologyOccurrenceDigest "regressed vector")
    )
  assertBool
    "retirement-control admission remains with the owner holding checked history"
    ( isRight
        ( topologyCut
            successorPredecessor
            (topologyFrontier successorInitialVector (controlIndex 8))
            (deriveTopologyOccurrenceDigest "owner-checked control")
        )
    )

caseGenesisMembership :: IO ()
caseGenesisMembership =
  assertEqual
    "a successor generation cannot be used as the distinguished genesis root"
    ( Left
        ( TopologyGenesisRequiresGenesisMembership
            (heraldMembershipGenerationId successorMembership)
        )
    )
    (deriveGenesisTopologyCutId system successorMembership initialProjection)

caseDomainSeparation :: IO ()
caseDomainSeparation = do
  let occurrenceDigest = deriveTopologyOccurrenceDigest "same caller bytes"
      dynamic =
        deriveTopologyCutId
          ( checked
              "dynamic topology cut"
              ( topologyCut
                  (sameGenerationPredecessor genesisRoot)
                  (topologyFrontier emptyVector prerequisite)
                  occurrenceDigest
              )
          )
  assertBool
    "the three digest domains are nominally and bytewise distinct"
    ( memberSetDigestBytes memberSet /= topologyOccurrenceDigestBytes occurrenceDigest
        && topologyCutIdBytes genesisRoot /= topologyCutIdBytes dynamic
    )

compoundHistory :: Membership.HeraldMembershipHistory
compoundHistory = checked "compound history" (Membership.heraldMembershipHistory (first :| [second, third]))
  where
    first = checked "compound genesis" (genesisHeraldMembershipGeneration system (heraldA :| [heraldB, heraldC]))
    second = checked "first retirement" (retireHeraldMembershipGeneration retirementIndex (retirementResolution 8) heraldB first)
    third = checked "second retirement" (retireHeraldMembershipGeneration (controlIndex 12) (retirementResolution 11) heraldC second)

compoundLineages :: (Membership.HeraldMembershipLineage, Membership.HeraldMembershipLineage, Membership.HeraldMembershipLineage)
compoundLineages = (between first second, between second third, between first third)
  where
    (first, second, third) = case Membership.heraldMembershipHistoryGenerations compoundHistory of
      a :| [b, c] -> (a, b, c)
      _ -> error "compound history must have three generations"
    between origin target = checked "compound lineage interval" (Membership.heraldMembershipLineage (heraldMembershipGenerationId origin) (heraldMembershipGenerationId target) compoundHistory)

propCompoundProjection :: Word16 -> Word16 -> Word16 -> Property
propCompoundProjection a b c =
  let (firstStep, secondStep, compound) = compoundLineages
      original = checked "generated original vector" (mkStructuralVersionVector (Membership.heraldMembershipLineageOrigin compound) [(heraldA, prefix a), (heraldB, prefix b), (heraldC, prefix c)])
      intermediate = checked "first projection" (Structural.projectStructuralVersionVector firstStep original)
      final = checked "compound projection" (Structural.projectStructuralVersionVector compound original)
   in conjoin
        [ Structural.projectStructuralVersionVector secondStep intermediate === Right final,
          structuralVersionVectorEntries final === [(heraldA, prefix a)],
          Structural.structuralVersionVectorCovers final original === False,
          Structural.structuralVersionVectorCovers original final === False,
          counterexample "a descendant vector cannot be treated as the origin" (isLeft (Structural.projectStructuralVersionVector compound final))
        ]
  where
    prefix value = structuralPrefixThrough (checked "generated positive prefix" (mkStructuralSequence (fromIntegral value + 1)))

caseCompoundLineage :: IO ()
caseCompoundLineage = do
  let (firstStep, secondStep, compound) = compoundLineages
      original = emptyStructuralVersionVector (Membership.heraldMembershipLineageOrigin compound)
      intermediate = checked "intermediate projection" (Structural.projectStructuralVersionVector firstStep original)
      final = checked "final projection" (Structural.projectStructuralVersionVector compound original)
      firstPredecessor = checked "first successor predecessor" (membershipSuccessorPredecessor firstStep genesisRoot original intermediate terminalUnionDigest)
      firstCut = checked "first established successor" (topologyCut firstPredecessor (topologyFrontier intermediate retirementIndex) (deriveTopologyOccurrenceDigest "first retirement"))
      secondPredecessor = membershipSuccessorPredecessor secondStep (deriveTopologyCutId firstCut) intermediate final terminalUnionDigest
      combined = membershipSuccessorPredecessor compound genesisRoot original final terminalUnionDigest
      target = Membership.heraldMembershipLineageTarget compound
      reflexive = checked "reflexive lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId target) (heraldMembershipGenerationId target) compoundHistory)
  assertBool "an established non-genesis ancestor supports the next retirement" (isRight secondPredecessor)
  assertBool "an unfinished intermediate base can be skipped only through the checked chain" (isRight combined)
  assertEqual "all retired origins are retained in order" [heraldB, heraldC] (Membership.heraldMembershipLineageRetiredHeraldEpochs compound)
  assertEqual "reflexive projection is exact" (Right final) (Structural.projectStructuralVersionVector reflexive final)
  assertEqual "a topology membership transition must contract" (Left (TopologyMembershipLineageNotContraction (heraldMembershipGenerationId target))) (membershipSuccessorPredecessor reflexive genesisRoot final final terminalUnionDigest)

caseCanonicalTopology :: IO ()
caseCanonicalTopology = do
  let (_, _, lineage) = compoundLineages
      original = emptyStructuralVersionVector (Membership.heraldMembershipLineageOrigin lineage)
      projected = checked "codec compound projection" (Structural.projectStructuralVersionVector lineage original)
      compound = checked "codec compound predecessor" (membershipSuccessorPredecessor lineage genesisRoot original projected terminalUnionDigest)
      cuts =
        [ checked "same-generation codec cut" (topologyCut (sameGenerationPredecessor genesisRoot) (topologyFrontier emptyVector prerequisite) digest),
          checked "successor codec cut" (topologyCut successorPredecessor (topologyFrontier successorInitialVector retirementIndex) digest),
          checked "compound codec cut" (topologyCut compound (topologyFrontier projected (controlIndex 12)) digest)
        ]
      digest = deriveTopologyOccurrenceDigest "canonical round-trip"
  mapM_
    ( \cut -> do
        let bytes = topologyCutCanonicalBytes cut
            tagOffset = 8 + ByteString.length "ECLIPS-TOPOLOGY-CUT"
            unknownTag = ByteString.take tagOffset bytes <> ByteString.singleton 2 <> ByteString.drop (tagOffset + 1) bytes
        assertEqual "exact canonical inverse" (Right cut) (decodeTopologyCutCanonicalBytes bytes)
        assertBool "trailing bytes reject" (isLeft (decodeTopologyCutCanonicalBytes (bytes <> "\NUL")))
        assertBool "unknown predecessor tags reject" (isLeft (decodeTopologyCutCanonicalBytes unknownTag))
    )
    cuts

genesisMembership :: HeraldMembershipGeneration
genesisMembership =
  checked
    "genesis membership"
    (genesisHeraldMembershipGeneration system fixedMembership)

successorMembership :: HeraldMembershipGeneration
successorMembership =
  checked
    "successor membership"
    (retireHeraldMembershipGeneration retirementIndex (retirementResolution 8) heraldB genesisMembership)

successorLineage :: Membership.HeraldMembershipLineage
successorLineage = checked "successor lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId genesisMembership) (heraldMembershipGenerationId successorMembership) (checked "successor history" (Membership.heraldMembershipHistory (genesisMembership :| [successorMembership]))))

retirementResolution :: Word -> Membership.FailureProbeResolutionId
retirementResolution index = Membership.deriveFailureProbeResolutionId (checked "retirement resolution" (Membership.deriveHeraldFailureProbeId (controlIndex (fromIntegral index)))) Membership.RetireFailureProbeTarget

memberSet :: MemberSetDigest
memberSet = heraldMembershipGenerationActiveMemberSetDigest genesisMembership

emptyVector :: StructuralVersionVector
emptyVector = emptyStructuralVersionVector genesisMembership

terminalVector :: StructuralVersionVector
terminalVector =
  checked
    "terminal predecessor vector"
    ( mkStructuralVersionVector
        genesisMembership
        [ (heraldA, structuralPrefixThrough firstStructuralSequence),
          (heraldB, emptyStructuralPrefix)
        ]
    )

successorInitialVector :: StructuralVersionVector
successorInitialVector =
  checked
    "successor initial vector"
    ( mkStructuralVersionVector
        successorMembership
        [(heraldA, structuralPrefixThrough firstStructuralSequence)]
    )

lowerSuccessorVector :: StructuralVersionVector
lowerSuccessorVector = emptyStructuralVersionVector successorMembership

terminalUnionDigest :: TerminalSourceUnionDigest
terminalUnionDigest =
  checked
    "terminal-source union digest"
    (mkTerminalSourceUnionDigest (ByteString.replicate 32 0x55))

successorPredecessor :: TopologyPredecessor
successorPredecessor =
  checked
    "membership-successor predecessor"
    ( membershipSuccessorPredecessor
        successorLineage
        genesisRoot
        terminalVector
        successorInitialVector
        terminalUnionDigest
    )

fixedMembership :: NonEmpty HeraldEpoch
fixedMembership = heraldA :| [heraldB]

heraldA, heraldB, heraldC :: HeraldEpoch
heraldA = checked "herald A" (mkHeraldEpoch (ByteString.replicate 32 0x11))
heraldB = checked "herald B" (mkHeraldEpoch (ByteString.replicate 32 0x22))
heraldC = checked "herald C" (mkHeraldEpoch (ByteString.replicate 32 0x23))

system :: SystemId
system = checked "system" (mkSystemId (ByteString.replicate 32 0x33))

otherSystem :: SystemId
otherSystem = checked "other system" (mkSystemId (ByteString.replicate 32 0x34))

initialProjection :: InitialProjectionDigest
initialProjection =
  checked "initial projection" (mkInitialProjectionDigest (ByteString.replicate 32 0x44))

genesisRoot :: TopologyCutId
genesisRoot =
  checked
    "genesis topology root"
    (deriveGenesisTopologyCutId system genesisMembership initialProjection)

prerequisite :: ControlIndex
prerequisite = controlIndex 7

retirementIndex :: ControlIndex
retirementIndex = controlIndex 9

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
