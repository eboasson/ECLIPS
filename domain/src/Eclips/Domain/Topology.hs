{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical low-level identities and products for structural stabilization.
--
-- This module owns only checked digest shapes and canonical transcripts.  It
-- does not decide which structural winners or control facts constitute a
-- topology occurrence projection; the Herald graph owner supplies that
-- already-normalized semantic transcript.
module Eclips.Domain.Topology
  ( TopologyDigestError (..),
    MemberSetDigest,
    mkMemberSetDigest,
    memberSetDigestBytes,
    deriveMemberSetDigest,
    TopologyOccurrenceDigest,
    mkTopologyOccurrenceDigest,
    topologyOccurrenceDigestBytes,
    deriveTopologyOccurrenceDigest,
    TerminalSourceUnionDigest,
    mkTerminalSourceUnionDigest,
    terminalSourceUnionDigestBytes,
    HeraldJoinBaseRecipe,
    HeraldJoinBaseRecipeDigest,
    mkHeraldJoinBaseRecipeDigest,
    heraldJoinBaseRecipe,
    heraldJoinBaseRecipeAdmissionId,
    heraldJoinBaseRecipeApplicant,
    heraldJoinBaseRecipeGeneration,
    heraldJoinBaseRecipeEstablishedCut,
    heraldJoinBaseRecipeContributionDigest,
    heraldJoinBaseRecipeCanonicalBytes,
    heraldJoinBaseRecipeDigest,
    heraldJoinBaseRecipeDigestBytes,
    heraldJoinBaseRecipeDigestValueBytes,
    HeraldJoinBaseRecipeCanonicalProblem (..),
    decodeHeraldJoinBaseRecipeCanonicalBytes,
    activateHeraldJoinBase,
    TopologyShapeProblem (..),
    TopologyPredecessor,
    TopologyPredecessorView (..),
    sameGenerationPredecessor,
    membershipSuccessorPredecessor,
    topologyPredecessorFromClaimedCoordinates,
    admissionTopologyPredecessorFromClaimedCoordinates,
    topologyPredecessorView,
    topologyPredecessorCutId,
    TopologyFrontier,
    topologyFrontier,
    topologyFrontierMembershipGenerationId,
    topologyFrontierMemberSetDigest,
    topologyFrontierStructuralVersionVector,
    topologyFrontierAppliedControlPrefix,
    TopologyCut,
    topologyCut,
    topologyCutPredecessor,
    topologyCutFrontier,
    topologyCutOccurrenceDigest,
    topologyCutCanonicalBytes,
    TopologyCutCanonicalProblem (..),
    decodeTopologyCutCanonicalBytes,
    deriveTopologyCutId,
    deriveGenesisTopologyCutId,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    IdentityError,
    SystemId,
    TopologyCutId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkStructuralSequence,
    mkTopologyCutId,
    structuralSequenceWord64,
    systemIdBytes,
    topologyCutIdBytes,
  )
import Eclips.Domain.MemberSet
  ( MemberSetDigest,
    TopologyDigestError (..),
    deriveMemberSetDigest,
    memberSetDigestBytes,
    mkMemberSetDigest,
  )
import Eclips.Domain.Membership
  ( HeraldAdmissionId,
    HeraldAdmissionIdCanonicalProblem,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    MembershipGenerationCanonicalProblem,
    MembershipGenerationProblem,
    MembershipIdentityError,
    admitHeraldMembershipGeneration,
    decodeHeraldAdmissionIdCanonicalBytes,
    decodeHeraldMembershipGenerationCanonicalBytes,
    heraldAdmissionControlIndex,
    heraldAdmissionIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageRetiredHeraldEpochs,
    heraldMembershipLineageTarget,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    initialProjectionDigestBytes,
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVectorProblem,
    StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorCovers,
    structuralVersionVectorEntries,
    structuralVersionVectorFromClaimedCoordinates,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

sha256ByteCount :: Int
sha256ByteCount = 32

newtype TopologyOccurrenceDigest = TopologyOccurrenceDigest ByteString
  deriving stock (Eq, Ord)

instance Show TopologyOccurrenceDigest where
  show = renderGroupedHex . topologyOccurrenceDigestBytes

mkTopologyOccurrenceDigest ::
  ByteString -> Either TopologyDigestError TopologyOccurrenceDigest
mkTopologyOccurrenceDigest = fmap TopologyOccurrenceDigest . checkDigestBytes

topologyOccurrenceDigestBytes :: TopologyOccurrenceDigest -> ByteString
topologyOccurrenceDigestBytes (TopologyOccurrenceDigest bytes) = bytes

-- | Hash one Herald-owner-supplied canonical topology projection transcript.
--
-- The caller owns normalization and the semantic promise that the bytes cover
-- the exact structural winner/control projection at its named frontier.  This
-- low module adds the stable domain separator and digest shape only.
deriveTopologyOccurrenceDigest :: ByteString -> TopologyOccurrenceDigest
deriveTopologyOccurrenceDigest canonicalProjection =
  digestInvariant
    "topology-occurrence digest"
    mkTopologyOccurrenceDigest
    ( hashTranscript
        ( TopologyOccurrenceDigestTranscript
            "ECLIPS-TOPOLOGY-OCCURRENCE"
            canonicalProjection
        )
    )

-- | Nominal digest of one canonical terminal-source union. The union algebra
-- lives above Domain; Topology owns this checked identity because the first
-- successor predecessor embeds it directly.
newtype TerminalSourceUnionDigest = TerminalSourceUnionDigest ByteString
  deriving stock (Eq, Ord)

instance Show TerminalSourceUnionDigest where
  show = renderGroupedHex . terminalSourceUnionDigestBytes

mkTerminalSourceUnionDigest ::
  ByteString -> Either TopologyDigestError TerminalSourceUnionDigest
mkTerminalSourceUnionDigest = fmap TerminalSourceUnionDigest . checkDigestBytes

terminalSourceUnionDigestBytes :: TerminalSourceUnionDigest -> ByteString
terminalSourceUnionDigestBytes (TerminalSourceUnionDigest bytes) = bytes

-- | Readiness names a recipe independent of the future activation coordinate.
-- The old cut already commits the complete old topology; the contribution
-- digest names the exact closed newcomer system-view manifest checked by its
-- semantic owner. No graph or store payload is held in this low domain value.
data HeraldJoinBaseRecipe = HeraldJoinBaseRecipe HeraldAdmissionId HeraldEpoch HeraldMembershipGeneration TopologyCut TopologyOccurrenceDigest
  deriving stock (Eq, Show)

newtype HeraldJoinBaseRecipeDigest = HeraldJoinBaseRecipeDigest ByteString
  deriving stock (Eq, Ord)

instance Show HeraldJoinBaseRecipeDigest where
  show = renderGroupedHex . heraldJoinBaseRecipeDigestValueBytes

mkHeraldJoinBaseRecipeDigest :: ByteString -> Either TopologyDigestError HeraldJoinBaseRecipeDigest
mkHeraldJoinBaseRecipeDigest = fmap HeraldJoinBaseRecipeDigest . checkDigestBytes

heraldJoinBaseRecipeDigestValueBytes :: HeraldJoinBaseRecipeDigest -> ByteString
heraldJoinBaseRecipeDigestValueBytes (HeraldJoinBaseRecipeDigest bytes) = bytes

heraldJoinBaseRecipe :: HeraldAdmissionId -> HeraldEpoch -> HeraldMembershipGeneration -> TopologyCut -> TopologyOccurrenceDigest -> Either TopologyShapeProblem HeraldJoinBaseRecipe
heraldJoinBaseRecipe admission applicant generation established contribution = do
  requireVectorCoordinate TopologyTerminalVectorGenerationMismatch TopologyTerminalVectorMemberSetMismatch generation (topologyFrontierStructuralVersionVector (topologyCutFrontier established))
  if applicant `elem` heraldMembershipGenerationActiveHeraldEpochs generation then Left (TopologyAdmissionApplicantAlreadyActive applicant) else Right ()
  let index = topologyFrontierAppliedControlPrefix (topologyCutFrontier established)
  if index < heraldAdmissionControlIndex admission then Left (TopologyAdmissionCutBeforeBegin (heraldAdmissionControlIndex admission) index) else Right ()
  Right (HeraldJoinBaseRecipe admission applicant generation established contribution)

heraldJoinBaseRecipeAdmissionId :: HeraldJoinBaseRecipe -> HeraldAdmissionId
heraldJoinBaseRecipeAdmissionId (HeraldJoinBaseRecipe admission _ _ _ _) = admission

heraldJoinBaseRecipeApplicant :: HeraldJoinBaseRecipe -> HeraldEpoch
heraldJoinBaseRecipeApplicant (HeraldJoinBaseRecipe _ applicant _ _ _) = applicant

heraldJoinBaseRecipeGeneration :: HeraldJoinBaseRecipe -> HeraldMembershipGeneration
heraldJoinBaseRecipeGeneration (HeraldJoinBaseRecipe _ _ generation _ _) = generation

heraldJoinBaseRecipeEstablishedCut :: HeraldJoinBaseRecipe -> TopologyCut
heraldJoinBaseRecipeEstablishedCut (HeraldJoinBaseRecipe _ _ _ established _) = established

heraldJoinBaseRecipeContributionDigest :: HeraldJoinBaseRecipe -> TopologyOccurrenceDigest
heraldJoinBaseRecipeContributionDigest (HeraldJoinBaseRecipe _ _ _ _ contribution) = contribution

heraldJoinBaseRecipeCanonicalBytes :: HeraldJoinBaseRecipe -> ByteString
heraldJoinBaseRecipeCanonicalBytes (HeraldJoinBaseRecipe admission applicant generation established contribution) =
  Serialize.encode (HeraldJoinBaseRecipeTranscript "ECLIPS-HERALD-JOIN-BASE-RECIPE" (heraldAdmissionIdCanonicalBytes admission) (heraldEpochBytes applicant) (heraldMembershipGenerationCanonicalBytes generation) (topologyCutCanonicalBytes established) (topologyOccurrenceDigestBytes contribution))

heraldJoinBaseRecipeDigest :: HeraldJoinBaseRecipe -> HeraldJoinBaseRecipeDigest
heraldJoinBaseRecipeDigest = HeraldJoinBaseRecipeDigest . SHA256.hash . heraldJoinBaseRecipeCanonicalBytes

heraldJoinBaseRecipeDigestBytes :: HeraldJoinBaseRecipe -> ByteString
heraldJoinBaseRecipeDigestBytes = heraldJoinBaseRecipeDigestValueBytes . heraldJoinBaseRecipeDigest

data HeraldJoinBaseRecipeCanonicalProblem
  = HeraldJoinBaseRecipeCanonicalDecodeFailed String
  | HeraldJoinBaseRecipeCanonicalWrongDomain ByteString
  | HeraldJoinBaseRecipeCanonicalAdmissionProblem HeraldAdmissionIdCanonicalProblem
  | HeraldJoinBaseRecipeCanonicalIdentityProblem IdentityError
  | HeraldJoinBaseRecipeCanonicalGenerationProblem MembershipGenerationCanonicalProblem
  | HeraldJoinBaseRecipeCanonicalCutProblem TopologyCutCanonicalProblem
  | HeraldJoinBaseRecipeCanonicalDigestProblem TopologyDigestError
  | HeraldJoinBaseRecipeCanonicalShapeProblem TopologyShapeProblem
  | HeraldJoinBaseRecipeCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeHeraldJoinBaseRecipeCanonicalBytes :: ByteString -> Either HeraldJoinBaseRecipeCanonicalProblem HeraldJoinBaseRecipe
decodeHeraldJoinBaseRecipeCanonicalBytes bytes = do
  HeraldJoinBaseRecipeTranscript domain admissionRaw applicantRaw generationRaw establishedRaw contributionRaw <- either (Left . HeraldJoinBaseRecipeCanonicalDecodeFailed) Right (Serialize.decode bytes)
  if domain == "ECLIPS-HERALD-JOIN-BASE-RECIPE" then Right () else Left (HeraldJoinBaseRecipeCanonicalWrongDomain domain)
  admission <- either (Left . HeraldJoinBaseRecipeCanonicalAdmissionProblem) Right (decodeHeraldAdmissionIdCanonicalBytes admissionRaw)
  applicant <- either (Left . HeraldJoinBaseRecipeCanonicalIdentityProblem) Right (mkHeraldEpoch applicantRaw)
  generation <- either (Left . HeraldJoinBaseRecipeCanonicalGenerationProblem) Right (decodeHeraldMembershipGenerationCanonicalBytes generationRaw)
  established <- either (Left . HeraldJoinBaseRecipeCanonicalCutProblem) Right (decodeTopologyCutCanonicalBytes establishedRaw)
  contribution <- either (Left . HeraldJoinBaseRecipeCanonicalDigestProblem) Right (mkTopologyOccurrenceDigest contributionRaw)
  recipe <- either (Left . HeraldJoinBaseRecipeCanonicalShapeProblem) Right (heraldJoinBaseRecipe admission applicant generation established contribution)
  if heraldJoinBaseRecipeCanonicalBytes recipe == bytes then Right recipe else Left HeraldJoinBaseRecipeCanonicalNonCanonical

-- | Substitute the committed coordinate only after readiness has agreed on the
-- recipe. The semantic Graph owner supplies the final occurrence digest after
-- installing the contribution with that exact admission provenance.
activateHeraldJoinBase :: ControlIndex -> TopologyOccurrenceDigest -> HeraldJoinBaseRecipe -> Either TopologyShapeProblem (HeraldMembershipGeneration, TopologyCut)
activateHeraldJoinBase index occurrence recipe@(HeraldJoinBaseRecipe admission applicant generation established _) = do
  let oldFrontier = topologyCutFrontier established
      oldIndex = topologyFrontierAppliedControlPrefix oldFrontier
      terminal = topologyFrontierStructuralVersionVector oldFrontier
  if index > oldIndex then Right () else Left (TopologyAdmissionActivationNotAfterCut oldIndex index)
  successor <- either (Left . TopologyAdmissionGenerationProblem) Right (admitHeraldMembershipGeneration index admission applicant generation)
  initial <- either (Left . TopologyAdmissionVectorProblem) Right (mkStructuralVersionVector successor ((applicant, emptyStructuralPrefix) : structuralVersionVectorEntries terminal))
  let predecessor = AdmissionTopologyPredecessor (deriveTopologyCutId established) (heraldMembershipGenerationId generation) (heraldMembershipGenerationId successor) terminal initial admission (heraldJoinBaseRecipeDigest recipe)
  cut <- topologyCut predecessor (topologyFrontier initial index) occurrence
  Right (successor, cut)

-- | The predecessor of a topology cut. Constructors stay hidden so ordinary
-- owner code cannot omit exact lineage and vector projection checks; the narrow
-- boundary constructor retains only shape-checked claims for later owner
-- admission.
data TopologyPredecessor
  = SameGenerationPredecessor TopologyCutId
  | MembershipSuccessorPredecessor
      TopologyCutId
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
      StructuralVersionVector
      StructuralVersionVector
      TerminalSourceUnionDigest
  | AdmissionTopologyPredecessor
      TopologyCutId
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
      StructuralVersionVector
      StructuralVersionVector
      HeraldAdmissionId
      HeraldJoinBaseRecipeDigest
  deriving stock (Eq, Show)

data TopologyPredecessorView
  = SameGenerationPredecessorView TopologyCutId
  | MembershipSuccessorPredecessorView
      TopologyCutId
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
      StructuralVersionVector
      StructuralVersionVector
      TerminalSourceUnionDigest
  | AdmissionTopologyPredecessorView
      TopologyCutId
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
      StructuralVersionVector
      StructuralVersionVector
      HeraldAdmissionId
      HeraldJoinBaseRecipeDigest
  deriving stock (Eq, Show)

data TopologyShapeProblem
  = TopologyMembershipLineageNotContraction HeraldMembershipGenerationId
  | TopologyTerminalVectorGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TopologyTerminalVectorMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  | TopologySuccessorVectorGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TopologySuccessorVectorMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  | TopologySuccessorVectorProjectionMismatch
      [(HeraldEpoch, StructuralPrefix)]
      [(HeraldEpoch, StructuralPrefix)]
  | TopologyClaimedSuccessorProjectionNotProperContraction
      [(HeraldEpoch, StructuralPrefix)]
      [(HeraldEpoch, StructuralPrefix)]
  | TopologySuccessorFrontierGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TopologySuccessorFrontierMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  | TopologySuccessorFrontierDoesNotCoverInitialVector
  | TopologyGenesisRequiresGenesisMembership
      HeraldMembershipGenerationId
  | TopologyAdmissionApplicantAlreadyActive HeraldEpoch
  | TopologyAdmissionCutBeforeBegin ControlIndex ControlIndex
  | TopologyAdmissionActivationNotAfterCut ControlIndex ControlIndex
  | TopologyAdmissionGenerationProblem MembershipGenerationProblem
  | TopologyAdmissionVectorProblem StructuralVectorProblem
  | TopologyClaimedAdmissionProjectionNotOneEmptyExtension [(HeraldEpoch, StructuralPrefix)] [(HeraldEpoch, StructuralPrefix)]
  deriving stock (Eq, Show)

sameGenerationPredecessor :: TopologyCutId -> TopologyPredecessor
sameGenerationPredecessor = SameGenerationPredecessor

-- | Construct a membership-successor predecessor from an exact ordered lineage.
-- The terminal vector belongs to the established ancestor, and the target's
-- initial vector is its prefix-preserving projection through every retirement.
membershipSuccessorPredecessor ::
  HeraldMembershipLineage ->
  TopologyCutId ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  TerminalSourceUnionDigest ->
  Either TopologyShapeProblem TopologyPredecessor
membershipSuccessorPredecessor lineage predecessorCut terminalVector successorInitialVector unionDigest = do
  let predecessorGeneration = heraldMembershipLineageOrigin lineage
      successorGeneration = heraldMembershipLineageTarget lineage
      predecessorId = heraldMembershipGenerationId predecessorGeneration
      successorId = heraldMembershipGenerationId successorGeneration
      successorMembers = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successorGeneration)
  if null (heraldMembershipLineageRetiredHeraldEpochs lineage)
    then Left (TopologyMembershipLineageNotContraction predecessorId)
    else Right ()
  requireVectorCoordinate
    TopologyTerminalVectorGenerationMismatch
    TopologyTerminalVectorMemberSetMismatch
    predecessorGeneration
    terminalVector
  requireVectorCoordinate
    TopologySuccessorVectorGenerationMismatch
    TopologySuccessorVectorMemberSetMismatch
    successorGeneration
    successorInitialVector
  let terminalEntries = structuralVersionVectorEntries terminalVector
      expectedProjection =
        [ (herald, prefix)
        | herald <- successorMembers,
          Just prefix <- [lookup herald terminalEntries]
        ]
      actualProjection =
        structuralVersionVectorEntries successorInitialVector
  if actualProjection == expectedProjection
    then Right ()
    else
      Left
        ( TopologySuccessorVectorProjectionMismatch
            expectedProjection
            actualProjection
        )
  Right
    ( MembershipSuccessorPredecessor
        predecessorCut
        predecessorId
        successorId
        terminalVector
        successorInitialVector
        unionDigest
    )

-- | Boundary-only reconstruction of the membership-successor predecessor arm.
--
-- The vector generation coordinates must match the two claimed IDs and the
-- successor vector must be the exact prefix-preserving projection obtained by
-- deleting the exact nonempty set of removed terminal components. This does not prove that the generation IDs
-- form checked membership lineage; the owning Graph state must resolve the ordered interval between both IDs
-- from checked membership history before admitting the value semantically.
topologyPredecessorFromClaimedCoordinates ::
  TopologyCutId ->
  HeraldMembershipGenerationId ->
  HeraldMembershipGenerationId ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  TerminalSourceUnionDigest ->
  Either TopologyShapeProblem TopologyPredecessor
topologyPredecessorFromClaimedCoordinates
  predecessorCut
  predecessorId
  successorId
  terminalVector
  successorInitialVector
  unionDigest = do
    let terminalGeneration =
          structuralVersionVectorMembershipGenerationId terminalVector
        successorGeneration =
          structuralVersionVectorMembershipGenerationId successorInitialVector
    if terminalGeneration == predecessorId
      then Right ()
      else
        Left
          ( TopologyTerminalVectorGenerationMismatch
              predecessorId
              terminalGeneration
          )
    if successorGeneration == successorId
      then Right ()
      else
        Left
          ( TopologySuccessorVectorGenerationMismatch
              successorId
              successorGeneration
          )
    let terminalEntries = structuralVersionVectorEntries terminalVector
        successorEntries =
          structuralVersionVectorEntries successorInitialVector
        successorHeralds = fmap fst successorEntries
        removedHeralds =
          [ herald
          | (herald, _) <- terminalEntries,
            herald `notElem` successorHeralds
          ]
        expectedProjection =
          [ entry
          | entry@(herald, _) <- terminalEntries,
            herald `elem` successorHeralds
          ]
    if predecessorId /= successorId && not (null removedHeralds) && successorEntries == expectedProjection
      then Right ()
      else
        Left
          ( TopologyClaimedSuccessorProjectionNotProperContraction
              terminalEntries
              successorEntries
          )
    Right
      ( MembershipSuccessorPredecessor
          predecessorCut
          predecessorId
          successorId
          terminalVector
          successorInitialVector
          unionDigest
      )

topologyPredecessorView :: TopologyPredecessor -> TopologyPredecessorView
topologyPredecessorView predecessor = case predecessor of
  SameGenerationPredecessor cut -> SameGenerationPredecessorView cut
  MembershipSuccessorPredecessor cut predecessorId successorId terminal initialVector digest ->
    MembershipSuccessorPredecessorView
      cut
      predecessorId
      successorId
      terminal
      initialVector
      digest
  AdmissionTopologyPredecessor cut predecessorId successorId terminal initialVector admission digest ->
    AdmissionTopologyPredecessorView cut predecessorId successorId terminal initialVector admission digest

topologyPredecessorCutId :: TopologyPredecessor -> TopologyCutId
topologyPredecessorCutId predecessor = case predecessor of
  SameGenerationPredecessor cut -> cut
  MembershipSuccessorPredecessor cut _ _ _ _ _ -> cut
  AdmissionTopologyPredecessor cut _ _ _ _ _ _ -> cut

-- | Boundary shape admission. Authority still requires the recipe, membership
-- history and exact activation certificate retained by the semantic owner.
admissionTopologyPredecessorFromClaimedCoordinates :: TopologyCutId -> HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> StructuralVersionVector -> StructuralVersionVector -> HeraldAdmissionId -> HeraldJoinBaseRecipeDigest -> Either TopologyShapeProblem TopologyPredecessor
admissionTopologyPredecessorFromClaimedCoordinates cut origin target terminal initial admission digest = do
  let terminalId = structuralVersionVectorMembershipGenerationId terminal
      initialId = structuralVersionVectorMembershipGenerationId initial
  if terminalId == origin then Right () else Left (TopologyTerminalVectorGenerationMismatch origin terminalId)
  if initialId == target then Right () else Left (TopologySuccessorVectorGenerationMismatch target initialId)
  let oldEntries = structuralVersionVectorEntries terminal
      newEntries = structuralVersionVectorEntries initial
      added = filter ((`notElem` fmap fst oldEntries) . fst) newEntries
      retained = filter ((`elem` fmap fst oldEntries) . fst) newEntries
  case added of
    [(_, prefix)] | origin /= target && prefix == emptyStructuralPrefix && retained == oldEntries -> Right ()
    _ -> Left (TopologyClaimedAdmissionProjectionNotOneEmptyExtension oldEntries newEntries)
  Right (AdmissionTopologyPredecessor cut origin target terminal initial admission digest)

requireVectorCoordinate ::
  (HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> TopologyShapeProblem) ->
  (MemberSetDigest -> MemberSetDigest -> TopologyShapeProblem) ->
  HeraldMembershipGeneration ->
  StructuralVersionVector ->
  Either TopologyShapeProblem ()
requireVectorCoordinate generationProblem memberProblem generation vector = do
  let expectedGeneration = heraldMembershipGenerationId generation
      actualGeneration = structuralVersionVectorMembershipGenerationId vector
      expectedMembers =
        heraldMembershipGenerationActiveMemberSetDigest generation
      actualMembers = structuralVersionVectorMemberSetDigest vector
  if actualGeneration == expectedGeneration
    then Right ()
    else Left (generationProblem expectedGeneration actualGeneration)
  if actualMembers == expectedMembers
    then Right ()
    else Left (memberProblem expectedMembers actualMembers)

data TopologyFrontier = TopologyFrontier
  { structuralVersionVector :: StructuralVersionVector,
    appliedControlPrefix :: ControlIndex
  }
  deriving stock (Eq, Show)

topologyFrontier ::
  StructuralVersionVector ->
  ControlIndex ->
  TopologyFrontier
topologyFrontier = TopologyFrontier

topologyFrontierMembershipGenerationId ::
  TopologyFrontier -> HeraldMembershipGenerationId
topologyFrontierMembershipGenerationId =
  structuralVersionVectorMembershipGenerationId . structuralVersionVector

topologyFrontierMemberSetDigest :: TopologyFrontier -> MemberSetDigest
topologyFrontierMemberSetDigest =
  structuralVersionVectorMemberSetDigest . structuralVersionVector

topologyFrontierStructuralVersionVector ::
  TopologyFrontier -> StructuralVersionVector
topologyFrontierStructuralVersionVector = structuralVersionVector

topologyFrontierAppliedControlPrefix :: TopologyFrontier -> ControlIndex
topologyFrontierAppliedControlPrefix = appliedControlPrefix

data TopologyCut = TopologyCut
  { predecessor :: TopologyPredecessor,
    frontier :: TopologyFrontier,
    occurrenceDigest :: TopologyOccurrenceDigest
  }
  deriving stock (Eq, Show)

topologyCut ::
  TopologyPredecessor ->
  TopologyFrontier ->
  TopologyOccurrenceDigest ->
  Either TopologyShapeProblem TopologyCut
topologyCut predecessor frontier occurrenceDigest = do
  validateSuccessorFrontier predecessor frontier
  Right (TopologyCut predecessor frontier occurrenceDigest)

validateSuccessorFrontier ::
  TopologyPredecessor ->
  TopologyFrontier ->
  Either TopologyShapeProblem ()
validateSuccessorFrontier predecessor frontier = case predecessor of
  SameGenerationPredecessor _ -> Right ()
  MembershipSuccessorPredecessor _ _ successorId _ initialVector _ -> check successorId initialVector
  AdmissionTopologyPredecessor _ _ successorId _ initialVector _ _ -> check successorId initialVector
  where
    check successorId initialVector = do
      let frontierVector = topologyFrontierStructuralVersionVector frontier
          expectedMembers = structuralVersionVectorMemberSetDigest initialVector
          actualMembers = structuralVersionVectorMemberSetDigest frontierVector
          actualGeneration =
            structuralVersionVectorMembershipGenerationId frontierVector
      if actualGeneration == successorId
        then Right ()
        else
          Left
            ( TopologySuccessorFrontierGenerationMismatch
                successorId
                actualGeneration
            )
      if actualMembers == expectedMembers
        then Right ()
        else
          Left
            ( TopologySuccessorFrontierMemberSetMismatch
                expectedMembers
                actualMembers
            )
      if structuralVersionVectorCovers frontierVector initialVector
        then Right ()
        else Left TopologySuccessorFrontierDoesNotCoverInitialVector

topologyCutPredecessor :: TopologyCut -> TopologyPredecessor
topologyCutPredecessor = predecessor

topologyCutFrontier :: TopologyCut -> TopologyFrontier
topologyCutFrontier = frontier

topologyCutOccurrenceDigest :: TopologyCut -> TopologyOccurrenceDigest
topologyCutOccurrenceDigest = occurrenceDigest

deriveTopologyCutId :: TopologyCut -> TopologyCutId
deriveTopologyCutId cut =
  identityInvariant
    "topology-cut identity"
    (SHA256.hash (topologyCutCanonicalBytes cut))

-- | Stable canonical transcript hashed by 'deriveTopologyCutId' and embedded
-- unchanged in an alignment cut.  Exposing these semantic bytes prevents the
-- Alignment domain from independently reimplementing the topology transcript.
topologyCutCanonicalBytes :: TopologyCut -> ByteString
topologyCutCanonicalBytes cut =
  Serialize.encode
    ( TopologyCutTranscript
        "ECLIPS-TOPOLOGY-CUT"
        (predecessorTranscript cut.predecessor)
        (frontierTranscript cut.frontier)
        (topologyOccurrenceDigestBytes cut.occurrenceDigest)
    )

data TopologyCutCanonicalProblem
  = TopologyCutCanonicalDecodeFailed String
  | TopologyCutCanonicalWrongDomain ByteString
  | TopologyCutCanonicalInvalidIdentity IdentityError
  | TopologyCutCanonicalInvalidMembershipIdentity MembershipIdentityError
  | TopologyCutCanonicalInvalidDigest TopologyDigestError
  | TopologyCutCanonicalInvalidVector StructuralVectorProblem
  | TopologyCutCanonicalInvalidPrefix Word8 Word64
  | TopologyCutCanonicalInvalidPredecessorTag Word8
  | TopologyCutCanonicalInvalidAdmission HeraldAdmissionIdCanonicalProblem
  | TopologyCutCanonicalEmptyVector
  | TopologyCutCanonicalShapeProblem TopologyShapeProblem
  | TopologyCutCanonicalNonCanonical
  deriving stock (Eq, Show)

-- | Decode intrinsic shape and canonical bytes. A membership-successor arm
-- remains a claim until its owner resolves the full checked membership lineage.
decodeTopologyCutCanonicalBytes :: ByteString -> Either TopologyCutCanonicalProblem TopologyCut
decodeTopologyCutCanonicalBytes bytes = do
  TopologyCutTranscript domain predecessor frontier occurrence <- either (Left . TopologyCutCanonicalDecodeFailed) Right (Serialize.decode bytes)
  if domain == "ECLIPS-TOPOLOGY-CUT" then Right () else Left (TopologyCutCanonicalWrongDomain domain)
  decodedPredecessor <- decodePredecessor predecessor
  decodedFrontier <- case frontier of
    TopologyFrontierTranscript vector index -> topologyFrontier <$> decodeVector vector <*> pure (controlIndex index)
  digest <- either (Left . TopologyCutCanonicalInvalidDigest) Right (mkTopologyOccurrenceDigest occurrence)
  cut <- either (Left . TopologyCutCanonicalShapeProblem) Right (topologyCut decodedPredecessor decodedFrontier digest)
  if topologyCutCanonicalBytes cut == bytes then Right cut else Left TopologyCutCanonicalNonCanonical
  where
    decodePredecessor (TopologyPredecessorTranscript tag cutRaw originRaw targetRaw terminal initialRaw digestRaw) = do
      cut <- either (Left . TopologyCutCanonicalInvalidIdentity) Right (mkTopologyCutId cutRaw)
      case tag of
        0
          | ByteString.null originRaw && ByteString.null targetRaw && isNothing terminal && isNothing initialRaw && ByteString.null digestRaw -> Right (sameGenerationPredecessor cut)
          | otherwise -> Left TopologyCutCanonicalNonCanonical
        1 -> do
          origin <- either (Left . TopologyCutCanonicalInvalidMembershipIdentity) Right (mkHeraldMembershipGenerationId originRaw)
          target <- either (Left . TopologyCutCanonicalInvalidMembershipIdentity) Right (mkHeraldMembershipGenerationId targetRaw)
          terminalVector <- maybe (Left TopologyCutCanonicalNonCanonical) decodeVector terminal
          initialVector <- maybe (Left TopologyCutCanonicalNonCanonical) decodeVector initialRaw
          digest <- either (Left . TopologyCutCanonicalInvalidDigest) Right (mkTerminalSourceUnionDigest digestRaw)
          either (Left . TopologyCutCanonicalShapeProblem) Right (topologyPredecessorFromClaimedCoordinates cut origin target terminalVector initialVector digest)
        2 -> do
          origin <- either (Left . TopologyCutCanonicalInvalidMembershipIdentity) Right (mkHeraldMembershipGenerationId originRaw)
          target <- either (Left . TopologyCutCanonicalInvalidMembershipIdentity) Right (mkHeraldMembershipGenerationId targetRaw)
          terminalVector <- maybe (Left TopologyCutCanonicalNonCanonical) decodeVector terminal
          initialVector <- maybe (Left TopologyCutCanonicalNonCanonical) decodeVector initialRaw
          AdmissionPredecessorTranscript admissionRaw recipeRaw <- either (Left . TopologyCutCanonicalDecodeFailed) Right (Serialize.decode digestRaw)
          admission <- either (Left . TopologyCutCanonicalInvalidAdmission) Right (decodeHeraldAdmissionIdCanonicalBytes admissionRaw)
          recipe <- either (Left . TopologyCutCanonicalInvalidDigest) Right (mkHeraldJoinBaseRecipeDigest recipeRaw)
          either (Left . TopologyCutCanonicalShapeProblem) Right (admissionTopologyPredecessorFromClaimedCoordinates cut origin target terminalVector initialVector admission recipe)
        other -> Left (TopologyCutCanonicalInvalidPredecessorTag other)
    decodeVector (StructuralVersionVectorTranscript generationRaw digestRaw entries) = do
      generation <- either (Left . TopologyCutCanonicalInvalidMembershipIdentity) Right (mkHeraldMembershipGenerationId generationRaw)
      digest <- either (Left . TopologyCutCanonicalInvalidDigest) Right (mkMemberSetDigest digestRaw)
      decoded <- traverse decodeEntry entries
      nonempty <- maybe (Left TopologyCutCanonicalEmptyVector) Right (NonEmpty.nonEmpty decoded)
      either (Left . TopologyCutCanonicalInvalidVector) Right (structuralVersionVectorFromClaimedCoordinates generation digest nonempty)
    decodeEntry (StructuralVectorEntryTranscript heraldRaw tag sequenceNumber) = do
      herald <- either (Left . TopologyCutCanonicalInvalidIdentity) Right (mkHeraldEpoch heraldRaw)
      prefix <- case (tag, sequenceNumber) of
        (0, 0) -> Right emptyStructuralPrefix
        (1, _) -> either (const (Left (TopologyCutCanonicalInvalidPrefix tag sequenceNumber))) (Right . structuralPrefixThrough) (mkStructuralSequence sequenceNumber)
        _ -> Left (TopologyCutCanonicalInvalidPrefix tag sequenceNumber)
      Right (herald, prefix)

-- | Derive the distinguished predecessor root from checked startup facts.
--
-- The checked generation must be genesis. Its all-empty vector is derived
-- internally, so a caller cannot combine a generation identity, member digest,
-- and component set from different facts. The control prefix is normatively
-- zero.
deriveGenesisTopologyCutId ::
  SystemId ->
  HeraldMembershipGeneration ->
  InitialProjectionDigest ->
  Either TopologyShapeProblem TopologyCutId
deriveGenesisTopologyCutId system generation initialProjection = do
  case heraldMembershipGenerationPredecessor generation of
    Nothing -> Right ()
    Just _ ->
      Left
        ( TopologyGenesisRequiresGenesisMembership
            (heraldMembershipGenerationId generation)
        )
  let emptyVector = emptyStructuralVersionVector generation
  Right
    ( identityInvariant
        "genesis topology-cut identity"
        ( SHA256.hash
            ( Serialize.encode
                ( GenesisTopologyCutTranscript
                    "ECLIPS-GENESIS-TOPOLOGY-CUT"
                    (systemIdBytes system)
                    ( heraldMembershipGenerationIdBytes
                        (heraldMembershipGenerationId generation)
                    )
                    ( memberSetDigestBytes
                        (heraldMembershipGenerationActiveMemberSetDigest generation)
                    )
                    (initialProjectionDigestBytes initialProjection)
                    (fmap vectorEntryTranscript (structuralVersionVectorEntries emptyVector))
                    0
                )
            )
        )
    )

checkDigestBytes :: ByteString -> Either TopologyDigestError ByteString
checkDigestBytes bytes
  | ByteString.length bytes == sha256ByteCount = Right bytes
  | otherwise =
      Left
        TopologyDigestWrongByteCount
          { expectedTopologyDigestByteCount = sha256ByteCount,
            actualTopologyDigestByteCount = ByteString.length bytes
          }

hashTranscript :: (Serialize transcript) => transcript -> ByteString
hashTranscript = SHA256.hash . Serialize.encode

digestInvariant ::
  String ->
  (ByteString -> Either TopologyDigestError digest) ->
  ByteString ->
  digest
digestInvariant label constructor bytes =
  either (error . ((label <> " invariant: ") <>) . show) id (constructor bytes)

identityInvariant :: String -> ByteString -> TopologyCutId
identityInvariant label bytes =
  either (error . ((label <> " invariant: ") <>) . show) id (mkTopologyCutId bytes)

data TopologyOccurrenceDigestTranscript
  = TopologyOccurrenceDigestTranscript ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data TopologyFrontierTranscript
  = TopologyFrontierTranscript
      StructuralVersionVectorTranscript
      Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data TopologyCutTranscript
  = TopologyCutTranscript
      ByteString
      TopologyPredecessorTranscript
      TopologyFrontierTranscript
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data GenesisTopologyCutTranscript
  = GenesisTopologyCutTranscript
      ByteString
      ByteString
      ByteString
      ByteString
      ByteString
      [StructuralVectorEntryTranscript]
      Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data StructuralVectorEntryTranscript
  = StructuralVectorEntryTranscript ByteString Word8 Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data StructuralVersionVectorTranscript
  = StructuralVersionVectorTranscript
      ByteString
      ByteString
      [StructuralVectorEntryTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data TopologyPredecessorTranscript
  = TopologyPredecessorTranscript
      Word8
      ByteString
      ByteString
      ByteString
      (Maybe StructuralVersionVectorTranscript)
      (Maybe StructuralVersionVectorTranscript)
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldJoinBaseRecipeTranscript = HeraldJoinBaseRecipeTranscript ByteString ByteString ByteString ByteString ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data AdmissionPredecessorTranscript = AdmissionPredecessorTranscript ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

frontierTranscript :: TopologyFrontier -> TopologyFrontierTranscript
frontierTranscript value =
  TopologyFrontierTranscript
    (vectorTranscript value.structuralVersionVector)
    (controlIndexWord64 value.appliedControlPrefix)

vectorTranscript :: StructuralVersionVector -> StructuralVersionVectorTranscript
vectorTranscript vector =
  StructuralVersionVectorTranscript
    ( heraldMembershipGenerationIdBytes
        (structuralVersionVectorMembershipGenerationId vector)
    )
    (memberSetDigestBytes (structuralVersionVectorMemberSetDigest vector))
    (fmap vectorEntryTranscript (structuralVersionVectorEntries vector))

predecessorTranscript :: TopologyPredecessor -> TopologyPredecessorTranscript
predecessorTranscript predecessor = case predecessor of
  SameGenerationPredecessor cut ->
    TopologyPredecessorTranscript
      0
      (topologyCutIdBytes cut)
      ByteString.empty
      ByteString.empty
      Nothing
      Nothing
      ByteString.empty
  MembershipSuccessorPredecessor cut predecessorId successorId terminal initialVector digest ->
    TopologyPredecessorTranscript
      1
      (topologyCutIdBytes cut)
      (heraldMembershipGenerationIdBytes predecessorId)
      (heraldMembershipGenerationIdBytes successorId)
      (Just (vectorTranscript terminal))
      (Just (vectorTranscript initialVector))
      (terminalSourceUnionDigestBytes digest)
  AdmissionTopologyPredecessor cut predecessorId successorId terminal initialVector admission digest ->
    TopologyPredecessorTranscript
      2
      (topologyCutIdBytes cut)
      (heraldMembershipGenerationIdBytes predecessorId)
      (heraldMembershipGenerationIdBytes successorId)
      (Just (vectorTranscript terminal))
      (Just (vectorTranscript initialVector))
      (Serialize.encode (AdmissionPredecessorTranscript (heraldAdmissionIdCanonicalBytes admission) (heraldJoinBaseRecipeDigestValueBytes digest)))

vectorEntryTranscript ::
  (HeraldEpoch, StructuralPrefix) -> StructuralVectorEntryTranscript
vectorEntryTranscript (herald, prefix) =
  case structuralPrefixSequence prefix of
    Nothing -> StructuralVectorEntryTranscript (heraldEpochBytes herald) 0 0
    Just sequenceNumber ->
      StructuralVectorEntryTranscript
        (heraldEpochBytes herald)
        1
        (structuralSequenceWord64 sequenceNumber)
