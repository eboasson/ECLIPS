{-# LANGUAGE ImportQualifiedPost #-}

-- | Narrow owner-backed readiness for opening a disappearance probe.
--
-- The Oracle projection owns the current membership body.  Structural
-- progress independently owns proof that a topology base for its current
-- membership is established.  This adapter keeps both captures opaque and
-- joins them only after checking their complete coordinate; callers cannot
-- manufacture readiness from a bare membership value or a cut identity.
module Eclips.Herald.Graph.DisappearanceReadiness
  ( CurrentMembershipCapture,
    captureCurrentMembership,
    currentMembershipCaptureGenerationId,
    currentMembershipCaptureMemberSetDigest,
    currentMembershipCaptureMembers,
    currentMembershipCaptureLocalHerald,
    EstablishedStructuralBaseCapture,
    captureEstablishedStructuralBase,
    structuralBaseCaptureGenerationId,
    structuralBaseCaptureMemberSetDigest,
    structuralBaseCaptureMembers,
    structuralBaseCaptureLocalHerald,
    structuralBaseCaptureCutId,
    structuralBaseCaptureEstablishedCut,
    DisappearanceOpenReadinessProblem (..),
    disappearanceOpenContextFromOwnerCaptures,
  )
where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( HeraldEpoch,
    TopologyCutId,
  )
import Eclips.Domain.MemberSet (MemberSetDigest)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Topology
  ( topologyCutFrontier,
    topologyFrontierMemberSetDigest,
    topologyFrontierMembershipGenerationId,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceOpenContext,
    disappearanceOpenContext,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( TopologyCutAcceptance,
    TopologyCutEstablished,
    structuralAppliedReportMemberSetDigest,
    structuralAppliedReportMembershipGenerationId,
    topologyCutAcceptanceReport,
    topologyCutAcceptanceReporter,
    topologyCutEstablishedAcceptances,
    topologyCutEstablishedCut,
    topologyCutEstablishedId,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection

-- | The exact current membership body and local identity read together from
-- one Oracle-projection owner state.
data CurrentMembershipCapture
  = CurrentMembershipCapture HeraldMembershipGeneration HeraldEpoch
  deriving stock (Eq, Show)

captureCurrentMembership ::
  OracleProjection.State -> CurrentMembershipCapture
captureCurrentMembership state =
  let view = OracleProjection.oracleView state
   in CurrentMembershipCapture
        (OracleProjection.oracleViewCurrentHeraldMembership view)
        (OracleProjection.oracleViewLocalHeraldEpoch view)

currentMembershipCaptureMembership ::
  CurrentMembershipCapture -> HeraldMembershipGeneration
currentMembershipCaptureMembership (CurrentMembershipCapture membership _) =
  membership

currentMembershipCaptureGenerationId ::
  CurrentMembershipCapture -> HeraldMembershipGenerationId
currentMembershipCaptureGenerationId =
  heraldMembershipGenerationId . currentMembershipCaptureMembership

currentMembershipCaptureMemberSetDigest ::
  CurrentMembershipCapture -> MemberSetDigest
currentMembershipCaptureMemberSetDigest =
  heraldMembershipGenerationActiveMemberSetDigest
    . currentMembershipCaptureMembership

currentMembershipCaptureMembers ::
  CurrentMembershipCapture -> NonEmpty HeraldEpoch
currentMembershipCaptureMembers =
  heraldMembershipGenerationActiveHeraldEpochs
    . currentMembershipCaptureMembership

currentMembershipCaptureLocalHerald ::
  CurrentMembershipCapture -> HeraldEpoch
currentMembershipCaptureLocalHerald (CurrentMembershipCapture _ local) = local

-- | The current structural coordinate together with its retained establishment
-- evidence.  Genesis is explicit: its checked initial projection is the base
-- installed by 'initialStructuralProgressState', whereas every later capture
-- retains the exact 'TopologyCutEstablished' found in installed Graph history.
data EstablishedStructuralBaseCapture
  = EstablishedGenesisStructuralBase
      HeraldMembershipGenerationId
      MemberSetDigest
      (NonEmpty HeraldEpoch)
      HeraldEpoch
      TopologyCutId
  | EstablishedInstalledStructuralBase
      HeraldMembershipGenerationId
      MemberSetDigest
      (NonEmpty HeraldEpoch)
      HeraldEpoch
      TopologyCutEstablished
  deriving stock (Eq, Show)

-- | Contradictions between the two owner captures, or between Graph's current
-- coordinate and the concrete established cut retained at its current tip.
data DisappearanceOpenReadinessProblem
  = DisappearanceStructuralBaseMissing TopologyCutId
  | DisappearanceStructuralBaseCutIdMismatch TopologyCutId TopologyCutId
  | DisappearanceStructuralBaseGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | DisappearanceStructuralBaseMemberSetDigestMismatch
      MemberSetDigest
      MemberSetDigest
  | DisappearanceStructuralBaseReporterSetMismatch
      (Set HeraldEpoch)
      (Set HeraldEpoch)
  | DisappearanceStructuralBaseReportGenerationMismatch
      HeraldEpoch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | DisappearanceStructuralBaseReportMemberSetDigestMismatch
      HeraldEpoch
      MemberSetDigest
      MemberSetDigest
  | DisappearanceStructuralBaseLocalHeraldNotMember HeraldEpoch
  | DisappearanceOpenGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | DisappearanceOpenMemberSetDigestMismatch
      MemberSetDigest
      MemberSetDigest
  | DisappearanceOpenMemberSetMismatch
      (Set HeraldEpoch)
      (Set HeraldEpoch)
  | DisappearanceOpenLocalHeraldMismatch HeraldEpoch HeraldEpoch
  | DisappearanceOpenLocalHeraldNotMember HeraldEpoch
  deriving stock (Eq, Show)

-- | Capture only a base whose establishment is visible in the actual Graph
-- owner.  A non-genesis tip must be recoverable from installed-cut history;
-- the retained established message must name that tip and authenticate the
-- same generation, member digest, and complete reporter set as Graph's current
-- membership coordinate.
captureEstablishedStructuralBase ::
  GraphProgress.StructuralProgressState ->
  Either DisappearanceOpenReadinessProblem EstablishedStructuralBaseCapture
captureEstablishedStructuralBase state = do
  let generation = GraphProgress.structuralProgressMembershipGenerationId state
      memberDigest = GraphProgress.structuralProgressMemberSetDigest state
      members = GraphProgress.structuralProgressMembers state
      memberSet = memberSetOf members
      local = GraphProgress.structuralProgressLocalHerald state
      currentCut = GraphProgress.structuralLastInstalledCutId state
      genesisCut = GraphProgress.structuralGenesisCutId state
  unless
    (Set.member local memberSet)
    (Left (DisappearanceStructuralBaseLocalHeraldNotMember local))
  if currentCut == genesisCut
    then
      Right
        ( EstablishedGenesisStructuralBase
            generation
            memberDigest
            members
            local
            currentCut
        )
    else do
      installed <-
        maybe
          (Left (DisappearanceStructuralBaseMissing currentCut))
          Right
          (GraphProgress.lookupInstalledTopologyCut currentCut state)
      let established = GraphProgress.installedTopologyCutEstablished installed
          actualCut = topologyCutEstablishedId established
          frontier = topologyCutFrontier (topologyCutEstablishedCut established)
          actualGeneration = topologyFrontierMembershipGenerationId frontier
          actualMemberDigest = topologyFrontierMemberSetDigest frontier
          acceptances = topologyCutEstablishedAcceptances established
          reporters = Set.fromList (fmap topologyCutAcceptanceReporter acceptances)
      unless
        (actualCut == currentCut)
        ( Left
            ( DisappearanceStructuralBaseCutIdMismatch
                currentCut
                actualCut
            )
        )
      unless
        (actualGeneration == generation)
        ( Left
            ( DisappearanceStructuralBaseGenerationMismatch
                generation
                actualGeneration
            )
        )
      unless
        (actualMemberDigest == memberDigest)
        ( Left
            ( DisappearanceStructuralBaseMemberSetDigestMismatch
                memberDigest
                actualMemberDigest
            )
        )
      unless
        (reporters == memberSet)
        ( Left
            ( DisappearanceStructuralBaseReporterSetMismatch
                memberSet
                reporters
            )
        )
      mapM_
        (checkAcceptanceCoordinate generation memberDigest)
        acceptances
      Right
        ( EstablishedInstalledStructuralBase
            generation
            memberDigest
            members
            local
            established
        )

checkAcceptanceCoordinate ::
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  TopologyCutAcceptance ->
  Either DisappearanceOpenReadinessProblem ()
checkAcceptanceCoordinate expectedGeneration expectedMemberDigest acceptance = do
  let reporter = topologyCutAcceptanceReporter acceptance
      report = topologyCutAcceptanceReport acceptance
      actualGeneration = structuralAppliedReportMembershipGenerationId report
      actualMemberDigest = structuralAppliedReportMemberSetDigest report
  unless
    (actualGeneration == expectedGeneration)
    ( Left
        ( DisappearanceStructuralBaseReportGenerationMismatch
            reporter
            expectedGeneration
            actualGeneration
        )
    )
  unless
    (actualMemberDigest == expectedMemberDigest)
    ( Left
        ( DisappearanceStructuralBaseReportMemberSetDigestMismatch
            reporter
            expectedMemberDigest
            actualMemberDigest
        )
    )

structuralBaseCaptureGenerationId ::
  EstablishedStructuralBaseCapture -> HeraldMembershipGenerationId
structuralBaseCaptureGenerationId capture = case capture of
  EstablishedGenesisStructuralBase generation _ _ _ _ -> generation
  EstablishedInstalledStructuralBase generation _ _ _ _ -> generation

structuralBaseCaptureMemberSetDigest ::
  EstablishedStructuralBaseCapture -> MemberSetDigest
structuralBaseCaptureMemberSetDigest capture = case capture of
  EstablishedGenesisStructuralBase _ digest _ _ _ -> digest
  EstablishedInstalledStructuralBase _ digest _ _ _ -> digest

structuralBaseCaptureMembers ::
  EstablishedStructuralBaseCapture -> NonEmpty HeraldEpoch
structuralBaseCaptureMembers capture = case capture of
  EstablishedGenesisStructuralBase _ _ members _ _ -> members
  EstablishedInstalledStructuralBase _ _ members _ _ -> members

structuralBaseCaptureLocalHerald ::
  EstablishedStructuralBaseCapture -> HeraldEpoch
structuralBaseCaptureLocalHerald capture = case capture of
  EstablishedGenesisStructuralBase _ _ _ local _ -> local
  EstablishedInstalledStructuralBase _ _ _ local _ -> local

structuralBaseCaptureCutId ::
  EstablishedStructuralBaseCapture -> TopologyCutId
structuralBaseCaptureCutId capture = case capture of
  EstablishedGenesisStructuralBase _ _ _ _ cut -> cut
  EstablishedInstalledStructuralBase _ _ _ _ established ->
    topologyCutEstablishedId established

-- | The explicit retained establishment evidence.  Genesis has no peer
-- 'TopologyCutEstablished' message; its distinguished cut was installed from
-- checked startup state.
structuralBaseCaptureEstablishedCut ::
  EstablishedStructuralBaseCapture -> Maybe TopologyCutEstablished
structuralBaseCaptureEstablishedCut capture = case capture of
  EstablishedGenesisStructuralBase {} -> Nothing
  EstablishedInstalledStructuralBase _ _ _ _ established -> Just established

-- | Join independently captured owner facts.  Every equality required by an
-- Open is checked here before delegating to the existing opaque context
-- constructor.  The complete Oracle membership body is the resulting context;
-- Graph contributes readiness, never an independently reconstructed body.
disappearanceOpenContextFromOwnerCaptures ::
  CurrentMembershipCapture ->
  EstablishedStructuralBaseCapture ->
  Either DisappearanceOpenReadinessProblem DisappearanceOpenContext
disappearanceOpenContextFromOwnerCaptures membershipCapture baseCapture = do
  let oracleGeneration = currentMembershipCaptureGenerationId membershipCapture
      graphGeneration = structuralBaseCaptureGenerationId baseCapture
      oracleDigest = currentMembershipCaptureMemberSetDigest membershipCapture
      graphDigest = structuralBaseCaptureMemberSetDigest baseCapture
      oracleMembers = memberSetOf (currentMembershipCaptureMembers membershipCapture)
      graphMembers = memberSetOf (structuralBaseCaptureMembers baseCapture)
      oracleLocal = currentMembershipCaptureLocalHerald membershipCapture
      graphLocal = structuralBaseCaptureLocalHerald baseCapture
  unless
    (oracleGeneration == graphGeneration)
    (Left (DisappearanceOpenGenerationMismatch oracleGeneration graphGeneration))
  unless
    (oracleDigest == graphDigest)
    (Left (DisappearanceOpenMemberSetDigestMismatch oracleDigest graphDigest))
  unless
    (oracleMembers == graphMembers)
    (Left (DisappearanceOpenMemberSetMismatch oracleMembers graphMembers))
  unless
    (oracleLocal == graphLocal)
    (Left (DisappearanceOpenLocalHeraldMismatch oracleLocal graphLocal))
  unless
    (Set.member oracleLocal oracleMembers && Set.member oracleLocal graphMembers)
    (Left (DisappearanceOpenLocalHeraldNotMember oracleLocal))
  Right
    ( disappearanceOpenContext
        (currentMembershipCaptureMembership membershipCapture)
    )

memberSetOf :: NonEmpty HeraldEpoch -> Set HeraldEpoch
memberSetOf = Set.fromList . NonEmpty.toList
