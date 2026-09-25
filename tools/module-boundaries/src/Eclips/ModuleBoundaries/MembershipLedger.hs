module Eclips.ModuleBoundaries.MembershipLedger
  ( MembershipSource,
    membershipSource,
    auditMembershipLedger,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | One production source gathered during the module/import checker's existing
-- traversal. The ledger deliberately consumes this observation instead of
-- walking or reading the tree a second time.
data MembershipSource
  = MembershipSource FilePath [(Int, String)]

membershipSource :: FilePath -> [(Int, String)] -> MembershipSource
membershipSource = MembershipSource

data MembershipSiteCategory
  = GenesisMembershipRead
  | MembershipVectorOrCut
  | UnavailableMemberWait
  | ProtocolProducerConsumer
  | RuntimeAdmission
  deriving stock (Eq, Ord, Show)

data MembershipMigrationStatus
  = RetainedGenesisSource
  | Migrated
  deriving stock (Eq, Ord, Show)

data MembershipTarget
  = FixedGenesisBaseline
  | Step15Increment4
  | Step15Increment5
  | Step15Increment6
  | Step15Increment7
  | Step15Increment8
  | Step16Increment1
  | Step16Increment2
  | Step16Increment3
  | Step16Increment6
  | Step16Increment8
  | Profile02P01
  | Profile02P02
  | Profile02P04
  | Profile02P05
  | Profile02P06
  | Profile02P09
  | LabelControlBaseAdmission
  deriving stock (Eq, Ord, Show)

data MembershipAnchor
  = MembershipAnchor String Int

data MembershipSite
  = MembershipSite
      String
      MembershipSiteCategory
      MembershipMigrationStatus
      MembershipTarget
      FilePath
      (NonEmpty MembershipAnchor)

-- | Check the fixed-genesis migration surface. Every listed anchor is an exact
-- token-count assertion, while the watched-token pass prevents a known
-- membership-sensitive identifier from appearing in a newly unreviewed source.
-- All checks operate on the source observations accumulated by the ordinary
-- package pass.
auditMembershipLedger :: [MembershipSource] -> [String]
auditMembershipLedger sources =
  ledgerDefinitionViolations
    <> concatMap (checkSite sourceIndex) membershipSites
    <> unledgeredWatchedTokenViolations sources
  where
    sourceIndex =
      Map.fromList
        [ (path, identifierCounts identifiers)
        | MembershipSource path identifiers <- sources
        ]

identifierCounts :: [(Int, String)] -> Map String Int
identifierCounts = Map.fromListWith (+) . fmap (\(_, identifier) -> (identifier, 1))

checkSite :: Map FilePath (Map String Int) -> MembershipSite -> [String]
checkSite sourceIndex (MembershipSite siteId _ _ _ path anchors) =
  case Map.lookup path sourceIndex of
    Nothing ->
      [ "Step-15 membership ledger site "
          <> siteId
          <> " source is absent: "
          <> path
      ]
    Just counts -> concatMap checkAnchor (NonEmpty.toList anchors)
      where
        checkAnchor (MembershipAnchor identifier expected) =
          let observed = Map.findWithDefault 0 identifier counts
           in [ "Step-15 membership ledger anchor "
                  <> siteId
                  <> "/"
                  <> identifier
                  <> " in "
                  <> path
                  <> " expected "
                  <> show expected
                  <> " occurrence(s), found "
                  <> show observed
              | observed /= expected
              ]

unledgeredWatchedTokenViolations :: [MembershipSource] -> [String]
unledgeredWatchedTokenViolations sources =
  [ path
      <> ":"
      <> show firstLine
      <> ": unreviewed Step-15 membership-sensitive token: "
      <> identifier
      <> "; add or update a stable membership-ledger site"
  | MembershipSource path identifiers <- sources,
    (identifier, firstLine) <- Map.toAscList (firstLines identifiers),
    identifier `Set.member` watchedMembershipIdentifiers,
    (path, identifier) `Set.notMember` ledgerAnchorPairs
  ]
  where
    firstLines = Map.fromListWith min . fmap (\(line, identifier) -> (identifier, line))

ledgerDefinitionViolations :: [String]
ledgerDefinitionViolations =
  duplicateIdViolations
    <> duplicateSiteCategoryViolations
    <> emptyIdViolations
    <> invalidStatusTargetViolations
    <> unwitnessedWatchedIdentifierViolations
  where
    duplicateIdViolations =
      [ "Step-15 membership ledger has duplicate stable ID: " <> siteId
      | (siteId, count) <- Map.toAscList (counts membershipSiteIds),
        count /= 1
      ]
    duplicateSiteCategoryViolations =
      [ "Step-15 membership ledger duplicates category/path owner: "
          <> show category
          <> " at "
          <> path
      | ((category, path), count) <- Map.toAscList (counts membershipCategoryPaths),
        count /= 1
      ]
    emptyIdViolations =
      [ "Step-15 membership ledger contains an empty stable ID"
      | any null membershipSiteIds
      ]
    invalidStatusTargetViolations =
      [ "Step-15 membership ledger site "
          <> siteId
          <> " has incoherent status/target: "
          <> show status
          <> "/"
          <> show target
      | MembershipSite siteId _ status target _ _ <- membershipSites,
        not (validStatusTarget status target)
      ]
    unwitnessedWatchedIdentifierViolations =
      [ "Step-15 membership ledger watches an identifier without an anchor: "
          <> identifier
      | identifier <- Set.toAscList (Set.difference watchedMembershipIdentifiers anchoredIdentifiers)
      ]
    counts :: (Ord value) => [value] -> Map value Int
    counts = Map.fromListWith (+) . fmap (,1)

validStatusTarget :: MembershipMigrationStatus -> MembershipTarget -> Bool
validStatusTarget RetainedGenesisSource FixedGenesisBaseline = True
validStatusTarget RetainedGenesisSource Profile02P01 = True
validStatusTarget RetainedGenesisSource Profile02P04 = True
validStatusTarget Migrated target =
  target
    `elem` [ Step15Increment4,
             Step15Increment5,
             Step15Increment6,
             Step15Increment7,
             Step15Increment8,
             Step16Increment1,
             Step16Increment2,
             Step16Increment3,
             Step16Increment6,
             Step16Increment8,
             Profile02P02,
             Profile02P05,
             Profile02P06,
             Profile02P09,
             LabelControlBaseAdmission
           ]
validStatusTarget _ _ = False

membershipSiteIds :: [String]
membershipSiteIds =
  [ siteId
  | MembershipSite siteId _ _ _ _ _ <- membershipSites
  ]

membershipCategoryPaths :: [(MembershipSiteCategory, FilePath)]
membershipCategoryPaths =
  [ (category, path)
  | MembershipSite _ category _ _ path _ <- membershipSites
  ]

ledgerAnchorPairs :: Set (FilePath, String)
ledgerAnchorPairs =
  Set.fromList
    [ (path, identifier)
    | MembershipSite _ _ _ _ path anchors <- membershipSites,
      MembershipAnchor identifier _ <- NonEmpty.toList anchors
    ]

anchoredIdentifiers :: Set String
anchoredIdentifiers = Set.map snd ledgerAnchorPairs

watchedMembershipIdentifiers :: Set String
watchedMembershipIdentifiers =
  Set.fromList
    [ "historyLineage",
      "heraldLaneGenerationAuthorized",
      "statusMembershipHistory",
      "failureCheckedMembershipHistory",
      "oracleCheckedMembershipHistory",
      "validateHistoryLineage",
      "HeraldMembershipHistory",
      "HeraldMembershipLineage",
      "oracleViewHeraldMembershipHistoryChecked",
      "heraldMembershipLineage",
      "heraldMembershipLineageFrom",
      "projectStructuralVersionVector",
      "establishStructuralAppliedMembership",
      "MembershipBaseClosure",
      "MemberSetDigest",
      "OracleHelloDto",
      "PeerHelloDto",
      "StructuralVersionVector",
      "activeHeralds",
      "awaitingPeerProgress",
      "checkedActiveHeraldEpochs",
      "checkedActiveHeralds",
      "checkedOracleActiveHeralds",
      "deploymentActiveHeralds",
      "deriveMemberSetDigest",
      "destinationOutcomes",
      "fixedMembership",
      "labelMembershipTransitionPending",
      "memberSetDigest",
      "oracleGenesisActiveHeralds",
      "peerPublications",
      "pendingGenerationEvidence",
      "prepareAlignmentLossAfterMembershipRetirement",
      "preparePendingGenerationEvidenceRetirement",
      "prepareRetirementClosure",
      "remoteMarkers",
      "retiredPlacementCoordinates",
      "stateActiveHeralds",
      "structuralAppliedFixedMembership"
    ]

anchor :: String -> Int -> MembershipAnchor
anchor = MembershipAnchor

site ::
  String ->
  MembershipSiteCategory ->
  MembershipMigrationStatus ->
  MembershipTarget ->
  FilePath ->
  MembershipAnchor ->
  [MembershipAnchor] ->
  MembershipSite
site siteId category status target path firstAnchor remainingAnchors =
  MembershipSite siteId category status target path (firstAnchor :| remainingAnchors)

-- Keep this registry in stable-ID order. The human-readable obligations and
-- rationale keyed by these IDs live in docs/verification/membership-ledger.md.
membershipSites :: [MembershipSite]
membershipSites =
  [ site "MEM-GEN-001" GenesisMembershipRead RetainedGenesisSource FixedGenesisBaseline "domain/src/Eclips/Domain/Startup.hs" (anchor "oracleGenesisActiveHeralds" 1) [],
    site "MEM-GEN-003" GenesisMembershipRead RetainedGenesisSource FixedGenesisBaseline "herald-core/internal/Eclips/Herald/Genesis/Internal.hs" (anchor "oracleGenesisActiveHeralds" 1) [anchor "deploymentActiveHeralds" 3, anchor "checkedActiveHeralds" 5, anchor "checkedActiveHeraldEpochs" 3],
    site "MEM-GEN-004" GenesisMembershipRead Migrated Step15Increment5 "herald-core/src/Eclips/Herald/Initialization.hs" (anchor "checkedActiveHeralds" 2) [anchor "checkedActiveHeraldEpochs" 4, anchor "oracleViewCurrentHeraldMembership" 1],
    -- Portable failure-base admission checks a retained supersession against
    -- the already admitted history, alongside the ordinary historical view.
    site "MEM-GEN-005" GenesisMembershipRead Migrated Profile02P05 "herald-core/internal/Eclips/Herald/OracleProjection/State.hs" (anchor "checkedActiveHeralds" 2) [anchor "checkedOracleActiveHeralds" 1, anchor "oracleViewCurrentHeraldMembership" 4, anchor "prepareMembershipAdvance" 3, anchor "HeraldMembershipHistory" 7, anchor "HeraldMembershipLineage" 2, anchor "oracleViewHeraldMembershipHistoryChecked" 5, anchor "heraldMembershipLineage" 2],
    site "MEM-GEN-006" GenesisMembershipRead Migrated Profile02P02 "herald-core/internal/Eclips/Herald/ConfiguredProcess/State.hs" (anchor "heraldMembershipGenerationActiveHeraldEpochs" 0) [],
    site "MEM-GEN-007" GenesisMembershipRead Migrated Profile02P06 "herald-core/internal/Eclips/Herald/Publication/Route.hs" (anchor "heraldMembershipGenerationActiveHeraldEpochs" 2) [anchor "extendPredefinedSystemViews" 4, anchor "extendStructuralSystemViews" 3],
    site "MEM-GEN-008" GenesisMembershipRead Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Startup/Invariant.hs" (anchor "checkedActiveHeralds" 3) [anchor "checkedActiveHeraldEpochs" 3, anchor "oracleViewCurrentHeraldMembership" 5, anchor "oracleClientWitnessMembershipAdvances" 1, anchor "peerStreamPeerRetired" 2, anchor "oracleViewHeraldMembershipHistoryChecked" 2, anchor "historyLineage" 1],
    site "MEM-GEN-009" GenesisMembershipRead Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs" (anchor "oracleViewCurrentHeraldMembership" 1) [],
    site "MEM-GEN-010" GenesisMembershipRead RetainedGenesisSource FixedGenesisBaseline "oracle-core/src/Eclips/Oracle/Genesis.hs" (anchor "checkedOracleActiveHeralds" 3) [],
    site "MEM-GEN-011" GenesisMembershipRead Migrated Profile02P09 "oracle-core/src/Eclips/Oracle/Internal/Label.hs" (anchor "checkedOracleActiveHeralds" 3) [anchor "stateActiveHeralds" 11, anchor "HeraldMembershipHistory" 4, anchor "oracleCheckedMembershipHistory" 5, anchor "failureCheckedMembershipHistory" 3],
    site "MEM-GEN-012" GenesisMembershipRead Migrated Step15Increment8 "oracle-runtime/src/Eclips/Oracle/Runtime/ConformanceClient.hs" (anchor "checkedOracleActiveHeralds" 2) [],
    site "MEM-GEN-013" GenesisMembershipRead Migrated Profile02P06 "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs" (anchor "checkedOracleActiveHeralds" 0) [],
    site "MEM-GEN-014" GenesisMembershipRead Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/Discovery/Internal.hs" (anchor "activeHeralds" 0) [anchor "heraldCatalogue" 2, anchor "initialMembership" 3],
    -- Checked base admission binds immutable genesis and the applicant's exact
    -- pending predecessor before installing current global discovery facts.
    site "MEM-GEN-015" GenesisMembershipRead Migrated LabelControlBaseAdmission "herald-core/internal/Eclips/Herald/Discovery/State.hs" (anchor "currentMembership" 3) [anchor "prepareMembershipAdvance" 3, anchor "HeraldMembershipHistory" 2],
    site "MEM-GEN-016" GenesisMembershipRead Migrated Step15Increment4 "oracle-core/src/Eclips/Oracle/Step15/Reference.hs" (anchor "checkedOracleActiveHeralds" 2) [],
    site "MEM-GEN-017" GenesisMembershipRead Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/UseCase/AlignmentTransfer.hs" (anchor "currentActiveHeralds" 7) [anchor "heraldMembershipGenerationActiveHeraldEpochs" 2, anchor "oracleViewCurrentHeraldMembership" 2],
    site "MEM-GEN-018" GenesisMembershipRead Migrated Profile02P05 "oracle-core/src/Eclips/Oracle/Internal/Failure.hs" (anchor "checkedOracleActiveHeralds" 2) [anchor "HeraldMembershipHistory" 4, anchor "failureCheckedMembershipHistory" 3],
    site "MEM-GEN-019" GenesisMembershipRead RetainedGenesisSource Profile02P01 "examples/hello-world/FounderConfiguration.hs" (anchor "deploymentActiveHeralds" 1) [anchor "oracleGenesisActiveHeralds" 1],
    site "MEM-GEN-020" GenesisMembershipRead RetainedGenesisSource Profile02P01 "examples/hello-world/FounderProperties.hs" (anchor "checkedOracleActiveHeralds" 4) [],
    site "MEM-GEN-021" GenesisMembershipRead RetainedGenesisSource Profile02P04 "deployment/src/Eclips/Deployment/Manifest.hs" (anchor "deploymentActiveHeralds" 1) [anchor "oracleGenesisActiveHeralds" 1],
    site "MEM-GEN-022" GenesisMembershipRead RetainedGenesisSource Profile02P04 "examples/hello-world/FounderDeployment.hs" (anchor "checkedOracleActiveHeralds" 2) [],
    site "MEM-GEN-023" GenesisMembershipRead RetainedGenesisSource FixedGenesisBaseline "oracle-core/src/Eclips/Oracle/Internal/Admission.hs" (anchor "checkedOracleActiveHeralds" 2) [],
    site "MEM-CUT-001" MembershipVectorOrCut Migrated Profile02P06 "domain/src/Eclips/Domain/Structural.hs" (anchor "StructuralVersionVector" 23) [anchor "MemberSetDigest" 8, anchor "deriveMemberSetDigest" 2, anchor "HeraldMembershipGenerationId" 6, anchor "HeraldMembershipLineage" 2, anchor "projectStructuralVersionVector" 3],
    site "MEM-CUT-002" MembershipVectorOrCut Migrated Profile02P06 "domain/src/Eclips/Domain/Topology.hs" (anchor "StructuralVersionVector" 20) [anchor "MemberSetDigest" 11, anchor "deriveMemberSetDigest" 2, anchor "memberSetDigest" 0, anchor "HeraldMembershipGenerationId" 24, anchor "HeraldMembershipLineage" 2],
    site "MEM-CUT-003" MembershipVectorOrCut Migrated Step15Increment6 "domain/src/Eclips/Domain/Alignment.hs" (anchor "fixedMembership" 0) [anchor "MemberSetDigest" 8, anchor "deriveMemberSetDigest" 2, anchor "HeraldMembershipGenerationId" 6],
    site "MEM-CUT-004" MembershipVectorOrCut Migrated Step15Increment4 "domain/src/Eclips/Domain/MemberSet.hs" (anchor "MemberSetDigest" 9) [anchor "deriveMemberSetDigest" 3],
    site "MEM-CUT-005" MembershipVectorOrCut Migrated Profile02P06 "domain/src/Eclips/Domain/Membership.hs" (anchor "MemberSetDigest" 6) [anchor "deriveMemberSetDigest" 5, anchor "HeraldMembershipHistory" 19, anchor "HeraldMembershipLineage" 14, anchor "heraldMembershipLineage" 3, anchor "heraldMembershipLineageFrom" 3],
    site "MEM-CUT-006" MembershipVectorOrCut Migrated Step15Increment6 "domain/src/Eclips/Domain/Label.hs" (anchor "MemberSetDigest" 11) [anchor "deriveMemberSetDigest" 0, anchor "HeraldMembershipGenerationId" 5],
    site "MEM-CUT-007" MembershipVectorOrCut Migrated Profile02P06 "herald-core/internal/Eclips/Herald/Graph/Progress.hs" (anchor "StructuralVersionVector" 21) [anchor "structuralAppliedFixedMembership" 2, anchor "MemberSetDigest" 5, anchor "deriveMemberSetDigest" 0, anchor "memberSetDigest" 0, anchor "HeraldMembershipGenerationId" 15, anchor "HeraldMembershipHistory" 4, anchor "HeraldMembershipLineage" 5, anchor "heraldMembershipLineage" 7, anchor "establishStructuralAppliedMembership" 1],
    site "MEM-CUT-008" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Graph/Protocol.hs" (anchor "StructuralVersionVector" 5) [anchor "MemberSetDigest" 2, anchor "memberSetDigest" 0, anchor "HeraldMembershipGenerationId" 5],
    site "MEM-CUT-009" MembershipVectorOrCut Migrated Step15Increment6 "herald-core/internal/Eclips/Herald/Peer/RPC/Internal.hs" (anchor "StructuralVersionVector" 2) [anchor "MemberSetDigest" 4, anchor "HeraldMembershipGenerationId" 4],
    site "MEM-CUT-010" MembershipVectorOrCut Migrated Step15Increment6 "herald-core/internal/Eclips/Herald/PeerPublication.hs" (anchor "StructuralVersionVector" 4) [],
    site "MEM-CUT-011" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Publication/State.hs" (anchor "StructuralVersionVector" 4) [anchor "HeraldMembershipLineage" 2],
    site "MEM-CUT-012" MembershipVectorOrCut Migrated Profile02P06 "herald-core/internal/Eclips/Herald/Structural/Reconciliation.hs" (anchor "StructuralVersionVector" 47) [anchor "HeraldMembershipLineage" 6, anchor "HeraldMembershipHistory" 2, anchor "heraldMembershipLineage" 1, anchor "fixedMembership" 0, anchor "structuralAppliedFixedMembership" 3, anchor "MemberSetDigest" 4, anchor "memberSetDigest" 10, anchor "HeraldMembershipGenerationId" 10, anchor "heraldMembershipLineageFrom" 4, anchor "projectStructuralVersionVector" 3, anchor "establishStructuralAppliedMembership" 3, anchor "prepareNablaProjectionsAt" 3],
    site "MEM-CUT-013" MembershipVectorOrCut Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/LabelBarrier/State.hs" (anchor "MemberSetDigest" 4) [anchor "deriveMemberSetDigest" 0, anchor "memberSetDigest" 2],
    site "MEM-CUT-014" MembershipVectorOrCut Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/PeerPayload.hs" (anchor "MemberSetDigest" 0) [],
    site "MEM-CUT-015" MembershipVectorOrCut Migrated Step15Increment8 "oracle-core/src/Eclips/Oracle/Internal/Label.hs" (anchor "MemberSetDigest" 9) [anchor "deriveMemberSetDigest" 2],
    site "MEM-CUT-016" MembershipVectorOrCut Migrated Step15Increment8 "oracle-core/src/Eclips/Oracle/Internal/LabelCanonical.hs" (anchor "MemberSetDigest" 3) [],
    site "MEM-CUT-017" MembershipVectorOrCut Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/OracleClient/State.hs" (anchor "oracleClientWitnessMembershipAdvances" 6) [anchor "prepareMembershipCursorAdvance" 3],
    site "MEM-CUT-018" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Graph/TerminalSource.hs" (anchor "StructuralVersionVector" 40) [anchor "HeraldMembershipGenerationId" 56, anchor "HeraldMembershipLineage" 46, anchor "heraldMembershipLineageFrom" 3, anchor "MembershipBaseClosure" 8, anchor "historyLineage" 27, anchor "validateHistoryLineage" 7, anchor "heraldMembershipLineage" 2],
    site "MEM-CUT-019" MembershipVectorOrCut Migrated Step15Increment6 "herald-core/internal/Eclips/Herald/Placement/State.hs" (anchor "HeraldMembershipGeneration" 2) [anchor "currentPhysicalPlacementRevisionVector" 3, anchor "physicalPlacementRevisionVector" 2],
    site "MEM-CUT-020" MembershipVectorOrCut Migrated Step15Increment6 "herald-core/internal/Eclips/Herald/UseCase/Alignment.hs" (anchor "oracleViewCurrentHeraldMembership" 6) [anchor "currentPhysicalPlacementRevisionVector" 1],
    site "MEM-CUT-021" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/Step15StructuralBase.hs" (anchor "StructuralVersionVector" 4) [anchor "HeraldMembershipGenerationId" 6, anchor "oracleViewCurrentHeraldMembership" 0, anchor "HeraldMembershipLineage" 10, anchor "MembershipBaseClosure" 56, anchor "historyLineage" 9],
    site "MEM-CUT-022" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs" (anchor "prepareRetirementClosure" 3) [anchor "oracleViewCurrentHeraldMembership" 2, anchor "heraldMembershipGenerationPredecessor" 0, anchor "structuralBaseSuccessorMembership" 4, anchor "advanceRetirementClosureWork" 3, anchor "MembershipBaseClosure" 5],
    site "MEM-CUT-023" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/TerminalSourceHold/State.hs" (anchor "retainAheadControl" 3) [anchor "resolveForMembershipAdvance" 3, anchor "HeraldMembershipHistory" 12, anchor "heraldMembershipLineage" 2],
    site "MEM-CUT-024" MembershipVectorOrCut Migrated Step16Increment1 "domain/src/Eclips/Domain/Disappearance.hs" (anchor "MemberSetDigest" 4) [anchor "HeraldMembershipGenerationId" 4],
    site "MEM-CUT-025" MembershipVectorOrCut Migrated Step16Increment2 "herald-core/internal/Eclips/Herald/Application/Environment.hs" (anchor "MemberSetDigest" 7) [anchor "HeraldMembershipGenerationId" 7],
    site "MEM-CUT-026" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/StructuralSettlement.hs" (anchor "structuralVersionVectorMembershipGenerationId" 4) [anchor "structuralVersionVectorMemberSetDigest" 4, anchor "structuralBaseSuccessorMembership" 0, anchor "successorStructuralBaseGenerationId" 0, anchor "MemberSetDigest" 2],
    site "MEM-CUT-027" MembershipVectorOrCut Migrated Step16Increment6 "herald-core/internal/Eclips/Herald/Graph/DisappearanceReadiness.hs" (anchor "MemberSetDigest" 12) [anchor "HeraldMembershipGenerationId" 12],
    site "MEM-CUT-028" MembershipVectorOrCut Migrated Profile02P06 "herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs" (anchor "StructuralVersionVector" 2) [anchor "structuralVersionVectorMembershipGenerationId" 9, anchor "HeraldMembershipGenerationId" 7],
    site "MEM-CUT-029" MembershipVectorOrCut Migrated Profile02P05 "herald-core/src/Eclips/Herald/Transition.hs" (anchor "oracleViewHeraldMembershipHistoryChecked" 1) [],
    site "MEM-CUT-030" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/OracleProjection/Step15.hs" (anchor "HeraldMembershipHistory" 2) [],
    site "MEM-CUT-031" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Startup/State.hs" (anchor "MembershipBaseClosure" 3) [],
    site "MEM-CUT-032" MembershipVectorOrCut Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/TerminalStructuralStart.hs" (anchor "MembershipBaseClosure" 2) [anchor "historyLineage" 3, anchor "oracleViewHeraldMembershipHistoryChecked" 1],
    site "MEM-CUT-033" MembershipVectorOrCut Migrated Profile02P05 "oracle-core/src/Eclips/Oracle/State.hs" (anchor "oracleCheckedMembershipHistory" 2) [],
    site "MEM-CUT-034" MembershipVectorOrCut Migrated Profile02P06 "herald-core/internal/Eclips/Herald/Join/History.hs" (anchor "StructuralVersionVector" 1) [anchor "heraldMembershipLineage" 1, anchor "oracleViewHeraldMembershipHistoryChecked" 1],
    -- The composed base reads the checked receiver projection's history for
    -- Discovery admission; this does not authorize mutation or control replay.
    site "MEM-CUT-035" MembershipVectorOrCut Migrated LabelControlBaseAdmission "herald-core/internal/Eclips/Herald/UseCase/ControlBase.hs" (anchor "oracleViewHeraldMembershipHistoryChecked" 2) [],
    site "MEM-WAIT-001" UnavailableMemberWait Migrated Step15Increment8 "oracle-core/src/Eclips/Oracle/Internal/Label.hs" (anchor "oracleLabelCollector" 4) [anchor "liveDecisionCapturedHeralds" 5],
    site "MEM-WAIT-002" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/Graph/Progress.hs" (anchor "structuralReportEntries" 3) [anchor "prepareTopologyCutAcceptance" 3, anchor "prepareTopologyCutEstablishedAck" 3],
    site "MEM-WAIT-003" UnavailableMemberWait Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/LabelBarrier/State.hs" (anchor "remoteMarkers" 0) [],
    site "MEM-WAIT-004" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/PeerStream/State.hs" (anchor "awaitingPeerProgress" 28) [],
    site "MEM-WAIT-005" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/Alignment/State.hs" (anchor "pendingGenerationEvidence" 19) [anchor "preparePendingGenerationEvidenceRetirement" 3],
    site "MEM-WAIT-006" UnavailableMemberWait Migrated Profile02P05 "herald-core/internal/Eclips/Herald/Publication/State.hs" (anchor "destinationOutcomes" 20) [anchor "peerPublications" 14, anchor "activeHeralds" 3, anchor "SuccessorStructuralBaseDependency" 5, anchor "dependenciesCanAdvance" 3],
    site "MEM-WAIT-007" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs" (anchor "destinationOutcomes" 2) [],
    site "MEM-WAIT-008" UnavailableMemberWait Migrated Profile02P06 "herald-core/internal/Eclips/Herald/UseCase/StructuralProgress.hs" (anchor "peerPublications" 3) [anchor "StructuralVersionVector" 2, anchor "activeHeralds" 2],
    site "MEM-WAIT-009" UnavailableMemberWait Migrated Step15Increment5 "oracle-core/src/Eclips/Oracle/Step15/WorkflowReference.hs" (anchor "MemberSetDigest" 4) [anchor "applyReferenceWorkflowRetirement" 3],
    site "MEM-WAIT-010" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs" (anchor "labelMembershipTransitionPending" 3) [anchor "oracleViewCurrentHeraldMembershipId" 1, anchor "structuralProgressMembershipGenerationId" 1],
    site "MEM-WAIT-011" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/Alignment/Loss.hs" (anchor "prepareAlignmentLossAfterMembershipRetirement" 3) [anchor "DeferUntilSuccessorStructuralBase" 4],
    site "MEM-WAIT-012" UnavailableMemberWait Migrated Step15Increment7 "herald-core/internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs" (anchor "preparePendingGenerationEvidenceRetirement" 1) [anchor "retiredPlacementCoordinates" 3, anchor "prepareAlignmentLossAfterMembershipRetirement" 1, anchor "deliverAlignmentLossTransferDelta" 1],
    site "MEM-WAIT-013" UnavailableMemberWait Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs" (anchor "preparePendingGenerationEvidenceRetirement" 1) [anchor "prepareAlignmentLossAfterMembershipRetirement" 1],
    site "MEM-WAIT-014" UnavailableMemberWait Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/UseCase/LabelCollection.hs" (anchor "oracleViewHeraldMembershipHistoryChecked" 1) [],
    site "MEM-PROTO-001" ProtocolProducerConsumer Migrated Step15Increment8 "protocol-peer/src/Eclips/Protocol/Peer/Types.hs" (anchor "PeerHelloDto" 29) [],
    site "MEM-PROTO-002" ProtocolProducerConsumer Migrated Step15Increment8 "protocol-peer/src/Eclips/Protocol/Peer/Codec.hs" (anchor "encodePeerEnvelope" 3) [anchor "decodePeerEnvelope" 3],
    site "MEM-PROTO-003" ProtocolProducerConsumer Migrated Step15Increment8 "protocol-peer/src/Eclips/Protocol/Peer/Frame.hs" (anchor "encodePeerFrame" 3) [anchor "feedPeerFrame" 3],
    site "MEM-PROTO-004" ProtocolProducerConsumer Migrated Step15Increment8 "herald-core/internal/Eclips/Herald/Peer/RPC/Internal.hs" (anchor "PeerHelloDto" 2) [],
    site "MEM-PROTO-005" ProtocolProducerConsumer Migrated Profile02P09 "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs" (anchor "OracleHelloDto" 24) [],
    site "MEM-PROTO-006" ProtocolProducerConsumer Migrated Step15Increment8 "protocol-oracle/src/Eclips/Protocol/Oracle/Codec.hs" (anchor "encodeOracleEnvelope" 3) [anchor "decodeOracleEnvelope" 3],
    site "MEM-PROTO-007" ProtocolProducerConsumer Migrated Step15Increment8 "protocol-oracle/src/Eclips/Protocol/Oracle/Frame.hs" (anchor "encodeOracleFrame" 3) [anchor "feedOracleFrame" 4],
    site "MEM-PROTO-008" ProtocolProducerConsumer Migrated Step15Increment8 "oracle-runtime/src/Eclips/Oracle/Runtime/ConformanceClient.hs" (anchor "OracleHelloDto" 1) [],
    site "MEM-PROTO-009" ProtocolProducerConsumer Migrated Profile02P09 "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs" (anchor "OracleHelloDto" 3) [],
    site "MEM-PROTO-010" ProtocolProducerConsumer Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/Peer/Step15.hs" (anchor "membershipPeerHelloGeneration" 3) [anchor "directFailureProbeRequestGeneration" 3],
    site "MEM-PROTO-011" ProtocolProducerConsumer Migrated Step15Increment8 "herald-core/src/Eclips/Herald/EffectBatch.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-PROTO-012" ProtocolProducerConsumer Migrated Step15Increment8 "herald-core/src/Eclips/Herald/Input.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-PROTO-013" ProtocolProducerConsumer Migrated Step15Increment8 "herald-core/src/Eclips/Herald/Peer/RPC.hs" (anchor "MemberSetDigest" 3) [],
    site "MEM-PROTO-014" ProtocolProducerConsumer Migrated Profile02P05 "herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs" (anchor "MemberSetDigest" 2) [anchor "HeraldMembershipHistory" 2, anchor "HeraldMembershipLineage" 2, anchor "oracleViewHeraldMembershipHistoryChecked" 2, anchor "heraldMembershipLineage" 2, anchor "MembershipBaseClosure" 15],
    site "MEM-PROTO-015" ProtocolProducerConsumer Migrated Step15Increment8 "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-RUNTIME-001" RuntimeAdmission Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/Discovery/State.hs" (anchor "preparePeerHello" 3) [anchor "peerDialIntentFor" 4],
    site "MEM-RUNTIME-002" RuntimeAdmission Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs" (anchor "applyPeerHello" 3) [anchor "oracleViewIsActiveHerald" 2],
    site "MEM-RUNTIME-003" RuntimeAdmission Migrated Step15Increment5 "herald-core/src/Eclips/Herald/EffectBatch.hs" (anchor "ClosePeerBinding" 1) [anchor "CancelPeerDial" 1, anchor "CancelPeerDestination" 1],
    site "MEM-RUNTIME-004" RuntimeAdmission Migrated Step15Increment5 "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs" (anchor "submitPeerHelloIngress" 3) [anchor "ClosePeerBinding" 1, anchor "CancelPeerDial" 1, anchor "CancelPeerDestination" 1],
    site "MEM-RUNTIME-005" RuntimeAdmission Migrated Step15Increment5 "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs" (anchor "admitCandidatePeerEnvelope" 2) [anchor "retainConfiguredTargetsForHello" 4, anchor "cancelPeerDial" 3],
    site "MEM-RUNTIME-006" RuntimeAdmission Migrated Profile02P09 "herald-runtime/src/Eclips/Herald/Runtime/Oracle.hs" (anchor "oracleHelloDto" 2) [anchor "OracleHelloDto" 1],
    site "MEM-RUNTIME-007" RuntimeAdmission Migrated Step15Increment8 "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs" (anchor "oracleConnectHelloEnvelope" 2) [],
    site "MEM-RUNTIME-008" RuntimeAdmission Migrated Profile02P06 "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs" (anchor "authorizeHerald" 3) [anchor "HeraldMembershipHistory" 2, anchor "statusMembershipHistory" 2, anchor "heraldLaneGenerationAuthorized" 2],
    site "MEM-RUNTIME-009" RuntimeAdmission Migrated Step15Increment5 "herald-core/internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs" (anchor "prepareMembershipAdvance" 5) [anchor "ClosePeerBinding" 2, anchor "CancelPeerDial" 2, anchor "CancelPeerDestination" 2],
    site "MEM-RUNTIME-010" RuntimeAdmission Migrated Step15Increment8 "herald-runtime/src/Eclips/Herald/Runtime/Ingress.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-RUNTIME-011" RuntimeAdmission Migrated Step15Increment8 "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Coordination.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-RUNTIME-012" RuntimeAdmission Migrated Step15Increment8 "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Types.hs" (anchor "MemberSetDigest" 2) [],
    site "MEM-RUNTIME-013" RuntimeAdmission Migrated Profile02P09 "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Hello.hs" (anchor "HeraldMembershipHistory" 4) [anchor "heraldMembershipLineage" 2, anchor "heraldLaneGenerationAuthorized" 4],
    site "MEM-RUNTIME-014" RuntimeAdmission Migrated Profile02P05 "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Types.hs" (anchor "HeraldMembershipHistory" 2) [anchor "statusMembershipHistory" 1],
    site "MEM-RUNTIME-015" RuntimeAdmission Migrated Profile02P09 "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs" (anchor "oracleCheckedMembershipHistory" 2) [anchor "statusMembershipHistory" 3],
    site "MEM-RUNTIME-016" RuntimeAdmission Migrated Profile02P09 "oracle-runtime/src/Eclips/Oracle/Runtime.hs" (anchor "oracleCheckedMembershipHistory" 3) [anchor "statusMembershipHistory" 2]
  ]
