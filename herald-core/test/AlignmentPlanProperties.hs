module AlignmentPlanProperties (tests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isLeft)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment qualified as Domain
import Eclips.Domain.Context qualified as Context
import Eclips.Domain.Graph qualified as Graph
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence qualified as Consequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Structural.Debt qualified as Debt
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, chooseInt, conjoin, counterexample, elements, forAll, shuffle, testProperty, vectorOf, (===))

tests :: TestTree
tests =
  testGroup
    "alignment plans"
    [ testProperty "carry agrees with an independent recursive context reference" propCarryReference,
      testProperty "member and graph presentation permutations preserve checked plans" propPresentation,
      testProperty "unrelated progress and Store revisions preserve immutable birth cuts without certificates" propUnrelatedProgress,
      testCase "topology and placement coordinates can advance independently" caseIndependentCoordinates,
      testCase "new incoming strength invalidates downstream history but preserves its source" caseDependencyCascade,
      testCase "adding and removing incoming relations changes only dependency-affected classes" caseRelationChanges,
      testCase "split and merge create new exact-member generations" caseSplitMerge,
      testCase "source incarnation and host replacement propagate to dependent classes" caseMemberReplacement,
      testCase "certificates gate created ancestry and do not gate carried classes" caseCertificateScope,
      testCase "membership and invalidated predecessor plans start fresh classes with the appropriate ancestry" caseNoCarryBoundaries,
      testCase "invalidated predecessor recovery does not wait for unavailable old certificates" caseInvalidatedWithoutCertificate,
      testCase "a different sort occurrence requires an explicit fresh plan lineage" caseSortOccurrence,
      testCase "same-coordinate successors are rejected even when every class would carry" caseSameCoordinate,
      testProperty "fresh attempts distinguish plans and generation births at unchanged coordinates" propAttemptIdentity,
      testProperty "reset plans create complete fresh bases without ancestry or certificate waits" propResetPlan,
      testCase "empty plans retain their coordinate and predecessor under fixed plan authority" caseEmptyPlans,
      testCase "topology placement mismatch is rejected for empty and otherwise all-carried plans" caseCoordinateMismatch,
      testCase "member admission remains exact for empty and nonempty classes" caseMemberAdmission,
      testCase "plan authority uses current fixed membership rather than class birth announcer" caseAnnouncer,
      testProperty "claim reconstruction accepts complete presentation permutations" propClaimPresentation,
      testProperty "announcements match exact carry create empty and recovery plan projections" propAnnouncementProjection,
      testCase "claim reconstruction rejects altered or incomplete binding and relation tables" caseClaimMismatch,
      testProperty "repeated cause coverage retains one plan without replaying created work" propLedgerCoverage,
      testCase "ledger requires predecessor retention and distinguishes plan from cause work" caseLedgerPredecessor,
      testCase "a matching predecessor ID cannot conceal a different checked predecessor" caseLedgerPredecessorConflict
    ]

-- Edges below are complete admitted graph projections. Changing a projection
-- models deletion and creation of fresh immutable topology object identities;
-- it does not model an in-place update to an edge's payload or strength.
propCarryReference :: Property
propCarryReference =
  forAll (elements graphProjections) $ \oldEdges ->
    forAll (elements graphProjections) $ \newEdges ->
      forAll (elements activeSets) $ \oldActive ->
        forAll (elements activeSets) $ \newActive ->
          forAll (vectorOf 4 (elements [False, True])) $ \replacedStores ->
            forAll (vectorOf 4 (elements [False, True])) $ \movedHosts ->
              let oldMembers = membersAt oldActive [] [] 0
                  newMembers = membersAt newActive replacedStores movedHosts 7
                  oldContext = context oldEdges oldActive
                  newContext = context newEdges newActive
                  previous = initial oldContext oldMembers
                  actual = successor 1 membership newContext newMembers previous Plan.AlignmentPredecessorUsable (certificates previous)
                  expected = referenceCarry oldContext oldMembers newContext newMembers
               in counterexample (show (oldEdges, newEdges, oldActive, newActive, bindingsView actual))
                    $ conjoin
                      [ carriedClasses actual === expected,
                        map fst (Plan.alignmentPlanBindings actual) === Context.contextGraphClasses newContext,
                        Plan.alignmentPlanPredecessor actual === Just (Plan.alignmentPlanId previous),
                        relationView actual === expectedRelations newContext actual,
                        all (bindingMatchesPrevious previous expected) (Plan.alignmentPlanBindings actual) === True,
                        Plan.alignmentPlanAnnouncer actual === expectedAnnouncer newContext
                      ]

-- Recursive evaluation over the condensed context DAG, independent of the
-- planner's predecessor selection, representative table, or fixed-point loop.
referenceCarry :: Context.ContextGraph -> [Generation.GenerationMemberInput] -> Context.ContextGraph -> [Generation.GenerationMemberInput] -> Set Context.ContextClass
referenceCarry oldContext oldMembers newContext newMembers =
  Set.fromList [contextClass | contextClass <- Context.contextGraphClasses newContext, survives contextClass]
  where
    survives contextClass =
      contextClass `elem` Context.contextGraphClasses oldContext
        && exactMembers oldMembers contextClass == exactMembers newMembers contextClass
        && incoming oldContext contextClass == incoming newContext contextClass
        && all (survives . fst) (Set.toList (incoming newContext contextClass))
    exactMembers members contextClass =
      sort
        [ (Generation.generationMemberDelta member, Generation.generationMemberStoreIncarnation member, Generation.generationMemberHerald member)
        | member <- members,
          Generation.generationMemberDelta member `elem` NonEmpty.toList (Context.contextClassMembers contextClass)
        ]
    incoming graph contextClass =
      Set.fromList
        [ (Context.contextRelationSource relation, Context.contextRelationStrength relation)
        | relation <- Context.contextGraphRelations graph,
          Context.contextRelationDestination relation == contextClass
        ]

propPresentation :: Property
propPresentation =
  forAll (elements graphProjections) $ \edges ->
    forAll (shuffle (membersAt allDeltas [] [] 0)) $ \shuffledMembers ->
      forAll (shuffle (edges <> edges)) $ \shuffledEdges ->
        forAll (shuffle (map Graph.DeltaVertex (allDeltas <> allDeltas))) $ \shuffledVertices ->
          let originalContext = context edges allDeltas
              shuffledContext = checked "permuted context" (Context.deriveContextGraph shuffledVertices shuffledEdges (reverse allDeltas))
              previous = initial originalContext (membersAt allDeltas [] [] 0)
              original = successor 1 membership originalContext (membersAt allDeltas [] [] 0) previous Plan.AlignmentPredecessorUsable Set.empty
              shuffled = successor 1 membership shuffledContext shuffledMembers previous Plan.AlignmentPredecessorUsable Set.empty
           in conjoin
                [ Plan.alignmentPlanId shuffled === Plan.alignmentPlanId original,
                  bindingsView shuffled === bindingsView original,
                  relationView shuffled === relationView original,
                  Plan.alignmentPlanPredecessor shuffled === Plan.alignmentPlanPredecessor original
                ]

propUnrelatedProgress :: Property
propUnrelatedProgress =
  forAll (chooseInt (1, 20)) $ \revision ->
    forAll (elements graphProjections) $ \edges ->
      let graph = context edges allDeltas
          previous = initial graph (membersAt allDeltas [] [] 0)
          supplied = membersAt allDeltas [] [] (fromIntegral revision)
          next = successor (fromIntegral revision) membership graph supplied previous Plan.AlignmentPredecessorUsable Set.empty
          oldCuts = Map.fromList [(contextClass, Domain.alignmentCutCanonicalBytes (Generation.alignmentGenerationCut generation)) | (contextClass, _, generation) <- bindingsView previous]
          nextCuts = Map.fromList [(contextClass, Domain.alignmentCutCanonicalBytes (Generation.alignmentGenerationCut generation)) | (contextClass, _, generation) <- bindingsView next]
       in conjoin
            [ carriedClasses next === Set.fromList (Context.contextGraphClasses graph),
              nextCuts === oldCuts,
              Plan.alignmentPlanTopologyCut next === topology (fromIntegral revision) membership,
              Topology.deriveTopologyCutId (Plan.alignmentPlanTopologyCut next) === Plan.alignmentPlanIdTopology (Plan.alignmentPlanId next),
              (Plan.alignmentPlanId next /= Plan.alignmentPlanId previous) === True
            ]

caseIndependentCoordinates :: Assertion
caseIndependentCoordinates = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
      next selectedTopology selectedPlacement = ready "independent coordinate" (Plan.prepareAlignmentPlan typedSort selectedTopology selectedPlacement graph standardMembers (Just (previous, Plan.AlignmentPredecessorUsable)) Set.empty)
      topologyOnly = next (topology 1 membership) (placement 1 membership)
      placementOnly = next (topology 0 membership) (placement 2 membership)
  mapM_ (assertCarried "independent route progress preserves all exact history" activeABC) [topologyOnly, placementOnly]
  assertEqual "unchanged topology is still the current plan evidence" (Plan.alignmentPlanTopologyCut previous) (Plan.alignmentPlanTopologyCut placementOnly)
  assertEqual "placement-only progress retains exact birth evidence" (generations previous) (generations placementOnly)

caseDependencyCascade :: Assertion
caseDependencyCascade = do
  let previous = initial (context chain activeABC) standardMembers
      next = successor 1 membership (context weakChain activeABC) standardMembers previous Plan.AlignmentPredecessorUsable (certificates previous)
  assertCarried "A's own incoming context is unchanged" [deltaA] next
  assertEqual "the preserved source retains its complete birth generation" (generationFor deltaA previous) (generationFor deltaA next)
  assertBool "C rotates even though only its upstream source's history changed" (generationIdFor deltaC previous /= generationIdFor deltaC next)
  let later = successor 2 membership (context weakChain activeABC) standardMembers next Plan.AlignmentPredecessorUsable Set.empty
  assertCarried "the next unchanged plan retains all three new history identities" activeABC later

caseRelationChanges :: Assertion
caseRelationChanges = do
  let disconnected = initial (context [] activeABC) standardMembers
      added = successor 1 membership (context [edge deltaA deltaB Graph.Preserve] activeABC) standardMembers disconnected Plan.AlignmentPredecessorUsable (certificates disconnected)
      removed = successor 2 membership (context [] activeABC) standardMembers added Plan.AlignmentPredecessorUsable (certificates added)
  assertCarried "new A to B changes only B" [deltaA, deltaC] added
  assertCarried "removing A to B changes only B" [deltaA, deltaC] removed
  assertBool "recreated incoming history cannot alias its old disconnected generation" (generationIdFor deltaB disconnected /= generationIdFor deltaB removed)

caseSplitMerge :: Assertion
caseSplitMerge = do
  let separate = initial (context [] activeABC) standardMembers
      merged = successor 1 membership (context mutualAB activeABC) standardMembers separate Plan.AlignmentPredecessorUsable (certificates separate)
      split = successor 2 membership (context [] activeABC) standardMembers merged Plan.AlignmentPredecessorUsable (certificates merged)
  assertCarried "merging A and B leaves only unrelated C" [deltaC] merged
  assertCarried "splitting AB creates both resulting classes" [deltaC] split
  assertBool "split A records the merged generation as ancestry" (generationIdFor deltaA merged `elem` Domain.alignmentCutPredecessorGenerationIds (Generation.alignmentGenerationCut (generationFor deltaA split)))

caseMemberReplacement :: Assertion
caseMemberReplacement = do
  let previous = initial (context chain activeABC) standardMembers
      replacements = [membersAt activeABC [True] [] 0, membersAt activeABC [] [True] 0]
  mapM_
    ( \members -> do
        let next = successor 1 membership (context chain activeABC) members previous Plan.AlignmentPredecessorUsable (certificates previous)
        assertCarried "exact source replacement changes its full downstream dependency cone" [] next
        assertEqual "a replaced incarnation is still represented by prior delta ancestry, not a fresh empty base" [] (Domain.alignmentCutFreshMemberBaseEvidence (Generation.alignmentGenerationCut (generationFor deltaA next)))
    )
    replacements

caseCertificateScope :: Assertion
caseCertificateScope = do
  let previous = initial (context [] activeABC) standardMembers
      changedContext = context [edge deltaA deltaB Graph.Preserve] activeABC
      result supplied = prepare 1 membership occurrence changedContext standardMembers (Just (previous, Plan.AlignmentPredecessorUsable)) supplied
      needed = Set.singleton (generationIdFor deltaB previous)
  case checked "unavailable created ancestry" (result Set.empty) of
    Plan.AlignmentPlanHeld missing -> assertEqual "only B's selected predecessor needs certification" needed missing
    Plan.AlignmentPlanReady _ -> assertBool "new B ancestry must wait" False
  let next = ready "one created predecessor certificate" (result needed)
  assertCarried "carried A and C do not need new certificate availability" [deltaA, deltaC] next
  assertEqual "extra evidence cannot change ancestry or carry decisions" (bindingsView next) (bindingsView (ready "all evidence" (result (certificates previous))))

caseNoCarryBoundaries :: Assertion
caseNoCarryBoundaries = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
  mapM_
    ( \(selectedMembership, status) -> do
        let next = successor 1 selectedMembership graph standardMembers previous status (certificates previous)
            oldGenerations = if status == Plan.AlignmentPredecessorInvalidated then [] else generations previous
            oldRelations = if status == Plan.AlignmentPredecessorInvalidated then [] else Plan.alignmentPlanRelations previous
            oldLaw = legacyReady (Generation.prepareAlignmentGenerationPlan typedSort (topology 1 selectedMembership) (placement 2 selectedMembership) graph standardMembers oldGenerations oldRelations (certificates previous))
        assertCarried "boundary starts fresh history generations" [] next
        assertEqual "created ancestry follows the existing generation law" (Set.fromList (map Generation.alignmentGenerationId (Generation.alignmentGenerationPlanGenerations oldLaw))) (certificates next)
    )
    [(widerMembership, Plan.AlignmentPredecessorUsable), (membership, Plan.AlignmentPredecessorInvalidated)]

caseInvalidatedWithoutCertificate :: Assertion
caseInvalidatedWithoutCertificate = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
      recovered = successor 1 membership graph standardMembers previous Plan.AlignmentPredecessorInvalidated Set.empty
  assertCarried "recovery creates all classes" [] recovered
  assertEqual "old plan remains the explicit frontier predecessor" (Just (Plan.alignmentPlanId previous)) (Plan.alignmentPlanPredecessor recovered)
  assertEqual "invalidated status is immutable plan evidence" Plan.AlignmentPredecessorInvalidated (Plan.alignmentPlanPredecessorStatus recovered)
  assertBool "no recovered class waits on lost old history" (all (null . Domain.alignmentCutPredecessorGenerationIds . Generation.alignmentGenerationCut) (generations recovered))
  assertEqual "subsequently arriving old certificates cannot change recovered history" recovered (successor 1 membership graph standardMembers previous Plan.AlignmentPredecessorInvalidated (certificates previous))

caseSortOccurrence :: Assertion
caseSortOccurrence = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
  assertBool "different occurrences cannot share a supplied predecessor lineage" (isLeft (prepare 1 membership otherOccurrence graph standardMembers (Just (previous, Plan.AlignmentPredecessorUsable)) (certificates previous)))
  let fresh = ready "new occurrence starts explicitly" (prepare 1 membership otherOccurrence graph standardMembers Nothing Set.empty)
  assertEqual "fresh occurrence has no predecessor plan" Nothing (Plan.alignmentPlanPredecessor fresh)
  assertCarried "fresh occurrence creates every class" [] fresh

caseSameCoordinate :: Assertion
caseSameCoordinate = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
  assertBool "a successor cannot masquerade as its own predecessor coordinate" (isLeft (prepare 0 membership occurrence graph standardMembers (Just (previous, Plan.AlignmentPredecessorUsable)) (certificates previous)))

propAttemptIdentity :: Property
propAttemptIdentity =
  forAll (chooseInt (0, 40)) $ \attemptNumber ->
    forAll (elements graphProjections) $ \edges ->
      let attempt = Domain.mkAlignmentPlanAttempt (fromIntegral attemptNumber)
          nextAttempt = Domain.nextAlignmentPlanAttempt attempt
          graph = context edges allDeltas
          members = membersAt allDeltas [] [] 0
          atAttempt selected predecessor =
            Plan.prepareAlignmentPlanAtAttempt
              selected
              (Debt.sortOccurrence sortId occurrence)
              (topology 0 membership)
              (placement 1 membership)
              graph
              members
              predecessor
              Set.empty
          previous = ready "identified initial attempt" (atAttempt attempt Nothing)
          replacement = ready "identified fresh attempt" (atAttempt nextAttempt (Just (previous, Plan.AlignmentPredecessorInvalidated)))
          ordinary = successor 1 membership graph members replacement Plan.AlignmentPredecessorUsable Set.empty
          empty selected = ready "empty identified attempt" (Plan.prepareAlignmentPlanAtAttempt selected (Debt.sortOccurrence sortId occurrence) (topology 0 membership) (placement 1 membership) (context [] []) [] Nothing Set.empty)
       in conjoin
            [ Plan.alignmentPlanTopologyCut previous === Plan.alignmentPlanTopologyCut replacement,
              Plan.alignmentPlanIdPlacement (Plan.alignmentPlanId previous) === Plan.alignmentPlanIdPlacement (Plan.alignmentPlanId replacement),
              (Plan.alignmentPlanId previous /= Plan.alignmentPlanId replacement) === True,
              Set.disjoint (certificates previous) (certificates replacement) === True,
              map (Domain.alignmentCutAttempt . Generation.alignmentGenerationCut) (generations replacement) === replicate (length (generations replacement)) nextAttempt,
              map Plan.alignmentGenerationBirthPlanId (generations replacement) === replicate (length (generations replacement)) (Plan.alignmentPlanId replacement),
              carriedClasses replacement === Set.empty,
              Plan.alignmentPlanIdAttempt (Plan.alignmentPlanId ordinary) === nextAttempt,
              certificates ordinary === certificates replacement,
              isLeft (prepare 0 membership occurrence graph members (Just (replacement, Plan.AlignmentPredecessorUsable)) Set.empty) === True,
              (Plan.alignmentPlanId (empty attempt) /= Plan.alignmentPlanId (empty nextAttempt)) === True
            ]

propResetPlan :: Property
propResetPlan =
  forAll (chooseInt (1, 40)) $ \attemptNumber ->
    forAll (elements graphProjections) $ \edges ->
      let attempt = Domain.mkAlignmentPlanAttempt (fromIntegral attemptNumber)
          graph = context edges allDeltas
          members = membersAt allDeltas [] [] 9
          prepareReset selected = Plan.prepareAlignmentResetPlan selected (Debt.sortOccurrence sortId occurrence) (topology 0 membership) (placement 1 membership) graph members
          reset = ready "fresh reset" (prepareReset attempt)
          cuts = map Generation.alignmentGenerationCut (generations reset)
          freshComplete cut = map Domain.freshMemberBaseDelta (Domain.alignmentCutFreshMemberBaseEvidence cut) == NonEmpty.toList (fmap Domain.alignmentMemberDelta (Domain.alignmentCutExactMembers cut))
       in conjoin
            [ Plan.alignmentPlanPredecessor reset === Nothing,
              Plan.alignmentPlanPredecessorStatus reset === Plan.AlignmentPredecessorReset,
              Plan.alignmentPlanIdAttempt (Plan.alignmentPlanId reset) === attempt,
              carriedClasses reset === Set.empty,
              all (null . Domain.alignmentCutPredecessorGenerationIds) cuts === True,
              all freshComplete cuts === True,
              length (Plan.alignmentPlanRelations reset) === length (Context.contextGraphRelations graph),
              prepareReset Domain.initialAlignmentPlanAttempt === Left Plan.AlignmentPlanResetShapeMismatch
            ]

caseEmptyPlans :: Assertion
caseEmptyPlans = do
  let previous = initial (context chain activeABC) standardMembers
      removed = successor 1 membership (context [] []) [] previous Plan.AlignmentPredecessorUsable Set.empty
      unchangedEmpty = successor 2 membership (context [] []) [] removed Plan.AlignmentPredecessorUsable Set.empty
  assertEqual "removal-only plan has no class bindings" [] (Plan.alignmentPlanBindings removed)
  assertEqual "removal-only plan has no relations" [] (Plan.alignmentPlanRelations removed)
  assertEqual "empty plan keeps the fixed plan authority" (Just heraldA) (Plan.alignmentPlanAnnouncer removed)
  assertEqual "empty plan still names its complete predecessor" (Just (Plan.alignmentPlanId previous)) (Plan.alignmentPlanPredecessor removed)
  assertEqual "successor to empty records the empty plan identity" (Just (Plan.alignmentPlanId removed)) (Plan.alignmentPlanPredecessor unchangedEmpty)
  assertBool "separate empty coordinates remain distinguishable" (Plan.alignmentPlanId removed /= Plan.alignmentPlanId unchangedEmpty)

caseCoordinateMismatch :: Assertion
caseCoordinateMismatch = do
  let graph = context chain activeABC
      previous = initial graph standardMembers
      mismatched contextGraph members predecessor = Plan.prepareAlignmentPlan typedSort (topology 1 membership) (placement 2 widerMembership) contextGraph members predecessor Set.empty
  assertBool "empty context cannot bypass full membership qualification" (isLeft (mismatched (context [] []) [] Nothing))
  assertBool "all-carried context cannot bypass full membership qualification" (isLeft (mismatched graph standardMembers (Just (previous, Plan.AlignmentPredecessorUsable))))

caseMemberAdmission :: Assertion
caseMemberAdmission = do
  let graph = context chain activeABC
  assertBool "missing exact physical member is rejected" (isLeft (prepare 0 membership occurrence graph (membersAt [deltaA, deltaB] [] [] 0) Nothing Set.empty))
  assertBool "duplicate physical member is rejected" (isLeft (prepare 0 membership occurrence graph (standardMembers <> take 1 standardMembers) Nothing Set.empty))
  assertBool "empty context rejects an unexpected physical member" (isLeft (prepare 0 membership occurrence (context [] []) (take 1 standardMembers) Nothing Set.empty))

caseAnnouncer :: Assertion
caseAnnouncer = do
  let members = membersAt activeABC [] (replicate 3 True) 0
      previous = initial (context [] activeABC) members
      next = successor 1 membership (context [] activeABC) members previous Plan.AlignmentPredecessorUsable Set.empty
  assertBool "all class birth announcers host members at B" (all ((== heraldB) . Generation.alignmentGenerationAnnouncer) (generations next))
  assertEqual "the plan is still coordinated by the least fixed member" (Just (min heraldA heraldB)) (Plan.alignmentPlanAnnouncer next)

propClaimPresentation :: Property
propClaimPresentation =
  let previous = initial (context chain activeABC) standardMembers
      expected = successor 1 membership (context chain activeABC) standardMembers previous Plan.AlignmentPredecessorUsable Set.empty
      Plan.AlignmentPlanClaim identifier predecessor predecessorStatus bindings relations = Plan.alignmentPlanClaim expected
   in forAll (shuffle bindings) $ \shuffledBindings ->
        forAll (shuffle relations) $ \shuffledRelations ->
          Plan.checkAlignmentPlanClaim expected (Plan.AlignmentPlanClaim identifier predecessor predecessorStatus shuffledBindings shuffledRelations) === Right expected

propAnnouncementProjection :: Property
propAnnouncementProjection =
  forAll (elements graphProjections) $ \oldEdges ->
    forAll (elements graphProjections) $ \newEdges ->
      forAll (elements activeSets) $ \active ->
        forAll (elements [Plan.AlignmentPredecessorUsable, Plan.AlignmentPredecessorInvalidated]) $ \status ->
          let members = membersAt active [] [] 8
              previous = initial (context oldEdges active) members
              next = successor 1 membership (context newEdges active) members previous status (certificates previous)
           in conjoin (map exact [previous, next])
  where
    exact plan =
      let announce = Plan.alignmentPlanAnnouncement plan
          bindings = Plan.alignmentPlanBindings plan
          bindingView binding = (Protocol.alignmentPlanBindingClaimMembers binding, Protocol.alignmentPlanBindingClaimDisposition binding, Protocol.alignmentPlanBindingClaimGeneration binding)
          relationClaim relation = (Protocol.alignmentPlanRelationClaimSource relation, Protocol.alignmentPlanRelationClaimDestination relation, Protocol.alignmentPlanRelationClaimStrength relation)
          expectedCuts = [Domain.alignmentCutCanonicalBytes (Generation.alignmentGenerationCut (Plan.alignmentPlanBindingGeneration binding)) | (_, binding) <- bindings, Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCreated]
       in conjoin
            [ Protocol.alignmentPlanAnnounceId announce === Plan.alignmentPlanId plan,
              Protocol.alignmentPlanAnnounceTopology announce === Plan.alignmentPlanTopologyCut plan,
              Protocol.alignmentPlanAnnouncePredecessor announce === Plan.alignmentPlanPredecessor plan,
              Protocol.alignmentPlanAnnouncePredecessorStatus announce === Plan.alignmentPlanPredecessorStatus plan,
              sort (map bindingView (Protocol.alignmentPlanAnnounceBindings announce)) === sort [(Context.contextClassMembers contextClass, Plan.alignmentPlanBindingDisposition binding, Generation.alignmentGenerationId (Plan.alignmentPlanBindingGeneration binding)) | (contextClass, binding) <- bindings],
              Set.fromList (map relationClaim (Protocol.alignmentPlanAnnounceRelations announce)) === relationView plan,
              sort (map (Domain.alignmentCutCanonicalBytes . Protocol.alignmentCutAnnounceCut) (Protocol.alignmentPlanAnnounceCreatedCuts announce)) === sort expectedCuts
            ]

caseClaimMismatch :: Assertion
caseClaimMismatch = do
  let previous = initial (context chain activeABC) standardMembers
      expected = successor 1 membership (context chain activeABC) standardMembers previous Plan.AlignmentPredecessorUsable Set.empty
      Plan.AlignmentPlanClaim identifier predecessor predecessorStatus bindings relations = Plan.alignmentPlanClaim expected
      claim = Plan.AlignmentPlanClaim identifier predecessor predecessorStatus
      otherClass = initial (context [] [deltaD]) (membersAt [deltaD] [] [] 0)
      Plan.AlignmentPlanClaim _ _ _ extraBindings _ = Plan.alignmentPlanClaim otherClass
      alterDisposition (contextClass, _, generation) = (contextClass, Plan.AlignmentCreated, generation)
      alterGeneration (contextClass, disposition, _) = (contextClass, disposition, generationIdFor deltaD otherClass)
      alterStrength relation = Generation.alignmentGenerationRelation (Generation.alignmentGenerationRelationSource relation) (Generation.alignmentGenerationRelationDestination relation) Route.Weak
      invalid =
        [ Plan.AlignmentPlanClaim (Plan.alignmentPlanId previous) predecessor predecessorStatus bindings relations,
          Plan.AlignmentPlanClaim identifier Nothing predecessorStatus bindings relations,
          Plan.AlignmentPlanClaim identifier predecessor Plan.AlignmentPredecessorInvalidated bindings relations,
          claim (drop 1 bindings) relations,
          claim (bindings <> take 1 bindings) relations,
          claim (bindings <> extraBindings) relations,
          claim (mapFirst alterDisposition bindings) relations,
          claim (mapFirst alterGeneration bindings) relations,
          claim bindings (drop 1 relations),
          claim bindings (relations <> take 1 relations),
          claim bindings (mapFirst alterStrength relations)
        ]
  mapM_ (\supplied -> assertBool ("rejects altered complete claim: " <> show supplied) (isLeft (Plan.checkAlignmentPlanClaim expected supplied))) invalid

propLedgerCoverage :: Property
propLedgerCoverage =
  forAll (chooseInt (1, 15)) $ \count ->
    forAll (vectorOf count (elements [cause 1, cause 2, cause 3])) $ \events ->
      let plan = initial (context chain activeABC) standardMembers
          (ledger, dispositions) = foldl (retain plan) (Plan.emptyAlignmentPlanLedger, []) events
          unique = Set.fromList events
       in conjoin
            [ Plan.alignmentPlanEntries ledger === [(Plan.alignmentPlanId plan, plan)],
              Plan.alignmentPlanCoverage ledger === [(event, Plan.alignmentPlanId plan) | event <- Set.toAscList unique],
              length (filter (== Plan.AlignmentPlanInserted) dispositions) === 1,
              length (filter (== Plan.AlignmentPlanCauseAdded) dispositions) === Set.size unique - 1,
              length (filter (== Plan.AlignmentPlanReplay) dispositions) === count - Set.size unique
            ]
  where
    retain plan (ledger, dispositions) event =
      let (next, disposition) = checked "retain generated coverage" (Plan.retainAlignmentPlan event plan ledger)
       in (next, disposition : dispositions)

caseLedgerPredecessor :: Assertion
caseLedgerPredecessor = do
  let previous = initial (context chain activeABC) standardMembers
      next = successor 1 membership (context chain activeABC) standardMembers previous Plan.AlignmentPredecessorUsable Set.empty
      (withPrevious, firstDisposition) = checked "retain predecessor" (Plan.retainAlignmentPlan (cause 1) previous Plan.emptyAlignmentPlanLedger)
      (withNext, nextDisposition) = checked "retain successor" (Plan.retainAlignmentPlan (cause 2) next withPrevious)
      (withCause, causeDisposition) = checked "additional successor coverage" (Plan.retainAlignmentPlan (cause 3) next withNext)
      (replayed, replayDisposition) = checked "exact successor replay" (Plan.retainAlignmentPlan (cause 3) next withCause)
  assertBool "an absent predecessor cannot be fabricated from its identity" (isLeft (Plan.retainAlignmentPlan (cause 2) next Plan.emptyAlignmentPlanLedger))
  assertEqual "first retention owns plan insertion" Plan.AlignmentPlanInserted firstDisposition
  assertEqual "successor owns one new plan insertion" Plan.AlignmentPlanInserted nextDisposition
  assertEqual "another cause owns no new plan work" Plan.AlignmentPlanCauseAdded causeDisposition
  assertEqual "exact replay is identified" Plan.AlignmentPlanReplay replayDisposition
  assertEqual "replay changes no ledger fact" withCause replayed
  assertEqual "three causes still retain only two complete plans" 2 (length (Plan.alignmentPlanEntries replayed))
  assertEqual "all three exact cause-plan facts survive" 3 (length (Plan.alignmentPlanCoverage replayed))

caseLedgerPredecessorConflict :: Assertion
caseLedgerPredecessorConflict = do
  let graph = context chain activeABC
      first = initial graph standardMembers
      conflicting = initial graph (membersAt activeABC [] [] 9)
      child = successor 1 membership graph (membersAt activeABC [] [] 12) conflicting Plan.AlignmentPredecessorUsable Set.empty
      (firstLedger, _) = checked "first checked origin" (Plan.retainAlignmentPlan (cause 1) first Plan.emptyAlignmentPlanLedger)
      (matchingLedger, _) = checked "alternate checked origin" (Plan.retainAlignmentPlan (cause 1) conflicting Plan.emptyAlignmentPlanLedger)
  assertEqual "coordinates alone coincide" (Plan.alignmentPlanId first) (Plan.alignmentPlanId conflicting)
  assertBool "captured fresh bases make different checked plan contents" (first /= conflicting)
  assertBool "same-coordinate unequal plan contents conflict" (isLeft (Plan.retainAlignmentPlan (cause 2) conflicting firstLedger))
  assertBool "a successor cannot use a different plan with the same predecessor ID" (isLeft (Plan.retainAlignmentPlan (cause 2) child firstLedger))
  assertEqual "the exact captured predecessor admits the child" (Right Plan.AlignmentPlanInserted) (snd <$> Plan.retainAlignmentPlan (cause 2) child matchingLedger)

mapFirst :: (value -> value) -> [value] -> [value]
mapFirst _ [] = []
mapFirst mapping (first : remaining) = mapping first : remaining

bindingMatchesPrevious :: Plan.AlignmentPlan -> Set Context.ContextClass -> (Context.ContextClass, Plan.AlignmentPlanBinding) -> Bool
bindingMatchesPrevious previous expected (contextClass, binding)
  | Set.member contextClass expected =
      Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCarried
        && (Plan.alignmentPlanBindingGeneration <$> lookup contextClass (Plan.alignmentPlanBindings previous)) == Just (Plan.alignmentPlanBindingGeneration binding)
  | otherwise =
      Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCreated
        && Set.notMember (Generation.alignmentGenerationId (Plan.alignmentPlanBindingGeneration binding)) (certificates previous)

bindingsView :: Plan.AlignmentPlan -> [(Context.ContextClass, Plan.AlignmentBindingDisposition, Generation.AlignmentGeneration)]
bindingsView plan = [(contextClass, Plan.alignmentPlanBindingDisposition binding, Plan.alignmentPlanBindingGeneration binding) | (contextClass, binding) <- Plan.alignmentPlanBindings plan]

carriedClasses :: Plan.AlignmentPlan -> Set Context.ContextClass
carriedClasses plan = Set.fromList [contextClass | (contextClass, binding) <- Plan.alignmentPlanBindings plan, Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCarried]

assertCarried :: String -> [Identity.DeltaId] -> Plan.AlignmentPlan -> Assertion
assertCarried message expected plan = assertEqual message (Set.fromList expected) (Set.fromList [delta | contextClass <- Set.toList (carriedClasses plan), delta <- NonEmpty.toList (Context.contextClassMembers contextClass)])

relationView :: Plan.AlignmentPlan -> Set (Domain.ContextClassGenerationId, Domain.ContextClassGenerationId, Route.ReplicaStrength)
relationView plan = Set.fromList [(Generation.alignmentGenerationRelationSource relation, Generation.alignmentGenerationRelationDestination relation, Generation.alignmentGenerationRelationStrength relation) | relation <- Plan.alignmentPlanRelations plan]

expectedRelations :: Context.ContextGraph -> Plan.AlignmentPlan -> Set (Domain.ContextClassGenerationId, Domain.ContextClassGenerationId, Route.ReplicaStrength)
expectedRelations graph plan =
  Set.fromList
    [ (identifier (Context.contextRelationSource relation), identifier (Context.contextRelationDestination relation), Context.contextRelationStrength relation)
    | relation <- Context.contextGraphRelations graph
    ]
  where
    identifier contextClass = Generation.alignmentGenerationId (Plan.alignmentPlanBindingGeneration (checkedLookup "relation class" contextClass (Map.fromList (Plan.alignmentPlanBindings plan))))

generations :: Plan.AlignmentPlan -> [Generation.AlignmentGeneration]
generations = map (Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings

certificates :: Plan.AlignmentPlan -> Set Domain.ContextClassGenerationId
certificates = Set.fromList . map Generation.alignmentGenerationId . generations

generationFor :: Identity.DeltaId -> Plan.AlignmentPlan -> Generation.AlignmentGeneration
generationFor delta plan = case [Plan.alignmentPlanBindingGeneration binding | (contextClass, binding) <- Plan.alignmentPlanBindings plan, delta `elem` NonEmpty.toList (Context.contextClassMembers contextClass)] of
  [generation] -> generation
  _ -> error "fixture delta is not in exactly one class"

generationIdFor :: Identity.DeltaId -> Plan.AlignmentPlan -> Domain.ContextClassGenerationId
generationIdFor delta = Generation.alignmentGenerationId . generationFor delta

initial :: Context.ContextGraph -> [Generation.GenerationMemberInput] -> Plan.AlignmentPlan
initial graph members = ready "initial plan" (prepare 0 membership occurrence graph members Nothing Set.empty)

successor :: Word64 -> Membership.HeraldMembershipGeneration -> Context.ContextGraph -> [Generation.GenerationMemberInput] -> Plan.AlignmentPlan -> Plan.AlignmentPredecessorStatus -> Set Domain.ContextClassGenerationId -> Plan.AlignmentPlan
successor index selectedMembership graph members previous status supplied = ready "successor plan" (prepare index selectedMembership occurrence graph members (Just (previous, status)) supplied)

prepare :: Word64 -> Membership.HeraldMembershipGeneration -> Identity.SortDefinitionOccurrenceId -> Context.ContextGraph -> [Generation.GenerationMemberInput] -> Maybe (Plan.AlignmentPlan, Plan.AlignmentPredecessorStatus) -> Set Domain.ContextClassGenerationId -> Either Plan.AlignmentPlanProblem Plan.AlignmentPlanPreparation
prepare index selectedMembership selectedOccurrence = Plan.prepareAlignmentPlan (Debt.sortOccurrence sortId selectedOccurrence) (topology index selectedMembership) (placement (index + 1) selectedMembership)

ready :: String -> Either Plan.AlignmentPlanProblem Plan.AlignmentPlanPreparation -> Plan.AlignmentPlan
ready label supplied = case checked label supplied of
  Plan.AlignmentPlanReady plan -> plan
  Plan.AlignmentPlanHeld missing -> error (label <> " unexpectedly held: " <> show missing)

legacyReady :: Either Generation.AlignmentGenerationProblem Generation.AlignmentGenerationPreparation -> Generation.AlignmentGenerationPlan
legacyReady supplied = case checked "legacy generation law" supplied of
  Generation.AlignmentGenerationReady plan -> plan
  Generation.AlignmentGenerationHeld missing -> error ("legacy fixture unexpectedly held: " <> show missing)

context :: [Graph.EdgePayload] -> [Identity.DeltaId] -> Context.ContextGraph
context edges active = checked "context" (Context.deriveContextGraph (map Graph.DeltaVertex allDeltas) edges active)

topology :: Word64 -> Membership.HeraldMembershipGeneration -> Topology.TopologyCut
topology index selectedMembership = checked "topology" (Topology.topologyCut (Topology.sameGenerationPredecessor predecessorCut) (Topology.topologyFrontier (Structural.emptyStructuralVersionVector selectedMembership) (Identity.controlIndex index)) (Topology.deriveTopologyOccurrenceDigest (ByteString.pack (show index))))

placement :: Word64 -> Membership.HeraldMembershipGeneration -> Domain.PhysicalPlacementRevisionVector
placement revision selectedMembership = checked "placement" (Domain.physicalPlacementRevisionVector selectedMembership [(herald, checked "placement revision" (Domain.mkPlacementRevision revision)) | herald <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs selectedMembership)])

expectedAnnouncer :: Context.ContextGraph -> Maybe Identity.HeraldEpoch
expectedAnnouncer _ = Just (min heraldA heraldB)

standardMembers :: [Generation.GenerationMemberInput]
standardMembers = membersAt activeABC [] [] 0

membersAt :: [Identity.DeltaId] -> [Bool] -> [Bool] -> Word64 -> [Generation.GenerationMemberInput]
membersAt active replaced moved revision =
  [ Generation.generationMemberInput delta (storeAt index changed) (if transferred then heraldB else heraldA) (Domain.storeRevisionFromWord64 revision)
  | (index, delta, changed, transferred) <- zipFour [0 :: Int ..] allDeltas (replaced <> repeat False) (moved <> repeat False),
    delta `elem` active
  ]
  where
    storeAt index changed = checked "store" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes (fromIntegral (40 + index + if changed then 10 else 0))))
    zipFour (index : indices) (delta : deltas) (changed : changes) (transferred : transfers) = (index, delta, changed, transferred) : zipFour indices deltas changes transfers
    zipFour _ _ _ _ = []

edge :: Identity.DeltaId -> Identity.DeltaId -> Graph.EdgeStrength -> Graph.EdgePayload
edge source destination = Graph.edgePayload (Graph.DeltaVertex source) (Graph.DeltaVertex destination)

chain, weakChain, mutualAB :: [Graph.EdgePayload]
chain = [edge deltaA deltaB Graph.Preserve, edge deltaB deltaC Graph.Preserve]
weakChain = [edge deltaA deltaB Graph.Weaken, edge deltaB deltaC Graph.Preserve]
mutualAB = [edge deltaA deltaB Graph.Preserve, edge deltaB deltaA Graph.Preserve]

graphProjections :: [[Graph.EdgePayload]]
graphProjections = [[], chain, weakChain, mutualAB, chain <> [edge deltaC deltaA Graph.Preserve], [edge deltaC deltaB Graph.Preserve, edge deltaB deltaA Graph.Weaken], chain <> [edge deltaD deltaA Graph.Preserve], [edge deltaA deltaD Graph.Preserve, edge deltaD deltaC Graph.Preserve]]

activeABC, allDeltas :: [Identity.DeltaId]
activeABC = [deltaA, deltaB, deltaC]
allDeltas = activeABC <> [deltaD]

activeSets :: [[Identity.DeltaId]]
activeSets = [[], activeABC, allDeltas, [deltaA, deltaB], [deltaA, deltaC], [deltaB, deltaC], [deltaD]]

membership, widerMembership :: Membership.HeraldMembershipGeneration
membership = checked "membership" (Membership.genesisHeraldMembershipGeneration system (heraldA :| [heraldB]))
widerMembership = checked "wider membership" (Membership.genesisHeraldMembershipGeneration system (heraldA :| [heraldB, heraldC]))

system :: Identity.SystemId
system = checked "system" (Identity.mkSystemId (fixtureIdentifierBytes 1))

sortId :: Identity.SortId
sortId = checked "sort" (Identity.mkSortId (fixtureIdentifierBytes 2))

typedSort :: Debt.SortOccurrence
typedSort = Debt.sortOccurrence sortId occurrence

cause :: Word64 -> Consequence.StructuralConsequenceCause
cause index = checked "control cause" (Consequence.processEndCause process (Identity.controlIndex index))
  where
    process = checked "ended process" (Identity.mkProcessEpochId (fixtureIdentifierBytes 15))

occurrence, otherOccurrence :: Identity.SortDefinitionOccurrenceId
occurrence = checked "occurrence" (Identity.mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 3))
otherOccurrence = checked "other occurrence" (Identity.mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 4))

deltaA, deltaB, deltaC, deltaD :: Identity.DeltaId
deltaA = checked "delta A" (Identity.mkDeltaId (fixtureIdentifierBytes 5))
deltaB = checked "delta B" (Identity.mkDeltaId (fixtureIdentifierBytes 6))
deltaC = checked "delta C" (Identity.mkDeltaId (fixtureIdentifierBytes 7))
deltaD = checked "delta D" (Identity.mkDeltaId (fixtureIdentifierBytes 8))

heraldA, heraldB, heraldC :: Identity.HeraldEpoch
heraldA = checked "Herald A" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 10))
heraldB = checked "Herald B" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 11))
heraldC = checked "Herald C" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 12))

predecessorCut :: Identity.TopologyCutId
predecessorCut = checked "predecessor cut" (Identity.mkTopologyCutId (fixtureIdentifierBytes 14))

checked :: (Show problem) => String -> Either problem value -> value
checked _ (Right value) = value
checked label (Left problem) = error (label <> ": " <> show problem)

checkedLookup :: (Ord key) => String -> key -> Map.Map key value -> value
checkedLookup label key = maybe (error (label <> " missing")) id . Map.lookup key
