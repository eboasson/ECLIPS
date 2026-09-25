{-# LANGUAGE OverloadedRecordDot #-}

module PublicationGroupIntegrationProperties (tests) where

import ApplicationLabelProperties
  ( predecessorAdmittedApplicationPublicationFixture,
    settledNeutralControlledFixtureWithForwardRequest,
  )
import Control.Monad (forM_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Application.Types.Operation (ApplicationOperation (NewEnvironmentApplication))
import Eclips.Domain.Identity (GlobalObjectId, ProcessEpochId)
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Request.Internal (requestId)
import Eclips.Herald.Input (ApplicationRequestIngress (CallApplicationRequest))
import Eclips.Herald.Publication.Groups qualified as Groups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.State
  ( startupPublicationState,
  )
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import NewEnvironmentProperties qualified as NewEnvironment
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "publication-group integration"
    [ testCase "controlled first use is counted before structural stamping" caseStructuralFirstUse,
      testCase "structural dispatch preserves membership and settles the local stage" caseStructuralDispatch,
      testCase "forward attaches to the caller's self-publication group exactly once" caseForward,
      testCase "newenv registers each root despite their shared application position" caseEnvironmentRoots
    ]

caseStructuralFirstUse :: Assertion
caseStructuralFirstUse = do
  state <- predecessorAdmittedApplicationPublicationFixture
  let publication = startupPublicationState state
  record <- sole "first controlled structural publication" (map snd (Publication.applicationPublicationEntries publication))
  (process, object) <- controlledTarget record
  let key = Groups.groupKey process object
      groups = Publication.publicationGroups publication
  assertEqual "unsequenced source still belongs to the published object's group" (Set.singleton key) (Groups.membershipKeys (Publication.applicationPublicationGroupMembership record))
  assertEqual "first use remains locally pending" 1 (Groups.groupLocalPendingCount key groups)
  assertEqual "unstamped first use cannot escape the label eligibility check" 1 (Groups.groupUnstampedCount key groups)
  assertBool "accepted publication owns its pending token" (Groups.publicationPending (Publication.applicationPublicationId record) groups)
  assertBool "the group cannot report readiness before structural application" (not (Groups.groupReady key groups))

caseStructuralDispatch :: Assertion
caseStructuralDispatch = do
  predecessor <- predecessorAdmittedApplicationPublicationFixture
  record <- sole "structural publication before dispatch" (map snd (Publication.applicationPublicationEntries (startupPublicationState predecessor)))
  (process, object) <- controlledTarget record
  (successor, _) <- checked "advance accepted structural publication" (StructuralProgress.advanceReadyApplicationStructuralStages predecessor)
  after <- sole "structural publication after dispatch" (map snd (Publication.applicationPublicationEntries (startupPublicationState successor)))
  let key = Groups.groupKey process object
      groups = Publication.publicationGroups (startupPublicationState successor)
  assertEqual "stamping preserves accepted issuer and object membership" (Publication.applicationPublicationGroupMembership record) (Publication.applicationPublicationGroupMembership after)
  assertEqual "dispatch finishes the selected local stage" 0 (Groups.groupLocalPendingCount key groups)
  assertEqual "dispatch clears the unstamped eligibility dependency" 0 (Groups.groupUnstampedCount key groups)
  assertBool "remote delivery remains an obligation after local application" (Groups.groupPendingPeerCount key groups > 0)

caseForward :: Assertion
caseForward = do
  (predecessor, object, process, _, ingress) <- settledNeutralControlledFixtureWithForwardRequest
  let beforeIds = Set.fromList (map (Publication.applicationPublicationId . snd) (Publication.applicationPublicationEntries (startupPublicationState predecessor)))
  (accepted, _) <- checked "accept controlled forward" (ApplicationCall.applyApplicationRequest ingress predecessor)
  let publication = startupPublicationState accepted
      newRecords =
        [ record
        | (_, record) <- Publication.applicationPublicationEntries publication,
          Publication.applicationPublicationId record `Set.notMember` beforeIds
        ]
  record <- sole "one semantic forward" newRecords
  let key = Groups.groupKey process object
      groups = Publication.publicationGroups publication
  assertEqual "forward retains caller-specific self-publication membership" (Set.singleton key) (Groups.membershipKeys (Publication.applicationPublicationGroupMembership record))
  assertEqual "the application composer stamps and settles the ready forward" 0 (Groups.groupUnstampedCount key groups)
  assertBool "the settled forward no longer owns a local token" (not (Groups.publicationPending (Publication.applicationPublicationId record) groups))
  assertBool "the forwarded publication still awaits its remote destinations" (Groups.groupPendingPeerCount key groups > 0)
  (retried, _) <- checked "retry controlled forward" (ApplicationCall.applyApplicationRequest ingress accepted)
  assertEqual "retry does not add another group obligation" groups (Publication.publicationGroups (startupPublicationState retried))

caseEnvironmentRoots :: Assertion
caseEnvironmentRoots = do
  let fixture = NewEnvironment.fixture
      ingress = CallApplicationRequest (NewEnvironment.fixtureBinding fixture) (NewEnvironment.fixtureSession fixture) (requestId 1) NewEnvironmentApplication
  (accepted, _) <- checked "accept public newenv" (ApplicationCall.applyApplicationRequest ingress (NewEnvironment.fixtureState fixture))
  let publication = startupPublicationState accepted
      groups = Publication.publicationGroups publication
      manifests = Publication.publicationWitnessEnvironmentManifests (Publication.publicationStateWitness publication)
  manifest <- sole "one environment acceptance" manifests
  let roots = NonEmpty.toList (Environment.environmentManifestRoots manifest)
  assertEqual "the first phase accepts twelve independently tracked roots" 12 (length roots)
  assertEqual "the application composer settles all twelve local tokens" 0 (Groups.retainedPublicationCount groups)
  assertEqual "all twelve generated objects retain distinct remote groups" 12 (Groups.retainedGroupCount groups)
  forM_ roots $ \root -> do
    let plan = Environment.positionedEnvironmentRootPlan root
        key = Groups.groupKey (Environment.environmentManifestProcess manifest) (Environment.environmentRootPlanTargetObject plan)
    assertEqual "primordial source explicitly captures no sequencing object" Nothing (Environment.environmentRootPlanSourceSequencingObject plan)
    assertEqual "every generated root finishes local stamping" 0 (Groups.groupUnstampedCount key groups)
    assertEqual "every generated root retains its obligation to the fixture's one remote Herald" 1 (Groups.groupPendingPeerCount key groups)
  assertBool "the composed environment leaves valid group indexes" (Groups.valid groups)
  (retried, _) <- checked "retry public newenv" (ApplicationCall.applyApplicationRequest ingress accepted)
  assertEqual "manifest retry retains the exact publication groups" groups (Publication.publicationGroups (startupPublicationState retried))

controlledTarget :: Publication.ApplicationPublicationRecord -> IO (ProcessEpochId, GlobalObjectId)
controlledTarget record =
  maybe
    (assertFailure "expected controlled first use")
    pure
    ( Publication.applicationPublicationPossession
        (Publication.applicationPublicationSourceProcess record)
        (Publication.applicationPublicationRetention record)
    )

sole :: String -> [value] -> IO value
sole _ [value] = pure value
sole context values = assertFailure (context <> ": expected exactly one item, got " <> show (length values))

checked :: (Show problem) => String -> Either problem value -> IO value
checked _ (Right value) = pure value
checked context (Left problem) = assertFailure (context <> ": " <> show problem)
