module AdministrationCancellationProperties (tests) where

import ConfiguredProcessProperties (step)
import Data.ByteString qualified as Bytes
import Eclips.Application.Types.Lifecycle
import Eclips.Domain.Identity (ProcessEpochId)
import Eclips.Herald.Administration qualified as Admin
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis.Internal (checkedSystemId)
import Eclips.Herald.Input
import Eclips.Herald.OracleClient (OracleClientAction (SubmitOracleRequest))
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.State
import Eclips.Oracle.Transition (initialOracle)
import GenesisFixtures qualified as Genesis
import ProcessPreparationProperties qualified as Child
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "administrative prepared-child cancellation"
    [ testCase "cancellation before ordered Start seals, retries, and settles after ordered End" casePendingStart,
      testCase "administrator can cancel a never-claimed child after parent End" caseParentEnd,
      testCase "an attached child cannot be cancelled by administration" caseAttached
    ]

openAdmin :: HeraldState -> IO (HeraldState, Admin.AdministrationBinding)
openAdmin state = do
  (successor, effects) <- step (AdministrationInput (OpenAdministrationConnection (candidateAdministrationLane 501) (checkedSystemId (startupGenesis state)))) state
  case [binding | SetAdministrationConnectionDisposition _ binding <- effectBatchMembers effects] of
    [binding] -> pure (successor, binding)
    _ -> assertFailure "admin role did not bind"

correlation :: Admin.AdministrationBinding -> Admin.AdminCorrelationId
correlation binding = Admin.adminCorrelationIdForBinding binding 701
cancel :: Admin.AdministrationBinding -> ChildPreparation -> HeraldState -> IO (HeraldState, EffectBatch)
cancel binding reference = step (AdministrationInput (CancelChildPreparation binding (correlation binding) reference))

expectStatus :: Admin.AdministrationBinding -> LifecycleStatus -> HeraldState -> Assertion
expectStatus binding expected state = assertEqual "retained administrator result" (Just (Admin.AdminPreparationCancellation expected)) (Administration.lookupAdministrationResultStatus (correlation binding) (startupAdministrationState state))

casePendingStart :: Assertion
casePendingStart = do
  fixture@(Child.Fixture _ oracleBinding _ _ _ _) <- Child.fixture
  oracle <- checkedIO (initialOracle Genesis.fixtureCheckedOracleGenesis)
  (pending, startEffects, reference) <- Child.begin fixture Child.emptySelection
  (authorized, admin) <- openAdmin pending
  (cancelling, _) <- cancel admin reference authorized
  expectStatus admin (LifecyclePending []) cancelling
  (retried, retryEffects) <- cancel admin reference cancelling
  assertBool "retry preserves the exact preparation and generated identities" (startupProcessPreparationState cancelling == startupProcessPreparationState retried && startupIdGeneratorState cancelling == startupIdGeneratorState retried)
  assertEqual "retry creates no additional Oracle submission" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers retryEffects]
  let other = checked (childPreparation (childPreparationScopeBytes reference) (childPreparationSessionOrdinal reference) (childPreparationOrdinal reference + 1))
  (conflicted, conflictEffects) <- cancel admin other retried
  assertBool "one correlation cannot cancel a different child" (SendAdministrationReply admin (Admin.ConflictingAdminResult (correlation admin)) `elem` effectBatchMembers conflictEffects)
  (started, oracle1, endEffects) <- Child.apply oracleBinding oracle startEffects conflicted
  process <- Child.childProcess reference started
  assertEqual "sealed child exposes no application attachment" Nothing (Application.applicationBootstrapAccess process (startupApplicationState started))
  (ended, _, _) <- Child.apply oracleBinding oracle1 endEffects started
  expectStatus admin (LifecycleCompleted ChildCancelled) ended
  assertNotLive process ended
  (reconnected, replacement) <- openAdmin ended
  (_, queryEffects) <- step (AdministrationInput (GetAdministrationResult replacement (correlation replacement))) reconnected
  assertEqual "new administrator cannot recover a closed receipt lifetime" [SendAdministrationReply replacement (Admin.AbsentAdminResult (correlation replacement))] (effectBatchMembers queryEffects)
  assertEqual "closed terminal cancellation receipt was reclaimed" Nothing (Administration.lookupAdministrationResultStatus (correlation admin) (startupAdministrationState reconnected))

caseParentEnd :: Assertion
caseParentEnd = do
  fixture@(Child.Fixture _ oracleBinding _ _ _ parent) <- Child.fixture
  (prepared, oracle, reference, _) <- Child.prepare fixture Child.emptySelection
  child <- Child.childProcess reference prepared
  (endingParent, parentEffects) <- Child.call fixture 3 EndOwnProcess prepared
  (parentEnded, oracle1, _) <- Child.apply oracleBinding oracle parentEffects endingParent
  assertNotLive parent parentEnded
  assertBool "parent End leaves child live" (Projection.oracleViewProcessIsLive child (Projection.oracleView (startupOracleProjectionState parentEnded)))
  (authorized, admin) <- openAdmin parentEnded
  (cancelling, endEffects) <- cancel admin reference authorized
  expectStatus admin (LifecyclePending []) cancelling
  (ended, _, _) <- Child.apply oracleBinding oracle1 endEffects cancelling
  expectStatus admin (LifecycleCompleted ChildCancelled) ended
  assertNotLive child ended

caseAttached :: Assertion
caseAttached = do
  fixture <- Child.fixture
  (prepared, _, reference, child) <- Child.prepare fixture Child.emptySelection
  let claim = checked (initialClaimId (Bytes.replicate 32 64))
  (attached, _) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 511) (preparedChildConnection child) claim Nothing)) prepared
  (authorized, admin) <- openAdmin attached
  (unchanged, effects) <- cancel admin reference authorized
  expectStatus admin (LifecycleCompleted ChildAlreadyAttached) unchanged
  assertEqual "administrative cancellation never kills a claimed child" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers effects]

assertNotLive :: ProcessEpochId -> HeraldState -> Assertion
assertNotLive process state = assertBool "ordered End removed process liveness" (not (Projection.oracleViewProcessIsLive process (Projection.oracleView (startupOracleProjectionState state))))

checkedIO :: (Show failure) => Either failure value -> IO value
checkedIO = either (assertFailure . show) pure
checked :: (Show failure) => Either failure value -> value
checked = either (error . show) id
