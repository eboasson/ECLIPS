{-# LANGUAGE OverloadedRecordDot #-}

module ApplicationInitialClaimProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Domain.Identity (ProcessEpochId, mkProcessEpochId)
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Recovery (applicationRecoveryGraceMicroseconds)
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Startup.State (startupApplicationState, startupControlledState, startupStructuralProgressState)
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures (fixtureApplicationRecoveryConfiguration, fixtureCheckedGenesis)
import NewEnvironmentProperties (Fixture (..), fixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), Property, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup
    "initial application claims"
    [ testProperty "pending attempts do not consume a session and any ready attempt may win" propPending,
      testProperty "membership admission holds ready name-only claims without consuming a session" propMembershipGate,
      testProperty "exact claim reoffers one session and startup result" propExactRetry,
      testCase "ordinary Open cannot bypass the child claim" caseNoOpen,
      testCase "a competing ready attempt cannot consume the attachment twice" caseCompetitor,
      testCase "repeated lost first replies keep the original recovery deadline" caseAbsoluteDeadline,
      testCase "normal Resume owns reconnect after its first successful transition" caseResumeOwns,
      testCase "session End overrides retained initial success" caseSessionEnd,
      testCase "process End overrides retained initial success" caseProcessEnd,
      testCase "the genesis launcher claims its existing bootstrap without recreating access" caseGenesisClaim
    ]

child :: ProcessEpochId
child = checked (mkProcessEpochId (Bytes.replicate 32 0xf5))
attachment :: Session.ApplicationAttachment
attachment = Session.applicationAttachmentForProcess child
initial :: Application.State
initial = fst (Application.commitApplicationBootstrap (checked (Application.prepareApplicationChildBootstrap attachment child grant Application.emptyState)))

grant :: Primordial.CheckedPrimordialGrant
grant = checked (Primordial.checkPrimordialSelection (fixtureProcess fixture) selection (Application.applicationPrivateIdentity (startupApplicationState base)) (startupControlledState base) (startupStructuralProgressState base))
  where
    base = fixtureState fixture
    selection = checked (Access.primordialSelection [] Set.empty Nothing)

claim :: Word8 -> Lifecycle.InitialClaimId
claim value = checked (Lifecycle.initialClaimId (Bytes.replicate 32 value))
prepare :: Word64 -> Word8 -> Bool -> Application.State -> Either Application.ApplicationInitialClaimError Application.PreparedApplicationInitialClaim
prepare now identity ready = Application.prepareApplicationInitialClaim (monotonicInstant now) (checkedLocalHeraldEpoch fixtureCheckedGenesis) attachment (claim identity) ready
apply :: Word64 -> Word8 -> Bool -> Application.State -> (Application.State, Application.ApplicationInitialClaimOutcome)
apply now identity ready = Application.commitApplicationInitialClaim . checked . prepare now identity ready
opened :: Application.ApplicationInitialClaimOutcome -> Session.ApplicationSessionAcceptance
opened (Application.ApplicationInitialClaimOpened acceptance) = acceptance
opened _ = error "ready claim stayed pending"

propPending :: Word8 -> Word8 -> Property
propPending first second =
  counterexample "pending claim consumed identity or armed recovery"
    $ Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness pending) == 1
      && null (Application.applicationSessionRecoveryEntries pending)
      && null (Application.applicationProcessRecoveryEntries pending)
      && case Application.applicationInitialClaimEntries attached of
        [(_, _, Application.ApplicationInitialClaimClaimed winner _ Nothing False)] -> winner == claim second
        _ -> False
  where
    (pending, _) = apply 0 first False initial
    (attached, _) = apply 0 second True pending

propMembershipGate :: [Word8] -> Word8 -> Property
propMembershipGate attempts winner =
  counterexample "membership gate granted a first attachment or blocked retained claim recovery"
    $ all (== Application.ApplicationInitialClaimPending) outcomes
      && Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness pending) == 1
      && Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness openedState) == 2
      && repeated == first
  where
    gated = Application.setApplicationMembershipGate True initial
    (pending, outcomes) = foldl (\(state, replies) attempt -> let (next, reply) = apply 0 attempt True state in (next, reply : replies)) (gated, []) attempts
    (openedState, first) = apply 0 winner True (Application.setApplicationMembershipGate False pending)
    (_, repeated) = apply 0 winner True (Application.setApplicationMembershipGate True openedState)

propExactRetry :: NonNegative Int -> Property
propExactRetry (NonNegative count) =
  counterexample "exact initial claim allocated or changed the open result"
    $ all (== first) replies && Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness final) == 2
  where
    (attached, first) = apply 0 1 True initial
    (final, replies) = foldl retry (attached, []) [1 .. count `mod` 30]
    retry (state, outputs) _ = let (successor, output) = apply 0 1 True state in (successor, output : outputs)

caseNoOpen :: Assertion
caseNoOpen = case Application.prepareApplicationSessionOpen (checkedLocalHeraldEpoch fixtureCheckedGenesis) attachment (Session.clientNonce 33) initial of
  Left (Application.ApplicationSessionRejected Session.ApplicationAttachmentNotAdmitted) -> pure ()
  _ -> assertFailure "ordinary Open bypassed the initial claim"

caseCompetitor :: Assertion
caseCompetitor = expectRejected Lifecycle.StartupAlreadyClaimed (prepare 0 2 True attached)
  where
    (attached, _) = apply 0 1 True initial

caseAbsoluteDeadline :: Assertion
caseAbsoluteDeadline = do
  let (attached, output) = apply 0 1 True initial
      acceptance = opened output
      binding = Session.sessionAcceptanceBinding acceptance
      lose now = Application.commitApplicationBindingLoss . Application.prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant now) binding
      lost = lose 10 attached
      prepared = checked (prepare 11 1 True lost)
      (retried, repeated) = Application.commitApplicationInitialClaim prepared
      lostAgain = lose 12 retried
      deadline = monotonicInstant (10 + applicationRecoveryGraceMicroseconds fixtureApplicationRecoveryConfiguration)
  assertEqual "lost response recovers exact initial result" output repeated
  assertBool "recovery timer cancelled on reconnect" (Application.preparedApplicationInitialClaimTimerCancellation prepared /= Nothing)
  assertEqual "first absolute deadline remains unchanged" [deadline] (map (.deadline) (Application.applicationSessionRecoveryEntries lostAgain))
  expectRejected Lifecycle.StartupSessionExpired (prepare (10 + applicationRecoveryGraceMicroseconds fixtureApplicationRecoveryConfiguration) 1 True lostAgain)

caseResumeOwns :: Assertion
caseResumeOwns = do
  let (attached, output) = apply 0 1 True initial
      acceptance = opened output
  case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened session token _ -> do
      let grace = applicationRecoveryGraceMicroseconds fixtureApplicationRecoveryConfiguration
          firstLoss = Application.commitApplicationBindingLoss (Application.prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 10) (Session.sessionAcceptanceBinding acceptance) attached)
          (reoffered, _) = apply 11 1 True firstLoss
          (resumed, resumedAcceptance) = Application.commitApplicationSessionAcceptance (checked (Application.prepareApplicationSessionResume session token (Session.sessionAcceptanceCursor acceptance) reoffered))
          muchLater = 10 + grace * 3
          laterLoss = Application.commitApplicationBindingLoss (Application.prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant muchLater) (Session.sessionAcceptanceBinding resumedAcceptance) resumed)
      expectRejected Lifecycle.StartupAlreadyClaimed (prepare 12 1 True resumed)
      assertEqual "normal Resume releases the old initial recovery deadline" [monotonicInstant (muchLater + grace)] (map (.deadline) (Application.applicationSessionRecoveryEntries laterLoss))
      let (_, recovered) = Application.commitApplicationSessionAcceptance (checked (Application.prepareApplicationSessionResume session token (Session.sessionAcceptanceCursor resumedAcceptance) laterLoss))
      assertEqual "ordinary recovery preserves session identity" session (Session.sessionBindingSessionId (Session.sessionAcceptanceBinding recovered))
    _ -> assertFailure "initial reply did not open"

caseSessionEnd :: Assertion
caseSessionEnd = do
  let (attached, output) = apply 0 1 True initial
      binding = Session.sessionAcceptanceBinding (opened output)
      ended = Application.commitApplicationSessionEnd (checked (Application.prepareApplicationSessionEnd (Session.sessionBindingSessionId binding) binding attached))
  expectRejected Lifecycle.StartupSessionExpired (prepare 0 1 True ended)

caseProcessEnd :: Assertion
caseProcessEnd = do
  let (attached, _) = apply 0 1 True initial
      (ended, _) = Application.commitApplicationProcessRetirement (Application.prepareApplicationProcessRetirement child attached)
  expectRejected Lifecycle.StartupNotLive (prepare 0 1 True ended)

caseGenesisClaim :: Assertion
caseGenesisClaim = do
  let (installed, access) = Application.commitApplicationBootstrap (checked (Application.prepareApplicationPrimordialBootstrap attachment child grant Application.emptyState))
      admit identity state = Application.prepareApplicationGenesisInitialClaim (monotonicInstant 0) (checkedLocalHeraldEpoch fixtureCheckedGenesis) attachment child (claim identity) state
      (claimed, result) = Application.commitApplicationInitialClaim (checked (admit 1 installed))
      (repeated, replay) = Application.commitApplicationInitialClaim (checked (admit 1 claimed))
  assertEqual "claim uses the existing exact bootstrap" (Just access) (Application.applicationBootstrapAccess child claimed)
  assertBool "claim allocates no private aliases" (Application.applicationPrivateIdentity installed == Application.applicationPrivateIdentity claimed)
  assertEqual "exact launcher claim recovers the same session" result replay
  assertEqual "only one session was allocated" 2 (Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness repeated))
  expectRejected Lifecycle.StartupAlreadyClaimed (admit 2 repeated)
  let (alreadyOpened, _) = Application.commitApplicationSessionAcceptance (checked (Application.prepareApplicationSessionOpen (checkedLocalHeraldEpoch fixtureCheckedGenesis) attachment (Session.clientNonce 99) installed))
  expectRejected Lifecycle.StartupAlreadyClaimed (admit 1 alreadyOpened)

expectRejected :: Lifecycle.StartupError -> Either Application.ApplicationInitialClaimError value -> Assertion
expectRejected expected result = case result of
  Left actual -> assertEqual "terminal claim rejection" (Application.ApplicationInitialClaimRejected expected) actual
  Right _ -> assertFailure "claim unexpectedly accepted"

checked :: (Show failure) => Either failure value -> value
checked = either (error . show) id
