{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module DiscoveryProperties
  ( tests,
  )
where

import GenesisFixtures (fixtureCheckedOracleGenesisFor, fixtureRetirementResolution)

import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Domain.Identity
  ( HeraldEpoch,
    HeraldId,
    SystemId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
    mkSystemId,
    mkTopologyCutId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipHistory,
    genesisHeraldMembershipGeneration,
    heraldMembershipHistory,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Startup
  ( CatalogueDigest,
    HeraldMember (..),
    InitialProjectionDigest,
    mkCatalogueDigest,
    mkInitialProjectionDigest,
  )
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.Internal
  ( DiscoveryStatic,
    PeerAddress (..),
    PeerDialClass (ImmediatePeerDial),
    PeerDialGeneration (..),
    PeerDialIntent (..),
    discoveryStatic,
  )
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.Genesis.Internal
  ( PrimordialProcessManifest (..),
    checkInitialBootstraps,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedSystemId,
  )
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Command qualified as OracleCommand
import Eclips.Oracle.Effect (OracleStepOutcome (OracleCommitted))
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State qualified as OracleState
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureIdentifierBytes,
    fixtureRemoteMember,
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
  ( Property,
    counterexample,
    property,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "logical peer discovery"
    [ testCase "candidate-open authors a checked local Hello" caseCandidateOpened,
      testCase "application contacts come from admitted Hello and retire with membership" caseApplicationContact,
      testCase "hello admission distinguishes ordinary rejection from projection fault" caseHelloDecisionTable,
      testCase "the lexicographically smaller simultaneous candidate replaces once" caseCandidateChoice,
      testCase "reconnect catalogue permission follows the exact Hello and binding lifecycle" caseReconnectCatalogueLifecycle,
      testProperty
        "generated loss/replacement/Resume/reoffer schedules agree with the level-triggered catalogue reference"
        propReconnectCatalogueReference,
      testCase "contact hints union without granting a binding" caseContactHints,
      testCase "pending contact becomes an ordinary peer only after exact Oracle activation" caseDynamicAdmission,
      testCase "a later newcomer replays prior admissions from immutable genesis" caseJoiningDiscoveryReplay,
      testCase "joining control base agrees with activated, cancelled and pending admission replay" caseJoiningControlBaseReplay,
      testCase "joining control base rejects a serving or already advanced receiver" caseJoiningControlBaseReceiver,
      testCase "joining control base binds genesis and the exact pending predecessor" caseJoiningControlBaseContext,
      testCase "same-generation address growth retains the dial cancellation key" caseDialCancellationKey,
      testCase "stale binding loss is a no-op and current loss preserves hints" caseBindingLoss,
      testCase "retirement rejects an old Hello when queued work is rechecked" caseRetiredHelloRejected,
      testCase "retirement removes its binding and retained contacts cannot redial it" caseRetiredBindingRemoved,
      testCase "duplicate and conflicting membership advances are rejected" caseMembershipAdvanceConflicts,
      testCase "local retirement makes Hello and dialing inert while retaining valid state" caseLocalRetired,
      testCase "a surviving member remains admissible after another member retires" caseSurvivorContinuity,
      testCase "successive retirements preserve survivor bindings and remove each exact retired binding" caseRepeatedSurvivorContinuity
    ]

caseApplicationContact :: Assertion
caseApplicationContact = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      candidate = remoteCandidate fixture 31
  locator <- checked "application locator" (Lifecycle.heraldLocator "remote.example" 41001)
  local <- checked "local application locator" (Lifecycle.heraldLocator "local.example" 42001)
  let hello =
        Discovery.peerHello
          fixture.systemId
          fixture.remoteId
          fixture.remoteEpoch
          (Discovery.connectionNonce 31)
          (Set.singleton (PeerAddress "tcp://remote.example:41000"))
          (controlIndex 99)
          fixture.catalogueDigest
          fixture.projectionDigest
          (Just locator)
  assertEqual
    "unseen application address has no membership authority"
    Nothing
    (DiscoveryState.resolveApplicationLocator locator initial)
  opened <-
    checked
      "local serving contact"
      ( DiscoveryState.prepareCandidateHello
          (controlIndex 0)
          (Discovery.peerCandidateOpened (Discovery.connectionNonce 17) Set.empty (Just local))
          initial
      )
  let (announced, localHello) = DiscoveryState.commitCandidateHello opened
  assertEqual
    "the local Hello reports its runtime application contact"
    (Just local)
    (Discovery.peerHelloApplicationLocator (Discovery.candidateHelloMessage localHello))
  assertEqual
    "local lookup uses the same admitted locator"
    (Just local)
    (DiscoveryState.applicationLocatorFor fixture.localEpoch announced)
  accepted <- checked "admitted remote contact" (DiscoveryState.preparePeerHello candidate hello announced)
  let (connected, disposition) = DiscoveryState.commitPeerHello accepted
  binding <- admittedBinding "remote application contact" Discovery.FirstBinding disposition
  assertEqual
    "application locator resolves to the admitted epoch"
    (Just fixture.remoteEpoch)
    (DiscoveryState.resolveApplicationLocator locator connected)
  lost <- checked "remote binding loss" (DiscoveryState.prepareBindingLoss binding connected)
  let (disconnected, _) = DiscoveryState.commitBindingLoss lost
  assertEqual
    "recoverable lane loss retains target resolution"
    (Just fixture.remoteEpoch)
    (DiscoveryState.resolveApplicationLocator locator disconnected)
  membership <- checked "retire contacted target" (retireHeraldMembershipGeneration (controlIndex 14) (fixtureRetirementResolution (controlIndex 14)) fixture.remoteEpoch fixture.initialMembership)
  retirement <- checked "apply target retirement" (DiscoveryState.prepareMembershipAdvance membership disconnected)
  let (retired, _) = DiscoveryState.commitMembershipAdvance retirement
  assertEqual
    "retained address cannot resolve to a retired target"
    Nothing
    (DiscoveryState.resolveApplicationLocator locator retired)

caseDialCancellationKey :: Assertion
caseDialCancellationKey = do
  let fixture = discoveryFixture
      addressA = PeerAddress "peer-a.example:4040"
      addressB = PeerAddress "peer-b.example:4040"
      first =
        PeerDialIntent
          fixture.remoteId
          fixture.remoteEpoch
          (Set.singleton addressA)
          ImmediatePeerDial
          (PeerDialGeneration 7)
      grown =
        PeerDialIntent
          fixture.remoteId
          fixture.remoteEpoch
          (Set.fromList [addressA, addressB])
          ImmediatePeerDial
          (PeerDialGeneration 7)
      successor =
        PeerDialIntent
          fixture.remoteId
          fixture.remoteEpoch
          (Set.fromList [addressA, addressB])
          ImmediatePeerDial
          (PeerDialGeneration 8)
  assertBool "address growth changes the complete dial value" (first /= grown)
  assertEqual
    "address growth does not change cancellation identity"
    (Discovery.peerDialIntentCancellationKey first)
    (Discovery.peerDialIntentCancellationKey grown)
  assertBool
    "a later dial generation has a distinct cancellation identity"
    (Discovery.peerDialIntentCancellationKey grown /= Discovery.peerDialIntentCancellationKey successor)

caseCandidateOpened :: Assertion
caseCandidateOpened = do
  let fixture = discoveryFixture
      address = Discovery.peerAddress "local.example:4040"
      opened =
        Discovery.peerCandidateOpened
          (Discovery.connectionNonce 7)
          (Set.singleton address)
          Nothing
      initial = DiscoveryState.initialState fixture.staticView
  prepared <-
    checked
      "local candidate Hello"
      (DiscoveryState.prepareCandidateHello (controlIndex 41) opened initial)
  let (successor, offered) = DiscoveryState.commitCandidateHello prepared
      candidate = Discovery.candidateHelloCandidate offered
      hello = Discovery.candidateHelloMessage offered
  assertEqual "candidate is locally initiated" fixture.localId (Discovery.peerCandidateInitiatorHeraldId candidate)
  assertEqual "candidate uses the local epoch" fixture.localEpoch (Discovery.peerCandidateInitiatorHeraldEpoch candidate)
  assertEqual "candidate and Hello share the nonce" (Discovery.peerCandidateConnectionNonce candidate) (Discovery.peerHelloConnectionNonce hello)
  assertEqual "Hello carries current control progress" (controlIndex 41) (Discovery.peerHelloAppliedControlIndex hello)
  assertEqual "Hello carries supplied hints" (Set.singleton address) (Discovery.peerHelloAdvertisedAddresses hello)
  assertEqual "candidate-open does not mutate discovery" initial successor

caseHelloDecisionTable :: Assertion
caseHelloDecisionTable = do
  let fixture = discoveryFixture
      candidate = remoteCandidate fixture 11
      accepted = remoteHello fixture 11 Set.empty
      initial = DiscoveryState.initialState fixture.staticView
  acceptedPrepared <- checked "matching hello" (DiscoveryState.preparePeerHello candidate accepted initial)
  assertAccepted "matching hello" acceptedPrepared

  otherSystem <- checked "other SystemId" (mkSystemId (fixtureIdentifierBytes 201))
  assertRejected
    "system mismatch"
    Discovery.HelloSystemMismatch
    candidate
    ( Discovery.peerHello
        otherSystem
        fixture.remoteId
        fixture.remoteEpoch
        (Discovery.connectionNonce 11)
        Set.empty
        (controlIndex 99)
        fixture.catalogueDigest
        fixture.projectionDigest
        Nothing
    )
    initial

  otherCatalogue <- checked "other catalogue digest" (mkCatalogueDigest (fixtureIdentifierBytes 202))
  assertRejected
    "catalogue mismatch"
    Discovery.HelloCatalogueMismatch
    candidate
    ( Discovery.peerHello
        fixture.systemId
        fixture.remoteId
        fixture.remoteEpoch
        (Discovery.connectionNonce 11)
        Set.empty
        (controlIndex 99)
        otherCatalogue
        fixture.projectionDigest
        Nothing
    )
    initial

  otherProjection <- checked "other projection digest" (mkInitialProjectionDigest (fixtureIdentifierBytes 203))
  case DiscoveryState.preparePeerHello
    candidate
    ( Discovery.peerHello
        fixture.systemId
        fixture.remoteId
        fixture.remoteEpoch
        (Discovery.connectionNonce 11)
        Set.empty
        (controlIndex 99)
        fixture.catalogueDigest
        otherProjection
        Nothing
    )
    initial of
    Left (Discovery.DiscoveryInitialProjectionMismatch expected observed) -> do
      assertEqual "expected digest" fixture.projectionDigest expected
      assertEqual "observed digest" otherProjection observed
    Left problem -> assertFailure ("unexpected projection problem: " <> show problem)
    Right _ -> assertFailure "projection mismatch was admitted"

  let malformedCandidate =
        Discovery.peerCandidate
          fixture.remoteId
          fixture.localEpoch
          (Discovery.connectionNonce 11)
  assertRejected
    "malformed candidate tuple remains an ordinary rejection"
    Discovery.HelloCandidateTupleInvalid
    malformedCandidate
    accepted
    initial

caseCandidateChoice :: Assertion
caseCandidateChoice = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      localCandidate =
        Discovery.peerCandidate
          fixture.localId
          fixture.localEpoch
          (Discovery.connectionNonce 17)
      remoteCandidateValue = remoteCandidate fixture 23
      (winner, winnerHello, loser, loserHello) =
        if localCandidate < remoteCandidateValue
          then
            ( localCandidate,
              remoteHello fixture 17 Set.empty,
              remoteCandidateValue,
              remoteHello fixture 23 Set.empty
            )
          else
            ( remoteCandidateValue,
              remoteHello fixture 23 Set.empty,
              localCandidate,
              remoteHello fixture 17 Set.empty
            )
  loserPrepared <- checked "first candidate" (DiscoveryState.preparePeerHello loser loserHello initial)
  let (afterLoser, loserDisposition) = DiscoveryState.commitPeerHello loserPrepared
  loserBinding <- admittedBinding "first candidate" Discovery.FirstBinding loserDisposition
  assertEqual
    "the first binding has generation one"
    1
    (Discovery.peerBindingGenerationWord64 (Discovery.peerBindingGeneration loserBinding))

  winnerPrepared <- checked "winning replacement" (DiscoveryState.preparePeerHello winner winnerHello afterLoser)
  let (afterWinner, winnerDisposition) = DiscoveryState.commitPeerHello winnerPrepared
  winnerBinding <- admittedBinding "winning replacement" Discovery.ReplacedBinding winnerDisposition
  assertEqual
    "replacement advances the logical generation"
    2
    (Discovery.peerBindingGenerationWord64 (Discovery.peerBindingGeneration winnerBinding))
  assertEqual
    "the selected candidate is the lexicographic minimum"
    winner
    (Discovery.peerBindingSelectedCandidate winnerBinding)

  retryPrepared <- checked "winner retry" (DiscoveryState.preparePeerHello winner winnerHello afterWinner)
  let (afterRetry, retryDisposition) = DiscoveryState.commitPeerHello retryPrepared
  retryBinding <- admittedBinding "winner retry" Discovery.ReofferedBinding retryDisposition
  assertEqual "retry returns the exact binding" winnerBinding retryBinding
  assertEqual "retry is state-identical" afterWinner afterRetry

  rejectedPrepared <- checked "loser retry" (DiscoveryState.preparePeerHello loser loserHello afterWinner)
  let (afterRejected, rejectedDisposition) = DiscoveryState.commitPeerHello rejectedPrepared
  assertEqual
    "the superseded candidate names the retained winner"
    (Discovery.PeerHelloRejected (Discovery.HelloCandidateNotSelected winner))
    rejectedDisposition
  assertEqual "loser retry is state-identical" afterWinner afterRejected
  assertEqual "new and replaced bindings coalesce into one peer delivery notification" (Set.singleton fixture.remoteEpoch) (fst (DiscoveryState.takePreparationBindingChanges afterRejected))
  assertEqual "draining delivery changes leaves the selected logical binding intact" (Just winnerBinding) (DiscoveryState.currentPeerBinding fixture.remoteEpoch (snd (DiscoveryState.takePreparationBindingChanges afterRejected)))

caseReconnectCatalogueLifecycle :: Assertion
caseReconnectCatalogueLifecycle = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      candidateA =
        Discovery.peerCandidate
          fixture.localId
          fixture.localEpoch
          (Discovery.connectionNonce 17)
      candidateB = remoteCandidate fixture 23
      (winner, winnerHello, loser, loserHello) =
        if candidateA < candidateB
          then
            ( candidateA,
              remoteHello fixture 17 Set.empty,
              candidateB,
              remoteHello fixture 23 Set.empty
            )
          else
            ( candidateB,
              remoteHello fixture 23 Set.empty,
              candidateA,
              remoteHello fixture 17 Set.empty
            )
  firstPrepared <-
    checked
      "catalogue first binding"
      (DiscoveryState.preparePeerHello loser loserHello initial)
  let (afterFirstHello, firstDisposition) =
        DiscoveryState.commitPeerHello firstPrepared
  firstBinding <-
    admittedBinding "catalogue first binding" Discovery.FirstBinding firstDisposition

  (afterFirstCatalogue, firstWasPending) <-
    consumeCatalogue "first ResumeOffer" firstBinding afterFirstHello
  assertBool "the first ResumeOffer consumes the reconnect catalogue" firstWasPending
  (afterRepeatedCatalogue, repeatedWasPending) <-
    consumeCatalogue "repeated ResumeOffer" firstBinding afterFirstCatalogue
  assertBool "a repeated ResumeOffer has no reconnect catalogue" (not repeatedWasPending)
  assertEqual
    "a repeated ResumeOffer leaves the consumed permission unchanged"
    afterFirstCatalogue
    afterRepeatedCatalogue

  reofferPrepared <-
    checked
      "same-binding Hello reoffer"
      ( DiscoveryState.preparePeerHello
          loser
          loserHello
          afterRepeatedCatalogue
      )
  let (afterReofferedHello, reofferDisposition) =
        DiscoveryState.commitPeerHello reofferPrepared
  reofferedBinding <-
    admittedBinding
      "same-binding Hello reoffer"
      Discovery.ReofferedBinding
      reofferDisposition
  assertEqual "the reoffered Hello retains its binding" firstBinding reofferedBinding
  (afterReofferedCatalogue, reofferedWasPending) <-
    consumeCatalogue "ResumeOffer after Hello reoffer" reofferedBinding afterReofferedHello
  assertBool "a reoffered Hello restores one reconnect catalogue" reofferedWasPending

  replacementPrepared <-
    checked
      "catalogue replacement binding"
      ( DiscoveryState.preparePeerHello
          winner
          winnerHello
          afterReofferedCatalogue
      )
  let (afterReplacement, replacementDisposition) =
        DiscoveryState.commitPeerHello replacementPrepared
  replacementBinding <-
    admittedBinding
      "catalogue replacement binding"
      Discovery.ReplacedBinding
      replacementDisposition
  (afterStaleOffer, staleWasPending) <-
    consumeCatalogue "stale replaced-binding ResumeOffer" firstBinding afterReplacement
  assertBool "a stale replaced binding cannot consume the catalogue" (not staleWasPending)
  assertEqual "stale offer leaves the replacement permission" afterReplacement afterStaleOffer
  (afterReplacementCatalogue, replacementWasPending) <-
    consumeCatalogue
      "replacement ResumeOffer"
      replacementBinding
      afterStaleOffer
  assertBool "a replacement binding receives one reconnect catalogue" replacementWasPending

  staleLoss <-
    checked
      "stale replaced-binding loss"
      (DiscoveryState.prepareBindingLoss firstBinding afterReplacementCatalogue)
  let (afterStaleLoss, staleWasCurrent) = DiscoveryState.commitBindingLoss staleLoss
  assertBool "replaced-binding loss remains stale" (not staleWasCurrent)
  assertEqual
    "stale loss does not disturb replacement state"
    afterReplacementCatalogue
    afterStaleLoss
  currentLoss <-
    checked
      "current replacement loss"
      ( DiscoveryState.prepareBindingLoss
          replacementBinding
          afterStaleLoss
      )
  let (afterCurrentLoss, currentWasCurrent) =
        DiscoveryState.commitBindingLoss currentLoss
  assertBool "current loss clears the replacement" currentWasCurrent
  (afterLostOffer, lostWasPending) <-
    consumeCatalogue
      "lost-binding ResumeOffer"
      replacementBinding
      afterCurrentLoss
  assertBool "a lost binding has no reconnect catalogue" (not lostWasPending)
  assertEqual "a lost-binding offer is state-identical" afterCurrentLoss afterLostOffer

data CatalogueTraceOperation
  = OfferLosingCandidate
  | OfferWinningCandidate
  | ReofferCurrentBinding
  | ResumeCurrentBinding
  | ResumeStaleBinding
  | LoseCurrentBinding
  | LoseStaleBinding
  deriving stock (Eq, Show)

data CatalogueResumeTarget
  = CurrentResumeTarget
  | StaleResumeTarget
  deriving stock (Eq, Show)

data CatalogueReferenceBinding
  = CatalogueReferenceBinding Discovery.PeerCandidate Word64
  deriving stock (Eq, Show)

data CatalogueReference
  = CatalogueReference
      (Maybe CatalogueReferenceBinding)
      Word64
      Bool
  deriving stock (Eq, Show)

data CatalogueTrace
  = CatalogueTrace
      DiscoveryState.State
      CatalogueReference
      [Discovery.PeerBinding]
      [(CatalogueResumeTarget, Bool)]

data CatalogueReferenceHelloDisposition
  = CatalogueReferenceAccepted
      Discovery.BindingAdmission
      CatalogueReferenceBinding
  | CatalogueReferenceRejected Discovery.PeerCandidate
  deriving stock (Eq, Show)

-- Every generated trace begins with the replacement-before-first-Resume
-- ordering complementary to 'caseReconnectCatalogueLifecycle', then proves
-- that a same-binding reoffer restores one permission after the first
-- catalogue has been consumed.  The generated tail interleaves further loss,
-- stale/current Resume, replacement, and reoffer operations.
propReconnectCatalogueReference :: [Word8] -> Property
propReconnectCatalogueReference rawOperations =
  counterexample ("catalogue operations: " <> show operations)
    $ case runCatalogueTrace generatedOperations of
      Left problem -> counterexample problem (property False)
      Right () -> property True
  where
    generatedOperations =
      fmap decodeCatalogueOperation (take 80 rawOperations)
    operations = mandatoryCatalogueSchedule <> generatedOperations

mandatoryCatalogueSchedule :: [CatalogueTraceOperation]
mandatoryCatalogueSchedule =
  [ OfferLosingCandidate,
    OfferWinningCandidate,
    ResumeStaleBinding,
    ResumeCurrentBinding,
    ResumeCurrentBinding,
    ReofferCurrentBinding,
    ResumeCurrentBinding,
    LoseCurrentBinding,
    ResumeStaleBinding,
    OfferWinningCandidate,
    ResumeCurrentBinding
  ]

decodeCatalogueOperation :: Word8 -> CatalogueTraceOperation
decodeCatalogueOperation value = case value `mod` 7 of
  0 -> OfferLosingCandidate
  1 -> OfferWinningCandidate
  2 -> ReofferCurrentBinding
  3 -> ResumeCurrentBinding
  4 -> ResumeStaleBinding
  5 -> LoseCurrentBinding
  _ -> LoseStaleBinding

runCatalogueTrace :: [CatalogueTraceOperation] -> Either String ()
runCatalogueTrace generatedOperations = do
  let fixture = discoveryFixture
      (winner, loser) = catalogueCandidateOrder fixture
      initial =
        CatalogueTrace
          (DiscoveryState.initialState fixture.staticView)
          (CatalogueReference Nothing 0 False)
          []
          []
  checkCatalogueTrace "initial catalogue state" fixture initial
  afterMandatory <-
    foldM
      (stepCatalogueTrace fixture winner loser)
      initial
      mandatoryCatalogueSchedule
  let CatalogueTrace _ _ _ mandatoryResumes = afterMandatory
  ensureCatalogue
    ( "mandatory replacement-before-Resume lifecycle differed: "
        <> show mandatoryResumes
    )
    ( mandatoryResumes
        == [ (StaleResumeTarget, False),
             (CurrentResumeTarget, True),
             (CurrentResumeTarget, False),
             (CurrentResumeTarget, True),
             (StaleResumeTarget, False),
             (CurrentResumeTarget, True)
           ]
    )
  afterGenerated <-
    foldM
      (stepCatalogueTrace fixture winner loser)
      afterMandatory
      generatedOperations
  armed <- armCurrentCatalogue fixture winner loser afterGenerated
  consumed <-
    stepCatalogueTrace
      fixture
      winner
      loser
      armed
      ResumeCurrentBinding
  duplicate <-
    stepCatalogueTrace
      fixture
      winner
      loser
      consumed
      ResumeCurrentBinding
  let CatalogueTrace _ _ _ finalResumes = duplicate
  ensureCatalogue
    ( "eventual current Resume did not consume exactly one permission: "
        <> show (take 2 (reverse finalResumes))
    )
    ( take 2 (reverse finalResumes)
        == [ (CurrentResumeTarget, False),
             (CurrentResumeTarget, True)
           ]
    )
  checkCatalogueTrace "final catalogue state" fixture duplicate

stepCatalogueTrace ::
  DiscoveryFixture ->
  Discovery.PeerCandidate ->
  Discovery.PeerCandidate ->
  CatalogueTrace ->
  CatalogueTraceOperation ->
  Either String CatalogueTrace
stepCatalogueTrace fixture winner loser (CatalogueTrace state reference retained resumes) operation = do
  let clean = DiscoveryState.clearPreparationBindingChanges state
      predecessor = CatalogueTrace clean reference retained resumes
  successor@(CatalogueTrace successorState _ _ _) <-
    stepCatalogueTraceUnchecked fixture winner loser predecessor operation
  let before = DiscoveryState.currentPeerBinding fixture.remoteEpoch clean
      after = DiscoveryState.currentPeerBinding fixture.remoteEpoch successorState
      expected = if before == after then Set.empty else Set.singleton fixture.remoteEpoch
      (observed, consumed) = DiscoveryState.takePreparationBindingChanges successorState
  ensureCatalogue
    ("preparation binding notifications differ after " <> show operation <> ": " <> show observed)
    (observed == expected)
  ensureCatalogue
    "consuming delivery notifications changes no discovery semantics"
    (consumed == DiscoveryState.clearPreparationBindingChanges successorState)
  ensureCatalogue
    "consuming delivery notifications twice is empty"
    (Set.null (fst (DiscoveryState.takePreparationBindingChanges consumed)))
  pure successor

stepCatalogueTraceUnchecked ::
  DiscoveryFixture ->
  Discovery.PeerCandidate ->
  Discovery.PeerCandidate ->
  CatalogueTrace ->
  CatalogueTraceOperation ->
  Either String CatalogueTrace
stepCatalogueTraceUnchecked fixture winner loser trace operation = case operation of
  OfferLosingCandidate -> applyCatalogueHello fixture loser trace
  OfferWinningCandidate -> applyCatalogueHello fixture winner trace
  ReofferCurrentBinding -> case catalogueTraceCurrentBinding fixture trace of
    Nothing -> pure trace
    Just binding ->
      applyCatalogueHello
        fixture
        (Discovery.peerBindingSelectedCandidate binding)
        trace
  ResumeCurrentBinding ->
    applyCatalogueResume fixture CurrentResumeTarget trace
  ResumeStaleBinding ->
    applyCatalogueResume fixture StaleResumeTarget trace
  LoseCurrentBinding ->
    applyCatalogueLoss fixture CurrentResumeTarget trace
  LoseStaleBinding ->
    applyCatalogueLoss fixture StaleResumeTarget trace

applyCatalogueHello ::
  DiscoveryFixture ->
  Discovery.PeerCandidate ->
  CatalogueTrace ->
  Either String CatalogueTrace
applyCatalogueHello fixture candidate (CatalogueTrace state reference retained resumes) = do
  prepared <-
    catalogueEither
      "generated peer Hello"
      ( DiscoveryState.preparePeerHello
          candidate
          (catalogueHelloForCandidate fixture candidate)
          state
      )
  let (successorState, observedDisposition) =
        DiscoveryState.commitPeerHello prepared
      (successorReference, expectedDisposition) =
        referenceCatalogueHello candidate reference
  successorBindings <- case (expectedDisposition, observedDisposition) of
    ( CatalogueReferenceAccepted expectedAdmission expectedBinding,
      Discovery.PeerHelloAccepted observedAdmission observedBinding
      ) -> do
        ensureCatalogue
          ( "generated Hello admission differed: expected "
              <> show expectedAdmission
              <> ", observed "
              <> show observedAdmission
          )
          (observedAdmission == expectedAdmission)
        ensureCatalogue
          ( "generated Hello binding differed: expected "
              <> show expectedBinding
              <> ", observed "
              <> show (referenceBinding observedBinding)
          )
          (referenceBinding observedBinding == expectedBinding)
        pure
          ( if observedBinding `elem` retained
              then retained
              else observedBinding : retained
          )
    ( CatalogueReferenceRejected expectedWinner,
      Discovery.PeerHelloRejected
        (Discovery.HelloCandidateNotSelected observedWinner)
      ) -> do
        ensureCatalogue
          "generated losing Hello named the wrong retained winner"
          (observedWinner == expectedWinner)
        ensureCatalogue
          "generated rejected Hello changed discovery state"
          (successorState == state)
        pure retained
    _ ->
      Left
        ( "generated Hello disposition differed: expected "
            <> show expectedDisposition
            <> ", observed "
            <> show observedDisposition
        )
  let successor =
        CatalogueTrace
          successorState
          successorReference
          successorBindings
          resumes
  checkCatalogueTrace "after generated Hello" fixture successor
  pure successor

applyCatalogueResume ::
  DiscoveryFixture ->
  CatalogueResumeTarget ->
  CatalogueTrace ->
  Either String CatalogueTrace
applyCatalogueResume fixture target trace@(CatalogueTrace state reference retained resumes) =
  case selectCatalogueBinding fixture target trace of
    Nothing -> pure trace
    Just binding -> do
      prepared <-
        catalogueEither
          "generated Resume"
          (DiscoveryState.prepareReconnectCatalogue binding state)
      let (successorState, observedPending) =
            DiscoveryState.commitReconnectCatalogue prepared
          (successorReference, expectedPending) =
            referenceCatalogueResume (referenceBinding binding) reference
      ensureCatalogue
        ( "generated Resume permission differed for "
            <> show target
            <> ": expected "
            <> show expectedPending
            <> ", observed "
            <> show observedPending
        )
        (observedPending == expectedPending)
      ensureCatalogue
        "a stale or duplicate generated Resume changed discovery state"
        (expectedPending || successorState == state)
      let successor =
            CatalogueTrace
              successorState
              successorReference
              retained
              (resumes <> [(target, observedPending)])
      checkCatalogueTrace "after generated Resume" fixture successor
      pure successor

applyCatalogueLoss ::
  DiscoveryFixture ->
  CatalogueResumeTarget ->
  CatalogueTrace ->
  Either String CatalogueTrace
applyCatalogueLoss fixture target trace@(CatalogueTrace state reference retained resumes) =
  case selectCatalogueBinding fixture target trace of
    Nothing -> pure trace
    Just binding -> do
      prepared <-
        catalogueEither
          "generated binding loss"
          (DiscoveryState.prepareBindingLoss binding state)
      let (successorState, observedCurrent) =
            DiscoveryState.commitBindingLoss prepared
          (successorReference, expectedCurrent) =
            referenceCatalogueLoss (referenceBinding binding) reference
      ensureCatalogue
        ( "generated binding-loss classification differed: expected "
            <> show expectedCurrent
            <> ", observed "
            <> show observedCurrent
        )
        (observedCurrent == expectedCurrent)
      ensureCatalogue
        "a stale generated binding loss changed discovery state"
        (expectedCurrent || successorState == state)
      let successor =
            CatalogueTrace
              successorState
              successorReference
              retained
              resumes
      checkCatalogueTrace "after generated binding loss" fixture successor
      pure successor

armCurrentCatalogue ::
  DiscoveryFixture ->
  Discovery.PeerCandidate ->
  Discovery.PeerCandidate ->
  CatalogueTrace ->
  Either String CatalogueTrace
armCurrentCatalogue fixture winner loser trace =
  stepCatalogueTrace fixture winner loser trace operation
  where
    operation = case catalogueTraceCurrentBinding fixture trace of
      Nothing -> OfferWinningCandidate
      Just _ -> ReofferCurrentBinding

referenceCatalogueHello ::
  Discovery.PeerCandidate ->
  CatalogueReference ->
  (CatalogueReference, CatalogueReferenceHelloDisposition)
referenceCatalogueHello candidate reference@(CatalogueReference current lastGeneration _) =
  case current of
    Nothing -> install (if lastGeneration == 0 then Discovery.FirstBinding else Discovery.ReplacedBinding)
    Just binding@(CatalogueReferenceBinding selected _)
      | candidate == selected ->
          ( CatalogueReference (Just binding) lastGeneration True,
            CatalogueReferenceAccepted Discovery.ReofferedBinding binding
          )
      | candidate > selected ->
          (reference, CatalogueReferenceRejected selected)
      | otherwise -> install Discovery.ReplacedBinding
  where
    install admission =
      let generation = lastGeneration + 1
          binding = CatalogueReferenceBinding candidate generation
       in ( CatalogueReference (Just binding) generation True,
            CatalogueReferenceAccepted admission binding
          )

referenceCatalogueResume ::
  CatalogueReferenceBinding ->
  CatalogueReference ->
  (CatalogueReference, Bool)
referenceCatalogueResume binding reference@(CatalogueReference current lastGeneration pending)
  | current == Just binding,
    pending =
      (CatalogueReference current lastGeneration False, True)
  | otherwise = (reference, False)

referenceCatalogueLoss ::
  CatalogueReferenceBinding ->
  CatalogueReference ->
  (CatalogueReference, Bool)
referenceCatalogueLoss binding reference@(CatalogueReference current lastGeneration _)
  | current == Just binding =
      (CatalogueReference Nothing lastGeneration False, True)
  | otherwise = (reference, False)

checkCatalogueTrace ::
  String ->
  DiscoveryFixture ->
  CatalogueTrace ->
  Either String ()
checkCatalogueTrace context fixture (CatalogueTrace state reference retained _) = do
  catalogueEither context (DiscoveryState.validateDiscoveryState state)
  let current = DiscoveryState.currentPeerBinding fixture.remoteEpoch state
      allCurrent = DiscoveryState.currentPeerBindings state
      CatalogueReference expectedCurrent expectedLastGeneration expectedPending = reference
      observedCurrent = referenceBinding <$> current
      observedLastGeneration =
        maximum
          ( 0
              : fmap
                ( Discovery.peerBindingGenerationWord64
                    . Discovery.peerBindingGeneration
                )
                retained
          )
  ensureCatalogue
    (context <> ": current binding differs from reference")
    (observedCurrent == expectedCurrent)
  ensureCatalogue
    (context <> ": current binding collection differs from reference")
    (allCurrent == maybe [] pure current)
  ensureCatalogue
    (context <> ": retained last generation differs from reference")
    (observedLastGeneration == expectedLastGeneration)
  case current of
    Nothing ->
      ensureCatalogue
        (context <> ": absent binding retained catalogue permission")
        (not expectedPending)
    Just binding -> do
      prepared <-
        catalogueEither
          (context <> ": inspect catalogue permission")
          (DiscoveryState.prepareReconnectCatalogue binding state)
      ensureCatalogue
        (context <> ": catalogue permission differs from reference")
        ( DiscoveryState.preparedReconnectCatalogueWasPending prepared
            == expectedPending
        )

catalogueTraceCurrentBinding ::
  DiscoveryFixture ->
  CatalogueTrace ->
  Maybe Discovery.PeerBinding
catalogueTraceCurrentBinding fixture (CatalogueTrace state _ _ _) =
  DiscoveryState.currentPeerBinding fixture.remoteEpoch state

selectCatalogueBinding ::
  DiscoveryFixture ->
  CatalogueResumeTarget ->
  CatalogueTrace ->
  Maybe Discovery.PeerBinding
selectCatalogueBinding fixture target trace@(CatalogueTrace _ _ retained _) =
  case target of
    CurrentResumeTarget -> current
    StaleResumeTarget -> find ((/= current) . Just) retained
  where
    current = catalogueTraceCurrentBinding fixture trace

referenceBinding :: Discovery.PeerBinding -> CatalogueReferenceBinding
referenceBinding binding =
  CatalogueReferenceBinding
    (Discovery.peerBindingSelectedCandidate binding)
    ( Discovery.peerBindingGenerationWord64
        (Discovery.peerBindingGeneration binding)
    )

catalogueCandidateOrder ::
  DiscoveryFixture ->
  (Discovery.PeerCandidate, Discovery.PeerCandidate)
catalogueCandidateOrder fixture =
  if candidateA < candidateB
    then (candidateA, candidateB)
    else (candidateB, candidateA)
  where
    candidateA =
      Discovery.peerCandidate
        fixture.localId
        fixture.localEpoch
        (Discovery.connectionNonce 17)
    candidateB = remoteCandidate fixture 23

catalogueHelloForCandidate ::
  DiscoveryFixture ->
  Discovery.PeerCandidate ->
  Discovery.PeerHello
catalogueHelloForCandidate fixture candidate =
  Discovery.peerHello
    fixture.systemId
    fixture.remoteId
    fixture.remoteEpoch
    (Discovery.peerCandidateConnectionNonce candidate)
    Set.empty
    (controlIndex 99)
    fixture.catalogueDigest
    fixture.projectionDigest
    Nothing

catalogueEither ::
  (Show problem) =>
  String ->
  Either problem value ->
  Either String value
catalogueEither context =
  either (Left . ((context <> ": ") <>) . show) Right

ensureCatalogue :: String -> Bool -> Either String ()
ensureCatalogue _ True = Right ()
ensureCatalogue problem False = Left problem

caseDynamicAdmission :: Assertion
caseDynamicAdmission = do
  let fixture = discoveryFixture
      (preparing, activatedRecord, activatedMembership, _) = discoveryAdmissionFixture fixture
      manifest = Admission.admissionRecordManifest preparing
      applicant = Admission.admissionManifestHeraldEpoch manifest
      identifier = Admission.admissionManifestHeraldId manifest
      address = Discovery.peerAddress "tcp://newcomer.example:49000"
      nonce = Discovery.connectionNonce 991
      candidate = Discovery.peerCandidate identifier applicant nonce
      hello =
        Discovery.peerHello
          fixture.systemId
          identifier
          applicant
          nonce
          (Set.singleton address)
          (controlIndex 99)
          fixture.catalogueDigest
          fixture.projectionDigest
          Nothing
  (bound, binding) <- bindRemote fixture (Set.singleton (Discovery.peerAddress "tcp://old.example:49001"))
  learned <- checked "unknown admission hint" (DiscoveryState.prepareKnownContacts binding [Discovery.knownHerald identifier applicant (Set.singleton address)] bound)
  let hinted = fst (DiscoveryState.commitKnownContacts learned)
  pending <- checked "Oracle Begin admission" (DiscoveryState.prepareHeraldAdmission preparing hinted)
  let catalogued = DiscoveryState.commitHeraldAdmission pending
  assertEqual "genesis map remains immutable" (DiscoveryState.discoveryStaticView bound) (DiscoveryState.discoveryStaticView catalogued)
  assertEqual "explicit pending manifest is in live catalogue" (Just identifier) (Map.lookup applicant (DiscoveryState.liveHeraldCatalogue catalogued))
  assertEqual "pending contact receives no ordinary dial intent" Nothing (DiscoveryState.peerDialIntentFor identifier applicant catalogued)
  rejected <- checked "pending Hello rejected" (DiscoveryState.preparePeerHello candidate hello catalogued)
  assertEqual "pending is not an active binding" (Discovery.PeerHelloRejected Discovery.HelloInactiveHerald) (DiscoveryState.preparedPeerHelloDisposition rejected)
  assertMembershipAdvanceRejected "generation without activated Oracle outcome" activatedMembership catalogued
  activated <- checked "Oracle activated record" (DiscoveryState.prepareHeraldAdmission activatedRecord catalogued)
  transition <- checked "exact activated membership" (DiscoveryState.prepareMembershipAdvance activatedMembership (DiscoveryState.commitHeraldAdmission activated))
  let serving = fst (DiscoveryState.commitMembershipAdvance transition)
  assertBool "activated contact now owns an ordinary dial intent" (DiscoveryState.peerDialIntentFor identifier applicant serving /= Nothing)
  accepted <- checked "activated Hello" (DiscoveryState.preparePeerHello candidate hello serving)
  let (connected, disposition) = DiscoveryState.commitPeerHello accepted
  _ <- admittedBinding "activated newcomer" Discovery.FirstBinding disposition
  assertEqual "activation preserves old binding" (Just binding) (DiscoveryState.currentPeerBinding fixture.remoteEpoch connected)
  retiredMembership <- checked "retire admitted newcomer" (retireHeraldMembershipGeneration (controlIndex 100) (fixtureRetirementResolution (controlIndex 100)) applicant activatedMembership)
  retirement <- checked "retire admitted binding" (DiscoveryState.prepareMembershipAdvance retiredMembership connected)
  let retired = fst (DiscoveryState.commitMembershipAdvance retirement)
  assertEqual "retired epoch remains append-only catalogue history" (Just identifier) (Map.lookup applicant (DiscoveryState.liveHeraldCatalogue retired))
  assertEqual "retired contact cannot dial" Nothing (DiscoveryState.peerDialIntentFor identifier applicant retired)
  stale <- checked "retired admitted Hello" (DiscoveryState.preparePeerHello candidate hello retired)
  assertEqual "retired Hello is terminal" (Discovery.PeerHelloRejected Discovery.HelloRetiredHerald) (DiscoveryState.preparedPeerHelloDisposition stale)

-- Construct real Oracle evidence, including every old member's seal and base
-- readiness. Discovery consumes only the initial and terminal retained records.
discoveryAdmissionFixture :: DiscoveryFixture -> (Admission.HeraldAdmissionRecord, Admission.HeraldAdmissionRecord, HeraldMembershipGeneration, OracleState.OracleState)
discoveryAdmissionFixture fixture =
  let targetId = checkedValue "join ID" (mkHeraldId (fixtureIdentifierBytes 240))
      targetEpoch = checkedValue "join epoch" (mkHeraldEpoch (fixtureIdentifierBytes 241))
      manifest = Admission.heraldAdmissionManifest fixture.systemId targetId targetEpoch
      generation = fixture.initialMembership
      members = checkedActiveHeralds fixtureCheckedGenesis
      old = Set.toAscList (Set.fromList (map heraldMemberEpoch members))
      occurrence = checkedValue "join occurrence" (Topology.mkTopologyOccurrenceDigest (fixtureIdentifierBytes 242))
      predecessor = Topology.sameGenerationPredecessor (checkedValue "join predecessor" (mkTopologyCutId (fixtureIdentifierBytes 243)))
      cut index = checkedValue "join cut" (Topology.topologyCut predecessor (Topology.topologyFrontier (Structural.emptyStructuralVersionVector generation) (controlIndex index)) occurrence)
      initial = checkedValue "join Oracle genesis" (initialOracle (fixtureCheckedOracleGenesisFor fixtureCheckedGenesis))
      begun = step initial (1, (fixture.localEpoch, Admission.BeginHeraldAdmission manifest (cut 0)))
      preparing = maybe (error "Oracle Begin omitted admission") id (OracleState.oraclePendingHeraldAdmission begun)
      identity = Admission.admissionRecordId preparing
      recipe = checkedValue "join recipe" (Topology.heraldJoinBaseRecipe identity targetEpoch generation (cut 1) occurrence)
      digest = checkedValue "join digest" (Admission.mkHeraldJoinDigest (Topology.topologyOccurrenceDigestBytes occurrence))
      recipeDigest = checkedValue "join recipe digest" (Admission.mkHeraldJoinDigest (Topology.heraldJoinBaseRecipeDigestBytes recipe))
      seal = checkedValue "seal" (Admission.heraldJoinSeal identity 1 (cut 1) (controlIndex 1) [(h, Admission.heraldJoinMemberCut 0 0 digest) | h <- old] digest recipeDigest)
      report h = Admission.heraldJoinReadyReport identity 1 h (Admission.joinSealDigest seal) (controlIndex 1) recipeDigest
      coordinator = case old of
        member : _ -> member
        [] -> error "discovery admission fixture has no predecessor member"
      commands =
        [(coordinator, Admission.SealHeraldAdmission seal)]
          <> [(h, Admission.AcceptHeraldJoinSeal identity 1 (Admission.joinSealDigest seal)) | h <- old]
          <> [(h, Admission.HeraldJoinBaseReady (report h)) | h <- old]
          <> [(fixture.localEpoch, Admission.HeraldJoinReady (report targetEpoch)), (fixture.localEpoch, Admission.ActivateHerald identity)]
      step state (index, (home, command)) =
        let envelope = OracleCommand.oracleEnvelope (oracleClientRequestId home index) Nothing home (OracleCommand.heraldAdmissionCommand command)
         in case checkedValue "join evidence" (stepOracle envelope state) of
              (next, OracleCommitted _, _) -> next
              (_, outcome, _) -> error ("join command rejected: " <> show outcome)
      final = foldl step begun (zip [2 ..] commands)
      activated = maybe (error "complete Oracle admission omitted record") id (OracleState.oracleHeraldAdmission identity final)
   in case Admission.admissionRecordPhase activated of
        Admission.AdmissionActivated _ successor -> (preparing, activated, successor, final)
        _ -> error "complete Oracle admission did not activate"

caseJoiningDiscoveryReplay :: Assertion
caseJoiningDiscoveryReplay = do
  let fixture = discoveryFixture
      (firstPreparing, firstActivated, predecessor, oracle) = discoveryAdmissionFixture fixture
  identifier <- checked "later newcomer ID" (mkHeraldId (fixtureIdentifierBytes 244))
  epoch <- checked "later newcomer epoch" (mkHeraldEpoch (fixtureIdentifierBytes 245))
  parentCut <- checked "later newcomer predecessor cut" (mkTopologyCutId (fixtureIdentifierBytes 246))
  digest <- checked "later newcomer cut digest" (Topology.mkTopologyOccurrenceDigest (fixtureIdentifierBytes 247))
  cut <-
    checked
      "later newcomer anchor"
      ( Topology.topologyCut
          (Topology.sameGenerationPredecessor parentCut)
          (Topology.topologyFrontier (Structural.emptyStructuralVersionVector predecessor) (OracleState.oracleGreatestControlIndex oracle))
          digest
      )
  let manifest = Admission.heraldAdmissionManifest fixture.systemId identifier epoch
      envelope =
        OracleCommand.oracleEnvelope
          (oracleClientRequestId fixture.localEpoch 1000)
          Nothing
          fixture.localEpoch
          (OracleCommand.heraldAdmissionCommand (Admission.BeginHeraldAdmission manifest cut))
  (advanced, outcome, _) <- checked "begin later newcomer" (stepOracle envelope oracle)
  pending <- case (outcome, OracleState.oraclePendingHeraldAdmission advanced) of
    (OracleCommitted _, Just record) -> pure record
    _ -> assertFailure ("later newcomer Begin failed: " <> show outcome)
  let static =
        discoveryStatic
          fixture.systemId
          identifier
          epoch
          (checkedActiveHeralds fixtureCheckedGenesis)
          fixture.catalogueDigest
          fixture.projectionDigest
          fixture.initialMembership
  initial <- checked "later pending initializer" (DiscoveryState.initialJoiningState static pending)
  assertEqual "pending provenance does not skip global admission replay" Nothing (Map.lookup epoch (DiscoveryState.liveHeraldCatalogue initial))
  first <- checked "replay first Begin" (DiscoveryState.prepareHeraldAdmission firstPreparing initial)
  activated <- checked "replay first Activated" (DiscoveryState.prepareHeraldAdmission firstActivated (DiscoveryState.commitHeraldAdmission first))
  membership <- checked "replay prior membership activation" (DiscoveryState.prepareMembershipAdvance predecessor (DiscoveryState.commitHeraldAdmission activated))
  let previous = fst (DiscoveryState.commitMembershipAdvance membership)
  second <- checked "replay this applicant Begin at its actual predecessor" (DiscoveryState.prepareHeraldAdmission pending previous)
  let replayed = DiscoveryState.commitHeraldAdmission second
  assertEqual "contiguous replay catalogues the second applicant" (Just identifier) (Map.lookup epoch (DiscoveryState.liveHeraldCatalogue replayed))
  assertEqual "pending newcomer remains without an ordinary peer binding" Nothing (DiscoveryState.currentPeerBinding epoch replayed)
  checked "replayed joining discovery remains valid" (DiscoveryState.validateDiscoveryState replayed)

data JoiningControlBaseFixture = JoiningControlBaseFixture
  { initial :: DiscoveryState.State,
    history :: HeraldMembershipHistory,
    admissions :: [Admission.HeraldAdmissionRecord],
    replayed :: DiscoveryState.State
  }

-- The source catalogue contains both kinds of historical terminal admission
-- before the current applicant. Each fact comes from real checked Oracle steps.
joiningControlBaseFixture :: IO JoiningControlBaseFixture
joiningControlBaseFixture = do
  let fixture = discoveryFixture
      (firstPreparing, firstActivated, predecessor, oracle) = discoveryAdmissionFixture fixture
      step request command source = do
        let envelope = OracleCommand.oracleEnvelope (oracleClientRequestId fixture.localEpoch request) Nothing fixture.localEpoch (OracleCommand.heraldAdmissionCommand command)
        (successor, outcome, _) <- checked "control-base admission command" (stepOracle envelope source)
        case outcome of
          OracleCommitted _ -> pure successor
          _ -> assertFailure ("control-base admission command rejected: " <> show outcome)
      begin idByte epochByte request source = do
        identifier <- checked "control-base applicant ID" (mkHeraldId (fixtureIdentifierBytes idByte))
        epoch <- checked "control-base applicant epoch" (mkHeraldEpoch (fixtureIdentifierBytes epochByte))
        parentCut <- checked "control-base admission predecessor cut" (mkTopologyCutId (fixtureIdentifierBytes 246))
        digest <- checked "control-base admission digest" (Topology.mkTopologyOccurrenceDigest (fixtureIdentifierBytes 247))
        cut <- checked "control-base admission anchor" (Topology.topologyCut (Topology.sameGenerationPredecessor parentCut) (Topology.topologyFrontier (Structural.emptyStructuralVersionVector predecessor) (OracleState.oracleGreatestControlIndex source)) digest)
        successor <- step request (Admission.BeginHeraldAdmission (Admission.heraldAdmissionManifest fixture.systemId identifier epoch) cut) source
        record <- maybe (assertFailure "control-base Begin omitted its pending admission") pure (OracleState.oraclePendingHeraldAdmission successor)
        pure (successor, record)
      observe record before = DiscoveryState.commitHeraldAdmission <$> checked "replay control-base admission" (DiscoveryState.prepareHeraldAdmission record before)
  (begunCancelled, cancelledPreparing) <- begin 244 245 1000 oracle
  afterCancel <- step 1001 (Admission.CancelHeraldAdmission (Admission.admissionRecordId cancelledPreparing)) begunCancelled
  cancelled <- maybe (assertFailure "control-base Cancel omitted its admission") pure (OracleState.oracleHeraldAdmission (Admission.admissionRecordId cancelledPreparing) afterCancel)
  (source, pending) <- begin 248 249 1002 afterCancel
  let manifest = Admission.admissionRecordManifest pending
      static = discoveryStatic fixture.systemId (Admission.admissionManifestHeraldId manifest) (Admission.admissionManifestHeraldEpoch manifest) (checkedActiveHeralds fixtureCheckedGenesis) fixture.catalogueDigest fixture.projectionDigest fixture.initialMembership
  pristine <- checked "control-base joining initializer" (DiscoveryState.initialJoiningState static pending)
  locator <- checked "receiver-local joining application locator" (Lifecycle.heraldLocator "joining.example" 42002)
  let initial = DiscoveryState.rememberLocalApplicationLocator (Just locator) pristine
  first <- observe firstPreparing initial >>= observe firstActivated
  membership <- checked "replay control-base activated membership" (DiscoveryState.prepareMembershipAdvance predecessor first)
  let advanced = fst (DiscoveryState.commitMembershipAdvance membership)
  replayed <- observe cancelledPreparing advanced >>= observe cancelled >>= observe pending
  pure (JoiningControlBaseFixture initial (OracleState.oracleCheckedMembershipHistory source) (OracleState.oracleHeraldAdmissions source) replayed)

caseJoiningControlBaseReplay :: Assertion
caseJoiningControlBaseReplay = do
  fixture <- joiningControlBaseFixture
  prepared <- checked "prepare joining discovery control base" (DiscoveryState.prepareJoiningControlBase fixture.history fixture.admissions fixture.initial)
  let installed = DiscoveryState.commitJoiningControlBase prepared
  assertEqual "current facts and receiver-local contacts equal contiguous replay" fixture.replayed installed
  assertEqual "install creates no ordinary peer bindings" [] (DiscoveryState.currentPeerBindings installed)
  assertEqual "install preserves immutable receiver discovery identity" (DiscoveryState.discoveryStaticView fixture.initial) (DiscoveryState.discoveryStaticView installed)
  checked "installed discovery owner remains valid" (DiscoveryState.validateDiscoveryState installed)

caseJoiningControlBaseReceiver :: Assertion
caseJoiningControlBaseReceiver = do
  fixture <- joiningControlBaseFixture
  (serving, _) <- bindRemote discoveryFixture Set.empty
  reject "an active receiver's binding cannot be replaced" DiscoveryState.JoiningControlBaseNotFreshObserver (DiscoveryState.prepareJoiningControlBase fixture.history fixture.admissions serving)
  prepared <- checked "initial checked control base" (DiscoveryState.prepareJoiningControlBase fixture.history fixture.admissions fixture.initial)
  reject "an already advanced observer cannot rewind" DiscoveryState.JoiningControlBaseNotFreshObserver (DiscoveryState.prepareJoiningControlBase fixture.history fixture.admissions (DiscoveryState.commitJoiningControlBase prepared))
  where
    reject context expected result = case result of
      Left observed -> assertEqual context expected observed
      Right _ -> assertFailure (context <> " was accepted")

caseJoiningControlBaseContext :: Assertion
caseJoiningControlBaseContext = do
  fixture <- joiningControlBaseFixture
  let genesis = discoveryFixture.initialMembership
  earlier <- checked "original membership history" (heraldMembershipHistory (NonEmpty.singleton genesis))
  otherGenesis <- checked "different bootstrap membership" (genesisHeraldMembershipGeneration discoveryFixture.systemId (NonEmpty.singleton discoveryFixture.localEpoch))
  other <- checked "different checked membership history" (heraldMembershipHistory (NonEmpty.singleton otherGenesis))
  reject "a different genesis cannot authorize this observer" DiscoveryState.JoiningControlBaseGenesisMismatch (DiscoveryState.prepareJoiningControlBase other fixture.admissions fixture.initial)
  reject "an earlier cut cannot authorize the current pending predecessor" DiscoveryState.JoiningControlBaseAdmissionMismatch (DiscoveryState.prepareJoiningControlBase earlier fixture.admissions fixture.initial)
  reject "a catalogue before this applicant began is not this base" DiscoveryState.JoiningControlBaseAdmissionMismatch (DiscoveryState.prepareJoiningControlBase fixture.history [] fixture.initial)
  where
    reject context expected result = case result of
      Left observed -> assertEqual context expected observed
      Right _ -> assertFailure (context <> " was accepted")

caseContactHints :: Assertion
caseContactHints = do
  let fixture = discoveryFixture
      addressA = Discovery.peerAddress "peer-a.example:4040"
      addressB = Discovery.peerAddress "peer-b.example:4040"
  (bound, binding) <- bindRemote fixture (Set.singleton addressA)
  learnedId <- checked "learned HeraldId" (mkHeraldId (fixtureIdentifierBytes 204))
  learnedEpoch <- checked "learned HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes 205))
  let observation = Discovery.knownHerald learnedId learnedEpoch (Set.singleton addressB)
  prepared <- checked "known contacts" (DiscoveryState.prepareKnownContacts binding [observation, observation] bound)
  let (successor, disposition) = DiscoveryState.commitKnownContacts prepared
  assertEqual "new hints change the hint set" Discovery.KnownContactsChanged disposition
  assertEqual "a learned hint grants no binding" Nothing (DiscoveryState.currentPeerBinding learnedEpoch successor)
  let retained = DiscoveryState.knownHeralds successor
  assertBool
    "the hello address and learned address are both retained"
    ( Set.fromList
        [ (Discovery.knownHeraldId known, Discovery.knownHeraldObservedEpoch known)
        | known <- retained
        ]
        == Set.fromList
          [ (fixture.remoteId, fixture.remoteEpoch),
            (learnedId, learnedEpoch)
          ]
    )
  retry <- checked "known contacts retry" (DiscoveryState.prepareKnownContacts binding [observation] successor)
  let (afterRetry, retryDisposition) = DiscoveryState.commitKnownContacts retry
  assertEqual "reoffer is idempotent" Discovery.KnownContactsUnchanged retryDisposition
  assertEqual "reoffer is state-identical" successor afterRetry

caseBindingLoss :: Assertion
caseBindingLoss = do
  let fixture = discoveryFixture
      address = Discovery.peerAddress "peer.example:4040"
  (afterFirst, firstBinding) <- bindRemote fixture (Set.singleton address)
  let replacement =
        Discovery.peerCandidate
          fixture.localId
          fixture.localEpoch
          (Discovery.connectionNonce 5)
      replacementHello = remoteHello fixture 5 Set.empty
  replacementPrepared <-
    checked
      "replacement"
      (DiscoveryState.preparePeerHello replacement replacementHello afterFirst)
  let (afterReplacement, replacementDisposition) = DiscoveryState.commitPeerHello replacementPrepared
  replacementBinding <- admittedBinding "replacement" Discovery.ReplacedBinding replacementDisposition

  staleLoss <- checked "stale loss" (DiscoveryState.prepareBindingLoss firstBinding afterReplacement)
  let (afterStaleLoss, staleWasCurrent) = DiscoveryState.commitBindingLoss staleLoss
  assertBool "stale loss is not current" (not staleWasCurrent)
  assertEqual "stale loss is state-identical" afterReplacement afterStaleLoss

  currentLoss <- checked "current loss" (DiscoveryState.prepareBindingLoss replacementBinding afterReplacement)
  let (afterCurrentLoss, currentWasCurrent) = DiscoveryState.commitBindingLoss currentLoss
  assertBool "current loss clears the binding" currentWasCurrent
  assertEqual "the binding is absent" Nothing (DiscoveryState.currentPeerBinding fixture.remoteEpoch afterCurrentLoss)
  assertBool
    "disconnect preserves address hints"
    ( any
        ((== Set.singleton address) . Discovery.knownHeraldAddresses)
        (DiscoveryState.knownHeralds afterCurrentLoss)
    )

caseRetiredHelloRejected :: Assertion
caseRetiredHelloRejected = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      candidate = remoteCandidate fixture 71
      hello = remoteHello fixture 71 Set.empty
  _ <- checked "pre-retirement queued Hello" (DiscoveryState.preparePeerHello candidate hello initial)
  afterRetirement <- retireMembership "retire remote" fixture.remoteEpoch fixture.initialMembership initial
  assertRejected
    "rechecked old Hello"
    Discovery.HelloRetiredHerald
    candidate
    hello
    afterRetirement
  let unknownId = checkedValue "unknown Herald id" (mkHeraldId (fixtureIdentifierBytes 0xe1))
      unknownEpoch = checkedValue "unknown Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0xe2))
      unknownCandidate = Discovery.peerCandidate unknownId unknownEpoch (Discovery.connectionNonce 72)
      unknownHello =
        Discovery.peerHello
          fixture.systemId
          unknownId
          unknownEpoch
          (Discovery.connectionNonce 72)
          Set.empty
          (controlIndex 0)
          fixture.catalogueDigest
          fixture.projectionDigest
          Nothing
  assertRejected
    "unknown Herald remains merely inactive"
    Discovery.HelloInactiveHerald
    unknownCandidate
    unknownHello
    afterRetirement

caseRetiredBindingRemoved :: Assertion
caseRetiredBindingRemoved = do
  let fixture = discoveryFixture
      address = Discovery.peerAddress "retired.example:4040"
      candidate = remoteCandidate fixture 73
      hello = remoteHello fixture 73 (Set.singleton address)
      initial = DiscoveryState.initialState fixture.staticView
  prepared <- checked "bound before retirement" (DiscoveryState.preparePeerHello candidate hello initial)
  let (bound, disposition) = DiscoveryState.commitPeerHello prepared
  binding <- admittedBinding "bound before retirement" Discovery.FirstBinding disposition
  successor <- checked "retired generation" (retireHeraldMembershipGeneration (controlIndex 12) (fixtureRetirementResolution (controlIndex 12)) fixture.remoteEpoch fixture.initialMembership)
  advance <- checked "prepared retirement" (DiscoveryState.prepareMembershipAdvance successor (DiscoveryState.clearPreparationBindingChanges bound))
  assertEqual "the exact current binding is returned" [binding] (DiscoveryState.preparedMembershipRetiredBindings advance)
  assertBool "the local member survives" (not (DiscoveryState.preparedMembershipLocalRetired advance))
  let (retired, _) = DiscoveryState.commitMembershipAdvance advance
  assertEqual "retired binding is absent" Nothing (DiscoveryState.currentPeerBinding fixture.remoteEpoch retired)
  assertEqual "binding retirement wakes exactly that peer's delivery work" (Set.singleton fixture.remoteEpoch) (fst (DiscoveryState.takePreparationBindingChanges retired))
  assertEqual "retired retained contact cannot redial" Nothing (DiscoveryState.peerDialIntentFor fixture.remoteId fixture.remoteEpoch retired)

caseMembershipAdvanceConflicts :: Assertion
caseMembershipAdvanceConflicts = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
  remoteSuccessor <- checked "remote successor" (retireHeraldMembershipGeneration (controlIndex 13) (fixtureRetirementResolution (controlIndex 13)) fixture.remoteEpoch fixture.initialMembership)
  localSuccessor <- checked "local successor" (retireHeraldMembershipGeneration (controlIndex 13) (fixtureRetirementResolution (controlIndex 13)) fixture.localEpoch fixture.initialMembership)
  prepared <- checked "first successor" (DiscoveryState.prepareMembershipAdvance remoteSuccessor initial)
  let (advanced, _) = DiscoveryState.commitMembershipAdvance prepared
  assertMembershipAdvanceRejected "duplicate successor" remoteSuccessor advanced
  assertMembershipAdvanceRejected "conflicting genesis successor" localSuccessor advanced

caseLocalRetired :: Assertion
caseLocalRetired = do
  let fixture = discoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      opened = Discovery.peerCandidateOpened (Discovery.connectionNonce 79) Set.empty Nothing
  successor <- checked "local retired generation" (retireHeraldMembershipGeneration (controlIndex 14) (fixtureRetirementResolution (controlIndex 14)) fixture.localEpoch fixture.initialMembership)
  advance <- checked "local retirement" (DiscoveryState.prepareMembershipAdvance successor initial)
  assertBool "advance reports local retirement" (DiscoveryState.preparedMembershipLocalRetired advance)
  let (retired, _) = DiscoveryState.commitMembershipAdvance advance
  checked "locally retired state remains valid" (DiscoveryState.validateDiscoveryState retired)
  case DiscoveryState.prepareCandidateHello (controlIndex 15) opened retired of
    Left Discovery.DiscoveryLocalHeraldRetired -> pure ()
    Left problem -> assertFailure ("unexpected local-retirement fault: " <> show problem)
    Right _ -> assertFailure "locally retired Discovery authored a Hello"
  assertRejected
    "inbound Hello is rejected"
    Discovery.HelloLocalHeraldRetired
    (remoteCandidate fixture 79)
    (remoteHello fixture 79 Set.empty)
    retired
  assertEqual "local retirement suppresses every dial" [] (DiscoveryState.peerDialIntents retired)

caseSurvivorContinuity :: Assertion
caseSurvivorContinuity = do
  let fixture = threeMemberDiscoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
  afterRetirement <- retireMembership "retire third member" fixture.thirdEpoch fixture.initialMembership initial
  prepared <- checked "surviving remote Hello" (DiscoveryState.preparePeerHello (remoteCandidate fixture.base 83) (remoteHello fixture.base 83 Set.empty) afterRetirement)
  assertAccepted "surviving remote Hello" prepared

caseRepeatedSurvivorContinuity :: Assertion
caseRepeatedSurvivorContinuity = do
  let fixture = threeMemberDiscoveryFixture
      initial = DiscoveryState.initialState fixture.staticView
      candidate = remoteCandidate fixture.base 84
      hello = remoteHello fixture.base 84 Set.empty
  admitted <- checked "initial survivor binding" (DiscoveryState.preparePeerHello candidate hello initial)
  let (bound, disposition) = DiscoveryState.commitPeerHello admitted
  binding <- admittedBinding "initial survivor binding" Discovery.FirstBinding disposition
  first <- checked "first contraction" (retireHeraldMembershipGeneration (controlIndex 12) (fixtureRetirementResolution (controlIndex 12)) fixture.thirdEpoch fixture.initialMembership)
  firstAdvance <- checked "first Discovery contraction" (DiscoveryState.prepareMembershipAdvance first bound)
  assertEqual "another member's retirement leaves the survivor binding untouched" [] (DiscoveryState.preparedMembershipRetiredBindings firstAdvance)
  let (afterFirst, _) = DiscoveryState.commitMembershipAdvance firstAdvance
  assertEqual "the exact surviving binding remains current" (Just binding) (DiscoveryState.currentPeerBinding fixture.base.remoteEpoch afterFirst)
  second <- checked "second contraction" (retireHeraldMembershipGeneration (controlIndex 14) (fixtureRetirementResolution (controlIndex 14)) fixture.base.remoteEpoch first)
  secondAdvance <- checked "second Discovery contraction" (DiscoveryState.prepareMembershipAdvance second afterFirst)
  assertEqual "only the newly retired member's exact binding is returned" [binding] (DiscoveryState.preparedMembershipRetiredBindings secondAdvance)
  let (afterSecond, _) = DiscoveryState.commitMembershipAdvance secondAdvance
  checked "successive contraction invariant" (DiscoveryState.validateDiscoveryState afterSecond)
  assertEqual "no dial authority survives either removed member" [] (DiscoveryState.peerDialIntents afterSecond)
  assertRejected "historical binding cannot reopen after the second retirement" Discovery.HelloRetiredHerald candidate hello afterSecond
  assertMembershipAdvanceRejected "a historical generation cannot regain authority" first afterSecond

retireMembership :: String -> HeraldEpoch -> HeraldMembershipGeneration -> DiscoveryState.State -> IO DiscoveryState.State
retireMembership context target predecessor state = do
  successor <- checked (context <> " generation") (retireHeraldMembershipGeneration (controlIndex 11) (fixtureRetirementResolution (controlIndex 11)) target predecessor)
  prepared <- checked context (DiscoveryState.prepareMembershipAdvance successor state)
  pure (fst (DiscoveryState.commitMembershipAdvance prepared))

assertMembershipAdvanceRejected :: String -> HeraldMembershipGeneration -> DiscoveryState.State -> Assertion
assertMembershipAdvanceRejected context successor state =
  case DiscoveryState.prepareMembershipAdvance successor state of
    Left Discovery.DiscoveryMembershipNotExactSuccessor -> pure ()
    Left problem -> assertFailure (context <> " produced an unexpected fault: " <> show problem)
    Right _ -> assertFailure (context <> " was accepted")

data DiscoveryFixture = DiscoveryFixture
  { staticView :: DiscoveryStatic,
    systemId :: SystemId,
    localId :: HeraldId,
    localEpoch :: HeraldEpoch,
    remoteId :: HeraldId,
    remoteEpoch :: HeraldEpoch,
    catalogueDigest :: CatalogueDigest,
    projectionDigest :: InitialProjectionDigest,
    initialMembership :: HeraldMembershipGeneration
  }

discoveryFixture :: DiscoveryFixture
discoveryFixture =
  DiscoveryFixture
    { staticView =
        discoveryStatic
          systemId
          localId
          localEpoch
          (checkedActiveHeralds fixtureCheckedGenesis)
          catalogueDigest
          projectionDigest
          initialMembership,
      systemId,
      localId,
      localEpoch,
      remoteId = heraldMemberId fixtureRemoteMember,
      remoteEpoch = heraldMemberEpoch fixtureRemoteMember,
      catalogueDigest,
      projectionDigest,
      initialMembership
    }
  where
    systemId = checkedSystemId fixtureCheckedGenesis
    localId = checkedLocalHeraldId fixtureCheckedGenesis
    localEpoch = checkedLocalHeraldEpoch fixtureCheckedGenesis
    catalogueDigest = checkedCatalogueDigest fixtureCheckedGenesis
    projectionDigest =
      checkedInitialProjectionDigest
        ( checkedValue
            "empty checked initial projection"
            ( checkInitialBootstraps
                fixtureCheckedGenesis
                (PrimordialProcessManifest [])
            )
        )
    initialMembership =
      checkedValue
        "fixture membership"
        (genesisHeraldMembershipGeneration systemId (NonEmpty.fromList (fmap heraldMemberEpoch (checkedActiveHeralds fixtureCheckedGenesis))))

data ThreeMemberDiscoveryFixture = ThreeMemberDiscoveryFixture
  { base :: DiscoveryFixture,
    staticView :: DiscoveryStatic,
    initialMembership :: HeraldMembershipGeneration,
    thirdEpoch :: HeraldEpoch
  }

threeMemberDiscoveryFixture :: ThreeMemberDiscoveryFixture
threeMemberDiscoveryFixture =
  ThreeMemberDiscoveryFixture
    { base = base,
      staticView =
        discoveryStatic
          base.systemId
          base.localId
          base.localEpoch
          members
          base.catalogueDigest
          base.projectionDigest
          membership,
      initialMembership = membership,
      thirdEpoch
    }
  where
    base = discoveryFixture
    thirdEpoch = checkedValue "third epoch" (mkHeraldEpoch (fixtureIdentifierBytes 210))
    thirdId = checkedValue "third id" (mkHeraldId (fixtureIdentifierBytes 211))
    members = checkedActiveHeralds fixtureCheckedGenesis <> [HeraldMember thirdId thirdEpoch]
    membership = checkedValue "three-member membership" (genesisHeraldMembershipGeneration base.systemId (NonEmpty.fromList (fmap heraldMemberEpoch members)))

remoteCandidate :: DiscoveryFixture -> Word -> Discovery.PeerCandidate
remoteCandidate fixture nonce =
  Discovery.peerCandidate
    fixture.remoteId
    fixture.remoteEpoch
    (Discovery.connectionNonce (fromIntegral nonce))

remoteHello ::
  DiscoveryFixture ->
  Word ->
  Set.Set Discovery.PeerAddress ->
  Discovery.PeerHello
remoteHello fixture nonce addresses =
  Discovery.peerHello
    fixture.systemId
    fixture.remoteId
    fixture.remoteEpoch
    (Discovery.connectionNonce (fromIntegral nonce))
    addresses
    (controlIndex 99)
    fixture.catalogueDigest
    fixture.projectionDigest
    Nothing

bindRemote ::
  DiscoveryFixture ->
  Set.Set Discovery.PeerAddress ->
  IO (DiscoveryState.State, Discovery.PeerBinding)
bindRemote fixture addresses = do
  let candidate = remoteCandidate fixture 31
  prepared <-
    checked
      "initial peer binding"
      ( DiscoveryState.preparePeerHello
          candidate
          (remoteHello fixture 31 addresses)
          (DiscoveryState.initialState fixture.staticView)
      )
  let (state, disposition) = DiscoveryState.commitPeerHello prepared
  binding <- admittedBinding "initial peer binding" Discovery.FirstBinding disposition
  pure (state, binding)

assertAccepted :: String -> DiscoveryState.PreparedPeerHello -> Assertion
assertAccepted context prepared = case DiscoveryState.preparedPeerHelloDisposition prepared of
  Discovery.PeerHelloAccepted {} -> pure ()
  disposition -> assertFailure (context <> " was not accepted: " <> show disposition)

assertRejected ::
  String ->
  Discovery.HelloRejection ->
  Discovery.PeerCandidate ->
  Discovery.PeerHello ->
  DiscoveryState.State ->
  Assertion
assertRejected context expected candidate hello state = do
  prepared <- checked context (DiscoveryState.preparePeerHello candidate hello state)
  let (successor, disposition) = DiscoveryState.commitPeerHello prepared
  assertEqual context (Discovery.PeerHelloRejected expected) disposition
  assertEqual (context <> " leaves state unchanged") state successor

admittedBinding ::
  String ->
  Discovery.BindingAdmission ->
  Discovery.PeerHelloDisposition ->
  IO Discovery.PeerBinding
admittedBinding context expected disposition = case disposition of
  Discovery.PeerHelloAccepted observed binding -> do
    assertEqual (context <> " admission") expected observed
    pure binding
  _ -> assertFailure (context <> " was not admitted: " <> show disposition)

consumeCatalogue ::
  String ->
  Discovery.PeerBinding ->
  DiscoveryState.State ->
  IO (DiscoveryState.State, Bool)
consumeCatalogue context binding state = do
  prepared <-
    checked
      context
      (DiscoveryState.prepareReconnectCatalogue binding state)
  pure (DiscoveryState.commitReconnectCatalogue prepared)

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
