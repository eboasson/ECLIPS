{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerDialIntentProperties
  ( tests,
  )
where

import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    HeraldId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
  )
import Eclips.Domain.Membership
  ( heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Herald.Administration
  ( adminCorrelationId,
    checkedInitialAdministrationBinding,
  )
import Eclips.Herald.Discovery
  ( KnownHerald,
    PeerAddress,
    PeerBinding,
    PeerDialClass (..),
    PeerDialIntent,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    knownHerald,
    peerAddress,
    peerCandidate,
    peerDialIntentAddresses,
    peerDialIntentClass,
    peerDialIntentGenerationWord64,
    peerDialIntentHeraldEpoch,
    peerDialIntentHeraldId,
    peerHello,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    DeploymentManifest (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkedInitialProjectionDigest,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedSystemId,
  )
import Eclips.Herald.Initialization (HeraldState, initialHerald)
import Eclips.Herald.Input
  ( AdministrationIngress (OrderlyHeraldShutdown),
    HeraldInputBody (..),
    PeerControl (PeerKnownHeralds),
    PeerIngress (PeerControlReceived, PeerHelloReceived),
    RuntimeObservation (PeerBindingLost),
    heraldInput,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Startup.State (startupOracleProjectionState)
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureMembers,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
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
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Discovery-owned direct-dial intentions"
    [ testCase "changed gossip starts recovery and excludes its source before canonical eligible dials" caseKnownContactDialOrder,
      testCase "a live exact binding suppresses dialing and current loss re-emits retained intent" caseBindingPresenceAndLoss,
      testCase "identity ordering selects one immediate endpoint and one delayed fallback" caseDialClass,
      testCase "draining and stale loss emit no dial intention" caseDrainingAndStaleLoss,
      testProperty "contact input order and duplication preserve canonical dial effects" propCanonicalContactOrder
    ]

caseKnownContactDialOrder :: Assertion
caseKnownContactDialOrder = do
  let fixture = dialFixture
      (bound, sourceBinding) = bindMember 11 fixture.source mempty fixture.initial
      addressA = peerAddress "tcp://127.0.0.1:4103"
      addressB = peerAddress "tcp://127.0.0.1:4203"
      target = fixture.targetA
      observations =
        [ contact target (Set.singleton addressB),
          contact fixture.local (Set.singleton (peerAddress "tcp://127.0.0.1:4101")),
          knownHerald fixture.inactiveId fixture.inactiveEpoch (Set.singleton (peerAddress "tcp://127.0.0.1:4999")),
          contact fixture.targetB Set.empty
        ]
      (afterFirst, firstEffects) =
        stepBody 12 (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds observations))) bound
      firstMembers = effectBatchMembers firstEffects
  intent <- case firstMembers of
    [ArmTimer _ _, DialPeer dial] -> pure dial
    _ -> assertFailure ("unexpected changed-contact effects: " <> show firstMembers)
  assertIntent "first eligible target" target (Set.singleton addressB) intent
  assertEqual "the first unbound contact has generation one" 1 (peerDialIntentGenerationWord64 intent)

  let (afterDuplicate, duplicateEffects) =
        stepBody 13 (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds observations))) afterFirst
  assertEqual "exact duplicate gossip is effect-idempotent" [] (effectBatchMembers duplicateEffects)

  let growth = contact target (Set.singleton addressA)
      (_, growthEffects) =
        stepBody 14 (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds [growth]))) afterDuplicate
  grown <- case effectBatchMembers growthEffects of
    [DialPeer dial] -> pure dial
    members -> assertFailure ("unexpected address-growth effects: " <> show members)
  assertEqual
    "address growth cannot renew the already active recovery deadline"
    []
    (timerArmEffects growthEffects)
  assertIntent "address union" target (Set.fromList [addressA, addressB]) grown
  assertEqual
    "address growth retains the same permission generation"
    (peerDialIntentGenerationWord64 intent)
    (peerDialIntentGenerationWord64 grown)

caseBindingPresenceAndLoss :: Assertion
caseBindingPresenceAndLoss = do
  let fixture = dialFixture
      target = fixture.targetA
      retainedAddress = peerAddress "tcp://127.0.0.1:4303"
      (withSource, sourceBinding) = bindMember 21 fixture.source mempty fixture.initial
      (withContact, contactEffects) =
        stepBody
          22
          (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds [contact target (Set.singleton retainedAddress)])))
          withSource
      (withTarget, targetBinding) = bindMember 23 target Set.empty withContact
      growthAddress = peerAddress "tcp://127.0.0.1:4403"
      (afterGrowth, growthEffects) =
        stepBody
          24
          (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds [contact target (Set.singleton growthAddress)])))
          withTarget
  firstIntent <- soleDial "first unbound contact" contactEffects
  assertBool
    "contact growth is still re-gossiped"
    (any isKnownHeraldGossip (effectBatchMembers growthEffects))
  assertEqual
    "contact growth is not echoed to the source binding"
    [targetBinding]
    [ current
    | SendPeerControl current (PeerKnownHeralds _) <-
        effectBatchMembers growthEffects
    ]
  assertEqual "a live exact binding suppresses dialing" [] (dialEffects growthEffects)

  let (afterLoss, lossEffects) =
        stepBody 25 (RuntimeObserved (PeerBindingLost targetBinding)) afterGrowth
  intent <- soleDial "current binding loss" lossEffects
  assertIntent
    "current binding loss retains the complete address union"
    target
    (Set.fromList [retainedAddress, growthAddress])
    intent
  assertEqual
    "loss advances the pure permission generation"
    (peerDialIntentGenerationWord64 firstIntent + 1)
    (peerDialIntentGenerationWord64 intent)
  let (_, staleEffects) =
        stepBody 26 (RuntimeObserved (PeerBindingLost targetBinding)) afterLoss
  assertEqual "repeated stale loss is silent" [] (effectBatchMembers staleEffects)

caseDialClass :: Assertion
caseDialClass = do
  let fixture = dialFixture
      (bound, sourceBinding) = bindMember 27 fixture.source mempty fixture.initial
      highAddress = Set.singleton (peerAddress "tcp://127.0.0.1:4703")
      lowAddress = Set.singleton (peerAddress "tcp://127.0.0.1:4704")
      (_, effects) =
        stepBody
          28
          ( PeerInput
              ( PeerControlReceived
                  sourceBinding
                  ( PeerKnownHeralds
                      [ contact fixture.targetA highAddress,
                        contact fixture.targetLow lowAddress
                      ]
                  )
              )
          )
          bound
      intents = dialEffects effects
      forTarget remote =
        [ intent
        | intent <- intents,
          peerDialIntentHeraldEpoch intent == heraldMemberEpoch remote
        ]
  immediate <- case forTarget fixture.targetA of
    [intent] -> pure intent
    observed -> assertFailure ("expected one high-target intent, got " <> show observed)
  fallback <- case forTarget fixture.targetLow of
    [intent] -> pure intent
    observed -> assertFailure ("expected one low-target intent, got " <> show observed)
  assertEqual "the smaller local identity is the immediate initiator" ImmediatePeerDial (peerDialIntentClass immediate)
  assertEqual "the larger local identity retains the delayed fallback" FallbackPeerDial (peerDialIntentClass fallback)

caseDrainingAndStaleLoss :: Assertion
caseDrainingAndStaleLoss = do
  let fixture = dialFixture
      target = fixture.targetA
      (withSource, sourceBinding) = bindMember 31 fixture.source mempty fixture.initial
      (withContact, _) =
        stepBody
          32
          (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds [contact target (Set.singleton (peerAddress "tcp://127.0.0.1:4503"))])))
          withSource
      (withTarget, targetBinding) = bindMember 33 target Set.empty withContact
      (draining, _) =
        stepBody
          34
          ( AdministrationInput
              ( OrderlyHeraldShutdown
                  (checkedInitialAdministrationBinding fixture.genesis)
                  (adminCorrelationId 1)
              )
          )
          withTarget
      (_, drainingLossEffects) =
        stepBody 35 (RuntimeObserved (PeerBindingLost targetBinding)) draining
  assertEqual "draining does not re-arm direct dialing" [] (effectBatchMembers drainingLossEffects)

propCanonicalContactOrder :: [Word8] -> Property
propCanonicalContactOrder rawOrder =
  counterexample ("contact order: " <> show rawOrder)
    $ effectBatchMembers forwardEffects == effectBatchMembers reverseEffects
      && dials == sort dials
  where
    fixture = dialFixture
    (bound, sourceBinding) = bindMember 41 fixture.source mempty fixture.initial
    selected = fmap (selectTarget fixture . fromIntegral) (take 100 rawOrder)
    observations =
      zipWith
        (\remote ordinal -> contact remote (Set.singleton (generatedAddress remote ordinal)))
        selected
        [(0 :: Word64) ..]
    (_, forwardEffects) =
      stepBody 42 (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds observations))) bound
    (_, reverseEffects) =
      stepBody 42 (PeerInput (PeerControlReceived sourceBinding (PeerKnownHeralds (reverse observations)))) bound
    dials = dialEffects forwardEffects

data DialFixture = DialFixture
  { genesis :: CheckedHeraldGenesis,
    bootstraps :: CheckedInitialBootstraps,
    initial :: HeraldState,
    local :: HeraldMember,
    source :: HeraldMember,
    targetA :: HeraldMember,
    targetB :: HeraldMember,
    targetC :: HeraldMember,
    targetLow :: HeraldMember,
    inactiveId :: HeraldId,
    inactiveEpoch :: HeraldEpoch
  }

dialFixture :: DialFixture
dialFixture =
  DialFixture
    { genesis,
      bootstraps,
      initial,
      local = fixtureLocalMember,
      source = fixtureRemoteMember,
      targetA,
      targetB,
      targetC,
      targetLow,
      inactiveId = heraldId 240,
      inactiveEpoch = heraldEpoch 241
    }
  where
    targetA = heraldMember 210 211
    targetB = heraldMember 212 213
    targetC = heraldMember 214 215
    targetLow = heraldMember 1 216
    targets = [targetA, targetB, targetC, targetLow]
    active = fixtureMembers <> targets
    base = fixtureDeploymentAt fixtureLocalMember
    oracle = (deploymentOracleGenesis base) {oracleGenesisActiveHeralds = active}
    deployment =
      base
        { deploymentActiveHeralds = active,
          deploymentOracleGenesis = oracle
        }
    genesis = checked "dial fixture genesis" (checkHeraldGenesis deployment)
    bootstraps =
      checked
        "dial fixture bootstraps"
        (checkInitialBootstraps genesis (PrimordialProcessManifest fixtureLocalBootstrapIds))
    initial = case initialHerald (monotonicInstant 10) genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration of
      Left problem -> error ("dial fixture initialization: " <> show problem)
      Right (state, _) -> state

bindMember ::
  Word64 ->
  HeraldMember ->
  Set.Set PeerAddress ->
  HeraldState ->
  (HeraldState, PeerBinding)
bindMember nonceValue remote addresses predecessor =
  case [binding | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects] of
    [binding] -> (successor, binding)
    bindings -> error ("expected one admitted binding, observed " <> show bindings)
  where
    fixture = dialFixture
    nonce = connectionNonce nonceValue
    candidate = peerCandidate (heraldMemberId remote) (heraldMemberEpoch remote) nonce
    hello =
      peerHello
        (checkedSystemId fixture.genesis)
        (heraldMemberId remote)
        (heraldMemberEpoch remote)
        nonce
        addresses
        (controlIndex 0)
        (checkedCatalogueDigest fixture.genesis)
        (checkedInitialProjectionDigest fixture.bootstraps)
        Nothing
    (successor, effects) =
      stepBody
        nonceValue
        ( PeerInput
            ( PeerHelloReceived
                candidate
                Set.empty
                hello
                (heraldMembershipGenerationId membership)
                (heraldMembershipGenerationActiveMemberSetDigest membership)
                Nothing
            )
        )
        predecessor
    membership =
      OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView (startupOracleProjectionState predecessor))

contact :: HeraldMember -> Set.Set PeerAddress -> KnownHerald
contact remote = knownHerald (heraldMemberId remote) (heraldMemberEpoch remote)

generatedAddress :: HeraldMember -> Word64 -> PeerAddress
generatedAddress remote ordinal =
  peerAddress
    ( Text.pack
        ( "tcp://127.0.0.1:"
            <> show (10000 + heraldTag remote * 100 + ordinal)
        )
    )

heraldTag :: HeraldMember -> Word64
heraldTag remote
  | peer == heraldMemberEpoch dialFixture.targetA = 1
  | peer == heraldMemberEpoch dialFixture.targetB = 2
  | otherwise = 3
  where
    peer = heraldMemberEpoch remote

stepBody :: Word64 -> HeraldInputBody -> HeraldState -> (HeraldState, EffectBatch)
stepBody observed body predecessor =
  case verifiedStepHerald (heraldInput (monotonicInstant observed) body) predecessor of
    Left problem -> error ("dial fixture step: " <> show problem)
    Right result -> result

dialEffects :: EffectBatch -> [PeerDialIntent]
dialEffects effects = [intent | DialPeer intent <- effectBatchMembers effects]

timerArmEffects :: EffectBatch -> [HeraldEffect]
timerArmEffects effects =
  [ effect
  | effect@ArmTimer {} <- effectBatchMembers effects
  ]

soleDial :: String -> EffectBatch -> IO PeerDialIntent
soleDial _ effects | [intent] <- dialEffects effects = pure intent
soleDial context effects =
  assertFailure (context <> " expected one dial, observed " <> show (dialEffects effects))

isKnownHeraldGossip :: HeraldEffect -> Bool
isKnownHeraldGossip (SendPeerControl _ (PeerKnownHeralds _)) = True
isKnownHeraldGossip _ = False

assertIntent ::
  String ->
  HeraldMember ->
  Set.Set PeerAddress ->
  PeerDialIntent ->
  Assertion
assertIntent context remote addresses intent = do
  assertEqual (context <> " HeraldId") (heraldMemberId remote) (peerDialIntentHeraldId intent)
  assertEqual (context <> " HeraldEpoch") (heraldMemberEpoch remote) (peerDialIntentHeraldEpoch intent)
  assertEqual (context <> " addresses") addresses (peerDialIntentAddresses intent)

selectTarget :: DialFixture -> Int -> HeraldMember
selectTarget fixture index = case index `mod` 3 of
  0 -> fixture.targetA
  1 -> fixture.targetB
  _ -> fixture.targetC

heraldMember :: Word8 -> Word8 -> HeraldMember
heraldMember identifier epoch = HeraldMember (heraldId identifier) (heraldEpoch epoch)

heraldId :: Word8 -> HeraldId
heraldId seed = checked "HeraldId" (mkHeraldId (fixtureIdentifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes seed))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
