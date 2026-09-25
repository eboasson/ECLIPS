module ReceiptRetirementProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch, controlIndex, controlIndexWord64)
import Eclips.Domain.ProcessStart (processStartProcessEpochId, processStartResidence)
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Identity
import Eclips.Oracle.Label (oracleCommandCanonicalBytes, oracleStateCanonicalBytes)
import Eclips.Oracle.Progress (oracleProgress)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import OracleFixtures
import Step15ReferenceProperties (retireLiveTarget, step15Genesis, targetH4)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- The reference ledger deliberately never deletes an accepted receipt. Only
-- its explicit lifetime contract decides whether callers may still observe it.
data Retained = Retained OracleClientRequestId OracleCommandDigest OracleReceiptResult Word64
  deriving stock (Eq, Show)
data Reference = Reference Word64 [Retained] (Map HeraldEpoch Word64)
  deriving stock (Eq, Show)
data Action = Submit Bool Word64 Bool | Retire Bool Word64 | Piggyback Bool Word64 Bool Word64
  deriving stock (Eq, Show)
newtype History = History [Action]
  deriving stock (Show)
instance Arbitrary History where
  arbitrary = do
    count <- chooseInt (1, 100)
    let piggyback = do
          sequenceNumber <- choose (1, 24)
          through <- choose (0, sequenceNumber - 1)
          Piggyback <$> arbitrary <*> pure sequenceNumber <*> arbitrary <*> pure through
    History <$> vectorOf count (frequency [(3, Submit <$> arbitrary <*> choose (0, 24) <*> arbitrary), (2, piggyback), (1, Retire <$> arbitrary <*> choose (0, 24))])
  shrink (History actions) = History <$> shrinkList (const []) actions

tests :: TestTree
tests =
  testGroup
    "Oracle receipt retirement"
    [ testProperty "a permanent unresolved ID does not retain later consumed receipts" propSparsePlateau,
      testProperty "current Oracle checkpoints resume the same next transition" propCheckpointResume,
      testProperty "compaction agrees at every prefix with an append-only lifetime model" propReference,
      testCase "retiring an accepted receipt preserves its process and fences exact and conflicting retries" caseAccepted,
      testCase "a lower unresolved request remains available below a later completed request" caseHole,
      testCase "reordered and replayed acknowledgements do not create receipts or control entries" caseReplay,
      testCase "committed home retirement prunes receipts and fences every old-home request" caseClosedHome,
      testCase "fixed live state and outstanding window keep receipt bytes constant" casePlateau,
      testCase "maintenance envelopes and entries round trip through the current canonical codec" caseCanonical,
      testCase "maintenance rejects mismatched home or sequence without retaining a rejection" caseMalformed,
      testCase "piggyback preserves retry identity and uses one fresh-command control entry" casePiggyback
    ]

homeFor :: Bool -> HeraldEpoch
homeFor False = fixtureHeraldEpoch
homeFor True = fixtureThirdHeraldEpoch

-- Generated commands change the freshness precondition to distinguish payloads.
-- Both variants reject without changing live state.
envelopeForAction :: Action -> OracleEnvelope
envelopeForAction (Retire homeChoice through) = retirement (homeFor homeChoice) through
envelopeForAction (Piggyback homeChoice sequenceNumber alternate through) =
  oracleEnvelopeWithReceiptRetirement through (envelopeForAction (Submit homeChoice sequenceNumber alternate))
envelopeForAction (Submit homeChoice sequenceNumber alternate) =
  oracleEnvelope (oracleClientRequestId home sequenceNumber) (if alternate then Just (controlIndex 1000000) else Nothing) home (startProcessEpochCommand fixtureRemoteStart)
  where
    home = homeFor homeChoice

retirement :: HeraldEpoch -> Word64 -> OracleEnvelope
retirement home through = oracleEnvelope (oracleClientRequestId home through) Nothing home (retireOracleReceiptsCommand through)

transition :: OracleEnvelope -> OracleState -> (OracleState, OracleStepOutcome, OracleEffectBatch)
transition envelope = checked "receipt retirement transition" . stepOracle envelope

propReference :: History -> Property
propReference (History actions) =
  counterexample (show actions) $ case foldM check (fixtureInitialState, Reference 0 [] Map.empty) actions of
    Left problem -> counterexample problem False
    Right _ -> property True
  where
    check (state, reference) action = do
      let envelope = envelopeForAction action
          (next, outcome, effects) = transition envelope state
          (expected, wanted, changed) = referenceStep action envelope reference
      require "outcome agrees with append-only reference" (matches wanted outcome)
      require "control coordinate agrees" (controlIndexWord64 (oracleGreatestControlIndex next) == referenceIndex expected)
      require "only live receipts are retained" (oracleRequestCount next == length (activeRecords expected))
      require "frontiers agree" (oracleReceiptRetirementFrontiers next == Map.toAscList (referenceFrontiers expected))
      require "immutable result and query classification agree for every known sequence" (all (queryMatches next expected) [(h, n) | h <- [False, True], n <- [0 .. 24]])
      require "one entry for every fresh or advancing operation" (length (oracleEffects effects) == if changed then 1 else 0)
      require "inert transition preserves complete state" (changed || state == next)
      require "every emitted entry is canonically coherent" (all (entryRoundTrips next) (oracleEffects effects))
      require "diagnostic byte sections exactly partition the canonical transcript" (sum (fmap snd (oracleStateDiagnosticSections next)) == ByteString.length (oracleStateCanonicalBytes next))
      require "diagnostic cardinalities observe the retained request table" (lookup "requests" (oracleStateDiagnosticCardinalities next) == Just (length (activeRecords expected)))
      pure (next, expected)

referenceIndex :: Reference -> Word64
referenceIndex (Reference index _ _) = index
referenceFrontiers :: Reference -> Map HeraldEpoch Word64
referenceFrontiers (Reference _ _ frontiers) = frontiers
activeRecords :: Reference -> [Retained]
activeRecords (Reference _ history frontiers) = filter active history
  where
    active (Retained request _ _ _) = maybe True (oracleClientRequestSequence request >) (Map.lookup (oracleClientRequestHome request) frontiers)

data Expected = Fresh Retained | Duplicate Retained | Conflict OracleClientRequestId OracleCommandDigest OracleCommandDigest | Expired Word64 | Acknowledged HeraldEpoch Word64
  deriving stock (Eq, Show)
referenceStep :: Action -> OracleEnvelope -> Reference -> (Reference, Expected, Bool)
referenceStep action envelope reference@(Reference index history frontiers) = case action of
  Piggyback homeChoice sequenceNumber alternate through ->
    let (Reference semanticIndex semanticHistory semanticFrontiers, wanted, semanticChanged) = referenceStep (Submit homeChoice sequenceNumber alternate) envelope reference
        advances = maybe True (through >) (Map.lookup home frontiers)
     in case wanted of
          Expired _ -> (reference, wanted, False)
          _ ->
            ( Reference
                (if advances && not semanticChanged then semanticIndex + 1 else semanticIndex)
                semanticHistory
                (Map.insertWith max home through semanticFrontiers),
              wanted,
              semanticChanged || advances
            )
  Retire _ through -> case Map.lookup home frontiers of
    Just previous | through <= previous -> (reference, Acknowledged home previous, False)
    _ -> (Reference (index + 1) history (Map.insert home through frontiers), Acknowledged home through, True)
  Submit _ _ alternate
    | Just through <- Map.lookup home frontiers, oracleClientRequestSequence request <= through -> (reference, Expired through, False)
    | Just retained@(Retained _ previous _ _) <- find (\(Retained ident _ _ _) -> ident == request) history ->
        (reference, if previous == digest then Duplicate retained else Conflict request previous digest, False)
    | otherwise ->
        let result =
              if alternate
                then OracleRejected (StaleExpectedControlIndex (controlIndex 1000000) (controlIndex index))
                else OracleRejected (StartProcessResidenceMismatch (processStartProcessEpochId fixtureRemoteStart) home (processStartResidence fixtureRemoteStart))
            retained = Retained request digest result (index + 1)
         in (Reference (index + 1) (history <> [retained]) frontiers, Fresh retained, True)
  where
    request = oracleEnvelopeRequestId envelope
    home = oracleEnvelopeHomeHeraldEpoch envelope
    digest = oracleEnvelopeDigest envelope

matches :: Expected -> OracleStepOutcome -> Bool
matches (Fresh record) (OracleCommitted receipt) = receiptMatches record receipt
matches (Duplicate record) (OracleDuplicate receipt) = receiptMatches record receipt
matches (Conflict request previous supplied) outcome = outcome == OracleProtocolRejected (ConflictingOracleRequestId request previous supplied)
matches (Expired through) outcome = outcome == OracleRequestRetired (OracleRequestPrefixRetired through)
matches (Acknowledged home through) outcome = outcome == OracleProgressRetired home (oracleProgress (Lifetime.receiptRetirementPrefix (Just through)) (controlIndex 0))
matches _ _ = False
receiptMatches :: Retained -> OracleReceipt -> Bool
receiptMatches (Retained request digest result index) receipt =
  oracleReceiptRequestId receipt == request && oracleReceiptCommandDigest receipt == digest && oracleReceiptResult receipt == result && oracleReceiptControlIndex receipt == controlIndex index
queryMatches :: OracleState -> Reference -> (Bool, Word64) -> Bool
queryMatches state reference (homeChoice, sequenceNumber) =
  oracleRequestRetirement request state == retired && case (retired, record, oracleRequestReceipt request state) of
    (Just _, _, Nothing) -> True
    (Nothing, Nothing, Nothing) -> True
    (Nothing, Just retained, Just receipt) -> receiptMatches retained receipt
    _ -> False
  where
    request = oracleClientRequestId (homeFor homeChoice) sequenceNumber
    retired = case Map.lookup (homeFor homeChoice) (referenceFrontiers reference) of
      Just through | sequenceNumber <= through -> Just (OracleRequestPrefixRetired through)
      _ -> Nothing
    record = find (\(Retained ident _ _ _) -> ident == request) (activeRecords reference)
require :: String -> Bool -> Either String ()
require _ True = Right ()
require problem False = Left problem
entryRoundTrips :: OracleState -> OracleEffect -> Bool
entryRoundTrips state (EmitAppliedOracleEntry entry) =
  appliedEntryPostStateDigest entry == oracleStateDigest state && (canonicalAppliedOracleEntryValue <$> decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry))) == Right entry

caseAccepted :: Assertion
caseAccepted = do
  let envelope = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 0) Nothing fixtureHeraldEpoch (startProcessEpochCommand fixtureStart)
      (accepted, outcome, _) = transition envelope fixtureInitialState
      (retired, _, _) = transition (retirement fixtureHeraldEpoch 0) accepted
      conflict = oracleEnvelope (oracleEnvelopeRequestId envelope) Nothing fixtureHeraldEpoch (startProcessEpochCommand fixtureAlternateStart)
  case outcome of { OracleCommitted receipt -> oracleReceiptResult receipt @?= OracleAccepted; other -> assertFailure (show other) }
  oracleProcessRecords retired @?= oracleProcessRecords accepted
  oracleRequestCount retired @?= 0
  forM_ [envelope, conflict] $ \retry -> do
    let (unchanged, result, effects) = transition retry retired
    unchanged @?= retired
    result @?= OracleRequestRetired (OracleRequestPrefixRetired 0)
    oracleEffects effects @?= []

caseHole :: Assertion
caseHole = do
  let high = envelopeForAction (Submit False 2 False)
      low = envelopeForAction (Submit False 1 False)
      (afterHigh, _, _) = transition high fixtureInitialState
      (afterSafePrefix, _, _) = transition (retirement fixtureHeraldEpoch 0) afterHigh
      (afterLow, outcome, _) = transition low afterSafePrefix
  oracleRequestRetirement (oracleEnvelopeRequestId low) afterSafePrefix @?= Nothing
  case outcome of { OracleCommitted _ -> pure (); other -> assertFailure (show other) }
  oracleRequestCount afterLow @?= 2
  let (drained, _, _) = transition (retirement fixtureHeraldEpoch 2) afterLow
  oracleRequestCount drained @?= 0

caseReplay :: Assertion
caseReplay = do
  let schedule = [Submit False 0 False, Submit False 1 True, Retire False 1, Retire False 0, Retire False 1, Submit False 0 True]
      run decoder = foldl (\prior action -> let (next, _, _) = transition (decoder (envelopeForAction action)) prior in next) fixtureInitialState
      state = run id schedule
      decode = canonicalOracleEnvelopeValue . checked "replayed canonical envelope" . decodeCanonicalOracleEnvelope . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope
  state @?= run decode schedule
  oracleGreatestControlIndex state @?= controlIndex 3
  oracleRequestCount state @?= 0
  forM_ [0, 1] $ \through -> do
    let (next, result, effects) = transition (retirement fixtureHeraldEpoch through) state
    next @?= state
    result @?= OracleProgressRetired fixtureHeraldEpoch (oracleProgress (Lifetime.receiptRetirementPrefix (Just 1)) (controlIndex 0))
    oracleEffects effects @?= []

caseClosedHome :: Assertion
caseClosedHome = do
  let initial = checked "retirement genesis" (initialOracle step15Genesis)
      command n = oracleEnvelope (oracleClientRequestId targetH4 n) Nothing targetH4 (startProcessEpochCommand fixtureStart)
      (one, _, _) = transition (command 0) initial
      (acknowledged, _, _) = transition (retirement targetH4 0) one
      (two, _, _) = transition (command 1) acknowledged
      retired = retireLiveTarget targetH4 two
  oracleReceiptRetirementFrontier targetH4 retired @?= Nothing
  forM_ [0, 1, 100000] $ \sequenceNumber -> do
    let envelope = command sequenceNumber
        (next, result, effects) = transition envelope retired
    oracleRequestRetirement (oracleEnvelopeRequestId envelope) retired @?= Just OracleRequestHomeRetired
    oracleRequestReceipt (oracleEnvelopeRequestId envelope) retired @?= Nothing
    next @?= retired
    result @?= OracleRequestRetired OracleRequestHomeRetired
    oracleEffects effects @?= []
  let (next, result, effects) = transition (retirement targetH4 100000) retired
  next @?= retired
  result @?= OracleRequestRetired OracleRequestHomeRetired
  oracleEffects effects @?= []

casePlateau :: Assertion
casePlateau = do
  let advance state sequenceNumber =
        let (withReceipt, _, _) = transition (envelopeForAction (Submit False sequenceNumber False)) state
            (retired, _, _) = transition (retirement fixtureHeraldEpoch sequenceNumber) withReceipt
         in retired
      states = drop 1 (scanl advance fixtureInitialState [0 .. 200])
      sizes = fmap (ByteString.length . oracleStateCanonicalBytes) states
  case sizes of
    firstSize : rest -> assertBool "constant canonical retained state across rejected-request history" (all (== firstSize) rest)
    [] -> assertFailure "empty plateau experiment"
  assertBool "no receipts retained after each acknowledgement" (all ((== 0) . oracleRequestCount) states)
  assertBool "one frontier per issuing home" (all ((== 1) . length . oracleReceiptRetirementFrontiers) states)

caseCanonical :: Assertion
caseCanonical = do
  let envelope = retirement fixtureHeraldEpoch 123
      (next, _, effects) = transition envelope fixtureInitialState
  -- The 25-byte progress body includes the receipt prefix and its empty holes
  -- plus the eight-byte label frontier, zero for this receipt-only facade.
  oracleCommandCanonicalBytes (retireOracleReceiptsCommand 123) @?= ByteString.pack ([21] <> replicate 7 0 <> [25, 1] <> replicate 7 0 <> [123] <> replicate 16 0)
  (canonicalOracleEnvelopeValue <$> decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope envelope))) @?= Right envelope
  let sparse = checked "sparse canonical progress" (Lifetime.receiptRetirement (Just 123) (Set.fromList [1, 17]))
      maintenance = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 123) Nothing fixtureHeraldEpoch (retireOracleReceiptProgressCommand sparse)
      exceptionalCarrier = oracleEnvelopeWithReceiptRetirementProgress sparse (envelopeForAction (Submit False 1 False))
  forM_ [maintenance, exceptionalCarrier] $ \candidate ->
    (canonicalOracleEnvelopeValue <$> decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope candidate))) @?= Right candidate
  forM_ [mempty, Lifetime.receiptRetirementPrefix (Just 0), checked "zero remains unresolved" (Lifetime.receiptRetirement (Just 0) (Set.singleton 0))] $ \progress -> do
    let candidate = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 0) Nothing fixtureHeraldEpoch (retireOracleReceiptProgressCommand progress)
        (restored, _, _) = transition candidate fixtureInitialState
    (canonicalOracleEnvelopeValue <$> decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope candidate))) @?= Right candidate
    decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes restored) @?= Right restored
  forM_ (oracleEffects effects) $ \effect@(EmitAppliedOracleEntry entry) -> do
    assertBool "maintenance entry round trip" (entryRoundTrips next effect)
    appliedEntryReceiptRetirement entry @?= Just (fixtureHeraldEpoch, 123)
    appliedEntryCommand entry @?= Nothing
    appliedEntryConfiguration entry @?= Nothing
    appliedEntryProjectionEvents entry @?= []

caseMalformed :: Assertion
caseMalformed = forM_ [(fixtureHeraldEpoch, 0), (fixtureRemoteHeraldEpoch, 1)] $ \(requestHome, sequenceNumber) -> do
  let request = oracleClientRequestId requestHome sequenceNumber
      envelope = oracleEnvelope request Nothing fixtureHeraldEpoch (retireOracleReceiptsCommand 1)
      (next, result, effects) = transition envelope fixtureInitialState
  next @?= fixtureInitialState
  result @?= OracleProtocolRejected (InvalidOracleProgress request)
  oracleEffects effects @?= []

casePiggyback :: Assertion
casePiggyback = do
  let original n = envelopeForAction (Submit False n False)
      decorate through = oracleEnvelopeWithReceiptRetirement through
      advance prior n = let (next, _, _) = transition (original n) prior in next
      prepared = foldl advance fixtureInitialState [1, 2, 3]
      duplicate = decorate 1 (original 3)
      (afterDuplicate, duplicateOutcome, duplicateEffects) = transition duplicate prepared
      conflict = decorate 2 (envelopeForAction (Submit False 3 True))
      (afterConflict, conflictOutcome, conflictEffects) = transition conflict afterDuplicate
      fresh = decorate 3 (original 4)
      (afterFresh, freshOutcome, freshEffects) = transition fresh afterConflict
  oracleEnvelopeDigest duplicate @?= oracleEnvelopeDigest (original 3)
  assertBool "piggyback is represented in the wire transcript" (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope duplicate) /= canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope (original 3)))
  case duplicateOutcome of { OracleDuplicate _ -> pure (); other -> assertFailure (show other) }
  case conflictOutcome of { OracleProtocolRejected (ConflictingOracleRequestId {}) -> pure (); other -> assertFailure (show other) }
  case freshOutcome of { OracleCommitted _ -> pure (); other -> assertFailure (show other) }
  oracleGreatestControlIndex afterDuplicate @?= controlIndex 4
  oracleGreatestControlIndex afterConflict @?= controlIndex 5
  oracleGreatestControlIndex afterFresh @?= controlIndex 6
  oracleRequestCount afterFresh @?= 1
  oracleReceiptRetirementFrontier fixtureHeraldEpoch afterFresh @?= Just 3
  forM_ [(afterDuplicate, duplicateEffects, False, 1), (afterConflict, conflictEffects, False, 2), (afterFresh, freshEffects, True, 3)] $ \(state, effects, ordinary, through) -> case oracleEffects effects of
    [effect@(EmitAppliedOracleEntry entry)] -> do
      assertBool "piggyback effect round trip" (entryRoundTrips state effect)
      appliedEntryReceiptRetirement entry @?= Just (fixtureHeraldEpoch, through)
      (case appliedEntryCommand entry of Just _ -> True; Nothing -> False) @?= ordinary
    other -> assertFailure (show other)
  forM_ [original 4, decorate 1 (original 4), fresh] $ \retry -> do
    let (next, outcome, effects) = transition retry afterFresh
    next @?= afterFresh
    case (freshOutcome, outcome) of
      (OracleCommitted receipt, OracleDuplicate same) -> same @?= receipt
      other -> assertFailure (show other)
    oracleEffects effects @?= []
  forM_ [duplicate, conflict, fresh] $ \envelope ->
    (canonicalOracleEnvelopeValue <$> decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope envelope))) @?= Right envelope
  let invalid = decorate 4 (original 4)
      (unchanged, invalidOutcome, invalidEffects) = transition invalid afterFresh
  unchanged @?= afterFresh
  invalidOutcome @?= OracleProtocolRejected (InvalidOracleProgress (oracleEnvelopeRequestId invalid))
  oracleEffects invalidEffects @?= []
  assertBool "canonical admission rejects a prefix that covers its own request" (case decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope invalid)) of Left _ -> True; Right _ -> False)

propSparsePlateau :: Positive Int -> Property
propSparsePlateau (Positive rawCount) = conjoin (snd (foldl step (fixtureInitialState, []) [2 .. count + 1]))
  where
    count = fromIntegral (1 + rawCount `mod` 80)
    home = fixtureHeraldEpoch
    pending = oracleClientRequestId home 1
    step (state, checks) sequenceNumber =
      let command = envelopeForAction (Submit False sequenceNumber False)
          (completed, _, _) = transition command state
          progress = checked "sparse progress" (Lifetime.receiptRetirement (Just sequenceNumber) (Set.singleton 1))
          maintenance = oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home (retireOracleReceiptProgressCommand progress)
          (retired, _, _) = transition maintenance completed
          (replayed, outcome, _) = transition command retired
          stale = checked "stale snapshot" (Lifetime.receiptRetirement (Just sequenceNumber) (Set.fromList [1, sequenceNumber]))
          (stillRetired, _, _) = transition (oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home (retireOracleReceiptProgressCommand stale)) retired
       in ( retired,
            checks
              <> [ counterexample
                     (show sequenceNumber)
                     ( conjoin
                         [ oracleRequestCount retired === 0,
                           oracleRequestRetirement pending retired === Nothing,
                           oracleReceiptRetirementProgress home retired === Just progress,
                           oracleStateDigest replayed === oracleStateDigest retired,
                           outcome === OracleRequestRetired (OracleRequestPrefixRetired sequenceNumber),
                           oracleStateDigest stillRetired === oracleStateDigest retired,
                           decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes retired) === Right retired
                         ]
                     )
                 ]
          )

propCheckpointResume :: History -> Property
propCheckpointResume (History actions) =
  conjoin
    [ conjoin (snd (foldl step (checked "digest mode genesis" (initialOracleWithStateDigestMode mode fixtureCheckedGenesis), []) actions))
    | mode <- [OracleStateDigestDisabled, OracleStateDigestEnabled]
    ]
  where
    step (state, checks) action =
      let restored = decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes state)
          envelope = envelopeForAction action
          result@(successor, _, _) = transition envelope state
          resumed = fmap (transition envelope) restored
       in (successor, checks <> [counterexample (show action) (conjoin [restored === Right state, resumed === Right result])])
