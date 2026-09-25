{-# LANGUAGE OverloadedStrings #-}

module JoinControlTailProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Identity
import Eclips.Domain.Membership (HeraldAdmissionId)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Startup (heraldMemberEpoch)
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Topology
import Eclips.Herald.Discovery qualified as PeerDiscovery
import Eclips.Herald.EffectBatch (HeraldEffect (SendPeerCandidate, SetPeerCandidateDisposition), effectBatchMembers)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedSystemId)
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Input (HeraldInputBody (OracleInput, PeerInput), PeerIngress (PeerCandidateOpened, PeerHelloReceived), heraldInput)
import Eclips.Herald.Join.State qualified as Owner
import Eclips.Herald.OracleClient (OracleClientIngress (OracleEntriesReceived))
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    freezeStartupSemanticControl,
    startupControlOracleProjectionState,
    startupGenesis,
    startupJoinState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.OracleAdvance qualified as Advance
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalAppliedOracleEntryValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command
import Eclips.Oracle.Effect (OracleEffect (EmitAppliedOracleEntry), OracleStepOutcome (OracleCommitted), oracleEffects)
import Eclips.Oracle.Genesis (checkedOracleSystemId)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import Eclips.Oracle.Receipt (OracleReceiptResult (OracleAccepted), oracleReceiptResult)
import Eclips.Oracle.State
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures qualified as Fixtures
import JoinHistoryProperties qualified as JoinFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "join admission control tails"
    [ testProperty "overlapping admission lifetimes preserve the exact minimum through generated updates" propOverlappingLifetimes,
      testProperty "duplicate Begin preserves an advanced floor and backward imports are rejected" propMonotoneFloor,
      testProperty "joining replacement preserves receiver obligations without importing donor pins" propReplacement,
      testCase "detached applicant retirement releases its outstanding admission tail" (caseDetachedRetirement False),
      testCase "detached self-retirement releases every outstanding donor tail" (caseDetachedRetirement True),
      testCase "detached retirement rejects an unrelated surviving canonical tail" caseDetachedRetirementRequiresCanonicalTail,
      testCase "frozen cancellation releases old promises without registering later Begin or duplicate callbacks" caseFrozenCancellation,
      testCase "frozen canonical applicant retirement releases its outstanding admission tail" caseFrozenRetirement,
      testCase "ordinary completed batches reclaim unpinned bytes while preserving an outstanding admission tail" caseAutomaticBatchReclamation,
      testCase "accepted activation Hello reclaims its donor tail at the same cursor and duplicates preserve coverage" caseAutomaticHelloReclamation
    ]

type Model = [(HeraldAdmissionRecord, ControlIndex)]

propOverlappingLifetimes :: Word8 -> [(Word8, Word8, Word8)] -> Property
propOverlappingLifetimes seed supplied = go initialModel initial (take 80 supplied)
  where
    records = take (2 + fromIntegral seed `mod` 10) admissionRecords
    initialModel = [(record, admissionRecordBeginIndex record) | record <- records]
    initial = foldl (flip retain) Owner.emptyState records
    go model state [] =
      let released = foldl (flip release) state records
          replayed = foldl (flip release) released (reverse records)
       in conjoin
            [ agrees records model state,
              counterexample "completed obligations leave no history ledger" (released === Owner.emptyState),
              replayed === released
            ]
    go model state ((selected, operation, offset) : rest) =
      let record = records !! (fromIntegral selected `mod` length records)
          identifier = admissionRecordId record
          epoch = applicant record
          present = lookup record model
          following = records !! ((fromIntegral selected + 1) `mod` length records)
          (nextModel, next, outcome) = case operation `mod` 4 of
            0 -> case present of
              -- The coordinator registers a canonical Begin once, rather than
              -- reopening a discharged lifetime from an old projection record.
              Nothing -> (model, state, property True)
              Just _ -> (model, retain record state, property True)
            1 -> case present of
              Nothing -> (model, state, Owner.advanceAdmissionControlTailFloor identifier epoch (admissionRecordBeginIndex record) state === Left (Owner.AdmissionControlTailMissing epoch))
              Just before ->
                let after = controlIndex (controlIndexWord64 before + fromIntegral offset)
                    advanced = checked (Owner.advanceAdmissionControlTailFloor identifier epoch after state)
                 in ([(known, if known == record then after else floorIndex) | (known, floorIndex) <- model], advanced, property True)
            2 -> (filter ((/= record) . fst) model, release record state, property True)
            _ -> (model, Owner.releaseAdmissionControlTail (admissionRecordId following) epoch state, property True)
       in conjoin [agrees records model state, outcome, go nextModel next rest]

propMonotoneFloor :: Word8 -> Word8 -> Property
propMonotoneFloor selected offset =
  let record = admissionRecords !! (fromIntegral selected `mod` length admissionRecords)
      begin = admissionRecordBeginIndex record
      after = controlIndex (controlIndexWord64 begin + 1 + fromIntegral offset)
      before = controlIndex (controlIndexWord64 after - 1)
      epoch = applicant record
      identifier = admissionRecordId record
      initial = retain record Owner.emptyState
      advanced = checked (Owner.advanceAdmissionControlTailFloor identifier epoch after initial)
      duplicate = retain record advanced
   in conjoin
        [ agrees [record] [(record, after)] advanced,
          duplicate === advanced,
          Owner.advanceAdmissionControlTailFloor identifier epoch after duplicate === Right duplicate,
          Owner.advanceAdmissionControlTailFloor identifier epoch before duplicate === Left (Owner.AdmissionControlTailFloorRegressed epoch after before),
          release record duplicate === Owner.emptyState
        ]

propReplacement :: Word8 -> Word8 -> Property
propReplacement seed offset =
  let count = 1 + fromIntegral seed `mod` 8
      receiverRecords = take count admissionRecords
      donorRecords = take count (drop 8 admissionRecords)
      receiver = Owner.retainCapture (admissionRecordId (firstRecord receiverRecords)) 1 "receiver capture" (foldl (flip retain) Owner.emptyState receiverRecords)
      donor0 = foldl (flip retain) Owner.emptyState donorRecords
      first = firstRecord donorRecords
      after = controlIndex (controlIndexWord64 (admissionRecordBeginIndex first) + fromIntegral offset)
      donor =
        Owner.retainInstalledHistory
          (admissionRecordId first)
          1
          (applicant first)
          "donor material"
          (checked (Owner.advanceAdmissionControlTailFloor (admissionRecordId first) (applicant first) after donor0))
      adopted = Owner.replaceJoiningMaterial donor receiver
      bundlesReleased = Owner.releaseTransferHistories (\_ _ -> True) adopted
   in conjoin
        [ Owner.admissionControlTails adopted === Owner.admissionControlTails receiver,
          Owner.admissionControlTailFloor adopted === Owner.admissionControlTailFloor receiver,
          conjoin [Owner.lookupAdmissionControlTail (applicant record) adopted === Nothing | record <- donorRecords],
          Owner.capturedHistories adopted === Owner.capturedHistories receiver,
          Owner.installedHistories adopted === Owner.installedHistories donor,
          Owner.admissionControlTails bundlesReleased === Owner.admissionControlTails receiver,
          Owner.replaceJoiningMaterial donor adopted === adopted
        ]

-- The legacy detached retirement seam has no canonical row to extend a
-- surviving tail. Exercise cases which discharge all promised bytes: the
-- target applicant's one tail, and every tail of a locally retired donor.
caseDetachedRetirement :: Bool -> Assertion
caseDetachedRetirement retireLocal = do
  let (oracle0, first, source, _) = JoinFixture.activatedFixture
      (oracle1, admitted) =
        if retireLocal
          then let (next, entry) = beginNextAdmission 0xf1 oracle0 source in (next, JoinFixture.deliverEntry entry source)
          else (oracle0, source)
      local = checkedLocalHeraldEpoch (startupGenesis admitted)
      target = if retireLocal then local else applicant first
      (probe, _, opened) = openProbe target oracle1
      predecessor = JoinFixture.deliverEntry opened admitted
      index = controlIndex (controlIndexWord64 (appliedEntryControlIndex (canonicalAppliedOracleEntryValue opened)) + 1)
      resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      generation = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView (startupOracleProjectionState predecessor))
      successorMembership = checked (Membership.retireHeraldMembershipGeneration index resolution target generation)
      prepared = checked (MembershipAdvance.prepareMembershipAdvance index successorMembership [] predecessor)
      retired = MembershipAdvance.commitMembershipAdvance prepared
      duplicate = checked (MembershipAdvance.prepareMembershipAdvance index successorMembership [] retired)
  assertEqual "real Begin entries retain the intended number of donor promises" (if retireLocal then 2 else 1) (length (Owner.admissionControlTails (startupJoinState predecessor)))
  assertEqual "retirement starts from an independently valid whole owner" (Right ()) (validateHeraldState predecessor)
  assertEqual "retirement discharges the relevant local promises" [] (Owner.admissionControlTails (startupJoinState retired))
  assertEqual "no retention floor remains" Nothing (Owner.admissionControlTailFloor (startupJoinState retired))
  assertEqual "retirement preserves every whole-owner invariant" (Right ()) (validateHeraldState retired)
  assertBool "duplicate retirement cannot resurrect a promise" (MembershipAdvance.commitMembershipAdvance duplicate == retired)
  assertEqual "duplicate retirement emits no work" [] (effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects duplicate))

caseDetachedRetirementRequiresCanonicalTail :: Assertion
caseDetachedRetirementRequiresCanonicalTail = do
  let (oracle0, first, source, _) = JoinFixture.activatedFixture
      (oracle1, begun) = beginNextAdmission 0xf1 oracle0 source
      target = applicant first
      (probe, _, opened) = openProbe target oracle1
      predecessor = JoinFixture.deliverEntry opened (JoinFixture.deliverEntry begun source)
      index = controlIndex (controlIndexWord64 (appliedEntryControlIndex (canonicalAppliedOracleEntryValue opened)) + 1)
      resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      generation = Projection.oracleViewCurrentHeraldMembership (Projection.oracleView (startupOracleProjectionState predecessor))
      successorMembership = checked (Membership.retireHeraldMembershipGeneration index resolution target generation)
  assertEqual "both genuine admission promises precede retirement" 2 (length (Owner.admissionControlTails (startupJoinState predecessor)))
  assertEqual "the complete predecessor has every promised canonical byte" (Right ()) (validateHeraldState predecessor)
  -- A detached membership coordinate cannot stand in for the next canonical
  -- byte owed to the unrelated applicant. Live canonical retirement supplies
  -- that row and independently projects any cancellation of a pending Begin.
  assertEqual
    "the private reference seam requires canonical ingress when a tail survives"
    (Left (MembershipAdvance.MembershipAdmissionTailRequiresCanonicalEntry index))
    (fmap (const ()) (MembershipAdvance.prepareMembershipAdvance index successorMembership [] predecessor))

caseFrozenCancellation :: Assertion
caseFrozenCancellation = do
  let (oracle0, record, _, source, _) = JoinFixture.capturedFixture
      begin = JoinFixture.capturedBeginEntry
      (oracle1, cancellation) = JoinFixture.submit (CancelHeraldAdmission (admissionRecordId record)) oracle0
      (_, nextBegin) = beginNextAdmission 0xf2 oracle1 source
      frozen = freezeStartupSemanticControl source
      cancelled = applyFrozen cancellation frozen
      advanced = applyFrozen nextBegin cancelled
      repeated = foldl (flip applyFrozen) advanced [begin, cancellation, nextBegin]
  assertEqual "the admitted Begin retains its real source obligation" [applicant record] (map Owner.admissionControlTailApplicant (Owner.admissionControlTails (startupJoinState frozen)))
  assertEqual "fresh frozen cancellation releases the existing promise" [] (Owner.admissionControlTails (startupJoinState cancelled))
  assertEqual "a later fresh Begin on the frozen lane creates no new promise" [] (Owner.admissionControlTails (startupJoinState advanced))
  assertBool "old Begin and duplicate terminal callbacks are inert" (repeated == advanced)
  assertBool "control-only ingress leaves the semantic owner frozen" (startupOracleProjectionState advanced == startupOracleProjectionState source)
  assertEqual "frozen ingress does not advance semantic coverage" (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState source)) (Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState advanced))
  assertEqual "frozen ingress does not reclaim newly observed control bytes" (Just nextBegin) (Projection.appliedEntryEvidence (appliedEntryControlIndex (canonicalAppliedOracleEntryValue nextBegin)) (startupControlOracleProjectionState advanced))
  assertEqual "the live control owner received the new Begin" (appliedEntryControlIndex (canonicalAppliedOracleEntryValue nextBegin)) (Projection.oracleViewControlIndex (Projection.oracleView (startupControlOracleProjectionState advanced)))
  mapM_ (assertEqual "frozen retention lifecycle preserves all owners" (Right ()) . validateHeraldState) [frozen, cancelled, advanced, repeated]

caseFrozenRetirement :: Assertion
caseFrozenRetirement = do
  let (oracle0, record, source, _) = JoinFixture.activatedFixture
      target = applicant record
      (probe, openedOracle, opened) = openProbe target oracle0
      configuration = Voter.voterConfigurationId (Voter.oracleVoterConfiguration openedOracle)
      (reportedOracle, reported) = JoinFixture.submitCommand (reportHeraldFailureProbeCommand probe configuration ProbeUnreachable) openedOracle
      resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      (_, retirement) = JoinFixture.submitCommand (retireHeraldEpochCommand resolution target) reportedOracle
      frozen = freezeStartupSemanticControl source
      prepared = foldl (flip applyFrozen) frozen [opened, reported]
      retired = applyFrozen retirement prepared
      repeated = foldl (flip applyFrozen) retired [opened, reported, retirement]
  assertEqual "the genuine activated applicant still awaits its donor acknowledgement" [target] (map Owner.admissionControlTailApplicant (Owner.admissionControlTails (startupJoinState prepared)))
  assertEqual "fresh frozen retirement releases that applicant's promise" [] (Owner.admissionControlTails (startupJoinState retired))
  assertBool "frozen retirement leaves the semantic projection unchanged" (startupOracleProjectionState retired == startupOracleProjectionState source)
  assertBool "fresh control projection records the actual retirement" (not (Projection.oracleViewIsActiveHerald target (Projection.oracleView (startupControlOracleProjectionState retired))))
  assertBool "duplicate probe and retirement callbacks are inert" (repeated == retired)
  mapM_ (assertEqual "frozen canonical retirement preserves every owner" (Right ()) . validateHeraldState) [prepared, retired, repeated]

caseAutomaticBatchReclamation :: Assertion
caseAutomaticBatchReclamation = do
  let (oracle0, record, _, source, _) = JoinFixture.capturedFixture
      home = checkedLocalHeraldEpoch (startupGenesis source)
      append (previous, entries) _ =
        let identifier = oracleClientRequestId home (700000 + controlIndexWord64 (oracleGreatestControlIndex previous))
            command = Voter.cancelVoterChangeCommand (checked (Voter.mkVoterChangeId (controlIndex 999999)))
            (next, outcome, effects) = checked (stepOracle (oracleEnvelope identifier Nothing home command) previous)
            entry = case (outcome, [canonicalizeAppliedOracleEntry applied | EmitAppliedOracleEntry applied <- oracleEffects effects]) of
              (OracleCommitted receipt, [applied]) | oracleReceiptResult receipt /= OracleAccepted -> applied
              other -> error ("expected one unrelated canonical rejection: " <> show other)
         in (next, entries <> [entry])
      (oracle1, suffix) = foldl append (oracle0, []) [1 :: Int .. 3]
      batch = case suffix of first : rest -> first :| rest; [] -> error "automatic reclamation fixture needs a batch"
      retained = applyOrdinary batch source
      cursor = oracleGreatestControlIndex oracle1
      (_, cancellation) = JoinFixture.submit (CancelHeraldAdmission (admissionRecordId record)) oracle1
      released = applyOrdinary (cancellation :| []) retained
      repeated = applyOrdinary (cancellation :| []) released
      projection = startupOracleProjectionState retained
      releasedProjection = startupOracleProjectionState released
  assertEqual "ordinary batch automatically advances Projection coverage" cursor (Projection.projectionCanonicalCoveredThrough projection)
  assertEqual "ordinary batch advances paired Client coverage" cursor (Client.oracleClientCanonicalCoveredThrough (startupOracleClientState retained))
  assertEqual "an outstanding admission keeps its complete canonical suffix" (Just suffix) (Projection.retainedControlSuffixAfter (admissionRecordBeginIndex record) projection)
  assertEqual "coverage releases the unpinned Begin itself" Nothing (Projection.appliedEntryEvidence (admissionRecordBeginIndex record) projection)
  assertEqual "canonical cancellation releases the outstanding promise" [] (Owner.admissionControlTails (startupJoinState released))
  assertEqual "the cancelling batch immediately reclaims all unpinned bytes" [] (Projection.projectionWitnessAppliedEntries (Projection.stateWitness releasedProjection))
  assertEqual "reclamation covers the cancellation in the same batch" (appliedEntryControlIndex (canonicalAppliedOracleEntryValue cancellation)) (Projection.projectionCanonicalCoveredThrough releasedProjection)
  assertBool "an exact covered callback preserves reclaimed Projection" (startupOracleProjectionState repeated == releasedProjection)
  assertEqual "an exact covered callback preserves reclaimed Client" (startupOracleClientState released) (startupOracleClientState repeated)
  mapM_ (assertEqual "automatic batch reclamation preserves every owner" (Right ()) . validateHeraldState) [retained, released, repeated]

caseAutomaticHelloReclamation :: Assertion
caseAutomaticHelloReclamation = do
  let (oracle, record, source, observer) = JoinFixture.activatedFixture
      (_, offeredEffects) = checked (stepHerald (heraldInput (startupLastObservedTime observer) (PeerInput (PeerCandidateOpened (PeerDiscovery.peerCandidateOpened (PeerDiscovery.connectionNonce 8451) Set.empty Nothing)))) observer)
      (candidate, generation, members, hello) = case [(peer, advertised, active, message) | SendPeerCandidate peer advertised active message <- effectBatchMembers offeredEffects] of
        [offered] -> offered
        other -> error ("activated applicant did not offer one Hello: " <> show other)
      receive state = checked (stepHerald (heraldInput (startupLastObservedTime state) (PeerInput (PeerHelloReceived candidate Set.empty hello generation members Nothing))) state)
      (acknowledged, acknowledgedEffects) = receive source
      (repeated, repeatedEffects) = receive acknowledged
      cursor = oracleGreatestControlIndex oracle
      accepted effects = case [disposition | SetPeerCandidateDisposition _ disposition <- effectBatchMembers effects] of [PeerDiscovery.PeerHelloAccepted {}] -> True; _ -> False
      beforeProjection = startupOracleProjectionState source
      afterProjection = startupOracleProjectionState acknowledged
  assertEqual "the donor still owes the activated applicant its Begin tail" (Just (admissionRecordBeginIndex record)) (Owner.admissionControlTailFloor (startupJoinState source))
  assertBool "the donor's retained tail contains real canonical entries" (maybe False (not . null) (Projection.retainedControlSuffixAfter (admissionRecordBeginIndex record) beforeProjection))
  assertBool "the activation acknowledgement passes ordinary Hello admission" (accepted acknowledgedEffects)
  assertEqual "accepted Hello immediately discharges the donor promise" [] (Owner.admissionControlTails (startupJoinState acknowledged))
  assertEqual "accepted Hello advances no Oracle cursor" cursor (Client.oracleClientAppliedCursor (startupOracleClientState acknowledged))
  assertEqual "same-cursor release preserves semantic coverage" cursor (Projection.projectionCanonicalCoveredThrough afterProjection)
  assertEqual "accepted Hello immediately removes unpinned old bytes" [] (Projection.projectionWitnessAppliedEntries (Projection.stateWitness afterProjection))
  assertBool "the duplicate Hello is also admitted" (accepted repeatedEffects)
  assertBool "duplicate Hello preserves the reclaimed Projection owner" (startupOracleProjectionState repeated == afterProjection)
  assertEqual "duplicate Hello preserves the reclaimed Client owner" (startupOracleClientState acknowledged) (startupOracleClientState repeated)
  mapM_ (assertEqual "Hello-triggered reclamation preserves every owner" (Right ()) . validateHeraldState) [source, acknowledged, repeated]

applyOrdinary :: NonEmpty CanonicalAppliedOracleEntry -> HeraldState -> HeraldState
applyOrdinary entries state =
  let binding = fromJust (Client.oracleClientCurrentBinding (startupOracleClientState state))
   in fst (checked (stepHerald (heraldInput (startupLastObservedTime state) (OracleInput (OracleEntriesReceived binding entries))) state))

beginNextAdmission :: Word8 -> OracleState -> HeraldState -> (OracleState, CanonicalAppliedOracleEntry)
beginNextAdmission byte oracle source =
  let manifest = heraldAdmissionManifest (checkedSystemId (startupGenesis source)) (checked (mkHeraldId (Bytes.replicate 32 byte))) (checked (mkHeraldEpoch (Bytes.replicate 32 byte)))
      anchor = checked (Progress.structuralAdmissionAnchorClaim (Advance.structuralReconciliationViews source) (startupStructuralProgressState source))
   in JoinFixture.submit (BeginHeraldAdmission manifest anchor) oracle

openProbe :: HeraldEpoch -> OracleState -> (Membership.HeraldFailureProbeId, OracleState, CanonicalAppliedOracleEntry)
openProbe target oracle =
  let command = openHeraldFailureProbeCommand target (Membership.heraldMembershipGenerationId (oracleCurrentMembership oracle)) (Voter.voterConfigurationId (Voter.oracleVoterConfiguration oracle))
      (opened, entry) = JoinFixture.submitCommand command oracle
      probe = checked (Membership.deriveHeraldFailureProbeId (appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)))
   in (probe, opened, entry)

applyFrozen :: CanonicalAppliedOracleEntry -> HeraldState -> HeraldState
applyFrozen entry state =
  let binding = fromJust (Client.oracleClientCurrentBinding (startupOracleClientState state))
   in fst (checked (Advance.applyFencedOracleIngress (OracleEntriesReceived binding (entry :| [])) state))

agrees :: [HeraldAdmissionRecord] -> Model -> Owner.State -> Property
agrees records model state =
  conjoin
    [ map tailView (Owner.admissionControlTails state) === sortOn firstOf4 expected,
      Owner.admissionControlTailFloor state === case map snd model of [] -> Nothing; floors -> Just (minimum floors),
      conjoin
        [ fmap tailView (Owner.lookupAdmissionControlTail (applicant record) state)
            === fmap (recordView record) (lookup record model)
        | record <- records
        ]
    ]
  where
    expected = [recordView record after | (record, after) <- model]

tailView :: Owner.AdmissionControlTail -> (HeraldEpoch, HeraldAdmissionId, ControlIndex, ControlIndex)
tailView retained =
  ( Owner.admissionControlTailApplicant retained,
    Owner.admissionControlTailAdmission retained,
    Owner.admissionControlTailBegin retained,
    Owner.admissionControlTailExclusiveFloor retained
  )

recordView :: HeraldAdmissionRecord -> ControlIndex -> (HeraldEpoch, HeraldAdmissionId, ControlIndex, ControlIndex)
recordView record after = (applicant record, admissionRecordId record, admissionRecordBeginIndex record, after)

firstOf4 :: (a, b, c, d) -> a
firstOf4 (value, _, _, _) = value

firstRecord :: [HeraldAdmissionRecord] -> HeraldAdmissionRecord
firstRecord (record : _) = record
firstRecord [] = error "control-tail fixture requires a nonempty admission set"

retain :: HeraldAdmissionRecord -> Owner.State -> Owner.State
retain record = checked . Owner.retainAdmissionControlTail record

release :: HeraldAdmissionRecord -> Owner.State -> Owner.State
release record = Owner.releaseAdmissionControlTail (admissionRecordId record) (applicant record)

applicant :: HeraldAdmissionRecord -> HeraldEpoch
applicant = admissionManifestHeraldEpoch . admissionRecordManifest

-- Real canonical Begin records from one Oracle history. Cancellation lets the
-- Oracle admit the next identity; the owner tests independently control when
-- each local retention obligation is discharged.
admissionRecords :: [HeraldAdmissionRecord]
admissionRecords = gather (checked (initialOracle genesis)) [101 .. 116]
  where
    genesis = Fixtures.fixtureCheckedOracleGenesis
    gather _ [] = []
    gather previous (byte : rest) =
      let manifest = heraldAdmissionManifest (checkedOracleSystemId genesis) (checked (mkHeraldId (Bytes.replicate 32 byte))) (checked (mkHeraldEpoch (Bytes.replicate 32 byte)))
          cut =
            checked
              ( topologyCut
                  (sameGenerationPredecessor (checked (mkTopologyCutId (Bytes.replicate 32 201))))
                  (topologyFrontier (emptyStructuralVersionVector (oracleCurrentMembership previous)) (oracleGreatestControlIndex previous))
                  (checked (mkTopologyOccurrenceDigest (Bytes.replicate 32 202)))
              )
          begun = submit (BeginHeraldAdmission manifest cut) previous
          record = fromJust (oraclePendingHeraldAdmission begun)
          cancelled = submit (CancelHeraldAdmission (admissionRecordId record)) begun
       in record : gather cancelled rest

submit :: HeraldAdmissionCommand -> OracleState -> OracleState
submit command state =
  let home = heraldMemberEpoch Fixtures.fixtureLocalMember
      request = oracleClientRequestId home (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
   in case checked (stepOracle (oracleEnvelope request Nothing home (heraldAdmissionCommand command)) state) of
        (successor, OracleCommitted receipt, _) | oracleReceiptResult receipt == OracleAccepted -> successor
        (_, outcome, _) -> error ("control-tail admission fixture failed: " <> show outcome)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
