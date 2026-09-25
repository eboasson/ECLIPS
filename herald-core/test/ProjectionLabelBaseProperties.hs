{-# LANGUAGE OverloadedStrings #-}

module ProjectionLabelBaseProperties (tests, projectionLabelTestHistory) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Startup (appliedProcessEpochId, appliedProcessResidence, heraldMemberEpoch)
import Eclips.Domain.Value (Label, LabelOwner (..))
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Oracle
import Eclips.Oracle.Receipt (OracleReceiptResult (OracleAccepted), oracleReceiptResult)
import GenesisFixtures qualified as Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "portable Projection label admission"
    [ QC.testProperty "generated decisions, completions and End prefixes round-trip without replay" propLabelPrefixes,
      testCase "admission ignores unrelated context label maps" caseIndependentContext,
      testCase "canonical NotApplied may name an unknown caller" caseUnknownCaller,
      testCase "later End preserves old release while a zombie takeover derives its successor" caseEndAndTakeover,
      testCase "malformed and incoherent projected label facts are rejected" caseMalformedFacts
    ]

type History = (Oracle.OracleState, [CanonicalAppliedOracleEntry])

type RawLabelBase =
  (ByteString, (ByteString, ByteString, ByteString, ByteString), Word64, [(ByteString, Either ByteString (ByteString, Bool))])

local, remote :: HeraldEpoch
local = heraldMemberEpoch Fixtures.fixtureLocalMember
remote = heraldMemberEpoch Fixtures.fixtureRemoteMember

processAt :: HeraldEpoch -> ProcessEpochId
processAt home = case [appliedProcessEpochId bootstrap | bootstrap <- Projection.oracleViewAppliedBootstraps (Projection.oracleView Fixtures.fixtureOracleProjectionState), appliedProcessResidence bootstrap == home] of
  process : _ -> process
  [] -> error "missing label-base fixture process"

object :: Word8 -> GlobalObjectId
object = checked "label object" . mkGlobalObjectId . Fixtures.fixtureIdentifierBytes

initialHistory :: History
initialHistory = (Oracle.initialOracleState Fixtures.fixtureCheckedOracleGenesis, [])

submit :: HeraldEpoch -> Oracle.OracleCommand -> History -> History
submit home command (state, entries) = case Oracle.oracleSubmissionOutcomeView outcome of
  Oracle.OracleSubmissionCommittedView receipt entry
    | oracleReceiptResult receipt == OracleAccepted -> (next, entries <> [canonicalizeAppliedOracleEntry entry])
  other -> error ("label-base fixture command failed: " <> show other)
  where
    request = oracleClientRequestId home (1 + controlIndexWord64 (Oracle.oracleGreatestControlIndex state))
    (next, outcome) = Oracle.submitOracleState (Oracle.oracleEnvelope request Nothing home command) state

decide :: HeraldEpoch -> ProcessEpochId -> GlobalObjectId -> Label -> Label.LabelTarget -> History -> History
decide home caller targetObject expected target history@(state, _) = submit home command history
  where
    request = oracleClientRequestId home (1 + controlIndexWord64 (Oracle.oracleGreatestControlIndex state))
    ident = Oracle.deriveLabelDecisionId (Genesis.checkedSystemId Fixtures.fixtureStep14CheckedGenesis) request
    cut = Label.homeLabelAcceptanceCut (Label.firstLabelProcessAcceptancePosition caller) EmptyHeraldPublicationPrefix
    command = Oracle.decideLabelCommand ident caller targetObject expected (Just (Label.initialBootstrapLabelEvidence targetObject (processAt local))) Nothing Nothing target cut

complete :: History -> History
complete history@(state, _) = case Oracle.oracleOpenDecisions state of
  [decision] ->
    let ident = Oracle.liveDecisionId decision
        terminal = fromJust (Oracle.oracleTerminalOutcome ident state)
        collector = fromJust (Oracle.oracleLabelCollector ident state)
        report = Oracle.labelCompletionAttestation ident (Oracle.liveDecisionRequestControlIndex decision) (Oracle.deriveLabelOutcomeDigest terminal) collector (Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership state))
     in submit collector (Oracle.completeLabelDecisionCommand report) history
  other -> error ("expected one pending label-base fixture decision: " <> show other)

labelChain :: Word8 -> Int -> History
labelChain identifier count = foldl' step initialHistory [0 .. count - 1]
  where
    step state generation = complete (decide local (processAt local) (object identifier) (ProcessLabel (processAt local), fromIntegral generation) (Label.targetProcess (processAt local)) state)

endLocal :: History -> History
endLocal = submit local (checked "label-base End" (Oracle.endProcessEpochCommand (processAt local) ExplicitAdministrativeEnd))

projectionLabelTestHistory :: Int -> (Oracle.OracleState, [CanonicalAppliedOracleEntry])
projectionLabelTestHistory = mixedHistory

mixedHistory :: Int -> History
mixedHistory count =
  decide remote (processAt remote) (object 221) (ZombieLabel (processAt local), fromIntegral count) (Label.targetProcess (processAt remote)) ended
  where
    prior = labelChain 221 count
    failed = decide local (processAt local) (object 222) (VoidLabel, 9) Label.targetVoid prior
    ended = endLocal failed

prefixes :: History -> [Projection.State]
prefixes (_, entries) = scanl apply Fixtures.fixtureOracleProjectionState entries
  where
    apply state entry = Projection.commitAppliedEntry (checked "label-base projection" (Projection.prepareAppliedEntry entry state))

source :: History -> Projection.State
source = last . prefixes

base :: Projection.State -> Projection.ProjectionLabelBase
base = checked "capture projected labels" . Projection.captureProjectionLabelBase

bytesFor :: Projection.State -> ByteString
bytesFor = Projection.encodeProjectionLabelBase . base

propLabelPrefixes :: QC.Positive Int -> QC.Property
propLabelPrefixes (QC.Positive seed) =
  QC.conjoin
    [ QC.counterexample ("label prefix " <> show ordinal) (Projection.decodeProjectionLabelBase state (bytesFor state) QC.=== Right (base state))
    | (ordinal, state) <- zip [(0 :: Int) ..] (prefixes (mixedHistory (1 + seed `mod` 5)))
    ]

caseIndependentContext :: Assertion
caseIndependentContext = do
  let original = source (labelChain 221 2)
      unrelated = source (labelChain 222 2)
  assertBool "contexts retain different label maps" (Projection.projectedLabelWorkflows original /= Projection.projectedLabelWorkflows unrelated)
  assertEqual "context supplies identity and cut, not expected label contents" (Right (base original)) (Projection.decodeProjectionLabelBase unrelated (bytesFor original))

caseUnknownCaller :: Assertion
caseUnknownCaller = do
  let caller = checked "unknown process" (mkProcessEpochId (Fixtures.fixtureIdentifierBytes 241))
      state = source (decide local caller (object 222) (ProcessLabel (processAt local), 0) Label.targetVoid initialHistory)
  assertBool "caller is absent from current process facts" (not (Projection.oracleViewProcessIsLive caller (Projection.oracleView state)))
  assertEqual "NotApplied retains no prepared facts" [Nothing] [Projection.projectedLabelWorkflowPreparedFacts workflow | (_, workflow) <- Projection.projectedLabelWorkflows state]
  assertEqual "unknown caller failure remains a canonical completed result" (Right (base state)) (Projection.decodeProjectionLabelBase state (bytesFor state))

caseEndAndTakeover :: Assertion
caseEndAndTakeover = do
  let ended = source (endLocal (labelChain 221 1))
      taken = source (mixedHistory 1)
      overlay state = fromJust (Projection.oracleProjectionReleasedLabelAt (Projection.oracleViewControlIndex (Projection.oracleView state)) (object 221) state)
  assertEqual "End retains stored Process overlay" (Label.releasedLabel (ProcessLabel (processAt local), 1)) (Oracle.releasedLabelOverlayState (overlay ended))
  assertEqual "takeover successor is remote process at next generation" (Label.releasedLabel (ProcessLabel (processAt remote), 2)) (Oracle.releasedLabelOverlayState (overlay taken))
  forM_ [ended, taken] $ \state ->
    assertEqual "current process state does not rewrite historical release facts" (Right (base state)) (Projection.decodeProjectionLabelBase state (bytesFor state))

caseMalformedFacts :: Assertion
caseMalformedFacts = do
  let state = source (mixedHistory 2)
      raw@(domain, identity, at, rows) = checked "label tuple" (Serialize.decode (bytesFor state) :: Either String RawLabelBase)
      reject name claim = assertBool name (isLeft (Projection.decodeProjectionLabelBase state (Serialize.encode claim)))
      otherFacts = [facts | (_, Right (facts, _)) <- rows]
      wrongFacts = case otherFacts of
        first : second : _ -> [(decision, case outcome of Right (facts, done) -> Right (if facts == first then second else first, done); _ -> outcome) | (decision, outcome) <- rows]
        _ -> error "label fixture needs distinct releases"
      revived = [(decision, case outcome of Right (facts, _) -> Right (facts, False); _ -> outcome) | (decision, outcome) <- rows]
      invalidDigest = [(decision, case outcome of Left _ -> Left ByteString.empty; _ -> outcome) | (decision, outcome) <- rows]
  reject "wrong domain" ("wrong-domain" :: ByteString, identity, at, rows)
  reject "wrong cut" (domain, identity, at + 1, rows)
  reject "duplicate decision" (domain, identity, at, rows <> take 1 rows)
  reject "noncanonical map order" (domain, identity, at, reverse rows)
  reject "prepared facts belong to another decision" (domain, identity, at, wrongFacts)
  reject "multiple old pending generations cannot occupy one object" (domain, identity, at, revived)
  reject "NotApplied digest has exact width" (domain, identity, at, invalidDigest)
  assertBool "trailing transcript bytes rejected" (isLeft (Projection.decodeProjectionLabelBase state (Serialize.encode raw <> "x")))
  assertBool "truncated transcript rejected" (isLeft (Projection.decodeProjectionLabelBase state (ByteString.init (bytesFor state))))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
