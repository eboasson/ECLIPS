{-# LANGUAGE OverloadedStrings #-}

module AdmissionProperties (tests) where

import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Oracle.Admission
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Identity
import Eclips.Oracle.Receipt qualified as Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Protocol.Oracle.Codec
import Eclips.Protocol.Oracle.Types qualified as Wire
import OracleFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Herald admission EORC carriers"
    [testCase "Begin, exact old seals, old/new Ready and activation cross canonical carriers" caseAdmissionCarriers]

caseAdmissionCarriers :: Assertion
caseAdmissionCarriers = do
  let initial = F.fixtureInitialState
      generation = oracleCurrentMembership initial
      members = NE.toList (heraldMembershipGenerationActiveHeraldEpochs generation)
      applicant = F.heraldEpoch 201
      manifest = heraldAdmissionManifest F.fixtureSystemId (F.heraldId 200) applicant
      contribution = F.checked "contribution" (mkTopologyOccurrenceDigest (F.identifierBytes 220))
      anchor = F.checked "anchor" (topologyCut (sameGenerationPredecessor (F.checked "predecessor" (mkTopologyCutId (F.identifierBytes 240)))) (topologyFrontier (emptyStructuralVersionVector generation) (controlIndex 0)) contribution)
      cut = F.checked "cut" (topologyCut (topologyCutPredecessor anchor) (topologyFrontier (emptyStructuralVersionVector generation) (controlIndex 1)) contribution)
  begun <- submitCarrier (NE.head (heraldMembershipGenerationActiveHeraldEpochs generation)) (beginHeraldAdmissionCommand manifest anchor) initial
  let record = fromJust (oraclePendingHeraldAdmission begun)
      ident = admissionRecordId record
      recipe = F.checked "recipe" (heraldJoinBaseRecipe ident applicant generation cut contribution)
      digest = F.checked "join digest" (mkHeraldJoinDigest (topologyOccurrenceDigestBytes contribution))
      recipeDigest = F.checked "recipe digest" (mkHeraldJoinDigest (heraldJoinBaseRecipeDigestBytes recipe))
      seal = F.checked "seal" (heraldJoinSeal ident 1 cut (controlIndex 1) [(h, heraldJoinMemberCut 0 0 digest) | h <- members] digest recipeDigest)
      report h = heraldJoinReadyReport ident 1 h (joinSealDigest seal) (controlIndex 1) recipeDigest
  sealed <- submitCarrier (NE.head (heraldMembershipGenerationActiveHeraldEpochs generation)) (sealHeraldAdmissionCommand seal) begun
  accepted <- foldM (\state h -> submitCarrier h (acceptHeraldJoinSealCommand ident 1 (joinSealDigest seal)) state) sealed members
  oldReady <- foldM (\state h -> submitCarrier h (reportHeraldJoinBaseReadyCommand (report h)) state) accepted members
  ready <- submitCarrier (NE.head (heraldMembershipGenerationActiveHeraldEpochs generation)) (reportHeraldJoinReadyCommand (report applicant)) oldReady
  active <- submitCarrier (NE.head (heraldMembershipGenerationActiveHeraldEpochs generation)) (activateHeraldCommand ident) ready
  assertBool "activation installed" (applicant `elem` NE.toList (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership active)))
  cancelled <- submitCarrier (NE.head (heraldMembershipGenerationActiveHeraldEpochs generation)) (cancelHeraldAdmissionCommand ident) begun
  oraclePendingHeraldAdmission cancelled @?= Nothing

submitCarrier :: HeraldEpoch -> OracleCommand -> OracleState -> IO OracleState
submitCarrier home command state = do
  let request = oracleClientRequestId home (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
      envelope = oracleEnvelope request Nothing home command
      dto = canonicalOracleEnvelopeDtoFromValue envelope
      wire = Wire.OracleClientEnvelope (Wire.SubmitOracleCommand dto)
  decodeOracleEnvelope (encodeOracleEnvelope wire) @?= Right wire
  canonicalOracleEnvelopeDtoValue dto @?= Right envelope
  let (next, outcome, effects) = F.checked "Oracle transition" (stepOracle envelope state)
  case outcome of
    OracleCommitted receipt -> do
      Receipt.oracleReceiptResult receipt @?= Receipt.OracleAccepted
      let receiptDto = canonicalOracleReceiptDtoFromValue receipt
          receiptWire = Wire.OracleServerEnvelope (Wire.OracleReceipt receiptDto)
      canonicalOracleReceiptDtoValue receiptDto @?= Right receipt
      decodeOracleEnvelope (encodeOracleEnvelope receiptWire) @?= Right receiptWire
    other -> assertFailure ("admission did not commit: " <> show other)
  case oracleEffects effects of
    [EmitAppliedOracleEntry entry] -> do
      let dtoEntry = canonicalAppliedOracleEntryDtoFromValue entry
      canonicalAppliedOracleEntryDtoValue dtoEntry @?= Right entry
    _ -> assertFailure "admission did not emit one canonical entry"
  pure next
