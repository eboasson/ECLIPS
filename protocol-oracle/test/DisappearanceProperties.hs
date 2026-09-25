{-# LANGUAGE ImportQualifiedPost #-}

module DisappearanceProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.List (permutations)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Disappearance
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Sort.Profile
import Eclips.Domain.SortOccurrence
import Eclips.Domain.Startup (HeraldMember (..), deriveInitialProjectionDigest)
import Eclips.Oracle.Disappearance qualified as D
import Eclips.Oracle.Genesis (checkOracleGenesis, deriveRaftConfigurationDigest, oracleGenesis)
import Eclips.Oracle.Identity
import Eclips.Oracle.Label qualified as O
import Eclips.Oracle.Voter qualified as V
import Eclips.Protocol.Oracle.Codec
import Eclips.Protocol.Oracle.Types
import OracleFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "sole-current disappearance EORC carriers"
    [ testCase "both subject arms and all five commands cross unchanged EORC envelope, receipt, and applied-entry carriers" caseLiveCarriers,
      testCase "repeated non-voter retirements and stale failure receipts cross canonical EORC carriers" caseRepeatedRetirementCarriers
    ]

caseLiveCarriers :: Assertion
caseLiveCarriers = forM_ subjects $ \subject -> do
  -- Resolve, Invalidate, and authorized Abort each get their own complete run.
  forM_ [0 :: Int, 1, 2] $ \terminal -> do
    let initial = O.initialOracleState F.fixtureCheckedGenesis
        membership = O.oracleCurrentMembership initial
        home = F.fixtureHeraldEpoch
        envelope number reporter = O.oracleEnvelope (oracleClientRequestId reporter number) Nothing reporter
    (opened, receipt) <- submitCarrier (envelope 1 home (O.openDisappearanceProbeCommand subject (disappearanceSubjectMembershipCoordinate subject membership))) initial
    probe <- case O.oracleReceiptDisappearanceResult receipt of
      Just (D.DisappearanceOpenAccepted result) -> pure $ case disappearanceOpenResultView result of
        OpenedDisappearanceProbe identifier -> identifier
        AliasedDisappearanceProbe identifier -> identifier
      other -> assertFailure ("missing live Open result " <> show other)
    let members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
        claims = [F.checked "evidence" (admitDisappearanceEvidenceClaim subject membership probe reporter (deriveDisappearanceEvidenceDigest (heraldEpochBytes reporter))) | reporter <- members]
    reported <-
      foldM
        ( \state claim ->
            fst
              <$> submitCarrier
                (envelope 2 (disappearanceEvidenceClaimReporter claim) (O.reportPredefinedAbsenceCommand probe claim))
                state
        )
        opened
        claims
    let command = case terminal of
          0 -> O.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest claims)
          1 -> O.invalidateDisappearanceProbeCommand probe home D.MatchingPublicationObserved (deriveDisappearanceEvidenceDigest (heraldEpochBytes home))
          _ -> O.abortDisappearanceProbeCommand probe D.authorizedDisappearanceAbortReason
    (_, terminalReceipt) <- submitCarrier (envelope 3 home command) reported
    assertEqual "live terminal accepted" O.OracleAccepted (O.oracleReceiptResult terminalReceipt)
  where
    subjects =
      [ F.checked
          "controlled subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              (F.globalObjectId 200)
              (structuralOccurrenceId F.fixtureHeraldEpoch (F.checked "sequence" (mkStructuralSequence 1)))
              Nothing
          ),
        regularSortDefinitionDisappearanceSubject
          (deriveRegularSortOccurrenceClaim F.fixtureSystemId (predefinedCatalogueDescriptor (profileEntryFor EdgeRole)) Genesis)
      ]

caseRepeatedRetirementCarriers :: Assertion
caseRepeatedRetirementCarriers = forM_ (permutations [h4, h5]) $ \targets -> do
  final <- foldM retire initial targets
  assertEqual "the complete checked membership chain survives EORC" 3 (length (O.oracleMembershipHistory final))
  assertEqual "retirement tombstones preserve control order" targets (O.oracleRetiredHeralds final)
  where
    h4 = F.heraldEpoch 17
    h5 = F.heraldEpoch 19
    home = F.fixtureHeraldEpoch
    members = F.fixtureMembers <> [HeraldMember (F.heraldId 16) h4, HeraldMember (F.heraldId 18) h5]
    topology = F.fixtureTopologyFor F.fixtureSystemId members F.fixtureBootstraps
    initial =
      O.initialOracleState
        $ F.checked "repeated retirement genesis"
        $ checkOracleGenesis
        $ oracleGenesis
          F.fixtureSystemId
          members
          profileCatalogueDigest
          F.fixtureDescriptors
          F.fixtureBootstraps
          F.fixtureConfigurationDigest
          topology
          (deriveInitialProjectionDigest F.fixtureBootstraps topology)
          F.fixtureBindings
          F.fixtureRaftConfiguration
          (deriveRaftConfigurationDigest F.fixtureSystemId F.fixtureBindings F.fixtureRaftConfiguration)
    submit reporter command state =
      let sequenceNumber = 1 + controlIndexWord64 (O.oracleGreatestControlIndex state)
       in submitCarrier (O.oracleEnvelope (oracleClientRequestId reporter sequenceNumber) Nothing reporter command) state
    retire state target = do
      let generation = heraldMembershipGenerationId (O.oracleCurrentMembership state)
      (opened, openReceipt) <- submit home (O.openHeraldFailureProbeCommand target generation (V.voterConfigurationId (V.oracleVoterConfiguration state))) state
      probe <- case O.oracleReceiptFailureResult openReceipt of
        Just (O.FailureProbeOpened identifier) -> pure identifier
        other -> assertFailure ("missing failure probe " <> show other)
      reported <- foldM (\current reporter -> fst <$> submit reporter (O.reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration state)) O.ProbeUnreachable) current) opened [home, F.fixtureRemoteHeraldEpoch]
      let resolution = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
          command = O.retireHeraldEpochCommand resolution target
      (retired, receipt) <- submit home command reported
      assertEqual "current retirement accepted" O.OracleAccepted (O.oracleReceiptResult receipt)
      assertEqual "canonical successor carries the exact retirement identity" (Just resolution) (heraldMembershipGenerationRetirementId (O.oracleCurrentMembership retired))
      (stale, staleReceipt) <- submit home (O.reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration state)) O.ProbeUnreachable) retired
      assertBool "terminal evidence is rejected through the same carrier" (O.oracleReceiptResult staleReceipt /= O.OracleAccepted)
      (replayed, replayReceipt) <- submit home command stale
      assertEqual "fresh semantic retirement retry stays accepted" O.OracleAccepted (O.oracleReceiptResult replayReceipt)
      assertEqual "semantic retry cannot extend membership history" (O.oracleMembershipHistory retired) (O.oracleMembershipHistory replayed)
      pure replayed

submitCarrier :: O.OracleEnvelope -> O.OracleState -> IO (O.OracleState, O.OracleReceipt)
submitCarrier envelope state = do
  let submitDto = canonicalOracleEnvelopeDtoFromValue envelope
      client = OracleClientEnvelope (SubmitOracleCommand submitDto)
  assertEqual "EORC submit" (Right client) (decodeOracleEnvelope (encodeOracleEnvelope client))
  decodedEnvelope <- either (assertFailure . show) pure (canonicalOracleEnvelopeDtoValue submitDto)
  let (successor, outcome) = O.submitOracleState decodedEnvelope state
  case O.oracleSubmissionOutcomeView outcome of
    O.OracleSubmissionCommittedView receipt entry -> do
      let receiptDto = canonicalOracleReceiptDtoFromValue receipt
          entryDto = canonicalAppliedOracleEntryDtoFromValue entry
          receiptMessage = OracleServerEnvelope (OracleReceipt receiptDto)
      assertEqual "EORC receipt" (Right receiptMessage) (decodeOracleEnvelope (encodeOracleEnvelope receiptMessage))
      assertEqual "canonical result survives carrier" (Right receipt) (canonicalOracleReceiptDtoValue receiptDto)
      entries <- either (assertFailure . show) pure (committedOracleEntriesDtoFromValues (O.oracleGreatestControlIndex state) [entry])
      let watch = OracleServerEnvelope (CommittedOracleEntries entries)
      assertEqual "EORC applied-entry watch" (Right watch) (decodeOracleEnvelope (encodeOracleEnvelope watch))
      assertEqual "complete projection survives carrier" (Right entry) (canonicalAppliedOracleEntryDtoValue entryDto)
      pure (successor, receipt)
    other -> assertFailure ("expected live carrier commit " <> show other)
