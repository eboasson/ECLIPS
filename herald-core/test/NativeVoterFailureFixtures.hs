{-# LANGUAGE ImportQualifiedPost #-}

-- | Complete public Oracle/native history for composing accepted voter failure
-- with retained Herald work. Every prior command also appears in the sealed
-- native log; no synthetic semantic prefix or unchecked configuration is used.
module NativeVoterFailureFixtures
  ( NativeVoterFailureTranscript (..),
    nativeVoterFailureTranscript,
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch, controlIndex, controlIndexWord64)
import Eclips.Domain.Membership qualified as M
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId, oracleClientRequestSequence)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter
import Eclips.Raft.Effect qualified as RE
import Eclips.Raft.Genesis qualified as RG
import Eclips.Raft.Identity qualified as RI
import Eclips.Raft.Input qualified as R
import Eclips.Raft.Transition qualified as RT

data NativeVoterFailureTranscript = NativeVoterFailureTranscript
  { nativeFailurePrelude :: [(OracleState, CanonicalAppliedOracleEntry)],
    nativeFailureEvidence :: [(OracleState, CanonicalAppliedOracleEntry)],
    nativeFailureJoint :: (OracleState, CanonicalAppliedOracleEntry),
    nativeFailureExcluded :: (OracleState, CanonicalAppliedOracleEntry),
    nativeFailureRetired :: (OracleState, CanonicalAppliedOracleEntry),
    nativeFailureCertificate :: AcceptedVoterHostFailure
  }

nativeVoterFailureTranscript :: CheckedOracleGenesis -> [OracleEnvelope] -> HeraldEpoch -> NativeVoterFailureTranscript
nativeVoterFailureTranscript genesis prefix target =
  let initial = checked (initialOracle genesis)
      prelude = commandStates initial prefix
      predecessor = case prelude of [] -> initial; _ -> fst (lastValue prelude)
      configuration = oracleVoterConfiguration predecessor
      bindings = voterConfigurationBindings configuration
      survivors = filter (/= target) (fmap raftVoterBindingHeraldEpoch bindings)
      majority = length bindings `div` 2 + 1
      reporters = if length survivors >= majority then take majority survivors else error "failure fixture has no surviving old majority"
      home = first reporters
      firstIndex = controlIndexWord64 (oracleGreatestControlIndex predecessor) + 1
      -- Keep the fixture's backoffice requests distinct from the composed
      -- Herald owner's ordinary request allocator, which starts at one.
      firstRequest = 900001 + maximum (0 : fmap (oracleClientRequestSequence . oracleEnvelopeRequestId) prefix)
      envelope ordinal reporter = oracleEnvelope (oracleClientRequestId reporter (firstRequest + ordinal - firstIndex)) Nothing reporter
      probe = checked (M.deriveHeraldFailureProbeId (controlIndex firstIndex))
      resolution = M.deriveFailureProbeResolutionId probe M.RetireFailureProbeTarget
      commands =
        envelope firstIndex home (openHeraldFailureProbeCommand target (M.heraldMembershipGenerationId (oracleCurrentMembership predecessor)) (voterConfigurationId configuration))
          : [envelope ordinal reporter (reportHeraldFailureProbeCommand probe (voterConfigurationId configuration) ProbeUnreachable) | (ordinal, reporter) <- zip [firstIndex + 1 ..] reporters]
            <> [envelope (firstIndex + fromIntegral majority + 1) home (acceptVoterHostFailureCommand resolution)]
      commandHistory = commandStates predecessor commands
      preparing = fst (lastValue commandHistory)
      certificate = case oracleFailureProbe probe preparing of
        Just (FailureProbeView _ _ _ _ _ _ (Just (ProbeVoterHostFailureAcceptedView accepted))) -> accepted
        other -> error ("missing failure certificate: " <> show other)
      ident = acceptedVoterHostFailureChangeId certificate
      (joint, jointMetadata) = checked (prepareOracleConfiguration ident JointConfigurationStage preparing)
      payloads = fmap (R.Application . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope) (prefix <> commands) <> [R.Configuration joint jointMetadata]
      (jointState, jointEntry) = checked (applyCommittedOracleConfiguration (sealedLast initial payloads) preparing)
      (final, finalMetadata) = checked (prepareOracleConfiguration ident FinalConfigurationStage jointState)
      (excluded, finalEntry) = checked (applyCommittedOracleConfiguration (sealedLast initial (payloads <> [R.Configuration final finalMetadata])) jointState)
      retired = retireExcludedHost (firstRequest + fromIntegral majority + 2) certificate excluded
      canonical (state, entry) = (state, canonicalizeAppliedOracleEntry entry)
   in NativeVoterFailureTranscript
        { nativeFailurePrelude = fmap canonical prelude,
          nativeFailureEvidence = fmap canonical commandHistory,
          nativeFailureJoint = canonical (jointState, jointEntry),
          nativeFailureExcluded = canonical (excluded, finalEntry),
          nativeFailureRetired = canonical retired,
          nativeFailureCertificate = certificate
        }

retireExcludedHost :: Word64 -> AcceptedVoterHostFailure -> OracleState -> (OracleState, AppliedOracleEntry)
retireExcludedHost ordinal certificate state =
  let home = fst (first (acceptedVoterHostFailureReports certificate))
   in command (oracleEnvelope (oracleClientRequestId home ordinal) Nothing home (retireHeraldEpochCommand (acceptedVoterHostFailureResolution certificate) (acceptedVoterHostFailureTarget certificate))) state

commandStates :: OracleState -> [OracleEnvelope] -> [(OracleState, AppliedOracleEntry)]
commandStates _ [] = []
commandStates state (envelope : remaining) = let result@(next, _) = command envelope state in result : commandStates next remaining
command :: OracleEnvelope -> OracleState -> (OracleState, AppliedOracleEntry)
command envelope state = case checked (stepOracle envelope state) of
  (next, OracleCommitted receipt, effects) | oracleReceiptResult receipt == OracleAccepted -> case [entry | EmitAppliedOracleEntry entry <- oracleEffects effects] of
    [entry] -> (next, entry)
    _ -> error "failure fixture command omitted its applied entry"
  other -> error ("failure fixture command was not accepted: " <> show other)

sealedLast :: OracleState -> [R.RaftEntry ByteString] -> RE.RaftCommittedEntry ByteString
sealedLast state payloads =
  let native = checkedOracleRaftNativeConfiguration (oracleStateGenesis state)
      nodes = RG.raftNativeVoters native
      leader = first nodes
      follower = first (drop 1 nodes)
      genesis = checked (RG.checkRaftGenesis (RG.raftGenesis follower nodes (RG.raftNativeHeartbeatInterval native) (RG.raftNativeElectionTimeoutLower native) (RG.raftNativeElectionTimeoutUpper native)))
      entries = zipWith (\ordinal payload -> R.raftLogEntry (RI.raftLogIndex ordinal) (RI.raftTerm 1) payload) [1 ..] payloads
      rpc = checked (R.appendEntries (RI.raftTerm 1) leader (RI.raftLogIndex 0) (RI.raftTerm 0) entries (RI.raftLogIndex (fromIntegral (length payloads))))
      (_, effects) = checked (RT.stepRaft (checked (R.observeRaftRequest leader rpc)) (RT.initialRaft genesis))
   in lastValue [entry | RE.ExposeCommittedEntries batch <- RE.raftEffectBatchEffects effects, entry <- NE.toList batch]
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
first :: [value] -> value
first (value : _) = value
first [] = error "empty failure intersection fixture"
lastValue :: [value] -> value
lastValue [value] = value
lastValue (_ : remaining) = lastValue remaining
lastValue [] = error "empty failure intersection fixture"
