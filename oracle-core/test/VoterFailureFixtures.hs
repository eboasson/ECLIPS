-- | Reusable pure intersection fixture. Existing semantic preludes retain their
-- Oracle state; the native fixture supplies sealed forward configuration entries.
-- Runtime properties separately replay the complete command/native log over TCP.
module VoterFailureFixtures
  ( excludeVoterHost,
    retireExcludedHost,
    retireVoterHost,
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Eclips.Domain.Identity (HeraldEpoch, controlIndex, controlIndexWord64)
import Eclips.Domain.Membership qualified as M
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter
import Eclips.Raft.Configuration (RaftConfigurationRefView (GenesisRaftConfigurationRefView), raftConfigurationRefView)
import Eclips.Raft.Effect qualified as RE
import Eclips.Raft.Genesis qualified as RG
import Eclips.Raft.Identity qualified as RI
import Eclips.Raft.Input qualified as R
import Eclips.Raft.Transition qualified as RT

excludeVoterHost :: HeraldEpoch -> OracleState -> (OracleState, AcceptedVoterHostFailure, [AppliedOracleEntry])
excludeVoterHost target initial =
  let configuration = oracleVoterConfiguration initial
      bindings = voterConfigurationBindings configuration
      survivors = filter (/= target) (fmap raftVoterBindingHeraldEpoch bindings)
      majority = length bindings `div` 2 + 1
      reporters = if length survivors >= majority then take majority survivors else error "failure fixture has no surviving old majority"
      home = first reporters
      firstIndex = controlIndexWord64 (oracleGreatestControlIndex initial) + 1
      envelope ordinal reporter = oracleEnvelope (oracleClientRequestId reporter ordinal) Nothing reporter
      probe = checked (M.deriveHeraldFailureProbeId (controlIndex firstIndex))
      resolution = M.deriveFailureProbeResolutionId probe M.RetireFailureProbeTarget
      commands =
        envelope firstIndex home (openHeraldFailureProbeCommand target (M.heraldMembershipGenerationId (oracleCurrentMembership initial)) (voterConfigurationId configuration))
          : [envelope ordinal reporter (reportHeraldFailureProbeCommand probe (voterConfigurationId configuration) ProbeUnreachable) | (ordinal, reporter) <- zip [firstIndex + 1 ..] reporters]
            <> [envelope (firstIndex + fromIntegral majority + 1) home (acceptVoterHostFailureCommand resolution)]
      commandHistory = commandStates initial commands
      preparing = fst (lastValue commandHistory)
      certificate = case oracleFailureProbe probe preparing of
        Just (FailureProbeView _ _ _ _ _ _ (Just (ProbeVoterHostFailureAcceptedView accepted))) -> accepted
        other -> error ("missing failure certificate: " <> show other)
      ident = acceptedVoterHostFailureChangeId certificate
      (joint, jointMetadata) = checked (prepareOracleConfiguration ident JointConfigurationStage preparing)
      prior = case raftConfigurationRefView (voterConfigurationNativeRef configuration) of
        GenesisRaftConfigurationRefView -> replicate (fromIntegral (firstIndex - 1)) R.LeaderNoOp
        _ -> error "failure intersection fixture begins before the first native change"
      payloads = prior <> fmap (R.Application . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope) commands <> [R.Configuration joint jointMetadata]
      (jointState, jointEntry) = checked (applyCommittedOracleConfiguration (sealedLast initial payloads) preparing)
      (final, finalMetadata) = checked (prepareOracleConfiguration ident FinalConfigurationStage jointState)
      (excluded, finalEntry) = checked (applyCommittedOracleConfiguration (sealedLast initial (payloads <> [R.Configuration final finalMetadata])) jointState)
   in (excluded, certificate, fmap snd commandHistory <> [jointEntry, finalEntry])

retireExcludedHost :: AcceptedVoterHostFailure -> OracleState -> (OracleState, AppliedOracleEntry)
retireExcludedHost certificate state =
  let home = fst (first (acceptedVoterHostFailureReports certificate))
      ordinal = 1 + controlIndexWord64 (oracleGreatestControlIndex state)
   in command (oracleEnvelope (oracleClientRequestId home ordinal) Nothing home (retireHeraldEpochCommand (acceptedVoterHostFailureResolution certificate) (acceptedVoterHostFailureTarget certificate))) state

retireVoterHost :: HeraldEpoch -> OracleState -> OracleState
retireVoterHost target initial = let (excluded, certificate, _) = excludeVoterHost target initial in fst (retireExcludedHost certificate excluded)

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
