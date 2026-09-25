{-# LANGUAGE OverloadedStrings #-}

module AdminProperties (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word8)
import Eclips.Deployment.Admin.Exchange (receiveAdministrationReplies)
import Eclips.Deployment.Configuration (deploymentEndpoints)
import Eclips.Deployment.Manifest (checkDeployment, deploymentCheckedOracleGenesis)
import Eclips.Oracle.Canonical (canonicalizeOracleReceipt)
import Eclips.Oracle.Command (oracleEnvelope, registerOracleReplicaCommand)
import Eclips.Oracle.Effect (OracleStepOutcome (OracleCommitted))
import Eclips.Oracle.Genesis (checkedOracleRaftVoterBindings, raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter (oracleReplicaContact, oracleReplicaEndpoint)
import Eclips.Protocol.Admin.Frame (encodeAdminFrame)
import Eclips.Protocol.Admin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "administration reply framing"
    [ testCase "coalesced exact terminal receipts produce one result" caseCoalescedReceipt,
      testCase "every frame split preserves one exact terminal receipt" caseReceiptSplits,
      testCase "one-byte fragments preserve one exact terminal receipt" caseReceiptBytes,
      testProperty "arbitrary chunks preserve drain acceptance and one final receipt" propDrainChunks,
      testProperty "arbitrary chunks keep the connection through pending mutation acceptance" propMutationChunks,
      testCase "coalesced distinct replies and status transitions remain observable" caseDistinctReplies
    ]

caseCoalescedReceipt :: Assertion
caseCoalescedReceipt = do
  replies <- receiveChunks [frames [receiptReply, receiptReply]]
  replies @?= [receiptReply]

caseReceiptSplits :: Assertion
caseReceiptSplits = do
  let bytes = frames [receiptReply, receiptReply]
  forM_ [1 .. Bytes.length bytes - 1] $ \offset -> do
    replies <- receiveChunks [Bytes.take offset bytes, Bytes.drop offset bytes]
    assertEqual ("terminal receipt split at byte " <> show offset) [receiptReply] replies

caseReceiptBytes :: Assertion
caseReceiptBytes = do
  replies <- receiveChunks (map Bytes.singleton (Bytes.unpack (frames [receiptReply, receiptReply])))
  replies @?= [receiptReply]

propDrainChunks :: NonEmptyList (Positive Word8) -> Property
propDrainChunks (NonEmpty sizes) = ioProperty $ do
  let accepted = HeraldDrainAccepted correlation
      completed = HeraldDrained correlation
      bytes = frames [accepted, accepted, completed, completed]
      chunks = splitChunks (cycle (map (fromIntegral . getPositive) sizes)) bytes
  replies <- receiveChunks chunks
  pure (replies === [accepted, completed])

propMutationChunks :: NonEmptyList (Positive Word8) -> Property
propMutationChunks (NonEmpty sizes) = ioProperty $ do
  let accepted = AdminResult correlation AdminAccepted
      bytes = frames [accepted, accepted, receiptReply, receiptReply]
      chunks = splitChunks (cycle (map (fromIntegral . getPositive) sizes)) bytes
  replies <- receiveChunks chunks
  pure (replies === [accepted, receiptReply])

caseDistinctReplies :: Assertion
caseDistinctReplies = do
  let accepted = AdminResult correlation AdminAccepted
      conflict = AdminConflict correlation
      otherCorrelation = AdminResult (adminCorrelationIdClaim 2) AdminAccepted
      rejected = AdminResult correlation (AdminRejected AdminOracleReplicaEndpointsNotConfiguredDto)
      distinct = [accepted, receiptReply, conflict, otherCorrelation, rejected]
  replies <- receiveChunks [frames (distinct <> reverse distinct)]
  replies @?= distinct

frames :: [AdminServerDto] -> ByteString
frames = foldMap (encodeAdminFrame . AdminServerEnvelope)

receiveChunks :: [ByteString] -> IO [AdminServerDto]
receiveChunks chunks = do
  remaining <- newIORef chunks
  receiveAdministrationReplies
    $ atomicModifyIORef' remaining
    $ \case
      [] -> ([], Bytes.empty)
      next : rest -> (rest, next)

splitChunks :: [Int] -> ByteString -> [ByteString]
splitChunks _ bytes | Bytes.null bytes = []
splitChunks [] bytes = [bytes]
splitChunks (size : sizes) bytes =
  let (next, rest) = Bytes.splitAt size bytes
   in next : splitChunks sizes rest

correlation :: AdminCorrelationIdClaim
correlation = adminCorrelationIdClaim 1

-- Use a real committed voter receipt, the terminal result retained by mutation
-- retry and GetAdminResult within one established operator lifetime.
receiptReply :: AdminServerDto
receiptReply =
  let planes = checked (deploymentEndpoints "127.0.0.1" 48000 Nothing)
      deployment = checked (checkDeployment (Bytes.replicate 32 83) (NonEmpty.singleton planes))
      genesis = deploymentCheckedOracleGenesis deployment
      initial = checked (initialOracle genesis)
      voter = case checkedOracleRaftVoterBindings genesis of [value] -> value; _ -> error "expected one founder voter"
      home = raftVoterBindingHeraldEpoch voter
      endpoint = checked . oracleReplicaEndpoint "127.0.0.1"
      contact = oracleReplicaContact (endpoint 48003) (endpoint 48004) (endpoint 48000)
      command = registerOracleReplicaCommand (raftVoterBindingNode voter) home contact
      envelope = oracleEnvelope (oracleClientRequestId home 1) Nothing home command
   in case checked (stepOracle envelope initial) of
        (_, OracleCommitted receipt, _) -> AdminResult correlation (AdminOracleVoterResult (canonicalizeOracleReceipt receipt))
        (_, outcome, _) -> error ("expected a committed voter receipt: " <> show outcome)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
