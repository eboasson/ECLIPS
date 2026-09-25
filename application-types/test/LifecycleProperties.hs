{-# LANGUAGE OverloadedStrings #-}

module LifecycleProperties (tests) where

import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Lazy
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Identity
import Eclips.Application.Types.Lifecycle
import Eclips.Public.Types.Timing qualified as Timing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "lifecycle shapes"
    [ testProperty "all lifecycle identities preserve session scope in Binary" propIdentities,
      testProperty "session-scoped request and preparation identities do not alias" propSessionScopes,
      testProperty "connection descriptors carry checked nondefault timing through Binary" propDescriptorTiming,
      testCase "descriptor and initial claim decode recheck exact widths" caseMalformed,
      testCase "all lifecycle commands and replies round trip without widening operations" caseVocabulary
    ]
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
roundTrip :: (Binary value, Eq value) => value -> Bool
roundTrip value = case decodeOrFail (encode value) of
  Right (rest, _, actual) -> Lazy.null rest && actual == value
  Left _ -> False
locator :: HeraldLocator
locator = checked (heraldLocator "127.0.0.1" 7123)
bytes :: Bytes.ByteString
bytes = Bytes.replicate 32 0x28
propIdentities :: Word8 -> Word64 -> Word64 -> Bool
propIdentities byte session ordinal =
  and
    [ roundTrip (checked (lifecycleRequestId scope session ordinal)),
      roundTrip (checked (childPreparation scope session ordinal)),
      roundTrip (checked (connectionDescriptor locator scope bytes scope)),
      roundTrip (checked (initialClaimId scope))
    ]
  where
    scope = Bytes.replicate 32 byte
propSessionScopes :: NonNegative Int -> Bool
propSessionScopes (NonNegative value) = first /= second && prepFirst /= prepSecond
  where
    session = fromIntegral (value `mod` 100000)
    first = checked (lifecycleRequestId bytes session 0)
    second = checked (lifecycleRequestId bytes (session + 1) 0)
    prepFirst = checked (childPreparation bytes session 0)
    prepSecond = checked (childPreparation bytes (session + 1) 0)
propDescriptorTiming :: NonNegative Int -> Bool
propDescriptorTiming (NonNegative sample) =
  connectionDescriptorTakeoverTarget descriptor == target
    && roundTrip descriptor
  where
    target = checked (Timing.takeoverTarget (500 + fromIntegral (sample `mod` 60_000_000)))
    descriptor = checked (connectionDescriptorWithTiming target locator bytes bytes bytes)
caseMalformed :: IO ()
caseMalformed = do
  assertEqual "empty host" (Left LifecycleEmptyHost) (heraldLocator "" 1)
  assertEqual "zero port" (Left LifecycleZeroPort) (heraldLocator "127.0.0.1" 0)
  assertBool "short claim fails decoded admission" (isLeft (decodeOrFail (encode (Bytes.replicate 31 1)) :: Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, InitialClaimId)))
  assertBool "short descriptor epoch fails decoded admission" (isLeft (decodeOrFail (encode (Timing.defaultTakeoverTarget, locator, bytes, Bytes.replicate 31 2, bytes)) :: Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, ConnectionDescriptor)))
  assertBool "descriptor target fails decoded admission" (isLeft (decodeOrFail (encode (0 :: Word64, locator, bytes, bytes, bytes)) :: Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, ConnectionDescriptor)))
  assertEqual "ordinary descriptor supplies the deployment default" Timing.defaultTakeoverTarget (connectionDescriptorTakeoverTarget (checked (connectionDescriptor locator bytes bytes bytes)))
  where
    isLeft (Left _) = True
    isLeft _ = False
caseVocabulary :: IO ()
caseVocabulary = do
  let preparation = checked (childPreparation bytes 1 2)
      request = checked (lifecycleRequestId bytes 1 2)
      descriptor = checked (connectionDescriptor locator bytes bytes bytes)
      child = preparedChild preparation (Identity.asPrivateProcessId (checked (Identity.mkPrivateUniqueId 1))) descriptor
      selection = checked (Access.primordialSelection [] Set.empty Nothing)
      commands = [BeginChild locator selection, AwaitPreparedChild preparation, AwaitChildReady preparation, CancelChild preparation, EndOwnProcess]
      results = [ChildPreparationAccepted preparation, ChildPrepared child, ChildReady, ChildCancelled, ChildAlreadyAttached, ProcessEnded]
      replies =
        [LifecycleReply request (LifecyclePending ["writer", "reader"]), LifecycleAbsent request, LifecycleConflict request, LifecycleRetired request 2]
          <> [LifecycleReply request (LifecycleCompleted result) | result <- results]
          <> [LifecycleReply request (LifecycleRejected problem) | problem <- [LifecycleUnknownTarget, LifecycleTargetUnavailable, LifecycleSelectionNotAdmitted, LifecycleUnknownPreparation, LifecycleNotPreparingParent, LifecycleAlreadyTerminal, LifecycleRequestNotAdmitted, LifecycleOwnProcessNotLive, LifecycleOracleRejected, LifecycleStartupFailed (StartupRequiredObjectUnavailable "reader")]]
  assertBool "commands" (all roundTrip commands)
  assertBool "replies" (all roundTrip replies)
  assertBool "separate kind witness" (not (lifecycleResultMatches EndOwnProcess ChildReady))
