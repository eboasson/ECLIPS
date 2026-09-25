module PrivateIdentityProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (mapAccumL, nub)
import Data.Word (Word8)
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    mkPrivateUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
    mkGlobalUniqueId,
    mkProcessEpochId,
  )
import Eclips.Herald.Application.PrivateIdentity
  ( PrivateIdentity,
    PrivateIdentityError
      ( ProcessEpochAlreadyRegistered,
        ProcessEpochIsRetired,
        ProcessEpochIsUnknown
      ),
    commitLocalization,
    commitPrivateUniqueIdAllocation,
    commitProcessRegistration,
    commitProcessRetirement,
    emptyPrivateIdentity,
    lookupPrivateUniqueId,
    prepareLocalization,
    preparePrivateUniqueIdAllocation,
    prepareProcessRegistration,
    prepareProcessRetirement,
    preparedAllocatedPrivateUniqueId,
    preparedPrivateUniqueId,
    resolvePrivateUniqueId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonEmptyList (getNonEmpty),
    Positive (getPositive),
    Property,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "process-private identity"
    [ testProperty "bindings form a bijection" propBijection,
      testProperty "localization is stable and does not advance on reuse" propStableLocalization,
      testCase "registration successor remains opaque until commit" casePreparedRegistration,
      testCase "process lifecycle is explicit" caseExplicitRegistration,
      testProperty "equal local bits are isolated by process epoch" propProcessIsolation,
      testProperty "uneven allocations leave process counters independent" propUnevenCounters,
      testProperty "retirement removes bindings and prevents epoch reuse" propRetirement,
      testCase "prepared output agrees with committed output" casePreparedOutput,
      testCase "generated allocation prepares a fresh slot before global installation" caseGeneratedAllocation
    ]

propBijection :: NonEmptyList Word8 -> Property
propBijection generated =
  let globals = fmap fixtureGlobalUniqueId (nub (getNonEmpty generated))
      (state, privateIdentities) = localizeMany processA globals (registered processA)
      bindings = zip globals privateIdentities
      forwardAndReverseAgree =
        all
          ( \(globalIdentity, privateIdentity) ->
              lookupPrivateUniqueId processA globalIdentity state == Right (Just privateIdentity)
                && resolvePrivateUniqueId processA privateIdentity state == Right (Just globalIdentity)
          )
          bindings
      privateIdentitiesAreDistinct =
        length (nub privateIdentities) == length privateIdentities
   in counterexample
        ("localized bindings: " <> show bindings)
        (forwardAndReverseAgree && privateIdentitiesAreDistinct)

propStableLocalization :: Word8 -> Property
propStableLocalization byte =
  let firstGlobal = fixtureGlobalUniqueId byte
      nextByte
        | byte == maxBound = minBound
        | otherwise = byte + 1
      nextGlobal = fixtureGlobalUniqueId nextByte
      (afterFirst, firstPrivate) =
        localizeOne processA firstGlobal (registered processA)
      (afterRepeat, repeatedPrivate) =
        localizeOne processA firstGlobal afterFirst
      (afterNext, nextAfterRepeat) =
        localizeOne processA nextGlobal afterRepeat
      (freshAfterFirst, _) =
        localizeOne processA firstGlobal (registered processA)
      (_, nextWithoutRepeat) =
        localizeOne processA nextGlobal freshAfterFirst
   in ( firstPrivate,
        nextAfterRepeat,
        lookupPrivateUniqueId processA firstGlobal afterNext,
        resolvePrivateUniqueId processA firstPrivate afterNext
      )
        === ( repeatedPrivate,
              nextWithoutRepeat,
              Right (Just firstPrivate),
              Right (Just firstGlobal)
            )

casePreparedRegistration :: IO ()
casePreparedRegistration =
  case prepareProcessRegistration processA emptyPrivateIdentity of
    Left problem -> assertFailure ("registration failed: " <> show problem)
    Right prepared -> do
      assertEqual
        "the predecessor still has no process namespace"
        (Left ProcessEpochIsUnknown)
        (lookupPrivateUniqueId processA (fixtureGlobalUniqueId 1) emptyPrivateIdentity)
      assertEqual
        "commit reveals the registered empty namespace"
        (Right Nothing)
        ( lookupPrivateUniqueId
            processA
            (fixtureGlobalUniqueId 1)
            (commitProcessRegistration prepared)
        )

caseExplicitRegistration :: IO ()
caseExplicitRegistration = do
  let globalIdentity = fixtureGlobalUniqueId 7
      privateIdentity = checkedPrivateUniqueId 1
      forgedPrivateIdentity = checkedPrivateUniqueId 42
  assertEqual
    "unknown process cannot localize"
    (Left ProcessEpochIsUnknown)
    (prepareLocalizationError processA globalIdentity emptyPrivateIdentity)
  assertEqual
    "unknown process cannot look up"
    (Left ProcessEpochIsUnknown)
    (lookupPrivateUniqueId processA globalIdentity emptyPrivateIdentity)
  assertEqual
    "unknown process cannot resolve"
    (Left ProcessEpochIsUnknown)
    (resolvePrivateUniqueId processA privateIdentity emptyPrivateIdentity)
  let state = registered processA
  assertEqual
    "registered process begins with an empty namespace"
    (Right Nothing)
    (lookupPrivateUniqueId processA globalIdentity state)
  assertEqual
    "a structurally valid but unallocated private ID does not resolve"
    (Right Nothing)
    (resolvePrivateUniqueId processA forgedPrivateIdentity state)
  let (localizedState, allocatedPrivateIdentity) =
        localizeOne processA globalIdentity state
  assertEqual
    "a forged token does not consume the next private ID"
    privateIdentity
    allocatedPrivateIdentity
  assertEqual
    "the forged token remains unmapped after another allocation"
    (Right Nothing)
    (resolvePrivateUniqueId processA forgedPrivateIdentity localizedState)
  assertPrivateIdentityError
    "duplicate registration is rejected"
    ProcessEpochAlreadyRegistered
    (prepareProcessRegistration processA state)

propProcessIsolation :: Word8 -> Word8 -> Property
propProcessIsolation byteA candidateB =
  let byteB
        | candidateB == byteA && candidateB == maxBound = minBound
        | candidateB == byteA = candidateB + 1
        | otherwise = candidateB
      globalA = fixtureGlobalUniqueId byteA
      globalB = fixtureGlobalUniqueId byteB
      initial = registerInto processB (registered processA)
      (afterA, privateA) =
        localizeOne processA globalA initial
      (state, privateB) = localizeOne processB globalB afterA
   in counterexample
        ("private aliases: " <> show (privateA, privateB))
        ( privateA == privateB
            && resolvePrivateUniqueId processA privateA state == Right (Just globalA)
            && resolvePrivateUniqueId processB privateA state == Right (Just globalB)
            && lookupPrivateUniqueId processA globalB state == Right Nothing
            && lookupPrivateUniqueId processB globalA state == Right Nothing
        )

propUnevenCounters :: Positive Word8 -> Property
propUnevenCounters generatedCount =
  let globalsA =
        fmap
          fixtureGlobalUniqueId
          (take (fromIntegral (getPositive generatedCount)) [minBound .. maxBound])
      (afterA, privateA) = localizeMany processA globalsA initial
      (state, privateB) =
        localizeOne processB (fixtureGlobalUniqueId 0) afterA
   in counterexample
        ("private aliases: " <> show (privateA, privateB))
        ( privateUniqueIdWord64 privateB == 1
            && fmap privateUniqueIdWord64 privateA
              == [1 .. fromIntegral (length privateA)]
            && resolvePrivateUniqueId processB privateB state
              == Right (Just (fixtureGlobalUniqueId 0))
        )
  where
    initial = registerInto processB (registered processA)

propRetirement :: Word8 -> Word8 -> Property
propRetirement byteA byteB =
  let globalA = fixtureGlobalUniqueId byteA
      globalB = fixtureGlobalUniqueId byteB
      initial = registerInto processB (registered processA)
      (afterA, privateA) = localizeOne processA globalA initial
      (liveState, privateB) = localizeOne processB globalB afterA
   in case prepareProcessRetirement processA liveState of
        Left problem -> counterexample ("retirement failed: " <> show problem) False
        Right prepared ->
          let retiredState = commitProcessRetirement prepared
              afterLaterRegistration = registerInto processC retiredState
           in ( resolvePrivateUniqueId processA privateA liveState,
                resolvePrivateUniqueId processA privateA afterLaterRegistration,
                lookupPrivateUniqueId processA globalA afterLaterRegistration,
                leftError (prepareLocalization processA globalA afterLaterRegistration),
                leftError (prepareProcessRegistration processA afterLaterRegistration),
                resolvePrivateUniqueId processB privateB afterLaterRegistration,
                lookupPrivateUniqueId processB globalB afterLaterRegistration,
                lookupPrivateUniqueId processC globalA afterLaterRegistration
              )
                === ( Right (Just globalA),
                      Left ProcessEpochIsRetired,
                      Left ProcessEpochIsRetired,
                      Just ProcessEpochIsRetired,
                      Just ProcessEpochIsRetired,
                      Right (Just globalB),
                      Right (Just privateB),
                      Right Nothing
                    )

casePreparedOutput :: IO ()
casePreparedOutput = do
  let globalIdentity = fixtureGlobalUniqueId 53
      predecessor = registered processA
  case prepareLocalization processA globalIdentity predecessor of
    Left problem -> assertFailure ("localization failed: " <> show problem)
    Right prepared -> do
      let (successor, committed) = commitLocalization prepared
      assertEqual
        "preparing does not install the binding in the predecessor"
        (Right Nothing)
        (lookupPrivateUniqueId processA globalIdentity predecessor)
      assertEqual
        "prepared and committed aliases"
        (preparedPrivateUniqueId prepared)
        committed
      assertEqual
        "commit exposes the installed binding"
        (Right (Just committed))
        (lookupPrivateUniqueId processA globalIdentity successor)

caseGeneratedAllocation :: IO ()
caseGeneratedAllocation = do
  let firstGlobal = fixtureGlobalUniqueId 61
      generatedGlobal = fixtureGlobalUniqueId 62
      (predecessor, firstPrivate) =
        localizeOne processA firstGlobal (registered processA)
  prepared <-
    case preparePrivateUniqueIdAllocation processA predecessor of
      Left problem -> assertFailure ("generated allocation failed: " <> show problem)
      Right value -> pure value
  assertEqual
    "fresh prepared slot follows existing allocation"
    2
    (privateUniqueIdWord64 (preparedAllocatedPrivateUniqueId prepared))
  assertEqual
    "preparation does not install generated global bits"
    (Right Nothing)
    (lookupPrivateUniqueId processA generatedGlobal predecessor)
  let (successor, generatedPrivate) =
        commitPrivateUniqueIdAllocation generatedGlobal prepared
  assertEqual
    "prepared and committed generated private IDs agree"
    (preparedAllocatedPrivateUniqueId prepared)
    generatedPrivate
  assertEqual
    "generated forward binding is installed"
    (Right (Just generatedPrivate))
    (lookupPrivateUniqueId processA generatedGlobal successor)
  assertEqual
    "generated reverse binding is installed"
    (Right (Just generatedGlobal))
    (resolvePrivateUniqueId processA generatedPrivate successor)
  assertEqual
    "existing binding remains intact"
    (Right (Just firstGlobal))
    (resolvePrivateUniqueId processA firstPrivate successor)

localizeMany ::
  ProcessEpochId ->
  [GlobalUniqueId] ->
  PrivateIdentity ->
  (PrivateIdentity, [PrivateUniqueId])
localizeMany process globals initial =
  mapAccumL (flip (localizeOne process)) initial globals

localizeOne ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  (PrivateIdentity, PrivateUniqueId)
localizeOne process globalIdentity state =
  case prepareLocalization process globalIdentity state of
    Left problem -> error ("fixture localization failed: " <> show problem)
    Right prepared -> commitLocalization prepared

fixtureGlobalUniqueId :: Word8 -> GlobalUniqueId
fixtureGlobalUniqueId byte =
  checkedIdentity
    "GlobalUniqueId"
    (mkGlobalUniqueId (ByteString.replicate 32 byte))

processA :: ProcessEpochId
processA =
  checkedIdentity
    "process A"
    (mkProcessEpochId (ByteString.replicate 32 0xa1))

processB :: ProcessEpochId
processB =
  checkedIdentity
    "process B"
    (mkProcessEpochId (ByteString.replicate 32 0xb2))

processC :: ProcessEpochId
processC =
  checkedIdentity
    "process C"
    (mkProcessEpochId (ByteString.replicate 32 0xc3))

checkedIdentity :: (Show error) => String -> Either error value -> value
checkedIdentity description result =
  case result of
    Left problem -> error (description <> " fixture failed: " <> show problem)
    Right value -> value

registered :: ProcessEpochId -> PrivateIdentity
registered process = registerInto process emptyPrivateIdentity

registerInto :: ProcessEpochId -> PrivateIdentity -> PrivateIdentity
registerInto process state =
  case prepareProcessRegistration process state of
    Left problem -> error ("register process failed: " <> show problem)
    Right prepared -> commitProcessRegistration prepared

checkedPrivateUniqueId :: Word8 -> PrivateUniqueId
checkedPrivateUniqueId byte =
  case mkPrivateUniqueId (fromIntegral byte) of
    Left problem -> error ("private-ID fixture failed: " <> show problem)
    Right value -> value

prepareLocalizationError ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  Either PrivateIdentityError ()
prepareLocalizationError process globalIdentity state =
  case prepareLocalization process globalIdentity state of
    Left problem -> Left problem
    Right _ -> Right ()

assertPrivateIdentityError ::
  String ->
  PrivateIdentityError ->
  Either PrivateIdentityError value ->
  IO ()
assertPrivateIdentityError description expected result =
  case result of
    Left actual -> assertEqual description expected actual
    Right _ -> assertFailure (description <> ": transition succeeded")

leftError :: Either error value -> Maybe error
leftError result =
  case result of
    Left problem -> Just problem
    Right _ -> Nothing
