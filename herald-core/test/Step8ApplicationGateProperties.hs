{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Step8ApplicationGateProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Word (Word16, Word64)
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value
  ( ApplicationValue (SortDefinitionValue),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    SortId,
    bootstrapManifestIdBytes,
  )
import Eclips.Herald.Application.RPC qualified as Rpc
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( CandidateApplicationLane,
    HeraldInputBody,
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (HeraldState)
import Eclips.Protocol.Application.Codec qualified as Codec
import Eclips.Protocol.Application.Frame qualified as Frame
import Eclips.Protocol.Application.Types qualified as Protocol
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    initialEffectsAreOracleWatchAndGrace,
  )
import PrimordialTestAccess (conventionalStartupPairs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-8 application protocol gate"
    [ testCase
        "two live clients publish, retry, query dropped replies, and read through frames"
        caseComposedGate,
      testCase
        "malformed and wrong-direction frames admit no application input"
        caseFramePreAdmission,
      testCase
        "state-invalid claims reach Herald and project the typed session rejection"
        caseStatefulClaimRejection,
      testProperty
        "the complete framed application trace replays deterministically"
        propDeterministicGate
    ]

caseStatefulClaimRejection :: Assertion
caseStatefulClaimRejection = case statefulClaimRejectionTrace of
  Left problem -> assertFailure problem
  Right (actualBinding, projected) ->
    assertEqual
      "the decoded foreign session is rejected by stateful Herald admission"
      ( Rpc.RejectEstablishedApplication
          actualBinding
          (Protocol.SessionRejected Protocol.ApplicationRequestSessionMismatchDto)
      )
      projected

caseComposedGate :: Assertion
caseComposedGate = case gateTrace 1 of
  Left problem -> assertFailure problem
  Right summary -> do
    assertEqual
      "the lost write settles with its public SortId"
      ( Client.ApplicationCallSucceeded
          (WriteCompleted (SortDefinitionWritten (writtenSort summary)))
      )
      (recoveredOutcome summary)
    assertEqual "the exact retry reoffered one retained reply" 1 (retryEffectCount summary)
    assertEqual "the read sees the primordial catalogue plus one declaration" 7 (length (readValues summary))
    assertEqual "local take returns the same canonical visible cut" (readValues summary) (localTakeValues summary)
    assertEqual
      "the second client observes the definition written by the first"
      True
      (declaredValue summary `elem` readValues summary)
    assertEqual
      "the dropped wait wake recovers through GetRequestResult"
      (Client.ApplicationCallSucceeded (WaitCompleted WaitReady))
      (recoveredWaitOutcome summary)
    assertEqual "the second wait reaches local cancellation" True (secondWaitCancelled summary)
    assertEqual "both framed session ends are silent" True (sessionsEndedSilently summary)
    assertEqual
      "the deterministic transcript covers every public-seam event category"
      True
      (allGateEventKindsPresent (recordedEvents summary))

propDeterministicGate :: Word16 -> Property
propDeterministicGate seed =
  counterexample diagnostic (first == second && successful first)
  where
    first = gateTrace seed
    second = gateTrace seed
    diagnostic =
      "equal pure Step-8 traces diverged or failed\nfirst: "
        <> show first
        <> "\nsecond: "
        <> show second

    successful (Right _) = True
    successful (Left _) = False

caseFramePreAdmission :: Assertion
caseFramePreAdmission = do
  let wrongDirection =
        Frame.encodeApplicationFrame
          ( Protocol.ApplicationServerEnvelope
              (Protocol.SessionRejected Protocol.ApplicationSessionNotLiveDto)
          )
      malformedFamily = ByteString.pack [0, 0, 0, 4, 0, 0, 0, 0]
      malformedPayload = ByteString.pack [0, 0, 0, 5, 0x45, 0x41, 0x50, 0x50, 0xff]
  assertNoAdmission
    "server envelope on the client-ingress decoder"
    Frame.ApplicationFrameWrongEnvelopeDirection
    wrongDirection
  assertNoAdmission
    "wrong application-family magic"
    Frame.ApplicationFrameWrongFamily
    malformedFamily
  assertNoAdmission
    "unknown generic envelope constructor"
    (Frame.ApplicationFramePayloadError Codec.ApplicationPayloadMalformed)
    malformedPayload
  case zeroReplyCursorFrame of
    Left problem -> assertFailure problem
    Right invalidRefinedFrame ->
      assertNoAdmission
        "zero reply cursor in an otherwise valid ResumeSession"
        (Frame.ApplicationFramePayloadError Codec.ApplicationPayloadMalformed)
        invalidRefinedFrame
  case Frame.feedApplicationFrame
    (Frame.initialApplicationFrameDecoder Frame.ApplicationServerFrames)
    (ByteString.init wrongDirection) of
    Frame.ApplicationFrameFeedResult [] (Frame.NeedApplicationFrameBytes decoder) ->
      assertEqual
        "a truncated frame reaches no DTO and fails only at EOF"
        (Left Frame.ApplicationFrameTruncated)
        (Frame.finishApplicationFrameDecoder decoder)
    other -> assertFailure ("unexpected truncated-frame result: " <> show other)
  assertEqual
    "an empty payload does not construct an application envelope"
    (Left Codec.ApplicationPayloadMalformed)
    (Codec.decodeApplicationEnvelope ByteString.empty)
  case coalescedPrefixFailureTrace of
    Left problem -> assertFailure problem
    Right summary -> do
      assertEqual "both valid prefix frames are decoded once" 2 (decodedPrefixCount summary)
      assertEqual "both valid prefix DTOs reach stepHerald once" 2 (heraldStepCount summary)
      assertEqual "the malformed suffix adds no Herald step" 0 (malformedAdditionalStepCount summary)
      assertEqual
        "coalesced application inputs preserve their runtime lane order"
        [candidateApplicationLane 701, candidateApplicationLane 702]
        (acceptedLaneOrder summary)

assertNoAdmission :: String -> Frame.ApplicationFrameError -> ByteString -> Assertion
assertNoAdmission context expected bytes =
  case Frame.feedApplicationFrame
    (Frame.initialApplicationFrameDecoder Frame.ApplicationClientFrames)
    bytes of
    Frame.ApplicationFrameFeedResult
      admitted
      (Frame.ApplicationFrameFailed actual) -> do
        assertEqual (context <> ": no decoded prefix") [] admitted
        assertEqual (context <> ": terminal boundary failure") expected actual
    other -> assertFailure (context <> ": unexpected decoder result: " <> show other)

zeroReplyCursorFrame :: Either String ByteString
zeroReplyCursorFrame = do
  let claimScope = ByteString.replicate 32 0x5a
  session <-
    checked
      "zero-cursor session claim"
      (Protocol.applicationSessionClaim claimScope 17)
  token <-
    checked
      "zero-cursor resume-token claim"
      (Protocol.applicationResumeTokenClaim claimScope 17)
  cursor <-
    checked
      "positive cursor fixture"
      (Protocol.applicationReplyCursorClaim 1)
  let validFrame =
        Frame.encodeApplicationFrame
          ( Protocol.ApplicationClientEnvelope
              (Protocol.ResumeSession session token cursor)
          )
      cursorByteCount = 8
  require
    "valid ResumeSession frame is shorter than its cursor representation"
    (ByteString.length validFrame >= cursorByteCount)
  pure
    ( ByteString.take (ByteString.length validFrame - cursorByteCount) validFrame
        <> ByteString.replicate cursorByteCount 0
    )

data PrefixFailureSummary = PrefixFailureSummary
  { decodedPrefixCount :: Int,
    heraldStepCount :: Int,
    malformedAdditionalStepCount :: Int,
    acceptedLaneOrder :: [CandidateApplicationLane]
  }
  deriving stock (Eq, Show)

coalescedPrefixFailureTrace :: Either String PrefixFailureSummary
coalescedPrefixFailureTrace = do
  bootstrap <- firstBootstrap
  checkedBootstraps <-
    checked
      "coalesced boundary bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest [bootstrap])
      )
  (initialState, initialEffects) <-
    checked
      "coalesced boundary Herald"
      (initialHerald (monotonicInstant 0) fixtureCheckedGenesis checkedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  require "coalesced boundary initialization must emit one Oracle watch and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace initialEffects)
  attachment <-
    checked
      "coalesced boundary attachment"
      (Protocol.applicationAttachmentClaim (bootstrapManifestIdBytes bootstrap))
  let firstDto = Protocol.OpenSession attachment (Protocol.applicationClientNonce 701)
      secondDto = Protocol.OpenSession attachment (Protocol.applicationClientNonce 702)
      firstEnvelope = Protocol.ApplicationClientEnvelope firstDto
      secondEnvelope = Protocol.ApplicationClientEnvelope secondDto
      wrongFamily = ByteString.pack [0, 0, 0, 4, 0, 0, 0, 0]
      coalesced =
        Frame.encodeApplicationFrame firstEnvelope
          <> Frame.encodeApplicationFrame secondEnvelope
          <> wrongFamily
      initialKernel = Kernel initialState 1
  decoded <-
    case Frame.feedApplicationFrame
      (Frame.initialApplicationFrameDecoder Frame.ApplicationClientFrames)
      coalesced of
      Frame.ApplicationFrameFeedResult
        prefix
        (Frame.ApplicationFrameFailed Frame.ApplicationFrameWrongFamily) ->
          Right prefix
      other -> Left ("coalesced valid-prefix schedule did not fail as expected: " <> show other)
  require
    "coalesced decoder changed or duplicated valid-prefix order"
    (decoded == [firstEnvelope, secondEnvelope])
  firstRouted <-
    routeAdmittedClientDto
      "first coalesced prefix"
      (Rpc.CandidateApplicationIngress (candidateApplicationLane 701))
      firstDto
      initialKernel
  secondRouted <-
    routeAdmittedClientDto
      "second coalesced prefix"
      (Rpc.CandidateApplicationIngress (candidateApplicationLane 702))
      secondDto
      (kernel firstRouted)
  let lanes =
        [ lane
        | Rpc.AcceptApplicationCandidate lane _ _ <- outbound firstRouted <> outbound secondRouted
        ]
      steps = fromIntegral (nextObservedAt (kernel secondRouted) - nextObservedAt initialKernel)
  pure
    PrefixFailureSummary
      { decodedPrefixCount = length decoded,
        heraldStepCount = steps,
        malformedAdditionalStepCount = steps - length decoded,
        acceptedLaneOrder = lanes
      }

statefulClaimRejectionTrace ::
  Either String (ApplicationSessionBinding, Rpc.ApplicationOutbound)
statefulClaimRejectionTrace = do
  bootstrap <- firstBootstrap
  checkedBootstraps <-
    checked
      "stateful-rejection bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest [bootstrap])
      )
  (initialState, initialEffects) <-
    checked
      "stateful-rejection Herald"
      (initialHerald (monotonicInstant 0) fixtureCheckedGenesis checkedBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  require "stateful-rejection initialization must emit one Oracle watch and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace initialEffects)
  attachment <-
    checked
      "stateful-rejection attachment"
      (Protocol.applicationAttachmentClaim (bootstrapManifestIdBytes bootstrap))
  let initialOwner =
        Client.initialApplicationClient
          fixtureClientRecoveryConfiguration
          attachment
          (Protocol.applicationClientNonce 811)
  (openedKernel, openedClient, _, _) <-
    openClient
      "stateful-rejection client"
      (candidateApplicationLane 811)
      initialOwner
      (Kernel initialState 1)
  foreignSession <-
    checked
      "structurally valid foreign session"
      (Protocol.applicationSessionClaim (bootstrapManifestIdBytes bootstrap) 999)
  rejected <-
    routeEstablished
      "foreign session claim"
      True
      (binding openedClient)
      ( Protocol.GetRequestResult
          foreignSession
          (Protocol.applicationRequestIdClaim 0)
      )
      openedKernel
  projected <- exactlyOne "stateful foreign-session rejection" (outbound rejected)
  pure (binding openedClient, projected)

data GateSummary = GateSummary
  { writtenSort :: SortId,
    retryEffectCount :: Int,
    recoveredOutcome :: Client.ApplicationCallOutcome,
    readValues :: [ApplicationValue],
    localTakeValues :: [ApplicationValue],
    declaredValue :: ApplicationValue,
    recoveredWaitOutcome :: Client.ApplicationCallOutcome,
    secondWaitCancelled :: Bool,
    sessionsEndedSilently :: Bool,
    recordedEvents :: [GateEvent]
  }
  deriving stock (Eq, Show)

data GateEvent
  = ClientStateToken Client.ApplicationClientState
  | FrameChunksAdmitted
      Frame.ApplicationFrameDirection
      [ByteString]
      [Protocol.ApplicationEnvelope]
  | TypedHeraldInput HeraldInputBody
  | HeraldOutput [HeraldEffect] [Rpc.ApplicationOutbound]
  | ClientApiEffects [Client.ApplicationClientEffect]
  deriving stock (Eq, Show)

allGateEventKindsPresent :: [GateEvent] -> Bool
allGateEventKindsPresent events =
  and
    [ any isClientState events,
      any isFrameAdmission events,
      any isDroppedFrame events,
      any isTypedInput events,
      any isHeraldOutput events,
      any isClientApi events
    ]
  where
    isClientState (ClientStateToken _) = True
    isClientState _ = False
    isFrameAdmission (FrameChunksAdmitted _ _ (_ : _)) = True
    isFrameAdmission _ = False
    isDroppedFrame (FrameChunksAdmitted _ (_ : _) []) = True
    isDroppedFrame _ = False
    isTypedInput (TypedHeraldInput _) = True
    isTypedInput _ = False
    isHeraldOutput (HeraldOutput _ _) = True
    isHeraldOutput _ = False
    isClientApi (ClientApiEffects _) = True
    isClientApi _ = False

data Kernel = Kernel
  { state :: HeraldState,
    nextObservedAt :: Word64
  }

data LiveClient = LiveClient
  { owner :: Client.ApplicationClientState,
    binding :: ApplicationSessionBinding
  }

data Routed = Routed
  { kernel :: Kernel,
    admittedInput :: HeraldInputBody,
    effects :: [HeraldEffect],
    outbound :: [Rpc.ApplicationOutbound]
  }

gateTrace :: Word16 -> Either String GateSummary
gateTrace seed = do
  bootstrap <- firstBootstrap
  checkedBootstraps <-
    checked
      "checked local bootstrap"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest [bootstrap])
      )
  (initialState, initialEffects) <-
    checked
      "initial Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          checkedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  require "initialization must emit one Oracle watch and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace initialEffects)
  attachment <-
    checked
      "application attachment claim"
      ( Protocol.applicationAttachmentClaim
          (bootstrapManifestIdBytes bootstrap)
      )
  let seedWord = fromIntegral seed
      initialKernel = Kernel initialState 1
      firstOwner =
        Client.initialApplicationClient
          fixtureClientRecoveryConfiguration
          attachment
          (Protocol.applicationClientNonce (seedWord * 16 + 1))
      secondOwner =
        Client.initialApplicationClient
          fixtureClientRecoveryConfiguration
          attachment
          (Protocol.applicationClientNonce (seedWord * 16 + 2))
      firstLane = candidateApplicationLane (seedWord * 16 + 1)
      firstRetryLane = candidateApplicationLane (seedWord * 16 + 2)
      secondLane = candidateApplicationLane (seedWord * 16 + 3)

  (afterFirstOpen, firstClient, firstAccess, firstOpenEvents) <-
    openClientWithLostReply
      "first client unobserved open reply on a live binding"
      firstLane
      firstRetryLane
      firstOwner
      initialKernel

  firstSortAccess <- accessFor Access.SortDefinitionRole firstAccess
  let definition = DeclaredSortDefinition declaredDescriptor Nothing
      writeInvocation = Client.applicationInvocationId 1
      firstQuery =
        ApplicationQuery
          (Set.singleton (Access.predefinedReader firstSortAccess))
          QueryAlways

  (pendingFirstOwner, writeClientEffects) <-
    advanceClient
      "submit declared definition"
      ( Client.submitWrite
          writeInvocation
          (Access.predefinedWriter firstSortAccess)
          (PublishValue (SortDefinitionValue definition))
      )
      (owner firstClient)
  writeDto <- soleSentDto "publish client effect" writeClientEffects
  firstWrite <-
    routeEstablished
      "publish declared definition"
      False
      (binding firstClient)
      writeDto
      afterFirstOpen
  firstReply <-
    soleEstablishedReply
      "publish retained reply"
      (binding firstClient)
      (outbound firstWrite)
  written <- writtenSortFromReply declaredDescriptor firstReply

  writeQueryDto <- queryForCall writeDto
  writeQuery <-
    routeEstablished
      "query dropped publish reply on the live binding"
      True
      (binding firstClient)
      writeQueryDto
      (kernel firstWrite)
  queriedWriteReply <-
    soleEstablishedReply
      "queried publish reply"
      (binding firstClient)
      (outbound writeQuery)
  (activeFirstOwner, recoveryEffects) <-
    deliverServerDto
      "deliver queried publish result"
      True
      queriedWriteReply
      pendingFirstOwner
  recovered <- recoveredCall writeInvocation recoveryEffects
  let expectedWriteOutcome =
        Client.ApplicationCallSucceeded
          (WriteCompleted (SortDefinitionWritten written))
  require
    "the dropped write reply was not recovered from its retained result"
    (recovered == expectedWriteOutcome)
  let activeFirstBinding = binding firstClient

  exactRetry <-
    routeEstablished
      "exact publish retry after recovery"
      True
      activeFirstBinding
      writeDto
      (kernel writeQuery)
  retryReply <-
    soleEstablishedReply
      "exact retry retained reply"
      activeFirstBinding
      (outbound exactRetry)
  require "the exact retry changed its retained server DTO" (retryReply == firstReply)
  require
    "the exact retry emitted semantic work beyond its retained reply"
    (length (effects exactRetry) == 1)

  conflictingDto <- conflictingCall writeDto (ReadApplication firstQuery)
  conflict <-
    routeEstablished
      "conflicting operation under retained request"
      True
      activeFirstBinding
      conflictingDto
      (kernel exactRetry)
  conflictReply <-
    soleEstablishedReply
      "request conflict"
      activeFirstBinding
      (outbound conflict)
  require "the conflicting request was not rejected as RequestConflict" (isRequestConflict conflictReply)
  require "request conflict emitted additional effects" (length (effects conflict) == 1)

  (afterSecondOpen, secondClient, secondAccess, secondOpenEvents) <-
    openClient "second client" secondLane secondOwner (kernel conflict)
  secondSortAccess <- accessFor Access.SortDefinitionRole secondAccess
  let readInvocation = Client.applicationInvocationId 2
      takeInvocation = Client.applicationInvocationId 3
      readQuery =
        ApplicationQuery
          (Set.singleton (Access.predefinedReader secondSortAccess))
          QueryAlways

  (pendingSecondOwner, readClientEffects) <-
    advanceClient
      "submit read from second client"
      (Client.submitRead readInvocation readQuery)
      (owner secondClient)
  readDto <- soleSentDto "read client effect" readClientEffects
  routedRead <-
    routeEstablished
      "read declared definition"
      True
      (binding secondClient)
      readDto
      afterSecondOpen
  readReply <-
    soleEstablishedReply
      "read retained reply"
      (binding secondClient)
      (outbound routedRead)
  (activeSecondOwner, readOutcomeEffects) <-
    deliverServerDto
      "deliver read reply"
      True
      readReply
      pendingSecondOwner
  values <- completedRead readInvocation readOutcomeEffects

  (pendingTakeOwner, takeClientEffects) <-
    advanceClient
      "submit local take"
      (Client.submitLocalTake takeInvocation readQuery)
      activeSecondOwner
  takeDto <- soleSentDto "local-take client effect" takeClientEffects
  routedTake <-
    routeEstablished
      "framed local take"
      True
      (binding secondClient)
      takeDto
      (kernel routedRead)
  takeReply <-
    soleEstablishedReply
      "local-take retained reply"
      (binding secondClient)
      (outbound routedTake)
  (afterTakeOwner, takeOutcomeEffects) <-
    deliverServerDto
      "deliver local-take reply"
      True
      takeReply
      pendingTakeOwner
  taken <- completedTake takeInvocation takeOutcomeEffects
  require "local take did not return the canonical read cut" (taken == values)

  takeRetry <-
    routeEstablished
      "exact local-take retry"
      False
      (binding secondClient)
      takeDto
      (kernel routedTake)
  retainedTakeReply <-
    soleEstablishedReply
      "exact local-take retained reply"
      (binding secondClient)
      (outbound takeRetry)
  require "local-take retry changed its retained result" (retainedTakeReply == takeReply)
  require "local-take retry repeated semantic work" (length (effects takeRetry) == 1)

  let firstWaitInvocation = Client.applicationInvocationId 3
  (submittedWaitOwner, waitClientEffects) <-
    advanceClient
      "submit first pending wait"
      (Client.submitWait firstWaitInvocation [firstQuery])
      activeFirstOwner
  waitDto <- soleSentDto "first wait client effect" waitClientEffects
  routedWait <-
    routeEstablished
      "admit first pending wait"
      True
      activeFirstBinding
      waitDto
      (kernel takeRetry)
  waitReply <-
    soleEstablishedReply
      "first wait acceptance"
      activeFirstBinding
      (outbound routedWait)
  firstWait <- waitClaimFromReply waitReply
  (waitingOwner, waitAcceptanceEffects) <-
    deliverServerDto
      "deliver first wait acceptance"
      True
      waitReply
      submittedWaitOwner
  require "pending wait completed at acceptance" (null waitAcceptanceEffects)

  getResultDto <- queryForCall waitDto
  pendingWaitQuery <-
    routeEstablished
      "query the still-pending wait on its live binding"
      True
      activeFirstBinding
      getResultDto
      (kernel routedWait)
  pendingWaitReply <-
    soleEstablishedReply
      "pending wait query reply"
      activeFirstBinding
      (outbound pendingWaitQuery)
  require "the pending query changed the retained acceptance" (pendingWaitReply == waitReply)

  let triggerInvocation = Client.applicationInvocationId 4
      triggerDefinition = DeclaredSortDefinition triggerDescriptor Nothing
  (pendingTriggerOwner, triggerEffects) <-
    advanceClient
      "submit wait-triggering definition"
      ( Client.submitWrite
          triggerInvocation
          (Access.predefinedWriter secondSortAccess)
          (PublishValue (SortDefinitionValue triggerDefinition))
      )
      afterTakeOwner
  triggerDto <- soleSentDto "wait-triggering client effect" triggerEffects
  triggered <-
    routeEstablished
      "publish wait-triggering definition"
      True
      (binding secondClient)
      triggerDto
      (kernel pendingWaitQuery)
  triggerReply <-
    exactlyOne
      "triggering write reply"
      (establishedDtosFor (binding secondClient) (outbound triggered))
  triggeringSort <- writtenSortFromReply triggerDescriptor triggerReply
  require "the triggering definition reused the first SortId" (triggeringSort /= written)
  droppedWake <-
    exactlyOne
      "dropped wait wake"
      (establishedDtosFor activeFirstBinding (outbound triggered))
  require "the triggering write did not emit the expected wait wake" (isWaitWake firstWait droppedWake)
  (afterTriggerOwner, triggerOutcomeEffects) <-
    deliverServerDto
      "deliver triggering write reply"
      True
      triggerReply
      pendingTriggerOwner

  recoveredWait <-
    routeEstablished
      "recover completed wait"
      True
      activeFirstBinding
      getResultDto
      (kernel triggered)
  completedWaitReply <-
    soleEstablishedReply
      "completed wait retained reply"
      activeFirstBinding
      (outbound recoveredWait)
  (afterFirstWaitOwner, completedWaitEffects) <-
    deliverServerDto
      "deliver recovered wait completion"
      True
      completedWaitReply
      waitingOwner
  recoveredWaitResult <- recoveredCall firstWaitInvocation completedWaitEffects

  let clearingTakeInvocation = Client.applicationInvocationId 5
  (pendingClearingOwner, clearingEffects) <-
    advanceClient
      "submit take before cancellable wait"
      (Client.submitLocalTake clearingTakeInvocation readQuery)
      afterTriggerOwner
  clearingDto <- soleSentDto "clearing take DTO" clearingEffects
  cleared <-
    routeEstablished
      "clear triggering definition"
      True
      (binding secondClient)
      clearingDto
      (kernel recoveredWait)
  clearingReply <-
    soleEstablishedReply
      "clearing take reply"
      (binding secondClient)
      (outbound cleared)
  (afterClearingOwner, clearingOutcomeEffects) <-
    deliverServerDto
      "deliver clearing take reply"
      True
      clearingReply
      pendingClearingOwner
  clearedValues <- completedTake clearingTakeInvocation clearingOutcomeEffects
  require "clearing take did not observe the triggering definition" (not (null clearedValues))

  let secondWaitInvocation = Client.applicationInvocationId 4
  (submittedSecondWaitOwner, secondWaitEffects) <-
    advanceClient
      "submit cancellable wait"
      (Client.submitWait secondWaitInvocation [firstQuery])
      afterFirstWaitOwner
  secondWaitDto <- soleSentDto "second wait DTO" secondWaitEffects
  routedSecondWait <-
    routeEstablished
      "admit cancellable wait"
      True
      activeFirstBinding
      secondWaitDto
      (kernel cleared)
  secondWaitReply <-
    soleEstablishedReply
      "second wait acceptance"
      activeFirstBinding
      (outbound routedSecondWait)
  secondWait <- waitClaimFromReply secondWaitReply
  (acceptedSecondWaitOwner, secondWaitAcceptanceEffects) <-
    deliverServerDto
      "deliver cancellable wait acceptance"
      True
      secondWaitReply
      submittedSecondWaitOwner
  require "second wait completed before cancellation" (null secondWaitAcceptanceEffects)
  (cancellingOwner, cancelEffects) <-
    advanceClient "cancel second wait" (Client.Cancel secondWaitInvocation) acceptedSecondWaitOwner
  cancelDto <- soleSentDto "wait cancellation DTO" cancelEffects
  require "client cancellation did not preserve the learned wait claim" (cancelNamesWait secondWait cancelDto)
  cancelled <-
    routeEstablished
      "route wait cancellation"
      True
      activeFirstBinding
      cancelDto
      (kernel routedSecondWait)
  cancelReply <-
    soleEstablishedReply
      "wait cancellation reply"
      activeFirstBinding
      (outbound cancelled)
  (afterCancelOwner, cancellationOutcomeEffects) <-
    deliverServerDto
      "deliver wait cancellation"
      True
      cancelReply
      cancellingOwner
  let cancellationObserved = cancellationOutcomeEffects == [Client.CallCancelled secondWaitInvocation]
  require "second wait did not settle as cancelled" cancellationObserved

  (closedFirstOwner, firstEndEffects) <-
    advanceClient "end first session" Client.End afterCancelOwner
  firstEndDto <- soleSentDtoAmong "first EndSession DTO" firstEndEffects
  endedFirst <-
    routeEstablished
      "route first EndSession"
      True
      activeFirstBinding
      firstEndDto
      (kernel cancelled)
  require "first EndSession was not silent" (null (effects endedFirst) && null (outbound endedFirst))

  (closedSecondOwner, secondEndEffects) <-
    advanceClient "end second session" Client.End afterClearingOwner
  secondEndDto <- soleSentDtoAmong "second EndSession DTO" secondEndEffects
  endedSecond <-
    routeEstablished
      "route second EndSession"
      True
      (binding secondClient)
      secondEndDto
      (kernel endedFirst)
  require "second EndSession was not silent" (null (effects endedSecond) && null (outbound endedSecond))
  let bothClosed =
        Client.applicationClientPhase closedFirstOwner == Client.ApplicationClientClosed
          && Client.applicationClientPhase closedSecondOwner == Client.ApplicationClientClosed
  require "client owners did not close after End" bothClosed

  let traceEvents =
        firstOpenEvents
          <> clientTransitionEvents pendingFirstOwner writeClientEffects
          <> [clientFrameEvent False writeDto]
          <> routedEvents firstWrite
          <> [droppedServerFrameEvent False firstReply]
          <> [clientFrameEvent True writeQueryDto]
          <> routedEvents writeQuery
          <> [serverFrameEvent True queriedWriteReply]
          <> clientTransitionEvents activeFirstOwner recoveryEffects
          <> [clientFrameEvent True writeDto]
          <> routedEvents exactRetry
          <> [serverFrameEvent True retryReply]
          <> [clientFrameEvent True conflictingDto]
          <> routedEvents conflict
          <> [serverFrameEvent True conflictReply]
          <> secondOpenEvents
          <> clientTransitionEvents pendingSecondOwner readClientEffects
          <> [clientFrameEvent True readDto]
          <> routedEvents routedRead
          <> [serverFrameEvent True readReply]
          <> clientTransitionEvents activeSecondOwner readOutcomeEffects
          <> clientTransitionEvents pendingTakeOwner takeClientEffects
          <> [clientFrameEvent True takeDto]
          <> routedEvents routedTake
          <> [serverFrameEvent True takeReply]
          <> clientTransitionEvents afterTakeOwner takeOutcomeEffects
          <> [clientFrameEvent False takeDto]
          <> routedEvents takeRetry
          <> [serverFrameEvent False retainedTakeReply]
          <> clientTransitionEvents submittedWaitOwner waitClientEffects
          <> [clientFrameEvent True waitDto]
          <> routedEvents routedWait
          <> [serverFrameEvent True waitReply]
          <> clientTransitionEvents waitingOwner waitAcceptanceEffects
          <> [clientFrameEvent True getResultDto]
          <> routedEvents pendingWaitQuery
          <> [droppedServerFrameEvent True pendingWaitReply]
          <> clientTransitionEvents pendingTriggerOwner triggerEffects
          <> [clientFrameEvent True triggerDto]
          <> routedEvents triggered
          <> [ serverFrameEvent True triggerReply,
               droppedServerFrameEvent True droppedWake
             ]
          <> clientTransitionEvents afterTriggerOwner triggerOutcomeEffects
          <> [clientFrameEvent True getResultDto]
          <> routedEvents recoveredWait
          <> [serverFrameEvent True completedWaitReply]
          <> clientTransitionEvents afterFirstWaitOwner completedWaitEffects
          <> clientTransitionEvents pendingClearingOwner clearingEffects
          <> [clientFrameEvent True clearingDto]
          <> routedEvents cleared
          <> [serverFrameEvent True clearingReply]
          <> clientTransitionEvents afterClearingOwner clearingOutcomeEffects
          <> clientTransitionEvents submittedSecondWaitOwner secondWaitEffects
          <> [clientFrameEvent True secondWaitDto]
          <> routedEvents routedSecondWait
          <> [serverFrameEvent True secondWaitReply]
          <> clientTransitionEvents acceptedSecondWaitOwner secondWaitAcceptanceEffects
          <> clientTransitionEvents cancellingOwner cancelEffects
          <> [clientFrameEvent True cancelDto]
          <> routedEvents cancelled
          <> [serverFrameEvent True cancelReply]
          <> clientTransitionEvents afterCancelOwner cancellationOutcomeEffects
          <> clientTransitionEvents closedFirstOwner firstEndEffects
          <> [clientFrameEvent True firstEndDto]
          <> routedEvents endedFirst
          <> clientTransitionEvents closedSecondOwner secondEndEffects
          <> [clientFrameEvent True secondEndDto]
          <> routedEvents endedSecond
      presented =
        SortDefinitionValue
          (DeclaredSortDefinition declaredDescriptor (Just written))
  require "the second client did not read the declared definition" (presented `elem` values)
  pure
    GateSummary
      { writtenSort = written,
        retryEffectCount = length (effects exactRetry),
        recoveredOutcome = recovered,
        readValues = values,
        localTakeValues = taken,
        declaredValue = presented,
        recoveredWaitOutcome = recoveredWaitResult,
        secondWaitCancelled = cancellationObserved,
        sessionsEndedSilently = bothClosed,
        recordedEvents = traceEvents
      }

openClientWithLostReply ::
  String ->
  CandidateApplicationLane ->
  CandidateApplicationLane ->
  Client.ApplicationClientState ->
  Kernel ->
  Either String (Kernel, LiveClient, Access.ApplicationStartupAccess, [GateEvent])
openClientWithLostReply context firstLane retryLane initialOwner initialKernel = do
  (started, startEffects) <- advanceClient (context <> ": start") Client.Start initialOwner
  require (context <> ": start did not request transport") (isSoleTransportRequest startEffects)
  (awaitingFirstReply, firstTransportEffects) <-
    advanceClient (context <> ": first transport") CurrentTransportAvailable started
  firstOpenDto <- soleSentDto (context <> ": first OpenSession") firstTransportEffects
  firstOpen <- routeCandidate (context <> ": first open") True firstLane firstOpenDto initialKernel
  (firstBinding, droppedOpened) <-
    soleCandidateAcceptance (context <> ": dropped SessionOpened") firstLane (outbound firstOpen)
  -- The first result is unobserved, but its admitted binding stays live.
  -- This is exact Open retry coverage, not established-connection recovery.
  let retriedOpenDto = firstOpenDto
  retried <- routeCandidate (context <> ": exact live Open retry") True retryLane retriedOpenDto (kernel firstOpen)
  (retryBinding, reofferedOpened) <-
    soleCandidateAcceptance (context <> ": retried SessionOpened") retryLane (outbound retried)
  require (context <> ": open retry changed the session binding") (retryBinding == firstBinding)
  require (context <> ": open retry changed session/token/cursor/access") (reofferedOpened == droppedOpened)
  (activeOwner, openingEffects) <-
    deliverServerDto (context <> ": deliver retried open") True reofferedOpened awaitingFirstReply
  access <- case openingEffects of
    [Client.SessionReady startup] -> Right startup
    actual -> Left (context <> ": unexpected opening effects: " <> show actual)
  pure
    ( kernel retried,
      LiveClient activeOwner retryBinding,
      access,
      [ClientStateToken initialOwner]
        <> clientTransitionEvents started startEffects
        <> clientTransitionEvents awaitingFirstReply firstTransportEffects
        <> [clientFrameEvent True firstOpenDto]
        <> routedEvents firstOpen
        <> [droppedServerFrameEvent True droppedOpened]
        <> [clientFrameEvent True retriedOpenDto]
        <> routedEvents retried
        <> [serverFrameEvent True reofferedOpened]
        <> clientTransitionEvents activeOwner openingEffects
    )

openClient ::
  String ->
  CandidateApplicationLane ->
  Client.ApplicationClientState ->
  Kernel ->
  Either String (Kernel, LiveClient, Access.ApplicationStartupAccess, [GateEvent])
openClient context lane initialOwner initialKernel = do
  (started, startEffects) <- advanceClient (context <> ": start") Client.Start initialOwner
  require
    (context <> ": start did not request transport")
    (isSoleTransportRequest startEffects)
  (awaitingReply, transportEffects) <-
    advanceClient (context <> ": transport available") CurrentTransportAvailable started
  openDto <- soleSentDto (context <> ": open DTO") transportEffects
  routed <- routeCandidate (context <> ": route open") True lane openDto initialKernel
  (openedBinding, openedDto) <-
    soleCandidateAcceptance (context <> ": open acceptance") lane (outbound routed)
  (activeOwner, openingEffects) <-
    deliverServerDto (context <> ": deliver open") True openedDto awaitingReply
  access <- case openingEffects of
    [Client.SessionReady startup] -> Right startup
    actual -> Left (context <> ": unexpected opening effects: " <> show actual)
  pure
    ( kernel routed,
      LiveClient activeOwner openedBinding,
      access,
      [ClientStateToken initialOwner]
        <> clientTransitionEvents started startEffects
        <> clientTransitionEvents awaitingReply transportEffects
        <> [clientFrameEvent True openDto]
        <> routedEvents routed
        <> [serverFrameEvent True openedDto]
        <> clientTransitionEvents activeOwner openingEffects
    )

routeCandidate ::
  String ->
  Bool ->
  CandidateApplicationLane ->
  Protocol.ApplicationClientDto ->
  Kernel ->
  Either String Routed
routeCandidate context fragmented lane =
  routeClientDto context fragmented (Rpc.CandidateApplicationIngress lane)

routeEstablished ::
  String ->
  Bool ->
  ApplicationSessionBinding ->
  Protocol.ApplicationClientDto ->
  Kernel ->
  Either String Routed
routeEstablished context fragmented established =
  routeClientDto
    context
    fragmented
    (Rpc.EstablishedApplicationIngress established)

routeClientDto ::
  String ->
  Bool ->
  Rpc.ApplicationIngressContext ->
  Protocol.ApplicationClientDto ->
  Kernel ->
  Either String Routed
routeClientDto context fragmented ingress dto predecessor = do
  transported <-
    transportEnvelope
      (context <> ": client frame")
      fragmented
      Frame.ApplicationClientFrames
      (Protocol.ApplicationClientEnvelope dto)
  admittedDto <- case transported of
    Protocol.ApplicationClientEnvelope decoded -> Right decoded
    other -> Left (context <> ": wrong transported envelope: " <> show other)
  routeAdmittedClientDto context ingress admittedDto predecessor

routeAdmittedClientDto ::
  String ->
  Rpc.ApplicationIngressContext ->
  Protocol.ApplicationClientDto ->
  Kernel ->
  Either String Routed
routeAdmittedClientDto context ingress admittedDto predecessor = do
  body <- checked (context <> ": RPC admission") (Rpc.admitApplicationClientDto ingress admittedDto)
  (successor, batch) <-
    checked
      (context <> ": Herald transition")
      ( verifiedStepHerald
          (heraldInput (monotonicInstant (nextObservedAt predecessor)) body)
          (state predecessor)
      )
  projected <-
    checked
      (context <> ": RPC projection")
      (traverse Rpc.projectApplicationEffect (effectBatchMembers batch))
  pure
    Routed
      { kernel =
          Kernel
            { state = successor,
              nextObservedAt = nextObservedAt predecessor + 1
            },
        admittedInput = body,
        effects = effectBatchMembers batch,
        outbound = catMaybes projected
      }

deliverServerDto ::
  String ->
  Bool ->
  Protocol.ApplicationServerDto ->
  Client.ApplicationClientState ->
  Either String (Client.ApplicationClientState, [Client.ApplicationClientEffect])
deliverServerDto context fragmented dto owner = do
  transported <-
    transportEnvelope
      (context <> ": server frame")
      fragmented
      Frame.ApplicationServerFrames
      (Protocol.ApplicationServerEnvelope dto)
  decoded <- case transported of
    Protocol.ApplicationServerEnvelope serverDto -> Right serverDto
    other -> Left (context <> ": wrong transported envelope: " <> show other)
  advanceClient context (CurrentServerDto decoded) owner

transportEnvelope ::
  String ->
  Bool ->
  Frame.ApplicationFrameDirection ->
  Protocol.ApplicationEnvelope ->
  Either String Protocol.ApplicationEnvelope
transportEnvelope context fragmented direction envelope = do
  let payload = Codec.encodeApplicationEnvelope envelope
  payloadEnvelope <- checked (context <> ": payload decode") (Codec.decodeApplicationEnvelope payload)
  require (context <> ": payload round-trip changed DTO") (payloadEnvelope == envelope)
  let framed = Frame.encodeApplicationFrame envelope
      chunks = if fragmented then fragment framed else [framed]
  decoded <- decodeChunks context direction chunks
  exactlyOne (context <> ": decoded frame") decoded

clientFrameEvent :: Bool -> Protocol.ApplicationClientDto -> GateEvent
clientFrameEvent fragmented dto =
  admittedFrameEvent
    Frame.ApplicationClientFrames
    fragmented
    (Protocol.ApplicationClientEnvelope dto)

serverFrameEvent :: Bool -> Protocol.ApplicationServerDto -> GateEvent
serverFrameEvent fragmented dto =
  admittedFrameEvent
    Frame.ApplicationServerFrames
    fragmented
    (Protocol.ApplicationServerEnvelope dto)

droppedServerFrameEvent :: Bool -> Protocol.ApplicationServerDto -> GateEvent
droppedServerFrameEvent fragmented dto =
  frameEvent
    Frame.ApplicationServerFrames
    fragmented
    (Protocol.ApplicationServerEnvelope dto)
    []

admittedFrameEvent ::
  Frame.ApplicationFrameDirection ->
  Bool ->
  Protocol.ApplicationEnvelope ->
  GateEvent
admittedFrameEvent direction fragmented envelope =
  frameEvent direction fragmented envelope [envelope]

frameEvent ::
  Frame.ApplicationFrameDirection ->
  Bool ->
  Protocol.ApplicationEnvelope ->
  [Protocol.ApplicationEnvelope] ->
  GateEvent
frameEvent direction fragmented envelope admitted =
  FrameChunksAdmitted direction chunks admitted
  where
    frame = Frame.encodeApplicationFrame envelope
    chunks = if fragmented then fragment frame else [frame]

routedEvents :: Routed -> [GateEvent]
routedEvents routed =
  [ TypedHeraldInput (admittedInput routed),
    HeraldOutput (effects routed) (outbound routed)
  ]

clientTransitionEvents ::
  Client.ApplicationClientState ->
  [Client.ApplicationClientEffect] ->
  [GateEvent]
clientTransitionEvents successor transitionEffects =
  [ ClientStateToken successor,
    ClientApiEffects transitionEffects
  ]

decodeChunks ::
  String ->
  Frame.ApplicationFrameDirection ->
  [ByteString] ->
  Either String [Protocol.ApplicationEnvelope]
decodeChunks context direction =
  go (Frame.initialApplicationFrameDecoder direction) []
  where
    go decoder admitted [] = do
      checked (context <> ": finish frame") (Frame.finishApplicationFrameDecoder decoder)
      Right admitted
    go decoder admitted (chunk : rest) =
      case Frame.feedApplicationFrame decoder chunk of
        Frame.ApplicationFrameFeedResult
          decoded
          (Frame.NeedApplicationFrameBytes continuation) ->
            go continuation (admitted <> decoded) rest
        Frame.ApplicationFrameFeedResult decoded (Frame.ApplicationFrameFailed problem) ->
          Left
            ( context
                <> ": frame failed after "
                <> show (length (admitted <> decoded))
                <> " admissions: "
                <> show problem
            )

fragment :: ByteString -> [ByteString]
fragment bytes = filter (not . ByteString.null) [first, second, remaining]
  where
    (first, afterFirst) = ByteString.splitAt 1 bytes
    (second, remaining) = ByteString.splitAt 3 afterFirst

data CurrentTransportAvailable = CurrentTransportAvailable

newtype CurrentServerDto = CurrentServerDto Protocol.ApplicationServerDto

class GateClientInput input where
  resolveGateClientInput ::
    String ->
    input ->
    Client.ApplicationClientState ->
    Either String Client.ApplicationClientInput

instance GateClientInput Client.ApplicationClientInput where
  resolveGateClientInput _ input _ = Right input

instance GateClientInput CurrentTransportAvailable where
  resolveGateClientInput context _ owner = do
    attempt <- currentClientAttempt context owner
    Right (Client.TransportAvailable attempt fixtureClientObservedAt)

instance GateClientInput CurrentServerDto where
  resolveGateClientInput context (CurrentServerDto dto) owner = do
    attempt <- currentClientAttempt context owner
    Right (Client.ServerDtoReceived attempt fixtureClientObservedAt dto)

advanceClient ::
  (GateClientInput input) =>
  String ->
  input ->
  Client.ApplicationClientState ->
  Either String (Client.ApplicationClientState, [Client.ApplicationClientEffect])
advanceClient context abstractInput predecessor = do
  input <- resolveGateClientInput context abstractInput predecessor
  case Client.stepApplicationClient input predecessor of
    Left fault -> Left (context <> ": client invariant fault: " <> show fault)
    Right (Client.ApplicationClientCommandRejected rejection) ->
      Left (context <> ": client command rejected: " <> show rejection)
    Right (Client.ApplicationClientAdvanced successor batch) ->
      advancePacedRetry context successor (Client.applicationClientEffects batch)

advancePacedRetry ::
  String ->
  Client.ApplicationClientState ->
  [Client.ApplicationClientEffect] ->
  Either String (Client.ApplicationClientState, [Client.ApplicationClientEffect])
advancePacedRetry context successor effects =
  case [ (scope, attempt, retryAt)
       | Client.ArmTransportRetry scope attempt retryAt <- effects
       ] of
    [] -> Right (successor, visibleGateClientEffects effects)
    [(scope, attempt, retryAt)] ->
      case Client.stepApplicationClient
        (Client.TransportRetryElapsed scope attempt retryAt)
        successor of
        Left fault -> Left (context <> ": paced retry invariant fault: " <> show fault)
        Right (Client.ApplicationClientCommandRejected rejection) ->
          Left (context <> ": paced retry rejected: " <> show rejection)
        Right (Client.ApplicationClientAdvanced retried retryBatch) ->
          Right
            ( retried,
              visibleGateClientEffects
                (effects <> Client.applicationClientEffects retryBatch)
            )
    actual -> Left (context <> ": multiple retry timers armed: " <> show actual)

visibleGateClientEffects ::
  [Client.ApplicationClientEffect] ->
  [Client.ApplicationClientEffect]
visibleGateClientEffects = filter $ \case
  Client.ArmTransportRetry {} -> False
  Client.CancelTransportRetry {} -> False
  Client.ArmRecoveryDeadline {} -> False
  Client.CancelRecoveryDeadline {} -> False
  Client.SessionEstablishedOnTransport {} -> False
  Client.ScheduleReceiptRetirement {} -> False
  _ -> True

currentClientAttempt ::
  String ->
  Client.ApplicationClientState ->
  Either String Client.ApplicationTransportAttemptGeneration
currentClientAttempt context owner =
  maybe
    (Left (context <> ": client has no current transport attempt"))
    Right
    (Client.applicationClientCurrentTransportAttempt owner)

isSoleTransportRequest :: [Client.ApplicationClientEffect] -> Bool
isSoleTransportRequest = \case
  [Client.RequestTransport _] -> True
  _ -> False

fixtureClientObservedAt :: Client.ApplicationClientMonotonicInstant
fixtureClientObservedAt = Client.applicationClientMonotonicInstant 100

fixtureClientRecoveryConfiguration :: Client.ApplicationClientRecoveryConfiguration
fixtureClientRecoveryConfiguration =
  case Client.applicationClientRecoveryConfiguration 10 1_000_000 of
    Left problem -> error ("invalid Step-8 client recovery fixture: " <> show problem)
    Right configuration -> configuration

soleSentDto ::
  String ->
  [Client.ApplicationClientEffect] ->
  Either String Protocol.ApplicationClientDto
soleSentDto context effects = case effects of
  [Client.SendApplicationDto dto] -> Right dto
  actual -> Left (context <> ": expected one send effect, got " <> show actual)

soleSentDtoAmong ::
  String ->
  [Client.ApplicationClientEffect] ->
  Either String Protocol.ApplicationClientDto
soleSentDtoAmong context effects =
  exactlyOne
    context
    [dto | Client.SendApplicationDto dto <- effects]

soleCandidateAcceptance ::
  String ->
  CandidateApplicationLane ->
  [Rpc.ApplicationOutbound] ->
  Either String (ApplicationSessionBinding, Protocol.ApplicationServerDto)
soleCandidateAcceptance context expectedLane outbound = case outbound of
  [Rpc.AcceptApplicationCandidate actualLane acceptedBinding dto]
    | actualLane == expectedLane -> Right (acceptedBinding, dto)
  actual -> Left (context <> ": unexpected projected output: " <> show actual)

soleEstablishedReply ::
  String ->
  ApplicationSessionBinding ->
  [Rpc.ApplicationOutbound] ->
  Either String Protocol.ApplicationServerDto
soleEstablishedReply context expectedBinding outbound = case filter (not . receiptConfirmation) outbound of
  [Rpc.SendEstablishedApplication actualBinding dto]
    | actualBinding == expectedBinding -> Right dto
  actual -> Left (context <> ": unexpected projected output: " <> show actual)

establishedDtosFor ::
  ApplicationSessionBinding ->
  [Rpc.ApplicationOutbound] ->
  [Protocol.ApplicationServerDto]
establishedDtosFor expectedBinding outbound =
  [ dto
  | Rpc.SendEstablishedApplication actualBinding dto <- outbound,
    actualBinding == expectedBinding,
    not (isReceiptConfirmation dto)
  ]

-- Confirmations are independently replayable metadata. This trace leaves them
-- undelivered and exercises their repeated monotone piggyback on later calls.
receiptConfirmation :: Rpc.ApplicationOutbound -> Bool
receiptConfirmation = \case
  Rpc.SendEstablishedApplication _ dto -> isReceiptConfirmation dto
  _ -> False

isReceiptConfirmation :: Protocol.ApplicationServerDto -> Bool
isReceiptConfirmation = \case
  Protocol.ReceiptsRetired {} -> True
  _ -> False

queryForCall :: Protocol.ApplicationClientDto -> Either String Protocol.ApplicationClientDto
queryForCall = \case
  Protocol.Call session request _ -> Right (Protocol.GetRequestResult session request)
  Protocol.CallWithRetirement session request _ _ -> Right (Protocol.GetRequestResult session request)
  other -> Left ("expected Call DTO to query, got " <> show other)

conflictingCall ::
  Protocol.ApplicationClientDto ->
  ApplicationOperation ->
  Either String Protocol.ApplicationClientDto
conflictingCall original replacement = case original of
  Protocol.Call session request _ -> Right (Protocol.Call session request replacement)
  Protocol.CallWithRetirement session request _ progress -> Right (Protocol.CallWithRetirement session request replacement progress)
  other -> Left ("expected retained Call DTO, got " <> show other)

isRequestConflict :: Protocol.ApplicationServerDto -> Bool
isRequestConflict (Protocol.RequestConflict _ _) = True
isRequestConflict _ = False

waitClaimFromReply ::
  Protocol.ApplicationServerDto ->
  Either String Protocol.ApplicationWaitClaim
waitClaimFromReply = \case
  Protocol.RequestRetained _ _ _ (Protocol.WaitAccepted wait) -> Right wait
  other -> Left ("unexpected wait acceptance: " <> show other)

isWaitWake :: Protocol.ApplicationWaitClaim -> Protocol.ApplicationServerDto -> Bool
isWaitWake expectedWait = \case
  Protocol.WaitWake _ _ _ actualWait WaitReady -> actualWait == expectedWait
  _ -> False

cancelNamesWait :: Protocol.ApplicationWaitClaim -> Protocol.ApplicationClientDto -> Bool
cancelNamesWait expectedWait = \case
  Protocol.CancelPendingWait _ _ actualWait -> actualWait == expectedWait
  _ -> False

writtenSortFromReply ::
  ApplicationSortDescriptor ->
  Protocol.ApplicationServerDto ->
  Either String SortId
writtenSortFromReply descriptor = \case
  Protocol.RequestRetained
    _
    _
    _
    (Protocol.Completed (WriteCompleted (SortDefinitionWritten actual))) -> do
      expected <-
        admittedApplicationSortId
          <$> checked
            "admitted written sort"
            ( admitApplicationSortDefinition
                (DeclaredSortDefinition descriptor Nothing)
            )
      if actual == expected
        then Right actual
        else Left ("publish reply carried the wrong SortId: " <> show actual)
  other -> Left ("unexpected publish reply: " <> show other)

recoveredCall ::
  Client.ApplicationInvocationId ->
  [Client.ApplicationClientEffect] ->
  Either String Client.ApplicationCallOutcome
recoveredCall expectedInvocation = \case
  [Client.CallFinished actualInvocation outcome]
    | actualInvocation == expectedInvocation -> Right outcome
  other -> Left ("unexpected recovered call effects: " <> show other)

completedRead ::
  Client.ApplicationInvocationId ->
  [Client.ApplicationClientEffect] ->
  Either String [ApplicationValue]
completedRead expectedInvocation = \case
  [ Client.CallFinished
      actualInvocation
      (Client.ApplicationCallSucceeded (ReadCompleted values))
    ]
      | actualInvocation == expectedInvocation -> Right values
  other -> Left ("unexpected read effects: " <> show other)

completedTake ::
  Client.ApplicationInvocationId ->
  [Client.ApplicationClientEffect] ->
  Either String [ApplicationValue]
completedTake expectedInvocation = \case
  [ Client.CallFinished
      actualInvocation
      (Client.ApplicationCallSucceeded (LocalTakeCompleted values))
    ]
      | actualInvocation == expectedInvocation -> Right values
  other -> Left ("unexpected local-take effects: " <> show other)

accessFor ::
  Access.ApplicationPredefinedSortRole ->
  Access.ApplicationStartupAccess ->
  Either String Access.PredefinedAccess
accessFor role access =
  maybe
    (Left ("missing predefined startup access: " <> show role))
    Right
    ( find
        ((== role) . Access.predefinedAccessRole)
        (conventionalStartupPairs access)
    )

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

triggerDescriptor :: ApplicationSortDescriptor
triggerDescriptor =
  declaredDescriptor
    { minimumRetentionMicros = 1
    }

firstBootstrap :: Either String BootstrapManifestId
firstBootstrap = case fixtureLocalBootstrapIds of
  first : _ -> Right first
  [] -> Left "fixture has no local bootstrap"

exactlyOne :: String -> [value] -> Either String value
exactlyOne _ [value] = Right value
exactlyOne context values =
  Left (context <> ": expected one value, got " <> show (length values))

checked :: (Show problem) => String -> Either problem value -> Either String value
checked context = either (Left . ((context <> ": ") <>) . show) Right

require :: String -> Bool -> Either String ()
require _ True = Right ()
require problem False = Left problem
