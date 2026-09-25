{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step11GeneratedIdentityProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    ApplicationStartupAccess,
    predefinedAccessRole,
    predefinedWriter,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateUniqueId,
    asPrivateNablaId,
    asPrivateObjectId,
    mkPrivateUniqueId,
    privateNablaUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result (RegularCallResult (..))
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
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    GlobalUniqueId,
    ProcessEpochId,
    globalObjectIdFromGlobalUniqueId,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Request
  ( ApplicationOperation (..),
    ApplicationRequestReply (..),
    RetainedRequestReplyBody (..),
    requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortId,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (PrimordialProcessManifest),
    checkInitialBootstraps,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    ApplicationSessionIngress (OpenApplicationSession),
    HeraldInputBody (..),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupLastObservedTime,
    startupAdministrationState,
    startupApplicationState,
    startupControlledState,
    startupDiscoveryState,
    startupGraphState,
    startupIdGeneratorState,
    startupLastObservedTime,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStateWitness,
    startupStoreState,
    startupWaitState,
    startupWitnessNextGeneratorCounter,
  )
import Eclips.Herald.Time (monotonicInstant)
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
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    ioProperty,
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-11 generated identity composition"
    [ testCase
        "bare, controlled, retry, rejection, delete, and sort write compose through the top level"
        pureOwnerScript,
      testCase
        "controlled and delete rejection branches preserve every non-request owner"
        rejectionBranches,
      testCase
        "session retirement preserves reserved and consumed semantic facts"
        reservationSessionRetirement,
      testProperty
        "mixed first, retry, and conflict ordering follows one generator reference model"
        propMixedOrdering
    ]

data Opened = Opened
  { openedSession :: ApplicationSessionId,
    openedBinding :: ApplicationSessionBinding,
    openedAccess :: ApplicationStartupAccess
  }

reservationSessionRetirement :: Assertion
reservationSessionRetirement = do
  bootstraps <-
    checkedIO
      "check reservation-retirement bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest fixtureLocalBootstrapIds)
      )
  (bootstrapP, _) <- localBootstrapPair
  attachmentP <- attachment bootstraps bootstrapP
  (initial, _) <-
    checkedIO
      "initialize reservation-retirement Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  processP <- attachmentProcess "retirement process" attachmentP initial
  (openedState, opened) <- open 1 601 1 attachmentP initial
  writer <- writerFor Access.NeutralVertexRole opened.openedAccess
  (reservedState, newIdEffects) <-
    call
      2
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateObject <- completedNewId "retirement controlled newid" 1 newIdEffects
  assertReservationPhase "pristine reservation" Controlled.Reserved reservedState

  endedReserved <- retireSession opened reservedState
  assertEqual
    "Reserved session requests are retired"
    []
    (Application.applicationRequestEntries opened.openedSession (startupApplicationState endedReserved))
  assertEqual
    "Reserved survives session retirement as valid semantic state"
    (Right ())
    (validateHeraldState endedReserved)
  assertReservationPhase "retired pristine reservation" Controlled.Reserved endedReserved
  assertEqual
    "Reserved retirement preserves the process alias"
    (resolvePrivateEither processP privateObject reservedState)
    (resolvePrivateEither processP privateObject endedReserved)

  let deleteCall =
        WriteApplication
          writer
          (DeleteReserved (asPrivateObjectId privateObject))
  (consumedState, deleteEffects) <- call 3 opened 2 deleteCall reservedState
  assertCompletedWriteAccepted "retirement delete" 2 deleteEffects
  assertReservationPhase
    "consumed reservation"
    Controlled.ConsumedByDelete
    consumedState

  endedConsumed <- retireSession opened consumedState
  assertEqual
    "ConsumedByDelete session requests are retired"
    []
    (Application.applicationRequestEntries opened.openedSession (startupApplicationState endedConsumed))
  assertEqual
    "ConsumedByDelete survives session retirement as valid semantic state"
    (Right ())
    (validateHeraldState endedConsumed)
  assertReservationPhase
    "retired consumed reservation"
    Controlled.ConsumedByDelete
    endedConsumed
  assertEqual
    "ConsumedByDelete retirement preserves the process alias"
    (resolvePrivateEither processP privateObject consumedState)
    (resolvePrivateEither processP privateObject endedConsumed)

assertReservationPhase :: String -> Controlled.ReservationPhase -> HeraldState -> Assertion
assertReservationPhase context expected state = do
  reservation <- oneReservation state
  assertEqual context expected (Controlled.reservationWitnessPhase reservation)

retireSession :: Opened -> HeraldState -> IO HeraldState
retireSession opened state = do
  prepared <-
    checkedIO
      "prepare application session retirement"
      ( Application.prepareApplicationSessionEnd
          opened.openedSession
          opened.openedBinding
          (startupApplicationState state)
      )
  pure
    ( replaceStartupApplicationState
        (Application.commitApplicationSessionEnd prepared)
        state
    )

pureOwnerScript :: Assertion
pureOwnerScript = do
  bootstraps <-
    checkedIO
      "check Step-11 bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest fixtureLocalBootstrapIds)
      )
  (bootstrapP, bootstrapQ) <- localBootstrapPair
  attachmentP <- attachment bootstraps bootstrapP
  attachmentQ <- attachment bootstraps bootstrapQ
  (initial, initialEffects) <-
    checkedIO
      "initialize seeded Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  assertBool "initialization emits one Oracle watch and one isolation grace timer" (initialEffectsAreOracleWatchAndGrace initialEffects)
  processP <- attachmentProcess "P process" attachmentP initial
  processQ <- attachmentProcess "Q process" attachmentQ initial

  (afterP1Open, p1) <- open 1 101 1 attachmentP initial
  (afterP2Open, p2) <- open 2 102 2 attachmentP afterP1Open
  (afterQOpen, q) <- open 3 103 1 attachmentQ afterP2Open
  pControlledWriter <- writerFor Access.NeutralVertexRole p1.openedAccess
  pSecondControlledWriter <- writerFor Access.NeutralVertexRole p2.openedAccess
  pSortWriter <- writerFor Access.SortDefinitionRole p1.openedAccess
  assertEqual "sessions of P share the controlled writer" pControlledWriter pSecondControlledWriter

  (afterBare, bareEffects) <-
    call 4 p1 1 (NewIdApplication BareNewId) afterQOpen
  pBare <- completedNewId "P bare newid" 1 bareEffects
  assertEqual "bare advances the shared generator once" 1 (generatorCounter afterBare)
  assertEqual "bare creates no reservation" [] (reservationWitnesses afterBare)
  assertGeneratedNonOwnersUnchanged afterQOpen afterBare
  assertOwnerEqual
    "bare leaves the complete Controlled owner unchanged"
    startupControlledState
    afterQOpen
    afterBare

  (afterControlled, controlledEffects) <-
    call
      5
      p2
      1
      (NewIdApplication (ControlledNewId pSecondControlledWriter))
      afterBare
  pControlled <- completedNewId "P controlled newid" 1 controlledEffects
  assertEqual "controlled advances the shared generator once" 2 (generatorCounter afterControlled)
  assertGeneratedNonOwnersUnchanged afterBare afterControlled
  assertControlledFactsUnchanged afterBare afterControlled
  controlledGlobal <- resolvePrivate "controlled global" processP pControlled afterControlled
  reservation <- oneReservation afterControlled
  assertEqual
    "reservation uses generated bits"
    controlledGlobal
    (Controlled.reservationWitnessGlobalUniqueId reservation)
  assertEqual "reservation names P" processP (Controlled.reservationWitnessProcess reservation)
  reservedWriter <-
    maybe
      (assertFailure "reservation writer fact is unavailable")
      pure
      ( Controlled.controlledWriterFact
          (Controlled.reservationWitnessNabla reservation)
          (startupControlledState afterControlled)
      )
  assertEqual
    "reservation freezes the resolved writer sort"
    (Controlled.rootFactSortId reservedWriter)
    (Controlled.reservationWitnessSortId reservation)
  assertEqual "reservation starts pristine" Controlled.Reserved (Controlled.reservationWitnessPhase reservation)
  let generatedObject = globalObjectIdFromGlobalUniqueId controlledGlobal
      controlledState = startupControlledState afterControlled
  assertBool
    "reserved identity has no Controlled role"
    (not (Controlled.controlledHasAnyRole generatedObject controlledState))
  assertBool
    "reserved identity has no normal possession"
    (not (Controlled.controlledHasAnyNormalPossession generatedObject controlledState))

  (afterBareRetry, bareRetryEffects) <-
    call 6 p1 1 (NewIdApplication BareNewId) afterControlled
  assertEqual "bare retry returns equal private bits" pBare
    =<< completedNewId "P bare retry" 1 bareRetryEffects
  assertOnlyTimeAdvanced afterControlled afterBareRetry
  (afterControlledRetry, controlledRetryEffects) <-
    call
      7
      p2
      1
      (NewIdApplication (ControlledNewId pSecondControlledWriter))
      afterBareRetry
  assertEqual "controlled retry returns equal private bits" pControlled
    =<< completedNewId "P controlled retry" 1 controlledRetryEffects
  assertOnlyTimeAdvanced afterBareRetry afterControlledRetry

  (afterConflict, conflictEffects) <-
    call
      8
      p1
      1
      (NewIdApplication (ControlledNewId pControlledWriter))
      afterControlledRetry
  assertRequestConflict "newid request conflict" 1 conflictEffects
  assertOnlyTimeAdvanced afterControlledRetry afterConflict

  (afterQBare, qBareEffects) <-
    call 8 q 1 (NewIdApplication BareNewId) afterConflict
  qBare <- completedNewId "Q bare newid" 1 qBareEffects
  assertEqual "isolated processes may reuse private numeric identities" pBare qBare
  pBareGlobal <- resolvePrivate "P bare global" processP pBare afterQBare
  qBareGlobal <- resolvePrivate "Q bare global" processQ qBare afterQBare
  assertBool "isolated equal private IDs map to distinct globals" (pBareGlobal /= qBareGlobal)
  assertEqual "Q bare uses the next shared generator slot" 3 (generatorCounter afterQBare)

  (afterRegularRejection, regularEffects) <-
    call
      9
      p1
      2
      (NewIdApplication (ControlledNewId pSortWriter))
      afterQBare
  assertRejected
    "regular writer controlled newid"
    2
    (ApplicationNablaSortNotControlled pSortWriter)
    regularEffects
  assertEqual "rejected controlled newid does not advance generator" 3 (generatorCounter afterRegularRejection)
  assertEqual
    "rejected controlled newid does not create a reservation"
    (reservationWitnesses afterQBare)
    (reservationWitnesses afterRegularRejection)
  assertAllocationOwnersUnchanged afterQBare afterRegularRejection

  let deleteCall =
        WriteApplication
          pControlledWriter
          (DeleteReserved (asPrivateObjectId pControlled))
  (afterDelete, deleteEffects) <- call 10 p1 3 deleteCall afterRegularRejection
  assertCompletedWriteAccepted "delete reserved" 3 deleteEffects
  deletedReservation <- oneReservation afterDelete
  assertEqual
    "delete retains terminal evidence"
    Controlled.ConsumedByDelete
    (Controlled.reservationWitnessPhase deletedReservation)
  assertEqual "delete does not advance generator" 3 (generatorCounter afterDelete)
  assertEqual "delete retains generated alias" (Right controlledGlobal) (resolvePrivateEither processP pControlled afterDelete)
  assertDeleteNonOwnersUnchanged afterRegularRejection afterDelete

  (afterDeleteRetry, deleteRetryEffects) <- call 11 p1 3 deleteCall afterDelete
  assertCompletedWriteAccepted "delete retry" 3 deleteRetryEffects
  assertOnlyTimeAdvanced afterDelete afterDeleteRetry
  (afterCompetingDelete, competingEffects) <-
    call
      12
      p2
      2
      ( WriteApplication
          pSecondControlledWriter
          (DeleteReserved (asPrivateObjectId pControlled))
      )
      afterDeleteRetry
  assertRejected
    "competing delete"
    2
    ( ApplicationReservationUnavailable
        pSecondControlledWriter
        (asPrivateObjectId pControlled)
    )
    competingEffects
  assertEqual
    "competing delete retains terminal evidence"
    (reservationWitnesses afterDeleteRetry)
    (reservationWitnesses afterCompetingDelete)

  let sortWrite =
        WriteApplication
          pSortWriter
          ( PublishValue
              (ApplicationValue.SortDefinitionValue (DeclaredSortDefinition declaredDescriptor Nothing))
          )
  (finalState, sortEffects) <- call 13 p1 4 sortWrite afterCompetingDelete
  assertCompletedSortWrite 4 sortEffects
  assertEqual "sort write does not advance generator" 3 (generatorCounter finalState)
  assertEqual "all retained state validates" (Right ()) (validateHeraldState finalState)

rejectionBranches :: Assertion
rejectionBranches = do
  bootstraps <-
    checkedIO
      "check rejection-branch bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest fixtureLocalBootstrapIds)
      )
  (bootstrapP, bootstrapQ) <- localBootstrapPair
  attachmentP <- attachment bootstraps bootstrapP
  attachmentQ <- attachment bootstraps bootstrapQ
  (initial, _) <-
    checkedIO
      "initialize rejection-branch Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  processP <- attachmentProcess "rejection P process" attachmentP initial
  processQ <- attachmentProcess "rejection Q process" attachmentQ initial
  (afterPOpen, p) <- open 1 301 1 attachmentP initial
  (afterQOpen, q) <- open 2 302 1 attachmentQ afterPOpen
  pWriter <- writerFor Access.NeutralVertexRole p.openedAccess
  pOtherWriter <- writerFor Access.EdgeRole p.openedAccess
  pSortWriter <- writerFor Access.SortDefinitionRole p.openedAccess
  qWriter <- writerFor Access.NeutralVertexRole q.openedAccess
  unknownPrivate <- checkedIO "unknown private identity" (mkPrivateUniqueId 999)
  let unknownNabla = asPrivateNablaId unknownPrivate
      unknownObject = asPrivateObjectId unknownPrivate

  (afterPBare, pBareEffects) <-
    call 3 p 1 (NewIdApplication BareNewId) afterQOpen
  pBare <- completedNewId "rejection fixture P bare" 1 pBareEffects
  (afterPControlled, pControlledEffects) <-
    call
      4
      p
      2
      (NewIdApplication (ControlledNewId pWriter))
      afterPBare
  pControlled <- completedNewId "rejection fixture P controlled" 2 pControlledEffects
  (afterQBare, qBareEffects) <-
    call 5 q 1 (NewIdApplication BareNewId) afterPControlled
  _ <- completedNewId "rejection fixture Q first bare" 1 qBareEffects
  (afterQForeign, qForeignEffects) <-
    call 6 q 2 (NewIdApplication BareNewId) afterQBare
  qForeign <- completedNewId "rejection fixture Q foreign-number bare" 2 qForeignEffects
  assertEqual
    "isolated processes may assign the reserved private number to different globals"
    pControlled
    qForeign
  pControlledGlobal <- resolvePrivate "P controlled global" processP pControlled afterQForeign
  qForeignGlobal <- resolvePrivate "Q foreign-number global" processQ qForeign afterQForeign
  assertBool
    "equal private numbers retain distinct process-local meanings"
    (pControlledGlobal /= qForeignGlobal)

  (afterUnknownControlled, unknownControlledEffects) <-
    call
      7
      p
      3
      (NewIdApplication (ControlledNewId unknownNabla))
      afterQForeign
  assertRejected
    "unknown controlled writer"
    3
    (ApplicationUnknownPrivateIdentity unknownPrivate)
    unknownControlledEffects
  assertAllocationOwnersUnchanged afterQForeign afterUnknownControlled

  let wrongRoleNabla = asPrivateNablaId pBare
  (afterWrongRole, wrongRoleEffects) <-
    call
      8
      p
      4
      (NewIdApplication (ControlledNewId wrongRoleNabla))
      afterUnknownControlled
  assertRejected
    "known non-writer controlled operand"
    4
    (ApplicationNablaRoleMismatch wrongRoleNabla)
    wrongRoleEffects
  assertAllocationOwnersUnchanged afterUnknownControlled afterWrongRole

  (withForeignWriter, qForeignWriter) <-
    localizeWriterForProcess
      processP
      pWriter
      processQ
      afterWrongRole
  (afterNoPossession, noPossessionEffects) <-
    call
      9
      q
      3
      (NewIdApplication (ControlledNewId qForeignWriter))
      withForeignWriter
  assertRejected
    "foreign controlled writer is not operable by the caller"
    3
    ApplicationOperateNotPermitted
    noPossessionEffects
  assertAllocationOwnersUnchanged withForeignWriter afterNoPossession

  let pBareDelete =
        WriteApplication pWriter (DeleteReserved (asPrivateObjectId pBare))
  (afterBareDelete, bareDeleteEffects) <- call 10 p 5 pBareDelete afterNoPossession
  assertRejected
    "delete bare identity"
    5
    (ApplicationReservationUnavailable pWriter (asPrivateObjectId pBare))
    bareDeleteEffects
  assertAllocationOwnersUnchanged afterNoPossession afterBareDelete

  let unknownDelete = WriteApplication pWriter (DeleteReserved unknownObject)
  (afterUnknownDelete, unknownDeleteEffects) <-
    call 11 p 6 unknownDelete afterBareDelete
  assertRejected
    "delete unknown identity"
    6
    (ApplicationUnknownPrivateIdentity unknownPrivate)
    unknownDeleteEffects
  assertAllocationOwnersUnchanged afterBareDelete afterUnknownDelete

  let invalidWriterUnknownObject =
        WriteApplication wrongRoleNabla (DeleteReserved unknownObject)
  (afterOrderedRejection, orderedRejectionEffects) <-
    call 12 p 7 invalidWriterUnknownObject afterUnknownDelete
  assertRejected
    "delete resolves both private operands before writer eligibility"
    7
    (ApplicationUnknownPrivateIdentity unknownPrivate)
    orderedRejectionEffects
  assertAllocationOwnersUnchanged afterUnknownDelete afterOrderedRejection

  let wrongNablaDelete =
        WriteApplication
          pOtherWriter
          (DeleteReserved (asPrivateObjectId pControlled))
  (afterWrongNabla, wrongNablaEffects) <-
    call 13 p 8 wrongNablaDelete afterOrderedRejection
  assertRejected
    "delete through the wrong controlled writer"
    8
    ( ApplicationReservationUnavailable
        pOtherWriter
        (asPrivateObjectId pControlled)
    )
    wrongNablaEffects
  assertAllocationOwnersUnchanged afterOrderedRejection afterWrongNabla

  let foreignPrivateDelete =
        WriteApplication
          qWriter
          (DeleteReserved (asPrivateObjectId qForeign))
  (afterForeignPrivate, foreignPrivateEffects) <-
    call 14 q 4 foreignPrivateDelete afterWrongNabla
  assertRejected
    "same-number foreign private identity"
    4
    ( ApplicationReservationUnavailable
        qWriter
        (asPrivateObjectId qForeign)
    )
    foreignPrivateEffects
  assertAllocationOwnersUnchanged afterWrongNabla afterForeignPrivate

  let regularWriterDelete =
        WriteApplication
          pSortWriter
          (DeleteReserved (asPrivateObjectId pControlled))
  (finalState, regularWriterEffects) <-
    call 15 p 9 regularWriterDelete afterForeignPrivate
  assertRejected
    "delete through regular sort-definition writer"
    9
    (ApplicationNablaSortNotControlled pSortWriter)
    regularWriterEffects
  assertAllocationOwnersUnchanged afterForeignPrivate finalState
  assertEqual "rejection branch state validates" (Right ()) (validateHeraldState finalState)

propMixedOrdering :: [(Bool, Bool, Bool)] -> Property
propMixedOrdering generatedSteps = ioProperty $ do
  let steps = take 16 generatedSteps
  bootstraps <-
    checkedIO
      "mixed-order bootstraps"
      ( checkInitialBootstraps
          fixtureCheckedGenesis
          (PrimordialProcessManifest fixtureLocalBootstrapIds)
      )
  (bootstrapP, _) <- localBootstrapPair
  applicationAttachment <- attachment bootstraps bootstrapP
  (initial, _) <-
    checkedIO
      "mixed-order initial Herald"
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  process <- attachmentProcess "mixed-order process" applicationAttachment initial
  (openedState, opened) <- open 1 201 1 applicationAttachment initial
  writer <- writerFor Access.NeutralVertexRole opened.openedAccess
  (finalState, privateIdentities, globals) <-
    foldM
      (runMixedCall process opened writer)
      (openedState, [], [])
      (zip [1 ..] steps)
  let targets = fmap (\(controlled, _, _) -> controlled) steps
      expectedCount = length steps
      expectedReservations = length (filter id targets)
      privateWords = fmap privateUniqueIdWord64 privateIdentities
      startup = opened.openedAccess
      startupIdentities = privateProcessUniqueId (Access.startupAccessProcess startup) : fmap Access.primordialEntryIdentity (Map.elems (Access.accessEntries (Access.startupAccessPrimordial startup)))
      firstGenerated = 1 + maximum (fmap privateUniqueIdWord64 startupIdentities)
      modelHolds =
        generatorCounter finalState == fromIntegral expectedCount
          && length (reservationWitnesses finalState) == expectedReservations
          && length (nub globals) == expectedCount
          && privateWords == take expectedCount [firstGenerated ..]
          && validateHeraldState finalState == Right ()
  pure
    ( counterexample
        ( "steps/private/globals/reservations: "
            <> show
              ( steps,
                privateWords,
                length globals,
                length (reservationWitnesses finalState)
              )
        )
        modelHolds
    )

runMixedCall ::
  ProcessEpochId ->
  Opened ->
  PrivateNablaId ->
  (HeraldState, [PrivateUniqueId], [GlobalUniqueId]) ->
  (Word, (Bool, Bool, Bool)) ->
  IO (HeraldState, [PrivateUniqueId], [GlobalUniqueId])
runMixedCall process opened writer (predecessor, privateIdentities, globals) (ordinal, (controlled, retry, conflict)) = do
  let target
        | controlled = ControlledNewId writer
        | otherwise = BareNewId
      firstObserved = ordinal * 3 + 1
  (afterFirst, effects) <-
    call
      firstObserved
      opened
      ordinal
      (NewIdApplication target)
      predecessor
  privateIdentity <- completedNewId "mixed newid" ordinal effects
  assertGeneratedNonOwnersUnchanged predecessor afterFirst
  if controlled
    then assertControlledFactsUnchanged predecessor afterFirst
    else
      assertOwnerEqual
        "mixed bare Controlled owner"
        startupControlledState
        predecessor
        afterFirst
  global <- resolvePrivate "mixed global" process privateIdentity afterFirst
  afterRetry <-
    if retry
      then do
        (successor, retryEffects) <-
          call
            (firstObserved + 1)
            opened
            ordinal
            (NewIdApplication target)
            afterFirst
        assertEqual "mixed retry returns equal private identity" privateIdentity
          =<< completedNewId "mixed newid retry" ordinal retryEffects
        assertOnlyTimeAdvanced afterFirst successor
        pure successor
      else pure afterFirst
  afterConflict <-
    if conflict
      then do
        let conflictingTarget
              | controlled = BareNewId
              | otherwise = ControlledNewId writer
        (successor, conflictEffects) <-
          call
            (firstObserved + 2)
            opened
            ordinal
            (NewIdApplication conflictingTarget)
            afterRetry
        assertRequestConflict "mixed newid conflict" ordinal conflictEffects
        assertOnlyTimeAdvanced afterRetry successor
        pure successor
      else pure afterRetry
  pure
    ( afterConflict,
      privateIdentities <> [privateIdentity],
      globals <> [global]
    )

localizeWriterForProcess ::
  ProcessEpochId ->
  PrivateNablaId ->
  ProcessEpochId ->
  HeraldState ->
  IO (HeraldState, PrivateNablaId)
localizeWriterForProcess sourceProcess sourceWriter targetProcess predecessor = do
  global <-
    resolvePrivate
      "foreign controlled writer global"
      sourceProcess
      (privateNablaUniqueId sourceWriter)
      predecessor
  prepared <-
    checkedIO
      "localize foreign controlled writer"
      ( Application.prepareOrdinaryApplicationValueLocalization
          (Application.processRoleView [])
          targetProcess
          (Domain.globalUniqueIdValue global)
          (startupApplicationState predecessor)
      )
  let (applicationSuccessor, localized) =
        Application.commitOrdinaryApplicationValueLocalization prepared
      successor =
        replaceStartupApplicationState applicationSuccessor predecessor
  privateIdentity <- case localized of
    ApplicationValue.UniqueIdValue identity -> pure identity
    other -> assertFailure ("unexpected localized writer value: " <> show other)
  assertEqual
    "foreign-writer localization preserves the composed invariant"
    (Right ())
    (validateHeraldState successor)
  pure (successor, asPrivateNablaId privateIdentity)

assertGeneratedNonOwnersUnchanged :: HeraldState -> HeraldState -> Assertion
assertGeneratedNonOwnersUnchanged = assertCommonNonOwnersUnchanged

assertAllocationOwnersUnchanged :: HeraldState -> HeraldState -> Assertion
assertAllocationOwnersUnchanged before after = do
  assertOwnerEqual "generator owner" startupIdGeneratorState before after
  assertOwnerEqual
    "private-identity owner"
    (Application.applicationPrivateIdentity . startupApplicationState)
    before
    after
  assertOwnerEqual "Controlled owner" startupControlledState before after
  assertCommonNonOwnersUnchanged before after

assertDeleteNonOwnersUnchanged :: HeraldState -> HeraldState -> Assertion
assertDeleteNonOwnersUnchanged before after = do
  assertOwnerEqual "delete leaves generator unchanged" startupIdGeneratorState before after
  assertOwnerEqual
    "delete leaves private maps and supply unchanged"
    (Application.applicationPrivateIdentity . startupApplicationState)
    before
    after
  assertControlledFactsUnchanged before after
  assertCommonNonOwnersUnchanged before after

assertControlledFactsUnchanged :: HeraldState -> HeraldState -> Assertion
assertControlledFactsUnchanged before after = do
  assertEqual
    "controlled process facts"
    (Controlled.controlledProcessFacts (startupControlledState before))
    (Controlled.controlledProcessFacts (startupControlledState after))
  assertEqual
    "controlled root and possession facts"
    (Controlled.controlledRootFacts (startupControlledState before))
    (Controlled.controlledRootFacts (startupControlledState after))

assertCommonNonOwnersUnchanged :: HeraldState -> HeraldState -> Assertion
assertCommonNonOwnersUnchanged before after = do
  assertOwnerEqual "administration owner" startupAdministrationState before after
  assertOwnerEqual "Oracle projection owner" startupOracleProjectionState before after
  assertOwnerEqual "sort registry owner" startupSortRegistryState before after
  assertOwnerEqual "discovery owner" startupDiscoveryState before after
  assertOwnerEqual "graph owner" startupGraphState before after
  assertOwnerEqual "peer-stream owner" startupPeerStreamState before after
  assertOwnerEqual "placement owner" startupPlacementState before after
  assertOwnerEqual "store owner" startupStoreState before after
  assertOwnerEqual "publication owner" startupPublicationState before after
  assertOwnerEqual "wait owner" startupWaitState before after

assertOwnerEqual ::
  (Eq owner) =>
  String ->
  (HeraldState -> owner) ->
  HeraldState ->
  HeraldState ->
  Assertion
assertOwnerEqual context project before after =
  assertBool (context <> " changed") (project before == project after)

localBootstrapPair :: IO (BootstrapManifestId, BootstrapManifestId)
localBootstrapPair = case fixtureLocalBootstrapIds of
  [bootstrapP, bootstrapQ] -> pure (bootstrapP, bootstrapQ)
  actual -> assertFailure ("expected two local bootstraps, got " <> show (length actual))

attachment :: CheckedInitialBootstraps -> BootstrapManifestId -> IO ApplicationAttachment
attachment bootstraps bootstrap =
  checkedMaybe
    "primordial application attachment"
    (primordialApplicationAttachment fixtureCheckedGenesis bootstraps bootstrap)

attachmentProcess :: String -> ApplicationAttachment -> HeraldState -> IO ProcessEpochId
attachmentProcess context applicationAttachment state =
  checkedMaybe
    context
    (Application.applicationAttachmentProcess applicationAttachment (startupApplicationState state))

open ::
  Word ->
  Word ->
  Word ->
  ApplicationAttachment ->
  HeraldState ->
  IO (HeraldState, Opened)
open observed lane nonce applicationAttachment predecessor = do
  (successor, effects) <-
    step
      observed
      ( ApplicationSessionInput
          ( OpenApplicationSession
              (candidateApplicationLane (fromIntegral lane))
              applicationAttachment
              (Session.clientNonce (fromIntegral nonce))
          )
      )
      predecessor
  acceptance <- case effectBatchMembers effects of
    [SetApplicationConnectionDisposition _ value] -> pure value
    actual -> assertFailure ("unexpected open effects: " <> show actual)
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ access ->
      pure
        ( successor,
          Opened session (sessionAcceptanceBinding acceptance) access
        )
    other -> assertFailure ("unexpected open reply: " <> show other)

writerFor :: ApplicationPredefinedSortRole -> ApplicationStartupAccess -> IO PrivateNablaId
writerFor role access =
  predefinedWriter
    <$> checkedMaybe
      ("startup writer for " <> show role)
      (find ((== role) . predefinedAccessRole) (conventionalStartupPairs access))

call ::
  Word ->
  Opened ->
  Word ->
  ApplicationOperation ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
call observed opened request callBody =
  step
    observed
    ( ApplicationRequestInput
        ( CallApplicationRequest
            opened.openedBinding
            opened.openedSession
            (requestId (fromIntegral request))
            callBody
        )
    )

step :: Word -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step observed body state =
  checkedIO
    "Step-11 Herald step"
    (verifiedStepHerald (heraldInput (monotonicInstant (fromIntegral observed)) body) state)

completedNewId :: String -> Word -> EffectBatch -> IO PrivateUniqueId
completedNewId context expected effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed actual (NewIdCompleted privateIdentity))
        )
    ] -> do
      assertEqual (context <> " request") (requestId (fromIntegral expected)) actual
      pure privateIdentity
  actual -> assertFailure (context <> ": unexpected effects " <> show actual)

assertCompletedWriteAccepted :: String -> Word -> EffectBatch -> Assertion
assertCompletedWriteAccepted context expected effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed actual (WriteCompleted WriteAccepted))
        )
    ] -> assertEqual (context <> " request") (requestId (fromIntegral expected)) actual
  actual -> assertFailure (context <> ": unexpected effects " <> show actual)

assertCompletedSortWrite :: Word -> EffectBatch -> Assertion
assertCompletedSortWrite expected effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed actual (WriteCompleted (SortDefinitionWritten actualSort)))
        )
    ] -> do
      expectedSort <-
        admittedApplicationSortId
          <$> checkedIO
            "admit expected sort definition"
            ( admitApplicationSortDefinition
                (DeclaredSortDefinition declaredDescriptor Nothing)
            )
      assertEqual "sort write request" (requestId (fromIntegral expected)) actual
      assertEqual "sort write result" expectedSort actualSort
  actual -> assertFailure ("sort write: unexpected effects " <> show actual)

assertRejected :: String -> Word -> ApplicationRejection -> EffectBatch -> Assertion
assertRejected context expected rejection effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      (RetainedRequestReply _ (Rejected actual observedRejection))
    ] -> do
      assertEqual (context <> " request") (requestId (fromIntegral expected)) actual
      assertEqual (context <> " rejection") rejection observedRejection
  actual -> assertFailure (context <> ": unexpected effects " <> show actual)

assertRequestConflict :: String -> Word -> EffectBatch -> Assertion
assertRequestConflict context expected effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RequestConflict actual)] ->
    assertEqual (context <> " request") (requestId (fromIntegral expected)) actual
  actual -> assertFailure (context <> ": unexpected effects " <> show actual)

resolvePrivate ::
  String ->
  ProcessEpochId ->
  PrivateUniqueId ->
  HeraldState ->
  IO GlobalUniqueId
resolvePrivate context process privateIdentity state =
  checkedIO context (resolvePrivateEither process privateIdentity state)

resolvePrivateEither ::
  ProcessEpochId ->
  PrivateUniqueId ->
  HeraldState ->
  Either Application.ApplicationValueGlobalizationError GlobalUniqueId
resolvePrivateEither process privateIdentity state =
  Application.resolveApplicationPrivateUniqueId
    process
    privateIdentity
    (startupApplicationState state)

generatorCounter :: HeraldState -> Word
generatorCounter =
  fromIntegral
    . startupWitnessNextGeneratorCounter
    . startupStateWitness

reservationWitnesses :: HeraldState -> [Controlled.ReservationWitness]
reservationWitnesses =
  Controlled.controlledReservationWitnesses . startupControlledState

oneReservation :: HeraldState -> IO Controlled.ReservationWitness
oneReservation state = case reservationWitnesses state of
  [reservation] -> pure reservation
  actual -> assertFailure ("expected one reservation, got " <> show (length actual))

assertOnlyTimeAdvanced :: HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvanced before after =
  assertBool
    "retry changes only the monotonic observation"
    (replaceStartupLastObservedTime (startupLastObservedTime before) after == before)

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections =
        [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

checkedIO :: (Show error) => String -> Either error value -> IO value
checkedIO context = either (\problem -> assertFailure (context <> ": " <> show problem)) pure

checkedMaybe :: String -> Maybe value -> IO value
checkedMaybe context = maybe (assertFailure context) pure
