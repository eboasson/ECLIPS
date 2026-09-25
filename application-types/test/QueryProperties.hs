{-# LANGUAGE OverloadedStrings #-}

module QueryProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Eclips.Application.Types.Access
  ( EnvironmentAccess,
    allApplicationPredefinedSortRoles,
    environmentAccess,
    predefinedAccess,
  )
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
  )
import Eclips.Application.Types.Label (LabelResult (LabelApplied))
import Eclips.Application.Types.Query
  ( ApplicationProjection (ApplicationProjection),
    ApplicationQuery (..),
    ApplicationQueryLiteral (..),
    ApplicationQueryPredicate (..),
    ApplicationScalarComparison (ScalarEqual),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (..),
    ApplicationValue (BoolValue),
  )
import Eclips.Application.Types.Write (WriteResult (SortDefinitionWritten, WriteAccepted))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    resize,
    sized,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "application query and regular result vocabulary"
    [ testCase "empty delta sets remain representable" caseEmptyDeltaSet,
      testCase "identity, label, and enum literals remain explicit" casePrivateLiterals,
      testProperty "generated predicates retain every recursive constructor" propPredicateStructure,
      testCase "the current result union distinguishes all eight calls" caseRegularResults
    ]

caseEmptyDeltaSet :: IO ()
caseEmptyDeltaSet =
  assertEqual
    "empty local cut"
    Set.empty
    (applicationQueryDeltas (ApplicationQuery Set.empty QueryAlways))

casePrivateLiterals :: IO ()
casePrivateLiterals = do
  let privateIdentity = checkedPrivateUniqueId 7
      privateProcess = asPrivateProcessId privateIdentity
      literals =
        [ QueryUniqueId privateIdentity,
          QueryLabel (VoidLabel, 0),
          QueryLabel (ProcessLabel privateProcess, 7),
          QueryLabel (ZombieLabel privateProcess, 0),
          QueryEnum "preserve"
        ]
      predicate =
        QueryAll
          ( QueryCompare projection ScalarEqual (QueryUniqueId privateIdentity)
              :| [ QueryCompare projection ScalarEqual (QueryLabel (VoidLabel, 0)),
                   QueryCompare projection ScalarEqual (QueryLabel (ProcessLabel privateProcess, 7)),
                   QueryCompare projection ScalarEqual (QueryLabel (ZombieLabel privateProcess, 0)),
                   QueryCompare projection ScalarEqual (QueryEnum "preserve")
                 ]
          )
  assertEqual "private literal identities are unchanged" literals (predicateLiterals predicate)
  where
    projection = ApplicationProjection ("identity" :| [])

propPredicateStructure :: Property
propPredicateStructure =
  forAll (genPredicate privateIdentity) $ \predicate ->
    rebuildPredicate predicate === predicate
  where
    privateIdentity = checkedPrivateUniqueId 9

caseRegularResults :: IO ()
caseRegularResults = do
  let values = [BoolValue True]
      privateIdentity = checkedPrivateUniqueId 7
      sortId = checked "sort ID" (mkSortId (ByteString.replicate 32 4))
      writeResult = SortDefinitionWritten sortId
      environment =
        checked
          "environment access"
          ( environmentAccess
              [ predefinedAccess
                  role
                  (asPrivateNablaId (checkedPrivateUniqueId (100 + fromIntegral (fromEnum role) * 2)))
                  (asPrivateDeltaId (checkedPrivateUniqueId (101 + fromIntegral (fromEnum role) * 2)))
              | role <- allApplicationPredefinedSortRoles
              ]
              (asPrivateObjectId (checkedPrivateUniqueId 1000))
              [asPrivateObjectId (checkedPrivateUniqueId value) | value <- [1001 .. 1018]]
          )
  assertEqual "newid result" privateIdentity (newIdResult (NewIdCompleted privateIdentity))
  assertEqual "write result" writeResult (writtenResult (WriteCompleted writeResult))
  assertEqual "accepted write result" WriteAccepted (writtenResult (WriteCompleted WriteAccepted))
  assertEqual "forward result" ForwardAccepted (forwardResult (ForwardCompleted ForwardAccepted))
  assertEqual "read result" values (readValues (ReadCompleted values))
  assertEqual "take result" values (takeValues (LocalTakeCompleted values))
  assertEqual "wait result" WaitReady (waitResult (WaitCompleted WaitReady))
  assertEqual "label result" LabelApplied (labelResult (LabelCompleted LabelApplied))
  assertEqual
    "environment result"
    environment
    (environmentResult (NewEnvironmentCompleted environment))

genPredicate :: PrivateUniqueId -> Gen ApplicationQueryPredicate
genPredicate privateIdentity = sized generate
  where
    generate remaining
      | remaining <= 0 = leaf
      | otherwise =
          frequency
            [ (5, leaf),
              (1, QueryNot <$> generate (remaining - 1)),
              (1, QueryAll <$> children (remaining `div` 2)),
              (1, QueryAny <$> children (remaining `div` 2))
            ]
    children remaining =
      (:|)
        <$> generate remaining
        <*> resize 3 (listOf (generate remaining))
    leaf =
      oneof
        [ pure QueryAlways,
          pure QueryNever,
          QueryCompare projection
            <$> elements [minBound .. maxBound]
            <*> literal
        ]
    literal =
      oneof
        [ QueryBool <$> arbitrary,
          QueryInt64 <$> (arbitrary :: Gen Int64),
          QueryBytes . ByteString.pack <$> resize 8 (listOf arbitrary),
          QueryText . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z'])),
          pure (QueryUniqueId privateIdentity),
          QueryLabel
            <$> ((,) <$> elements [VoidLabel, ProcessLabel privateProcess, ZombieLabel privateProcess] <*> arbitrary),
          QueryEnum . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z']))
        ]
    projection = ApplicationProjection ("outer" :| ["field"])
    privateProcess = asPrivateProcessId privateIdentity

rebuildPredicate :: ApplicationQueryPredicate -> ApplicationQueryPredicate
rebuildPredicate predicate = case predicate of
  QueryAlways -> QueryAlways
  QueryNever -> QueryNever
  QueryCompare projection comparison literal ->
    QueryCompare projection comparison literal
  QueryNot child -> QueryNot (rebuildPredicate child)
  QueryAll children -> QueryAll (fmap rebuildPredicate children)
  QueryAny children -> QueryAny (fmap rebuildPredicate children)

predicateLiterals :: ApplicationQueryPredicate -> [ApplicationQueryLiteral]
predicateLiterals = \case
  QueryAlways -> []
  QueryNever -> []
  QueryCompare _ _ literal -> [literal]
  QueryNot child -> predicateLiterals child
  QueryAll children -> foldMap predicateLiterals children
  QueryAny children -> foldMap predicateLiterals children

writtenResult :: RegularCallResult -> WriteResult
writtenResult (WriteCompleted result) = result
writtenResult _ = error "test expected a write result"

forwardResult :: RegularCallResult -> ForwardResult
forwardResult (ForwardCompleted result) = result
forwardResult _ = error "test expected a forward result"

newIdResult :: RegularCallResult -> PrivateUniqueId
newIdResult (NewIdCompleted identifier) = identifier
newIdResult _ = error "test expected a newid result"

readValues :: RegularCallResult -> [ApplicationValue]
readValues (ReadCompleted values) = values
readValues _ = []

takeValues :: RegularCallResult -> [ApplicationValue]
takeValues (LocalTakeCompleted values) = values
takeValues _ = []

waitResult :: RegularCallResult -> WaitResult
waitResult (WaitCompleted result) = result
waitResult _ = WaitReady

labelResult :: RegularCallResult -> LabelResult
labelResult (LabelCompleted result) = result
labelResult _ = LabelApplied

environmentResult :: RegularCallResult -> EnvironmentAccess
environmentResult (NewEnvironmentCompleted result) = result
environmentResult _ = error "test expected an environment result"

checkedPrivateUniqueId :: Word -> PrivateUniqueId
checkedPrivateUniqueId value =
  checked "private ID" (mkPrivateUniqueId (fromIntegral value))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
