module SortOccurrenceProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    controlIndex,
    mkSortId,
    mkSystemId,
    sortDefinitionOccurrenceIdBytes,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    SortOccurrenceBaseProblem (ResolvedRetirementControlIndexMustBePositive),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "sort-definition occurrence identity"
    [ testCase "resolved retirement bases require a positive Oracle index" caseBaseAdmission,
      testCase "equal checked coordinates derive the same occurrence" caseStableDerivation,
      testCase "genesis and resolved-retirement bases are domain-separated" caseBaseSeparation,
      testCase "system, sort, and resolved-retirement index are identity inputs" caseCoordinateSeparation,
      testCase "all bases produce checked fixed-width occurrence identities" caseCheckedWidth
    ]

caseStableDerivation :: IO ()
caseStableDerivation = do
  assertEqual
    "genesis is stable"
    (derive fixtureSystem fixtureSort Genesis)
    (derive fixtureSystem fixtureSort Genesis)
  assertEqual
    "one resolution base is stable"
    (derive fixtureSystem fixtureSort (retirementBase 7))
    (derive fixtureSystem fixtureSort (retirementBase 7))

caseBaseAdmission :: IO ()
caseBaseAdmission = do
  assertEqual
    "genesis index cannot pose as a retirement resolution"
    (Left ResolvedRetirementControlIndexMustBePositive)
    (resolvedRetirementOccurrenceBase (controlIndex 0))
  assertEqual
    "a positive applied Oracle index is retained"
    (Right (retirementBase 7))
    (resolvedRetirementOccurrenceBase (controlIndex 7))

caseBaseSeparation :: IO ()
caseBaseSeparation =
  assertBool
    "genesis cannot alias a retirement-derived occurrence"
    ( derive fixtureSystem fixtureSort Genesis
        /= derive fixtureSystem fixtureSort (retirementBase 1)
    )

caseCoordinateSeparation :: IO ()
caseCoordinateSeparation = do
  let occurrence = derive fixtureSystem fixtureSort (retirementBase 7)
  assertBool
    "system identity participates"
    (occurrence /= derive alternateSystem fixtureSort (retirementBase 7))
  assertBool
    "sort identity participates"
    (occurrence /= derive fixtureSystem alternateSort (retirementBase 7))
  assertBool
    "resolved-retirement index participates"
    (occurrence /= derive fixtureSystem fixtureSort (retirementBase 8))

caseCheckedWidth :: IO ()
caseCheckedWidth =
  mapM_
    ( assertEqual "occurrence width" 32
        . ByteString.length
        . sortDefinitionOccurrenceIdBytes
        . derive fixtureSystem fixtureSort
    )
    [Genesis, retirementBase 1, retirementBase 42]

derive :: SystemId -> SortId -> SortOccurrenceBase -> SortDefinitionOccurrenceId
derive = deriveSortDefinitionOccurrenceId

retirementBase :: Word64 -> SortOccurrenceBase
retirementBase index =
  checked
    "resolved retirement occurrence base"
    (resolvedRetirementOccurrenceBase (controlIndex index))

fixtureSystem, alternateSystem :: SystemId
fixtureSystem = checked "fixture system" (mkSystemId (ByteString.replicate 32 0x11))
alternateSystem = checked "alternate system" (mkSystemId (ByteString.replicate 32 0x12))

fixtureSort, alternateSort :: SortId
fixtureSort = checked "fixture sort" (mkSortId (ByteString.replicate 32 0x21))
alternateSort = checked "alternate sort" (mkSortId (ByteString.replicate 32 0x22))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
