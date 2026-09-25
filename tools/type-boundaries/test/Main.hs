module Main (main) where

import Data.List (isInfixOf, nub)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    elements,
    listOf1,
    testProperty,
    (===),
  )

import Eclips.TypeBoundaries (diagnosticSupportsRejection, extractCppSelectors, prepareProbeSource)
import Eclips.TypeBoundaries.Spec
  ( ProbeExpectation (MustReject, MustSucceed),
    ProbeSpec (..),
    RejectionClass (HiddenImport, TypeMismatch),
    cabalPrivateProbeSelectors,
    expectedCabalPrivateProbeCount,
    expectedNegativeProbeCount,
    expectedPositiveProbeCount,
    negativeProbes,
    positiveProbes,
    publicUnits,
  )
import Eclips.TypeBoundaries.Surface
  ( SurfaceEntry (..),
    parseSurface,
    renderSurface,
    surfaceDifference,
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "semantic type-boundary checker"
    [ testCase "the migrated case ledger has its reviewed cardinalities" $ do
        length negativeProbes @?= expectedNegativeProbeCount
        length positiveProbes @?= expectedPositiveProbeCount
        length cabalPrivateProbeSelectors @?= expectedCabalPrivateProbeCount,
      testCase "logical units and probe IDs are unique" $ do
        length (nub publicUnits) @?= length publicUnits
        let identifiers = fmap probeId (negativeProbes <> positiveProbes)
        length (nub identifiers) @?= length identifiers,
      testCase "probe IDs are nonempty"
        $ assertBool "empty probe ID" (all (not . null . probeId) (negativeProbes <> positiveProbes)),
      testCase "every rejected probe has nonempty diagnostic evidence"
        $ assertBool "empty rejection evidence" (all hasEvidence negativeProbes),
      testCase "rejection evidence must occur in one error of the expected class" $ do
        let hiddenEvidence = diagnosticSupportsRejection HiddenImport ["Private.Module"]
            mismatchEvidence = diagnosticSupportsRejection TypeMismatch ["ExpectedType", "ActualType"]
        assertBool "valid hidden-module error was rejected" (hiddenEvidence "Could not load module ‘Private.Module’")
        assertBool "unrelated error was accepted" (not (hiddenEvidence "Private.Module: parse error"))
        assertBool
          "evidence split across errors was accepted"
          ( not
              ( any
                  mismatchEvidence
                  [ "Couldn't match type ‘ExpectedType’ with something else",
                    "Couldn't match type something else with ‘ActualType’"
                  ]
              )
          ),
      testCase "fixture preparation selects CPP and gives every case a unique module" $ do
        let prepared =
              prepareProbeSource
                "BoundaryProbe42"
                "PrivateRoleCoercion.hs"
                (Just "UNIQUE_WORD64")
                "{-# LANGUAGE CPP #-}\nmodule PrivateRoleCoercion where\nwitness = ()\n"
        case prepared of
          Left message -> fail message
          Right source -> do
            assertBool "selector missing" ("#define UNIQUE_WORD64" `isInfixOf` source)
            assertBool "module was not rewritten" ("module BoundaryProbe42 where" `isInfixOf` source),
      testCase "CPP selector extraction handles supported conditional forms"
        $ extractCppSelectors
          ( unlines
              [ "#if defined(FIRST)",
                "#elif defined (SECOND)",
                "#ifdef THIRD",
                "#if !defined(FOURTH)",
                "# if FIFTH && defined(SIXTH)",
                "#endif"
              ]
          )
          @?= ["FIFTH", "FIRST", "FOURTH", "SECOND", "SIXTH", "THIRD"],
      testCase "CPP selector extraction exposes selector-free conditionals"
        $ extractCppSelectors "#if 0\n#endif\n"
          @?= ["<conditional-without-selector>"],
      testProperty "surface rendering and parsing round-trip" propSurfaceRoundTrip,
      testProperty "surface comparison ignores enumeration order" propSurfaceOrder,
      testCase "surface comparison preserves duplicate-name multiplicity" $ do
        let entry = getSafeSurfaceEntry exampleSurfaceEntry
        surfaceDifference [entry] [entry, entry] @?= ([entry], []),
      testProperty "surface normalization is idempotent" propSurfaceIdempotent
    ]

propSurfaceRoundTrip :: [SafeSurfaceEntry] -> Property
propSurfaceRoundTrip wrapped =
  parseSurface (renderSurface entries) === Right (ordered entries)
  where
    entries = fmap getSafeSurfaceEntry wrapped

propSurfaceOrder :: [SafeSurfaceEntry] -> Property
propSurfaceOrder wrapped =
  surfaceDifference entries (reverse entries) === ([], [])
  where
    entries = fmap getSafeSurfaceEntry wrapped

propSurfaceIdempotent :: [SafeSurfaceEntry] -> Property
propSurfaceIdempotent wrapped =
  (parseSurface (renderSurface entries) >>= (parseSurface . renderSurface))
    === parseSurface (renderSurface entries)
  where
    entries = fmap getSafeSurfaceEntry wrapped

newtype SafeSurfaceEntry = SafeSurfaceEntry
  { getSafeSurfaceEntry :: SurfaceEntry
  }
  deriving stock (Eq, Show)

exampleSurfaceEntry :: SafeSurfaceEntry
exampleSurfaceEntry =
  SafeSurfaceEntry
    SurfaceEntry
      { publicUnit = "example:main",
        publicModule = "Example",
        exportNamespace = "field",
        exportOccurrence = "name",
        exportParent = "type:Example",
        definingUnit = "example:main",
        definingModule = "Example.Internal"
      }

instance Arbitrary SafeSurfaceEntry where
  arbitrary =
    SafeSurfaceEntry
      <$> ( SurfaceEntry
              <$> token
              <*> token
              <*> token
              <*> token
              <*> token
              <*> token
              <*> token
          )

token :: Gen String
token = listOf1 (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "._:-"))

ordered :: (Ord value) => [value] -> [value]
ordered [] = []
ordered (value : values) =
  ordered [candidate | candidate <- values, candidate < value]
    <> [value]
    <> ordered [candidate | candidate <- values, candidate >= value]

hasEvidence :: ProbeSpec -> Bool
hasEvidence ProbeSpec {expectation = MustReject _ evidence} = not (null evidence) && all (not . null) evidence
hasEvidence ProbeSpec {expectation = MustSucceed} = False
