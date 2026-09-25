{-# LANGUAGE OverloadedStrings #-}

module SortDefinitionValueProperties
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalizeDescriptor,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor),
    DescriptorSpec (..),
    PredicateExpression (AlwaysPredicate, NeverPredicate),
    RankDirection (Ascending),
    RankTerm (RankApplicationValue),
    SortKind (RegularSort),
  )
import Eclips.Domain.Sort.Profile
  ( decodeSortDefinitionValue,
    predefinedCatalogueDescriptor,
    profilePredefinedCatalogue,
    sortDefinitionValue,
  )
import Eclips.Domain.Value
  ( FieldName,
    directProjection,
    mkFieldName,
    recordSchema,
    textSchema,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "sort-definition values"
    [ testCase "every exact catalogue descriptor decodes through the primordial path" caseCatalogueDecode,
      testCase "an ordinary declared descriptor decodes through the application path" caseDeclaredDecode
    ]

caseCatalogueDecode :: IO ()
caseCatalogueDecode =
  mapM_
    (assertDescriptorDecode . predefinedCatalogueDescriptor)
    profilePredefinedCatalogue

caseDeclaredDecode :: IO ()
caseDeclaredDecode = assertDescriptorDecode declaredDescriptor

assertDescriptorDecode :: CanonicalDescriptor -> IO ()
assertDescriptorDecode descriptor =
  assertEqual
    "the exact checked descriptor is recovered"
    (Right descriptor)
    (decodeSortDefinitionValue (sortDefinitionValue descriptor))

declaredDescriptor :: CanonicalDescriptor
declaredDescriptor =
  checked
    "declared descriptor"
    ( canonicalizeDescriptor
        ApplicationDescriptor
        DescriptorSpec
          { descriptorSpecKind = RegularSort,
            descriptorSpecSchema =
              checked "declared schema" (recordSchema [(keyField, textSchema)]),
            descriptorSpecKeyProjections = [directProjection keyField],
            descriptorSpecValidity = AlwaysPredicate,
            descriptorSpecObsolescence = NeverPredicate,
            descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
            descriptorSpecMinimumRetentionMicros = 19,
            descriptorSpecImmutable = False,
            descriptorSpecLabelField = Nothing,
            descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
            descriptorSpecStructuralCarrierRole = Nothing
          }
    )

keyField :: FieldName
keyField = checked "key field" (mkFieldName "key")

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
