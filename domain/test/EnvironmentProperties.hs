{-# LANGUAGE OverloadedStrings #-}

module EnvironmentProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word8)
import Eclips.Domain.Environment
  ( EnvironmentManifestShapeProblem (..),
    EnvironmentRootClaim,
    EnvironmentRootClaimView (..),
    checkEnvironmentManifestShape,
    environmentManifestRootCount,
    environmentManifestShapeCanonicalBytes,
    environmentManifestShapeRoots,
    environmentReaderRootClaim,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotOrdinal,
    environmentRootSlotStructuralCarrierRole,
    environmentWriterRootClaim,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Identity
  ( NablaSequencing (..),
    mkGlobalObjectId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, NablaCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedSortRoleTag,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "environment manifest shape"
    [ testCase "the closed manifest is six writer/reader pairs" caseCanonicalShape,
      testCase "the checker accepts only the exact complete order" caseExactAdmission,
      testCase "writers must start unsequenced" caseWriterSequencing,
      testCase "canonical bytes pin count, role, and writer/reader order" caseCanonicalBytes
    ]

caseCanonicalShape :: IO ()
caseCanonicalShape = do
  assertEqual "root count" 12 environmentManifestRootCount
  assertEqual "ordinals" [0 .. 11] (fmap environmentRootSlotOrdinal roots)
  assertEqual "root claims" expectedViews (fmap (environmentRootClaimView . environmentRootSlotClaim) roots)
  assertEqual
    "writer and reader source carriers"
    (concatMap (const [NablaCarrier, DeltaCarrier]) canonicalRoles)
    (fmap environmentRootSlotStructuralCarrierRole roots)
  where
    roots = NonEmpty.toList (environmentManifestShapeRoots profileEnvironmentManifestShape)

caseExactAdmission :: IO ()
caseExactAdmission = do
  assertEqual
    "canonical claims"
    (Right profileEnvironmentManifestShape)
    (checkEnvironmentManifestShape canonicalClaims)
  assertEqual
    "empty"
    (Left (EnvironmentManifestWrongRootCount 12 0))
    (checkEnvironmentManifestShape [])
  assertEqual
    "missing root"
    (Left (EnvironmentManifestWrongRootCount 12 11))
    (checkEnvironmentManifestShape (init canonicalClaims))
  case checkEnvironmentManifestShape (swapFirstPair canonicalClaims) of
    Left (EnvironmentManifestUnexpectedRoot 0 _ _) -> pure ()
    actual -> assertBool ("expected reordered-root rejection, got " <> show actual) False
  case checkEnvironmentManifestShape (take 1 canonicalClaims <> take 1 canonicalClaims <> drop 2 canonicalClaims) of
    Left EnvironmentManifestWrongRootCount {} -> pure ()
    Left EnvironmentManifestUnexpectedRoot {} -> pure ()
    actual -> assertBool ("expected duplicate/missing-root rejection, got " <> show actual) False

caseWriterSequencing :: IO ()
caseWriterSequencing =
  case checkEnvironmentManifestShape claims of
    Left (EnvironmentManifestUnexpectedRoot 0 expected actual) -> do
      assertEqual
        "expected root"
        (EnvironmentWriterRootClaimView SortDefinitionRole UnsequencedNabla)
        (environmentRootClaimView expected)
      assertEqual
        "actual root"
        (EnvironmentWriterRootClaimView SortDefinitionRole (NablaSequencedBy objectId))
        (environmentRootClaimView actual)
    result -> assertBool ("expected sequenced-writer rejection, got " <> show result) False
  where
    objectId = checked "global object" (mkGlobalObjectId (ByteString.replicate 32 0x7a))
    claims = environmentWriterRootClaim SortDefinitionRole (NablaSequencedBy objectId) : drop 1 canonicalClaims

caseCanonicalBytes :: IO ()
caseCanonicalBytes =
  assertEqual
    "canonical transcript"
    expected
    (environmentManifestShapeCanonicalBytes profileEnvironmentManifestShape)
  where
    expected :: ByteString
    expected =
      "ECLIPS-ENVIRONMENT-MANIFEST-SHAPE"
        <> ByteString.pack
          ( 12
              : concat
                [ [ordinal, predefinedSortRoleTag role, kindTag]
                | (ordinal, (role, kindTag)) <- zip [0 ..] canonicalRoleKinds
                ]
          )

canonicalClaims :: [EnvironmentRootClaim]
canonicalClaims =
  concatMap
    ( \role ->
        [ environmentWriterRootClaim role UnsequencedNabla,
          environmentReaderRootClaim role
        ]
    )
    canonicalRoles

expectedViews :: [EnvironmentRootClaimView]
expectedViews =
  concatMap
    ( \role ->
        [ EnvironmentWriterRootClaimView role UnsequencedNabla,
          EnvironmentReaderRootClaimView role
        ]
    )
    canonicalRoles

canonicalRoleKinds :: [(PredefinedSortRole, Word8)]
canonicalRoleKinds =
  concatMap (\role -> [(role, 0), (role, 1)]) canonicalRoles

canonicalRoles :: [PredefinedSortRole]
canonicalRoles =
  [ SortDefinitionRole,
    NeutralVertexRole,
    EdgeRole,
    NablaRole,
    DeltaRole,
    ProcessEpochRole
  ]

swapFirstPair :: [value] -> [value]
swapFirstPair (first : second : rest) = second : first : rest
swapFirstPair values = values

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
