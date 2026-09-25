module IdentityProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Application.Types.Identity
  ( PrivateIdError (PrivateIdIsZero),
    PrivateUniqueId,
    SortIdError
      ( WrongSortIdByteCount,
        actualSortIdByteCount,
        expectedSortIdByteCount
      ),
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    initialPrivateUniqueIdSupply,
    mkPrivateUniqueId,
    mkSortId,
    privateDeltaUniqueId,
    privateNablaUniqueId,
    privateObjectUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
    sortIdBytes,
    takePrivateUniqueId,
  )
import Eclips.Public.Types.SortId qualified as PublicSortId
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Positive (getPositive),
    Property,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "application identity"
    [ testCase "zero is reserved"
        $ mkPrivateUniqueId 0 @?= Left PrivateIdIsZero,
      testCase "an allocation supply starts at one and advances" caseAllocationSupply,
      testCase "public SortId is re-exported unchanged" casePublicSortId,
      testProperty "checked positive representations round-trip" propRepresentationRoundTrip,
      testProperty "object refinement round-trips" propObjectRoundTrip,
      testProperty "process refinement round-trips" propProcessRoundTrip,
      testProperty "nabla refinement round-trips" propNablaRoundTrip,
      testProperty "delta refinement round-trips" propDeltaRoundTrip
    ]

propRepresentationRoundTrip :: Positive Word64 -> Property
propRepresentationRoundTrip positive =
  let value = getPositive positive
   in (privateUniqueIdWord64 <$> mkPrivateUniqueId value) === Right value

propObjectRoundTrip :: Positive Word64 -> Property
propObjectRoundTrip = roleRoundTrip (privateObjectUniqueId . asPrivateObjectId)

propProcessRoundTrip :: Positive Word64 -> Property
propProcessRoundTrip = roleRoundTrip (privateProcessUniqueId . asPrivateProcessId)

propNablaRoundTrip :: Positive Word64 -> Property
propNablaRoundTrip = roleRoundTrip (privateNablaUniqueId . asPrivateNablaId)

propDeltaRoundTrip :: Positive Word64 -> Property
propDeltaRoundTrip = roleRoundTrip (privateDeltaUniqueId . asPrivateDeltaId)

roleRoundTrip :: (PrivateUniqueId -> PrivateUniqueId) -> Positive Word64 -> Property
roleRoundTrip roundTrip positive =
  case mkPrivateUniqueId (getPositive positive) of
    Left problem -> counterexample (show problem) False
    Right identifier -> roundTrip identifier === identifier

caseAllocationSupply :: IO ()
caseAllocationSupply = do
  let (first, afterFirst) = takePrivateUniqueId initialPrivateUniqueIdSupply
      (second, _) = takePrivateUniqueId afterFirst
  fmap privateUniqueIdWord64 [first, second] @?= [1, 2]

casePublicSortId :: IO ()
casePublicSortId = do
  let bytes = ByteString.pack [0 .. 31]
      throughApplication = mkSortId bytes
      throughPublic = PublicSortId.mkSortId bytes
  throughApplication @?= throughPublic
  case throughPublic of
    Left problem -> fail ("public SortId fixture failed: " <> show problem)
    Right publicIdentifier -> sortIdBytes publicIdentifier @?= bytes
  mkSortId (ByteString.replicate 31 0)
    @?= Left
      WrongSortIdByteCount
        { expectedSortIdByteCount = 32,
          actualSortIdByteCount = 31
        }
