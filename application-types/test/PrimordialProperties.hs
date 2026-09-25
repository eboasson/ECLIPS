{-# LANGUAGE OverloadedStrings #-}

module PrimordialProperties (tests) where

import Data.Binary (decodeOrFail, encode, put)
import Data.Binary.Put (runPut)
import Data.ByteString qualified as StrictBytes
import Data.ByteString.Lazy qualified as Bytes
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Identity
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), Property, counterexample, forAll, shuffle, testProperty)

tests :: TestTree
tests =
  testGroup
    "selected primordial interface"
    [ testProperty "arbitrary distinct exact keys normalize and round-trip every entry kind" propCanonical,
      testProperty "verified endpoint sorts survive canonical startup serialization" propEndpointSorts,
      testProperty "identity/object aliases can coexist with one typed role" propAliases,
      testProperty "a complete environment selects its own thirty-one distinct objects" propEnvironmentSelection,
      testCase "object sort evidence survives startup transfer and rejects inconsistent aliases" caseObjectSortMetadata,
      testCase "source keys must select two distinct writers" caseSources,
      testCase "endpoint sort metadata is complete and consistent across aliases" caseEndpointSortMetadata,
      testCase "wire admission rejects noncanonical keys, duplicate keys, roles, and readiness" caseMalformed
    ]

propCanonical :: NonNegative Int -> Property
propCanonical (NonNegative count) =
  counterexample "selected entry map or canonical codec changed"
    $ and
      [ Access.selectionEntries selected == Map.fromList entries,
        encode selected == encode reversed,
        decodeSelection (encode selected) == Right selected,
        Access.accessEntries (Access.primordialAccessFromSelection selected) == Map.fromList entries
      ]
  where
    entries = [(Text.pack (show index), entry index) | index <- [0 .. count `mod` 24]]
    selected = checked (Access.primordialSelection entries Set.empty Nothing)
    reversed = checked (Access.primordialSelection (reverse entries) Set.empty Nothing)
    entry index = case index `mod` 5 of
      0 -> Access.Identity (private index)
      1 -> Access.Object (Identity.asPrivateObjectId (private index))
      2 -> Access.Writer (Identity.asPrivateNablaId (private index))
      3 -> Access.Reader (Identity.asPrivateDeltaId (private index))
      _ -> Access.Process (Identity.asPrivateProcessId (private index))

propEndpointSorts :: NonNegative Int -> Property
propEndpointSorts (NonNegative seed) =
  counterexample "startup endpoint sorts were discarded or assigned to non-endpoints"
    $ and
      [ Access.accessEndpointSorts access == sorts,
        Access.accessEntries access == Map.fromList entries,
        encode access == encode reversed,
        decodeAccess (encode access) == Right access,
        Access.accessEndpointSorts (Access.primordialAccessFromSelection selected) == Map.empty
      ]
  where
    count = seed `mod` 24
    entries = [(Text.pack (show index), entry index) | index <- [0 .. count]]
    entry index = case index `mod` 3 of
      0 -> Access.Writer (Identity.asPrivateNablaId (private index))
      1 -> Access.Reader (Identity.asPrivateDeltaId (private index))
      _ -> Access.Identity (private index)
    sorts = Map.fromList [(Text.pack (show index), sortId index) | index <- [0 .. count], index `mod` 3 /= 2]
    access = checked (Access.primordialAccessWithEndpointSorts entries Set.empty Nothing sorts)
    reversed = checked (Access.primordialAccessWithEndpointSorts (reverse entries) Set.empty Nothing sorts)
    selected = checked (Access.primordialSelection entries Set.empty Nothing)

propEnvironmentSelection :: Property
propEnvironmentSelection = forAll (shuffle [0 .. 30]) $ \indices ->
  let ids = fmap private indices
      pairs = zipWith (\role (writer, reader) -> Access.predefinedAccess role (Identity.asPrivateNablaId writer) (Identity.asPrivateDeltaId reader)) Access.allApplicationPredefinedSortRoles (zip (take 6 ids) (take 6 (drop 6 ids)))
      hub = Identity.asPrivateObjectId (ids !! 12)
      edges = fmap Identity.asPrivateObjectId (drop 13 ids)
      environment = checked (Access.environmentAccess pairs hub edges)
      selected = Access.selectEnvironment environment
      entries = Access.selectionEntries selected
      required = Access.selectionRequiredEndpoints selected
   in counterexample "environment selection changed its graph membership or endpoint requirements"
        $ Map.size entries == 31
          && Set.fromList (fmap Access.primordialEntryIdentity (Map.elems entries)) == Set.fromList ids
          && Map.lookup Access.environmentHubKey entries == Just (Access.Object hub)
          && [Map.lookup (Access.environmentEdgeKey role direction) entries | role <- Access.allApplicationPredefinedSortRoles, direction <- Access.allEnvironmentEdgeRoles] == fmap (Just . Access.Object) edges
          && Set.size required == 12
          && all (\key -> case Map.lookup key entries of Just (Access.Writer _) -> True; Just (Access.Reader _) -> True; _ -> False) (Set.toList required)
          && Access.selectionEnvironmentSources selected == Just (Access.environmentWriterKey Access.NablaRole, Access.environmentWriterKey Access.DeltaRole)

caseObjectSortMetadata :: IO ()
caseObjectSortMetadata = do
  let object = Access.Object (Identity.asPrivateObjectId (private 1))
      entries = [("hub", object), ("alias", object), ("name", Access.Identity (private 2))]
      sorts = Map.fromList [("hub", sortId 1), ("alias", sortId 1)]
      construct = Access.primordialAccessWithSorts entries Set.empty Nothing Map.empty
      access = checked (construct sorts)
      selected = checked (Access.primordialSelection entries Set.empty Nothing)
      wire objects = runPut (put selected >> put ([] :: [(Text, Identity.SortId)]) >> put (objects :: [(Text, Identity.SortId)]))
  assertEqual "object metadata survives current-build startup serialization" (Right access) (decodeAccess (encode access))
  assertEqual "object metadata is exposed separately from endpoint carried sorts" sorts (Access.accessObjectSorts access)
  assertEqual "mere identities do not carry object evidence" (Left (Access.PrimordialObjectSortKeyNotSelected "name")) (construct (Map.insert "name" (sortId 1) sorts))
  assertEqual "object aliases cannot disagree" (Left (Access.PrimordialObjectSortAliasMismatch (private 1))) (construct (Map.insert "alias" (sortId 2) sorts))
  mapM_
    (assertBool "malformed object metadata decoded" . either (const True) (const False) . decodeAccess . wire)
    [reverse (Map.toAscList sorts), ("alias", sortId 1) : Map.toAscList sorts, Map.toAscList (Map.insert "alias" (sortId 2) sorts), Map.toAscList (Map.insert "missing" (sortId 1) sorts)]

propAliases :: NonNegative Int -> Bool
propAliases (NonNegative index) = case Access.primordialSelection
  [("Name", Access.Identity identity), ("name", Access.Object (Identity.asPrivateObjectId identity)), (" name", Access.Process (Identity.asPrivateProcessId identity))]
  Set.empty
  Nothing of
  Right selected -> Map.size (Access.selectionEntries selected) == 3
  Left _ -> False
  where
    identity = private (index `mod` 10000)

caseSources :: IO ()
caseSources = do
  let writer = Access.Writer (Identity.asPrivateNablaId (private 1))
      other = Access.Writer (Identity.asPrivateNablaId (private 2))
      entries = [("n", writer), ("d", other)]
  assertEqual
    "missing source cannot create authority"
    (Left (Access.PrimordialEnvironmentSourceNotWriter "missing"))
    (Access.primordialSelection entries Set.empty (Just ("n", "missing")))
  assertEqual
    "two keys naming one writer cannot be the source pair"
    (Left Access.PrimordialEnvironmentSourcesCoincide)
    (Access.primordialSelection [("n", writer), ("d", writer)] Set.empty (Just ("n", "d")))
  assertBool
    "sources may be passive and need not be required"
    (case Access.primordialSelection entries Set.empty (Just ("n", "d")) of Right _ -> True; Left _ -> False)

caseEndpointSortMetadata :: IO ()
caseEndpointSortMetadata = do
  let writer = Access.Writer (Identity.asPrivateNablaId (private 1))
      reader = Access.Reader (Identity.asPrivateDeltaId (private 2))
      entries = [("n", writer), ("alias", writer), ("d", reader), ("name", Access.Identity (private 3))]
      sorts = Map.fromList [("n", sortId 1), ("alias", sortId 1), ("d", sortId 2)]
      construct = Access.primordialAccessWithEndpointSorts entries Set.empty Nothing
      selected = checked (Access.primordialSelection entries Set.empty Nothing)
      invalidMaps = [Map.delete "d" sorts, Map.insert "name" (sortId 3) sorts, Map.insert "alias" (sortId 9) sorts]
      wire values = runPut (put selected >> put (values :: [(Text, Identity.SortId)]) >> put ([] :: [(Text, Identity.SortId)]))
  assertEqual "each alias retains the identical carried sort" (Right sorts) (Access.accessEndpointSorts <$> construct sorts)
  assertEqual
    "one private endpoint cannot carry two sorts"
    (Left (Access.PrimordialEndpointSortAliasMismatch (private 1)))
    (construct (Map.insert "alias" (sortId 9) sorts))
  mapM_ (assertBool "invalid endpoint metadata admitted" . either (const True) (const False) . construct) invalidMaps
  mapM_ (assertBool "invalid endpoint metadata decoded" . either (const True) (const False) . decodeAccess . wire . Map.toAscList) invalidMaps
  mapM_
    (assertBool "noncanonical endpoint metadata decoded" . either (const True) (const False) . decodeAccess . wire)
    [reverse (Map.toAscList sorts), ("alias", sortId 1) : Map.toAscList sorts]

caseMalformed :: IO ()
caseMalformed = do
  let writer = Access.Writer (Identity.asPrivateNablaId (private 1))
      reader = Access.Reader (Identity.asPrivateDeltaId (private 1))
      bytes entries required sources = runPut (put (entries :: [(Text, Access.PrimordialEntry)]) >> put (required :: [Text]) >> put (sources :: Maybe (Text, Text)))
      bad =
        [ bytes [("z", writer), ("a", writer)] [] Nothing,
          bytes [("a", writer), ("a", writer)] [] Nothing,
          bytes [("a", writer), ("b", reader)] [] Nothing,
          bytes [("a", writer)] ["a", "a"] Nothing,
          bytes [] ["a"] Nothing,
          bytes [("a", writer)] [] (Just ("a", "a"))
        ]
  mapM_ (assertBool "malformed selected carrier decoded" . either (const True) (const False) . decodeSelection) bad

private :: Int -> Identity.PrivateUniqueId
private index = checked (Identity.mkPrivateUniqueId (fromIntegral index + 1))
sortId :: Int -> Identity.SortId
sortId index = checked (Identity.mkSortId (StrictBytes.replicate 32 (fromIntegral index)))
checked :: (Show error) => Either error value -> value
checked = either (error . show) id

decodeSelection :: Bytes.ByteString -> Either String Access.PrimordialSelection
decodeSelection bytes = case decodeOrFail bytes of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, value) | Bytes.null remaining -> Right value
  Right _ -> Left "trailing bytes"

decodeAccess :: Bytes.ByteString -> Either String Access.PrimordialAccess
decodeAccess bytes = case decodeOrFail bytes of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, value) | Bytes.null remaining -> Right value
  Right _ -> Left "trailing bytes"
