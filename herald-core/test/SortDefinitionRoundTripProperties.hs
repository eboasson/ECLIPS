{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SortDefinitionRoundTripProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Application
import Eclips.Application.Types.Identity (PrivateUniqueId)
import Eclips.Application.Types.SortDescriptor qualified as Application
import Eclips.Application.Types.Typed qualified as Typed
import Eclips.Application.Types.Value (ApplicationLabel)
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Herald.Application.SortDefinition
  ( ApplicationSortDefinitionRejection (..),
    admitApplicationSortDefinition,
    admittedApplicationSortId,
    admittedApplicationSortValue,
    presentApplicationSortDefinition,
  )
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "sort-definition presentation"
    [ testCase "all six predefined definitions use the closed arm" casePredefinedRoundTrip,
      testProperty "declared definitions round-trip every selected fact" propDeclaredRoundTrip,
      testProperty "typed policies have the Herald's admitted canonical identity" propTypedCanonicalIdentity,
      testCase "an empty key list survives definition presentation" caseEmptyKeyRoundTrip,
      testCase "controlled declared facts remain application-defined" caseControlledRoundTrip,
      testCase "a predefined identity claim is still checked" casePredefinedClaimMismatch
    ]

-- Reifying the generated policy keeps the one-complete-sort-per-Haskell-type
-- contract while exercising independently lowered client and Herald descriptors.
-- The enum deliberately has a nonlexical declaration order.
data TypedMode = Zeta | Alpha
  deriving stock (Eq, Show, Generic)

instance Typed.ValueType TypedMode
instance Typed.Scalar TypedMode
instance Typed.QueryScalar TypedMode
instance Typed.DescriptorScalar TypedMode

data TypedNested = TypedNested
  { nestedText :: Text,
    nestedFlag :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Typed.ValueType TypedNested

data TypedRegular (policy :: Nat) = TypedRegular
  { sampleKey :: Text,
    sampleEnabled :: Bool,
    sampleCount :: Int64,
    sampleBytes :: ByteString,
    sampleIdentity :: PrivateUniqueId,
    sampleOptionalIdentity :: Maybe PrivateUniqueId,
    sampleLabel :: ApplicationLabel,
    sampleMode :: TypedMode,
    sampleNested :: TypedNested
  }
  deriving stock (Eq, Show, Generic)

instance (KnownNat policy) => Typed.ValueType (TypedRegular policy)

instance (KnownNat policy) => Typed.ApplicationSort (TypedRegular policy) where
  sortPolicy =
    Typed.withRetention retention
      . Typed.withRank
        [ Typed.ascending (Typed.field @"sampleMode"),
          countRank (Typed.field @"sampleCount"),
          Typed.ascending (Typed.field @"sampleIdentity"),
          Typed.ascending (Typed.field @"sampleLabel")
        ]
        direction
      . Typed.withValidity
        ( Typed.descriptorAll
            ( Typed.descriptorCompare (Typed.field @"sampleCount") Application.ScalarGreaterThanOrEqual (fromIntegral retention)
                :| [ Typed.descriptorAny
                       ( Typed.descriptorEqual (Typed.field @"sampleEnabled") (odd retention)
                           :| [Typed.descriptorEqual (Typed.field @"sampleBytes") (Text.encodeUtf8 text)]
                       ),
                     Typed.descriptorEqual (Typed.field @"sampleMode") (if odd retention then Alpha else Zeta),
                     Typed.descriptorEqual (Typed.composeField (Typed.field @"sampleNested") (Typed.field @"nestedText")) text
                   ]
            )
        )
      . Typed.withObsolescence (Typed.descriptorNot Typed.descriptorNever)
      $ Typed.regularPolicy [Typed.key (Typed.field @"sampleKey")]
    where
      retention = policyNumber @policy
      text = Text.pack (show retention)
      direction = if odd retention then Application.Descending else Application.Ascending
      countRank = if odd retention then Typed.descending else Typed.ascending

data TypedControlled (policy :: Nat) = TypedControlled
  { controlledObject :: PrivateUniqueId,
    controlledLabel :: ApplicationLabel,
    controlledNested :: TypedNested,
    controlledMode :: TypedMode
  }
  deriving stock (Eq, Show, Generic)

instance (KnownNat policy) => Typed.ValueType (TypedControlled policy)

instance (KnownNat policy) => Typed.ApplicationSort (TypedControlled policy) where
  type SortKindOf (TypedControlled policy) = 'Application.ControlledSort
  sortPolicy =
    Typed.withRetention retention
      . Typed.withImmutable (odd retention)
      . Typed.withRank [Typed.descending (Typed.field @"controlledMode")] Application.Ascending
      . Typed.withValidity
        (Typed.descriptorEqual (Typed.composeField (Typed.field @"controlledNested") (Typed.field @"nestedFlag")) (even retention))
      $ Typed.controlledPolicy (Typed.field @"controlledObject") (Just (Typed.field @"controlledLabel"))
    where
      retention = policyNumber @policy

policyNumber :: forall policy. (KnownNat policy) => Word64
policyNumber = fromIntegral (natVal (Proxy @policy))

propTypedCanonicalIdentity :: Word64 -> Property
propTypedCanonicalIdentity policy =
  case someNatVal (fromIntegral policy) of
    SomeNat (_ :: Proxy policy) ->
      conjoin
        [ typedCanonicalIdentity @(TypedRegular policy),
          typedCanonicalIdentity @(TypedControlled policy)
        ]

typedCanonicalIdentity :: forall a. (Typed.ApplicationSort a) => Property
typedCanonicalIdentity =
  case Typed.compileSort @a of
    Left problem -> counterexample ("typed compilation failed: " <> show problem) False
    Right compiled ->
      -- Omit the client identity claim so Herald independently computes its hash.
      case admitApplicationSortDefinition (Application.DeclaredSortDefinition (Typed.sortDescriptor compiled) Nothing) of
        Left problem -> counterexample ("Herald rejected the typed descriptor: " <> show problem) False
        Right admitted ->
          conjoin
            [ Typed.sortId compiled === admittedApplicationSortId admitted,
              presentApplicationSortDefinition (admittedApplicationSortValue admitted)
                === Right (Application.DeclaredSortDefinition (Typed.sortDescriptor compiled) (Just (Typed.sortId compiled)))
            ]

casePredefinedRoundTrip :: IO ()
casePredefinedRoundTrip =
  mapM_ assertPredefinedRoundTrip Application.allApplicationPredefinedSortRoles

assertPredefinedRoundTrip :: Application.ApplicationPredefinedSortRole -> IO ()
assertPredefinedRoundTrip role = do
  let sortId = Profile.profileSortFor (domainRole role)
      definition = Application.PredefinedSortDefinition role sortId
  admitted <- checked "predefined admission" (admitApplicationSortDefinition definition)
  presented <-
    checked
      "predefined presentation"
      (presentApplicationSortDefinition (admittedApplicationSortValue admitted))
  readmitted <- checked "presented predefined admission" (admitApplicationSortDefinition presented)
  assertEqual "the exact closed syntax is recovered" definition presented
  assertEqual
    "re-admission preserves the canonical carrier value"
    (admittedApplicationSortValue admitted)
    (admittedApplicationSortValue readmitted)

propDeclaredRoundTrip :: Word64 -> Bool -> Int64 -> Property
propDeclaredRoundTrip retention descending integer =
  case admitApplicationSortDefinition definition of
    Left problem -> counterexample ("declared admission failed: " <> show problem) False
    Right admitted ->
      case presentApplicationSortDefinition (admittedApplicationSortValue admitted) of
        Left problem -> counterexample ("declared presentation failed: " <> show problem) False
        Right presented ->
          case admitApplicationSortDefinition presented of
            Left problem -> counterexample ("presented admission failed: " <> show problem) False
            Right readmitted ->
              conjoin
                [ presented
                    === Application.DeclaredSortDefinition
                      descriptor
                      (Just (admittedApplicationSortId admitted)),
                  admittedApplicationSortValue readmitted
                    === admittedApplicationSortValue admitted
                ]
  where
    direction =
      if descending
        then Application.Descending
        else Application.Ascending
    descriptor =
      declaredDescriptor
        retention
        direction
        integer
    definition = Application.DeclaredSortDefinition descriptor Nothing

caseControlledRoundTrip :: IO ()
caseControlledRoundTrip = do
  let definition = Application.DeclaredSortDefinition controlledDescriptor Nothing
  admitted <- checked "controlled admission" (admitApplicationSortDefinition definition)
  presented <-
    checked
      "controlled presentation"
      (presentApplicationSortDefinition (admittedApplicationSortValue admitted))
  readmitted <- checked "presented controlled admission" (admitApplicationSortDefinition presented)
  assertEqual
    "ordinary controlled policy does not become a predefined role"
    ( Application.DeclaredSortDefinition
        controlledDescriptor
        (Just (admittedApplicationSortId admitted))
    )
    presented
  assertEqual
    "the controlled draft re-admits identically"
    (admittedApplicationSortValue admitted)
    (admittedApplicationSortValue readmitted)

caseEmptyKeyRoundTrip :: IO ()
caseEmptyKeyRoundTrip = do
  let descriptor =
        (declaredDescriptor 0 Application.Ascending 0)
          { Application.keyProjections = []
          }
      definition = Application.DeclaredSortDefinition descriptor Nothing
  admitted <- checked "empty-key admission" (admitApplicationSortDefinition definition)
  presented <-
    checked
      "empty-key presentation"
      (presentApplicationSortDefinition (admittedApplicationSortValue admitted))
  assertEqual
    "presented empty key"
    ( Application.DeclaredSortDefinition
        descriptor
        (Just (admittedApplicationSortId admitted))
    )
    presented

casePredefinedClaimMismatch :: IO ()
casePredefinedClaimMismatch = do
  let expected = Profile.profileSortFor Profile.SortDefinitionRole
      different = Profile.profileSortFor Profile.NeutralVertexRole
      definition =
        Application.PredefinedSortDefinition
          Application.SortDefinitionRole
          different
  assertEqual
    "the role, rather than the supplied identity, selects the descriptor"
    (Left (ApplicationDescriptorClaimedSortIdMismatch different expected))
    (admitApplicationSortDefinition definition)

declaredDescriptor ::
  Word64 ->
  Application.ApplicationRankDirection ->
  Int64 ->
  Application.ApplicationSortDescriptor
declaredDescriptor retention direction threshold =
  Application.ApplicationSortDescriptor
    { Application.sortKind = Application.RegularSort,
      Application.valueSchema =
        Application.RecordSchema
          ( Map.fromList
              [ ("key", Application.TextSchema),
                ("enabled", Application.BoolSchema),
                ("count", Application.Int64Schema),
                ("bytes", Application.BytesSchema),
                ("identity", Application.UniqueIdSchema),
                ("optional_identity", Application.OptionalUniqueIdSchema),
                ("label", Application.LabelSchema),
                ("mode", Application.EnumSchema ("zeta" :| ["alpha"])),
                ( "nested",
                  Application.RecordSchema
                    (Map.singleton "text" Application.TextSchema)
                )
              ]
          ),
      Application.keyProjections = [keyProjection],
      Application.validityPredicate =
        Application.AllPredicates
          ( Application.CompareField
              countProjection
              Application.ScalarGreaterThanOrEqual
              (Application.LiteralInt64 threshold)
              :| [ Application.AnyPredicates
                     ( Application.CompareField
                         enabledProjection
                         Application.ScalarEqual
                         (Application.LiteralBool True)
                         :| [ Application.CompareField
                                bytesProjection
                                Application.ScalarNotEqual
                                (Application.LiteralBytes "")
                            ]
                     ),
                   Application.CompareField
                     modeProjection
                     Application.ScalarEqual
                     (Application.LiteralEnum "alpha")
                 ]
          ),
      Application.obsolescencePredicate =
        Application.NotPredicate Application.NeverPredicate,
      Application.rankTerms =
        Application.RankField modeProjection Application.Ascending
          :| [ Application.RankField countProjection direction,
               Application.RankApplicationValue direction
             ],
      Application.minimumRetentionMicros = retention,
      Application.isImmutable = False,
      Application.labelField = Nothing
    }

controlledDescriptor :: Application.ApplicationSortDescriptor
controlledDescriptor =
  Application.ApplicationSortDescriptor
    { Application.sortKind = Application.ControlledSort,
      Application.valueSchema =
        Application.RecordSchema
          ( Map.fromList
              [ ("object", Application.UniqueIdSchema),
                ("label", Application.LabelSchema),
                ("payload", Application.TextSchema)
              ]
          ),
      Application.keyProjections = [objectProjection],
      Application.validityPredicate = Application.AlwaysPredicate,
      Application.obsolescencePredicate = Application.NeverPredicate,
      Application.rankTerms =
        Application.RankApplicationValue Application.Ascending :| [],
      Application.minimumRetentionMicros = 23,
      Application.isImmutable = True,
      Application.labelField = Just "label"
    }

keyProjection,
  enabledProjection,
  countProjection,
  bytesProjection,
  modeProjection,
  objectProjection ::
    Application.ApplicationProjection
keyProjection = Application.ApplicationProjection ("key" :| [])
enabledProjection = Application.ApplicationProjection ("enabled" :| [])
countProjection = Application.ApplicationProjection ("count" :| [])
bytesProjection = Application.ApplicationProjection ("bytes" :| [])
modeProjection = Application.ApplicationProjection ("mode" :| [])
objectProjection = Application.ApplicationProjection ("object" :| [])

domainRole :: Application.ApplicationPredefinedSortRole -> Profile.PredefinedSortRole
domainRole = \case
  Application.SortDefinitionRole -> Profile.SortDefinitionRole
  Application.NeutralVertexRole -> Profile.NeutralVertexRole
  Application.EdgeRole -> Profile.EdgeRole
  Application.NablaRole -> Profile.NablaRole
  Application.DeltaRole -> Profile.DeltaRole
  Application.ProcessEpochRole -> Profile.ProcessEpochRole

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (fail . ((context <> ": ") <>) . show) pure
