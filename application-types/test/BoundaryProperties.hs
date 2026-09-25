{-# LANGUAGE OverloadedStrings #-}

module BoundaryProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    EnvironmentAccess,
    PredefinedAccess,
    StartupAccessError (StartupAccessRolesNotCanonical),
    allApplicationPredefinedSortRoles,
    applicationStartupAccess,
    environmentAccess,
    environmentAccessPredefined,
    predefinedAccess,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateObjectId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (..),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (DeleteReserved, PublishValue),
    WriteResult (SortDefinitionWritten, WriteAccepted),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "application boundary vocabulary"
    [ testCase "startup accepts an exact selected interface, including empty" caseStartupAccess,
      testCase "startup rejects invalid selected keys and role claims" caseStartupAccessRejectsShape,
      testCase "environment access retains all thirty-one graph objects without a process handle" caseEnvironmentAccess,
      testCase "environment access rejects incomplete, duplicated, or reordered roles" caseEnvironmentAccessRejectsShape,
      testCase "environment wiring has exact cardinality and distinct identities" caseEnvironmentAccessWiring,
      testCase "sort definitions use the generic publication arm" caseStructuredDefinition,
      testCase "reservation cancellation remains an outer write arm" caseDeleteReservedOuterArm,
      testCase "sort-definition success carries only its public SortId" caseSortDefinitionResult,
      testCase "reservation deletion success carries no identity" caseWriteAccepted
    ]

caseStartupAccess :: IO ()
caseStartupAccess = do
  process <- asPrivateProcessId <$> privateId 1
  accesses <- traverse accessFor (zip allApplicationPredefinedSortRoles [2 ..])
  environment <- checked "canonical environment" (environmentAccess accesses fixtureHub fixtureEdges)
  let selected = Access.primordialAccessFromSelection (Access.selectEnvironment environment)
      startup = applicationStartupAccess process selected
  assertEqual "process handle is retained" process (startupAccessProcess startup)
  assertEqual "exact selected access is retained" selected (Access.startupAccessPrimordial startup)
  empty <- checked "empty startup" (Access.primordialAccess [] Set.empty Nothing)
  assertEqual "empty interface is valid" Map.empty (Access.accessEntries empty)

caseStartupAccessRejectsShape :: IO ()
caseStartupAccessRejectsShape = do
  identity <- privateId 1
  let writer = Access.Writer (asPrivateNablaId identity)
  assertEqual
    "duplicate exact keys are rejected"
    (Left (Access.PrimordialDuplicateKey "x"))
    (Access.primordialSelection [("x", writer), ("x", writer)] Set.empty Nothing)
  assertEqual
    "inconsistent roles are rejected"
    (Left (Access.PrimordialInconsistentRoles identity))
    (Access.primordialSelection [("x", writer), ("y", Access.Reader (asPrivateDeltaId identity))] Set.empty Nothing)
  assertEqual
    "readiness cannot supply an omitted endpoint"
    (Left (Access.PrimordialRequiredEndpointNotSelected "missing"))
    (Access.primordialSelection [] (Set.singleton "missing") Nothing)

caseEnvironmentAccess :: IO ()
caseEnvironmentAccess = do
  accesses <- traverse accessFor (zip allApplicationPredefinedSortRoles [2 ..])
  environment <-
    checkedEnvironment
      "canonical environment access"
      (environmentAccess accesses fixtureHub fixtureEdges)
  assertEqual
    "the environment result contains exactly the canonical root catalogue"
    accesses
    (environmentAccessPredefined environment)
  assertEqual "hub is retained" fixtureHub (Access.environmentAccessHub environment)
  assertEqual "all eighteen supporting edges are retained in order" fixtureEdges (Access.environmentAccessEdges environment)
  let selected = Access.selectEnvironment environment
  assertEqual "all graph objects are selected" 31 (Map.size (Access.selectionEntries selected))
  assertEqual "only twelve endpoints are readiness requirements" 12 (Set.size (Access.selectionRequiredEndpoints selected))
  assertEqual "construction still uses two source writers" (Just (Access.environmentWriterKey NablaRole, Access.environmentWriterKey DeltaRole)) (Access.selectionEnvironmentSources selected)

caseEnvironmentAccessRejectsShape :: IO ()
caseEnvironmentAccessRejectsShape = do
  accesses <- traverse accessFor (zip allApplicationPredefinedSortRoles [2 ..])
  assertEqual
    "missing environment role is rejected"
    (Left (StartupAccessRolesNotCanonical (init allApplicationPredefinedSortRoles)))
    (environmentAccess (init accesses) fixtureHub fixtureEdges)
  assertEqual
    "environment role order is canonical rather than caller-selected"
    ( Left
        ( StartupAccessRolesNotCanonical
            [ NeutralVertexRole,
              SortDefinitionRole,
              EdgeRole,
              NablaRole,
              DeltaRole,
              ProcessEpochRole
            ]
        )
    )
    (environmentAccess (swapFirstTwo accesses) fixtureHub fixtureEdges)
  assertEqual
    "duplicated environment roles are rejected"
    ( Left
        ( StartupAccessRolesNotCanonical
            [ SortDefinitionRole,
              SortDefinitionRole,
              EdgeRole,
              NablaRole,
              DeltaRole,
              ProcessEpochRole
            ]
        )
    )
    (environmentAccess (duplicateFirst accesses) fixtureHub fixtureEdges)

caseEnvironmentAccessWiring :: IO ()
caseEnvironmentAccessWiring = do
  accesses <- traverse accessFor (zip allApplicationPredefinedSortRoles [2 ..])
  let create = environmentAccess accesses fixtureHub
  assertEqual "missing supporting edge is rejected" (Left (Access.EnvironmentAccessEdgeCount 17)) (create (init fixtureEdges))
  assertEqual "extra supporting edge is rejected" (Left (Access.EnvironmentAccessEdgeCount 19)) (create (fixtureHub : fixtureEdges))
  assertEqual "hub cannot also be an edge" (Left (Access.EnvironmentAccessRepeatedIdentity (Access.primordialEntryIdentity (Access.Object fixtureHub)))) (create (fixtureHub : drop 1 fixtureEdges))
  first <- accessFor (SortDefinitionRole, 2)
  let writer = Access.primordialEntryIdentity (Access.Writer (Access.predefinedWriter first))
  assertEqual "hub cannot alias an endpoint" (Left (Access.EnvironmentAccessRepeatedIdentity writer)) (environmentAccess accesses (asPrivateObjectId writer) fixtureEdges)

caseStructuredDefinition :: IO ()
caseStructuredDefinition = do
  sortId <- checked "predefined public identity" (mkSortId (ByteString.replicate 32 8))
  let definition = regularDefinition Nothing
      predefined = PredefinedSortDefinition SortDefinitionRole sortId
  assertEqual
    "structured syntax is retained as an application value"
    definition
    (publishedDefinition (PublishValue (SortDefinitionValue definition)))
  assertEqual
    "closed predefined syntax uses the same generic publication arm"
    predefined
    (publishedDefinition (PublishValue (SortDefinitionValue predefined)))

caseDeleteReservedOuterArm :: IO ()
caseDeleteReservedOuterArm = do
  object <- asPrivateObjectId <$> privateId 7
  assertEqual
    "the cancellation arm retains its nominal object operand"
    object
    (cancelledObject (DeleteReserved object))

caseSortDefinitionResult :: IO ()
caseSortDefinitionResult = do
  sortId <- checked "public sort identity" (mkSortId (ByteString.replicate 32 3))
  assertEqual
    "success returns the exact public bits"
    sortId
    (writtenSortId (SortDefinitionWritten sortId))

caseWriteAccepted :: IO ()
caseWriteAccepted =
  assertEqual "accepted write has one nullary representation" WriteAccepted acceptedWrite

regularDefinition :: Maybe SortId -> ApplicationSortDefinition
regularDefinition claimed =
  DeclaredSortDefinition
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
    claimed

accessFor ::
  (ApplicationPredefinedSortRole, Word64) ->
  IO PredefinedAccess
accessFor (role, offset) = do
  writer <- asPrivateNablaId <$> privateId (offset * 2)
  reader <- asPrivateDeltaId <$> privateId (offset * 2 + 1)
  pure (predefinedAccess role writer reader)

publishedDefinition :: ApplicationWriteValue -> ApplicationSortDefinition
publishedDefinition (PublishValue (SortDefinitionValue definition)) = definition
publishedDefinition (PublishValue _) = error "test expected a sort-definition value"
publishedDefinition (DeleteReserved _) = error "test expected a sort-definition publication"

cancelledObject :: ApplicationWriteValue -> PrivateObjectId
cancelledObject (DeleteReserved object) = object
cancelledObject (PublishValue _) = error "test expected a cancellation"

writtenSortId :: WriteResult -> SortId
writtenSortId (SortDefinitionWritten sortId) = sortId
writtenSortId WriteAccepted = error "test expected a sort-definition result"

acceptedWrite :: WriteResult
acceptedWrite = WriteAccepted

swapFirstTwo :: [value] -> [value]
swapFirstTwo (first : second : remaining) = second : first : remaining
swapFirstTwo values = values

duplicateFirst :: [value] -> [value]
duplicateFirst (first : _ : remaining) = first : first : remaining
duplicateFirst values = values

privateId :: Word64 -> IO PrivateUniqueId
privateId value = checked "positive private identity" (mkPrivateUniqueId value)

checked :: (Show problem) => String -> Either problem value -> IO value
checked description =
  either (fail . ((description <> ": ") <>) . show) pure

checkedEnvironment :: String -> Either StartupAccessError EnvironmentAccess -> IO EnvironmentAccess
checkedEnvironment = checked

fixtureHub :: PrivateObjectId
fixtureHub = asPrivateObjectId (either (error . show) id (mkPrivateUniqueId 1000))
fixtureEdges :: [PrivateObjectId]
fixtureEdges = [asPrivateObjectId (either (error . show) id (mkPrivateUniqueId value)) | value <- [1001 .. 1018]]
