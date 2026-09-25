module JoinGateProperties (tests) where

import Data.Word (Word64)
import Eclips.Herald.Application.State qualified as Application
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "membership admission gate"
    [ testCase "membership pause is idempotent and reopening advances its generation" caseMembershipGate,
      testProperty "arbitrary membership changes match the admission state" propMembership
    ]

caseMembershipGate :: Assertion
caseMembershipGate = do
  let initial = Application.emptyState
      paused = Application.setApplicationMembershipGate True initial
      pausedAgain = Application.setApplicationMembershipGate True paused
      opened = Application.setApplicationMembershipGate False pausedAgain
      openedAgain = Application.setApplicationMembershipGate False opened
  assertEqual "membership closes admission" True (closed pausedAgain)
  assertEqual "repeated closure retains the generation" 1 (generation pausedAgain)
  assertEqual "membership release opens admission" False (closed openedAgain)
  assertEqual "repeated release does not advance again" 2 (generation openedAgain)

propMembership :: [Bool] -> Bool
propMembership commands = go False 1 Application.emptyState commands
  where
    go _ _ _ [] = True
    go previous count state (value : rest) =
      let nextCount = if previous && not value then count + 1 else count
          successor = Application.setApplicationMembershipGate value state
       in closed successor == value
            && generation successor == nextCount
            && go value nextCount successor rest

closed :: Application.State -> Bool
closed state = case Application.applicationGatePhase state of
  Application.ApplicationGateClosed _ -> True
  _ -> False

generation :: Application.State -> Word64
generation state = case Application.applicationGatePhase state of
  Application.ApplicationGateClosed value -> Application.applicationGateGenerationWord64 value
  Application.ApplicationGateOpen value -> Application.applicationGateGenerationWord64 value
