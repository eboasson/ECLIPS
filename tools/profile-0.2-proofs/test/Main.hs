module Main (main) where

import LabelGenerationContracts qualified
import LifecycleContracts qualified
import MembershipContracts qualified
import RaftContracts qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "profile 0.2 P00 proof contracts"
      [ RaftContracts.tests,
        LifecycleContracts.tests,
        MembershipContracts.tests,
        LabelGenerationContracts.tests
      ]
