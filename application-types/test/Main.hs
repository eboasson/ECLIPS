module Main
  ( main,
  )
where

import BinaryProperties qualified
import BoundaryProperties qualified
import IdentityProperties qualified
import LifecycleProperties qualified
import LifetimeProperties qualified
import PrimordialProperties qualified
import QueryProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TypedProperties qualified
import ValueProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-application-types"
        [ BinaryProperties.tests,
          BoundaryProperties.tests,
          IdentityProperties.tests,
          LifecycleProperties.tests,
          LifetimeProperties.tests,
          PrimordialProperties.tests,
          QueryProperties.tests,
          TypedProperties.tests,
          ValueProperties.tests
        ]
    )
