module Main
  ( main,
  )
where

import AlignmentProperties qualified
import CanonicalGoldenProperties qualified
import ContextProperties qualified
import DescriptorProperties qualified
import DisappearanceProperties qualified
import EnvironmentProperties qualified
import FoundationProperties qualified
import LabelProperties qualified
import MembershipProperties qualified
import ProcessLifecycleProperties qualified
import QueryProperties qualified
import SortDefinitionValueProperties qualified
import SortOccurrenceProperties qualified
import StartupProperties qualified
import StoreProperties qualified
import StructuralProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TopologyProperties qualified
import ValueProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-domain"
        [ AlignmentProperties.tests,
          CanonicalGoldenProperties.tests,
          ContextProperties.tests,
          DescriptorProperties.tests,
          DisappearanceProperties.tests,
          EnvironmentProperties.tests,
          FoundationProperties.tests,
          LabelProperties.tests,
          MembershipProperties.tests,
          ProcessLifecycleProperties.tests,
          QueryProperties.tests,
          SortDefinitionValueProperties.tests,
          SortOccurrenceProperties.tests,
          StartupProperties.tests,
          StoreProperties.tests,
          StructuralProperties.tests,
          TopologyProperties.tests,
          ValueProperties.tests
        ]
    )
