module Main
  ( main,
  )
where

import AdmissionProperties qualified
import CanonicalProperties qualified
import DynamicProcessProperties qualified
import GenesisProperties qualified
import LabelProperties qualified
import LabelRetirementProperties qualified
import OracleLabelProperties qualified
import OracleModelProperties qualified
import ReceiptRetirementProperties qualified
import Step15ReferenceProperties qualified
import Step15WorkflowProperties qualified
import Step16DisappearanceProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TransitionProperties qualified
import VoterProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "oracle-core"
        [ VoterProperties.tests,
          AdmissionProperties.tests,
          CanonicalProperties.tests,
          DynamicProcessProperties.tests,
          GenesisProperties.tests,
          LabelProperties.tests,
          LabelRetirementProperties.tests,
          OracleLabelProperties.tests,
          OracleModelProperties.tests,
          ReceiptRetirementProperties.tests,
          Step15ReferenceProperties.tests,
          Step15WorkflowProperties.tests,
          Step16DisappearanceProperties.tests,
          TransitionProperties.tests
        ]
    )
