module PublicDeploymentFacade
  ( checkLocalPlanes,
    checkRun,
    runResident,
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Eclips.Deployment.Configuration
import Eclips.Deployment.Manifest
import Eclips.Deployment.Runtime

checkLocalPlanes :: Text -> Word16 -> Maybe Text -> Either String DeploymentEndpoints
checkLocalPlanes = deploymentEndpoints

checkRun :: ByteString -> NonEmpty DeploymentEndpoints -> Either String CheckedDeployment
checkRun = checkDeployment

runResident :: CheckedDeployment -> Word64 -> (DeploymentRuntime -> IO value) -> IO value
runResident = withDeploymentResident
