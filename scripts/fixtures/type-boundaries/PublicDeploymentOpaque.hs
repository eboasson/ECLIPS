{-# LANGUAGE CPP #-}

module PublicDeploymentOpaque where

#if defined(DEPLOYMENT_ENDPOINT_CONSTRUCTOR)
import Eclips.Deployment.Configuration (Endpoint(..))
forged :: Endpoint
forged = Endpoint undefined undefined
#elif defined(DEPLOYMENT_PLANES_CONSTRUCTOR)
import Eclips.Deployment.Configuration (DeploymentEndpoints(..))
forged :: DeploymentEndpoints
forged = DeploymentEndpoints undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
#elif defined(DEPLOYMENT_CHECKED_CONSTRUCTOR)
import Eclips.Deployment.Manifest (CheckedDeployment(..))
forged :: CheckedDeployment
forged = CheckedDeployment undefined undefined undefined undefined undefined
#elif defined(DEPLOYMENT_RESIDENT_CONSTRUCTOR)
import Eclips.Deployment.Manifest (ResidentDeployment(..))
forged :: ResidentDeployment
forged = ResidentDeployment undefined undefined undefined undefined undefined
#elif defined(DEPLOYMENT_RUNTIME_CONSTRUCTOR)
import Eclips.Deployment.Runtime (DeploymentRuntime(..))
forged :: DeploymentRuntime
forged = DeploymentRuntime undefined undefined undefined
#elif defined(DEPLOYMENT_ENDPOINT_RECORD)
import Eclips.Deployment.Configuration (Endpoint, endpointHost)
forged :: Endpoint -> Endpoint
forged value = value {endpointHost = undefined}
#elif defined(DEPLOYMENT_PLANES_RECORD)
import Eclips.Deployment.Configuration (DeploymentEndpoints, applicationListen)
forged :: DeploymentEndpoints -> DeploymentEndpoints
forged value = value {applicationListen = undefined}
#elif defined(DEPLOYMENT_CHECKED_RECORD)
import Eclips.Deployment.Manifest (CheckedDeployment, deploymentResidents)
forged :: CheckedDeployment -> CheckedDeployment
forged value = value {deploymentResidents = undefined}
#elif defined(DEPLOYMENT_RESIDENT_RECORD)
import Eclips.Deployment.Manifest (ResidentDeployment, residentEndpoints)
forged :: ResidentDeployment -> ResidentDeployment
forged value = value {residentEndpoints = undefined}
#elif defined(DEPLOYMENT_RUNTIME_RECORD)
import Eclips.Deployment.Runtime (DeploymentRuntime, deploymentResident)
forged :: DeploymentRuntime -> DeploymentRuntime
forged value = value {deploymentResident = undefined}
#else
#error "select one deployment opacity probe"
#endif
