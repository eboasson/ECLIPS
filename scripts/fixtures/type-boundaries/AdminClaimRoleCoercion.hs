{-# LANGUAGE CPP #-}

module AdminClaimRoleCoercion where

import Data.Coerce (coerce)
import Eclips.Protocol.Admin.Types

#if defined(DEPLOYMENT_PROCESS)
invalidCoercion :: AdminDeploymentIdClaim -> AdminProcessIdClaim
#elif defined(PROCESS_EPOCH_PROCESS)
invalidCoercion :: AdminProcessEpochIdClaim -> AdminProcessIdClaim
#elif defined(HERALD_ATTACHMENT)
invalidCoercion :: AdminHeraldEpochClaim -> AdminApplicationAttachmentClaim
#elif defined(DEPLOYMENT_ATTACHMENT)
invalidCoercion :: AdminDeploymentIdClaim -> AdminApplicationAttachmentClaim
#else
#error "select one administration claim pair"
#endif
invalidCoercion = coerce
