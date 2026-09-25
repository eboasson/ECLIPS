-- | Atomic receipt reclamation across ordinary and lifecycle request owners.
module Eclips.Herald.UseCase.ApplicationRetirement
  ( applyApplicationReceiptRetirement,
  )
where

import Eclips.Application.Types.Lifetime (lifecycleReceiptRetirement)
import Eclips.Herald.Application.Request (ApplicationRequestReply (ApplicationReceiptsRetired))
import Eclips.Herald.Application.Session (ApplicationSessionRejection (ApplicationReceiptRetirementNotReady))
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (EstablishedApplicationDisposition),
    EffectBatch,
    HeraldEffect (RejectApplicationConnection, SendApplicationReply),
    emptyEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Input
  ( ApplicationLifecycleIngress (CallApplicationLifecycle),
    ApplicationReceiptRetirementIngress (..),
    ApplicationRequestIngress (CallApplicationRequest),
    ApplicationRetirementWork (..),
    HeraldInputBody (ApplicationLifecycleInput, ApplicationRequestInput),
  )
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldApplicationTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupProcessPreparationState,
    startupApplicationState,
    startupProcessPreparationState,
  )

-- | A rejected announcement changes neither owner and cannot admit its attached
-- call. The caller commits these successors and the normalized work atomically.
applyApplicationReceiptRetirement ::
  ApplicationReceiptRetirementIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch, Maybe HeraldInputBody)
applyApplicationReceiptRetirement (RetireApplicationReceipts binding session progress work) predecessor =
  case Application.prepareApplicationReceiptRetirement binding session progress application of
    Left (Application.ApplicationSessionRejected reason) -> reject reason
    Left _ -> contradiction
    Right prepared ->
      let successorApplication = Application.commitApplicationReceiptRetirement prepared
       in case Application.applicationReceiptRetirementProgress session successorApplication of
            Nothing -> contradiction
            Just merged -> case retireLifecycle merged of
              Left Preparation.PreparationReceiptRetirementNotReady -> reject ApplicationReceiptRetirementNotReady
              Left _ -> contradiction
              Right preparation ->
                let successor = replaceStartupApplicationState successorApplication (replaceStartupProcessPreparationState preparation predecessor)
                    advanced = Just merged /= Application.applicationReceiptRetirementProgress session application
                    acknowledgement =
                      if advanced || work == Nothing
                        then singletonEffectBatch (SendApplicationReply binding (ApplicationReceiptsRetired merged))
                        else emptyEffectBatch
                 in Right (successor, acknowledgement, normalize <$> work)
  where
    application = startupApplicationState predecessor
    retireLifecycle merged
      | lifecycleReceiptRetirement merged /= maybe mempty lifecycleReceiptRetirement (Application.applicationReceiptRetirementProgress session application) =
          Preparation.retireLifecycleReceipts session (lifecycleReceiptRetirement merged) (startupProcessPreparationState predecessor)
      | otherwise = Right (startupProcessPreparationState predecessor)
    reject reason = Right (predecessor, singletonEffectBatch (RejectApplicationConnection (EstablishedApplicationDisposition binding) reason), Nothing)
    normalize = \case
      RetiringApplicationRequest request operation -> ApplicationRequestInput (CallApplicationRequest binding session request operation)
      RetiringApplicationLifecycle request locator command -> ApplicationLifecycleInput (CallApplicationLifecycle binding session request locator command)
    contradiction = Left (HeraldTransitionInvariant HeraldApplicationTransitionContradiction)
