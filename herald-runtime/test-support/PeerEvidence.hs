{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Semantic observations for runtime fixtures. Delivery metadata stays in the
-- recorded trace; these patterns let existing protocol assertions name the
-- payload independently of its receipt header and delivery sequence.
module PeerEvidence
  ( data SemanticSendPeerControl,
    data SemanticSendPeerItem,
    data SemanticPeerControlReceived,
    data SemanticPeerPublicationReceived,
    data SemanticPeerAlignmentControl,
  )
where

import Eclips.Herald.Discovery (PeerBinding)
import Eclips.Herald.EffectBatch (HeraldEffect)
import Eclips.Herald.EffectBatch qualified as Effect
import Eclips.Herald.Input (AlignmentControl, PeerControl, PeerIngress)
import Eclips.Herald.Input qualified as Input
import Eclips.Herald.PeerDispatch (PeerLogicalAttempt, PeerLogicalItem)

pattern SemanticSendPeerControl :: PeerBinding -> PeerControl -> HeraldEffect
pattern SemanticSendPeerControl binding control <- (sentControl -> Just (binding, control))
  where
    SemanticSendPeerControl binding control = Effect.SendPeerControl binding control

pattern SemanticSendPeerItem :: PeerBinding -> PeerLogicalAttempt -> HeraldEffect
pattern SemanticSendPeerItem binding attempt <- (sentItem -> Just (binding, attempt))
  where
    SemanticSendPeerItem binding attempt = Effect.SendPeerItem binding attempt

pattern SemanticPeerControlReceived :: PeerBinding -> PeerControl -> PeerIngress
pattern SemanticPeerControlReceived binding control <- (receivedControl -> Just (binding, control))
  where
    SemanticPeerControlReceived binding control = Input.PeerControlReceived binding control

pattern SemanticPeerPublicationReceived :: PeerBinding -> PeerLogicalItem -> PeerIngress
pattern SemanticPeerPublicationReceived binding item <- (receivedItem -> Just (binding, item))
  where
    SemanticPeerPublicationReceived binding item = Input.PeerPublicationReceived binding item

pattern SemanticPeerAlignmentControl :: AlignmentControl -> PeerControl
pattern SemanticPeerAlignmentControl control <- (alignmentControl -> Just control)
  where
    SemanticPeerAlignmentControl control = Input.PeerAlignmentControl control

sentControl :: HeraldEffect -> Maybe (PeerBinding, PeerControl)
sentControl = \case
  Effect.SendPeerControl binding control -> Just (binding, control)
  Effect.SendPeerControlWithProgress binding _ control -> Just (binding, control)
  _ -> Nothing

sentItem :: HeraldEffect -> Maybe (PeerBinding, PeerLogicalAttempt)
sentItem = \case
  Effect.SendPeerItem binding attempt -> Just (binding, attempt)
  Effect.SendPeerItemWithProgress binding _ attempt -> Just (binding, attempt)
  _ -> Nothing

receivedControl :: PeerIngress -> Maybe (PeerBinding, PeerControl)
receivedControl = \case
  Input.PeerControlReceived binding control -> Just (binding, control)
  Input.PeerControlReceivedWithProgress binding _ control -> Just (binding, control)
  _ -> Nothing

receivedItem :: PeerIngress -> Maybe (PeerBinding, PeerLogicalItem)
receivedItem = \case
  Input.PeerPublicationReceived binding item -> Just (binding, item)
  Input.PeerPublicationReceivedWithProgress binding _ item -> Just (binding, item)
  _ -> Nothing

alignmentControl :: PeerControl -> Maybe AlignmentControl
alignmentControl = \case
  Input.PeerAlignmentControl control -> Just control
  Input.PeerAlignmentEvidenceDelivered _ control -> Just control
  _ -> Nothing

{-# COMPLETE Input.PeerCandidateOpened, Input.PeerHelloReceived, Input.PeerDispatchSelected, SemanticPeerControlReceived, SemanticPeerPublicationReceived #-}
