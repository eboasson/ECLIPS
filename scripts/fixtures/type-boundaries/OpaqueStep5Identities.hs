{-# LANGUAGE CPP #-}

module OpaqueStep5Identities where

#if defined(WAIT_ID)
import Eclips.Herald.Application.Request (WaitId)

ownerOnlyIdentityConstructor = WaitId
#elif defined(REPLY_CURSOR)
import Eclips.Herald.Application.Request (ApplicationReplyCursor)

ownerOnlyIdentityConstructor = ApplicationReplyCursor
#elif defined(PROCESS_ACCEPTANCE_POSITION)
import Eclips.Herald.Application.Request (ProcessAcceptancePosition (..))

ownerOnlyIdentityConstructor = ProcessAcceptancePosition
#elif defined(PUBLICATION_POSITION)
import Eclips.Herald.Publication.State
  ( HeraldPublicationPosition (..),
    OutgoingPublicationCandidate,
    proposeOutgoingPublication,
  )

ownerOnlyIdentityConstructor = HeraldPublicationPosition
#else
#error "select one Step-5 opaque identity fixture"
#endif
