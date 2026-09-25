-- | Configured-process administration listener and one-reader socket ownership.
--
-- This reconnectable EADM plane is deliberately separate from the private
-- one-shot orderly-drain registration retained by the composite facade.
module Eclips.Herald.Runtime.TCP.Internal.Administration
  ( runConfiguredAdministrationListener,
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception
  ( IOException,
    SomeException,
    finally,
    mask_,
    try,
  )
import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Eclips.Herald.Administration.RPC
  ( AdministrationOutbound (..),
  )
import Eclips.Herald.Runtime (HeraldRuntime)
import Eclips.Herald.Runtime.Connection
  ( ConfiguredAdministrationPlane,
    ConnectionRef,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    runtimeConfiguredAdministrationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerConfiguredAdministrationConnection,
    submitConfiguredAdministrationDto,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope (claimTcpSocket, newTcpSocketKey, releaseTcpSocket)
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( closeSocketOnce,
    prepareConnectedTcpSocket,
    receiveTcpSocketBytes,
    sendTcpSocketBytesDirect,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types (TcpContext)
import Eclips.Protocol.Admin.Frame
  ( AdminFrameContinuation (..),
    AdminFrameDirection (..),
    AdminFrameFeedResult (..),
    encodeAdminFrame,
    feedAdminFrame,
    initialAdminFrameDecoder,
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto,
    AdminEnvelope (..),
    AdminServerDto,
  )
import Network.Socket
  ( Socket,
    accept,
  )

-- | Accept and install reconnectable configured-administration leases until
-- the listener is closed.
runConfiguredAdministrationListener ::
  (IO () -> IO ()) ->
  TcpContext ->
  HeraldRuntime ->
  Socket ->
  IO ()
runConfiguredAdministrationListener spawn context runtime listener = loop
  where
    loop = do
      (connection, _) <- accept listener
      prepared <- try (prepareConnectedTcpSocket connection) :: IO (Either IOException ())
      case prepared of
        Left _ -> pure ()
        Right () -> installConfiguredAdministrationSocket spawn context runtime connection
      loop

installConfiguredAdministrationSocket ::
  (IO () -> IO ()) ->
  TcpContext ->
  HeraldRuntime ->
  Socket ->
  IO ()
installConfiguredAdministrationSocket spawn context runtime socketHandle = do
  closed <- newTVarIO False
  socketKey <- newTcpSocketKey
  let closeAction = mask_ (closeSocketOnce closed socketHandle >> releaseTcpSocket context socketKey)
      handlers =
        runtimeConfiguredAdministrationConnectionHandlers
          (writeAdministrationOutbound socketHandle)
          closeAction
  claimed <- claimTcpSocket context socketKey closeAction
  if not claimed
    then closeAction
    else do
      registration <- registerConfiguredAdministrationConnection runtime handlers
      case registration of
        RegistrationStopped -> closeAction
        ConnectionRegistered connectionRef ->
          spawn
            ( runConfiguredAdministrationReader runtime connectionRef socketHandle
                `finally` do
                  _ <- closeRuntimeConnection runtime connectionRef
                  closeAction
            )

writeAdministrationOutbound ::
  Socket ->
  AdministrationOutbound ->
  IO RuntimeLaneOffer
writeAdministrationOutbound socketHandle outbound =
  case administrationOutboundDto outbound of
    Nothing -> pure LaneOffered
    Just dto -> do
      result <-
        try
          ( sendTcpSocketBytesDirect
              socketHandle
              (encodeAdminFrame (AdminServerEnvelope dto))
          ) ::
          IO (Either SomeException ())
      pure $ case result of
        Left _ -> LaneLost
        Right () -> LaneOffered

administrationOutboundDto :: AdministrationOutbound -> Maybe AdminServerDto
administrationOutboundDto = \case
  AcceptAdministrationCandidate {} -> Nothing
  RejectAdministrationCandidate {} -> Nothing
  RejectEstablishedAdministration {} -> Nothing
  SendEstablishedAdministration _ dto -> Just dto

runConfiguredAdministrationReader ::
  HeraldRuntime ->
  ConnectionRef ConfiguredAdministrationPlane ->
  Socket ->
  IO ()
runConfiguredAdministrationReader runtime connectionRef socketHandle =
  loop (initialAdminFrameDecoder AdminClientFrames)
  where
    loop decoder = do
      chunk <- receiveTcpSocketBytes socketHandle 32768
      if ByteString.null chunk
        then pure ()
        else case feedAdminFrame decoder chunk of
          AdminFrameFeedResult envelopes continuation -> do
            continue <- foldM submitEnvelope True envelopes
            if continue
              then case continuation of
                NeedAdminFrameBytes successor -> loop successor
                AdminFrameFailed _ -> pure ()
              else pure ()
    submitEnvelope False _ = pure False
    submitEnvelope True envelope = case envelope of
      AdminClientEnvelope dto -> submitDto dto
      AdminServerEnvelope _ -> pure False
    submitDto :: AdminClientDto -> IO Bool
    submitDto dto = do
      result <- submitConfiguredAdministrationDto runtime connectionRef dto
      pure (result == Queued)
