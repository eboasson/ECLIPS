module PublicTcpFacade
  ( checkedListenEndpoint,
    optionalAdvertisedEndpoints,
  )
where

import Data.Text (Text)
import Data.Word (Word16)
import Eclips.Herald.Runtime.TCP
  ( HeraldTcpConfiguration,
    ResolvedTcpEndpoint,
    TcpEndpointError,
    TcpListenEndpoint,
    configureHeraldTcpAdvertisedEndpoints,
    tcpListenEndpoint,
  )

checkedListenEndpoint ::
  Text ->
  Word16 ->
  Either TcpEndpointError TcpListenEndpoint
checkedListenEndpoint = tcpListenEndpoint

optionalAdvertisedEndpoints ::
  Maybe ResolvedTcpEndpoint ->
  Maybe ResolvedTcpEndpoint ->
  HeraldTcpConfiguration ->
  HeraldTcpConfiguration
optionalAdvertisedEndpoints = configureHeraldTcpAdvertisedEndpoints
