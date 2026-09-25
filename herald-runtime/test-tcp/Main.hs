module Main (main) where

import AdministrationTcpProperties qualified
import ApplicationTcpProperties qualified
import HeartbeatTcpProperties qualified
import PeerManagerTcpProperties qualified
import PeerTcpProperties qualified
import SocketProgressProperties qualified
import Test.Tasty (defaultMain, testGroup)
import ThreeHeraldTcpProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-herald-runtime TCP"
        [ AdministrationTcpProperties.tests,
          ApplicationTcpProperties.tests,
          HeartbeatTcpProperties.tests,
          PeerManagerTcpProperties.tests,
          PeerTcpProperties.tests,
          SocketProgressProperties.tests,
          ThreeHeraldTcpProperties.tests
        ]
    )
