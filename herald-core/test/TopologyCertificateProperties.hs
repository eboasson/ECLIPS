module TopologyCertificateProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Herald.Graph.Protocol
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (vector)

tests :: TestTree
tests =
  testGroup
    "retained topology certificates"
    [ testProperty "complete cut and reporter evidence round-trips canonically"
        $ forAll (chooseInt (1, 50))
        $ \prefix ->
          let certificate = fixture prefix 60
           in decodeTopologyCutEstablishedCanonicalBytes (topologyCutEstablishedCanonicalBytes certificate) === Right certificate,
      testProperty "reporter presentation order has one canonical transcript"
        $ forAll (shuffle [0, 1, 2])
        $ \order ->
          let certificate = fixture 5 60
              reports = topologyCutEstablishedAcceptances certificate
              reordered = checked (topologyCutEstablished (topologyCutEstablishedId certificate) (topologyCutEstablishedCut certificate) [reports !! index | index <- order])
           in topologyCutEstablishedCanonicalBytes reordered === topologyCutEstablishedCanonicalBytes certificate,
      testProperty "the transcript retains report evidence beyond the cut digest"
        $ forAll (chooseInt (1, 50))
        $ \prefix ->
          let first = fixture prefix 60
              later = fixture prefix 61
           in conjoin
                [ topologyCutEstablishedId first === topologyCutEstablishedId later,
                  property (topologyCutEstablishedCanonicalBytes first /= topologyCutEstablishedCanonicalBytes later)
                ],
      testProperty "trailing bytes cannot become certificate evidence"
        $ forAll (chooseInt (1, 50))
        $ \prefix ->
          property (isLeft (decodeTopologyCutEstablishedCanonicalBytes (topologyCutEstablishedCanonicalBytes (fixture prefix 60) <> Bytes.singleton 0)))
    ]

fixture :: Int -> Int -> TopologyCutEstablished
fixture prefix reportControl =
  checked
    ( topologyCutEstablished
        identifier
        cut
        [topologyCutAcceptance identifier (structuralAppliedReport member vector (controlIndex (fromIntegral reportControl))) | member <- members]
    )
  where
    firstMember = checked (mkHeraldEpoch (Bytes.replicate 32 1))
    remainingMembers = [checked (mkHeraldEpoch (Bytes.replicate 32 byte)) | byte <- [2, 3]]
    members = firstMember : remainingMembers
    membership = checked (genesisHeraldMembershipGeneration (checked (mkSystemId (Bytes.replicate 32 4))) (firstMember :| remainingMembers))
    vector = checked (mkStructuralVersionVector membership [(member, structuralPrefixThrough (checked (mkStructuralSequence (fromIntegral prefix)))) | member <- members])
    cut =
      checked
        ( topologyCut
            (sameGenerationPredecessor (checked (mkTopologyCutId (Bytes.replicate 32 5))))
            (topologyFrontier vector (controlIndex 50))
            (checked (mkTopologyOccurrenceDigest (Bytes.replicate 32 6)))
        )
    identifier = deriveTopologyCutId cut

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
