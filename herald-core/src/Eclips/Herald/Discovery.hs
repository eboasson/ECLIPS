-- | Safe nominal and read-only vocabulary for pure peer discovery.
--
-- Addresses remain hints. A 'PeerBinding' is a logical kernel correlation, not a
-- socket, connection ID, authentication credential, or membership grant.
module Eclips.Herald.Discovery
  ( PeerAddress,
    peerAddress,
    peerAddressText,
    ConnectionNonce,
    connectionNonce,
    connectionNonceWord64,
    PeerCandidate,
    peerCandidate,
    peerCandidateInitiatorHeraldId,
    peerCandidateInitiatorHeraldEpoch,
    peerCandidateConnectionNonce,
    PeerCandidateOpened,
    peerCandidateOpened,
    peerCandidateOpenedConnectionNonce,
    peerCandidateOpenedAdvertisedAddresses,
    peerCandidateOpenedApplicationLocator,
    PeerHello,
    peerHello,
    peerHelloSystemId,
    peerHelloHeraldId,
    peerHelloHeraldEpoch,
    peerHelloConnectionNonce,
    peerHelloAdvertisedAddresses,
    peerHelloAppliedControlIndex,
    peerHelloCatalogueDigest,
    peerHelloInitialProjectionDigest,
    peerHelloApplicationLocator,
    CandidateHello,
    candidateHelloCandidate,
    candidateHelloMessage,
    KnownHerald,
    knownHerald,
    knownHeraldId,
    knownHeraldObservedEpoch,
    knownHeraldAddresses,
    PeerDialClass (..),
    PeerDialIntent,
    PeerDialCancellationKey,
    peerDialIntentCancellationKey,
    peerDialIntentHeraldId,
    peerDialIntentHeraldEpoch,
    peerDialIntentAddresses,
    peerDialIntentClass,
    peerDialIntentGenerationWord64,
    PeerBindingGeneration,
    peerBindingGenerationWord64,
    PeerBinding,
    peerBindingRemoteHeraldId,
    peerBindingRemoteHeraldEpoch,
    peerBindingGeneration,
    peerBindingSelectedCandidate,
    HelloRejection (..),
    BindingAdmission (..),
    PeerHelloDisposition (..),
    KnownContactsDisposition (..),
    DiscoveryInvariantFault (..),
  )
where

import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    SystemId,
  )
import Eclips.Domain.Startup (CatalogueDigest, InitialProjectionDigest)
import Eclips.Herald.Discovery.Internal
  ( BindingAdmission (..),
    CandidateHello (..),
    ConnectionNonce (..),
    DiscoveryInvariantFault (..),
    HelloRejection (..),
    KnownContactsDisposition (..),
    KnownHerald (..),
    PeerAddress (..),
    PeerBinding (..),
    PeerBindingGeneration (..),
    PeerCandidate (..),
    PeerCandidateOpened (..),
    PeerDialCancellationKey (..),
    PeerDialClass (..),
    PeerDialGeneration (..),
    PeerDialIntent (..),
    PeerHello (..),
    PeerHelloDisposition (..),
  )

peerAddress :: Text -> PeerAddress
peerAddress = PeerAddress

peerAddressText :: PeerAddress -> Text
peerAddressText (PeerAddress address) = address

connectionNonce :: Word64 -> ConnectionNonce
connectionNonce = ConnectionNonce

connectionNonceWord64 :: ConnectionNonce -> Word64
connectionNonceWord64 (ConnectionNonce value) = value

peerCandidate :: HeraldId -> HeraldEpoch -> ConnectionNonce -> PeerCandidate
peerCandidate = PeerCandidate

peerCandidateInitiatorHeraldId :: PeerCandidate -> HeraldId
peerCandidateInitiatorHeraldId (PeerCandidate identifier _ _) = identifier

peerCandidateInitiatorHeraldEpoch :: PeerCandidate -> HeraldEpoch
peerCandidateInitiatorHeraldEpoch (PeerCandidate _ epoch _) = epoch

peerCandidateConnectionNonce :: PeerCandidate -> ConnectionNonce
peerCandidateConnectionNonce (PeerCandidate _ _ nonce) = nonce

peerCandidateOpened :: ConnectionNonce -> Set PeerAddress -> Maybe HeraldLocator -> PeerCandidateOpened
peerCandidateOpened = PeerCandidateOpened

peerCandidateOpenedConnectionNonce :: PeerCandidateOpened -> ConnectionNonce
peerCandidateOpenedConnectionNonce (PeerCandidateOpened nonce _ _) = nonce

peerCandidateOpenedAdvertisedAddresses :: PeerCandidateOpened -> Set PeerAddress
peerCandidateOpenedAdvertisedAddresses (PeerCandidateOpened _ addresses _) = addresses

peerCandidateOpenedApplicationLocator :: PeerCandidateOpened -> Maybe HeraldLocator
peerCandidateOpenedApplicationLocator (PeerCandidateOpened _ _ locator) = locator

-- | Construct an untrusted inbound typed claim for admission or a test fixture.
-- Outbound local Hello values are authored only by the Discovery owner from
-- checked static facts, whether prompted by a candidate-open observation or an
-- accepted remote-initiated Hello.
peerHello ::
  SystemId ->
  HeraldId ->
  HeraldEpoch ->
  ConnectionNonce ->
  Set PeerAddress ->
  ControlIndex ->
  CatalogueDigest ->
  InitialProjectionDigest ->
  Maybe HeraldLocator ->
  PeerHello
peerHello = PeerHello

peerHelloSystemId :: PeerHello -> SystemId
peerHelloSystemId (PeerHello system _ _ _ _ _ _ _ _) = system

peerHelloHeraldId :: PeerHello -> HeraldId
peerHelloHeraldId (PeerHello _ identifier _ _ _ _ _ _ _) = identifier

peerHelloHeraldEpoch :: PeerHello -> HeraldEpoch
peerHelloHeraldEpoch (PeerHello _ _ epoch _ _ _ _ _ _) = epoch

peerHelloConnectionNonce :: PeerHello -> ConnectionNonce
peerHelloConnectionNonce (PeerHello _ _ _ nonce _ _ _ _ _) = nonce

peerHelloAdvertisedAddresses :: PeerHello -> Set PeerAddress
peerHelloAdvertisedAddresses (PeerHello _ _ _ _ addresses _ _ _ _) = addresses

peerHelloAppliedControlIndex :: PeerHello -> ControlIndex
peerHelloAppliedControlIndex (PeerHello _ _ _ _ _ index _ _ _) = index

peerHelloCatalogueDigest :: PeerHello -> CatalogueDigest
peerHelloCatalogueDigest (PeerHello _ _ _ _ _ _ digest _ _) = digest

peerHelloInitialProjectionDigest :: PeerHello -> InitialProjectionDigest
peerHelloInitialProjectionDigest (PeerHello _ _ _ _ _ _ _ digest _) = digest

peerHelloApplicationLocator :: PeerHello -> Maybe HeraldLocator
peerHelloApplicationLocator (PeerHello _ _ _ _ _ _ _ _ locator) = locator

candidateHelloCandidate :: CandidateHello -> PeerCandidate
candidateHelloCandidate (CandidateHello candidate _) = candidate

candidateHelloMessage :: CandidateHello -> PeerHello
candidateHelloMessage (CandidateHello _ hello) = hello

knownHerald :: HeraldId -> HeraldEpoch -> Set PeerAddress -> KnownHerald
knownHerald = KnownHerald

knownHeraldId :: KnownHerald -> HeraldId
knownHeraldId (KnownHerald identifier _ _) = identifier

knownHeraldObservedEpoch :: KnownHerald -> HeraldEpoch
knownHeraldObservedEpoch (KnownHerald _ epoch _) = epoch

knownHeraldAddresses :: KnownHerald -> Set PeerAddress
knownHeraldAddresses (KnownHerald _ _ addresses) = addresses

peerDialIntentHeraldId :: PeerDialIntent -> HeraldId
peerDialIntentHeraldId (PeerDialIntent identifier _ _ _ _) = identifier

peerDialIntentHeraldEpoch :: PeerDialIntent -> HeraldEpoch
peerDialIntentHeraldEpoch (PeerDialIntent _ epoch _ _ _) = epoch

peerDialIntentAddresses :: PeerDialIntent -> Set PeerAddress
peerDialIntentAddresses (PeerDialIntent _ _ addresses _ _) = addresses

-- | Read the pure owner's immediate/fallback decision.  The intent constructor
-- remains hidden, so applications and runtimes cannot forge dial authority.
peerDialIntentClass :: PeerDialIntent -> PeerDialClass
peerDialIntentClass (PeerDialIntent _ _ _ dialClass _) = dialClass

-- | Read the opaque pure dial generation for runtime coalescing and tracing.
peerDialIntentGenerationWord64 :: PeerDialIntent -> Word64
peerDialIntentGenerationWord64
  (PeerDialIntent _ _ _ _ (PeerDialGeneration generation)) = generation

peerDialIntentCancellationKey :: PeerDialIntent -> PeerDialCancellationKey
peerDialIntentCancellationKey
  (PeerDialIntent identifier epoch _ _ generation) =
    PeerDialCancellationKey identifier epoch generation

peerBindingGenerationWord64 :: PeerBindingGeneration -> Word64
peerBindingGenerationWord64 (PeerBindingGeneration value) = value

peerBindingRemoteHeraldId :: PeerBinding -> HeraldId
peerBindingRemoteHeraldId (PeerBinding identifier _ _ _) = identifier

peerBindingRemoteHeraldEpoch :: PeerBinding -> HeraldEpoch
peerBindingRemoteHeraldEpoch (PeerBinding _ epoch _ _) = epoch

peerBindingGeneration :: PeerBinding -> PeerBindingGeneration
peerBindingGeneration (PeerBinding _ _ generation _) = generation

peerBindingSelectedCandidate :: PeerBinding -> PeerCandidate
peerBindingSelectedCandidate (PeerBinding _ _ _ candidate) = candidate
