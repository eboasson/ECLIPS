{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Owner-only representation of logical peer discovery vocabulary.
--
-- The safe facade intentionally withholds binding and static-view constructors.
-- Only the Discovery owner may mint a binding generation; a runtime connection
-- identity is not part of any value in this module.
module Eclips.Herald.Discovery.Internal
  ( PeerAddress (..),
    ConnectionNonce (..),
    PeerCandidate (..),
    PeerCandidateOpened (..),
    PeerHello (..),
    CandidateHello (..),
    KnownHerald (..),
    PeerDialClass (..),
    PeerDialGeneration (..),
    PeerDialCancellationKey (..),
    PeerDialIntent (..),
    PeerBindingGeneration (..),
    firstPeerBindingGeneration,
    nextPeerBindingGeneration,
    PeerBinding (..),
    DiscoveryStatic (..),
    discoveryStatic,
    HelloRejection (..),
    BindingAdmission (..),
    PeerHelloDisposition (..),
    KnownContactsDisposition (..),
    DiscoveryInvariantFault (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Domain.Startup
  ( CatalogueDigest,
    HeraldMember (..),
    InitialProjectionDigest,
  )

-- | An uninterpreted address hint. Discovery never treats it as authority.
newtype PeerAddress = PeerAddress Text
  deriving stock (Eq, Ord, Show)

-- | Finite-run nominal connection correlation, with no security meaning.
newtype ConnectionNonce = ConnectionNonce Word64
  deriving stock (Eq, Ord, Show)

-- | The fully exchanged initiator tuple used to choose one logical binding.
data PeerCandidate = PeerCandidate
  { initiatorHeraldId :: HeraldId,
    initiatorHeraldEpoch :: HeraldEpoch,
    connectionNonce :: ConnectionNonce
  }
  deriving stock (Eq, Ord, Show)

-- | A logical local-candidate observation. It carries no physical lane ID.
data PeerCandidateOpened = PeerCandidateOpened
  { localConnectionNonce :: ConnectionNonce,
    advertisedAddresses :: Set PeerAddress,
    applicationLocator :: Maybe HeraldLocator
  }
  deriving stock (Eq, Show)

-- | The typed, codec-free peer handshake.
data PeerHello = PeerHello
  { systemId :: SystemId,
    heraldId :: HeraldId,
    heraldEpoch :: HeraldEpoch,
    connectionNonce :: ConnectionNonce,
    advertisedAddresses :: Set PeerAddress,
    appliedControlIndex :: ControlIndex,
    catalogueDigest :: CatalogueDigest,
    initialProjectionDigest :: InitialProjectionDigest,
    applicationLocator :: Maybe HeraldLocator
  }
  deriving stock (Eq, Show)

-- | The exact candidate tuple and Hello produced from one local observation.
data CandidateHello = CandidateHello PeerCandidate PeerHello
  deriving stock (Eq, Show)

-- | One non-authoritative known-Herald address observation.
data KnownHerald = KnownHerald
  { heraldId :: HeraldId,
    observedHeraldEpoch :: HeraldEpoch,
    addresses :: Set PeerAddress
  }
  deriving stock (Eq, Ord, Show)

-- | Pure ownership decision for one exact known-peer dial generation.
--
-- The preferred endpoint is the one whose @(HeraldId, HeraldEpoch)@ pair
-- precedes its target.  The TCP shell may interpret the fallback class with a
-- bounded delay, but cannot change this choice.
data PeerDialClass
  = ImmediatePeerDial
  | FallbackPeerDial
  deriving stock (Eq, Ord, Show)

-- | One pure permission generation for dialing an exact target incarnation.
-- The constructor remains owner-private; the runtime can only compare and
-- observe generations carried by admitted intents.
newtype PeerDialGeneration = PeerDialGeneration Word64
  deriving stock (Eq, Ord, Show)

-- | Stable identity of one Discovery-authorized dial generation. Address
-- hints may grow while this identity remains current and therefore do not
-- participate in cancellation matching.
data PeerDialCancellationKey
  = PeerDialCancellationKey HeraldId HeraldEpoch PeerDialGeneration
  deriving stock (Eq, Ord, Show)

-- | One Discovery-authorized request for direct physical contact.
--
-- The constructor remains owner-private: an address hint alone cannot grant
-- permission to dial a Herald.  The generation and immediate/fallback choice
-- are pure owner facts; the value contains no socket, attempt, retry, or
-- transport identity.
data PeerDialIntent
  = PeerDialIntent
      HeraldId
      HeraldEpoch
      (Set PeerAddress)
      PeerDialClass
      PeerDialGeneration
  deriving stock (Eq, Ord, Show)

-- | A positive, peer-qualified logical binding generation.
newtype PeerBindingGeneration = PeerBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

firstPeerBindingGeneration :: PeerBindingGeneration
firstPeerBindingGeneration = PeerBindingGeneration 1

-- | Advance a logical generation. Counter exhaustion is outside profile 0.1.
nextPeerBindingGeneration :: PeerBindingGeneration -> PeerBindingGeneration
nextPeerBindingGeneration (PeerBindingGeneration value) =
  PeerBindingGeneration (value + 1)

-- | A logical peer binding. It contains no physical connection reference.
data PeerBinding = PeerBinding
  { remoteHeraldId :: HeraldId,
    remoteHeraldEpoch :: HeraldEpoch,
    generation :: PeerBindingGeneration,
    selectedCandidate :: PeerCandidate
  }
  deriving stock (Eq, Ord, Show)

-- | Checked immutable facts needed for peer admission.
data DiscoveryStatic = DiscoveryStatic
  { systemId :: SystemId,
    localHeraldId :: HeraldId,
    localHeraldEpoch :: HeraldEpoch,
    heraldCatalogue :: Map HeraldEpoch HeraldId,
    initialMembership :: HeraldMembershipGeneration,
    catalogueDigest :: CatalogueDigest,
    initialProjectionDigest :: InitialProjectionDigest
  }
  deriving stock (Eq, Show)

-- | Construct the private admission view from already checked startup facts.
discoveryStatic ::
  SystemId ->
  HeraldId ->
  HeraldEpoch ->
  [HeraldMember] ->
  CatalogueDigest ->
  InitialProjectionDigest ->
  HeraldMembershipGeneration ->
  DiscoveryStatic
discoveryStatic systemId localId localEpoch members catalogueDigest projectionDigest initialMembership =
  DiscoveryStatic
    { systemId,
      localHeraldId = localId,
      localHeraldEpoch = localEpoch,
      heraldCatalogue =
        Map.fromList
          [ (heraldMemberEpoch member, heraldMemberId member)
          | member <- members
          ],
      initialMembership,
      catalogueDigest,
      initialProjectionDigest = projectionDigest
    }

-- | Ordinary candidate rejection categories.
data HelloRejection
  = HelloSystemMismatch
  | HelloCatalogueMismatch
  | HelloMembershipMismatch
  | HelloSelfConnection
  | HelloInactiveHerald
  | HelloRetiredHerald
  | HelloLocalHeraldRetired
  | HelloCandidateTupleInvalid
  | HelloCandidateNotSelected PeerCandidate
  deriving stock (Eq, Show)

data BindingAdmission
  = FirstBinding
  | ReofferedBinding
  | ReplacedBinding
  deriving stock (Eq, Ord, Show)

data PeerHelloDisposition
  = PeerHelloRejected HelloRejection
  | PeerHelloAccepted BindingAdmission PeerBinding
  deriving stock (Eq, Show)

data KnownContactsDisposition
  = KnownContactsChanged
  | KnownContactsUnchanged
  | KnownContactsStaleBinding
  deriving stock (Eq, Ord, Show)

-- | A compatible checked run cannot continue across this mismatch.
data DiscoveryInvariantFault
  = DiscoveryInitialProjectionMismatch
      InitialProjectionDigest
      InitialProjectionDigest
  | DiscoveryLocalMembershipContradiction
  | DiscoveryBindingMembershipContradiction HeraldEpoch
  | DiscoveryBindingGenerationContradiction HeraldEpoch
  | DiscoveryBindingCandidateContradiction HeraldEpoch
  | DiscoveryReconnectCatalogueContradiction HeraldEpoch
  | DiscoveryLocalHeraldRetired
  | DiscoveryMembershipNotExactSuccessor
  | DiscoveryMembershipOutsideCatalogue HeraldEpoch
  | DiscoveryAdmissionCatalogueContradiction HeraldEpoch
  deriving stock (Eq, Show)
