module PublicStep15Facade
  ( checkedApplicationRecovery,
    checkedMembership,
    checkedPeerRecovery,
    timerAttemptWord64,
    timerDeadline,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)
import Eclips.Application.Client.Recovery
  ( ApplicationClientRecoveryConfiguration,
    ApplicationClientRecoveryConfigurationError,
    applicationClientRecoveryConfiguration,
  )
import Eclips.Domain.Identity (HeraldEpoch, SystemId)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    MembershipGenerationProblem,
    genesisHeraldMembershipGeneration,
  )
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError,
    checkPeerRecoveryConfiguration,
  )
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerSpec,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerSpecAbsoluteDeadline,
  )

checkedApplicationRecovery ::
  Word64 ->
  Word64 ->
  Either
    ApplicationClientRecoveryConfigurationError
    ApplicationClientRecoveryConfiguration
checkedApplicationRecovery = applicationClientRecoveryConfiguration

checkedMembership ::
  SystemId ->
  NonEmpty HeraldEpoch ->
  Either MembershipGenerationProblem HeraldMembershipGeneration
checkedMembership = genesisHeraldMembershipGeneration

checkedPeerRecovery ::
  Word64 -> Either PeerRecoveryConfigurationError PeerRecoveryConfiguration
checkedPeerRecovery = checkPeerRecoveryConfiguration

timerAttemptWord64 :: TimerAttempt -> Word64
timerAttemptWord64 = timerAttemptGenerationWord64 . timerAttemptGeneration

timerDeadline :: TimerSpec -> MonotonicInstant
timerDeadline = timerSpecAbsoluteDeadline
