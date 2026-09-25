module Main
  ( main,
  )
where

import AdministrationCancellationProperties qualified
import AdministrationReceiptRetirementProperties qualified
import AdministrationRpcProperties qualified
import AdministrationTransitionProperties qualified
import AlignmentCoordinatorProperties qualified
import AlignmentGenerationProperties qualified
import AlignmentGenerationWorkProperties qualified
import AlignmentLossProperties qualified
import AlignmentPlanProperties qualified
import AlignmentProtocolProperties qualified
import AlignmentRecoveryProperties qualified
import AlignmentTransferWorkProperties qualified
import ApplicationCallProperties qualified
import ApplicationEnvironmentProperties qualified
import ApplicationInitialClaimProperties qualified
import ApplicationLabelProperties qualified
import ApplicationOwnerProperties qualified
import ApplicationPublicationProperties qualified
import ApplicationRecoveryProperties qualified
import ApplicationRequestProperties qualified
import ApplicationRetirementProperties qualified
import ApplicationRpcProperties qualified
import ApplicationSessionProperties qualified
import ApplicationValueProperties qualified
import BootstrapProperties qualified
import CompoundTerminalSourceProperties qualified
import ConfiguredProcessProperties qualified
import ControlledRemovalProperties qualified
import ControlledReservationProperties qualified
import DiagnosticsProperties qualified
import DisappearanceAlignmentRaceProperties qualified
import DisappearanceEvidenceProperties qualified
import DisappearanceGateProperties qualified
import DisappearanceLiveProperties qualified
import DisappearanceLocalAlignmentProperties qualified
import DisappearanceMembershipProperties qualified
import DisappearanceRaceProperties qualified
import DiscoveryProperties qualified
import EffectivePublicationProperties qualified
import EnvironmentControlledProperties qualified
import EnvironmentPublicationProperties qualified
import EnvironmentSuccessorProperties qualified
import FailureDetectionProperties qualified
import ForwardProperties qualified
import GenesisProperties qualified
import GraphAdmissionProperties qualified
import GraphProperties qualified
import HeraldInvariantProperties qualified
import HeraldTransitionProperties qualified
import IdGeneratorProperties qualified
import InitialBootstrapProperties qualified
import InitialHeraldProperties qualified
import IsolationStateProperties qualified
import JoinAlignmentHistoryProperties qualified
import JoinControlTailProperties qualified
import JoinGateProperties qualified
import JoinHistoryProperties qualified
import JoinSourceBundleProperties qualified
import LabelBarrierProperties qualified
import LabelCollectionProperties qualified
import LabelPatchProperties qualified
import LiveAlignmentPlanProperties qualified
import MembershipSuccessorProgressProperties qualified
import NewEnvironmentProperties qualified
import OracleAdvanceProperties qualified
import OracleClientProperties qualified
import OracleHealthProperties qualified
import OracleProjectionMembershipProperties qualified
import OracleVoterProjectionProperties qualified
import PeerDeliveryCoordinatorProperties qualified
import PeerDeliveryProperties qualified
import PeerDialIntentProperties qualified
import PeerInputProperties qualified
import PeerLivenessProperties qualified
import PeerReceiptRetirementProperties qualified
import PeerRpcProperties qualified
import PeerStoreProperties qualified
import PeerStreamProperties qualified
import PlacementStep7Properties qualified
import PreparationWorkIndexProperties qualified
import PreparedProperties qualified
import PrimordialGrantProperties qualified
import PrivateIdentityProperties qualified
import ProcessEndProperties qualified
import ProcessPreparationProperties qualified
import ProcessPreparationReceiptRetirementProperties qualified
import ProcessPreparationWorkProperties qualified
import ProjectionBaseAdmissionProperties qualified
import ProjectionBaseCodecProperties qualified
import ProjectionDisappearanceBaseProperties qualified
import ProjectionLabelBaseProperties qualified
import ProjectionVoterFailureBaseProperties qualified
import PublicationGroupIntegrationProperties qualified
import PublicationGroupsProperties qualified
import PublicationProperties qualified
import ReceiptRetirementClientProperties qualified
import RegularDefinitionStoreRetirementProperties qualified
import RegularSortRetirementProperties qualified
import RemoteProcessPreparationProperties qualified
import SortDefinitionProperties qualified
import SortDefinitionRoundTripProperties qualified
import Step11GeneratedIdentityProperties qualified
import Step15FailureVerticalProperties qualified
import Step15MembershipAdvanceProperties qualified
import Step15RetirementClosureProperties qualified
import Step15StructuralBaseProperties qualified
import Step15StructuralReleaseProperties qualified
import Step16RegularRetirementAcceptanceProperties qualified
import Step5SemanticProperties qualified
import Step7PairProperties qualified
import Step8ApplicationGateProperties qualified
import StoreObservationProperties qualified
import StructuralProgressProperties qualified
import StructuralPublicationProperties qualified
import StructuralReconciliationProperties qualified
import StructuralSettlementProperties qualified
import TerminalSourceHoldProperties qualified
import TerminalSourceUnionProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TopologyCertificateProperties qualified
import VisibilityProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-herald-core"
        [ JoinGateProperties.tests,
          JoinControlTailProperties.tests,
          JoinHistoryProperties.tests,
          JoinSourceBundleProperties.tests,
          JoinAlignmentHistoryProperties.tests,
          AlignmentRecoveryProperties.tests,
          AlignmentCoordinatorProperties.tests,
          AlignmentGenerationProperties.tests,
          AlignmentPlanProperties.tests,
          LiveAlignmentPlanProperties.tests,
          DiagnosticsProperties.tests,
          AlignmentGenerationWorkProperties.tests,
          AlignmentTransferWorkProperties.tests,
          AlignmentLossProperties.tests,
          AlignmentProtocolProperties.tests,
          ApplicationCallProperties.tests,
          ApplicationEnvironmentProperties.tests,
          ApplicationLabelProperties.tests,
          ApplicationOwnerProperties.tests,
          ApplicationPublicationProperties.tests,
          ApplicationRecoveryProperties.tests,
          ApplicationRetirementProperties.tests,
          ApplicationRequestProperties.tests,
          ApplicationRpcProperties.tests,
          AdministrationCancellationProperties.tests,
          AdministrationReceiptRetirementProperties.tests,
          AdministrationRpcProperties.tests,
          AdministrationTransitionProperties.tests,
          ApplicationSessionProperties.tests,
          ApplicationInitialClaimProperties.tests,
          ApplicationValueProperties.tests,
          GenesisProperties.tests,
          GraphProperties.tests,
          GraphAdmissionProperties.tests,
          TopologyCertificateProperties.tests,
          InitialBootstrapProperties.tests,
          BootstrapProperties.tests,
          ControlledRemovalProperties.tests,
          ControlledReservationProperties.tests,
          ConfiguredProcessProperties.tests,
          ProcessPreparationProperties.tests,
          PreparationWorkIndexProperties.tests,
          ProcessPreparationWorkProperties.tests,
          ProcessPreparationReceiptRetirementProperties.tests,
          RemoteProcessPreparationProperties.tests,
          DiscoveryProperties.tests,
          DisappearanceEvidenceProperties.tests,
          DisappearanceLiveProperties.tests,
          DisappearanceMembershipProperties.tests,
          DisappearanceRaceProperties.tests,
          DisappearanceAlignmentRaceProperties.tests,
          DisappearanceLocalAlignmentProperties.tests,
          DisappearanceGateProperties.tests,
          EffectivePublicationProperties.tests,
          EnvironmentControlledProperties.tests,
          EnvironmentPublicationProperties.tests,
          EnvironmentSuccessorProperties.tests,
          FailureDetectionProperties.tests,
          ForwardProperties.tests,
          InitialHeraldProperties.tests,
          IsolationStateProperties.tests,
          LabelCollectionProperties.tests,
          LabelBarrierProperties.tests,
          LabelPatchProperties.tests,
          MembershipSuccessorProgressProperties.tests,
          NewEnvironmentProperties.tests,
          OracleClientProperties.tests,
          ReceiptRetirementClientProperties.tests,
          OracleHealthProperties.tests,
          HeraldInvariantProperties.tests,
          HeraldTransitionProperties.tests,
          IdGeneratorProperties.tests,
          OracleAdvanceProperties.tests,
          OracleProjectionMembershipProperties.tests,
          ProjectionBaseAdmissionProperties.tests,
          ProjectionBaseCodecProperties.tests,
          ProjectionVoterFailureBaseProperties.tests,
          ProjectionDisappearanceBaseProperties.tests,
          ProjectionLabelBaseProperties.tests,
          OracleVoterProjectionProperties.tests,
          PeerStoreProperties.tests,
          PeerStreamProperties.tests,
          PeerReceiptRetirementProperties.tests,
          PeerDeliveryProperties.tests,
          PeerDeliveryCoordinatorProperties.tests,
          PeerInputProperties.tests,
          PeerDialIntentProperties.tests,
          PeerLivenessProperties.tests,
          PeerRpcProperties.tests,
          PlacementStep7Properties.tests,
          PreparedProperties.tests,
          PrimordialGrantProperties.tests,
          PrivateIdentityProperties.tests,
          ProcessEndProperties.tests,
          PublicationProperties.tests,
          PublicationGroupsProperties.tests,
          PublicationGroupIntegrationProperties.tests,
          RegularDefinitionStoreRetirementProperties.tests,
          RegularSortRetirementProperties.tests,
          SortDefinitionProperties.tests,
          SortDefinitionRoundTripProperties.tests,
          Step5SemanticProperties.tests,
          Step7PairProperties.tests,
          Step8ApplicationGateProperties.tests,
          Step11GeneratedIdentityProperties.tests,
          Step15FailureVerticalProperties.tests,
          Step15MembershipAdvanceProperties.tests,
          Step15RetirementClosureProperties.tests,
          Step15StructuralBaseProperties.tests,
          Step15StructuralReleaseProperties.tests,
          Step16RegularRetirementAcceptanceProperties.tests,
          TerminalSourceUnionProperties.tests,
          CompoundTerminalSourceProperties.tests,
          TerminalSourceHoldProperties.tests,
          StoreObservationProperties.tests,
          StructuralPublicationProperties.tests,
          StructuralProgressProperties.tests,
          StructuralReconciliationProperties.tests,
          StructuralSettlementProperties.tests,
          VisibilityProperties.tests
        ]
    )
