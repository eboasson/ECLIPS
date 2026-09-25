module Eclips.TypeBoundaries.Spec
  ( LogicalUnit (..),
    ProbeExpectation (..),
    RejectionClass (..),
    ProbeSpec (..),
    cabalPrivateProbeSelectors,
    expectedCabalPrivateProbeCount,
    expectedNegativeProbeCount,
    expectedPositiveProbeCount,
    negativeProbes,
    positiveProbes,
    publicUnits,
    fixtureRelativeRoot,
  )
where

data LogicalUnit = LogicalUnit
  { packageName :: String,
    componentName :: Maybe String
  }
  deriving stock (Eq, Ord, Show)

data ProbeExpectation
  = MustReject RejectionClass [String]
  | MustSucceed
  deriving stock (Eq, Show)

data RejectionClass
  = CoercionFailure
  | HiddenImport
  | MissingInstance
  | TypeMismatch
  | UnavailableName
  deriving stock (Eq, Ord, Show)

data ProbeSpec = ProbeSpec
  { probeId :: String,
    fixtureName :: FilePath,
    cppSelector :: Maybe String,
    expectation :: ProbeExpectation
  }
  deriving stock (Eq, Show)

fixtureRelativeRoot :: FilePath
fixtureRelativeRoot = "scripts/fixtures/type-boundaries"

publicUnits :: [LogicalUnit]
publicUnits =
  fmap
    (`LogicalUnit` Nothing)
    [ "eclips-public-types",
      "eclips-application-types",
      "eclips-domain",
      "eclips-protocol-frame",
      "eclips-protocol-application",
      "eclips-protocol-admin",
      "eclips-protocol-peer",
      "eclips-raft-core",
      "eclips-oracle-core",
      "eclips-protocol-raft",
      "eclips-protocol-oracle",
      "eclips-herald-core",
      "eclips-application-client",
      "eclips-application-api",
      "eclips-oracle-runtime",
      "eclips-herald-runtime",
      "eclips-deployment"
    ]
    <> [LogicalUnit "eclips-herald-runtime" (Just "tcp")]

expectedNegativeProbeCount :: Int
expectedNegativeProbeCount = 217

expectedPositiveProbeCount :: Int
expectedPositiveProbeCount = 11

expectedCabalPrivateProbeCount :: Int
expectedCabalPrivateProbeCount = 2

cabalPrivateProbeSelectors :: [(FilePath, String)]
cabalPrivateProbeSelectors =
  [ ("PublicRuntimeInternalImport.hs", "RUNTIME_PRIVATE_DEPENDENCY"),
    ("PublicStep10InternalImport.hs", "TCP_PRIVATE_DEPENDENCY")
  ]

negativeProbes :: [ProbeSpec]
negativeProbes =
  concat
    [ selected
        UnavailableName
        "profile02/oracle-voter-opacity"
        "PublicOracleVoterOpaque.hs"
        [ ("ORACLE_ACCEPTED_FAILURE_CONSTRUCTOR", ["Illegal term-level use", "AcceptedVoterHostFailure"]),
          ("ORACLE_ACCEPTED_FAILURE_UPDATE", ["Not in scope: record field", "acceptedVoterHostFailureConfiguration"]),
          ("ORACLE_RUNTIME_HEALTH_CONSTRUCTOR", ["Illegal term-level use", "OracleRuntimeHealth"]),
          ("ORACLE_HEALTH_DTO_CONSTRUCTOR", ["Illegal term-level use", "OracleHealthReplyDto"]),
          ("ORACLE_REPLICA_ENDPOINT_CONSTRUCTOR", ["Illegal term-level use", "OracleReplicaEndpoint"]),
          ("ORACLE_REPLICA_REGISTRATION_CONSTRUCTOR", ["Illegal term-level use", "OracleReplicaRegistration"]),
          ("ORACLE_REPLICA_REGISTRATION_UPDATE", ["Not in scope: record field", "replicaRegistrationHost"]),
          ("ORACLE_VOTER_BINDINGS_CONSTRUCTOR", ["Illegal term-level use", "OracleVoterBindings"]),
          ("ORACLE_VOTER_CONFIGURATION_CONSTRUCTOR", ["Illegal term-level use", "VoterConfiguration"]),
          ("ORACLE_VOTER_CONFIGURATION_UPDATE", ["Not in scope: record field", "voterConfigurationNativeRef"]),
          ("ORACLE_VOTER_CHANGE_CONSTRUCTOR", ["Illegal term-level use", "VoterChange"]),
          ("ORACLE_VOTER_CHANGE_UPDATE", ["Not in scope: record field", "voterChangePhase"]),
          ("ORACLE_VOTER_PRIVATE_APPLY", ["does not export", "applyCommittedVoterState"])
        ],
      selected
        CoercionFailure
        "profile02/oracle-voter-opacity"
        "PublicOracleVoterOpaque.hs"
        [("ORACLE_VOTER_BINDINGS_COERCION", ["coerce", "OracleVoterBindings"])],
      selected
        HiddenImport
        "profile02/oracle-voter-opacity"
        "PublicOracleVoterOpaque.hs"
        [ ("ORACLE_VOTER_INTERNAL_IMPORT", ["Eclips.Oracle.Internal.Voter"]),
          ("ORACLE_ACCEPTED_FAILURE_INTERNAL_IMPORT", ["Eclips.Oracle.Internal.Failure"])
        ],
      selected
        TypeMismatch
        "profile02/oracle-voter-origin"
        "PublicOracleVoterOpaque.hs"
        [("ORACLE_CONFIGURATION_IS_CLIENT_COMMAND", ["AppliedOracleEntry", "AppliedOracleCommandEntry"])],
      selected
        UnavailableName
        "profile02/raft-reconfiguration-opacity"
        "PublicRaftReconfigurationOpaque.hs"
        [ ("RAFT_VOTER_SET_CONSTRUCTOR", ["Illegal term-level use", "RaftVoterSet"]),
          ("RAFT_STABLE_CONSTRUCTOR", ["Data constructor not in scope", "StableRaftConfiguration"]),
          ("RAFT_JOINT_CONSTRUCTOR", ["Data constructor not in scope", "JointRaftConfiguration"]),
          ("RAFT_CONFIGURATION_REF_CONSTRUCTOR", ["Data constructor not in scope", "RaftConfigurationEntryRef"]),
          ("RAFT_FRONTIER_CONSTRUCTOR", ["Illegal term-level use", "RaftCatchUpFrontier"]),
          ("RAFT_READINESS_CONSTRUCTOR", ["Illegal term-level use", "RaftLearnerReadiness"]),
          ("RAFT_COMMITTED_ENTRY_CONSTRUCTOR", ["Illegal term-level use", "RaftCommittedEntry"]),
          ("RAFT_CHECKPOINT_CONSTRUCTOR", ["Illegal term-level use", "RaftCheckpoint"]),
          ("RAFT_CHECKPOINT_RECORD_UPDATE", ["Not in scope: record field", "raftCheckpointIndex"]),
          ("RAFT_FRONTIER_RECORD_UPDATE", ["Not in scope: record field", "raftCatchUpFrontierIndex"]),
          ("RAFT_COMMITTED_ENTRY_RECORD_UPDATE", ["Not in scope: record field", "committedEntryIndex"])
        ],
      selected
        HiddenImport
        "profile02/raft-reconfiguration-opacity"
        "PublicRaftReconfigurationOpaque.hs"
        [ ("RAFT_CONFIGURATION_INTERNAL_IMPORT", ["Eclips.Raft.Internal.Configuration"]),
          ("RAFT_LOG_INTERNAL_IMPORT", ["Eclips.Raft.Internal.Log"])
        ],
      selected
        CoercionFailure
        "profile02/raft-reconfiguration-opacity"
        "PublicRaftReconfigurationOpaque.hs"
        [ ("RAFT_VOTER_SET_COERCION", ["coerce", "RaftVoterSet"]),
          ("RAFT_CHECKPOINT_COERCION", ["coerce", "RaftCheckpoint"])
        ],
      selected
        UnavailableName
        "receipt-retirement/opacity"
        "PublicReceiptRetirementOpaque.hs"
        [ ("RECEIPT_RETIREMENT_CONSTRUCTOR", ["Illegal term-level use", "ReceiptRetirement"]),
          ("RECEIPT_RETIREMENT_HIGH_WATER_UPDATE", ["Not in scope: record field", "receiptRetirementHighWater"]),
          ("RECEIPT_RETIREMENT_EXCEPTIONS_UPDATE", ["Not in scope: record field", "receiptRetirementExceptions"])
        ],
      selected
        CoercionFailure
        "receipt-retirement/opacity"
        "PublicReceiptRetirementOpaque.hs"
        [("RECEIPT_RETIREMENT_COERCION", ["coerce", "ReceiptRetirement"])],
      selected
        CoercionFailure
        "private-role"
        "PrivateRoleCoercion.hs"
        [ ("UNIQUE_WORD64", ["coerce", "PrivateUniqueId", "Word64"]),
          ("UNIQUE_OBJECT", ["coerce", "PrivateUniqueId", "PrivateObjectId"]),
          ("UNIQUE_PROCESS", ["coerce", "PrivateUniqueId", "PrivateProcessId"]),
          ("UNIQUE_NABLA", ["coerce", "PrivateUniqueId", "PrivateNablaId"]),
          ("UNIQUE_DELTA", ["coerce", "PrivateUniqueId", "PrivateDeltaId"]),
          ("SORT_BYTES", ["coerce", "SortId", "ByteString"]),
          ("OBJECT_PROCESS", ["coerce", "PrivateObjectId", "PrivateProcessId"]),
          ("OBJECT_NABLA", ["coerce", "PrivateObjectId", "PrivateNablaId"]),
          ("OBJECT_DELTA", ["coerce", "PrivateObjectId", "PrivateDeltaId"]),
          ("PROCESS_NABLA", ["coerce", "PrivateProcessId", "PrivateNablaId"]),
          ("PROCESS_DELTA", ["coerce", "PrivateProcessId", "PrivateDeltaId"]),
          ("NABLA_DELTA", ["coerce", "PrivateNablaId", "PrivateDeltaId"])
        ],
      selected
        CoercionFailure
        "admin-claim-role"
        "AdminClaimRoleCoercion.hs"
        [ ("DEPLOYMENT_PROCESS", ["arising from a use of", "coerce"]),
          ("PROCESS_EPOCH_PROCESS", ["arising from a use of", "coerce"]),
          ("HERALD_ATTACHMENT", ["arising from a use of", "coerce"]),
          ("DEPLOYMENT_ATTACHMENT", ["arising from a use of", "coerce"])
        ],
      selected
        CoercionFailure
        "peer-claim-role"
        "PeerClaimRoleCoercion.hs"
        [ ("SYSTEM_HERALD_ID", ["coerce", "SystemClaim", "HeraldIdClaim"]),
          ("HERALD_ID_EPOCH", ["coerce", "HeraldIdClaim", "HeraldEpochClaim"]),
          ("GLOBAL_PROCESS", ["coerce", "GlobalObjectClaim", "ProcessEpochClaim"]),
          ("NABLA_DELTA", ["coerce", "NablaClaim", "DeltaClaim"]),
          ("CATALOGUE_PROJECTION", ["coerce", "CatalogueDigestClaim", "InitialProjectionDigestClaim"]),
          ("PROJECTION_ITEM", ["coerce", "InitialProjectionDigestClaim", "PeerItemDigestClaim"])
        ],
      [ rejected
          "application-write/delete-reserved-shape"
          "ApplicationWriteShape.hs"
          Nothing
          TypeMismatch
          ["ApplicationWriteValue", "ApplicationValue"],
        rejected
          "application-query/global-identity"
          "ApplicationQueryGlobalIdentity.hs"
          Nothing
          TypeMismatch
          ["GlobalUniqueId", "PrivateUniqueId"],
        rejected
          "herald-genesis/private-module"
          "PublicGenesisInternalImport.hs"
          Nothing
          HiddenImport
          ["Eclips.Herald.Genesis.Internal"],
        rejected
          "application-session/owner-construction"
          "PublicSessionOwnerConstruction.hs"
          Nothing
          UnavailableName
          ["applicationAttachmentForBootstrap"],
        rejected
          "administration/owner-construction"
          "PublicAdministrationOwnerConstruction.hs"
          Nothing
          UnavailableName
          ["drainId"],
        rejected
          "peer-stream/private-state-module"
          "PublicPeerStreamStateImport.hs"
          Nothing
          HiddenImport
          ["Eclips.Herald.PeerStream.State"]
      ],
      selected
        HiddenImport
        "step7/private-module"
        "PublicStep7InternalImport.hs"
        [ ("DISCOVERY_INTERNAL", ["Eclips.Herald.Discovery.Internal"]),
          ("DISCOVERY_STATE", ["Eclips.Herald.Discovery.State"]),
          ("PEER_LIVENESS_INTERNAL", ["Eclips.Herald.PeerLiveness.Internal"]),
          ("PEER_LIVENESS_STATE", ["Eclips.Herald.PeerLiveness.State"]),
          ("TIMER_INTERNAL", ["Eclips.Herald.Timer.Internal"]),
          ("PLACEMENT_STATE", ["Eclips.Herald.Placement.State"]),
          ("PUBLICATION_STATE", ["Eclips.Herald.Publication.State"]),
          ("PEER_PUBLICATION", ["Eclips.Herald.PeerPublication"]),
          ("PEER_STREAM", ["Eclips.Herald.PeerStream"]),
          ("PEER_STREAM_STATE", ["Eclips.Herald.PeerStream.State"]),
          ("PEER_CONTROL", ["Eclips.Herald.UseCase.PeerControl"]),
          ("PEER_INPUT", ["Eclips.Herald.UseCase.PeerInput"]),
          ("PEER_PLACEMENT", ["Eclips.Herald.UseCase.PeerPlacement"])
        ],
      selected
        UnavailableName
        "step7/opaque-constructor"
        "PublicStep7Opaque.hs"
        [ ("DISCOVERY_BINDING_GENERATION", ["PeerBindingGeneration"]),
          ("DISCOVERY_BINDING", ["PeerBinding"]),
          ("PLACEMENT_SEQUENCE", ["PlacementSequence"]),
          ("PLACEMENT_ROUTE", ["DeltaRoute"]),
          ("PLACEMENT_SNAPSHOT", ["PlacementSnapshot"])
        ],
      [ rejected
          "step10/peer-dial-intent-constructor"
          "PublicStep10Opaque.hs"
          (Just "PEER_DIAL_INTENT_CONSTRUCTOR")
          UnavailableName
          ["PeerDialIntent"]
      ],
      selected
        UnavailableName
        "peer-dispatch/owner-constructor"
        "PublicPeerDispatch.hs"
        [ ("MINT_PEER_DISPATCH_TICKET", ["PeerDispatchTicket"]),
          ("MINT_PEER_DISPATCH_ATTEMPT", ["PeerDispatchAttempt"])
        ],
      selected
        UnavailableName
        "step5/opaque-identity"
        "OpaqueStep5Identities.hs"
        [ ("WAIT_ID", ["WaitId"]),
          ("REPLY_CURSOR", ["ApplicationReplyCursor"]),
          ("PROCESS_ACCEPTANCE_POSITION", ["ProcessAcceptancePosition"])
        ],
      selected
        HiddenImport
        "step5/opaque-identity"
        "OpaqueStep5Identities.hs"
        [ ("PUBLICATION_POSITION", ["Eclips.Herald.Publication.State"])
        ],
      selected
        UnavailableName
        "startup/opaque-carrier"
        "OpaqueStartupMutation.hs"
        [ ("CHECKED_GENESIS_RECORD", ["checkedLocalHeraldId"]),
          ("APPLIED_BOOTSTRAP_RECORD", ["appliedProcessResidence"]),
          ("CONFIGURED_PROCESS_BOOTSTRAP_RECORD", ["configuredProcessBootstrapResidence"]),
          ("PRIMORDIAL_DEFINITION_REPLICA_RECORD", ["primordialReplicaPublicationId"]),
          ("APPLIED_ROOT_RECORD", ["appliedRootControlPrerequisite"])
        ],
      selected
        MissingInstance
        "startup/opaque-carrier"
        "OpaqueStartupMutation.hs"
        [ ("CHECKED_GENESIS_GENERIC", ["Generic", "CheckedHeraldGenesis"]),
          ("APPLIED_BOOTSTRAP_GENERIC", ["Generic", "AppliedProcessBootstrap"])
        ],
      selected
        HiddenImport
        "step8/private-module"
        "PublicStep8InternalImport.hs"
        [ ("APPLICATION_CLIENT_INTERNAL", ["Eclips.Application.Client.Internal"]),
          ("APPLICATION_RECOVERY_INTERNAL", ["Eclips.Application.Client.Recovery.Internal"]),
          ("APPLICATION_RPC_INTERNAL", ["Eclips.Herald.Application.RPC.Internal"])
        ],
      selected
        TypeMismatch
        "application-protocol/nominal-separation"
        "ProtocolClaimSeparation.hs"
        [ ("CLAIM_IS_BINDING", ["ApplicationSessionClaim", "ApplicationSessionBinding"]),
          ("GLOBAL_ID_IS_SCOPE", ["GlobalUniqueId"]),
          ("DELETE_RESERVED_IS_OPERATION", ["ApplicationOperation"])
        ],
      selected
        HiddenImport
        "runtime/private-module"
        "PublicRuntimeInternalImport.hs"
        [ ("RUNTIME_INTERNAL_TYPES", ["Eclips.Herald.Runtime.Internal.Types"]),
          ("RUNTIME_EXACT_TRACE", ["Eclips.Herald.Runtime.Internal.Trace"]),
          ("RUNTIME_OWNER", ["Eclips.Herald.Runtime.Internal.Owner"]),
          ("RUNTIME_RECORDING", ["Eclips.Herald.Runtime.Internal.Recording"]),
          ("RUNTIME_CONNECTION", ["Eclips.Herald.Runtime.Internal.Connection"]),
          ("RUNTIME_TIMER", ["Eclips.Herald.Runtime.Internal.Timer"])
        ],
      selected
        UnavailableName
        "runtime/physical-reference"
        "PublicRuntimeReference.hs"
        [ ("RUNTIME_REF_CONSTRUCTOR", ["ConnectionRef"])
        ],
      selected
        TypeMismatch
        "runtime/physical-reference"
        "PublicRuntimeReference.hs"
        [ ("APPLICATION_PEER_REF", ["ApplicationPlane", "PeerPlane"]),
          ("APPLICATION_ADMINISTRATION_REF", ["ApplicationPlane", "AdministrationPlane"]),
          ("ADMINISTRATION_PEER_REF", ["AdministrationPlane", "PeerPlane"])
        ],
      selected
        HiddenImport
        "step10/private-boundary"
        "PublicStep10InternalImport.hs"
        [ ("APPLICATION_API_INTERNAL", ["Eclips.Application.Runtime.Internal.Owner"]),
          ("PEER_RPC_INTERNAL", ["Eclips.Herald.Peer.RPC.Internal"]),
          ("TCP_INTERNAL_MODULE", ["Eclips.Herald.Runtime.TCP.Internal.Facade"]),
          ("TCP_HEARTBEAT", ["Eclips.Herald.Runtime.TCP.Internal.Heartbeat"])
        ],
      selected
        UnavailableName
        "profile02/deployment-opacity"
        "PublicDeploymentOpaque.hs"
        [ ("DEPLOYMENT_ENDPOINT_CONSTRUCTOR", ["Endpoint"]),
          ("DEPLOYMENT_PLANES_CONSTRUCTOR", ["DeploymentEndpoints"]),
          ("DEPLOYMENT_CHECKED_CONSTRUCTOR", ["CheckedDeployment"]),
          ("DEPLOYMENT_RESIDENT_CONSTRUCTOR", ["ResidentDeployment"]),
          ("DEPLOYMENT_RUNTIME_CONSTRUCTOR", ["DeploymentRuntime"]),
          ("DEPLOYMENT_ENDPOINT_RECORD", ["endpointHost"]),
          ("DEPLOYMENT_PLANES_RECORD", ["applicationListen"]),
          ("DEPLOYMENT_CHECKED_RECORD", ["deploymentResidents"]),
          ("DEPLOYMENT_RESIDENT_RECORD", ["residentEndpoints"]),
          ("DEPLOYMENT_RUNTIME_RECORD", ["deploymentResident"])
        ],
      selected
        UnavailableName
        "profile02/typed-application-opacity"
        "PublicTypedApplicationOpaque.hs"
        [ ("TYPED_HERALD_CONSTRUCTOR", ["Herald"]),
          ("TYPED_CALL_CONSTRUCTOR", ["Call"]),
          ("TYPED_LIFECYCLE_CONSTRUCTOR", ["LifecycleCall"]),
          ("TYPED_HERALD_RECORD", ["startup"]),
          ("TYPED_CALL_RECORD", ["status"]),
          ("TYPED_LIFECYCLE_RECORD", ["lifecycleRequestId"])
        ],
      selected
        TypeMismatch
        "profile02/typed-application-opacity"
        "PublicTypedApplicationOpaque.hs"
        [("TYPED_RESULT_MISMATCH", ["EnvironmentAccess", "PrivateUniqueId"])],
      selected
        CoercionFailure
        "profile02/typed-application-opacity"
        "PublicTypedApplicationOpaque.hs"
        [("TYPED_RESULT_COERCION", ["coerce", "EnvironmentAccess", "PrivateUniqueId"])],
      selected
        HiddenImport
        "profile02/typed-application-opacity"
        "PublicTypedApplicationOpaque.hs"
        [("TYPED_INTERNAL_IMPORT", ["Eclips.Application.Internal"])],
      selected
        UnavailableName
        "application-payload/opacity"
        "PublicApplicationPayloadOpaque.hs"
        [ ("PAYLOAD_NABLA_CONSTRUCTOR", ["Nabla"]),
          ("PAYLOAD_DELTA_CONSTRUCTOR", ["Delta"]),
          ("PAYLOAD_QUERY_CONSTRUCTOR", ["Query"]),
          ("PAYLOAD_SORT_CONSTRUCTOR", ["Sort"]),
          ("PAYLOAD_FIELD_CONSTRUCTOR", ["Field"]),
          ("PAYLOAD_RESERVATION_CONSTRUCTOR", ["Reservation"]),
          ("PAYLOAD_OBJECT_CONSTRUCTOR", ["Object"]),
          ("PAYLOAD_CALL_CONSTRUCTOR", ["Call"]),
          ("PAYLOAD_EDGE_RESERVATION_CONSTRUCTOR", ["EdgeReservation"]),
          ("PAYLOAD_SEQUENCER_CONSTRUCTOR", ["Sequencer"]),
          ("PAYLOAD_CREATION_CONSTRUCTOR", ["Creation"]),
          ("PAYLOAD_SORT_RECORD", ["sortDescriptor"]),
          ("PAYLOAD_NABLA_RECORD", ["nablaId"]),
          ("PAYLOAD_MISSING_FIELD", ["Unknown field", "missing", "Message"])
        ],
      selected
        CoercionFailure
        "application-payload/nominal"
        "PublicApplicationPayloadOpaque.hs"
        [ ("PAYLOAD_NABLA_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_DELTA_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_QUERY_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_SORT_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_FIELD_SOURCE_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_FIELD_RESULT_COERCION", ["coerce", "Text", "WrappedText"]),
          ("PAYLOAD_RESERVATION_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_OBJECT_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_CALL_COERCION", ["coerce", "Message", "Alternate"]),
          ("PAYLOAD_CREATION_COERCION", ["coerce", "Message", "Alternate"])
        ],
      selected
        TypeMismatch
        "application-payload/types"
        "PublicApplicationPayloadOpaque.hs"
        [ ("PAYLOAD_WRITE_MISMATCH", ["Message", "Bool"]),
          ("PAYLOAD_QUERY_LITERAL_MISMATCH", ["Text", "Bool"]),
          ("PAYLOAD_REGULAR_RESERVATION", ["RegularSort", "ControlledSort"]),
          ("PAYLOAD_RAW_ID_BINDING", ["PrivateNablaId", "Text"]),
          ("PAYLOAD_CALL_RESULT_MISMATCH", ["Message", "Bool"])
        ],
      selected
        MissingInstance
        "application-payload/generic"
        "PublicApplicationPayloadOpaque.hs"
        [ ("PAYLOAD_NABLA_GENERIC", ["Generic", "Nabla"]),
          ("PAYLOAD_SORT_GENERIC", ["Generic", "Sort"])
        ],
      selected
        HiddenImport
        "application-payload/opacity"
        "PublicApplicationPayloadOpaque.hs"
        [("PAYLOAD_INTERNAL_IMPORT", ["Eclips.Application.Typed.Internal"])],
      selected
        UnavailableName
        "profile02/primordial-opacity"
        "PublicPrimordialOpaque.hs"
        [ ("PRIMORDIAL_SELECTION_CONSTRUCTOR", ["PrimordialSelection"]),
          ("PRIMORDIAL_ACCESS_CONSTRUCTOR", ["PrimordialAccess"]),
          ("PRIMORDIAL_SELECTION_RECORD", ["selectionEntries"]),
          ("PRIMORDIAL_ACCESS_RECORD", ["accessEntries"])
        ],
      selected
        CoercionFailure
        "profile02/primordial-opacity"
        "PublicPrimordialOpaque.hs"
        [("PRIMORDIAL_SELECTION_ACCESS_COERCION", ["coerce", "PrimordialAccess", "PrimordialSelection"])],
      selected
        HiddenImport
        "profile02/primordial-opacity"
        "PublicPrimordialOpaque.hs"
        [("PRIMORDIAL_GRANT_INTERNAL_IMPORT", ["Eclips.Herald.Application.Primordial"])],
      selected
        UnavailableName
        "profile02/lifecycle-opacity"
        "PublicLifecycleOpaque.hs"
        [ ("LIFECYCLE_DESCRIPTOR_CONSTRUCTOR", ["ConnectionDescriptor"]),
          ("LIFECYCLE_CLAIM_CONSTRUCTOR", ["InitialClaimId"]),
          ("LIFECYCLE_PREPARATION_CONSTRUCTOR", ["ChildPreparation"]),
          ("LIFECYCLE_REQUEST_CONSTRUCTOR", ["LifecycleRequestId"]),
          ("LIFECYCLE_CHILD_CONSTRUCTOR", ["PreparedChild"]),
          ("LIFECYCLE_DESCRIPTOR_RECORD", ["connectionDescriptorAttachmentBytes"])
        ],
      selected
        CoercionFailure
        "profile02/lifecycle-opacity"
        "PublicLifecycleOpaque.hs"
        [("LIFECYCLE_CORRELATION_COERCION", ["coerce", "ChildPreparation", "LifecycleRequestId"])],
      selected
        HiddenImport
        "profile02/lifecycle-opacity"
        "PublicLifecycleOpaque.hs"
        [("LIFECYCLE_OWNER_IMPORT", ["Eclips.Herald.ProcessPreparation.State"])],
      selected
        UnavailableName
        "step11/opaque-owner"
        "PublicStep11Opaque.hs"
        [ ("GENERATOR_SEED_CONSTRUCTOR", ["GeneratorSeed"]),
          ("GENERATOR_SEED_BYTES", ["generatorSeedBytes"]),
          ("ENVIRONMENT_ACCESS_CONSTRUCTOR", ["EnvironmentAccess"])
        ],
      selected
        CoercionFailure
        "step16/environment-access-opacity"
        "PublicStep11Opaque.hs"
        [ ("STARTUP_ENVIRONMENT_COERCION", ["coerce", "ApplicationStartupAccess", "EnvironmentAccess"])
        ],
      selected
        HiddenImport
        "step11/opaque-owner"
        "PublicStep11Opaque.hs"
        [ ("ID_GENERATOR_STATE", ["Eclips.Herald.IdGenerator.State"]),
          ("CONTROLLED_RESERVATION_STATE", ["Eclips.Herald.Controlled.State"])
        ],
      selected
        HiddenImport
        "step12/oracle-client-boundary"
        "PublicStep12Opaque.hs"
        [ ("ORACLE_CLIENT_INTERNAL", ["Eclips.Herald.OracleClient.Internal"]),
          ("ORACLE_CLIENT_STATE", ["Eclips.Herald.OracleClient.State"]),
          ("ORACLE_PROJECTION_STATE", ["Eclips.Herald.OracleProjection.State"])
        ],
      selected
        UnavailableName
        "step12/oracle-client-boundary"
        "PublicStep12Opaque.hs"
        [ ("ORACLE_CONNECT_ATTEMPT_CONSTRUCTOR", ["OracleConnectAttempt"]),
          ("ORACLE_RETRY_CONSTRUCTOR", ["OracleRetry"]),
          ("ORACLE_BINDING_GENERATION_CONSTRUCTOR", ["OracleBindingGeneration"]),
          ("ORACLE_BINDING_CONSTRUCTOR", ["OracleBinding"]),
          ("ORACLE_CONTACT_SET_CONSTRUCTOR", ["OracleContactSet"])
        ],
      selected
        TypeMismatch
        "step12/oracle-client-boundary"
        "PublicStep12Opaque.hs"
        [ ("DTO_NODE_CLAIM_IS_CORE_CLAIM", ["RaftNodeIdClaim", "OracleNodeClaim"]),
          ("DTO_ENTRY_IS_CANONICAL_ENTRY", ["CanonicalAppliedOracleEntryDto", "CanonicalAppliedOracleEntry"])
        ],
      selected
        UnavailableName
        "profile02/admission-opacity"
        "PublicMembershipOpaque.hs"
        [ ("HERALD_ADMISSION_ID_CONSTRUCTOR", ["Illegal term-level use", "HeraldAdmissionId"]),
          ("HERALD_JOIN_RECIPE_CONSTRUCTOR", ["Illegal term-level use", "HeraldJoinBaseRecipe"]),
          ("HERALD_JOIN_RECIPE_RECORD_UPDATE", ["Not in scope: record field", "heraldJoinBaseRecipeApplicant"]),
          ("ORACLE_ADMISSION_RECORD_CONSTRUCTOR", ["Illegal term-level use", "HeraldAdmissionRecord"]),
          ("ORACLE_ADMISSION_RECORD_UPDATE", ["Not in scope: record field", "admissionRecordPhase"])
        ],
      selected
        CoercionFailure
        "profile02/admission-opacity"
        "PublicMembershipOpaque.hs"
        [("HERALD_ADMISSION_ID_COERCION", ["coerce", "HeraldAdmissionId", "ByteString"])],
      selected
        HiddenImport
        "profile02/admission-opacity"
        "PublicMembershipOpaque.hs"
        [ ("ORACLE_ADMISSION_INTERNAL_IMPORT", ["Eclips.Oracle.Internal.Admission"]),
          ("HERALD_JOIN_OWNER_IMPORT", ["Eclips.Herald.Join.State"])
        ],
      selected
        UnavailableName
        "step15/membership-opacity"
        "PublicMembershipOpaque.hs"
        [ ("MEMBERSHIP_GENERATION_ID_CONSTRUCTOR", ["Illegal term-level use", "HeraldMembershipGenerationId"]),
          ("MEMBERSHIP_HISTORY_CONSTRUCTOR", ["Illegal term-level use", "HeraldMembershipHistory"]),
          ("MEMBERSHIP_LINEAGE_CONSTRUCTOR", ["Illegal term-level use", "HeraldMembershipLineage"]),
          ("MEMBERSHIP_HISTORY_RECORD_UPDATE", ["Not in scope: record field", "heraldMembershipHistoryGenerations"]),
          ("MEMBERSHIP_LINEAGE_RECORD_UPDATE", ["Not in scope: record field", "heraldMembershipLineageTarget"]),
          ("FAILURE_PROBE_ID_CONSTRUCTOR", ["Illegal term-level use", "HeraldFailureProbeId"]),
          ("FAILURE_RESOLUTION_ID_CONSTRUCTOR", ["Illegal term-level use", "FailureProbeResolutionId"]),
          ("MEMBERSHIP_GENERATION_CONSTRUCTOR", ["Illegal term-level use", "HeraldMembershipGeneration"]),
          ("MEMBERSHIP_GENERATION_BODY_CONSTRUCTOR", ["Data constructor not in scope", "GenesisHeraldMembership"]),
          ("MEMBERSHIP_GENERATION_RECORD_UPDATE", ["Not in scope: record field", "heraldMembershipGenerationId"])
        ],
      selected
        CoercionFailure
        "step15/membership-opacity"
        "PublicMembershipOpaque.hs"
        [ ("MEMBERSHIP_GENERATION_ID_COERCION", ["coerce", "HeraldMembershipGenerationId", "ByteString"]),
          ("FAILURE_PROBE_ID_COERCION", ["coerce", "HeraldFailureProbeId", "ByteString"])
        ]
    ]

positiveProbes :: [ProbeSpec]
positiveProbes =
  fmap
    (\(identifier, fixture) -> ProbeSpec identifier fixture Nothing MustSucceed)
    [ ("raft/public-facade", "PublicRaftFacade.hs"),
      ("peer-dispatch/public-shell", "PublicPeerDispatch.hs"),
      ("runtime/public-facade", "PublicRuntimeFacade.hs"),
      ("step15/public-facade", "PublicStep15Facade.hs"),
      ("application/public-facade", "PublicApplicationFacade.hs"),
      ("application-payload/public-facade", "PublicApplicationPayloadOpaque.hs"),
      ("deployment/public-facade", "PublicDeploymentFacade.hs"),
      ("peer-rpc/public-facade", "PublicPeerRpcFacade.hs"),
      ("tcp/public-facade", "PublicTcpFacade.hs"),
      ("step11/public-facade", "PublicStep11Facade.hs"),
      ("administration/public-protocol", "PublicAdminProtocol.hs")
    ]

selected :: RejectionClass -> String -> FilePath -> [(String, [String])] -> [ProbeSpec]
selected rejectionClass prefix fixture =
  fmap
    ( \(selector, needles) ->
        rejected
          (prefix <> "/" <> selector)
          fixture
          (Just selector)
          rejectionClass
          needles
    )

rejected :: String -> FilePath -> Maybe String -> RejectionClass -> [String] -> ProbeSpec
rejected identifier fixture selector rejectionClass needles =
  ProbeSpec
    { probeId = identifier,
      fixtureName = fixture,
      cppSelector = selector,
      expectation = MustReject rejectionClass needles
    }
