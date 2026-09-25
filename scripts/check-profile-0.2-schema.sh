#!/bin/sh

# Current profile-0.2 source/schema audit. It checks owner and wire obligations,
# property registration, and gate composition. Stable milestone identifiers below
# also occur in test names; they do not indicate a historical certification claim.
# Executable properties and compiled boundaries are separate gate phases.

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

fail()
{
  echo "Profile-0.2 schema audit: $1" >&2
  exit 1
}

require_file()
{
  [ -f "$1" ] || fail "missing required file: $1"
}

require_text()
{
  text=$1
  file=$2
  description=$3
  if ! rg -q --fixed-strings -- "$text" "$file"
  then
    fail "$description ($file)"
  fi
}

forbid_text()
{
  text=$1
  file=$2
  description=$3
  if rg -q --fixed-strings -- "$text" "$file"
  then
    fail "$description ($file)"
  fi
}

forbid_pattern()
{
  pattern=$1
  file=$2
  description=$3
  if rg -q -- "$pattern" "$file"
  then
    fail "$description ($file)"
  fi
}

require_count()
{
  expected=$1
  pattern=$2
  file=$3
  description=$4
  actual=$(rg --count-matches --no-filename -- "$pattern" "$file" 2>/dev/null || :)
  [ -n "$actual" ] || actual=0
  if [ "$actual" -ne "$expected" ]
  then
    fail "$description: expected $expected, found $actual ($file)"
  fi
}

forbid_source_text()
{
  text=$1
  description=$2
  shift 2
  if matches=$(rg -n --fixed-strings --glob '*.hs' -- "$text" "$@" 2>/dev/null)
  then
    fail "$description:\n$matches"
  fi
}

forbid_source_pattern()
{
  pattern=$1
  description=$2
  shift 2
  if matches=$(rg -n --glob '*.hs' -- "$pattern" "$@" 2>/dev/null)
  then
    fail "$description:\n$matches"
  fi
}

require_opaque_owner_adapter()
{
  file=$1
  owner=$2
  prefix=$3
  require_text "${owner}DisappearanceView," "$file" \
    "$owner disappearance view is not exported opaquely"
  forbid_text "${owner}DisappearanceView (..)" "$file" \
    "$owner disappearance view exposes its constructor"
  for suffix in View Subject Blockers AbsenceDigest FactCount
  do
    require_text "${prefix}Disappearance${suffix}" "$file" \
      "$owner disappearance adapter is missing ${prefix}Disappearance${suffix}"
  done
}

restrict_symbol_sources()
{
  symbol=$1
  shift
  sources=$(rg -l --glob '*.hs' "\\b${symbol}\\b" herald-core 2>/dev/null || :)
  for source in $sources
  do
    allowed=false
    for expected in "$@"
    do
      if [ "$source" = "$expected" ]
      then
        allowed=true
      fi
    done
    [ "$allowed" = true ] ||
      fail "owner-only disappearance constructor $symbol escaped into $source"
  done
}

ledger=docs/verification/owner-contracts.md
inventory=scripts/profile-0.2-test-components.txt
inventory_check=scripts/check-test-component-inventory.sh
development_gate=scripts/check-development.sh
application_operation=application-types/src/Eclips/Application/Types/Operation.hs
application_result=application-types/src/Eclips/Application/Types/Result.hs
application_access=application-types/src/Eclips/Application/Types/Access.hs
application_binary_properties=application-types/test/BinaryProperties.hs
application_boundary_properties=application-types/test/BoundaryProperties.hs
application_client=application-client/src/Eclips/Application/Client.hs
application_runtime=application-api/src/Eclips/Application/Runtime.hs
application_client_properties=application-client/test/ClientProperties.hs
application_runtime_properties=application-api/test/RuntimeProperties.hs
protocol_application_fixtures=protocol-application/test/TestFixtures.hs
protocol_application_codec_properties=protocol-application/test/CodecProperties.hs
protocol_application_frame_properties=protocol-application/test/FrameProperties.hs
public_application_fixture=scripts/fixtures/type-boundaries/PublicStep11Facade.hs
module_boundary_policy=tools/module-boundaries/src/Eclips/ModuleBoundaries.hs
hello_application=examples/hello-world/HelloApplication.hs
domain_disappearance=domain/src/Eclips/Domain/Disappearance.hs
domain_disappearance_properties=domain/test/DisappearanceProperties.hs
domain_sort_descriptor=domain/src/Eclips/Domain/Sort/Descriptor.hs
domain_store=domain/src/Eclips/Domain/Store.hs
domain_store_properties=domain/test/StoreProperties.hs
domain_structural_consequence=domain/src/Eclips/Domain/StructuralConsequence.hs
domain_structural_properties=domain/test/StructuralProperties.hs
structural_source=herald-core/internal/Eclips/Herald/Publication/State.hs
oracle_command=oracle-core/src/Eclips/Oracle/Internal/Label.hs
oracle_core_cabal=oracle-core/eclips-oracle-core.cabal
oracle_core_readme=oracle-core/README.md
oracle_properties_main=oracle-core/test/Main.hs
prospective_reference=oracle-core/test/Eclips/Oracle/Step16/DisappearanceReference.hs
prospective_target=oracle-core/src/Eclips/Oracle/Internal/Disappearance.hs
prospective_properties=oracle-core/test/Step16DisappearanceProperties.hs
herald_disappearance_protocol=herald-core/internal/Eclips/Herald/Disappearance/Protocol.hs
herald_disappearance_state=herald-core/internal/Eclips/Herald/Disappearance/State.hs
herald_disappearance_use_case=herald-core/internal/Eclips/Herald/UseCase/Disappearance.hs
herald_disappearance_evidence=herald-core/internal/Eclips/Herald/Disappearance/Evidence.hs
herald_disappearance_evidence_internal=herald-core/internal/Eclips/Herald/Disappearance/Evidence/Internal.hs
herald_disappearance_owner_evidence=herald-core/internal/Eclips/Herald/Disappearance/OwnerEvidence.hs
herald_alignment_owner=herald-core/internal/Eclips/Herald/Alignment/State.hs
herald_alignment_disappearance=herald-core/internal/Eclips/Herald/Alignment/Disappearance.hs
herald_application_disappearance=herald-core/internal/Eclips/Herald/Application/Disappearance.hs
herald_controlled_disappearance=herald-core/internal/Eclips/Herald/Controlled/Disappearance.hs
herald_graph_disappearance=herald-core/internal/Eclips/Herald/Graph/Disappearance.hs
herald_graph_disappearance_readiness=herald-core/internal/Eclips/Herald/Graph/DisappearanceReadiness.hs
herald_peer_stream_disappearance=herald-core/internal/Eclips/Herald/PeerStream/Disappearance.hs
herald_placement_disappearance=herald-core/internal/Eclips/Herald/Placement/Disappearance.hs
herald_publication_disappearance=herald-core/internal/Eclips/Herald/Publication/Disappearance.hs
herald_store_disappearance=herald-core/internal/Eclips/Herald/Store/Disappearance.hs
herald_alignment_transfer_owner=herald-core/internal/Eclips/Herald/Alignment/Transfer.hs
herald_placement_owner=herald-core/internal/Eclips/Herald/Placement/State.hs
herald_prospective_main=herald-core/test-step16-prospective/Main.hs
herald_prospective_fixtures=herald-core/test-step16-prospective/Step16ProspectiveFixtures.hs
herald_prospective_harness=herald-core/test-step16-prospective/Step16ProspectiveHarness.hs
herald_prospective_projection=herald-core/test-step16-prospective/Step16ProspectiveProjection.hs
herald_prospective_properties=herald-core/test-step16-prospective/Step16DisappearanceProperties.hs
herald_disappearance_evidence_properties=herald-core/test/DisappearanceEvidenceProperties.hs
herald_effective_publication_properties=herald-core/test/EffectivePublicationProperties.hs
herald_controlled_removal=herald-core/internal/Eclips/Herald/UseCase/ControlledRemoval.hs
herald_controlled_removal_properties=herald-core/test/ControlledRemovalProperties.hs
herald_regular_retirement=herald-core/internal/Eclips/Herald/UseCase/RegularRetirement.hs
herald_regular_retirement_properties=herald-core/test/RegularSortRetirementProperties.hs
herald_regular_store_retirement_properties=herald-core/test/RegularDefinitionStoreRetirementProperties.hs
herald_regular_retirement_acceptance_properties=herald-core/test/Step16RegularRetirementAcceptanceProperties.hs
herald_oracle_advance=herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs
herald_structural_reconciliation=herald-core/internal/Eclips/Herald/Structural/Reconciliation.hs
herald_graph_progress=herald-core/internal/Eclips/Herald/Graph/Progress.hs
herald_store_owner=herald-core/internal/Eclips/Herald/Store/State.hs
herald_sort_registry_owner=herald-core/internal/Eclips/Herald/SortRegistry/State.hs
herald_application_forward=herald-core/internal/Eclips/Herald/Application/Forward.hs
herald_application_publication=herald-core/internal/Eclips/Herald/Application/Publication.hs
herald_peer_input=herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs
herald_peer_rpc_internal=herald-core/internal/Eclips/Herald/Peer/RPC/Internal.hs
herald_peer_rpc_public=herald-core/src/Eclips/Herald/Peer/RPC.hs
herald_peer_rpc_properties=herald-core/test/PeerRpcProperties.hs
herald_peer_store_properties=herald-core/test/PeerStoreProperties.hs
herald_peer_input_properties=herald-core/test/PeerInputProperties.hs
herald_application_label_properties=herald-core/test/ApplicationLabelProperties.hs
herald_forward_properties=herald-core/test/ForwardProperties.hs
herald_application_publication_properties=herald-core/test/ApplicationPublicationProperties.hs
herald_invariant_properties=herald-core/test/HeraldInvariantProperties.hs
herald_alignment_coordinator=herald-core/internal/Eclips/Herald/UseCase/Alignment.hs
herald_alignment_cut_queries=herald-core/internal/Eclips/Herald/Alignment/CutQueries.hs
herald_alignment_coordinator_properties=herald-core/test/AlignmentCoordinatorProperties.hs
peer_types=protocol-peer/src/Eclips/Protocol/Peer/Types.hs
peer_payload=herald-core/internal/Eclips/Herald/PeerPayload.hs
alignment_control=herald-core/internal/Eclips/Herald/Alignment/Protocol.hs
disappearance=domain/src/Eclips/Domain/Disappearance.hs
disappearance_properties=domain/test/DisappearanceProperties.hs
environment=domain/src/Eclips/Domain/Environment.hs
environment_properties=domain/test/EnvironmentProperties.hs
sort_occurrence=domain/src/Eclips/Domain/SortOccurrence.hs
sort_occurrence_properties=domain/test/SortOccurrenceProperties.hs
startup=domain/src/Eclips/Domain/Startup.hs
id_generator=herald-core/internal/Eclips/Herald/IdGenerator/State.hs
id_generator_properties=herald-core/test/IdGeneratorProperties.hs
private_environment=herald-core/internal/Eclips/Herald/Application/Environment.hs
application_owner=herald-core/internal/Eclips/Herald/Application/State.hs
controlled_owner=herald-core/internal/Eclips/Herald/Controlled/State.hs
publication_owner=herald-core/internal/Eclips/Herald/Publication/State.hs
new_environment=herald-core/internal/Eclips/Herald/UseCase/NewEnvironment.hs
structural_progress=herald-core/internal/Eclips/Herald/UseCase/StructuralProgress.hs
structural_settlement=herald-core/internal/Eclips/Herald/UseCase/StructuralSettlement.hs
startup_invariant=herald-core/internal/Eclips/Herald/Startup/Invariant.hs
alignment_transfer=herald-core/internal/Eclips/Herald/UseCase/AlignmentTransfer.hs
application_environment_properties=herald-core/test/ApplicationEnvironmentProperties.hs
environment_publication_properties=herald-core/test/EnvironmentPublicationProperties.hs
environment_successor_properties=herald-core/test/EnvironmentSuccessorProperties.hs
herald_properties_main=herald-core/test/Main.hs
herald_core_cabal=herald-core/eclips-herald-core.cabal

for required in \
  "$ledger" \
  "$inventory" \
  "$inventory_check" \
  "$development_gate" \
  "$herald_regular_retirement" \
  "$herald_regular_retirement_properties" \
  "$herald_regular_store_retirement_properties" \
  "$herald_regular_retirement_acceptance_properties" \
  "$herald_alignment_owner" \
  "$herald_alignment_coordinator" \
  "$herald_alignment_coordinator_properties" \
  "$application_operation" \
  "$application_result" \
  "$application_access" \
  "$application_binary_properties" \
  "$application_boundary_properties" \
  "$application_client" \
  "$application_runtime" \
  "$application_client_properties" \
  "$application_runtime_properties" \
  "$protocol_application_fixtures" \
  "$protocol_application_codec_properties" \
  "$protocol_application_frame_properties" \
  "$public_application_fixture" \
  "$module_boundary_policy" \
  "$hello_application" \
  "$domain_sort_descriptor" \
  "$domain_store" \
  "$domain_store_properties" \
  "$domain_structural_consequence" \
  "$domain_structural_properties" \
  "$structural_source" \
  "$oracle_command" \
  "$oracle_core_cabal" \
  "$oracle_core_readme" \
  "$oracle_properties_main" \
  "$prospective_reference" \
  "$prospective_target" \
  "$prospective_properties" \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_state" \
  "$herald_disappearance_use_case" \
  "$herald_disappearance_evidence" \
  "$herald_disappearance_evidence_internal" \
  "$herald_disappearance_owner_evidence" \
  "$herald_alignment_disappearance" \
  "$herald_application_disappearance" \
  "$herald_controlled_disappearance" \
  "$herald_graph_disappearance" \
  "$herald_graph_disappearance_readiness" \
  "$herald_peer_stream_disappearance" \
  "$herald_placement_disappearance" \
  "$herald_publication_disappearance" \
  "$herald_store_disappearance" \
  "$herald_alignment_transfer_owner" \
  "$herald_placement_owner" \
  "$herald_prospective_main" \
  "$herald_prospective_fixtures" \
  "$herald_prospective_harness" \
  "$herald_prospective_projection" \
  "$herald_prospective_properties" \
  "$herald_disappearance_evidence_properties" \
  "$herald_effective_publication_properties" \
  "$herald_controlled_removal" \
  "$herald_controlled_removal_properties" \
  "$herald_oracle_advance" \
  "$herald_structural_reconciliation" \
  "$herald_store_owner" \
  "$herald_sort_registry_owner" \
  "$herald_application_forward" \
  "$herald_application_publication" \
  "$herald_peer_input" \
  "$herald_peer_rpc_internal" \
  "$herald_peer_rpc_public" \
  "$herald_peer_rpc_properties" \
  "$herald_peer_store_properties" \
  "$herald_peer_input_properties" \
  "$herald_application_label_properties" \
  "$herald_forward_properties" \
  "$herald_application_publication_properties" \
  "$herald_invariant_properties" \
  "$peer_types" \
  "$peer_payload" \
  "$alignment_control" \
  "$disappearance" \
  "$disappearance_properties" \
  "$environment" \
  "$environment_properties" \
  "$sort_occurrence" \
  "$sort_occurrence_properties" \
  "$startup" \
  "$id_generator" \
  "$id_generator_properties" \
  "$private_environment" \
  "$application_owner" \
  "$controlled_owner" \
  "$publication_owner" \
  "$new_environment" \
  "$structural_progress" \
  "$structural_settlement" \
  "$startup_invariant" \
  "$alignment_transfer" \
  "$application_environment_properties" \
  "$environment_publication_properties" \
  "$environment_successor_properties" \
  "$herald_properties_main" \
  "$herald_core_cabal"
do
  require_file "$required"
done

# The environment shape is closed Domain vocabulary only: six canonical
# predefined roles, writer before reader, hence exactly twelve endpoints with
# every writer unsequenced, followed by one hub and eighteen preserving spokes.
# It owns neither generated identities nor a callable/wire surface.
require_count 4 '^  (=|\|) (EnvironmentWriterRootClaim|EnvironmentReaderRootClaim|EnvironmentHubRootClaim|EnvironmentEdgeRootClaim)\b' \
  "$environment" "EnvironmentRootClaim is not exactly writer/reader/hub/edge"
require_text 'environmentManifestRootCount' "$environment" \
  "environment manifest root count is missing"
require_text 'environmentConnectedObjectCount' "$environment" \
  "connected environment object count is missing"
require_text 'environmentWiringSlots' "$environment" \
  "connected environment wiring suffix is missing"
require_text 'ECLIPS-ENVIRONMENT-MANIFEST-SHAPE' "$environment" \
  "environment manifest canonical domain is missing"
require_text 'allPredefinedSortRoles' "$environment" \
  "environment manifest does not derive its closed role order from the catalogue"
require_text 'Eclips.Domain.Environment' domain/eclips-domain.cabal \
  "environment vocabulary is not exposed by Domain"
require_text 'EnvironmentProperties' domain/eclips-domain.cabal \
  "environment property module is absent from the Domain suite"
require_text 'EnvironmentProperties.tests' domain/test/Main.hs \
  "environment properties are not registered"
for environment_property in \
  'the closed manifest is six writer/reader pairs' \
  'the checker accepts only the exact complete order' \
  'writers must start unsequenced' \
  'canonical bytes pin count, role, and writer/reader order'
do
  require_text "$environment_property" "$environment_properties" \
    "environment manifest law is not registered: $environment_property"
done
forbid_pattern '^import Eclips\.(Application|Herald|Oracle|Protocol)' "$environment" \
  "environment vocabulary depends on an application/runtime/protocol owner"
forbid_text 'deriving anyclass (Binary)' "$environment" \
  "private environment shape prematurely gained a wire Binary instance"

# Disappearance is constructor-hidden Domain vocabulary only.  It fixes the two
# non-coercible subject arms, their canonical ordering/digests, probe/evidence
# coordinates, Open classification, and two successful resolution outcomes.  It
# does not make an Oracle command, EPRP marker, or application call live.
require_count 2 '^  (=|\|) (ControlledPredefinedSubject|RegularSortDefinitionSubject)\b' \
  "$disappearance" "DisappearanceSubject is not exactly controlled/regular"
require_count 2 '^  (=|\|) (DisappearanceProbeOpened|DisappearanceProbeAliased)\b' \
  "$disappearance" "DisappearanceOpenResult is not exactly opened/aliased"
require_count 2 '^  (=|\|) (ControlledDisappearanceResolution|RegularDefinitionRetirementResolution)\b' \
  "$disappearance" "DisappearanceResolutionOutcome is not exactly controlled/regular"
for opaque_type in \
  DisappearanceSubject \
  DisappearanceProbeId \
  DisappearanceEvidenceClaim \
  DisappearanceOpenResult \
  DisappearanceResolutionOutcome
do
  require_text "    $opaque_type," "$disappearance" \
    "constructor-hidden disappearance export is missing: $opaque_type"
  forbid_text "$opaque_type (..)" "$disappearance" \
    "private disappearance constructor is exported: $opaque_type"
done
for canonical_domain in \
  ECLIPS-DISAPPEARANCE-SUBJECT \
  ECLIPS-DISAPPEARANCE-PROBE \
  ECLIPS-DISAPPEARANCE-EVIDENCE \
  ECLIPS-DISAPPEARANCE-EVIDENCE-CLAIM \
  ECLIPS-DISAPPEARANCE-OUTCOME
do
  require_text "$canonical_domain" "$disappearance" \
    "private disappearance canonical domain is missing: $canonical_domain"
done
require_text 'Eclips.Domain.Disappearance' domain/eclips-domain.cabal \
  "disappearance vocabulary is not exposed by Domain"
require_text 'DisappearanceProperties' domain/eclips-domain.cabal \
  "disappearance property module is absent from the Domain suite"
require_text 'DisappearanceProperties.tests' domain/test/Main.hs \
  "disappearance properties are not registered"
for disappearance_property in \
  'genesis and resolved occurrence claims recompute their identity' \
  'controlled subjects admit only eligible predefined roles' \
  'subjects expose exactly the controlled and regular shapes' \
  'controlled subjects sort before regular and bytewise within each arm' \
  'controlled subject digest has a fixed golden' \
  'probe IDs require a positive Open index and validate claimed bytes' \
  'evidence claims bind subject, membership, reporter, probe, and digest' \
  'Open results distinguish creation from aliasing without changing the probe' \
  'resolution outcomes require a positive index and derive the next occurrence' \
  'canonical identity surfaces are manual private transcripts, not wire codecs'
do
  require_text "$disappearance_property" "$disappearance_properties" \
    "disappearance vocabulary law is not registered: $disappearance_property"
done
forbid_pattern '^import Eclips\.(Application|Herald|Oracle|Protocol)' "$disappearance" \
  "disappearance vocabulary depends on an application/runtime/protocol owner"
forbid_text 'deriving anyclass (Binary)' "$disappearance" \
  "private disappearance vocabulary prematurely gained a wire Binary instance"

# Increment 1 installs one neutral occurrence derivation.  The Genesis arm is
# byte-for-byte the established transcript; a resolved retirement is a distinct
# base, not a second disappearance-owned derivation.
require_count 2 '^  (=|\|) (Genesis|ResolvedRetirement)\b' \
  "$sort_occurrence" "SortOccurrenceBase is not exactly two-arm"
require_text 'SortOccurrenceBase (Genesis)' "$sort_occurrence" \
  "retirement occurrence constructor is not hidden"
forbid_text 'SortOccurrenceBase (..)' "$sort_occurrence" \
  "unchecked retirement occurrence constructor is exported"
require_text 'resolvedRetirementOccurrenceBase' "$sort_occurrence" \
  "positive retirement occurrence admission is missing"
require_text 'deriveSortDefinitionOccurrenceId' "$sort_occurrence" \
  "neutral occurrence derivation is missing"
require_text 'ECLIPS-OCCURRENCE-GENESIS' "$sort_occurrence" \
  "established genesis transcript is not preserved"
require_text 'ECLIPS-OCCURRENCE-RESOLVED-RETIREMENT' "$sort_occurrence" \
  "resolved-retirement transcript is not domain-separated"
require_text 'Eclips.Domain.SortOccurrence' domain/eclips-domain.cabal \
  "neutral occurrence module is not exposed by Domain"
require_text 'SortOccurrenceProperties' domain/eclips-domain.cabal \
  "occurrence property module is absent from the Domain suite"
require_text 'SortOccurrenceProperties.tests' domain/test/Main.hs \
  "occurrence properties are not registered"
require_text 'resolved retirement bases require a positive Oracle index' \
  "$sort_occurrence_properties" \
  "positive retirement occurrence admission property is not registered"
forbid_source_text 'deriveGenesisOccurrenceId' \
  "the superseded Startup-owned occurrence derivation remains" \
  domain herald-core
forbid_pattern '^deriveSortDefinitionOccurrenceId[[:space:]]*::' "$startup" \
  "Startup defines a second occurrence derivation"

# The generated range exposes only a checked positive count, complete ordered
# output, and the one whole-range successor.  Properties pin its one-step laws.
for range_symbol in \
  PositiveCount \
  PositiveCountMustBePositive \
  mkPositiveCount \
  PreparedGeneratedIdRange \
  prepareGeneratedIdRange \
  preparedGlobalUniqueIds \
  commitGeneratedIdRange
do
  require_text "$range_symbol" "$id_generator" \
    "generated-range vocabulary is incomplete: $range_symbol"
done
for range_property in \
  'range count rejects zero' \
  'range equals repeated single-step preparation' \
  'range is distinct under the non-wrapping premise' \
  'range exposes no partial state and has one exact commit point'
do
  require_text "$range_property" "$id_generator_properties" \
    "generated-range law is not registered: $range_property"
done
forbid_text 'PreparedGeneratedIdRange (NonEmpty GlobalUniqueId) State State' \
  "$id_generator" "range exposes a partial/intermediate successor"

# Increment 2 installs only the constructor-hidden four-owner acceptance
# transaction.  Application retains request reachability and the immutable
# manifest, Publication owns the twelve checked roots and their positions, and
# Controlled admits one closed all-or-nothing observation range.
for private_name in \
  EnvironmentManifestKey \
  EnvironmentRootPlan \
  PositionedEnvironmentRoot \
  EnvironmentManifest \
  PendingEnvironment
do
  require_text "$private_name" "$private_environment" \
    "private environment vocabulary is missing: $private_name"
done
require_text 'classifyEnvironmentRequest' "$application_owner" \
  "Application does not classify the private request namespace"
require_text 'prepareEnvironmentRequestAcceptance' "$application_owner" \
  "Application does not prepare retained environment acceptance"
require_text 'prepareControlledEnvironmentRoots' "$controlled_owner" \
  "Controlled does not prepare the closed environment-root range"
require_text 'prepareEnvironmentRootRange' "$publication_owner" \
  "Publication does not prepare the distinct environment-root range"
require_text 'planNewEnvironment' "$new_environment" \
  "the four-owner private environment coordinator is missing"

# Increment 3 supplied the structural vertical and Increment 4 exposes its
# checked access through the sole application result. The environment source arm
# retains its complete manifest, shares the established fixed point, and may
# cross checked installed descendant lineage. Finalization remains
# an access-only Application transition after one common cut covers all twelve
# exact roots.
require_count 2 '^  (=|\|) (ApplicationStructuralSourceStage|EnvironmentRootStructuralSourceStage)\b' \
  "$structural_source" "StructuralSourceStage is not exactly application/environment"
for structural_symbol in \
  structuralSourceEnvironmentRoot \
  structuralSourceMembershipGenerationId \
  stampedStructuralEnvironmentRoot \
  unstampedStructuralSourceStageEntries
do
  require_text "$structural_symbol" "$publication_owner" \
    "environment structural-source ownership is incomplete: $structural_symbol"
done
require_text 'Publication.structuralSourceMembershipGenerationId' "$structural_progress" \
  "structural readiness does not use retained source membership"
require_text 'environmentRootPrefixSettledFor' "$alignment_transfer" \
  "alignment prefix settlement omits environment-root work"
for settlement_symbol in \
  settleReadyEnvironments \
  environmentManifestIsReady \
  descendantEvidence \
  Application.prepareEnvironmentSettlement \
  Application.commitEnvironmentSettlement
do
  require_text "$settlement_symbol" "$structural_settlement" \
    "environment structural settlement is incomplete: $settlement_symbol"
done
for environment_completion_symbol in \
  EnvironmentAccess \
  EnvironmentSettlementEvidence \
  environmentSettlementEvidenceCut \
  environmentSettlementEvidenceOccurrences \
  CompletedEnvironment \
  completePendingEnvironment \
  completedEnvironmentAccess
do
  require_text "$environment_completion_symbol" "$private_environment" \
    "environment completion vocabulary is missing: $environment_completion_symbol"
done
for application_settlement_symbol in \
  prepareEnvironmentSettlement \
  preparedEnvironmentSettlementCompletionOutcome \
  commitEnvironmentSettlement \
  applicationCompletedEnvironmentEntries
do
  require_text "$application_settlement_symbol" "$application_owner" \
    "Application environment finalization is incomplete: $application_settlement_symbol"
done
require_text 'stampedStructuralEnvironmentRoot' "$startup_invariant" \
  "whole-state invariants do not classify stamped environment sources"
require_text 'environmentStructuralStageLineageEvidence' "$startup_invariant" \
  "whole-state invariants do not validate environment stamp lineage"
require_text 'completedEnvironmentSettlementEvidence' "$structural_settlement" \
  "completed environments do not validate their retained exact cut witness"
forbid_text 'structuralInstalledCutIds' "$structural_settlement" \
  "environment validation searches the complete installed-cut history"
for environment_evidence in \
  'settlement localizes canonical aliases once and exact replay is inert' \
  'session End before settlement completes globally without a reply' \
  'process End before settlement records completion without aliases' \
  'the third structural source arm retains every manifest-qualified root in position order' \
  'an environment source stamps exactly once through the shared structural owner' \
  'post-retirement acceptance captures the current H1/H2/H3 successor' \
  'H1 predecessor acceptance settles through the direct successor without H4' \
  'the complete four-Herald private schedule is deterministic' \
  'the selected acceptance/application/delivery/final settlement ledger is 2 + R + E with zero retry work'
do
  if ! rg -q --fixed-strings -- "$environment_evidence" \
    "$application_environment_properties" \
    "$environment_publication_properties" \
    "$environment_successor_properties"
  then
    fail "Increment-3 environment evidence is not registered: $environment_evidence"
  fi
done
require_text 'EnvironmentSuccessorProperties.tests' "$herald_properties_main" \
  "environment successor properties are not registered"
require_text 'EnvironmentSuccessorProperties' "$herald_core_cabal" \
  "environment successor property module is absent from the Herald suite"

# Increment 4 is one incompatible EAPP cutover: the pre-existing constructors
# retain tags 0..6 and the appended newenv operation/result/pending tags are
# respectively 7, 7, and 2. There is no seven-call compatibility decoder.
require_count 8 '^  (=|\|) (NewIdApplication|WriteApplication|ForwardApplication|ReadApplication|LocalTakeApplication|WaitApplication|LabelApplication|NewEnvironmentApplication)\b' \
  "$application_operation" "ApplicationOperation is not exactly eight-arm"
require_count 3 '^  (=|\|) (StructuralStabilizationPending|LabelSettlementPending|EnvironmentStabilizationPending)\b' \
  "$application_result" "OperationPendingReason is not exactly three-arm"
require_count 8 '^  (=|\|) (NewIdCompleted|WriteCompleted|ForwardCompleted|ReadCompleted|LocalTakeCompleted|WaitCompleted|LabelCompleted|NewEnvironmentCompleted)\b' \
  "$application_result" "RegularCallResult is not exactly eight-arm"
for submit_helper in \
  submitNewId submitWrite submitForward submitRead submitLocalTake submitWait submitLabel \
  submitNewEnvironment
do
  require_text "$submit_helper" "$application_client" \
    "current application client helper is missing: $submit_helper"
done
for callable in newid write forward read localTake wait label newenv
do
  require_text "    $callable," "$application_runtime" \
    "current callable application operation is missing: $callable"
done
for obsolete in NewEnvApplication NewEnvCompleted
do
  forbid_source_text "$obsolete" \
    "alternate/obsolete EAPP newenv surface: $obsolete" \
    application-types/src application-client/src application-api/src \
    protocol-application/src herald-core/src herald-core/internal \
    herald-runtime/src herald-runtime/internal herald-runtime/tcp-internal examples
done
require_text '    EnvironmentAccess,' "$application_access" \
  "EnvironmentAccess is not exported opaquely"
forbid_text 'EnvironmentAccess (..)' "$application_access" \
  "EnvironmentAccess exposes its representation"
require_text 'selectEnvironment' "$application_access" \
  "checked environments cannot be selected for startup"
require_text 'operationConstructorTag NewEnvironmentApplication = 7' \
  "$application_binary_properties" "newenv operation Binary tag 7 is not pinned"
require_text 'regularResultConstructorTag (NewEnvironmentCompleted _) = 7' \
  "$application_binary_properties" "newenv result Binary tag 7 is not pinned"
require_text '"pending tags"' "$application_binary_properties" \
  "newenv pending Binary tag vector is not pinned"
require_text '[0, 1, 2]' "$application_binary_properties" \
  "pending Binary tags 0, 1, and 2 are not pinned"
require_text 'caseEnvironmentAccessRejectsShape' "$application_boundary_properties" \
  "checked EnvironmentAccess rejection evidence is absent"
require_text 'submitNewEnvironment invocation' "$application_client_properties" \
  "newenv client helper is not exercised"
require_text 'the eight-operation facade carries every operation and pending reason over loopback' \
  "$application_runtime_properties" "the public facade does not exercise the all-eight RPC matrix"
require_text 'NewEnvironmentApplication, NewEnvironmentCompleted _' \
  application-client/src/Eclips/Application/Client/Internal.hs \
  "newenv client completion pairing is absent"
require_text 'OperationAccepted EnvironmentStabilizationPending' \
  "$protocol_application_fixtures" "EAPP fixture omits newenv pending"
require_text 'NewEnvironmentCompleted environmentAccessFixture' \
  "$protocol_application_fixtures" "EAPP fixture omits newenv completion"
require_text 'new-environment Call has stable current-build golden bytes' \
  "$protocol_application_codec_properties" "EAPP newenv golden vector is absent"
require_text 'a new-environment call follows an earlier client frame' \
  "$protocol_application_frame_properties" "fragmented/coalesced EAPP newenv evidence is absent"
require_text 'NewEnvironmentApplication -> 7' "$public_application_fixture" \
  "public exhaustive operation fixture omits newenv"
require_text 'NewEnvironmentCompleted _ -> 7' "$public_application_fixture" \
  "public exhaustive result fixture omits newenv"
require_text 'EnvironmentStabilizationPending -> 2' "$public_application_fixture" \
  "public exhaustive pending fixture omits newenv"
require_text '"Eclips.Herald.UseCase.NewEnvironment" ->' "$module_boundary_policy" \
  "newenv coordinator is absent from the Herald import policy"
require_text 'relativePath == "internal/Eclips/Herald/UseCase/ApplicationCall.hs"' \
  "$module_boundary_policy" "newenv coordinator is not localized to ApplicationCall"
require_text 'App.newEnvironment' "$hello_application" \
  "the real-TCP example does not exercise callable newenv"

# The independent reference lives in test sources. There is no second target
# implementation, cross-package prospective library or production adapter.
forbid_text 'library step16-prospective' "$oracle_core_cabal" "temporary Oracle sublibrary survived cutover"
forbid_source_text 'Eclips.Oracle.Step16.' "production Step16 Oracle adapter survived cutover" oracle-core/src herald-core/src herald-core/internal
forbid_text 'eclips-oracle-core:step16-prospective' "$herald_core_cabal" "Herald still depends on the prospective Oracle unit"
require_text 'Eclips.Oracle.Disappearance' "$oracle_core_cabal" "live Oracle payload facade is absent"
require_text 'Step16DisappearanceProperties.tests' "$oracle_properties_main" "live Oracle disappearance properties are not registered"
require_text 'parallel-safe eclips-herald-core:test:herald-step16-prospective-properties' "$inventory" "retained leaf property component is not inventoried"
require_text 'Step16DisappearanceProperties.tests' "$herald_prospective_main" "retained leaf properties are not registered"
for private_module in \
  Eclips.Herald.Alignment.Disappearance \
  Eclips.Herald.Application.Disappearance \
  Eclips.Herald.Controlled.Disappearance \
  Eclips.Herald.Disappearance.Evidence \
  Eclips.Herald.Disappearance.Evidence.Internal \
  Eclips.Herald.Disappearance.OwnerEvidence \
  Eclips.Herald.Disappearance.Protocol \
  Eclips.Herald.Disappearance.State \
  Eclips.Herald.Graph.Disappearance \
  Eclips.Herald.Graph.DisappearanceReadiness \
  Eclips.Herald.PeerStream.Disappearance \
  Eclips.Herald.Placement.Disappearance \
  Eclips.Herald.Publication.Disappearance \
  Eclips.Herald.Store.Disappearance \
  Eclips.Herald.UseCase.ControlledRemoval \
  Eclips.Herald.UseCase.Disappearance
do
  require_count 1 "^      $private_module\$" "$herald_core_cabal" \
    "private Herald disappearance module is absent or duplicated: $private_module"
done
require_text 'Step16ProspectiveHarness' "$herald_core_cabal" \
  "the sole prospective Herald/Oracle adapter is absent from the named component"
require_text 'Step16ProspectiveProjection' "$herald_core_cabal" \
  "the test-only Oracle-to-Herald projection translator is absent from the named component"
require_text 'import DisappearanceReferenceDriver qualified as Target' \
  "$herald_prospective_harness" \
  "the named Herald component does not exercise the prospective Oracle target"
require_text 'import DisappearanceReferenceDriver qualified as Target' \
  "$herald_prospective_projection" \
  "the test-only projection translator does not consume the prospective Oracle target"
require_text 'import Step16ProspectiveProjection qualified as Projection' \
  "$herald_prospective_harness" \
  "the prospective harness bypasses its checked projection translator"
require_text 'Step16DisappearanceProperties.tests' "$herald_prospective_main" \
  "the prospective Herald property group is not registered"
require_text 'DisappearanceEvidenceProperties' "$herald_core_cabal" \
  "the owner-backed disappearance evidence property module is absent"
require_text 'DisappearanceEvidenceProperties.tests' "$herald_properties_main" \
  "the owner-backed disappearance evidence properties are not registered"

# The reference cannot delegate semantic choices to the live transition.
forbid_pattern '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Oracle\.(Canonical|Command|Effect|Genesis|Input|Label|Projection|Receipt|State|Transition|Internal)(\.|[[:space:](])' "$prospective_reference" "independent reference imports a live Oracle owner"
for herald_prospective_source in \
  "$herald_alignment_disappearance" \
  "$herald_application_disappearance" \
  "$herald_controlled_disappearance" \
  "$herald_disappearance_evidence" \
  "$herald_disappearance_evidence_internal" \
  "$herald_disappearance_owner_evidence" \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_state" \
  "$herald_graph_disappearance" \
  "$herald_graph_disappearance_readiness" \
  "$herald_peer_stream_disappearance" \
  "$herald_placement_disappearance" \
  "$herald_publication_disappearance" \
  "$herald_store_disappearance" \
  "$herald_controlled_removal" \
  "$herald_disappearance_use_case"
do
  forbid_pattern '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Oracle\.Step16\.' \
    "$herald_prospective_source" \
    "private Herald owner imports the test-only Oracle adapter"
  forbid_pattern '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Protocol\.' \
    "$herald_prospective_source" \
    "private Herald owner imports a live wire protocol"
  forbid_pattern '^import[[:space:]]+(qualified[[:space:]]+)?(Control\.Concurrent|Effectful|Network|System\.IO)' \
    "$herald_prospective_source" \
    "private Herald disappearance owner imports a runtime/effect boundary"
  forbid_pattern 'deriving anyclass \(Binary\)|instance[[:space:]]+Binary' \
    "$herald_prospective_source" \
    "private prospective marker/state gained a generic wire Binary instance"
done
require_opaque_owner_adapter "$herald_alignment_disappearance" Alignment alignment
require_opaque_owner_adapter "$herald_application_disappearance" Application application
require_opaque_owner_adapter "$herald_controlled_disappearance" Controlled controlled
require_opaque_owner_adapter "$herald_graph_disappearance" Graph graph
require_opaque_owner_adapter "$herald_peer_stream_disappearance" PeerStream peerStream
require_opaque_owner_adapter "$herald_placement_disappearance" Placement placement
require_opaque_owner_adapter "$herald_publication_disappearance" Publication publication
require_opaque_owner_adapter "$herald_store_disappearance" Store store
for live_alignment_owner_view in \
  liveAlignmentStructuralDebtEntries \
  liveAlignmentHistoricalCertificateEntries
do
  require_text "$live_alignment_owner_view" "$herald_alignment_owner" \
    "Alignment owner omits its live/history projection: $live_alignment_owner_view"
  require_text "Alignment.$live_alignment_owner_view owner" \
    "$herald_alignment_disappearance" \
    "Alignment disappearance does not consume its live/history projection: $live_alignment_owner_view"
done
for alignment_cut_accessor in \
  alignmentDisappearanceIncomingCuts \
  alignmentDisappearanceOutgoingCuts
do
  require_text "$alignment_cut_accessor" "$herald_alignment_disappearance" \
    "Alignment disappearance adapter does not capture every live subscription cut: $alignment_cut_accessor"
done
for transfer_evidence_accessor in \
  destinationSubscriptionHasSnapshotTranscript \
  destinationSubscriptionAppliedSnapshotBaseRevision \
  destinationSubscriptionSnapshotFacts \
  destinationSubscriptionChanges
do
  require_text "$transfer_evidence_accessor" "$herald_alignment_transfer_owner" \
    "Alignment Transfer does not expose its narrow retained evidence: $transfer_evidence_accessor"
  require_text "Transfer.$transfer_evidence_accessor" "$herald_alignment_disappearance" \
    "Alignment disappearance does not consume Transfer-owned evidence: $transfer_evidence_accessor"
done
require_text 'retainedPlacementRouteEntries' "$herald_placement_owner" \
  "Placement does not expose its normalized retained route history"
require_text 'currentPlacementRouteEntries owner' "$herald_placement_disappearance" \
  "Placement disappearance does not restrict blockers to current route dependencies"
require_text 'UnresolvedStructuralReferenceBlocker' "$herald_publication_disappearance" \
  "Publication does not retain unresolved structural-reference evidence"
for carried_sort_classifier in \
  canonicalPublicationReferencesSubjectSort \
  checkedPublicationReferencesSubjectSort \
  peerPublicationReferencesSubjectSort \
  valueReferencesSubjectSort
do
  require_text "$carried_sort_classifier" "$herald_disappearance_owner_evidence" \
    "owner evidence omits a carried-sort classifier: $carried_sort_classifier"
done
for retained_alignment_work in \
  bootstrapImportEntries \
  bootstrapImportAttemptEntries \
  pendingGenerationEvidenceEntries
do
  require_text "Alignment.$retained_alignment_work" "$herald_alignment_disappearance" \
    "Alignment disappearance omits retained owner work: $retained_alignment_work"
done
for owner_evidence_symbol in \
  OwnerEvidenceSnapshot \
  ownerEvidenceSnapshotSubject \
  ownerEvidenceSnapshotBlockers \
  ownerEvidenceSnapshotAbsenceDigest \
  ownerEvidenceSnapshotFactCount
do
  require_text "$owner_evidence_symbol" "$herald_disappearance_owner_evidence" \
    "normalized owner-evidence support is missing: $owner_evidence_symbol"
done
for evidence_snapshot_symbol in \
  disappearanceEvidenceSnapshot \
  disappearanceEvidenceSnapshotSubject \
  disappearanceEvidenceSnapshotLocalPublicationCut \
  disappearanceEvidenceSnapshotIncomingAlignmentCuts \
  disappearanceEvidenceSnapshotOutgoingAlignmentCuts \
  disappearanceEvidenceSnapshotBlockers \
  disappearanceEvidenceSnapshotAbsenceAttestation \
  disappearanceEvidenceSnapshotMatchingPublications \
  disappearanceEvidenceSnapshotBlockerCount
do
  require_text "$evidence_snapshot_symbol" "$herald_disappearance_evidence" \
    "owner-composed disappearance snapshot is missing: $evidence_snapshot_symbol"
done
for readiness_symbol in \
  CurrentMembershipCapture \
  EstablishedStructuralBaseCapture \
  captureCurrentMembership \
  captureEstablishedStructuralBase \
  disappearanceOpenContextFromOwnerCaptures
do
  require_text "$readiness_symbol" "$herald_graph_disappearance_readiness" \
    "owner-backed disappearance Open readiness is missing: $readiness_symbol"
done
for owner_evidence_property in \
  'actual empty owner views compose the controlled and regular attestations' \
  'actual non-empty owners expose their exact blocker and matching-write classes' \
  'an ordinary accepted owner write feeds matching invalidation' \
  'accepted structural work is an unsequenced publication blocker' \
  'a structural carrier exposes its embedded regular-sort dependency' \
  'checked Nabla and Delta peer carriers expose their embedded regular-sort dependency' \
  'retained pending generation evidence blocks every subject it may still name' \
  'bootstrap imports and active attempts expose their retained alignment work' \
  'only uncovered debt and current certificates remain live alignment evidence' \
  'superseded placement routes remain retained without blocking disappearance' \
  'owner composition rejects a mixed-subject snapshot' \
  'Open readiness joins actual Oracle membership and established Graph base' \
  'Open readiness rejects successor membership before its Graph base exists'
do
  require_text "$owner_evidence_property" "$herald_disappearance_evidence_properties" \
    "owner-backed disappearance property is not registered: $owner_evidence_property"
done

# Evidence.Internal is the one raw snapshot assembly seam. Production uses it
# only from the checked composer; the named prospective fixture is the sole
# test-only importer allowed to inject a synthetic owner snapshot.
evidence_internal_importers=$(
  rg -l --glob '*.hs' \
    '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Herald\.Disappearance\.Evidence\.Internal([[:space:](]|$)' \
    herald-core 2>/dev/null || :
)
evidence_internal_importer_count=$(printf '%s\n' "$evidence_internal_importers" | sed '/^$/d' | wc -l | tr -d ' ')
[ "$evidence_internal_importer_count" -eq 3 ] ||
  fail "Evidence.Internal import wall differs from composer + prospective fixture:\n$evidence_internal_importers"
for evidence_internal_importer in $evidence_internal_importers
do
  case "$evidence_internal_importer" in
    "$herald_disappearance_evidence"|"$herald_prospective_fixtures"|"$herald_disappearance_state") ;;
    *) fail "Evidence.Internal escaped into $evidence_internal_importer" ;;
  esac
done

# All real owner adapters feed the one detached composer. Increment-8 whole-state
# validation may additionally read the epoch-aware adapter projections when it
# checks that retained retirement history has no live stale work. No other live
# Herald owner/coordinator may reach around the composer before Increment 9.
owner_adapter_importers=$(
  rg -l --glob '*.hs' \
    '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Herald\.(Alignment|Application|Controlled|Graph|PeerStream|Placement|Publication|Store)\.Disappearance([[:space:](]|$)' \
    herald-core/src herald-core/internal 2>/dev/null || :
)
for owner_adapter_importer in $owner_adapter_importers
do
  case "$owner_adapter_importer" in
    "$herald_disappearance_evidence"|"$startup_invariant") ;;
    *) fail "owner-backed disappearance adapter escaped into $owner_adapter_importer" ;;
  esac
done

# Raw factories remain confined to their actual owners plus the named
# prospective fixtures/properties/translator. This lets tests express corrupt
# or early schedules without giving production coordinators minting authority.
restrict_symbol_sources \
  assembleDisappearanceEvidenceSnapshot \
  "$herald_disappearance_evidence_internal" \
  "$herald_disappearance_evidence" \
  "$herald_prospective_fixtures"
restrict_symbol_sources \
  disappearanceBlockerWitness \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_owner_evidence" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  matchingPublicationObservation \
  "$herald_disappearance_protocol" \
  "$herald_publication_disappearance" \
  "$herald_disappearance_evidence_properties" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  disappearanceAbsenceAttestation \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_evidence" \
  "$herald_prospective_fixtures" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  projectedDisappearanceProbe \
  herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs \
  herald-core/test/PeerInputProperties.hs \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_evidence_properties" \
  "$herald_controlled_removal_properties" \
  "$herald_regular_retirement_acceptance_properties" \
  "$herald_prospective_projection" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  projectedLabelTerminal \
  herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs \
  "$herald_disappearance_protocol" \
  "$herald_prospective_projection" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  completedIncomingAlignmentEvidenceForOwner \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_state" \
  "$herald_prospective_properties"
restrict_symbol_sources \
  localAbsenceReportForOwner \
  "$herald_disappearance_protocol" \
  "$herald_disappearance_state" \
  "$herald_controlled_removal_properties" \
  "$herald_prospective_properties"

for herald_prospective_symbol in \
  localTakeCandidateObservation \
  projectedDisappearanceProbe \
  disappearancePublicationMarkerItem \
  disappearanceAlignmentMarker \
  completedIncomingAlignmentEvidenceForOwner \
  disappearanceAbsenceAttestation \
  localAbsenceReportForOwner
do
  if ! rg -q --fixed-strings -- "$herald_prospective_symbol" \
    "$herald_disappearance_protocol" "$herald_disappearance_state" "$herald_disappearance_use_case"
  then
    fail "private Herald disappearance seam is missing: $herald_prospective_symbol"
  fi
done
for absence_attestation_class in \
  ApplicationStoreAbsence \
  PublicationWorkAbsence \
  PeerStreamWorkAbsence \
  AlignmentWorkAbsence \
  GraphDependencyAbsence \
  ControlledUseAbsence
do
  require_text "$absence_attestation_class" "$herald_disappearance_protocol" \
    "checked absence-attestation class is absent: $absence_attestation_class"
done
require_text 'completedIncomingAlignmentEvidenceSource' \
  "$herald_disappearance_protocol" \
  "completed alignment evidence does not retain its authenticated source"
# The matching-write gate is a leaf-owned consequence of projected Open. Its
# hidden constructor must not migrate back into Protocol or become a token that
# a coordinator can mint independently.
require_text '    MatchingWriteGate,' "$herald_disappearance_state" \
  "the matching-write gate is not exported opaquely by the leaf"
forbid_text 'MatchingWriteGate (..)' "$herald_disappearance_state" \
  "the matching-write gate constructor escaped the leaf"
require_text 'matchingWriteGateForProbe' "$herald_disappearance_state" \
  "the matching-write gate has no read-only leaf observation"
forbid_text 'matchingWriteGateForOwner' "$herald_disappearance_protocol" \
  "Protocol can still mint a matching-write gate outside the leaf transition"
require_count 3 '^  MatchingWriteGate ->$' "$herald_disappearance_state" \
  "the leaf transitions and validator must require the opaque matching-write gate"
require_count 1 '^  Disappearance\.MatchingWriteGate ->$' "$herald_disappearance_use_case" \
  "the matching-publication coordinator does not require the leaf-owned gate"
require_text 'matchingPublicationPosition ::' "$herald_disappearance_protocol" \
  "a matching-publication observation does not expose its exact accepted Herald position"
require_text '(matchingPublicationPosition observation) probe.localPublicationCut' \
  "$herald_disappearance_state" \
  "matching-publication admission does not compare the exact position with the captured local cut"
require_text 'DisappearanceMatchingPublicationAtOrBeforeCut' \
  "$herald_disappearance_state" \
  "at-or-before-cut matching work has no explicit rejection"
require_text 'at-or-before-cut work cannot be classified as a matching invalidation' \
  "$herald_prospective_properties" \
  "the at-or-before-cut matching rejection is not exercised"

# Pin the first attributable cut/evidence formula rather than accepting a
# generous aggregate threshold.
require_text '  7 * dimensions.members' "$herald_disappearance_state" \
  "the worst-case captured-member work coefficient is not 7"
require_text '    + 4 * dimensions.publicationMarkers' "$herald_disappearance_state" \
  "the publication-marker work coefficient is not 4"
require_text '    + 3 * dimensions.alignmentMarkers' "$herald_disappearance_state" \
  "the alignment-marker work coefficient is not 3"
require_text '    + 2 * dimensions.matchingWork' "$herald_disappearance_state" \
  "the matching-work coefficient is not 2"

# Production owner imports are checked by the package module-boundary graph.
# The state imports only Evidence.Internal read accessors, avoiding a dependency
# back through the whole-Herald evidence composer.
forbid_text 'assembleDisappearanceEvidenceSnapshot' "$herald_disappearance_state" "disappearance leaf acquired raw evidence assembly authority"
for prospective_request_symbol in \
  ProspectiveRequestLedger \
  prospectiveOutstandingRequestCount \
  retainOpenRequest \
  retainReportRequest \
  retainInvalidationRequest \
  retainResolveRequest \
  settleProspectiveRequest \
  releaseProspectiveOpenRequest \
  submitRetainedOpenRequest
do
  require_text "$prospective_request_symbol" "$herald_prospective_harness" \
    "the test-only prospective request ledger is missing: $prospective_request_symbol"
done
require_text 'a settled request cannot be dispatched again' \
  "$herald_prospective_properties" \
  "prospective request settlement does not suppress further dispatch"
require_text 'a stale exact-ref release is classified duplicate' \
  "$herald_prospective_properties" \
  "prospective terminal request release is not idempotent"
require_text 'a stale exact-ref release cannot delete the fresh ABA binding' \
  "$herald_prospective_properties" \
  "prospective terminal request release is not ABA-safe"
for herald_prospective_property in \
  "absence attestations enforce each subject arm's exact checked class set" \
  'completed alignment evidence binds its authenticated source' \
  'the projection translator covers every command and compound context arm' \
  'prospective Oracle intentions bind, settle, and retain stable request references' \
  'projected Open atomically installs the leaf-owned matching-write gate' \
  'one exact coordinate excludes competitors and terminal release requires settled authority' \
  'alignment marker repair replay preserves identity and charges work once' \
  'a marker first received after terminalization drains as obsolete' \
  'owner evidence snapshots charge exact fact deltas and replay is inert' \
  'only gate-qualified strictly post-cut matching work invalidates' \
  'three racing opener intentions and alias projections reach the worst-case bound' \
  'the symbolic work formula keeps each dimension independent'
do
  require_text "$herald_prospective_property" "$herald_prospective_properties" \
    "prospective Herald disappearance evidence is not registered: $herald_prospective_property"
done
require_text 'MarkerTerminallyObsolete -> Completed' "$herald_disappearance_use_case" \
  "a terminally obsolete disappearance marker can wedge the generic peer stream"
require_text 'reviseControlledDisappearanceSubject' "$domain_disappearance" \
  "controlled disappearance subject revision has no checked domain helper"
for domain_disappearance_property in \
  'controlled subject revision changes only the revision' \
  'regular subjects reject controlled revision'
do
  require_text "$domain_disappearance_property" "$domain_disappearance_properties" \
    "controlled disappearance revision property is not registered: $domain_disappearance_property"
done
require_count 5 '^  (=|\|) Reference(OpenDisappearanceProbe|ReportPredefinedAbsence|InvalidateDisappearanceProbe|ResolveDisappearanceProbe|AbortDisappearanceProbe)\b' \
  "$prospective_reference" \
  "reference disappearance command family is not exactly Open/Report/Invalidate/Resolve/Abort"
require_count 5 '^  (=|\|) Disappearance(Open|Report|Invalidate|Resolve|Abort)\b' "$prospective_target" "live disappearance command payload has drifted"
require_text 'Step16DisappearanceProperties' "$oracle_core_cabal" "live Oracle property module absent"

# Increment 7 factors one role-qualified controlled-removal applicator without
# making disappearance live.  Label deletion and the detached disappearance
# leaf have distinct checked authority entry points; only the normalized owner
# fallout is shared.
require_count 2 '^  (=|\|) (LabelControlledRemoval|DisappearanceControlledRemoval)\b' \
  "$herald_controlled_removal" \
  "controlled-removal origin is not exactly label/disappearance"
require_count 2 '^  (=|\|) (ControlledRemovalApplied|ControlledRemovalExactReplay)\b' \
  "$herald_controlled_removal" \
  "controlled-removal disposition is not exactly applied/replay"
for controlled_removal_symbol in \
  ControlledRemovalSummary \
  controlledRemovalSummaryOrigin \
  controlledRemovalSummaryObject \
  controlledRemovalSummaryRole \
  controlledRemovalSummaryDisposition \
  controlledRemovalSummaryStorePurgedFacts \
  controlledRemovalSummaryCancelledReservations \
  controlledRemovalSummaryFalloutCount \
  controlledRemovalSummaryLogicalWork \
  PreparedControlledRemoval \
  prepareLabelControlledRemoval \
  prepareDisappearanceControlledRemoval \
  commitControlledRemoval
do
  require_text "$controlled_removal_symbol" "$herald_controlled_removal" \
    "shared controlled-removal applicator is missing: $controlled_removal_symbol"
done
forbid_text 'PreparedControlledRemoval (..)' "$herald_controlled_removal" \
  "shared controlled-removal preparation exposes its constructor"
require_text '    ControlledRemovalAuthority,' "$herald_disappearance_state" \
  "disappearance leaf does not export opaque controlled-removal authority"
forbid_text 'ControlledRemovalAuthority (..)' "$herald_disappearance_state" \
  "controlled-removal authority constructor escaped the disappearance leaf"
for disappearance_authority_accessor in \
  controlledRemovalAuthorityProbe \
  controlledRemovalAuthoritySubject \
  controlledRemovalAuthorityCoordinate \
  controlledRemovalAuthorityObject \
  controlledRemovalAuthorityStructuralOccurrence \
  controlledRemovalAuthorityExpectedLabelRevision \
  controlledRemovalAuthorityResolveIndex
do
  require_text "$disappearance_authority_accessor" "$herald_disappearance_state" \
    "controlled-removal authority omits: $disappearance_authority_accessor"
done
require_text 'prepareProjectedControlledResolution' "$herald_disappearance_state" \
  "the disappearance leaf cannot prepare its single-use removal authority"
require_text 'coordinateProjectedControlledResolution' "$herald_disappearance_use_case" \
  "the detached disappearance coordinator does not apply controlled removal atomically"
require_text 'ControlledRemoval.prepareDisappearanceControlledRemoval authority herald' \
  "$herald_disappearance_use_case" \
  "the detached coordinator bypasses the shared disappearance entry point"
require_text 'ControlledRemoval.prepareLabelControlledRemoval' "$herald_oracle_advance" \
  "live label delete does not use the shared controlled-removal entry point"
require_text 'ReleasedDeletedView {} -> do' "$herald_oracle_advance" \
  "label deletion is not separated from nonterminal label topology refresh"
require_text '(ReleasedDeletedView _, _) -> controlledWithRelease' \
  "$herald_oracle_advance" \
  "label deletion pre-retires Nabla reservations before the shared applicator can count them"
require_text 'structuralAppliedControlledProjectionAtOccurrence' \
  "$herald_structural_reconciliation" \
  "disappearance authority cannot authenticate its immutable captured occurrence"
require_text '"Eclips.Herald.UseCase.ControlledRemoval" ->' \
  "$module_boundary_policy" \
  "the shared controlled-removal coordinator is absent from the Herald import policy"

# Controlled owns the payload-free terminal record, possession revocation, and
# canonical pristine-Nabla-reservation cancellation report.
# The transient sort/key coordinate is available only to the enclosing prepared
# transaction; Store independently owns purge history and stale suppression.
for controlled_terminal_symbol in \
  ControlledTerminalDeletion \
  controlledTerminalDeletion \
  controlledTerminalDeletionCause \
  controlledTerminalDeletionEntries \
  controlledObjectTerminallyDeleted \
  ControlledRemovalPayloadCoordinate \
  prepareControlledRemoval \
  preparedControlledRemovalPayloadCoordinate \
  preparedControlledRemovalRevokedDirectPossessions \
  preparedControlledRemovalRevokedStorePossessions \
  prepareControlledNablaReservationRetirement \
  preparedControlledNablaReservationRetirementCancelled \
  commitControlledNablaReservationRetirement \
  commitControlledRemoval
do
  require_text "$controlled_terminal_symbol" "$controlled_owner" \
    "Controlled terminal-removal owner is incomplete: $controlled_terminal_symbol"
done
forbid_text 'ControlledTerminalDeletion (..)' "$controlled_owner" \
  "payload-free Controlled terminal deletion exposes its constructor"
forbid_text 'ControlledRemovalPayloadCoordinate (..)' "$controlled_owner" \
  "ephemeral Store purge coordinate exposes its constructor"
require_text 'controlledObjectKey' "$domain_sort_descriptor" \
  "controlled objects have no canonical one-direct-ID Store key helper"
require_text 'purgeObjectKey' "$domain_store" \
  "Domain Store lacks the projection-wide terminal purge primitive"
require_text 'terminal purge removes every projection and is idempotent' \
  "$domain_store_properties" \
  "Domain Store terminal-purge law is not registered"
for store_purge_symbol in \
  storeSlotTerminalObjectPurges \
  storeSlotTerminalObjectPurgeCause \
  storeTerminalObjectPurgeEntries \
  removeTerminalObjectPurgeForInvariantTest \
  ControlledObjectPurgeSummary \
  prepareControlledObjectPurge \
  commitControlledObjectPurge \
  PeerStoreTerminallyIgnored
do
  require_text "$store_purge_symbol" "$herald_store_owner" \
    "Store terminal-purge owner is incomplete: $store_purge_symbol"
done
require_text 'terminalObjectPurgesForSort specification.sortId state' \
  "$herald_store_owner" \
  "a later Store incarnation does not inherit terminal suppression"
require_text 'controlled purge suppresses every retained incarnation and stale replay' \
  "$herald_peer_store_properties" \
  "multi-incarnation Store purge/stale-replay property is not registered"
require_text 'a zero-match controlled purge is inherited by the first Store incarnation' \
  "$herald_peer_store_properties" \
  "Store suppression can be lost when deletion precedes the first incarnation"
require_text 'the coherently omitted purge leaves Store history internally valid' \
  "$herald_controlled_removal_properties" \
  "whole-Herald validation does not distinguish a missing Store purge ledger from Store-owner corruption"
for terminal_structural_property in \
  'a label-deleted Delta current is transcript-only without a Store-local tombstone' \
  'a label-deleted Edge current retains history without recreating topology'
do
  require_text "$terminal_structural_property" \
    "$herald_peer_input_properties" \
    "terminal structural traffic can still recreate live state: $terminal_structural_property"
done

# Disappearance consequences use a private, honestly typed control provenance.
# Its canonical tag is pinned locally, but the still-current EPRP projection
# must refuse it rather than fabricate a pre-cutover DTO arm.
require_text 'PredefinedDisappearanceCause DisappearanceProbeId ControlIndex' \
  "$domain_structural_consequence" \
  "predefined disappearance has no distinct structural-consequence cause"
require_text 'predefinedDisappearanceCause' "$domain_structural_consequence" \
  "predefined disappearance cause has no checked constructor"
require_text 'Serialize.putWord8 3' "$domain_structural_consequence" \
  "predefined disappearance consequence does not retain canonical tag 3"
for consequence_property in \
  'a predefined disappearance cannot claim control zero' \
  'predefined disappearance retains its ordered control position' \
  'predefined-disappearance tag, probe, then control index'
do
  require_text "$consequence_property" "$domain_structural_properties" \
    "predefined-disappearance consequence law is not registered: $consequence_property"
done
require_text 'PredefinedDisappearanceCauseDto' "$herald_peer_rpc_internal" "live peer bridge omits disappearance cause"
forbid_text 'PredefinedDisappearanceCauseNotEncodable' "$herald_peer_rpc_public" "pre-cutover cause refusal survived"

# Pin exact work and whole-owner validation at the first applicator checkpoint.
require_text '4 + controlledRemovalSummaryFalloutCount summary' \
  "$herald_controlled_removal" \
  "fresh controlled removal is not charged as exact 4 + F"
require_text 'purged + direct + store + reservations + graph + placement + debts + losses + completedWaits' \
  "$herald_controlled_removal" \
  "controlled-removal F omits an owner-local fallout class"
require_text 'ControlledRemovalExactReplay -> 0' "$herald_controlled_removal" \
  "exact controlled-removal replay is not a zero-work control"
for controlled_work_assertion in \
  'the summary F independently matches observable owner fallout' \
  'Increment-7 work is independently exactly 4 + F' \
  'cancelled reservations' \
  'shared-applicator replay reports zero fallout' \
  'shared-applicator replay reports zero logical work'
do
  require_text "$controlled_work_assertion" "$herald_controlled_removal_properties" \
    "controlled-removal work evidence is missing: $controlled_work_assertion"
done
require_text 'ControlledTerminalDeletionInvariant' "$startup_invariant" \
  "whole-state validation omits controlled terminal deletion"
require_text 'validateControlledTerminalDeletions state' "$startup_invariant" \
  "whole-state validation does not execute the controlled terminal check"
require_text 'structuralAppliedRetainedObjectRoles reconciliation' \
  "$startup_invariant" \
  "terminal deletion does not rule out a structurally identified active Delta Store"
require_text 'validateStructuralAppliedState state.reconciliation' \
  "$herald_graph_progress" \
  "structural progress does not validate immutable control history"
require_text 'structuralAppliedControlHistory reconciliation' \
  "$startup_invariant" \
  "whole-state validation authenticates only the current control overlay"
require_text 'StructuralAppliedCurrentControlOverlayMismatch' \
  "$herald_structural_reconciliation" \
  "retained control history is not checked against the current overlay"
require_text 'a payload-free terminal tombstone hides retained history' \
  "$herald_effective_publication_properties" \
  "effective publication can still expose terminally removed payload history"
require_text 'ControlledRemovalProperties.tests' "$herald_properties_main" \
  "controlled-removal properties are not registered in the Herald suite"
require_text 'ControlledRemovalProperties' "$herald_core_cabal" \
  "controlled-removal property module is absent from the Herald component"
for controlled_removal_property in \
  'Resolve removes every live owner atomically and exact replay is inert' \
  'all four eligible structural roles share the terminal removal contract' \
  'disappearance authority authenticates its exact captured occurrence' \
  'Nabla removal counts and cancels every pristine child reservation' \
  'Delta removal wakes a wait on its passivated Store incarnation' \
  'Delta removal does not count a wait already removed by connection loss' \
  'post-disappearance Label and republish calls cannot resurrect the object' \
  'a terminal tombstone rejects restored live structural ownership' \
  'matching released-label revision authorizes removal and stale revision is inert' \
  'label-delete and disappearance share one normalized removal applicator'
do
  require_text "$controlled_removal_property" "$herald_controlled_removal_properties" \
    "composed controlled-removal property is not registered: $controlled_removal_property"
done
require_text 'terminal purge preserves raw history across an unrelated retained update' \
  "$herald_peer_store_properties" \
  "terminal Store suppression can corrupt immutable history after an unrelated update"
require_text 'completeAlignmentWaits afterControls' "$alignment_transfer" \
  "alignment loss does not release waits after exact Store passivation"
require_text 'queryInvalidated' "$alignment_transfer" \
  "wait completion does not recognize a passivated exact Store branch"
require_text 'matching publication and relabel races obey projected terminal order' \
  "$herald_disappearance_evidence_properties" \
  "controlled disappearance publication/relabel race property is not registered"
for controlled_resolution_authority_property in \
  'controlled Resolve mints authority once and exact replay is state-identical' \
  'regular Resolve, Invalidated, and Abort cannot mint controlled-removal authority'
do
  require_text "$controlled_resolution_authority_property" \
    "$herald_disappearance_evidence_properties" \
    "controlled-resolution authority property is not registered: $controlled_resolution_authority_property"
done
for retained_label_delete_property in \
  'a genesis delta delete removes live topology and cannot be resurrected' \
  'genesis Nabla deletion retires current authority without erasing history' \
  'an unrouted ordinary controlled delete is inherited by the first later Store'
do
  require_text "$retained_label_delete_property" "$herald_application_label_properties" \
    "shared removal regressed an established label-delete property: $retained_label_delete_property"
done

# Increment 8 adds only the detached regular-definition retirement transaction;
# the live Oracle/EPRP cutover remains reserved for Increment 9.  This is the
# fast source-shape gate, not by itself the Increment-8 acceptance claim.
# Terminal authority is one-shot, Registry and Store history are monotone, and
# a later equal definition crosses the Resolve index on a new contained
# occurrence while its enclosing sort-sort carrier remains primordial.
require_text '    RegularRetirementAuthority,' "$herald_disappearance_state" \
  "disappearance leaf does not export opaque regular-retirement authority"
forbid_text 'RegularRetirementAuthority (..)' "$herald_disappearance_state" \
  "regular-retirement authority constructor escaped the disappearance leaf"
for regular_authority_accessor in \
  regularRetirementAuthorityProbe \
  regularRetirementAuthoritySubject \
  regularRetirementAuthorityCoordinate \
  regularRetirementAuthoritySortId \
  regularRetirementAuthorityDescriptorDigest \
  regularRetirementAuthorityOccurrence \
  regularRetirementAuthorityResolveIndex \
  regularRetirementAuthoritySuccessorOccurrence \
  regularRetirementAuthorityEvidenceSnapshot \
  regularRetirementAuthorityLocalPublicationCut
do
  require_text "$regular_authority_accessor" "$herald_disappearance_state" \
    "regular-retirement authority omits: $regular_authority_accessor"
done
require_text 'prepareProjectedRegularResolution' "$herald_disappearance_state" \
  "the disappearance leaf cannot prepare regular retirement authority"
require_text 'coordinateProjectedRegularResolution' "$herald_disappearance_use_case" \
  "the detached disappearance coordinator does not apply regular retirement atomically"
require_text 'DisappearanceRegularResolutionRequiresOwnerCoordination' \
  "$herald_disappearance_use_case" \
  "generic terminal coordination can bypass regular-retirement ownership"
require_text 'RegularRetirement.prepareDisappearanceRegularRetirement' \
  "$herald_disappearance_use_case" \
  "the detached coordinator bypasses the regular-retirement applicator"

for regular_retirement_symbol in \
  RegularRetirementSummary \
  regularRetirementSummaryMatchingWork \
  regularRetirementSummaryPurgedStoreFacts \
  regularRetirementSummaryFalloutCount \
  regularRetirementSummaryLogicalWork \
  PreparedRegularRetirement \
  prepareDisappearanceRegularRetirement \
  commitRegularRetirement
do
  require_text "$regular_retirement_symbol" "$herald_regular_retirement" \
    "regular-retirement applicator is missing: $regular_retirement_symbol"
done
forbid_text 'PreparedRegularRetirement (..)' "$herald_regular_retirement" \
  "regular-retirement preparation exposes its constructor"
forbid_text 'replaceStartupControlledState' "$herald_regular_retirement" \
  "regular retirement can create controlled lifecycle state"
require_text 'Evidence.regularRetirementEvidenceSnapshotForHerald' \
  "$herald_regular_retirement" \
  "regular retirement does not use the Resolve-aware full owner recheck"
require_text 'regularRetirementEvidenceSnapshotForHerald' \
  "$herald_disappearance_evidence" \
  "the evidence adapter lacks the Resolve-aware full owner snapshot"
require_text '4 + regularRetirementSummaryFalloutCount summary' \
  "$herald_regular_retirement" \
  "fresh regular retirement is not charged as exact 4 + W + F"
require_text 'RegularRetirementExactReplay -> 0' "$herald_regular_retirement" \
  "regular-retirement replay is not a zero-work control"

for registry_retirement_symbol in \
  RegularSortRetirementView \
  regularSortRetirements \
  regularSortReferenceControlPrerequisite \
  prepareExactRegularSortRetirement \
  prepareUnseenRegularSortRetirement \
  regularSortRetirementDescriptorDigest \
  SortInductionPlan \
  planSortInduction \
  sortInductionPlanOccurrenceId \
  sortInductionPlanControlPrerequisite
do
  require_text "$registry_retirement_symbol" "$herald_sort_registry_owner" \
    "SortRegistry retirement epoch is missing: $registry_retirement_symbol"
done
for store_retirement_symbol in \
  RegularRetirementSuppression \
  classifyRegularDefinitionWrite \
  prepareRegularDefinitionRetirement \
  projectEffectiveStoreContents \
  observationSuppressedByRegularRetirement \
  retainedStoreAlignmentSnapshotAt
do
  require_text "$store_retirement_symbol" "$herald_store_owner" \
    "Store regular-retirement projection is missing: $store_retirement_symbol"
done
require_text 'SortRegistry.planSortInduction' "$herald_application_publication" \
  "local equal redefinition does not use the current retirement epoch"
require_text 'SortRegistry.planSortInduction' "$herald_peer_input" \
  "peer equal redefinition does not use the current retirement epoch"
require_text 'PeerInputDefinitionControlPrerequisiteMismatch' "$herald_peer_input" \
  "peer redefinition does not authenticate the Resolve prerequisite"
require_text 'applicationPublicationStructuralReferenceControlPrerequisite' \
  "$herald_application_publication" \
  "local Nabla/Delta publication does not retain its embedded-sort control floor"
require_text 'SortRegistry.regularSortReferenceControlPrerequisite' \
  "$herald_application_publication" \
  "local embedded-sort control floor is not obtained from the SortRegistry epoch"
require_count 2 \
  'ApplicationPublication.applicationPublicationStructuralReferenceControlPrerequisite' \
  "$herald_application_forward" \
  "first and fence-held forwards do not both join the embedded-sort retirement floor"
require_text 'first and fence-held structural forwards carry their referenced-sort retirement floor' \
  "$herald_forward_properties" \
  "forward retirement-floor regression is missing"
require_text 'structuralReferenceRetirementSuppression' "$herald_peer_input" \
  "peer Nabla/Delta admission does not recompute its embedded-sort retirement evidence"
require_text 'Reconciliation.structuralRetirementSuppressionControlPrerequisite' \
  "$herald_peer_input" \
  "peer edge admission does not inherit predecessor-covered endpoint retirement evidence"
require_text 'SortRegistry.latestRegularSortRetirement' "$herald_peer_input" \
  "peer embedded-sort retirement does not come from retained SortRegistry history"
require_text 'SortRegistry.regularSortRetirementResolveIndex' "$herald_peer_input" \
  "peer embedded-sort control floor is not obtained from the retained retirement epoch"
require_text 'equal definition after retirement uses the Resolve-derived inner occurrence' \
  "$herald_application_publication_properties" \
  "local equal redefinition does not exercise both carrier and contained coordinates"
require_text 'Nabla and Delta publications carry the retirement floor of their referenced sort' \
  "$herald_application_publication_properties" \
  "local structural publications do not exercise both embedded-sort carrier roles"

for held_coordinate_accessor in \
  applicationFenceHeldWorkSortId \
  applicationFenceHeldWorkSortOccurrenceId \
  applicationFenceHeldWorkControlPrerequisite
do
  require_text "$held_coordinate_accessor" "$application_owner" \
    "application fence-held work omits its exact carrier coordinate/floor: $held_coordinate_accessor"
  require_text "Application.$held_coordinate_accessor" \
    "$herald_application_disappearance" \
    "regular-retirement evidence does not consume held provenance: $held_coordinate_accessor"
done
for held_coordinate_property in \
  'both held operations retain the exact acceptance-time source sort' \
  'both held operations retain the exact acceptance-time source occurrence' \
  'both held operations retain their acceptance-time control floor'
do
  require_text "$held_coordinate_property" "$herald_application_label_properties" \
    "fence-held provenance property is missing: $held_coordinate_property"
done

require_text 'applicationRegularRetirementDisappearanceView' \
  "$herald_application_disappearance" \
  "Application has no epoch-aware regular-retirement residual view"
require_text 'alignmentRegularRetirementDisappearanceView' \
  "$herald_alignment_disappearance" \
  "Alignment has no epoch-aware regular-retirement residual view"
for alignment_settlement_symbol in \
  sourceSnapshotIsAcknowledged \
  sourceChangeIsAcknowledged \
  destinationEvidenceTerminallyIgnored
do
  require_text "$alignment_settlement_symbol" "$herald_alignment_disappearance" \
    "Alignment residual classification omits terminal settlement: $alignment_settlement_symbol"
done
for alignment_source_settlement_coordinate in \
  sourceSubscriptionCancellation \
  sourceSubscriptionAcknowledgedThrough \
  sourceSubscriptionBaseRevision
do
  require_text "Transfer.$alignment_source_settlement_coordinate" \
    "$herald_alignment_disappearance" \
    "Alignment source residual settlement omits: $alignment_source_settlement_coordinate"
done
require_text 'Transfer.destinationSubscriptionCancellation' \
  "$herald_alignment_disappearance" \
  "Alignment destination residual settlement ignores cancellation"
for alignment_destination_settlement_coordinate in \
  appliedDestinationEvidenceSubscription \
  appliedDestinationEvidenceDestination \
  appliedDestinationEvidencePublication \
  appliedDestinationEvidenceKind \
  appliedDestinationEvidenceSourceRevision \
  appliedDestinationEvidenceDisposition
do
  require_text "Transfer.$alignment_destination_settlement_coordinate" \
    "$herald_alignment_disappearance" \
    "Alignment destination terminal settlement omits: $alignment_destination_settlement_coordinate"
done
require_text 'Transfer.StoreTerminallyIgnored' "$herald_alignment_disappearance" \
  "Alignment destination residual settlement accepts a non-terminal disposition"
for shared_cut_selection_path in \
  'groups <- pendingGroupsAtPlacementFor coordinates selection vector reset' \
  'mapLocalPlanAsProtocol (pendingGroupsAtPlacement selection placement state)' \
  'pendingGroupsAtPlacement selection placement state = pendingGroupsAtPlacementFor (alignmentDebtCoordinates (startupAlignmentState state)) selection placement state'
do
  require_text "$shared_cut_selection_path" "$herald_alignment_coordinator" \
    "selective and anchor alignment paths do not share canonical recovery-cut selection: $shared_cut_selection_path"
done
require_count 2 'Alignment.liveAlignmentStructuralDebtEntries alignment' \
  "$herald_alignment_coordinator" \
  "alignment recovery still scans covered immutable debt as executable work"
# Current routing plans may carry generations born at older coordinates. The
# explicit predecessor and its status replace selection by birth coordinate;
# an invalidated plan remains frontier evidence but supplies no live ancestry.
herald_alignment_plan=herald-core/internal/Eclips/Herald/Alignment/Plan.hs
herald_alignment_plan_properties=herald-core/test/AlignmentPlanProperties.hs
herald_live_alignment_plan_properties=herald-core/test/LiveAlignmentPlanProperties.hs
for current in "$herald_alignment_plan" "$herald_alignment_plan_properties" "$herald_live_alignment_plan_properties"
do
  require_file "$current"
done
for current_plan_recovery_contract in \
  'Alignment.currentAlignmentPlanForSort group.affectedSort alignment' \
  'if Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId plan) alignment then Plan.AlignmentPredecessorInvalidated else Plan.AlignmentPredecessorUsable' \
  'Plan.prepareAlignmentPlanAtAttempt attempt group.affectedSort cut placement context members parent (retainedCertificateGenerations alignment)'
do
  require_text "$current_plan_recovery_contract" "$herald_alignment_coordinator" \
    "alignment recovery omits explicit current-plan predecessor admission: $current_plan_recovery_contract"
done
for invalidated_ancestry_contract in \
  'Just (prior, AlignmentPredecessorUsable) -> Map.fromList (alignmentPlanBindings prior)' \
  'priorGenerations = map alignmentPlanBindingGeneration (Map.elems priorBindings)' \
  'Just (prior, AlignmentPredecessorUsable) -> alignmentPlanRelations prior' \
  'carryBoundary = case predecessor of'
do
  require_text "$invalidated_ancestry_contract" "$herald_alignment_plan" \
    "alignment planning omits usable-predecessor ancestry admission: $invalidated_ancestry_contract"
done
for invalidated_ancestry_property in \
  'invalidated predecessor recovery does not wait for unavailable old certificates' \
  'old plan remains the explicit frontier predecessor' \
  'invalidated status is immutable plan evidence' \
  'no recovered class waits on lost old history' \
  'subsequently arriving old certificates cannot change recovered history'
do
  require_text "$invalidated_ancestry_property" "$herald_alignment_plan_properties" \
    "alignment recovery lacks an invalidated-predecessor regression: $invalidated_ancestry_property"
done
require_text 'mixed-age invalidation targets the current plan and preserves birth history' \
  "$herald_live_alignment_plan_properties" \
  "live alignment recovery lacks mixed-age current-plan invalidation coverage"
require_count 2 \
  'applicationRoutesMatchProjection affectedSort projection activeRoutes' \
  "$herald_alignment_coordinator" \
  "alignment plan admission does not retain its historical-projection/placement-route matcher"
for batched_cut_selection_contract in \
  'structuralDebtSetEntries (Alignment.alignmentStructuralDebts alignment)' \
  'advanceAlignmentGenerationStateUsing selected afterCurrent' \
  'drainPendingGenerationEvidence traversal selection afterPlans'
do
  require_text "$batched_cut_selection_contract" "$herald_alignment_coordinator" \
    "batched alignment recovery omits a shared query contract: $batched_cut_selection_contract"
done
for batched_cut_selection_contract in \
  'applicationProjectionRoutesBySort <$> GraphProgress.structuralProjectionAtInstalledCut views cut progress' \
  'installedDeltaRouteCoordinate snapshot (object, vertex)' \
  'case placementDeltaRouteCoordinate owner route of' \
  'IntMap.lookupGE lowerBound answers'
do
  require_text "$batched_cut_selection_contract" "$herald_alignment_cut_queries" \
    "batched alignment recovery omits a shared query contract: $batched_cut_selection_contract"
done
# Selective coordination keeps exact owner lifetimes and independent full-scan
# reference drivers. Shared scheduling metadata stays ordinary, validated state.
shared_work_index=herald-core/internal/Eclips/Herald/Internal/WorkIndex.hs
alignment_generation_work_properties=herald-core/test/AlignmentGenerationWorkProperties.hs
alignment_transfer_work_properties=herald-core/test/AlignmentTransferWorkProperties.hs
for current in "$shared_work_index" "$alignment_generation_work_properties" "$alignment_transfer_work_properties"
do
  require_file "$current"
done
require_text 'module Eclips.Herald.Internal.WorkIndex' "$shared_work_index" \
  "ordered work-index utility has not moved below its semantic owners"
require_text 'Eclips.Herald.Internal.WorkIndex' "$herald_core_cabal" \
  "shared work-index module is not registered"
require_text '"Eclips.Herald.Internal.WorkIndex"' "$module_boundary_policy" \
  "shared work-index module is absent from the exact private catalogue"
for current in "$herald_core_cabal" "$module_boundary_policy"
do
  forbid_text 'Eclips.Herald.ProcessPreparation.WorkIndex' "$current" \
    "shared scheduling still uses the preparation-owned module name"
done
for module in AlignmentGenerationWorkProperties AlignmentTransferWorkProperties
do
  require_text "$module" "$herald_core_cabal" "selective coordination property module is not registered: $module"
  require_text "$module.tests" "$herald_properties_main" "selective coordination property suite is not executed: $module"
done
for property in \
  'pending evidence lifetime, replay and consumption match an independent model' \
  'historical generation insertion and member readiness wake exact evidence' \
  'sort grouping retains every live cause and replay stays quiet' \
  'promoting one sort cannot enable or wake another parked sort' \
  'parked certificates and reordered readiness match exhaustive traces' \
  'invalid evidence still closes the current binding in the original order' \
  'retired announcement queries are trimmed while unrelated evidence stays parked'
do
  require_text "$property" "$alignment_generation_work_properties" \
    "selective generation work lacks its retained-lifetime/order property: $property"
done
for property in \
  'destination progress journal matches exact transcript tuple changes' \
  'loss journal consumption preserves prepared cut-query entries' \
  'blocked attempts stay dormant on unrelated inputs' \
  'remote member evidence wakes only certificate work and agrees with full scan' \
  'bootstrap fulfillment waits for Live before readiness; every ready precedes every certificate'
do
  require_text "$property" "$alignment_transfer_work_properties" \
    "selective transfer work lacks its dependency/order property: $property"
done
require_text 'marker head work ignores blocked suffix changes and wakes exact dependencies' \
  herald-core/test/PeerStreamProperties.hs "ordered marker work lacks exact head/dependency coverage"
for contract in generationSchedulingValid transferSchedulingValid initializeTransferMemberWork
do
  require_text "$contract" herald-core/internal/Eclips/Herald/Alignment/State.hs \
    "selective Alignment owner omits its lifecycle contract: $contract"
done
require_text 'advanceAlignmentGenerationsWithDispositionExhaustive' "$herald_alignment_coordinator" \
  "selective generation work has no independent full-scan driver"
require_text 'advanceAlignmentTransferPassExhaustive' herald-core/internal/Eclips/Herald/UseCase/AlignmentTransfer.hs \
  "selective transfer work has no independent full-scan driver"
require_text 'AlignmentTransferDestinationProgressWorkInvariant' "$herald_alignment_transfer_owner" \
  "destination-change journal lacks its owner-lifetime invariant"
forbid_text 'PeerLogicalLabelFenceMarker' "$peer_payload" "obsolete label markers remain in the peer semantic union"
forbid_text 'PeerLabelFenceMarkerDto' "$peer_types" "obsolete label markers remain in the peer wire union"
forbid_text 'releasePendingLabelFenceMarkers' "$herald_peer_input" "obsolete label marker scheduling remains"
for family in AlignmentRouteCutovers DisappearanceProbeMarkers
do
  require_text "releasePending${family}Exhaustive" "$herald_peer_input" \
    "ordered marker family lacks independent full-scan release: $family"
done

# Cached answers are non-authoritative and must follow their retained inputs.
for retained_cut_query_contract in \
  'CutQueries.prepareCutQueryCache' \
  'CutQueries.trimCutQueryCache' \
  'alignmentCutReconciliationViews' \
  'replaceStartupAlignmentCutQueryCache retained state'
do
  require_text "$retained_cut_query_contract" "$herald_alignment_coordinator" \
    "retained alignment queries omit their preparation/retention boundary: $retained_cut_query_contract"
done
for retained_cut_query_property in \
  'retained cut queries equal fresh preparation through repeated scope churn' \
  'retained cut queries reuse answers without touching fresh source inputs' \
  'idle generation passes bypass fresh cut inputs and retire orphan vector queries' \
  'unchanged non-announcement evidence preserves cut queries without fresh placement reads' \
  'cut-query owner invalidation preserves semantic state equality' \
  'held marker-release coordinator passes retain cut queries' \
  'warm and cold cut caches produce identical pending-anchor transitions'
do
  require_text "$retained_cut_query_property" "$herald_alignment_coordinator_properties" \
    "retained alignment queries lack their reference property: $retained_cut_query_property"
done
for batched_cut_selection_property in \
  'batched recovery suffixes preserve ordered scans through holes, failures and changing bounds' \
  'batched recovery does not evaluate unreachable projections'
do
  require_text "$batched_cut_selection_property" "$herald_alignment_coordinator_properties" \
    "batched alignment recovery lacks its reference property: $batched_cut_selection_property"
done
require_text \
  'invalidated Delta plans recover at the first route-matching deletion cut' \
  "$herald_alignment_coordinator_properties" \
  "invalidated alignment-plan recovery lacks its end-to-end controlled-removal property"
require_text \
  'recovery selection ignores a later installed suffix and pins anchor replay' \
  "$herald_alignment_coordinator_properties" \
  "invalidated alignment-plan recovery lacks schedule-independence and anchor parity"
for invalidated_ancestry_property in \
  'an invalidated latest generation is not a replacement prerequisite' \
  'the historical predecessor really would wait for its impossible certificate' \
  'replacement planning retains no invalidated predecessor' \
  'replacement planning treats the complete member set as fresh'
do
  require_text "$invalidated_ancestry_property" \
    "$herald_alignment_coordinator_properties" \
    "invalidated alignment ancestry property is missing: $invalidated_ancestry_property"
done
require_text 'regularRetirementResidualBlockersForHerald' \
  "$herald_disappearance_evidence" \
  "whole-Herald validation has no epoch-aware residual composer"
require_text 'validateRetirementChain' "$startup_invariant" \
  "whole-state validation omits the ordered regular-retirement chain"
require_text 'length retirements == length suppressions' "$startup_invariant" \
  "whole-state validation does not require a Registry/Store retirement bijection"
require_text 'DisappearanceEvidence.regularRetirementResidualBlockersForHerald' \
  "$startup_invariant" \
  "whole-state validation does not reconstruct live retired-epoch residual work"
require_text 'null currentBlockers' \
  "$startup_invariant" \
  "whole-state validation can admit live delayed old-occurrence work"
require_text 'matchingPositionsCovered' "$startup_invariant" \
  "whole-state validation does not enforce exact local publication cut coverage"
require_text 'Store.regularRetirementSuppressionLocalPublicationCut' \
  "$startup_invariant" \
  "whole-state cut coverage is not anchored at the frozen retirement cut"
require_text 'PublicationDisappearance.publicationDisappearanceMatchingObservations' \
  "$startup_invariant" \
  "whole-state cut coverage is not derived from matching publication positions"
require_text 'RegularSortRetirementProperties.tests' "$herald_properties_main" \
  "regular SortRegistry properties are not registered in the Herald suite"
require_text 'RegularSortRetirementProperties' "$herald_core_cabal" \
  "regular SortRegistry property module is absent from the Herald component"
require_text 'RegularDefinitionStoreRetirementProperties.tests' "$herald_properties_main" \
  "regular Store retirement properties are not registered in the Herald suite"
require_text 'RegularDefinitionStoreRetirementProperties' "$herald_core_cabal" \
  "regular Store retirement property module is absent from the Herald component"
require_text 'Step16RegularRetirementAcceptanceProperties.tests' "$herald_properties_main" \
  "composed regular-retirement acceptance is not registered in the Herald suite"
require_text 'Step16RegularRetirementAcceptanceProperties' "$herald_core_cabal" \
  "composed regular-retirement acceptance module is absent from the Herald component"
for registry_retirement_property in \
  'fresh exact retirement removes only the regular effective entry' \
  'retirement history is immutable and an exact replay is a no-op' \
  'every admitted resolve index determines the successor coordinate and prerequisite' \
  'a stale retired occurrence cannot restore the old definition' \
  'the equal successor definition induces once and then repeats idempotently' \
  'two retirement and equal-redefinition rounds form an ordered chain'
do
  require_text "$registry_retirement_property" "$herald_regular_retirement_properties" \
    "regular SortRegistry property is not registered: $registry_retirement_property"
done
for regular_store_retirement_property in \
  'retirement purges only the effective projection' \
  'a stale lower-control reoffer is terminally ignored' \
  'an equal definition after Resolve is admitted and visible' \
  'a post-Resolve definition can outrank a suppressed cross-writer predecessor by epoch' \
  'retirement counts a replaced same-key Store winner as removed work' \
  'an unrelated later write cannot resurrect the retired definition' \
  'retirement preserves an unrelated local take in the same Store' \
  'a receipt-only fallback remains revision-addressable for alignment after retirement' \
  'exact retirement replay after redefinition is a whole-state no-op' \
  'classification separates covered, unordered, and post-Resolve writes' \
  'a second retirement cycle suppresses only through its later Resolve'
do
  require_text "$regular_store_retirement_property" \
    "$herald_regular_store_retirement_properties" \
    "regular Store retirement property is not registered: $regular_store_retirement_property"
done
require_text 'retired peer definitions and structural references respect the Resolve-qualified successor' \
  "$herald_peer_input_properties" \
  "the live peer path does not exercise stale settlement and successor induction"
for peer_structural_floor_property in \
  'stale structural reference is terminally ignored' \
  'stale reference performs no Store work' \
  'stale reference performs no Graph work' \
  'stale reference performs no Placement work' \
  'Resolve-qualified structural reference applies'
do
  require_text "$peer_structural_floor_property" "$herald_peer_input_properties" \
    "peer structural reference-floor property is missing: $peer_structural_floor_property"
done
for regular_retirement_acceptance_property in \
  'fresh resolution retires atomically, replay is inert, and equal redefinition advances coordinates' \
  'complete exact authority replay remains inert after a coherent membership advance' \
  'a never-applied predecessor-membership authority is rejected after a coherent membership advance' \
  'a resolve-time owner blocker rejects before either successor commits' \
  'a matching definition written after the frozen cut rejects old retirement authority' \
  'alignment rolls back stale Nabla and Delta controlled observations rejected by Store' \
  'Resolve-held peer definition survives retirement and releases into the successor epoch' \
  'the outer and contained coordinates are deliberately distinct' \
  'logical work is the four fixed owner actions plus matching writes and purged facts' \
  'the retired whole-Herald owner product validates' \
  'the redefined whole-Herald owner product validates' \
  'exact-authority replay after redefinition is whole-Herald-identical'
do
  require_text "$regular_retirement_acceptance_property" \
    "$herald_regular_retirement_acceptance_properties" \
    "composed regular-retirement evidence is missing: $regular_retirement_acceptance_property"
done
require_text 'an authority resolved under another SystemId cannot retire local state' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement authority is not pinned to the local SystemId"
for terminal_alignment_property in \
  'alignment terminally ignores stale definition evidence but applies successor evidence' \
  'unacknowledged stale source evidence retains its continuing-change blocker' \
  'terminally ignored destination evidence is immutable history, not live stale work' \
  'the source stops blocking only after the terminal destination acknowledgement'
do
  require_text "$terminal_alignment_property" \
    "$herald_regular_retirement_acceptance_properties" \
    "alignment terminal-settlement property is missing: $terminal_alignment_property"
done
require_text 'a Store suppression cut that misses its local definition is rejected' \
  "$herald_invariant_properties" \
  "whole-state validation does not exercise an incompletely covered local definition cut"
require_text 'successor-epoch owner work is not a retired-epoch residual' \
  "$herald_invariant_properties" \
  "whole-state validation does not distinguish successor work from retired residual work"
require_text 'capture disappearance evidence before the application takes its definition' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement acceptance does not begin with application-visible evidence"
require_text 'ApplicationCall.applyApplicationRequest ingress predecessor' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement eligibility bypasses the composed application-call boundary"
require_text 'LocalTakeApplication' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement eligibility does not issue a local-take application operation"
require_text 'LocalTakeCompleted values' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement eligibility does not require a completed local take"
require_text 'VisibleApplicationCopyBlocker' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement acceptance does not prove that the visible application copy is its sole blocker"
require_text 'application-visible definition LocalTake did not complete exactly' \
  "$herald_regular_retirement_acceptance_properties" \
  "regular-retirement acceptance does not require one exact application-visible definition take"
require_text 'regular Resolve yields one exact retirement authority and replay yields none' \
  "$herald_prospective_properties" \
  "regular resolution authority/replay property is not registered"
require_text 'authority separately freezes the Open-time local publication cut' \
  "$herald_prospective_properties" \
  "regular-retirement authority does not pin its immutable Open-time publication cut"
require_text '"Eclips.Herald.UseCase.RegularRetirement" ->' \
  "$module_boundary_policy" \
  "the regular-retirement coordinator is absent from the Herald import policy"

forbid_source_pattern '^import[[:space:]]+(qualified[[:space:]]+)?Eclips\.Oracle\.Step16\.' \
  "prospective Oracle module imported by a live production source" \
  application-types/src application-client/src application-api/src \
  domain/src protocol-application/src protocol-admin/src protocol-peer/src \
  oracle-core/src oracle-runtime/src oracle-runtime/internal \
  protocol-oracle/src herald-core/src herald-core/internal \
  herald-runtime/src herald-runtime/internal herald-runtime/tcp herald-runtime/tcp-internal \
  examples


# Both subject arms and all five commands use one live canonical language.
require_count 13 '^  (=|\|) (StartProcessEpoch|EndProcessEpoch|DecideLabel$|CompleteLabelDecision|OpenHeraldFailureProbe|ReportHeraldFailureProbe|DismissHeraldFailureProbe|RetireHeraldEpoch|OpenDisappearanceProbe|ReportPredefinedAbsence|InvalidateDisappearanceProbe|ResolveDisappearanceProbe|AbortDisappearanceProbe)\b' "$oracle_command" "Oracle process/label/failure/disappearance family is not thirteen-arm"
require_count 3 '^  (=|\|) (PeerLogicalPublication|PeerLogicalRouteCutover|PeerLogicalDisappearanceProbeMarker)\b' "$peer_payload" "peer semantic item union has drifted"
require_count 3 '^  (=|\|) (PeerPublicationDto|PeerRouteCutoverDto|PeerDisappearanceProbeMarkerDto)\b' "$peer_types" "peer wire item union has drifted"
require_count 13 '^  (=|\|) (AlignmentCutAnnounced|AlignmentCutAcceptanceAdvertised|AlignmentMemberReadyAdvertised|AlignmentHistoricalCertificateAdvertised|AlignmentSubscribeRequested|AlignmentSnapshotStarted|AlignmentSnapshotChunkTransferred|AlignmentSnapshotEnded|AlignmentChangeTransferred|AlignmentLiveAdvertised|AlignmentAcknowledged|AlignmentCancelled|AlignmentProbeMarker)\b' "$alignment_control" "alignment semantic union has drifted"
require_count 11 '^  (=|\|) (AlignmentCutAnnouncedDto|AlignmentHistoricalCertificateAdvertisedDto|AlignmentSubscribeRequestedDto|AlignmentSnapshotStartedDto|AlignmentSnapshotChunkTransferredDto|AlignmentSnapshotEndedDto|AlignmentChangeTransferredDto|AlignmentLiveAdvertisedDto|AlignmentAcknowledgedDto|AlignmentCancelledDto|AlignmentProbeMarkerDto)\b' "$peer_types" "alignment wire union has drifted"
# Acceptance/readiness retain their semantic facts while logical delivery uses
# one indexed tail and sparse cumulative receipts per directed Herald pair.
require_count 2 '^  (=|\|) (AlignmentCutAcceptanceEvidenceDto|AlignmentMemberReadyEvidenceDto)\b' "$peer_types" "alignment retained-evidence wire union has drifted"
for token in PositiveAlignmentDeliverySequenceDto AlignmentRetainedEvidenceDto AlignmentEvidenceDeliveredDto AlignmentDeliveryProgressDto
do
  require_text "$token" "$peer_types" "alignment cumulative delivery DTO is absent: $token"
done
require_text 'newtype AlignmentDeliverySequence' domain/src/Eclips/Domain/Alignment.hs "alignment delivery sequence lacks a nominal domain identity"
require_text 'PeerControlEnvelope ReceiptRetirement PeerControlDto' "$peer_types" "peer controls cannot piggyback delivery receipts"
require_text 'PeerPublicationEnvelope ReceiptRetirement PeerStreamItemDto' "$peer_types" "peer publications cannot piggyback delivery receipts"
for obsolete in AlignmentCutAcceptanceAcknowledged AlignmentMemberReadyAcknowledged AlignmentCutAcceptedAck ClassMemberReadyAck
do
  forbid_text "$obsolete" "$alignment_control" "per-fact alignment acknowledgement survived: $obsolete"
  forbid_text "$obsolete" "$peer_types" "per-fact alignment acknowledgement DTO survived: $obsolete"
done
require_file herald-core/internal/Eclips/Herald/PeerDelivery/State.hs
require_file herald-core/internal/Eclips/Herald/UseCase/PeerDelivery.hs
require_text 'PeerDeliveryProperties.tests' "$herald_properties_main" "cumulative delivery properties are not registered"
require_text 'PeerDeliveryCoordinatorProperties.tests' "$herald_properties_main" "piggyback and idle-flush coordinator properties are not registered"

require_text 'startupDisappearanceState' herald-core/internal/Eclips/Herald/Startup/State.hs "live Herald product omits disappearance owner"
require_text 'DisappearanceInput DisappearanceIngress' herald-core/src/Eclips/Herald/Input.hs "authorized Abort input absent"
require_text 'applyProjectedDisappearanceTerminal' herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs "live terminal projection composition absent"
require_text 'releasePendingDisappearanceProbeMarkers' herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs "early marker projection redrive absent"
require_text 'observeTakenPublications' herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs "local-take candidate creation absent"
forbid_source_pattern 'Prospective(Publication|Alignment)Marker|ECLIPS-STEP16-PROSPECTIVE' "private staging marker survived cutover" herald-core/src herald-core/internal
forbid_source_pattern 'DisappearanceInput|AbortDisappearanceProbe' "public application/admin protocol gained disappearance management" protocol-application/src protocol-admin/src application-types/src

# The checked inventory is the one source of development-test scheduling.
sh "$inventory_check" "$inventory"
require_text 'sh scripts/check-profile-0.2-schema.sh' "$development_gate" \
  "development gate omits the current-profile audit"
require_text 'scripts/profile-0.2-test-components.txt' "$development_gate" \
  "development gate does not consume the checked test inventory"
require_text 'test $parallel_targets -j4' "$development_gate" \
  "parallel-safe tests are not one explicit -j4 union"
require_text 'test "$component" -j4' "$development_gate" \
  "timing-sensitive tests are not separate -j4 invocations"
forbid_text 'test all' "$development_gate" \
  "development gate still uses the blanket Cabal test target"
forbid_pattern '^[[:space:]]*--test-options(=|[[:space:]])' "$development_gate" \
  "development gate overrides suite-local Tasty concurrency"
forbid_text 'scripts/test-module-boundaries.sh' "$development_gate" \
  "development gate runs the expensive negative module fixture matrix"
forbid_text 'scripts/check-type-boundaries.sh' "$development_gate" \
  "development gate runs the release-only semantic/public boundary matrix"
forbid_text 'scripts/check-semantic-type-boundaries.sh' "$development_gate" \
  "development gate runs the release-only semantic/public boundary matrix"
require_text '(NumThreads 4)' herald-runtime/test-step14/Main.hs \
  "Step-14 suite-local concurrency cap changed"
require_text '(NumThreads 1)' herald-runtime/test-step15/Main.hs \
  "Step-15 suite-local serialization cap changed"


# Package-local test copies preserve independent reference ownership and sdist.
cmp -s oracle-core/test/Eclips/Oracle/Step16/DisappearanceReference.hs \
  herald-core/test-step16-prospective/Eclips/Oracle/Step16/DisappearanceReference.hs \
  || fail "Herald test reference differs from the Oracle-owned independent reference"
cmp -s oracle-core/test/OracleFixtures.hs protocol-oracle/test/OracleFixtures.hs \
  || fail "EORC test fixtures differ from Oracle owner fixtures"

# P01 replaces native cardinality assumptions without changing the retained
# owner/wire obligations above. Test labels pin independently executed evidence,
# rather than accepting the mere presence of a new gate or module.
raft_genesis=raft-core/src/Eclips/Raft/Genesis.hs
raft_transition=raft-core/src/Eclips/Raft/Internal/Prepared.hs
raft_small_cluster=raft-core/test/SmallClusterProperties.hs
founder_configuration=examples/hello-world/FounderConfiguration.hs
founder_deployment=examples/hello-world/FounderDeployment.hs
founder_properties=examples/hello-world/FounderProperties.hs
acceptance_gate=scripts/check-profile-0.2-acceptance.sh

for required in "$raft_small_cluster" "$founder_configuration" \
  "$founder_deployment" "$founder_properties" "$acceptance_gate" \
  tools/profile-0.2-proofs/eclips-profile02-proofs.cabal
do
  require_file "$required"
done
forbid_source_pattern 'RaftVoterCountTooSmall|RaftVoterCountEven' \
  "obsolete native voter cardinality rejection survived P01" raft-core
require_text '| null voters = Left RaftVotersEmpty' "$raft_genesis" \
  "Raft does not admit the checked nonempty voter shape"
require_text '2 * Set.size (Set.intersection voters acknowledgers) > Set.size voters' "$raft_genesis" \
  "the shared native quorum does not count distinct configured voters"
require_text 'hasQuorum = raftConfigurationHasQuorum . snd . effectiveConfiguration' "$raft_transition" \
  "Raft election and commit do not use the log-effective native quorum"
require_text 'if hasQuorum base (stateCandidateVotes base)' "$raft_transition" \
  "Raft omits immediate election victory after the local self-vote"
require_text 'recordLocalQuorumAcknowledgement leader' "$raft_transition" \
  "Raft omits its explicit local leader no-op acknowledgement"
require_text 'recordLocalQuorumAcknowledgement appended' "$raft_transition" \
  "Raft omits its explicit local application acknowledgement"
require_count 2 'advanceCommittedPrefix \(leaderCommitCandidate acknowledged\) acknowledged' "$raft_transition" \
  "Raft must check immediate commitment after both local no-op and application append"
require_text 'SmallClusterProperties.tests' raft-core/test/Main.hs \
  "small-cluster conformance properties are not executed"
require_text 'singleton commits generated opaque applications with ordered readiness' "$raft_small_cluster" \
  "generated singleton application/readiness conformance is missing"
require_text 'one through eight voters elect and commit at exactly their majority' "$raft_small_cluster" \
  "one shared majority is not exercised across voter cardinalities"
require_text 'isolated two- and three-voter survivors cannot elect, commit, or shrink' "$raft_small_cluster" \
  "minority unavailability and unchanged membership conformance is missing"
require_text 'all nonempty cardinalities use the same voter-only majority' raft-core/test/GenesisProperties.hs \
  "checked voter-only quorum properties are missing"
require_text 'one-, two-, and three-voter composition preserves exact normalized bindings' oracle-core/test/GenesisProperties.hs \
  "small Oracle/native genesis composition properties are missing"
require_text 'one-, two-, and three-voter failure reports use a strict majority at every prefix' oracle-core/test/Step15ReferenceProperties.hs \
  "small failure-reporter majority properties are missing"
require_text 'singleton commits ordinary Start, retry, conflict, and watch without peer RPC' oracle-runtime/test/ClusterProperties.hs \
  "singleton real-log Oracle/adapter/watch conformance is missing"
require_text '["--bootstrap"] -> withFounderDeployment runFounderApplication' examples/hello-world/Main.hs \
  "explicit founder mode does not invoke the complete application workflow"
require_text 'seed <- getEntropy 32' "$founder_deployment" \
  "founder runtime does not acquire fresh identity entropy"
require_text 'withOracleTcpCluster (founderOracleConfiguration configuration)' "$founder_deployment" \
  "founder does not run the ordinary Oracle/Raft TCP owners"
require_text 'withHeraldTcpRuntime herald' "$founder_deployment" \
  "founder does not run the ordinary Herald TCP owner"
require_text 'withFounderDeployment = withFounderDeploymentAtControl (controlIndex 3)' "$founder_deployment" \
  "founder smoke does not witness label decision, collection and launcher End"
require_text 'expect "founder launcher End" =<< App.endProcess application' "$hello_application" \
  "founder launcher does not await explicit process End"
require_text 'awaitOracleControlIndex terminal observation cluster' "$founder_deployment" \
  "founder smoke does not await its selected complete control barrier"
require_text 'propFounderConfiguration' "$founder_properties" \
  "founder checked-configuration properties are missing"
require_text 'FounderProperties' examples/hello-world/TestMain.hs \
  "founder properties are not executed by integration tests"
require_text 'MEM-GEN-019' tools/module-boundaries/src/Eclips/ModuleBoundaries/MembershipLedger.hs \
  "founder genesis membership assignments are absent from the compiled ledger"
# The example derives only index-zero native-health authority from its checked
# genesis. Live Oracle transitions still belong exclusively to the native adapter.
forbid_source_pattern 'stepOracle|applyCommittedOracleConfiguration' \
  "example founder bypasses the ordinary Raft/Oracle adapter" examples/hello-world
require_text 'import Eclips.Oracle.Transition (initialOracle)' examples/hello-world/HelloDeployment.hs \
  "founder initial authority imports more than the checked initializer"
require_count 2 'initialOracle' examples/hello-world/HelloDeployment.hs \
  "founder must use Oracle initialization only for index-zero voter metadata"
require_text 'initial = checked "hello initial Oracle authority" (initialOracle oracleGenesis)' examples/hello-world/HelloDeployment.hs \
  "founder initial authority does not derive from its supplied checked genesis"
require_text 'configureHeraldRuntimeOracleVoters (Voter.oracleVoterConfiguration initial) (Voter.oracleReplicaRegistrations initial)' examples/hello-world/HelloDeployment.hs \
  "founder omits checked initial native-health authority"
for current_gate in "$development_gate" "$acceptance_gate"
do
  require_text 'tools/profile-0.2-proofs' "$current_gate" \
    "current gate omits the independent proof-contract project"
  require_text 'test eclips-profile02-proofs:test:proof-contracts -j4' "$current_gate" \
    "current gate does not execute the independent proof-contract component"
  require_text '-- --bootstrap' "$current_gate" \
    "current gate omits the explicit founder executable smoke"
  forbid_pattern 'sh scripts/check-step-(12|14|15|16)-(acceptance|migration)[.]sh' "$current_gate" \
    "current gate executes an obsolete historical schema/acceptance entrypoint"
done

# P02 cuts over the live Start and startup-access shapes together. Immutable
# genesis templates remain only an initialization input; no protocol/version
# fallback or hidden configured-root publication path may remain.
process_start=domain/src/Eclips/Domain/ProcessStart.hs
primordial=herald-core/internal/Eclips/Herald/Application/Primordial.hs
dynamic_properties=herald-core/test/ConfiguredProcessProperties.hs
grant_properties=herald-core/test/PrimordialGrantProperties.hs
require_file "$process_start"
require_file "$primordial"
require_text 'ProcessStart,' "$process_start" "minimal process Start is not opaque"
for start_field in processStartProcessId processStartProcessEpochId processStartResidence
do
  require_text "$start_field" "$process_start" "minimal Start field is missing: $start_field"
done
forbid_source_pattern '\b(ConfiguredProcessCatalogueDigest|configuredProcessCatalogue|ConfiguredRootStructuralSourceStage|ConfiguredStartView|StartConfiguredProcess)\b' \
  "obsolete future catalogue, manifested Start, or hidden root publication survived P02" \
  domain/src oracle-core/src protocol-oracle/src protocol-admin/src herald-core/src herald-core/internal herald-runtime/src herald-runtime/internal
require_text 'DynamicStartView' oracle-core/src/Eclips/Oracle/Internal/Label.hs "Start projection lacks dynamic provenance"
require_text 'DynamicProcessProperties.tests' oracle-core/test/Main.hs "generated dynamic Oracle history is not executed"
require_text 'unused process templates cannot change immutable configuration' herald-core/test/GenesisProperties.hs "immutable configuration split has no property"
require_text 'fresh local correlations generate distinct identities and exact retries consume none' "$dynamic_properties" "dynamic ID allocation/retry law is missing"
require_text 'independent active Herald generators admit starts without future manifests' "$dynamic_properties" "different-residence dynamic Start evidence is missing"
require_text 'End after applied Start but before attachment prevents later readiness' "$dynamic_properties" "Start/End readiness ordering evidence is missing"
for opaque in PrimordialSelection PrimordialAccess
do
  require_text "    $opaque," "$application_access" "selected startup representation is not opaque: $opaque"
  forbid_text "$opaque (..)" "$application_access" "selected startup representation is public: $opaque"
done
require_text 'startupAccessPrimordial' "$application_access" "startup lacks exact selected access"
forbid_text 'startupAccessPredefined' "$application_access" "startup still requires six root pairs"
require_text 'EnvironmentSourcesUnavailable' "$new_environment" "newenv lacks missing-source rejection"
require_text 'PrimordialGrantProperties.tests' "$herald_properties_main" "checked grant properties are not executed"
require_text 'removing either retained source rejects without borrowing another writer' "$grant_properties" "source loss operation evidence is missing"
require_text 'a child process name needs a normal grant, never only a private alias' "$grant_properties" "child-name possession evidence is missing"
require_text 'PRIMORDIAL_SELECTION_ACCESS_COERCION' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "compiled selected-access opacity probes are absent"

# P03 is an executable local lifecycle cutover, with retained own-End recovery.
lifecycle_types=application-types/src/Eclips/Application/Types/Lifecycle.hs
preparation_owner=herald-core/internal/Eclips/Herald/ProcessPreparation/State.hs
preparation_use_case=herald-core/internal/Eclips/Herald/UseCase/ProcessPreparation.hs
preparation_readiness=herald-core/internal/Eclips/Herald/ProcessPreparation/Readiness.hs
preparation_properties=herald-core/test/ProcessPreparationProperties.hs
initial_claim_properties=herald-core/test/ApplicationInitialClaimProperties.hs
child_tour=examples/hello-world/PreparedChildProperties.hs
for current in "$lifecycle_types" "$preparation_owner" "$preparation_use_case" \
  "$preparation_readiness" "$preparation_properties" "$initial_claim_properties" "$child_tour"
do
  require_file "$current"
done
for opaque in ConnectionDescriptor ChildPreparation PreparedChild InitialClaimId LifecycleRequestId
do
  require_text "    $opaque," "$lifecycle_types" "lifecycle representation is not opaque: $opaque"
  forbid_text "$opaque (..)" "$lifecycle_types" "lifecycle constructor is public: $opaque"
done
for command in BeginChild AwaitPreparedChild AwaitChildReady CancelChild EndOwnProcess
do
  require_text "$command" "$lifecycle_types" "current lifecycle command is missing: $command"
done
require_text 'applicationInitialClaimEntries' herald-core/internal/Eclips/Herald/Startup/Invariant.hs "startup does not check retained claim ownership"
require_text 'requiredDeltaReady' "$preparation_readiness" "required Delta readiness lacks a local-owner witness"
require_text 'Alignment.currentAlignmentMemberReady local (alignmentGenerationId generation) incarnation alignment' \
  "$preparation_readiness" "required Delta can bypass current-plan local alignment evidence"
for current_member_readiness_contract in \
  'Map.member (identifier, store) state.localMemberReadiness' \
  'not (alignmentPlanInvalidated (Plan.alignmentPlanId plan) state)' \
  'identifier `elem` map alignmentGenerationId (planGenerations plan)' \
  'Map.member (Plan.alignmentPlanId plan, local) state.planAcceptances'
do
  require_text "$current_member_readiness_contract" "$herald_alignment_owner" \
    "current-plan member readiness omits an owner-local condition: $current_member_readiness_contract"
done
require_text 'ProcessPreparationProperties.tests' "$herald_properties_main" "preparation properties are not executed"
require_text 'PreparationWorkIndexProperties.tests' "$herald_properties_main" "selective work-index laws are not executed"
require_text 'ProcessPreparationWorkProperties.tests' "$herald_properties_main" "preparation work lifetime laws are not executed"
require_text 'preparation readiness work stays parked until a relevant gate change' "$preparation_properties" "preparation has no dormant-work regression"
require_text 'successful genesis claims retire their pending work before the next drive' "$preparation_properties" "successful genesis claims retain pending work"
require_text 'ApplicationInitialClaimProperties.tests' "$herald_properties_main" "initial claim properties are not executed"
require_text 'AdministrationCancellationProperties.tests' "$herald_properties_main" "administration cancellation properties are not executed"
require_text 'preparation completes before required writer ownership and installs exact grants' "$preparation_properties" "preparation waits for its dependent labels"
require_text 'self-fence cannot acknowledge cancellation while submitted Start is unresolved' "$preparation_properties" "cancellation has no unresolved-Start fencing evidence"
require_text 'required reader needs current owner-local alignment completion' "$preparation_properties" "reader readiness has no local-versus-advertised evidence"
require_text 'casePartialTransfer' "$preparation_properties" "partial label cancellation has no regression"
require_text 'repeated lost first replies keep the original recovery deadline' "$initial_claim_properties" "initial claim retry lacks fixed-grace evidence"
require_text 'CancelChildPreparation' protocol-admin/src/Eclips/Protocol/Admin/Types.hs "EADM has no explicit preparation cancellation"
require_text 'RecoverLifecycleResult' protocol-application/src/Eclips/Protocol/Application/Types.hs "EAPP has no restricted End recovery query"
require_text 'own-End lost terminal and later restricted recovery use no session reopen' "$application_runtime_properties" "own-End has no real TCP recovery evidence"
require_text 'runPreparedChildTour' examples/hello-world/TestMain.hs "OS child tour is not executed"
require_text 'readCreateProcessWithExitCode (proc executable' "$child_tour" "child tour does not launch an independent OS process"
require_text 'App.label parent' "$child_tour" "child tour bypasses ordinary parent labels"
require_text 'App.recoverEndProcess configuration request' "$child_tour" "OS child tour omits the restricted End query"
require_text 'App.ApplicationLifecycleUnknown App.LifecycleResultNotRetained' "$child_tour" "OS child tour omits released End receipt classification"
require_text 'expectLifecycle ProcessEnded call' "$child_tour" "OS child tour omits caller-owned End completion"
require_text 'LIFECYCLE_CORRELATION_COERCION' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "compiled lifecycle opacity probes are absent"

# P04 keeps the direct application surface distinct from owner/transport tools,
# admits retained remote grants through ordinary local facts, and ships production
# deployment and independent application executable components.
application_direct=application-api/src/Eclips/Application.hs
application_advanced=application-api/src/Eclips/Application/Advanced.hs
startup_transfer=herald-core/internal/Eclips/Herald/Application/Primordial/Transfer.hs
remote_properties=herald-core/test/RemoteProcessPreparationProperties.hs
deployment_package=deployment/eclips-deployment.cabal
for current in "$application_direct" "$application_advanced" "$startup_transfer" "$remote_properties" "$deployment_package"
do
  require_file "$current"
done
require_text 'Herald,' "$application_direct" "ordinary facade has no opaque Herald"
require_text 'Operation (..)' "$application_advanced" "advanced calls lack a result witness"
require_text 'encodeConnectionDescriptor' application-api/src/Eclips/Application/Connection.hs "descriptor text encoding is absent"
require_text 'decodeConnectionDescriptor' application-api/src/Eclips/Application/Connection.hs "descriptor text admission is absent"
require_text 'captureStartupTransfer' "$startup_transfer" "source does not retain exact accepted transfer material"
require_text 'prepareSortInduction' "$startup_transfer" "transfer skips ordinary Registry admission"
require_text 'prepareControlledStartupObservation' "$startup_transfer" "transfer skips checked controlled publication admission"
require_text 'prepareControlledStartupObservation = prepareControlledObservation True' herald-core/internal/Eclips/Herald/Controlled/State.hs "startup observation bypasses the shared controlled admission owner"
require_text 'prepareControlledPeerObservation = prepareControlledObservation False' herald-core/internal/Eclips/Herald/Controlled/State.hs "peer observation bypasses the shared controlled admission owner"
require_text 'oracleProjectionProcessResidenceAt' "$startup_transfer" "transfer does not check historical parent residence"
require_text 'RemoteProcessPreparationProperties.tests' "$herald_properties_main" "remote handoff properties are not executed"
require_text 'caseParentEnd' "$remote_properties" "remote parent End independence is not exercised"
require_text 'caseReconnect' "$remote_properties" "remote retained handoff replay is not exercised"
require_text 'casePreparedReconnect' "$remote_properties" "remote Prepared reply and known-child reconnect recovery are not exercised"
require_text 'caseTransferObservation' herald-core/test/PrimordialGrantProperties.hs "controlled transfer evidence has no exercised admission"
require_text 'configureHeraldTcpAdvertisedEndpoints' herald-runtime/tcp/Eclips/Herald/Runtime/TCP.hs "Herald cannot advertise configured endpoints"
require_text 'configureOracleTcpListeners' oracle-runtime/src/Eclips/Oracle/Runtime/TCP.hs "Oracle listeners have no explicit deployment configuration"
for executable in eclips-herald eclips-admin
do
  require_text "executable $executable" "$deployment_package" "production deployment executable is missing: $executable"
done
for operation in GetHeraldStatus ListChildPreparations DrainHerald HeraldDrained
do
  require_text "$operation" protocol-admin/src/Eclips/Protocol/Admin/Types.hs "EADM operator operation is missing: $operation"
done
for executable in eclips-hello-launcher eclips-hello-publisher eclips-hello-reader
do
  require_text "executable $executable" examples/hello-world/eclips-hello-world.cabal "independent application executable is missing: $executable"
done
forbid_source_pattern 'awaitApplicationCall|ApplicationCallCompletion|RegularCallResult|Eclips\.Application\.Runtime|Eclips\.Domain|Eclips\.Herald|Eclips\.Oracle' \
  "ordinary example bypasses the typed facade" examples/hello-world/applications examples/hello-world/launcher examples/hello-world/publisher examples/hello-world/reader
require_text 'deployment-os-integration' "$deployment_package" "split deployment OS component is absent"
require_text 'withReplyLossProxy' deployment/test-os/Main.hs "split applications have no terminal child lane-loss schedule"
require_text 'reader first' deployment/test-os/Main.hs "split applications omit reversed startup order"
require_text 'different Heralds' deployment/test-os/Main.hs "split applications omit remote residence"
p04_failure_tours=scripts/check-profile-0.2-failure-tours.sh
require_file "$p04_failure_tours"
require_text 'component=eclips-deployment:test:deployment-os-integration' "$p04_failure_tours" "P04 fault repetitions select the wrong OS component"
for token in p04-child-reply-loss p04-failed-spawn
do
  require_count 1 "$token" deployment/test-os/Main.hs "P04 fault schedule must have one unique test name"
  require_text "$token" "$p04_failure_tours" "P04 fault schedule has no fresh-process repetition"
done
require_text 'for repetition in 1 2 3' "$p04_failure_tours" "P04 fault schedules are not repeated three times"
require_text 'sh scripts/check-profile-0.2-failure-tours.sh' "$acceptance_gate" "P04 fault repetitions are absent from aggregate acceptance"
require_text 'Advanced.BeginChild' examples/hello-world/applications/HelloLauncher.hs "launcher does not prepare ordinary children"
require_text 'Advanced.AwaitPreparedChild' examples/hello-world/applications/HelloLauncher.hs "launcher does not await retained child preparation"
require_text 'withPreparationReferences' examples/hello-world/applications/HelloLauncher.hs "launcher does not retain accepted preparations for scoped cleanup"
require_text 'proc executable' examples/hello-world/applications/HelloLauncher.hs "launcher does not spawn separate OS children"
require_text 'TYPED_RESULT_COERCION' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "typed Call result cannot be checked for coercion safety"
# P05 retires repeatedly through one checked history and one evolving closure.
require_text 'HeraldMembershipHistory' "$project_root/domain/src/Eclips/Domain/Membership.hs" "checked membership history is missing"
require_text 'HeraldMembershipLineage' "$project_root/domain/src/Eclips/Domain/Membership.hs" "checked membership lineage is missing"
require_text 'MembershipBaseClosure' "$project_root/herald-core/internal/Eclips/Herald/Graph/TerminalSource.hs" "compound terminal closure is missing"
require_text 'structuralDescendantSettlement' "$project_root/herald-core/internal/Eclips/Herald/Graph/Progress.hs" "installed descendant settlement evidence is missing"
require_text 'repairTerminalInventoryAnchor' "$project_root/herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs" "established anchor repair is missing"
require_text 'RepeatedRetirementProperties.tests' "$project_root/herald-runtime/test-step15/Main.hs" "P05 TCP schedules are not registered"
require_text 'installedLineageAdmitsApplicationStage' "$project_root/herald-core/internal/Eclips/Herald/UseCase/StructuralProgress.hs" "application staging lacks installed history evidence"
require_text 'sh scripts/check-profile-0.2-retirement-tours.sh' "$acceptance_gate" "P05 repeated retirement tours are absent from aggregate acceptance"
require_text 'for repetition in 1 2 3' "$project_root/scripts/check-profile-0.2-retirement-tours.sh" "P05 schedules are not repeated in three fresh processes"
forbid_text 'RetirementAlreadyCommitted' "$project_root/oracle-core/src/Eclips/Oracle/Internal/Failure.hs" "single-retirement guard remains"
# P06 admits exactly one restricted observer through checked Oracle history.
require_text 'admitHeraldMembershipGeneration' domain/src/Eclips/Domain/Membership.hs "checked admission generation is missing"
forbid_text 'HeraldAdmissionId (..)' domain/src/Eclips/Domain/Membership.hs "admission identity constructor escaped"
require_text 'HeraldJoinBaseRecipe,' domain/src/Eclips/Domain/Topology.hs "opaque join base recipe is missing"
require_text 'activateHeraldJoinBase' domain/src/Eclips/Domain/Topology.hs "activation does not bind its actual control index"
require_text 'AdmissionTopologyPredecessorView' domain/src/Eclips/Domain/Topology.hs "join predecessor has no distinct insertion provenance"
require_file oracle-core/src/Eclips/Oracle/Admission.hs
require_file oracle-core/src/Eclips/Oracle/Internal/Admission.hs
require_text 'admissionRecordSeal' oracle-core/src/Eclips/Oracle/Admission.hs "admission certificate lacks its sealed cut"
require_text 'initialJoiningStructuralProgressState' herald-core/internal/Eclips/Herald/Graph/Progress.hs "joining observer initialization is missing"
require_text 'prepareHeraldJoinBaseCandidate' herald-core/internal/Eclips/Herald/Graph/Progress.hs "readiness cannot check the candidate before activation"
require_text 'prepareMembershipAdmissionBaseInstallation' herald-core/internal/Eclips/Herald/Graph/Progress.hs "checked admission base installation is missing"
require_text 'structuralProjectionSnapshotAdmissionVertices' herald-core/internal/Eclips/Herald/Structural/Reconciliation.hs "historical projection loses admission provenance"
require_text 'ApplicationSessionGatePending' herald-core/internal/Eclips/Herald/Application/State.hs "attachment grants bypass membership admission"
require_text 'deferredBegins' herald-core/internal/Eclips/Herald/ProcessPreparation/State.hs "BeginChild cannot retain its call before allocating identity"
require_text 'ConfiguredStartGated' herald-core/internal/Eclips/Herald/Administration/State.hs "administrative Start bypasses membership admission"
require_file herald-core/internal/Eclips/Herald/Join/History.hs
require_file herald-core/internal/Eclips/Herald/Join/Replay.hs
require_file herald-core/internal/Eclips/Herald/Alignment/History.hs
require_file herald-core/internal/Eclips/Herald/UseCase/AlignmentHistory.hs
require_text 'joinHistoryPlacementSnapshots' herald-core/internal/Eclips/Herald/Join/History.hs "join history omits exact historical placement"
require_text 'joinHistoryAlignmentHistory' herald-core/internal/Eclips/Herald/Join/History.hs "join history omits historical alignment ancestry"
require_text 'adoptJoinAlignmentFrontier' herald-core/internal/Eclips/Herald/UseCase/JoinHistory.hs "joining activation omits the sealed old-anchor frontier handoff"
require_text 'alignmentPromotionHandoffPending' herald-core/internal/Eclips/Herald/UseCase/Alignment.hs "captured alignment frontier can change before activation"
require_text 'retainHistoricalAlignmentPlan' herald-core/internal/Eclips/Herald/UseCase/AlignmentHistory.hs "newcomer history bypasses passive checked alignment admission"
require_file deployment/src/Eclips/Deployment/Joining.hs
require_text 'GraphAdmissionProperties.tests' herald-core/test/Main.hs "admission graph properties are not registered"
require_text 'JoinHistoryProperties.tests' herald-core/test/Main.hs "join transfer properties are not registered"
require_text 'JoinAlignmentHistoryProperties.tests' herald-core/test/Main.hs "join-after-application alignment regression is not registered"
require_text 'JoinGateProperties.tests' herald-core/test/Main.hs "join admission gates are not exercised"
require_text 'HERALD_JOIN_RECIPE_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "join recipe opacity lacks a compiled probe"
p06_join_tours=scripts/check-profile-0.2-join-tours.sh
require_file "$p06_join_tours"
require_text 'component=eclips-deployment:test:deployment-os-integration' "$p06_join_tours" "P06 join repetitions select the wrong OS component"
for token in p06-seed-chain p06-retirement-retry
do
  require_count 1 "$token" deployment/test-os/Main.hs "P06 join schedule must have one unique test name"
done
require_text 'for repetition in 1 2 3' "$p06_join_tours" "P06 join schedules are not repeated three times"
require_text 'sh scripts/check-profile-0.2-join-tours.sh' "$acceptance_gate" "P06 join repetitions are absent from aggregate acceptance"
# P07 replaces genesis-only native authority; P08 below connects the retained
# Oracle registration and configuration semantics. Typed boundaries and live/reference/ERFT
# schedules must accompany the exact native owner and adapter cutover.
require_file raft-core/src/Eclips/Raft/Configuration.hs
require_text 'Configuration RaftVotingConfiguration bytes' raft-core/src/Eclips/Raft/Input.hs "native log lacks opaque configuration entries"
require_text 'checkRaftLearnerGenesis' raft-core/src/Eclips/Raft/Genesis.hs "native learner initialization is absent"
require_text 'ExposeCommittedEntries' raft-core/src/Eclips/Raft/Effect.hs "native committed stream omits non-application positions"
require_text 'AcknowledgeCommittedEntriesView' raft-core/src/Eclips/Raft/Input.hs "native applied frontier is not explicit"
forbid_source_pattern 'ExposeCommittedApplications|acknowledgeCommittedApplications|raftStateApplicationAppliedThrough' \
  "application-only native progress survived P07" raft-core oracle-runtime
require_text 'stateQuorumDispatchFloors' "$raft_transition" "recent leader guard lacks fresh acknowledgement generations"
require_text 'startRecentLeaderObservationWindow' "$raft_transition" "partial quorum evidence has no bounded observation window"
require_text 'fireRecentLeaderTimer' oracle-runtime/src/Eclips/Oracle/Runtime.hs "recent leader guard has no runtime timer boundary"
require_text 'LeaderNoOp -> do' oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs "Oracle adapter does not advance no-op positions"
require_text 'ReconfigurationModelProperties.tests' raft-core/test/Main.hs "independent native configuration model is not executed"
require_text 'LogWorkProperties.tests' raft-core/test/Main.hs "admitted-log work regressions are not executed"
require_text 'heartbeat allocation does not grow with retained application history' raft-core/test/LogWorkProperties.hs "idle Raft history-scaling regression is absent"
require_text 'partly learned old, joint and final groups cannot elect divergent histories' raft-core/test/ReconfigurationModelProperties.hs "partial configuration safety model is absent"
require_text 'partial quorum evidence expires while leader protection is inactive' raft-core/test/ReconfigurationModelProperties.hs "unprotected quorum freshness regression is absent"
require_text 'a reservation retries after leadership loss, intervening commit and regain' oracle-runtime/test/CoordinationProperties.hs "cross-term submission reservation regression is absent"
require_text 'NativeTcpProperties.tests' protocol-raft/test/Main.hs "real ERFT configuration schedules are not executed"
require_text 'RaftAdmissionProperties.tests' oracle-runtime/test/Main.hs "actual ERFT source binding checks are not executed"
require_text 'RAFT_READINESS_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "learner readiness lacks compiled opacity coverage"
require_text 'RAFT_COMMITTED_ENTRY_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "native committed evidence lacks compiled opacity coverage"
p07_raft_tours=scripts/check-profile-0.2-raft-tours.sh
require_file "$p07_raft_tours"
require_text 'component=eclips-protocol-raft:test:protocol-raft-properties' "$p07_raft_tours" "P07 repetitions select the wrong ERFT component"
require_text 'for repetition in 1 2 3' "$p07_raft_tours" "P07 ERFT schedules are not repeated three times"
require_text 'sh scripts/check-profile-0.2-raft-tours.sh' "$acceptance_gate" "P07 ERFT repetitions are absent from aggregate acceptance"
# P08 connects retained administration to the sealed native stream and ordinary
# cohost supervision, with immutable client receipts and independent vote authority.
require_file docs/verification/owner-contracts.md
require_file oracle-core/src/Eclips/Oracle/Voter.hs
require_file oracle-core/src/Eclips/Oracle/Internal/Voter.hs
for opaque in OracleReplicaRegistration OracleVoterBindings VoterConfiguration VoterChange
do
  forbid_text "$opaque (..)" oracle-core/src/Eclips/Oracle/Voter.hs "P08 checked value exposes its constructor: $opaque"
done
for command in registerOracleReplicaCommand beginVoterChangeCommand cancelVoterChangeCommand
do
  require_text "$command" oracle-core/src/Eclips/Oracle/Command.hs "P08 retained command is absent: $command"
done
require_text 'applyCommittedOracleConfiguration' oracle-core/src/Eclips/Oracle/Transition.hs "sealed configuration application is absent"
require_text 'appliedEntryCommand' oracle-core/src/Eclips/Oracle/Projection.hs "watch origin cannot be narrowed to a client command"
require_text 'ApplyOracleConfigurationOwner' oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs "configuration entries bypass their Oracle owner"
require_text 'projectedConfigurationProposal' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs "native proposal metadata is not produced by the Oracle owner"
require_text 'beginRaftConfigurationChange' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs "retained intent does not capture native readiness"
require_text 'CoreInput.AppendAccepted matchIndex appliedIndex' protocol-raft/src/Eclips/Protocol/Raft/Types.hs "ERFT success lacks acknowledged applied progress"
require_text 'VoterProperties.tests' oracle-core/test/Main.hs "P08 Oracle properties are not registered"
require_text 'VoterProperties.tests' protocol-oracle/test/Main.hs "P08 EORC properties are not registered"
require_text 'VoterRuntimeProperties.tests' oracle-runtime/test/Main.hs "P08 real TCP schedules are not registered"
require_text 'OracleVoterProjectionProperties.tests' herald-core/test/Main.hs "P08 Herald role properties are not registered"
require_text 'OracleReplicaWaitProperties.tests' herald-runtime/test-step12/Main.hs "registration notification lacks runtime tests"
require_text 'awaitHeraldOracleReplicaRegistration' deployment/src/Eclips/Deployment/Runtime.hs "replica startup does not await owner registration"
require_text 'withDeploymentResidentOracle' deployment/src/Eclips/Deployment/Runtime.hs "cohost role lifetime lacks a public configuration boundary"
for operation in PrepareOracleReplica BeginVoterChange CancelVoterChange GetOracleConfiguration GetVoterChangeStatus
do
  require_text "$operation" protocol-admin/src/Eclips/Protocol/Admin/Types.hs "P08 EADM operation is absent: $operation"
done
require_text 'ORACLE_VOTER_CONFIGURATION_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "P08 configuration opacity lacks a compiled probe"
require_text 'ORACLE_CONFIGURATION_IS_CLIENT_COMMAND' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "configuration watch entries can be mistaken for client commands"
p08_voter_tours=scripts/check-profile-0.2-voter-tours.sh
require_file "$p08_voter_tours"
require_text 'for repetition in 1 2 3' "$p08_voter_tours" "P08 schedules are not repeated three times"
require_text 'p08-voter- 8' "$p08_voter_tours" "P08 TCP corpus is not counted exactly"
require_text 'p08- 9' "$p08_voter_tours" "P08 OS corpus is not counted exactly"
require_text 'sh scripts/check-profile-0.2-voter-tours.sh' "$acceptance_gate" "P08 voter tours are absent from aggregate acceptance"
# P09 retains the original failure certificate across normal native exclusion,
# then applies semantic retirement. Health is a fresh native-owner observation,
# independent of EPRP connectivity and committed control-watch authority.
require_file docs/verification/owner-contracts.md
require_file oracle-core/src/Eclips/Oracle/Failure.hs
forbid_text 'AcceptedVoterHostFailure (..)' oracle-core/src/Eclips/Oracle/Failure.hs "accepted failure certificate exposes construction"
require_text 'acceptVoterHostFailureCommand' oracle-core/src/Eclips/Oracle/Command.hs "automatic failure acceptance is absent"
require_text 'VoterExcludedAwaitingHeraldRetirement' oracle-core/src/Eclips/Oracle/Internal/Voter.hs "native exclusion silently completes semantic retirement"
require_text 'oracleReplicaReplicationTargets' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs "retired hosts remain native replication targets"
require_text 'managerRetiredPeers' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs "retired replica transport work is not terminalized"
require_text 'OracleVoterConfigurationClaim' protocol-peer/src/Eclips/Protocol/Peer/Types.hs "direct failure probes omit captured voter identity"
require_text 'OracleHealthQuery' protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs "current EORC health query is absent"
require_text 'OracleHealthLaneComplete' protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs "health can accidentally establish a command lane"
require_text 'queryOracleRuntimeHealth' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs "health replies bypass the live native owner"
require_text 'QueryRaftHealth' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs "native health has no serialized owner request"
require_text 'HealthRuntimeProperties.tests' oracle-runtime/test/Main.hs "native health TCP/stop evidence is not registered"
require_text 'FailureRuntimeProperties.tests' oracle-runtime/test/Main.hs "automatic failure TCP schedules are not registered"
require_text 'ORACLE_ACCEPTED_FAILURE_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "accepted failure evidence lacks compiled opacity coverage"
require_text 'ORACLE_RUNTIME_HEALTH_CONSTRUCTOR' tools/type-boundaries/src/Eclips/TypeBoundaries/Spec.hs "native owner health lacks compiled opacity coverage"
require_text 'healthSourceAuthorized status hello' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs 'health cannot renew a retired epoch'
require_text 'RunOracleHealthRound' herald-core/src/Eclips/Herald/EffectBatch.hs 'health round is not owner-issued'
require_text 'OracleHealthRoundObserved' herald-core/src/Eclips/Herald/Input.hs 'health result has no typed ingress'
forbid_text 'HeraldRuntimeIsolated' herald-runtime/src/Eclips/Herald/Runtime.hs 'semantic fencing still exits physical runtime scope'
require_text 'OracleHealthProperties' herald-core/test/Main.hs 'fresh health quorum properties are absent'
require_text 'OracleHealthTcpProperties' herald-runtime/test-step12/Main.hs 'fresh health TCP tests are absent'
require_text 'mayCancelVoterPreparation' oracle-runtime/src/Eclips/Oracle/Runtime/Internal/RaftOwner.hs 'automatic failure preparation is cancellable through native gate'
require_text 'RaftRetryProperties' oracle-runtime/test/Main.hs 'refused-handshake retry regression is absent'

require_file scripts/check-profile-0.2-completion-tours.sh
require_text 'p09-failure- 9' scripts/check-profile-0.2-completion-tours.sh 'P09 automatic native schedules are not counted exactly'
require_text 'p09-health- 2' scripts/check-profile-0.2-completion-tours.sh 'P09 health schedules are not counted exactly'
require_text 'p09- 2' scripts/check-profile-0.2-completion-tours.sh 'P09 partition schedules are not counted exactly'
require_text 'p10- 1' scripts/check-profile-0.2-completion-tours.sh 'P10 combined tour is not counted exactly'
require_text 'for repetition in 1 2 3' scripts/check-profile-0.2-completion-tours.sh 'P09/P10 schedules are not repeated three times'
require_text 'sh scripts/check-profile-0.2-completion-tours.sh' "$acceptance_gate" 'P09/P10 tours are absent from aggregate acceptance'

# The memory envelope is part of the current release workflow. Diagnostics
# remain observable while ordinary replicas discard lifetime conformance traces.
require_file scripts/run-memory-guard.py
require_text 'scripts/run-memory-guard.py' "$acceptance_gate" 'macOS aggregate acceptance has no memory watchdog'
require_text '--heap 768m --process-mib 900 --compiler-mib 2816' "$acceptance_gate" 'aggregate process memory budgets changed without review'
require_text 'ECLIPS_ACCEPTANCE_TREE_MIB:-4096' "$acceptance_gate" 'aggregate tree memory default changed without review'
require_text 'exec "$ECLIPS_ACCEPTANCE_GHC" "$@" +RTS -M2304m -RTS' "$acceptance_gate" 'compiler heap is not independently bounded'
require_text 'exec "$ECLIPS_ACCEPTANCE_RUN_GHC" --ghc-arg=+RTS --ghc-arg=-M2304m --ghc-arg=-RTS "$@"' "$acceptance_gate" 'runghc compiler heap is not independently bounded'
require_text 'exec "$ECLIPS_ACCEPTANCE_HADDOCK" "$@" +RTS -M2304m -RTS' "$acceptance_gate" 'Haddock compiler heap is not independently bounded'
require_text 'exec "$ECLIPS_ACCEPTANCE_HSC2HS" "$@" +RTS -M2304m -RTS' "$acceptance_gate" 'hsc2hs compiler heap is not independently bounded'
forbid_text 'test $parallel_targets -j4' "$acceptance_gate" 'aggregate property workloads still overlap at top level'
require_text 'configuredRecordingMode = OracleDiagnosticsOnly' oracle-runtime/src/Eclips/Oracle/Runtime.hs 'ordinary Oracle runtimes retain lifetime recording'
require_text 'caseDiagnosticsOnly' oracle-runtime/test/RecordingProperties.hs 'diagnostics-only recording has no regression coverage'
require_text 'caseDisableCapture' oracle-runtime/test/RecordingProperties.hs 'recording cannot be explicitly disabled in regression coverage'


# Consumer-qualified application receipt retirement keeps allocation/provenance
# facts independently of terminal result history.
require_text 'Eclips.Application.Types.Lifetime' application-types/eclips-application-types.cabal "application retirement vocabulary is not exported"
require_text 'ApplicationReceiptRetirementInput' herald-core/src/Eclips/Herald/Input.hs "application retirement lacks typed kernel ingress"
require_text 'applyApplicationReceiptRetirement' herald-core/src/Eclips/Herald/Transition.hs "application retirement is not connected to the live transition"
require_text 'prepareApplicationBindingTermination' herald-core/internal/Eclips/Herald/UseCase/ApplicationLiveness.hs "established application loss is not terminal"
require_text 'ApplicationRetirementProperties.tests' herald-core/test/Main.hs "composed application retirement properties are not registered"
require_text 'ProcessPreparationReceiptRetirementProperties.tests' herald-core/test/Main.hs "lifecycle receipt retirement properties are not registered"
require_text 'ReceiptRetirementProperties.tests' application-client/test/Main.hs "client receipt retirement properties are not registered"

echo "Profile-0.2 schema audit passed (P09/P10 cutover and bounded certification; retained P01-P08 and Step-16 obligations)"
