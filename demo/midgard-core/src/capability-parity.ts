import { canonicalJson } from "./canonical-json.js";
import { computeHash32 } from "./codec/hash.js";
import {
  MIDGARD_CONSENSUS_FEATURES,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "./consensus-profile.js";

export const MIDGARD_CAPABILITY_PARITY_REPORT_VERSION = 1 as const;

export const MIDGARD_CAPABILITY_DIMENSIONS = Object.freeze([
  "total_transaction_bytes",
  "spend_inputs_field_bytes",
  "reference_inputs_field_bytes",
  "outputs_field_bytes",
  "required_observers_field_bytes",
  "required_signers_field_bytes",
  "mint_field_bytes",
  "address_witnesses_field_bytes",
  "script_witnesses_field_bytes",
  "redeemers_field_bytes",
  "serialized_output_bytes",
  "serialized_value_bytes",
  "spend_input_count",
  "reference_input_count",
  "output_count",
  "address_witness_count",
  "required_signer_count",
  "required_observer_count",
  "script_execution_count",
  "distinct_asset_count",
  "native_script_depth",
  "native_script_node_count",
  "transaction_execution_memory_units",
  "transaction_execution_cpu_units",
  "reference_script_material_bytes",
  "feature_mint_burn",
  "feature_reference_inputs",
  "feature_native_cardano_scripts",
  "feature_plutus_v3_scripts",
  "feature_midgard_v1_scripts",
  "feature_script_witnesses",
  "feature_redeemers",
  "feature_reference_scripts",
  "feature_script_payment_credentials",
  "feature_protected_outputs",
  "feature_required_observers",
  "feature_valid_forced_transactions",
  "feature_invalid_forced_transactions",
] as const);

export type MidgardCapabilityDimension =
  (typeof MIDGARD_CAPABILITY_DIMENSIONS)[number];

export type CardanoCapabilitySnapshot = Readonly<{
  version: 1;
  network: string;
  effectiveEpoch: number;
  observedAt: string;
  source: Readonly<{
    kind: "trusted_cardano_node" | "diagnostic_fixture";
    identity: string;
    tip: string;
  }>;
  pendingProtocolParameterChanges: Readonly<{
    queried: boolean;
    morePermissiveChangePending: boolean | null;
  }>;
  parameters: Readonly<{
    protocolMajorVersion: number;
    protocolMinorVersion: number;
    maxTxSize: number;
    maxValueSize: number;
    maxTxExecutionMemoryUnits: string;
    maxTxExecutionCpuUnits: string;
    maxReferenceScriptBytesPerTransaction: number | null;
  }>;
}>;

export type MidgardCapabilityBoundaryEvidence = Readonly<{
  cardanoBoundaryFixtureDigest: string;
  midgardAdjacentBoundaryDigest: string;
  normalProofPathDigest: string;
  forcedProofPathDigest: string;
  appliedValidatorsDigest: string;
  concreteMeasurementsDigest: string;
}>;

export type MidgardCapabilityBoundaryEvidenceSet = Readonly<
  Partial<Record<MidgardCapabilityDimension, MidgardCapabilityBoundaryEvidence>>
>;

export type MidgardCapabilityParityRow = Readonly<{
  dimension: MidgardCapabilityDimension;
  comparison: "greater_than_or_equal";
  cardanoRequired: string | null;
  midgardSupported: string;
  status: "pass" | "fail" | "unknown";
}>;

export type MidgardCapabilityParityReport = Readonly<{
  version: 1;
  profileDigest: string;
  cardanoSnapshot: CardanoCapabilitySnapshot;
  rows: readonly MidgardCapabilityParityRow[];
  boundaryEvidence: MidgardCapabilityBoundaryEvidenceSet;
  blockers: readonly string[];
  reportDigest: string | null;
}>;

const HASH_32_PATTERN = /^[0-9a-f]{64}$/u;
const DECIMAL_PATTERN = /^(0|[1-9][0-9]*)$/u;

const isSafeNonNegativeInteger = (value: unknown): value is number =>
  typeof value === "number" && Number.isSafeInteger(value) && value >= 0;

const parseDecimal = (value: string): bigint | null =>
  DECIMAL_PATTERN.test(value) ? BigInt(value) : null;

const stableJson = (value: unknown): string =>
  canonicalJson(value, "Parity report");

const dimensionFeature = (
  dimension: MidgardCapabilityDimension,
): string | null =>
  dimension.startsWith("feature_") ? dimension.slice("feature_".length) : null;

const cardanoRequiredForDimension = (
  dimension: MidgardCapabilityDimension,
  snapshot: CardanoCapabilitySnapshot,
): bigint | null => {
  const parameters = snapshot.parameters;
  // Feature dimensions intentionally share the default feature fallback.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (dimension) {
    case "total_transaction_bytes":
      return isSafeNonNegativeInteger(parameters.maxTxSize)
        ? BigInt(parameters.maxTxSize)
        : null;
    case "spend_inputs_field_bytes":
    case "reference_inputs_field_bytes":
    case "outputs_field_bytes":
    case "required_observers_field_bytes":
    case "required_signers_field_bytes":
    case "mint_field_bytes":
    case "address_witnesses_field_bytes":
    case "script_witnesses_field_bytes":
    case "redeemers_field_bytes":
      return isSafeNonNegativeInteger(parameters.maxTxSize)
        ? 2n * BigInt(parameters.maxTxSize)
        : null;
    case "serialized_output_bytes":
      return isSafeNonNegativeInteger(parameters.maxTxSize)
        ? BigInt(parameters.maxTxSize)
        : null;
    case "serialized_value_bytes":
      return isSafeNonNegativeInteger(parameters.maxValueSize)
        ? BigInt(parameters.maxValueSize)
        : null;
    case "spend_input_count":
    case "reference_input_count":
    case "output_count":
    case "address_witness_count":
    case "required_signer_count":
    case "required_observer_count":
    case "script_execution_count":
    case "distinct_asset_count":
    case "native_script_depth":
    case "native_script_node_count":
      return isSafeNonNegativeInteger(parameters.maxTxSize)
        ? BigInt(parameters.maxTxSize)
        : null;
    case "transaction_execution_memory_units":
      return parseDecimal(parameters.maxTxExecutionMemoryUnits);
    case "transaction_execution_cpu_units":
      return parseDecimal(parameters.maxTxExecutionCpuUnits);
    case "reference_script_material_bytes":
      return isSafeNonNegativeInteger(
        parameters.maxReferenceScriptBytesPerTransaction,
      )
        ? BigInt(parameters.maxReferenceScriptBytesPerTransaction)
        : null;
    default:
      return dimensionFeature(dimension) === null ? null : 1n;
  }
};

const midgardSupportedForDimension = (
  dimension: MidgardCapabilityDimension,
): bigint => {
  const limits = MIDGARD_CONSENSUS_LIMITS;
  // Feature dimensions intentionally share the default support value.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (dimension) {
    case "total_transaction_bytes":
      return BigInt(limits.maxTxCanonicalCborBytes);
    case "spend_inputs_field_bytes":
      return BigInt(limits.maxSpendInputsPreimageBytes);
    case "reference_inputs_field_bytes":
      return BigInt(limits.maxReferenceInputsPreimageBytes);
    case "outputs_field_bytes":
      return BigInt(limits.maxOutputsPreimageBytes);
    case "required_observers_field_bytes":
      return BigInt(limits.maxRequiredObserversPreimageBytes);
    case "required_signers_field_bytes":
      return BigInt(limits.maxRequiredSignersPreimageBytes);
    case "mint_field_bytes":
      return BigInt(limits.maxMintPreimageBytes);
    case "address_witnesses_field_bytes":
      return BigInt(limits.maxAddressWitnessesPreimageBytes);
    case "script_witnesses_field_bytes":
      return BigInt(limits.maxScriptWitnessesPreimageBytes);
    case "redeemers_field_bytes":
      return BigInt(limits.maxRedeemersPreimageBytes);
    case "serialized_output_bytes":
      return BigInt(limits.maxLedgerOutputPreimageBytes);
    case "serialized_value_bytes":
      return BigInt(limits.maxOutputValueCborBytes);
    case "spend_input_count":
      return BigInt(limits.maxSpendInputCount);
    case "reference_input_count":
      return BigInt(limits.maxReferenceInputCount);
    case "output_count":
      return BigInt(limits.maxOutputCount);
    case "address_witness_count":
      return BigInt(limits.maxAddressWitnessCount);
    case "required_signer_count":
      return BigInt(limits.maxRequiredSignerCount);
    case "required_observer_count":
      return BigInt(limits.maxRequiredObserverCount);
    case "script_execution_count":
      return BigInt(limits.maxScriptExecutionCount);
    case "distinct_asset_count":
      return BigInt(limits.maxDistinctAssetCount);
    case "native_script_depth":
      return BigInt(limits.maxNativeScriptDepth);
    case "native_script_node_count":
      return BigInt(limits.maxNativeScriptNodeCount);
    case "transaction_execution_memory_units":
      return BigInt(limits.minSupportedTransactionExecutionMemoryUnits);
    case "transaction_execution_cpu_units":
      return BigInt(limits.minSupportedTransactionExecutionCpuUnits);
    case "reference_script_material_bytes":
      return BigInt(limits.maxCekProgramMaterialBytes);
    default: {
      const feature = dimensionFeature(dimension);
      return feature !== null &&
        MIDGARD_CONSENSUS_FEATURES.includes(
          feature as (typeof MIDGARD_CONSENSUS_FEATURES)[number],
        )
        ? 1n
        : 0n;
    }
  }
};

const boundaryEvidenceIsComplete = (
  evidence: MidgardCapabilityBoundaryEvidence | undefined,
): boolean =>
  evidence !== undefined &&
  Object.values(evidence).length === 6 &&
  Object.values(evidence).every(
    (digest) => typeof digest === "string" && HASH_32_PATTERN.test(digest),
  );

const snapshotBlockers = (snapshot: CardanoCapabilitySnapshot): string[] => {
  const blockers: string[] = [];
  if (snapshot.version !== 1) blockers.push("unsupported_snapshot_version");
  if (snapshot.source.kind !== "trusted_cardano_node") {
    blockers.push("untrusted_snapshot_source");
  }
  if (
    snapshot.network.length === 0 ||
    snapshot.source.identity.length === 0 ||
    snapshot.source.tip.length === 0
  ) {
    blockers.push("incomplete_snapshot_identity");
  }
  if (
    !isSafeNonNegativeInteger(snapshot.effectiveEpoch) ||
    !isSafeNonNegativeInteger(snapshot.parameters.protocolMajorVersion) ||
    !isSafeNonNegativeInteger(snapshot.parameters.protocolMinorVersion)
  ) {
    blockers.push("invalid_snapshot_integer");
  }
  const observedAt = new Date(snapshot.observedAt);
  if (
    Number.isNaN(observedAt.valueOf()) ||
    observedAt.toISOString() !== snapshot.observedAt
  ) {
    blockers.push("invalid_snapshot_timestamp");
  }
  if (!snapshot.pendingProtocolParameterChanges.queried) {
    blockers.push("pending_parameter_changes_unknown");
  } else if (
    snapshot.pendingProtocolParameterChanges.morePermissiveChangePending ===
    null
  ) {
    blockers.push("pending_parameter_changes_unknown");
  } else if (
    snapshot.pendingProtocolParameterChanges.morePermissiveChangePending
  ) {
    blockers.push("more_permissive_parameter_change_pending");
  }
  return blockers;
};

export const buildMidgardCapabilityParityReport = (
  cardanoSnapshot: CardanoCapabilitySnapshot,
  boundaryEvidence: MidgardCapabilityBoundaryEvidenceSet,
): MidgardCapabilityParityReport => {
  const rows = MIDGARD_CAPABILITY_DIMENSIONS.map((dimension) => {
    const cardanoRequired = cardanoRequiredForDimension(
      dimension,
      cardanoSnapshot,
    );
    const midgardSupported = midgardSupportedForDimension(dimension);
    return Object.freeze({
      dimension,
      comparison: "greater_than_or_equal" as const,
      cardanoRequired: cardanoRequired?.toString() ?? null,
      midgardSupported: midgardSupported.toString(),
      status:
        cardanoRequired === null
          ? ("unknown" as const)
          : midgardSupported >= cardanoRequired
            ? ("pass" as const)
            : ("fail" as const),
    });
  });
  const blockers = snapshotBlockers(cardanoSnapshot);
  for (const row of rows) {
    if (row.status !== "pass") {
      blockers.push(`${row.dimension}:${row.status}`);
    }
    if (!boundaryEvidenceIsComplete(boundaryEvidence[row.dimension])) {
      blockers.push(`${row.dimension}:boundary_evidence_incomplete`);
    }
  }
  const unsignedReport = Object.freeze({
    version: MIDGARD_CAPABILITY_PARITY_REPORT_VERSION,
    profileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    cardanoSnapshot,
    rows: Object.freeze(rows),
    boundaryEvidence,
    blockers: Object.freeze(blockers),
  });
  const reportDigest =
    blockers.length === 0
      ? computeHash32(Buffer.from(stableJson(unsignedReport), "utf8")).toString(
          "hex",
        )
      : null;
  return Object.freeze({
    ...unsignedReport,
    reportDigest,
  });
};

export const assertMidgardCapabilityParityReportComplete = (
  report: MidgardCapabilityParityReport,
): void => {
  const rebuilt = buildMidgardCapabilityParityReport(
    report.cardanoSnapshot,
    report.boundaryEvidence,
  );
  if (
    rebuilt.reportDigest === null ||
    report.reportDigest !== rebuilt.reportDigest ||
    stableJson(report) !== stableJson(rebuilt)
  ) {
    throw new Error(
      "Midgard V1 capability parity evidence is incomplete, stale, or invalid",
    );
  }
};
