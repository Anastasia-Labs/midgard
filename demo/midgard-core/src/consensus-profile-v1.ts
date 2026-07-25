import {
  MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
  MIDGARD_CEK_PROGRAM_ENVELOPE_V1_VERSION,
  MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_V1_VERSION,
  MIDGARD_CEK_PROGRAM_MATERIAL_V1_VERSION,
  MIDGARD_MAX_DA_PAYLOAD_BYTES_V1,
  MIDGARD_PROOF_SUBMISSION_ENVELOPE_V1_VERSION,
} from "./cek-proof.js";
import { computeHash32 } from "./codec/hash.js";
import { MIDGARD_NATIVE_TX_V1_VERSION } from "./codec/native-constants.js";

/**
 * Exact protocol tuple for the canonical V1 validator deployment.
 *
 * Importing the tuple does not activate a release. Activation requires exact
 * deployment-manifest, validator-hash, and release-evidence matches at every
 * trust boundary.
 */
export const MIDGARD_PROTOCOL_V1_VERSION = 1 as const;
export const MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION = 1 as const;
// V1 retains the newest machine semantics: every source constant is
// authenticated through its exact bounded direct witness, context constants
// are separated from source-program terms, and ByteArray work witnesses use
// Aiken's exact chunked cbor.serialise representation.
export const MIDGARD_VALIDATION_MACHINE_V1_VERSION = 1 as const;
export const MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION = 1 as const;
export const MIDGARD_VALIDATION_DISPUTE_V1_VERSION = 1 as const;
export const MIDGARD_DA_INNER_V1_SCHEMA_VERSION = 1 as const;
export const MIDGARD_CEK_VALUE_V1_SCHEMA_VERSION = 1 as const;
export const MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION =
  "midgard-deployment-manifest-v1" as const;
export const MIDGARD_PROTOCOL_INFO_V1_API_VERSION = 1 as const;
export const MIDGARD_CONSENSUS_PROFILE_V1_ID = "midgard-consensus-v1" as const;

const cborByteStringSize = (payloadBytes: number): number => {
  if (payloadBytes < 24) return 1 + payloadBytes;
  if (payloadBytes <= 0xff) return 2 + payloadBytes;
  if (payloadBytes <= 0xffff) return 3 + payloadBytes;
  return 5 + payloadBytes;
};

const MAX_L1_FAULT_PROOF_TX_BYTES = 16 * 1024;
// This reservation applies to one independently revealed proof chunk. It is
// not a user-facing field or transaction limit: aggregate fields are
// authenticated and consumed through bounded-blob continuations.
const MAX_TRANSACTION_FIELD_PROOF_OVERHEAD_BYTES = 7 * 1024;
// A corresponding Cardano field must fit inside the complete L1 transaction.
// Midgard's canonical nested wrappers can expand that representation, so V1
// reserves twice the live L1 transaction envelope per aggregate field. Proofs
// consume it only through the bounded chunks below.
const MAX_TRANSACTION_AGGREGATE_FIELD_BYTES = 2 * MAX_L1_FAULT_PROOF_TX_BYTES;
const MAX_TRANSACTION_FIELD_CHUNK_BYTES = 4_095;
const MAX_LEDGER_MEMBERSHIP_PROOF_OVERHEAD_BYTES = 12 * 1024;
const MAX_LEDGER_OUTPUT_PREIMAGE_BYTES = MAX_L1_FAULT_PROOF_TX_BYTES;
const MAX_OUTPUT_VALUE_CBOR_BYTES = 5_000;
// Raw Flat is a local authoring input, not an L1-revealed script preimage.
// Consensus script bytes are the compact program envelope below; graph
// material is admitted separately and checked against its exact DA encoding.
const MAX_CEK_PROGRAM_NODE_COUNT = Number(
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
);
const MAX_CEK_PROGRAM_MATERIAL_BYTES = Number(
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
);

/**
 * Concrete serialization measurements that justify the profile's proof
 * envelope reservations. These are evidence, not independently configurable
 * consensus limits.
 */
export const MIDGARD_V1_ENVELOPE_MEASUREMENTS = Object.freeze({
  maxGeneralFieldResolverArgumentsBytes: 14_082,
  maxLedgerOutputResolverArgumentsBytes: 13_459,
  maxScriptEnvelopeResolverArgumentsBytes: 7_546,
  concreteConwayProofArgumentBytes: 14_546,
  concreteConwayProofTransactionFramingBytes: 395,
  concreteConwayProofTransactionBytes: 14_941,
  maxFieldPublicationDatumBytes: 4_574,
  maxFieldPublicationUnsignedTransactionBytes: 4_675,
  maxProgramMaterialPublicationDatumBytes: 4_268,
  maxProgramMaterialPublicationUnsignedTransactionBytes: 4_369,
  maxFieldChunkReceiptPublicationMemoryUnits: 3_398_228,
  maxFieldChunkReceiptPublicationCpuUnits: 1_209_745_039,
  canonicalReceiptOrderVerificationMemoryUnits: 1_233_800,
  canonicalReceiptOrderVerificationCpuUnits: 432_521_347,
  maxDirectBlsFinalVerificationMemoryUnits: 10_031_847,
  maxDirectBlsFinalVerificationCpuUnits: 7_530_426_553,
  maxBlsFinalBuiltinTransitionMemoryUnits: 10_557_458,
  maxBlsFinalBuiltinTransitionCpuUnits: 7_723_985_480,
  runtimeTypeFailureTransitionMemoryUnits: 887_982,
  runtimeTypeFailureTransitionCpuUnits: 331_669_040,
  secpNonResidueFailureTransitionMemoryUnits: 7_619_429,
  secpNonResidueFailureTransitionCpuUnits: 2_661_184_546,
  firstRejectedBlsFinalVerificationMemoryUnits: 11_692_648,
  firstRejectedBlsFinalVerificationCpuUnits: 8_853_636_332,
});

// One byte per item is the absolute lower encoding floor under a 16,384-byte
// Cardano transaction. Real items are larger, so these derived guardrails can
// never reject a shape Cardano could fit; field bytes remain the effective
// constraint.
const MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT = MAX_L1_FAULT_PROOF_TX_BYTES;
const MAX_SPEND_INPUT_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const MAX_REFERENCE_INPUT_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const MAX_OUTPUT_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const MAX_ADDRESS_WITNESS_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const MAX_REQUIRED_SIGNER_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const MAX_REQUIRED_OBSERVER_COUNT = MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT;
const VALIDATION_DISPUTE_RESPONSE_WINDOW_MS = 300_000;
const MAX_VALIDATION_BISECTION_ROUNDS = 32;
const PROOF_BLOCK_MATURITY_MS = 7 * 24 * 60 * 60 * 1000;
// Opening, two moves per round, and settlement must fit in the first half of
// maturity even if every party uses its complete response window.
const MIN_VALIDATION_DISPUTE_MATURITY_MS =
  2 *
  (2 * MAX_VALIDATION_BISECTION_ROUNDS + 2) *
  VALIDATION_DISPUTE_RESPONSE_WINDOW_MS;

const MAX_INPUT_PREIMAGE_BYTES = MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_REFERENCE_INPUT_PREIMAGE_BYTES =
  MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_REQUIRED_OBSERVERS_PREIMAGE_BYTES =
  MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_REQUIRED_SIGNERS_PREIMAGE_BYTES =
  MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_ADDRESS_WITNESSES_PREIMAGE_BYTES =
  MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;

const MAX_OUTPUTS_PREIMAGE_BYTES = MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_MINT_PREIMAGE_BYTES = MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_SCRIPT_WITNESSES_PREIMAGE_BYTES =
  MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
const MAX_REDEEMERS_PREIMAGE_BYTES = MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;

// This is not an independently chosen transaction cap. It is the exact upper
// bound obtained by wrapping every bounded dynamic preimage in the canonical
// full-transaction encoding and adding the fixed-size fields.
const MAX_EFFECTIVE_TRANSACTION_CBOR_BYTES = (() => {
  const bodyBytes =
    1 + // 12-field body array
    cborByteStringSize(MAX_INPUT_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_REFERENCE_INPUT_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_OUTPUTS_PREIMAGE_BYTES) +
    9 + // fee
    9 + // validity start
    9 + // validity end
    cborByteStringSize(MAX_REQUIRED_OBSERVERS_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_REQUIRED_SIGNERS_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_MINT_PREIMAGE_BYTES) +
    cborByteStringSize(32) +
    cborByteStringSize(32) +
    2; // network-id sentinel 255
  const witnessSetBytes =
    1 + // three-field witness-set array
    cborByteStringSize(MAX_ADDRESS_WITNESSES_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_SCRIPT_WITNESSES_PREIMAGE_BYTES) +
    cborByteStringSize(MAX_REDEEMERS_PREIMAGE_BYTES);
  return 1 + 1 + bodyBytes + witnessSetBytes + 1;
})();

export const MIDGARD_CONSENSUS_LIMITS_V1 = Object.freeze({
  minSupportedL1MaxTxBytes: MAX_L1_FAULT_PROOF_TX_BYTES,
  minSupportedL1MaxTxMemoryUnits: 16_500_000,
  minSupportedL1MaxTxCpuUnits: 10_000_000_000,
  // These are capability floors, not execution caps. Midgard must admit at
  // least the aggregate execution budget of any transaction admitted by the
  // target Cardano network; validation may span more L1 proof transactions.
  minSupportedTransactionExecutionMemoryUnits: 16_500_000,
  minSupportedTransactionExecutionCpuUnits: 10_000_000_000,
  maxTransactionFieldProofOverheadBytes:
    MAX_TRANSACTION_FIELD_PROOF_OVERHEAD_BYTES,
  maxTransactionAggregateFieldBytes: MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  maxTransactionFieldChunkBytes: MAX_TRANSACTION_FIELD_CHUNK_BYTES,
  maxLedgerMembershipProofOverheadBytes:
    MAX_LEDGER_MEMBERSHIP_PROOF_OVERHEAD_BYTES,
  maxLedgerOutputPreimageBytes: MAX_LEDGER_OUTPUT_PREIMAGE_BYTES,
  maxOutputValueCborBytes: MAX_OUTPUT_VALUE_CBOR_BYTES,
  maxCekProgramEnvelopeBytes: MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1,
  maxCekProgramNodeCount: MAX_CEK_PROGRAM_NODE_COUNT,
  maxCekProgramMaterialBytes: MAX_CEK_PROGRAM_MATERIAL_BYTES,
  maxCekBlobChunkBytes: 4_095,
  maxCekBuiltinTag: 86,
  // The complete ten-leaf CEK transition (root checks, semantics, pinned
  // builtin budget, and exact successor) measures 10,557,458 memory /
  // 7,723,985,480 CPU, below the reserved 80% L1 ceilings. The smaller direct
  // evaluator establishes that twelve leaves already measure 11,692,648 /
  // 8,853,636,332, so the complete transition cannot fit that CPU reserve.
  maxCekDirectBlsMillerLoopLeaves: 10,
  maxCekDirectBlsExpressionDepth: 10,
  maxSpendInputsPreimageBytes: MAX_INPUT_PREIMAGE_BYTES,
  maxReferenceInputsPreimageBytes: MAX_REFERENCE_INPUT_PREIMAGE_BYTES,
  maxOutputsPreimageBytes: MAX_OUTPUTS_PREIMAGE_BYTES,
  maxRequiredObserversPreimageBytes: MAX_REQUIRED_OBSERVERS_PREIMAGE_BYTES,
  maxRequiredSignersPreimageBytes: MAX_REQUIRED_SIGNERS_PREIMAGE_BYTES,
  maxMintPreimageBytes: MAX_MINT_PREIMAGE_BYTES,
  maxAddressWitnessesPreimageBytes: MAX_ADDRESS_WITNESSES_PREIMAGE_BYTES,
  maxScriptWitnessesPreimageBytes: MAX_SCRIPT_WITNESSES_PREIMAGE_BYTES,
  maxRedeemersPreimageBytes: MAX_REDEEMERS_PREIMAGE_BYTES,
  maxTxCanonicalCborBytes: MAX_EFFECTIVE_TRANSACTION_CBOR_BYTES,
  maxNativeScriptDepth: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  maxNativeScriptNodeCount: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  maxSpendInputCount: MAX_SPEND_INPUT_COUNT,
  maxReferenceInputCount: MAX_REFERENCE_INPUT_COUNT,
  maxOutputCount: MAX_OUTPUT_COUNT,
  maxAddressWitnessCount: MAX_ADDRESS_WITNESS_COUNT,
  maxRequiredSignerCount: MAX_REQUIRED_SIGNER_COUNT,
  maxScriptExecutionCount: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  maxRequiredObserverCount: MAX_REQUIRED_OBSERVER_COUNT,
  maxDistinctAssetCount: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  maxL2TransactionCount: 10_000,
  maxWithdrawalCount: 10_000,
  maxForcedTransactionCount: 10_000,
  maxDepositCount: 10_000,
  maxTotalEventCount: 40_000,
  maxTransitionStepCount: 40_000,
  maxValidationTraceCount: 20_000,
  maxLedgerOperationCount: 40_000,
  maxValidationMachineStepCount: 0xffff_ffff,
  maxValidationBisectionRounds: MAX_VALIDATION_BISECTION_ROUNDS,
  validationDisputeResponseWindowMs: VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
  minValidationDisputeMaturityMs: MIN_VALIDATION_DISPUTE_MATURITY_MS,
  blockMaturityMs: PROOF_BLOCK_MATURITY_MS,
  maxCanonicalTransactionBytesPerBlock: 16 * 1024 * 1024,
  maxDaPayloadBytes: MIDGARD_MAX_DA_PAYLOAD_BYTES_V1,
});

export const MIDGARD_CONSENSUS_FEATURES_V1 = Object.freeze([
  "mint_burn",
  "reference_inputs",
  "native_cardano_scripts",
  "plutus_v3_scripts",
  "midgard_v1_scripts",
  "script_witnesses",
  "redeemers",
  "reference_scripts",
  "l1_program_material_publication",
  "script_payment_credentials",
  "protected_outputs",
  "required_observers",
  "valid_forced_transactions",
  "invalid_forced_transactions",
] as const);

export const MIDGARD_V1_REQUIRED_PROOF_FAMILIES = Object.freeze([
  "validation-trace-endpoint",
  "validation-trace-bisection",
  "validation-machine-one-step",
  "validation-dispute-timeout",
  "transition-trace-accepted-transaction",
  "transition-trace-rejected-no-op",
  "forced-transaction-verdict-mismatch",
  "forced-program-material-availability",
] as const);

/**
 * The V1 profile must not become a runtime feature flag. A release changes
 * this compiled value only after the validator-hash-bound evidence bundle for
 * every required proof family has passed the proof-fit gate.
 */
export const MIDGARD_V1_RELEASE_EVIDENCE_DIGEST: string | null = null;

export const assertMidgardConsensusV1ReleaseReady = (): void => {
  if (MIDGARD_V1_RELEASE_EVIDENCE_DIGEST === null) {
    throw new Error(
      "midgard-consensus-v1 is not activated: the compiled L1 verifier and validator-hash-bound release evidence are incomplete",
    );
  }
  if (!/^[0-9a-f]{64}$/u.test(MIDGARD_V1_RELEASE_EVIDENCE_DIGEST)) {
    throw new Error(
      "midgard-consensus-v1 has an invalid compiled release evidence digest",
    );
  }
};

export const MIDGARD_CONSENSUS_PROFILE_V1 = Object.freeze({
  profileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
  protocolVersion: MIDGARD_PROTOCOL_V1_VERSION,
  nativeTransactionVersion: Number(MIDGARD_NATIVE_TX_V1_VERSION) as 1,
  nativeTransactionProofSourceVersion: 1,
  transitionStepSchemaVersion: MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION,
  headerSchemaVersion: 1,
  stateQueueSchemaVersion: 1,
  transactionOrderSchemaVersion: 1,
  transactionFieldPublicationSchemaVersion: 1,
  forcedTransactionJournalVersion: 1,
  daPayloadVersion: MIDGARD_DA_INNER_V1_SCHEMA_VERSION,
  daEnvelopeVersion: 1,
  daTransportProtocolVersion: 1,
  daRuntimeManifestSchemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  validationMachineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  validationTraceDescriptorVersion:
    MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
  validationDisputeVersion: MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
  cekProgramEnvelopeVersion: Number(
    MIDGARD_CEK_PROGRAM_ENVELOPE_V1_VERSION,
  ) as 1,
  cekValueSchemaVersion: MIDGARD_CEK_VALUE_V1_SCHEMA_VERSION,
  cekProgramMaterialVersion: Number(
    MIDGARD_CEK_PROGRAM_MATERIAL_V1_VERSION,
  ) as 1,
  cekProgramMaterialSidecarVersion: Number(
    MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_V1_VERSION,
  ) as 1,
  proofSubmissionEnvelopeVersion: Number(
    MIDGARD_PROOF_SUBMISSION_ENVELOPE_V1_VERSION,
  ) as 1,
  scriptProofSchemaVersion: 1,
  ledgerOutputSchemaVersion: 1,
  mpfProofSchemaVersion: 1,
  deploymentManifestSchemaVersion:
    MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  protocolInfoApiVersion: MIDGARD_PROTOCOL_INFO_V1_API_VERSION,
  limits: MIDGARD_CONSENSUS_LIMITS_V1,
  features: MIDGARD_CONSENSUS_FEATURES_V1,
  requiredProofFamilies: MIDGARD_V1_REQUIRED_PROOF_FAMILIES,
});

export type MidgardConsensusProfileV1 = typeof MIDGARD_CONSENSUS_PROFILE_V1;

const stableJsonValue = (value: unknown): unknown => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new TypeError("V1 consensus profile numbers must be safe integers");
    }
    return value;
  }
  if (Array.isArray(value)) {
    if (Object.keys(value).length !== value.length) {
      throw new TypeError("V1 consensus profile arrays must be dense");
    }
    return value.map(stableJsonValue);
  }
  if (typeof value !== "object") {
    throw new TypeError("V1 consensus profile contains an unsupported value");
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new TypeError("V1 consensus profile objects must be plain records");
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new TypeError("V1 consensus profile contains a non-string key");
  }
  return Object.fromEntries(
    Object.entries(value)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([key, child]) => [key, stableJsonValue(child)]),
  );
};

const CONSENSUS_PROFILE_V1_STABLE_JSON = JSON.stringify(
  stableJsonValue(MIDGARD_CONSENSUS_PROFILE_V1),
);

export const encodeMidgardConsensusProfileV1 = (): Buffer =>
  Buffer.from(CONSENSUS_PROFILE_V1_STABLE_JSON, "utf8");

export const MIDGARD_CONSENSUS_PROFILE_V1_DIGEST = computeHash32(
  encodeMidgardConsensusProfileV1(),
).toString("hex");

export const isMidgardConsensusProfileV1 = (
  value: unknown,
): value is MidgardConsensusProfileV1 => {
  try {
    return (
      JSON.stringify(stableJsonValue(value)) ===
      CONSENSUS_PROFILE_V1_STABLE_JSON
    );
  } catch {
    return false;
  }
};
