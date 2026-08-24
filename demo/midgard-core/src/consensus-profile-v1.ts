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
import {
  MIDGARD_NATIVE_SCRIPT_MAX_DEPTH_V1,
  MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT_V1,
} from "./codec/native-script.js";

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
// The repository-owned Conway measurements apply the final proof-item and
// five-stage canonical-decode item validators. The publication cap remains
// 14,396 raw item bytes: the applied SDK publisher produces a 15,256-byte
// complete signed transaction at that cap, retaining 1,128 bytes below the
// 16,384-byte deployment floor. Direct carriage is separately bounded by the
// authenticate stage of the production five-stage submitter, measured below.
const MAX_SINGLE_PUBLICATION_COMPLETE_ITEM_BYTES = 14_396;
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
  proofItemEnvelopeReliabilityReserveBytes: 512,
  // Corrected 2026-08-14 (owner ruling, #579 lane): these two counted-era
  // item-size frontiers had drifted ~80 bytes below the shape they describe.
  // The tell is internal: the three sibling rows below
  // (`...DatumBytes` 15,624, `...MinAdaLovelace` 68,231,610,
  // `...FeeLovelace` 853,925) are measurements of *the same publication*, and
  // all three land exactly on an item size of 15,073 — not on 14,993. Measured
  // by the publication producer itself (`publishProofItem` ->
  // `deriveValidationProofItemPublicationV1` ->
  // `buildUnsignedValidationProofItemPublicationV1Program`):
  //   item 15,072 -> signed 15,871   item 15,073 -> signed 15,872 (= 16,384-512)
  //   item 15,074 -> signed 15,873   item 15,570 -> signed 16,384 (= maxTxSize)
  //   item 15,571 -> signed 16,385
  // This publication carries no script — it is an inline-datum output to a
  // 28-byte script address — so no blueprint regeneration can move it, which
  // is why this is a correction and not a #579 cascade re-pin.
  maxExactCompleteItemPublicationBytes: 15_570,
  maxReliableCompleteItemPublicationBytes: 15_073,
  maxReliableCompleteItemPublicationTransactionBytes: 15_872,
  maxReliableCompleteItemPublicationDatumBytes: 15_624,
  maxReliableCompleteItemPublicationMinAdaLovelace: 68_231_610,
  maxReliableCompleteItemPublicationFeeLovelace: 853_925,
  // Re-pinned 2026-08-23 at the #617 wave sign-off point (owner rulings on
  // the #622 sign-off table, 2026-08-22: ruling (b) APPROVED; the
  // `referenceCompleteItem*` rows ride the same signature per the #624
  // ruling). Option B (#620) made the canonical-decode item semantic
  // transition-only, so the item preimage no longer rides the AUTHENTICATE
  // redeemer: prepare-selected and authenticate became item-size-independent
  // and the direct-route binder moved to the OBSERVE door. Measured
  // end-to-end through the production five-stage submitter on the
  // regenerated blueprint by the three
  // demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*-v1
  // suites (7/7 green in the re-pin lane, at items 8,277 / 13,522 / 13,523 /
  // 14,004 / 14,005 / 14,336).
  //
  // Adjacent-item probe ledger (signed OBSERVE bytes; the frontier is the
  // largest item whose transaction the builder completes):
  //   item 13,522 -> 15,872 (= budget)   item 13,523 -> 15,888, completes
  //   item 14,004 -> 16,369 (margin 15)  item 14,005 -> refused PRE-SIGN at
  //                                      a projected 16,385 > maxTxSize,
  //                                      auto-demoting to the publication
  //                                      fallback and completing by
  //                                      reference to award
  //
  // The two review-mandated caveats that must ride these numbers, verbatim
  // from the #622 owner sign-off table:
  //   1. *Framing-relative*: 13,522/14,004 are measured under this fixture
  //      family's journey framing (constant 1,926–1,927 bytes across all
  //      measured items). A different production framing shifts them by tens
  //      of bytes — a routing-cost effect only; the pre-sign projection +
  //      envelope gate are the operative guards and are proven live at
  //      14,005.
  //   2. *Quantization ladder*: signed observe size is nonmonotone near the
  //      envelope (CBOR integer-width crossings in fee/change, +16/−4
  //      jumps); **no in-family item signs at exactly 16,384** — the usable
  //      frontier is the contiguous 14,004. The 512-byte reserve absorbs the
  //      ≤16-byte wobble.
  //
  // The retired per-stage rows re-derive in the same pass, as the ruling
  // requires. At the reserve frontier the item-independent stages measure
  // prepare-selected 1,864 / authenticate 2,656 / source 7,895 / proof 5,701
  // / settle 5,064, byte-identical across every measured item from 8,277 to
  // 14,336 - which is exactly why rebinding these two frontiers changes
  // routing only inside the complete-item lane. The `...ProofTransaction`
  // row keeps its established reading (the largest transaction of the
  // direct-route proof journey, which the reserve frontier by construction
  // places on the budget); post-Option-B that transaction is the observe
  // door rather than authenticate, which is the whole of the change to the
  // three per-stage rows below.
  maxExactDirectCompleteItemBytes: 14_004,
  maxReliableDirectCompleteItemBytes: 13_522,
  maxReliableDirectCompleteItemProofTransactionBytes: 15_872,
  maxReliableDirectCompleteItemAuthenticationTransactionBytes: 2_656,
  maxReliableDirectCompleteItemObservationTransactionBytes: 15_872,
  // Reference route, re-pinned from the same #622 table at that route's own
  // frontier: item 14,336, the measured tier-1 stageability boundary (14,336
  // stages, 14,337 refuses as tier-2 carriage). The `...ProofTransaction`
  // row names the transaction that carries the proof with the item resolved
  // by reference; post-Option-B the observe door IS that transaction, so it
  // and the observation row are one and the same 1,959-byte measurement, and
  // its two reference inputs are its own reference script plus the published
  // proof item. Measured reference-input counts across the six-transaction
  // reference leg (publication, authenticate, source, observe, proof,
  // settle): [0, 1, 0, 2, 0, 0]. The item itself rides the 15,135-byte
  // section 8 publication, pinned by the publication rows above.
  referenceCompleteItemProofTransactionBytes: 1_959,
  referenceCompleteItemAuthenticationTransactionBytes: 2_656,
  referenceCompleteItemObservationTransactionBytes: 1_959,
  referenceCompleteItemProofReferenceInputCount: 2,
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
// C49/C70 (#618, #627). The target network's `coins_per_utxo_byte` protocol
// parameter, the only free parameter of the minimum-Ada floor
// `coins_per_utxo_byte * (160 + serialized canonical output bytes)`
// (`minAdaLovelaceV1`/`outputMeetsMinAdaV1` in
// demo/midgard-validation/src/value-accounting.ts, and their Aiken twin
// `min_ada_lovelace_v1`/`output_meets_min_ada_v1` in
// onchain/aiken/lib/midgard/validation-machine-v1.ak).
//
// Provenance: the C70 target-parameter snapshot, preprod epoch 303, pinned as
// `PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.coinsPerUtxoByte === 4_310n`
// (demo/midgard-validation/tests/helpers/ordered-collection-boundary-v1.ts).
// That helper is a test corpus; this is the production pin the validator half
// mirrors, and its Aiken counterpart is `env.coins_per_utxo_byte`, held
// identical in onchain/aiken/env/default.ak and onchain/aiken/env/testnet.ak.
// demo/midgard-validation/tests/min-ada-twin-cross-check-v1.test.ts reads all
// three as source text and fails if any pair disagrees.
//
// A COMPILED DEPLOYMENT CONSTANT, NOT A DECLARED FIELD (owner ruling
// 2026-08-23 on #627, option B): the rate that convicts a block is not the
// block producer's to declare, so it is neither a block-header field nor an
// element of the hashed validation context. Changing it is a redeploy event,
// exactly as for `maxDistinctAssetCount` below.
const TARGET_COINS_PER_UTXO_BYTE = 4_310;
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
  maxSinglePublicationCompleteItemBytes:
    MAX_SINGLE_PUBLICATION_COMPLETE_ITEM_BYTES,
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
  maxNativeScriptDepth: MIDGARD_NATIVE_SCRIPT_MAX_DEPTH_V1,
  maxNativeScriptNodeCount: MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT_V1,
  maxSpendInputCount: MAX_SPEND_INPUT_COUNT,
  maxReferenceInputCount: MAX_REFERENCE_INPUT_COUNT,
  maxOutputCount: MAX_OUTPUT_COUNT,
  maxAddressWitnessCount: MAX_ADDRESS_WITNESS_COUNT,
  maxRequiredSignerCount: MAX_REQUIRED_SIGNER_COUNT,
  maxScriptExecutionCount: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  maxRequiredObserverCount: MAX_REQUIRED_OBSERVER_COUNT,
  maxDistinctAssetCount: MAX_TX_SIZE_DERIVED_COLLECTION_ITEM_COUNT,
  coinsPerUtxoByte: TARGET_COINS_PER_UTXO_BYTE,
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
