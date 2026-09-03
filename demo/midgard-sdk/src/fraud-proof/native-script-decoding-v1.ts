/**
 * `native-script-decoding` family (#635, #633) — off-chain codec twins.
 *
 * Proves an operator verdict wrong about the decodability of a resolved
 * reference script — the `ResolvedReferenceScriptMalformed` / `NodeLimit` /
 * `DepthLimit` corner of the rejection catalogue — in either direction:
 * wrongful acceptance (direction A, the frozen scan refuses the accepted
 * script) or wrongful rejection (direction B, the accused script scans to the
 * exact canonical terminal).
 *
 * Violation: `native-script-decoding`.
 * Production catalogue category: `nativeScriptDecoding` (`0000000d`). The
 * asset-name helper still accepts the deployed category id so callers remain
 * explicitly bound to the manifest they are submitting against.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/
 * step-0{1,2,3,4}.ak` (and `engine.ak` for the thread states) field for field
 * and constructor index for constructor index; the exact bytes are pinned in
 * `tests/native-script-decoding-v1.test.ts` against values measured out of
 * those Aiken modules over their own `thread_fixture_v1` fixtures.
 */
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  hashHexWithBlake2b,
  type HashingError,
  MerkleRootSchema,
  OutputReferenceSchema,
  ProofSchema,
} from "../common.js";
import {
  BoundedItemChunkProofSchema,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxSchema,
  HeaderSchema,
  TransitionStepSchema,
} from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { type ChallengedHeaderHash } from "./fabricated-deposit-v1.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import { NativeScriptFrameSchema } from "./validation-auxiliary-witness-v1.js";

/** Normative violation identifier. */
export const NATIVE_SCRIPT_DECODING_VIOLATION_ID =
  "native-script-decoding" as const;

// ## Engine constants (twin of `engine.ak:54-99`), as `Data` integers

export const NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE = 0n;
export const NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION = 1n;
export const NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL = 0n;
export const NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED = 1n;
export const NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND = 0n;
export const NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE = 1n;
export const NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED = 0n;
export const NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT = 1n;
export const NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT = 2n;
/** Sentinel for a class not (yet) established. */
export const NATIVE_SCRIPT_DECODING_CLASS_PENDING = -1n;
/** Sentinel for the descriptor fields before the bind step froze them. */
export const NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND = -2n;

// ## Thread NFT asset name

/**
 * A decoding-fault computation-thread token's asset name: the family's
 * deployed category id (4 bytes) followed by the challenged header hash.
 */
export const nativeScriptDecodingThreadTokenAssetName = (
  categoryId: string,
  challengedHeaderHash: ChallengedHeaderHash,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "native-script-decoding category id must be 4 bytes of lowercase hex",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error("challenged header hash must be 28 bytes of lowercase hex");
  }
  return `${categoryId}${challengedHeaderHash}`;
};

// ## Thread states (twin of `engine.ak:111-152`)

/** Step-02's input state (step-01's output): the bound verdict subject. */
export const NativeScriptDecodingBindStateSchema = Data.Object({
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  /** Id-verified subject tx id; `""` sentinel for forced threads until step-02. */
  verified_tx_id: Data.Bytes(),
});
export type NativeScriptDecodingBindState = Data.Static<
  typeof NativeScriptDecodingBindStateSchema
>;
export const NativeScriptDecodingBindState =
  NativeScriptDecodingBindStateSchema as unknown as NativeScriptDecodingBindState;

/**
 * The constant-size thread state from step-02's output onward — 15 fields in
 * `engine.ak` declaration order. `tx_order_id` is the serialised forced-leaf
 * trie key (`""` for normal threads) — the design's `Int` was a type repair.
 */
export const NativeScriptDecodingScanThreadStateSchema = Data.Object({
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  verified_tx_id: Data.Bytes(),
  tx_order_id: Data.Bytes(),
  /** Direction B: 0/1/2 mirroring the leaf's arm; `-1` for direction A. */
  scan_reason_class: Data.Integer(),
  /** The transition step's `pre_utxos_root`. */
  prior_ledger_root: MerkleRootSchema,
  outpoint_source_kind: Data.Integer(),
  outpoint_cursor: Data.Integer(),
  /** blake2b-256 of the accused outpoint's trie-key bytes; `""` until OpenSubject. */
  outpoint_key_hash: Data.Bytes(),
  reference_script_language: Data.Integer(),
  output_index: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  /** `hash_machine_control_v1` of the current control; `""` until the machine runs. */
  machine_state_hash: Data.Bytes(),
  refusal_class: Data.Integer(),
});
export type NativeScriptDecodingScanThreadState = Data.Static<
  typeof NativeScriptDecodingScanThreadStateSchema
>;
export const NativeScriptDecodingScanThreadState =
  NativeScriptDecodingScanThreadStateSchema as unknown as NativeScriptDecodingScanThreadState;

// ## Step 01 — bind the verdict subject

export const NativeScriptDecodingStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type NativeScriptDecodingStep01Datum = Data.Static<
  typeof NativeScriptDecodingStep01DatumSchema
>;
export const NativeScriptDecodingStep01Datum =
  NativeScriptDecodingStep01DatumSchema as unknown as NativeScriptDecodingStep01Datum;

/**
 * Twin of `step_01.Args`: `BindNormalTransaction` (direction A over a normal
 * leaf, bound through the counted `transactions_root`) is constructor 0,
 * `RecordForcedSource` (either direction, forced leaf bound at step-02) is
 * constructor 1.
 */
export const NativeScriptDecodingStep01ArgsSchema = Data.Enum([
  Data.Object({
    BindNormalTransaction: Data.Object({
      carriage: NativeTxInclusionCarriageSchema,
    }),
  }),
  Data.Object({
    RecordForcedSource: Data.Object({
      direction: Data.Integer(),
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export type NativeScriptDecodingStep01Args = Data.Static<
  typeof NativeScriptDecodingStep01ArgsSchema
>;
export const NativeScriptDecodingStep01Args =
  NativeScriptDecodingStep01ArgsSchema as unknown as NativeScriptDecodingStep01Args;

export const NativeScriptDecodingStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep01ArgsSchema);
export type NativeScriptDecodingStep01SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep01SpendRedeemerSchema
>;
export const NativeScriptDecodingStep01SpendRedeemer =
  NativeScriptDecodingStep01SpendRedeemerSchema as unknown as NativeScriptDecodingStep01SpendRedeemer;

// ## Step 02 — committed-claim openings

export const NativeScriptDecodingStep02DatumSchema = faultProofStepDatumSchema(
  NativeScriptDecodingBindStateSchema,
);
export type NativeScriptDecodingStep02Datum = Data.Static<
  typeof NativeScriptDecodingStep02DatumSchema
>;
export const NativeScriptDecodingStep02Datum =
  NativeScriptDecodingStep02DatumSchema as unknown as NativeScriptDecodingStep02Datum;

export const NativeScriptDecodingEventToStepMembershipSchema =
  rootMembershipProofSchema(EventKeySchema, EventToStepValueSchema);
export const NativeScriptDecodingTransitionStepMembershipSchema =
  rootMembershipProofSchema(Data.Integer(), TransitionStepSchema);
export const NativeScriptDecodingForcedMembershipSchema =
  rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxSchema);

/** Twin of `step_02.Args`. */
export const NativeScriptDecodingStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  /** The disputed block's header, bound to the thread NFT's asset name. */
  header: HeaderSchema,
  event_to_step_membership: NativeScriptDecodingEventToStepMembershipSchema,
  transition_step_membership:
    NativeScriptDecodingTransitionStepMembershipSchema,
  /** Forced threads: the verdict leaf. `null` for normal threads. */
  forced_membership: Data.Nullable(NativeScriptDecodingForcedMembershipSchema),
  /** Direction A: the prover-chosen accused pair. Ignored for direction B. */
  chosen_outpoint_source_kind: Data.Integer(),
  chosen_outpoint_cursor: Data.Integer(),
});
export type NativeScriptDecodingStep02Args = Data.Static<
  typeof NativeScriptDecodingStep02ArgsSchema
>;
export const NativeScriptDecodingStep02Args =
  NativeScriptDecodingStep02ArgsSchema as unknown as NativeScriptDecodingStep02Args;

export const NativeScriptDecodingStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep02ArgsSchema);
export type NativeScriptDecodingStep02SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep02SpendRedeemerSchema
>;
export const NativeScriptDecodingStep02SpendRedeemer =
  NativeScriptDecodingStep02SpendRedeemerSchema as unknown as NativeScriptDecodingStep02SpendRedeemer;

// ## Split step 03 — OpenSubject, BindDescriptor, AdvanceOrClose

const NativeScriptDecodingStep03DatumSchema = faultProofStepDatumSchema(
  NativeScriptDecodingScanThreadStateSchema,
);

export const NativeScriptDecodingStep03OpenSubjectDatumSchema =
  NativeScriptDecodingStep03DatumSchema;
export type NativeScriptDecodingStep03OpenSubjectDatum = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectDatumSchema
>;
export const NativeScriptDecodingStep03OpenSubjectDatum =
  NativeScriptDecodingStep03OpenSubjectDatumSchema as unknown as NativeScriptDecodingStep03OpenSubjectDatum;
export const NativeScriptDecodingStep03OpenSubjectArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  subject_field_opening: Data.Nullable(FieldOpeningSchema),
});
export type NativeScriptDecodingStep03OpenSubjectArgs = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectArgsSchema
>;
export const NativeScriptDecodingStep03OpenSubjectArgs =
  NativeScriptDecodingStep03OpenSubjectArgsSchema as unknown as NativeScriptDecodingStep03OpenSubjectArgs;
export const NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep03OpenSubjectArgsSchema);
export type NativeScriptDecodingStep03OpenSubjectSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema
>;
export const NativeScriptDecodingStep03OpenSubjectSpendRedeemer =
  NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema as unknown as NativeScriptDecodingStep03OpenSubjectSpendRedeemer;

export const NativeScriptDecodingStep03BindDescriptorDatumSchema =
  NativeScriptDecodingStep03DatumSchema;
export type NativeScriptDecodingStep03BindDescriptorDatum = Data.Static<
  typeof NativeScriptDecodingStep03BindDescriptorDatumSchema
>;
export const NativeScriptDecodingStep03BindDescriptorDatum =
  NativeScriptDecodingStep03BindDescriptorDatumSchema as unknown as NativeScriptDecodingStep03BindDescriptorDatum;
export const NativeScriptDecodingStep03BindDescriptorArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  outpoint_key_cbor: Data.Bytes(),
  descriptor_cbor: Data.Bytes(),
  ledger_membership_proof: ProofSchema,
  first_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
});
export type NativeScriptDecodingStep03BindDescriptorArgs = Data.Static<
  typeof NativeScriptDecodingStep03BindDescriptorArgsSchema
>;
export const NativeScriptDecodingStep03BindDescriptorArgs =
  NativeScriptDecodingStep03BindDescriptorArgsSchema as unknown as NativeScriptDecodingStep03BindDescriptorArgs;
export const NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    NativeScriptDecodingStep03BindDescriptorArgsSchema,
  );
export type NativeScriptDecodingStep03BindDescriptorSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema
>;
export const NativeScriptDecodingStep03BindDescriptorSpendRedeemer =
  NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema as unknown as NativeScriptDecodingStep03BindDescriptorSpendRedeemer;

export const NativeScriptDecodingStep03AdvanceOrCloseDatumSchema =
  NativeScriptDecodingStep03DatumSchema;
export type NativeScriptDecodingStep03AdvanceOrCloseDatum = Data.Static<
  typeof NativeScriptDecodingStep03AdvanceOrCloseDatumSchema
>;
export const NativeScriptDecodingStep03AdvanceOrCloseDatum =
  NativeScriptDecodingStep03AdvanceOrCloseDatumSchema as unknown as NativeScriptDecodingStep03AdvanceOrCloseDatum;
export const NativeScriptDecodingStep03AdvanceOrCloseArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  control_cbor: Data.Bytes(),
  chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  frames: Data.Array(NativeScriptFrameSchema),
  step_budget: Data.Integer(),
});
export type NativeScriptDecodingStep03AdvanceOrCloseArgs = Data.Static<
  typeof NativeScriptDecodingStep03AdvanceOrCloseArgsSchema
>;
export const NativeScriptDecodingStep03AdvanceOrCloseArgs =
  NativeScriptDecodingStep03AdvanceOrCloseArgsSchema as unknown as NativeScriptDecodingStep03AdvanceOrCloseArgs;
export const NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    NativeScriptDecodingStep03AdvanceOrCloseArgsSchema,
  );
export type NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema
>;
export const NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer =
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema as unknown as NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer;

// ## Step 04 — finalize

export const NativeScriptDecodingStep04DatumSchema =
  NativeScriptDecodingStep03DatumSchema;
export type NativeScriptDecodingStep04Datum = Data.Static<
  typeof NativeScriptDecodingStep04DatumSchema
>;
export const NativeScriptDecodingStep04Datum =
  NativeScriptDecodingStep04DatumSchema as unknown as NativeScriptDecodingStep04Datum;

/** Twin of `step_04.Args`. */
export const NativeScriptDecodingStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type NativeScriptDecodingStep04Args = Data.Static<
  typeof NativeScriptDecodingStep04ArgsSchema
>;
export const NativeScriptDecodingStep04Args =
  NativeScriptDecodingStep04ArgsSchema as unknown as NativeScriptDecodingStep04Args;

export const NativeScriptDecodingStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep04ArgsSchema);
export type NativeScriptDecodingStep04SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep04SpendRedeemerSchema
>;
export const NativeScriptDecodingStep04SpendRedeemer =
  NativeScriptDecodingStep04SpendRedeemerSchema as unknown as NativeScriptDecodingStep04SpendRedeemer;

// ## Step resolver

export const NATIVE_SCRIPT_DECODING_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03_open_subject",
  "step_03_bind_descriptor",
  "step_03_advance_or_close",
  "step_04",
] as const;
export type NativeScriptDecodingStepName =
  (typeof NATIVE_SCRIPT_DECODING_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch:
 * adding a step without adding its schema fails to compile.
 */
export const nativeScriptDecodingStepDatumSchema = (
  step: NativeScriptDecodingStepName,
) => {
  switch (step) {
    case "step_01":
      return NativeScriptDecodingStep01DatumSchema;
    case "step_02":
      return NativeScriptDecodingStep02DatumSchema;
    case "step_03_open_subject":
      return NativeScriptDecodingStep03OpenSubjectDatum;
    case "step_03_bind_descriptor":
      return NativeScriptDecodingStep03BindDescriptorDatum;
    case "step_03_advance_or_close":
      return NativeScriptDecodingStep03AdvanceOrCloseDatum;
    case "step_04":
      return NativeScriptDecodingStep04DatumSchema;
  }
};

// ## Handoffs (twins of the split engine state constructors)

/**
 * The state step-02 emits: verdict subject and accusation bound, the
 * descriptor and machine fields still at their sentinels.
 */
export const nativeScriptDecodingPreBindScanState = ({
  direction,
  sourceKind,
  verifiedTxId,
  txOrderId,
  scanReasonClass,
  priorLedgerRoot,
  outpointSourceKind,
  outpointCursor,
}: {
  readonly direction: bigint;
  readonly sourceKind: bigint;
  readonly verifiedTxId: string;
  readonly txOrderId: string;
  readonly scanReasonClass: bigint;
  readonly priorLedgerRoot: string;
  readonly outpointSourceKind: bigint;
  readonly outpointCursor: bigint;
}): NativeScriptDecodingScanThreadState => ({
  direction,
  source_kind: sourceKind,
  verified_tx_id: verifiedTxId,
  tx_order_id: txOrderId,
  scan_reason_class: scanReasonClass,
  prior_ledger_root: priorLedgerRoot,
  outpoint_source_kind: outpointSourceKind,
  outpoint_cursor: outpointCursor,
  outpoint_key_hash: "",
  reference_script_language: NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND,
  output_index: -1n,
  total_length: -1n,
  item_commitment: "",
  machine_state_hash: "",
  refusal_class: NATIVE_SCRIPT_DECODING_CLASS_PENDING,
});

/**
 * `OpenSubject` freezes the accused outpoint's exact trie-key hash and output
 * index without widening the 15-field datum.
 */
export const nativeScriptDecodingOpenedSubjectState = ({
  state,
  outpointKeyBytes,
  outputIndex,
}: {
  readonly state: NativeScriptDecodingScanThreadState;
  /** The accused outpoint's canonical trie-key bytes, as hex. */
  readonly outpointKeyBytes: string;
  readonly outputIndex: bigint;
}): Effect.Effect<NativeScriptDecodingScanThreadState, HashingError> =>
  Effect.map(hashHexWithBlake2b(outpointKeyBytes, 32), (outpoint_key_hash) => ({
    ...state,
    outpoint_key_hash,
    output_index: outputIndex,
  }));

/** `BindDescriptor` freezes the authenticated reference-script item anchor. */
export const nativeScriptDecodingBoundDescriptorState = ({
  state,
  referenceScriptLanguage,
  referenceScriptTotalLength,
  referenceScriptItemCommitment,
}: {
  readonly state: NativeScriptDecodingScanThreadState;
  readonly referenceScriptLanguage: bigint;
  readonly referenceScriptTotalLength: bigint;
  readonly referenceScriptItemCommitment: string;
}): NativeScriptDecodingScanThreadState => ({
  ...state,
  reference_script_language: referenceScriptLanguage,
  total_length: referenceScriptTotalLength,
  item_commitment: referenceScriptItemCommitment,
});
