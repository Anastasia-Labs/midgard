import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  MerkleRootSchema,
  OutputReferenceSchema,
  ProofSchema,
} from "../common.js";
import {
  BoundedItemChunkProofSchema,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  TransitionStepSchema,
} from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { type ChallengedHeaderHash } from "./fabricated-deposit.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import { NativeScriptFrameSchema } from "./validation-auxiliary-witness.js";

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
  asDataType<NativeScriptDecodingBindState>(
    NativeScriptDecodingBindStateSchema,
  );

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
  asDataType<NativeScriptDecodingScanThreadState>(
    NativeScriptDecodingScanThreadStateSchema,
  );

// ## Step 01 — bind the verdict subject

export const NativeScriptDecodingStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);

export type NativeScriptDecodingStep01Datum = Data.Static<
  typeof NativeScriptDecodingStep01DatumSchema
>;

export const NativeScriptDecodingStep01Datum =
  asDataType<NativeScriptDecodingStep01Datum>(
    NativeScriptDecodingStep01DatumSchema,
  );

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
  asDataType<NativeScriptDecodingStep01Args>(
    NativeScriptDecodingStep01ArgsSchema,
  );

export const NativeScriptDecodingStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep01ArgsSchema);

export type NativeScriptDecodingStep01SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep01SpendRedeemerSchema
>;

export const NativeScriptDecodingStep01SpendRedeemer =
  asDataType<NativeScriptDecodingStep01SpendRedeemer>(
    NativeScriptDecodingStep01SpendRedeemerSchema,
  );

// ## Step 02 — committed-claim openings

export const NativeScriptDecodingStep02DatumSchema = faultProofStepDatumSchema(
  NativeScriptDecodingBindStateSchema,
);

export type NativeScriptDecodingStep02Datum = Data.Static<
  typeof NativeScriptDecodingStep02DatumSchema
>;

export const NativeScriptDecodingStep02Datum =
  asDataType<NativeScriptDecodingStep02Datum>(
    NativeScriptDecodingStep02DatumSchema,
  );

export const NativeScriptDecodingEventToStepMembershipSchema =
  rootMembershipProofSchema(EventKeySchema, EventToStepValueSchema);

export const NativeScriptDecodingTransitionStepMembershipSchema =
  rootMembershipProofSchema(Data.Integer(), TransitionStepSchema);

export const NativeScriptDecodingForcedMembershipSchema =
  rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxV1Schema);

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
  asDataType<NativeScriptDecodingStep02Args>(
    NativeScriptDecodingStep02ArgsSchema,
  );

export const NativeScriptDecodingStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep02ArgsSchema);

export type NativeScriptDecodingStep02SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep02SpendRedeemerSchema
>;

export const NativeScriptDecodingStep02SpendRedeemer =
  asDataType<NativeScriptDecodingStep02SpendRedeemer>(
    NativeScriptDecodingStep02SpendRedeemerSchema,
  );

// ## Split step 03 — OpenSubject, BindDescriptor, AdvanceOrClose

export const NativeScriptDecodingStep03DatumSchema = faultProofStepDatumSchema(
  NativeScriptDecodingScanThreadStateSchema,
);

export const NativeScriptDecodingStep03OpenSubjectDatumSchema =
  NativeScriptDecodingStep03DatumSchema;

export type NativeScriptDecodingStep03OpenSubjectDatum = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectDatumSchema
>;

export const NativeScriptDecodingStep03OpenSubjectDatum =
  asDataType<NativeScriptDecodingStep03OpenSubjectDatum>(
    NativeScriptDecodingStep03OpenSubjectDatumSchema,
  );

export const NativeScriptDecodingStep03OpenSubjectArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  subject_field_opening: Data.Nullable(FieldOpeningSchema),
});

export type NativeScriptDecodingStep03OpenSubjectArgs = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectArgsSchema
>;

export const NativeScriptDecodingStep03OpenSubjectArgs =
  asDataType<NativeScriptDecodingStep03OpenSubjectArgs>(
    NativeScriptDecodingStep03OpenSubjectArgsSchema,
  );

export const NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep03OpenSubjectArgsSchema);

export type NativeScriptDecodingStep03OpenSubjectSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema
>;

export const NativeScriptDecodingStep03OpenSubjectSpendRedeemer =
  asDataType<NativeScriptDecodingStep03OpenSubjectSpendRedeemer>(
    NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema,
  );

export const NativeScriptDecodingStep03BindDescriptorDatumSchema =
  NativeScriptDecodingStep03DatumSchema;

export type NativeScriptDecodingStep03BindDescriptorDatum = Data.Static<
  typeof NativeScriptDecodingStep03BindDescriptorDatumSchema
>;

export const NativeScriptDecodingStep03BindDescriptorDatum =
  asDataType<NativeScriptDecodingStep03BindDescriptorDatum>(
    NativeScriptDecodingStep03BindDescriptorDatumSchema,
  );

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
  asDataType<NativeScriptDecodingStep03BindDescriptorArgs>(
    NativeScriptDecodingStep03BindDescriptorArgsSchema,
  );

export const NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    NativeScriptDecodingStep03BindDescriptorArgsSchema,
  );

export type NativeScriptDecodingStep03BindDescriptorSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema
>;

export const NativeScriptDecodingStep03BindDescriptorSpendRedeemer =
  asDataType<NativeScriptDecodingStep03BindDescriptorSpendRedeemer>(
    NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema,
  );

export const NativeScriptDecodingStep03AdvanceOrCloseDatumSchema =
  NativeScriptDecodingStep03DatumSchema;

export type NativeScriptDecodingStep03AdvanceOrCloseDatum = Data.Static<
  typeof NativeScriptDecodingStep03AdvanceOrCloseDatumSchema
>;

export const NativeScriptDecodingStep03AdvanceOrCloseDatum =
  asDataType<NativeScriptDecodingStep03AdvanceOrCloseDatum>(
    NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
  );

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
  asDataType<NativeScriptDecodingStep03AdvanceOrCloseArgs>(
    NativeScriptDecodingStep03AdvanceOrCloseArgsSchema,
  );

export const NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    NativeScriptDecodingStep03AdvanceOrCloseArgsSchema,
  );
