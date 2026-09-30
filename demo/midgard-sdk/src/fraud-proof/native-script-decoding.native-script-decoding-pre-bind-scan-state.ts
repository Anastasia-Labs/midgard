import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { hashHexWithBlake2b, type HashingError } from "../common.js";
import { faultProofStepRedeemerSchema } from "./native.js";
import {
  NATIVE_SCRIPT_DECODING_CLASS_PENDING,
  NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND,
  NativeScriptDecodingScanThreadState,
  NativeScriptDecodingStep01DatumSchema,
  NativeScriptDecodingStep02DatumSchema,
  NativeScriptDecodingStep03AdvanceOrCloseDatum,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema,
  NativeScriptDecodingStep03BindDescriptorDatum,
  NativeScriptDecodingStep03DatumSchema,
  NativeScriptDecodingStep03OpenSubjectDatum,
} from "./native-script-decoding.native-script-decoding-scan-thread-state-schema.js";

export type NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema
>;

export const NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer =
  asDataType<NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer>(
    NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema,
  );

// ## Step 04 — finalize

export const NativeScriptDecodingStep04DatumSchema =
  NativeScriptDecodingStep03DatumSchema;

export type NativeScriptDecodingStep04Datum = Data.Static<
  typeof NativeScriptDecodingStep04DatumSchema
>;

export const NativeScriptDecodingStep04Datum =
  asDataType<NativeScriptDecodingStep04Datum>(
    NativeScriptDecodingStep04DatumSchema,
  );

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
  asDataType<NativeScriptDecodingStep04Args>(
    NativeScriptDecodingStep04ArgsSchema,
  );

export const NativeScriptDecodingStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptDecodingStep04ArgsSchema);

export type NativeScriptDecodingStep04SpendRedeemer = Data.Static<
  typeof NativeScriptDecodingStep04SpendRedeemerSchema
>;

export const NativeScriptDecodingStep04SpendRedeemer =
  asDataType<NativeScriptDecodingStep04SpendRedeemer>(
    NativeScriptDecodingStep04SpendRedeemerSchema,
  );

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
