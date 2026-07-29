import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInputListSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/**
 * Reference-input-with-no-index (`reference_input_no_idx`) fault proof — the
 * reference-input mirror of `input-no-idx`. Proves a Midgard block includes a
 * native L2 transaction that *reads* a reference input whose producing
 * transaction IS present in the block, but whose output index is out of range of
 * that producing transaction's outputs.
 *
 * Only step 01 differs from `input-no-idx`: it commits the bad transaction's
 * native `reference_inputs_hash` instead of its `spend_inputs_hash`. Steps 02-04
 * are structurally identical and compile to the same UPLC, so the two families
 * share those three scripts (and their addresses) and stay distinguishable by
 * step 01 plus the category id in the computation-thread token minted at Init.
 *
 * Field order in every `Data.Object` below mirrors the on-chain aiken record
 * declarations 1:1 — the PlutusData encoding is positional, so re-ordering here
 * would silently produce redeemers the validators reject.
 */

export const ReferenceInputNoIdxTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;
export type ReferenceInputNoIdxTxInclusionArgs = NativeTxInclusionArgs;
export const ReferenceInputNoIdxTxInclusionArgs =
  NativeTxInclusionArgs as unknown as ReferenceInputNoIdxTxInclusionArgs;

export const ReferenceInputNoIdxStepCancelSchema = FaultProofStepCancelSchema;
export type ReferenceInputNoIdxStepCancel = FaultProofStepCancel;
export const ReferenceInputNoIdxStepCancel =
  FaultProofStepCancel as unknown as ReferenceInputNoIdxStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const ReferenceInputNoIdxStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxTxInclusionArgsSchema);
export type ReferenceInputNoIdxStep01SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep01SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep01SpendRedeemer =
  ReferenceInputNoIdxStep01SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep01SpendRedeemer;

// ## Step 02 — provide the reference-inputs preimage and select the bad input

export const ReferenceInputNoIdxStep02StateSchema = Data.Object({
  verified_tx_reference_inputs_hash: H32Schema,
});
export type ReferenceInputNoIdxStep02State = Data.Static<
  typeof ReferenceInputNoIdxStep02StateSchema
>;
export const ReferenceInputNoIdxStep02State =
  ReferenceInputNoIdxStep02StateSchema as unknown as ReferenceInputNoIdxStep02State;

export const ReferenceInputNoIdxStep02DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep02StateSchema,
);
export type ReferenceInputNoIdxStep02Datum = Data.Static<
  typeof ReferenceInputNoIdxStep02DatumSchema
>;
export const ReferenceInputNoIdxStep02Datum =
  ReferenceInputNoIdxStep02DatumSchema as unknown as ReferenceInputNoIdxStep02Datum;

export const ReferenceInputNoIdxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_preimage: MidgardTxInputListSchema,
  bad_reference_input_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep02Args = Data.Static<
  typeof ReferenceInputNoIdxStep02ArgsSchema
>;
export const ReferenceInputNoIdxStep02Args =
  ReferenceInputNoIdxStep02ArgsSchema as unknown as ReferenceInputNoIdxStep02Args;

export const ReferenceInputNoIdxStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxStep02ArgsSchema);
export type ReferenceInputNoIdxStep02SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep02SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep02SpendRedeemer =
  ReferenceInputNoIdxStep02SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep02SpendRedeemer;

// ## Step 03 — verify inclusion of the producing transaction
//
// The producing transaction is the one whose id equals the bad reference input's
// `tx_id`. It is bound to the same block via the native inclusion redeemer, and
// its native `outputs_hash` is threaded to step-04.

export const ReferenceInputNoIdxStep03StateSchema = Data.Object({
  bad_reference_input_tx_id: H32Schema,
  bad_reference_input_output_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep03State = Data.Static<
  typeof ReferenceInputNoIdxStep03StateSchema
>;
export const ReferenceInputNoIdxStep03State =
  ReferenceInputNoIdxStep03StateSchema as unknown as ReferenceInputNoIdxStep03State;

export const ReferenceInputNoIdxStep03DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep03StateSchema,
);
export type ReferenceInputNoIdxStep03Datum = Data.Static<
  typeof ReferenceInputNoIdxStep03DatumSchema
>;
export const ReferenceInputNoIdxStep03Datum =
  ReferenceInputNoIdxStep03DatumSchema as unknown as ReferenceInputNoIdxStep03Datum;

export const ReferenceInputNoIdxStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxTxInclusionArgsSchema);
export type ReferenceInputNoIdxStep03SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep03SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep03SpendRedeemer =
  ReferenceInputNoIdxStep03SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep03SpendRedeemer;

// ## Step 04 — verify the producing tx outputs and the out-of-range index
//
// `outputs_preimage` is the producing transaction's native outputs preimage as
// the raw per-output CBOR byte list. On-chain it is re-wrapped with
// `encode_native_byte_list` and hashed to match the committed `outputs_hash`;
// because that encoding is length prefixed, matching the hash also fixes the
// output count, making the out-of-range index check sound.

export const ReferenceInputNoIdxStep04StateSchema = Data.Object({
  producing_tx_outputs_hash: H32Schema,
  bad_reference_input_output_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep04State = Data.Static<
  typeof ReferenceInputNoIdxStep04StateSchema
>;
export const ReferenceInputNoIdxStep04State =
  ReferenceInputNoIdxStep04StateSchema as unknown as ReferenceInputNoIdxStep04State;

export const ReferenceInputNoIdxStep04DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep04StateSchema,
);
export type ReferenceInputNoIdxStep04Datum = Data.Static<
  typeof ReferenceInputNoIdxStep04DatumSchema
>;
export const ReferenceInputNoIdxStep04Datum =
  ReferenceInputNoIdxStep04DatumSchema as unknown as ReferenceInputNoIdxStep04Datum;

export const ReferenceInputNoIdxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_preimage: Data.Array(Data.Bytes()),
});
export type ReferenceInputNoIdxStep04Args = Data.Static<
  typeof ReferenceInputNoIdxStep04ArgsSchema
>;
export const ReferenceInputNoIdxStep04Args =
  ReferenceInputNoIdxStep04ArgsSchema as unknown as ReferenceInputNoIdxStep04Args;

export const ReferenceInputNoIdxStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxStep04ArgsSchema);
export type ReferenceInputNoIdxStep04SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep04SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep04SpendRedeemer =
  ReferenceInputNoIdxStep04SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep04SpendRedeemer;
