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
 * Input-with-no-index (`input_no_idx`) fault proof — proves a Midgard block
 * includes a native L2 transaction that spends an input whose producing
 * transaction IS present in the block, but whose output index is out of range
 * of that producing transaction's outputs.
 *
 * Native transaction-root inclusion path (same as non-existent-input): both the
 * bad transaction (step 01) and the producing transaction (step 03) are
 * committed by the block's counted `transactions_root` rather than as
 * PlutusData. Schemas reuse the shared `native.js` helpers so the step
 * datum/redeemer envelope matches every other fault-proof family.
 *
 * Field order in every `Data.Object` below mirrors the on-chain aiken record
 * declarations 1:1 — the PlutusData encoding is positional, so re-ordering here
 * would silently produce redeemers the validators reject.
 */

export const InputNoIdxTxInclusionArgsSchema = NativeTxInclusionArgsSchema;
export type InputNoIdxTxInclusionArgs = NativeTxInclusionArgs;
export const InputNoIdxTxInclusionArgs =
  NativeTxInclusionArgs as unknown as InputNoIdxTxInclusionArgs;

export const InputNoIdxStepCancelSchema = FaultProofStepCancelSchema;
export type InputNoIdxStepCancel = FaultProofStepCancel;
export const InputNoIdxStepCancel =
  FaultProofStepCancel as unknown as InputNoIdxStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const InputNoIdxStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxTxInclusionArgsSchema,
);
export type InputNoIdxStep01SpendRedeemer = Data.Static<
  typeof InputNoIdxStep01SpendRedeemerSchema
>;
export const InputNoIdxStep01SpendRedeemer =
  InputNoIdxStep01SpendRedeemerSchema as unknown as InputNoIdxStep01SpendRedeemer;

// ## Step 02 — provide the spend-inputs preimage and select the bad input

export const InputNoIdxStep02StateSchema = Data.Object({
  verified_tx_inputs_hash: H32Schema,
});
export type InputNoIdxStep02State = Data.Static<
  typeof InputNoIdxStep02StateSchema
>;
export const InputNoIdxStep02State =
  InputNoIdxStep02StateSchema as unknown as InputNoIdxStep02State;

export const InputNoIdxStep02DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep02StateSchema,
);
export type InputNoIdxStep02Datum = Data.Static<
  typeof InputNoIdxStep02DatumSchema
>;
export const InputNoIdxStep02Datum =
  InputNoIdxStep02DatumSchema as unknown as InputNoIdxStep02Datum;

export const InputNoIdxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  inputs_preimage: MidgardTxInputListSchema,
  bad_inputs_index: Data.Integer(),
});
export type InputNoIdxStep02Args = Data.Static<
  typeof InputNoIdxStep02ArgsSchema
>;
export const InputNoIdxStep02Args =
  InputNoIdxStep02ArgsSchema as unknown as InputNoIdxStep02Args;

export const InputNoIdxStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxStep02ArgsSchema,
);
export type InputNoIdxStep02SpendRedeemer = Data.Static<
  typeof InputNoIdxStep02SpendRedeemerSchema
>;
export const InputNoIdxStep02SpendRedeemer =
  InputNoIdxStep02SpendRedeemerSchema as unknown as InputNoIdxStep02SpendRedeemer;

// ## Step 03 — verify inclusion of the producing transaction
//
// The producing transaction is the one whose id equals the bad input's
// `tx_id`. It is bound to the same block via the native inclusion redeemer, and
// its native `outputs_hash` is threaded to step-04.

export const InputNoIdxStep03StateSchema = Data.Object({
  bad_input_tx_id: H32Schema,
  bad_input_output_index: Data.Integer(),
});
export type InputNoIdxStep03State = Data.Static<
  typeof InputNoIdxStep03StateSchema
>;
export const InputNoIdxStep03State =
  InputNoIdxStep03StateSchema as unknown as InputNoIdxStep03State;

export const InputNoIdxStep03DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep03StateSchema,
);
export type InputNoIdxStep03Datum = Data.Static<
  typeof InputNoIdxStep03DatumSchema
>;
export const InputNoIdxStep03Datum =
  InputNoIdxStep03DatumSchema as unknown as InputNoIdxStep03Datum;

export const InputNoIdxStep03SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxTxInclusionArgsSchema,
);
export type InputNoIdxStep03SpendRedeemer = Data.Static<
  typeof InputNoIdxStep03SpendRedeemerSchema
>;
export const InputNoIdxStep03SpendRedeemer =
  InputNoIdxStep03SpendRedeemerSchema as unknown as InputNoIdxStep03SpendRedeemer;

// ## Step 04 — verify the producing tx outputs and the out-of-range index
//
// `outputs_preimage` is the producing transaction's native outputs preimage as
// the raw per-output CBOR byte list. On-chain it is re-wrapped with
// `encode_native_byte_list` and hashed to match the committed `outputs_hash`;
// because that encoding is length prefixed, matching the hash also fixes the
// output count, making the out-of-range index check sound.

export const InputNoIdxStep04StateSchema = Data.Object({
  producing_tx_outputs_hash: H32Schema,
  bad_input_output_index: Data.Integer(),
});
export type InputNoIdxStep04State = Data.Static<
  typeof InputNoIdxStep04StateSchema
>;
export const InputNoIdxStep04State =
  InputNoIdxStep04StateSchema as unknown as InputNoIdxStep04State;

export const InputNoIdxStep04DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep04StateSchema,
);
export type InputNoIdxStep04Datum = Data.Static<
  typeof InputNoIdxStep04DatumSchema
>;
export const InputNoIdxStep04Datum =
  InputNoIdxStep04DatumSchema as unknown as InputNoIdxStep04Datum;

export const InputNoIdxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_preimage: Data.Array(Data.Bytes()),
});
export type InputNoIdxStep04Args = Data.Static<
  typeof InputNoIdxStep04ArgsSchema
>;
export const InputNoIdxStep04Args =
  InputNoIdxStep04ArgsSchema as unknown as InputNoIdxStep04Args;

export const InputNoIdxStep04SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxStep04ArgsSchema,
);
export type InputNoIdxStep04SpendRedeemer = Data.Static<
  typeof InputNoIdxStep04SpendRedeemerSchema
>;
export const InputNoIdxStep04SpendRedeemer =
  InputNoIdxStep04SpendRedeemerSchema as unknown as InputNoIdxStep04SpendRedeemer;
