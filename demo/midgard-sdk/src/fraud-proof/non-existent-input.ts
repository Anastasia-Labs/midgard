import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema } from "@/common.js";

import { MidgardTxInputSchema } from "./double-spend.js";

/**
 * Mirrors the on-chain `fraud_proofs/common.NativeTxInclusionArgs` — the native
 * transaction-root inclusion path (the same one double-spend uses), where the
 * bad transaction is committed by the node's native transaction root rather
 * than as a PlutusData `MidgardTxCompact`.
 */
export const TxInclusionArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  native_tx_id: H32Schema,
  native_tx_compact_cbor: Data.Bytes(),
  tx_membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type TxInclusionArgs = Data.Static<typeof TxInclusionArgsSchema>;
export const TxInclusionArgs =
  TxInclusionArgsSchema as unknown as TxInclusionArgs;

export const NonExistentInputStepCancelSchema = Data.Object({
  input_index: Data.Integer(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type NonExistentInputStepCancel = Data.Static<
  typeof NonExistentInputStepCancelSchema
>;
export const NonExistentInputStepCancel =
  NonExistentInputStepCancelSchema as unknown as NonExistentInputStepCancel;

type DataSchema = Parameters<typeof Data.Nullable>[0];

const stepRedeemerSchema = <A extends DataSchema>(argsSchema: A) =>
  Data.Enum([
    Data.Object({ Cancel: NonExistentInputStepCancelSchema }),
    Data.Object({ Continue: Data.Tuple([argsSchema]) }),
  ]);

const stepDatumSchema = <A extends DataSchema>(stateSchema: A) =>
  Data.Object({
    fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
    data: Data.Nullable(stateSchema),
  });

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic `FraudProofComputationThreadStepDatum`. Spending it
// only requires the inclusion redeemer; the produced UTxO carries the step-02
// state.

export const NonExistentInputStep01SpendRedeemerSchema =
  stepRedeemerSchema(TxInclusionArgsSchema);
export type NonExistentInputStep01SpendRedeemer = Data.Static<
  typeof NonExistentInputStep01SpendRedeemerSchema
>;
export const NonExistentInputStep01SpendRedeemer =
  NonExistentInputStep01SpendRedeemerSchema as unknown as NonExistentInputStep01SpendRedeemer;

// ## Step 02 — provide the spend-inputs preimage and select the bad input

export const NonExistentInputStep02StateSchema = Data.Object({
  bad_tx_inputs_hash: H32Schema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep02State = Data.Static<
  typeof NonExistentInputStep02StateSchema
>;
export const NonExistentInputStep02State =
  NonExistentInputStep02StateSchema as unknown as NonExistentInputStep02State;

export const NonExistentInputStep02DatumSchema = stepDatumSchema(
  NonExistentInputStep02StateSchema,
);
export type NonExistentInputStep02Datum = Data.Static<
  typeof NonExistentInputStep02DatumSchema
>;
export const NonExistentInputStep02Datum =
  NonExistentInputStep02DatumSchema as unknown as NonExistentInputStep02Datum;

export const NonExistentInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  inputs_preimage: Data.Array(MidgardTxInputSchema),
  bad_input_index: Data.Integer(),
});
export type NonExistentInputStep02Args = Data.Static<
  typeof NonExistentInputStep02ArgsSchema
>;
export const NonExistentInputStep02Args =
  NonExistentInputStep02ArgsSchema as unknown as NonExistentInputStep02Args;

export const NonExistentInputStep02SpendRedeemerSchema = stepRedeemerSchema(
  NonExistentInputStep02ArgsSchema,
);
export type NonExistentInputStep02SpendRedeemer = Data.Static<
  typeof NonExistentInputStep02SpendRedeemerSchema
>;
export const NonExistentInputStep02SpendRedeemer =
  NonExistentInputStep02SpendRedeemerSchema as unknown as NonExistentInputStep02SpendRedeemer;

// ## Step 03 — prove the missing input is absent from the block's prev ledger

export const NonExistentInputStep03StateSchema = Data.Object({
  missing_input: MidgardTxInputSchema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep03State = Data.Static<
  typeof NonExistentInputStep03StateSchema
>;
export const NonExistentInputStep03State =
  NonExistentInputStep03StateSchema as unknown as NonExistentInputStep03State;

export const NonExistentInputStep03DatumSchema = stepDatumSchema(
  NonExistentInputStep03StateSchema,
);
export type NonExistentInputStep03Datum = Data.Static<
  typeof NonExistentInputStep03DatumSchema
>;
export const NonExistentInputStep03Datum =
  NonExistentInputStep03DatumSchema as unknown as NonExistentInputStep03Datum;

export const NonExistentInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_proof_in_ledger: ProofSchema,
  non_membership_proof_script_redeemer_index: Data.Integer(),
});
export type NonExistentInputStep03Args = Data.Static<
  typeof NonExistentInputStep03ArgsSchema
>;
export const NonExistentInputStep03Args =
  NonExistentInputStep03ArgsSchema as unknown as NonExistentInputStep03Args;

export const NonExistentInputStep03SpendRedeemerSchema = stepRedeemerSchema(
  NonExistentInputStep03ArgsSchema,
);
export type NonExistentInputStep03SpendRedeemer = Data.Static<
  typeof NonExistentInputStep03SpendRedeemerSchema
>;
export const NonExistentInputStep03SpendRedeemer =
  NonExistentInputStep03SpendRedeemerSchema as unknown as NonExistentInputStep03SpendRedeemer;

// ## Step 04 — prove the missing input was not produced within the block

export const NonExistentInputStep04StateSchema = Data.Object({
  missing_input_tx_id: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep04State = Data.Static<
  typeof NonExistentInputStep04StateSchema
>;
export const NonExistentInputStep04State =
  NonExistentInputStep04StateSchema as unknown as NonExistentInputStep04State;

export const NonExistentInputStep04DatumSchema = stepDatumSchema(
  NonExistentInputStep04StateSchema,
);
export type NonExistentInputStep04Datum = Data.Static<
  typeof NonExistentInputStep04DatumSchema
>;
export const NonExistentInputStep04Datum =
  NonExistentInputStep04DatumSchema as unknown as NonExistentInputStep04Datum;

export const NonExistentInputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_proof_in_txs: ProofSchema,
  non_membership_proof_script_redeemer_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type NonExistentInputStep04Args = Data.Static<
  typeof NonExistentInputStep04ArgsSchema
>;
export const NonExistentInputStep04Args =
  NonExistentInputStep04ArgsSchema as unknown as NonExistentInputStep04Args;

export const NonExistentInputStep04SpendRedeemerSchema = stepRedeemerSchema(
  NonExistentInputStep04ArgsSchema,
);
export type NonExistentInputStep04SpendRedeemer = Data.Static<
  typeof NonExistentInputStep04SpendRedeemerSchema
>;
export const NonExistentInputStep04SpendRedeemer =
  NonExistentInputStep04SpendRedeemerSchema as unknown as NonExistentInputStep04SpendRedeemer;
