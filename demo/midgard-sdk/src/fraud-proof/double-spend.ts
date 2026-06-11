import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema } from "@/common.js";

export const NativeTxBodyCompactSchema = Data.Object({
  spend_inputs_hash: H32Schema,
  reference_inputs_hash: H32Schema,
  outputs_hash: H32Schema,
  fee: Data.Integer(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  required_observers_hash: H32Schema,
  required_signers_hash: H32Schema,
  mint_hash: H32Schema,
  script_integrity_hash: H32Schema,
  auxiliary_data_hash: H32Schema,
  network_id: Data.Integer(),
});
export type NativeTxBodyCompact = Data.Static<typeof NativeTxBodyCompactSchema>;
export const NativeTxBodyCompact =
  NativeTxBodyCompactSchema as unknown as NativeTxBodyCompact;

export const NativeTxCompactSchema = Data.Object({
  body: NativeTxBodyCompactSchema,
  witness_set_hash: H32Schema,
  validity_code: Data.Integer(),
});
export type NativeTxCompact = Data.Static<typeof NativeTxCompactSchema>;
export const NativeTxCompact =
  NativeTxCompactSchema as unknown as NativeTxCompact;

export const MidgardTxInputSchema = Data.Object({
  tx_id: H32Schema,
  output_index: Data.Integer(),
});
export type MidgardTxInput = Data.Static<typeof MidgardTxInputSchema>;
export const MidgardTxInput = MidgardTxInputSchema as unknown as MidgardTxInput;

export const MidgardTxInputListSchema = Data.Array(MidgardTxInputSchema);
export type MidgardTxInputList = Data.Static<typeof MidgardTxInputListSchema>;
export const MidgardTxInputList =
  MidgardTxInputListSchema as unknown as MidgardTxInputList;

export const DoubleSpendTxInclusionArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  native_tx_id: H32Schema,
  native_tx_compact_cbor: Data.Bytes(),
  tx_membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type DoubleSpendTxInclusionArgs = Data.Static<
  typeof DoubleSpendTxInclusionArgsSchema
>;
export const DoubleSpendTxInclusionArgs =
  DoubleSpendTxInclusionArgsSchema as unknown as DoubleSpendTxInclusionArgs;

export const DoubleSpendStepCancelSchema = Data.Object({
  input_index: Data.Integer(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type DoubleSpendStepCancel = Data.Static<
  typeof DoubleSpendStepCancelSchema
>;
export const DoubleSpendStepCancel =
  DoubleSpendStepCancelSchema as unknown as DoubleSpendStepCancel;

type DataSchema = Parameters<typeof Data.Nullable>[0];

const stepRedeemerSchema = <A extends DataSchema>(argsSchema: A) =>
  Data.Enum([
    Data.Object({ Cancel: DoubleSpendStepCancelSchema }),
    Data.Object({ Continue: Data.Tuple([argsSchema]) }),
  ]);

const stepDatumSchema = <A extends DataSchema>(stateSchema: A) =>
  Data.Object({
    fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
    data: Data.Nullable(stateSchema),
  });

export const DoubleSpendStep01StateSchema = Data.Object({
  verified_tx1_id: H32Schema,
  verified_tx1_spend_inputs_hash: H32Schema,
});
export type DoubleSpendStep01State = Data.Static<
  typeof DoubleSpendStep01StateSchema
>;
export const DoubleSpendStep01State =
  DoubleSpendStep01StateSchema as unknown as DoubleSpendStep01State;

export const DoubleSpendStep01DatumSchema = stepDatumSchema(
  DoubleSpendStep01StateSchema,
);
export type DoubleSpendStep01Datum = Data.Static<
  typeof DoubleSpendStep01DatumSchema
>;
export const DoubleSpendStep01Datum =
  DoubleSpendStep01DatumSchema as unknown as DoubleSpendStep01Datum;

export const DoubleSpendStep01SpendRedeemerSchema = stepRedeemerSchema(
  DoubleSpendTxInclusionArgsSchema,
);
export type DoubleSpendStep01SpendRedeemer = Data.Static<
  typeof DoubleSpendStep01SpendRedeemerSchema
>;
export const DoubleSpendStep01SpendRedeemer =
  DoubleSpendStep01SpendRedeemerSchema as unknown as DoubleSpendStep01SpendRedeemer;

export const DoubleSpendStep02StateSchema = Data.Object({
  verified_tx1_id: H32Schema,
  verified_tx1_spend_inputs_hash: H32Schema,
});
export type DoubleSpendStep02State = Data.Static<
  typeof DoubleSpendStep02StateSchema
>;
export const DoubleSpendStep02State =
  DoubleSpendStep02StateSchema as unknown as DoubleSpendStep02State;

export const DoubleSpendStep02DatumSchema = stepDatumSchema(
  DoubleSpendStep02StateSchema,
);
export type DoubleSpendStep02Datum = Data.Static<
  typeof DoubleSpendStep02DatumSchema
>;
export const DoubleSpendStep02Datum =
  DoubleSpendStep02DatumSchema as unknown as DoubleSpendStep02Datum;

export const DoubleSpendStep02SpendRedeemerSchema = stepRedeemerSchema(
  DoubleSpendTxInclusionArgsSchema,
);
export type DoubleSpendStep02SpendRedeemer = Data.Static<
  typeof DoubleSpendStep02SpendRedeemerSchema
>;
export const DoubleSpendStep02SpendRedeemer =
  DoubleSpendStep02SpendRedeemerSchema as unknown as DoubleSpendStep02SpendRedeemer;

export const DoubleSpendStep03StateSchema = Data.Object({
  verified_tx1_spend_inputs_hash: H32Schema,
  verified_tx2_spend_inputs_hash: H32Schema,
});
export type DoubleSpendStep03State = Data.Static<
  typeof DoubleSpendStep03StateSchema
>;
export const DoubleSpendStep03State =
  DoubleSpendStep03StateSchema as unknown as DoubleSpendStep03State;

export const DoubleSpendStep03DatumSchema = stepDatumSchema(
  DoubleSpendStep03StateSchema,
);
export type DoubleSpendStep03Datum = Data.Static<
  typeof DoubleSpendStep03DatumSchema
>;
export const DoubleSpendStep03Datum =
  DoubleSpendStep03DatumSchema as unknown as DoubleSpendStep03Datum;

export const DoubleSpendStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  tx1_spend_inputs_ref_input_index: Data.Integer(),
  double_spent_input_index: Data.Integer(),
});
export type DoubleSpendStep03Args = Data.Static<
  typeof DoubleSpendStep03ArgsSchema
>;
export const DoubleSpendStep03Args =
  DoubleSpendStep03ArgsSchema as unknown as DoubleSpendStep03Args;

export const DoubleSpendStep03SpendRedeemerSchema = stepRedeemerSchema(
  DoubleSpendStep03ArgsSchema,
);
export type DoubleSpendStep03SpendRedeemer = Data.Static<
  typeof DoubleSpendStep03SpendRedeemerSchema
>;
export const DoubleSpendStep03SpendRedeemer =
  DoubleSpendStep03SpendRedeemerSchema as unknown as DoubleSpendStep03SpendRedeemer;

export const DoubleSpendStep04StateSchema = Data.Object({
  verified_tx2_spend_inputs_hash: H32Schema,
  double_spent_input: MidgardTxInputSchema,
});
export type DoubleSpendStep04State = Data.Static<
  typeof DoubleSpendStep04StateSchema
>;
export const DoubleSpendStep04State =
  DoubleSpendStep04StateSchema as unknown as DoubleSpendStep04State;

export const DoubleSpendStep04DatumSchema = stepDatumSchema(
  DoubleSpendStep04StateSchema,
);
export type DoubleSpendStep04Datum = Data.Static<
  typeof DoubleSpendStep04DatumSchema
>;
export const DoubleSpendStep04Datum =
  DoubleSpendStep04DatumSchema as unknown as DoubleSpendStep04Datum;

export const DoubleSpendStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  tx2_spend_inputs_ref_input_index: Data.Integer(),
  double_spent_input_index: Data.Integer(),
});
export type DoubleSpendStep04Args = Data.Static<
  typeof DoubleSpendStep04ArgsSchema
>;
export const DoubleSpendStep04Args =
  DoubleSpendStep04ArgsSchema as unknown as DoubleSpendStep04Args;

export const DoubleSpendStep04SpendRedeemerSchema = stepRedeemerSchema(
  DoubleSpendStep04ArgsSchema,
);
export type DoubleSpendStep04SpendRedeemer = Data.Static<
  typeof DoubleSpendStep04SpendRedeemerSchema
>;
export const DoubleSpendStep04SpendRedeemer =
  DoubleSpendStep04SpendRedeemerSchema as unknown as DoubleSpendStep04SpendRedeemer;
