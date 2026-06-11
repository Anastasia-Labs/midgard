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
export type NativeTxBodyCompact = Data.Static<
  typeof NativeTxBodyCompactSchema
>;
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

export const NativeTxInclusionArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  native_tx_id: H32Schema,
  native_tx_compact_cbor: Data.Bytes(),
  tx_membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type NativeTxInclusionArgs = Data.Static<
  typeof NativeTxInclusionArgsSchema
>;
export const NativeTxInclusionArgs =
  NativeTxInclusionArgsSchema as unknown as NativeTxInclusionArgs;

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

export const FaultProofStepCancelSchema = Data.Object({
  input_index: Data.Integer(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type FaultProofStepCancel = Data.Static<
  typeof FaultProofStepCancelSchema
>;
export const FaultProofStepCancel =
  FaultProofStepCancelSchema as unknown as FaultProofStepCancel;

type DataSchema = Parameters<typeof Data.Nullable>[0];

export const faultProofStepRedeemerSchema = <A extends DataSchema>(
  argsSchema: A,
) =>
  Data.Enum([
    Data.Object({ Cancel: FaultProofStepCancelSchema }),
    Data.Object({ Continue: Data.Tuple([argsSchema]) }),
  ]);

export const faultProofStepDatumSchema = <A extends DataSchema>(
  stateSchema: A,
) =>
  Data.Object({
    fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
    data: Data.Nullable(stateSchema),
  });
