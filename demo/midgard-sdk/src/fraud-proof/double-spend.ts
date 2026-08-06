import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInput,
  MidgardTxInputList,
  MidgardTxInputListSchema,
  MidgardTxInputSchema,
  NativeTxBodyCompact,
  NativeTxBodyCompactSchema,
  NativeTxCompact,
  NativeTxCompactSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export {
  MidgardTxInput,
  MidgardTxInputList,
  MidgardTxInputListSchema,
  MidgardTxInputSchema,
  NativeTxBodyCompact,
  NativeTxBodyCompactSchema,
  NativeTxCompact,
  NativeTxCompactSchema,
};

export const DoubleSpendTxInclusionArgsSchema = NativeTxInclusionArgsSchema;
export type DoubleSpendTxInclusionArgs = NativeTxInclusionArgs;
export const DoubleSpendTxInclusionArgs =
  NativeTxInclusionArgs as unknown as DoubleSpendTxInclusionArgs;

export const DoubleSpendStepCancelSchema = FaultProofStepCancelSchema;
export type DoubleSpendStepCancel = FaultProofStepCancel;
export const DoubleSpendStepCancel =
  FaultProofStepCancel as unknown as DoubleSpendStepCancel;

export const DoubleSpendStep01StateSchema = Data.Object({
  verified_tx1_id: H32Schema,
  verified_tx1_spend_inputs_hash: H32Schema,
});
export type DoubleSpendStep01State = Data.Static<
  typeof DoubleSpendStep01StateSchema
>;
export const DoubleSpendStep01State =
  DoubleSpendStep01StateSchema as unknown as DoubleSpendStep01State;

export const DoubleSpendStep01DatumSchema = faultProofStepDatumSchema(
  DoubleSpendStep01StateSchema,
);
export type DoubleSpendStep01Datum = Data.Static<
  typeof DoubleSpendStep01DatumSchema
>;
export const DoubleSpendStep01Datum =
  DoubleSpendStep01DatumSchema as unknown as DoubleSpendStep01Datum;

export const DoubleSpendStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema);
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

export const DoubleSpendStep02DatumSchema = faultProofStepDatumSchema(
  DoubleSpendStep02StateSchema,
);
export type DoubleSpendStep02Datum = Data.Static<
  typeof DoubleSpendStep02DatumSchema
>;
export const DoubleSpendStep02Datum =
  DoubleSpendStep02DatumSchema as unknown as DoubleSpendStep02Datum;

export const DoubleSpendStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema);
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

export const DoubleSpendStep03DatumSchema = faultProofStepDatumSchema(
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

export const DoubleSpendStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DoubleSpendStep03ArgsSchema);
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

export const DoubleSpendStep04DatumSchema = faultProofStepDatumSchema(
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

export const DoubleSpendStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DoubleSpendStep04ArgsSchema);
export type DoubleSpendStep04SpendRedeemer = Data.Static<
  typeof DoubleSpendStep04SpendRedeemerSchema
>;
export const DoubleSpendStep04SpendRedeemer =
  DoubleSpendStep04SpendRedeemerSchema as unknown as DoubleSpendStep04SpendRedeemer;
