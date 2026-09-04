/** Lucid `Data` twins of the two Aiken step modules. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const L2TxMistagStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type L2TxMistagStep01Datum = Data.Static<
  typeof L2TxMistagStep01DatumSchema
>;
export const L2TxMistagStep01Datum = asDataType<L2TxMistagStep01Datum>(
  L2TxMistagStep01DatumSchema,
);

export const L2TxMistagStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NativeTxInclusionCarriageSchema,
);
export type L2TxMistagStep01SpendRedeemer = Data.Static<
  typeof L2TxMistagStep01SpendRedeemerSchema
>;
export const L2TxMistagStep01SpendRedeemer =
  asDataType<L2TxMistagStep01SpendRedeemer>(
    L2TxMistagStep01SpendRedeemerSchema,
  );

export const L2TxMistagStep02StateSchema = Data.Object({
  bad_tx_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
  committed_validity_code: Data.Integer(),
});
export type L2TxMistagStep02State = Data.Static<
  typeof L2TxMistagStep02StateSchema
>;
export const L2TxMistagStep02State = asDataType<L2TxMistagStep02State>(
  L2TxMistagStep02StateSchema,
);

export const L2TxMistagStep02DatumSchema = faultProofStepDatumSchema(
  L2TxMistagStep02StateSchema,
);
export type L2TxMistagStep02Datum = Data.Static<
  typeof L2TxMistagStep02DatumSchema
>;
export const L2TxMistagStep02Datum = asDataType<L2TxMistagStep02Datum>(
  L2TxMistagStep02DatumSchema,
);

export const L2TxMistagStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type L2TxMistagStep02Args = Data.Static<
  typeof L2TxMistagStep02ArgsSchema
>;
export const L2TxMistagStep02Args = asDataType<L2TxMistagStep02Args>(
  L2TxMistagStep02ArgsSchema,
);

export const L2TxMistagStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  L2TxMistagStep02ArgsSchema,
);
export type L2TxMistagStep02SpendRedeemer = Data.Static<
  typeof L2TxMistagStep02SpendRedeemerSchema
>;
export const L2TxMistagStep02SpendRedeemer =
  asDataType<L2TxMistagStep02SpendRedeemer>(
    L2TxMistagStep02SpendRedeemerSchema,
  );
