import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

export const FraudProofTokenDatumSchema = Data.Object({
  fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type FraudProofTokenDatum = Data.Static<
  typeof FraudProofTokenDatumSchema
>;
export const FraudProofTokenDatum = asDataType<FraudProofTokenDatum>(
  FraudProofTokenDatumSchema,
);

export const FraudProofTokenSpendRedeemerSchema = Data.Enum([
  Data.Literal("Never"),
]);
export type FraudProofTokenSpendRedeemer = Data.Static<
  typeof FraudProofTokenSpendRedeemerSchema
>;
export const FraudProofTokenSpendRedeemer =
  asDataType<FraudProofTokenSpendRedeemer>(FraudProofTokenSpendRedeemerSchema);

export const FraudProofTokenMintRedeemerSchema = Data.Object({
  computation_thread_token_asset_name: Data.Bytes(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type FraudProofTokenMintRedeemer = Data.Static<
  typeof FraudProofTokenMintRedeemerSchema
>;
export const FraudProofTokenMintRedeemer =
  asDataType<FraudProofTokenMintRedeemer>(FraudProofTokenMintRedeemerSchema);
