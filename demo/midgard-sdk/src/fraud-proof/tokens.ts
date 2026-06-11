import { Data } from "@lucid-evolution/lucid";

export const FraudProofTokenDatumSchema = Data.Object({
  fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type FraudProofTokenDatum = Data.Static<
  typeof FraudProofTokenDatumSchema
>;
export const FraudProofTokenDatum =
  FraudProofTokenDatumSchema as unknown as FraudProofTokenDatum;

export const FraudProofTokenSpendRedeemerSchema = Data.Enum([
  Data.Literal("Never"),
]);
export type FraudProofTokenSpendRedeemer = Data.Static<
  typeof FraudProofTokenSpendRedeemerSchema
>;
export const FraudProofTokenSpendRedeemer =
  FraudProofTokenSpendRedeemerSchema as unknown as FraudProofTokenSpendRedeemer;

export const FraudProofTokenMintRedeemerSchema = Data.Object({
  computation_thread_token_asset_name: Data.Bytes(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type FraudProofTokenMintRedeemer = Data.Static<
  typeof FraudProofTokenMintRedeemerSchema
>;
export const FraudProofTokenMintRedeemer =
  FraudProofTokenMintRedeemerSchema as unknown as FraudProofTokenMintRedeemer;
