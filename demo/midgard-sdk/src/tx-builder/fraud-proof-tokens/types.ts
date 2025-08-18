import { Data } from "@lucid-evolution/lucid";

export const DatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const SpendRedeemerSchema = Data.Enum([Data.Literal("Never")]);
export type SpendRedeemer = Data.Static<typeof SpendRedeemerSchema>;
export const SpendRedeemer = SpendRedeemerSchema as unknown as SpendRedeemer;

export const MintRedeemerSchema = Data.Object({
  hubOracleRefInputIndex: Data.Integer(),
  fraudProofLastStepInputIndex: Data.Integer(),
  computationThreadTokenAssetName: Data.Bytes(),
  outputWithFraudProofIndex: Data.Integer(),
  hubOracleAssetName: Data.Bytes(),
});
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;
