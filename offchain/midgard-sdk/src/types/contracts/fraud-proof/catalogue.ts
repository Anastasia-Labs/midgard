import { Data } from "@lucid-evolution/lucid";
import { POSIXTimeSchema } from "../common.js";

export const DatumSchema = Data.Object({
  insertTime: POSIXTimeSchema,
  initStepScriptHash: Data.Bytes(),
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const MintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Literal("NewFraudCategory"),
  Data.Literal("RemoveFraudCategory"),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

export const SpendRedeemerSchema = Data.Object({
  fraudProofCatalogueAssetName: Data.Bytes(),
});
export type SpendRedeemer = Data.Static<typeof SpendRedeemerSchema>;
export const SpendRedeemer = SpendRedeemerSchema as unknown as SpendRedeemer;
