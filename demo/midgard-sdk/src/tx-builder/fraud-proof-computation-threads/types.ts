import { Data } from "@lucid-evolution/lucid";

export const StepDatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
  data: Data.Any(),
});
export type StepDatum = Data.Static<typeof StepDatumSchema>;
export const StepDatum = StepDatumSchema as unknown as StepDatum;

export const RedeemerSchema = Data.Enum([
  Data.Object({
    Mint: Data.Object({
      fraudProofSetNodeRefInputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      fraudedStateQueueNodeRefInputIndex: Data.Integer(),
      outputToInitStepIndex: Data.Integer(),
      fraudProverHash: Data.Bytes(),
      fraudProofCatalogueAssetName: Data.Bytes(),
      hubOracleAssetName: Data.Bytes(),
      stateQueueAssetName: Data.Bytes(),
    }),
  }),
  Data.Object({
    Success: Data.Object({ tokenToBurnAssetName: Data.Bytes() }),
  }),
  Data.Object({
    Cancel: Data.Object({ tokenToBurnAssetName: Data.Bytes() }),
  }),
]);
export type Redeemer = Data.Static<typeof RedeemerSchema>;
export const Redeemer = RedeemerSchema as unknown as Redeemer;
