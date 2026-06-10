import { Data } from "@lucid-evolution/lucid";

export const FraudProofComputationThreadStepDatumSchema = Data.Object({
  fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
  data: Data.Nullable(Data.Any()),
});
export type FraudProofComputationThreadStepDatum = Data.Static<
  typeof FraudProofComputationThreadStepDatumSchema
>;
export const FraudProofComputationThreadStepDatum =
  FraudProofComputationThreadStepDatumSchema as unknown as FraudProofComputationThreadStepDatum;

export const FraudProofComputationThreadRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      first_step_output_index: Data.Integer(),
      fraud_proof_catalogue_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      fraudulent_block_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Success: Data.Object({ burning_token_asset_name: Data.Bytes() }),
  }),
  Data.Object({
    BurnForCancellation: Data.Object({
      burning_token_asset_name: Data.Bytes(),
    }),
  }),
]);
export type FraudProofComputationThreadRedeemer = Data.Static<
  typeof FraudProofComputationThreadRedeemerSchema
>;
export const FraudProofComputationThreadRedeemer =
  FraudProofComputationThreadRedeemerSchema as unknown as FraudProofComputationThreadRedeemer;
