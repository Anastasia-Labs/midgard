import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { ProofSchema, ScriptHashSchema } from "@/common.js";

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
      fraud_category_id: Data.Bytes(),
      fraud_category: ScriptHashSchema,
      fraud_category_membership_proof: ProofSchema,
      fraud_proof_catalogue_ref_input_index: Data.Integer(),
      inclusion_proof_script_redeemer_index: Data.Integer(),
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

export type FraudProofComputationThreadInitParams = {};
export type FraudProofComputationThreadSuccessParams = {};
export type FraudProofComputationThreadCancelParams = {};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofComputationThreadInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadInitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Success
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofComputationThreadSuccessTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadSuccessParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Cancel
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofComputationThreadCancelTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadCancelParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
