import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export const FraudProofComputationThreadStepDatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
  data: Data.Any(),
});
export type FraudProofComputationThreadStepDatum = Data.Static<
  typeof FraudProofComputationThreadStepDatumSchema
>;
export const FraudProofComputationThreadStepDatum =
  FraudProofComputationThreadStepDatumSchema as unknown as FraudProofComputationThreadStepDatum;

export const FraudProofComputationThreadRedeemerSchema = Data.Enum([
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
