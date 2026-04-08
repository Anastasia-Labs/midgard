import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * SDK types and builders for fraud-proof computation threads.
 *
 * A computation thread represents the stepwise execution state of a fraud proof
 * challenge. Even though the transaction builders are still stubs, the datum
 * and redeemer schemas define the public contract that off-chain code must
 * satisfy.
 */
export const FraudProofComputationThreadStepDatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
  data: Data.Any(),
});
export type FraudProofComputationThreadStepDatum = Data.Static<
  typeof FraudProofComputationThreadStepDatumSchema
>;
export const FraudProofComputationThreadStepDatum =
  FraudProofComputationThreadStepDatumSchema as unknown as FraudProofComputationThreadStepDatum;

/**
 * Redeemers for creating, concluding, or cancelling a computation thread.
 */
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
 * Stub entry point for initializing a fraud-proof computation thread.
 */
export const incompleteFraudProofComputationThreadInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadInitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for concluding a computation thread successfully.
 */
export const incompleteFraudProofComputationThreadSuccessTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadSuccessParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for cancelling a computation thread.
 */
export const incompleteFraudProofComputationThreadCancelTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofComputationThreadCancelParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
